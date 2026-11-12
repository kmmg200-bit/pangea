package pangea.service.state.states.village

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.Monster
import pangea.model.quest.Difficulty
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.model.village.VillageContract
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.village.VillageBanditsState.Won
import pangea.service.state.{CharacterMenu, NpcQuestLog, State, UserAction, Villages}
import zio.{Random, Task, ZIO}

/** Логово разбойников у деревни — беда «логово разбойников» (см. [[Villages]]).
  * Сюда ведёт карточка «Контракта деревни» — из лабиринта.
  *
  * У входа видно банду и главаря, которого назвал староста. «Напасть» — один
  * сюжетный бой со всей бандой (люди уровня задания, главарь именной): без
  * «Сбежать», награда как за обычный бой, отряд с героем. Добыча возвращает
  * сюда ([[Won]]): логово разбито, дальше — к старосте за платой.
  *
  * «Уйти», смерть или ничья контракт не двигают: он при герое, логово
  * ждёт. «Отказаться от контракта» — с подтверждением: контракт и договор уходят,
  * и деревни снова встречаются. */
case class VillageBanditsState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "BanditsFight"      -> Target.Run { (u, _, r) => atLair(u, r)(c => fight(u, c, r)) },
      "BanditsLeave"      -> Target.Run { (u, _, r) => leave(u, r) },
      "BanditsAbandon"    -> Target.Run { (u, _, r) => atLair(u, r)(c => askAbandon(u, c, r)) },
      "BanditsAbandonYes" -> Target.Run { (u, _, r) => atLair(u, r)(_ => abandon(u, r)) },
      "BanditsBack"       -> Target.Run { (u, _, r) => enter(u, r).as(StateType.VillageBandits) },
      "BanditsOut"        -> Target.Run { (u, _, _) => clear(u).as(StateType.Dungeon) },
      "OpenCharacter"     -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.VillageBandits) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.VillageBandits) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Dungeon, StateType.Battle, StateType.HeroStats, StateType.VillageBandits)

  /** Вход: банда перебита (добыча положила [[Won]]) — логово разбито; логово
    * ещё стоит — вход в него; иначе здесь пусто. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      contract <- lair(user)
      won      <- heroDao.readSceneData(user.userId).map(_.exists(_.as[Won].exists(_.won)))
      _        <- contract match {
                    case Some(c) if won => victory(user, c, renderer)
                    case Some(c)        => renderer.show(user, introScreen(c))
                    case None           =>
                      clear(user) *> renderer.show(user, Screen(content.text("village.bandits.gone"), List(outChoice)))
                  }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Контракт на это логово, пока оно стоит. */
  private def lair(user: User): Task[Option[VillageContract]] =
    NpcQuestLog.load(heroDao, user.userId).map(q => q.village.filter(_ => Villages.toBandits(q)))

  /** Кнопка логова — пока логово стоит; иначе показать, что сейчас. */
  private def atLair(user: User, renderer: Renderer)(f: VillageContract => Task[StateType]): Task[StateType] =
    lair(user).flatMap {
      case Some(c) => f(c)
      case None    => enter(user, renderer).as(StateType.VillageBandits)
    }

  // ── Экраны ──────────────────────────────────────────────────────────────────

  private def introScreen(c: VillageContract): Screen =
    Screen(
      Villages.joined(content.format("village.bandits.intro", Villages.args(content, c): _*), difficulty(c.level)),
      List(
        content.choice("BanditsFight", "village.bandits.fightLabel").copy(row = Some(0)),
        content.choice("OpenCharacter", "village.bandits.characterLabel").copy(row = Some(1)),
        content.choice("BanditsLeave", "village.bandits.leaveLabel").copy(row = Some(1)),
        content.choice("BanditsAbandon", "village.bandits.abandonLabel").copy(row = Some(2))))

  private def difficulty(level: Int): String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(level.max(1)))

  private def outChoice = content.choice("BanditsOut", "village.toDungeonLabel")

  // ── Бой ─────────────────────────────────────────────────────────────────────

  /** Бой со всей бандой. Добыча вернёт сюда — с отметкой, что она перебита. */
  private def fight(user: User, contract: VillageContract, renderer: Renderer): Task[StateType] =
    for {
      hero     <- getHero(user)
      seed     <- Random.nextLong
      gang      = Villages.banditGang(contract, Rng(seed))
      energies <- ZIO.foreach(gang)(startEnergy)
      battle    = SoloPveBattle.fromGroup(gang, hero, energies)
                    .copy(story = Some(Villages.BanditsStory), lootLevel = Some(contract.level.max(1).toLong))
      routing   = LootData(Nil, Nil, returnState = Some(StateType.VillageBandits), eventData = Some(Won(won = true).asJson))
      _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _        <- heroDao.writeSceneData(user.userId, routing.asJson)
      _        <- renderer.show(user, Screen(content.format("village.bandits.fight", Villages.args(content, contract): _*), Nil))
    } yield StateType.Battle

  /** Банда перебита: логово разбито — к старосте за платой. */
  private def victory(user: User, contract: VillageContract, renderer: Renderer): Task[Unit] =
    for {
      _ <- clear(user)
      _ <- Villages.banditsBeaten(heroDao, user.userId)
      _ <- renderer.show(user, Screen(
             Villages.joined(
               content.format("village.bandits.victory", Villages.args(content, contract): _*),
               content.text("village.bandits.returnHint")),
             List(outChoice)))
    } yield ()

  // ── Уход и отказ ────────────────────────────────────────────────────────────

  /** «Уйти»: логово ждёт, контракт при герое. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    clear(user) *> renderer.show(user, Screen(content.text("village.bandits.left"), Nil)).as(StateType.Dungeon)

  private def askAbandon(user: User, contract: VillageContract, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.format("village.bandits.abandon.text", Villages.args(content, contract): _*), List(
      content.choice("BanditsAbandonYes", "village.bandits.abandon.yesLabel").copy(row = Some(0)),
      content.choice("BanditsBack", "village.bandits.abandon.noLabel").copy(row = Some(0))))).as(StateType.VillageBandits)

  /** Отказался: контракт и договор уходят — деревни снова встречаются. */
  private def abandon(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      _    <- Villages.abandon(heroDao, inventoryRepo, user.userId, hero)
      _    <- clear(user)
      _    <- renderer.show(user, Screen(content.text("village.bandits.abandon.done"), Nil))
    } yield StateType.Dungeon

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  private def startEnergy(m: Monster): Task[Long] =
    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct))

  private def clear(user: User): Task[Unit] = heroDao.writeSceneData(user.userId, Json.Null)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object VillageBanditsState {

  /** Банда перебита — пишет добыча, читает вход. */
  final case class Won(won: Boolean)

  object Won {
    implicit val encoder: Encoder[Won] = (w: Won) => Json.obj("villageBanditsWon" -> w.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры, и чужое поле не должно
    // читаться как наше.
    implicit val decoder: Decoder[Won] = (c: HCursor) => c.get[Boolean]("villageBanditsWon").map(Won(_))
  }
}
