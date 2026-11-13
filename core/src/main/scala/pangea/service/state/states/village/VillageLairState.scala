package pangea.service.state.states.village

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
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
import pangea.service.state.{CharacterMenu, NpcQuestLog, State, UserAction, VillageLair, Villages}
import zio.{Random, Task, ZIO}

/** Логово беды деревни — куда ведёт карточка «Контракта деревни», из
  * лабиринта (см. [[Villages]]). У каждой беды своё логово ([[VillageLair]]):
  * банда на заставе, гнездо сетеплётов в овраге; сцена одна, отличаются
  * тексты (`village.<логово>.*`), кнопки (`<приставка>Fight`…) и бой.
  *
  * У входа видно, кто там и насколько опасен. «Напасть» — один сюжетный бой со
  * всеми обитателями логова уровня задания: без «Сбежать», награда как за
  * обычный бой, отряд с героем. Добыча возвращает сюда (отметка `lair.wonKey`):
  * логово разбито, дальше — к старосте за платой.
  *
  * «Уйти», смерть или ничья контракт не двигают: он при герое, логово ждёт.
  * «Отказаться от контракта» — с подтверждением: контракт и договор уходят, и
  * деревни снова встречаются. */
case class VillageLairState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  lair:          VillageLair
) extends State {

  private val ids = lair.ids

  private val branch = new Branch(
    routes = Map(
      s"${ids}Fight"      -> Target.Run { (u, _, r) => atLair(u, r)(c => fight(u, c, r)) },
      s"${ids}Leave"      -> Target.Run { (u, _, r) => leave(u, r) },
      s"${ids}Abandon"    -> Target.Run { (u, _, r) => atLair(u, r)(c => askAbandon(u, c, r)) },
      s"${ids}AbandonYes" -> Target.Run { (u, _, r) => atLair(u, r)(_ => abandon(u, r)) },
      s"${ids}Back"       -> Target.Run { (u, _, r) => enter(u, r).as(lair.scene) },
      s"${ids}Out"        -> Target.Run { (u, _, _) => clear(u).as(StateType.Dungeon) },
      "OpenCharacter"     -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, lair.scene) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(lair.scene) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Dungeon, StateType.Battle, StateType.HeroStats, lair.scene)

  /** Вход: обитатели перебиты (добыча положила отметку) — логово разбито;
    * логово ещё стоит — вход в него; иначе здесь пусто. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      contract <- standing(user)
      won      <- heroDao.readSceneData(user.userId).map(_.exists(VillageLairState.won(lair, _)))
      _        <- contract match {
                    case Some(c) if won => victory(user, c, renderer)
                    case Some(c)        => renderer.show(user, introScreen(c))
                    case None           =>
                      clear(user) *> renderer.show(user, Screen(text("gone"), List(outChoice)))
                  }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Контракт на это логово, пока оно стоит. */
  private def standing(user: User): Task[Option[VillageContract]] =
    NpcQuestLog.load(heroDao, user.userId).map(q => q.village.filter(_ => Villages.toLair(q, lair.quest)))

  /** Кнопка логова — пока логово стоит; иначе показать, что сейчас. */
  private def atLair(user: User, renderer: Renderer)(f: VillageContract => Task[StateType]): Task[StateType] =
    standing(user).flatMap {
      case Some(c) => f(c)
      case None    => enter(user, renderer).as(lair.scene)
    }

  // ── Тексты и экраны ─────────────────────────────────────────────────────────

  /** Текст этого логова: `village.<логово>.<key>`. */
  private def key(k: String): String = s"village.${lair.texts}.$k"

  private def text(k: String): String = content.text(key(k))

  private def introScreen(c: VillageContract): Screen =
    Screen(
      Villages.joined(content.format(key("intro"), Villages.args(content, c): _*), difficulty(c.level)),
      List(
        content.choice(s"${ids}Fight", key("fightLabel")).copy(row = Some(0)),
        content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(1)),
        content.choice(s"${ids}Leave", "village.lair.leaveLabel").copy(row = Some(1)),
        content.choice(s"${ids}Abandon", "village.lair.abandonLabel").copy(row = Some(2))))

  private def difficulty(level: Int): String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(level.max(1)))

  private def outChoice = content.choice(s"${ids}Out", "village.toDungeonLabel")

  // ── Бой ─────────────────────────────────────────────────────────────────────

  /** Бой со всеми обитателями логова. Добыча вернёт сюда — с отметкой, что они
    * перебиты. */
  private def fight(user: User, contract: VillageContract, renderer: Renderer): Task[StateType] =
    for {
      hero     <- getHero(user)
      seed     <- Random.nextLong
      foes      = Villages.lairMonsters(contract, Rng(seed))
      energies <- ZIO.foreach(foes)(startEnergy)
      battle    = Villages.withBreedTraits(SoloPveBattle.fromGroup(foes, hero, energies)
                    .copy(story = Some(lair.story), lootLevel = Some(contract.level.max(1).toLong)))
      // Выполз красный сетеплёт — сказать об этом сразу, в начале боя.
      red       = foes.exists(_.breed.contains(pangea.model.monster.Breed.RedSpider))
      routing   = LootData(Nil, Nil, returnState = Some(lair.scene), eventData = Some(VillageLairState.wonMark(lair)))
      _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _        <- heroDao.writeSceneData(user.userId, routing.asJson)
      _        <- renderer.show(user, Screen(Villages.joined(
                    content.format(key("fight"), Villages.args(content, contract): _*),
                    if (red) content.text(key("redSpider")) else ""), Nil))
    } yield StateType.Battle

  /** Обитатели перебиты: логово разбито — к старосте за платой. */
  private def victory(user: User, contract: VillageContract, renderer: Renderer): Task[Unit] =
    for {
      _ <- clear(user)
      _ <- Villages.lairCleared(heroDao, user.userId, lair.quest)
      _ <- renderer.show(user, Screen(
             Villages.joined(
               content.format(key("victory"), Villages.args(content, contract): _*),
               content.text("village.lair.returnHint")),
             List(outChoice)))
    } yield ()

  // ── Уход и отказ ────────────────────────────────────────────────────────────

  /** «Уйти»: логово ждёт, контракт при герое. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    clear(user) *> renderer.show(user, Screen(text("left"), Nil)).as(StateType.Dungeon)

  private def askAbandon(user: User, contract: VillageContract, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.format("village.lair.abandon.text", Villages.args(content, contract): _*), List(
      content.choice(s"${ids}AbandonYes", "village.lair.abandon.yesLabel").copy(row = Some(0)),
      content.choice(s"${ids}Back", "village.lair.abandon.noLabel").copy(row = Some(0))))).as(lair.scene)

  /** Отказался: контракт и договор уходят — деревни снова встречаются. */
  private def abandon(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      _    <- Villages.abandon(heroDao, inventoryRepo, user.userId, hero)
      _    <- clear(user)
      _    <- renderer.show(user, Screen(content.text("village.lair.abandon.done"), Nil))
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

object VillageLairState {

  /** Отметка в scene_data, что обитатели логова перебиты: её пишет добыча,
    * читает вход. Ключ у каждого логова свой (`lair.wonKey`): в scene_data
    * лежат и чужие структуры, и чужое поле не должно читаться как наше. */
  def wonMark(lair: VillageLair): Json = Json.fromJsonObject(JsonObject(lair.wonKey -> true.asJson))

  def won(lair: VillageLair, sceneData: Json): Boolean =
    sceneData.hcursor.get[Boolean](lair.wonKey).toOption.contains(true)
}
