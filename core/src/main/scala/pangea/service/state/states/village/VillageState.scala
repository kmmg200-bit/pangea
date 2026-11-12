package pangea.service.state.states.village

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.{Monster, Race}
import pangea.model.quest.Difficulty
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.model.village.{VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{CharacterMenu, NpcQuestLog, State, UserAction, Villages}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Деревня в лабиринте (см. [[Villages]]). Сюда ведёт событие лабиринта — и
  * карточка «Контракта деревни», когда дело сделано.
  *
  * Приход: деревня одной из смертных рас, и видно, что ей нужна помощь. Дальше
  * — разграбить (подтверждение и сюжетный бой с жителями и сторожевой башней;
  * добыча вернёт сюда — пепелище), пойти к старосте (рассказ о беде и просьба
  * за плату: взяться или отказаться) или уйти. Деревня и её беда лежат в
  * `scene_data` ([[VillageScene]]): «Персонаж» и возврат из него показывают ту
  * же деревню.
  *
  * С контрактом при себе герой приходит сюда за платой — с карточки, когда дело
  * сделано: староста платит один раз, дальше — прощание. Пришёл раньше — староста
  * ждёт, пока беда не кончится. */
case class VillageState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  import VillageScene.{AfterRaid, Arrival, Headman, Paid, Razed}

  private val branch = new Branch(
    routes = Map(
      "VillageRaid"    -> Target.Run { (u, _, r) => atVillage(u, r, Arrival)(_ => confirmRaid(u, r)) },
      "VillageRaidGo"  -> Target.Run { (u, _, r) => atVillage(u, r, Arrival)(s => startRaid(u, s, r)) },
      "VillageBack"    -> Target.Run { (u, _, r) => atVillage(u, r, Arrival)(s => r.show(u, arrivalScreen(s)).as(StateType.Village)) },
      "VillageHeadman" -> Target.Run { (u, _, r) => atVillage(u, r, Arrival)(s => headman(u, s, r)) },
      "VillageAccept"  -> Target.Run { (u, _, r) => atVillage(u, r, Headman)(s => accept(u, s, r)) },
      "VillageDecline" -> Target.Run { (u, _, r) => atVillage(u, r, Headman)(_ => leave(u, r, content.text("village.plea.declined"))) },
      "VillageLeave"   -> Target.Run { (u, _, r) => atVillage(u, r, Arrival)(s =>
                            leave(u, r, content.format("village.left", Villages.args(content, s): _*))) },
      "VillageOut"     -> Target.Run { (u, _, r) => out(u, r) },
      "OpenCharacter"  -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.Village) }
    ),
    // Написал текст или нажал старую кнопку — та же деревня тем же экраном.
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.Village) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Dungeon, StateType.Battle, StateType.HeroStats, StateType.Village)

  /** Вход. Сначала то, что уже случилось: налёт выигран — развязка, дальше
    * пепелище; староста заплатил — прощание. Потом контракт при герое: дело
    * сделано — плата, нет — староста ждёт. Иначе — деревня из сцены (у
    * старосты или у входа), а сцены нет — новая деревня. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      scene  <- readScene(user)
      _      <- scene match {
                  case Some(s) if s.step == AfterRaid => afterRaid(user, s, renderer)
                  case Some(s) if s.step == Razed =>
                    renderer.show(user, Screen(content.text("village.razed"), List(outChoice)))
                  case Some(s) if s.step == Paid =>
                    renderer.show(user, Screen(content.format("village.farewell", Villages.args(content, s): _*), List(outChoice)))
                  case _ =>
                    quests.village match {
                      case Some(c) if c.done => pay(user, c, renderer)
                      case Some(c) =>
                        renderer.show(user, Screen(content.format(s"village.waiting.${c.quest.key}",
                          Villages.args(content, c): _*), List(outChoice)))
                      case None =>
                        scene match {
                          case Some(s) if s.step == Headman => renderer.show(user, pleaScreen(s))
                          case Some(s)                      => renderer.show(user, arrivalScreen(s))
                          case None                         => discover(user, renderer)
                        }
                    }
                }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Кнопка деревни — только на своём шаге и пока контракта нет: старая кнопка с
    * прошлого экрана показывает то, что сейчас. */
  private def atVillage(user: User, renderer: Renderer, step: String)(f: VillageScene => Task[StateType]): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      scene  <- readScene(user)
      res    <- scene match {
                  case Some(s) if quests.village.isEmpty && s.step == step => f(s)
                  case _                                                   => enter(user, renderer).as(StateType.Village)
                }
    } yield res

  // ── Приход ──────────────────────────────────────────────────────────────────

  /** Новая деревня: раса, название, беда, местность вокруг, соседний город,
    * уровень задания и плата — броском; у логова разбойников и главарь, чтобы
    * староста назвал того, кто потом выйдет в бою. Уровень деревни — уровень
    * лабиринта, где её нашли; задание — от −1 до +4 от него, не глубже 150-го. */
  private def discover(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      level   = hero.dungeonLevel.max(1)
      raceI  <- Random.nextIntBounded(Race.mortals.size)
      race    = Race.mortals.lift(raceI).getOrElse(Race.Human)
      nameI  <- Random.nextIntBounded(Villages.names(content, race).size.max(1))
      open    = VillageQuest.openAt(level)
      questI <- Random.nextIntBounded(open.size)
      quest   = open.lift(questI).getOrElse(VillageQuest.BanditLair)
      seed   <- Random.nextLong
      // Главарь банды — любой из смертных рас лабиринта, имя — из списка его расы.
      chiefI <- Random.nextIntBounded(VillageRates.BanditRaces.size.max(1))
      chief   = Option.when(quest == VillageQuest.BanditLair)(
                  VillageRates.BanditRaces.lift(chiefI).getOrElse(VillageRates.BanditLeaderDefaultRace))
      leader  = chief.flatMap(r => MonsterGenerator.legendaryName(r, Rng(seed))._1)
      landI  <- Random.nextIntBounded(Villages.lands(content, race).size.max(1))
      cityI  <- Random.nextIntBounded(Villages.cities(content).size.max(1))
      pct    <- Random.nextLongBetween(100L - VillageRates.SilverSpreadPct, 100L + VillageRates.SilverSpreadPct + 1L)
      // Задание старосты — на уровень проще деревни или до четырёх сложнее: по
      // нему и плата.
      shift  <- Random.nextIntBetween(VillageRates.QuestLevelMin, VillageRates.QuestLevelMax + 1)
      qLevel  = VillageRates.questLevel(level, shift)
      scene   = VillageScene(race, nameI, quest, level, qLevel, VillageRates.silver(qLevel, pct), leader,
                  leaderRace = chief, city = cityI, land = landI)
      _      <- writeScene(user, scene)
      _      <- renderer.show(user, arrivalScreen(scene))
    } yield ()

  /** Деревня у входа: куда вывела изгородь, какая деревня, что у неё
    * стряслось и насколько опасна. */
  private def arrivalScreen(s: VillageScene): Screen =
    Screen(
      Villages.joined(
        Villages.land(content, s.race, s.land),
        content.format(s"village.arrive.${s.race.entryName}", Villages.args(content, s): _*),
        content.text(s"village.trouble.${s.quest.key}"),
        difficulty(s.level)),
      List(
        content.choice("VillageHeadman", "village.headmanLabel").copy(row = Some(0)),
        content.choice("VillageRaid", "village.raidLabel").copy(row = Some(0)),
        content.choice("OpenCharacter", "village.characterLabel").copy(row = Some(1)),
        content.choice("VillageLeave", "village.leaveLabel").copy(row = Some(1))))

  // ── Налёт ───────────────────────────────────────────────────────────────────

  private def confirmRaid(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("village.raid.confirm"), List(
      content.choice("VillageRaidGo", "village.raid.goLabel").copy(row = Some(0)),
      content.choice("VillageBack", "village.raid.backLabel").copy(row = Some(0))))).as(StateType.Village)

  /** Бой с деревней: жители её расы и сторожевая башня в хвосте строя — всё
    * уровня деревни. Бой сюжетный: без «Сбежать», награда как за обычный бой.
    * Отряд идёт с героем. Добыча вернёт сюда — с отметкой, что налёт выигран. */
  private def startRaid(user: User, scene: VillageScene, renderer: Renderer): Task[StateType] =
    for {
      hero     <- getHero(user)
      seed     <- Random.nextLong
      monsters  = Villages.raidMonsters(scene, Rng(seed))
      energies <- ZIO.foreach(monsters)(startEnergy)
      battle    = SoloPveBattle.fromGroup(monsters, hero, energies, towers = VillageRates.RaidTowers)
                    .copy(story = Some(Villages.RaidStory), lootLevel = Some(scene.level.max(1).toLong))
      routing   = LootData(Nil, Nil, returnState = Some(StateType.Village),
                    eventData = Some(scene.copy(step = AfterRaid).asJson))
      _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _        <- heroDao.writeSceneData(user.userId, routing.asJson)
      _        <- renderer.show(user, Screen(content.format("village.raid.start", Villages.args(content, scene): _*), Nil))
    } yield StateType.Battle

  /** Налёт выигран: развязка один раз, дальше — пепелище. */
  private def afterRaid(user: User, scene: VillageScene, renderer: Renderer): Task[Unit] =
    writeScene(user, scene.copy(step = Razed)) *>
      renderer.show(user, Screen(content.format("village.raid.victory", Villages.args(content, scene): _*), List(outChoice)))

  // ── Староста ────────────────────────────────────────────────────────────────

  /** К старосте: кто он — отдельно, потом его просьба с платой и сложностью. */
  private def headman(user: User, scene: VillageScene, renderer: Renderer): Task[StateType] =
    for {
      _ <- writeScene(user, scene.copy(step = Headman))
      _ <- renderer.show(user, Screen(content.text(s"village.headman.${scene.race.entryName}"), Nil))
      _ <- renderer.show(user, pleaScreen(scene))
    } yield StateType.Village

  private def pleaScreen(s: VillageScene): Screen = {
    val a = Villages.args(content, s)
    Screen(
      Villages.joined(
        content.format(s"village.plea.${s.quest.key}.text", a: _*),
        difficulty(s.questLevel) + "\n" + content.format("village.plea.pay", a: _*)),
      List(
        content.choice("VillageAccept", "village.plea.acceptLabel").copy(row = Some(0)),
        content.choice("VillageDecline", "village.plea.declineLabel").copy(row = Some(0))))
  }

  /** Взялся: контракт — в сумку, договор — в журнал, и в лабиринт: дальше ведёт
    * карточка. Уже взят другой контракт — показать, что сейчас. */
  private def accept(user: User, scene: VillageScene, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      took <- Villages.take(heroDao, inventoryRepo, itemRepo, user.userId, hero, VillageContract.of(scene))
      res  <- if (!took) enter(user, renderer).as(StateType.Village)
              else leave(user, renderer, Villages.joined(
                content.format(s"village.plea.${scene.quest.key}.accepted", Villages.args(content, scene): _*),
                content.format("marisa.questItemAdded", "item" -> QuestItemKind.VillageContract.displayName)))
    } yield res

  // ── Плата ───────────────────────────────────────────────────────────────────

  /** Дело сделано — староста платит: серебро, опыт, контракт уходит из сумки. Дальше —
    * прощание: повторный вход второй платы не даёт. */
  private def pay(user: User, contract: VillageContract, renderer: Renderer): Task[Unit] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      paid <- Villages.pay(heroDao, inventoryRepo, user.userId, hero, contract, now)
      (after, exp) = paid
      _    <- writeScene(user, VillageScene(contract.race, contract.name, contract.quest, contract.level, contract.level,
                contract.silver, contract.leader, contract.leaderRace, contract.city, step = Paid))
      a     = Villages.args(content, contract)
      lvlUp = if (after.lvl > hero.lvl) "\n" + content.format("npcQuests.common.levelUp", "level" -> after.lvl.toString) else ""
      _    <- renderer.show(user, Screen(
                Villages.joined(
                  content.format(s"village.pay.${contract.quest.key}", a: _*),
                  content.format("village.pay.reward", ("exp" -> exp.toString) +: a: _*) + lvlUp,
                  content.text("village.questDone")),
                List(outChoice)))
    } yield ()

  // ── Уход ────────────────────────────────────────────────────────────────────

  /** Уйти из деревни: сцена кончается, в лабиринт — сказав `text`. */
  private def leave(user: User, renderer: Renderer, text: String): Task[StateType] =
    clear(user) *> renderer.show(user, Screen(text, Nil)).as(StateType.Dungeon)

  /** «В лабиринт» — с пепелища, после платы или от ждущего старосты. У входа в
    * деревню и у старосты эта кнопка старая: показать, что сейчас. */
  private def out(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      scene  <- readScene(user)
      res    <- scene match {
                  case Some(s) if quests.village.isEmpty && (s.step == Arrival || s.step == Headman) =>
                    enter(user, renderer).as(StateType.Village)
                  case _ => clear(user).as(StateType.Dungeon)
                }
    } yield res

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  private def startEnergy(m: Monster): Task[Long] =
    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct))

  private def difficulty(level: Int): String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(level.max(1)))

  private def outChoice = content.choice("VillageOut", "village.toDungeonLabel")

  private def readScene(user: User): Task[Option[VillageScene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[VillageScene].toOption))

  private def writeScene(user: User, scene: VillageScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def clear(user: User): Task[Unit] = heroDao.writeSceneData(user.userId, Json.Null)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
