package pangea.service.state.states.lostsquad

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.{BattleEffects, MonsterSlot, SoloPveBattle}
import pangea.model.hero.{Achievement, Hero}
import pangea.model.monster.Monster
import pangea.model.quest.QuestOutcome
import pangea.model.skill.MonsterEnergy
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.lostsquad.SquadMineState._
import pangea.service.state.{LostSquadQuest, NpcQuestLog, QuestOutcomeLog, QuestSupport, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Старая гномья штольня под Серебряной жилой — сюда проваливается кирка на
  * первой же добыче, пока фонарь старшины на руках (см. SilverVeinState).
  * Внизу, в зале предков, спят гоблины-мародёры, а за свежим завалом ждут
  * семеро из «Двенадцати фонарей». Развилка:
  *
  * «Разобрать завал» — грохот будит гоблинов: сюжетный бой (`story =
  * squadEscort`, награда как за обычный бой), строй и очередь мародёров уровня
  * героя, последним — их вожак Хруст Собиратель; трое дружинников бьются на
  * стороне героя (см. LostSquadQuest.withEscort). Победа возвращает сюда
  * (`Progress(AfterEscort)` через добычу): дружина наверху, дары — шлем
  * старшины и четыре куска от дружины, «Друг подгорного народа».
  *
  * «Забрать ящик» — груз в сумку, дружина остаётся у фонаря. Брамбл в отряде
  * уходит по свитку, а за стол возвращается с двойной ценой; ящик ждёт
  * Гуннара на Стройке.
  *
  * Погиб в бою — дружина снова за завалом: спуститься можно через любую жилу.
  * Ушёл, не приняв даров, — дружина ждёт у выхода из штольни, вход тот же.
  * Дары приняты — задание закрыто, исход «вывели дружину» записан (`outcomes`,
  * см. [[QuestOutcome]]); с ящиком задание закрывает Гуннар (ConstructionState). */
case class SquadMineState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  outcomes:      Option[QuestOutcomeDao] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "MineDig"   -> Target.Run { (u, _, r) => dig(u, r) },
      "MineCrate" -> Target.Run { (u, _, r) => takeCrate(u, r) },
      "MineGifts" -> Target.Run { (u, _, r) => gifts(u, r) },
      "MineLeave" -> Target.Goto(StateType.Dungeon)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.SquadMine) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.Loot, StateType.SquadMine)

  /** Вход: после боя (добыча положила `Progress(AfterEscort)`) — дружина
    * наверху; первый спуск — провал под жилой; дальше — зал у завала. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero     <- getHero(user)
      quests   <- NpcQuestLog.load(heroDao, user.userId)
      progress <- heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Progress].toOption))
      _        <- if (progress.exists(_.step == Step.AfterEscort) && LostSquadQuest.mineFound(quests))
                    heroDao.writeSceneData(user.userId, Json.Null) *> LostSquadQuest.rescued(heroDao, user.userId) *>
                      renderer.show(user, thanksScreen(hero))
                  // Бой позади, дары не приняты: дружина ждёт у выхода — и когда
                  // герой вернулся через жилу, и когда написал что-то своё.
                  else if (LostSquadQuest.giftsWait(quests)) renderer.show(user, giftsWaitingScreen)
                  else if (LostSquadQuest.lanternOnHands(quests))
                    LostSquadQuest.found(heroDao, user.userId) *>
                      renderer.show(user, Screen(content.text("lostSquad.mine.fall"), Nil)) *>
                      renderer.show(user, hallScreen(hero))
                  else if (LostSquadQuest.mineFound(quests))
                    renderer.show(user, Screen(content.text("lostSquad.mine.again"), Nil)) *>
                      renderer.show(user, hallScreen(hero))
                  else renderer.show(user, Screen(content.text("lostSquad.mine.empty"),
                         List(content.choice("MineLeave", "lostSquad.mine.leaveLabel"))))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Зал у завала ────────────────────────────────────────────────────────────

  /** Спящие гоблины, стук из-за завала, шёпот старшины — и развилка. Брамбл в
    * отряде кричит в щель своё. */
  private def hallScreen(hero: Hero): Screen = {
    val brambl = Option.when(hero.squad.has(AllyKind.Gnome))(content.text("lostSquad.mine.bramblAtRubble"))
    Screen(content.text("lostSquad.mine." + LostSquadQuest.key(hero, "hall")) + brambl.fold("")("\n\n" + _), List(
      content.choice("MineDig", "lostSquad.mine.digLabel").copy(row = Some(0)),
      content.choice("MineCrate", "lostSquad.mine.crateLabel").copy(row = Some(1)),
      content.choice("MineLeave", "lostSquad.mine.leaveLabel").copy(row = Some(1))))
  }

  // ── Вывести дружину ─────────────────────────────────────────────────────────

  /** Завал разбирают — гоблины просыпаются: строй, очередь и вожак, у каждого
    * своя стартовая энергия. Отряд героя идёт с ним. */
  private def dig(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!LostSquadQuest.mineFound(quests)) enter(user, renderer).as(StateType.SquadMine)
                else for {
                  seed          <- Random.nextLong
                  (front, queue, _) = LostSquadQuest.escortMonsters(hero.lvl.toInt, Rng(seed))
                  frontEnergies <- ZIO.foreach(front)(startEnergy)
                  queueSlots    <- ZIO.foreach(queue)(m => startEnergy(m).map(e => slot(m, e)))
                  // Трое из дружины идут в бой вместе с героем.
                  battle         = LostSquadQuest.withEscort(
                                     queueSlots.foldLeft(SoloPveBattle.fromGroup(front, hero, frontEnergies))(_ admit _)
                                       .copy(story = Some(LostSquadQuest.EscortStory)), hero.lvl)
                  routing        = LootData(Nil, Nil, returnState = Some(StateType.SquadMine),
                                     eventData = Some(Progress(Step.AfterEscort).asJson))
                  _             <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _             <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _             <- renderer.show(user, Screen(content.text("lostSquad.escort.start"), Nil))
                } yield StateType.Battle
    } yield res

  private def startEnergy(m: Monster): Task[Long] =
    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct))

  private def slot(m: Monster, energy: Long): MonsterSlot =
    MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp, m.fightStats.armor,
      m.marked, energy, BattleEffects.empty, customName = m.customName)

  /** Дружина наверху: старшина отдаёт шлем, каждый — кусок того, что нёс. */
  private def thanksScreen(hero: Hero): Screen = {
    val brambl = Option.when(hero.squad.has(AllyKind.Gnome))(content.text("lostSquad.mine.bramblThanks"))
    Screen(content.text("lostSquad.mine." + LostSquadQuest.key(hero, "thanks")) + brambl.fold("")("\n\n" + _),
      List(content.choice("MineGifts", "lostSquad.mine.giftsLabel")))
  }

  /** Дружина наверху, дары ещё не приняты: ждёт у выхода из штольни. */
  private def giftsWaitingScreen: Screen =
    Screen(content.text("lostSquad.mine.giftsWaiting"), List(content.choice("MineGifts", "lostSquad.mine.giftsLabel")))

  /** Принять дары: шлем и четыре куска — экраном добычи, задание
    * закрыто, «Друг подгорного народа». */
  private def gifts(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!LostSquadQuest.giftsWait(quests)) enter(user, renderer).as(StateType.SquadMine)
                else for {
                  seed      <- Random.nextLong
                  helmet     = LostSquadQuest.helmet(hero, Rng(seed))
                  (gifts, _) = LostSquadQuest.gifts(Rng(seed + 1L))
                  line      <- LostSquadQuest.finish(heroDao, inventoryRepo, content, user.userId, hero)
                  _         <- QuestOutcomeLog.record(outcomes, hero, QuestOutcome.SquadLedOut)
                  _         <- renderer.show(user, Screen(line, Nil))
                  _         <- QuestSupport.grant(heroDao, content, user, hero, Achievement.SquadFriend, renderer)
                  _         <- heroDao.writeSceneData(user.userId, LootData(items = helmet :: gifts, silvers = Nil,
                                 returnState = Some(StateType.Dungeon),
                                 header = Some(content.text("lostSquad.mine.giftsHeader"))).asJson)
                } yield StateType.Loot
    } yield res

  // ── Забрать груз ────────────────────────────────────────────────────────────

  /** Ящик — в сумку, дружина остаётся у фонаря. Брамбл в отряде этого не
    * прощает: уходит по свитку, и дальше его найм стоит вдвое. */
  private def takeCrate(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!LostSquadQuest.mineFound(quests)) enter(user, renderer).as(StateType.SquadMine)
                else {
                  val brambl = hero.squad.has(AllyKind.Gnome)
                  for {
                    _ <- ZIO.when(brambl)(heroDao.updateSquad(user.userId, hero.squad.sentAway(AllyKind.Gnome, now)))
                    _ <- LostSquadQuest.abandon(heroDao, inventoryRepo, itemRepo, user.userId, hero)
                    _ <- renderer.show(user, Screen(content.text("lostSquad.mine.crateTaken"), Nil))
                    _ <- ZIO.when(brambl)(renderer.show(user, Screen(content.text("lostSquad.mine.bramblLeaves"), Nil)))
                  } yield StateType.Dungeon
                }
    } yield res

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object SquadMineState {

  /** Где мы: развязка после боя у завала. */
  object Step {
    val AfterEscort: String = "afterEscort"
  }

  final case class Progress(step: String)

  object Progress {
    implicit val encoder: Encoder[Progress] = (p: Progress) => Json.obj("squadMineStep" -> p.step.asJson)
    // Ключ свой, а не общий «step»: в scene_data лежат и чужие структуры, и
    // случайное совпадение поля не должно читаться как наш прогресс.
    implicit val decoder: Decoder[Progress] = (c: HCursor) => c.get[String]("squadMineStep").map(Progress(_))
  }
}
