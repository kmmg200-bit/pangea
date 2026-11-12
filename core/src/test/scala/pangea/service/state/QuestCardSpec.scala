package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.item.QuestItemKind
import pangea.model.quest.{FirstSteps, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.marisa.MarisaHuntState
import pangea.service.state.states.race.RaceQuestState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.ZIO

/** Кнопки заданий на карточках сюжетных предметов — общее бота и приложения:
  * какая кнопка стоит, где её можно нажать, что будет и где герой, пока открыт
  * «Персонаж». Сами сцены заданий проверены в своих тестах, а то, что бот после
  * переезда кнопок ведёт себя как раньше, — в их же тестах карточек. */
object QuestCardSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("1"), TelegramId("-1"))
  private val hero   = TestFixtures.hero(userId)

  private val toLair: NpcQuests = NpcQuests.empty.take(NpcQuest.JorgenDebt, 0L)
  // «Ходовой товар» сдан — к Барти домой за «Большим делом».
  private val bartiOpen: NpcQuests = NpcQuests.empty.take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods)
  private val marisaWaits: NpcQuests =
    NpcQuests.empty.take(NpcQuest.Marisa, 0L).update(NpcQuest.Marisa)(_.copy(step = 3))
  // Первая глава закрыта — вторая, «Порох», ещё не взята.
  private val afterMark: NpcQuests =
    FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)

  private def setup =
    for {
      c   <- ZIO.attempt(SceneContent.load())
      dao <- TestHeroDao.withHero(userId, hero)
      bag  = TestInventoryRepository.withItems(Nil)
      say <- TestRenderer.make
    } yield (new QuestCard(dao, bag, TestItemRepository.make, c), dao, bag, say, c)

  def spec = suite("QuestCard: кнопки заданий на карточках")(
    test("задание лабиринта: с развилки — в сцену; из города — отказ словами бота; мелко — тоже; не на шаге — устарела") {
      val deep = hero.copy(dungeonLevel = JorgenQuest.LairDungeonLevel)
      for {
        t <- setup
        (card, _, _, _, c) = t
      } yield assertTrue(
        card.go("JorgenLairGo", deep, toLair, Some(StateType.Dungeon)) match {
          case QuestCard.Go.Enter(scene, _) => scene == StateType.JorgenLair
          case _                            => false
        },
        card.go("JorgenLairGo", deep, toLair, Some(StateType.GlobalMap)) ==
          QuestCard.Go.Refused(c.text("jorgen.card.onlyInLabyrinth"), toList = false),
        card.go("JorgenLairGo", deep.copy(dungeonLevel = JorgenQuest.LairDungeonLevel - 1), toLair, Some(StateType.Dungeon)) ==
          QuestCard.Go.Refused(c.format("jorgen.card.tooShallow", "level" -> JorgenQuest.LairDungeonLevel.toString), toList = false),
        card.go("JorgenLairGo", deep, NpcQuests.empty, Some(StateType.Dungeon)) == QuestCard.Go.Stale
      )
    },
    test("городское: из любой сцены города — в сцену, из лабиринта — отказ и на карточку") {
      for {
        t <- setup
        (card, _, _, _, c) = t
      } yield assertTrue(
        card.go("BartiHouseGo", hero, bartiOpen, Some(StateType.MarketSquare)) match {
          case QuestCard.Go.Enter(scene, _) => scene == StateType.BartiHouse
          case _                            => false
        },
        card.go("BartiHouseGo", hero, bartiOpen, Some(StateType.Dungeon)) ==
          QuestCard.Go.Refused(c.text("barti.card.onlyInCity"), toList = false)
      )
    },
    test("купон: не из города — отказ и к списку; из города — сцена заведена перед входом") {
      for {
        t <- setup
        (card, dao, _, say, c) = t
        away   = card.go("RaceChange", hero, NpcQuests.empty, Some(StateType.Dungeon))
        inCity = card.go("RaceChange", hero, NpcQuests.empty, Some(StateType.GlobalMap))
        _     <- inCity match {
                   case QuestCard.Go.Enter(_, prep) => prep(user, say)
                   case _                           => ZIO.unit
                 }
        data  <- dao.readSceneData(userId)
      } yield assertTrue(
        away == QuestCard.Go.Refused(c.text("raceQuest.onlyInCity"), toList = true),
        inCity match {
          case QuestCard.Go.Enter(scene, _) => scene == StateType.RaceQuest
          case _                            => false
        },
        data.contains(RaceQuestState.Progress.start.asJson)
      )
    },
    test("карта Кельвина: Мариса ждёт — сначала вопрос с кнопками бота; ответ — в сцену охоты") {
      for {
        t <- setup
        (card, dao, _, say, _) = t
        ask    = card.go("UseKelvinMap", hero, marisaWaits, Some(StateType.GlobalMap))
        alone  = card.go("UseKelvinMap", hero, NpcQuests.empty, Some(StateType.GlobalMap))
        withM  = card.go("MapWithMarisa", hero, marisaWaits, Some(StateType.GlobalMap))
        _     <- withM match {
                   case QuestCard.Go.Enter(_, prep) => prep(user, say)
                   case _                           => ZIO.unit
                 }
        data  <- dao.readSceneData(userId)
      } yield assertTrue(
        ask match {
          case QuestCard.Go.Ask(screen) => screen.choices.map(_.id) == List("MapWithMarisa", "MapAlone")
          case _                        => false
        },
        QuestCard.answers("UseKelvinMap") == Set("MapWithMarisa", "MapAlone"),
        alone match {
          case QuestCard.Go.Enter(scene, _) => scene == StateType.MarisaHunt
          case _                            => false
        },
        data.contains(MarisaHuntState.Progress(MarisaHuntState.Step.Road, withMarisa = true).asJson)
      )
    },
    test("глава сюжета берётся у входа: «Новое задание» говорится тому, кто показывает сцену") {
      for {
        t <- setup
        (card, dao, _, say, c) = t
        go     = card.go("StoryPortGo", hero, afterMark, Some(StateType.GlobalMap))
        _     <- go match {
                   case QuestCard.Go.Enter(_, prep) => prep(user, say)
                   case _                           => ZIO.unit
                 }
        quests <- NpcQuestLog.load(dao, userId)
        said   <- say.sentScreens.map(_.map(_.text).toVector)
      } yield assertTrue(
        go match {
          case QuestCard.Go.Enter(scene, _) => scene == StateType.StoryWarehouse
          case _                            => false
        },
        quests.isTaken(NpcQuest.StoryPowder),
        said == Vector(StoryLine.takenLine(c, NpcQuest.StoryPowder, quests))
      )
    },
    test("письмо Марисы — прямо здесь: текст Кельвина и карта в сумке") {
      for {
        t <- setup
        (card, _, bag, say, c) = t
        _    <- card.go("OpenLetter", hero, NpcQuests.empty, None) match {
                  case QuestCard.Go.Here(run) => run(user, say)
                  case _                      => ZIO.unit
                }
        said <- say.sentScreens.map(_.map(_.text).toVector)
      } yield assertTrue(
        said == Vector(c.text("marisa.letterText"),
          c.format("marisa.questItemAdded", "item" -> QuestItemKind.KelvinMap.displayName)),
        bag.snapshot.exists(_.questItem.contains(QuestItemKind.KelvinMap))
      )
    },
    test("на карточке — та же кнопка, что в боте; не на шаге — никакой; ответ на вопрос — её же") {
      val roster = QuestItemKind.item(QuestItemKind.JorgenRoster).copy(id = 801L)
      val letter = QuestItemKind.item(QuestItemKind.MarisaLetter).copy(id = 802L)
      val map    = QuestItemKind.item(QuestItemKind.KelvinMap).copy(id = 803L)
      val opened = NpcQuests.empty.update(NpcQuest.Marisa)(_.copy(bonus = true))
      for {
        c <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(
        QuestCard.choice(c, roster, toLair).map(_.id).contains("JorgenLairGo"),
        QuestCard.choice(c, roster, NpcQuests.empty).isEmpty,
        QuestCard.choice(c, letter, NpcQuests.empty).map(_.label).contains(c.text("marisa.openLetterLabel")),
        QuestCard.choice(c, letter, opened).map(_.label).contains(c.text("marisa.readLetterLabel")),
        QuestCard.offers(c, map, marisaWaits, "MapAlone"),
        !QuestCard.offers(c, roster, toLair, "BartiHouseGo")
      )
    },
    test("«Персонаж» открыт — герой там, откуда его открыли; в прочих сценах — там, где он есть") {
      assertTrue(
        QuestCard.place(StateType.Inventory, Some(StateType.Dungeon)).contains(StateType.Dungeon),
        QuestCard.place(StateType.HeroStats, Some(StateType.GlobalMap)).contains(StateType.GlobalMap),
        QuestCard.place(StateType.Dungeon, Some(StateType.GlobalMap)).contains(StateType.Dungeon),
        QuestCard.place(StateType.Inventory, None).isEmpty
      )
    }
  )
}
