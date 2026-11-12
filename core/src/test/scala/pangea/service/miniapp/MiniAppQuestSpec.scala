package pangea.service.miniapp

import io.circe.syntax.EncoderOps
import pangea.engine.{Renderer, SceneContent}
import pangea.model.artifact.HeroArtifacts
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.hero.Hero
import pangea.model.home.HomeData
import pangea.model.inventory.Inventory
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.quest.{FirstSteps, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.miniapp.MiniAppView.{QuestActView, QuestAskView}
import pangea.service.state.states.marisa.MarisaHuntState
import pangea.service.state.{JorgenQuest, NpcQuestLog, QuestCard, SceneDoor, StoryLine}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository}
import zio.test._
import zio.{Ref, Task, ZIO}

/** Кнопка задания с карточки сюжетного предмета — из приложения. Правила кнопок
  * — общие с ботом и проверены в QuestCardSpec; здесь — что место считается по
  * сцене героя (а при открытом «Персонаже» — по тому, откуда он открыт), что
  * сцена задания открывается в чате, а приложению говорят, что дальше там, и что
  * устаревшая кнопка никуда не ведёт. */
object MiniAppQuestSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("1"), TelegramId("-1"))

  /** Дверь бота без бота: куда вошли (откуда → куда) и что сказали в чат. */
  private final class TestDoor(entered: Ref[Vector[(StateType, StateType)]], said: Ref[Vector[String]]) extends SceneDoor {
    val chat: Renderer = new MiniApp.Collect(said)
    def enter(user: User, from: StateType, to: StateType): Task[Unit] = entered.update(_ :+ (from -> to))
  }

  private val address = QuestItemKind.item(QuestItemKind.BartiAddress).copy(id = 701L)
  private val roster  = QuestItemKind.item(QuestItemKind.JorgenRoster).copy(id = 702L)
  private val cipher  = QuestItemKind.item(QuestItemKind.StoryCipher).copy(id = 703L)
  private val kelvin  = QuestItemKind.item(QuestItemKind.KelvinMap).copy(id = 704L)
  private val letter  = QuestItemKind.item(QuestItemKind.MarisaLetter).copy(id = 705L)

  // «Ходовой товар» сдан — к Барти домой за «Большим делом».
  private val bartiOpen: NpcQuests = NpcQuests.empty.take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods)
  private val toLair: NpcQuests    = NpcQuests.empty.take(NpcQuest.JorgenDebt, 0L)
  private val marisaWaits: NpcQuests =
    NpcQuests.empty.take(NpcQuest.Marisa, 0L).update(NpcQuest.Marisa)(_.copy(step = 3))
  // Первая глава закрыта — вторая, «Порох», ещё не взята.
  private val afterMark: NpcQuests =
    FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)

  /** Герой в сцене `state` (`back` — откуда открыт «Персонаж»), журнал `quests`,
    * в сумке — `items`. */
  private def setup(state: StateType, quests: NpcQuests, items: List[Item], back: Option[StateType] = None) =
    for {
      c       <- ZIO.attempt(SceneContent.load())
      hero     = TestFixtures.hero(userId).copy(state = state, dungeonLevel = JorgenQuest.LairDungeonLevel)
      dao     <- TestHeroDao.withHero(userId, hero)
      _       <- NpcQuestLog.save(dao, userId, quests)
      _       <- dao.writeReturnState(userId, back)
      bag      = TestInventoryRepository.withItems(items)
      entered <- Ref.make(Vector.empty[(StateType, StateType)])
      said    <- Ref.make(Vector.empty[String])
      app      = new AppQuests(dao, bag, new QuestCard(dao, bag, TestItemRepository.make, c), c, new TestDoor(entered, said))
    } yield (hero, dao, bag, app, entered, said, c)

  private def view(hero: Hero, items: List[Item], quests: NpcQuests, c: SceneContent) =
    MiniAppView.inventory(hero, Inventory(1L, hero.id, 20L, Items(items)),
      MiniAppView.Stores(HeroArtifacts.empty(hero.id), Barrel.empty(hero.id), BankVault.empty(hero.id), HomeData.empty),
      c, _.name, 0L, quests)

  def spec = suite("MiniApp: кнопки заданий")(
    test("из города: сцена задания — в чате, вошли из сцены героя; приложению — «продолжение в чате»") {
      for {
        t <- setup(StateType.MarketSquare, bartiOpen, List(address))
        (hero, _, _, app, entered, _, c) = t
        out  <- app.run(user, hero, address.id, "BartiHouseGo")
        went <- entered.get
      } yield assertTrue(
        out == AppQuests.Outcome(Some(c.text("miniapp.quest.toChat")), chat = true, None),
        went == Vector(StateType.MarketSquare -> StateType.BartiHouse)
      )
    },
    test("в чате открыт «Персонаж» — место то, откуда его открыли: из лабиринта — к логову, из города — отказ") {
      for {
        a <- setup(StateType.Inventory, toLair, List(roster), back = Some(StateType.Dungeon))
        (heroA, _, _, appA, enteredA, _, _) = a
        outA  <- appA.run(user, heroA, roster.id, "JorgenLairGo")
        wentA <- enteredA.get
        b <- setup(StateType.Inventory, toLair, List(roster), back = Some(StateType.GlobalMap))
        (heroB, _, _, appB, enteredB, _, c) = b
        outB  <- appB.run(user, heroB, roster.id, "JorgenLairGo")
        wentB <- enteredB.get
      } yield assertTrue(
        outA.chat,
        wentA == Vector(StateType.Inventory -> StateType.JorgenLair),
        outB == AppQuests.Outcome.said(c.text("jorgen.card.onlyInLabyrinth")),
        wentB.isEmpty
      )
    },
    test("кнопки нет на карточке или предмета нет — устарела, никуда не идём") {
      for {
        t <- setup(StateType.Dungeon, toLair, List(address))
        (hero, _, _, app, entered, _, c) = t
        wrong <- app.run(user, hero, address.id, "JorgenLairGo")
        gone  <- app.run(user, hero, 999L, "BartiHouseGo")
        went  <- entered.get
      } yield assertTrue(
        wrong == AppQuests.Outcome.said(c.text("miniapp.quest.stale")),
        gone == AppQuests.Outcome.said(c.text("miniapp.quest.stale")),
        went.isEmpty
      )
    },
    test("глава сюжета: «Новое задание» — в чат перед сценой, в приложение — только «продолжение в чате»") {
      for {
        t <- setup(StateType.GlobalMap, afterMark, List(cipher))
        (hero, dao, _, app, entered, said, c) = t
        out    <- app.run(user, hero, cipher.id, "StoryPortGo")
        went   <- entered.get
        chat   <- said.get
        quests <- NpcQuestLog.load(dao, userId)
      } yield assertTrue(
        out.chat && out.note.contains(c.text("miniapp.quest.toChat")),
        went == Vector(StateType.GlobalMap -> StateType.StoryWarehouse),
        chat == Vector(StoryLine.takenLine(c, NpcQuest.StoryPowder, quests))
      )
    },
    test("карта Кельвина, Мариса ждёт: сначала вопрос в приложении; ответ — в сцену охоты") {
      for {
        t <- setup(StateType.GlobalMap, marisaWaits, List(kelvin))
        (hero, dao, _, app, entered, _, c) = t
        ask    <- app.run(user, hero, kelvin.id, "UseKelvinMap")
        before <- entered.get
        alone  <- app.run(user, hero, kelvin.id, "MapAlone")
        went   <- entered.get
        data   <- dao.readSceneData(userId)
      } yield assertTrue(
        ask == AppQuests.Outcome(None, chat = false, Some(QuestAskView(c.screen("marisa.mapTakeMarisa").text,
          List(QuestActView("MapWithMarisa", "Да, зайти за ней"), QuestActView("MapAlone", "Нет, пойти одному"))))),
        before.isEmpty,
        alone.chat,
        went == Vector(StateType.GlobalMap -> StateType.MarisaHunt),
        data.contains(MarisaHuntState.Progress(MarisaHuntState.Step.Road, withMarisa = false).asJson)
      )
    },
    test("письмо Марисы — прямо в приложении: текст заметкой, карта в сумке, в чат — ничего") {
      for {
        t <- setup(StateType.Dungeon, NpcQuests.empty, List(letter))
        (hero, _, bag, app, entered, said, c) = t
        out  <- app.run(user, hero, letter.id, "OpenLetter")
        went <- entered.get
        chat <- said.get
      } yield assertTrue(
        !out.chat,
        out.note.exists(_.startsWith(c.text("marisa.letterText"))),
        bag.snapshot.exists(_.questItem.contains(QuestItemKind.KelvinMap)),
        went.isEmpty && chat.isEmpty
      )
    },
    test("в сумке — кнопка задания с подписью бота без значка; не на шаге — нет") {
      for {
        c   <- ZIO.attempt(SceneContent.load())
        hero = TestFixtures.hero(userId)
        open = view(hero, List(address, letter), bartiOpen, c)
        shut = view(hero, List(address), NpcQuests.empty, c)
      } yield assertTrue(
        open.bagActs.find(_.item == address.id).flatMap(_.quest).contains(QuestActView("BartiHouseGo", "Зайти к Барти")),
        open.bagActs.find(_.item == letter.id).flatMap(_.quest).contains(QuestActView("OpenLetter", "Вскрыть письмо")),
        shut.bagActs.find(_.item == address.id).forall(_.quest.isEmpty),
        MiniAppView.bare("⚓ Идти в порт") == "Идти в порт",
        MiniAppView.bare("«Порох»") == "«Порох»"
      )
    }
  )
}
