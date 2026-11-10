package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.{Hero, LoreData}
import pangea.model.home.HomeData
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.quest.{DailyQuests, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.states.InventoryState
import pangea.service.state.states.guild.MentorKazimirState
import pangea.service.state.states.home.HomeState
import pangea.service.state.states.temple.TempleAzatState
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryBrandQuest, StoryPowderQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Основная линия, глава 3 «Что горит на груди»: набросок узора ведёт к
  * Казимиру (с 13-го уровня), жрецу и Юноне; встреча с Сойкой — с карточки
  * наброска; страница списка с новым именем героя — зацепка главы 4. */
object StoryBrandSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 13L): Hero = TestFixtures.hero(userId).copy(lvl = lvl, name = Some("Ворон"))

  /** Главы 1 и 2 закрыты; Смотрителя — по выбору. */
  private def afterPowder(fate: WardenFate = WardenFate.Killed): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    StoryPowderQuest.finish(base, fate)
  }

  /** Глава 3 на шаге `step`. */
  private def brand(step: Int, fate: WardenFate = WardenFate.Killed): NpcQuests =
    StoryBrandQuest.step(afterPowder(fate).take(NpcQuest.StoryBrand, 0L), step)

  private val ready: LoreData = LoreData(firstSteps = true, firstStepsDone = true, cluePasses = 5)

  private def dao(h: Hero, q: NpcQuests) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- d.writeLoreData(userId, ready.asJson)
      _ <- d.writeHomeData(userId, HomeData(tier = 1).asJson)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def sketchItem: Item = QuestItemKind.item(QuestItemKind.StorySketch).copy(id = 902L)

  private def openCard(state: InventoryState, item: Item, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${item.id}"}""")), r)

  override def spec = suite("Основная линия: «Что горит на груди»")(

    test("карточка наброска: сразу после главы 2 — к Казимиру, на любом уровне; «на встречу» — только на четвёртом шаге") {
      def card(lvl: Long, q: NpcQuests) =
        for {
          d     <- dao(hero(lvl), q)
          _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(List(sketchItem)), TestItemRepository.make, c)
          _     <- openCard(state, sketchItem, r)
          shown <- r.sentScreens.map(_.last)
        } yield shown
      for {
        low     <- card(1L, afterPowder())
        ready13 <- card(13L, afterPowder())
        third   <- card(13L, brand(3))
        fourth  <- card(13L, brand(4))
      } yield assertTrue(low.text.contains("Покажу лист ему") && ready13.text.contains("Покажу лист ему") && !low.text.contains("окрепну")) &&
              assertTrue(List(low, ready13, third).forall(!_.choices.exists(_.id == "StoryMeetGo"))) &&
              assertTrue(fourth.text.contains("Сойка") && fourth.choices.exists(_.id == "StoryMeetGo"))
    },

    test("Казимир: кнопка — с наброском при герое, на любом уровне; глава берётся и сразу ведёт к жрецу") {
      def menu(lvl: Long, items: List[Item]) =
        for {
          d     <- dao(hero(lvl), afterPowder())
          r     <- TestRenderer.make
          c     <- content
          state  = MentorKazimirState(d, TestInventoryRepository.withItems(items), c)
          _     <- state.enter(testUser, r)
          shown <- r.sentScreens.map(_.last)
        } yield (d, state, r, shown)
      for {
        young   <- menu(12L, List(sketchItem))
        empty   <- menu(13L, Nil)
        open    <- menu(13L, List(sketchItem))
        (d, st, r, shown) = open
        _       <- st.action(testUser, tap("KazStory"), r)
        said    <- texts(r)
        q       <- questOf(d)
      } yield assertTrue(young._4.choices.exists(_.id == "KazStory") && !empty._4.choices.exists(_.id == "KazStory")) &&
              assertTrue(shown.choices.exists(_.id == "KazStory")) &&
              assertTrue(said.contains("Это не руна") && said.contains("📜 Новое задание: «Что горит на груди»")) &&
              assertTrue(StoryBrandQuest.toPriest(q))
    },

    test("жрец: кнопка на шаге жреца с наброском — об отмеченных, дальше к Юноне") {
      for {
        d     <- dao(hero(), brand(2))
        r     <- TestRenderer.make
        c     <- content
        state  = TempleAzatState(d, TestInventoryRepository.withItems(List(sketchItem)), TestItemRepository.make, c)
        _     <- state.action(testUser, tap("Priest"), r)
        menu  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("PriestStory"), r)
        said  <- texts(r)
        q     <- questOf(d)
      } yield assertTrue(menu.choices.exists(_.id == "PriestStory")) &&
              assertTrue(said.contains("Отмеченным не место среди мёртвых") && said.contains("Юнону")) &&
              assertTrue(StoryBrandQuest.toJunona(q))
    },

    test("Юнона: кнопка в канцелярии на её шаге; про Смотрителя — по тому, что с ним сделали; называет Сойку") {
      def ask(fate: WardenFate) =
        for {
          d     <- dao(hero(), brand(3, fate))
          sch   <- TestScheduler.make
          r     <- TestRenderer.make
          c     <- content
          state  = HomeState(d, sch, c, TestInventoryRepository.withItems(List(sketchItem)), TestItemRepository.make)
          _     <- state.action(testUser, tap("HomeOffice"), r)
          menu  <- r.sentScreens.map(_.last)
          _     <- state.action(testUser, tap("HomeJunonaStory"), r)
          said  <- texts(r)
          q     <- questOf(d)
        } yield (menu, said, q)
      for {
        killed   <- ask(WardenFate.Killed)
        released <- ask(WardenFate.Released)
        handed   <- ask(WardenFate.Handed)
      } yield assertTrue(killed._1.choices.exists(_.id == "HomeJunonaStory")) &&
              assertTrue(killed._2.contains("перерезанным горлом") && released._2.contains("ушёл первым кораблём") &&
                handed._2.contains("Спасибо за Смотрителя")) &&
              assertTrue(List(killed, released, handed).forall { case (_, said, q) =>
                said.contains("Зовут Сойкой") && StoryBrandQuest.toMeeting(q) })
    },

    test("встреча с Сойкой: с карточки, из города; «Взять страницу» — набросок уходит, страница с новым именем героя в сумке, глава закрыта") {
      for {
        d     <- dao(hero(), brand(4))
        _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
        inv    = TestInventoryRepository.withItems(List(sketchItem))
        r     <- TestRenderer.make
        c     <- content
        card   = InventoryState(d, inv, TestItemRepository.make, c)
        _     <- openCard(card, sketchItem, r)
        go    <- card.action(testUser, tap("StoryMeetGo"), r)
        state  = StoryMeetingState(d, inv, TestItemRepository.make, c)
        _     <- state.enter(testUser, r)
        meet  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("MeetTake"), r)
        said  <- texts(r)
        q     <- questOf(d)
        out   <- state.action(testUser, tap("MeetLeave"), r)
      } yield assertTrue(go == StateType.StoryMeeting) &&
              assertTrue(meet.text.contains("Клеймо к Клейму тянет") && meet.choices.map(_.id) == List("MeetTake")) &&
              assertTrue(said.contains("моё новое имя: «Ворон»") && said.contains("Кинэт. Ждать") &&
                said.contains("Квест «Что горит на груди» завершён")) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.StoryPage)) &&
                !inv.snapshot.exists(_.questItem.contains(QuestItemKind.StorySketch))) &&
              assertTrue(q.isDone(NpcQuest.StoryBrand) && out == StateType.Tavern)
    },

    test("журнал: глава 3 — сюжетным заданием со своим шагом") {
      for {
        c   <- content
        text = QuestJournal.render(c, hero(), ready, brand(3), DailyQuests.empty, None, 0L)
      } yield assertTrue(text.contains("📌 «Что горит на груди»") && text.contains("спросить Юнону Кинтслев"))
    }
  )
}
