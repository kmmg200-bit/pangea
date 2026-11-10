package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.{Hero, LoreData}
import pangea.model.home.{HomeData, House}
import pangea.model.quest.{DailyQuests, FirstSteps, NpcQuests, QuestOutcome}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{NpcQuestLog, QuestJournal}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Встать на ноги» засчитывается дома: при входе, когда все пункты
  * выполнены, — сцена по дому и «Задание выполнено»; до тех пор задание висит
  * в журнале с подсказкой вернуться домой. */
object FirstStepsHomeSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def hero(lvl: Long = 5L): Hero = TestFixtures.hero(userId).copy(lvl = lvl)

  private val cityDone: NpcQuests =
    FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))

  private val taken: LoreData = LoreData(firstSteps = true)

  private def home(h: House): HomeData = HomeData(tier = h.tier)

  private def enterHome(h: Hero, house: House, q: NpcQuests = cityDone, lore: LoreData = taken,
                        outcomes: Option[TestQuestOutcomeDao] = None) =
    for {
      d   <- TestHeroDao.withHero(userId, h)
      _   <- NpcQuestLog.save(d, userId, q)
      _   <- d.writeHomeData(userId, home(house).asJson)
      _   <- d.writeLoreData(userId, lore.asJson)
      sch <- TestScheduler.make
      c   <- content
      r   <- TestRenderer.make
      st   = HomeState(d, sch, c, TestInventoryRepository.accepting, TestItemRepository.make, outcomes = outcomes)
      _   <- st.enter(testUser, r)
      all <- texts(r)
    } yield (d, st, all)

  private def loreOf(d: TestHeroDao): Task[LoreData] =
    d.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                = ZIO.attempt(SceneContent.load())

  override def spec = suite("«Встать на ноги» — засчитано дома")(

    test("все пункты выполнены — при входе домой сцена по дому и «Задание выполнено»; засчитано, счёт исследований с нуля") {
      for {
        cottage <- enterHome(hero(), House.Cottage)
        town    <- enterHome(hero(), House.Townhouse)
        island  <- enterHome(hero(), House.Island)
        lore    <- loreOf(cottage._1)
      } yield assertTrue(cottage._3.contains("задвигаю засов") && cottage._3.contains("📜 Задание «Встать на ноги» выполнено.")) &&
              assertTrue(town._3.contains("Каменный низ, крепкая дверь") && town._3.contains("Задание «Встать на ноги» выполнено")) &&
              assertTrue(island._3.contains("вместо улицы облака") && island._3.contains("Задание «Встать на ноги» выполнено")) &&
              // у каждого дома — своя сцена
              assertTrue(!cottage._3.contains("облака") && !island._3.contains("задвигаю засов")) &&
              assertTrue(lore.firstStepsDone && lore.cluePasses == 0)
    },

    test("под «Задание выполнено» — в каком доме закончили другие: исход записан; мало закончивших — «среди первых»") {
      for {
        o      <- TestQuestOutcomeDao.make
        _      <- o.seed(QuestOutcome.FirstStepsCottage -> 6, QuestOutcome.FirstStepsTownhouse -> 2, QuestOutcome.FirstStepsIsland -> 1)
        island <- enterHome(hero(), House.Island, outcomes = Some(o))
        h      <- island._1.getHeroByUserId(userId).map(_.get)
        mine   <- o.of(h.id, FirstSteps.Key)
        first  <- TestQuestOutcomeDao.make
        alone  <- enterHome(hero(), House.Cottage, outcomes = Some(first))
      } yield assertTrue(mine.contains(QuestOutcome.FirstStepsIsland)) &&
              assertTrue(island._3.contains(List(
                "📜 Задание «Встать на ноги» выполнено.",
                "",
                "📊 Как закончили «Встать на ноги» герои Пангеи:",
                "• 60% обзавелись домиком в две комнаты",
                "• 20% обзавелись добротным домом",
                "• 20% обзавелись домом на острове — как и вы").mkString("\n"))) &&
              assertTrue(alone._3.contains("Вы — среди первых, кто закончил «Встать на ноги»"))
    },

    test("второй вход домой — сцены нет; пункт не выполнен — сцены нет и задание не засчитано") {
      for {
        first   <- enterHome(hero(), House.Cottage)
        (d, st, _) = first
        r       <- TestRenderer.make
        _       <- st.enter(testUser, r)
        again   <- texts(r)
        weak    <- enterHome(hero(lvl = 4L), House.Cottage)
        lore    <- loreOf(weak._1)
        partial <- enterHome(hero(), House.Cottage, q = NpcQuests.empty)
      } yield assertTrue(!again.contains("Задание «Встать на ноги» выполнено") && d != null) &&
              assertTrue(!weak._3.contains("Задание «Встать на ноги» выполнено") && !lore.firstStepsDone) &&
              assertTrue(!partial._3.contains("Задание «Встать на ноги» выполнено"))
    },

    test("журнал: всё сделано, но дома ещё не был — задание висит с подсказкой; засчитано — его нет") {
      for {
        c     <- content
        house  = home(House.Cottage)
        before = QuestJournal.render(c, hero(), taken, cityDone, DailyQuests.empty, None, 0L, house)
        after  = QuestJournal.render(c, hero(), taken.copy(firstStepsDone = true), cityDone, DailyQuests.empty, None, 0L, house)
      } yield assertTrue(before.contains("📌 «Встать на ноги»") && before.contains("🏠 Всё сделано — осталось вернуться домой")) &&
              assertTrue(!after.contains("Встать на ноги"))
    },

    test("старая запись без новых полей читается: не засчитано, исследований ноль") {
      val old = LoreData(firstSteps = true).asJson.mapObject(_.remove("firstStepsDone").remove("cluePasses"))
      assertTrue(old.as[LoreData].map(l => (l.firstStepsDone, l.cluePasses)) == Right((false, 0)))
    }
  )
}
