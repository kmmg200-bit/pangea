package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.LoreData
import pangea.model.quest.QuestOutcome._
import pangea.model.quest.{NpcQuest, NpcQuestProgress, NpcQuests}
import pangea.model.user.UserId
import pangea.test.{TestFixtures, TestHeroDao, TestQuestOutcomeDao}
import zio.ZIO
import zio.test._

/** Пройденные задания для Mini App: что герой закончил — по порядку журнала,
  * и как эти задания закончили другие герои. */
object QuestsDoneSpec extends ZIOSpecDefault {

  private val userId  = UserId(1L)
  private def content = ZIO.attempt(SceneContent.load())

  /** Задания, закрытые героем. */
  private def finished(qs: NpcQuest*): NpcQuests =
    qs.foldLeft(NpcQuests.empty)((acc, q) => acc.updated(q, NpcQuestProgress(step = 3, done = true)))

  private def title(c: SceneContent, q: NpcQuest): String = c.text(s"journal.quests.${q.key}.title")

  override def spec = suite("Пройденные задания")(

    test("сюжет первым, потом горожане по порядку журнала; взятое, но не законченное — не пройдено") {
      for {
        c <- content
        quests = finished(NpcQuest.Marisa, NpcQuest.Innkeeper, NpcQuest.StoryMark).take(NpcQuest.Gustavo, 0L)
        list   = QuestsDone.build(c, LoreData.empty.copy(firstStepsDone = true), quests, Map.empty, Map.empty)
      } yield assertTrue(list.map(d => (d.title, d.story)) == List(
        (c.text("firstSteps.title"), true),
        (StoryLine.title(c, NpcQuest.StoryMark), true),
        (title(c, NpcQuest.Innkeeper), false),
        (title(c, NpcQuest.Marisa), false)))
    },

    test("«Встать на ноги», не засчитанное дома, в пройденных не стоит") {
      for {
        c <- content
        list = QuestsDone.build(c, LoreData.empty.copy(firstSteps = true), NpcQuests.empty, Map.empty, Map.empty)
      } yield assertTrue(list.isEmpty)
    },

    test("развилка: проценты других и своя строка; мало закончивших — «среди первых»; без развилки — только название") {
      for {
        c <- content
        list = QuestsDone.build(c, LoreData.empty, finished(NpcQuest.Innkeeper, NpcQuest.Marisa, NpcQuest.LostSquad),
                 mine    = Map(NpcQuest.Marisa.key -> MarisaSaved, NpcQuest.LostSquad.key -> SquadLedOut),
                 tallies = Map(NpcQuest.Marisa.key    -> Map(MarisaAbandoned -> 6L, MarisaSaved -> 4L),
                               NpcQuest.LostSquad.key -> Map(SquadLedOut -> 3L)))
        byTitle = list.map(d => d.title -> d).toMap
        inn     = byTitle(title(c, NpcQuest.Innkeeper))
        marisa  = byTitle(title(c, NpcQuest.Marisa))
        squad   = byTitle(title(c, NpcQuest.LostSquad))
      } yield assertTrue(inn.first.isEmpty && inn.stats.isEmpty) &&
              assertTrue(marisa.first.isEmpty && marisa.stats == List(
                QuestOutcomeStats.Row("60%", "бросили Марису ни с чем", mine = false),
                QuestOutcomeStats.Row("40%", "спасли Марису", mine = true))) &&
              assertTrue(squad.stats.isEmpty && squad.first.exists(_.contains("Вы — среди первых")))
    },

    test("из базы: свой исход и подсчёт других — по пройденным заданиям с развилкой") {
      val hero = TestFixtures.hero(userId)
      for {
        c    <- content
        dao  <- TestHeroDao.withHero(userId, hero)
        _    <- dao.writeNpcQuests(userId, finished(NpcQuest.Marisa).asJson)
        out  <- TestQuestOutcomeDao.make
        _    <- out.seed(MarisaAbandoned -> 6, MarisaSaved -> 3)
        _    <- out.record(hero.id, MarisaSaved, 0L)
        list <- QuestsDone.load(dao, out, c, hero)
      } yield assertTrue(list.map(_.title) == List(title(c, NpcQuest.Marisa))) &&
              assertTrue(list.flatMap(_.stats).map(r => (r.pct, r.mine)) == List(("60%", false), ("40%", true)))
    }
  )
}
