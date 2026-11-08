package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.quest.QuestOutcome._
import zio.ZIO
import zio.test._

/** Строки «как закончили задание другие»: проценты среди закончивших, от
  * большей доли к меньшей, своя — «как и вы»; мало закончивших — «среди
  * первых». */
object QuestOutcomeStatsSpec extends ZIOSpecDefault {

  private def content = ZIO.attempt(SceneContent.load())

  override def spec = suite("Статистика исходов")(
    test("проценты среди закончивших, от большей доли к меньшей; своя строка — «как и вы»; нулевых нет") {
      for {
        c <- content
        text = QuestOutcomeStats.render(c, MarisaSaved, Map(MarisaAbandoned -> 9L, MarisaSaved -> 4L, MarisaFell -> 7L))
      } yield assertTrue(text ==
        "📊 Как закончили «Письмо Марисе» герои Пангеи:\n" +
          "• 45% бросили Марису ни с чем\n" +
          "• 35% пали от руки коллектора\n" +
          "• 20% спасли Марису — как и вы")
    },
    test("равные доли — в порядке исходов; кто выбрал первым из десяти — видит проценты") {
      for {
        c <- content
        text = QuestOutcomeStats.render(c, MurlocBetrayed, Map(MurlocHelped -> 6L, MurlocBetrayed -> 2L, MurlocRaided -> 2L))
      } yield assertTrue(text ==
        "📊 Как закончили «Деревня Мурлоков» герои Пангеи:\n" +
          "• 60% помогли деревне снаряжением\n" +
          "• 20% вырезали деревню\n" +
          "• 20% сначала носили снаряжение, а потом вырезали деревню — как и вы")
    },
    test("меньше десяти закончивших — «вы среди первых», без процентов") {
      for {
        c <- content
        text = QuestOutcomeStats.render(c, SquadLedOut, Map(SquadLedOut -> 5L, SquadLooted -> 4L))
      } yield assertTrue(text == "📊 Вы — среди первых, кто закончил «Пропавшая дружина»: как поступают другие, станет видно позже.")
    },
    test("своя запись не дошла до базы — герой всё равно в подсчёте") {
      for {
        c <- content
        text = QuestOutcomeStats.render(c, SquadLooted, Map(SquadLedOut -> 9L))
      } yield assertTrue(text.contains("• 90% вывели дружину наверх") && text.contains("• 10% бросили дружину у фонаря и унесли её груз — как и вы"))
    },
    test("доля меньше половины процента — «меньше 1%»; пока выбрали не все — не больше 99%; все — 100%") {
      for {
        c <- content
      } yield assertTrue(QuestOutcomeStats.pct(c, 1L, 1000L) == "меньше 1%") &&
              assertTrue(QuestOutcomeStats.pct(c, 999L, 1000L) == "99%") &&
              assertTrue(QuestOutcomeStats.pct(c, 12L, 12L) == "100%") &&
              assertTrue(QuestOutcomeStats.pct(c, 1L, 3L) == "33%" && QuestOutcomeStats.pct(c, 2L, 3L) == "67%")
    }
  )
}
