package pangea.model.quest

import zio.test._

/** Исходы заданий с развилкой лежат в базе строками: имена не меняются, у
  * каждого задания — свои. */
object QuestOutcomeSpec extends ZIOSpecDefault {

  override def spec = suite("Исходы заданий")(
    test("строки исходов — те, что уже лежат в quest_outcomes (переименовывать нельзя)") {
      val names = QuestOutcome.values.map(o => o.quest.key -> o.entryName).toList
      assertTrue(names == List(
        "marisa"    -> "saved",
        "marisa"    -> "abandoned",
        "marisa"    -> "paidAlone",
        "marisa"    -> "killedAlone",
        "marisa"    -> "fell",
        "murloc"    -> "helped",
        "murloc"    -> "raided",
        "murloc"    -> "betrayed",
        "lostSquad" -> "ledOut",
        "lostSquad" -> "looted"
      ))
    },
    test("отслеживаются только три задания с развилкой: Мариса — пять исходов, мурлоки — три, дружина — два") {
      val tracked = NpcQuest.values.filter(q => QuestOutcome.of(q).nonEmpty).toList
      assertTrue(tracked == List(NpcQuest.Marisa, NpcQuest.Murloc, NpcQuest.LostSquad)) &&
      assertTrue(QuestOutcome.of(NpcQuest.Marisa).size == 5 && QuestOutcome.of(NpcQuest.Murloc).size == 3 &&
        QuestOutcome.of(NpcQuest.LostSquad).size == 2)
    },
    test("исход по записи из базы: задание и строка; чужая строка или чужое задание — ничего") {
      assertTrue(QuestOutcome.find(NpcQuest.Murloc, "betrayed").contains(QuestOutcome.MurlocBetrayed)) &&
      assertTrue(QuestOutcome.find(NpcQuest.Marisa, "betrayed").isEmpty && QuestOutcome.find(NpcQuest.Marisa, "nope").isEmpty)
    }
  )
}
