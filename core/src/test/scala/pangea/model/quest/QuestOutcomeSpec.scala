package pangea.model.quest

import zio.test._

/** Исходы заданий с развилкой лежат в базе строками: имена не меняются, у
  * каждого задания — свои. */
object QuestOutcomeSpec extends ZIOSpecDefault {

  override def spec = suite("Исходы заданий")(
    test("строки исходов — те, что уже лежат в quest_outcomes (переименовывать нельзя)") {
      val names = QuestOutcome.values.map(o => o.questKey -> o.entryName).toList
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
        "lostSquad" -> "looted",
        "storyPowder" -> "killed",
        "storyPowder" -> "released",
        "storyPowder" -> "handed",
        "storyList" -> "forgave",
        "storyList" -> "broke",
        "firstSteps" -> "cottage",
        "firstSteps" -> "townhouse",
        "firstSteps" -> "island"
      ))
    },
    test("отслеживаются задания с развилкой: Мариса — пять исходов, мурлоки — три, дружина — два, «Порох у причала» — три, «Список» — два") {
      val tracked = NpcQuest.values.filter(q => QuestOutcome.of(q).nonEmpty).toList
      assertTrue(tracked == List(NpcQuest.Marisa, NpcQuest.Murloc, NpcQuest.LostSquad, NpcQuest.StoryPowder, NpcQuest.StoryList)) &&
      assertTrue(QuestOutcome.of(NpcQuest.Marisa).size == 5 && QuestOutcome.of(NpcQuest.Murloc).size == 3 &&
        QuestOutcome.of(NpcQuest.LostSquad).size == 2) &&
      // «Встать на ноги» — по дому, строка исхода — ключ дома
      assertTrue(QuestOutcome.of(FirstSteps.Key).map(_.entryName) == pangea.model.home.House.values.map(_.key) &&
        QuestOutcome.FirstStepsIsland.titleKey == "firstSteps.title" &&
        QuestOutcome.MarisaSaved.titleKey == "journal.quests.marisa.title")
    },
    test("исход по записи из базы: задание и строка; чужая строка или чужое задание — ничего") {
      assertTrue(QuestOutcome.find(NpcQuest.Murloc, "betrayed").contains(QuestOutcome.MurlocBetrayed)) &&
      assertTrue(QuestOutcome.find(NpcQuest.Marisa, "betrayed").isEmpty && QuestOutcome.find(NpcQuest.Marisa, "nope").isEmpty)
    }
  )
}
