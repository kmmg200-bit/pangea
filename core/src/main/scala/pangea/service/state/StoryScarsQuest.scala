package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.quest.{NpcQuest, NpcQuests}

/** Акт III «Шрамы мира», глава 11 ([[NpcQuest.StoryScars]], см. [[StoryLine]]).
  * Пока берётся в конце Рубежа II и висит в журнале — откроется с обновлением.
  * Ведёт её та же купчая на дегтярный склад: склад остаётся у героя. */
object StoryScarsQuest {

  /** Строка карточки купчей: Рубеж II пройден, а глава 11 ещё не открыта —
    * «откроется с обновлением». Иначе — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryFeed) || q.isDone(NpcQuest.StoryScars)) ""
    else content.text("story.storyScars.card.soon")
}
