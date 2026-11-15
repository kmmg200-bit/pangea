package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.quest.{NpcQuest, NpcQuests}

/** Рубеж II основной линии, «Чем кормить Клеймо» ([[NpcQuest.StoryFeed]], см.
  * [[StoryLine]]). Ведёт его купчая на дегтярный склад из главы 10: Сборщик
  * придёт туда, и взять его надо живым. Пока Рубеж берётся в конце «Сборщика»
  * и висит в журнале — откроется с обновлением. */
object StoryFeedQuest {

  /** Уровень задания: по нему сложность, враги и награда. */
  val Level: Int = 50

  /** Строка карточки купчей: глава 10 закрыта, а Рубеж II ещё не открыт —
    * «откроется с обновлением». Иначе — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryCollector) || q.isDone(NpcQuest.StoryFeed)) ""
    else content.text("story.storyFeed.card.soon")
}
