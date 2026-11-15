package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.quest.{NpcQuest, NpcQuests}

/** Глава 10 основной линии, «Сборщик» ([[NpcQuest.StoryCollector]], см.
  * [[StoryLine]]). Ведёт её книга печи из главы 9: «Сборщику лично —
  * отмеченный из Кинэта». Пока глава берётся в конце «Печи» и висит в журнале
  * — откроется с обновлением. */
object StoryCollectorQuest {

  /** Уровень задания: по нему сложность, враги и награда. */
  val Level: Int = 46

  /** Строка карточки книги печи: глава 9 закрыта, а 10-я ещё не открыта —
    * «откроется с обновлением». Иначе — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryFurnace) || q.isDone(NpcQuest.StoryCollector)) ""
    else content.text("story.storyCollector.card.soon")
}
