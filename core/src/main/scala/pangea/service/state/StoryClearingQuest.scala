package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.quest.{NpcQuest, NpcQuests}

/** Глава 8 основной линии, «Просека» ([[NpcQuest.StoryClearing]], см.
  * [[StoryLine]]). Ведёт её пропуск на просеку из главы 7: Чернолом рубит в
  * изгороди прямую дорогу. Пока глава берётся в конце «Новенького» и висит в
  * журнале — откроется с обновлением. */
object StoryClearingQuest {

  /** Уровень задания: по нему сложность, враги и награда. */
  val Level: Int = 36

  /** Строка карточки пропуска: глава 7 закрыта, а 8-я ещё не открыта —
    * «откроется с обновлением». Иначе — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryNewcomer) || q.isDone(NpcQuest.StoryClearing)) ""
    else content.text("story.storyClearing.card.soon")
}
