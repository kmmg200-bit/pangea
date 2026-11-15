package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.quest.{NpcQuest, NpcQuests}

/** Глава 7 основной линии, «Новенький» ([[NpcQuest.StoryNewcomer]], см.
  * [[StoryLine]]). Ведёт её опись урожая из главы 6: на главную площадь
  * падает новый отмеченный, и за ним приходят раньше героя. Пока глава
  * берётся в конце «Наутро» и висит в журнале — откроется с обновлением. */
object StoryNewcomerQuest {

  /** Уровень задания: по нему сложность, враги и награда. */
  val Level: Int = 31

  /** Строка карточки описи: глава 6 закрыта, а 7-я ещё не открыта —
    * «откроется с обновлением». Иначе — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryMorning) || q.isDone(NpcQuest.StoryNewcomer)) ""
    else content.text("story.storyNewcomer.card.soon")
}
