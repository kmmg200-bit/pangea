package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.quest.{NpcQuest, NpcQuests}

/** Глава 9 основной линии, «Печь» ([[NpcQuest.StoryFurnace]], см.
  * [[StoryLine]]). Ведёт её накладная на лес из главы 8: живое дерево из
  * изгороди везут в Загон, на печной двор. Пока глава берётся в конце
  * «Просеки» и висит в журнале — откроется с обновлением. */
object StoryFurnaceQuest {

  /** Уровень задания: по нему сложность, враги и награда. */
  val Level: Int = 41

  /** Строка карточки накладной: глава 8 закрыта, а 9-я ещё не открыта —
    * «откроется с обновлением». Одиночка едет в Загон с соляным караваном —
    * у него строка «Соли» (см. BartiSaltQuest). Иначе — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryClearing) || q.isDone(NpcQuest.StoryFurnace)) ""
    else if (StoryToolQuest.choice(q).contains(StoryToolQuest.Path.Alone)) BartiSaltQuest.cardLine(content, q)
    else content.text("story.storyFurnace.card.soon")
}
