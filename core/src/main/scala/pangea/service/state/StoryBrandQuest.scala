package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.quest.{NpcQuest, NpcQuests}

/** Глава 3 основной линии, «Что горит на груди» ([[NpcQuest.StoryBrand]], см.
  * [[StoryLine]]). Ведёт её набросок узора из бумаг Смотрителя — Клеймо героя
  * и приписка «брать живым». Казимир читает узоры и шлёт к жрецу; жрец
  * называет таких, как герой, отмеченными и шлёт к Юноне — совет записывает
  * всех, кто попадает в Кинэт не по-людски; Юнона называет Сойку — второго
  * отмеченного в городе. Встреча с ней — с карточки наброска, из города
  * (StoryMeetingState): она отдаёт следующую зацепку, страницу из списка
  * отмеченных, где стоит и новое имя героя.
  *
  * Шаги: глава берётся у Казимира (набросок при герое) и сразу идёт к жрецу —
  * 2; 3 — Юнона; 4 — встреча с Сойкой. Закрывается, когда страница у героя:
  * набросок уходит. */
object StoryBrandQuest {

  /** Уровень задания — его сложность на карточке наброска. */
  val Level: Int = 13

  def toKazimir(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryBrand, 1)
  def toPriest(q: NpcQuests): Boolean  = q.onStep(NpcQuest.StoryBrand, 2)
  def toJunona(q: NpcQuests): Boolean  = q.onStep(NpcQuest.StoryBrand, 3)
  def toMeeting(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryBrand, 4)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryBrand)(_.copy(step = n))

  /** Строка карточки наброска — что делать сейчас. Глава 2 не закрыта или 3
    * уже закрыта — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryPowder) || q.isDone(NpcQuest.StoryBrand)) ""
    else if (!q.isTaken(NpcQuest.StoryBrand)) content.text("story.storyBrand.card.ready")
    else content.list("story.storyBrand.card.steps").lift(q.of(NpcQuest.StoryBrand).step - 1).getOrElse("")
}
