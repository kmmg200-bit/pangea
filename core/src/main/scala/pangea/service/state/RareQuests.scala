package pangea.service.state

import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.state.StateType
import zio.{Random, UIO, ZIO}

/** Редкие задания лабиринта: с [[MinDungeonLevel]]-го уровня лабиринта каждый
  * осмотр уровня с шансом [[Chance]] выводит героя на одно из них — на то,
  * которого у него ещё не было. Это отдельный бросок, до пула событий: билеты
  * пула он не трогает. Обязательные встречи (банда, расплата, приговор, зовы) —
  * раньше него. Каждое задание встречается один раз: начато или пройдено — из
  * броска уходит. Отказался при встрече — задание не начато и может встретиться
  * снова. */
object RareQuests {

  /** С какого уровня лабиринта они встречаются. */
  val MinDungeonLevel: Int = 5

  /** Шанс на осмотр: полпроцента. */
  val Chance: Double = 0.005

  /** Задание — и сцена, где оно начинается. Открыто несколько — выпадает
    * любое из них поровну. */
  val Encounters: List[(NpcQuest, StateType)] = List(
    NpcQuest.StrayCasket -> StateType.StrayCasket,
    NpcQuest.HedgeSong   -> StateType.HedgeSong
  )

  /** Что ещё может встретиться на этом уровне: задания, которых у героя не было. */
  def open(quests: NpcQuests, dungeonLevel: Int): List[StateType] =
    if (dungeonLevel < MinDungeonLevel) Nil
    else Encounters.collect { case (q, scene) if !quests.isTaken(q) && !quests.isDone(q) => scene }

  /** Бросок осмотра: выпало — сцена встречи; из нескольких открытых — наугад.
    * Бросок дробный: целые броски пула и событий он не сдвигает. Встречать
    * нечего — не бросает вовсе. */
  def roll(quests: NpcQuests, dungeonLevel: Int): UIO[Option[StateType]] =
    open(quests, dungeonLevel) match {
      case Nil        => ZIO.none
      case candidates =>
        Random.nextDouble.flatMap { d =>
          if (d >= Chance) ZIO.none
          else candidates match {
            case only :: Nil => ZIO.some(only)
            case many        => Random.nextIntBounded(many.size).map(many.lift)
          }
        }
    }
}
