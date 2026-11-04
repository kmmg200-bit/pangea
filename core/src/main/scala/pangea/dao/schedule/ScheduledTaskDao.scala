package pangea.dao.schedule

import doobie.util.transactor
import pangea.model.schedule.{ScheduledTask, TaskKind}
import pangea.model.state.StateType
import pangea.model.user.UserId
import zio.{Task, ZLayer}

/** Durable-доступ к очереди отложенных действий (`scheduled_tasks`). */
trait ScheduledTaskDao {

  /** Поставить задачу; возвращает её id. */
  def insert(
    userId:        UserId,
    fireAt:        Long,
    kind:          TaskKind,
    expectedState: StateType,
    action:        String,
    createdAt:     Long
  ): Task[Long]

  /** Захватить готовые к исполнению `Pending`-задачи (`fireAt <= now`) до
    * `until` и вернуть их по возрастанию `fireAt`. Захваченную другим ботом
    * задачу не возьмёт никто, пока не выйдет её аренда; игрока, чья задача уже
    * в работе, пропускаем целиком — его таймеры идут по очереди. */
  def claimDue(now: Long, until: Long, limit: Int): Task[List[ScheduledTask]]

  def markDone(id: Long): Task[Unit]
  def markDead(id: Long): Task[Unit]

  /** Неудачная попытка: счётчик +1, аренда снимается — повтор на следующем тике. */
  def bumpAttempts(id: Long): Task[Int]

  /** Отменить все `Pending`-задачи игрока данного типа (идемпотентно). */
  def cancel(userId: UserId, kind: TaskKind): Task[Int]
}

object ScheduledTaskDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, ScheduledTaskDaoLive] =
    ZLayer.fromFunction(new ScheduledTaskDaoLive(_))
}
