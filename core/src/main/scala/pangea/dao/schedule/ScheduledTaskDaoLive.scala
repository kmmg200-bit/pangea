package pangea.dao.schedule

import doobie.implicits._
import doobie.util.transactor.Transactor
import pangea.model.schedule.{ScheduledTask, TaskKind, TaskStatus}
import pangea.model.state.StateType
import pangea.model.user.UserId
import zio.Task
import zio.interop.catz._

class ScheduledTaskDaoLive(xa: Transactor[Task]) extends ScheduledTaskDao {

  // Литералы статусов приводим к базовому типу: doobie ищет Meta[TaskStatus],
  // а не для singleton-подтипа `Pending.type`.
  private val pending:   TaskStatus = TaskStatus.Pending
  private val done:      TaskStatus = TaskStatus.Done
  private val dead:      TaskStatus = TaskStatus.Dead
  private val cancelled: TaskStatus = TaskStatus.Cancelled

  override def insert(
    userId:        UserId,
    fireAt:        Long,
    kind:          TaskKind,
    expectedState: StateType,
    action:        String,
    createdAt:     Long
  ): Task[Long] =
    sql"""insert into scheduled_tasks(user_id, fire_at, kind, expected_state, action, status, attempts, created_at)
          values(${userId.value}, $fireAt, $kind, $expectedState, $action, $pending, 0, $createdAt)"""
      .update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(xa)

  // Захват одним запросом: SKIP LOCKED пропускает строки, которые прямо сейчас
  // захватывает другой бот, а аренда (locked_until) — те, что уже исполняются.
  // RETURNING порядка не обещает — сортируем сами.
  override def claimDue(now: Long, until: Long, limit: Int): Task[List[ScheduledTask]] =
    sql"""update scheduled_tasks set locked_until = $until
          where id in (
            select t.id from scheduled_tasks t
            where t.status = $pending and t.fire_at <= $now
              and (t.locked_until is null or t.locked_until <= $now)
              and not exists (
                select 1 from scheduled_tasks busy
                where busy.user_id = t.user_id and busy.status = $pending and busy.locked_until > $now)
            order by t.fire_at
            limit $limit
            for update skip locked)
          returning id, user_id, fire_at, kind, expected_state, action, status, attempts, created_at"""
      .query[ScheduledTask]
      .to[List]
      .transact(xa)
      .map(_.sortBy(t => (t.fireAt, t.id)))

  override def markDone(id: Long): Task[Unit] =
    sql"update scheduled_tasks set status = $done where id = $id"
      .update.run.transact(xa).unit

  override def markDead(id: Long): Task[Unit] =
    sql"update scheduled_tasks set status = $dead where id = $id"
      .update.run.transact(xa).unit

  override def bumpAttempts(id: Long): Task[Int] =
    sql"update scheduled_tasks set attempts = attempts + 1, locked_until = null where id = $id returning attempts"
      .query[Int].unique.transact(xa)

  override def cancel(userId: UserId, kind: TaskKind): Task[Int] =
    sql"""update scheduled_tasks set status = $cancelled
          where user_id = ${userId.value} and kind = $kind and status = $pending"""
      .update.run.transact(xa)
}
