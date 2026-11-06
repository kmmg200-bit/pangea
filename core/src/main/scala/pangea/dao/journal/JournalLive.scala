package pangea.dao.journal

import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import doobie.postgres.implicits._
import doobie.util.transactor.Transactor
import pangea.engine.{Journal, PartitionManager}
import pangea.model.GameEvent
import pangea.model.user.UserId
import zio.interop.catz._
import zio.{Task, ZIO, ZLayer}

class JournalLive(xa: Transactor[Task]) extends Journal {
  override def append(event: GameEvent): Task[Unit] =
    sql"""INSERT INTO event_log (user_id, event_type, payload, trace_id)
          VALUES (${event.userId.value}, ${event.eventType}, ${event.payload}, ${event.traceId})"""
      .update.run.transact(xa).unit

  // occurred_at — TIMESTAMP без зоны: now() кладёт туда время в зоне сессии.
  // Поэтому и границу переводим в зону сессии — тогда сравнение верно при
  // любой зоне сервера базы и JVM.
  override def count(userId: UserId, eventType: String, sinceMs: Long): Task[Long] =
    sql"""SELECT count(*) FROM event_log
          WHERE user_id = ${userId.value} AND event_type = $eventType
            AND occurred_at >= (to_timestamp(${sinceMs.toDouble} / 1000.0) AT TIME ZONE current_setting('TimeZone'))"""
      .query[Long].unique.transact(xa)
}

object JournalLive {
  val live: ZLayer[Transactor[Task], Throwable, Journal] =
    ZLayer.fromZIO(
      ZIO.serviceWithZIO[Transactor[Task]] { xa =>
        PartitionManager.ensurePartitions(xa).as(new JournalLive(xa))
      }
    )
}
