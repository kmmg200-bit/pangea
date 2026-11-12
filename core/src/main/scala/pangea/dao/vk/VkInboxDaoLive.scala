package pangea.dao.vk

import doobie.implicits._
import doobie.postgres.circe.jsonb.implicits._
import doobie.util.transactor.Transactor
import io.circe.Json
import pangea.model.vk.{InboxEvent, InboxKind, InboxStatus}
import pangea.service.sender.vk.LongPollCursor
import zio.Task
import zio.interop.catz._

class VkInboxDaoLive(xa: Transactor[Task]) extends VkInboxDao {

  // Литералы статусов — базовым типом: doobie ищет Meta[InboxStatus], а не
  // для singleton-подтипа.
  private val Pending: InboxStatus = InboxStatus.Pending
  private val Done:    InboxStatus = InboxStatus.Done
  private val Failed:  InboxStatus = InboxStatus.Failed

  override def accept(dedupKey: String, peerId: Long, kind: InboxKind, source: String, event: Json,
                      sentAt: Option[Long], now: Long): Task[Boolean] =
    sql"""insert into vk_inbox(dedup_key, peer_id, kind, source, event, sent_at, received_at)
          values($dedupKey, $peerId, $kind, $source, $event, $sentAt, $now)
          on conflict (dedup_key) do nothing"""
      .update.run.transact(xa).map(_ > 0)

  // Захват одним запросом, как у планировщика. RETURNING порядка не обещает —
  // сортируем сами.
  override def claim(now: Long, until: Long, limit: Int): Task[List[InboxEvent]] =
    sql"""update vk_inbox set locked_until = $until
          where id in (
            select i.id from vk_inbox i
            where i.status = $Pending
              and (i.locked_until is null or i.locked_until <= $now)
              and not exists (
                select 1 from vk_inbox earlier
                where earlier.peer_id = i.peer_id and earlier.status = $Pending and earlier.id < i.id)
            order by i.id
            limit $limit
            for update skip locked)
          returning id, peer_id, kind, source, event, sent_at, received_at, attempts"""
      .query[InboxEvent]
      .to[List]
      .transact(xa)
      .map(_.sortBy(_.id))

  override def done(id: Long, now: Long, event: Option[Json]): Task[Unit] = finish(id, Done, now, event)

  override def failed(id: Long, now: Long, event: Option[Json]): Task[Unit] = finish(id, Failed, now, event)

  private def finish(id: Long, status: InboxStatus, now: Long, event: Option[Json]): Task[Unit] =
    (event match {
      case Some(kept) =>
        sql"""update vk_inbox set status = $status, done_at = $now, locked_until = null, event = $kept
              where id = $id"""
      case None =>
        sql"update vk_inbox set status = $status, done_at = $now, locked_until = null where id = $id"
    }).update.run.transact(xa).unit

  override def bumpAttempts(id: Long): Task[Int] =
    sql"update vk_inbox set attempts = attempts + 1, locked_until = null where id = $id returning attempts"
      .query[Int].unique.transact(xa)

  override def pending: Task[Long] =
    sql"select count(*) from vk_inbox where status = $Pending".query[Long].unique.transact(xa)

  override def forget(before: Long): Task[Int] =
    sql"delete from vk_inbox where status <> $Pending and done_at < $before".update.run.transact(xa)

  override def load: Task[Option[String]] = loadAt(VkInboxDaoLive.MainCursor)

  override def save(ts: String, now: Long): Task[Unit] = saveAt(VkInboxDaoLive.MainCursor, ts, now)

  override val guildsCursor: LongPollCursor = new LongPollCursor {
    def load: Task[Option[String]]             = loadAt(VkInboxDaoLive.GuildsCursor)
    def save(ts: String, now: Long): Task[Unit] = saveAt(VkInboxDaoLive.GuildsCursor, ts, now)
  }

  private def loadAt(id: Int): Task[Option[String]] =
    sql"select ts from vk_longpoll where id = $id".query[String].option.transact(xa)

  private def saveAt(id: Int, ts: String, now: Long): Task[Unit] =
    sql"""insert into vk_longpoll(id, ts, updated_at) values($id, $ts, $now)
          on conflict (id) do update set ts = excluded.ts, updated_at = excluded.updated_at"""
      .update.run.transact(xa).unit
}

object VkInboxDaoLive {

  /** Строки `vk_longpoll`: основное сообщество и гильдейское. */
  val MainCursor: Int   = 1
  val GuildsCursor: Int = 2
}
