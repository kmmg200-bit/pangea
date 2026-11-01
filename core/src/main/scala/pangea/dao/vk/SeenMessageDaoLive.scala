package pangea.dao.vk

import doobie.implicits._
import doobie.util.transactor.Transactor
import zio.Task
import zio.interop.catz._

class SeenMessageDaoLive(xa: Transactor[Task]) extends SeenMessageDao {

  override def firstTime(peerId: Long, messageId: Long, conversationMessageId: Long, now: Long): Task[Boolean] =
    sql"""insert into vk_seen_messages(peer_id, message_id, conversation_message_id, seen_at)
          values($peerId, $messageId, $conversationMessageId, $now)
          on conflict do nothing"""
      .update.run.transact(xa).map(_ > 0)

  override def forget(before: Long): Task[Int] =
    sql"delete from vk_seen_messages where seen_at < $before"
      .update.run.transact(xa)
}
