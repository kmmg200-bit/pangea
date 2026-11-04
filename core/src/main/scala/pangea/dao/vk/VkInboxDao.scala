package pangea.dao.vk

import doobie.util.transactor.Transactor
import io.circe.Json
import pangea.model.vk.{InboxEvent, InboxKind}
import pangea.service.sender.vk.LongPollCursor
import zio.{Task, ZLayer}

/** Очередь входящих событий ВК (`vk_inbox`) и место Long Poll (`vk_longpoll`).
  * Callback и Long Poll кладут события ([[accept]]), боты их разбирают
  * ([[claim]] → [[done]] / [[bumpAttempts]] / [[failed]]). */
trait VkInboxDao extends LongPollCursor {

  /** Положить событие в очередь. false — такое уже лежит: пришло другим путём
    * или ВК повторил Callback. */
  def accept(dedupKey: String, peerId: Long, kind: InboxKind, source: String, event: Json,
             sentAt: Option[Long], now: Long): Task[Boolean]

  /** Захватить до `until` ждущие события и вернуть их в порядке поступления. У
    * каждой очереди (`peer_id`) берётся только самое раннее ждущее событие:
    * следующее станет доступно, когда это будет обработано, — так ходы игрока
    * идут по порядку и при нескольких ботах. Захваченное другим ботом (FOR
    * UPDATE SKIP LOCKED) или в аренде — пропускается. */
  def claim(now: Long, until: Long, limit: Int): Task[List[InboxEvent]]

  /** Обработано. `event` — что оставить вместо события (без секретов); None —
    * событие как было. */
  def done(id: Long, now: Long, event: Option[Json]): Task[Unit]

  /** Не вышло и после повторов: очередь игрока идёт дальше. */
  def failed(id: Long, now: Long, event: Option[Json]): Task[Unit]

  /** Неудачная попытка: счётчик +1, аренда снимается — повтор на следующем опросе. */
  def bumpAttempts(id: Long): Task[Int]

  /** Сколько событий ждёт (вместе с теми, что в работе). */
  def pending: Task[Long]

  /** Забыть обработанное раньше `before` (мс). */
  def forget(before: Long): Task[Int]
}

object VkInboxDao {
  val live: ZLayer[Transactor[Task], Nothing, VkInboxDao] =
    ZLayer.fromFunction(new VkInboxDaoLive(_))
}
