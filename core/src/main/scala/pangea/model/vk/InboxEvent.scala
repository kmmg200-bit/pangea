package pangea.model.vk

import enumeratum.{DoobieEnum, Enum, EnumEntry}
import io.circe.Json

/** Событие ВК в очереди `vk_inbox` (см. [[pangea.dao.vk.VkInboxDao]]): каким
  * пришло — целиком, — чьё и когда. `source` — callback или longpoll: каким
  * путём пришло первым. `sentAt` — когда его отправил игрок (мс), если ВК это
  * сообщил; `receivedAt` — когда оно легло в очередь. */
final case class InboxEvent(
  id:         Long,
  peerId:     Long,
  kind:       InboxKind,
  source:     String,
  event:      Json,
  sentAt:     Option[Long],
  receivedAt: Long,
  attempts:   Int
)

/** Вид события: сообщение игрока в личке, сообщение из беседы, событие группы
  * (пост, лайк, комментарий — для конкурса активности). Значения лежат в базе:
  * не переименовывать. */
sealed trait InboxKind extends EnumEntry with EnumEntry.Lowercase
object InboxKind extends Enum[InboxKind] with DoobieEnum[InboxKind] {
  val values = findValues
  case object Message extends InboxKind
  case object Chat    extends InboxKind
  case object Group   extends InboxKind
}

/** `Pending` — ждёт или в работе (тогда у него аренда), `Done` — обработано,
  * `Failed` — не вышло и после повторов. Значения лежат в базе: не переименовывать. */
sealed trait InboxStatus extends EnumEntry
object InboxStatus extends Enum[InboxStatus] with DoobieEnum[InboxStatus] {
  val values = findValues
  case object Pending extends InboxStatus
  case object Done    extends InboxStatus
  case object Failed  extends InboxStatus
}

object InboxEvent {

  /** Ключ от повторов сообщения: одно сообщение приходит и по Callback, и по
    * Long Poll, а ВК ещё и повторяет Callback. В беседах ВК шлёт `id = 0`, и
    * сообщение различает `cmid`. */
  def messageKey(peerId: Long, messageId: Long, conversationMessageId: Long): String =
    s"m:$peerId:$messageId:$conversationMessageId"

  /** Ключ события группы: ВК метит каждое событие своим `event_id`. Нет его —
    * ключом становится само событие (повтор пришёл бы тем же текстом). */
  def groupKey(event: Json): String =
    event.hcursor.get[String]("event_id").toOption.filter(_.nonEmpty) match {
      case Some(id) => s"e:$id"
      case None     => s"e:sha256:${sha256(event.noSpaces)}"
    }

  /** Событие с текстом сообщения, пропущенным через `f`. У событий без
    * сообщения (события группы) — как было. */
  def mapText(event: Json)(f: String => String): Json =
    event.hcursor.downField("object").downField("message").downField("text")
      .withFocus(_.mapString(f)).top.getOrElse(event)

  private def sha256(s: String): String =
    java.security.MessageDigest.getInstance("SHA-256")
      .digest(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      .map("%02x".format(_)).mkString
}
