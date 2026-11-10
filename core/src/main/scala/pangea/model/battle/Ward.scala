package pangea.model.battle

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** От чего бережёт выпитый защитный отвар — до конца боя: яд, огонь, холод или
  * кровотечение. Что уже висело на герое, отвар снимает, а новое того же толка
  * на него не ложится. Лежит в бою строкой (`entryName`) — варианты не
  * переименовывать. */
sealed trait Ward extends EnumEntry

object Ward extends Enum[Ward] {
  case object Poison extends Ward
  case object Fire   extends Ward
  case object Cold   extends Ward
  case object Bleed  extends Ward

  val values: IndexedSeq[Ward] = findValues

  implicit val encoder: Encoder[Ward] = (w: Ward) => w.entryName.asJson
  implicit val decoder: Decoder[Ward] = (c: HCursor) =>
    c.as[String].flatMap(n => withNameOption(n)
      .toRight(io.circe.DecodingFailure(s"Неизвестная защита '$n'", c.history)))
}
