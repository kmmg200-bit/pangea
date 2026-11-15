package pangea.model.trade

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import enumeratum.{DoobieEnum, Enum, EnumEntry}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.item.Item
import pangea.model.user.UserId

/** Одна сторона обмена — заклад: вещи целиком, какими их положили, серебро и
  * дублоны, комиссия сверх них (при обмене она сгорит, при отмене вернётся) и
  * записка — чего хочется взамен. */
final case class TradeSide(
  items:        List[Item],
  silver:       Long,
  silverFee:    Long,
  doubloons:    Long,
  doubloonsFee: Long,
  note:         Option[String]
) {

  /** Ничего не положено — записка не в счёт. */
  def isEmpty: Boolean = items.isEmpty && silver == 0L && doubloons == 0L
}

object TradeSide {

  val empty: TradeSide = TradeSide(Nil, 0L, 0L, 0L, 0L, None)

  implicit val encoder: Encoder[TradeSide] = (s: TradeSide) => Json.obj(
    "items"        -> s.items.asJson,
    "silver"       -> s.silver.asJson,
    "silverFee"    -> s.silverFee.asJson,
    "doubloons"    -> s.doubloons.asJson,
    "doubloonsFee" -> s.doubloonsFee.asJson,
    "note"         -> s.note.asJson)

  // Значения по умолчанию — в декодере: поле, добавленное позже, прочитается и
  // из записей, где его нет.
  implicit val decoder: Decoder[TradeSide] = (c: HCursor) =>
    for {
      items        <- c.getOrElse[List[Item]]("items")(Nil)
      silver       <- c.getOrElse[Long]("silver")(0L)
      silverFee    <- c.getOrElse[Long]("silverFee")(0L)
      doubloons    <- c.getOrElse[Long]("doubloons")(0L)
      doubloonsFee <- c.getOrElse[Long]("doubloonsFee")(0L)
      note         <- c.getOrElse[Option[String]]("note")(None)
    } yield TradeSide(items, silver, silverFee, doubloons, doubloonsFee, note)

  implicit val meta: Meta[TradeSide] = new Meta(pgDecoderGet, pgEncoderPut)
}

/** Чем кончилось предложение. Значения лежат в базе: не переименовывать. */
sealed trait TradeStatus extends EnumEntry

object TradeStatus extends Enum[TradeStatus] with DoobieEnum[TradeStatus] {
  val values = findValues

  /** Ждёт ответа: согласиться, подтвердить или поменять свою сторону может
    * тот, чей ход. */
  case object Open      extends TradeStatus
  case object Accepted  extends TradeStatus
  /** Отклонил тот, кому предложили. */
  case object Declined  extends TradeStatus
  /** Отозвал тот, кто предложил. */
  case object Cancelled extends TradeStatus
  case object Expired   extends TradeStatus
}

/** Предложение обмена: кто (`from`) кому (`to`), что лежит в закладе у каждого,
  * чей ход (`turn`), кто уже согласился с нынешними условиями (`agreedBy`) и
  * чем кончилось. `version` растёт с каждой записью: запись по устаревшей
  * версии не пройдёт.
  *
  * Обмен — в три шага и больше: правка стороны — предложение, а не согласие.
  * После неё ход у второго: он соглашается или отвечает своим. Согласился —
  * ход у автора последних условий, и подтверждает обмен он. Любая правка
  * снимает согласие. */
final case class TradeOffer(
  id:        Long,
  from:      UserId,
  to:        UserId,
  fromSide:  TradeSide,
  toSide:    TradeSide,
  turn:      UserId,
  agreedBy:  Option[UserId],
  status:    TradeStatus,
  createdAt: Long,
  updatedAt: Long,
  expiresAt: Long,
  doneAt:    Option[Long],
  version:   Int
) {

  def isParty(user: UserId): Boolean = user == from || user == to

  def partnerOf(user: UserId): UserId = if (user == from) to else from

  def sideOf(user: UserId): TradeSide = if (user == from) fromSide else toSide

  def withSide(user: UserId, side: TradeSide): TradeOffer =
    if (user == from) copy(fromSide = side) else copy(toSide = side)

  /** Партнёр уже согласился с нынешними условиями — игроку осталось
    * подтвердить. */
  def partnerAgreed(user: UserId): Boolean = agreedBy.contains(partnerOf(user))

  /** Игрок сам согласился и ждёт подтверждения партнёра. */
  def agreed(user: UserId): Boolean = agreedBy.contains(user)
}
