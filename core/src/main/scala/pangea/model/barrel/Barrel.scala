package pangea.model.barrel

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.Item

/** Личная неприметная бочка игрока в Портовом квартале: хранилище до 100 предметов
 *  и до 10 000 серебра, независимое от инвентаря и кошелька героя.
 *
 *  `thieves` — воры у бочки («Вскрытая бочка», см. `BarrelThievesQuest`): когда
 *  последний раз бросали жребий на кражу и что украдено, пока банда не найдена.
 */
case class Barrel(
  id: Long,
  heroId: HeroId,
  items: Items,
  silver: Long,
  thieves: Option[BarrelThieves] = None
) {
  def addItem(item: Item): Barrel =
    copy(items = items.copy(data = items.data.appended(item)))

  def withItems(items: List[Item]): Barrel = copy(items = Items(items))

  /** Сколько в бочке лежит того, что занимает место: пыль и малые руны не в счёт. */
  def occupied: Long = items.data.count(!_.weightless).toLong

  def freeSlots: Long = (Barrel.MaxItems - occupied).max(0L)

  /** Сколько такого же невесомого добра уже в бочке — у него свой предел вместо места. */
  def hoardCount(key: String): Int = items.data.count(_.hoardKey.contains(key))

  def hasRoomForHoard(item: Item): Boolean =
    item.hoardKey.forall(k => hoardCount(k) < Item.HoardLimit)

  def freeSilverSpace: Long = (Barrel.MaxSilver - silver).max(0L)

  /** Бочку вскрыли, и украденное ещё у воров: класть в неё нельзя. */
  def robbed: Boolean = thieves.exists(_.stolen.isDefined)

  /** Когда последний раз бросали жребий на кражу (0 — ни разу). */
  def thievesRolledAt: Long = thieves.fold(0L)(_.rolledAt)
}

object Barrel {
  val MaxItems: Long  = 10L
  val MaxSilver: Long = 10000L

  def empty(heroId: HeroId): Barrel =
    Barrel(id = 0L, heroId = heroId, items = Items(Nil), silver = 0L)
}

/** Воры у бочки: когда бросали жребий и что унесли. Лежит в `barrels.thieves`
  * (JSONB, NULL — ни броска, ни кражи). */
final case class BarrelThieves(rolledAt: Long = 0L, stolen: Option[StolenGoods] = None)

/** Что унесли из бочки: вещи как были и серебро — вернутся, когда банду найдут. */
final case class StolenGoods(items: List[Item], silver: Long)

object StolenGoods {
  implicit val encoder: Encoder[StolenGoods] = (s: StolenGoods) =>
    Json.obj("items" -> s.items.asJson, "silver" -> s.silver.asJson)

  implicit val decoder: Decoder[StolenGoods] = (c: HCursor) =>
    for {
      items  <- c.getOrElse[List[Item]]("items")(Nil)
      silver <- c.getOrElse[Long]("silver")(0L)
    } yield StolenGoods(items, silver)
}

object BarrelThieves {
  implicit val encoder: Encoder[BarrelThieves] = (t: BarrelThieves) =>
    Json.obj("rolledAt" -> t.rolledAt.asJson, "stolen" -> t.stolen.asJson)

  /** Декодер рукописный, каждое поле с запасным значением: старые записи и
    * будущие поля не должны ронять чтение бочки. */
  implicit val decoder: Decoder[BarrelThieves] = (c: HCursor) =>
    for {
      rolledAt <- c.getOrElse[Long]("rolledAt")(0L)
      stolen   <- c.getOrElse[Option[StolenGoods]]("stolen")(None)
    } yield BarrelThieves(rolledAt, stolen)

  implicit val meta: Meta[BarrelThieves] = new Meta(pgDecoderGet, pgEncoderPut)
}
