package pangea.model.inventory

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.Item

case class Inventory(
  id: Long,
  heroId: HeroId,
  maxItems: Long,
  items: Items
) {
  def addItem(item: Item): Inventory =
    copy(items = items.copy(data = items.data.appended(item)))

  def withItems(items: List[Item]): Inventory = copy(items = Items(items))

  /** Сколько слотов занято: сюжетные предметы, пыль и малые руны места не занимают. */
  def occupied: Long = items.data.count(i => !i.isQuestItem && !i.weightless).toLong

  /** Сколько такого же невесомого добра уже лежит — предел у него вместо места. */
  def hoardCount(key: String): Int = items.data.count(_.hoardKey.contains(key))

  /** Влезет ли ещё одна такая невесомая вещь (пыль, малая руна). */
  def hasRoomForHoard(item: Item): Boolean =
    item.hoardKey.forall(k => hoardCount(k) < Item.HoardLimit)

  /** Свободных слотов в сумке (не уходит ниже нуля). */
  def freeSlots: Long = (maxItems - occupied).max(0L)

  /** Влезет ли ещё `count` обычных предметов. */
  def hasRoomFor(count: Long): Boolean = occupied + count <= maxItems

  /** Убрать `remove` (каждую вещь — один раз, по полному совпадению) и положить
    * `add` — одним изменением: так стопку собирают и распаковывают целиком или
    * никак (см. [[pangea.model.item.Bundling]]). Место проверяется только на
    * прирост: обмен, после которого занято не больше прежнего, пройдёт и в
    * переполненной сумке. Невесомое добро, которое кладём, не должно перевалить
    * за свой предел. */
  def swapped(remove: List[Item], add: List[Item]): Either[Inventory.SwapRefusal, Inventory] =
    remove.foldLeft(Option(items.data)) { (left, r) =>
      left.flatMap(data => Option(data.indexOf(r)).filter(_ >= 0).map(i => data.patch(i, Nil, 1)))
    } match {
      case None => Left(Inventory.SwapRefusal.Missing)
      case Some(rest) =>
        val next = withItems(rest ++ add)
        if (next.occupied > maxItems && next.occupied > occupied) Left(Inventory.SwapRefusal.NoRoom)
        else if (add.flatMap(_.hoardKey).exists(k => next.hoardCount(k) > Item.HoardLimit))
          Left(Inventory.SwapRefusal.HoardFull)
        else Right(next)
    }
}

object Inventory {
  /** Вместимость сумки по умолчанию (слотов). */
  val DefaultCapacity: Long = 20L

  /** Почему обмен вещей ([[Inventory.swapped]]) не прошёл. */
  sealed trait SwapRefusal
  object SwapRefusal {
    /** Того, что убираем, в сумке уже нет. */
    case object Missing   extends SwapRefusal
    /** Сумке не хватает мест. */
    case object NoRoom    extends SwapRefusal
    /** Невесомого добра одного вида стало бы больше предела. */
    case object HoardFull extends SwapRefusal
  }

  case class Items(data: List[Item]) {

    /** Без одной вещи с номером `id` — первой по порядку. Номер у каждой вещи
      * свой, пока она сохранена в базу; не сохранённая (−1) могла лечь рядом с
      * такой же, и снять тогда надо одну, а не все разом (так у игрока
      * пропадала вся пыль, когда одну горсть клали в куб). */
    def withoutOne(id: Long): List[Item] =
      data.indexWhere(_.id == id) match {
        case -1 => data
        case i  => data.patch(i, Nil, 1)
      }
  }

  implicit val encoderForItems: Encoder[Items] = deriveEncoder[Items]
  implicit val decoderForItems: Decoder[Items] = deriveDecoder[Items]

  implicit val meta: Meta[Items] = new Meta(pgDecoderGet, pgEncoderPut)
}
