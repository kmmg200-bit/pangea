package pangea.service.miniapp

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.Hero
import pangea.model.inventory.Inventory
import pangea.model.item.{Item, ItemStack, ItemType, Rarity}
import pangea.service.state.states.EquipmentState

/** Что видит Mini App: те же вещи, что и бот, но данными, а не экраном.
  * Описание вещи (`text`) — тот же текст, что в карточке бота
  * ([[pangea.service.state.states.InventoryState.cardText]], его передаёт `describe`): правила показа
  * остаются в одном месте, а приложение раскладывает вещи, фильтрует и
  * сортирует их. */
object MiniAppView {

  /** Вещь. `kind` — тип ([[ItemType]]), `rarity` — редкость ([[Rarity]]),
    * `rarityRank` — её место от серой к оранжевой (для сортировки),
    * `count` — сколько таких лежит одной стопкой. */
  final case class ItemView(
    id:         Long,
    title:      String,
    kind:       String,
    rarity:     String,
    rarityRank: Int,
    lvl:        Long,
    count:      Int,
    text:       String
  )

  /** Слот снаряжения: что надето; `busy` — пустой, но занят (левая рука под
    * двуручным: само оружие — в правой). */
  final case class SlotView(name: String, item: Option[ItemView], busy: Option[String])

  final case class HeroView(lvl: Long, silver: Long, doubloons: Long)

  final case class InventoryView(
    hero:      HeroView,
    capacity:  Long,
    occupied:  Long,
    items:     List[ItemView],
    equipment: List[SlotView]
  )

  /** Лот. `currency` — `silver` или `doubloons`; `unsold` — неделя вышла, лот
    * ждёт хозяина. */
  final case class LotView(
    id:        Long,
    item:      ItemView,
    price:     Long,
    currency:  String,
    hoursLeft: Long,
    mine:      Boolean,
    unsold:    Boolean
  )

  /** Витрина и свои лоты (свои — с непроданными). */
  final case class AuctionView(hero: HeroView, lots: List[LotView], mine: List[LotView])

  implicit val itemEncoder:      Encoder[ItemView]      = deriveEncoder
  implicit val slotEncoder:      Encoder[SlotView]      = deriveEncoder
  implicit val heroEncoder:      Encoder[HeroView]      = deriveEncoder
  implicit val inventoryEncoder: Encoder[InventoryView] = deriveEncoder
  implicit val lotEncoder:       Encoder[LotView]       = deriveEncoder
  implicit val auctionEncoder:   Encoder[AuctionView]   = deriveEncoder

  def hero(h: Hero): HeroView = HeroView(h.lvl, h.silver, h.doubloons)

  /** Сумка — стопками, как в боте, — и всё надетое. */
  def inventory(h: Hero, inv: Inventory, describe: Item => String): InventoryView =
    InventoryView(
      hero      = hero(h),
      capacity  = inv.maxItems,
      occupied  = inv.occupied,
      items     = ItemStack.grouped(inv.items.data).map { case (item, count) =>
                    view(item, count, describe(item))
                  },
      equipment = EquipmentState.slots.map { slot =>
                    val worn = slot.get(h.equipment)
                    SlotView(
                      name = slot.name,
                      item = Option.when(worn.itemType != ItemType.NoItem)(view(worn, 1, EquipmentState.itemStats(worn))),
                      busy = Option.when(slot.busy(h.equipment))(EquipmentState.LeftBusyText)
                    )
                  }
    )

  /** Витрина и свои лоты. Описание вещи в лоте — со сравнением с надетым на
    * герое, как в карточке бота. */
  def auction(h: Hero, onSale: List[AuctionLot], mine: List[AuctionLot], now: Long,
              describe: Item => String): AuctionView = {
    def lot(l: AuctionLot): LotView =
      LotView(
        id        = l.id,
        item      = view(l.item, 1, describe(l.item)),
        price     = l.price,
        currency  = currency(l.currency),
        hoursLeft = l.hoursLeft(now),
        mine      = l.sellerId == h.id,
        unsold    = l.unsold(now)
      )
    AuctionView(hero(h), onSale.map(lot), mine.map(lot))
  }

  private def view(item: Item, count: Int, text: String): ItemView =
    ItemView(
      id         = item.id,
      title      = item.displayTitle,
      kind       = item.itemType.entryName,
      rarity     = item.rarity.entryName,
      rarityRank = Rarity.order(item.rarity),
      lvl        = item.lvl,
      count      = count,
      text       = text
    )

  private def currency(c: AuctionCurrency): String = c match {
    case AuctionCurrency.Silver    => "silver"
    case AuctionCurrency.Doubloons => "doubloons"
  }
}
