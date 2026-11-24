package pangea.service.equipment

import pangea.engine.SceneContent
import pangea.generator.item.TreasureMapGenerator
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.item.{Item, ItemDetails, ItemType, MapZone}
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** Две половинки карты клада одной зоны — одна целая карта. Правила и слова
  * общие с карточкой в боте ([[pangea.service.state.states.InventoryState]]). */
final class MapWork(inventoryRepo: InventoryRepository, itemRepository: ItemRepository, content: SceneContent) {

  def zoneOf(item: Item): Option[MapZone] = item.details match {
    case ItemDetails.TreasureMap(zone) => Some(zone)
    case _                             => None
  }

  /** Сложить половинку `halfId` с парной из сумки `inv`. Итог — что сказать;
    * `None` — не половинка, пары нет или сумка полна. */
  def combine(heroId: HeroId, inv: Inventory, halfId: Long): Task[Option[String]] =
    inv.items.data.find(_.id == halfId) match {
      case Some(first) if first.itemType == ItemType.TreasureMapHalf =>
        val firstZone = zoneOf(first)
        val second = inv.items.data.find(i =>
          i.id != first.id && i.itemType == ItemType.TreasureMapHalf && zoneOf(i) == firstZone)
        (firstZone, second) match {
          case (Some(zone), Some(other)) =>
            val full = TreasureMapGenerator.full(zone)
            for {
              _         <- inventoryRepo.removeItems(Set(first.id, other.id), heroId).mapError(e => new Throwable(e.toString))
              persisted <- itemRepository.persist(heroId, full)
              _         <- inventoryRepo.addItem(heroId, persisted).mapError(e => new Throwable(e.toString))
            } yield Some(content.format("inventory.combined", "name" -> full.name))
          case _ =>
            val name = firstZone.map(_.treasureName).getOrElse("")
            ZIO.some(content.format("inventory.combineNeedSecond", "name" -> name))
        }
      case _ => ZIO.none
    }
}