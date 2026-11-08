package pangea.repository.inventory

sealed trait InventoryRepoError

object InventoryRepoError {
  case object NoMorePlaceForItems  extends InventoryRepoError
  /** Горстей этой пыли уже [[pangea.model.item.MaterialKind.MaxDustPerKind]] —
    * места она не занимает, но и сверх предела не ложится. */
  case object DustLimitReached     extends InventoryRepoError
  case object CantFindInventory    extends InventoryRepoError
  case object CantUpdateInventory  extends InventoryRepoError
  case object CantFindItemToRemove extends InventoryRepoError

  /** Почему не прошёл обмен вещей в сумке (см. `Inventory.swapped`). */
  def of(refusal: pangea.model.inventory.Inventory.SwapRefusal): InventoryRepoError = refusal match {
    case pangea.model.inventory.Inventory.SwapRefusal.Missing   => CantFindItemToRemove
    case pangea.model.inventory.Inventory.SwapRefusal.NoRoom    => NoMorePlaceForItems
    case pangea.model.inventory.Inventory.SwapRefusal.HoardFull => DustLimitReached
  }
}
