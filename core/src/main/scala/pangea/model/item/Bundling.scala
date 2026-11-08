package pangea.model.item

/** Стопка: пять одинаковых вещей на одном месте.
  *
  * Внутри лежат сами вещи — с номерами и всем остальным, — и распаковка
  * возвращает их в сумку как были. Собирают только неразличимое между собой
  * (см. [[ItemStack.key]]): камни, материалы (и пыль), отвары, рунные камни.
  * Трофеи и малые руны в стопку не собираются, стопка в стопку — тоже.
  *
  * Пока вещи в стопке, их нет ни для рецептов, ни для заданий, ни для
  * Ришелье: стопку хранят, передают, выставляют на аукцион — и распаковывают. */
object Bundling {

  /** Сколько вещей в стопке. */
  val Size: Int = 5

  /** Собирается ли вещь в стопку с такими же. */
  def packable(item: Item): Boolean =
    ItemStack.key(item).isDefined && !item.isBundle && !item.isSmallRune && item.itemType != ItemType.Trophy

  /** Стопка из `items` — ровно [[Size]] одинаковых вещей, которые собираются.
    * Номер у неё пока 0: его даст база (см. `ItemRepository.persist`). Уровень
    * и редкость — как у того, что внутри: по ним её ищут на аукционе. */
  def pack(items: List[Item]): Option[Item] = items match {
    case first :: _ if items.sizeIs == Size && packable(first) &&
                       items.forall(i => ItemStack.key(i) == ItemStack.key(first)) =>
      Some(Item(
        id       = 0L,
        name     = s"Стопка: ${first.name} ×$Size",
        lvl      = first.lvl,
        rarity   = first.rarity,
        itemType = ItemType.Bundle,
        attack   = 0L,
        accuracy = 0L,
        energy   = 0L,
        armor    = 0L,
        defence  = 0L,
        evasion  = 0L,
        details  = ItemDetails.Bundle(items)))
    case _ => None
  }

  /** Что лежит в стопке; у прочих вещей — ничего. */
  def contents(item: Item): List[Item] = item.bundle.fold(List.empty[Item])(_.items)
}
