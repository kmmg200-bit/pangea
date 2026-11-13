package pangea.service.equipment

import pangea.engine.SceneContent
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.item.{Bundling, Item, ItemStack}
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** Стопка из сумки без экрана (см. [[Bundling]]). Одно на бота — карточку
  * вещи и стопки — и на Mini App: правила и слова у них одни. Меняет сумку,
  * поэтому зовётся под локом игрока: в боте его держит ход, в приложении — сам
  * запрос. Что сказать игроку, возвращается строкой: бот ставит её над
  * карточкой, приложение — в ответ. */
final class BundleWork(inventoryRepo: InventoryRepository, itemRepository: ItemRepository, content: SceneContent) {

  /** Собрать стопку из вещей, как `item`, — пять одинаковых с конца сумки, как
    * и всё на карточке. Показанная вещь остаётся в сумке, пока таких больше
    * пяти. Собирают где угодно, и в лабиринте. Места стопка не просит, а
    * освобождает — кроме пыли: та места не занимала, а стопке оно нужно.
    * Итог — что сказать; `None` — вещь в стопку не собирается, говорить не о
    * чем. */
  def pack(heroId: HeroId, inv: Inventory, item: Item): Task[Option[String]] =
    if (!Bundling.packable(item)) ZIO.none
    else {
      val taken = ItemStack.key(item).fold(List(item))(k => inv.items.data.filter(i => ItemStack.key(i).contains(k)))
        .takeRight(Bundling.Size)
      Bundling.pack(taken) match {
        // Таких уже меньше пяти: часть успели убрать.
        case None => ZIO.some(content.format("inventory.bundle.short", "n" -> Bundling.Size.toString))
        case Some(draft) if inv.swapped(taken, List(draft)).isLeft =>
          ZIO.some(content.text("inventory.bundle.noRoom"))
        case Some(draft) =>
          for {
            bundle <- itemRepository.persist(heroId, draft)
            _      <- inventoryRepo.swap(heroId, taken, List(bundle)).mapError(e => new Throwable(e.toString))
          } yield Some(content.format("inventory.bundle.packed",
            "name" -> item.displayTitle, "n" -> Bundling.contents(bundle).size.toString))
      }
    }

  /** Распаковать стопку `item` из сумки `inv`: вещи ложатся в сумку, как были.
    * Вместо одного места они займут пять — нужно ещё четыре свободных; пыли
    * место не нужно, но сверх своего предела она не ляжет. Итог — что сказать;
    * `None` — это не стопка, говорить не о чем. */
  def unpack(heroId: HeroId, inv: Inventory, item: Item): Task[Option[String]] =
    Bundling.contents(item) match {
      case Nil => ZIO.none
      case inside @ (first :: _) =>
        inv.swapped(List(item), inside) match {
          case Left(Inventory.SwapRefusal.HoardFull) =>
            ZIO.some(content.format("inventory.bundle.hoardFull",
              "name" -> first.displayTitle, "limit" -> Item.HoardLimit.toString))
          case Left(_) =>
            // Сколько мест не хватает: стопка своё место отдаст, а вещи займут свои.
            val short = inv.occupied - 1L + inside.count(!_.weightless).toLong - inv.maxItems
            ZIO.some(content.format("inventory.bundle.unpackNoRoom", "count" -> short.max(1L).toString))
          case Right(_) =>
            inventoryRepo.swap(heroId, List(item), inside).mapError(e => new Throwable(e.toString)).as(
              Some(content.format("inventory.bundle.unpacked", "name" -> first.displayTitle, "n" -> inside.size.toString)))
        }
    }
}
