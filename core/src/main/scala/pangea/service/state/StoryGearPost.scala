package pangea.service.state

import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.parcel.Parcels
import zio.{Task, ZIO}

/** Сюжетную экипировку не теряют (см. `Item.isStoryGear`): что не влезло в
  * сумку, уходит посылкой — в банковскую ячейку, а нет там места — на почту
  * Торгового дома, где сюжетное не сгорает. */
object StoryGearPost {

  /** Отправить вещи (уже с номерами из базы) и сразу разложить: это ход
    * самого героя, его лок уже взят. Строка о том, куда что легло; вещей нет —
    * ничего. */
  def send(parcels: Parcels, hero: Hero, fromName: String, items: List[Item], now: Long): Task[Option[String]] =
    if (items.isEmpty) ZIO.none
    else
      for {
        sent   <- ZIO.foreach(items)(parcels.send(hero.id, fromName, _, now))
        landed <- parcels.arrive(hero.id, hero.userId, sent.map(_.id), now)
      } yield parcels.report(landed)

  /** Выдать сюжетную экипировку (вещи ещё без номеров): сохранить, положить в
    * сумку, а что не влезло — посылкой ([[send]]). Ответ — что легло в сумку, и
    * строка о посылках. Без почты (в тестах) не влезшее не выдаётся вовсе. */
  def give(inventoryRepo: InventoryRepository, itemRepo: ItemRepository, parcels: Option[Parcels],
           hero: Hero, fromName: String, items: List[Item], now: Long): Task[(List[Item], Option[String])] =
    for {
      saved  <- ZIO.foreach(items)(itemRepo.persist(hero.id, _))
      placed <- ZIO.foreach(saved)(item =>
                  inventoryRepo.addItem(hero.id, item).as(true).catchAll(_ => ZIO.succeed(false)).map(item -> _))
      bagged  = placed.collect { case (item, true) => item }
      left    = placed.collect { case (item, false) => item }
      line   <- parcels.fold[Task[Option[String]]](ZIO.none)(send(_, hero, fromName, left, now))
    } yield (bagged, line)
}
