package pangea.service.state

import pangea.model.hero.Hero
import pangea.model.item.Item
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
        landed <- parcels.arrive(hero.id, sent.map(_.id), now)
      } yield parcels.report(landed)
}
