package pangea.test

import pangea.dao.parcel.ParcelDao
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.model.parcel.Parcel
import pangea.model.user.UserId
import zio.{Task, ZIO}

/** Посылки в памяти: те же правила, что в проде, включая «забрать можно один
  * раз» — второй `claim` той же посылки вернёт `false` — и общий срок у всего,
  * что лежит на почте. */
class TestParcelDao(private var parcels: List[Parcel] = Nil) extends ParcelDao {

  private var nextId: Long = parcels.map(_.id).maxOption.getOrElse(0L) + 1L

  def add(heroId: HeroId, fromName: String, item: Item, now: Long): Task[Parcel] =
    ZIO.succeed {
      val parcel = Parcel(nextId, heroId, fromName, item, now)
      nextId += 1L
      parcels = parcels :+ parcel
      parcel
    }

  def list(heroId: HeroId, limit: Long): Task[List[Parcel]] =
    ZIO.succeed(parcels.filter(_.heroId == heroId).sortBy(_.id).take(limit.toInt))

  def count(heroId: HeroId): Task[Long] = ZIO.succeed(parcels.count(_.heroId == heroId).toLong)

  def claim(id: Long, heroId: HeroId): Task[Boolean] =
    ZIO.succeed {
      val found = parcels.exists(p => p.id == id && p.heroId == heroId)
      if (found) parcels = parcels.filterNot(_.id == id)
      found
    }

  def toMail(heroId: HeroId, ids: List[Long], now: Long): Task[Boolean] =
    ZIO.succeed {
      val since = parcels.filter(_.heroId == heroId).flatMap(_.mailedAt).minOption
      val moved = parcels.filter(p => p.heroId == heroId && p.mailedAt.isEmpty && ids.contains(p.id)).map(_.id)
      parcels = parcels.map(p => if (moved.contains(p.id)) p.copy(mailedAt = Some(since.getOrElse(now))) else p)
      since.isEmpty && moved.nonEmpty
    }

  def burn(heroId: HeroId, before: Long): Task[List[Parcel]] =
    ZIO.succeed {
      val mailed = parcels.filter(p => p.heroId == heroId && p.mailedAt.isDefined)
      if (!mailed.flatMap(_.mailedAt).minOption.exists(_ <= before)) Nil
      else { parcels = parcels.filterNot(mailed.contains); mailed }
    }

  def inTransit(heroId: HeroId, ids: List[Long]): Task[List[Parcel]] =
    ZIO.succeed(parcels.filter(p => p.heroId == heroId && p.mailedAt.isEmpty && ids.contains(p.id)).sortBy(_.id))

  /** Хозяин героя — по номеру: в тестах у героя N игрок N. */
  def expired(before: Long): Task[List[(HeroId, UserId)]] =
    ZIO.succeed(parcels.groupBy(_.heroId).toList.collect {
      case (heroId, mine) if mine.flatMap(_.mailedAt).minOption.exists(_ <= before) => heroId -> UserId(heroId.value)
    }.sortBy(_._1.value))

  def stranded(before: Long): Task[List[(HeroId, UserId, Long)]] =
    ZIO.succeed(parcels.filter(p => p.mailedAt.isEmpty && p.sentAt <= before).sortBy(_.id)
      .map(p => (p.heroId, UserId(p.heroId.value), p.id)))

  def snapshot: List[Parcel] = parcels
}

object TestParcelDao {
  def empty: TestParcelDao = new TestParcelDao()
}
