package pangea.dao.parcel

import cats.data.NonEmptyList
import doobie.Fragments
import doobie.implicits._
import doobie.util.transactor
import doobie.util.transactor.Transactor
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.model.item.Item.meta
import pangea.model.parcel.Parcel
import pangea.model.user.UserId
import zio.interop.catz._
import zio.{Task, ZIO, ZLayer}

/** Посылки от игрока к игроку: лежат тут, пока получатель не заберёт их —
 *  сперва в банковскую ячейку сами, а что не влезло, руками с почты. */
trait ParcelDao {
  def add(heroId: HeroId, fromName: String, item: Item, now: Long): Task[Parcel]
  def list(heroId: HeroId, limit: Long): Task[List[Parcel]]
  def count(heroId: HeroId): Task[Long]
  /** Забрать посылку: `false` — её уже забрали. */
  def claim(id: Long, heroId: HeroId): Task[Boolean]
  /** Положить посылки из пути на почту. Срок у почты общий: если на ней уже
   *  что-то лежит, новые получают то же время, иначе — `now`. `true` — почта
   *  была пуста, и срок пошёл с этих посылок. */
  def toMail(heroId: HeroId, ids: List[Long], now: Long): Task[Boolean]
  /** Сжечь всё, что лежит на почте, если первая вещь легла туда не позже
   *  `before`. Возвращает сгоревшее; посылок в пути это не касается. */
  def burn(heroId: HeroId, before: Long): Task[List[Parcel]]
  /** Какие из этих посылок ещё в пути: не разложены по ячейке и почте. */
  def inTransit(heroId: HeroId, ids: List[Long]): Task[List[Parcel]]
  /** Чьи почты пора сжечь: первая вещь легла туда не позже `before`. */
  def expired(before: Long): Task[List[(HeroId, UserId)]]
  /** Посылки, застрявшие в пути: отправлены не позже `before`, а разложить их
   *  сразу не вышло. Герой, его хозяин и номер посылки. */
  def stranded(before: Long): Task[List[(HeroId, UserId, Long)]]
}

class ParcelDaoLive(xa: Transactor[Task]) extends ParcelDao {

  private val columns = fr"id, hero_id, from_name, item, sent_at, mailed_at"

  override def add(heroId: HeroId, fromName: String, item: Item, now: Long): Task[Parcel] =
    sql"""insert into parcels(hero_id, from_name, item, sent_at)
          values(${heroId.value}, $fromName, $item, $now)"""
      .update.withUniqueGeneratedKeys[Long]("id").transact(xa)
      .map(id => Parcel(id, heroId, fromName, item, now))

  override def list(heroId: HeroId, limit: Long): Task[List[Parcel]] =
    (fr"select" ++ columns ++ fr"from parcels where hero_id = ${heroId.value} order by id limit $limit")
      .query[Parcel].to[List].transact(xa)

  override def count(heroId: HeroId): Task[Long] =
    sql"select count(*) from parcels where hero_id = ${heroId.value}"
      .query[Long].unique.transact(xa)

  // Забираем удалением: одна и та же посылка не достанется дважды.
  override def claim(id: Long, heroId: HeroId): Task[Boolean] =
    sql"delete from parcels where id = $id and hero_id = ${heroId.value}"
      .update.run.transact(xa).map(_ == 1)

  override def toMail(heroId: HeroId, ids: List[Long], now: Long): Task[Boolean] =
    NonEmptyList.fromList(ids) match {
      case None => ZIO.succeed(false)
      case Some(nel) =>
        (for {
          since <- sql"select min(mailed_at) from parcels where hero_id = ${heroId.value}"
                     .query[Option[Long]].unique
          moved <- (fr"update parcels set mailed_at = ${since.getOrElse(now)}" ++
                     fr"where hero_id = ${heroId.value} and mailed_at is null and" ++ Fragments.in(fr"id", nel))
                     .update.run
        } yield since.isEmpty && moved > 0).transact(xa)
    }

  // Сгорает почта целиком: срок у всех вещей на ней общий — по первой легшей.
  override def burn(heroId: HeroId, before: Long): Task[List[Parcel]] =
    (fr"delete from parcels where hero_id = ${heroId.value} and mailed_at is not null" ++
      fr"and (select min(mailed_at) from parcels where hero_id = ${heroId.value}) <= $before" ++
      fr"returning" ++ columns)
      .query[Parcel].to[List].transact(xa)

  override def inTransit(heroId: HeroId, ids: List[Long]): Task[List[Parcel]] =
    NonEmptyList.fromList(ids) match {
      case None => ZIO.succeed(Nil)
      case Some(nel) =>
        (fr"select" ++ columns ++ fr"from parcels where hero_id = ${heroId.value} and mailed_at is null and" ++
          Fragments.in(fr"id", nel) ++ fr"order by id")
          .query[Parcel].to[List].transact(xa)
    }

  override def expired(before: Long): Task[List[(HeroId, UserId)]] =
    sql"""select p.hero_id, h.user_id from parcels p join heroes h on h.id = p.hero_id
          where p.mailed_at is not null
          group by p.hero_id, h.user_id
          having min(p.mailed_at) <= $before"""
      .query[(HeroId, UserId)].to[List].transact(xa)

  override def stranded(before: Long): Task[List[(HeroId, UserId, Long)]] =
    sql"""select p.hero_id, h.user_id, p.id from parcels p join heroes h on h.id = p.hero_id
          where p.mailed_at is null and p.sent_at <= $before
          order by p.id"""
      .query[(HeroId, UserId, Long)].to[List].transact(xa)
}

object ParcelDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, ParcelDao] =
    ZLayer.fromFunction(new ParcelDaoLive(_))
}
