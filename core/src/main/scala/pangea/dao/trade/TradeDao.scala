package pangea.dao.trade

import doobie.implicits._
import doobie.util.transactor.Transactor
import pangea.model.trade.{TradeOffer, TradeStatus}
import pangea.model.user.UserId
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Предложения обмена (`trade_offers`). */
trait TradeDao {

  /** Новое предложение; возвращает его номер. */
  def insert(offer: TradeOffer): Task[Long]

  def get(id: Long): Task[Option[TradeOffer]]

  /** Открытые предложения игрока — и его, и ему: свежие первыми. */
  def openOf(user: UserId): Task[List[TradeOffer]]

  /** Сколько открытых предложений игрок сделал сам. */
  def countOpenFrom(user: UserId): Task[Long]

  /** Записать новое состояние — только если предложение ещё открыто и с тех
    * пор, как его прочли (`offer.version`), никто его не менял. `false` —
    * опоздали, ничего не записано. */
  def update(offer: TradeOffer): Task[Boolean]

  /** Открытые предложения, срок которых вышел к `now`. */
  def expiring(now: Long): Task[List[TradeOffer]]
}

class TradeDaoLive(xa: Transactor[Task]) extends TradeDao {

  // Литерал статуса — базовым типом: doobie ищет Meta[TradeStatus], а не для
  // singleton-подтипа.
  private val Open: TradeStatus = TradeStatus.Open

  private val columns =
    fr"id, from_user, to_user, from_side, to_side, turn, status, created_at, updated_at, expires_at, done_at, version"

  override def insert(o: TradeOffer): Task[Long] =
    sql"""insert into trade_offers(from_user, to_user, from_side, to_side, turn, status, created_at, updated_at,
                                   expires_at, done_at)
          values(${o.from}, ${o.to}, ${o.fromSide}, ${o.toSide}, ${o.turn}, ${o.status}, ${o.createdAt},
                 ${o.updatedAt}, ${o.expiresAt}, ${o.doneAt})"""
      .update.withUniqueGeneratedKeys[Long]("id").transact(xa)

  override def get(id: Long): Task[Option[TradeOffer]] =
    (fr"select" ++ columns ++ fr"from trade_offers where id = $id").query[TradeOffer].option.transact(xa)

  override def openOf(user: UserId): Task[List[TradeOffer]] =
    (fr"select" ++ columns ++
      fr"from trade_offers where status = $Open and (from_user = $user or to_user = $user) order by updated_at desc")
      .query[TradeOffer].to[List].transact(xa)

  override def countOpenFrom(user: UserId): Task[Long] =
    sql"select count(*) from trade_offers where status = $Open and from_user = $user".query[Long].unique.transact(xa)

  override def update(o: TradeOffer): Task[Boolean] =
    sql"""update trade_offers
          set from_side = ${o.fromSide}, to_side = ${o.toSide}, turn = ${o.turn}, status = ${o.status},
              updated_at = ${o.updatedAt}, expires_at = ${o.expiresAt}, done_at = ${o.doneAt}, version = version + 1
          where id = ${o.id} and status = $Open and version = ${o.version}"""
      .update.run.transact(xa).map(_ > 0)

  override def expiring(now: Long): Task[List[TradeOffer]] =
    (fr"select" ++ columns ++ fr"from trade_offers where status = $Open and expires_at <= $now order by id")
      .query[TradeOffer].to[List].transact(xa)
}

object TradeDao {
  val live: ZLayer[Transactor[Task], Nothing, TradeDao] = ZLayer.fromFunction(new TradeDaoLive(_))
}
