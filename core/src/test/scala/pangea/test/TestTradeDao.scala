package pangea.test

import pangea.dao.trade.TradeDao
import pangea.model.trade.{TradeOffer, TradeStatus}
import pangea.model.user.UserId
import zio.{Task, ZIO}

/** Предложения обмена в памяти — с той же записью по версии, что и в базе. */
final class TestTradeDao extends TradeDao {
  private var offers: Map[Long, TradeOffer] = Map.empty
  private var next: Long                    = 1L
  private var failNextUpdate: Boolean       = false

  def insert(o: TradeOffer): Task[Long] =
    ZIO.succeed { val id = next; next += 1; offers += id -> o.copy(id = id, version = 0); id }

  def get(id: Long): Task[Option[TradeOffer]] = ZIO.succeed(offers.get(id))

  def openOf(user: UserId): Task[List[TradeOffer]] =
    ZIO.succeed(offers.values.filter(o => o.status == TradeStatus.Open && o.isParty(user)).toList.sortBy(-_.updatedAt))

  def countOpenFrom(user: UserId): Task[Long] =
    ZIO.succeed(offers.values.count(o => o.status == TradeStatus.Open && o.from == user).toLong)

  def update(o: TradeOffer): Task[Boolean] =
    ZIO.succeed {
      if (failNextUpdate) { failNextUpdate = false; false }
      else offers.get(o.id) match {
        case Some(cur) if cur.status == TradeStatus.Open && cur.version == o.version =>
          offers += o.id -> o.copy(version = o.version + 1); true
        case _ => false
      }
    }

  def expiring(now: Long): Task[List[TradeOffer]] =
    ZIO.succeed(offers.values.filter(o => o.status == TradeStatus.Open && o.expiresAt <= now).toList.sortBy(_.id))

  /** Следующая запись опоздает — будто предложение в ту же секунду тронул партнёр. */
  def loseNextUpdate(): Unit = failNextUpdate = true

  def snapshot: List[TradeOffer] = offers.values.toList.sortBy(_.id)
}
