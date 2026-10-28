package pangea.test

import pangea.dao.promo.PromoDao
import pangea.model.user.UserId
import zio.{Task, ZIO}

/** Промокоды в памяти: кто какой код ввёл и когда. */
class TestPromoDao extends PromoDao {
  private var rows: Map[(UserId, String), Long] = Map.empty

  def redeemed(userId: UserId, code: String): Task[Boolean] = ZIO.succeed(rows.contains((userId, code)))

  def redeem(userId: UserId, code: String, now: Long): Task[Boolean] =
    ZIO.succeed {
      if (rows.contains((userId, code))) false
      else { rows = rows.updated((userId, code), now); true }
    }

  def snapshot: Map[(UserId, String), Long] = rows
}
