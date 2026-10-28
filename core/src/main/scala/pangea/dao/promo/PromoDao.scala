package pangea.dao.promo

import doobie.implicits._
import doobie.util.transactor
import doobie.util.transactor.Transactor
import pangea.model.user.UserId
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Промокоды: кто какой код уже ввёл. Запись — по игроку, а не по герою:
  * после /restart тот же код второй раз ничего не даст. */
trait PromoDao {
  /** Вводил ли игрок этот код. */
  def redeemed(userId: UserId, code: String): Task[Boolean]

  /** Записать, что игрок ввёл код. `false` — уже вводил: дважды не получится
    * даже двумя сообщениями разом. */
  def redeem(userId: UserId, code: String, now: Long): Task[Boolean]
}

class PromoDaoLive(xa: Transactor[Task]) extends PromoDao {

  override def redeemed(userId: UserId, code: String): Task[Boolean] =
    sql"select exists(select 1 from promo_redemptions where user_id = ${userId.value} and code = $code)"
      .query[Boolean].unique.transact(xa)

  override def redeem(userId: UserId, code: String, now: Long): Task[Boolean] =
    sql"""insert into promo_redemptions(user_id, code, redeemed_at)
          values(${userId.value}, $code, $now)
          on conflict (user_id, code) do nothing"""
      .update.run.transact(xa).map(_ == 1)
}

object PromoDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, PromoDao] =
    ZLayer.fromFunction(new PromoDaoLive(_))
}
