package pangea.dao.payment

import cats.data.NonEmptyList
import doobie.{ConnectionIO, Fragments}
import doobie.free.{connection => FC}
import doobie.implicits._
import doobie.util.fragment.Fragment
import doobie.util.transactor.Transactor
import pangea.model.payment.{OrderId, Payment, PaymentStatus}
import pangea.model.user.UserId
import zio.interop.catz._
import zio.{Task, ZIO}

class PaymentDaoLive(xa: Transactor[Task]) extends PaymentDao {

  private val columns = Fragment.const(PaymentDaoLive.Columns)

  override def insert(payment: Payment): Task[Long] =
    sql"""insert into payments(order_id, user_id, sku, amount_kopecks, doubloons, receipt_email,
                               payment_id, payment_url, status, bank_status, granted,
                               created_at, updated_at)
          values(${payment.orderId.value}, ${payment.userId.value}, ${payment.sku},
                 ${payment.amountKopecks}, ${payment.doubloons}, ${payment.receiptEmail},
                 ${payment.paymentId}, ${payment.paymentUrl}, ${payment.status},
                 ${payment.bankStatus}, ${payment.granted}, ${payment.createdAt}, ${payment.updatedAt})"""
      .update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(xa)

  override def setStarted(orderId: OrderId, paymentId: String, paymentUrl: String, updatedAt: Long): Task[Unit] =
    sql"""update payments set payment_id = $paymentId, payment_url = $paymentUrl, updated_at = $updatedAt
          where order_id = ${orderId.value}"""
      .update.run.transact(xa).unit

  /** Статус назад не ходит ([[PaymentStatus.replaces]]): проверка — в самом
    * `update`, иначе две параллельные нотификации снова гоняются между чтением
    * и записью. Опоздавший статус просто ничего не меняет. */
  override def setStatus(orderId: OrderId, status: PaymentStatus, bankStatus: String, updatedAt: Long): Task[Unit] =
    (fr"""update payments set status = $status, bank_status = $bankStatus, updated_at = $updatedAt
          where order_id = ${orderId.value} and""" ++
      Fragments.in(fr"status", NonEmptyList.of(status, PaymentStatus.values.filter(s => s != status && status.replaces(s)): _*)))
      .update.run.transact(xa).unit

  override def byOrderId(orderId: OrderId): Task[Option[Payment]] =
    (fr"select" ++ columns ++ fr"from payments where order_id = ${orderId.value}")
      .query[Payment].option.transact(xa)

  override def pendingOf(userId: UserId): Task[List[Payment]] =
    (fr"select" ++ columns ++
      fr"""from payments
           where user_id = ${userId.value} and status in ('New', 'FormShowed', 'Authorized')
           order by created_at desc""")
      .query[Payment].to[List].transact(xa)

  override def lastOf(userId: UserId): Task[Option[Payment]] =
    (fr"select" ++ columns ++
      fr"from payments where user_id = ${userId.value} order by created_at desc limit 1")
      .query[Payment].option.transact(xa)

  override def unsettled(olderThan: Long, limit: Int): Task[List[Payment]] =
    (fr"select" ++ columns ++
      fr"""from payments
           where not granted and updated_at <= $olderThan
             and status in ('New', 'FormShowed', 'Authorized', 'Confirmed')
           order by updated_at limit $limit""")
      .query[Payment].to[List].transact(xa)

  override def paidSince(userId: UserId, since: Long): Task[Long] =
    sql"""select coalesce(sum(amount_kopecks), 0) from payments
          where user_id = ${userId.value} and created_at >= $since and status = 'Confirmed'"""
      .query[Long].unique.transact(xa)

  /** Флаг выдачи и начисление — одной транзакцией.
    *
    * Тянуться из платёжного DAO в `heroes` некрасиво, но альтернатива хуже:
    * двумя отдельными запросами падение между ними либо теряет оплаченные
    * дублоны, либо начисляет их дважды, и ни то ни другое не лечится
    * повтором нотификации.
    *
    * Начисление идёт дельтой (`doubloons = doubloons + ?`), а не записью
    * абсолютного значения: выдача приходит из вебхука, то есть вне хода
    * игрока и вне `PlayerLock`, и запись целого значения затёрла бы трату,
    * случившуюся в этот же момент. */
  override def grantOnce(orderId: OrderId, userId: UserId, doubloons: Long, updatedAt: Long): Task[GrantResult] =
    markThenCredit(
      sql"""update payments set granted = true, updated_at = $updatedAt
            where order_id = ${orderId.value} and not granted""".update.run,
      userId, doubloons)

  /** Отметка «выдано» и начисление — одной транзакцией. Отметка не легла
    * (уже стоит) — значит, выдано раньше; героя нет — транзакция рвётся, и
    * отметка снимается вместе с ней. */
  private def markThenCredit(mark: ConnectionIO[Int], userId: UserId, doubloons: Long): Task[GrantResult] = {
    val credit =
      sql"""update heroes set doubloons = doubloons + $doubloons
            where user_id = ${userId.value}""".update.run

    val tx =
      mark.flatMap {
        case 0 => FC.pure[GrantResult](GrantResult.AlreadyGranted)
        case _ =>
          credit.flatMap {
            // Героя нет — рвём транзакцию, чтобы снялась и отметка.
            case 0 => FC.raiseError[GrantResult](PaymentDaoLive.NoHero)
            case _ => FC.pure[GrantResult](GrantResult.Granted)
          }
      }

    tx.transact(xa).catchSome { case PaymentDaoLive.NoHero => ZIO.succeed(GrantResult.NoHero) }
  }
}

object PaymentDaoLive {

  /** Колонки `payments` в порядке полей [[Payment]]: `Read[Payment]` читает их
    * по позиции, поэтому новое поле без колонки здесь падает не на компиляции,
    * а уже на живой базе. На соответствие стоит тест (`PaymentDaoSpec`). */
  val Columns: String =
    "id, order_id, user_id, sku, amount_kopecks, doubloons, receipt_email, " +
      "payment_id, payment_url, status, bank_status, granted, created_at, updated_at"

  /** Внутренний сигнал «героя нет», которым откатывается транзакция выдачи.
    * Наружу не выходит: [[PaymentDaoLive.grantOnce]] ловит его сам. */
  private case object NoHero extends RuntimeException("payment grant: hero is gone")
    with scala.util.control.NoStackTrace
}
