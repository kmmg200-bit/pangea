package pangea.model.payment

import pangea.model.payment.PaymentStatus._
import zio.test._

/** Статус заказа назад не ходит: нотификации банка приходят вперемешку. */
object PaymentStatusSpec extends ZIOSpecDefault {

  override def spec = suite("Статус заказа")(
    test("промежуточный не затирает итоговый") {
      assertTrue(!Authorized.replaces(Confirmed)) &&
      assertTrue(!FormShowed.replaces(Confirmed)) &&
      assertTrue(!New.replaces(Rejected)) &&
      assertTrue(!Authorized.replaces(Refunded))
    },
    test("вперёд — можно, и тот же статус — тоже: уточняется сырой статус банка") {
      assertTrue(Authorized.replaces(FormShowed)) &&
      assertTrue(Confirmed.replaces(Authorized)) &&
      assertTrue(Refunded.replaces(Confirmed)) &&
      assertTrue(Refunded.replaces(Refunded)) &&
      assertTrue(PaymentStatus.values.forall(s => s.replaces(s)))
    },
    test("CONFIRMED после отказа проходит — деньги взяты; отказ после CONFIRMED — нет") {
      assertTrue(Confirmed.replaces(Rejected)) &&
      assertTrue(!Rejected.replaces(Confirmed))
    },
    test("незнакомый статус банка не переоткрывает закрытый заказ и не отменяет возврат") {
      assertTrue(!Unknown.replaces(Confirmed)) &&
      assertTrue(!Unknown.replaces(Rejected)) &&
      assertTrue(!Confirmed.replaces(Refunded)) &&
      assertTrue(Unknown.replaces(Authorized))
    }
  )
}
