package pangea.service.admin

import pangea.model.payment.{OrderId, Payment, PaymentStatus}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.Background
import pangea.test._
import zio.test._
import zio.{Duration, Ref, Task}

/** Доплата за смену курса: каждому заказу по старому курсу — столько же
  * дублонов ещё раз, ровно один раз; игроку — одно письмо на всю сумму. */
object RateBonusSpec extends ZIOSpecDefault {

  private val alice = User(UserId(1L), VkId("vk1"), TelegramId("tg1"))
  private val bob   = User(UserId(2L), VkId("vk2"), TelegramId("tg2"))

  private def order(id: String, user: User, sku: String, doubloons: Long,
                    status: PaymentStatus = PaymentStatus.Confirmed, granted: Boolean = true, at: Long = 1L): Payment =
    Payment(0L, OrderId(id), user.userId, sku, doubloons * 200L, doubloons, "x@y.z", Some(s"pay-$id"), None,
      status, Some(status.entryName.toUpperCase), granted, at, at)

  private def kit(payments: TestPaymentDao): Task[(RateBonus, TestApi)] =
    for {
      users   <- TestUserRepository.withUsers(alice, bob)
      api     <- TestApi.make
      bg      <- Background.make(Background.Grace)
      running <- Ref.make(false)
    } yield (new RateBonus(payments, users, api, bg, running, Duration.Zero), api)

  override def spec = suite("Доплата за новый курс")(

    test("старые заказы доплачены столько же, по письму на игрока с суммой; повтор ничего не начисляет") {
      for {
        payments <- TestPaymentDao.withHeroes(alice.userId, bob.userId)
        _        <- payments.insert(order("a1", alice, "d100", 100L, at = 1L))
        _        <- payments.insert(order("a2", alice, "d550", 550L, at = 2L))
        _        <- payments.insert(order("b1", bob, "d250", 250L))
        // не доплачиваются: новый пакет, неоплаченный, возвращённый
        _        <- payments.insert(order("a3", alice, "d200", 200L))
        _        <- payments.insert(order("b2", bob, "d250", 250L, PaymentStatus.Rejected, granted = false))
        _        <- payments.insert(order("b3", bob, "d1200", 1200L, PaymentStatus.Refunded))
        k        <- kit(payments)
        (bonus, api) = k
        preview  <- bonus.preview
        report   <- bonus.run
        aCoins   <- payments.doubloonsOf(alice.userId)
        bCoins   <- payments.doubloonsOf(bob.userId)
        letters  <- api.sentMessages.map(_.map(_._1))
        again    <- bonus.run
        aAgain   <- payments.doubloonsOf(alice.userId)
        letters2 <- api.sentMessages.map(_.map(_._1))
        marked   <- payments.rateBonuses
      } yield assertTrue(preview == RateBonus.Preview(players = 2, orders = 3, doubloons = 900L)) &&
              assertTrue(report == RateBonus.Report(players = 2, orders = 3, doubloons = 900L, noHero = 0, lettersFailed = 0)) &&
              assertTrue(aCoins == 650L && bCoins == 250L) &&
              assertTrue(letters == List(RateBonus.letter(650L), RateBonus.letter(250L))) &&
              assertTrue(letters.forall(_.contains("1 дублон стоит 1 ₽ вместо 2 ₽"))) &&
              assertTrue(again == RateBonus.Report.empty && aAgain == 650L && letters2.size == 2) &&
              assertTrue(marked == Map("a1" -> 100L, "a2" -> 550L, "b1" -> 250L))
    },

    test("героя нет — заказ ждёт и доплачивается повторным запуском, когда персонаж появится") {
      for {
        payments <- TestPaymentDao.withHeroes(alice.userId)
        _        <- payments.insert(order("b1", bob, "d250", 250L))
        k        <- kit(payments)
        (bonus, api) = k
        first    <- bonus.run
        _        <- payments.addHero(bob.userId)
        second   <- bonus.run
        coins    <- payments.doubloonsOf(bob.userId)
        letters  <- api.sentMessages
      } yield assertTrue(first == RateBonus.Report(0, 0, 0L, noHero = 1, lettersFailed = 0)) &&
              assertTrue(second == RateBonus.Report(1, 1, 250L, noHero = 0, lettersFailed = 0)) &&
              assertTrue(coins == 250L && letters.size == 1)
    },

    test("старые пакеты — ровно те, что продавались по 2 ₽ за дублон") {
      assertTrue(RateBonus.OldRateSkus == Set("d100", "d250", "d550", "d1200", "d2500"))
    }
  )
}
