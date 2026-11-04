package pangea.service.admin

import pangea.dao.payment.{GrantResult, PaymentDao}
import pangea.model.payment.Payment
import pangea.model.user.{User, UserId}
import pangea.repository.user.UserRepository
import pangea.service.Background
import pangea.service.sender.Api
import zio.{Clock, Duration, Ref, Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** Доплата за смену курса доната: с ноября 2026 дублон стоит 1 ₽ вместо 2 ₽, и
  * каждому заказу по старому курсу доплачивается столько же дублонов, сколько
  * по нему выдано, — а игроку уходит письмо с общей суммой. Запускается из
  * админ-панели и идёт в фоне, как [[NameBackfill]].
  *
  * Дважды не доплатить — это забота базы, а не кнопки: отметка о доплате
  * ложится на заказ одной транзакцией с начислением (`doubloon_rate_bonus`,
  * ключ по заказу). Поэтому повторный запуск — хоть оборванный остановкой,
  * хоть на втором боте — доплачивает только то, что не доплачено, а заказ,
  * оплаченный по старой ссылке уже после выкладки, доплатит следующий запуск. */
final class RateBonus(payments: PaymentDao, userRepo: UserRepository, api: Api, background: Background,
                      running: Ref[Boolean], pause: Duration) {
  import RateBonus._

  /** Что начислится, если запустить сейчас. */
  def preview: Task[Preview] =
    payments.rateBonusDue(OldRateSkus).map(due =>
      Preview(players = due.map(_.userId).distinct.size, orders = due.size, doubloons = due.map(_.doubloons).sum))

  /** Начать в фоне; по окончании — сводка админу `admin`. `false` — уже идёт. */
  def start(admin: User): Task[Boolean] =
    running.getAndSet(true).flatMap {
      case true  => ZIO.succeed(false)
      case false =>
        background.forkUntilShutdown(
          run
            .foldCauseZIO(
              cause => ZIO.logErrorCause("Доплата за новый курс прервалась", cause) *> notify(admin, Failed),
              report => notify(admin, summary(report)))
            .onInterrupt(ZIO.logWarning("Доплата за новый курс: оборвана остановкой сервера — повторный запуск доделает"))
            .ensuring(running.set(false))
        ).as(true)
    }

  /** Доплатить всё, что положено: по игроку, с паузой между письмами. */
  def run: Task[Report] =
    for {
      now    <- Clock.currentTime(TimeUnit.MILLISECONDS)
      due    <- payments.rateBonusDue(OldRateSkus)
      byUser  = due.groupBy(_.userId).toList.sortBy(_._1.value)
      report <- ZIO.foldLeft(byUser)(Report.empty) { case (report, (userId, orders)) =>
                  payUser(userId, orders, now).map(report.add) <* ZIO.sleep(pause)
                }
    } yield report

  /** Все заказы игрока, затем одно письмо на всю сумму. Письмо не ушло —
    * дублоны всё равно на месте: это отметится в сводке, а не откатит доплату. */
  private def payUser(userId: UserId, orders: List[Payment], now: Long): Task[Paid] =
    ZIO.foldLeft(orders)(Paid(userId, 0, 0L, 0, letter = false)) { (paid, order) =>
      payments.grantRateBonus(order.orderId, userId, order.doubloons, now).map {
        case GrantResult.Granted        => paid.copy(orders = paid.orders + 1, doubloons = paid.doubloons + order.doubloons)
        case GrantResult.AlreadyGranted => paid
        case GrantResult.NoHero         => paid.copy(noHero = paid.noHero + 1)
      }
    }.flatMap {
      case paid if paid.doubloons <= 0L => ZIO.succeed(paid)
      case paid =>
        tell(userId, paid.doubloons).as(paid.copy(letter = true))
          .catchAll(e => ZIO.logWarning(s"Доплата за новый курс: письмо игроку ${userId.value} не ушло: ${e.getMessage}").as(paid))
    }

  /** Без клавиатуры: игрок может стоять в бою, и экран с кнопками затёр бы ему
    * текущий (как письмо о зачислении доната). */
  private def tell(userId: UserId, doubloons: Long): Task[Unit] =
    userRepo.getUserById(userId).flatMap {
      case None       => ZIO.fail(new Throwable("игрок не найден"))
      case Some(user) => api.sendMessage(user, letter(doubloons), Nil, None)
    }

  private def notify(admin: User, text: String): Task[Unit] =
    api.sendMessage(admin, text, Nil, None).unit
}

object RateBonus {

  /** Пакеты по прежнему курсу, 2 ₽ за дублон. Из продажи сняты; заказы с
    * этими ключами — те, кому положена доплата. */
  val OldRateSkus: Set[String] = Set("d100", "d250", "d550", "d1200", "d2500")

  /** Пауза между игроками: письма идут в ВК, а бот в это время ещё и играет. */
  val Pause: Duration = Duration.fromMillis(350L)

  /** Письмо игроку. */
  def letter(doubloons: Long): String =
    s"""🟡 Мы снизили цену дублонов: теперь 1 дублон стоит 1 ₽ вместо 2 ₽.
       |
       |Всё, что вы уже купили, пересчитали по новому курсу — вам начислено ещё 🟡 $doubloons дублонов.
       |
       |Спасибо, что поддерживаете Пангею!""".stripMargin

  final case class Preview(players: Int, orders: Int, doubloons: Long)

  /** Итог по одному игроку. */
  final case class Paid(userId: UserId, orders: Int, doubloons: Long, noHero: Int, letter: Boolean)

  /** Сколько игроков и заказов доплачено и на сколько; сколько заказов ждут
    * героя (игрок удалил персонажа — доплатит повторный запуск); скольким
    * игрокам письмо не дошло. */
  final case class Report(players: Int, orders: Int, doubloons: Long, noHero: Int, lettersFailed: Int) {
    def add(p: Paid): Report =
      Report(
        players       = players + (if (p.orders > 0) 1 else 0),
        orders        = orders + p.orders,
        doubloons     = doubloons + p.doubloons,
        noHero        = noHero + p.noHero,
        lettersFailed = lettersFailed + (if (p.orders > 0 && !p.letter) 1 else 0))
  }
  object Report {
    val empty: Report = Report(0, 0, 0L, 0, 0)
  }

  // Тексты админа — служебные, как и у панели (см. AdminPanel).
  val Started: String = "🟡 Начисляю доплату за новый курс. Это в фоне: пришлю сводку, когда закончу."
  val Busy: String    = "🟡 Доплата за новый курс уже начисляется — дождитесь сводки."
  val Failed: String  = "🟡 Доплата за новый курс: прервалось, причина — в логе. Запустите ещё раз: уже доплаченное не повторится."

  def previewText(p: Preview): String =
    if (p.orders == 0) "🟡 Доплата за новый курс: доплачивать некому — всё уже начислено."
    else
      s"""🟡 Доплата за новый курс (1 дублон = 1 ₽).
         |Каждому заказу по старому курсу начислится ещё столько же дублонов, игроку уйдёт письмо.
         |
         |  игроков: ${p.players}
         |  заказов: ${p.orders}
         |  дублонов: ${AdminPanel.amount(p.doubloons)}
         |
         |Начислить?""".stripMargin

  def summary(r: Report): String =
    s"""🟡 Доплата за новый курс начислена.
       |  игроков: ${r.players}
       |  заказов: ${r.orders}
       |  дублонов: ${AdminPanel.amount(r.doubloons)}
       |  ждут героя (персонаж удалён): ${r.noHero}
       |  письмо не дошло: ${r.lettersFailed}""".stripMargin

  val live: ZLayer[PaymentDao with UserRepository with Api with Background, Nothing, RateBonus] =
    ZLayer.fromZIO(
      for {
        payments   <- ZIO.service[PaymentDao]
        userRepo   <- ZIO.service[UserRepository]
        api        <- ZIO.service[Api]
        background <- ZIO.service[Background]
        running    <- Ref.make(false)
      } yield new RateBonus(payments, userRepo, api, background, running, Pause))
}
