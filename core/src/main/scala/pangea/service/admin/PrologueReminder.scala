package pangea.service.admin

import pangea.dao.admin.{AdminDao, PrologueReminderDao}
import pangea.dao.admin.PrologueReminderDao.Outcome
import pangea.engine.SceneContent
import pangea.model.user.User
import pangea.service.Background
import pangea.service.sender.Api
import zio.{Duration, Ref, Task, UIO, ZIO, ZLayer, durationInt}

/** Напоминание тем, кто застрял в прологе: герой ещё в регистрации, а игрок не
  * заходил дольше [[PrologueReminder.Idle]] — кто играет прямо сейчас, того не
  * дёргаем. Кому напоминали за последние [[PrologueReminder.Repeat]], того
  * рассылка пропускает. Запускается из админ-панели и идёт в фоне; по
  * окончании — сводка админу.
  *
  * Напоминание уходит без клавиатуры: ВК тогда оставляет игроку прежнюю — кнопки
  * того места пролога, где он остановился, — и он продолжает с него.
  *
  * Рассылка, её адресаты и исход каждого письма пишутся в базу
  * ([[PrologueReminderDao]]); что адресат сделал после — вернулся, выбрал расу,
  * прошёл пролог — отмечает игра (RegistrationState). Отсюда — прошлая рассылка
  * на экране панели и графики на дашборде «Воронка».
  *
  * Письма идут по [[PrologueReminder.Parallel]] разом: пачки ВК (VkBatcher)
  * вывезли бы и всё сразу, но тогда ходы игроков на секунду-другую встали бы в
  * очередь за рассылкой. */
final class PrologueReminder(
    adminDao:   AdminDao,
    dao:        PrologueReminderDao,
    api:        Api,
    background: Background,
    text:       String,
    running:    Ref[Boolean]
) {
  import PrologueReminder._

  /** Кому придёт напоминание, если разослать сейчас, и скольких пропустим. */
  def plan: Task[Plan] =
    for {
      stuck  <- adminDao.inPrologue(Idle)
      recent <- dao.remindedWithin(Repeat)
      (skip, send) = stuck.partition(u => recent.contains(u.userId))
    } yield Plan(send, skip.size)

  /** Прошлая рассылка и что было после неё. */
  def last: Task[Option[PrologueReminderDao.Last]] = dao.last

  /** Текст напоминания — как его увидит игрок. */
  def message: String = text

  /** Начать в фоне от имени `admin`; по окончании — ему сводка. `false` — уже
    * идёт. При остановке сервера начатое дожидаются (см. [[Background.fork]]). */
  def start(admin: User): Task[Boolean] =
    running.getAndSet(true).flatMap {
      case true  => ZIO.succeed(false)
      case false =>
        background.fork(
          run(admin)
            .foldCauseZIO(
              cause => ZIO.logErrorCause("Напоминание о прологе не разослано", cause) *> notify(admin, Failed),
              report => notify(admin, summary(report)))
            .onInterrupt(ZIO.logWarning("Напоминание о прологе: оборвано остановкой сервера"))
            .ensuring(running.set(false))
        ).as(true)
    }

  /** Разослать по [[plan]] и записать: рассылку с адресатами, исход каждого
    * письма, итоги. */
  def run(admin: User): Task[Report] =
    for {
      plan     <- plan
      id       <- dao.open(admin.userId, Idle, text, plan.recipients.map(_.userId), plan.skipped)
      outcomes <- ZIO.foreachPar(plan.recipients)(u => remind(u).tap(record(id, u, _))).withParallelism(Parallel)
      report    = Report.of(outcomes, plan.skipped)
      _        <- dao.close(id, report.sent, report.closed, report.failed)
    } yield report

  /** Одно напоминание. Не дошло — не беда для прочих: исход в сводку, сбой —
    * в журнал `send_failures` (его пишет сама отправка). */
  private def remind(user: User): UIO[Outcome] =
    api.sendMessage(user, text, Nil, None).as(Outcome.Sent: Outcome).catchAll {
      case err if Api.unreachable(err) => ZIO.succeed(Outcome.Closed)
      case err =>
        ZIO.logWarning(s"Напоминание о прологе игроку ${user.userId.value} не ушло: ${err.getMessage}").as(Outcome.Failed)
    }

  /** Исход письма — в базу. Не записался — письмо всё равно ушло: только в лог. */
  private def record(reminder: Long, user: User, outcome: Outcome): UIO[Unit] =
    dao.outcome(reminder, user.userId, outcome).catchAll(err =>
      ZIO.logWarning(s"Напоминание о прологе: исход для игрока ${user.userId.value} не записан: ${err.getMessage}"))

  private def notify(admin: User, report: String): Task[Unit] =
    api.sendMessage(admin, report, Nil, None)
}

object PrologueReminder {

  /** Кто заходил позже, тому не напоминаем: он, может, прямо сейчас в прологе. */
  val Idle: Duration = 1.hour

  /** Не чаще раза за этот срок одному игроку — так решил пользователь. */
  val Repeat: Duration = 7.days

  /** Сколько напоминаний в пути разом. */
  val Parallel: Int = 20

  /** Ключ текста напоминания в scenes.yaml. */
  val TextKey: String = "registration.reminder"

  /** Кому напомнить и скольких пропустить — им напоминали за [[Repeat]]. */
  final case class Plan(recipients: List[User], skipped: Int)

  /** Сколько напоминаний дошло, скольким игрокам не дошло — закрыли сообщения, —
    * на скольких ВК не ответил и скольких пропустили. */
  final case class Report(sent: Int, closed: Int, failed: Int, skipped: Int)

  object Report {
    def of(outcomes: Iterable[Outcome], skipped: Int): Report =
      Report(
        sent    = outcomes.count(_ == Outcome.Sent),
        closed  = outcomes.count(_ == Outcome.Closed),
        failed  = outcomes.count(_ == Outcome.Failed),
        skipped = skipped)
  }

  // Тексты — служебные, как и у панели (см. AdminPanel). Текст для игроков — в
  // scenes.yaml (TextKey).
  val Started: String = "💭 Рассылаю напоминание о прологе — это в фоне: пришлю сводку, когда закончу."
  val Busy: String    = "💭 Напоминание о прологе уже рассылается — дождитесь сводки."
  val Failed: String  = "💭 Напоминание о прологе: прервалось, причина — в логе."

  def summary(r: Report): String =
    s"""💭 Напоминание о прологе разослано.
       |  дошло: ${r.sent}
       |  закрыли сообщения от сообщества: ${r.closed}
       |  не дошло (сбой ВК): ${r.failed}
       |  пропущено (напоминали за неделю): ${r.skipped}""".stripMargin

  val live: ZLayer[AdminDao with PrologueReminderDao with Api with Background with SceneContent, Nothing, PrologueReminder] =
    ZLayer.fromZIO(
      for {
        adminDao   <- ZIO.service[AdminDao]
        dao        <- ZIO.service[PrologueReminderDao]
        api        <- ZIO.service[Api]
        background <- ZIO.service[Background]
        content    <- ZIO.service[SceneContent]
        running    <- Ref.make(false)
      } yield new PrologueReminder(adminDao, dao, api, background, content.text(TextKey), running))
}
