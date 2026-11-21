package pangea.service.admin

import pangea.dao.admin.AdminDao
import pangea.engine.SceneContent
import pangea.model.user.User
import pangea.service.Background
import pangea.service.sender.Api
import zio.{Duration, Ref, Task, UIO, ZIO, ZLayer, durationInt}

/** Напоминание тем, кто застрял в прологе: герой ещё в регистрации, а игрок не
  * заходил дольше [[PrologueReminder.Idle]] — кто играет прямо сейчас, того не
  * дёргаем. Запускается из админ-панели и идёт в фоне; по окончании — сводка
  * админу.
  *
  * Напоминание уходит без клавиатуры: ВК тогда оставляет игроку прежнюю — кнопки
  * того места пролога, где он остановился, — и он продолжает с него.
  *
  * Письма идут по [[PrologueReminder.Parallel]] разом: пачки ВК (VkBatcher)
  * вывезли бы и всё сразу, но тогда ходы игроков на секунду-другую встали бы в
  * очередь за рассылкой. Кому уже напомнили, не запоминаем: повторный запуск
  * напомнит всем, кто так и стоит в прологе. */
final class PrologueReminder(adminDao: AdminDao, api: Api, background: Background, text: String, running: Ref[Boolean]) {
  import PrologueReminder._

  /** Кому придёт напоминание, если разослать сейчас. */
  def recipients: Task[List[User]] = adminDao.inPrologue(Idle)

  /** Текст напоминания — как его увидит игрок. */
  def message: String = text

  /** Начать в фоне; по окончании — сводка админу `admin`. `false` — уже идёт.
    * При остановке сервера начатое дожидаются (см. [[Background.fork]]). */
  def start(admin: User): Task[Boolean] =
    running.getAndSet(true).flatMap {
      case true  => ZIO.succeed(false)
      case false =>
        background.fork(
          run
            .foldCauseZIO(
              cause => ZIO.logErrorCause("Напоминание о прологе не разослано", cause) *> notify(admin, Failed),
              report => notify(admin, summary(report)))
            .onInterrupt(ZIO.logWarning("Напоминание о прологе: оборвано остановкой сервера"))
            .ensuring(running.set(false))
        ).as(true)
    }

  /** Разослать всем, кто сейчас в [[recipients]]. */
  def run: Task[Report] =
    recipients.flatMap(users => ZIO.foreachPar(users)(remind).withParallelism(Parallel)).map(Report.of)

  /** Одно напоминание. Не дошло — не беда для прочих: исход в сводку, сбой —
    * в журнал `send_failures` (его пишет сама отправка). */
  private def remind(user: User): UIO[Outcome] =
    api.sendMessage(user, text, Nil, None).as(Outcome.Sent: Outcome).catchAll {
      case err if Api.unreachable(err) => ZIO.succeed(Outcome.Closed)
      case err =>
        ZIO.logWarning(s"Напоминание о прологе игроку ${user.userId.value} не ушло: ${err.getMessage}").as(Outcome.Failed)
    }

  private def notify(admin: User, report: String): Task[Unit] =
    api.sendMessage(admin, report, Nil, None)
}

object PrologueReminder {

  /** Кто заходил позже, тому не напоминаем: он, может, прямо сейчас в прологе. */
  val Idle: Duration = 1.hour

  /** Сколько напоминаний в пути разом. */
  val Parallel: Int = 20

  /** Ключ текста напоминания в scenes.yaml. */
  val TextKey: String = "registration.reminder"

  sealed trait Outcome
  object Outcome {
    case object Sent   extends Outcome
    /** Игрок закрыл сообщения от сообщества (900–902). */
    case object Closed extends Outcome
    case object Failed extends Outcome
  }

  /** Сколько напоминаний дошло, скольким игрокам не дошло — закрыли сообщения, —
    * и на скольких ВК не ответил. */
  final case class Report(sent: Int, closed: Int, failed: Int)

  object Report {
    def of(outcomes: Iterable[Outcome]): Report =
      Report(
        sent   = outcomes.count(_ == Outcome.Sent),
        closed = outcomes.count(_ == Outcome.Closed),
        failed = outcomes.count(_ == Outcome.Failed))
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
       |  не дошло (сбой ВК): ${r.failed}""".stripMargin

  val live: ZLayer[AdminDao with Api with Background with SceneContent, Nothing, PrologueReminder] =
    ZLayer.fromZIO(
      for {
        adminDao   <- ZIO.service[AdminDao]
        api        <- ZIO.service[Api]
        background <- ZIO.service[Background]
        content    <- ZIO.service[SceneContent]
        running    <- Ref.make(false)
      } yield new PrologueReminder(adminDao, api, background, content.text(TextKey), running))
}
