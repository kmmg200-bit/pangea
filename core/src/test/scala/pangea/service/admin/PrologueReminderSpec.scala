package pangea.service.admin

import pangea.dao.admin.{AdminDao, AdminStats, GameStats, PrologueReminderDao}
import pangea.engine.SceneContent
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.vk.Attachment
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.UserResponse
import pangea.service.Background
import pangea.service.sender.vk.VkApi
import pangea.service.sender.{Api, ChatTarget}
import pangea.test.TestPrologueReminderDao
import zio.test._
import zio.{Duration, Promise, Ref, Task, UIO, ZIO, durationInt}

/** Напоминание застрявшим в прологе: кому, что и как — без клавиатуры, чтобы у
  * игрока остались кнопки того места, где он остановился. */
object PrologueReminderSpec extends ZIOSpecDefault {

  private def user(n: Long): User = User(UserId(n), VkId(s"vk$n"), TelegramId(s"tg$n"))

  private val admin = user(1L)
  private val Text  = "💭 Герой задумался — пора продолжить."

  /** В прологе — `stuck`; с каким сроком спросили — в `asked`. Прочее панели
    * здесь не нужно. */
  private final class PrologueDao(stuck: List[User], asked: Ref[List[Duration]]) extends AdminDao {
    def inPrologue(idle: Duration): Task[List[User]] = asked.update(_ :+ idle).as(stuck)
    def stats: Task[AdminStats]                       = ZIO.dieMessage("не нужно")
    def gameStats: Task[GameStats]                    = ZIO.dieMessage("не нужно")
    def level(userId: UserId): Task[Int]              = ZIO.succeed(0)
    def setLevel(userId: UserId, level: Int): Task[Unit] = ZIO.unit
    def withLevel(min: Int): Task[List[(UserId, VkId)]]  = ZIO.succeed(Nil)
  }

  /** ВК: кому, что и с какой клавиатурой; `refuse` — кому что ответить ошибкой;
    * `gate` — письмо ждёт, пока его не откроют; в пути — `inFlight`, наибольшее —
    * `peak`. */
  private final class Vk(
      sent:     Ref[List[(String, String, Option[Keyboard])]],
      refuse:   Map[String, Throwable],
      gate:     Promise[Nothing, Unit],
      inFlight: Ref[Int],
      peak:     Ref[Int]
  ) extends Api {
    def sendMessage(user: User, message: String, attachments: List[Attachment], keyboard: Option[Keyboard]): Task[Unit] =
      (inFlight.updateAndGet(_ + 1).flatMap(n => peak.update(_ max n)) *> gate.await *>
        sent.update(_ :+ ((user.vkId.value, message, keyboard))) *>
        ZIO.foreachDiscard(refuse.get(user.vkId.value))(ZIO.fail(_))).ensuring(inFlight.update(_ - 1))
    def getName(user: User): Task[UserResponse]                                   = ZIO.dieMessage("не нужно")
    def sendToChat(to: ChatTarget, message: String): Task[Unit]                   = ZIO.unit
    def forwardToUser(to: VkId, message: String, forwarded: Option[Long]): Task[Unit] = ZIO.unit

    def letters: UIO[List[(String, String, Option[Keyboard])]] = sent.get
    def open: UIO[Unit]                                       = gate.succeed(()).unit
    def flying: UIO[Int]                                      = inFlight.get
    def mostAtOnce: UIO[Int]                                  = peak.get
  }

  private def vk(refuse: Map[String, Throwable] = Map.empty, open: Boolean = true): UIO[Vk] =
    for {
      sent     <- Ref.make(List.empty[(String, String, Option[Keyboard])])
      gate     <- Promise.make[Nothing, Unit]
      _        <- ZIO.when(open)(gate.succeed(()))
      inFlight <- Ref.make(0)
      peak     <- Ref.make(0)
    } yield new Vk(sent, refuse, gate, inFlight, peak)

  private def reminder(stuck: List[User], api: Api, dao: PrologueReminderDao = TestPrologueReminderDao.silent) =
    for {
      asked   <- Ref.make(List.empty[Duration])
      bg      <- Background.make(Background.Grace)
      running <- Ref.make(false)
    } yield (new PrologueReminder(new PrologueDao(stuck, asked), dao, api, bg, Text, running), asked)

  private def summaryIn(api: Vk) =
    api.letters.repeatUntil(_.exists { case (to, text, _) => to == admin.vkId.value && text.startsWith("💭 Напоминание о прологе разослано") })

  override def spec = suite("Напоминание о прологе")(

    test("каждому застрявшему — то самое напоминание и без клавиатуры: у игрока остаются кнопки пролога") {
      val stuck = (2L to 5L).toList.map(user)
      for {
        api    <- vk()
        t      <- reminder(stuck, api)
        (r, _)  = t
        report <- r.run(admin)
        sent   <- api.letters
      } yield assertTrue(report == PrologueReminder.Report(sent = 4, closed = 0, failed = 0, skipped = 0)) &&
              assertTrue(sent.map(_._1).sorted == stuck.map(_.vkId.value).sorted) &&
              assertTrue(sent.forall { case (_, text, keys) => text == Text && keys.isEmpty })
    },

    test("кому — решает выборка: в прологе и не заходил дольше часа") {
      for {
        api    <- vk()
        t      <- reminder(List(user(2L)), api)
        (r, asked) = t
        whom   <- r.plan
        _      <- r.run(admin)
        idles  <- asked.get
      } yield assertTrue(whom == PrologueReminder.Plan(List(user(2L)), 0), idles.forall(_ == 1.hour), idles.nonEmpty,
                         PrologueReminder.Idle == 1.hour)
    },

    test("закрыл сообщения (900–902) — в сводке отдельно; сбой ВК у одного не мешает остальным") {
      val stuck = (2L to 6L).toList.map(user)
      for {
        api    <- vk(Map(
                    "vk3" -> VkApi.VkApiError(901, "Can't send messages for users without permission"),
                    "vk4" -> VkApi.VkApiError(902, "privacy"),
                    "vk5" -> new RuntimeException("connection reset")))
        t      <- reminder(stuck, api)
        (r, _)  = t
        report <- r.run(admin)
        tried  <- api.letters
      } yield assertTrue(report == PrologueReminder.Report(sent = 2, closed = 2, failed = 1, skipped = 0)) &&
              assertTrue(tried.map(_._1).sorted == stuck.map(_.vkId.value).sorted)
    },

    test("не больше 20 писем в пути разом — ходы игроков не встают в очередь за рассылкой") {
      val stuck = (2L to 61L).toList.map(user)
      for {
        api    <- vk(open = false)
        t      <- reminder(stuck, api)
        (r, _)  = t
        fiber  <- r.run(admin).fork
        _      <- api.flying.repeatUntil(_ == PrologueReminder.Parallel)
        _      <- ZIO.yieldNow.repeatN(100)
        held   <- api.flying
        _      <- api.open
        report <- fiber.join
        most   <- api.mostAtOnce
      } yield assertTrue(held == PrologueReminder.Parallel, most == PrologueReminder.Parallel) &&
              assertTrue(report.sent == 60)
    },

    test("в фоне: пока идёт — второй запуск не начинается; по окончании — сводка админу, и можно снова") {
      val stuck = (2L to 4L).toList.map(user)
      for {
        api     <- vk(open = false)
        t       <- reminder(stuck, api)
        (r, _)   = t
        first   <- r.start(admin)
        second  <- r.start(admin)
        _       <- api.open
        sent    <- summaryIn(api)
        again   <- r.start(admin).repeatUntil(identity)
      } yield assertTrue(first, !second, again) &&
              assertTrue(sent.exists { case (to, text, _) => to == admin.vkId.value && text.contains("дошло: 3") })
    },

    test("сводка: дошло, закрыли сообщения, сбой ВК, пропущено") {
      val text = PrologueReminder.summary(PrologueReminder.Report(sent = 300, closed = 90, failed = 2, skipped = 7))
      assertTrue(text.contains("дошло: 300"), text.contains("закрыли сообщения от сообщества: 90"),
                 text.contains("не дошло (сбой ВК): 2"), text.contains("пропущено (напоминали за неделю): 7"))
    },

    test("кому напоминали за неделю — пропущены: письма им нет, в сводке — сколько") {
      val stuck = (2L to 5L).toList.map(user)
      for {
        api    <- vk()
        dao    <- TestPrologueReminderDao.make(recent = Set(UserId(3L), UserId(5L)))
        t      <- reminder(stuck, api, dao)
        (r, _)  = t
        plan   <- r.plan
        report <- r.run(admin)
        sent   <- api.letters
      } yield assertTrue(plan == PrologueReminder.Plan(List(user(2L), user(4L)), 2)) &&
              assertTrue(sent.map(_._1).sorted == List("vk2", "vk4"), report.skipped == 2, report.sent == 2) &&
              assertTrue(PrologueReminder.Repeat == 7.days)
    },

    test("рассылка — в учёт: кто запустил, порог, текст, адресаты, пропущенные, исход каждого письма и итоги") {
      val stuck = (2L to 5L).toList.map(user)
      for {
        api  <- vk(Map("vk3" -> VkApi.VkApiError(901, "closed"), "vk4" -> new RuntimeException("connection reset")))
        dao  <- TestPrologueReminderDao.make(recent = Set(UserId(5L)))
        t    <- reminder(stuck, api, dao)
        (r, _) = t
        _    <- r.run(admin)
        runs <- dao.runs
      } yield assertTrue(runs.size == 1) &&
              assertTrue(runs.forall(run =>
                run.admin == admin.userId && run.idle == PrologueReminder.Idle && run.message == Text &&
                run.recipients == List(UserId(2L), UserId(3L), UserId(4L)) && run.skipped == 1 &&
                run.outcomes == Map(
                  UserId(2L) -> PrologueReminderDao.Outcome.Sent,
                  UserId(3L) -> PrologueReminderDao.Outcome.Closed,
                  UserId(4L) -> PrologueReminderDao.Outcome.Failed) &&
                run.totals.contains((1, 1, 1))))
    },

    test("учёт не записал исход письма — рассылка идёт дальше, письмо засчитано") {
      val broken = new PrologueReminderDao {
        def remindedWithin(within: Duration): Task[Set[UserId]] = ZIO.succeed(Set.empty)
        def open(a: UserId, idle: Duration, m: String, rs: List[UserId], skipped: Int): Task[Long] = ZIO.succeed(1L)
        def outcome(reminder: Long, u: UserId, o: PrologueReminderDao.Outcome): Task[Unit] = ZIO.fail(new RuntimeException("база"))
        def close(reminder: Long, sent: Int, closed: Int, failed: Int): Task[Unit] = ZIO.unit
        def last: Task[Option[PrologueReminderDao.Last]] = ZIO.none
        def returned(u: UserId): Task[Unit] = ZIO.unit
        def choseRace(u: UserId): Task[Unit] = ZIO.unit
        def completed(u: UserId): Task[Unit] = ZIO.unit
      }
      for {
        api    <- vk()
        t      <- reminder(List(user(2L), user(3L)), api, broken)
        (r, _)  = t
        report <- r.run(admin)
      } yield assertTrue(report.sent == 2)
    },

    test("текст напоминания — в scenes.yaml, без незаполненных подстановок") {
      val text = SceneContent.load().text(PrologueReminder.TextKey)
      assertTrue(text.contains("задумался"), !text.contains("{"), text.length < 300)
    }
  ) @@ TestAspect.timeout(30.seconds)
}
