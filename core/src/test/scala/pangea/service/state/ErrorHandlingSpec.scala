package pangea.service.state

import pangea.dao.schedule.ScheduledTaskDao
import pangea.engine.{Renderer, SceneContent}
import pangea.model.schedule.{ScheduledTask, TaskKind, TaskStatus}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.ErrorLog
import pangea.service.admin.AdminPanel
import pangea.service.schedule.SchedulerPollerLive
import pangea.test._
import zio.test._
import zio.{Cause, FiberId, LogLevel, Ref, Task, ZIO}

/** Поломки в логе: и ошибка, и падение кода (defect) пишутся со всей причиной;
  * ход игрока, упавший кодом, отвечает ему, а задача по таймеру, упавшая
  * кодом, — это попытка, и партия за ней не встаёт. */
object ErrorHandlingSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val vkId     = VkId("vk_test")
  private val testUser = User(userId, vkId, TelegramId("tg_test"))

  /** Сцена, чей ход падает самим кодом, а не ошибкой. */
  private object Boom extends State {
    def enter(user: User, renderer: Renderer): Task[Unit] = ZIO.unit
    def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
      ZIO.die(new IllegalStateException("бум"))
  }

  private def handler(states: Map[StateType, State], start: StateType) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = start))
      heroRepo <- TestHeroRepository.withHero(userId, TestFixtures.hero(userId, state = start))
      userRepo <- TestUserRepository.withUser(testUser)
      api      <- TestApi.make
      content  <- ZIO.attempt(SceneContent.load())
      lock     <- PlayerLock.make
      payouts   = pangea.service.payout.Payouts(TestPayoutDao.empty, heroDao, content)
      parcels   = pangea.service.parcel.Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
      transfers = pangea.service.parcel.Transfers(TestInventoryRepository.accepting, userRepo, parcels,
                    new TestPlayers, content, heroDao, TestBankRepository.empty, payouts, lock)
    } yield (new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers, states,
               AdminPanel.disabled, content, lock), api)

  /** Очередь из готовых задач: что отмечено — видно по спискам. */
  private final class Queue(tasks: List[ScheduledTask], done: Ref[List[Long]], bumped: Ref[List[Long]])
      extends ScheduledTaskDao {
    def insert(userId: UserId, fireAt: Long, kind: TaskKind, expectedState: StateType, action: String,
               createdAt: Long): Task[Long] = ZIO.succeed(0L)
    def claimDue(now: Long, until: Long, limit: Int): Task[List[ScheduledTask]] = ZIO.succeed(tasks)
    def markDone(id: Long): Task[Unit]                             = done.update(_ :+ id)
    def markDead(id: Long): Task[Unit]                             = ZIO.unit
    def bumpAttempts(id: Long): Task[Int]                          = bumped.updateAndGet(_ :+ id).map(_.count(_ == id))
    def cancel(userId: UserId, kind: TaskKind): Task[Int]          = ZIO.succeed(0)
  }

  private def task(id: Long, expected: StateType): ScheduledTask =
    ScheduledTask(id, userId, 0L, TaskKind.Construction, expected, """{"action":"Finish"}""", TaskStatus.Pending, 0, 0L)

  private def errors = ZTestLogger.logOutput.map(_.filter(_.logLevel == LogLevel.Error))

  override def spec = suite("Поломки в логе")(

    test("ошибка и падение кода — в лог со всей причиной; прерывание не пишется и идёт дальше") {
      for {
        _    <- ErrorLog.logged("ошибка")(Cause.fail(new RuntimeException("плохо")))
        _    <- ErrorLog.logged("падение")(Cause.die(new IllegalStateException("бум")))
        intr <- ErrorLog.logged("прервали")(Cause.interrupt(FiberId.None)).exit
        logs <- errors
      } yield assertTrue(logs.exists(e => e.message() == "ошибка" && e.cause.failureOption.isDefined)) &&
              assertTrue(logs.exists(e => e.message() == "падение" && e.cause.dieOption.exists(_.getMessage == "бум"))) &&
              assertTrue(intr.isFailure && !logs.exists(_.message() == "прервали"))
    },

    test("ход упал самим кодом: игроку — «Произошла ошибка», в лог — ERROR с причиной и номером игрока") {
      for {
        t <- handler(Map(StateType.GlobalMap -> Boom), StateType.GlobalMap)
        (h, api) = t
        _    <- h.makeActionVK(vkId, 1L, UserAction("что угодно", None))
        sent <- api.sentMessages.map(_.map(_._1))
        logs <- errors
      } yield assertTrue(sent.exists(_.contains("Произошла ошибка"))) &&
              assertTrue(logs.exists(e => e.message().contains("Ход игрока 1 не обработан") &&
                e.cause.dieOption.exists(_.getMessage == "бум")))
    },

    test("задача по таймеру упала самим кодом: это попытка, в лог — причина; следующая в партии исполнилась") {
      for {
        t      <- handler(Map(StateType.GlobalMap -> Boom), StateType.GlobalMap)
        (h, _)  = t
        done   <- Ref.make(List.empty[Long])
        bumped <- Ref.make(List.empty[Long])
        // первая ждёт героя на площади — там ход и падает; вторая ждёт его на
        // отдыхе — его там нет, она просто пропускается
        poller  = new SchedulerPollerLive(new Queue(List(task(1L, StateType.GlobalMap), task(2L, StateType.Rest)), done, bumped), h)
        _      <- poller.poll
        d      <- done.get
        b      <- bumped.get
        logs   <- errors
      } yield assertTrue(b == List(1L) && d == List(2L)) &&
              assertTrue(logs.exists(e => e.message().contains("Scheduled task 1") && e.cause.dieOption.exists(_.getMessage == "бум")))
    }
  )
}
