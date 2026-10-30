package pangea.service.schedule

import pangea.dao.schedule.ScheduledTaskDao
import pangea.engine.{Renderer, SceneContent}
import pangea.model.schedule.{ScheduledTask, TaskKind, TaskStatus}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.sender.vk.VkApi
import pangea.service.state.{PlayerLock, State, StateHandler, UserAction}
import pangea.test._
import zio.test._
import zio.{Promise, Ref, Task, ZIO, durationInt}

/** Партия поллера: игроки исполняются параллельно, задачи одного игрока — по
  * очереди в порядке fire_at. */
object SchedulerPollerSpec extends ZIOSpecDefault {

  private val first  = User(UserId(1L), VkId("vk_1"), TelegramId("tg_1"))
  private val second = User(UserId(2L), VkId("vk_2"), TelegramId("tg_2"))

  /** Сцена-журнал: пишет начало и конец каждого хода. Ход «a1» первого игрока
    * ждёт, пока начнётся ход второго, — при последовательной партии он не
    * дождётся никогда. */
  private final class Gate(log: Ref[List[String]], secondStarted: Promise[Nothing, Unit]) extends State {
    def enter(user: User, renderer: Renderer): Task[Unit] = ZIO.unit
    def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] = {
      val label = ua.payload.getOrElse("")
      for {
        _ <- log.update(_ :+ s"start $label")
        _ <- label match {
               case "a1" => secondStarted.await
               case "b1" => secondStarted.succeed(()).unit
               case _    => ZIO.unit
             }
        _ <- log.update(_ :+ s"end $label")
      } yield StateType.Construction
    }
  }

  /** Сцена, чей ход не дошёл до игрока: он закрыл сообщения от сообщества. */
  private object Closed extends State {
    def enter(user: User, renderer: Renderer): Task[Unit] = ZIO.unit
    def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
      ZIO.fail(VkApi.VkApiError(901, "Can't send messages for users without permission"))
  }

  private final class Queue(tasks: List[ScheduledTask], done: Ref[List[Long]], dead: Ref[List[Long]],
                            bumped: Ref[List[Long]]) extends ScheduledTaskDao {
    def insert(userId: UserId, fireAt: Long, kind: TaskKind, expectedState: StateType, action: String,
               createdAt: Long): Task[Long] = ZIO.succeed(0L)
    def claimDue(now: Long, limit: Int): Task[List[ScheduledTask]] = ZIO.succeed(tasks)
    def markDone(id: Long): Task[Unit]                             = done.update(_ :+ id)
    def markDead(id: Long): Task[Unit]                             = dead.update(_ :+ id)
    def bumpAttempts(id: Long): Task[Int]                          = bumped.update(_ :+ id).as(1)
    def cancel(userId: UserId, kind: TaskKind): Task[Int]          = ZIO.succeed(0)
  }

  private def task(id: Long, user: User, fireAt: Long, label: String): ScheduledTask =
    ScheduledTask(id, user.userId, fireAt, TaskKind.Construction, StateType.Construction, label,
      TaskStatus.Pending, 0, 0L)

  private def handler(state: State) =
    for {
      heroDao  <- TestHeroDao.make
      _        <- ZIO.foreachDiscard(List(first, second))(u =>
                    heroDao.insertHero(TestFixtures.hero(u.userId, state = StateType.Construction)))
      heroRepo  = TestHeroRepository.backedBy(heroDao)
      userRepo <- TestUserRepository.withUsers(first, second)
      api      <- TestApi.make
      content  <- ZIO.attempt(SceneContent.load())
      lock     <- PlayerLock.make
      payouts   = pangea.service.payout.Payouts(TestPayoutDao.empty, heroDao, content)
      parcels   = pangea.service.parcel.Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
      transfers = pangea.service.parcel.Transfers(TestInventoryRepository.accepting, userRepo, parcels,
                    new TestPlayers, content, heroDao, TestBankRepository.empty, payouts, lock)
    } yield new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers,
      Map(StateType.Construction -> state), AdminPanel.disabled, content, lock)

  override def spec = suite("Поллер отложенных задач")(

    test("игроки — параллельно, задачи одного игрока — по очереди в порядке fire_at") {
      for {
        log           <- Ref.make(List.empty[String])
        secondStarted <- Promise.make[Nothing, Unit]
        h             <- handler(new Gate(log, secondStarted))
        done          <- Ref.make(List.empty[Long])
        none          <- Ref.make(List.empty[Long])
        tasks          = List(task(1L, first, 1L, "a1"), task(2L, first, 2L, "a2"), task(3L, second, 3L, "b1"))
        _             <- new SchedulerPollerLive(new Queue(tasks, done, none, none), h).poll
        events        <- log.get
        d             <- done.get
      } yield assertTrue(d.sorted == List(1L, 2L, 3L)) &&
              assertTrue(events.indexOf("end a1") < events.indexOf("start a2")) &&
              assertTrue(events.indexOf("start b1") < events.indexOf("end a1"))
    } @@ TestAspect.timeout(10.seconds),

    test("игрок закрыл сообщения — задача мертва сразу, без пяти попыток") {
      for {
        h      <- handler(Closed)
        done   <- Ref.make(List.empty[Long])
        dead   <- Ref.make(List.empty[Long])
        bumped <- Ref.make(List.empty[Long])
        _      <- new SchedulerPollerLive(new Queue(List(task(1L, first, 1L, "")), done, dead, bumped), h).poll
        d      <- done.get
        x      <- dead.get
        b      <- bumped.get
      } yield assertTrue(x == List(1L) && b.isEmpty && d.isEmpty)
    }
  )
}
