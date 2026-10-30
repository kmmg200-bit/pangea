package pangea.service.schedule

import pangea.dao.schedule.ScheduledTaskDao
import pangea.metrics.GameMetrics
import pangea.model.schedule.ScheduledTask
import pangea.service.state.{StateHandler, UserAction}
import zio.{Duration, Task, UIO, ZIO}

import java.util.concurrent.TimeUnit

class SchedulerPollerLive(dao: ScheduledTaskDao, handler: StateHandler) extends SchedulerPoller {
  import SchedulerPollerLive._

  override def poll: Task[Unit] =
    for {
      now <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      due <- dao.claimDue(now, BatchLimit)
      _   <- GameMetrics.schedulerDue.set(due.size.toDouble)
      // Игроки — параллельно: медленный ответ ВК одному не держит остальных.
      // Задачи одного игрока — по очереди, в порядке fire_at (claimDue уже
      // отсортировал, groupBy порядок внутри группы сохраняет).
      _   <- ZIO
               .foreachParDiscard(due.groupBy(_.userId).values)(ZIO.foreachDiscard(_)(process))
               .withParallelism(Parallelism)
    } yield ()

  /** Исполнить одну задачу и проставить итоговый статус. Поломка — и ошибка,
    * и падение кода (defect) — это попытка: причина целиком в лог, после
    * [[MaxAttempts]] задача мертва. Раньше падение кода проходило мимо: задача
    * оставалась в очереди навсегда, а те, что за ней в партии, не исполнялись. */
  private def process(task: ScheduledTask): Task[Unit] = {
    val ua   = UserAction(text = "", payload = Some(task.action))
    val kind = task.kind.entryName
    for {
      // Опоздание — к началу исполнения: тик раз в 5 секунд, плюс очередь
      // задач того же игрока и ожидание его лока.
      now <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      _   <- GameMetrics.schedulerLagSeconds.update((now - task.fireAt).max(0L) / 1000.0)
      _   <- GameMetrics.timed(GameMetrics.schedulerTaskSeconds.tagged("kind", kind)) {
               handler.runScheduled(task.userId, task.expectedState, ua)
             }.foldCauseZIO(
               cause =>
                 if (cause.isInterruptedOnly) ZIO.refailCause(cause)
                 else dao.bumpAttempts(task.id).flatMap { attempts =>
                   val dead = attempts >= MaxAttempts
                   ZIO.logErrorCause(s"Scheduled task ${task.id} (${task.kind}) failed, attempt $attempts", cause) *>
                     ZIO.when(dead)(dao.markDead(task.id)) *>
                     outcome(kind, if (dead) "dead" else "failed")
                 },
               result =>
                 dao.markDone(task.id) *>
                   outcome(kind, result match {
                     case StateHandler.RunResult.Executed => "executed"
                     case StateHandler.RunResult.Skipped  => "skipped"
                   })
             )
    } yield ()
  }

  private def outcome(kind: String, outcome: String): UIO[Unit] =
    GameMetrics.schedulerTasks.tagged("kind", kind).tagged("outcome", outcome).increment
}

object SchedulerPollerLive {
  /** Сколько задач забирать за тик. */
  val BatchLimit: Int = 64
  /** После скольких неудачных попыток задача помечается `Dead`. */
  val MaxAttempts: Int = 5
  /** Сколько игроков исполнять одновременно: в пределах пула БД (Hikari — 10)
    * и лимитов ВК на отправку сообщений. */
  val Parallelism: Int = 8
  /** Период опроса очереди. */
  val PollInterval: Duration = Duration.fromSeconds(5)
}
