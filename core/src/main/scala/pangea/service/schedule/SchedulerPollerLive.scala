package pangea.service.schedule

import pangea.dao.schedule.ScheduledTaskDao
import pangea.model.schedule.ScheduledTask
import pangea.service.state.{StateHandler, UserAction}
import zio.{Duration, Task, ZIO}

import java.util.concurrent.TimeUnit

class SchedulerPollerLive(dao: ScheduledTaskDao, handler: StateHandler) extends SchedulerPoller {
  import SchedulerPollerLive._

  override def poll: Task[Unit] =
    for {
      now <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      due <- dao.claimDue(now, BatchLimit)
      // Игроки — параллельно: медленный ответ ВК одному не держит остальных.
      // Задачи одного игрока — по очереди, в порядке fire_at (claimDue уже
      // отсортировал, groupBy порядок внутри группы сохраняет).
      _   <- ZIO
               .foreachParDiscard(due.groupBy(_.userId).values)(ZIO.foreachDiscard(_)(process))
               .withParallelism(Parallelism)
    } yield ()

  override def start: Task[Unit] =
    (poll.catchAllCause(c => ZIO.logErrorCause("Scheduler poll failed", c)) *> ZIO.sleep(PollInterval)).forever

  /** Исполнить одну задачу и проставить итоговый статус. Поломка — и ошибка,
    * и падение кода (defect) — это попытка: причина целиком в лог, после
    * [[MaxAttempts]] задача мертва. Раньше падение кода проходило мимо: задача
    * оставалась в очереди навсегда, а те, что за ней в партии, не исполнялись. */
  private def process(task: ScheduledTask): Task[Unit] = {
    val ua = UserAction(text = "", payload = Some(task.action))
    handler
      .runScheduled(task.userId, task.expectedState, ua)
      .foldCauseZIO(
        cause =>
          if (cause.isInterruptedOnly) ZIO.refailCause(cause)
          else dao.bumpAttempts(task.id).flatMap { attempts =>
            ZIO.logErrorCause(s"Scheduled task ${task.id} (${task.kind}) failed, attempt $attempts", cause) *>
              ZIO.when(attempts >= MaxAttempts)(dao.markDead(task.id)).unit
          },
        _ => dao.markDone(task.id)
      )
  }
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
