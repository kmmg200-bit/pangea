package pangea.service.schedule

import pangea.dao.schedule.ScheduledTaskDao
import pangea.service.state.StateHandler
import zio.{Task, ZLayer}

/**
 * Execute-сторона планировщика: фоновый поллер. По `fireAt` забирает готовые задачи
 * и исполняет их через [[StateHandler.runScheduled]] под локом игрока. Отделён от
 * [[Scheduler]] (write-стороны), чтобы состояния, планирующие задачи, не зависели от
 * `StateHandler` — иначе слои зацикливаются.
 */
trait SchedulerPoller {

  /** Один тик: забрать готовые задачи и исполнить. Крутит его `Main` — циклом
    * фона ([[pangea.service.Background.repeat]]) раз в
    * [[SchedulerPollerLive.PollInterval]]. */
  def poll: Task[Unit]
}

object SchedulerPoller {
  val live: ZLayer[ScheduledTaskDao with StateHandler, Nothing, SchedulerPoller] =
    ZLayer.fromFunction(new SchedulerPollerLive(_, _))
}
