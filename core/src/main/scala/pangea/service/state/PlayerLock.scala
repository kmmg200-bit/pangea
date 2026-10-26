package pangea.service.state

import pangea.model.user.UserId
import zio.{Ref, Semaphore, Task, UIO, ULayer, ZLayer}

class PlayerLock(ref: Ref[Map[UserId, Semaphore]]) {
  def withLock[A](id: UserId)(action: Task[A]): Task[A] =
    getSemaphore(id).flatMap(_.withPermit(action))

  private def getSemaphore(id: UserId): UIO[Semaphore] =
    Semaphore.make(1).flatMap { fresh =>
      ref.modify { map =>
        map.get(id) match {
          case Some(existing) => (existing, map)
          case None           => (fresh, map.updated(id, fresh))
        }
      }
    }
}

object PlayerLock {
  def make: UIO[PlayerLock] =
    Ref.make(Map.empty[UserId, Semaphore]).map(new PlayerLock(_))

  /** Один на всё приложение: ходы игроков и доставка посылок получателю
    * (см. `Transfers`) должны ждать один и тот же лок. */
  val live: ULayer[PlayerLock] = ZLayer.fromZIO(make)
}
