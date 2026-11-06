package pangea.engine

import pangea.model.GameEvent
import pangea.model.user.UserId
import zio.Task

trait Journal {
  def append(event: GameEvent): Task[Unit]

  /** Сколько событий `eventType` у игрока с момента `sinceMs` (мс эпохи)
    * включительно — например, игр в лотерею Фета за сегодня. */
  def count(userId: UserId, eventType: String, sinceMs: Long): Task[Long]
}
