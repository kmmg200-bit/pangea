package pangea.service.state

import pangea.dao.lock.AdvisoryLocks
import pangea.model.user.UserId
import zio.{Ref, Semaphore, Task, UIO, URLayer, ZIO, ZLayer}

/** Лок игрока: всё, что меняет героя — ход, таймер, посылка, выплата, —
  * идёт под ним по одному. Два уровня: семафор в памяти — бесплатно внутри
  * одного бота — и advisory-лок Postgres по id игрока — между ботами, когда их
  * два (выкладка «новый рядом со старым»). Бот упал — его локи в Postgres
  * отпускаются вместе с соединением. */
class PlayerLock(ref: Ref[Map[UserId, Semaphore]], shared: AdvisoryLocks) {
  def withLock[A](id: UserId)(action: Task[A]): Task[A] =
    getSemaphore(id).flatMap(_.withPermit(shared.withLock(PlayerLock.key(id))(action)))

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

  /** Ключ advisory-лока игрока — его id: id положительные, служебные ключи
    * (см. `Leader`) — отрицательные, не пересекаются. */
  def key(id: UserId): Long = id.value

  /** Лок только в памяти — для тестов и одного процесса. */
  def make: UIO[PlayerLock] = make(AdvisoryLocks.none)

  def make(shared: AdvisoryLocks): UIO[PlayerLock] =
    Ref.make(Map.empty[UserId, Semaphore]).map(new PlayerLock(_, shared))

  /** Один на всё приложение: ходы игроков и доставка посылок получателю
    * (см. `Transfers`) должны ждать один и тот же лок. */
  val live: URLayer[AdvisoryLocks, PlayerLock] = ZLayer.fromZIO(ZIO.serviceWithZIO[AdvisoryLocks](make))
}
