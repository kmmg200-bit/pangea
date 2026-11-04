package pangea.service

import pangea.dao.lock.AdvisoryLocks
import pangea.metrics.GameMetrics
import zio.{Duration, Ref, Task, UIO, URLayer, ZIO, ZLayer, durationInt}

/** Ведущий бот. Одиночные фоновые дела — сверку платежей, оплату билетов за
  * активность, обход почты, уборку очереди ВК — делает только он: два бота
  * разом сделали бы их дважды. Ходы, таймеры и очередь ВК разбирают все боты.
  *
  * Ведущий — тот, кто держит advisory-лок [[Leader.Key]]. Отпускается он при
  * закрытии слоёв — уже после того, как фон доделал начатые витки, — и другой
  * бот подхватывает за [[Leader.Retry]]. Бот упал — Postgres отпускает лок
  * вместе с соединением. */
final class Leader(flag: Ref[Boolean]) {

  def isLeader: UIO[Boolean] = flag.get

  /** Виток — только у ведущего; у прочих ничего. */
  def ifLeader(tick: Task[Unit]): Task[Unit] = flag.get.flatMap(ZIO.when(_)(tick)).unit
}

object Leader {

  /** Ключ лока ведущего: отрицательный — с ключами игроков (их id) не пересекается. */
  val Key: Long = -1L

  /** Как часто не-ведущий пробует стать ведущим, а ведущий — проверяет соединение. */
  val Retry: Duration = 5.seconds

  val live: URLayer[AdvisoryLocks, Leader] =
    ZLayer.scoped {
      for {
        locks <- ZIO.service[AdvisoryLocks]
        flag  <- Ref.make(false)
        _     <- GameMetrics.leader.set(0.0)
        _     <- locks.hold(Key, Retry)(held =>
                   flag.set(held) *> GameMetrics.leader.set(if (held) 1.0 else 0.0) *>
                     ZIO.logInfo(if (held) "Ведущий: одиночные фоновые дела — у этого бота" else "Ведущий: больше не этот бот")
                 ).forkScoped
      } yield new Leader(flag)
    }
}
