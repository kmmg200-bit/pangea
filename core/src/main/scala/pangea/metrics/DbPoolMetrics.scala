package pangea.metrics

import com.zaxxer.hikari.metrics.{IMetricsTracker, MetricsTrackerFactory, PoolStats}
import zio.{UIO, ZIO}

import java.util.concurrent.atomic.AtomicReference

/** Пул соединений с базой — в метрики. Сколько ждали соединение и сколько его
  * держали, Hikari сообщает сам, на каждом соединении; занятость пула
  * (активные, свободные, ждущие) снимает [[sample]] — его зовут по таймеру. */
final class DbPoolMetrics extends MetricsTrackerFactory {

  private val stats = new AtomicReference[Option[PoolStats]](None)

  override def create(poolName: String, poolStats: PoolStats): IMetricsTracker = {
    stats.set(Some(poolStats))
    new IMetricsTracker {
      override def recordConnectionAcquiredNanos(elapsedAcquiredNanos: Long): Unit =
        GameMetrics.record(GameMetrics.dbAcquireSeconds, elapsedAcquiredNanos / 1e9)

      override def recordConnectionUsageMillis(elapsedBorrowedMillis: Long): Unit =
        GameMetrics.record(GameMetrics.dbUsageSeconds, elapsedBorrowedMillis / 1e3)

      override def recordConnectionTimeout(): Unit =
        GameMetrics.record(GameMetrics.dbTimeouts, 1L)
    }
  }

  /** Занятость пула — в метрику; пока пул не создан, молчим. */
  def sample: UIO[Unit] =
    ZIO.foreachDiscard(stats.get()) { s =>
      ZIO.foreachDiscard(List(
        "active"  -> s.getActiveConnections,
        "idle"    -> s.getIdleConnections,
        "pending" -> s.getPendingThreads,
        "total"   -> s.getTotalConnections
      )) { case (state, n) => GameMetrics.dbPool.tagged("state", state).set(n.toDouble) }
    }
}
