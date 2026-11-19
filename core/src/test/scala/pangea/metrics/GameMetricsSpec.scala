package pangea.metrics

import com.zaxxer.hikari.metrics.PoolStats
import zio.metrics.{Metric, MetricKeyType}
import zio.test._
import zio.{Chunk, Clock, Promise, ZIO, durationInt}

import java.util.concurrent.TimeUnit

/** Метрики пишутся и при поломке: медленный ход, который упал, важен не меньше
  * удачного. Вызовы ВК делятся по исходу, пул базы — по состоянию. */
object GameMetricsSpec extends ZIOSpecDefault {

  private val bounds = MetricKeyType.Histogram.Boundaries.fromChunk(Chunk(0.1, 1.0))

  /** Пул с заданной занятостью: в Hikari его считает сам пул. */
  private final class FixedStats(active: Int, idle: Int, pending: Int) extends PoolStats(0L) {
    override protected def update(): Unit = {
      activeConnections = active
      idleConnections = idle
      pendingThreads = pending
      totalConnections = active + idle
    }
  }

  override def spec = suite("Метрики игры")(

    test("время пишется и при успехе, и при поломке, и при прерывании") {
      val h = Metric.histogram("test_timed_seconds", bounds)
      for {
        _       <- GameMetrics.timed(h)(ZIO.unit)
        _       <- GameMetrics.timed(h)(ZIO.fail("поломка")).ignore
        // Прерываем, только когда замер начался: под нагрузкой файбер иначе
        // прерывался, не начав, — и замера не было вовсе.
        started <- Promise.make[Nothing, Unit]
        fiber   <- GameMetrics.timed(h)(started.succeed(()) *> ZIO.never).fork
        _       <- started.await *> fiber.interrupt
        state   <- h.value
      } yield assertTrue(state.count == 3L)
    },

    test("вызов ВК — по методу и исходу") {
      val ok    = GameMetrics.vkApiSeconds.tagged("method", "test.call").tagged("outcome", "ok")
      val error = GameMetrics.vkApiSeconds.tagged("method", "test.call").tagged("outcome", "error")
      for {
        _ <- GameMetrics.vkCall("test.call")(ZIO.unit)
        _ <- GameMetrics.vkCall("test.call")(ZIO.unit)
        _ <- GameMetrics.vkCall("test.call")(ZIO.fail("ВК не ответил")).ignore
        o <- ok.value
        e <- error.value
      } yield assertTrue(o.count == 2L && e.count == 1L)
    },

    test("доставка от ВК — по виду, пути и первенству; копия не считается событием") {
      def lag(first: String) =
        GameMetrics.vkDeliveryLagSeconds.tagged("kind", "chat").tagged("source", "longpoll").tagged("first", first)
      val events = GameMetrics.vkEvents.tagged("kind", "chat").tagged("source", "longpoll")
      for {
        first0  <- lag("yes").value
        copy0   <- lag("no").value
        events0 <- events.value
        now     <- Clock.currentTime(TimeUnit.SECONDS)
        _       <- GameMetrics.vkMessage("chat", "longpoll", first = true, Some(now - 3L))
        _       <- GameMetrics.vkMessage("chat", "longpoll", first = false, Some(now - 9L))
        _       <- GameMetrics.vkMessage("chat", "longpoll", first = true, None)
        first   <- lag("yes").value
        copy    <- lag("no").value
        events1 <- events.value
      } yield assertTrue(first.count == first0.count + 1 && copy.count == copy0.count + 1) &&
              assertTrue(events1.count == events0.count + 2)
    },

    test("пул базы: ожидание, занятость и таймауты — от Hikari") {
      val pool    = new DbPoolMetrics
      val tracker = pool.create("test", new FixedStats(active = 3, idle = 7, pending = 2))
      for {
        acquire0 <- GameMetrics.dbAcquireSeconds.value
        timeout0 <- GameMetrics.dbTimeouts.value
        _        <- ZIO.succeed(tracker.recordConnectionAcquiredNanos(5.millis.toNanos))
        _        <- ZIO.succeed(tracker.recordConnectionTimeout())
        _        <- pool.sample
        acquire  <- GameMetrics.dbAcquireSeconds.value
        timeout  <- GameMetrics.dbTimeouts.value
        active   <- GameMetrics.dbPool.tagged("state", "active").value
        pending  <- GameMetrics.dbPool.tagged("state", "pending").value
        total    <- GameMetrics.dbPool.tagged("state", "total").value
      } yield assertTrue(acquire.count == acquire0.count + 1 && timeout.count == timeout0.count + 1) &&
              assertTrue(active.value == 3.0 && pending.value == 2.0 && total.value == 10.0)
    }
  )
}
