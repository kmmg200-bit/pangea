package pangea.metrics

import zio.metrics.{Metric, MetricKeyType}
import zio.{Chunk, Clock, Runtime, UIO, Unsafe, ZIO}

import java.util.concurrent.TimeUnit

/** Метрики игры для Prometheus: время ходов и вызовов ВК, опоздание таймеров,
  * пул базы, ошибки. По ним в Grafana (docker/grafana) видно, что было с ботом
  * в любую минуту, — и задним числом разбирать, почему он тормозил.
  *
  * Метрики ZIO глобальны: здесь их объявления и то, как их писать. Наружу их
  * отдаёт `MetricsServer` в app. Время — в секундах, как принято в Prometheus. */
object GameMetrics {

  /** Границы для времени: от миллисекунды до минуты. Мелкие — для лока и
    * базы: с нижней границей в 10 мс они показывали лишь «меньше 10 мс». */
  private val Latency = MetricKeyType.Histogram.Boundaries.fromChunk(
    Chunk(0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.0, 3.0, 5.0, 8.0, 13.0, 20.0, 30.0, 60.0))

  /** Границы для опозданий: от полсекунды до 10 минут; до 10 секунд — через
    * секунду: доставка от ВК обычно там, а время у ВК и так с точностью до
    * секунды. */
  private val Lag = MetricKeyType.Histogram.Boundaries.fromChunk(
    Chunk(0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 12.0, 15.0, 20.0, 30.0, 60.0, 120.0, 300.0, 600.0))

  /** Границы для занятости окна частоты ВК: сколько вызовов за секунду, до
    * лимита в 20; у края — через один. */
  private val WindowFill = MetricKeyType.Histogram.Boundaries.fromChunk(
    Chunk(1.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 17.0, 18.0, 19.0, 20.0))

  /** Границы для размера пачки писем ВК: от одного письма до 25 — больше
    * `execute` не вмещает. */
  private val BatchSize = MetricKeyType.Histogram.Boundaries.fromChunk(
    Chunk(1.0, 2.0, 3.0, 5.0, 8.0, 10.0, 13.0, 16.0, 20.0, 24.0, 25.0))

  // ── Ходы ─────────────────────────────────────────────────────────────────────

  val turnSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_turn_seconds", "Ход игрока целиком: от события ВК до последнего ответа", Latency)

  val lockWaitSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_turn_lock_wait_seconds", "Сколько ход ждал лок игрока", Latency)

  /** С меткой `state` — состоянием героя до хода: так видно, какая сцена медленная. */
  val stateActionSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_state_action_seconds", "Ход в сцене, с показом, по состоянию героя", Latency)

  // ── ВК ───────────────────────────────────────────────────────────────────────

  /** С метками `kind` — message, chat, group — и `source` — callback, longpoll:
    * каким путём пришло (у group его нет, она только по Callback). Сообщение
    * считается один раз — пришедшее первым: копия другим путём не событие. */
  val vkEvents: Metric.Counter[Long] =
    Metric.counter("pangea_vk_events_total", "События ВК по виду")

  /** С метками `kind` — message (личка — ходы игроков), chat (общая беседа), —
    * `source` — callback, longpoll — и `first`: yes — пришло первым и пошло в
    * ход, no — копия, опоздавшая другим путём. Игрок ждёт столько, сколько
    * first="yes", а пути сравниваются по всем приходам. */
  val vkDeliveryLagSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_vk_delivery_lag_seconds", "От сообщения игрока до прихода события к нам", Lag)

  /** С метками `method` и `outcome` (ok, error); одна попытка — одно значение. */
  val vkApiSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_vk_api_seconds", "Вызов API ВК, одна попытка", Latency)

  /** С метками `method` и `code`: номер ошибки ВК (6 — слишком часто, 901 —
    * игрок закрыл сообщения, 911 — клавиатура), `http_<статус>` — ответ не 2xx,
    * `not_json` — 2xx не JSON, `net` — ответа нет (сеть, тайм-аут). Одна
    * неудачная попытка — одна единица. */
  val vkApiErrors: Metric.Counter[Long] =
    Metric.counter("pangea_vk_api_errors_total", "Ошибки вызовов API ВК по коду, одна попытка")

  val vkSendFailures: Metric.Counter[Long] =
    Metric.counter("pangea_vk_send_failures_total", "Сообщения игрокам, не ушедшие и после повторов (как send_failures)")

  // Окно частоты ключа сообщества (VkRateLimit): у всех метка `community` —
  // main, guilds. Окно у каждого бота своё.

  /** Сколько вызов API ВК ждал места в окне частоты. Почти всегда ноль; не
    * ноль — упёрлись в лимит. Число значений — сколько вызовов прошло окно. */
  val vkLimiterWaitSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_vk_limiter_wait_seconds", "Вызов API ВК: ожидание места в окне частоты", Latency)

  /** Сколько вызовов было за последнюю секунду вместе с этим, когда он занял
    * место: пики короче опроса Prometheus видны здесь. 20 — окно полно. */
  val vkLimiterWindowCalls: Metric.Histogram[Double] =
    Metric.histogram("pangea_vk_limiter_window_calls", "Вызов API ВК: вызовов за последнюю секунду вместе с ним", WindowFill)

  val vkLimiterWaiting: Metric.Gauge[Double] =
    Metric.gauge("pangea_vk_limiter_waiting", "Вызовов API ВК ждут места в окне частоты или занимают его")

  /** Больше всего вызовов за одну секунду за последние 20 секунд — посекундная
    * частота, которую Prometheus сам не видит: опрашивает раз в 15 секунд. */
  val vkLimiterPeakCalls: Metric.Gauge[Double] =
    Metric.gauge("pangea_vk_limiter_peak_calls", "Вызовов API ВК за одну секунду: пик за последние 20 секунд")

  /** Сколько писем ушло одним запросом (VkBatcher), метка `community`: 1 —
    * обычный messages.send, больше — execute. Сумма — сколько писем ушло,
    * число значений — сколькими запросами. Повторы писем идут мимо пачек и
    * здесь не видны. */
  val vkBatchLetters: Metric.Histogram[Double] =
    Metric.histogram("pangea_vk_batch_letters", "Писем ВК одним запросом: 1 — messages.send, больше — execute", BatchSize)

  /** Сколько письмо стояло в очереди пачек (VkBatcher) — от постановки до
    * отправки, метка `community`. Почти всегда ноль; не ноль — окно частоты
    * полно, и игрок видит ответ позже на это время. Окно ждёт уже пачка, а не
    * письмо: [[vkLimiterWaitSeconds]] письма в очереди не видит. */
  val vkBatchWaitSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_vk_batch_wait_seconds", "Письмо ВК: ожидание в очереди пачек до отправки", Latency)

  /** Больше всего писем в очереди пачек за последние 20 секунд, метка
    * `community`: очередь живёт миллисекунды, и опрос раз в 15 секунд её
    * почти не застаёт — пик за 20 секунд попадёт хотя бы в один опрос. */
  val vkBatchQueued: Metric.Gauge[Double] =
    Metric.gauge("pangea_vk_batch_queued", "Писем ВК в очереди пачек: пик за последние 20 секунд")

  // ── Несколько ботов: локи игроков и ведущий ─────────────────────────────────

  /** Пул соединений под локи игроков (AdvisoryLocks) по состоянию: active —
    * заняты ходами сейчас, idle — свободны, pending — ходы ждут соединения
    * (пул кончился), max — размер пула. Снимается раз в секунду. */
  val lockPool: Metric.Gauge[Double] =
    Metric.gauge("pangea_lock_pool_connections", "Пул соединений под локи игроков по состоянию")

  /** 1 — этот бот ведущий (делает одиночные фоновые дела), 0 — нет. */
  val leader: Metric.Gauge[Double] =
    Metric.gauge("pangea_leader", "Ведущий ли этот бот")

  // ── Очередь входящих событий ВК (vk_inbox) ────────────────────────────────────

  /** От записи события в очередь до начала его обработки: опрос, очередь
    * игрока (его следующее нажатие ждёт предыдущее). */
  val vkInboxWaitSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_vk_inbox_wait_seconds", "Событие ВК в очереди: от записи до начала хода", Latency)

  val vkInboxPending: Metric.Gauge[Double] =
    Metric.gauge("pangea_vk_inbox_pending", "Событий ВК в очереди: ждут и в работе")

  val vkInboxFailed: Metric.Counter[Long] =
    Metric.counter("pangea_vk_inbox_failed_total", "События ВК, не обработанные и после повторов")

  // ── Таймеры ──────────────────────────────────────────────────────────────────

  val schedulerLagSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_scheduler_lag_seconds", "Насколько таймер опоздал против fire_at", Lag)

  /** С меткой `kind` — видом таймера. */
  val schedulerTaskSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_scheduler_task_seconds", "Исполнение таймера по виду", Latency)

  /** С метками `kind` и `outcome`: executed, skipped, failed, dead, unreachable
    * (игрок закрыл сообщения — задача снята без повторов). */
  val schedulerTasks: Metric.Counter[Long] =
    Metric.counter("pangea_scheduler_tasks_total", "Таймеры по виду и исходу")

  val schedulerDue: Metric.Gauge[Double] =
    Metric.gauge("pangea_scheduler_due", "Сколько таймеров взял последний тик")

  // ── Фон, ошибки, база ────────────────────────────────────────────────────────

  val backgroundRunning: Metric.Gauge[Double] =
    Metric.gauge("pangea_background_running", "Фоновых дел в работе: ходы, доставки, циклы")

  val errors: Metric.Counter[Long] =
    Metric.counter("pangea_errors_total", "Поломки, записанные в лог через ErrorLog")

  val dbAcquireSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_db_acquire_seconds", "Ожидание соединения из пула базы", Latency)

  val dbUsageSeconds: Metric.Histogram[Double] =
    Metric.histogram("pangea_db_usage_seconds", "Сколько соединение было занято, то есть транзакция", Latency)

  val dbTimeouts: Metric.Counter[Long] =
    Metric.counter("pangea_db_timeouts_total", "Не дождались соединения из пула")

  /** С меткой `state`: active, idle, pending, total. */
  val dbPool: Metric.Gauge[Double] =
    Metric.gauge("pangea_db_pool_connections", "Соединения пула базы по состоянию")

  // ── Как писать ───────────────────────────────────────────────────────────────

  /** Засечь время эффекта — и при успехе, и при поломке или прерывании: медленный
    * ход, который в итоге упал, важен не меньше удачного. */
  def timed[R, E, A](histogram: Metric.Histogram[Double])(zio: ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.suspendSucceed {
      val start = java.lang.System.nanoTime()
      zio.onExit(_ => histogram.update(secondsSince(start)))
    }

  /** Метка времени для [[since]]. */
  val now: UIO[Long] = ZIO.succeed(java.lang.System.nanoTime())

  /** Записать, сколько прошло с `start` (из [[now]]). */
  def since(histogram: Metric.Histogram[Double], start: Long): UIO[Unit] =
    ZIO.suspendSucceed(histogram.update(secondsSince(start)))

  /** Одна попытка вызова API ВК: время и исход — по методу. */
  def vkCall[R, E, A](method: String)(call: ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.suspendSucceed {
      val start = java.lang.System.nanoTime()
      call.onExit(exit =>
        vkApiSeconds
          .tagged("method", method)
          .tagged("outcome", if (exit.isSuccess) "ok" else "error")
          .update(secondsSince(start)))
    }

  /** Неудачная попытка вызова API ВК: метод и код ошибки (см. [[vkApiErrors]]). */
  def vkError(method: String, code: String): UIO[Unit] =
    vkApiErrors.tagged("method", method).tagged("code", code).increment

  /** Сообщение из ВК, пришедшее путём `source`: счётчик — если пришло первым,
    * и — если ВК прислал время отправки (`date`, секунды) — сколько шло до нас. */
  def vkMessage(kind: String, source: String, first: Boolean, sentAt: Option[Long]): UIO[Unit] =
    ZIO.when(first)(vkEvents.tagged("kind", kind).tagged("source", source).increment) *>
      ZIO.foreachDiscard(sentAt) { at =>
        Clock.currentTime(TimeUnit.MILLISECONDS).flatMap(ms =>
          vkDeliveryLagSeconds.tagged("kind", kind).tagged("source", source).tagged("first", if (first) "yes" else "no")
            .update(((ms - at * 1000L).max(0L)) / 1000.0))
      }

  /** Записать из кода вне ZIO — из колбэков Hikari, на его потоке. Запись в
    * метрику синхронна и не падает, так что эффект исполняется тут же. */
  def record[In](metric: Metric[Any, In, Any], value: In): Unit =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(metric.update(value))).getOrElse(_ => ())

  private def secondsSince(start: Long): Double = (java.lang.System.nanoTime() - start) / 1e9
}
