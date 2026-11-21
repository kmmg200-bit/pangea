package pangea.service.sender.vk

import pangea.metrics.GameMetrics
import pangea.model.vk.Community
import zio.{Duration, Ref, Semaphore, UIO, ZIO, durationInt}

import java.util.concurrent.TimeUnit
import scala.collection.immutable.Queue

/** Окно частоты одного ключа сообщества: не больше [[VkRateLimit.PerSecond]]
  * вызовов API ВК за любую секунду — так ВК считает свой лимит (на ключ
  * сообщества, на все методы разом; сверх — ошибка 6). Вызов сверх окна ждёт,
  * пока из него выйдет самый ранний, и не теряется; ждущие проходят по
  * очереди — семафор пускает их по одному в порядке прихода.
  *
  * ВК всё же ответил «слишком часто» ([[tooFast]]) — окно закрыто на
  * [[VkRateLimit.Pause]] для всех вызовов этим ключом: тем же токеном пишет
  * кто-то ещё (второй бот при выкладке, сборщик), и повтор без паузы получил
  * бы ту же ошибку.
  *
  * Окно — в памяти бота. При выкладке ботов двое и окна у них свои, а
  * нагрузку они делят: вдвоём выйти за лимит можно, только если она и так у
  * края, — это минута-две, и ошибку 6 тогда лечит пауза. */
final class VkRateLimit private (
    community: String,
    gate:      Semaphore,
    sent:      Ref[Queue[Long]],
    closed:    Ref[Long],
    waiting:   Ref[Int],
    peaks:     Ref[Map[Long, Int]]
) {
  import VkRateLimit._

  /** Выполнить `call`, когда в окне есть место. */
  def apply[R, E, A](call: ZIO[R, E, A]): ZIO[R, E, A] = slot *> call

  /** ВК ответил «слишком часто»: окно закрыто на паузу. */
  def tooFast: UIO[Unit] = now.flatMap(t => closed.update(_ max (t + Pause.toMillis)))

  /** Пик за секунду — в метрику; бот зовёт раз в секунду (см. `Api.vk`). */
  def publish: UIO[Unit] =
    peak.flatMap(n => GameMetrics.vkLimiterPeakCalls.tagged("community", community).set(n.toDouble))

  /** Больше всего вызовов за одну секунду за последние [[PeakSpan]]: сколько
    * было в окне, когда вызов занял место, — наибольшее по секундам. Секунды
    * старше — забыты. */
  private[vk] def peak: UIO[Int] =
    now.flatMap(t =>
      peaks.modify { bySecond =>
        val fresh = bySecond.filter { case (second, _) => second * 1000L >= t - PeakSpan.toMillis }
        (fresh.values.maxOption.getOrElse(0), fresh)
      })

  /** Занять место в окне — дождаться его. Сам вызов — следом, у вызывающего:
    * так пачка писем ([[VkBatcher]]) собирается, пока ждёт места. */
  private[vk] def slot: UIO[Unit] =
    for {
      start <- now
      _     <- (count(1) *> gate.withPermit(take)).ensuring(count(-1))
      end   <- now
      _     <- GameMetrics.vkLimiterWaitSeconds.tagged("community", community).update((end - start) / 1000.0)
    } yield ()

  /** Занять место: окно открыто и в нём есть место — сразу; нет — ждать и
    * смотреть снова (за время ожидания окно могли и закрыть). `sent` — когда
    * выданы места за последнюю секунду, не больше [[PerSecond]]; `closed` — до
    * какого времени окно закрыто после ошибки 6. */
  private def take: UIO[Unit] =
    for {
      t      <- now
      until  <- closed.get
      recent <- sent.get.map(_.dropWhile(_ <= t - Window.toMillis))
      full    = recent.headOption.filter(_ => recent.sizeIs >= PerSecond).map(_ + Window.toMillis - t)
      delay   = full.getOrElse(0L) max (until - t)
      _      <- if (delay > 0) ZIO.sleep(Duration.fromMillis(delay)) *> take
                else sent.set(recent.enqueue(t)) *> taken(t, recent.size + 1)
    } yield ()

  /** Место занято в `t`, в окне теперь `calls` вызовов: в метрику и в пик секунды. */
  private def taken(t: Long, calls: Int): UIO[Unit] =
    peaks.update(bySecond => bySecond.updated(t / 1000L, bySecond.getOrElse(t / 1000L, 0) max calls)) *>
      GameMetrics.vkLimiterWindowCalls.tagged("community", community).update(calls.toDouble)

  private def count(delta: Int): UIO[Unit] =
    waiting.updateAndGet(_ + delta).flatMap(n => GameMetrics.vkLimiterWaiting.tagged("community", community).set(n.toDouble))
}

object VkRateLimit {

  /** Сколько вызовов за секунду ВК пускает на ключ сообщества
    * (dev.vk.com/ru/api/api-requests). Решено брать впритык, без запаса:
    * редкие вызовы тем же токеном мимо окна — Long Poll, сборщик, скрипт
    * бэкапа — могут дать ошибку 6, её лечит [[Pause]]. */
  val PerSecond: Int = 20

  /** За какое время ВК считает лимит. */
  val Window: Duration = 1.second

  /** На сколько закрыть окно, когда ВК ответил «слишком часто», — на его окно. */
  val Pause: Duration = 1.second

  /** За сколько последних секунд пик за секунду идёт в метрику: чуть больше
    * опроса Prometheus (15 с) и снимка метрик (5 с) — каждый пик попадёт хотя
    * бы в один опрос. */
  val PeakSpan: Duration = 20.seconds

  def make(community: Community): UIO[VkRateLimit] =
    for {
      gate    <- Semaphore.make(1)
      sent    <- Ref.make(Queue.empty[Long])
      closed  <- Ref.make(0L)
      waiting <- Ref.make(0)
      peaks   <- Ref.make(Map.empty[Long, Int])
    } yield new VkRateLimit(label(community), gate, sent, closed, waiting, peaks)

  /** Метка сообщества в метриках. */
  private[vk] def label(community: Community): String = community match {
    case Community.Main   => "main"
    case Community.Guilds => "guilds"
  }

  private val now: UIO[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
}
