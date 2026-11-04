package pangea.service.inbox

import io.circe.Json
import pangea.dao.vk.VkInboxDao
import pangea.metrics.GameMetrics
import pangea.model.vk.InboxEvent
import pangea.service.{Background, ErrorLog}
import zio.{Duration, Task, ZIO, ZLayer, durationInt}

import java.util.concurrent.TimeUnit

/** Что делать с событием из очереди — ход игрока, команда беседы, событие
  * группы, — и что оставить от него в базе после обработки. */
trait InboxHandler {
  def handle(event: InboxEvent): Task[Unit]

  /** Событие без секретов: так оно и хранится неделю (и попадает в разборы). */
  def redact(event: Json): Json
}

/** Разбор очереди входящих событий ВК (`vk_inbox`): опрос раз в
  * [[InboxWorker.PollInterval]], захват с арендой, каждое событие — в своём
  * фоновом деле, без ограничения числа. Ходы одного игрока идут по порядку:
  * его следующее событие захватывается, только когда обработано предыдущее
  * (см. [[VkInboxDao.claim]]). Остановка: новых событий не берём, начатые
  * дожидается [[Background]]. */
final class InboxWorker(dao: VkInboxDao, handler: InboxHandler, background: Background) {
  import InboxWorker._

  /** Опрашивать очередь: нашлись события — сразу снова, пусто — пауза.
    * Прерывается только остановкой. */
  def run: Task[Nothing] =
    poll
      .catchAllCause(cause => ErrorLog.logged("Очередь ВК: опрос упал")(cause).as(false))
      .flatMap(found => ZIO.sleep(PollInterval).unless(found))
      .forever

  private def poll: Task[Boolean] =
    for {
      now    <- clock
      events <- dao.claim(now, now + Lease.toMillis, BatchLimit)
      _      <- ZIO.foreachDiscard(events)(event => background.fork(process(event)))
    } yield events.nonEmpty

  /** Обработать и отметить. Поломка — попытка: причина в лог, повтор на
    * следующем опросе, после [[MaxAttempts]] — `Failed`, и очередь игрока идёт
    * дальше. */
  private def process(event: InboxEvent): Task[Unit] = {
    val kept = Some(handler.redact(event.event)).filter(_ != event.event)
    (for {
      start <- clock
      _     <- GameMetrics.vkInboxWaitSeconds.update((start - event.receivedAt).max(0L) / 1000.0)
      _     <- handler.handle(event)
      now   <- clock
      _     <- dao.done(event.id, now, kept)
    } yield ()).catchAllCause { cause =>
      (ErrorLog.logged(s"Очередь ВК: событие ${event.id} (peer ${event.peerId}) не обработано")(cause) *>
        dao.bumpAttempts(event.id).flatMap(attempts =>
          ZIO.when(attempts >= MaxAttempts)(
            clock.flatMap(dao.failed(event.id, _, kept)) *> GameMetrics.vkInboxFailed.increment).unit))
        .catchAllCause(ErrorLog.logged(s"Очередь ВК: не отметить неудачу события ${event.id}"))
    }
  }
}

object InboxWorker {

  /** Как часто опрашивать очередь, когда она пуста. */
  val PollInterval: Duration = 100.millis

  /** На сколько бот захватывает событие. Бот упал посреди хода — через столько
    * событие возьмёт другой бот. Аренда не продлевается: ход дольше неё (повтор
    * отправки в ВК при 900–902 сам ждёт 5 с) может взять и второй бот, но
    * второй раз ход не сыграется — личку и беседу отсекает `last_event_id`
    * игрока, события группы пишутся по ключу. */
  val Lease: Duration = 5.seconds

  /** Сколько событий захватывать за опрос: потолок одного запроса, а не
    * обработки — остальные возьмёт следующий опрос сразу же. */
  val BatchLimit: Int = 500

  /** После скольких неудачных попыток событие — `Failed`. */
  val MaxAttempts: Int = 3

  /** Сколько хранить обработанные события — для разборов. */
  val Keep: Duration = 7.days

  val live: ZLayer[VkInboxDao with InboxHandler with Background, Nothing, InboxWorker] =
    ZLayer.fromFunction(new InboxWorker(_, _, _))

  private val clock = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
}
