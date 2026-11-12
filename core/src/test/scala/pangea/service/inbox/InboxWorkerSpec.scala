package pangea.service.inbox

import io.circe.Json
import pangea.dao.vk.VkInboxDao
import pangea.model.vk.{InboxEvent, InboxKind}
import pangea.service.Background
import pangea.service.sender.vk.LongPollCursor
import zio.test._
import zio.{Ref, Task, ZIO, durationInt}

/** Разбор очереди: обработанное — `Done` и без секретов, сломанное — попытки и
  * `Failed` после трёх, упавший опрос цикл не останавливает. */
object InboxWorkerSpec extends ZIOSpecDefault {

  private def event(id: Long, text: String): InboxEvent =
    InboxEvent(id, 7L, InboxKind.Message, "callback",
      Json.obj("object" -> Json.obj("message" -> Json.obj("text" -> Json.fromString(text)))), None, 0L, 0)

  /** Очередь в памяти: захват отдаёт ждущие; неудача возвращает событие в
    * очередь (аренда снята), пока не исчерпаны попытки. */
  private final class Queue(
    all:        Map[Long, InboxEvent],
    waiting:    Ref[List[InboxEvent]],
    brokenPoll: Ref[Int],
    done:       Ref[List[(Long, Option[Json])]],
    failed:     Ref[List[Long]],
    attempts:   Ref[Map[Long, Int]]
  ) extends VkInboxDao {
    def accept(k: String, p: Long, kind: InboxKind, s: String, e: Json, at: Option[Long], now: Long): Task[Boolean] = ZIO.succeed(true)
    def claim(now: Long, until: Long, limit: Int): Task[List[InboxEvent]] =
      brokenPoll.getAndUpdate(n => (n - 1).max(0)).flatMap(n =>
        if (n > 0) ZIO.fail(new RuntimeException("база недоступна")) else waiting.getAndSet(Nil))
    def done(id: Long, now: Long, event: Option[Json]): Task[Unit]   = done.update(_ :+ (id -> event))
    def failed(id: Long, now: Long, event: Option[Json]): Task[Unit] = failed.update(_ :+ id)
    def bumpAttempts(id: Long): Task[Int] =
      attempts.updateAndGet(m => m.updated(id, m.getOrElse(id, 0) + 1)).map(_(id)).tap(n =>
        ZIO.when(n < InboxWorker.MaxAttempts)(waiting.update(_ ++ all.get(id).toList)))
    def pending: Task[Long]                     = ZIO.succeed(0L)
    def forget(before: Long): Task[Int]         = ZIO.succeed(0)
    def load: Task[Option[String]]              = ZIO.none
    def save(ts: String, now: Long): Task[Unit] = ZIO.unit
    def guildsCursor: LongPollCursor            = this
  }

  /** Ход: событие с текстом «boom» ломается, прочие — проходят; пароль «секрет»
    * маскируется. */
  private final class Handler(handled: Ref[List[Long]]) extends InboxHandler {
    def handle(e: InboxEvent): Task[Unit] =
      handled.update(_ :+ e.id) *>
        ZIO.when(InboxEvent.mapText(e.event)(identity).noSpaces.contains("boom"))(ZIO.fail(new RuntimeException("boom"))).unit
    def redact(e: Json): Json = InboxEvent.mapText(e)(t => if (t == "секрет") "***" else t)
  }

  private def worker(events: List[InboxEvent], brokenPolls: Int) =
    for {
      waiting  <- Ref.make(events)
      broken   <- Ref.make(brokenPolls)
      done     <- Ref.make(List.empty[(Long, Option[Json])])
      failed   <- Ref.make(List.empty[Long])
      attempts <- Ref.make(Map.empty[Long, Int])
      handled  <- Ref.make(List.empty[Long])
      bg       <- Background.make(1.second)
      queue     = new Queue(events.map(e => e.id -> e).toMap, waiting, broken, done, failed, attempts)
    } yield (new InboxWorker(queue, new Handler(handled), bg), done, failed, attempts, handled)

  override def spec = suite("Очередь ВК: разбор")(

    test("обработано — Done; в хранимом событии пароль замаскирован, обычное — как было") {
      for {
        w                                 <- worker(List(event(1, "Атака"), event(2, "секрет")), brokenPolls = 0)
        (run, done, _, _, handled)         = w
        fiber                             <- run.run.fork
        marks                             <- done.get.repeatUntil(_.size == 2)
        _                                 <- fiber.interrupt
        ids                               <- handled.get
      } yield assertTrue(
        ids.sorted == List(1L, 2L),
        marks.toMap.get(1L).contains(None),
        marks.toMap.get(2L).flatten.exists(_.noSpaces.contains("***")),
        !marks.toMap.get(2L).flatten.exists(_.noSpaces.contains("секрет"))
      )
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds),

    test("ломается — повтор; после трёх попыток — Failed, Done нет") {
      for {
        w                                    <- worker(List(event(3, "boom")), brokenPolls = 0)
        (run, done, failed, attempts, handled) = w
        fiber                                <- run.run.fork
        dead                                 <- failed.get.repeatUntil(_.nonEmpty)
        _                                    <- ZIO.sleep(300.millis)
        _                                    <- fiber.interrupt
        tries                                <- handled.get
        n                                    <- attempts.get
        ok                                   <- done.get
      } yield assertTrue(dead == List(3L), tries == List(3L, 3L, 3L), n.get(3L).contains(3), ok.isEmpty)
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds),

    test("опрос упал — цикл идёт дальше и событие всё равно обработано") {
      for {
        w                          <- worker(List(event(4, "Атака")), brokenPolls = 2)
        (run, done, _, _, _)        = w
        fiber                      <- run.run.fork
        marks                      <- done.get.repeatUntil(_.nonEmpty)
        _                          <- fiber.interrupt
      } yield assertTrue(marks.map(_._1) == List(4L))
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds)
  )
}
