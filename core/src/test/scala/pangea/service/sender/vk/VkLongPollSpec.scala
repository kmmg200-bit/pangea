package pangea.service.sender.vk

import io.circe.Json
import io.circe.parser.parse
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpApp, Response}
import zio.interop.catz._
import zio.test._
import zio.{Promise, Ref, Task, ZIO, durationInt}

/** Long Poll на поддельном ВК: сообщения доходят по порядку, прочие события
  * отсеиваются, устаревший ключ и потерянная часть очереди лечатся, а после
  * обрыва связи чтение продолжается с того же места. Место хранится вне
  * процесса: после перезапуска — с него, а несохранённая пачка придёт снова. */
object VkLongPollSpec extends ZIOSpecDefault {

  private val dsl = Http4sDsl[Task]
  import dsl._

  /** Место в памяти — как vk_longpoll. */
  private final class MemoryCursor(ts: Ref[Option[String]]) extends LongPollCursor {
    def load: Task[Option[String]]               = ts.get
    def save(next: String, now: Long): Task[Unit] = ts.set(Some(next))
  }

  private val pauses = VkLongPoll.Pauses(dropped = 10.millis, refused = 10.millis)

  /** Простой ВК: ключ один, ts 10 → сообщение 1 и ts 11, дальше — пусто. */
  private def plainVk(asked: Ref[List[String]]): Client[Task] = Client.fromHttpApp(plainApp(asked))

  private def plainApp(asked: Ref[List[String]]): HttpApp[Task] =
    HttpApp[Task] { req =>
      req.uri.path.renderString match {
        case "/method/groups.getById"           => Ok("""{"response":{"groups":[{"id":42}],"profiles":[]}}""")
        case "/method/groups.getLongPollServer" => Ok("""{"response":{"key":"k","server":"https://lp.vk.test/wh42","ts":"10"}}""")
        case "/wh42" =>
          val ts = req.params.getOrElse("ts", "")
          asked.update(_ :+ ts) *> (ts match {
            case "10" => Ok(s"""{"ts":"11","updates":[${message(1)}]}""")
            case _    => ZIO.sleep(5.millis) *> Ok(s"""{"ts":"$ts","updates":[]}""")
          })
        case _ => NotFound()
      }
    }

  private def message(n: Int) =
    s"""{"type":"message_new","object":{"message":{"id":$n,"peer_id":7,"text":"m$n"}},"group_id":42}"""

  /** ВК, который отвечает по номеру `ts` и ключу; на ts=20 один раз роняет связь. */
  private def fakeVk(requests: Ref[List[(String, String)]], keys: Ref[Int], dropped: Ref[Boolean]): Client[Task] =
    Client.fromHttpApp(HttpApp[Task] { req =>
      val q = req.params
      req.uri.path.renderString match {
        case "/method/groups.getById" =>
          Ok("""{"response":{"groups":[{"id":42}],"profiles":[]}}""")
        case "/method/groups.getLongPollServer" =>
          keys.updateAndGet(_ + 1).flatMap(k =>
            Ok(s"""{"response":{"key":"k$k","server":"https://lp.vk.test/wh42","ts":"10"}}"""))
        case "/wh42" =>
          val key = q.getOrElse("key", "")
          val ts  = q.getOrElse("ts", "")
          requests.update(_ :+ (key -> ts)) *> ((key, ts) match {
            case (_, "10")    => Ok(s"""{"ts":"11","updates":[${message(1)}]}""")
            case ("k1", "11") => Ok("""{"failed":2}""")
            case (_, "11")    => Ok(s"""{"ts":"12","updates":[${message(2)},{"type":"like_add","object":{}}]}""")
            case (_, "12")    => Ok("""{"failed":1,"ts":20}""")
            case (_, "20") =>
              dropped.getAndSet(true).flatMap(was =>
                if (was) Ok(s"""{"ts":"21","updates":[${message(3)}]}""")
                else ZIO.succeed(Response[Task](BadGateway)))
            case _ => ZIO.sleep(5.millis) *> Ok("""{"ts":"21","updates":[]}""")
          })
        case _ => NotFound()
      }
    })

  private def json(s: String): Json = parse(s).fold(throw _, identity)

  override def spec = suite("Long Poll ВК")(

    test("сообщения — по порядку и только они; ключ, потери и обрыв не сбивают с места") {
      for {
        requests <- Ref.make(List.empty[(String, String)])
        keys     <- Ref.make(0)
        dropped  <- Ref.make(false)
        got      <- Ref.make(List.empty[Long])
        three    <- Promise.make[Nothing, Unit]
        saved    <- Ref.make(Option.empty[String])
        poll      = new VkLongPoll(fakeVk(requests, keys, dropped), "token", "Long Poll", new MemoryCursor(saved), pauses)
        fiber    <- poll.run(updates =>
                      got.updateAndGet(_ ++ updates.flatMap(_.hcursor.downField("object").downField("message").get[Long]("id").toOption))
                        .flatMap(ids => ZIO.when(ids.size >= 3)(three.succeed(())).unit)).fork
        _        <- three.await
        _        <- fiber.interrupt
        ids      <- got.get
        asked    <- requests.get
        last     <- saved.get
      } yield assertTrue(ids == List(1L, 2L, 3L), last == Some("21")) &&
              // ключ устарел — новый, а место прежнее; потери — с места, что назвал ВК;
              // обрыв — снова с того же места, но уже с новым ключом
              assertTrue(asked.take(6) == List("k1" -> "10", "k1" -> "11", "k2" -> "11", "k2" -> "12", "k2" -> "20", "k3" -> "20"))
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds),

    test("после перезапуска — с сохранённого места, а не с текущего") {
      for {
        asked <- Ref.make(List.empty[String])
        saved <- Ref.make(Option("11"))
        fiber <- new VkLongPoll(plainVk(asked), "token", "Long Poll", new MemoryCursor(saved), pauses)
                   .run(_ => ZIO.unit).fork
        _     <- asked.get.repeatUntil(_.nonEmpty)
        _     <- fiber.interrupt
        first <- asked.get.map(_.headOption)
      } yield assertTrue(first == Some("11"))
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds),

    test("пачку не сохранили — место не двигается, та же пачка приходит ещё раз") {
      for {
        asked <- Ref.make(List.empty[String])
        saved <- Ref.make(Option.empty[String])
        calls <- Ref.make(0)
        kept  <- Promise.make[Nothing, Unit]
        fiber <- new VkLongPoll(plainVk(asked), "token", "Long Poll", new MemoryCursor(saved), pauses)
                   .run(_ => calls.updateAndGet(_ + 1).flatMap(n =>
                     if (n == 1) ZIO.fail(new RuntimeException("база недоступна")) else kept.succeed(()).unit)).fork
        _     <- kept.await
        _     <- saved.get.repeatUntil(_.nonEmpty)
        _     <- fiber.interrupt
        ts    <- asked.get
        last  <- saved.get
        n     <- calls.get
      } yield assertTrue(ts.take(2) == List("10", "10"), n == 2, last == Some("11"))
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds),

    test("сообщество — то, чей токен: и сообщество, и очередь спрашиваются этим токеном") {
      for {
        tokens <- Ref.make(List.empty[String])
        asked  <- Ref.make(List.empty[String])
        saved  <- Ref.make(Option.empty[String])
        vk      = Client.fromHttpApp(HttpApp[Task] { req =>
                    tokens.update(_ ++ req.params.get("access_token").toList) *> plainApp(asked).run(req)
                  })
        fiber  <- new VkLongPoll(vk, "guilds-token", "Long Poll гильдейского сообщества",
                    new MemoryCursor(saved), pauses)
                    .run(_ => ZIO.unit).fork
        _      <- asked.get.repeatUntil(_.nonEmpty)
        _      <- fiber.interrupt
        used   <- tokens.get
      } yield assertTrue(used.nonEmpty, used.forall(_ == "guilds-token"))
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds),

    test("id сообщества — и в новом ответе groups.getById, и в старом") {
      assertTrue(
        VkLongPoll.parseGroupId(json("""{"groups":[{"id":42}],"profiles":[]}""")) == Right(42L),
        VkLongPoll.parseGroupId(json("""[{"id":43}]""")) == Right(43L),
        VkLongPoll.parseGroupId(json("""{"groups":[]}""")).isLeft
      )
    },

    test("ответ очереди: события, потери с номером числом, устаревший ключ, полная потеря") {
      assertTrue(
        VkLongPoll.parseReply(json("""{"ts":"5","updates":[]}""")) == Right(VkLongPoll.Reply.Events("5", Nil)),
        VkLongPoll.parseReply(json("""{"failed":1,"ts":30}""")) == Right(VkLongPoll.Reply.Missed("30")),
        VkLongPoll.parseReply(json("""{"failed":2}""")) == Right(VkLongPoll.Reply.KeyExpired),
        VkLongPoll.parseReply(json("""{"failed":3}""")) == Right(VkLongPoll.Reply.Lost),
        VkLongPoll.parseReply(json("""{"failed":4}""")).isLeft
      )
    }
  )
}
