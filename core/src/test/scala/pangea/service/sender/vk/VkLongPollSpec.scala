package pangea.service.sender.vk

import io.circe.Json
import io.circe.parser.parse
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpApp, Response}
import pangea.service.sender.vk.config.VkConfig
import zio.interop.catz._
import zio.test._
import zio.{Promise, Ref, Task, ZIO, durationInt}

/** Long Poll на поддельном ВК: сообщения доходят по порядку, прочие события
  * отсеиваются, устаревший ключ и потерянная часть очереди лечатся, а после
  * обрыва связи чтение продолжается с того же места. */
object VkLongPollSpec extends ZIOSpecDefault {

  private val dsl = Http4sDsl[Task]
  import dsl._

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
        poll      = new VkLongPoll(fakeVk(requests, keys, dropped), VkConfig("token"),
                      VkLongPoll.Pauses(dropped = 10.millis, refused = 10.millis))
        fiber    <- poll.run(updates =>
                      got.updateAndGet(_ ++ updates.flatMap(_.hcursor.downField("object").downField("message").get[Long]("id").toOption))
                        .flatMap(ids => ZIO.when(ids.size >= 3)(three.succeed(())).unit)).fork
        _        <- three.await
        _        <- fiber.interrupt
        ids      <- got.get
        asked    <- requests.get
      } yield assertTrue(ids == List(1L, 2L, 3L)) &&
              // ключ устарел — новый, а место прежнее; потери — с места, что назвал ВК;
              // обрыв — снова с того же места, но уже с новым ключом
              assertTrue(asked.take(6) == List("k1" -> "10", "k1" -> "11", "k2" -> "11", "k2" -> "12", "k2" -> "20", "k3" -> "20"))
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
