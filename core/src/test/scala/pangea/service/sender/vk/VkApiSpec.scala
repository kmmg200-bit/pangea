package pangea.service.sender.vk

import io.circe.Json
import io.circe.parser.parse
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpApp, Status, UrlForm}
import pangea.dao.sendfailure.SendFailureDao
import pangea.engine.SceneContent
import pangea.model.user.UserId
import pangea.service.chat.{ChatButtons, Links}
import pangea.service.sender.ChatTarget
import zio.interop.catz._
import zio.test._
import zio.{Ref, Task, ZIO, durationInt}

object VkApiSpec extends ZIOSpecDefault {

  private val htmlBody = "<html><head><title>414 Request-URI Too Large</title></head></html>"

  def spec = suite("VkApi")(chatPeerSpec, chatKeysSpec, chatSendSpec, failSpec)

  private val dsl = Http4sDsl[Task]
  import dsl._

  /** Сбои отправки в беседу не пишутся — журнал тут ни при чём. */
  private object NoFailures extends SendFailureDao {
    override def record(userId: Option[UserId], vkId: String, messageText: String, keyboardJson: Option[String],
                        errorCode: Option[Int], errorMessage: String, attempts: Int, createdAt: Long): Task[Unit] =
      ZIO.unit
  }

  /** ВК, что записывает поля каждого messages.send; `refuseKeys` — клавиатуру
    * не принимает (911), а без неё сообщение берёт. */
  private def fakeVk(seen: Ref[List[Map[String, String]]], refuseKeys: Boolean): Client[Task] =
    Client.fromHttpApp(HttpApp[Task] { req =>
      req.as[UrlForm].flatMap { form =>
        val fields = form.values.map { case (k, vs) => k -> vs.toList.mkString }
        seen.update(_ :+ fields) *>
          (if (refuseKeys && fields.contains("keyboard"))
             Ok("""{"error":{"error_code":911,"error_msg":"Keyboard format is invalid"}}""")
           else Ok("""{"response":1}"""))
      }
    })

  private val chats = pangea.service.sender.vk.config.VkConfig("t", chatPeerId = Some(2000000001L),
    tradeChatPeerId = Some(2000000002L))

  private def chatPeerSpec = suite("VkApi.chatPeer")(
    test("общая, торговая и та, откуда спросили, — каждая в свой peer_id") {
      import pangea.service.sender.ChatTarget
      assertTrue(VkApi.chatPeer(chats, ChatTarget.General).contains(2000000001L)) &&
      assertTrue(VkApi.chatPeer(chats, ChatTarget.Trade).contains(2000000002L)) &&
      assertTrue(VkApi.chatPeer(chats, ChatTarget.Peer(2000000007L)).contains(2000000007L))
    },
    test("торговая не настроена — объявления идут в общую; не настроено ничего — молчим") {
      import pangea.service.sender.ChatTarget
      assertTrue(VkApi.chatPeer(chats.copy(tradeChatPeerId = None), ChatTarget.Trade).contains(2000000001L)) &&
      assertTrue(VkApi.chatPeer(chats.copy(chatPeerId = None, tradeChatPeerId = None), ChatTarget.Trade).isEmpty)
    }
  )

  private def chatKeysSpec = suite("Кнопки бесед")(
    test("общая и торговая — с кнопками приложения и туториала; прочие беседы — без") {
      assertTrue(VkApi.chatKeyboard(chats, 2000000001L).contains(ChatButtons.keyboard)) &&
      assertTrue(VkApi.chatKeyboard(chats, 2000000002L).contains(ChatButtons.keyboard)) &&
      assertTrue(VkApi.chatKeyboard(chats, 2000000007L).isEmpty) &&
      assertTrue(VkApi.chatKeyboard(chats.copy(chatPeerId = None, tradeChatPeerId = None), 2000000001L).isEmpty)
    },
    test("клавиатура под полем ввода, один ряд: приложение и ссылка на туториал, обе без цвета") {
      val json = parse(ChatButtons.keyboard.getJson).getOrElse(Json.Null)
      val app  = json.hcursor.downField("buttons").downArray.downArray
      val link = app.right
      assertTrue(json.hcursor.get[Boolean]("inline").contains(false) && json.hcursor.get[Boolean]("one_time").contains(false)) &&
      assertTrue(json.hcursor.downField("buttons").values.exists(_.size == 1)) &&
      assertTrue(app.downField("action").get[String]("type").contains("open_app") &&
                 app.downField("action").get[Long]("app_id").contains(54870345L) &&
                 app.downField("action").get[String]("label").contains("📱 Приложение") &&
                 app.downField("color").failed) &&
      assertTrue(link.downField("action").get[String]("type").contains("open_link") &&
                 link.downField("action").get[String]("link").contains(Links.Tutorial) &&
                 link.downField("action").get[String]("label").contains("📖 Туториал") &&
                 link.downField("color").failed)
    },
    test("подпись приложения — та же, что у кнопки в игре") {
      assertTrue(ChatButtons.AppLabel == SceneContent.load().text("common.openApp"))
    }
  )

  private def chatSendSpec = suite("VkApi.sendToChat")(
    test("в общую и торговую беседу — с кнопками; в чужую беседу — без") {
      for {
        seen <- Ref.make(List.empty[Map[String, String]])
        api   = new VkApi(fakeVk(seen, refuseKeys = false), chats, NoFailures)
        _    <- api.sendToChat(ChatTarget.General, "итоги")
        _    <- api.sendToChat(ChatTarget.Trade, "лот")
        _    <- api.sendToChat(ChatTarget.Peer(2000000007L), "ответ")
        sent <- seen.get
      } yield assertTrue(sent.map(_.get("peer_id")) == List(Some("2000000001"), Some("2000000002"), Some("2000000007"))) &&
              assertTrue(sent.map(_.contains("keyboard")) == List(true, true, false)) &&
              assertTrue(sent.headOption.flatMap(_.get("keyboard")).contains(ChatButtons.keyboard.getJson))
    },
    test("ВК не принял кнопки — то же сообщение уходит без них, с тем же random_id") {
      for {
        seen  <- Ref.make(List.empty[Map[String, String]])
        api    = new VkApi(fakeVk(seen, refuseKeys = true), chats, NoFailures)
        fiber <- api.sendToChat(ChatTarget.Trade, "лот").fork
        _     <- TestClock.adjust(1.minute)
        _     <- fiber.join
        sent  <- seen.get
      } yield assertTrue(sent.size == 4 && sent.take(3).forall(_.contains("keyboard")) &&
                         sent.lastOption.exists(f => !f.contains("keyboard") && f.get("message").contains("лот"))) &&
              assertTrue(sent.map(_.get("random_id")).distinct.size == 1)
    }
  )

  private def failSpec = suite("VkApi.failOnBadResponse")(
    test("2xx с обычным ответом ВК — успех") {
      VkApi.failOnBadResponse(Status.Ok, """{"response":1}""").as(assertCompletes)
    },
    test("2xx с полем error — VkApiError с кодом и текстом ВК") {
      VkApi
        .failOnBadResponse(Status.Ok, """{"error":{"error_code":914,"error_msg":"Message is too long"}}""")
        .flip
        .map(err => assertTrue(err == VkApi.VkApiError(914, "Message is too long")))
    },
    // Именно этот случай раньше считался успехом: сообщение не доставлено, а
    // герой переезжал в новое состояние без экрана (залипание в лавке Ришелье).
    test("414 от прокси — ошибка с кодом статуса") {
      VkApi
        .failOnBadResponse(Status.UriTooLong, htmlBody)
        .flip
        .map(err => assertTrue(err == VkApi.VkApiError(414, htmlBody)))
    },
    test("2xx с не-JSON телом — ошибка") {
      VkApi
        .failOnBadResponse(Status.Ok, htmlBody)
        .flip
        .map(err => assertTrue(err.getMessage.contains("non-JSON")))
    },
    test("длинное тело обрезается в тексте ошибки") {
      val long = "x" * 5000
      VkApi
        .failOnBadResponse(Status.UriTooLong, long)
        .flip
        .map(err => assertTrue(err.getMessage.length < 500, err.getMessage.endsWith("…")))
    },
    test("закрытые игроком сообщения (900–902) — ещё одна попытка, прочие сбои — до трёх попыток") {
      def attempts(err: Throwable) = for {
        n     <- Ref.make(0)
        fiber <- (n.update(_ + 1) *> ZIO.fail(err)).retry(VkApi.SendRetry).ignore.fork
        _     <- TestClock.adjust(1.minute)
        _     <- fiber.join
        r     <- n.get
      } yield r
      for {
        closed <- ZIO.foreach(VkApi.UnreachableCodes.toList)(c => attempts(VkApi.VkApiError(c, "closed")))
        broken <- attempts(VkApi.VkApiError(10, "Internal server error"))
        lost   <- attempts(new RuntimeException("connection reset"))
      } yield assertTrue(closed.forall(_ == 2) && broken == 3 && lost == 3)
    },
    // ВК порой ложно отвечает «закрыл сообщения» — повтор ждёт, пока сбой пройдёт.
    test("повтор после 901 — не раньше чем через 5 секунд") {
      for {
        n      <- Ref.make(0)
        fiber  <- (n.update(_ + 1) *> ZIO.fail(VkApi.VkApiError(901, "closed"))).retry(VkApi.SendRetry).ignore.fork
        _      <- TestClock.adjust(4.seconds)
        early  <- fiber.poll
        before <- n.get
        _      <- TestClock.adjust(1.second)
        _      <- fiber.join
        after  <- n.get
      } yield assertTrue(early.isEmpty, before == 1, after == 2)
    }
  )
}
