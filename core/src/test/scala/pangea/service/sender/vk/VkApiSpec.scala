package pangea.service.sender.vk

import io.circe.Json
import io.circe.parser.parse
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpApp, Status, UrlForm}
import pangea.dao.sendfailure.SendFailureDao
import pangea.engine.SceneContent
import pangea.metrics.GameMetrics
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.vk.Community
import pangea.model.vk.model.{UserResponse, User => VkUser}
import pangea.service.chat.{ChatButtons, Links}
import pangea.service.sender.ChatTarget
import zio.interop.catz._
import zio.test._
import zio.{Ref, Task, ZIO, durationInt}

object VkApiSpec extends ZIOSpecDefault {

  private val htmlBody = "<html><head><title>414 Request-URI Too Large</title></head></html>"

  // По порядку: метрики ошибок глобальные, и тесты, что считают их приращение,
  // не должны видеть чужие попытки.
  def spec = suite("VkApi")(chatPeerSpec, chatKeysSpec, chatSendSpec, tokenSpec, failSpec, errorCodeSpec) @@
    TestAspect.sequential

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
      assertTrue(VkApi.chatPeer(chats, ChatTarget.Peer(2000000007L, Community.Main)).contains(2000000007L))
    },
    test("торговая не настроена — объявления идут в общую; не настроено ничего — молчим") {
      import pangea.service.sender.ChatTarget
      assertTrue(VkApi.chatPeer(chats.copy(tradeChatPeerId = None), ChatTarget.Trade).contains(2000000001L)) &&
      assertTrue(VkApi.chatPeer(chats.copy(chatPeerId = None, tradeChatPeerId = None), ChatTarget.Trade).isEmpty)
    }
  )

  private def chatKeysSpec = suite("Кнопки бесед")(
    test("общая и торговая — с кнопками приложения и туториала; прочие беседы — без") {
      def main(peer: Long) = ChatTarget.Peer(peer, Community.Main)
      assertTrue(VkApi.chatKeyboard(chats, main(2000000001L)).contains(ChatButtons.keyboard)) &&
      assertTrue(VkApi.chatKeyboard(chats, main(2000000002L)).contains(ChatButtons.keyboard)) &&
      assertTrue(VkApi.chatKeyboard(chats, ChatTarget.General).contains(ChatButtons.keyboard)) &&
      assertTrue(VkApi.chatKeyboard(chats, main(2000000007L)).isEmpty) &&
      assertTrue(VkApi.chatKeyboard(chats.copy(chatPeerId = None, tradeChatPeerId = None), main(2000000001L)).isEmpty)
    },
    test("беседа гильдейского сообщества — без кнопок, даже под тем же номером, что общая") {
      assertTrue(VkApi.chatKeyboard(chats, ChatTarget.Peer(2000000001L, Community.Guilds)).isEmpty)
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
        _    <- api.sendToChat(ChatTarget.Peer(2000000007L, Community.Main), "ответ")
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

  private def tokenSpec = suite("Чьим токеном писать")(
    test("гильдейские беседы — гильдейским токеном, общая, торговая и беседы основного — основным") {
      val both = chats.copy(guildsToken = Some("g"))
      assertTrue(VkApi.tokenFor(both, ChatTarget.General).contains("t")) &&
      assertTrue(VkApi.tokenFor(both, ChatTarget.Trade).contains("t")) &&
      assertTrue(VkApi.tokenFor(both, ChatTarget.Peer(2000000001L, Community.Main)).contains("t")) &&
      assertTrue(VkApi.tokenFor(both, ChatTarget.Peer(2000000001L, Community.Guilds)).contains("g"))
    },
    test("гильдейского токена нет или он пустой — в его беседы писать нечем") {
      val guild = ChatTarget.Peer(2000000001L, Community.Guilds)
      assertTrue(VkApi.tokenFor(chats, guild).isEmpty) &&
      assertTrue(VkApi.tokenFor(chats.copy(guildsToken = Some("  ")), guild).isEmpty) &&
      assertTrue(chats.copy(guildsToken = Some(" g ")).guilds.contains("g"))
    },
    test("в беседу гильдейского — его токеном и без кнопок; без токена — не пишем вовсе") {
      for {
        seen <- Ref.make(List.empty[Map[String, String]])
        api   = new VkApi(fakeVk(seen, refuseKeys = false), chats.copy(guildsToken = Some("g")), NoFailures)
        _    <- api.sendToChat(ChatTarget.Peer(2000000001L, Community.Guilds), "ответ гильдии")
        _    <- api.sendToChat(ChatTarget.General, "итоги")
        none <- Ref.make(List.empty[Map[String, String]])
        bare  = new VkApi(fakeVk(none, refuseKeys = false), chats, NoFailures)
        _    <- bare.sendToChat(ChatTarget.Peer(2000000001L, Community.Guilds), "ответ гильдии")
        sent <- seen.get
        lost <- none.get
      } yield assertTrue(sent.map(_.get("access_token")) == List(Some("g"), Some("t"))) &&
              assertTrue(sent.map(_.contains("keyboard")) == List(false, true)) &&
              assertTrue(sent.headOption.flatMap(_.get("peer_id")).contains("2000000001")) &&
              assertTrue(lost.isEmpty)
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

  private val player = User(UserId(1L), VkId("7"), TelegramId("tg"))

  /** Сколько попыток `method` с кодом `code` записано в метрику ошибок. */
  private def errorsOf(method: String, code: String) =
    GameMetrics.vkApiErrors.tagged("method", method).tagged("code", code).value.map(_.count)

  private def errorCodeSpec = suite("Ошибки ВК в метрике")(
    test("код — номер ошибки ВК; ответ не 2xx — статус; 2xx не JSON — not_json") {
      assertTrue(VkApi.errorCode(Status.Ok, VkApi.VkApiError(6, "Too many requests per second")) == "6") &&
      assertTrue(VkApi.errorCode(Status.UriTooLong, VkApi.VkApiError(414, htmlBody)) == "http_414") &&
      assertTrue(VkApi.errorCode(Status.Ok, new RuntimeException("VK non-JSON response")) == "not_json")
    },
    test("ответа нет — попытка в метрике как net") {
      val down = Client.fromHttpApp(HttpApp[Task](_ => ZIO.fail(new RuntimeException("connection reset"))))
      for {
        before <- errorsOf("users.get", VkApi.NoResponse)
        failed <- new VkApi(down, chats, NoFailures).getName(player).either
        after  <- errorsOf("users.get", VkApi.NoResponse)
      } yield assertTrue(failed.isLeft, after - before == 1.0)
    },
    test("ошибка ВК — в метрике её номер, каждая попытка") {
      for {
        seen   <- Ref.make(List.empty[Map[String, String]])
        before <- errorsOf("messages.send", "911")
        fiber  <- new VkApi(fakeVk(seen, refuseKeys = true), chats, NoFailures).sendToChat(ChatTarget.Trade, "лот").fork
        _      <- TestClock.adjust(1.minute)
        _      <- fiber.join
        after  <- errorsOf("messages.send", "911")
      } yield assertTrue(after - before == 3.0)
    },
    test("имя из ВК — полем response, токеном основного сообщества") {
      val vk = Client.fromHttpApp(HttpApp[Task] { req =>
        req.as[UrlForm].flatMap { form =>
          if (req.uri.path.renderString.endsWith("/users.get") && form.getFirst("user_ids").contains("7") &&
              form.getFirst("access_token").contains("t"))
            Ok("""{"response":[{"id":7,"first_name":"Лео","last_name":"Вейн"}]}""")
          else BadRequest()
        }
      })
      new VkApi(vk, chats, NoFailures).getName(player)
        .map(r => assertTrue(r == UserResponse(List(VkUser("Лео", "Вейн")))))
    }
  )
}
