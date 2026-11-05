package pangea.service.sender.vk

import org.http4s.Status
import zio.test._
import zio.{Ref, ZIO, durationInt}

object VkApiSpec extends ZIOSpecDefault {

  private val htmlBody = "<html><head><title>414 Request-URI Too Large</title></head></html>"

  def spec = suite("VkApi")(chatPeerSpec, failSpec)

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
