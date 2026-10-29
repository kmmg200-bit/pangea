package pangea.service.chat

import zio.test._

/** Личка или беседа — по тому, совпадает ли `peer_id` с автором, а не по
  * величине id: у новых аккаунтов ВК id 12-значные. */
object ChatPeerSpec extends ZIOSpecDefault {

  private val oldPlayer = 89655198L
  private val newPlayer = 200003146683L
  private val chat      = ChatCommand.ChatPeerBase + 1L

  override def spec = suite("Личка или беседа")(

    test("личка — и у старого аккаунта, и у нового с 12-значным id") {
      assertTrue(!ChatCommand.fromChat(oldPlayer, Some(oldPlayer))) &&
      assertTrue(!ChatCommand.fromChat(newPlayer, Some(newPlayer)))
    },

    test("беседа — кто бы в ней ни написал") {
      assertTrue(ChatCommand.fromChat(chat, Some(oldPlayer))) &&
      assertTrue(ChatCommand.fromChat(chat, Some(newPlayer)))
    },

    test("без автора — по диапазону: беседу не принимаем за игрока") {
      assertTrue(ChatCommand.fromChat(chat, None)) &&
      assertTrue(!ChatCommand.fromChat(oldPlayer, None))
    }
  )
}
