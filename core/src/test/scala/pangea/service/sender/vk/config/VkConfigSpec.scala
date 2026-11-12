package pangea.service.sender.vk.config

import pangea.model.vk.Community
import zio.test._

/** Какие беседы бот слушает: основное сообщество — только общую и торговую,
  * гильдейское — любую, куда его позвали. Номера бесед у сообществ совпадают. */
object VkConfigSpec extends ZIOSpecDefault {

  private val config = VkConfig("t", chatPeerId = Some(2000000001L), tradeChatPeerId = Some(2000000002L),
    guildsToken = Some("g"))

  override def spec = suite("Беседы сообществ")(
    test("основное — только общая и торговая; прочие беседы для него не существуют") {
      assertTrue(
        config.listens(Community.Main, 2000000001L),
        config.listens(Community.Main, 2000000002L),
        !config.listens(Community.Main, 2000000003L),
        !config.copy(guildsToken = None).listens(Community.Main, 2000000003L)
      )
    },
    test("гильдейское — любая беседа, и под теми же номерами, что общая и торговая") {
      assertTrue(
        config.listens(Community.Guilds, 2000000001L),
        config.listens(Community.Guilds, 2000000003L),
        config.listens(Community.Guilds, 2000000777L)
      )
    }
  )
}
