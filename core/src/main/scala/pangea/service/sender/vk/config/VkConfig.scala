package pangea.service.sender.vk.config

import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.{ZIO, ZLayer}

/** Номера бесед — со стороны сообщества (2000000000 + chat_id): у сообщества
  * своя нумерация, не та, что в адресной строке у игрока. Узнать номер можно по
  * `vk_inbox.peer_id` сообщений из этой беседы.
  *
  * @param chatPeerId      `peer_id` общей беседы Пангеи: туда уходят итоги
  *                         лотереи. Не задан — не пишем.
  * @param tradeChatPeerId `peer_id` торговой беседы: объявления аукциона о
  *                         лотах и о передачах. Не задан — пишем в общую.
  * @param longPoll        слушать ли ВК ещё и по Long Poll (`VK_LONG_POLL`),
  *                         рядом с Callback API (см. [[pangea.service.sender.vk.VkLongPoll]]).
  * @param guildsToken     токен гильдейского сообщества (`VK_TOKEN_FOR_GROUP_CHATS`):
  *                         беседы гильдий и прочие общие беседы (см.
  *                         [[pangea.model.vk.Community]]). Не задан — его нет. */
case class VkConfig(token: String, chatPeerId: Option[Long] = None, tradeChatPeerId: Option[Long] = None,
                    longPoll: Boolean = false, guildsToken: Option[String] = None) {

  /** Токен гильдейского сообщества; пустая строка из `.env` — то же, что нет. */
  def guilds: Option[String] = guildsToken.map(_.trim).filter(_.nonEmpty)
}

object VkConfig {
  private val loadConfig =
    ZIO.attempt(ConfigSource.default.at("vk").loadOrThrow[VkConfig])

  val live: ZLayer[Any, Throwable, VkConfig] =
    ZLayer.fromZIO(loadConfig)

}
