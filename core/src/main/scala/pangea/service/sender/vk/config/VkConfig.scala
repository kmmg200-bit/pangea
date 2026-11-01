package pangea.service.sender.vk.config

import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.{ZIO, ZLayer}

/** @param chatPeerId `peer_id` общей беседы Пангеи (2000000000 + chat_id):
  *                    туда уходят объявления аукциона. Не задан — не пишем.
  * @param longPoll   слушать ли ВК ещё и по Long Poll (`VK_LONG_POLL`), рядом
  *                    с Callback API (см. [[pangea.service.sender.vk.VkLongPoll]]). */
case class VkConfig(token: String, chatPeerId: Option[Long] = None, longPoll: Boolean = false)

object VkConfig {
  private val loadConfig =
    ZIO.attempt(ConfigSource.default.at("vk").loadOrThrow[VkConfig])

  val live: ZLayer[Any, Throwable, VkConfig] =
    ZLayer.fromZIO(loadConfig)

}
