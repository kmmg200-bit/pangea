package pangea.service.sender

import pangea.model.vk.Community

/** В какую беседу пишет сообщество. Номера бесед — у сообщества свои, не те,
  * что видит в адресной строке игрок (см. [[pangea.service.sender.vk.config.VkConfig]]). */
sealed trait ChatTarget

object ChatTarget {

  /** Общая беседа Пангеи: итоги лотереи. */
  case object General extends ChatTarget

  /** Торговая беседа: объявления аукциона о лотах и о передачах. */
  case object Trade extends ChatTarget

  /** Беседа, откуда пришла команда: ответ уходит туда же и от имени того же
    * сообщества — номера бесед у сообществ совпадают. */
  final case class Peer(peerId: Long, community: Community) extends ChatTarget
}
