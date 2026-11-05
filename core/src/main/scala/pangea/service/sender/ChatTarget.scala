package pangea.service.sender

/** В какую беседу пишет сообщество. Номера бесед — у сообщества свои, не те,
  * что видит в адресной строке игрок (см. [[pangea.service.sender.vk.config.VkConfig]]). */
sealed trait ChatTarget

object ChatTarget {

  /** Общая беседа Пангеи: итоги лотереи. */
  case object General extends ChatTarget

  /** Торговая беседа: объявления аукциона о лотах и о передачах. */
  case object Trade extends ChatTarget

  /** Беседа, откуда пришла команда: ответ уходит туда же. */
  final case class Peer(peerId: Long) extends ChatTarget
}
