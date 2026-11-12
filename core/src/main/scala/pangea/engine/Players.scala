package pangea.engine

import pangea.model.user.User
import pangea.service.sender.ChatTarget
import zio.Task

trait Players {
  def getDisplayName(user: User): Task[String]

  /** Объявление в общую беседу Пангеи — например, итоги лотереи. */
  def announce(message: String): Task[Unit]

  /** Объявление в торговую беседу: новый лот на аукционе — и передача, если не
    * известно, в какой беседе её затеяли (см. [[announceAt]]). */
  def announceTrade(message: String): Task[Unit]

  /** Строка в беседу `chat` — туда, где затеяли дело: передачу из рук в руки
    * видно там же, где о ней договорились. */
  def announceAt(chat: ChatTarget.Peer, message: String): Task[Unit]

  /** Письмо игроку в личные сообщения — например, продавцу о том, что его лот
    * купили. Клавиатуру прошлого экрана не трогает. */
  def notify(user: User, message: String): Task[Unit]
}
