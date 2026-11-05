package pangea.engine

import pangea.model.user.User
import zio.Task

trait Players {
  def getDisplayName(user: User): Task[String]

  /** Объявление в общую беседу Пангеи — например, итоги лотереи. */
  def announce(message: String): Task[Unit]

  /** Объявление в торговую беседу: новый лот на аукционе, передача из рук в руки. */
  def announceTrade(message: String): Task[Unit]

  /** Письмо игроку в личные сообщения — например, продавцу о том, что его лот
    * купили. Клавиатуру прошлого экрана не трогает. */
  def notify(user: User, message: String): Task[Unit]
}
