package pangea.service.sender

import pangea.model.user.{User, VkId}
import pangea.model.vk.Attachment
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.UserResponse
import pangea.dao.sendfailure.SendFailureDao
import pangea.service.sender.vk.VkApi
import pangea.service.sender.vk.config.VkConfig
import zio.{Task, ZIO, ZLayer}

trait Api {
  def getName(user: User): Task[UserResponse]

  def sendMessage(
      user: User,
      message: String,
      attachments: List[Attachment],
      keyboard: Option[Keyboard]
  ): Task[Unit]

  /** Написать в общую беседу Пангеи (объявления аукциона). Беседа не настроена —
    * тихо ничего не делаем: это объявление, а не часть хода игрока. */
  def sendToChat(message: String): Task[Unit]

  /** Написать `to` от сообщества и приложить пересланным сообщение `forwarded` —
    * номер сообщения в диалоге сообщества, — если оно есть. Так «Админ 1»
    * получает /home игроков (см. [[pangea.service.admin.HomeAlert]]). */
  def forwardToUser(to: VkId, message: String, forwarded: Option[Long]): Task[Unit]
}

object Api {
  // Клиент живёт, пока жив слой: пул соединений с ВК не закрывается сразу
  // после создания (см. Client.scoped).
  val vk: ZLayer[VkConfig with SendFailureDao, Throwable, VkApi] = ZLayer.scoped(for {
    config   <- ZIO.service[VkConfig]
    failures <- ZIO.service[SendFailureDao]
    client   <- pangea.client.Client.scoped
  } yield new VkApi(client, config, failures))
}
