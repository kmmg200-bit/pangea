package pangea.service.sender

import pangea.model.user.{User, VkId}
import pangea.model.vk.Attachment
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.UserResponse
import pangea.dao.sendfailure.SendFailureDao
import pangea.service.sender.vk.VkApi
import pangea.service.sender.vk.config.VkConfig
import zio.{Schedule, Task, ZIO, ZLayer, durationInt}

trait Api {
  def getName(user: User): Task[UserResponse]

  def sendMessage(
      user: User,
      message: String,
      attachments: List[Attachment],
      keyboard: Option[Keyboard]
  ): Task[Unit]

  /** Написать в беседу `to`: общую, торговую или ту, откуда пришла команда.
    * Беседа не настроена — тихо ничего не делаем; ошибки ВК тоже не роняют
    * вызов: это объявление или ответ в беседу, а не часть хода игрока. */
  def sendToChat(to: ChatTarget, message: String): Task[Unit]

  /** Написать `to` от сообщества и приложить пересланным сообщение `forwarded` —
    * номер сообщения в диалоге сообщества, — если оно есть. Так «Админ 1»
    * получает /home и подтверждённые /restart игроков (см.
    * [[pangea.service.admin.HomeAlert]]). */
  def forwardToUser(to: VkId, message: String, forwarded: Option[Long]): Task[Unit]
}

object Api {
  /** Игрок не принимает сообщения от сообщества — повтор не поможет, пока он
    * сам их не откроет (см. [[VkApi.UnreachableCodes]]). */
  def unreachable(err: Throwable): Boolean = err match {
    case VkApi.VkApiError(code, _) => VkApi.UnreachableCodes(code)
    case _                         => false
  }

  // Клиент живёт, пока жив слой: пул соединений с ВК не закрывается сразу
  // после создания (см. Client.scoped). Окна частоты ключей — одни на бота;
  // их пик за секунду идёт в метрику раз в секунду, пока жив слой. Пачки писем
  // (VkBatcher) разбирает цикл, пока жив слой: он закрывается уже после того,
  // как Background дождался ходов, и их письма успевают уйти.
  val vk: ZLayer[VkConfig with SendFailureDao, Throwable, VkApi] = ZLayer.scoped(for {
    config   <- ZIO.service[VkConfig]
    failures <- ZIO.service[SendFailureDao]
    client   <- pangea.client.Client.scoped
    api      <- VkApi.make(client, config, failures)
    _        <- api.publishPeaks.repeat(Schedule.spaced(1.second)).forkScoped
  } yield api)
}
