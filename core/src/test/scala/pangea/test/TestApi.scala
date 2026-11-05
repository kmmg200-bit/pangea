package pangea.test

import pangea.model.user.{User, VkId}
import pangea.model.vk.Attachment
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.{UserResponse, User => VkUser}
import pangea.service.sender.{Api, ChatTarget}
import zio.{Ref, Task, ZIO}

class TestApi(
  messagesRef: Ref[List[(String, Option[Keyboard])]],
  chatRef:     Ref[List[(ChatTarget, String)]],
  forwardsRef: Ref[List[(VkId, String, Option[Long])]]
) extends Api {

  def getName(user: User): Task[UserResponse] =
    ZIO.succeed(UserResponse(List(VkUser("Иван", "Иванов"))))

  def sendMessage(
      user: User,
      message: String,
      attachments: List[Attachment],
      keyboard: Option[Keyboard]
  ): Task[Unit] =
    messagesRef.update(_ :+ (message, keyboard))

  def sendToChat(to: ChatTarget, message: String): Task[Unit] = chatRef.update(_ :+ (to -> message))

  def forwardToUser(to: VkId, message: String, forwarded: Option[Long]): Task[Unit] =
    forwardsRef.update(_ :+ ((to, message, forwarded)))

  def sentMessages: Task[List[(String, Option[Keyboard])]] = messagesRef.get

  /** Что ушло в беседы — без адреса. */
  def chatMessages: Task[List[String]] = chatRef.get.map(_.map(_._2))

  /** Что ушло в беседы — с тем, в какую. */
  def chatSent: Task[List[(ChatTarget, String)]] = chatRef.get

  /** Письма с пересылкой: кому, подпись, какое сообщение переслано. */
  def forwards: Task[List[(VkId, String, Option[Long])]] = forwardsRef.get
}

object TestApi {
  def make: Task[TestApi] =
    for {
      messages <- Ref.make(List.empty[(String, Option[Keyboard])])
      chat     <- Ref.make(List.empty[(ChatTarget, String)])
      forwards <- Ref.make(List.empty[(VkId, String, Option[Long])])
    } yield new TestApi(messages, chat, forwards)
}
