package server

import io.circe.Json
import pangea.model.user.VkId
import pangea.model.vk.{InboxEvent, InboxKind}
import pangea.service.activity.GroupEvent
import pangea.service.admin.{AdminConfig, AdminPanel}
import pangea.service.chat.ChatCommand
import pangea.service.inbox.InboxHandler
import pangea.service.state.{StateHandler, UserAction}
import server.model.VkEvent
import zio.{Task, ZIO, ZLayer}

/** Что бот делает с событием из очереди ВК (`vk_inbox`): ход игрока из лички,
  * команда из общей беседы, событие группы — билет за активность. Принимает
  * события [[ServerLive]] (Callback и Long Poll), а сюда они приходят уже без
  * повторов и для каждого игрока по порядку (см. InboxWorker). */
final class VkInboxHandler(stateHandler: StateHandler, adminConfig: AdminConfig) extends InboxHandler {

  override def handle(event: InboxEvent): Task[Unit] = event.kind match {
    case InboxKind.Message =>
      message(event).flatMap(msg =>
        stateHandler.makeActionVK(VkId(msg.peerId.toString), msg.id, UserAction(msg.text, msg.payload)))
    // Беседа — не игрок: её peer_id за героя принимать нельзя, оттуда мы
    // слушаем только команды вроде «Передать».
    case InboxKind.Chat  => message(event).flatMap(chat)
    case InboxKind.Group => ZIO.foreachDiscard(GroupEvent.parse(event.event))(stateHandler.groupEvent)
  }

  // Пароль админ-панели приходит обычным сообщением: в базе (и в бэкапах) от
  // него остаётся только метка.
  override def redact(event: Json): Json =
    InboxEvent.mapText(event)(AdminPanel.maskSecrets(_, adminConfig.password))

  private def message(event: InboxEvent): Task[VkEvent.Message] =
    ZIO.fromEither(event.event.as[VkEvent]).map(_.`object`.message)

  /** Что умеют беседы — общая и торговая, любая, где есть бот: «Передать» в
    * ответ (или пересылкой) на сообщение того, кому передают, команды о себе —
    * «Мой профиль» и «Моё снаряжение», ответ на которые уходит в ту же беседу, —
    * и «Туториал»: ссылка на статью о том, как всё устроено в игре. Всё прочее
    * там нас не касается. */
  private def chat(msg: VkEvent.Message): Task[Unit] =
    msg.fromId.filter(_ > 0L) match {
      case None => ZIO.unit
      case Some(from) if ChatCommand.tutorialCommand(msg.text) =>
        stateHandler.tutorialToChat(VkId(from.toString), msg.chatEventKey, msg.peerId)
      case Some(from) =>
        ChatCommand.selfCommand(msg.text) match {
          case Some(command) => stateHandler.selfToChat(VkId(from.toString), command, msg.chatEventKey, msg.peerId)
          case None =>
            (msg.quotedAuthor, ChatCommand.transferQuery(msg.text)) match {
              case (Some(to), Some(query)) if to > 0L =>
                // Строка о передаче уйдёт в эту же беседу (см. Transfers.log).
                stateHandler.transferFromChat(VkId(from.toString), VkId(to.toString), query, msg.chatEventKey,
                  Some(msg.peerId))
              case _ => ZIO.unit
            }
        }
    }
}

object VkInboxHandler {
  val live: ZLayer[StateHandler with AdminConfig, Nothing, InboxHandler] =
    ZLayer.fromFunction(new VkInboxHandler(_, _))
}
