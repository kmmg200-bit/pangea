package server

import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.dsl.Http4sDsl
import pangea.dao.vk.VkInboxDao
import pangea.metrics.GameMetrics
import pangea.model.vk.{Community, InboxEvent, InboxKind}
import pangea.service.activity.GroupEvent
import pangea.service.admin.{AdminConfig, AdminPanel}
import server.model.{ServerConfig, VkEvent}
import zio.interop.catz._
import zio.{Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** Приём событий ВК — колбэк (`POST /`) и пачки Long Poll — в очередь
  * `vk_inbox`. Ход здесь не играется: очередь разбирает бот (InboxWorker и
  * VkInboxHandler). Работает в сборщике (`ROLE=collector`) или в общем
  * процессе (`ROLE=all`). */
final class VkIngest(config: ServerConfig, adminConfig: AdminConfig, inbox: VkInboxDao) {

  private val dsl = Http4sDsl[Task]
  import dsl._

  val routes: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case req @ POST -> Root =>
      for {
//        vkChecking <- req.as[VkChecking].option
//        resp <- vkChecking match {
//          case Some(_) =>
//            Ok(
//              "b874fbde",
//              `Content-Type`(MediaType.text.plain)
//            )
//          case None => Ok("ok")
//        }

        // Тело запроса читаем один раз: Ember отдаёт его только единожды, и
        // второе чтение большого события (длинный текст, пересланные сообщения,
        // посты для конкурса) падало — «Body Has Been Consumed Completely
        // Already». Событие ВК разбираем из того же JSON.
        json <- req.as[Json]
        // «ok» — когда событие легло в очередь (vk_inbox), а не когда сыгран
        // ход: запись — миллисекунды, а ВК, не дождавшись ответа, повторяет
        // событие и после череды таймаутов притормаживает отправку всем
        // игрокам разом. Не легло (база недоступна) — 500, и ВК повторит
        // событие сам: потерять его нельзя. Ход сыграет бот.
        _    <- accept(json, VkIngest.Callback)
        resp <- Ok("ok")
      } yield resp
  }

  /** Пачка сообщений, которые Long Poll забрал у ВК (см. VkLongPoll), — в
    * очередь по одному, в порядке ВК: по нему и пойдут ходы каждого игрока.
    * Не легли — ошибка, и Long Poll запросит ту же пачку ещё раз. */
  def longPoll(updates: List[Json]): Task[Unit] =
    ZIO.foreachDiscard(updates)(accept(_, VkIngest.LongPoll))

  /** Событие ВК, пришедшее путём `source`, — в очередь: сообщение игрока или из
    * беседы, либо событие группы. Одно сообщение приходит двумя путями —
    * Callback и Long Poll: в очередь ложится пришедшее первым, второе только
    * отмечается в метриках. */
  private def accept(json: Json, source: String): Task[Unit] =
    json.as[VkEvent].toOption match {
      case Some(value) =>
        val msg  = value.`object`.message
        val kind = if (msg.fromChat) InboxKind.Chat else InboxKind.Message
        // Пароль админ-панели приходит обычным сообщением, а лог отдаётся
        // наружу — в логе от него остаётся только метка.
        val logged = AdminPanel.maskSecrets(msg.text, adminConfig.password)
        for {
          now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
          first <- inbox.accept(InboxEvent.messageKey(Community.Main, msg.peerId, msg.id, msg.conversationMessageId.getOrElse(0L)),
                                msg.peerId, kind, source, json, msg.date.map(_ * 1000L), now)
          _     <- GameMetrics.vkMessage(kind.entryName, source, first, msg.date)
          _     <- ZIO.when(first)(ZIO.logInfo(
                     s"peer=${msg.peerId} from=${msg.fromId.getOrElse(0L)} id=${msg.id} cmid=${msg.conversationMessageId.getOrElse(0L)} via=$source text=$logged payload=${msg.payload.getOrElse("")}"))
        } yield ()
      case None => acceptGroup(json, source)
    }

  /** События группы для билетов за активность. Их легко подделать — а за них
    * дают билеты, — поэтому в очередь кладём только с секретом из настроек
    * Callback API; пока секрет не задан, не принимаем вовсе. Прочие события — в
    * лог, как и раньше. */
  private def acceptGroup(json: Json, source: String): Task[Unit] =
    GroupEvent.typeOf(json) match {
      case Some(tpe) if GroupEvent.Types.contains(tpe) =>
        val expected = config.vkSecret.map(_.trim).filter(_.nonEmpty)
        if (expected.isEmpty) ZIO.logWarning(s"$tpe: VK_SECRET не задан — билеты за активность не выдаются")
        else if (GroupEvent.secretOf(json) != expected) ZIO.logWarning(s"$tpe с неверным секретом — пропускаю")
        else
          ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
            .flatMap(now => inbox.accept(InboxEvent.groupKey(json), VkIngest.GroupQueue, InboxKind.Group, source, json, None, now))
            .flatMap(first => GameMetrics.vkEvents.tagged("kind", "group").increment.when(first).unit)
      case _ => ZIO.attempt(println(json.noSpaces))
    }
}

object VkIngest {

  /** Каким путём пришло событие — метка `source` в метриках доставки и в очереди. */
  val Callback = "callback"
  val LongPoll = "longpoll"

  /** Очередь событий группы в vk_inbox: одна на всех — героя они не трогают, и
    * порядок с ходами игроков им не нужен. */
  val GroupQueue: Long = 0L

  val live: ZLayer[ServerConfig with AdminConfig with VkInboxDao, Nothing, VkIngest] =
    ZLayer.fromFunction(new VkIngest(_, _, _))
}
