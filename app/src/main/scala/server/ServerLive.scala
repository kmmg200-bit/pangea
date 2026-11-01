package server

import cats.syntax.semigroupk._
import fs2.io.net.Network
import io.circe.Json
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.{HttpApp, HttpRoutes}
import pangea.dao.vk.SeenMessageDao
import pangea.metrics.GameMetrics
import pangea.model.user.VkId
import pangea.service.{Background, ErrorLog}
import pangea.service.admin.{AdminConfig, AdminPanel}
import pangea.service.donation.Donations
import pangea.service.miniapp.MiniApp
import pangea.service.state.{StateHandler, UserAction}
import pangea.service.activity.GroupEvent
import pangea.service.chat.ChatCommand
import server.model.{ServerConfig, VkEvent}
import zio.interop.catz._
import zio.{Task, UIO, ZIO}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}

final class ServerLive(
  config: ServerConfig,
  stateHandler: StateHandler,
  adminConfig: AdminConfig,
  donations: Donations,
  background: Background,
  seen: SeenMessageDao,
  miniApp: MiniApp
) extends Server {

  private val payments = new PaymentRoutes(donations, background)

  private val logs = new LogsRoutes(adminConfig)

  private val miniAppRoutes = new MiniAppRoutes(miniApp)

  private val dsl = Http4sDsl[Task]

  import dsl._

  private val routes: HttpRoutes[Task] = HttpRoutes.of[Task] {
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
        // «ok» — сразу, событие — в фоне. ВК ждёт ответа: не дождавшись, он
        // повторяет событие, а после череды таймаутов притормаживает отправку
        // всем игрокам разом. Порядок ходов одного игрока держит его лок,
        // повтор ВК отсекает номер события. В фон — не обычным fork: тот
        // прервался бы вместе с запросом, как только мы ответим. И не
        // forkDaemon: остановка сервера оборвала бы ход посреди записей, а ВК
        // после «ok» его уже не повторит, — фон ход дождётся (см. Background).
        _    <- background.fork(handle(json, ServerLive.Callback))
        resp <- Ok("ok")
      } yield resp
  }

  /** Пачка сообщений, которые бот сам забрал у ВК по Long Poll (см. VkLongPoll).
    * По Callback ВК присылает их по одному, и порядок ходов игрока держит сам
    * ВК; здесь они приходят разом — поэтому сообщения одного игрока идут по
    * очереди, в порядке ВК, а разные игроки — параллельно, как и по Callback. */
  override def longPoll(updates: List[Json]): UIO[Unit] =
    ZIO.foreachDiscard(updates.groupBy(ServerLive.peerOf).values)(own =>
      background.fork(ZIO.foreachDiscard(own)(handle(_, ServerLive.LongPoll))))

  /** Событие ВК целиком, пришедшее путём `source`: сообщение игрока или из
    * беседы — либо событие группы. Любая поломка — и ошибка, и падение кода
    * (defect) — в лог со всей причиной и с тем, чьё это сообщение. */
  private def handle(json: Json, source: String): Task[Unit] =
    json.as[VkEvent].toOption match {
      case Some(value) =>
        val msg = value.`object`.message
        // Пароль админ-панели приходит обычным сообщением, а лог отдаётся
        // наружу — в логе от него остаётся только метка.
        val logged = AdminPanel.maskSecrets(msg.text, adminConfig.password)
        // Одно сообщение приходит двумя путями — Callback и Long Poll: в ход
        // идёт пришедшее первым, второе только отмечается в метриках.
        (for {
          now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
          first <- seen.firstTime(msg.peerId, msg.id, msg.conversationMessageId.getOrElse(0L), now)
          _     <- GameMetrics.vkMessage(if (msg.fromChat) "chat" else "message", source, first, msg.date)
          _     <- ZIO.when(first)(
                     ZIO.logInfo(s"peer=${msg.peerId} from=${msg.fromId.getOrElse(0L)} id=${msg.id} cmid=${msg.conversationMessageId.getOrElse(0L)} via=$source text=$logged payload=${msg.payload.getOrElse("")}") *>
                       // Беседа — не игрок: её peer_id за героя принимать нельзя, оттуда
                       // мы слушаем только команды вроде «Передать».
                       (if (msg.fromChat) handleChat(msg)
                        else
                          stateHandler
                            .makeActionVK(
                              VkId(msg.peerId.toString),
                              msg.id,
                              UserAction(msg.text, msg.payload)
                            )))
        } yield ())
          .catchAllCause(ErrorLog.logged(s"VK: сообщение не обработано: peer=${msg.peerId} id=${msg.id}"))
      case None =>
        GameMetrics.vkEvents.tagged("kind", "group").increment *>
          groupEvent(json).catchAllCause(ErrorLog.logged("VK: событие группы не обработано"))
    }

  /** Что умеет общая беседа: «Передать» в ответ (или пересылкой) на
    * сообщение того, кому передают, команды о себе — «Мой профиль» и «Моё
    * снаряжение», ответ на которые уходит в саму беседу, — и «Туториал»: ссылка
    * на статью о том, как всё устроено в игре. Всё прочее там нас не касается. */
  private def handleChat(msg: VkEvent.Message): Task[Unit] =
    msg.fromId.filter(_ > 0L) match {
      case None => ZIO.unit
      case Some(from) if ChatCommand.tutorialCommand(msg.text) =>
        stateHandler.tutorialToChat(VkId(from.toString), msg.chatEventKey)
      case Some(from) =>
        ChatCommand.selfCommand(msg.text) match {
          case Some(command) => stateHandler.selfToChat(VkId(from.toString), command, msg.chatEventKey)
          case None =>
            (msg.quotedAuthor, ChatCommand.transferQuery(msg.text)) match {
              case (Some(to), Some(query)) if to > 0L =>
                stateHandler.transferFromChat(VkId(from.toString), VkId(to.toString), query, msg.chatEventKey)
              case _ => ZIO.unit
            }
        }
    }

  /** События группы для конкурса активности. Их легко подделать — а за них
    * дают дублоны, — поэтому принимаем только с секретом из настроек Callback
    * API; пока секрет не задан, не принимаем вовсе. Прочие события — в лог,
    * как и раньше. */
  private def groupEvent(json: Json): Task[Unit] =
    GroupEvent.typeOf(json) match {
      case Some(tpe) if GroupEvent.Types.contains(tpe) =>
        val expected = config.vkSecret.map(_.trim).filter(_.nonEmpty)
        if (expected.isEmpty) ZIO.logWarning(s"$tpe: VK_SECRET не задан — конкурс активности не считает")
        else if (GroupEvent.secretOf(json) != expected) ZIO.logWarning(s"$tpe с неверным секретом — пропускаю")
        else ZIO.foreachDiscard(GroupEvent.parse(json))(stateHandler.groupEvent)
      case _ => ZIO.attempt(println(json.noSpaces))
    }

  // Последний рубеж: что бы ни уронило любой роут — ВК, кассу, логи, Mini App, —
  // причина целиком уходит в лог, клиенту — 500. В лог — только путь: в запросе к
  // логам в параметрах лежит пароль.
  private val httpApp: HttpApp[Task] = {
    val app = (routes <+> payments.routes <+> logs.routes <+> miniAppRoutes.routes).orNotFound
    HttpApp[Task] { req =>
      app.run(req).catchAllCause { cause =>
        ErrorLog.logged(s"HTTP ${req.method} ${req.uri.path}: запрос не обработан")(cause) *> InternalServerError()
      }
    }
  }

  override def run(): UIO[Unit] = {
    // Явный Network[Task] вместо устаревшего неявного implicitForAsync (http4s 3.7.0).
    implicit val network: Network[Task] = Network.forAsync[Task]
    EmberServerBuilder
      .default[Task]
      .withHost(config.host)
      .withPort(config.port)
      .withHttpApp(httpApp)
      // Остановка: запросы, что уже идут, ждём не дольше этого — ответ ВК и
      // кассе занимает миллисекунды. А вот простаивающие keep-alive
      // соединения ВК держали бы остановку все 30 секунд по умолчанию.
      .withShutdownTimeout(ServerLive.ShutdownTimeout)
      .build
      .useForever
      .orDie
  }
}

object ServerLive {
  val ShutdownTimeout: FiniteDuration = 5.seconds

  /** Каким путём пришло событие — метка `source` в метриках доставки. */
  val Callback = "callback"
  val LongPoll = "longpoll"

  /** Чья беседа: по ней пачка Long Poll делится на очереди игроков. */
  private def peerOf(update: Json): Option[Long] =
    update.hcursor.downField("object").downField("message").get[Long]("peer_id").toOption
}
