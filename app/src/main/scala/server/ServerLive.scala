package server

import cats.syntax.semigroupk._
import fs2.io.net.Network
import io.circe.Json
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.{HttpApp, HttpRoutes}
import pangea.dao.vk.VkInboxDao
import pangea.metrics.GameMetrics
import pangea.model.vk.{InboxEvent, InboxKind}
import pangea.service.{Background, ErrorLog}
import pangea.service.admin.{AdminConfig, AdminPanel}
import pangea.service.donation.Donations
import pangea.service.miniapp.MiniApp
import pangea.service.activity.GroupEvent
import server.model.{ServerConfig, VkEvent}
import zio.interop.catz._
import zio.{Task, UIO, ZIO}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}

final class ServerLive(
  config: ServerConfig,
  adminConfig: AdminConfig,
  donations: Donations,
  background: Background,
  inbox: VkInboxDao,
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
        // «ok» — когда событие легло в очередь (vk_inbox), а не когда сыгран
        // ход: запись — миллисекунды, а ВК, не дождавшись ответа, повторяет
        // событие и после череды таймаутов притормаживает отправку всем
        // игрокам разом. Не легло (база недоступна) — 500, и ВК повторит
        // событие сам: потерять его нельзя. Ход сыграет InboxWorker.
        _    <- accept(json, ServerLive.Callback)
        resp <- Ok("ok")
      } yield resp
  }

  /** Пачка сообщений, которые бот сам забрал у ВК по Long Poll (см.
    * VkLongPoll), — в очередь по одному, в порядке ВК: по нему и пойдут ходы
    * каждого игрока. */
  override def longPoll(updates: List[Json]): Task[Unit] =
    ZIO.foreachDiscard(updates)(accept(_, ServerLive.LongPoll))

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
          first <- inbox.accept(InboxEvent.messageKey(msg.peerId, msg.id, msg.conversationMessageId.getOrElse(0L)),
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
            .flatMap(now => inbox.accept(InboxEvent.groupKey(json), ServerLive.GroupQueue, InboxKind.Group, source, json, None, now))
            .flatMap(first => GameMetrics.vkEvents.tagged("kind", "group").increment.when(first).unit)
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

  /** Каким путём пришло событие — метка `source` в метриках доставки и в очереди. */
  val Callback = "callback"
  val LongPoll = "longpoll"

  /** Очередь событий группы в vk_inbox: одна на всех — героя они не трогают, и
    * порядок с ходами игроков им не нужен. */
  val GroupQueue: Long = 0L
}
