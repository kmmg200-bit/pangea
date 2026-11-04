package server

import fs2.io.net.Network
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.{HttpApp, HttpRoutes}
import pangea.service.ErrorLog
import server.model.ServerConfig
import zio.interop.catz._
import zio.{Task, UIO}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** HTTP-сервер процесса: какие у него маршруты, решает роль (см. Role и
  * [[Server]]) — колбэк ВК у сборщика, касса, Mini App и логи у бота. */
final class ServerLive(config: ServerConfig, routes: HttpRoutes[Task]) extends Server {

  private val dsl = Http4sDsl[Task]

  import dsl._

  // Последний рубеж: что бы ни уронило любой роут — ВК, кассу, логи, Mini App, —
  // причина целиком уходит в лог, клиенту — 500. В лог — только путь: в запросе к
  // логам в параметрах лежит пароль.
  private val httpApp: HttpApp[Task] = {
    val app = routes.orNotFound
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
}
