package server

import com.comcast.ip4s.{IpLiteralSyntax, Port}
import fs2.io.net.Network
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import zio.interop.catz._
import zio.metrics.connectors.prometheus.PrometheusPublisher
import zio.{Task, UIO, ZIO}

import scala.concurrent.duration.DurationInt

/** Метрики для Prometheus — `GET /metrics` на своём порту. Наружу он не
  * открыт: в docker-compose его читает только контейнер prometheus по
  * внутренней сети. На общем 80-м метрики видел бы весь интернет. */
object MetricsServer {

  val Port: Port = port"9464"

  def run(publisher: PrometheusPublisher): UIO[Unit] = {
    // Явный Network[Task] вместо устаревшего неявного implicitForAsync (http4s 3.7.0).
    implicit val network: Network[Task] = Network.forAsync[Task]
    val dsl = Http4sDsl[Task]
    import dsl._

    // Текстовый формат Prometheus требует перевода строки в конце, а коннектор
    // его не ставит: без него последняя строка — «unexpected end of input».
    val routes = HttpRoutes.of[Task] {
      case GET -> Root / "metrics" => publisher.get.flatMap(text => Ok(if (text.endsWith("\n")) text else text + "\n"))
    }

    EmberServerBuilder
      .default[Task]
      .withHost(ipv4"0.0.0.0")
      .withPort(Port)
      .withHttpApp(routes.orNotFound)
      .withShutdownTimeout(1.second)
      .build
      .useForever
      .orDie
      // Метрики — не игра: не поднялись — пишем в лог, бот работает дальше.
      .catchAllCause(cause => ZIO.logErrorCause("Метрики для Prometheus не поднялись", cause))
  }
}
