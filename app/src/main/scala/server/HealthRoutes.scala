package server

import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import zio.Task
import zio.interop.catz._

/** Жив ли процесс: `GET /health` → 200 «ok». По нему healthcheck в
  * docker-compose видит, что бот (или сборщик) поднялся, а
  * scripts/deploy-bot.sh — что старого бота пора гасить. Настоящий запрос, а не
  * голое TCP-соединение: пустое соединение сервер пишет в лог ошибкой. */
object HealthRoutes {

  private val dsl = Http4sDsl[Task]
  import dsl._

  val routes: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case GET -> Root / "health" => Ok("ok")
  }
}
