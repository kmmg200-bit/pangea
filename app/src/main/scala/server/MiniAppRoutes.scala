package server

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Request, Response}
import org.typelevel.ci.CIStringSyntax
import pangea.model.hero.Hero
import pangea.service.miniapp.{MiniApp, VkLaunchParams}
import pangea.service.miniapp.MiniApp.Denied
import pangea.service.miniapp.MiniAppView._
import zio.Task
import zio.interop.catz._

/** API VK Mini App: `/api/app/...`. Кто спрашивает, говорят параметры запуска
  * приложения в заголовке `X-Launch-Params` — их подписывает ВК (см.
  * [[VkLaunchParams]]).
  *
  * Ответы с ошибкой — JSON с полем `error`, по нему приложение пишет игроку,
  * что делать: `expired` — открыть приложение заново, `no-hero` — начать игру в
  * сообщениях сообщества. Ключ приложения не задан — 404 без тела, как будто
  * роута нет. */
final class MiniAppRoutes(app: MiniApp) {

  private val dsl = Http4sDsl[Task]
  import dsl._

  val routes: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case req @ GET -> Root / "api" / "app" / "inventory" =>
      asPlayer(req)(hero => app.inventory(hero).flatMap(view => Ok(view.asJson)))

    case req @ GET -> Root / "api" / "app" / "auction" =>
      asPlayer(req)(hero => app.auction(hero).flatMap(view => Ok(view.asJson)))
  }

  private def asPlayer(req: Request[Task])(answer: Hero => Task[Response[Task]]): Task[Response[Task]] = {
    val launch = req.headers.get(ci"X-Launch-Params").map(_.head.value).getOrElse("")
    app.player(launch).flatMap {
      case Right(hero)                                            => answer(hero)
      case Left(Denied.Off)                                       => NotFound()
      case Left(Denied.NoHero)                                    => NotFound(error("no-hero"))
      case Left(Denied.Rejected(VkLaunchParams.Rejected.Expired)) => Forbidden(error("expired"))
      case Left(Denied.Rejected(_))                               => Forbidden(error("bad-sign"))
    }
  }

  private def error(code: String): Json = Json.obj("error" -> Json.fromString(code))
}
