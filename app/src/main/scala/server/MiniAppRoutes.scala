package server

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Request, Response}
import org.typelevel.ci.CIStringSyntax
import pangea.model.hero.Hero
import pangea.model.top.TopKind
import pangea.service.miniapp.{MiniApp, VkLaunchParams}
import pangea.service.miniapp.MiniApp.{CubeAct, Denied, GearAct, SquadAct}
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
    case req @ GET -> Root / "api" / "app" / "character" =>
      asPlayer(req)(hero => app.character(hero).flatMap(view => Ok(view.asJson)))

    case req @ GET -> Root / "api" / "app" / "quests" =>
      asPlayer(req)(hero => app.quests(hero).flatMap(view => Ok(view.asJson)))

    case req @ GET -> Root / "api" / "app" / "squad" =>
      asPlayer(req)(hero => app.squad(hero).flatMap(view => Ok(view.asJson)))

    case req @ GET -> Root / "api" / "app" / "top" :? KindParam(kind) =>
      // Незнакомый вид топа — топ по уровню: приложение старее бота не ломается.
      val k = kind.flatMap(TopKind.withNameOption).getOrElse(TopKind.Level)
      asPlayer(req)(hero => app.top(hero, k).flatMap(view => Ok(view.asJson)))

    case req @ GET -> Root / "api" / "app" / "inventory" =>
      asPlayer(req)(hero => app.inventory(hero).flatMap(view => Ok(view.asJson)))

    case req @ GET -> Root / "api" / "app" / "auction" =>
      asPlayer(req)(hero => app.auction(hero).flatMap(view => Ok(view.asJson)))

    // Снаряжение: надеть вещь из сумки в слот, снять надетое. Меняют игру —
    // только POST.
    case req @ POST -> Root / "api" / "app" / "equipment" / "wear" :? SlotParam(slot) +& ItemParam(id) =>
      asPlayer(req)(hero => app.gear(hero, GearAct.Wear(slot, id)).flatMap(view => Ok(view.asJson)))

    case req @ POST -> Root / "api" / "app" / "equipment" / "take-off" :? SlotParam(slot) =>
      asPlayer(req)(hero => app.gear(hero, GearAct.TakeOff(slot)).flatMap(view => Ok(view.asJson)))

    // Переложить вещь (или всю стопку) из одного хранилища в другое. Меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "storages" / "move" :? PlaceFrom(from) +& PlaceTo(to) +& ItemParam(id) +& AllParam(all) =>
      asPlayer(req)(hero => app.move(hero, from, to, id, all.getOrElse(false)).flatMap(view => Ok(view.asJson)))

    // Стойка дома: надеть её комплект — обменяться надетым. Меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "racks" / "wear" :? RackParam(n) =>
      asPlayer(req)(hero => app.rack(hero, n).flatMap(view => Ok(view.asJson)))

    // Отряд: переставить союзника или героя, выгнать, решить про восставшего.
    // Меняют игру — только POST.
    case req @ POST -> Root / "api" / "app" / "squad" / "move" :? FromParam(from) +& ToParam(to) =>
      squadOrder(req, SquadAct.Move(from, to))

    case req @ POST -> Root / "api" / "app" / "squad" / "hero" :? ToParam(to) =>
      squadOrder(req, SquadAct.MoveHero(to))

    case req @ POST -> Root / "api" / "app" / "squad" / "dismiss" :? AtParam(at) =>
      squadOrder(req, SquadAct.Dismiss(at))

    case req @ POST -> Root / "api" / "app" / "squad" / "risen" / "take" :? AtParam(at) =>
      squadOrder(req, SquadAct.TakeRisen(at))

    case req @ POST -> Root / "api" / "app" / "squad" / "risen" / "dismiss" =>
      squadOrder(req, SquadAct.DismissRisen)

    // Покупка лота: меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "auction" / "buy" :? LotParam(id) =>
      asPlayer(req)(hero => app.buy(hero, id).flatMap(view => Ok(view.asJson)))

    case req @ GET -> Root / "api" / "app" / "craft" =>
      asPlayer(req)(hero => app.craft(hero).flatMap(view => Ok(view.asJson)))

    // Куб Азата: перенос вещи пальцем, «Забрать всё», «Активировать». Меняют игру —
    // только POST.
    case req @ POST -> Root / "api" / "app" / "craft" / "cube" / "deposit" :? ItemParam(id) =>
      cubeAct(req, CubeAct.Deposit(id))

    case req @ POST -> Root / "api" / "app" / "craft" / "cube" / "withdraw" :? ItemParam(id) =>
      cubeAct(req, CubeAct.Withdraw(id))

    case req @ POST -> Root / "api" / "app" / "craft" / "cube" / "take-all" =>
      cubeAct(req, CubeAct.TakeAll)

    case req @ POST -> Root / "api" / "app" / "craft" / "cube" / "activate" =>
      cubeAct(req, CubeAct.Activate)
  }

  private def cubeAct(req: Request[Task], act: CubeAct): Task[Response[Task]] =
    asPlayer(req)(hero => app.cube(hero, act).flatMap(view => Ok(view.asJson)))

  private def squadOrder(req: Request[Task], act: SquadAct): Task[Response[Task]] =
    asPlayer(req)(hero => app.squadOrder(hero, act).flatMap(view => Ok(view.asJson)))

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

  private object KindParam extends OptionalQueryParamDecoderMatcher[String]("kind")
  private object ItemParam extends QueryParamDecoderMatcher[Long]("item")
  private object LotParam  extends QueryParamDecoderMatcher[Long]("lot")
  private object SlotParam extends QueryParamDecoderMatcher[Int]("slot")
  private object FromParam extends QueryParamDecoderMatcher[Int]("from")
  private object ToParam   extends QueryParamDecoderMatcher[Int]("to")
  private object AtParam   extends QueryParamDecoderMatcher[Int]("at")
  private object RackParam extends QueryParamDecoderMatcher[Int]("n")
  private object PlaceFrom extends QueryParamDecoderMatcher[String]("from")
  private object PlaceTo   extends QueryParamDecoderMatcher[String]("to")
  private object AllParam  extends OptionalQueryParamDecoderMatcher[Boolean]("all")
}
