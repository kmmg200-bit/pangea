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
import pangea.service.miniapp.MiniApp.{CubeAct, Denied, GearAct, SquadAct, TradeAct}
import pangea.service.trade.Trades
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

    // Обмен: что открыто и что можно положить; действия меняют игру — только POST.
    case req @ GET -> Root / "api" / "app" / "trade" =>
      asPlayer(req)(hero => app.trade(hero).flatMap(view => Ok(view.asJson)))

    // Имена героев — подсказки, кому предложить обмен.
    case req @ GET -> Root / "api" / "app" / "heroes" =>
      asPlayer(req)(hero => app.heroNames(hero).flatMap(view => Ok(view.asJson)))

    case req @ POST -> Root / "api" / "app" / "trade" / "propose" :? PartnerParam(partner) +& ItemsParam(items)
        +& SilverParam(silver) +& DoubloonsParam(doubloons) +& NoteParam(note) =>
      tradeAct(req, TradeAct.Propose(partner, draft(items, silver, doubloons, note)))

    case req @ POST -> Root / "api" / "app" / "trade" / "answer" :? OfferParam(id) +& ItemsParam(items)
        +& SilverParam(silver) +& DoubloonsParam(doubloons) +& NoteParam(note) =>
      tradeAct(req, TradeAct.Answer(id, draft(items, silver, doubloons, note)))

    // Согласиться с условиями; «accept» — подтвердить, когда партнёр согласен.
    case req @ POST -> Root / "api" / "app" / "trade" / "agree" :? OfferParam(id) =>
      tradeAct(req, TradeAct.Agree(id))

    case req @ POST -> Root / "api" / "app" / "trade" / "accept" :? OfferParam(id) =>
      tradeAct(req, TradeAct.Accept(id))

    case req @ POST -> Root / "api" / "app" / "trade" / "cancel" :? OfferParam(id) =>
      tradeAct(req, TradeAct.Cancel(id))

    // Снаряжение: надеть вещь из сумки в слот, снять надетое. Меняют игру —
    // только POST.
    case req @ POST -> Root / "api" / "app" / "equipment" / "wear" :? SlotParam(slot) +& ItemParam(id) =>
      asPlayer(req)(hero => app.gear(hero, GearAct.Wear(slot, id)).flatMap(view => Ok(view.asJson)))

    case req @ POST -> Root / "api" / "app" / "equipment" / "take-off" :? SlotParam(slot) =>
      asPlayer(req)(hero => app.gear(hero, GearAct.TakeOff(slot)).flatMap(view => Ok(view.asJson)))

    // Камни и резец надетой вещи. Меняют игру — только POST.
    case req @ POST -> Root / "api" / "app" / "equipment" / "break-gem" :? SlotParam(slot) +& SocketParam(socket) =>
      asPlayer(req)(hero => app.gear(hero, GearAct.BreakGem(slot, socket)).flatMap(view => Ok(view.asJson)))

    case req @ POST -> Root / "api" / "app" / "equipment" / "insert-gem" :? SlotParam(slot) +& PlaceFrom(from) +& ItemParam(id) =>
      asPlayer(req)(hero => app.gear(hero, GearAct.InsertGem(slot, from, id)).flatMap(view => Ok(view.asJson)))

    case req @ POST -> Root / "api" / "app" / "equipment" / "chisel" :? SlotParam(slot) =>
      asPlayer(req)(hero => app.gear(hero, GearAct.UseChisel(slot)).flatMap(view => Ok(view.asJson)))

    // Пояс: положить из сумки (`slot` — вместо какой ячейки) и вынуть ячейку. Меняют игру — только POST.
    // Стопка из сумки: распаковать и собрать. Меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "bag" / "unpack" :? ItemParam(id) =>
      asPlayer(req)(hero => app.unpack(hero, id).flatMap(view => Ok(view.asJson)))

    case req @ POST -> Root / "api" / "app" / "bag" / "pack" :? ItemParam(id) =>
      asPlayer(req)(hero => app.pack(hero, id).flatMap(view => Ok(view.asJson)))

    // Камень из сумки: расколоть в пыль. Меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "bag" / "crush-gem" :? ItemParam(id) =>
      asPlayer(req)(hero => app.crush(hero, id).flatMap(view => Ok(view.asJson)))

    // Пыль из сумки: посыпать надетое оружие. Меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "bag" / "dust" :? ItemParam(id) =>
      asPlayer(req)(hero => app.dust(hero, id).flatMap(view => Ok(view.asJson)))

    // Трактат из сумки: прочитать. Меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "bag" / "read" :? ItemParam(id) =>
      asPlayer(req)(hero => app.read(hero, id).flatMap(view => Ok(view.asJson)))

    // Кнопка задания с карточки сюжетного предмета: `act` — её имя у бота. Меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "bag" / "quest" :? ItemParam(id) +& ActParam(act) =>
      asPlayer(req)(hero => app.quest(hero, id, act).flatMap(view => Ok(view.asJson)))

    case req @ POST -> Root / "api" / "app" / "belt" / "put" :? ItemParam(id) +& BeltCellParam(slot) =>
      asPlayer(req)(hero => app.gear(hero, GearAct.BeltPut(id, slot)).flatMap(view => Ok(view.asJson)))

    case req @ POST -> Root / "api" / "app" / "belt" / "take" :? SlotParam(slot) =>
      asPlayer(req)(hero => app.gear(hero, GearAct.BeltTake(slot)).flatMap(view => Ok(view.asJson)))

    // Отвар из сумки: выпить, смазать оружие, заправить флягу. Меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "brew" / how :? ItemParam(id) +& ArgParam(arg) =>
      MiniApp.BrewAct.parse(how, arg).fold(NotFound(error("off")))(act =>
        asPlayer(req)(hero => app.brew(hero, id, act).flatMap(view => Ok(view.asJson))))

    // Переложить вещь (или всю стопку) из одного хранилища в другое. Меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "storages" / "move" :? PlaceFrom(from) +& PlaceTo(to) +& ItemParam(id) +& AllParam(all) =>
      asPlayer(req)(hero => app.move(hero, from, to, id, all.getOrElse(false)).flatMap(view => Ok(view.asJson)))

    // Выбросить вещь (или всю стопку) из хранилища. Меняет игру — только POST.
    case req @ POST -> Root / "api" / "app" / "storages" / "drop" :? PlaceFrom(from) +& ItemParam(id) +& AllParam(all) =>
      asPlayer(req)(hero => app.drop(hero, from, id, all.getOrElse(false)).flatMap(view => Ok(view.asJson)))

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

    // Резерв: отправить туда из строя, выставить в строй (мест нет — вместо
    // союзника с места `swap`) или выгнать. В резерве союзник — номер `i` и имя.
    case req @ POST -> Root / "api" / "app" / "squad" / "reserve" :? AtParam(at) =>
      squadOrder(req, SquadAct.ToReserve(at))

    case req @ POST -> Root / "api" / "app" / "squad" / "reserve" / "deploy" :? IndexParam(i) +& WhoParam(who) +& SwapParam(swap) =>
      squadOrder(req, SquadAct.Deploy(i, who, swap))

    case req @ POST -> Root / "api" / "app" / "squad" / "reserve" / "dismiss" :? IndexParam(i) +& WhoParam(who) =>
      squadOrder(req, SquadAct.DismissReserve(i, who))

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

  private def tradeAct(req: Request[Task], act: TradeAct): Task[Response[Task]] =
    asPlayer(req)(hero => app.tradeAct(hero, act).flatMap(view => Ok(view.asJson)))

  private def draft(items: Option[String], silver: Option[Long], doubloons: Option[Long], note: Option[String]) =
    Trades.Draft(TradeAct.items(items), silver.getOrElse(0L), doubloons.getOrElse(0L), note)

  private object PartnerParam   extends QueryParamDecoderMatcher[String]("partner")
  private object OfferParam     extends QueryParamDecoderMatcher[Long]("id")
  private object ItemsParam     extends OptionalQueryParamDecoderMatcher[String]("items")
  private object SilverParam    extends OptionalQueryParamDecoderMatcher[Long]("silver")
  private object DoubloonsParam extends OptionalQueryParamDecoderMatcher[Long]("doubloons")
  private object NoteParam      extends OptionalQueryParamDecoderMatcher[String]("note")

  private object KindParam extends OptionalQueryParamDecoderMatcher[String]("kind")
  private object ItemParam extends QueryParamDecoderMatcher[Long]("item")
  private object ActParam  extends QueryParamDecoderMatcher[String]("act")
  private object LotParam  extends QueryParamDecoderMatcher[Long]("lot")
  private object SlotParam extends QueryParamDecoderMatcher[Int]("slot")
  private object SocketParam extends QueryParamDecoderMatcher[Int]("socket")
  private object BeltCellParam extends OptionalQueryParamDecoderMatcher[Int]("slot")
  private object ArgParam extends OptionalQueryParamDecoderMatcher[String]("arg")
  private object FromParam extends QueryParamDecoderMatcher[Int]("from")
  private object ToParam   extends QueryParamDecoderMatcher[Int]("to")
  private object AtParam   extends QueryParamDecoderMatcher[Int]("at")
  private object RackParam extends QueryParamDecoderMatcher[Int]("n")
  private object PlaceFrom extends QueryParamDecoderMatcher[String]("from")
  private object PlaceTo   extends QueryParamDecoderMatcher[String]("to")
  private object AllParam  extends OptionalQueryParamDecoderMatcher[Boolean]("all")
  private object IndexParam extends QueryParamDecoderMatcher[Int]("i")
  private object WhoParam   extends QueryParamDecoderMatcher[String]("who")
  private object SwapParam  extends OptionalQueryParamDecoderMatcher[Int]("swap")
}
