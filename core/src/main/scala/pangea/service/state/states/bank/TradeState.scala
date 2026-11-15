package pangea.service.state.states.bank

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Players, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemStack}
import pangea.model.state.StateType
import pangea.model.trade.{TradeOffer, TradeSide, TradeStatus}
import pangea.model.user.{User, UserId}
import pangea.repository.user.UserRepository
import pangea.service.parcel.Transfers
import pangea.service.state.states.bank.TradeState._
import pangea.service.state.{CityExit, ItemMenu, State, UserAction}
import pangea.service.trade.Trades
import pangea.util.TimeLeft
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Обмен» в Торговом доме: открытые предложения — свои и чужие, — карточка
  * предложения и сборка своей стороны (вещи из того, что при герое, серебро,
  * дублоны, записка). Правила — у [[Trades]], общего с Mini App; экран только
  * собирает черновик в `scene_data` ([[TradeScene]]) и показывает итог.
  * Имя партнёра, суммы и записку пишут текстом. */
case class TradeState(heroDao: HeroDao, trades: Trades, transfers: Transfers, userRepo: UserRepository,
                      players: Players, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "TradeList"      -> Target.Run { (u, _, r) => writeScene(u, TradeScene.empty) *> showList(u, r) },
      "TradeOpen"      -> Target.Run { (u, ua, r) => idOf(ua).fold(showList(u, r))(showCard(u, _, r)) },
      "TradeNew"       -> Target.Run { (u, _, r) => askName(u, r) },
      "TradeEdit"      -> Target.Run { (u, ua, r) => idOf(ua).fold(showList(u, r))(startEdit(u, _, r)) },
      "TradeDraft"     -> Target.Run { (u, _, r) => update(u)(_.copy(awaiting = None, picking = None)) *> showDraft(u, r) },
      "TradeAdd"       -> Target.Run { (u, _, r) => update(u)(_.copy(picking = Some(Adding), page = 0)) *> showPicker(u, r) },
      "TradeDel"       -> Target.Run { (u, _, r) => update(u)(_.copy(picking = Some(Removing), page = 0)) *> showPicker(u, r) },
      "TradePrev"      -> Target.Run { (u, _, r) => update(u)(s => s.copy(page = (s.page - 1).max(0))) *> showPicker(u, r) },
      "TradeNext"      -> Target.Run { (u, _, r) => update(u)(s => s.copy(page = s.page + 1)) *> showPicker(u, r) },
      "TradeSilver"    -> Target.Run { (u, _, r) => ask(u, AskSilver, r) },
      "TradeDoubloons" -> Target.Run { (u, _, r) => ask(u, AskDoubloons, r) },
      "TradeNote"      -> Target.Run { (u, _, r) => ask(u, AskNote, r) },
      "TradeSend"      -> Target.Run { (u, _, r) => send(u, r) },
      "TradeDraftBack" -> Target.Run { (u, _, r) => readScene(u).flatMap(s => s.offer.fold(showList(u, r))(showCard(u, _, r))) },
      "TradeAgree"     -> Target.Run { (u, ua, r) => idOf(ua).fold(showList(u, r))(reask(u, _, r, agreeing = true)) },
      "TradeAgreeYes"  -> Target.Run { (u, ua, r) => idOf(ua).fold(showList(u, r))(agree(u, _, r)) },
      "TradeAccept"    -> Target.Run { (u, ua, r) => idOf(ua).fold(showList(u, r))(reask(u, _, r, agreeing = false)) },
      "TradeAcceptYes" -> Target.Run { (u, ua, r) => idOf(ua).fold(showList(u, r))(accept(u, _, r)) },
      "TradeCancel"    -> Target.Run { (u, ua, r) => idOf(ua).fold(showList(u, r))(cancel(u, _, r)) },
      "TradeLeave"     -> Target.Goto(StateType.TradeHouse),
      CityExit.route
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    writeScene(user, TradeScene.empty) *> showList(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Список ─────────────────────────────────────────────────────────────────

  /** Открытые предложения: строкой каждое — с кем, что отдаёте и что получаете,
    * чей ход; под ними кнопка на каждое и «Предложить обмен». */
  private def showList(user: User, renderer: Renderer, note: String = ""): Task[StateType] =
    for {
      offers <- trades.open(user.userId)
      shown   = offers.take(MaxListed)
      named  <- ZIO.foreach(shown)(o => nameOf(o.partnerOf(user.userId)).map(o -> _))
      rows    = named.zipWithIndex.map { case ((o, name), i) =>
                  content.format("trade.menu.row", "n" -> (i + 1).toString, "name" -> name,
                    "mine" -> trades.describe(o.sideOf(user.userId)),
                    "theirs" -> trades.describe(o.sideOf(o.partnerOf(user.userId))),
                    "turn" -> content.text(turnKey("trade.menu", o, user.userId)))
                }
      more    = Option.when(offers.size > shown.size)(content.format("trade.menu.more", "n" -> (offers.size - shown.size).toString))
      text    = (content.format("trade.menu.head", "fee" -> Transfers.FeePct.toString) ::
                  (if (rows.isEmpty) List(content.text("trade.menu.empty")) else rows ++ more)).mkString("\n\n")
      buttons = named.zipWithIndex.map { case ((o, name), i) =>
                  val yours = o.turn == user.userId
                  Choice("TradeOpen", ItemMenu.truncate(content.format(
                    if (yours) "trade.menu.openYours" else "trade.menu.openTheirs", "n" -> (i + 1).toString, "name" -> name)),
                    data = Map(IdKey -> o.id.toString), color = if (yours) ChoiceColor.Positive else ChoiceColor.Primary,
                    row = Some(i))
                } ++ List(
                  Choice("TradeNew", content.text("trade.menu.new"), color = ChoiceColor.Positive, row = Some(shown.size)),
                  Choice("TradeLeave", content.text("trade.menu.leave"), color = ChoiceColor.Negative, row = Some(shown.size + 1)),
                  CityExit.button(content, Some(shown.size + 1)))
      _      <- renderer.show(user, Screen(text, buttons).noted(note))
    } yield StateType.Trade

  // ── Карточка предложения ───────────────────────────────────────────────────

  /** Обе стороны, записки, чей ход и сколько ждать. В свой ход — «Согласиться»
    * (а если партнёр уже согласен — «Подтвердить») и «Изменить своё»; отозвать
    * или отклонить — всегда. Закрыто — назад к списку. */
  private def showCard(user: User, id: Long, renderer: Renderer, note: String = ""): Task[StateType] =
    openOffer(user, id).flatMap {
      case None => showList(user, renderer, content.text("trade.closed"))
      case Some(offer) =>
        for {
          name   <- nameOf(offer.partnerOf(user.userId))
          now    <- nowMs
          mine    = offer.sideOf(user.userId)
          theirs  = offer.sideOf(offer.partnerOf(user.userId))
          yours   = offer.turn == user.userId
          text    = (List(
                      content.format("trade.card.head", "n" -> offer.id.toString, "name" -> name),
                      content.format("trade.card.mine", "what" -> trades.describe(mine))) ++
                      mine.note.map(n => content.format("trade.card.myNote", "note" -> n)) ++
                      List(content.format("trade.card.theirs", "what" -> trades.describe(theirs))) ++
                      theirs.note.map(n => content.format("trade.card.theirNote", "note" -> n)) ++
                      List(content.text(turnKey("trade.card", offer, user.userId)),
                        content.format("trade.card.until", "left" -> TimeLeft.format((offer.expiresAt - now).max(0L)))))
                      .mkString("\n")
          data    = Map(IdKey -> offer.id.toString)
          confirm = offer.partnerAgreed(user.userId)
          buttons = List(
                      Option.when(yours && !(mine.isEmpty && theirs.isEmpty))(
                        if (confirm) Choice("TradeAccept", content.text("trade.card.accept"), data = data, color = ChoiceColor.Positive, row = Some(0))
                        else Choice("TradeAgree", content.text("trade.card.agree"), data = data, color = ChoiceColor.Positive, row = Some(0))),
                      Option.when(yours)(Choice("TradeEdit", content.text("trade.card.edit"), data = data, row = Some(0))),
                      Some(Choice("TradeCancel",
                        content.text(if (offer.from == user.userId) "trade.card.cancel" else "trade.card.decline"),
                        data = data, color = ChoiceColor.Negative, row = Some(1))),
                      Some(Choice("TradeList", content.text("trade.card.back"), row = Some(1)))
                    ).flatten
          _      <- writeScene(user, TradeScene.empty.copy(offer = Some(offer.id)))
          _      <- renderer.show(user, Screen(text, buttons).noted(note))
        } yield StateType.Trade
    }

  /** Чей ход словами: свой — «согласиться» или, если партнёр согласен,
    * «подтвердить»; партнёра — ждём ответа или, если сами согласились,
    * подтверждения. `prefix` — раздел текстов: список или карточка. */
  private def turnKey(prefix: String, offer: TradeOffer, user: UserId): String =
    if (offer.turn == user) s"$prefix.${if (offer.partnerAgreed(user)) "confirmTurn" else "yourTurn"}"
    else s"$prefix.${if (offer.agreed(user)) "waitConfirm" else "theirTurn"}"

  /** Переспросить, что отдаёшь и что получаешь: перед согласием и перед
    * подтверждением — у обоих после него обмен может состояться. */
  private def reask(user: User, id: Long, renderer: Renderer, agreeing: Boolean): Task[StateType] =
    openOffer(user, id).flatMap {
      case None => showList(user, renderer, content.text("trade.closed"))
      case Some(offer) =>
        val data = Map(IdKey -> offer.id.toString)
        renderer.show(user, Screen(content.format(if (agreeing) "trade.agreeAsk" else "trade.confirm",
          "mine"   -> trades.describe(offer.sideOf(user.userId)),
          "theirs" -> trades.describe(offer.sideOf(offer.partnerOf(user.userId)))), List(
          Choice(if (agreeing) "TradeAgreeYes" else "TradeAcceptYes",
            content.text(if (agreeing) "trade.agreeYes" else "trade.confirmYes"), data = data, color = ChoiceColor.Positive, row = Some(0)),
          Choice("TradeOpen", content.text("trade.confirmNo"), data = data, color = ChoiceColor.Negative, row = Some(0))
        ))).as(StateType.Trade)
    }

  private def agree(user: User, id: Long, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      now  <- nowMs
      res  <- trades.agree(user, hero, id, now)
      next <- res match {
                case Right(_)  => showList(user, renderer, content.text("trade.done.agreed"))
                case Left(why) => showCard(user, id, renderer, why)
              }
    } yield next

  private def accept(user: User, id: Long, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      now  <- nowMs
      res  <- trades.confirm(user, hero, id, now)
      next <- res match {
                case Right(o) =>
                  showList(user, renderer, content.format("trade.done.accepted",
                    "got" -> trades.describe(o.sideOf(o.partnerOf(user.userId)))))
                case Left(why) => showCard(user, id, renderer, why)
              }
    } yield next

  private def cancel(user: User, id: Long, renderer: Renderer): Task[StateType] =
    nowMs.flatMap(trades.cancel(user, id, _)).flatMap {
      case Right(o) =>
        showList(user, renderer, trades.closedNote(user.userId, o))
      case Left(why) => showCard(user, id, renderer, why)
    }

  // ── Своя сторона ───────────────────────────────────────────────────────────

  private def askName(user: User, renderer: Renderer, note: String = ""): Task[StateType] =
    writeScene(user, TradeScene.empty.copy(awaiting = Some(AskName))) *>
      renderer.show(user, Screen(content.text("trade.ask.name"), List(
        Choice("TradeList", content.text("trade.cancelLabel"), color = ChoiceColor.Negative, row = Some(0))
      )).noted(note)).as(StateType.Trade)

  /** Имя пришло текстом: такой герой есть и он не ты — к своей стороне. */
  private def takeName(user: User, raw: String, renderer: Renderer): Task[StateType] =
    heroDao.heroByName(raw.trim).flatMap {
      case None                                     => askName(user, renderer, content.format("trade.noPartner", "name" -> raw.trim))
      case Some(h) if h.userId == user.userId       => askName(user, renderer, content.text("trade.self"))
      case Some(h) =>
        writeScene(user, TradeScene.empty.copy(partner = Some(h.name.getOrElse(raw.trim)), drafting = true)) *>
          showDraft(user, renderer)
    }

  /** Изменить своё — с того, что уже лежит в закладе. Не свой ход — карточка с
    * отказом. */
  private def startEdit(user: User, id: Long, renderer: Renderer): Task[StateType] =
    openOffer(user, id).flatMap {
      case None                                     => showList(user, renderer, content.text("trade.closed"))
      case Some(offer) if offer.turn != user.userId => showCard(user, id, renderer, content.text("trade.notYourTurn"))
      case Some(offer) =>
        val mine = offer.sideOf(user.userId)
        writeScene(user, TradeScene.empty.copy(offer = Some(id), drafting = true, items = mine.items.map(_.id),
          silver = mine.silver, doubloons = mine.doubloons, note = mine.note)) *> showDraft(user, renderer)
    }

  /** Черновик своей стороны: что кладёте и записка, кнопки — добавить и убрать
    * вещь, суммы, записка, отправить. */
  private def showDraft(user: User, renderer: Renderer, note: String = ""): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      pool  <- poolOf(user, hero, scene)
      name  <- partnerName(user, scene)
      side   = TradeSide(scene.items.flatMap(id => pool.find(_.id == id)), scene.silver, 0L, scene.doubloons, 0L, scene.note)
      text   = (List(content.format("trade.draft.head", "name" -> name),
                 content.format("trade.draft.what", "what" -> trades.describe(side))) ++
                 side.note.map(n => content.format("trade.draft.note", "note" -> n)) ++
                 List(content.format("trade.draft.hint", "fee" -> Transfers.FeePct.toString))).mkString("\n")
      buttons = List(
                  Some(Choice("TradeAdd", content.text("trade.draft.addItem"), row = Some(0))),
                  Option.when(side.items.nonEmpty)(Choice("TradeDel", content.text("trade.draft.delItem"), row = Some(0))),
                  Some(Choice("TradeSilver", content.text("trade.draft.silver"), row = Some(1))),
                  Some(Choice("TradeDoubloons", content.text("trade.draft.doubloons"), row = Some(1))),
                  Some(Choice("TradeNote", content.text("trade.draft.noteLabel"), row = Some(1))),
                  Some(Choice("TradeSend", content.text(if (scene.offer.isDefined) "trade.draft.reply" else "trade.draft.send"),
                    color = ChoiceColor.Positive, row = Some(2))),
                  Some(Choice("TradeDraftBack", content.text("trade.cancelLabel"), color = ChoiceColor.Negative, row = Some(2)))
                ).flatten
      _     <- renderer.show(user, Screen(text, buttons).noted(note))
    } yield StateType.Trade

  /** Список вещей: что положить (из того, что при герое и передаётся) или что
    * убрать (из черновика). Нажал — вещь в черновике или убрана, список
    * остаётся: так кладут по несколько. */
  private def showPicker(user: User, renderer: Renderer, note: String = ""): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      pool  <- poolOf(user, hero, scene)
      adding = !scene.picking.contains(Removing)
      list  <- if (adding) transfers.carried(hero).map(_.filter(i => transfers.sendable(i) && !scene.items.contains(i.id)))
               else ZIO.succeed(scene.items.flatMap(id => pool.find(_.id == id)))
      // Список опустел: только что положили (убрали) последнее — так и сказать,
      // а «нечего положить» — когда класть было нечего с самого начала.
      next  <- if (list.isEmpty)
                 update(user)(_.copy(picking = None)) *>
                   showDraft(user, renderer,
                     if (note.nonEmpty) note else content.text(if (adding) "trade.pick.none" else "trade.pick.empty"))
               else {
                 val (pageItems, pages, p) = ItemMenu.page(ItemStack.grouped(list), scene.page)
                 val prefix  = if (adding) AddPrefix else DelPrefix
                 val buttons = pageItems.zipWithIndex.map { case ((item, count), i) =>
                   Choice(s"$prefix${item.id}", ItemMenu.truncate(item.displayTitle + ItemStack.countSuffix(count)), row = Some(i))
                 } ++ List(
                   Some(Choice("TradeDraft", content.text("trade.pick.done"), color = ChoiceColor.Positive, row = Some(ItemMenu.NavRow))),
                   Option.when(p > 0)(Choice("TradePrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
                   Option.when(p < pages - 1)(Choice("TradeNext", content.text("common.next"), row = Some(ItemMenu.NavRow)))
                 ).flatten
                 val head = content.format(if (adding) "trade.pick.add" else "trade.pick.del",
                   "page" -> (p + 1).toString, "total" -> pages.toString)
                 renderer.show(user, Screen(head, buttons).noted(note)).as(StateType.Trade)
               }
    } yield next

  /** Нажали вещь в списке: положить её в черновик или убрать — и снова список. */
  private def pick(user: User, id: Long, adding: Boolean, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      pool  <- poolOf(user, hero, scene)
      found  = pool.find(_.id == id).filter(i => adding != scene.items.contains(i.id))
      _     <- ZIO.foreachDiscard(found)(i => update(user)(s =>
                 if (adding) s.copy(items = s.items :+ i.id) else s.copy(items = s.items.filterNot(_ == i.id))))
      next  <- showPicker(user, renderer, found.fold("")(i =>
                 content.format(if (adding) "trade.pick.added" else "trade.pick.removed", "name" -> i.displayTitle)))
    } yield next

  private def ask(user: User, what: String, renderer: Renderer, note: String = ""): Task[StateType] =
    update(user)(_.copy(awaiting = Some(what), picking = None)) *>
      renderer.show(user, Screen(content.format(s"trade.ask.$what", "fee" -> Transfers.FeePct.toString,
        "max" -> Trades.MaxNote.toString), List(
        Choice("TradeDraft", content.text("trade.ask.back"), row = Some(0))
      )).noted(note)).as(StateType.Trade)

  /** Текст по вопросу: имя, сумма или записка. */
  private def typed(user: User, scene: TradeScene, what: String, raw: String, renderer: Renderer): Task[StateType] =
    what match {
      case AskName => takeName(user, raw, renderer)
      case AskNote =>
        val text = raw.trim
        if (text.length > Trades.MaxNote) ask(user, AskNote, renderer, content.format("trade.noteTooLong", "max" -> Trades.MaxNote.toString))
        else writeScene(user, scene.copy(note = Option.unless(text == "-" || text.isEmpty)(text), awaiting = None)) *> showDraft(user, renderer)
      case money =>
        raw.trim.replace(" ", "").toLongOption.filter(_ >= 0L) match {
          case None => ask(user, money, renderer, content.text("trade.ask.notNumber"))
          case Some(n) =>
            val next = if (money == AskSilver) scene.copy(silver = n) else scene.copy(doubloons = n)
            writeScene(user, next.copy(awaiting = None)) *> showDraft(user, renderer)
        }
    }

  /** Отправить: новое предложение или ответ. Не вышло — черновик с причиной. */
  private def send(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      now   <- nowMs
      draft  = Trades.Draft(scene.items, scene.silver, scene.doubloons, scene.note)
      res   <- (scene.offer, scene.partner) match {
                 case (Some(id), _)   => trades.answer(user, hero, id, draft, now).map(_.map(_ -> "trade.done.answered"))
                 case (None, Some(p)) => trades.propose(user, hero, p, draft, now).map(_.map(_ -> "trade.done.sent"))
                 case (None, None)    => ZIO.left(content.text("trade.closed"))
               }
      next  <- res match {
                 case Right((offer, key)) => showCard(user, offer.id, renderer, content.text(key))
                 case Left(why) if scene.drafting => showDraft(user, renderer, why)
                 case Left(why)                   => showList(user, renderer, why)
               }
    } yield next

  // ── Вспомогательное ────────────────────────────────────────────────────────

  /** Нажатая вещь из списка или текст по вопросу; прочее — тот же экран заново. */
  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    readScene(user).flatMap { scene =>
      val action = field(ua.payload, "action")
      action.filter(_.startsWith(AddPrefix)).flatMap(_.drop(AddPrefix.length).toLongOption).map(pick(user, _, adding = true, renderer))
        .orElse(action.filter(_.startsWith(DelPrefix)).flatMap(_.drop(DelPrefix.length).toLongOption).map(pick(user, _, adding = false, renderer)))
        .orElse(Option.when(ua.payload.isEmpty && ua.text.trim.nonEmpty)(()).flatMap(_ => scene.awaiting)
          .map(typed(user, scene, _, ua.text, renderer)))
        .getOrElse(
          if (scene.awaiting.contains(AskName)) askName(user, renderer)
          else if (scene.drafting) showDraft(user, renderer)
          else scene.offer.fold(showList(user, renderer))(showCard(user, _, renderer)))
    }

  /** Предложение — если оно открыто и игрок в нём участвует. */
  private def openOffer(user: User, id: Long): Task[Option[TradeOffer]] =
    trades.get(id).map(_.filter(o => o.isParty(user.userId) && o.status == TradeStatus.Open))

  /** Откуда берутся вещи черновика: что уже в закладе и что при герое. */
  private def poolOf(user: User, hero: Hero, scene: TradeScene): Task[List[Item]] =
    for {
      held  <- scene.offer.fold(ZIO.succeed(List.empty[Item]): Task[List[Item]])(id => openOffer(user, id).map(_.toList.flatMap(_.sideOf(user.userId).items)))
      carry <- transfers.carried(hero)
    } yield held ++ carry

  private def partnerName(user: User, scene: TradeScene): Task[String] =
    scene.offer.fold(ZIO.succeed(scene.partner.getOrElse("")): Task[String])(id =>
      openOffer(user, id).flatMap(_.fold(ZIO.succeed(""): Task[String])(o => nameOf(o.partnerOf(user.userId)))))

  private def nameOf(userId: UserId): Task[String] =
    userRepo.getUserById(userId).flatMap {
      case Some(u) => players.getDisplayName(u).orElse(ZIO.succeed(content.text("transfer.someone")))
      case None    => ZIO.succeed(content.text("transfer.someone"))
    }

  private def idOf(ua: UserAction): Option[Long] = field(ua.payload, IdKey).flatMap(_.toLongOption)

  private def field(payload: Option[String], key: String): Option[String] =
    payload.flatMap(p => io.circe.parser.parse(p).toOption).flatMap(_.hcursor.get[String](key).toOption)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).someOrFail(new Throwable(s"Нет героя у игрока ${user.userId.value}"))

  private def readScene(user: User): Task[TradeScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[TradeScene].toOption).getOrElse(TradeScene.empty))

  private def writeScene(user: User, scene: TradeScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def update(user: User)(f: TradeScene => TradeScene): Task[Unit] =
    readScene(user).flatMap(s => writeScene(user, f(s)))

  private val nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
}

object TradeState {

  /** Черновик и где игрок на экране обмена: `offer` — карточка или ответ на это
    * предложение; `partner` — новое предложение этому герою; `drafting` —
    * собирается своя сторона (`items`, суммы, записка); `awaiting` — чего ждём
    * текстом; `picking` — открыт список вещей: что положить или что убрать. */
  final case class TradeScene(
    offer:     Option[Long],
    partner:   Option[String],
    drafting:  Boolean,
    items:     List[Long],
    silver:    Long,
    doubloons: Long,
    note:      Option[String],
    awaiting:  Option[String],
    picking:   Option[String],
    page:      Int
  )

  object TradeScene {
    val empty: TradeScene = TradeScene(None, None, drafting = false, Nil, 0L, 0L, None, None, None, 0)

    implicit val encoder: Encoder[TradeScene] = (s: TradeScene) => Json.obj(
      "offer" -> s.offer.asJson, "partner" -> s.partner.asJson, "drafting" -> s.drafting.asJson,
      "items" -> s.items.asJson, "silver" -> s.silver.asJson, "doubloons" -> s.doubloons.asJson,
      "note" -> s.note.asJson, "awaiting" -> s.awaiting.asJson, "picking" -> s.picking.asJson, "page" -> s.page.asJson)

    // Сцена чужого экрана или старая — читается пустой, а не роняет ход.
    implicit val decoder: Decoder[TradeScene] = (c: HCursor) =>
      for {
        offer     <- c.getOrElse[Option[Long]]("offer")(None)
        partner   <- c.getOrElse[Option[String]]("partner")(None)
        drafting  <- c.getOrElse[Boolean]("drafting")(false)
        items     <- c.getOrElse[List[Long]]("items")(Nil)
        silver    <- c.getOrElse[Long]("silver")(0L)
        doubloons <- c.getOrElse[Long]("doubloons")(0L)
        note      <- c.getOrElse[Option[String]]("note")(None)
        awaiting  <- c.getOrElse[Option[String]]("awaiting")(None)
        picking   <- c.getOrElse[Option[String]]("picking")(None)
        page      <- c.getOrElse[Int]("page")(0)
      } yield TradeScene(offer, partner, drafting, items, silver, doubloons, note, awaiting, picking, page)
  }

  /** Номер предложения в данных кнопки. */
  val IdKey = "id"

  /** Кнопки вещей в списке: положить в обмен и убрать из него. */
  val AddPrefix = "TradeAdd_"
  val DelPrefix = "TradeDel_"

  /** Что за список открыт. */
  val Adding   = "add"
  val Removing = "del"

  /** Чего ждём текстом — заодно ключ вопроса в `trade.ask`. */
  val AskName      = "name"
  val AskSilver    = "silver"
  val AskDoubloons = "doubloons"
  val AskNote      = "note"

  /** Сколько предложений показать строками и кнопками — клавиатура не резиновая. */
  val MaxListed = 8
}
