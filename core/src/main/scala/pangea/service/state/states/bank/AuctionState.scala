package pangea.service.state.states.bank

import io.circe.{Decoder, Encoder, Json, jawn}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Players, Renderer, SceneContent, Screen, Target}
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.auction.{AuctionRepoError, AuctionRepository}
import pangea.repository.bank.BankRepository
import pangea.service.auction.AuctionPurchase
import pangea.repository.inventory.InventoryRepository
import pangea.repository.user.UserRepository
import pangea.service.payout.Payouts
import pangea.service.purse.Purse
import pangea.service.chat.Links
import pangea.service.state.states.bank.AuctionState._
import pangea.model.quest.{DailyKind, DailyNpc}
import pangea.service.state.{CityExit, DailyQuestLog, ItemMenu, State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Аукцион Торгового дома. Любую вещь, которая занимает место, можно выставить
 *  за серебро или дублоны (не дешевле [[AuctionLot.MinPrice]]), заплатив
 *  Рахадиму десятину от запрошенной цены. Лот живёт неделю, потом уходит в
 *  «непроданные» и ждёт хозяина. О каждом новом лоте бот пишет в торговую беседу
 *  Пангеи — там же виден его номер, который можно просто написать в ответ.
 *
 *  Торговать можно и без ячейки в Торговом доме: купленное и снятое с торгов
 *  идёт в сумку, а выручка — в ячейку продавца; нет её, полна или монета не та —
 *  деньги ждут его в городе.
 *
 *  Гонку двух покупателей ловит сам аукцион: лот закрывается одним UPDATE, и
 *  тому, кто опоздал на пару секунд, приходит «лот уже ушёл» — до списания
 *  денег дело не доходит. */
case class AuctionState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  auctionRepo:   AuctionRepository,
  userRepo:      UserRepository,
  bankRepo:      BankRepository,
  payouts:       Payouts,
  players:       Players,
  content:       SceneContent
) extends State {

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, Some(bankRepo))

  private val purchase =
    new AuctionPurchase(heroDao, inventoryRepo, auctionRepo, userRepo, bankRepo, payouts, players, content)

  private val branch = new Branch(
    routes = Map(
      "Auction"       -> Target.Run { (u, _, r)  => resetScene(u) *> showMenu(u, r).as(StateType.Auction) },
      "AuctionBrowse" -> Target.Run { (u, _, r)  => writeScene(u, AuctionScene(page = Some(0))) *> showBrowse(u, r).as(StateType.Auction) },
      "AuctionPrev"   -> Target.Run { (u, _, r)  => turnPage(u, r, -1).as(StateType.Auction) },
      "AuctionNext"   -> Target.Run { (u, _, r)  => turnPage(u, r, +1).as(StateType.Auction) },
      "ShowLot"       -> Target.Run { (u, ua, r) => withLotId(ua)(showLot(u, _, r)).as(StateType.Auction) },
      "BuyLot"        -> Target.Run { (u, ua, r) => withLotId(ua)(confirmBuy(u, _, r)).as(StateType.Auction) },
      "BuyLotYes"     -> Target.Run { (u, ua, r) => withLotId(ua)(buy(u, _, r)).as(StateType.Auction) },
      "AuctionSell"   -> Target.Run { (u, _, r)  => writeScene(u, AuctionScene(sellPage = Some(0))) *> showSellList(u, r).as(StateType.Auction) },
      "SellPrev"      -> Target.Run { (u, _, r)  => turnSellPage(u, r, -1).as(StateType.Auction) },
      "SellNext"      -> Target.Run { (u, _, r)  => turnSellPage(u, r, +1).as(StateType.Auction) },
      "SellCurrency"  -> Target.Run { (u, ua, r) => askPrice(u, ua, r).as(StateType.Auction) },
      "SellConfirm"   -> Target.Run { (u, _, r)  => listLot(u, r).as(StateType.Auction) },
      "MyLots"        -> Target.Run { (u, _, r)  => showMine(u, r).as(StateType.Auction) },
      "Reclaim"       -> Target.Run { (u, ua, r) => withLotId(ua)(reclaim(u, _, r)).as(StateType.Auction) },
      "LeaveAuction"  -> Target.Goto(StateType.TradeHouse),
      CityExit.route
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  /** Вход в аукцион: приглашение в торговый чат — отдельным сообщением и без
    * кнопок, а следом меню. Ссылку в само меню не кладём: сообщение со ссылкой
    * (ВК рисует под ней карточку беседы) и с клавиатурой разом у игроков не
    * обновляло кнопки — оставались прежние, и действия будто не проходили. По
    * той же причине приглашение не повторяется при каждом «Назад» в меню. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    resetScene(user) *>
      renderer.show(user, Screen(content.format("bank.auction.tradeChat", "tradeChat" -> Links.TradeChat), Nil)) *>
      showMenu(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Меню ───────────────────────────────────────────────────────────────────

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      total <- auctionRepo.onSale(now).orElse(ZIO.succeed(0L))
      _     <- renderer.show(user, Screen(
                 content.format("bank.auction.menu", "onSale" -> total.toString),
                 List(
                   Choice("AuctionBrowse", content.text("bank.auction.browseLabel"), row = Some(0)),
                   Choice("AuctionSell",   content.text("bank.auction.sellLabel"),   row = Some(0)),
                   Choice("MyLots",        content.text("bank.auction.mineLabel"),   row = Some(1)),
                   Choice("LeaveAuction",  content.text("bank.auction.leave"), color = ChoiceColor.Negative, row = Some(2)),
                   CityExit.button(content, Some(2))
                 )))
    } yield ()

  // ── Витрина ────────────────────────────────────────────────────────────────

  private def showBrowse(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      scene <- readScene(user)
      res   <- auctionRepo.page(now, scene.page.getOrElse(0), ItemMenu.DefaultPageSize).mapError(asThrowable)
      (lots, pages, p) = res
      _ <- if (lots.isEmpty) renderer.show(user, Screen(content.text("bank.auction.empty"), backRow))
           else {
             // Не больше восьми лотов на страницу: девятый ряд занимает навигация.
             val buttons = lots.map(lot => Choice("ShowLot",
               ItemMenu.truncate(content.format("bank.auction.lotButton",
                 "id" -> lot.id.toString, "name" -> lot.title, "price" -> lot.priceLine)),
               data = Map("id" -> lot.id.toString)))
               .zipWithIndex.map { case (c, i) => c.copy(row = Some(i)) }
             val nav = List(
               Some(Choice("Auction", content.text("bank.auction.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))),
               Option.when(p > 0)(Choice("AuctionPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
               Option.when(p < pages - 1)(Choice("AuctionNext", content.text("common.next"), row = Some(ItemMenu.NavRow)))
             ).flatten
             // Навыки вещей — строками под заголовком: листая торги, их видно
             // сразу, не открывая каждый лот, а в кнопку (до 40 знаков) они не влезут.
             val skills = lots.flatMap(lot => lot.item.skillName.map(s => content.format("bank.auction.skillLine",
               "id" -> lot.id.toString, "name" -> lot.title, "skill" -> s)))
             renderer.show(user, Screen(
               content.format("bank.auction.browseHeader", "page" -> (p + 1).toString, "total" -> pages.toString) +
                 (if (skills.isEmpty) "" else "\n\n" + skills.mkString("\n")),
               buttons ++ nav))
           }
    } yield ()

  private def turnPage(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    for {
      scene <- readScene(user)
      _     <- writeScene(user, scene.copy(page = Some((scene.page.getOrElse(0) + delta).max(0))))
      _     <- showBrowse(user, renderer)
    } yield ()

  /** Карточка лота: всё о вещи, цена, что с лотом можно сделать и что надето
    * в тот же слот — для сравнения. */
  private def showLot(user: User, lotId: Long, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      res  <- auctionRepo.lot(lotId).either
      _ <- res match {
        case Left(_) => renderer.show(user, Screen(content.format("bank.auction.noSuchLot", "id" -> lotId.toString), Nil))
        case Right(lot) =>
          val stats = lot.item.statsLines
          val card  = content.format("bank.auction.card",
            "id"    -> lot.id.toString,
            "title" -> lot.title,
            // Описание вещи идёт сразу под именем: цену смотрят, уже зная, за что.
            "stats" -> (if (stats.isEmpty) "" else "\n" + stats.mkString("\n")),
            "price" -> lot.priceLine,
            "hours" -> lot.hoursLeft(now).toString)
          val own = lot.sellerId == hero.id
          val buttons =
            if (own && lot.unsold(now)) List(Choice("Reclaim", content.text("bank.auction.reclaimLabel"), data = Map("id" -> lot.id.toString), row = Some(0)))
            else if (own)               List(Choice("Reclaim", content.text("bank.auction.withdrawLabel"), data = Map("id" -> lot.id.toString), row = Some(0)))
            else if (lot.onSale(now))   List(Choice("BuyLot", Choice.fit(content.format("bank.auction.buyLabel", "price" -> lot.priceLine)),
                                          color = ChoiceColor.Positive, data = Map("id" -> lot.id.toString), row = Some(0)))
            else                        Nil
          val state = lotStateLine(lot, now, own)
          // Под лотом — что надето в тот же слот: брать ли, видно сразу.
          renderer.show(user, Screen(ItemMenu.withEquipped(card + state, lot.item, hero), buttons :+
            Choice("Auction", content.text("bank.auction.back"), color = ChoiceColor.Negative, row = Some(1))))
      }
    } yield ()

  private def lotStateLine(lot: AuctionLot, now: Long, own: Boolean): String =
    if (lot.unsold(now)) "\n\n" + content.text("bank.auction.stateUnsold")
    else if (own)        "\n\n" + content.text("bank.auction.stateOwn")
    else                 ""

  // ── Покупка ────────────────────────────────────────────────────────────────

  private def confirmBuy(user: User, lotId: Long, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      res  <- auctionRepo.lot(lotId).either
      _ <- res match {
        case Left(_) => renderer.show(user, Screen(content.format("bank.auction.noSuchLot", "id" -> lotId.toString), Nil))
        case Right(lot) if !lot.onSale(now) => renderer.show(user, Screen(content.text("bank.auction.lotGone"), backRow))
        case Right(lot) if lot.sellerId == hero.id => renderer.show(user, Screen(content.text("bank.auction.ownLot"), backRow))
        case Right(lot) =>
          renderer.show(user, Screen(
            content.format("bank.auction.buyConfirm",
              "id" -> lot.id.toString, "name" -> lot.title, "price" -> lot.priceLine),
            List(
              Choice("BuyLotYes", content.text("bank.auction.buyConfirmYes"), color = ChoiceColor.Positive, data = Map("id" -> lot.id.toString), row = Some(0)),
              Choice("Auction",   content.text("bank.auction.buyConfirmNo"),  color = ChoiceColor.Negative, row = Some(0))
            ), inline = true))
      }
    } yield ()

  /** Покупка — в [[AuctionPurchase]], общей с Mini App; здесь — что показать. */
  private def buy(user: User, lotId: Long, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap(purchase.buy(user, _, lotId)).flatMap {
      case AuctionPurchase.Result.Gone         => renderer.show(user, Screen(content.text("bank.auction.lotGone"), backRow))
      case AuctionPurchase.Result.Own          => renderer.show(user, Screen(content.text("bank.auction.ownLot"), backRow))
      case AuctionPurchase.Result.NoMoney(lot) =>
        renderer.show(user, Screen(content.format("bank.auction.noMoney", "price" -> lot.priceLine), backRow))
      case AuctionPurchase.Result.NoRoom       => renderer.show(user, Screen(content.text("bank.auction.noRoom"), backRow))
      case AuctionPurchase.Result.Bought(lot)  =>
        renderer.show(user, Screen(content.format("bank.auction.bought",
          "name" -> lot.title, "price" -> lot.priceLine), Nil)) *>
          afterPurchase(user, renderer)
    }

  /** После покупки — туда, откуда купили: на ту же страницу витрины. Лот
    * открыли по номеру из объявления, мимо витрины, — в меню аукциона. */
  private def afterPurchase(user: User, renderer: Renderer): Task[Unit] =
    readScene(user).flatMap(scene =>
      if (scene.page.isDefined) showBrowse(user, renderer) else showMenu(user, renderer))

  // ── Выставление ────────────────────────────────────────────────────────────

  private def showSellList(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      scene <- readScene(user)
      items  = inv.items.data.filter(sellable)
      _ <- if (items.isEmpty) renderer.show(user, Screen(content.text("bank.auction.nothingToSell"), backRow))
           else {
             val (pageItems, pages, p) = ItemMenu.page(items, scene.sellPage.getOrElse(0))
             val buttons = ItemMenu.itemButtons(pageItems, SellItemPrefix)
             val nav = List(
               Some(Choice("Auction", content.text("bank.auction.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))),
               Option.when(p > 0)(Choice("SellPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
               Option.when(p < pages - 1)(Choice("SellNext", content.text("common.next"), row = Some(ItemMenu.NavRow)))
             ).flatten
             renderer.show(user, Screen(
               content.format("bank.auction.sellHeader", "page" -> (p + 1).toString, "total" -> pages.toString),
               buttons ++ nav))
           }
    } yield ()

  /** Что можно выставить: не сюжетное (ни предмет, ни экипировка — см.
    * `Item.isStoryGear`), не трофей и занимающее место (пыль и малые руны места
    * не занимают, поэтому на торги не идут). Стопка место занимает — её
    * выставляют целиком, даже если в ней пыль (см. `Bundling`).
    *
    * Трофеи и сюжетная экипировка — личное: трофеи сдают в Гильдии за
    * репутацию, сюжетную экипировку носят сами или продают Ришелье, а не
    * продают друг другу, — ровно по той же причине, по которой их не передают
    * из рук в руки (см. [[pangea.service.parcel.Transfers.sendable]]). */
  private def sellable(item: Item): Boolean =
    !item.isQuestItem && !item.isStoryGear && !item.weightless && item.itemType != ItemType.Trophy && !item.isNamedTrophy

  private def turnSellPage(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    for {
      scene <- readScene(user)
      _     <- writeScene(user, scene.copy(sellPage = Some((scene.sellPage.getOrElse(0) + delta).max(0))))
      _     <- showSellList(user, renderer)
    } yield ()

  /** Выбрана вещь — спрашиваем монету. */
  private def askCurrency(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      scene <- readScene(user)
      _ <- inv.items.data.find(i => i.id == itemId && sellable(i)) match {
        case None => showSellList(user, renderer)
        case Some(item) =>
          writeScene(user, scene.copy(itemId = Some(itemId))) *>
            renderer.show(user, Screen(
              content.format("bank.auction.pickCurrency", "name" -> AuctionLot.titleOf(item)),
              AuctionCurrency.values.toList.map { c =>
                Choice("SellCurrency", s"${c.emoji} ${c.label}", data = Map("cur" -> c.entryName), row = Some(0))
              } :+ Choice("Auction", content.text("bank.auction.back"), color = ChoiceColor.Negative, row = Some(1))))
      }
    } yield ()

  /** Монета выбрана — просим цену текстом. */
  private def askPrice(user: User, ua: UserAction, renderer: Renderer): Task[Unit] =
    for {
      scene <- readScene(user)
      cur    = payload(ua, "cur").flatMap(AuctionCurrency.withNameOption)
      _ <- (scene.itemId, cur) match {
        case (Some(_), Some(c)) =>
          writeScene(user, scene.copy(currency = Some(c.entryName), mode = Some(ModePrice))) *>
            renderer.show(user, Screen(
              content.format("bank.auction.askPrice",
                "currency" -> s"${c.emoji} ${c.label}",
                "min"      -> AuctionLot.MinPrice.toString,
                "fee"      -> AuctionLot.FeePct.toString),
              cancelRow))
        case _ => showSellList(user, renderer)
      }
    } yield ()

  /** Цена введена — показываем, во что обойдётся выставление. */
  private def confirmPrice(user: User, priceText: String, renderer: Renderer): Task[Unit] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      item   = scene.itemId.flatMap(id => inv.items.data.find(i => i.id == id && sellable(i)))
      cur    = scene.currency.flatMap(AuctionCurrency.withNameOption)
      _ <- (item, cur, priceText.toLongOption) match {
        case (None, _, _) | (_, None, _) => resetScene(user) *> showMenu(user, renderer)
        case (_, _, None)                => renderer.show(user, Screen(content.text("bank.auction.priceNotANumber"), cancelRow))
        case (Some(_), Some(_), Some(p)) if p < AuctionLot.MinPrice =>
          renderer.show(user, Screen(content.format("bank.auction.priceTooLow", "min" -> AuctionLot.MinPrice.toString), cancelRow))
        case (Some(_), Some(_), Some(p)) if p > AuctionLot.MaxPrice =>
          renderer.show(user, Screen(content.format("bank.auction.priceTooHigh", "max" -> AuctionLot.MaxPrice.toString), cancelRow))
        case (Some(it), Some(c), Some(p)) =>
          writeScene(user, scene.copy(price = Some(p))) *>
            renderer.show(user, Screen(
              content.format("bank.auction.sellConfirm",
                "name"  -> AuctionLot.titleOf(it),
                "price" -> s"${c.emoji} $p",
                "fee"   -> s"${c.emoji} ${AuctionLot.fee(p)}"),
              List(
                Choice("SellConfirm", content.text("bank.auction.sellConfirmYes"), color = ChoiceColor.Positive, row = Some(0)),
                Choice("Auction",     content.text("bank.auction.sellConfirmNo"),  color = ChoiceColor.Negative, row = Some(0))
              ), inline = true))
      }
    } yield ()

  /** Плата за выставление, вещь из сумки — в лот, объявление в торговую беседу. */
  private def listLot(user: User, renderer: Renderer): Task[Unit] =
    for {
      now    <- nowMs
      scene  <- readScene(user)
      hero   <- getHero(user)
      inv    <- inventoryRepo.get(hero.id).mapError(asThrowable)
      wallet <- purse.wallet(hero)
      item    = scene.itemId.flatMap(id => inv.items.data.find(i => i.id == id && sellable(i)))
      cur     = scene.currency.flatMap(AuctionCurrency.withNameOption)
      mine   <- auctionRepo.mineCount(hero.id).mapError(asThrowable)
      _ <- (item, cur, scene.price) match {
        case (Some(_), _, _) if mine >= AuctionLot.MaxLots.toLong =>
          renderer.show(user, Screen(content.format("bank.auction.tooManyLots",
            "max" -> AuctionLot.MaxLots.toString), backRow))
        case (Some(it), Some(c), Some(price)) =>
          val fee = AuctionLot.fee(price)
          if (!hasFee(c, fee, hero, wallet.total))
            renderer.show(user, Screen(content.format("bank.auction.noFee", "fee" -> s"${c.emoji} $fee"), backRow))
          else
            chargeFee(user, hero, c, fee) *>
              // Вещь уходит из сумки в лот одной транзакцией с ним самим.
              auctionRepo.sell(hero.id, it, price, c, now).foldZIO(
                {
                  // Лот не записался — вещь осталась в сумке, деньги за сбор
                  // уже ушли; о сборе честно говорим в сообщении об отказе.
                  case e => renderer.show(user, Screen(content.text("bank.auction.listFailed"), backRow)) *>
                              ZIO.logError(s"auction sell failed: $e")
                },
                lot => players.announceTrade(AuctionLot.announcement(lot)).ignore *>
                         // Рахадиму нужно, чтобы торги шли, — лот идёт ему в книгу.
                         DailyQuestLog.add(heroDao, user.userId, DailyNpc.Rakhadim, DailyKind.BankLot, 1L) *>
                         resetScene(user) *>
                         renderer.show(user, Screen(content.format("bank.auction.listed",
                           "id" -> lot.id.toString, "name" -> AuctionLot.titleOf(it), "price" -> lot.priceLine,
                           "fee" -> s"${c.emoji} $fee"), Nil)) *>
                         showMenu(user, renderer)
              )
        case _ => resetScene(user) *> showMenu(user, renderer)
      }
    } yield ()

  private def hasFee(c: AuctionCurrency, fee: Long, hero: Hero, silverAvailable: Long): Boolean =
    c match {
      case AuctionCurrency.Silver    => silverAvailable >= fee
      case AuctionCurrency.Doubloons => hero.doubloons >= fee
    }

  private def chargeFee(user: User, hero: Hero, c: AuctionCurrency, fee: Long): Task[Unit] =
    c match {
      case AuctionCurrency.Silver    => purse.charge(user.userId, hero, fee).unit
      case AuctionCurrency.Doubloons => heroDao.updateDoubloons(user.userId, hero.doubloons - fee)
    }

  // ── Свои лоты ──────────────────────────────────────────────────────────────

  private def showMine(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      lots <- auctionRepo.mine(hero.id, MineLimit).mapError(asThrowable)
      _ <- if (lots.isEmpty) renderer.show(user, Screen(content.text("bank.auction.mineEmpty"), backRow))
           else {
             val lines = lots.map(mineLine(_, now))
             // В таблице только живые лоты, так что забрать можно любой из них.
             val actionable = lots.take(ItemMenu.DefaultPageSize)
             val buttons = actionable.zipWithIndex.map { case (lot, i) =>
               Choice("Reclaim",
                 ItemMenu.truncate(content.format(
                   if (lot.unsold(now)) "bank.auction.reclaimOne" else "bank.auction.withdrawOne",
                   "id" -> lot.id.toString, "name" -> lot.title)),
                 data = Map("id" -> lot.id.toString), row = Some(i))
             }
             renderer.show(user, Screen(
               content.text("bank.auction.mineHeader") + "\n" + lines.mkString("\n"),
               buttons :+ Choice("Auction", content.text("bank.auction.back"),
                 color = ChoiceColor.Negative, row = Some(buttons.size))))
           }
    } yield ()

  private def mineLine(lot: AuctionLot, now: Long): String = {
    val state =
      if (lot.unsold(now)) content.text("bank.auction.mineUnsold")
      else                 content.format("bank.auction.mineOnSale", "hours" -> lot.hoursLeft(now).toString)
    content.format("bank.auction.mineLine",
      "id" -> lot.id.toString, "name" -> lot.title, "price" -> lot.priceLine, "state" -> state)
  }

  /** Снять лот с торгов или забрать непроданное: вещь возвращается в сумку. */
  private def reclaim(user: User, lotId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      res  <- auctionRepo.lot(lotId).either
      _ <- res match {
        case Left(_) => renderer.show(user, Screen(content.format("bank.auction.noSuchLot", "id" -> lotId.toString), Nil))
        case Right(lot) if lot.sellerId != hero.id => renderer.show(user, Screen(content.text("bank.auction.notYours"), backRow))
        case Right(_) if inv.freeSlots <= 0        => renderer.show(user, Screen(content.text("bank.auction.noRoom"), backRow))
        case Right(lot) =>
          // Лот закрывается вместе с вещью в сумке. Упала транзакция — лот
          // на месте, а не «ушёл».
          auctionRepo.reclaim(lot.id, hero.id).foldZIO(
            {
              case AuctionRepoError.Failed => ZIO.fail(new Throwable(s"Лот ${lot.id} не вернулся: сбой базы"))
              case _                       => renderer.show(user, Screen(content.text("bank.auction.lotGone"), backRow))
            },
            _ => renderer.show(user, Screen(content.format("bank.auction.reclaimed",
                   "id" -> lot.id.toString, "name" -> lot.title), Nil)) *>
                   showMine(user, renderer)
          )
      }
    } yield ()

  // ── Ввод ───────────────────────────────────────────────────────────────────

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(SellItemPrefix) =>
        a.drop(SellItemPrefix.length).toLongOption
          .fold(showSellList(user, renderer))(askCurrency(user, _, renderer))
          .as(StateType.Auction)
      case _ =>
        readScene(user).flatMap { scene =>
          val typed = ua.text.trim
          if (scene.mode.contains(ModePrice)) confirmPrice(user, typed, renderer).as(StateType.Auction)
          else typed.toLongOption match {
            // Номер лота из объявления в общем чате: пишем цифру — видим лот.
            case Some(id) if id > 0 => showLot(user, id, renderer).as(StateType.Auction)
            case _                  => showMenu(user, renderer).as(StateType.Auction)
          }
        }
    }

  private def withLotId(ua: UserAction)(f: Long => Task[Unit]): Task[Unit] =
    payload(ua, "id").flatMap(_.toLongOption) match {
      case Some(id) => f(id)
      case None     => ZIO.unit
    }

  // ── Вспомогательное ────────────────────────────────────────────────────────

  private def backRow: List[Choice] =
    List(Choice("Auction", content.text("bank.auction.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def cancelRow: List[Choice] =
    List(Choice("Auction", content.text("bank.auction.cancel"), color = ChoiceColor.Negative, row = Some(0)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def readScene(user: User): Task[AuctionScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[AuctionScene].toOption).getOrElse(AuctionScene()))

  private def writeScene(user: User, scene: AuctionScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def resetScene(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null)

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object AuctionState {
  val SellItemPrefix = "AucSell_"
  val ModePrice      = "auctionPrice"

  /** Сколько своих лотов показываем в «Моих лотах» (вместе с историей). */
  val MineLimit: Long = 20L

  case class AuctionScene(
    mode:     Option[String] = None,
    page:     Option[Int]    = None,
    sellPage: Option[Int]    = None,
    itemId:   Option[Long]   = None,
    currency: Option[String] = None,
    price:    Option[Long]   = None
  )
  object AuctionScene {
    implicit val encoder: Encoder[AuctionScene] = deriveEncoder
    implicit val decoder: Decoder[AuctionScene] = deriveDecoder
  }

}
