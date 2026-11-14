package pangea.service.trade

import pangea.dao.hero.HeroDao
import pangea.dao.trade.TradeDao
import pangea.engine.{Players, SceneContent}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.item.{Item, ItemStack}
import pangea.model.state.StateType
import pangea.model.trade.{TradeOffer, TradeSide, TradeStatus}
import pangea.model.user.{User, UserId}
import pangea.repository.bank.BankRepository
import pangea.repository.user.UserRepository
import pangea.service.Background
import pangea.service.chat.ChatCommand.Money
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.purse.Purse
import pangea.service.state.PlayerLock
import zio.{Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** Обмен между игроками — предложением: один кладёт своё и пишет, чего хочет,
  * другой отвечает своим, и так до согласия; принять может тот, чей ход, —
  * то есть не тот, кто менял предложение последним. Отозвать (кто предложил)
  * или отклонить (кому предложили) можно когда угодно и откуда угодно; всё
  * прочее — только в городе. Одно на бота и на Mini App.
  *
  * Положенное в обмен — заклад: вещи и деньги с комиссией сверху сразу уходят
  * от хозяина и ждут в предложении ([[TradeSide]]). Поэтому каждый трогает
  * только своё и только под своим локом: всё зовётся под локом того, кто
  * действует (`user`), — в боте его держит ход, в приложении сам запрос. Чужое
  * уходит посылками ([[Parcels]]) и через банк: серебро — в ячейку, а не
  * приняла — ждёт хозяина в городе, дублоны ждут его так же ([[Payouts]]);
  * что ложится в чужой дом и ячейку — под локом хозяина, в фоне.
  *
  * Запись — по версии ([[TradeDao.update]]): что кладётся в заклад, уходит от
  * хозяина до записи, а не прошла запись — возвращается; что из заклада
  * выходит — только после записи. Так отзыв партнёра, пришедший в ту же
  * секунду, вернёт ровно то, что лежит в закладе. */
final class Trades(dao: TradeDao, transfers: Transfers, parcels: Parcels, bank: BankRepository, payouts: Payouts,
                   heroDao: HeroDao, userRepo: UserRepository, players: Players, content: SceneContent,
                   lock: PlayerLock, background: Background) {
  import Trades._

  private val purse = Purse(heroDao, Some(bank))

  /** Открытые предложения игрока — и его, и ему, свежие первыми. */
  def open(user: UserId): Task[List[TradeOffer]] = dao.openOf(user)

  def get(id: Long): Task[Option[TradeOffer]] = dao.get(id)

  /** Предложить обмен герою по имени `partnerName`: своё — в заклад, ход —
    * партнёру. */
  def propose(user: User, hero: Hero, partnerName: String, draft: Draft, now: Long): Task[Either[String, TradeOffer]] =
    if (!inCity(hero)) refuse("trade.notInCity")
    else heroDao.heroByName(partnerName.trim).flatMap {
      case None                                           => ZIO.left(content.format("trade.noPartner", "name" -> partnerName.trim))
      case Some(partner) if partner.userId == user.userId => refuse("trade.self")
      case Some(partner) =>
        dao.countOpenFrom(user.userId).flatMap { open =>
          if (open >= MaxOpen) ZIO.left(content.format("trade.tooMany", "max" -> MaxOpen.toString))
          else prepare(hero, TradeSide.empty, draft).flatMap {
            case Left(why)                                         => ZIO.left(why)
            case Right(ch) if ch.side.isEmpty && ch.side.note.isEmpty => refuse("trade.empty")
            case Right(ch) =>
              val offer = TradeOffer(0L, user.userId, partner.userId, ch.side, TradeSide.empty, partner.userId,
                TradeStatus.Open, now, now, now + LifetimeMs, None, 0)
              takeIn(user, hero, ch) *>
                dao.insert(offer).foldZIO(
                  err => undo(user, hero, ch, now) *> ZIO.fail(err),
                  id  => tell(partner.userId, "trade.offered", user.userId).as(Right(offer.copy(id = id))))
          }
        }
    }

  /** Поменять свою сторону, когда ход за тобой: ход переходит партнёру, срок
    * считается заново. */
  def answer(user: User, hero: Hero, id: Long, draft: Draft, now: Long): Task[Either[String, TradeOffer]] =
    withOpen(user, id) { offer =>
      if (offer.turn != user.userId) refuse("trade.notYourTurn")
      else if (!inCity(hero)) refuse("trade.notInCity")
      else prepare(hero, offer.sideOf(user.userId), draft).flatMap {
        case Left(why) => ZIO.left(why)
        case Right(ch) =>
          val next = offer.withSide(user.userId, ch.side)
            .copy(turn = offer.partnerOf(user.userId), updatedAt = now, expiresAt = now + LifetimeMs)
          takeIn(user, hero, ch) *> dao.update(next).flatMap {
            case false => undo(user, hero, ch, now) *> refuse("trade.stale")
            case true =>
              giveOut(user, hero, ch, now) *> tell(next.turn, "trade.answered", user.userId)
                .as(Right(next.copy(version = next.version + 1)))
          }
      }
    }

  /** Принять, когда ход за тобой: заклад каждого — другому. */
  def accept(user: User, hero: Hero, id: Long, now: Long): Task[Either[String, TradeOffer]] =
    withOpen(user, id) { offer =>
      if (offer.turn != user.userId) refuse("trade.notYourTurn")
      else if (!inCity(hero)) refuse("trade.notInCity")
      else if (offer.fromSide.isEmpty && offer.toSide.isEmpty) refuse("trade.nothing")
      else {
        val done = offer.copy(status = TradeStatus.Accepted, updatedAt = now, doneAt = Some(now))
        dao.update(done).flatMap {
          case false => refuse("trade.stale")
          case true =>
            for {
              fromName <- nameOf(offer.from)
              toName   <- nameOf(offer.to)
              actor     = Some(user.userId)
              _        <- hand(offer.fromSide, offer.to, fromName, actor, now, accepted(offer.to, actor, fromName, offer.fromSide))
              _        <- hand(offer.toSide, offer.from, toName, actor, now, accepted(offer.from, actor, toName, offer.toSide))
            } yield Right(done.copy(version = done.version + 1))
        }
      }
    }

  /** Отозвать (кто предложил) или отклонить (кому предложили) — когда угодно
    * и откуда угодно. Заклады — хозяевам целиком, с комиссией. */
  def cancel(user: User, id: Long, now: Long): Task[Either[String, TradeOffer]] =
    withOpen(user, id) { offer =>
      val (status, key) =
        if (user.userId == offer.from) (TradeStatus.Cancelled, "trade.cancelled") else (TradeStatus.Declined, "trade.declined")
      val done  = offer.copy(status = status, updatedAt = now, doneAt = Some(now))
      val other = offer.partnerOf(user.userId)
      dao.update(done).flatMap {
        case false => refuse("trade.stale")
        case true =>
          for {
            name <- nameOf(user.userId)
            _    <- giveBack(offer.sideOf(user.userId), user.userId, Some(user.userId), now, None)
            _    <- giveBack(offer.sideOf(other), other, Some(user.userId), now, Some(content.format(key, "name" -> name)))
          } yield Right(done.copy(version = done.version + 1))
      }
    }

  /** Фоном: срок вышел — предложение закрыто, заклады хозяевам, обоим письмо. */
  def expire(now: Long): Task[Unit] =
    dao.expiring(now).flatMap(ZIO.foreachDiscard(_) { offer =>
      val done = offer.copy(status = TradeStatus.Expired, updatedAt = now, doneAt = Some(now))
      dao.update(done).flatMap(ok => ZIO.when(ok) {
        for {
          fromName <- nameOf(offer.from)
          toName   <- nameOf(offer.to)
          _        <- giveBack(offer.fromSide, offer.from, None, now, Some(content.format("trade.expired", "name" -> toName)))
          _        <- giveBack(offer.toSide, offer.to, None, now, Some(content.format("trade.expired", "name" -> fromName)))
        } yield ()
      }).unit.catchAllCause(cause => ZIO.logErrorCause(s"Обмен ${offer.id}: срок вышел, а закрыть не вышло", cause))
    })

  /** Что лежит на стороне, одной строкой: «⚔️ Меч, Кольцо ×2, 🪙 500»; пусто —
    * «ничего». */
  def describe(side: TradeSide): String = {
    val things = ItemStack.grouped(side.items).map { case (item, n) =>
      if (n > 1) s"${item.displayTitle} ×$n" else item.displayTitle
    }
    val money = List(
      Option.when(side.silver > 0L)(Transfers.moneyLine(Money.Silver, side.silver)),
      Option.when(side.doubloons > 0L)(Transfers.moneyLine(Money.Doubloons, side.doubloons))).flatten
    (things ++ money) match {
      case Nil  => content.text("trade.nothingSide")
      case list => list.mkString(", ")
    }
  }

  private def refuse(key: String): Task[Either[String, TradeOffer]] = ZIO.left(content.text(key))

  /** Предложение, если оно открыто и игрок в нём участвует. */
  private def withOpen(user: User, id: Long)(f: TradeOffer => Task[Either[String, TradeOffer]]): Task[Either[String, TradeOffer]] =
    dao.get(id).flatMap {
      case Some(offer) if offer.isParty(user.userId) && offer.status == TradeStatus.Open => f(offer)
      case _                                                                              => refuse("trade.closed")
    }

  /** Новая сторона по черновику — проверки без единой записи: что добавилось
    * из того, что при герое, что убрано, сколько денег доложить или вернуть. */
  private def prepare(hero: Hero, current: TradeSide, draft: Draft): Task[Either[String, Change]] = {
    val note   = draft.note.map(_.trim).filter(_.nonEmpty)
    val ids    = draft.itemIds.distinct
    val kept   = current.items.filter(i => ids.contains(i.id))
    val newIds = ids.filterNot(id => current.items.exists(_.id == id))
    if (note.exists(_.length > MaxNote)) ZIO.left(content.format("trade.noteTooLong", "max" -> MaxNote.toString))
    else if (draft.silver < 0L || draft.doubloons < 0L) ZIO.left(content.text("trade.badAmount"))
    else tooLittle(Money.Silver, draft.silver).orElse(tooLittle(Money.Doubloons, draft.doubloons)) match {
      case Some(why) => ZIO.left(why)
      case None =>
        transfers.carried(hero).flatMap { carried =>
          val picked = newIds.map(id => carried.find(_.id == id))
          picked.collectFirst { case None => content.text("trade.itemGone") }
            .orElse(picked.flatten.collectFirst {
              case item if !transfers.sendable(item) => content.format("trade.notTradable", "name" -> item.name)
            }) match {
            case Some(why) => ZIO.left(why)
            case None =>
              val side = TradeSide(kept ++ picked.flatten, draft.silver, feeOf(draft.silver), draft.doubloons,
                feeOf(draft.doubloons), note)
              val silverIn    = (side.silver + side.silverFee) - (current.silver + current.silverFee)
              val doubloonsIn = (side.doubloons + side.doubloonsFee) - (current.doubloons + current.doubloonsFee)
              (if (silverIn > 0L) purse.total(hero) else ZIO.succeed(0L)).map { have =>
                if (silverIn > 0L && have < silverIn)
                  Left(notEnough("trade.notEnoughSilver", Money.Silver, silverIn, side.silverFee, have))
                else if (doubloonsIn > 0L && hero.doubloons < doubloonsIn)
                  Left(notEnough("trade.notEnoughDoubloons", Money.Doubloons, doubloonsIn, side.doubloonsFee, hero.doubloons))
                else Right(Change(side, picked.flatten, current.items.filterNot(i => ids.contains(i.id)), silverIn, doubloonsIn))
              }
          }
        }
    }
  }

  /** В заклад — до записи: добавленные вещи и доплата. */
  private def takeIn(user: User, hero: Hero, ch: Change): Task[Unit] =
    ZIO.foreachDiscard(ch.added)(transfers.takeCarried(hero, _)) *>
      ZIO.when(ch.silverIn > 0L)(purse.charge(user.userId, hero, ch.silverIn)
        .someOrFail(new Throwable(s"Обмен: серебра у игрока ${user.userId.value} не хватило"))) *>
      ZIO.when(ch.doubloonsIn > 0L)(addDoubloons(user.userId, -ch.doubloonsIn)).unit

  /** Из заклада — после записи: убранные вещи и лишние деньги — хозяину. */
  private def giveOut(user: User, hero: Hero, ch: Change, now: Long): Task[Unit] =
    returnOwn(user, hero, ch.removed, (-ch.silverIn).max(0L), (-ch.doubloonsIn).max(0L), now)

  /** Запись не прошла — положенное до неё возвращается хозяину. */
  private def undo(user: User, hero: Hero, ch: Change, now: Long): Task[Unit] =
    returnOwn(user, hero, ch.added, ch.silverIn.max(0L), ch.doubloonsIn.max(0L), now)

  /** Своё — себе, под своим локом: вещи в сумку, а не влезли — посылкой;
    * серебро в ячейку (не приняла — ждёт в городе), дублоны сразу в руки. */
  private def returnOwn(user: User, hero: Hero, items: List[Item], silver: Long, doubloons: Long, now: Long): Task[Unit] =
    for {
      rest <- intoBag(hero.id, items)
      sent <- ZIO.foreach(rest)(parcels.send(hero.id, content.text("trade.returnFrom"), _, now))
      _    <- credit(hero.id, silver, 0L)
      _    <- ZIO.when(doubloons > 0L)(addDoubloons(user.userId, doubloons))
      _    <- land(user.userId, hero.id, sent.map(_.id), Some(user.userId), None)
    } yield ()

  /** Заклад — хозяину: своему, кто действует, — как [[returnOwn]]; прочим —
    * посылками и через банк, и письмо `line`. */
  private def giveBack(side: TradeSide, owner: UserId, actor: Option[UserId], now: Long, line: Option[String]): Task[Unit] =
    heroDao.getHeroByUserId(owner).flatMap {
      case None => ZIO.logError(s"Обмен: у игрока ${owner.value} нет героя — заклад вернуть некому")
      case Some(hero) if actor.contains(owner) =>
        userRepo.getUserById(owner).flatMap(ZIO.foreachDiscard(_)(user =>
          returnOwn(user, hero, side.items, side.silver + side.silverFee, side.doubloons + side.doubloonsFee, now)))
      case Some(hero) =>
        for {
          sent <- ZIO.foreach(side.items)(parcels.send(hero.id, content.text("trade.returnFrom"), _, now))
          _    <- credit(hero.id, side.silver + side.silverFee, side.doubloons + side.doubloonsFee)
          _    <- land(owner, hero.id, sent.map(_.id), actor, line)
        } yield ()
    }

  /** Заклад одной стороны — другому: вещи посылками от `giverName`, деньги без
    * комиссии (она сгорела). */
  private def hand(side: TradeSide, to: UserId, giverName: String, actor: Option[UserId], now: Long,
                   line: Option[String]): Task[Unit] =
    heroDao.getHeroByUserId(to).flatMap {
      case None => ZIO.logError(s"Обмен: у игрока ${to.value} нет героя — отдать некому")
      case Some(hero) =>
        for {
          sent <- ZIO.foreach(side.items)(parcels.send(hero.id, giverName, _, now))
          _    <- credit(hero.id, side.silver, side.doubloons)
          _    <- land(to, hero.id, sent.map(_.id), actor, line)
        } yield ()
    }

  /** Письмо о состоявшемся обмене — тому, кто его не принимал: что ему
    * досталось. Принявший видит это на экране. */
  private def accepted(to: UserId, actor: Option[UserId], giverName: String, side: TradeSide): Option[String] =
    Option.unless(actor.contains(to))(content.format("trade.accepted", "name" -> giverName, "got" -> describe(side)))

  /** Серебро — в ячейку, а не приняла — ждёт в городе; дублоны ждут там же:
    * чужого героя мы не пишем. */
  private def credit(heroId: HeroId, silver: Long, doubloons: Long): Task[Unit] =
    ZIO.when(silver > 0L)(bank.depositSilver(heroId, silver).either.flatMap {
      case Right(_) => ZIO.unit
      case Left(_)  => payouts.queue(heroId, silver = silver)
    }) *> payouts.queue(heroId, doubloons = doubloons)

  /** Посылки — по местам (дом, ячейка, почта) и письмо: `line` и где что легло.
    * Хозяину, кто действует, — сразу: его лок у нас. Прочим — под их локом, в
    * фоне; не вышло — посылки подберёт обход почты ([[Transfers.mailRound]]). */
  private def land(owner: UserId, heroId: HeroId, ids: List[Long], actor: Option[UserId], line: Option[String]): Task[Unit] = {
    val arrive =
      ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        .flatMap(now => ZIO.foreach(Option.when(ids.nonEmpty)(ids))(parcels.arrive(heroId, owner, _, now)))
        .map(landed => (line.toList ++ landed.flatMap(parcels.report)).mkString("\n\n"))
        .flatMap(text => ZIO.when(text.nonEmpty)(write(owner, text)).unit)
    if (actor.contains(owner) || ids.isEmpty) arrive
    else background.fork(lock.withLock(owner)(arrive)
      .catchAllCause(cause => ZIO.logErrorCause(s"Обмен: посылки не дошли до игрока ${owner.value}", cause)))
  }

  /** В сумку, пока есть место; что не влезло — вернётся списком. */
  private def intoBag(heroId: HeroId, items: List[Item]): Task[List[Item]] =
    ZIO.foreach(items)(item => transfers.inventoryRepo.addItem(heroId, item).fold(_ => Some(item), _ => None))
      .map(_.flatten)

  /** Дублоны в руки — только своему герою, под его локом. */
  private def addDoubloons(userId: UserId, delta: Long): Task[Unit] =
    heroDao.getHeroByUserId(userId).someOrFail(new Throwable(s"Нет героя у игрока ${userId.value}"))
      .flatMap(h => heroDao.updateDoubloons(userId, h.doubloons + delta))

  private def tell(to: UserId, key: String, about: UserId): Task[Unit] =
    nameOf(about).flatMap(name => write(to, content.format(key, "name" -> name)))

  private def write(userId: UserId, text: String): Task[Unit] =
    userRepo.getUserById(userId).flatMap(ZIO.foreachDiscard(_)(players.notify(_, text).ignore))

  /** Как назвать игрока в обмене: имя героя, а нет его — имя из ВК. */
  def nameOf(userId: UserId): Task[String] =
    userRepo.getUserById(userId).flatMap {
      case Some(user) => players.getDisplayName(user).orElse(ZIO.succeed(content.text("transfer.someone")))
      case None       => ZIO.succeed(content.text("transfer.someone"))
    }

  private def tooLittle(money: Money, amount: Long): Option[String] =
    Option.when(amount > 0L && amount < Transfers.MinMoney)(content.format("trade.tooLittle",
      "min" -> Transfers.moneyLine(money, Transfers.MinMoney)))

  private def notEnough(key: String, money: Money, need: Long, fee: Long, have: Long): String =
    content.format(key,
      "need" -> Transfers.moneyLine(money, need),
      "fee"  -> Transfers.moneyLine(money, fee),
      "have" -> Transfers.moneyLine(money, have))
}

object Trades {

  /** Что игрок кладёт на свою сторону: номера вещей из того, что при нём (и
    * что уже лежит в его закладе), серебро, дублоны и записка. */
  final case class Draft(itemIds: List[Long], silver: Long, doubloons: Long, note: Option[String])

  /** Новая сторона и чем она отличается от прежней: что добавилось и что
    * убрано; `silverIn` и `doubloonsIn` — сколько доложить (меньше нуля —
    * вернуть), вместе с комиссией. */
  private final case class Change(side: TradeSide, added: List[Item], removed: List[Item], silverIn: Long,
                                  doubloonsIn: Long)

  /** Сколько открытых предложений игрок может сделать сам. */
  val MaxOpen: Int = 5

  /** Длина записки. */
  val MaxNote: Int = 200

  /** Сколько предложение ждёт ответа после последней правки. */
  val LifetimeMs: Long = 3L * 24 * 60 * 60 * 1000

  def inCity(hero: Hero): Boolean = StateType.cityStates.contains(hero.state)

  /** Комиссия — как у передачи денег (см. [[Transfers.fee]]); с нуля — ноль. */
  def feeOf(amount: Long): Long = if (amount > 0L) Transfers.fee(amount) else 0L

  val live: ZLayer[TradeDao with Transfers with Parcels with BankRepository with Payouts with HeroDao
                     with UserRepository with Players with SceneContent with PlayerLock with Background,
                   Nothing, Trades] =
    ZLayer.fromFunction(new Trades(_, _, _, _, _, _, _, _, _, _, _))
}
