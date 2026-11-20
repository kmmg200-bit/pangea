package pangea.service.auction

import pangea.dao.hero.HeroDao
import pangea.engine.Players
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType}
import pangea.model.quest.{DailyKind, DailyNpc}
import pangea.model.user.User
import pangea.repository.auction.AuctionRepository
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.service.purse.Purse
import pangea.service.state.DailyQuestLog
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Выставление лота без экрана. Одно на бота
  * ([[pangea.service.state.states.bank.AuctionState]]) и на Mini App — правила
  * у них одни: вещь из сумки, и только та, что идёт на торги
  * ([[AuctionSale.sellable]]); цена — от [[AuctionLot.MinPrice]] до
  * [[AuctionLot.MaxPrice]]; своих лотов на торгах — не больше
  * [[AuctionLot.MaxLots]]; сбор — десятина в монете лота: серебро — с рук и из
  * ячейки, дублоны — только с рук. Выставленный лот объявляется в торговой
  * беседе и идёт в счётную книгу Рахадима.
  *
  * Меняет героя, поэтому зовётся под локом продавца: в боте его держит ход, в
  * приложении — сам запрос. Всё проверяется до списания сбора: отказ ничего не
  * стоит. Вещь уходит из сумки в лот одной транзакцией с ним самим. */
final class AuctionSale(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  auctionRepo:   AuctionRepository,
  bankRepo:      BankRepository,
  players:       Players
) {
  import AuctionSale._

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, Some(bankRepo))

  def list(user: User, hero: Hero, itemId: Long, currency: AuctionCurrency, price: Long): Task[Result] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      inv    <- inventoryRepo.get(hero.id).mapError(asThrowable)
      mine   <- auctionRepo.mineCount(hero.id).mapError(asThrowable)
      wallet <- purse.wallet(hero)
      fee     = AuctionLot.fee(price)
      out    <- inv.items.data.find(i => i.id == itemId && sellable(i)) match {
        case None                                           => ZIO.succeed(Result.Gone)
        case Some(_) if price < AuctionLot.MinPrice         => ZIO.succeed(Result.PriceTooLow)
        case Some(_) if price > AuctionLot.MaxPrice         => ZIO.succeed(Result.PriceTooHigh)
        case Some(_) if mine >= AuctionLot.MaxLots.toLong   => ZIO.succeed(Result.TooManyLots)
        case Some(_) if !hasFee(currency, fee, hero, wallet.total) => ZIO.succeed(Result.NoFee(fee))
        case Some(item) =>
          chargeFee(user, hero, currency, fee) *>
            auctionRepo.sell(hero.id, item, price, currency, now).foldZIO(
              // Лот не записался — вещь осталась в сумке, а сбор уже взят.
              e => ZIO.logError(s"auction sell failed: $e").as(Result.Failed),
              lot => players.announceTrade(AuctionLot.announcement(lot)).ignore *>
                       // Рахадиму нужно, чтобы торги шли, — лот идёт ему в книгу.
                       DailyQuestLog.add(heroDao, user.userId, DailyNpc.Rakhadim, DailyKind.BankLot, 1L)
                         .as(Result.Listed(lot, fee))
            )
      }
    } yield out

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
}

object AuctionSale {

  /** Чем кончилось выставление. */
  sealed trait Result
  object Result {
    /** Вещи в сумке нет — или на торги её не берут ([[sellable]]). */
    case object Gone extends Result
    case object PriceTooLow extends Result
    case object PriceTooHigh extends Result
    /** На торгах уже [[AuctionLot.MaxLots]] своих лотов, считая непроданные. */
    case object TooManyLots extends Result
    /** Нечем заплатить сбор `fee` — в монете лота. */
    final case class NoFee(fee: Long) extends Result
    /** Лот не записался: вещь осталась в сумке, сбор уже взят. */
    case object Failed extends Result
    final case class Listed(lot: AuctionLot, fee: Long) extends Result
  }

  /** Что можно выставить: не сюжетное (ни предмет, ни экипировка — см.
    * `Item.isStoryGear`), не трофей и занимающее место (пыль и малые руны места
    * не занимают, поэтому на торги не идут). Стопка место занимает — её
    * выставляют целиком, даже если в ней пыль (см. `Bundling`).
    *
    * Трофеи и сюжетная экипировка — личное: трофеи сдают в Гильдии за
    * репутацию, сюжетную экипировку носят сами или продают Ришелье, а не
    * продают друг другу, — ровно по той же причине, по которой их не передают
    * из рук в руки (см. [[pangea.service.parcel.Transfers.sendable]]). */
  def sellable(item: Item): Boolean =
    !item.isQuestItem && !item.isStoryGear && !item.weightless && item.itemType != ItemType.Trophy && !item.isNamedTrophy

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}
