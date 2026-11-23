package pangea.service.auction

import pangea.dao.hero.HeroDao
import pangea.engine.{Players, SceneContent}
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.Hero
import pangea.model.user.User
import pangea.repository.auction.{AuctionRepoError, AuctionRepository}
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.user.UserRepository
import pangea.service.payout.Payouts
import pangea.service.purse.Purse
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Покупка лота без экрана. Одно на бота
  * ([[pangea.service.state.states.bank.AuctionState]]) и на Mini App — правила
  * у них одни: вещь — в сумку или в хранилище (посыльный), деньги — продавцу
  * в ячейку, а не
  * приняла ячейка — ждут его в городе. Меняет героя, поэтому зовётся под локом
  * покупателя: в боте его держит ход, в приложении — сам запрос.
  *
  * Гонку двух покупателей ловит сам аукцион: лот закрывается одним DELETE, и
  * тому, кто опоздал на пару секунд, приходит [[AuctionPurchase.Result.Gone]] —
  * до списания денег дело не доходит. Закрыть лот и положить вещь в сумку —
  * одна транзакция: вещь не пропадёт между лотом и покупателем. */
final class AuctionPurchase(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  auctionRepo:   AuctionRepository,
  userRepo:      UserRepository,
  bankRepo:      BankRepository,
  payouts:       Payouts,
  players:       Players,
  content:       SceneContent
) {
  import AuctionPurchase._

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, Some(bankRepo))

  /** @param vaultSilverOnly как вне города у личного посыльного: платить серебром
    *                        только из ячейки, не с рук (бот в городе — `false`).
    * @param deliverToVault  покупка вне города: вещь в хранилище Торгового
    *                        дома, не в сумку. */
  def buy(user: User, hero: Hero, lotId: Long, vaultSilverOnly: Boolean = false,
          deliverToVault: Boolean = false): Task[Result] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      inv    <- inventoryRepo.get(hero.id).mapError(asThrowable)
      wallet <- purse.wallet(hero)
      silver = if (vaultSilverOnly) wallet.vault else wallet.total
      res    <- auctionRepo.lot(lotId).either
      out    <- res match {
        case Left(_)                                  => ZIO.succeed(Result.Gone)
        case Right(lot) if !lot.onSale(now)            => ZIO.succeed(Result.Gone)
        case Right(lot) if lot.sellerId == hero.id     => ZIO.succeed(Result.Own)
        case Right(lot) if !canPay(lot, hero, silver)  => ZIO.succeed(Result.NoMoney(lot))
        case Right(lot) =>
          roomFor(lot, hero, deliverToVault, inv.freeSlots).flatMap {
            case false => ZIO.succeed(Result.NoRoom)
            case true  =>
              // Лот закрывается первым и вместе с вещью: опоздавший увидит
              // «лот ушёл», и никаких денег с него не спишется. Упала транзакция —
              // лот на месте, вещь у продавца, денег не трогаем.
              auctionRepo.buy(lot, hero.id, now, deliverToVault).foldZIO(
                {
                  case AuctionRepoError.Failed => ZIO.fail(new Throwable(s"Лот ${lot.id} не продался: сбой базы"))
                  case _                       => ZIO.succeed(Result.Gone)
                },
                _ => payForLot(user, hero, lot, vaultSilverOnly).flatMap(tellSeller(lot, _)).as(Result.Bought(lot))
              )
          }
      }
    } yield out

  /** Есть ли куда положить купленное: в сумку или в хранилище. */
  private def roomFor(lot: AuctionLot, hero: Hero, toVault: Boolean, bagFree: Long): Task[Boolean] =
    if (!toVault) ZIO.succeed(bagFree > 0L)
    else
      bankRepo.get(hero.id).either.map {
        case Right(vault) if vault.open =>
          if (lot.item.weightless) vault.hasRoomForHoard(lot.item) else vault.freeSlots > 0
        case _ => false
      }

  /** Покупатель платит, а выручка продавца идёт в его банковскую ячейку. Не
    * приняла (полна или монета не та) — деньги ждут его в городе. Возвращает,
    * легли ли они в ячейку: об этом говорит колокольчик. */
  private def payForLot(user: User, hero: Hero, lot: AuctionLot, vaultSilverOnly: Boolean): Task[Boolean] =
    lot.currency match {
      case AuctionCurrency.Silver =>
        val paid =
          if (vaultSilverOnly) purse.chargeVaultOnly(hero, lot.price).flatMap {
            case true  => ZIO.unit
            case false => ZIO.fail(new Throwable(s"В ячейке не хватило серебра на лот ${lot.id}"))
          }
          else purse.charge(user.userId, hero, lot.price).someOrFailException.unit
        paid *>
          bankRepo.depositSilver(lot.sellerId, lot.price).either.flatMap {
            case Right(_) => ZIO.succeed(true)
            case Left(_)  => payouts.queue(lot.sellerId, silver = lot.price).as(false)
          }
      case AuctionCurrency.Doubloons =>
        // Ячейка держит только серебро, поэтому дублоны всегда ждут в городе.
        heroDao.updateDoubloons(user.userId, hero.doubloons - lot.price) *>
          payouts.queue(lot.sellerId, doubloons = lot.price).as(false)
    }

  /** Колокольчик продавцу: лот купили, деньги уже пришли. Не дозвонились —
    * молчим: покупка от этого не рушится, продажу видно и в «Моих лотах». */
  private def tellSeller(lot: AuctionLot, intoVault: Boolean): Task[Unit] =
    (for {
      hero   <- heroDao.getHeroById(lot.sellerId).someOrFailException
      seller <- userRepo.getUserById(hero.userId).someOrFailException
      key     = if (intoVault) "bank.auction.soldNoticeVault" else "bank.auction.soldNoticeCity"
      _      <- players.notify(seller, content.format(key,
                  "id" -> lot.id.toString, "name" -> lot.title, "price" -> lot.priceLine))
    } yield ()).ignore
}

object AuctionPurchase {

  /** Чем кончилась покупка. */
  sealed trait Result
  object Result {
    /** Лота нет: купили, сняли с торгов или вышел срок. */
    case object Gone extends Result
    /** Свой же лот. */
    case object Own extends Result
    final case class NoMoney(lot: AuctionLot) extends Result
    /** Некуда положить: сумка или хранилище посыльного. */
    case object NoRoom extends Result
    final case class Bought(lot: AuctionLot) extends Result
  }

  /** Хватит ли на лот: серебро — с руками и ячейкой, дублоны — только на руках. */
  def canPay(lot: AuctionLot, hero: Hero, silverAvailable: Long): Boolean =
    lot.currency match {
      case AuctionCurrency.Silver    => silverAvailable >= lot.price
      case AuctionCurrency.Doubloons => hero.doubloons >= lot.price
    }

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}
