package pangea.repository.auction

import pangea.dao.auction.AuctionDao
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.service.ErrorLog
import zio.{IO, Task, ZIO}

final class AuctionRepositoryLive(dao: AuctionDao) extends AuctionRepository {

  def page(now: Long, page: Int, pageSize: Int): IO[AuctionRepoError, (List[AuctionLot], Int, Int)] =
    for {
      total <- dao.countOnSale(now).orElseFail(AuctionRepoError.Failed)
      pages  = ((total + pageSize - 1L) / pageSize.toLong).max(1L).toInt
      p      = page.max(0).min(pages - 1)
      lots  <- dao.listOnSale(now, p.toLong * pageSize.toLong, pageSize.toLong)
                 .orElseFail(AuctionRepoError.Failed)
    } yield (lots, pages, p)

  def onSale(now: Long): IO[AuctionRepoError, Long] =
    dao.countOnSale(now).orElseFail(AuctionRepoError.Failed)

  def lot(id: Long): IO[AuctionRepoError, AuctionLot] =
    dao.get(id).orElseFail(AuctionRepoError.Failed)
      .flatMap(ZIO.fromOption(_).orElseFail(AuctionRepoError.LotNotFound))

  def mine(sellerId: HeroId, limit: Long): IO[AuctionRepoError, List[AuctionLot]] =
    dao.listBySeller(sellerId, limit).orElseFail(AuctionRepoError.Failed)

  def mineCount(sellerId: HeroId): IO[AuctionRepoError, Long] =
    dao.countBySeller(sellerId).orElseFail(AuctionRepoError.Failed)

  def sell(sellerId: HeroId, item: Item, price: Long, currency: AuctionCurrency, now: Long): IO[AuctionRepoError, AuctionLot] =
    for {
      _   <- ZIO.when(item.weightless)(ZIO.fail(AuctionRepoError.Weightless))
      _   <- ZIO.when(price < AuctionLot.MinPrice)(ZIO.fail(AuctionRepoError.PriceTooLow))
      _   <- ZIO.when(price > AuctionLot.MaxPrice)(ZIO.fail(AuctionRepoError.PriceTooHigh))
      // Своих лотов на торгах — не больше десяти, считая непроданные.
      mine <- mineCount(sellerId)
      _   <- ZIO.when(mine >= AuctionLot.MaxLots.toLong)(ZIO.fail(AuctionRepoError.TooManyLots))
      lot <- moved(s"Лот героя ${sellerId.value} не выставился", dao.listed(AuctionLot.fresh(sellerId, item, price, currency, now)))
               .someOrFail(AuctionRepoError.NotInBag)
    } yield lot

  def buy(lot: AuctionLot, buyer: HeroId, now: Long): IO[AuctionRepoError, AuctionLot] =
    moved(s"Лот ${lot.id} не продался герою ${buyer.value}", dao.sold(lot, buyer, now))
      .filterOrFail(identity)(AuctionRepoError.LotGone).as(lot)

  def reclaim(lotId: Long, sellerId: HeroId): IO[AuctionRepoError, AuctionLot] =
    moved(s"Лот $lotId не вернулся герою ${sellerId.value}", dao.returned(lotId, sellerId))
      .someOrFail(AuctionRepoError.LotGone)

  /** Транзакция с вещью упала — откатилась целиком, вещь там же, где была.
    * Причину пишем в лог: снаружи останется только `Failed`. */
  private def moved[A](what: => String, tx: Task[A]): IO[AuctionRepoError, A] =
    tx.tapErrorCause(ErrorLog.logged(what)).orElseFail(AuctionRepoError.Failed)
}
