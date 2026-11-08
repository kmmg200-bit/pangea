package pangea.test

import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.repository.auction.{AuctionRepoError, AuctionRepository}
import zio.{IO, ZIO}

/** Аукцион в памяти: те же правила, что в проде, включая «кто первый, того и
  * лот» — покупка забирает лот, и второму достаётся отказ. Проданные и снятые
  * лоты не хранятся. Вещь ходит между лотом и сумкой `bag` разом, как в
  * транзакции прода: выставленная уходит из сумки, купленная и снятая — в
  * неё, с новым номером. */
class TestAuctionRepository(private var lots: List[AuctionLot] = Nil,
                            bag: TestInventoryRepository = TestInventoryRepository.accepting)
    extends AuctionRepository {

  private var nextItemId = 10000L

  /** В сумку с новым номером — как `ItemRepository.persist`. */
  private def toBag(item: Item): Unit = {
    nextItemId += 1
    bag.put(item.copy(id = nextItemId))
  }

  /** Наименьший свободный номер — как в проде: проданный лот освобождает свой. */
  private def freeId: Long = Iterator.from(1).map(_.toLong).find(n => !lots.exists(_.id == n)).get

  /** Свежие сверху. По номеру сортировать нельзя: они переиспользуются. */
  private def newestFirst(xs: List[AuctionLot]): List[AuctionLot] =
    xs.sortBy(l => (-l.listedAt, -l.id))

  def page(now: Long, page: Int, pageSize: Int): IO[AuctionRepoError, (List[AuctionLot], Int, Int)] = {
    val onSaleLots = newestFirst(lots.filter(_.onSale(now)))
    val pages      = ((onSaleLots.size + pageSize - 1) / pageSize).max(1)
    val p          = page.max(0).min(pages - 1)
    ZIO.succeed((onSaleLots.slice(p * pageSize, p * pageSize + pageSize), pages, p))
  }

  def onSale(now: Long): IO[AuctionRepoError, Long] = ZIO.succeed(lots.count(_.onSale(now)).toLong)

  def lot(id: Long): IO[AuctionRepoError, AuctionLot] =
    ZIO.fromOption(lots.find(_.id == id)).orElseFail(AuctionRepoError.LotNotFound)

  def mine(sellerId: HeroId, limit: Long): IO[AuctionRepoError, List[AuctionLot]] =
    ZIO.succeed(newestFirst(lots.filter(_.sellerId == sellerId)).take(limit.toInt))

  def mineCount(sellerId: HeroId): IO[AuctionRepoError, Long] =
    ZIO.succeed(lots.count(_.sellerId == sellerId).toLong)

  def sell(sellerId: HeroId, item: Item, price: Long, currency: AuctionCurrency, now: Long): IO[AuctionRepoError, AuctionLot] =
    if (item.weightless)                  ZIO.fail(AuctionRepoError.Weightless)
    else if (price < AuctionLot.MinPrice) ZIO.fail(AuctionRepoError.PriceTooLow)
    else if (price > AuctionLot.MaxPrice) ZIO.fail(AuctionRepoError.PriceTooHigh)
    else if (lots.count(_.sellerId == sellerId) >= AuctionLot.MaxLots) ZIO.fail(AuctionRepoError.TooManyLots)
    else bag.snapshot.find(_.id == item.id) match {
      case None => ZIO.fail(AuctionRepoError.NotInBag)
      case Some(inBag) =>
        bag.removeItem(inBag.id, sellerId).orElseFail(AuctionRepoError.Failed).as {
          val lot = AuctionLot.fresh(sellerId, inBag, price, currency, now).copy(id = freeId)
          lots = lots :+ lot
          lot
        }
    }

  // Лот сверяется целиком: номер мог занять чужой лот.
  def buy(lot: AuctionLot, buyer: HeroId, now: Long): IO[AuctionRepoError, AuctionLot] =
    lots.find(l => l == lot && l.onSale(now)) match {
      case Some(found) =>
        lots = lots.filterNot(_.id == found.id)
        toBag(found.item)
        ZIO.succeed(found)
      case None => ZIO.fail(AuctionRepoError.LotGone)
    }

  def reclaim(lotId: Long, sellerId: HeroId): IO[AuctionRepoError, AuctionLot] =
    lots.find(l => l.id == lotId && l.sellerId == sellerId) match {
      case Some(lot) =>
        lots = lots.filterNot(_.id == lotId)
        toBag(lot.item)
        ZIO.succeed(lot)
      case None => ZIO.fail(AuctionRepoError.LotGone)
    }

  def snapshot: List[AuctionLot] = lots
}

object TestAuctionRepository {
  def empty: TestAuctionRepository = new TestAuctionRepository()
  def of(lots: AuctionLot*): TestAuctionRepository = new TestAuctionRepository(lots.toList)
}
