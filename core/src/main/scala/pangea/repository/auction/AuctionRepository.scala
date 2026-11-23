package pangea.repository.auction

import pangea.dao.auction.AuctionDao
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.HeroId
import pangea.model.item.Item
import zio.{IO, ZLayer}

sealed trait AuctionRepoError
object AuctionRepoError {
  /** Дешевле [[AuctionLot.MinPrice]] не выставляют. */
  case object PriceTooLow  extends AuctionRepoError
  case object PriceTooHigh extends AuctionRepoError
  /** Пыль и малые руны места не занимают — их на торги не берут. */
  case object Weightless   extends AuctionRepoError
  case object LotNotFound  extends AuctionRepoError
  /** Лот только что купили, сняли или у него вышел срок. */
  case object LotGone      extends AuctionRepoError
  /** У героя уже [[pangea.model.auction.AuctionLot.MaxLots]] лотов на торгах. */
  case object TooManyLots  extends AuctionRepoError
  /** Выставляемой вещи в сумке уже нет. */
  case object NotInBag     extends AuctionRepoError
  case object Failed       extends AuctionRepoError
}

/** Аукцион Торгового дома: витрина, выставление, покупка и возврат вещей.
 *  Вещь переходит между сумкой и лотом одной транзакцией: перешла целиком или
 *  осталась, где была. Деньги репозиторий не трогает — их считает состояние
 *  через `Purse`. */
trait AuctionRepository {
  /** Страница витрины: лоты, сколько всего страниц и какая показана. */
  def page(now: Long, page: Int, pageSize: Int): IO[AuctionRepoError, (List[AuctionLot], Int, Int)]
  /** Сколько лотов сейчас в продаже. */
  def onSale(now: Long): IO[AuctionRepoError, Long]
  def lot(id: Long): IO[AuctionRepoError, AuctionLot]
  def mine(sellerId: HeroId, limit: Long): IO[AuctionRepoError, List[AuctionLot]]
  /** Сколько лотов героя сейчас на торгах (считая непроданные). */
  def mineCount(sellerId: HeroId): IO[AuctionRepoError, Long]

  /** Выставить вещь: из сумки продавца — в лот. Плату за выставление берёт
    * вызывающий. */
  def sell(sellerId: HeroId, item: Item, price: Long, currency: AuctionCurrency, now: Long): IO[AuctionRepoError, AuctionLot]

  /** Купить: лот закрывается, вещь — в сумку или в хранилище (`toVault`).
    * Лот удаляется атомарно, поэтому второму покупателю придёт
    * [[AuctionRepoError.LotGone]] и списывать с него нечего. */
  def buy(lot: AuctionLot, buyer: HeroId, now: Long, toVault: Boolean = false): IO[AuctionRepoError, AuctionLot]

  /** Снять свой лот с торгов или забрать непроданное: вещь — в сумку хозяина. */
  def reclaim(lotId: Long, sellerId: HeroId): IO[AuctionRepoError, AuctionLot]
}

object AuctionRepository {
  val live: ZLayer[AuctionDao, Nothing, AuctionRepository] =
    ZLayer.fromFunction(new AuctionRepositoryLive(_))
}
