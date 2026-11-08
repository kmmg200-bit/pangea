package pangea.dao.auction

import doobie.util.transactor
import pangea.model.auction.AuctionLot
import pangea.model.hero.HeroId
import zio.{Task, ZLayer}

/** Лоты аукциона. Вещь переходит между сумкой и лотом одной транзакцией — в
  * лот, покупателю, обратно хозяину: она либо перешла, либо осталась, где
  * была, но не пропадает посередине. */
trait AuctionDao {
  /** Выставить: вещь из сумки продавца — в новый лот. `None` — вещи в сумке
    * уже нет, ничего не выставлено. */
  def listed(lot: AuctionLot): Task[Option[AuctionLot]]

  def get(id: Long): Task[Option[AuctionLot]]
  /** Витрина: лоты в продаже, самые свежие сверху. */
  def listOnSale(now: Long, offset: Long, limit: Long): Task[List[AuctionLot]]
  def countOnSale(now: Long): Task[Long]
  /** Лоты героя: и в продаже, и непроданные. */
  def listBySeller(sellerId: HeroId, limit: Long): Task[List[AuctionLot]]
  def countBySeller(sellerId: HeroId): Task[Long]

  /** Продать лот — только если он всё ещё в продаже: строка удаляется (истории
    * продаж мы не держим), вещь ложится в сумку покупателя. Место в сумке
    * проверяют до того: здесь оно не мешает — лишняя вещь лучше пропавшей.
    * `false` — лот успели купить или снять, и покупателю ничего списывать
    * нельзя. Номера лотов переиспользуются, поэтому лот сверяется целиком:
    * под тем же номером мог встать чужой. */
  def sold(lot: AuctionLot, buyer: HeroId, now: Long): Task[Boolean]

  /** Вернуть вещь хозяину — в сумку, как и при покупке: снятие с торгов или
    * забор непроданного. `None` — лота уже нет. */
  def returned(id: Long, sellerId: HeroId): Task[Option[AuctionLot]]
}

object AuctionDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, AuctionDao] =
    ZLayer.fromFunction(new AuctionDaoLive(_))
}
