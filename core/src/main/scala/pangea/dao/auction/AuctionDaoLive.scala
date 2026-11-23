package pangea.dao.auction

import doobie.ConnectionIO
import doobie.free.connection.{pure, raiseError, unit}
import doobie.implicits._
import doobie.util.transactor.Transactor
import pangea.dao.bank.BankVaultDaoLive
import pangea.dao.inventory.InventoryDaoLive
import pangea.dao.item.ItemDaoLive
import pangea.model.auction.AuctionLot
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.model.item.Item.meta
import zio.Task
import zio.interop.catz._

class AuctionDaoLive(xa: Transactor[Task]) extends AuctionDao {

  private val columns = fr"id, seller_id, item, price, currency, listed_at, expires_at"

  // Номер лота — наименьший свободный: проданный или снятый лот освобождает
  // свой, и следующий продавец занимает дырку, а не берёт новое число. Искать
  // есть смысл только до «сколько лотов + 1»: среди стольких чисел свободное
  // найдётся всегда.
  private val freeId =
    sql"""select min(n)
          from generate_series(1, (select count(*) from auction_lots) + 1) as g(n)
          where not exists (select 1 from auction_lots a where a.id = g.n)"""
      .query[Long].unique

  // Замок на время транзакции: без него двое продавцов, зашедшие разом,
  // высчитают один и тот же свободный номер и один упрётся в первичный ключ.
  private val lockIds =
    sql"select pg_advisory_xact_lock(${AuctionDaoLive.LotIdLock})".query[Unit].unique

  // Вещь в сумку внутри той же транзакции: новый номер, как при любой выдаче
  // (см. ItemRepository.persist), и дописать её в сумку под замком строки.
  private def toBag(heroId: HeroId, item: Item): ConnectionIO[Unit] =
    for {
      id  <- ItemDaoLive.insertIo(heroId.value, item)
      bag <- InventoryDaoLive.lockedIo(heroId)
      _   <- InventoryDaoLive.updateIo(bag.addItem(item.copy(id = id)))
    } yield ()

  /** В хранилище Торгового дома — там же, куда ложат переданное. */
  private def putInVault(heroId: HeroId, item: Item): ConnectionIO[Unit] =
    for {
      _     <- BankVaultDaoLive.ensureIo(heroId)
      vault <- BankVaultDaoLive.lockedIo(heroId)
      _     <- if (!vault.open) raiseError(AuctionDaoLive.VaultClosed: Throwable)
               else if (item.weightless && !vault.hasRoomForHoard(item))
                 raiseError(AuctionDaoLive.VaultFull: Throwable)
               else if (!item.weightless && vault.freeSlots <= 0)
                 raiseError(AuctionDaoLive.VaultFull: Throwable)
               else unit
      id    <- ItemDaoLive.insertIo(heroId.value, item)
      _     <- BankVaultDaoLive.updateIo(vault.addItem(item.copy(id = id)))
    } yield ()

  // В лот идёт вещь, как она лежит в сумке, а не копия, с которой пришли.
  override def listed(lot: AuctionLot): Task[Option[AuctionLot]] =
    (for {
      bag <- InventoryDaoLive.lockedIo(lot.sellerId)
      res <- bag.items.data.find(_.id == lot.item.id) match {
               case None => pure(Option.empty[AuctionLot])
               case Some(item) =>
                 for {
                   _  <- InventoryDaoLive.updateIo(bag.withItems(bag.items.withoutOne(item.id)))
                   _  <- lockIds
                   id <- freeId
                   _  <- sql"""insert into auction_lots(id, seller_id, item, price, currency, listed_at, expires_at)
                               values($id, ${lot.sellerId.value}, $item, ${lot.price}, ${lot.currency},
                                      ${lot.listedAt}, ${lot.expiresAt})""".update.run
                 } yield Option(lot.copy(id = id, item = item))
             }
    } yield res).transact(xa)

  override def get(id: Long): Task[Option[AuctionLot]] =
    (fr"select" ++ columns ++ fr"from auction_lots where id = $id")
      .query[AuctionLot].option.transact(xa)

  override def listOnSale(now: Long, offset: Long, limit: Long): Task[List[AuctionLot]] =
    (fr"select" ++ columns ++
      fr"""from auction_lots
           where expires_at > $now
           order by listed_at desc, id desc
           limit $limit offset $offset""")
      .query[AuctionLot].to[List].transact(xa)

  override def countOnSale(now: Long): Task[Long] =
    sql"select count(*) from auction_lots where expires_at > $now"
      .query[Long].unique.transact(xa)

  override def listBySeller(sellerId: HeroId, limit: Long): Task[List[AuctionLot]] =
    (fr"select" ++ columns ++
      fr"""from auction_lots
           where seller_id = ${sellerId.value}
           order by listed_at desc, id desc
           limit $limit""")
      .query[AuctionLot].to[List].transact(xa)

  override def countBySeller(sellerId: HeroId): Task[Long] =
    sql"select count(*) from auction_lots where seller_id = ${sellerId.value}"
      .query[Long].unique.transact(xa)

  // Гонка двух покупателей решается здесь: строка либо удалилась (значит, лот
  // был наш), либо её уже не было — и второй получит `false`.
  override def sold(lot: AuctionLot, buyer: HeroId, now: Long, toVault: Boolean): Task[Boolean] =
    (for {
      gone <- sql"""delete from auction_lots
                    where id = ${lot.id} and seller_id = ${lot.sellerId.value} and listed_at = ${lot.listedAt}
                      and expires_at > $now""".update.run
      _    <- if (gone == 1)
                if (toVault) putInVault(buyer, lot.item) else toBag(buyer, lot.item)
              else unit
    } yield gone == 1).transact(xa)

  override def returned(id: Long, sellerId: HeroId): Task[Option[AuctionLot]] =
    (for {
      lot <- (fr"delete from auction_lots where id = $id and seller_id = ${sellerId.value} returning" ++ columns)
               .query[AuctionLot].option
      _   <- lot.fold(unit)(l => toBag(sellerId, l.item))
    } yield lot).transact(xa)
}

object AuctionDaoLive {
  /** Ключ advisory-замка, под которым выдаются номера лотов. Число произвольное,
    * важно лишь, чтобы его не занял кто-то ещё. */
  val LotIdLock: Long = 770001L

  private case object VaultClosed extends Throwable
  private case object VaultFull   extends Throwable
}
