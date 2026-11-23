package pangea.dao.bank

import doobie.ConnectionIO
import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import doobie.util.transactor.Transactor
import io.circe.syntax.EncoderOps
import pangea.model.bank.BankVault
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.{meta, Items}
import pangea.model.item.Item
import zio.Task
import zio.interop.catz._

class BankVaultDaoLive(xa: Transactor[Task]) extends BankVaultDao {

  override def getOrCreate(heroId: HeroId): Task[BankVault] =
    (for {
      _ <- sql"insert into bank_vaults(hero_id, cells, items, silver) values($heroId, 0, ${Items(List.empty[Item])}, 0) on conflict (hero_id) do nothing".update.run
      v <- select(heroId).unique
    } yield v).transact(xa)

  override def find(heroId: HeroId): Task[Option[BankVault]] =
    select(heroId).option.transact(xa)

  private def select(heroId: HeroId) =
    sql"select id, hero_id, cells, items, silver from bank_vaults where hero_id = $heroId".query[BankVault]

  override def update(vault: BankVault): Task[Unit] =
    sql"update bank_vaults set cells = ${vault.cells}, items = ${vault.items.asJson} where id = ${vault.id}".update.run
      .transact(xa)
      .unit

  // Место проверяем в том же запросе: между чтением ячейки и этой записью
  // туда могли положить ещё.
  override def addSilver(heroId: HeroId, amount: Long, perCell: Long): Task[Boolean] =
    sql"""update bank_vaults set silver = silver + $amount
          where hero_id = $heroId and silver + $amount <= cells * $perCell"""
      .update.run.transact(xa).map(_ == 1)

  override def takeSilver(heroId: HeroId, amount: Long): Task[Boolean] =
    sql"update bank_vaults set silver = silver - $amount where hero_id = $heroId and silver >= $amount"
      .update.run.transact(xa).map(_ == 1)
}

/** Ячейка внутри чужой транзакции — покупка на аукционе через посыльного. */
object BankVaultDaoLive {

  def ensureIo(heroId: HeroId): ConnectionIO[Unit] =
    sql"insert into bank_vaults(hero_id, cells, items, silver) values($heroId, 0, ${Items(List.empty[Item])}, 0) on conflict (hero_id) do nothing"
      .update.run.map(_ => ())

  def lockedIo(heroId: HeroId): ConnectionIO[BankVault] =
    sql"select id, hero_id, cells, items, silver from bank_vaults where hero_id = $heroId for update"
      .query[BankVault]
      .unique

  def updateIo(vault: BankVault): ConnectionIO[Int] =
    sql"update bank_vaults set cells = ${vault.cells}, items = ${vault.items.asJson} where id = ${vault.id}".update.run
}
