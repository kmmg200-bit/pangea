package pangea.dao.bank

import doobie.util.transactor
import pangea.model.bank.BankVault
import pangea.model.hero.HeroId
import zio.{Task, ZLayer}

trait BankVaultDao {
  /** Гарантирует наличие записи хранилища для героя и возвращает её. */
  def getOrCreate(heroId: HeroId): Task[BankVault]
  /** Ячейки и вещи — их пишет только хозяин, под своим локом. Серебро не
   *  трогает: его кладут и чужие (выручка с аукциона, передача), не дожидаясь
   *  хозяина, и запись ячейки целиком затёрла бы такое зачисление. Серебро
   *  меняется только приращением — [[addSilver]] и [[takeSilver]]. */
  def update(vault: BankVault): Task[Unit]
  /** Прибавить серебро, если оно влезает в купленные ячейки (`perCell` на
   *  каждую): `false` — не влезло или ячеек нет. */
  def addSilver(heroId: HeroId, amount: Long, perCell: Long): Task[Boolean]
  /** Убавить серебро, если его хватает: `false` — не хватает. */
  def takeSilver(heroId: HeroId, amount: Long): Task[Boolean]
}

object BankVaultDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, BankVaultDao] =
    ZLayer.fromFunction(new BankVaultDaoLive(_))
}
