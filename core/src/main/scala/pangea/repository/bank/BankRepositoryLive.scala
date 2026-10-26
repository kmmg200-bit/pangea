package pangea.repository.bank

import pangea.dao.bank.BankVaultDao
import pangea.model.bank.BankVault
import pangea.model.hero.HeroId
import pangea.model.item.Item
import zio.{IO, ZIO}

final class BankRepositoryLive(dao: BankVaultDao) extends BankRepository {

  def get(heroId: HeroId): IO[BankRepoError, BankVault] =
    dao.getOrCreate(heroId).orElseFail(BankRepoError.CantFindVault)

  def buyCell(heroId: HeroId): IO[BankRepoError, BankVault] =
    for {
      vault <- get(heroId)
      grown  = vault.copy(cells = vault.cells + 1)
      _     <- dao.update(grown).orElseFail(BankRepoError.CantUpdateVault)
    } yield grown

  def deposit(heroId: HeroId, item: Item): IO[BankRepoError, Unit] =
    for {
      vault <- get(heroId)
      _     <- ZIO.when(!vault.open)(ZIO.fail(BankRepoError.NoVault))
      // Пыль и малые руны места не занимают, но их не больше сотни на вид.
      _     <- ZIO.when(item.weightless && !vault.hasRoomForHoard(item))(ZIO.fail(BankRepoError.DustLimitReached))
      _     <- ZIO.when(!item.weightless && vault.freeSlots <= 0)(ZIO.fail(BankRepoError.VaultFull))
      _     <- dao.update(vault.addItem(item)).orElseFail(BankRepoError.CantUpdateVault)
    } yield ()

  def withdraw(heroId: HeroId, itemId: Long): IO[BankRepoError, Item] =
    for {
      vault <- get(heroId)
      item  <- ZIO.fromOption(vault.items.data.find(_.id == itemId))
                 .orElseFail(BankRepoError.CantFindItemToRemove)
      rest   = vault.items.data.filterNot(_.id == itemId)
      _     <- dao.update(vault.withItems(rest)).orElseFail(BankRepoError.CantUpdateVault)
    } yield item

  /** Серебро кладут и чужие — выручка с аукциона, передача из рук в руки, — не
    * дожидаясь хозяина. Поэтому приращением, а не записью всей ячейки: иначе
    * зачисление затёрло бы то, что хозяин сделал с ячейкой в ту же секунду, или
    * наоборот. Ячейку читаем, только чтобы назвать причину отказа; место
    * проверяет сама запись. */
  def depositSilver(heroId: HeroId, amount: Long): IO[BankRepoError, Unit] =
    for {
      _     <- ZIO.when(amount <= 0)(ZIO.fail(BankRepoError.NonPositiveAmount))
      vault <- get(heroId)
      _     <- ZIO.when(!vault.open)(ZIO.fail(BankRepoError.NoVault))
      _     <- ZIO.when(amount > vault.freeSilverSpace)(ZIO.fail(BankRepoError.SilverOverflow))
      added <- dao.addSilver(heroId, amount, BankVault.SilverPerCell).orElseFail(BankRepoError.CantUpdateVault)
      _     <- ZIO.when(!added)(ZIO.fail(BankRepoError.SilverOverflow))
    } yield ()

  /** Тоже приращением: пока хозяин снимает, ему могли что-то положить. */
  def withdrawSilver(heroId: HeroId, amount: Long): IO[BankRepoError, Unit] =
    for {
      _     <- ZIO.when(amount <= 0)(ZIO.fail(BankRepoError.NonPositiveAmount))
      vault <- get(heroId)
      _     <- ZIO.when(amount > vault.silver)(ZIO.fail(BankRepoError.NotEnoughSilver))
      taken <- dao.takeSilver(heroId, amount).orElseFail(BankRepoError.CantUpdateVault)
      _     <- ZIO.when(!taken)(ZIO.fail(BankRepoError.NotEnoughSilver))
    } yield ()
}
