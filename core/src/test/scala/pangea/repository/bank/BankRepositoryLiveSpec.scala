package pangea.repository.bank

import pangea.dao.bank.BankVaultDao
import pangea.model.bank.BankVault
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Item, ItemType, Rarity}
import zio.test._
import zio.{Task, ZIO}

/** Ячейка в Торговом доме: серебро в ней меняется только приращением. Кладут
  * его и чужие — выручка с аукциона, передача из рук в руки, — не дожидаясь
  * хозяина, и ни такое зачисление, ни то, что хозяин в ту же секунду сделал с
  * ячейкой сам, не должны пропасть. */
object BankRepositoryLiveSpec extends ZIOSpecDefault {

  private val heroId = HeroId(1L)

  private val sword =
    Item(7L, "Меч", 3L, Rarity.Blue, ItemType.Weapon, attack = 7, accuracy = 2, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  /** Хранилище в памяти по тем же правилам, что запросы в `BankVaultDaoLive`:
    * `update` пишет ячейки и вещи, серебро — только приращением, с проверкой в
    * той же записи. `meanwhile` — что успело случиться у ячейки между тем, как
    * её прочитали, и тем, как в неё записали. */
  private final class MemoryVault(start: BankVault) extends BankVaultDao {
    var stored: BankVault = start
    private var meanwhile: BankVault => BankVault = identity

    def whileReading(change: BankVault => BankVault): Unit = meanwhile = change

    def getOrCreate(heroId: HeroId): Task[BankVault] =
      ZIO.succeed { val read = stored; stored = meanwhile(stored); meanwhile = identity; read }

    def find(heroId: HeroId): Task[Option[BankVault]] = ZIO.succeed(Some(stored))

    def update(vault: BankVault): Task[Unit] =
      ZIO.succeed { stored = stored.copy(cells = vault.cells, items = vault.items) }

    def addSilver(heroId: HeroId, amount: Long, perCell: Long): Task[Boolean] =
      ZIO.succeed {
        val fits = stored.silver + amount <= stored.cells * perCell
        if (fits) stored = stored.copy(silver = stored.silver + amount)
        fits
      }

    def takeSilver(heroId: HeroId, amount: Long): Task[Boolean] =
      ZIO.succeed {
        val enough = stored.silver >= amount
        if (enough) stored = stored.copy(silver = stored.silver - amount)
        enough
      }
  }

  private def vault(silver: Long, cells: Int = 1, items: List[Item] = Nil) =
    new MemoryVault(BankVault(1L, heroId, cells, Items(items), silver))

  private def plus(n: Long): BankVault => BankVault  = v => v.copy(silver = v.silver + n)
  private def minus(n: Long): BankVault => BankVault = v => v.copy(silver = v.silver - n)

  override def spec = suite("Банковская ячейка: серебро приращением")(

    test("выручку кладут, пока хозяин снимает серебро, — не пропадает ни то ни другое") {
      val dao = vault(silver = 100L)
      dao.whileReading(minus(50L))            // хозяин в ту же секунду снял 50
      for {
        _ <- new BankRepositoryLive(dao).depositSilver(heroId, 500L)
      } yield assertTrue(dao.stored.silver == 550L)
    },

    test("хозяин снимает, пока ему кладут выручку, — выручка не пропадает") {
      val dao = vault(silver = 100L)
      dao.whileReading(plus(500L))            // пока снимал, пришла выручка
      for {
        _ <- new BankRepositoryLive(dao).withdrawSilver(heroId, 50L)
      } yield assertTrue(dao.stored.silver == 550L)
    },

    test("хозяин забирает вещь, пока ему кладут выручку, — выручка не пропадает") {
      val dao = vault(silver = 100L, items = List(sword))
      dao.whileReading(plus(500L))
      for {
        item <- new BankRepositoryLive(dao).withdraw(heroId, sword.id)
      } yield assertTrue(item.id == sword.id && dao.stored.items.data.isEmpty && dao.stored.silver == 600L)
    },

    test("места мало — не кладётся, даже если его заняли между чтением и записью") {
      val nearlyFull = vault(silver = BankVault.SilverPerCell - 10L)
      val raced      = vault(silver = BankVault.SilverPerCell - 30L)
      raced.whileReading(plus(25L))
      for {
        tooMuch <- new BankRepositoryLive(nearlyFull).depositSilver(heroId, 20L).either
        late    <- new BankRepositoryLive(raced).depositSilver(heroId, 20L).either
      } yield assertTrue(tooMuch == Left(BankRepoError.SilverOverflow) && nearlyFull.stored.silver == BankVault.SilverPerCell - 10L) &&
              assertTrue(late == Left(BankRepoError.SilverOverflow) && raced.stored.silver == BankVault.SilverPerCell - 5L)
    },

    test("ячейки нет — серебро не кладётся; больше, чем есть, не снять, даже если убыло между чтением и записью") {
      val none  = vault(silver = 0L, cells = 0)
      val poor  = vault(silver = 30L)
      val raced = vault(silver = 100L)
      raced.whileReading(minus(80L))
      for {
        noVault <- new BankRepositoryLive(none).depositSilver(heroId, 10L).either
        short   <- new BankRepositoryLive(poor).withdrawSilver(heroId, 50L).either
        late    <- new BankRepositoryLive(raced).withdrawSilver(heroId, 50L).either
      } yield assertTrue(noVault == Left(BankRepoError.NoVault) && none.stored.silver == 0L) &&
              assertTrue(short == Left(BankRepoError.NotEnoughSilver) && poor.stored.silver == 30L) &&
              assertTrue(late == Left(BankRepoError.NotEnoughSilver) && raced.stored.silver == 20L)
    }
  )
}
