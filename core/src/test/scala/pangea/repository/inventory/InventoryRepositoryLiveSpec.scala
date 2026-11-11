package pangea.repository.inventory

import pangea.dao.inventory.InventoryDao
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.item.{Item, ItemType, Rarity}
import zio.test._
import zio.{LogLevel, Ref, Task, ZIO}

/** Что сумка пишет в лог, когда вещь в неё не легла: «нет места» — обычный
  * отказ, его ждёт и обрабатывает вызывающий; ошибкой в лог — только сбой. */
object InventoryRepositoryLiveSpec extends ZIOSpecDefault {

  private val heroId = HeroId(1L)

  private def helmet(id: Long): Item =
    Item(id, s"Шлем $id", 1L, Rarity.Gray, ItemType.Helmet,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  /** Сумка в памяти вместо базы. */
  private def dao(start: Inventory): Task[InventoryDao] =
    Ref.make(start).map { ref =>
      new InventoryDao {
        def create(heroId: HeroId): Task[Unit]       = ZIO.unit
        def get(heroId: HeroId): Task[Inventory]     = ref.get
        def update(inventory: Inventory): Task[Unit] = ref.set(inventory)
      }
    }

  private def errors = ZTestLogger.logOutput.map(_.filter(_.logLevel == LogLevel.Error))

  override def spec = suite("Сумка: что идёт в лог")(

    test("«нет места» — отказ вызывающему, но не ошибка в логе") {
      for {
        d   <- dao(Inventory(1L, heroId, 1L, Inventory.Items(List(helmet(1L)))))
        res <- new InventoryRepositoryLive(d).addItem(heroId, helmet(2L)).either
        log <- errors
      } yield assertTrue(res == Left(InventoryRepoError.NoMorePlaceForItems)) &&
              assertTrue(log.isEmpty)
    },

    test("место есть — вещь в сумке, в логе пусто") {
      for {
        d   <- dao(Inventory(1L, heroId, 2L, Inventory.Items(List(helmet(1L)))))
        res <- new InventoryRepositoryLive(d).addItem(heroId, helmet(2L)).either
        bag <- d.get(heroId)
        log <- errors
      } yield assertTrue(res.isRight && bag.items.data.map(_.id) == List(1L, 2L)) &&
              assertTrue(log.isEmpty)
    },

    test("сумка не прочиталась — это сбой, и он в логе ошибкой") {
      val broken = new InventoryDao {
        def create(heroId: HeroId): Task[Unit]       = ZIO.unit
        def get(heroId: HeroId): Task[Inventory]     = ZIO.fail(new Throwable("нет связи с базой"))
        def update(inventory: Inventory): Task[Unit] = ZIO.unit
      }
      for {
        res <- new InventoryRepositoryLive(broken).addItem(heroId, helmet(2L)).either
        log <- errors
      } yield assertTrue(res == Left(InventoryRepoError.CantFindInventory)) &&
              assertTrue(log.exists(_.message().contains("CantFindInventory")))
    }
  )
}
