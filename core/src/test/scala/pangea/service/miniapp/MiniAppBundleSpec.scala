package pangea.service.miniapp

import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.artifact.HeroArtifacts
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.hero.Hero
import pangea.model.home.HomeData
import pangea.model.inventory.Inventory
import pangea.model.item.{BrewKind, Bundling, Item, ItemType, MaterialKind, Rarity}
import pangea.model.user.UserId
import pangea.service.equipment.BundleWork
import pangea.test.{TestFixtures, TestInventoryRepository, TestItemRepository}
import zio.test._

/** Стопка в приложении: кнопки «Распаковать» и «В стопку» приходят данными, а сами
  * распаковка и сборка — те же, что на карточке в боте ([[BundleWork]]): вещи
  * ложатся в сумку, мест не хватает — сказано, сколько освободить. */
object MiniAppBundleSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val base: Hero = TestFixtures.hero(userId).copy(equipment = TestFixtures.emptyEquipment)

  private lazy val content: SceneContent = SceneContent.load()

  private def mithril(id: Long): Item = MaterialGenerator.item(MaterialKind.Mithril).copy(id = id)
  private val stack: Item = Bundling.pack((101L to 105L).toList.map(mithril)).get.copy(id = 50L)

  private def sword(id: Long): Item =
    Item(id, "Меч", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  private def view(items: List[Item]): MiniAppView.InventoryView =
    MiniAppView.inventory(base, Inventory(1L, base.id, 20L, Inventory.Items(items)),
      MiniAppView.Stores(HeroArtifacts.empty(base.id), Barrel.empty(base.id), BankVault.empty(base.id), HomeData.empty),
      content, _.name, 0L)

  override def spec = suite("MiniApp: стопки")(

    test("у стопки в сумке — «Распаковать»; у меча кнопок нет, у отвара распаковки нет") {
      val bottle = BrewKind.item(BrewKind.LivingWater).copy(id = 60L)
      val acts   = view(List(stack, sword(1L), bottle)).bagActs
      assertTrue(acts.find(_.item == stack.id).exists(_.unpack)) &&
      assertTrue(!acts.exists(_.item == 1L)) &&
      assertTrue(acts.find(_.item == bottle.id).exists(a => !a.unpack))
    },

    test("в бою кнопок из сумки нет — и распаковки тоже") {
      val inBattle = base.copy(state = pangea.model.state.StateType.Battle)
      val v = MiniAppView.inventory(inBattle, Inventory(1L, base.id, 20L, Inventory.Items(List(stack))),
        MiniAppView.Stores(HeroArtifacts.empty(base.id), Barrel.empty(base.id), BankVault.empty(base.id), HomeData.empty),
        content, _.name, 0L)
      assertTrue(v.bagActs.isEmpty)
    },

    test("распаковать: место есть — пять вещей в сумке, стопки нет, и об этом сказано") {
      val bag  = TestInventoryRepository.withItems(List(stack))
      val work = new BundleWork(bag, TestItemRepository.make, content)
      for {
        inv  <- bag.get(base.id).mapError(e => new Throwable(e.toString))
        note <- work.unpack(base.id, inv, stack)
      } yield assertTrue(note.contains(content.format("inventory.bundle.unpacked",
                "name" -> mithril(101L).displayTitle, "n" -> "5"))) &&
              assertTrue(bag.snapshot.map(_.id).sorted == (101L to 105L).toList && !bag.snapshot.exists(_.isBundle))
    },

    test("распаковать: мест не хватает — сказано, сколько освободить, сумка как была") {
      // 1 стопка + 18 мечей = 19 из 20; распакованная займёт пять мест вместо одного.
      val swords = (1L to 18L).toList.map(sword)
      val bag    = TestInventoryRepository.withItems(stack :: swords)
      val work   = new BundleWork(bag, TestItemRepository.make, content)
      for {
        inv  <- bag.get(base.id).mapError(e => new Throwable(e.toString))
        note <- work.unpack(base.id, inv, stack)
      } yield assertTrue(note.contains(content.format("inventory.bundle.unpackNoRoom", "count" -> "3"))) &&
              assertTrue(bag.snapshot.exists(_.id == stack.id) && bag.snapshot.size == 19)
    },

    test("не стопка — говорить не о чем") {
      val bag  = TestInventoryRepository.withItems(List(sword(1L)))
      val work = new BundleWork(bag, TestItemRepository.make, content)
      for {
        inv  <- bag.get(base.id).mapError(e => new Throwable(e.toString))
        note <- work.unpack(base.id, inv, sword(1L))
      } yield assertTrue(note.isEmpty && bag.snapshot.map(_.id) == List(1L))
    },

    test("«В стопку» — когда таких в сумке на стопку хватает: пять мифрила — да, четыре — нет; мечам и стопке — нет") {
      val five  = (111L to 115L).toList.map(mithril)
      val acts  = view(five ++ (201L to 205L).toList.map(sword) :+ stack).bagActs
      val four  = view((111L to 114L).toList.map(mithril)).bagActs
      val metal = acts.find(a => five.exists(_.id == a.item))
      assertTrue(metal.exists(a => a.pack && !a.unpack)) &&
      assertTrue(!acts.exists(a => a.item >= 201L)) &&
      assertTrue(acts.find(_.item == stack.id).exists(a => a.unpack && !a.pack)) &&
      assertTrue(!four.exists(_.pack))
    },

    test("собрать: пять последних — в стопку с новым номером, остальное на месте, и об этом сказано") {
      val seven = (111L to 117L).toList.map(mithril)
      val bag   = TestInventoryRepository.withItems(sword(201L) :: seven)
      val work  = new BundleWork(bag, TestItemRepository.make, content)
      for {
        inv   <- bag.get(base.id).mapError(e => new Throwable(e.toString))
        note  <- work.pack(base.id, inv, seven.head)
        packed = bag.snapshot.filter(_.isBundle)
      } yield assertTrue(note.contains(content.format("inventory.bundle.packed",
                "name" -> mithril(111L).displayTitle, "n" -> "5"))) &&
              assertTrue(packed.map(b => Bundling.contents(b).map(_.id)) == List((113L to 117L).toList)) &&
              assertTrue(packed.forall(_.id > 0L)) &&
              assertTrue(bag.snapshot.filterNot(_.isBundle).map(_.id) == List(201L, 111L, 112L))
    },

    test("собрать: таких уже меньше пяти — сказано, что нужно пять, сумка как была") {
      val four = (111L to 114L).toList.map(mithril)
      val bag  = TestInventoryRepository.withItems(four)
      val work = new BundleWork(bag, TestItemRepository.make, content)
      for {
        inv  <- bag.get(base.id).mapError(e => new Throwable(e.toString))
        note <- work.pack(base.id, inv, four.head)
      } yield assertTrue(note.contains(content.format("inventory.bundle.short", "n" -> "5"))) &&
              assertTrue(bag.snapshot.map(_.id) == (111L to 114L).toList)
    },

    test("собрать пыль в полной сумке — стопке нужно место, сумка как была; меч в стопку не собирается") {
      val dust   = (111L to 115L).toList.map(i => MaterialGenerator.item(MaterialKind.RubyDust).copy(id = i))
      val swords = (201L to 220L).toList.map(sword)
      val bag    = TestInventoryRepository.withItems(swords ++ dust)
      val work   = new BundleWork(bag, TestItemRepository.make, content)
      for {
        inv   <- bag.get(base.id).mapError(e => new Throwable(e.toString))
        full  <- work.pack(base.id, inv, dust.head)
        blade <- work.pack(base.id, inv, swords.head)
      } yield assertTrue(full.contains(content.text("inventory.bundle.noRoom")) && blade.isEmpty) &&
              assertTrue(bag.snapshot.size == 25 && !bag.snapshot.exists(_.isBundle))
    }
  )
}
