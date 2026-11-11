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
import pangea.test.{TestFixtures, TestInventoryRepository}
import zio.test._

/** Стопка в приложении: кнопка «Распаковать» приходит данными, а сама
  * распаковка — та же, что на карточке стопки в боте ([[BundleWork]]): вещи
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
      val work = new BundleWork(bag, content)
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
      val work   = new BundleWork(bag, content)
      for {
        inv  <- bag.get(base.id).mapError(e => new Throwable(e.toString))
        note <- work.unpack(base.id, inv, stack)
      } yield assertTrue(note.contains(content.format("inventory.bundle.unpackNoRoom", "count" -> "3"))) &&
              assertTrue(bag.snapshot.exists(_.id == stack.id) && bag.snapshot.size == 19)
    },

    test("не стопка — говорить не о чем") {
      val bag  = TestInventoryRepository.withItems(List(sword(1L)))
      val work = new BundleWork(bag, content)
      for {
        inv  <- bag.get(base.id).mapError(e => new Throwable(e.toString))
        note <- work.unpack(base.id, inv, sword(1L))
      } yield assertTrue(note.isEmpty && bag.snapshot.map(_.id) == List(1L))
    }
  )
}
