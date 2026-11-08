package pangea.service.miniapp

import pangea.engine.SceneContent
import pangea.model.artifact.HeroArtifacts
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.home.HomeData
import pangea.model.inventory.Inventory
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.equipment.Outfit
import pangea.service.state.states.EquipmentState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository}
import zio.test._
import zio.{Ref, ZIO}

/** Снаряжение из приложения: общий с ботом [[Outfit]] и то, что приложение
  * добавляет сверху, — запрет в бою и строки итога в ответ, а не в чат. */
object MiniAppGearSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("1"), TelegramId("-1"))

  private val rightHand = EquipmentState.slots.indexWhere(_.name == EquipmentState.RightHandName)

  private def blade(id: Long, name: String, atk: Int): Item =
    Item(id, name, 1L, Rarity.Gray, ItemType.Weapon,
      attack = atk, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private val sword = blade(10L, "Меч", 20)
  private val axe   = blade(21L, "Топор", 30)

  private def setup(weapon: Item, bag: List[Item]) = {
    val base = TestFixtures.hero(userId, state = StateType.Dungeon)
    // Статы — уже с надетым, как у героя в игре.
    val hero = base.copy(equipment = TestFixtures.emptyEquipment.copy(weapon = weapon),
                 fightStats = base.fightStats.copy(atk = weapon.attack.toLong))
    for {
      dao  <- TestHeroDao.withHero(userId, hero)
      inv   = TestInventoryRepository.withItems(bag)
      c    <- ZIO.attempt(SceneContent.load())
    } yield (new Outfit(dao, inv, TestItemRepository.make, c), dao, inv, c, hero)
  }

  def spec = suite("MiniApp: снаряжение")(
    test("надеть вместо надетого: прежнее — в сумку, строка — в ответ приложению") {
      for {
        t     <- setup(sword, List(axe))
        (outfit, dao, inv, c, hero) = t
        lines <- Ref.make(Vector.empty[String])
        worn  <- outfit.wear(user, hero, axe, EquipmentState.slots(rightHand).pairSlot, new MiniApp.Collect(lines))(
                   Outfit.wornLine(c, axe, _))
        after <- dao.getHeroByUserId(userId).someOrFailException
        said  <- lines.get
      } yield assertTrue(
        worn,
        after.equipment.weapon.id == axe.id,
        after.fightStats.atk == axe.attack.toLong,
        inv.snapshot.map(_.id) == List(sword.id),
        said.size == 1 && said.head.contains(axe.name) && said.head.contains(sword.name)
      )
    },
    test("снять: вещь — в сумку; сумка полна — отказ, вещь остаётся на герое") {
      val full = (1L to 20L).toList.map(i => blade(100L + i, s"Хлам $i", 1))
      for {
        t1     <- setup(sword, Nil)
        (outfit, dao, inv, c, hero) = t1
        lines  <- Ref.make(Vector.empty[String])
        off    <- outfit.takeOff(user, hero, EquipmentState.slots(rightHand), new MiniApp.Collect(lines))
        after  <- dao.getHeroByUserId(userId).someOrFailException
        said   <- lines.get
        t2     <- setup(sword, full)
        (cramped, dao2, _, _, hero2) = t2
        lines2 <- Ref.make(Vector.empty[String])
        kept   <- cramped.takeOff(user, hero2, EquipmentState.slots(rightHand), new MiniApp.Collect(lines2))
        after2 <- dao2.getHeroByUserId(userId).someOrFailException
        said2  <- lines2.get
      } yield assertTrue(
        off && after.equipment.weapon.itemType == ItemType.NoItem && inv.snapshot.map(_.id) == List(sword.id),
        said.head.startsWith(c.format("equipment.unequipped", "name" -> sword.name)),
        !kept && after2.equipment.weapon.id == sword.id,
        said2 == Vector(c.text("equipment.unequipInventoryFull"))
      )
    },
    test("в бою снаряжение не меняют: где угодно ещё — можно; вид говорит почему и знает номера слотов") {
      val c      = SceneContent.load()
      val hero   = TestFixtures.hero(userId, state = StateType.Battle)
      val inv    = Inventory(1L, hero.id, 20L, Inventory.Items(Nil))
      val stores = MiniAppView.Stores(HeroArtifacts.empty(hero.id), Barrel.empty(hero.id), BankVault.empty(hero.id),
                     HomeData.empty)
      val fight  = MiniAppView.inventory(hero, inv, stores, c, _ => "", 0L)
      val calm   = MiniAppView.inventory(hero.copy(state = StateType.Dungeon), inv, stores, c, _ => "", 0L)
      assertTrue(
        MiniApp.inBattle(hero),
        !MiniApp.inBattle(hero.copy(state = StateType.Dungeon)),
        !MiniApp.inBattle(hero.copy(state = StateType.MarketSquare)),
        fight.gearLocked.contains(c.text("miniapp.equipment.inBattle")),
        calm.gearLocked.isEmpty,
        calm.equipment.map(_.index) == EquipmentState.slots.indices.toList,
        calm.equipment(rightHand).name == EquipmentState.RightHandName
      )
    }
  )
}
