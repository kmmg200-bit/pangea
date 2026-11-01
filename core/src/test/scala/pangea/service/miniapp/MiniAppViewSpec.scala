package pangea.service.miniapp

import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.item.{Gem, GemKind, Grip, Item, ItemDetails, ItemType, Rarity}
import pangea.model.user.UserId
import pangea.service.state.states.EquipmentState
import pangea.test.TestFixtures
import zio.test._

/** Инвентарь для Mini App: стопки — как в боте, все слоты снаряжения на месте,
  * левая рука под двуручным — занята, а не пуста. */
object MiniAppViewSpec extends ZIOSpecDefault {

  private def ruby(id: Long): Item =
    Item(id, "Надколотый рубин", 1L, Rarity.Gray, ItemType.Gem,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Gem(Gem(GemKind.Ruby, Gem.MinGrade)))

  private val sword: Item =
    Item(10L, "Меч", 3L, Rarity.Blue, ItemType.Weapon,
      attack = 12, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(Grip.OneHanded))

  private val greatsword: Item =
    Item(20L, "Двуручник", 5L, Rarity.Green, ItemType.Weapon,
      attack = 30, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(Grip.TwoHanded))

  override def spec = suite("Инвентарь Mini App")(

    test("одинаковые камни — одной стопкой со счётчиком, оружие — само по себе") {
      val hero = TestFixtures.hero(UserId(1L))
      val view = MiniAppView.inventory(hero, Inventory(1L, hero.id, 20L, Inventory.Items(List(ruby(1L), sword, ruby(2L)))), _.name)
      assertTrue(
        view.items.map(i => i.kind -> i.count) == List("Gem" -> 2, "Weapon" -> 1),
        view.items.lastOption.exists(i => i.id == 10L && i.rarity == "Blue" && i.text.contains("Меч")),
        view.occupied == 3L,
        view.capacity == 20L
      )
    },

    test("снаряжение — все слоты; левая рука под двуручным занята им") {
      val hero = TestFixtures.hero(UserId(1L)).copy(id = HeroId(7L),
        equipment = TestFixtures.emptyEquipment.copy(weapon = greatsword))
      val view  = MiniAppView.inventory(hero, Inventory(1L, hero.id, 20L, Inventory.Items(Nil)), _.name)
      val right = view.equipment.find(_.name == EquipmentState.RightHandName)
      val left  = view.equipment.find(_.name == EquipmentState.LeftHandName)
      assertTrue(
        view.equipment.size == EquipmentState.slots.size,
        right.flatMap(_.item).exists(_.id == 20L),
        left.exists(s => s.item.isEmpty && s.busy.contains(EquipmentState.LeftBusyText)),
        view.equipment.filter(_.item.isEmpty).forall(_.name != EquipmentState.RightHandName)
      )
    }
  )
}
