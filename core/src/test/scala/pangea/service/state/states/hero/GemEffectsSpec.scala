package pangea.service.state.states.hero

import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Item, ItemType, Rarity}
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Камни в снаряжении: карточка вещи говорит, что даёт каждый камень именно в
  * ней, а в «Эффектах» — сводки по оружию и по доспехам. */
object GemEffectsSpec extends ZIOSpecDefault {

  private def socketed(t: ItemType, gems: Gem*): Item =
    Item(1L, "Вещь", 10L, Rarity.Blue, t, attack = 0, accuracy = 0, energy = 0, armor = 10, defence = 0, evasion = 0,
      sockets = gems.toList.map(Some(_)))

  private val ruby2 = Gem(GemKind.Ruby, 2)

  private def hero(weapon: Item, helmet: Item): Hero = {
    val h = TestFixtures.hero(UserId(1L))
    h.copy(equipment = h.equipment.copy(weapon = weapon, helmet = helmet))
  }

  override def spec = suite("Камни в снаряжении")(

    test("карточка вещи: камень в броне и в щите — броневая грань, в оружии — оружейная") {
      val helm   = socketed(ItemType.Helmet, ruby2).statsLines.mkString("\n")
      val shield = socketed(ItemType.Shield, ruby2).statsLines.mkString("\n")
      val sword  = socketed(ItemType.Weapon, ruby2).statsLines.mkString("\n")
      assertTrue(helm.contains(s"💎 ${ruby2.displayName}: +2% к макс. HP и +40 к макс. HP.")) &&
      assertTrue(shield.contains("+2% к макс. HP") && !shield.contains("стихия Огня")) &&
      assertTrue(sword.contains(s"💎 ${ruby2.displayName}: стихия Огня") && !sword.contains("к макс. HP"))
    },

    test("«Эффекты оружия»: камни и итог по броне и HP — рубин 2 бьёт по броне на 84%, по HP на 114%") {
      val h     = hero(socketed(ItemType.Weapon, ruby2), socketed(ItemType.Helmet))
      val text  = SkillsState.weaponFx(h)
      val ids   = SkillsState.entries(h).map(_.buttonId)
      assertTrue(text.contains("стихия Огня") && text.contains("Урон по броне: 84%") && text.contains("Урон по HP: 114%")) &&
      assertTrue(ids.contains("Fx_Weapon") && !ids.contains("Fx_Armor"))
    },

    test("«Эффекты доспехов»: камни всего снаряжения, кроме оружия, и их сумма") {
      val h    = hero(socketed(ItemType.Weapon), socketed(ItemType.Helmet, ruby2, Gem(GemKind.Amethyst, 3)))
      val text = SkillsState.armorFx(h)
      val ids  = SkillsState.entries(h).map(_.buttonId)
      assertTrue(text.contains("Макс. HP: +2% и +40") && text.contains("Защита: +3%")) &&
      assertTrue(ids.contains("Fx_Armor") && !ids.contains("Fx_Weapon"))
    }
  )
}
