package pangea.model.hero

import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test.{assertTrue, Spec, TestEnvironment, ZIOSpecDefault}

object HeroStatBreakdownSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, Any] = suite("HeroStatBreakdown")(
    test("точность: строки сходятся с effectiveFightStats") {
      val base = TestFixtures.hero(UserId(1L)).copy(
        fightStats = TestFixtures.hero(UserId(1L)).fightStats.copy(accuracy = 100))
      val gloves = Item(10L, "перч", 1L, Rarity.Gray, ItemType.Gloves,
        attack = 0, accuracy = 29, energy = 0, armor = 0, defence = 0, evasion = 0)
      val amulet = Item(11L, "амул", 1L, Rarity.Gray, ItemType.Amulet,
        attack = 0, accuracy = 73, energy = 0, armor = 0, defence = 0, evasion = 0)
      val hero = base.copy(equipment = base.equipment.copy(gloves = gloves, amulet = amulet))
      val view = HeroStatBreakdown.accuracy(hero, 0L)
      val sum  = view.lines.map(_.value).sum
      assertTrue(view.current == hero.effectiveFightStats(0L).accuracy, sum == view.current)
    },

    test("раскладка точности: собственная и вещи по слотам") {
      val own = 50L
      val base = TestFixtures.hero(UserId(1L)).copy(
        fightStats = TestFixtures.hero(UserId(1L)).fightStats.copy(accuracy = own + 73 + 29))
      val gloves = Item(10L, "перч", 1L, Rarity.Gray, ItemType.Gloves,
        attack = 0, accuracy = 29, energy = 0, armor = 0, defence = 0, evasion = 0)
      val amulet = Item(11L, "амул", 1L, Rarity.Gray, ItemType.Amulet,
        attack = 0, accuracy = 73, energy = 0, armor = 0, defence = 0, evasion = 0)
      val hero = base.copy(equipment = base.equipment.copy(gloves = gloves, amulet = amulet))
      val labels = HeroStatBreakdown.accuracy(hero, 0L).lines.map(_.label).toSet
      assertTrue(labels.contains("Собственная"), labels.contains("Амулет"), labels.contains("Перчатки"))
    }
  )
}