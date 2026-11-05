package pangea.model.hero

import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Смена надетого (`Hero.reequip`): запасы — здоровье, броня, энергия — держат
  * долю от нового максимума, а не сдвигаются на разницу вещей. Иначе ключ
  * стойки чинил бы броню и лечил: слабый комплект и обратно. */
object ReequipSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)

  private def gear(id: Long, t: ItemType, armor: Long = 0L, hp: Long = 0L, energy: Long = 0L, atk: Long = 0L): Item =
    Item(id, s"Вещь $id", 1L, Rarity.Blue, t, attack = atk, accuracy = 0L, energy = energy, armor = armor,
      defence = 0L, evasion = 0L, hp = hp)

  /** Тяжёлый комплект: 1000 брони, +900 к здоровью, +100 к энергии, +7 к атаке. */
  private val heavy = TestFixtures.emptyEquipment.copy(
    helmet     = gear(1L, ItemType.Helmet, armor = 1000L),
    chestPlate = gear(2L, ItemType.ChestPlate, hp = 900L, energy = 100L),
    weapon     = gear(3L, ItemType.Weapon, atk = 7L))
  /** Лёгкий: 100 брони, ни здоровья, ни энергии с вещей. */
  private val light = TestFixtures.emptyEquipment.copy(helmet = gear(4L, ItemType.Helmet, armor = 100L))
  /** Совсем без брони: одно кольцо. */
  private val bare  = TestFixtures.emptyEquipment.copy(firstRing = gear(5L, ItemType.Ring, atk = 1L))

  private def dressed(eq: Equipment, armor: Long = 0L, hp: Long = 1L, energy: Long = 0L): Hero = {
    val h = TestFixtures.hero(userId).copy(equipment = eq)
    h.copy(fightStats = h.fightStats.copy(armor = armor, hp = hp, energy = energy))
  }

  private val now = 0L

  override def spec = suite("Смена надетого: запасы держат долю")(

    test("броня 100 из 1000 → лёгкий комплект 10 из 100 → обратно 100 из 1000, а не доверху") {
      val worn = dressed(heavy, armor = 100L)
      val off  = worn.reequip(light, now)
      val back = off.reequip(heavy, now)
      assertTrue(worn.effectiveMaxArmor(now) == 1000L && off.effectiveMaxArmor(now) == 100L) &&
      assertTrue(off.fightStats.armor == 10L && back.fightStats.armor == 100L)
    },

    test("здоровье: полное в тяжёлом → в лёгком полное, но по его максимуму → обратно снова полное") {
      val max  = dressed(heavy).effectiveMaxHp(now)
      val full = dressed(heavy, hp = max)
      val off  = full.reequip(light, now)
      val back = off.reequip(heavy, now)
      assertTrue(off.effectiveMaxHp(now) == max - 900L) &&
      // прежде здоровье при смене не трогалось и уходило в бой больше максимума
      assertTrue(off.fightStats.hp == off.effectiveMaxHp(now)) &&
      assertTrue(back.fightStats.hp == max)
    },

    test("половина здоровья остаётся половиной") {
      val max  = dressed(heavy).effectiveMaxHp(now)
      val half = dressed(heavy, hp = max / 2)
      val off  = half.reequip(light, now)
      assertTrue(off.fightStats.hp == off.effectiveMaxHp(now) * (max / 2) / max)
    },

    test("туда-обратно запас не прибывает ни при каком начальном значении") {
      val maxHp = dressed(heavy).effectiveMaxHp(now)
      val maxEn = dressed(heavy).maxEnergy(now)
      // Здоровье — от десятой доли: у самого дна его держит пол в единицу
      // (тест ниже), и с 1 из 1000 обратная смена даёт пару единиц.
      val grows = for {
        armor  <- (0L to 1000L by 7L).toList
        hp      = maxHp / 10L + armor * (maxHp - maxHp / 10L) / 1000L
        energy  = armor * maxEn / 1000L
        start   = dressed(heavy, armor = armor, hp = hp, energy = energy)
        back    = start.reequip(light, now).reequip(heavy, now).reequip(bare, now).reequip(heavy, now)
        if back.fightStats.armor > armor || back.fightStats.hp > hp || back.fightStats.energy > energy
      } yield armor
      assertTrue(grows.isEmpty)
    },

    test("комплект совсем без брони: 100 из 1000 → 0 из 0 → обратно пусто, до отдыха") {
      val off  = dressed(heavy, armor = 100L).reequip(bare, now)
      val back = off.reequip(heavy, now)
      assertTrue(off.effectiveMaxArmor(now) == 0L && off.fightStats.armor == 0L) &&
      assertTrue(back.fightStats.armor == 0L)
    },

    test("брони не было вовсе — надетая приходит пустой") {
      val first = dressed(bare).reequip(light, now)
      assertTrue(first.effectiveMaxArmor(now) == 100L && first.fightStats.armor == 0L)
    },

    test("энергия держит долю, как броня") {
      val maxEn = dressed(heavy).maxEnergy(now)
      val full  = dressed(heavy, energy = maxEn)
      val off   = full.reequip(light, now)
      val back  = off.reequip(heavy, now)
      assertTrue(off.maxEnergy(now) < maxEn && off.fightStats.energy == off.maxEnergy(now)) &&
      assertTrue(back.fightStats.energy == maxEn)
    },

    test("живой от переодевания не умирает: последняя единица здоровья остаётся") {
      val off = dressed(heavy, hp = 1L).reequip(light, now)
      assertTrue(off.fightStats.hp == 1L)
    },

    test("атака сдвигается на разницу вещей") {
      val worn = dressed(heavy)
      val off  = worn.reequip(light, now)
      assertTrue(off.fightStats.atk == worn.fightStats.atk - 7L) &&
      assertTrue(off.reequip(heavy, now).fightStats.atk == worn.fightStats.atk)
    },

    test("максимум не сдвинулся — запасы как были, даже выше максимума") {
      val odd = dressed(light, armor = 300L, hp = 5L, energy = 3L)
      val ring = odd.reequip(light.copy(firstRing = gear(6L, ItemType.Ring)), now)
      assertTrue(ring.fightStats.armor == 300L && ring.fightStats.hp == 5L && ring.fightStats.energy == 3L)
    }
  )
}
