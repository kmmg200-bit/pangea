package pangea.model.hero

import pangea.model.item.{Item, ItemSet, ItemType, Rarity, SetRates}
import pangea.model.monster.Race
import pangea.model.stats.BaseStats
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Малые расовые наборы: по одному на смертную расу, пороги 3/6/9 и скрытый
  * 12 — только своей расе. */
object RaceSetsSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)

  private val raceSets: List[ItemSet] = ItemSet.values.toList.filter(_.race.isDefined)

  private def piece(id: Long, t: ItemType, set: ItemSet): Item =
    Item(id, "Предмет", 1L, Rarity.Blue, t,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, set = Some(set))

  /** `n` первых сетовых слотов (без оружия) — из набора `set`. */
  private def wearing(set: ItemSet, n: Int): Equipment = {
    val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
      ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet, ItemType.Ring, ItemType.Ring,
      ItemType.Belt)
    slots.take(n).zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (eq, (t, i)) =>
      val it = piece(800L + i, t, set)
      i match {
        case 0 => eq.copy(helmet = it)
        case 1 => eq.copy(shoulderPads = it)
        case 2 => eq.copy(chestPlate = it)
        case 3 => eq.copy(bracelets = it)
        case 4 => eq.copy(gloves = it)
        case 5 => eq.copy(pants = it)
        case 6 => eq.copy(boots = it)
        case 7 => eq.copy(amulet = it)
        case 8 => eq.copy(firstRing = it)
        case 9 => eq.copy(secondRing = it)
        case _ => eq.copy(belt = it)
      }
    }
  }

  private def hero(eq: Equipment): Hero =
    TestFixtures.hero(userId).copy(baseStats = BaseStats(agi = 100, vit = 100, str = 100, int = 100), equipment = eq)

  override def spec = suite("Малые расовые наборы")(

    test("у каждой смертной расы свой набор, у элементалей, нежити, зверей и сооружений — нет") {
      val nonMortal = Race.values.filterNot(Race.mortals.contains)
      assertTrue(Race.mortals.forall(r => ItemSet.ofRace(r).isDefined)) &&
      assertTrue(Race.mortals.flatMap(ItemSet.ofRace).distinct.size == Race.mortals.size) &&
      assertTrue(raceSets.size == Race.mortals.size) &&
      assertTrue(nonMortal.nonEmpty && nonMortal.forall(r => ItemSet.ofRace(r).isEmpty))
    },

    test("пороги — 3/6/9 и скрытый 12; у больших наборов ни расы, ни скрытых шагов") {
      assertTrue(raceSets.forall(_.bonuses.map(_.pieces) == List(3, 6, 9, 12))) &&
      assertTrue(raceSets.forall(s => s.bonuses.map(_.ownRaceOnly) == List(false, false, false, true))) &&
      assertTrue(ItemSet.values.filter(_.race.isEmpty).forall(s =>
        s.bonuses.map(_.pieces) == ItemSet.Thresholds && !s.bonuses.exists(_.ownRaceOnly)))
    },

    test("шаг 12 есть только у своей расы: чужой не видит ни бонуса, ни порога, ни счёта выше девяти") {
      val human = HeroSets(Map(ItemSet.Horde -> 12), Some(Race.Human))
      val orc   = HeroSets(Map(ItemSet.Horde -> 12), Some(Race.Orc))
      assertTrue(human.bonuses(ItemSet.Horde).map(_.pieces) == List(3, 6, 9)) &&
      assertTrue(human.thresholds(ItemSet.Horde) == List(3, 6, 9) && human.shownPieces(ItemSet.Horde) == 9) &&
      assertTrue(orc.bonuses(ItemSet.Horde).map(_.pieces) == List(3, 6, 9, 12)) &&
      assertTrue(orc.thresholds(ItemSet.Horde) == List(3, 6, 9, 12) && orc.shownPieces(ItemSet.Horde) == 12) &&
      // большие наборы раса не трогает
      assertTrue(HeroSets(Map(ItemSet.Ghoul -> 12), Some(Race.Human)).bonuses(ItemSet.Ghoul).size == 6)
    },

    test("шаг 3: +8% к своему стату у каждого набора, «Первопроходец» — +3% ко всем четырём; двух вещей мало") {
      def at(set: ItemSet, n: Int): HeroSets = HeroSets(Map(set -> n))
      val pct = SetRates.RaceStatPct
      assertTrue(pct == 8L) &&
      assertTrue(at(ItemSet.QuietGrove, 3).accuracyBonusPct == pct) &&
      assertTrue(at(ItemSet.DeepMother, 3).evasionBonusPct == pct) &&
      assertTrue(at(ItemSet.Horde, 3).maxHpBonusPct == pct) &&
      assertTrue(at(ItemSet.Marauder, 3).attackBonusPct == pct) &&
      assertTrue(at(ItemSet.Underworld, 3).intBonusPct == pct) &&
      assertTrue(at(ItemSet.MountainMaster, 3).armorBonusPct == pct) &&
      assertTrue(at(ItemSet.Caravan, 3).agiBonusPct == pct) &&
      assertTrue(List(at(ItemSet.Pioneer, 3).strBonusPct, at(ItemSet.Pioneer, 3).vitBonusPct,
                      at(ItemSet.Pioneer, 3).agiBonusPct, at(ItemSet.Pioneer, 3).intBonusPct) == List.fill(4)(3L)) &&
      assertTrue(at(ItemSet.Marauder, 2).attackBonusPct == 0L && at(ItemSet.Horde, 2).maxHpBonusPct == 0L)
    },

    test("«Мать Глубин» 9 и «Подгорный мастер» 6/9: реген в конце раунда и шанс холода") {
      val deep = HeroSets(Map(ItemSet.DeepMother -> 9))
      assertTrue(deep.hpRegenPct == 2L && deep.armorRegenPct == 2L) &&
      assertTrue(HeroSets(Map(ItemSet.DeepMother -> 8)).hpRegenPct == 0L) &&
      assertTrue(HeroSets(Map(ItemSet.MountainMaster -> 6)).armorRegenPct == 4L) &&
      assertTrue(HeroSets(Map(ItemSet.MountainMaster -> 6)).coldChanceBonusPct == 0L) &&
      assertTrue(HeroSets(Map(ItemSet.MountainMaster -> 9)).coldChanceBonusPct == 15L)
    },

    test("на герое: «Первопроходец» 3 поднимает все четыре характеристики и HP, «Мать Глубин» 9 лечит в конце раунда") {
      val bare    = hero(TestFixtures.emptyEquipment)
      val pioneer = hero(wearing(ItemSet.Pioneer, 3))
      val base    = pioneer.effectiveBaseStats(0L)
      val murloc  = hero(wearing(ItemSet.DeepMother, 9))
      val maxHp   = murloc.effectiveMaxHp(0L)
      val healed  = murloc.copy(fightStats = murloc.fightStats.copy(hp = 100L)).withCombatRegen(0L)
      assertTrue(bare.effectiveBaseStats(0L) == BaseStats(agi = 100, vit = 100, str = 100, int = 100)) &&
      assertTrue(base == BaseStats(agi = 103, vit = 103, str = 103, int = 103)) &&
      assertTrue(pioneer.effectiveMaxHp(0L) == bare.effectiveMaxHp(0L) * 103L / 100L) &&
      assertTrue(healed.fightStats.hp == 100L + maxHp * 2L / 100L)
    }
  )
}
