package pangea.generator

import pangea.domain.Rng
import pangea.generator.loot.LootGenerator
import pangea.generator.loot.LootGenerator.LootDrop
import pangea.model.item.{Gem => GemModel, GemKind, Item, ItemDetails, ItemType, TrophyKind}
import pangea.model.monster.{Race, Rarity}
import zio.test._

object LootGeneratorSpec extends ZIOSpecDefault {

  private def raceOf(i: Item): Option[String] = i.details match {
    case ItemDetails.Trophy(r, _, _) => Some(r)
    case _                        => None
  }

  private def category(d: LootDrop): String = d match {
    case LootDrop.Gear(_)         => "gear"
    case LootDrop.Trophy(_)       => "trophy"
    case LootDrop.MapHalf(_)      => "mapHalf"
    case LootDrop.Gem(_)          => "gem"
    case LootDrop.Flask(_)        => "flask"
    case LootDrop.Rune(i)         => if (i.isSmallRune) "runeSmall" else "rune"
    case LootDrop.Silver(_, true) => "silverPile"
    case LootDrop.Silver(_, _)    => "silverSmall"
    case LootDrop.Doubloons(_)    => "doubloons"
  }

  // Доля боёв (в %), в которых выпала категория `cat`, по выборке сидов.
  private def catRatePct(tier: Rarity, cat: String, samples: Long = 20000L): Double = {
    val hits = (1L to samples).count { s =>
      LootGenerator.roll(tier, Race.Orc, 30L, Rng(s))._1.map(category).contains(cat)
    }
    hits * 100.0 / samples
  }

  def spec = suite("LootGeneratorSpec")(
    test("благословение после минибосса: лишняя вещь — из его набора, а не случайная") {
      val bosses = pangea.model.monster.MiniBoss.values.toList
      val got    = bosses.map(b => b -> LootGenerator.rollBlessingBossGear(100L, b, 10L, Rng(7L))._1)
      val none   = LootGenerator.rollBlessingBossGear(0L, pangea.model.monster.MiniBoss.FireElemental, 10L, Rng(7L))._1
      // Набор теперь есть у каждого минибосса (у Громового — «Гроза», у
      // Ледяного — «Стужа»), и лишняя вещь всегда из него.
      assertTrue(got.forall { case (b, item) => item.exists(_.set == b.set) }) &&
      assertTrue(bosses.forall(_.set.isDefined)) &&
      assertTrue(got.forall { case (_, item) => item.exists(_.set.isDefined) }) &&
      assertTrue(none.isEmpty)
    },


    test("детерминизм: один seed → одинаковый дроп") {
      val (a, _) = LootGenerator.roll(Rarity.Legendary, Race.Orc, 20L, Rng(123L))
      val (b, _) = LootGenerator.roll(Rarity.Legendary, Race.Orc, 20L, Rng(123L))
      assertTrue(a == b)
    },

    test("нет повторных категорий за один бой (ответ 4)") {
      val cases = (1L to 200L).map { s =>
        val (drops, _) = LootGenerator.roll(Rarity.Legendary, Race.Human, 30L, Rng(s))
        val cats       = drops.map(category)
        cats.distinct.size == cats.size
      }
      assertTrue(cases.forall(identity))
    },

    test("обычный моб: не больше одного дропа за бой") {
      val cases = (1L to 200L).map { s =>
        LootGenerator.roll(Rarity.Common, Race.Goblin, 5L, Rng(s))._1.size <= 1
      }
      assertTrue(cases.forall(identity))
    },

    test("легендарный моб роняет лут хотя бы иногда (первые два слота 100%)") {
      val any = (1L to 50L).exists(s => LootGenerator.roll(Rarity.Legendary, Race.Demon, 40L, Rng(s))._1.nonEmpty)
      assertTrue(any)
    },

    test("трофей хранит расу и уровень убийства") {
      val trophy = (1L to 300L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Rare, Race.Khajiit, 12L, Rng(s))._1)
        .collectFirst { case LootDrop.Trophy(i) => i }
      assertTrue(trophy.exists(i =>
        i.itemType == ItemType.Trophy &&
        raceOf(i).contains(Race.Khajiit.entryName) &&
        i.lvl == 12L &&
        i.name.contains("Каджит")))
    },

    test("серебро всегда положительное и около lvl×4 ±20%") {
      val silvers = (1L to 300L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Mythical, Race.Gnome, 10L, Rng(s))._1)
        .collect { case LootDrop.Silver(a, _) => a }
        .toList
      // база = 40, разброс 80..120% → [32, 48]
      assertTrue(silvers.nonEmpty) &&
      assertTrue(silvers.forall(g => g >= 32L && g <= 48L))
    },

    test("экипировка генерируется с разбросом уровня из ItemGenerator (killLevel-6 .. killLevel+1)") {
      val gears = (1L to 300L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Rare, Race.Elf, 50L, Rng(s))._1)
        .collect { case LootDrop.Gear(i) => i }
        .toList
      assertTrue(gears.nonEmpty) &&
      assertTrue(gears.forall(i => i.lvl >= 1L && i.lvl <= 51L)) &&
      assertTrue(gears.forall(_.itemType != ItemType.Trophy))
    },

    test("половинка карты падает у мифических и легендарных мобов") {
      def half(tier: Rarity) = (1L to 5000L).iterator
        .flatMap(s => LootGenerator.roll(tier, Race.Orc, 40L, Rng(s))._1)
        .collectFirst { case LootDrop.MapHalf(i) => i }
      assertTrue(half(Rarity.Legendary).exists(_.itemType == ItemType.TreasureMapHalf)) &&
        assertTrue(half(Rarity.Mythical).exists(_.itemType == ItemType.TreasureMapHalf))
    },

    test("у обычных/необычных/редких мобов половинка карты не падает") {
      val lowerTiers = List(Rarity.Common, Rarity.Uncommon, Rarity.Rare)
      val anyHalf = lowerTiers.exists { tier =>
        (1L to 3000L).iterator
          .flatMap(s => LootGenerator.roll(tier, Race.Orc, 40L, Rng(s))._1)
          .exists { case LootDrop.MapHalf(_) => true; case _ => false }
      }
      assertTrue(!anyHalf)
    },

    // ── Камни-усилители ───────────────────────────────────────────────────────
    test("камень падает у редких, мифических и легендарных; у обычных и необычных — нет") {
      def hasGem(tier: Rarity) = (1L to 5000L).iterator
        .flatMap(s => LootGenerator.roll(tier, Race.Orc, 30L, Rng(s))._1)
        .exists { case LootDrop.Gem(_) => true; case _ => false }
      assertTrue(hasGem(Rarity.Rare)) &&
      assertTrue(hasGem(Rarity.Mythical)) &&
      assertTrue(hasGem(Rarity.Legendary)) &&
      assertTrue(!hasGem(Rarity.Common)) &&
      assertTrue(!hasGem(Rarity.Uncommon))
    },

    test("выпавший камень — 1-го тира, вид случаен и черепа тоже падают") {
      val gems = (1L to 6000L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Legendary, Race.Orc, 30L, Rng(s))._1)
        .collect { case LootDrop.Gem(i) => i }
        .toList
      val kinds = gems.flatMap(_.gem).map(_.kind).toSet
      assertTrue(gems.nonEmpty) &&
      assertTrue(gems.forall(_.itemType == ItemType.Gem)) &&
      assertTrue(gems.flatMap(_.gem).forall(_.grade == GemModel.MinGrade)) &&
      assertTrue(kinds.contains(GemKind.Skull)) &&
      assertTrue(kinds.size == GemKind.values.size) // встречаются все семь видов
    },

    test("легендарный роняет камень заметно чаще редкого и мифического (вес 5% против 1%)") {
      val rare = catRatePct(Rarity.Rare, "gem")
      val myth = catRatePct(Rarity.Mythical, "gem")
      val leg  = catRatePct(Rarity.Legendary, "gem")
      assertTrue(math.abs(rare - myth) < 0.5) && // у обоих вес 1%
      assertTrue(leg > rare * 4)                 // у легендарного впятеро больший вес
    },

    // Веса категорий должны в сумме давать ровно 100, иначе в первом слоте
    // появляется дыра «пусто». У редких и выше первый слот срабатывает всегда,
    // поэтому пустая добыча тут же выдала бы ошибку в арифметике весов.
    test("у редких и выше добыча никогда не пуста (сумма весов категорий = 100)") {
      val tiers = List(Rarity.Rare, Rarity.Mythical, Rarity.Legendary)
      assertTrue(tiers.forall { tier =>
        (1L to 3000L).forall(s => LootGenerator.roll(tier, Race.Orc, 30L, Rng(s))._1.nonEmpty)
      })
    },

    // ── Большие руны ──────────────────────────────────────────────────────────
    test("рунные камни падают у всех тиров на те же 5% (за счёт экипировки): большая 1%, малая 4%, без уровня, любой руны") {
      import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
      val common = catRatePct(Rarity.Common, "rune") + catRatePct(Rarity.Common, "runeSmall")
      val leg    = catRatePct(Rarity.Legendary, "rune") + catRatePct(Rarity.Legendary, "runeSmall")
      val stones = (1L to 20000L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Legendary, Race.Orc, 30L, Rng(s))._1)
        .collect { case LootDrop.Rune(i) => i }.toList
      val (big, small) = stones.partition(_.runeStone.exists(_.size == RuneStoneSize.Big))
      val runes  = stones.flatMap(_.runeStone).flatMap(d => Rune.byKey(d.runeKey)).toSet
      // у обычных первый слот 30% × 5% ≈ 1,5%; у легендарных два гарантированных слота ≈ 10%
      assertTrue(common > 1.0 && common < 2.2) && assertTrue(leg > 7.0 && leg < 13.0) &&
      assertTrue(stones.nonEmpty && stones.forall(i => i.itemType == ItemType.RuneStone && i.lvl == 1L)) &&
      // малых вчетверо больше больших
      assertTrue(big.nonEmpty && small.size > big.size * 2 && small.size < big.size * 7) &&
      assertTrue(runes == RuneStone.all.toSet) &&
      assertTrue(big.forall(i => i.name.startsWith("Большая руна ") && !i.weightless)) &&
      assertTrue(small.forall(i => i.name.startsWith("Малая руна ") && i.weightless)) &&
      assertTrue(stones.forall(_.statsLines.contains(RuneStone.Insight)))
    },

    // ── Фляги ─────────────────────────────────────────────────────────────────
    test("фляга падает у всех тиров с весом 1% (за счёт экипировки), уровня 1, любой из семей") {
      import pangea.model.item.{FlaskKind, ItemDetails => ID}
      val common = catRatePct(Rarity.Common, "flask")
      val leg    = catRatePct(Rarity.Legendary, "flask")
      val flasks = (1L to 20000L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Legendary, Race.Orc, 30L, Rng(s))._1)
        .collect { case LootDrop.Flask(i) => i }.toList
      val kinds = flasks.flatMap(f => f.details match { case ID.Flask(e, _, _) => FlaskKind.ofEffect(e); case _ => None }).toSet
      // у обычных первый слот 30% × 1% ≈ 0,3%; у легендарных два гарантированных слота ≈ 2%
      assertTrue(common > 0.1 && common < 0.6) && assertTrue(leg > 1.2 && leg < 3.0) &&
      assertTrue(flasks.nonEmpty && flasks.forall(f => f.itemType == ItemType.Flask && f.lvl == 1L)) &&
      assertTrue(kinds == FlaskKind.values.toSet) &&
      // заряды по редкости, фляга полна
      assertTrue(flasks.forall(f => f.details match {
        case ID.Flask(_, c, m) => c == m && m == FlaskKind.chargesFor(f.rarity)
        case _                 => false
      })) &&
      assertTrue(flasks.map(_.rarity).toSet.contains(pangea.model.item.Rarity.Gray) &&
                 flasks.map(_.rarity).toSet.contains(pangea.model.item.Rarity.Orange))
    },

    // ── Дроп с элементаля ─────────────────────────────────────────────────────
    test("дроп с элементаля есть всегда: 0..1 + BossLvL предметов") {
      val counts = (1L to 300L).map { s =>
        LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, bossLvl = 3L, heroLvl = 40L, Rng(s))._1.size
      }
      assertTrue(counts.forall(n => n == 3 || n == 4)) && // BossLvL 3 плюс 0 или 1
      assertTrue(counts.contains(3)) && assertTrue(counts.contains(4))
    },

    test("с элементаля падают его ингредиент и вещи набора — фиолетовые и синие") {
      val items = (1L to 300L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, 2L, 40L, Rng(s))._1)
        .flatMap(_.itemOpt).toList
      val (materials, rest) = items.partition(_.itemType == ItemType.Material)
      val gear = rest.filterNot(_.itemType == ItemType.RuneStone)
      assertTrue(materials.nonEmpty) && assertTrue(gear.nonEmpty) &&
      // кроме его ингредиента из материалов — только резец рунописца
      assertTrue(materials.forall(m => m.material.contains(pangea.model.item.MaterialKind.EverburningIron) ||
                                       m.material.contains(pangea.model.item.MaterialKind.RuneChisel))) &&
      // 40% роллов — ингредиент, 10% — руна, 10% — резец, остальное вещь набора: синяя почти
      // всегда, фиолетовая — 5% вещей набора, оранжевая — полпроцента
      assertTrue(gear.forall(g => g.rarity == pangea.model.item.Rarity.Purple || g.rarity == pangea.model.item.Rarity.Blue ||
                                  g.rarity == pangea.model.item.Rarity.Orange)) &&
      assertTrue(gear.exists(_.rarity == pangea.model.item.Rarity.Purple)) &&
      assertTrue(gear.exists(_.rarity == pangea.model.item.Rarity.Blue)) &&
      assertTrue(gear.forall(_.set.contains(pangea.model.item.ItemSet.WildFlame))) &&
      // имя сетовое: набор встал вместо титула
      assertTrue(gear.forall(_.name.endsWith(pangea.model.item.ItemSet.WildFlame.title)))
    },

    test("оранжевые вещи падают только с именных мобов — легендарных") {
      import pangea.generator.monster.MonsterGenerator
      import pangea.model.monster.{Race => MobRace, Rarity => MobRarity}
      // Смотрим только на экипировку: у фляги своя шкала редкости (она расходник
      // со своим числом зарядов), трофеи и камни редкости вещей не несут.
      def rarities(tier: MobRarity, bump: Long = 0L): Set[pangea.model.item.Rarity] =
        (1L to 3000L).toList
          .flatMap(s => LootGenerator.roll(tier, MobRace.Orc, 40L, Rng(s), rarityBumpPct = bump)._1)
          .flatMap(_.itemOpt)
          .filter(i => ItemType.equippable.contains(i.itemType) && i.itemType != ItemType.Flask)
          .map(_.rarity).toSet
      // Имя есть только у легендарного — он и есть «именной»
      assertTrue(MonsterGenerator.legendaryName(MobRace.Orc, MobRarity.Mythical, Rng(1L))._1.isEmpty) &&
      assertTrue(MonsterGenerator.legendaryName(MobRace.Orc, MobRarity.Legendary, Rng(1L))._1.nonEmpty) &&
      assertTrue(!rarities(MobRarity.Common).contains(pangea.model.item.Rarity.Orange)) &&
      assertTrue(!rarities(MobRarity.Uncommon).contains(pangea.model.item.Rarity.Orange)) &&
      assertTrue(!rarities(MobRarity.Rare).contains(pangea.model.item.Rarity.Orange)) &&
      assertTrue(!rarities(MobRarity.Mythical).contains(pangea.model.item.Rarity.Orange)) &&
      assertTrue(rarities(MobRarity.Legendary).contains(pangea.model.item.Rarity.Orange)) &&
      // благословение Азата поднимает редкость на тир, но безымянному мобу
      // оранжевой вещи всё равно не выдаёт
      assertTrue(!rarities(MobRarity.Mythical, bump = 100L).contains(pangea.model.item.Rarity.Orange)) &&
      assertTrue(LootGenerator.rarityCap(MobRarity.Mythical) == pangea.model.item.Rarity.Violet) &&
      assertTrue(LootGenerator.rarityCap(MobRarity.Legendary) == pangea.model.item.Rarity.Orange)
    },

    test("редкость вещи набора с минибосса: 5% фиолетовых, 0,5% легендарных, остальное синее — и после ингредиента не больше") {
      import pangea.model.item.{Rarity => IR}
      import pangea.model.monster.MiniBoss
      def share(items: List[Item], r: IR): Double = items.count(_.rarity == r).toDouble * 100.0 / items.size
      // Первый бросок с босса всегда идёт по полной шкале: среди вещей набора с
      // него фиолетовых 5% и легендарных 0,5% — и у элементаля, где вещи берут
      // треть бросков, и у Джо, где десятую часть.
      def firstGear(boss: MiniBoss): List[Item] =
        (1L to 10000L).toList.flatMap(s => LootGenerator.rollMiniBoss(boss, 2L, 40L, Rng(s))._1.headOption)
          .flatMap(_.itemOpt).filter(_.set.isDefined)
      val fireFirst = firstGear(MiniBoss.FireElemental)
      val joeFirst  = firstGear(MiniBoss.RottenJoe)
      // На бросок добычи за весь бой — и после выпавшего ингредиента: фиолетовая
      // и легендарная выпадают с шансом 5% и 0,5% от вещевой доли ПОЛНОЙ шкалы.
      // На BossLvL 10 бросков десять-одиннадцать, и почти все — уже без
      // ингредиента: выросли бы доли после него — выросло бы и это.
      def perRoll(boss: MiniBoss): (Double, Double) = {
        val drops = (1L to 3000L).toList.flatMap(s => LootGenerator.rollMiniBoss(boss, 10L, 40L, Rng(s))._1)
        val gear  = drops.flatMap(_.itemOpt).filter(_.set.isDefined)
        (gear.count(_.rarity == IR.Purple).toDouble * 100.0 / drops.size,
         gear.count(_.rarity == IR.Orange).toDouble * 100.0 / drops.size)
      }
      val (firePurple, fireOrange) = perRoll(MiniBoss.FireElemental) // 1,67% и 0,17%: вещи — 40 из 120
      val (joePurple,  joeOrange)  = perRoll(MiniBoss.RottenJoe)     // 0,5% и 0,05%: вещи — 15 из 150
      assertTrue(LootGenerator.BossPurpleOfGearPerMille == 50L && LootGenerator.BossOrangeOfGearPerMille == 5L) &&
      assertTrue(share(fireFirst, IR.Purple) > 3.8 && share(fireFirst, IR.Purple) < 6.2) &&
      assertTrue(share(joeFirst, IR.Purple) > 3.0 && share(joeFirst, IR.Purple) < 7.0) &&
      // легендарных — полпроцента вещей: у Джо их больше не 3%, как было
      assertTrue(share(fireFirst, IR.Orange) < 1.0 && share(joeFirst, IR.Orange) < 1.5) &&
      // всё, что не фиолетовое и не легендарное, — синее
      assertTrue(share(fireFirst, IR.Blue) > 91.0 && share(joeFirst, IR.Blue) > 90.0) &&
      assertTrue((fireFirst ++ joeFirst).forall(g => g.rarity == IR.Purple || g.rarity == IR.Blue || g.rarity == IR.Orange)) &&
      assertTrue(firePurple > 1.4 && firePurple < 1.95) &&
      assertTrue(joePurple > 0.38 && joePurple < 0.62) &&
      assertTrue(fireOrange > 0.1 && fireOrange < 0.24) &&
      assertTrue(joeOrange > 0.02 && joeOrange < 0.09)
    },

    test("у Гнилого Джо вещь набора тоже бывает и фиолетовой, и синей") {
      val gear = (1L to 3000L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.RottenJoe, 2L, 40L, Rng(s))._1)
        .flatMap(_.itemOpt)
        .filter(i => i.set.contains(pangea.model.item.ItemSet.Ghoul)).toList
      val purple = gear.count(_.rarity == pangea.model.item.Rarity.Purple).toDouble
      val blue   = gear.count(_.rarity == pangea.model.item.Rarity.Blue).toDouble
      assertTrue(gear.nonEmpty) &&
      assertTrue(gear.forall(g => g.rarity == pangea.model.item.Rarity.Purple || g.rarity == pangea.model.item.Rarity.Blue ||
                                  g.rarity == pangea.model.item.Rarity.Orange)) &&
      assertTrue(purple > 0.0) && assertTrue(blue > 0.0) &&
      // синих много больше: фиолетовых — 5% вещей набора, остальное синим
      assertTrue(blue > purple * 2)
    },

    test("большая руна падает со всех минибоссов: на первом броске у элементалей 8,3%, у Джо 6,7%, у волка 3,3% — половину бросков берёт ингредиент") {
      import pangea.model.monster.MiniBoss
      import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
      // Первый бросок с босса — по полной шкале: ингредиент там половина, а
      // прочее делит другую половину по своим долям.
      def rateOf(boss: MiniBoss, floor: Long = 1L): (Double, List[Item]) = {
        val drops  = (1L to 4000L).toList.flatMap(s => LootGenerator.rollMiniBoss(boss, 1L, 40L, Rng(s), floor)._1.headOption)
        val stones = drops.flatMap(_.itemOpt).filter(_.itemType == ItemType.RuneStone)
        (stones.size.toDouble * 100.0 / drops.size, stones)
      }
      val (fire,  fireStones) = rateOf(MiniBoss.FireElemental)        // 10 из 120
      val (stone, _)          = rateOf(MiniBoss.StoneElemental)
      val (joe,   _)          = rateOf(MiniBoss.RottenJoe)            // 10 из 150
      val (wolf,  wolfStones) = rateOf(MiniBoss.WhiteWolf, floor = 17L) // 5 из 150
      val runes = fireStones.flatMap(_.runeStone).flatMap(d => Rune.byKey(d.runeKey)).toSet
      assertTrue(LootGenerator.BossIngredientSharePct == 50L) &&
      assertTrue(LootGenerator.ElementalRuneWeight == 10L && LootGenerator.ElementalChiselWeight == 10L) &&
      assertTrue(LootGenerator.ElementalGearWeight == 40L && LootGenerator.ElementalOthers == 60L) &&
      assertTrue(LootGenerator.WolfFangUntil == 20L && LootGenerator.WolfOthers - LootGenerator.WolfChiselUntil == 5L) &&
      assertTrue(LootGenerator.JoeGemUntil == 20L) &&
      assertTrue(LootGenerator.JoeDoubloonsUntil - LootGenerator.JoeGemUntil == 20L) &&
      assertTrue(LootGenerator.JoeRuneUntil - LootGenerator.JoeDoubloonsUntil == 10L) &&
      assertTrue(fire > 7.0 && fire < 9.7 && stone > 7.0 && stone < 9.7) &&
      assertTrue(joe > 5.5 && joe < 7.9) &&
      assertTrue(wolf > 2.5 && wolf < 4.3) &&
      assertTrue((fireStones ++ wolfStones).forall(_.runeStone.exists(_.size == RuneStoneSize.Big))) &&
      assertTrue(runes.size > 20 && runes.subsetOf(RuneStone.all.toSet))
    },

    test("уровень сетовой вещи — уровень героя ±1, а не уровень босса") {
      val gear = (1L to 300L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, 2L, 40L, Rng(s))._1)
        .flatMap(_.itemOpt).filter(i => i.itemType != ItemType.Material && i.itemType != ItemType.RuneStone).toList
      assertTrue(gear.nonEmpty) && assertTrue(gear.forall(i => i.lvl >= 39L && i.lvl <= 41L))
    },

    test("с Белого волка: клык (этаж встречи, коэффициент 6 × BossLvL), вещи «Охотника» обеих редкостей и шкура — не больше одной за бой") {
      import pangea.model.monster.MiniBoss
      val rolls = (1L to 400L).map(s => LootGenerator.rollMiniBoss(MiniBoss.WhiteWolf, 3L, 40L, Rng(s), floorLvl = 17L)._1)
      val items = rolls.flatten.flatMap(_.itemOpt)
      val fangs = items.filter(_.itemType == ItemType.Trophy)
      val hides = items.filter(_.material.contains(pangea.model.item.MaterialKind.WhiteWolfHide))
      val gear  = items.filter(_.set.contains(pangea.model.item.ItemSet.Hunter))
      assertTrue(rolls.forall(r => r.size == 3 || r.size == 4)) &&
      assertTrue(fangs.nonEmpty && hides.nonEmpty && gear.nonEmpty) &&
      assertTrue(fangs.forall(f => f.lvl == 17L && f.name == TrophyKind.Fang.displayName)) &&
      assertTrue(fangs.forall(_.details match {
        case t: ItemDetails.Trophy => t.kind == TrophyKind.Fang && t.race == pangea.model.monster.Race.Animal.entryName && t.coefValue == 18.0
        case _                     => false
      })) &&
      assertTrue(gear.exists(_.rarity == pangea.model.item.Rarity.Purple) && gear.exists(_.rarity == pangea.model.item.Rarity.Blue)) &&
      assertTrue(gear.forall(g => g.rarity == pangea.model.item.Rarity.Purple || g.rarity == pangea.model.item.Rarity.Blue ||
                                  g.rarity == pangea.model.item.Rarity.Orange)) &&
      assertTrue(gear.forall(_.name.endsWith(pangea.model.item.ItemSet.Hunter.title))) &&
      // с одного волка шкура — не больше одной, но со следующего может выпасть снова
      assertTrue(rolls.forall(_.count(LootGenerator.isIngredient(MiniBoss.WhiteWolf, _)) <= 1)) &&
      assertTrue(rolls.count(_.exists(LootGenerator.isIngredient(MiniBoss.WhiteWolf, _))) > 1)
    },

    test("свой ингредиент с любого минибосса — не больше одного за бой, со следующего — снова") {
      import pangea.model.monster.MiniBoss
      // BossLvL 10: бросков с босса десять-одиннадцать (у короля и Некроманта —
      // пять), и без предела ингредиент падал бы с одного босса по нескольку.
      val counts = MiniBoss.values.toList.map { boss =>
        boss -> (1L to 300L).toList.map(s =>
          LootGenerator.rollMiniBoss(boss, 10L, 40L, Rng(s), floorLvl = 20L)._1.count(LootGenerator.isIngredient(boss, _)))
      }
      assertTrue(counts.forall { case (_, ns) => ns.forall(_ <= 1) }) &&
      // и падает он почти с каждого: пока он не выпал, он — половина бросков
      assertTrue(counts.forall { case (_, ns) => ns.count(_ == 1) > ns.size * 9 / 10 })
    },

    test("выпавший ингредиент отдаёт свою долю прочей добыче: вещей набора с элементаля — около 60% бросков") {
      // Первый бросок идёт по полной шкале — вещи набора там 40 долей из 120, —
      // а после ингредиента уже 40 из 60.
      val drops = (1L to 400L).toList.flatMap(s =>
        LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, 10L, 40L, Rng(s))._1)
      val gearShare = drops.flatMap(_.itemOpt).count(_.set.isDefined).toDouble / drops.size
      assertTrue(gearShare > 0.55 && gearShare < 0.70)
    },

    test("с каменного падают его магические камни и вещи «Каменного стража»") {
      val items = (1L to 300L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.StoneElemental, 2L, 40L, Rng(s))._1)
        .flatMap(_.itemOpt).toList
      val (materials, rest) = items.partition(_.itemType == ItemType.Material)
      val gear = rest.filterNot(_.itemType == ItemType.RuneStone)
      assertTrue(materials.nonEmpty) && assertTrue(gear.nonEmpty) &&
      assertTrue(materials.forall(m => m.material.contains(pangea.model.item.MaterialKind.MagicStone) ||
                                       m.material.contains(pangea.model.item.MaterialKind.RuneChisel))) &&
      assertTrue(gear.forall(g => g.rarity == pangea.model.item.Rarity.Purple || g.rarity == pangea.model.item.Rarity.Blue ||
                                  g.rarity == pangea.model.item.Rarity.Orange)) &&
      assertTrue(gear.forall(_.set.contains(pangea.model.item.ItemSet.StoneGuard))) &&
      assertTrue(gear.forall(_.name.endsWith(pangea.model.item.ItemSet.StoneGuard.title)))
    },

    test("уровень вещи не выходит за границы игры: ни нулевого, ни 151-го") {
      def gearAt(heroLvl: Long) = (1L to 300L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, 2L, heroLvl, Rng(s))._1)
        .flatMap(_.itemOpt).filter(i => i.itemType != ItemType.Material && i.itemType != ItemType.RuneStone).toList
      val lowest  = gearAt(1L)   // разброс −1 увёл бы вещь в нулевой уровень
      val highest = gearAt(150L) // разброс +1 увёл бы её в 151-й
      assertTrue(lowest.nonEmpty) && assertTrue(highest.nonEmpty) &&
      assertTrue(lowest.forall(i => i.lvl >= 1L && i.lvl <= 2L)) &&
      assertTrue(highest.forall(i => i.lvl >= 149L && i.lvl <= 150L))
    },

    test("ингредиент — половина первого броска с каждого минибосса; у элементаля вещи набора — треть, руна и резец — по двенадцатой части") {
      import pangea.model.monster.MiniBoss
      // Смотрим первый бросок с каждого: он всегда идёт по полной шкале. Дальше
      // выпавший ингредиент из шкалы выбывает (см. тест выше).
      def firsts(boss: MiniBoss): List[LootDrop] =
        (1L to 1500L).toList.flatMap(s => LootGenerator.rollMiniBoss(boss, 2L, 40L, Rng(s), floorLvl = 20L)._1.headOption)
      val items = firsts(MiniBoss.FireElemental).flatMap(_.itemOpt)
      def share(p: pangea.model.item.Item => Boolean): Double = items.count(p).toDouble / items.size
      val ingredientShare = share(_.material.contains(pangea.model.item.MaterialKind.EverburningIron))
      val chiselShare     = share(_.material.contains(pangea.model.item.MaterialKind.RuneChisel))
      val gearShare       = share(_.set.isDefined)
      // у всех шести — около половины
      val ingredientShares = MiniBoss.values.toList.map { boss =>
        val f = firsts(boss)
        f.count(LootGenerator.isIngredient(boss, _)).toDouble / f.size
      }
      assertTrue(ingredientShare > 0.45 && ingredientShare < 0.55) &&
      assertTrue(chiselShare > 0.06 && chiselShare < 0.11) &&
      assertTrue(gearShare > 0.29 && gearShare < 0.38) &&
      assertTrue(ingredientShares.forall(s => s > 0.45 && s < 0.55))
    },

    test("резец рунописца падает с каждого минибосса — около десятой части бросков") {
      val shares = pangea.model.monster.MiniBoss.values.toList.map { boss =>
        val drops = (1L to 1500L).toList.flatMap(s => LootGenerator.rollMiniBoss(boss, 4L, 30L, Rng(s), floorLvl = 20L)._1)
        val chisels = drops.flatMap(_.itemOpt).count(_.material.contains(pangea.model.item.MaterialKind.RuneChisel))
        // у Некроманта череп с дублонами — один бросок, но две записи
        val rolls = drops.size - drops.count(_.isInstanceOf[LootDrop.Doubloons])
        boss -> chisels.toDouble / rolls
      }
      val off = shares.filterNot { case (_, s) => s > 0.06 && s < 0.16 }
      assertTrue(off.isEmpty)
    },

    // ── Пассивки лута ─────────────────────────────────────────────────────────
    test("без пассивок доп. дропов нет и RNG не тратится") {
      val (drops, r) = LootGenerator.rollPassiveDrops(
        trophyChancePct = 0L, silverChancePct = 0L, Rarity.Common, Race.Human, 10L, Rng(7L))
      assertTrue(drops.isEmpty) && assertTrue(r == Rng(7L))
    },

    test("Таксидермист иногда даёт лишний трофей этой расы (~10%)") {
      val trophies = (1L to 1000L).iterator.flatMap { s =>
        LootGenerator.rollPassiveDrops(trophyChancePct = 10L, silverChancePct = 0L, Rarity.Rare, Race.Orc, 12L, Rng(s))._1
      }.collect { case LootDrop.Trophy(i) => i }.toList
      assertTrue(trophies.nonEmpty) &&
        assertTrue(trophies.forall(i => i.itemType == ItemType.Trophy && raceOf(i).contains(Race.Orc.entryName))) &&
        assertTrue(trophies.forall(_.lvl == 12L))
    },

    test("Таксидермист срабатывает примерно в 10% случаев") {
      val hits = (1L to 2000L).count { s =>
        LootGenerator.rollPassiveDrops(trophyChancePct = 10L, silverChancePct = 0L, Rarity.Rare, Race.Orc, 12L, Rng(s))._1.nonEmpty
      }
      assertTrue(hits >= 140 && hits <= 260) // ~200 из 2000
    },

    test("Ювелир иногда даёт отдельную (не груду) порцию серебра ~lvl×4±20%") {
      val silvers = (1L to 1000L).iterator.flatMap { s =>
        LootGenerator.rollPassiveDrops(trophyChancePct = 0L, silverChancePct = 10L, Rarity.Common, Race.Human, 10L, Rng(s))._1
      }.collect { case LootDrop.Silver(a, pile) => (a, pile) }.toList
      assertTrue(silvers.nonEmpty) &&
        assertTrue(silvers.forall { case (a, pile) => a >= 32L && a <= 48L && !pile })
    },

    test("обе пассивки вместе могут дать и трофей, и серебро за один вызов") {
      val both = (1L to 2000L).iterator.map { s =>
        LootGenerator.rollPassiveDrops(trophyChancePct = 10L, silverChancePct = 10L, Rarity.Rare, Race.Orc, 12L, Rng(s))._1
      }.exists { ds =>
        ds.exists { case LootDrop.Trophy(_) => true; case _ => false } &&
        ds.exists { case LootDrop.Silver(_, _) => true; case _ => false }
      }
      assertTrue(both)
    }
  )
}
