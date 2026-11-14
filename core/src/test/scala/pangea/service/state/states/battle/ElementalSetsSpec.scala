package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.{CubeCraft, MaterialGenerator}
import pangea.generator.loot.{LootGenerator, TreasureHuntGenerator}
import pangea.generator.loot.LootGenerator.LootDrop
import pangea.model.battle.{Element, SoloPveBattle}
import pangea.model.hero.{Achievement, Equipment, Hero, HeroSets}
import pangea.model.item.{Gem, GemKind, Grip, Item, ItemDetails, ItemSet, ItemType, MaterialKind, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity => MobRarity}
import pangea.model.skill.Skill
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Наборы элементалей: «Гроза» у Громового и «Стужа» у Ледяного. Заодно — новое
  * выжигание энергии молнией: половина урона, что удар снял с HP, а с умения —
  * 20% его урона по HP. */
object ElementalSetsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  /** «Атака» по месту `target`: при двух врагах рядом без цели бой спросит, в кого. */
  private def aimed(target: Int): UserAction =
    UserAction("", Some(s"""{"action":"Attack","target":"$target"}"""))

  /** Броски: удар героя в цель; удар моба мимо. */
  private val HeroHit = 60
  private val Miss    = 1

  private val MobHp = 10000000L

  private val setSlots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
    ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet, ItemType.Ring, ItemType.Ring, ItemType.Belt)

  private def piece(id: Long, t: ItemType, set: ItemSet, armor: Long): Item =
    Item(id, "Вещь", 1L, ItemRarity.Blue, t, attack = 0, accuracy = 0, energy = 0, armor = armor, defence = 0,
      evasion = 0, set = Some(set))

  /** Одноручный меч с камнем `gem` и умением `skill`; на двенадцатой вещи набора
    * он и сам в наборе. */
  private def sword(gem: Option[GemKind], set: Option[ItemSet], skill: Option[Skill] = None): Item =
    Item(50L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 500, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = skill.fold[ItemDetails](ItemDetails.Plain)(ItemDetails.Weapon(_)),
      grip = Some(Grip.OneHanded), sockets = gem.map(g => Some(Gem(g, 1))).toList, set = set)

  /** `n` вещей набора `set` по сетовым местам (двенадцатая — меч); у каждой
    * `armor` брони. */
  private def gear(set: Option[ItemSet], n: Int, gem: Option[GemKind], armor: Long = 0L): Equipment = {
    val worn = set.toList.flatMap(s => setSlots.take(n.min(setSlots.size)).zipWithIndex.map { case (t, i) =>
      i -> piece(200L + i, t, s, armor) })
    val body = worn.foldLeft(TestFixtures.emptyEquipment) { case (eq, (i, it)) =>
      i match {
        case 0  => eq.copy(helmet = it)
        case 1  => eq.copy(shoulderPads = it)
        case 2  => eq.copy(chestPlate = it)
        case 3  => eq.copy(bracelets = it)
        case 4  => eq.copy(gloves = it)
        case 5  => eq.copy(pants = it)
        case 6  => eq.copy(boots = it)
        case 7  => eq.copy(amulet = it)
        case 8  => eq.copy(firstRing = it)
        case 9  => eq.copy(secondRing = it)
        case _  => eq.copy(belt = it)
      }
    }
    body.copy(weapon = sword(gem, set.filter(_ => n >= 12)))
  }

  /** Герой 15-го уровня: бьёт без промаха, энергии — под потолок. */
  private def hero(eq: Equipment, armor: Long = 0L): Hero = {
    val base = TestFixtures.hero(userId)
    val h = base.copy(
      lvl        = 15L,
      fightStats = FightStats(atk = 10, hp = 500000L, armor = armor, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = base.baseStats.copy(str = 1, vit = 50000, int = 100),
      equipment  = eq)
    h.copy(fightStats = h.fightStats.copy(energy = h.maxEnergy(0L)))
  }

  /** Зверь: ни стихии, ни лечения, удар — в единицу. Энергии — с запасом. */
  private def mob(armor: Long = 0L): Monster =
    Monster(0L, 10L, Race.Animal, MobRarity.Common,
      FightStats(atk = 1, hp = MobHp, armor = armor, defence = 0, evasion = 0, accuracy = 1, energy = 1000000L))

  /** Бой с зверем в паре; `side` — места его сородичей в строю. */
  private def fight(h: Hero, m: Monster, side: List[Int] = Nil): SoloPveBattle =
    if (side.isEmpty) SoloPveBattle.from(m, h).copy(noKin = true)
    else {
      val b = SoloPveBattle.fromGroup(List.fill(side.size + 1)(m), h, Nil)
      b.copy(noKin = true, group = b.group.copy(places = side))
    }

  private def makeState(h: Hero, battle: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  /** Удар героя: герой после хода, бой после хода и весь лог. */
  private def attack(h: Hero, battle: SoloPveBattle, procRoll: Int) =
    for {
      t <- makeState(h, battle)
      (state, dao, r) = t
      _       <- TestRandom.feedInts(HeroHit, procRoll, Miss, Miss, Miss) *> TestRandom.feedLongs(100L, 100L, 100L)
      _       <- state.action(testUser, aimed(battle.group.heroPos), r)
      updated <- dao.getHeroByUserId(userId).map(_.get)
      after   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      screens <- r.sentScreens
    } yield (updated, after.get, screens.map(_.text).mkString("\n"))

  /** Умение меча («Skill_50»): у него броска на попадание нет, сразу проки. */
  private def useSkill(h: Hero, battle: SoloPveBattle, procRoll: Int) =
    for {
      t <- makeState(h, battle)
      (state, dao, r) = t
      _       <- TestRandom.feedInts(procRoll, Miss, Miss, Miss) *> TestRandom.feedLongs(100L, 100L, 100L)
      _       <- state.action(testUser, tap("Skill_50"), r)
      after   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      screens <- r.sentScreens
    } yield (after.get, screens.map(_.text).mkString("\n"))

  /** Урон удара героя по цели — из строки «Вы наносите N урона». */
  private def dealtOf(log: String): Long =
    """Вы наносите (\d+) урона""".r.findFirstMatchIn(log).map(_.group(1).toLong).getOrElse(-1L)

  override def spec = suite("Наборы элементалей: «Гроза» и «Стужа»")(

    // ── Молния ─────────────────────────────────────────────────────────────────
    test("прок молнии выжигает врагу половину урона, что удар снял с его HP") {
      val h = hero(gear(None, 0, Some(GemKind.Topaz)))
      for {
        r <- attack(h, fight(h, mob()), procRoll = 1)
        (_, after, log) = r
        dealt = MobHp - after.monsterCurrentHp
      } yield assertTrue(dealt > 0L && dealt == dealtOf(log)) &&
              assertTrue(log.contains(s"⚡ Молния → ") && log.contains(s": −${dealt / 2} энергии.")) &&
              assertTrue(Element.LightningBurnOfHpPct == 50L && Element.lightningBurn(dealt) == dealt / 2)
    },

    test("прок молнии с умения выжигает 20% урона умения по HP, а не половину") {
      val h = hero(gear(None, 0, Some(GemKind.Topaz)).copy(weapon = sword(Some(GemKind.Topaz), None, Some(Skill.CunningStrike))))
      for {
        r <- useSkill(h, fight(h, mob()), procRoll = 1)
        (after, log) = r
        dealt = MobHp - after.monsterCurrentHp
      } yield assertTrue(dealt > 0L && log.contains(s"«Хитрый удар» → ") && log.contains(s": $dealt урона.")) &&
              assertTrue(log.contains(s": −${dealt * 20 / 100} энергии.") && !log.contains(s": −${dealt / 2} энергии.")) &&
              assertTrue(Element.LightningBurnOfSkillPct == 20L && Element.skillLightningBurn(dealt) == dealt * 20 / 100)
    },

    // ── Гроза ──────────────────────────────────────────────────────────────────
    test("«Гроза»: пороги 2–12, двойка — +5% к энергии, и тексты с её числами") {
      val storm = HeroSets(Map(ItemSet.Storm -> 12))
      val pierce = ItemSet.Storm.bonusAt(4).map(_.text).getOrElse("")
      assertTrue(ItemSet.Storm.bonuses.map(_.pieces) == List(2, 4, 6, 8, 10, 12)) &&
      assertTrue(ItemSet.Storm.label == "Гроза" && ItemSet.Storm.title == "Грозы") &&
      assertTrue(storm.energyBonusPct == 5L && storm.lightningPierceMult == 2L && storm.burnHurts &&
        storm.lightningChanceBonusPct == 30L && storm.chainLightning) &&
      assertTrue(pierce.contains("40%") && pierce.contains("20%")) &&
      assertTrue(HeroSets(Map(ItemSet.Storm -> 3)).lightningPierceMult == 1L && !HeroSets(Map(ItemSet.Storm -> 11)).chainLightning)
    },

    test("«Гроза» (4): молния прошивает броню вдвое — в HP уходит 40% урона по броне, а не 20%") {
      val plain = hero(gear(None, 0, Some(GemKind.Topaz)))
      val storm = hero(gear(Some(ItemSet.Storm), 4, Some(GemKind.Topaz)))
      val armor = 9000000L
      for {
        a <- attack(plain, fight(plain, mob(armor)), procRoll = 99)
        b <- attack(storm, fight(storm, mob(armor)), procRoll = 99)
        armorA = armor - a._2.monsterCurrentArmor
        armorB = armor - b._2.monsterCurrentArmor
        hpA    = MobHp - a._2.monsterCurrentHp
        hpB    = MobHp - b._2.monsterCurrentHp
      } yield assertTrue(armorA > 0L && armorA == armorB) &&
              assertTrue(hpA == (armorA * 0.2).toLong && hpB == (armorB * 0.2 * 2).toLong && hpB > hpA)
    },

    test("«Гроза» (6): энергия, выжженная молнией, бьёт врага и по HP — столько же урона") {
      val h = hero(gear(Some(ItemSet.Storm), 6, Some(GemKind.Topaz)))
      for {
        r <- attack(h, fight(h, mob()), procRoll = 1)
        (_, after, log) = r
        dealt = dealtOf(log)
        burnt = Element.lightningBurn(dealt)
      } yield assertTrue(dealt > 0L && burnt > 0L) &&
              assertTrue(log.contains(s": −$burnt энергии и $burnt урона.")) &&
              assertTrue(MobHp - after.monsterCurrentHp == dealt + burnt)
    },

    test("«Гроза» (10): шанс прока молнии — 60% вместо 30%") {
      val plain = hero(gear(None, 0, Some(GemKind.Topaz)))
      val storm = hero(gear(Some(ItemSet.Storm), 10, Some(GemKind.Topaz)))
      for {
        a <- attack(plain, fight(plain, mob()), procRoll = 50)
        b <- attack(storm, fight(storm, mob()), procRoll = 50)
      } yield assertTrue(!a._3.contains("⚡ Молния → ") && b._3.contains("⚡ Молния → "))
    },

    test("«Гроза» (12): прок молнии перескакивает на соседа цели — 20% урона удара и выжигание энергии") {
      val h     = hero(gear(Some(ItemSet.Storm), 12, Some(GemKind.Topaz)))
      val plain = hero(gear(Some(ItemSet.Storm), 11, Some(GemKind.Topaz)))
      for {
        r <- attack(h, fight(h, mob(), side = List(2)), procRoll = 1)
        (_, after, log) = r
        dealt = dealtOf(log)
        chain = dealt * ItemSet.Storm.ChainPct / 100L
        p <- attack(plain, fight(plain, mob(), side = List(2)), procRoll = 1)
      } yield assertTrue(log.contains(s"⚡ Цепная молния задевает") && log.contains(s"— $chain урона.")) &&
              assertTrue(after.monsterAt(2).exists(_.currentHp < MobHp)) &&
              // соседу молния тоже выжигает энергию
              assertTrue(log.linesIterator.count(_.contains("⚡ Молния → ")) == 2) &&
              // флаг раунда снят вместе с раундом
              assertTrue(!after.chainUsedThisRound) &&
              // без двенадцатой вещи перескока нет
              assertTrue(!p._3.contains("Цепная молния") && p._2.monsterAt(2).exists(_.currentHp == MobHp))
    },

    test("«Гроза» (12): не чаще раза за раунд — перескок в этом раунде уже был, и второго нет") {
      val h = hero(gear(Some(ItemSet.Storm), 12, Some(GemKind.Topaz)))
      for {
        r <- attack(h, fight(h, mob(), side = List(2)).copy(chainUsedThisRound = true, midRound = true), procRoll = 1)
        (_, after, log) = r
      } yield assertTrue(log.contains("⚡ Молния → ") && !log.contains("Цепная молния")) &&
              assertTrue(after.monsterAt(2).exists(_.currentHp == MobHp))
    },

    // ── Стужа ──────────────────────────────────────────────────────────────────
    test("«Стужа»: пороги 2–12, двойка — +5% к броне, и тексты с её числами") {
      val frost = HeroSets(Map(ItemSet.Frost -> 12))
      val cut   = ItemSet.Frost.bonusAt(4).map(_.text).getOrElse("")
      assertTrue(ItemSet.Frost.bonuses.map(_.pieces) == List(2, 4, 6, 8, 10, 12)) &&
      assertTrue(ItemSet.Frost.label == "Стужа" && ItemSet.Frost.title == "Стужи") &&
      assertTrue(frost.armorBonusPct == 5L && frost.coldCutPct == 15 && frost.coldArmorRestorePct == 5L &&
        frost.coldChanceBonusPct == 30L && frost.coldRoots) &&
      assertTrue(cut.contains("15%") && cut.contains("10%")) &&
      assertTrue(HeroSets(Map.empty[ItemSet, Int]).coldCutPct == Element.Cold.DefenceReductionCut)
    },

    test("«Стужа» (4): прок холода режет защиту врага на 15%, а не на 10%") {
      val plain = hero(gear(None, 0, Some(GemKind.Sapphire)))
      val frost = hero(gear(Some(ItemSet.Frost), 4, Some(GemKind.Sapphire)))
      for {
        a <- attack(plain, fight(plain, mob()), procRoll = 1)
        b <- attack(frost, fight(frost, mob()), procRoll = 1)
      } yield assertTrue(a._2.effects.monsterColdDefenceCut == 10 && a._3.contains("❄️ Холод: защита врага −10%.")) &&
              assertTrue(b._2.effects.monsterColdDefenceCut == 15 && b._3.contains("❄️ Холод: защита врага −15%."))
    },

    test("«Стужа» (6): ледяная корка — прок холода возвращает 5% брони, не выше потолка") {
      // шесть вещей по 1000 брони и +5% от двойки: потолок 6300, прок — +315
      val h = hero(gear(Some(ItemSet.Frost), 6, Some(GemKind.Sapphire), armor = 1000L))
      val full = h.copy(fightStats = h.fightStats.copy(armor = h.maxArmor))
      for {
        r <- attack(h, fight(h, mob()), procRoll = 1)
        f <- attack(full, fight(full, mob()), procRoll = 1)
      } yield assertTrue(h.maxArmor == 6300L && r._3.contains("❄️ Ледяная корка: +315 брони.")) &&
              assertTrue(!f._3.contains("Ледяная корка"))
    },

    test("«Стужа» (12): каждый прок холода обездвиживает врага на раунд") {
      val h     = hero(gear(Some(ItemSet.Frost), 12, Some(GemKind.Sapphire)))
      val plain = hero(gear(Some(ItemSet.Frost), 11, Some(GemKind.Sapphire)))
      for {
        r <- attack(h, fight(h, mob()), procRoll = 1)
        p <- attack(plain, fight(plain, mob()), procRoll = 1)
      } yield assertTrue(r._3.contains("❄️ Холод: защита врага −15%, раунд не сойдёт с места.")) &&
              // тик хода моба съел запас, держится ещё раунд
              assertTrue(r._2.effects.monsterRooted) &&
              assertTrue(!p._2.effects.monsterRooted && !p._3.contains("раунд не сойдёт с места"))
    },

    // ── Элементали и их добыча ─────────────────────────────────────────────────
    test("Громовой роняет вещи «Грозы», Ледяной — «Стужи»; сердце грозы и вечный лёд — в деле и в кладах") {
      def gearOf(boss: MiniBoss) = (1L to 200L).flatMap(s => LootGenerator.rollMiniBoss(boss, 3L, 15L, Rng(s))._1)
        .collect { case LootDrop.Gear(item) if item.material.isEmpty => item }
      val storm = gearOf(MiniBoss.ThunderElemental)
      val frost = gearOf(MiniBoss.IceElemental)
      assertTrue(MiniBoss.ThunderElemental.set.contains(ItemSet.Storm) && MiniBoss.IceElemental.set.contains(ItemSet.Frost)) &&
      assertTrue(storm.exists(_.set.contains(ItemSet.Storm)) && frost.exists(_.set.contains(ItemSet.Frost))) &&
      assertTrue(!MaterialKind.StormHeart.notYetUsed && !MaterialKind.EternalIce.notYetUsed) &&
      assertTrue(TreasureHuntGenerator.bossIngredients.contains(MaterialKind.StormHeart) &&
        TreasureHuntGenerator.bossIngredients.contains(MaterialKind.EternalIce))
    },

    test("куб: сердце грозы с вещью — та же вещь «Грозы», вечный лёд — «Стужи»; три вещи набора — обратно в ингредиент") {
      val helm = Item(1L, "🔵 Прочный Шлем Дворянина", 12L, ItemRarity.Blue, ItemType.Helmet,
        attack = 0, accuracy = 0, energy = 0, armor = 30, defence = 3, evasion = 0)
      def infuse(k: MaterialKind) = CubeCraft.craft(List(helm, MaterialGenerator.item(k).copy(id = 2L)), charges = 5, Rng(1L)).items
      def salvage(s: ItemSet) = CubeCraft.craft((1 to 3).map(i => helm.copy(id = 10L + i, set = Some(s))).toList,
        charges = 5, Rng(1L)).items.flatMap(_.material)
      val storm = infuse(MaterialKind.StormHeart)
      val frost = infuse(MaterialKind.EternalIce)
      assertTrue(storm.size == 1 && storm.forall(i => i.set.contains(ItemSet.Storm) && i.armor == 30 && i.name.endsWith("Грозы"))) &&
      assertTrue(frost.size == 1 && frost.forall(i => i.set.contains(ItemSet.Frost) && i.armor == 30 && i.name.endsWith("Стужи"))) &&
      assertTrue(salvage(ItemSet.Storm) == List(MaterialKind.StormHeart) && salvage(ItemSet.Frost) == List(MaterialKind.EternalIce))
    },

    test("собранный набор — достижение: «Ловец молний» (+3% к энергии) и «Холодная кровь» (+3% к броне)") {
      assertTrue(Achievement.forSet(ItemSet.Storm).contains(Achievement.StormSet) &&
        Achievement.forSet(ItemSet.Frost).contains(Achievement.FrostSet)) &&
      assertTrue(Achievement.StormSet.title == "Ловец молний" && Achievement.StormSet.bonusLine == "+3% к энергии") &&
      assertTrue(Achievement.FrostSet.title == "Холодная кровь" && Achievement.FrostSet.bonusLine == "+3% к броне")
    },

    test("бой, записанный до цепной молнии, читается: поля нет — перескока в раунде не было") {
      val h   = hero(gear(None, 0, None))
      val old = fight(h, mob()).copy(chainUsedThisRound = true).asJson.mapObject(_.remove("chainUsedThisRound"))
      assertTrue(old.as[SoloPveBattle].exists(!_.chainUsedThisRound))
    }
  )
}
