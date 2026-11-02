package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, Burn, SoloPveBattle}
import pangea.model.hero.{Equipment, Hero, HeroSets}
import pangea.model.item.{Item, ItemSet, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.skill.Skill
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Малые расовые наборы «Мародёр» (гоблины) и «Преисподняя» (демоны): шаги
  * 6/9 и скрытый 12 — только своей расе. */
object RaceSetGoblinDemonSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** Герой попадает, а дальше все броски проходят. */
  private val hitAndAllPass = TestRandom.feedInts(99 :: List.fill(40)(1): _*)

  private val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
    ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet,
    ItemType.Ring, ItemType.Ring, ItemType.Belt, ItemType.Weapon)

  /** `n` вещей набора `set` — двенадцатая встаёт в руку оружием. */
  private def wearing(set: ItemSet, n: Int): Equipment =
    slots.take(n).zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (eq, (t, i)) =>
      val it = Item(700L + i, "Предмет", 1L, ItemRarity.Blue, t,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, set = Some(set))
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
        case 10 => eq.copy(belt = it)
        case _  => eq.copy(weapon = it)
      }
    }

  private def hero(race: Race, set: ItemSet, pieces: Int, atk: Long = 500L): Hero = TestFixtures.hero(userId).copy(
    lvl        = 10L,
    race       = race,
    fightStats = FightStats(atk = atk, hp = 100000, armor = 0, defence = 0, evasion = 9999, accuracy = 9999, energy = 0),
    baseStats  = BaseStats(agi = 10, vit = 10, str = 1, int = 200),
    equipment  = wearing(set, pieces))

  /** Моб, которого герой не убьёт с одного удара и который сам его не убьёт. */
  private val tough = SoloPveBattle(
    monsterLvl          = 10L,
    monsterRace         = Race.Orc.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 100),
    monsterCurrentHp    = 100000L,
    monsterCurrentArmor = 0L,
    monsterCurrentEnergy = 100L)

  /** Моб, который падает с одного удара. */
  private val weak = tough.copy(monsterStats = tough.monsterStats.copy(hp = 10), monsterCurrentHp = 10L)

  private def run(h: Hero, b: SoloPveBattle, rolls: Task[Unit] = hitAndAllPass): Task[(StateType, Hero, String)] =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeActiveBattle(userId, b.asJson)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
      _   <- TestRandom.setSeed(42L) *> rolls
      res <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
      after <- dao.getHeroByUserId(userId).map(_.get)
      log <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (res, after, log)

  private val Hit = "Вы наносите (\\d+) урона".r
  private def dealt(log: String): Long = Hit.findFirstMatchIn(log).map(_.group(1).toLong).getOrElse(-1L)

  override def spec = suite("«Мародёр» и «Преисподняя»")(

    test("«Мародёр» 6: удар по HP с шансом выжигает врагу 30% энергии; без порога — ни строки, ни броска") {
      for {
        six  <- run(hero(Race.Human, ItemSet.Marauder, 6), tough)
        five <- run(hero(Race.Human, ItemSet.Marauder, 5), tough)
      } yield assertTrue(six._3.contains("Грязный приём") && six._3.contains("теряет 30 энергии")) &&
              assertTrue(!five._3.contains("Грязный приём")) &&
              assertTrue(ItemSet.Marauder.EnergyBurnChancePct == 20L && ItemSet.Marauder.EnergyBurnPct == 30L)
    },

    test("«Мародёр» 9: +20% серебра с боёв — поверх благословения") {
      assertTrue(HeroSets(Map(ItemSet.Marauder -> 9)).battleSilverBonusPct == 20L) &&
      assertTrue(HeroSets(Map(ItemSet.Marauder -> 8)).battleSilverBonusPct == 0L) &&
      assertTrue(BattleState.silverScalePct(blessed = false, 20L) == 120L) &&
      assertTrue(BattleState.silverScalePct(blessed = true, 20L) == 120L + BattleState.BlessingBonusPct) &&
      assertTrue(BattleState.silverScalePct(blessed = false, 0L) == 100L)
    },

    test("«Мародёр» 12 — только гоблину: лишняя вещь с добитых героем, окружают вдвое реже") {
      val goblin = HeroSets(Map(ItemSet.Marauder -> 12), Some(Race.Goblin))
      val human  = HeroSets(Map(ItemSet.Marauder -> 12), Some(Race.Human))
      val ally   = BattleAlly(AllyKind.Rat, 2, 10L, 0L, 0L, FightStats(1, 10, 0, 0, 0, 1, 0), 1L, kills = 2)
      val withAlly = tough.copy(group = tough.group.copy(allies = List(ally)))
      val (all, _)    = BattleState.pocketGear(100L, 3, Rarity.Common, 10L, Rng(1L))
      val (none, rng) = BattleState.pocketGear(0L, 3, Rarity.Common, 10L, Rng(1L))
      assertTrue(goblin.extraGearChancePct == 10L && goblin.surroundCutPct == 50L) &&
      assertTrue(human.extraGearChancePct == 0L && human.surroundCutPct == 0L) &&
      assertTrue(all.size == 3 && none.isEmpty && rng == Rng(1L)) &&
      // пятерых положили, двоих из них — крыса: герою в счёт трое
      assertTrue(BattleState.killedByHero(5, withAlly) == 3 && BattleState.killedByHero(1, withAlly) == 0)
    },

    test("«Преисподняя» 6: шанс поджечь +10, по горящему врагу удар на 10% сильнее") {
      val plainHit   = TestRandom.feedInts(99) *> TestRandom.feedLongs(100L)
      val burning    = tough.copy(effects = tough.effects.copy(monsterBurn = Some(Burn(2))))
      val both       = HeroSets(Map(ItemSet.Underworld -> 6, ItemSet.WildFlame -> 10))
      for {
        plain <- run(hero(Race.Human, ItemSet.Underworld, 6), tough, plainHit)
        hot   <- run(hero(Race.Human, ItemSet.Underworld, 6), burning, plainHit)
        bare  <- run(hero(Race.Human, ItemSet.Underworld, 5), burning, plainHit)
      } yield assertTrue(HeroSets(Map(ItemSet.Underworld -> 6)).igniteChanceBonusPct == 10L) &&
              assertTrue(both.igniteChanceBonusPct == 40L) &&
              // оружия в руках нет — удар вполсилы: (1×3 + 500) × 0,5 = 251, по горящему × 1,1 = 276
              assertTrue(dealt(plain._3) == 251L && dealt(hot._3) == 276L && dealt(bare._3) == 251L)
    },

    test("«Преисподняя» 9: умения на 15% дешевле, но не бесплатно") {
      val skill = Skill.values.head
      val bare  = hero(Race.Human, ItemSet.Underworld, 8)
      val cheap = hero(Race.Human, ItemSet.Underworld, 9)
      assertTrue(skill.energyCost(cheap) == (skill.energyCost(bare) * 85L / 100L).max(1L)) &&
      assertTrue(Skill.values.forall(s => s.energyCost(cheap) >= 1L))
    },

    test("«Преисподняя» 12 — только демону: добитый враг возвращает 20% энергии") {
      for {
        demon12 <- run(hero(Race.Demon, ItemSet.Underworld, 12, atk = 100000L), weak)
        demon11 <- run(hero(Race.Demon, ItemSet.Underworld, 11, atk = 100000L), weak)
        human12 <- run(hero(Race.Human, ItemSet.Underworld, 12, atk = 100000L), weak)
        human11 <- run(hero(Race.Human, ItemSet.Underworld, 11, atk = 100000L), weak)
        maxEn    = demon12._2.maxEnergy(0L)
      } yield assertTrue(demon12._1 == StateType.Loot) &&
              assertTrue(demon12._2.fightStats.energy - demon11._2.fightStats.energy == maxEn * 20L / 100L) &&
              assertTrue(demon12._3.contains("Напор")) &&
              assertTrue(human12._2.fightStats.energy == human11._2.fightStats.energy && !human12._3.contains("Напор"))
    }
  )
}
