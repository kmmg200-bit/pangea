package pangea.service.state.states.battle

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleEffects, MonsterSlot, Poison, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.monster.{BossRoundCap, MiniBoss, Monster, Rarity => MobRarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Предел минибосса за раунд: больше четверти HP и четверти брони за раунд он
  * не теряет — от чего бы то ни было (см. BossRoundCap). */
object BossRoundCapSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val wolf    = MiniBoss.WhiteWolf
  private val bossLvl = 2L
  private val stats   = wolf.stats(bossLvl)
  private val hpCap   = stats.hp * BossRoundCap.HpPct / 100L
  private val armCap  = stats.armor * BossRoundCap.ArmorPct / 100L

  private def weapon: Item =
    Item(50L, "Меч", 1L, Rarity.Blue, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  /** Герой не промахивается и переживает всё, что босс успеет ответить. */
  private def hero(atk: Long): Hero =
    TestFixtures.hero(userId).copy(
      lvl          = 8L,
      dungeonLevel = 20,
      fightStats   = FightStats(atk = atk, hp = 500000L, armor = 0, defence = 0,
                                evasion = 0, accuracy = 9999, energy = 0),
      baseStats    = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      equipment    = TestFixtures.emptyEquipment.copy(weapon = weapon)
    )

  /** Бой с Белым волком; `boss = false` — тот же зверь, но рядовой моб. */
  private def wolfBattle(h: Hero, boss: Boolean = true): SoloPveBattle = {
    val b = SoloPveBattle.from(Monster(0L, bossLvl, wolf.race, MobRarity.Legendary, stats), h)
    if (boss) b.copy(bossKind = Some(wolf.entryName), monsterCurrentEnergy = stats.energy) else b
  }

  private def strike(h: Hero, battle: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
      state     = BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content)
      _        <- TestRandom.feedInts(60, 90, 1, 1) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
      _        <- state.action(testUser, tap("Attack"), renderer)
      after    <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      screens  <- renderer.sentScreens
    } yield (after, screens.map(_.text).mkString("\n"))

  override def spec = suite("Предел минибосса за раунд")(

    // ── Сам предел ───────────────────────────────────────────────────────────

    test("четверть HP и четверть брони — порознь, что сверх, до босса не доходит") {
      val b    = wolfBattle(hero(1L))
      val hurt = b.hurtActive(stats.armor, stats.hp * 10L)
      assertTrue(BossRoundCap.HpPct == 25L && BossRoundCap.ArmorPct == 25L) &&
      assertTrue(hurt.armor == armCap && hurt.hp == hpCap) &&
      assertTrue(hurt.battle.monsterCurrentArmor == stats.armor - armCap) &&
      assertTrue(hurt.battle.monsterCurrentHp == stats.hp - hpCap) &&
      assertTrue(hurt.battle.bossTaken == hpCap && hurt.battle.bossArmorTaken == armCap) &&
      // предел встал впервые за раунд — пора сказать об этом строкой
      assertTrue(hurt.held && !hurt.absorbed)
    },

    test("предел исчерпан — следующий удар раунда не доходит вовсе, и строка о нём не повторяется") {
      val first  = wolfBattle(hero(1L)).hurtActive(0L, stats.hp).battle
      val second = first.hurtActive(0L, 100L)
      assertTrue(second.hp == 0L && second.battle.monsterCurrentHp == first.monsterCurrentHp) &&
      assertTrue(second.absorbed && !second.held)
    },

    test("в пределах предела удар доходит целиком и строки нет") {
      val hurt = wolfBattle(hero(1L)).hurtActive(0L, hpCap / 2L)
      assertTrue(hurt.hp == hpCap / 2L && !hurt.held && !hurt.absorbed) &&
      assertTrue(hurt.battle.bossTaken == hpCap / 2L)
    },

    test("в счёт идёт то, что босс потерял на деле: удар сверх остатка HP его не раздувает") {
      val dying = wolfBattle(hero(1L)).copy(monsterCurrentHp = 10L)
      val hurt  = dying.hurtActive(0L, hpCap)
      assertTrue(hurt.battle.monsterCurrentHp == 0L && hurt.battle.bossTaken == 10L) &&
      // павшему о пределе не говорят
      assertTrue(!hurt.held)
    },

    test("рядового моба предел не держит, и счёт его не трогает") {
      val hurt = wolfBattle(hero(1L), boss = false).hurtActive(stats.armor, stats.hp)
      assertTrue(hurt.battle.monsterCurrentHp == 0L && hurt.battle.monsterCurrentArmor == 0L) &&
      assertTrue(hurt.battle.bossTaken == 0L && hurt.battle.bossArmorTaken == 0L && !hurt.held)
    },

    test("новый раунд — счёт заново") {
      val spent = wolfBattle(hero(1L)).hurtActive(stats.armor, stats.hp).battle
      val fresh = spent.freshRound
      assertTrue(spent.bossTaken == hpCap && spent.bossArmorTaken == armCap) &&
      assertTrue(fresh.bossTaken == 0L && fresh.bossArmorTaken == 0L) &&
      assertTrue(fresh.hurtActive(0L, stats.hp).hp == hpCap)
    },

    test("минибосс в строю держит тот же предел, и счёт его — на бою") {
      val slot  = MonsterSlot(bossLvl, wolf.race.entryName, MobRarity.Legendary.entryName, stats,
                    stats.hp, stats.armor, marked = false, 0L, BattleEffects.empty, boss = Some(wolf.entryName))
      val plain = wolfBattle(hero(1L), boss = false)
      val lined = plain.admit(slot)
      val idx   = lined.group.others.indexWhere(_.boss.isDefined)
      val (b, hurt) = lined.hurtSlot(idx, 0L, stats.hp * 10L)
      assertTrue(idx >= 0 && hurt.hp == hpCap) &&
      assertTrue(b.group.others(idx).currentHp == stats.hp - hpCap) &&
      assertTrue(b.bossTaken == hpCap) &&
      // тот, кто в полях, — рядовой: его не тронули
      assertTrue(b.monsterCurrentHp == plain.monsterCurrentHp && b.bossKind.isEmpty)
    },

    // ── В бою ────────────────────────────────────────────────────────────────

    test("раны тоже упираются в предел: яд по боссу снимает не больше четверти HP за раунд") {
      // Удар героя — комариный, а яд такой, что снял бы с волка больше всего HP.
      val h = hero(1L)
      val b = wolfBattle(h).withEffects(BattleEffects.empty.copy(monsterPoison = Some(Poison(90))))
      for {
        t <- strike(h, b)
        (after, log) = t
      } yield assertTrue(after.exists(_.monsterCurrentHp == stats.hp - hpCap)) &&
              assertTrue(log.contains(s"Яд снимает $hpCap HP")) &&
              assertTrue(log.contains("Белый Волк держится: больше за этот раунд с него не снять"))
    },

    test("рядовому мобу тот же яд не ограничен") {
      val h = hero(1L)
      // Обычный зверь без энергии: ни подмоги, ни лечения себе — только яд.
      val b = wolfBattle(h, boss = false)
                .copy(monsterRarity = MobRarity.Common.entryName, monsterCurrentEnergy = 0L)
                .withEffects(BattleEffects.empty.copy(monsterPoison = Some(Poison(30))))
      for {
        t <- strike(h, b)
        (after, log) = t
      } yield assertTrue(after.exists(_.monsterCurrentHp == stats.hp - stats.hp * 30L / 100L)) &&
              assertTrue(!log.contains("держится"))
    },

    test("удар героя, который снёс бы босса целиком, снимает ровно четверть HP и брони") {
      val h = hero(1000000L)
      for {
        t <- strike(h, wolfBattle(h))
        (after, log) = t
      } yield assertTrue(after.exists(b => b.monsterCurrentHp == stats.hp - hpCap && b.monsterCurrentArmor == stats.armor - armCap)) &&
              // строка о пределе — одна на раунд, сколько бы ударов в него ни упёрлось
              assertTrue("держится".r.findAllMatchIn(log).size == 1)
    },

    // ── Совместимость ────────────────────────────────────────────────────────

    test("старая запись боя без счёта брони читается: счёт брони с нуля, счёт HP — как был") {
      val old = wolfBattle(hero(1L)).copy(bossTaken = 120L).asJson
                  .mapObject(_.remove("bossArmorTaken"))
      val read = old.as[SoloPveBattle]
      assertTrue(old.hcursor.downField("bossArmorTaken").focus.isEmpty) &&
      assertTrue(read.exists(b => b.bossTaken == 120L && b.bossArmorTaken == 0L))
    },

    test("счёт брони пишется и читается обратно") {
      val b    = wolfBattle(hero(1L)).copy(bossTaken = 7L, bossArmorTaken = 11L)
      val read = b.asJson.as[SoloPveBattle]
      assertTrue(read.exists(r => r.bossTaken == 7L && r.bossArmorTaken == 11L)) &&
      assertTrue(b.asJson.hcursor.downField("bossArmorTaken").focus.contains(Json.fromLong(11L)))
    }
  ) @@ TestAspect.sequential
}
