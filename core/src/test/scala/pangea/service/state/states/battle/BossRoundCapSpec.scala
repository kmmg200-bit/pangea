package pangea.service.state.states.battle

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleEffects, MonsterSlot, Poison, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Item, ItemType, Rarity}
import pangea.model.monster.{BossPercentDamage, BossRoundCap, MiniBoss, Monster, Race, Rarity => MobRarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.{UIO, ZIO}
import zio.test.TestRandom
import zio.test._

/** Предел минибосса за раунд: больше 40% HP и 40% брони за раунд он не теряет
  * — от чего бы то ни было (см. BossRoundCap), — а урон от процентов получает
  * вдвое меньший (см. BossPercentDamage). */
object BossRoundCapSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val wolf    = MiniBoss.WhiteWolf
  private val bossLvl = 2L
  private val stats   = wolf.stats(bossLvl)
  private val hpCap   = stats.hp * BossRoundCap.HpPct / 100L
  private val armCap  = stats.armor * BossRoundCap.ArmorPct / 100L

  private def weapon(gems: GemKind*): Item =
    Item(50L, "Меч", 1L, Rarity.Blue, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      sockets = gems.toList.map(k => Some(Gem(k, 1))))

  /** Герой не промахивается и переживает всё, что босс успеет ответить. */
  private def hero(atk: Long, gems: GemKind*): Hero =
    TestFixtures.hero(userId).copy(
      lvl          = 8L,
      dungeonLevel = 20,
      fightStats   = FightStats(atk = atk, hp = 500000L, armor = 0, defence = 0,
                                evasion = 0, accuracy = 9999, energy = 0),
      baseStats    = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      equipment    = TestFixtures.emptyEquipment.copy(weapon = weapon(gems: _*))
    )

  /** Бой с Белым волком; `boss = false` — тот же зверь, но рядовой моб. */
  private def wolfBattle(h: Hero, boss: Boolean = true): SoloPveBattle = {
    val b = SoloPveBattle.from(Monster(0L, bossLvl, wolf.race, MobRarity.Legendary, stats), h)
    if (boss) b.copy(bossKind = Some(wolf.entryName), monsterCurrentEnergy = stats.energy) else b
  }

  /** Обычный зверь без энергии: ни подмоги, ни лечения себе. */
  private def plainWolf(h: Hero): SoloPveBattle =
    wolfBattle(h, boss = false).copy(monsterRarity = MobRarity.Common.entryName, monsterCurrentEnergy = 0L)

  private val defaultFeed: UIO[Unit] =
    TestRandom.feedInts(60, 90, 1, 1) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)

  /** Все броски по 10: проки стихий оружия (30%) срабатывают, промахов нет. */
  private val procsFire: UIO[Unit] =
    TestRandom.feedInts(List.fill(200)(10): _*) *> TestRandom.feedLongs(List.fill(50)(100L): _*)

  private def strike(h: Hero, battle: SoloPveBattle, feed: UIO[Unit] = defaultFeed) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
      state     = BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content)
      _        <- feed
      _        <- state.action(testUser, tap("Attack"), renderer)
      after    <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      screens  <- renderer.sentScreens
    } yield (after, screens.map(_.text).mkString("\n"))

  override def spec = suite("Предел минибосса за раунд и урон от процентов")(

    // ── Сам предел ───────────────────────────────────────────────────────────

    test("40% HP и 40% брони — порознь, что сверх, до босса не доходит") {
      val b    = wolfBattle(hero(1L))
      val hurt = b.hurtActive(stats.armor, stats.hp * 10L)
      assertTrue(BossRoundCap.HpPct == 40L && BossRoundCap.ArmorPct == 40L) &&
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

    test("минибосс в строю держит тот же предел, и счёт его — на его слоте") {
      val slot  = MonsterSlot(bossLvl, wolf.race.entryName, MobRarity.Legendary.entryName, stats,
                    stats.hp, stats.armor, marked = false, 0L, BattleEffects.empty, boss = Some(wolf.entryName))
      val plain = wolfBattle(hero(1L), boss = false)
      val lined = plain.admit(slot)
      val idx   = lined.group.others.indexWhere(_.boss.isDefined)
      val (b, hurt) = lined.hurtSlot(idx, 0L, stats.hp * 10L)
      assertTrue(idx >= 0 && hurt.hp == hpCap) &&
      assertTrue(b.group.others(idx).currentHp == stats.hp - hpCap) &&
      // счёт едет вместе с минибоссом; у того, кто в полях, — свой, нетронутый
      assertTrue(b.group.others(idx).bossTaken == hpCap && b.bossTaken == 0L) &&
      // тот, кто в полях, — рядовой: его не тронули
      assertTrue(b.monsterCurrentHp == plain.monsterCurrentHp && b.bossKind.isEmpty)
    },

    // ── В бою ────────────────────────────────────────────────────────────────

    test("раны тоже упираются в предел: яд по боссу снимает не больше 40% HP за раунд") {
      // Удар героя — комариный, а яд такой, что даже вполсилы снял бы с волка
      // больше предела.
      val h = hero(1L)
      val b = wolfBattle(h).withEffects(BattleEffects.empty.copy(monsterPoison = Some(Poison(90))))
      for {
        t <- strike(h, b)
        (after, log) = t
      } yield assertTrue(after.exists(_.monsterCurrentHp == stats.hp - hpCap)) &&
              assertTrue(s"🟢 Яд → [^:\\n]+: $hpCap урона\\.".r.findFirstIn(log).isDefined) &&
              assertTrue(log.contains("Белый Волк держится: больше за этот раунд с него не снять"))
    },

    test("удар героя, который снёс бы босса целиком, снимает ровно 40% HP и брони") {
      val h = hero(1000000L)
      for {
        t <- strike(h, wolfBattle(h))
        (after, log) = t
      } yield assertTrue(after.exists(b => b.monsterCurrentHp == stats.hp - hpCap && b.monsterCurrentArmor == stats.armor - armCap)) &&
              // строка о пределе — одна на раунд, сколько бы ударов в него ни упёрлось
              assertTrue("держится".r.findAllMatchIn(log).size == 1)
    },

    // ── Урон от процентов ────────────────────────────────────────────────────

    test("урон от процентов по минибоссу — вдвое меньше, по рядовому мобу — как есть") {
      assertTrue(BossPercentDamage.Pct == 50L) &&
      assertTrue(wolfBattle(hero(1L)).pctDamage(400L) == 200L) &&
      assertTrue(wolfBattle(hero(1L), boss = false).pctDamage(400L) == 400L)
    },

    test("яд в 10% снимает с волка-босса вдвое меньше, рядовому зверю — сполна") {
      // 10% от 2000 = 200; волк-босс истекает ранами на пятую часть охотнее —
      // 240, — а урон от процентов по боссу вдвое меньше: 120.
      val h = hero(1L)
      def poisoned(b: SoloPveBattle) = b.withEffects(BattleEffects.empty.copy(monsterPoison = Some(Poison(10))))
      for {
        bt <- strike(h, poisoned(wolfBattle(h)))
        (bossAfter, bossLog) = bt
        pt <- strike(h, poisoned(plainWolf(h)))
        (plainAfter, plainLog) = pt
      } yield assertTrue(bossAfter.exists(_.monsterCurrentHp == stats.hp - 120L)) &&
              assertTrue("""🟢 Яд → [^:\n]+: 120 урона\.""".r.findFirstIn(bossLog).isDefined) &&
              assertTrue(plainAfter.exists(_.monsterCurrentHp == stats.hp - 200L)) &&
              assertTrue("""🟢 Яд → [^:\n]+: 200 урона\.""".r.findFirstIn(plainLog).isDefined)
    },

    test("взрыв Огня с Воздухом снимает с босса 15% HP вместо 30%, с рядового — 30%") {
      val h = hero(1L, GemKind.Ruby, GemKind.Diamond)
      for {
        bt <- strike(h, wolfBattle(h), procsFire)
        (_, bossLog) = bt
        pt <- strike(h, plainWolf(h), procsFire)
        (_, plainLog) = pt
      } yield assertTrue(s"🔥💨 Огонь и воздух → [^:\\n]+: ${stats.hp * 15L / 100L} урона\\.".r.findFirstIn(bossLog).isDefined) &&
              assertTrue(s"🔥💨 Огонь и воздух → [^:\\n]+: ${stats.hp * 30L / 100L} урона\\.".r.findFirstIn(plainLog).isDefined)
    },

    test("святая вода жжёт мёртвого минибосса вдвое слабее: 2,5% максимума HP вместо 5%") {
      // Гнилой Джо пропускает ход (круг на четвёртом шаге) — сам себя не лечит.
      // Одни и те же броски с водой и без: разница в HP — ровно её тик.
      val joe      = MiniBoss.RottenJoe
      val joeStats = joe.stats(bossLvl)
      val h        = hero(1L)
      // Звать некого (noKin): подкрепление со стороны бой бы не изменило, но
      // рядовой нежити его и собрать не из чего.
      def joeFight(water: Boolean) =
        SoloPveBattle.from(Monster(0L, bossLvl, joe.race, MobRarity.Legendary, joeStats), h)
          .copy(bossKind = Some(joe.entryName), bossTurn = 3, monsterCurrentEnergy = joeStats.energy,
                holyWater = water, noKin = true)
      def deadFight(water: Boolean) =
        SoloPveBattle.from(Monster(0L, bossLvl, Race.Undead, MobRarity.Common, joeStats), h)
          .copy(monsterCurrentEnergy = 0L, holyWater = water, noKin = true)
      for {
        dry      <- strike(h, joeFight(water = false))
        wet      <- strike(h, joeFight(water = true))
        deadDry  <- strike(h, deadFight(water = false))
        deadWet  <- strike(h, deadFight(water = true))
        joeLost   = dry._1.map(_.monsterCurrentHp).getOrElse(0L) - wet._1.map(_.monsterCurrentHp).getOrElse(0L)
        deadLost  = deadDry._1.map(_.monsterCurrentHp).getOrElse(0L) - deadWet._1.map(_.monsterCurrentHp).getOrElse(0L)
      } yield assertTrue(joeLost == joeStats.hp * 5L / 100L / 2L) &&
              assertTrue(deadLost == joeStats.hp * 5L / 100L)
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
