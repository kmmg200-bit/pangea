package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{SkillSlotState, SlainMonster, SoloPveBattle}
import pangea.model.home.HomeData
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{NamedTrophy, Race, Rarity}
import pangea.model.skill.Skill
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.test.{TestBankRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Уникальный трофей падает с самого именного — если у героя такого нет ни в
  * сумке, ни в хранилище, ни на Стене трофеев; стена прибавляет герою урон по
  * расе — и удару, и умению. */
object TrophyWallBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val Karkan = "Каркан Кровавый Топор"
  private val ear    = NamedTrophy.item(Karkan, Race.Orc.entryName, 3L).fold(Item.NoItem)(_.copy(id = 77L))

  // Попадает практически всегда.
  private def strongHero = TestFixtures.hero(userId).copy(
    fightStats = FightStats(atk = 50, hp = 200, armor = 0, defence = 0, evasion = 9999, accuracy = 9999, energy = 0),
    baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1))

  /** Каркан с 1 HP — падает с первого удара. */
  private val karkan = SoloPveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.Orc.entryName,
    monsterRarity       = Rarity.Legendary.entryName,
    monsterStats        = FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp    = 1L,
    monsterCurrentArmor = 0L,
    customName          = Some(Karkan))

  /** Орк, который не падает: голая сталь, без защиты — урон виден как есть. */
  private val sturdyOrc = karkan.copy(
    monsterRarity    = Rarity.Common.entryName,
    customName       = None,
    monsterStats     = karkan.monsterStats.copy(hp = 9999, accuracy = 9999),
    monsterCurrentHp = 9999L)

  private val island = HomeData(tier = 3, metJunona = true)

  private def make(battle: SoloPveBattle, home: HomeData, hero: pangea.model.hero.Hero = strongHero,
                   bag: List[Item] = Nil, vault: List[Item] = Nil) =
    for {
      dao <- TestHeroDao.withHero(userId, hero)
      _   <- dao.writeActiveBattle(userId, battle.asJson)
      _   <- dao.writeHomeData(userId, home.asJson)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(heroDao = dao, inventoryRepo = TestInventoryRepository.withItems(bag), itemRepo = TestItemRepository.make,
               content = c, bank = Some(TestBankRepository.of(1, vault))), dao, r)

  private def lootOf(dao: TestHeroDao): Task[List[Item]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).fold(List.empty[Item])(_.items))

  private def named(items: List[Item]): List[String] = items.filter(_.isNamedTrophy).map(_.name)

  override def spec = suite("Стена трофеев в бою")(

    test("именной роняет свой уникальный трофей") {
      for {
        t <- make(karkan, HomeData.empty)
        (state, dao, r) = t
        _    <- state.action(testUser, tap("Attack"), r)
        loot <- lootOf(dao)
      } yield assertTrue(named(loot) == List("Ухо Каркана Кровавого Топора"))
    },

    test("уникальный — один на героя: висит на стене, лежит в сумке или в хранилище — второй не падает") {
      for {
        t1 <- make(karkan, island.copy(wall = List(Karkan)))
        (s1, d1, r1) = t1
        _      <- s1.action(testUser, tap("Attack"), r1)
        onWall <- lootOf(d1)
        t2 <- make(karkan, HomeData.empty, bag = List(ear))
        (s2, d2, r2) = t2
        _      <- s2.action(testUser, tap("Attack"), r2)
        inBag  <- lootOf(d2)
        t3 <- make(karkan, HomeData.empty, vault = List(ear))
        (s3, d3, r3) = t3
        _       <- s3.action(testUser, tap("Attack"), r3)
        inVault <- lootOf(d3)
      } yield assertTrue(named(onWall).isEmpty && named(inBag).isEmpty && named(inVault).isEmpty)
    },

    test("в одном бою — по трофею на именного; поднятый некромантом и безымянный ничего не роняют") {
      val slain = List(
        SlainMonster(1L, Race.Orc.entryName, Rarity.Legendary.entryName, marked = false, name = Karkan),
        SlainMonster(1L, Race.Orc.entryName, Rarity.Legendary.entryName, marked = false, name = Karkan, undead = true),
        SlainMonster(1L, Race.Goblin.entryName, Rarity.Legendary.entryName, marked = false, name = "Шныга Трёхпалый"),
        SlainMonster(1L, Race.Goblin.entryName, Rarity.Legendary.entryName, marked = false, name = "Шныга Трёхпалый"),
        SlainMonster(1L, Race.Orc.entryName, Rarity.Common.entryName, marked = false, name = "Орк-раб"))
      val drops = BattleState.namedTrophies(slain, 4L, owned = Set.empty).map(_.map(_.name))
      assertTrue(drops == List(Some("Ухо Каркана Кровавого Топора"), None, Some("Палец Шныги Трёхпалого"), None, None)) &&
        assertTrue(BattleState.namedTrophies(slain.take(1), 4L, owned = Set(Karkan)) == List(None))
    },

    test("стена: +1% урона за каждого именного расы цели; в бою записано, сколько по какой расе") {
      // Удар: (сила 1×3 + атака 500) × разброс 100% = 503; два орка на стене — ×1.02 = 513.
      val sword = Item(2L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)
      val hero  = strongHero.copy(fightStats = strongHero.fightStats.copy(atk = 500),
        equipment = TestFixtures.emptyEquipment.copy(weapon = sword))
      val walls = island.copy(wall = List(Karkan, "Грумм Две Косы", "Шныга Трёхпалый"))
      for {
        t1 <- make(sturdyOrc, island, hero)
        (s1, d1, r1) = t1
        _  <- TestRandom.clearInts *> TestRandom.clearLongs
        _  <- TestRandom.feedInts(60, 90, 3, 90)
        _  <- TestRandom.feedLongs(100L)
        _  <- s1.action(testUser, tap("Attack"), r1)
        b1 <- d1.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        t2 <- make(sturdyOrc, walls, hero)
        (s2, d2, r2) = t2
        _  <- TestRandom.clearInts *> TestRandom.clearLongs
        _  <- TestRandom.feedInts(60, 90, 3, 90)
        _  <- TestRandom.feedLongs(100L)
        _  <- s2.action(testUser, tap("Attack"), r2)
        b2 <- d2.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      } yield assertTrue(b1.map(b => sturdyOrc.monsterCurrentHp - b.monsterCurrentHp).contains(503L)) &&
              assertTrue(b2.map(b => sturdyOrc.monsterCurrentHp - b.monsterCurrentHp).contains(513L)) &&
              assertTrue(b2.map(_.trophyWall).contains(Map(Race.Orc.entryName -> 2, Race.Goblin.entryName -> 1))) &&
              assertTrue(b2.map(_.trophyMult).contains(1.02))
    },

    test("умение по той же расе тоже бьёт сильнее, а лечение стена не трогает") {
      def gear(id: Long, t: ItemType, d: ItemDetails) =
        Item(id, "Вещь", 1L, ItemRarity.Gray, t, attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
          details = d)
      val base   = TestFixtures.hero(userId)
      val hero   = base.copy(
        baseStats  = base.baseStats.copy(str = 50, int = 50, vit = 50, agi = 50),
        fightStats = base.fightStats.copy(atk = 50, accuracy = 50, defence = 20, energy = 10000, hp = 50, evasion = 0),
        equipment  = TestFixtures.emptyEquipment.copy(
          weapon     = gear(101L, ItemType.Weapon, ItemDetails.Weapon(Skill.SweepingStrike)),
          chestPlate = gear(202L, ItemType.ChestPlate, ItemDetails.Armor(Skill.MinorHeal))))
      val battle = sturdyOrc.copy(skillSlots = List(SkillSlotState(101L, Skill.SweepingStrike), SkillSlotState(202L, Skill.MinorHeal)))
      val walls  = island.copy(wall = List(Karkan, "Грумм Две Косы"))
      // Сколько снято с моба и сколько HP у героя после хода.
      def run(home: HomeData, skill: String): Task[(Long, Long)] =
        for {
          t <- make(battle, home, hero)
          (s, d, r) = t
          _ <- TestRandom.clearInts *> TestRandom.clearLongs
          _ <- TestRandom.feedInts(90, 90, 90, 90)
          _ <- TestRandom.feedLongs(100L, 100L)
          _ <- s.action(testUser, tap(skill), r)
          b <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
          h <- d.getHeroByUserId(userId)
        } yield (b.fold(0L)(x => battle.monsterCurrentHp - x.monsterCurrentHp), h.fold(0L)(_.fightStats.hp))
      for {
        plain  <- run(island, "Skill_101")
        walled <- run(walls, "Skill_101")
        healA  <- run(island, "Skill_202")
        healB  <- run(walls, "Skill_202")
      } yield assertTrue(plain._1 > 0L && walled._1 == (plain._1 * 1.02).toLong) &&
              assertTrue(healA._2 == healB._2 && healA._2 > 50L)
    }
  )
}
