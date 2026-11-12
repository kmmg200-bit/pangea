package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{Bleed, Burn, Poison, SoloPveBattle}
import pangea.model.hero.{Equipment, Hero, HeroSets}
import pangea.model.item.{Item, ItemSet, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.skill.MonsterSkill
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.trauma.TraumaRoll
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.DeathState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Малые расовые наборы «Первопроходец» (люди) и «Орда» (орки): шаги 6/9 и
  * скрытый 12 — только своей расе. */
object RaceSetHumanOrcSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** Герой попадает, разброс урона — ровно 100%. */
  private val cleanHit = TestRandom.feedInts(99) *> TestRandom.feedLongs(100L)

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

  private def hero(race: Race, set: ItemSet, pieces: Int): Hero = TestFixtures.hero(userId).copy(
    lvl        = 10L,
    race       = race,
    fightStats = FightStats(atk = 500, hp = 100000, armor = 0, defence = 0, evasion = 9999, accuracy = 9999, energy = 0),
    baseStats  = BaseStats(agi = 10, vit = 10, str = 1, int = 10),
    equipment  = wearing(set, pieces))

  /** Тот же герой с `pct` процентами своего максимума HP. */
  private def wounded(h: Hero, pct: Long): Hero =
    h.copy(fightStats = h.fightStats.copy(hp = h.effectiveMaxHp(0L) * pct / 100L))

  /** Моб, которого герой не убьёт с одного удара и который сам его не убьёт. */
  private val tough = SoloPveBattle(
    monsterLvl          = 10L,
    monsterRace         = Race.Orc.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp    = 100000L,
    monsterCurrentArmor = 0L)

  /** Удар героя без оружия в руках (вполсилы) с атакой `atk` и разбросом 100%. */
  private def bareHit(h: Hero, atk: Long): Long = ((h.effectiveBaseStats(0L).str * 3L + atk) * 0.5).toLong

  /** Моб, который убивает с одного удара; на герое висят яд, кровь и пламя. */
  private val killer = tough.copy(
    monsterStats = tough.monsterStats.copy(atk = 100000L, accuracy = 9999L),
    effects      = tough.effects.copy(heroPoison = Some(Poison(5)), heroBleed = Some(Bleed(4)),
                                      heroBurn = Some(Burn(Burn.Initial))))

  /** Полный «Первопроходец» на герое с десятью HP, который не уворачивается. */
  private def frail(race: Race): Hero = {
    val h = hero(race, ItemSet.Pioneer, 12)
    h.copy(fightStats = h.fightStats.copy(hp = 10L, evasion = 0L))
  }

  /** Ход игрока: куда ушёл бой, каким он стал, каким стал герой и что увидел игрок. */
  private def attack(h: Hero, b: SoloPveBattle, rolls: Task[Unit],
                     action: String = "Attack"): Task[(StateType, Option[SoloPveBattle], Hero, String)] =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      _     <- TestRandom.setSeed(42L) *> rolls
      to    <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap(action), r)
      after <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      hero  <- dao.getHeroByUserId(userId).map(_.get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (to, after, hero, log)

  /** Смерть героя: каким он встал и сколько ему отдыхать. */
  private def die(h: Hero): Task[(Hero, Long)] =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      _     <- DeathState(dao, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
      after <- dao.getHeroByUserId(userId).map(_.get)
      rest  <- dao.readSceneData(userId).map(_.flatMap(_.hcursor.get[Long]("restDurationMs").toOption).getOrElse(0L))
    } yield (after, rest)

  override def spec = suite("«Первопроходец» и «Орда»")(

    test("«Первопроходец» 6 — открыт всем: каждый раунд боя +1% к атаке и защите, до +6%") {
      val six  = hero(Race.Elf, ItemSet.Pioneer, 6)
      val five = hero(Race.Elf, ItemSet.Pioneer, 5)
      val sets = HeroSets(Map(ItemSet.Pioneer -> 6))
      for {
        third <- attack(six, tough.copy(roundsFought = 3), cleanHit)
        late  <- attack(six, tough.copy(roundsFought = 40), cleanHit)
        bare  <- attack(five, tough.copy(roundsFought = 40), cleanHit)
      } yield assertTrue(third._4.contains(s"Вы наносите ${bareHit(six, 515L)} урона")) &&
              // дальше шестого раунда не растёт
              assertTrue(late._4.contains(s"Вы наносите ${bareHit(six, 530L)} урона")) &&
              assertTrue(bare._4.contains(s"Вы наносите ${bareHit(five, 500L)} урона")) &&
              // раунд прошёл — счётчик подрос
              assertTrue(third._2.exists(_.roundsFought == 4)) &&
              assertTrue(sets.roundBonusPct(0) == 0L && sets.roundBonusPct(2) == 2L && sets.roundBonusPct(9) == 6L)
    },

    test("«Первопроходец» 9: травма вдвое короче, но уже идущий срок новая травма не режет") {
      val hour = 3600000L
      val nine = hero(Race.Elf, ItemSet.Pioneer, 9)
      assertTrue(HeroSets(Map(ItemSet.Pioneer -> 9)).traumaDurationMs == 4L * hour) &&
      assertTrue(HeroSets(Map(ItemSet.Pioneer -> 8)).traumaDurationMs == TraumaRoll.DurationMs) &&
      assertTrue(nine.traumaUntilWithNew(0L) == 4L * hour) &&
      assertTrue(nine.copy(traumaUntil = Some(7L * hour)).traumaUntilWithNew(0L) == 7L * hour)
    },

    test("«Первопроходец» 9 при смерти: травма на 4 часа вместо 8, отдых вдвое короче") {
      for {
        nine  <- die(hero(Race.Elf, ItemSet.Pioneer, 9).copy(lvl = 50L))
        eight <- die(hero(Race.Elf, ItemSet.Pioneer, 8).copy(lvl = 50L))
      } yield assertTrue(nine._1.traumaUntil.contains(TraumaRoll.DurationMs / 2)) &&
              assertTrue(eight._1.traumaUntil.contains(TraumaRoll.DurationMs)) &&
              // половина отдыха — с точностью до округления миллисекунд
              assertTrue(math.abs(nine._2 - eight._2 / 2L) <= 1L && eight._2 > 60000L) &&
              assertTrue(HeroSets(Map(ItemSet.Pioneer -> 9)).deathRestMult == 0.5 &&
                         HeroSets(Map(ItemSet.Pioneer -> 8)).deathRestMult == 1.0)
    },

    test("«Первопроходец» 12 — только человеку: смерть оставляет 1 HP и снимает яд, кровь и пламя, раз за бой") {
      for {
        human <- attack(frail(Race.Human), killer, cleanHit)
        again <- attack(frail(Race.Human), killer.copy(secondWindUsed = true), cleanHit)
        elf   <- attack(frail(Race.Elf), killer, cleanHit)
      } yield assertTrue(human._1 == StateType.Battle && human._3.fightStats.hp == 1L) &&
              assertTrue(human._4.contains("Второе дыхание")) &&
              assertTrue(human._2.exists(b => b.secondWindUsed && b.effects.heroPoison.isEmpty &&
                           b.effects.heroBleed.isEmpty && b.effects.heroBurn.isEmpty)) &&
              // второй раз за бой не спасает
              assertTrue(again._1 == StateType.Death && !again._4.contains("Второе дыхание")) &&
              assertTrue(elf._1 == StateType.Death && !elf._4.contains("Второе дыхание"))
    },

    test("«Первопроходец» 12: устоял после удара в спину — и всё-таки ушёл из боя") {
      for {
        fled <- attack(frail(Race.Human), killer, cleanHit, action = "ConfirmFlee")
      } yield assertTrue(fled._1 == StateType.Dungeon && fled._3.fightStats.hp == 1L) &&
              assertTrue(fled._4.contains("Второе дыхание") && fled._2.isEmpty)
    },

    test("«Орда» 6: удар, снявший больше 20% HP, проходит на четверть слабее; малый — как есть") {
      val six   = hero(Race.Orc, ItemSet.Horde, 6)
      val five  = hero(Race.Orc, ItemSet.Horde, 5)
      val maxHp = six.effectiveMaxHp(0L)
      def lost(h: Hero, dmg: Long): Long = h.fightStats.hp - MonsterSkill.applyPhysicalDamage(tough, h, dmg, 0L)._1
      assertTrue(lost(six, maxHp / 2) == maxHp / 2 * 75L / 100L) &&
      assertTrue(lost(six, maxHp / 10) == maxHp / 10) &&
      assertTrue(lost(five, maxHp / 2) == maxHp / 2)
    },

    test("«Орда» 9 — открыта всем: ниже половины HP +15% к атаке") {
      val nine = hero(Race.Human, ItemSet.Horde, 9)
      for {
        low  <- attack(wounded(nine, 40L), tough, cleanHit)
        high <- attack(wounded(nine, 60L), tough, cleanHit)
      } yield assertTrue(low._4.contains(s"Вы наносите ${bareHit(nine, 575L)} урона")) &&
              assertTrue(high._4.contains(s"Вы наносите ${bareHit(nine, 500L)} урона"))
    },

    test("«Орда» 12 — только орку: +3% к атаке за каждые 10% недостающего HP, до +30%, и вампиризм ниже половины") {
      val orc   = HeroSets(Map(ItemSet.Horde -> 12), Some(Race.Orc))
      val human = HeroSets(Map(ItemSet.Horde -> 12), Some(Race.Human))
      // недостаёт 70% — семь шагов по 3%, и ярость девятого порога
      assertTrue(orc.woundAttackPct(30L, 100L) == 21L + 15L && human.woundAttackPct(30L, 100L) == 15L) &&
      // недостаёт 15% — один полный шаг, ярости ещё нет
      assertTrue(orc.woundAttackPct(85L, 100L) == 3L && orc.woundAttackPct(100L, 100L) == 0L) &&
      // шаги упираются в 30%
      assertTrue(orc.woundAttackPct(0L, 100L) == 30L + 15L) &&
      assertTrue(orc.woundLifestealPct(30L, 100L) == 3L && orc.woundLifestealPct(60L, 100L) == 0L &&
                 human.woundLifestealPct(30L, 100L) == 0L)
    },

    test("орк в «Орде» 12 ниже половины HP пьёт кровь своим ударом, человек в том же наборе — нет") {
      for {
        orc   <- attack(wounded(hero(Race.Orc, ItemSet.Horde, 12), 30L), tough, cleanHit)
        human <- attack(wounded(hero(Race.Human, ItemSet.Horde, 12), 30L), tough, cleanHit)
      } yield assertTrue(orc._4.contains("🩸 Вампиризм: +")) &&
              assertTrue(!human._4.contains("Вампиризм восстановил"))
    }
  )
}
