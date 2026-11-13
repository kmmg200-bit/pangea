package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleEffects, BattlePrefs, GroupState, MonsterSlot, SoloPveBattle}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.{Item, ItemSet, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Race, Rarity}
import pangea.model.squad.{Ally, AllyKind, Squad, UndeadForm}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Аура в конце раунда держит всё поле — и стоящего героя тоже: миазмы
  * Некроманта гноят живого, святая вода жжёт героя-нежить. Обнулила его — это
  * смерть в тот же ход, как от удара, и она важнее победы. Раньше исход
  * оставался «бой идёт»: герой стоял с нулём HP и следующим ходом ещё бил —
  * добил активного один на один — победа на нуле HP, не добил — смерть ходом
  * позже, а «Первопроходец» после ауры не спасал вовсе. */
object AuraFellsHeroSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

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

  /** Человек с `hp` HP, который выжидает: бить он не станет, а удар моба мимо
    * (см. [[missed]]) — всё, что он потеряет за ход, снимет аура. */
  private def hero(hp: Long, gear: Equipment = TestFixtures.emptyEquipment, squad: Squad = Squad.empty): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 1, hp = hp, armor = 0, defence = 0, evasion = 9999, accuracy = 9999, energy = 60),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(int = 20, str = 1),
      equipment  = gear,
      squad      = squad)

  /** Живой орк на сто тысяч HP: никого не убьёт и сам не умрёт. Без сородичей —
    * подкрепление в конце раунда не придёт. */
  private def foe(hp: Long = 100000L): SoloPveBattle = SoloPveBattle(
    monsterLvl          = 10L,
    monsterRace         = Race.Orc.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp    = hp,
    monsterCurrentArmor = 0L,
    noKin               = true)

  /** Тот же противник — Некромант с полной энергией: миазмы ему держать есть
    * чем (тысяча энергии — сотня в них за раунд), а колдовать в этом раунде
    * нечего: на очереди мистический шаг, а прятаться не за кем. Удар проклятой
    * энергией сюда не годится — он снял бы HP раньше миазм. */
  private def necromancer(b: SoloPveBattle): SoloPveBattle = b.copy(
    monsterRace          = Race.Undead.entryName,
    bossKind             = Some(MiniBoss.Necromancer.entryName),
    bossTurn             = 1,
    monsterStats         = b.monsterStats.copy(energy = 1000L),
    monsterCurrentEnergy = 1000L)

  /** Поднятый без запаса энергии: умений не применяет, миазмы его только лечат. */
  private def undeadAlly(pos: Int): Ally =
    Ally(AllyKind.Undead, pos, 400L, 0L, 0L,
      undead = Some(UndeadForm("Поднятый орк", 10L,
        FightStats(atk = 50, hp = 1000, armor = 200, defence = 10, evasion = 10, accuracy = 100, energy = 0))))

  /** Первый бросок хода — удар моба по герою: единица — мимо. */
  private val missed = TestRandom.feedInts(1)

  /** Ход игрока: куда ушёл бой, каким он стал, каким стал герой и что увидел игрок. */
  private def turn(h: Hero, b: SoloPveBattle, action: String = "Wait",
                   miasma: Boolean = true): Task[(StateType, Option[SoloPveBattle], Hero, String)] =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      _     <- dao.writeBattlePrefs(userId, BattlePrefs(miasma = miasma).asJson)
      sch   <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      _     <- missed
      to    <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c, sch)
                 .action(testUser, tap(action), r)
      after <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      hero  <- dao.getHeroByUserId(userId).map(_.get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (to, after, hero, log)

  override def spec = suite("Аура обнулила стоящего героя")(

    test("миазмы Некроманта обнулили живого героя — смерть в тот же ход") {
      // 2% максимума и ещё сотня вложенной энергии — больше полусотни, что у него есть
      for {
        out <- turn(hero(hp = 50L), necromancer(foe()))
        (to, _, h, log) = out
      } yield assertTrue(log.contains("🟣 Миазмы Некроманта держат поле боя")) &&
              assertTrue(log.contains("🟣 Тьма гложет и вас: -50 HP.")) &&
              assertTrue(h.fightStats.hp == 0L) &&
              assertTrue(to == StateType.Death && log.contains("Вы погибли..."))
    },

    test("святая вода обнулила героя-нежить — смерть в тот же ход") {
      // «Некромант» на десяти вещах: герой — нежить, и вода снимает с него 5% максимума
      val undead = hero(hp = 5L, gear = wearing(ItemSet.Necromancer, 10))
      for {
        // свои миазмы свёрнуты — иначе вода с ними погасли бы обе
        out <- turn(undead, foe().copy(holyWater = true), miasma = false)
        (to, _, h, log) = out
      } yield assertTrue(undead.sets.heroIsUndead && undead.effectiveMaxHp(0L) * 5L / 100L > 5L) &&
              assertTrue(log.contains("💧 Святая вода жжёт и вас: -5 HP.")) &&
              assertTrue(h.fightStats.hp == 0L) &&
              assertTrue(to == StateType.Death && log.contains("Вы погибли..."))
    },

    test("та же вода добила и мёртвого в паре — смерть важнее победы") {
      // у мёртвого 30 HP из тысячи, а вода снимает с него 50: пали оба, и это
      // смерть героя, а не победа с добычей
      val undead = hero(hp = 5L, gear = wearing(ItemSet.Necromancer, 10))
      val dying  = foe(hp = 1000L).copy(monsterUndead = true, monsterCurrentHp = 30L, holyWater = true)
      for {
        out <- turn(undead, dying, miasma = false)
        (to, _, h, log) = out
      } yield assertTrue(log.contains("💧 Святая вода: каждый мёртвый −5% HP.")) &&
              assertTrue(h.fightStats.hp == 0L) &&
              assertTrue(to == StateType.Death && log.contains("Вы погибли..."))
    },

    test("«Первопроходец» спасает и от ауры: 1 HP, и бой идёт") {
      val pioneer = hero(hp = 50L, gear = wearing(ItemSet.Pioneer, 12))
      for {
        out <- turn(pioneer, necromancer(foe()))
        (to, after, h, log) = out
      } yield assertTrue(pioneer.sets.secondWind) &&
              assertTrue(log.contains("🟣 Тьма гложет и вас: -50 HP.")) &&
              assertTrue(log.contains("🧑 Второе дыхание «Первопроходца»")) &&
              assertTrue(h.fightStats.hp == 1L) &&
              assertTrue(to == StateType.Battle && after.exists(_.secondWindUsed))
    },

    test("«Первопроходец» устоял, а та же аура добила того, кто в паре, — это победа над ним") {
      // Некромант держит миазмы из строя, поодаль, а в паре — живой орк на 30 HP
      // из тысячи: миазмы снимают с него 20 и сотню вложенной энергии
      val necro = MonsterSlot(10L, Race.Undead.entryName, Rarity.Legendary.entryName,
        FightStats(atk = 1, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 1000),
        100000L, 0L, marked = false, 1000L, BattleEffects.empty, boss = Some(MiniBoss.Necromancer.entryName),
        bossTurn = 1) // на очереди шаг, а не удар проклятой энергией: HP снимают одни миазмы
      val pair    = foe(hp = 1000L).copy(monsterCurrentHp = 30L,
        group = GroupState.empty.copy(others = List(necro), places = List(4)))
      val pioneer = hero(hp = 50L, gear = wearing(ItemSet.Pioneer, 12))
      for {
        out <- turn(pioneer, pair)
        (to, after, h, log) = out
      } yield assertTrue(log.contains("🧑 Второе дыхание «Первопроходца»")) &&
              assertTrue(h.fightStats.hp == 1L) &&
              assertTrue(log.contains("пал. Место перед вами опустело")) &&
              assertTrue(to == StateType.Battle && after.exists(b => b.secondWindUsed && b.group.slain.size == 1))
    },

    test("с живым союзником аура героя не убивает: он лежит, отряд бьётся дальше") {
      val h = hero(hp = 50L, squad = Squad(heroPos = 1, allies = List(undeadAlly(2))))
      val b = necromancer(SoloPveBattle.from(foe().toMonster, h)).copy(noKin = true)
      for {
        out <- turn(h, b)
        (to, after, hAfter, log) = out
      } yield assertTrue(log.contains("🟣 Тьма гложет и вас: -50 HP.")) &&
              assertTrue(hAfter.fightStats.hp == 0L) &&
              assertTrue(to == StateType.Battle && log.contains("💀 Вы падаете. Отряд бьётся без вас")) &&
              assertTrue(after.exists(b => b.group.heroDown && b.group.allies.exists(_.alive)))
    },

    test("лежачего аура не трогает: отряд бьётся, герой так и лежит") {
      val h    = hero(hp = 0L, squad = Squad(heroPos = 1, allies = List(undeadAlly(2))))
      val b0   = necromancer(SoloPveBattle.from(foe().toMonster, h)).copy(noKin = true)
      val down = b0.copy(group = b0.group.copy(heroDown = true))
      for {
        out <- turn(h, down, action = "SquadTick")
        (to, after, hAfter, log) = out
      } yield assertTrue(log.contains("🟣 Миазмы Некроманта держат поле боя")) &&
              assertTrue(!log.contains("Тьма гложет и вас")) &&
              assertTrue(hAfter.fightStats.hp == 0L) &&
              assertTrue(to == StateType.Battle && after.exists(_.group.heroDown))
    }
  )
}
