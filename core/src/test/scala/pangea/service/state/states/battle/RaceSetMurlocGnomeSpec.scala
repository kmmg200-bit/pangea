package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.arena.{ArenaAlly, ArenaFight, ArenaSide, ArenaStatus}
import pangea.model.battle.{BattleEffects, MonsterSlot, SoloPveBattle}
import pangea.model.hero.{Equipment, Hero, HeroSets}
import pangea.model.item.{Item, ItemSet, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.arena.ArenaBattle
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Малые расовые наборы «Мать Глубин» (мурлоки) и «Подгорный мастер» (гномы):
  * скользкая чешуя, мурлоки-собратья в начале боя и несгибаемый гном. */
object RaceSetMurlocGnomeSpec extends ZIOSpecDefault {

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

  private def hero(race: Race, set: ItemSet, pieces: Int, armor: Long = 0L, evasion: Long = 9999L): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = 10L,
      race       = race,
      fightStats = FightStats(atk = 500, hp = 100000, armor = armor, defence = 0, evasion = evasion,
                              accuracy = 9999, energy = 0),
      baseStats  = BaseStats(agi = 10, vit = 10, str = 1, int = 10),
      equipment  = wearing(set, pieces))

  /** Моб, которого герой не убьёт с одного удара и который сам его не убьёт. */
  private val tough = SoloPveBattle(
    monsterLvl          = 10L,
    monsterRace         = Race.Orc.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp    = 100000L,
    monsterCurrentArmor = 0L)

  /** Ещё один такой же — в строй рядом. */
  private val another = MonsterSlot(10L, Race.Orc.entryName, Rarity.Common.entryName, tough.monsterStats,
    100000L, 0L, false, 0L, BattleEffects.empty)

  private def kin(b: SoloPveBattle) = b.group.allies.filter(_.kind == AllyKind.MurlocKin)

  /** Вход в бой: каким стал бой и что увидел игрок. */
  private def enter(h: Hero, b: SoloPveBattle): Task[(SoloPveBattle, String)] =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      _     <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).enter(testUser, r)
      after <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (after, log)

  /** Ход «Атака»: куда ушёл бой, каким он стал, каким стал герой и что увидел игрок. */
  private def attack(h: Hero, b: SoloPveBattle, rolls: Task[Unit]): Task[(StateType, Option[SoloPveBattle], Hero, String)] =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      // Непотраченные броски прошлого прогона не должны съехать в этот.
      _     <- TestRandom.clearInts *> TestRandom.clearLongs *> TestRandom.setSeed(42L) *> rolls
      to    <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
      after <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      hero  <- dao.getHeroByUserId(userId).map(_.get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (to, after, hero, log)

  /** Логово каменного элементаля, у которого следующей идёт россыпь камней. */
  private def lair(h: Hero): SoloPveBattle = {
    val bossLvl = 2L // герой 15 уровня
    val stats   = MiniBoss.StoneElemental.stats(bossLvl)
    SoloPveBattle.from(Monster(0L, bossLvl, Race.Elemental, Rarity.Legendary, stats), h).copy(
      bossKind             = Some(MiniBoss.StoneElemental.entryName),
      bossTurn             = 1,
      bossCharges          = 2,
      monsterCurrentEnergy = 200L)
  }

  /** Удар героя, попадание камня и бросок травмы от его удара (5 — травма
    * прошла бы); разбросы ровные. */
  private val rocksWithTrauma = TestRandom.feedInts(60, 90, 5) *> TestRandom.feedLongs(100L, 100L)

  /** Герой под камнями: броня и HP — сколько скажут. Обычный удар каменного
    * бьёт и в HP мимо брони — 208 урона; при 400 HP это сокрушительный удар
    * (больше половины), и травма выпадает с шансом 50%. Залп россыпи сперва
    * ложится в броню. */
  private def underRocks(race: Race, armor: Long, hp: Long): Hero =
    hero(race, ItemSet.MountainMaster, 12).copy(
      lvl        = 15L,
      fightStats = FightStats(atk = 100, hp = hp, armor = armor, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  override def spec = suite("«Мать Глубин» и «Подгорный мастер»")(

    test("«Мать Глубин» 6 — открыта всем: обычный удар с шансом соскальзывает по чешуе и проходит вполовину") {
      val hitter = tough.copy(monsterStats = tough.monsterStats.copy(atk = 1000L, accuracy = 9999L))
      // Герой попадает, моб попадает, бросок чешуи проходит; разбросы ровные.
      val rolls = TestRandom.feedInts(99 :: 99 :: List.fill(10)(1): _*) *> TestRandom.feedLongs(100L, 100L)
      def lost(r: (StateType, Option[SoloPveBattle], Hero, String)): Long = 100000L - r._3.fightStats.hp
      for {
        six  <- attack(hero(Race.Human, ItemSet.DeepMother, 6, evasion = 0L), hitter, rolls)
        five <- attack(hero(Race.Human, ItemSet.DeepMother, 5, evasion = 0L), hitter, rolls)
      } yield assertTrue(six._4.contains("🐸 Чешуя «Матери Глубин»") && !five._4.contains("Чешуя «Матери Глубин»")) &&
              assertTrue(lost(five) > 0L && lost(six) == lost(five) / 2)
    },

    test("«Мать Глубин» 12 — только мурлоку: в начале боя выходит редкий мурлок уровня героя, против трёх — двое") {
      val murloc = hero(Race.Murloc, ItemSet.DeepMother, 12)
      val three  = tough.withReinforcement(another).withReinforcement(another)
      val stats  = MonsterGenerator.generateOfRaceAndRarity(10, Race.Murloc, Rarity.Rare).fightStats
      for {
        one   <- enter(murloc, tough)
        two   <- enter(murloc, three)
        human <- enter(hero(Race.Human, ItemSet.DeepMother, 12), tough)
        late  <- enter(murloc, tough.copy(roundsFought = 1))
        again <- enter(murloc, one._1)
      } yield assertTrue(kin(one._1).size == 1 && kin(two._1).size == 2) &&
              assertTrue(kin(one._1).forall(a => a.stats == stats && a.hp == stats.hp && a.lvl == 10L && a.home == 0)) &&
              assertTrue(one._2.contains("🐸 «Мать Глубин»: Мурлок-собрат встаёт в строй")) &&
              assertTrue(kin(human._1).isEmpty && kin(late._1).isEmpty) &&
              // вернулся в бой из снаряжения — второй раз не выходят
              assertTrue(kin(again._1).size == 1) &&
              assertTrue(HeroSets(Map(ItemSet.DeepMother -> 12), Some(Race.Murloc)).kinCount(2) == 1 &&
                         HeroSets(Map(ItemSet.DeepMother -> 12), Some(Race.Human)).kinCount(3) == 0)
    },

    test("после боя собрат уходит обратно в воду — в отряд он не встаёт") {
      val murloc = hero(Race.Murloc, ItemSet.DeepMother, 12)
      for {
        entered <- enter(murloc, tough.copy(monsterCurrentHp = 1L))
        won     <- attack(murloc, entered._1, TestRandom.feedInts(99) *> TestRandom.feedLongs(100L))
      } yield assertTrue(kin(entered._1).size == 1) &&
              assertTrue(won._1 == StateType.Loot && won._3.squad.allies.isEmpty)
    },

    test("на арене собратья выходят вместе с героем — по числу бойцов напротив") {
      val murloc = hero(Race.Murloc, ItemSet.DeepMother, 12)
      val me     = ArenaSide(userId, murloc.id, "Я", 10L)
      val lone   = ArenaSide(UserId(2L), murloc.id, "Соперник", 10L)
      val crowd  = lone.copy(allies = List(ArenaAlly(1, BattleState.kinAlly(murloc, 2)), ArenaAlly(2, BattleState.kinAlly(murloc, 3))))
      val one    = ArenaBattle.withStartingKin(me, murloc, lone)
      val two    = ArenaBattle.withStartingKin(me, murloc, crowd)
      assertTrue(one.allies.map(_.ally.kind) == List(AllyKind.MurlocKin)) &&
      assertTrue(two.allies.size == 2 && two.allies.map(_.ally.position).distinct.size == 2 && two.nextAlly == 3)
    },

    test("«Подгорный мастер» 12 — только гному: пока цела броня, камни не сбивают с ног и удары не травмируют") {
      def under(race: Race, armor: Long, hp: Long) = {
        val h = underRocks(race, armor, hp)
        attack(h, lair(h), rocksWithTrauma)
      }
      for {
        gnome <- under(Race.Gnome, 1000000L, 400L)
        human <- under(Race.Human, 1000000L, 400L)
        bare  <- under(Race.Gnome, 0L, 500000L)
      } yield assertTrue(gnome._2.exists(_.effects.heroStunnedTurns == 0) && gnome._4.contains("⛏ «Подгорный мастер»: с ног не сбить.")) &&
              assertTrue(gnome._3.traumaNames.isEmpty && !gnome._4.contains("сбивает Вас с ног")) &&
              // человеку в том же наборе скрытый шаг не дан: сбит и с травмой
              assertTrue(human._2.exists(_.effects.heroStunnedTurns == MiniBoss.StoneElemental.BurstDebuffTurns)) &&
              assertTrue(human._3.traumaNames.nonEmpty) &&
              // броню снесло — держать нечему
              assertTrue(bare._2.exists(_.effects.heroStunnedTurns == MiniBoss.StoneElemental.BurstDebuffTurns))
    },

    test("«Подгорный мастер» 12: гнома, пока на нём броня, на арене не пригвоздить") {
      val foe = TestFixtures.hero(UserId(2L))
      def pinned(h: Hero): Int = {
        val me    = ArenaSide(userId, h.id, "Я", 10L, rooted = 2)
        val other = ArenaSide(UserId(2L), foe.id, "Соперник", 10L)
        val fight = ArenaFight(7L, "1234", ArenaStatus.Fighting, me, Some(other))
        ArenaBattle.assemble(fight, me, other, h, foe, 0L).effects.heroRootedTurns
      }
      assertTrue(pinned(hero(Race.Gnome, ItemSet.MountainMaster, 12, armor = 100L)) == 0) &&
      assertTrue(pinned(hero(Race.Gnome, ItemSet.MountainMaster, 12, armor = 0L)) == 2) &&
      assertTrue(pinned(hero(Race.Human, ItemSet.MountainMaster, 12, armor = 100L)) == 2) &&
      assertTrue(HeroSets(Map(ItemSet.MountainMaster -> 12), Some(Race.Gnome)).steadfast(1L) &&
                 !HeroSets(Map(ItemSet.MountainMaster -> 12), Some(Race.Gnome)).steadfast(0L))
    }
  )
}
