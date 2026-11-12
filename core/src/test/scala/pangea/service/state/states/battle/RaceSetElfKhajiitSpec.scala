package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.TreasureMapGenerator
import pangea.model.battle.{Element, SoloPveBattle}
import pangea.model.hero.{Equipment, Hero, HeroSets}
import pangea.model.item.{BeltSlot, FlaskKind, Grip, GripRates, Item, ItemDetails, ItemSet, ItemType, MapZone, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.OutskirtsState
import pangea.service.state.states.events.treasure.{TreasureHuntProgress, TreasureHuntState}
import pangea.service.state.states.merchant.MerchantState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Малые расовые наборы «Тихая роща» (эльфы) и «Пустынь» (каджиты): шаги 6/9
  * и скрытый 12 — только своей расе. */
object RaceSetElfKhajiitSpec extends ZIOSpecDefault {

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

  private def hero(race: Race, set: ItemSet, pieces: Int, accuracy: Long = 9999L): Hero = TestFixtures.hero(userId).copy(
    lvl        = 10L,
    race       = race,
    fightStats = FightStats(atk = 500, hp = 100000, armor = 0, defence = 0, evasion = 9999, accuracy = accuracy, energy = 0),
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

  /** Тот же, но герой по нему уже бил: засады больше не будет. */
  private val struck = tough.copy(heroStruck = true)

  private def run(h: Hero, b: SoloPveBattle, rolls: Task[Unit], action: String = "Attack"): Task[(SoloPveBattle, String)] =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeActiveBattle(userId, b.asJson)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
      _   <- TestRandom.setSeed(42L) *> rolls
      _   <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap(action), r)
      after <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      log <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (after, log)

  override def spec = suite("«Тихая роща» и «Пустынь»")(

    test("«Тихая роща» 6: первый удар боя — из засады: без промаха и в полтора раза сильнее; второй — обычный") {
      // Моб почти неуловим, а бросок попадания — единица: обычный удар мажет.
      val slippery = tough.copy(monsterStats = tough.monsterStats.copy(evasion = 99999))
      val miss     = TestRandom.feedInts(1) *> TestRandom.feedLongs(100L)
      for {
        first  <- run(hero(Race.Human, ItemSet.QuietGrove, 6, accuracy = 1L), slippery, miss)
        second <- run(hero(Race.Human, ItemSet.QuietGrove, 6, accuracy = 1L), slippery.copy(heroStruck = true), miss)
        bare   <- run(hero(Race.Human, ItemSet.QuietGrove, 5, accuracy = 1L), slippery, miss)
      } yield assertTrue(first._2.contains("🌿 Засада «Тихой рощи»")) &&
              // оружия в руках нет — вполсилы: (1×3 + 500) × 0,5 × 1,5 = 377
              assertTrue(first._2.contains("Вы наносите 377 урона") && first._1.heroStruck) &&
              assertTrue(second._2.contains("промахиваетесь") && !second._2.contains("Удар из засады")) &&
              assertTrue(bare._2.contains("промахиваетесь"))
    },

    test("«Тихая роща» 9: удар по HP с шансом ослабляет врага на два раунда; без порога — нет") {
      for {
        nine  <- run(hero(Race.Human, ItemSet.QuietGrove, 9), struck, hitAndAllPass)
        eight <- run(hero(Race.Human, ItemSet.QuietGrove, 8), struck, hitAndAllPass)
      } yield assertTrue(nine._2.contains(": атака −15% на 2 раунда.")) &&
              // один раунд из двух уже прошёл
              assertTrue(nine._1.effects.monsterWeakenedPct == 15L && nine._1.effects.monsterWeakenedTurns == 1) &&
              assertTrue(eight._1.effects.monsterWeakenedTurns == 0 && !eight._2.contains("ослаблен"))
    },

    test("«Тихая роща» 12 — только эльфу: лук бьёт на место дальше, а вблизи теряет вдвое меньше") {
      def withBow(h: Hero): Hero = h.copy(equipment = h.equipment.copy(weapon = h.equipment.weapon.copy(grip = Some(Grip.Bow))))
      val elf    = withBow(hero(Race.Elf, ItemSet.QuietGrove, 12))
      val human  = withBow(hero(Race.Human, ItemSet.QuietGrove, 12))
      val sword  = hero(Race.Elf, ItemSet.QuietGrove, 12)
      // цель напротив — вблизи
      val close  = tough
      assertTrue(BattleState.heroReach(elf) == GripRates.RangedReach + 1) &&
      assertTrue(BattleState.heroReach(human) == GripRates.RangedReach) &&
      assertTrue(BattleState.heroReach(sword) == pangea.model.battle.GroupState.Reach) &&
      // вблизи лук теряет пятую часть, а у эльфа — десятую
      assertTrue(BattleState.rangedCloseMod(human, close) == 0.8 && BattleState.rangedCloseMod(elf, close) == 0.9) &&
      assertTrue(BattleState.displacement(elf) == BattleState.heroReach(elf) - 1)
    },

    test("«Пустынь» 6: шанс Воздуха +10, а попутный ветер от фляги держится на раунд дольше") {
      def withAirFlask(h: Hero): Hero = h.copy(equipment = h.equipment.copy(belt =
        Item(9L, "Пояс", 1L, ItemRarity.Blue, ItemType.Belt, attack = 0, accuracy = 0, energy = 0, armor = 0,
          defence = 0, evasion = 0, details = ItemDetails.Belt(List(BeltSlot.Flask(FlaskKind.Air, ItemRarity.Gray, 1))))))
      val belt = TestRandom.feedInts(List.fill(10)(99): _*)
      for {
        six  <- run(withAirFlask(hero(Race.Human, ItemSet.Desert, 6)), tough, belt, action = "BeltUse\",\"slot\":\"0")
        five <- run(withAirFlask(hero(Race.Human, ItemSet.Desert, 5)), tough, belt, action = "BeltUse\",\"slot\":\"0")
      } yield assertTrue(HeroSets(Map(ItemSet.Desert -> 6)).airChanceBonusPct == 10L) &&
              assertTrue(six._1.effects.airBoostTurns == Element.Air.ProcTurns + 1) &&
              assertTrue(five._1.effects.airBoostTurns == Element.Air.ProcTurns) &&
              // и в строке прока — тот же срок
              assertTrue(six._2.contains(s"💨 Ветер: ваши точность и уклонение +10% на ${Element.Air.ProcTurns + 1} хода.")) &&
              assertTrue(five._2.contains(s"💨 Ветер: ваши точность и уклонение +10% на ${Element.Air.ProcTurns} хода."))
    },

    test("«Пустынь» 9: Ришелье платит на 10% больше и продаёт на 10% дешевле") {
      assertTrue(HeroSets(Map(ItemSet.Desert -> 9)).tradePct == 10L) &&
      assertTrue(HeroSets(Map(ItemSet.Desert -> 8)).tradePct == 0L) &&
      assertTrue(MerchantState.withTrade(1000L, 10L) == 1100L && MerchantState.withTrade(1000L, -10L) == 900L) &&
      assertTrue(MerchantState.withTrade(1L, -10L) == 1L)
    },

    test("«Пустынь» 12 — только каджиту: поход по карте на четверть короче и +5% к уклонению") {
      val khajiit = hero(Race.Khajiit, ItemSet.Desert, 12)
      val human   = hero(Race.Human, ItemSet.Desert, 12)
      val short   = 7L * 3600000L * 75L / 100L
      assertTrue(TreasureHuntState.durationFor(khajiit) == short && TreasureHuntState.durationFor(human) == 7L * 3600000L) &&
      assertTrue(TreasureHuntState.formatDuration(short) == "5 часов 15 минут") &&
      assertTrue(TreasureHuntState.formatDuration(3660000L) == "1 час 1 минуту" &&
                 TreasureHuntState.formatDuration(2L * 3600000L) == "2 часа") &&
      assertTrue(khajiit.sets.evasionBonusPct == 5L && human.sets.evasionBonusPct == 0L)
    },

    test("каджит в «Пустыни» 12 уходит за кладом на 5 часов 15 минут — и дойдёт к их концу") {
      val khajiit = hero(Race.Khajiit, ItemSet.Desert, 12)
      val map     = TreasureMapGenerator.full(MapZone.values.head).copy(id = 5L)
      for {
        dao  <- TestHeroDao.withHero(userId, khajiit)
        inv   = TestInventoryRepository.withItems(List(map))
        sch  <- TestScheduler.make
        r    <- TestRenderer.make
        c    <- ZIO.attempt(SceneContent.load())
        out   = OutskirtsState(dao, inv, sch, c)
        _    <- out.action(testUser, tap("DepartTreasure"), r)
        _    <- out.action(testUser, tap(s"${OutskirtsState.MapPickPrefix}5"), r)
        ask  <- r.sentScreens.map(_.last)
        res  <- out.action(testUser, tap("ConfirmDepart"), r)
        tasks <- sch.scheduled
        prog <- dao.readSceneData(userId).map(_.flatMap(_.as[TreasureHuntProgress].toOption))
        hunt  = TreasureHuntState(dao, sch, c, inv, TestItemRepository.make)
        _    <- TestClock.adjust(zio.Duration.fromMillis(prog.map(_.duration).getOrElse(0L)))
        _    <- hunt.enter(testUser, r)
        there <- r.sentScreens.map(_.last)
      } yield assertTrue(ask.text.contains("займёт 5 часов 15 минут")) &&
              assertTrue(res == StateType.TreasureHunt) &&
              assertTrue(tasks.exists(t => t.kind == TaskKind.TreasureHunt && t.fireAt == 18900000L)) &&
              assertTrue(prog.flatMap(_.durationMs).contains(18900000L)) &&
              assertTrue(there.choices.exists(_.id == "HuntDone"))
    }
  )
}
