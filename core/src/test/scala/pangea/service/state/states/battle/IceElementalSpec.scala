package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.loot.LootGenerator
import pangea.generator.loot.LootGenerator.LootDrop
import pangea.model.battle.{BattleAlly, BattleEffects, Burn, Element, SoloPveBattle, Ward}
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Grip, Item, ItemType, MaterialKind, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity => MobRarity}
import pangea.model.squad.{Ally, AllyKind}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.events.ElementalLairState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Ледяной элементаль: дальник, как Громовой, — бьёт холодом на три места и
  * встаёт в начале боя на четвёртом; круг — ледяной шип, ледяная глыба и
  * ледопад (вмерзание), метель (отбрасывает задетых), ледяной панцирь, пропуск.
  * Холод героя наращивает на нём лёд, огонь плавит. */
object IceElementalSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val ice     = MiniBoss.IceElemental
  private val heroLvl = 15L
  private val bossLvl = 3L // 15 / 5
  private val stats   = ice.stats(bossLvl)

  /** Одноручный меч; с камнем — стихия в оружии. */
  private def sword(gem: Option[GemKind]): Item =
    Item(50L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      grip = Some(Grip.OneHanded), sockets = gem.map(g => Some(Gem(g, 1))).toList)

  /** Герой 15-го уровня: бьёт без промаха, брони нет — урон виден по HP. */
  private def hero(gem: Option[GemKind] = None): Hero = {
    val base = TestFixtures.hero(userId)
    base.copy(
      lvl          = heroLvl,
      dungeonLevel = 15,
      fightStats   = FightStats(atk = 10, hp = 500000L, armor = 0L, defence = 0,
                                evasion = 0, accuracy = 9999, energy = 0),
      baseStats    = base.baseStats.copy(str = 1, vit = 50000, int = 100),
      equipment    = TestFixtures.emptyEquipment.copy(weapon = sword(gem)))
  }

  /** Бой с Ледяным: он на месте `at`, герой — на `heroAt`; `turn` — шаг круга. */
  private def iceBattle(h: Hero, turn: Int, at: Int = 4, heroAt: Int = 1, blocks: Int = 0): SoloPveBattle = {
    val mob  = Monster(0L, bossLvl, Race.Elemental, MobRarity.Legendary, stats)
    val base = SoloPveBattle.from(mob, h).copy(bossKind = Some(ice.entryName), bossTurn = turn,
                 bossCharges = blocks, monsterCurrentEnergy = stats.energy)
    base.copy(group = base.group.copy(heroPos = heroAt, activePos = at))
  }

  /** Наёмник с запасом HP, без брони и без энергии: удары его не валят, а
    * умений он не тратит — броски в тесте только те, что поданы. */
  private def ally(pos: Int): BattleAlly = {
    val a = BattleAlly.of(Ally(AllyKind.Human, pos, 1L, 0L, 0L), heroLvl)
    a.copy(hp = 100000L, armor = 0L, energy = 0L, stats = a.stats.copy(hp = 100000L, armor = 0L))
  }

  private def withAllies(b: SoloPveBattle, allies: BattleAlly*): SoloPveBattle =
    b.copy(group = b.group.copy(allies = allies.toList))

  private def makeState(h: Hero, battle: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  /** Ход героя кнопкой `key`: герой после хода, бой после хода и весь лог. */
  private def turn(h: Hero, battle: SoloPveBattle, key: String, ints: List[Int], longs: List[Long] = Nil) =
    for {
      t <- makeState(h, battle)
      (state, dao, r) = t
      _       <- TestRandom.feedInts(ints: _*) *> TestRandom.feedLongs(longs: _*)
      _       <- state.action(testUser, tap(key), r)
      updated <- dao.getHeroByUserId(userId).map(_.get)
      after   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      screens <- r.sentScreens
    } yield (updated, after.get, screens.map(_.text).mkString("\n"))

  /** Экран боя, как его видит герой при входе. */
  private def screenOf(h: Hero, battle: SoloPveBattle) =
    for {
      t <- makeState(h, battle)
      (state, _, r) = t
      _      <- state.enter(testUser, r)
      screen <- r.sentScreens.map(_.last)
    } yield screen.text

  /** Бросок удара моба: 1 — мимо (уклонение не ниже 5%), 99 — в цель. */
  private val Miss = 1

  override def spec = suite("Ледяной элементаль")(

    test("статы от BossLvL, дальник на три места со стартом на четвёртом, бьёт холодом") {
      assertTrue(stats.hp == 3000L && stats.armor == 3300L && stats.atk == 690L && stats.defence == 0L) &&
      assertTrue(stats.energy == 540L && stats.accuracy == 420L && stats.evasion == 240L) &&
      assertTrue(ice.energyRegen(bossLvl) == 27L && ice.expReward(bossLvl) == 600L) &&
      assertTrue(ice.reach == 3 && ice.startPos == 4 && ice.lairMinHeroLvl == 15L && ice.abilities == 5) &&
      assertTrue(ice.race == Race.Elemental && ice.monsterName == "Ледяной Элементаль") &&
      assertTrue(ice.attackElement.contains(Element.Cold) && !ice.immuneToBurn)
    },

    test("стихии: холод проходит на 20%, огонь — на 150%, прочее как обычно") {
      assertTrue(ice.damageTakenMult(Element.Cold) == 0.2 && ice.damageTakenMult(Element.Fire) == 1.5) &&
      assertTrue(ice.damageTakenMult(Element.Lightning) == 1.0 && ice.damageTakenMult(Element.Air) == 1.0) &&
      assertTrue(ice.plainDamageTakenMult == 1.0)
    },

    test("вечный лёд: его ингредиент, набора пока нет — «пока не используется», Ришелье платит дублонами") {
      assertTrue(ice.ingredient == MaterialKind.EternalIce && ice.set.isEmpty) &&
      assertTrue(MaterialKind.EternalIce.displayName == "Вечный лёд") &&
      assertTrue(MaterialKind.EternalIce.notYetUsed && MaterialKind.EternalIce.doubloonPrice == 5L)
    },

    test("бой в логове: Ледяной встаёт на четвёртом месте, герой — на первом") {
      val h = hero()
      for {
        dao      <- TestHeroDao.withHero(userId, h)
        _        <- dao.writeSceneData(userId, ElementalLairState.LairScene(ice.entryName).asJson)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        out      <- ElementalLairState(dao, content).action(testUser, tap("AttackElemental"), renderer)
        battle   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(battle.exists(b => b.bossKind.contains(ice.entryName) && b.group.activePos == 4 && b.group.heroPos == 1))
    },

    test("ледяной шип издалека: половина атаки, и защита героя наверняка ниже на 10%") {
      val h = hero()
      for {
        r <- turn(h, iceBattle(h, turn = 0), "Wait", ints = List(Miss))
        (u, after, log) = r
      } yield assertTrue(log.contains("Ледяной шип вонзается в Вас на") && log.contains("❄️ Холод: ваша защита −10%.")) &&
              assertTrue(after.effects.heroColdDefenceCut == Element.Cold.DefenceReductionCut) &&
              assertTrue(u.fightStats.hp < h.fightStats.hp && after.bossTurn == 1)
    },

    test("ледяной шип по горящему гасит пламя и сам не ложится; защитный отвар от холода бережёт") {
      val h = hero()
      val b = iceBattle(h, turn = 0)
      for {
        burning <- turn(h, b.copy(effects = b.effects.copy(heroBurn = Some(Burn(5)))), "Wait", ints = List(Miss))
        warded  <- turn(h, b.copy(effects = b.effects.copy(heroWards = Some(Set[Ward](Ward.Cold)))), "Wait", ints = List(Miss))
      } yield assertTrue(burning._2.effects.heroBurn.isEmpty && burning._2.effects.heroColdDefenceCut == 0) &&
              assertTrue(warded._2.effects.heroColdDefenceCut == 0)
    },

    test("ледяная глыба копится до трёх: третья — ледопад, и герой вмерзает на два раунда") {
      val h = hero()
      for {
        first <- turn(h, iceBattle(h, turn = 1), "Wait", ints = List(Miss))
        third <- turn(h, iceBattle(h, turn = 1, blocks = 2), "Wait", ints = List(Miss, 99)) // травмы нет
      } yield assertTrue(first._3.contains("Рядом с элементалем намерзает ледяная глыба: 1 из 3.") && first._2.bossCharges == 1) &&
              assertTrue(third._3.contains("Ледопад! Глыбы обрушиваются на Вас на")) &&
              assertTrue(third._3.contains("Ноги вмерзают в лёд: 2 раунда с места не сойти.")) &&
              assertTrue(third._2.bossCharges == 0) &&
              // лёг посреди раунда на остаток его и два следующих: конец этого уже прошёл
              assertTrue(third._2.effects.heroFrozenTurns.contains(ice.FrozenTurns)) &&
              assertTrue(BattleState.stepTargets(third._2).isEmpty) &&
              assertTrue(third._1.fightStats.hp < h.fightStats.hp - 2L * stats.atk)
    },

    test("вмёрзший — со значком 🧊; лёд сходит в конце раунда, и шагать снова можно") {
      val h = hero()
      val b = iceBattle(h, turn = 4)
      val held = b.copy(effects = b.effects.copy(heroFrozenTurns = Some(1)))
      for {
        screen <- screenOf(h, held)
        r      <- turn(h, held, "Wait", ints = List(Miss))
        (_, after, _) = r
      } yield assertTrue(screen.contains(ice.FrozenIcon)) &&
              assertTrue(after.effects.heroFrozenTurns.isEmpty && BattleState.stepTargets(after).nonEmpty)
    },

    test("ледопад по союзнику: он вмерзает и с места не сходит, хоть бить ему и некого") {
      val h    = hero()
      val a    = ally(2)
      // Он на пятом: до героя четыре места, до союзника — три.
      val b    = withAllies(iceBattle(h, turn = 1, at = 5, blocks = 2), a)
      for {
        r <- turn(h, b, "Wait", ints = List(Miss))
        (_, after, log) = r
      } yield assertTrue(log.contains(s"Ледопад! Глыбы обрушиваются на союзника: ${a.name} получает")) &&
              assertTrue(log.contains(s"${a.name} вмерзает в лёд: 2 раунда с места не сойти.")) &&
              assertTrue(after.group.allyAt(2).exists(x => x.frozen == ice.FrozenTurns && x.hp < a.hp))
    },

    test("метель бьёт всех в трёх местах и отбрасывает задетых от элементаля на клетку") {
      val h = hero()
      val a = ally(7)
      // Он на четвёртом, герой на третьем, союзник — на седьмом (за ним).
      val b = withAllies(iceBattle(h, turn = 2, heroAt = 3), a)
      for {
        r <- turn(h, b, "Wait", ints = List(Miss))
        (u, after, log) = r
      } yield assertTrue(log.contains("Метель! Ледяной вихрь накрывает всех вокруг элементаля.")) &&
              assertTrue(log.contains("Мороз обжигает Вас на") && log.contains(s"Мороз обжигает союзника: ${a.name} получает")) &&
              assertTrue(log.contains("Ветер отбрасывает Вас на место 2.") && log.contains(s"Ветер отбрасывает ${a.name} на место 8.")) &&
              assertTrue(after.group.heroPos == 2 && u.fightStats.hp < h.fightStats.hp)
    },

    test("метель не сдвигает вмёрзшего и упёршегося в край строя") {
      val h      = hero()
      val frozen = iceBattle(h, turn = 2, heroAt = 3)
      val edge   = iceBattle(h, turn = 2, at = 2, heroAt = 1)
      for {
        held  <- turn(h, frozen.copy(effects = frozen.effects.copy(heroFrozenTurns = Some(2))), "Wait", ints = List(Miss))
        stuck <- turn(h, edge, "Wait", ints = List(Miss))
      } yield assertTrue(held._2.group.heroPos == 3 && !held._3.contains("Ветер отбрасывает Вас")) &&
              assertTrue(stuck._2.group.heroPos == 1 && !stuck._3.contains("Ветер отбрасывает Вас"))
    },

    test("ледяной панцирь возвращает 30% брони, а HP не трогает") {
      val h = hero()
      val b = iceBattle(h, turn = 3, at = 1).copy(monsterCurrentHp = 1500L, monsterCurrentArmor = 1650L)
      for {
        r <- turn(h, b, "Wait", ints = List(Miss))
        (_, after, log) = r
      } yield assertTrue(log.contains("Элементаль обрастает свежим льдом… Он восстановил 990 брони!")) &&
              assertTrue(after.monsterCurrentArmor == 1650L + 990L && after.monsterCurrentHp == 1500L)
    },

    test("холод героя его не сковывает, а наращивает лёд: броня прибавляется, защита не тает") {
      val h = hero(gem = Some(GemKind.Sapphire))
      val b = iceBattle(h, turn = 4, at = 1).copy(monsterCurrentArmor = 1650L)
      for {
        // удар героя — 60, прок холода — 1; удар Ледяного — мимо
        r <- turn(h, b, "Attack", ints = List(60, 1, Miss), longs = List(100L))
        (_, after, log) = r
      } yield assertTrue(log.contains("Ледяной Элементаль обрастает льдом: броня +165.")) &&
              assertTrue(after.effects.monsterColdDefenceCut == 0)
    },

    test("огонь плавит лёд: пламя ложится, гаснет глыба, и потолок брони ниже на 10%") {
      val h = hero(gem = Some(GemKind.Ruby))
      for {
        r <- turn(h, iceBattle(h, turn = 4, at = 1, blocks = 2), "Attack", ints = List(60, 1, Miss), longs = List(100L))
        (_, after, log) = r
      } yield assertTrue(log.contains("Лёд тает: глыб осталось 1, потолок брони элементаля −330.")) &&
              assertTrue(after.bossCharges == 1 && after.effects.monsterMaxArmorCut == 330L) &&
              assertTrue(after.effects.monsterBurn.isDefined)
    },

    test("добыча: вечный лёд — не больше одного с боя, вещи — без набора") {
      val rolls = (1L to 200L).map(s => LootGenerator.rollMiniBoss(ice, bossLvl, heroLvl, Rng(s))._1)
      def pieces(d: List[LootDrop]) = d.count(LootGenerator.isIngredient(ice, _))
      val gear  = rolls.flatten.collect { case LootDrop.Gear(item) if item.material.isEmpty => item }
      assertTrue(rolls.forall(d => pieces(d) <= 1) && rolls.exists(d => pieces(d) == 1)) &&
      assertTrue(gear.nonEmpty && gear.forall(_.set.isEmpty))
    },

    test("лёд — не «пригвождён» с арены: свои поля, переживают смену пары, старые записи без них читаются") {
      val fx     = BattleEffects(heroFrozenTurns = Some(2))
      val oldFx  = BattleEffects(heroWebTurns = Some(1)).asJson.mapObject(_.remove("heroFrozenTurns"))
      val oldAly = ally(1).asJson.mapObject(_.remove("frozen"))
      assertTrue(fx.heroRooted && fx.heroFrozen && fx.heroRootedTurns == 0) &&
      assertTrue(fx.withMonsterPart(BattleEffects.empty).heroFrozenTurns.contains(2)) &&
      assertTrue(oldFx.as[BattleEffects].exists(e => e.heroFrozenTurns.isEmpty && !e.heroFrozen)) &&
      assertTrue(oldAly.as[BattleAlly].exists(_.frozen == 0))
    }
  )
}
