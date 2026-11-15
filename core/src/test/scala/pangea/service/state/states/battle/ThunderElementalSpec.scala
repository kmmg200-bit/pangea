package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.loot.LootGenerator
import pangea.generator.loot.LootGenerator.LootDrop
import pangea.model.battle.{BattleAlly, BattleEffects, Element, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Grip, Item, ItemSet, ItemType, MaterialKind, Rarity => ItemRarity}
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

/** Громовой элементаль: дальник — бьёт молнией на три места и встаёт в начале
  * боя на четвёртом; круг — разряд, грозовой заряд и раскат, цепная молния,
  * ионный покров, пропуск. Молния героя его заряжает, ветер рвёт тучу. Заодно —
  * молния в ударе обычного моба (человека) выжигает герою энергию. */
object ThunderElementalSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val thunder = MiniBoss.ThunderElemental
  private val heroLvl = 15L
  private val bossLvl = 3L // 15 / 5
  private val stats   = thunder.stats(bossLvl)

  /** Одноручный меч; с камнем — стихия в оружии. */
  private def sword(gem: Option[GemKind]): Item =
    Item(50L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      grip = Some(Grip.OneHanded), sockets = gem.map(g => Some(Gem(g, 1))).toList)

  /** Герой 15-го уровня: бьёт без промаха, энергии — под потолок. `armor` — его
    * броня: с ней видно, что молния Громового пробивает её в HP. */
  private def hero(armor: Long = 0L, gem: Option[GemKind] = None): Hero = {
    val base = TestFixtures.hero(userId)
    val h = base.copy(
      lvl          = heroLvl,
      dungeonLevel = 15,
      fightStats   = FightStats(atk = 10, hp = 500000L, armor = armor, defence = 0,
                                evasion = 0, accuracy = 9999, energy = 0),
      baseStats    = base.baseStats.copy(str = 1, vit = 50000, int = 100),
      equipment    = TestFixtures.emptyEquipment.copy(weapon = sword(gem)))
    h.copy(fightStats = h.fightStats.copy(energy = h.maxEnergy(0L)))
  }

  /** Бой с Громовым: он на месте `at`, герой — на `heroAt`; `turn` — шаг круга. */
  private def thunderBattle(h: Hero, turn: Int, at: Int = 4, heroAt: Int = 1, charges: Int = 0,
                            energy: Long = stats.energy): SoloPveBattle = {
    val mob  = Monster(0L, bossLvl, Race.Elemental, MobRarity.Legendary, stats)
    val base = SoloPveBattle.from(mob, h).copy(bossKind = Some(thunder.entryName), bossTurn = turn,
                 bossCharges = charges, monsterCurrentEnergy = energy)
    base.copy(group = base.group.copy(heroPos = heroAt, activePos = at))
  }

  /** Наёмник с запасом HP и без брони: удары его не валят. */
  private def sturdyAlly(pos: Int): BattleAlly = {
    val a = BattleAlly.of(Ally(AllyKind.Human, pos, 1L, 0L, 0L), heroLvl)
    a.copy(hp = 100000L, armor = 0L, energy = a.stats.energy,
           stats = a.stats.copy(hp = 100000L, armor = 0L))
  }

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

  /** Бросок удара моба по герою: 1 — мимо (уклонение не ниже 5%), 99 — в цель. */
  private val Miss = 1
  private val Hit  = 99

  override def spec = suite("Громовой элементаль")(

    test("статы от BossLvL, дальник на три места со стартом на четвёртом, бьёт молнией") {
      assertTrue(stats.hp == 3600L && stats.armor == 2100L && stats.atk == 750L && stats.defence == 150L) &&
      assertTrue(stats.energy == 600L && stats.accuracy == 450L && stats.evasion == 300L) &&
      assertTrue(thunder.energyRegen(bossLvl) == 30L && thunder.expReward(bossLvl) == 600L) &&
      assertTrue(thunder.reach == 3 && thunder.startPos == 4 && thunder.lairMinHeroLvl == 15L && thunder.abilities == 5) &&
      assertTrue(thunder.race == Race.Elemental && thunder.monsterName == "Громовой Элементаль") &&
      assertTrue(thunder.attackElement.contains(Element.Lightning) && !thunder.immuneToBurn) &&
      // прочие минибоссы как были: бьют соседей и встают на первом
      assertTrue(MiniBoss.values.filterNot(b => b == thunder || b == MiniBoss.IceElemental).forall(b => b.reach == 1 && b.startPos == 1 && b.lairMinHeroLvl == 0L))
    },

    test("стихии: молния проходит на 20%, ветер — на 150%, прочее как обычно") {
      assertTrue(thunder.damageTakenMult(Element.Lightning) == 0.2 && thunder.damageTakenMult(Element.Air) == 1.5) &&
      assertTrue(thunder.damageTakenMult(Element.Fire) == 1.0 && thunder.damageTakenMult(Element.Cold) == 1.0) &&
      assertTrue(thunder.plainDamageTakenMult == 1.0)
    },

    test("сердце грозы: его ингредиент, переводит вещь в его набор «Гроза»; Ришелье платит дублонами") {
      assertTrue(thunder.ingredient == MaterialKind.StormHeart && thunder.set.contains(ItemSet.Storm)) &&
      assertTrue(!MaterialKind.StormHeart.notYetUsed && MaterialKind.StormHeart.doubloonPrice == 5L) &&
      assertTrue(MaterialKind.StormHeart.displayName == "Сердце грозы")
    },

    test("в логове — с 15-го уровня героя, наравне с огненным и каменным") {
      val early = ElementalLairState.lairPool(14L).toSet
      val ready = ElementalLairState.lairPool(15L).toSet
      assertTrue(early == Set[MiniBoss](MiniBoss.FireElemental, MiniBoss.StoneElemental)) &&
      assertTrue(ready == Set[MiniBoss](MiniBoss.FireElemental, MiniBoss.StoneElemental, thunder, MiniBoss.IceElemental))
    },

    test("бой в логове: Громовой встаёт на четвёртом месте, герой — на первом, пары нет") {
      val h = hero()
      for {
        dao      <- TestHeroDao.withHero(userId, h)
        _        <- dao.writeSceneData(userId, ElementalLairState.LairScene(thunder.entryName).asJson)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        out      <- ElementalLairState(dao, content).action(testUser, tap("AttackElemental"), renderer)
        battle   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(battle.exists(b => b.bossKind.contains(thunder.entryName) && b.monsterLvl == bossLvl)) &&
              assertTrue(battle.exists(b => b.group.activePos == 4 && b.group.heroPos == 1 && !b.group.paired))
    },

    test("бьёт издалека и с места не сходит; молния пробивает броню — пятая часть удара по ней уходит в HP") {
      val h = hero(armor = 100000L)
      for {
        // Шаг круга — пропуск: в раунде только его удар. Прока молнии нет (99).
        r <- turn(h, thunderBattle(h, turn = 4), "Wait", ints = List(Hit, 99), longs = List(100L))
        (u, after, log) = r
        armorLost = h.fightStats.armor - u.fightStats.armor
      } yield assertTrue(log.contains("Громовой Элементаль атаковал вас издалека на") && log.contains("молнией ⚡")) &&
              assertTrue(after.group.activePos == 4 && !after.group.paired) &&
              assertTrue(armorLost > 0L && h.fightStats.hp - u.fightStats.hp == (armorLost * 0.2).toLong)
    },

    test("разряд издалека: половина атаки и пятая часть энергии героя") {
      val h = hero()
      for {
        r <- turn(h, thunderBattle(h, turn = 0), "Wait", ints = List(Miss))
        (_, after, log) = r
      } yield assertTrue(log.contains("С элементаля срывается разряд и бьёт в Вас на")) &&
              assertTrue(log.contains(s"⚡ Молния: ваша энергия −${h.maxEnergy(0L) * thunder.BoltEnergyBurnPct / 100L}.")) &&
              assertTrue(after.bossTurn == 1)
    },

    test("грозовой заряд копится до трёх: третий срывается в раскат, и следующее умение героя сорвётся") {
      val h = hero()
      for {
        first <- turn(h, thunderBattle(h, turn = 1), "Wait", ints = List(Miss))
        third <- turn(h, thunderBattle(h, turn = 1, charges = 2), "Wait", ints = List(Miss, 99)) // травмы нет
      } yield assertTrue(first._3.contains("Над элементалем сгущается туча: заряд 1 из 3.") && first._2.bossCharges == 1) &&
              assertTrue(third._3.contains("Громовой раскат! Туча разряжается в Вас на")) &&
              assertTrue(third._3.contains("В ушах звенит: следующее ваше умение сорвётся.")) &&
              assertTrue(third._2.bossCharges == 0 && third._2.effects.heroSkillJammed.contains(true)) &&
              assertTrue(third._1.fightStats.hp < h.fightStats.hp - 2L * stats.atk * 9L / 10L)
    },

    test("цепная молния: по цели и по тем, кто стоит с ней рядом") {
      val h    = hero()
      // Он в паре с героем на втором месте; союзники — на первом и третьем.
      // Энергии у них нет: иначе раненые тут же выпили бы флягу и залечились.
      val base = thunderBattle(h, turn = 2, at = 2, heroAt = 2)
      val b    = base.copy(group = base.group.copy(
                   allies = List(sturdyAlly(1).copy(energy = 0L), sturdyAlly(3).copy(energy = 0L))))
      for {
        r <- turn(h, b, "Wait", ints = List(Miss))
        (u, after, log) = r
      } yield assertTrue(log.contains("Цепная молния бьёт в Вас на") && u.fightStats.hp < h.fightStats.hp) &&
              assertTrue("Молния перескакивает на соседа".r.findAllMatchIn(log).size == 2) &&
              assertTrue(after.group.allies.size == 2 && after.group.allies.forall(_.hp < 100000L))
    },

    test("ионный покров: чинит пятую часть брони и двадцатую — HP") {
      val h = hero()
      val b = thunderBattle(h, turn = 3, at = 1).copy(monsterCurrentHp = 1800L, monsterCurrentArmor = 1050L)
      for {
        r <- turn(h, b, "Wait", ints = List(Miss))
        (_, after, log) = r
      } yield assertTrue(log.contains("ионный покров") && log.contains("Он восстановил 180 HP и 420 брони!")) &&
              assertTrue(after.monsterCurrentHp == 1800L + 180L && after.monsterCurrentArmor == 1050L + 420L)
    },

    test("союзник в трёх местах, герой дальше: разряд и удар достаются союзнику, молния выжигает ему энергию") {
      val h    = hero()
      val ally = sturdyAlly(2)
      // Он на пятом: до героя четыре места, до союзника — три.
      val base = thunderBattle(h, turn = 0, at = 5)
      val b    = base.copy(group = base.group.copy(allies = List(ally)))
      for {
        // удар по союзнику — в цель, прок молнии — да (1); разряд — без бросков
        r <- turn(h, b, "Wait", ints = List(Hit, 1), longs = List(100L))
        (u, after, log) = r
        // молния выжигает половину урона, что удар снял с HP (брони у него нет)
        hit   = """бьёт .+? на (\d+) урона""".r.findFirstMatchIn(log).map(_.group(1).toLong).getOrElse(-1L)
        burnt = Element.lightningBurn(hit).min(ally.energy)
      } yield assertTrue(hit > 0L && log.contains(s"⚡ ${ally.name}: молния, энергия −$burnt.")) &&
              assertTrue(log.contains(s"бьёт в союзника: ${ally.name} получает")) &&
              // в конце раунда бить ему некого — он шагнул к врагу, поэтому ищем по отряду
              assertTrue(after.group.allies.headOption.exists(a => a.hp < ally.hp && a.energy < ally.energy)) &&
              assertTrue(u.fightStats.hp == h.fightStats.hp)
    },

    test("молния героя его не обесточивает, а заряжает") {
      val h = hero(gem = Some(GemKind.Topaz))
      for {
        // удар героя — 60, прок молнии — 1; удар Громового — мимо
        r <- turn(h, thunderBattle(h, turn = 4, at = 1, energy = 300L), "Attack", ints = List(60, 1, Miss), longs = List(100L))
        (_, after, log) = r
      } yield assertTrue(log.contains("Громовой Элементаль впитывает молнию: энергии у него +60.")) &&
              assertTrue(after.monsterCurrentEnergy == 300L + 60L + thunder.energyRegen(bossLvl))
    },

    test("ветер рвёт тучу: гаснет заряд, и он несколько раундов бьёт менее точно") {
      val h = hero(gem = Some(GemKind.Diamond))
      for {
        r <- turn(h, thunderBattle(h, turn = 4, at = 1, charges = 2), "Attack", ints = List(60, 1, Miss), longs = List(100L))
        (_, after, log) = r
      } yield assertTrue(log.contains("Ветер рвёт грозовую тучу: зарядов осталось 1")) &&
              assertTrue(after.bossCharges == 1) &&
              // лёг на пять раундов, один уже прошёл — в начале хода моба
              assertTrue(after.effects.mobDispersedTurns.contains(thunder.DispersedTurns - 1))
    },

    test("молния в ударе человека выжигает герою половину урона по HP; ушёл в броню — энергия цела, броню она не пробивает") {
      def man(h: Hero) = SoloPveBattle.from(Monster(0L, 10L, Race.Human, MobRarity.Common,
                           FightStats(atk = 100, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 10000, energy = 0)), h)
                           .copy(noKin = true)
      val bare    = hero()
      val armored = hero(armor = 10000L)
      for {
        // удар человека — в цель, прок молнии — да
        r1 <- turn(bare, man(bare), "Wait", ints = List(Hit, 1), longs = List(100L))
        (u1, _, log1) = r1
        lost = bare.fightStats.hp - u1.fightStats.hp
        r2 <- turn(armored, man(armored), "Wait", ints = List(Hit, 1), longs = List(100L))
        (u2, _, log2) = r2
      } yield assertTrue(lost > 0L && log1.contains(s"⚡ Молния: ваша энергия −${Element.lightningBurn(lost)}.")) &&
              assertTrue(log2.contains("молнией ⚡") && !log2.contains("⚡ Молния: ваша энергия")) &&
              assertTrue(u2.fightStats.armor < armored.fightStats.armor && u2.fightStats.hp == armored.fightStats.hp)
    },

    test("добыча: сердце грозы — не больше одного с боя, вещи — из «Грозы»") {
      val rolls = (1L to 200L).map(s => LootGenerator.rollMiniBoss(thunder, bossLvl, heroLvl, Rng(s))._1)
      def hearts(d: List[LootDrop]) = d.count(LootGenerator.isIngredient(thunder, _))
      val gear  = rolls.flatten.collect { case LootDrop.Gear(item) if item.material.isEmpty => item }
      assertTrue(rolls.forall(d => hearts(d) <= 1) && rolls.exists(d => hearts(d) == 1)) &&
      assertTrue(gear.exists(_.set.contains(ItemSet.Storm)) && gear.forall(_.set.forall(_ == ItemSet.Storm)))
    },

    test("бои, записанные раньше, читаются: тучи без поля нет") {
      val old = BattleEffects(mobInstinctTurns = 1).asJson.mapObject(_.remove("mobDispersedTurns"))
      assertTrue(old.as[BattleEffects].exists(e => e.mobDispersedTurns.isEmpty && !e.mobDispersed))
    }
  )
}
