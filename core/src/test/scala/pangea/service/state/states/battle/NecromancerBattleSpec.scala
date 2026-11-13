package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, BattleEffects, Bleed, HealCurse, Regen, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{BeltSlot, FlaskKind, Item, ItemDetails, ItemType, Rarity}
import pangea.model.monster.{MiniBoss, Monster, Rarity => MobRarity}
import pangea.model.squad.{Ally, AllyKind}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Некромант в бою: удар проклятой энергией — вся атака сразу в HP, мимо брони,
  * и проклятие, при котором раны не затягиваются до конца следующего раунда;
  * созидание — уровнем в саму пещеру. */
object NecromancerBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private val beltUse: UserAction = UserAction("", Some("""{"action":"BeltUse","slot":"0"}"""))

  private val necro   = MiniBoss.Necromancer
  private val heroLvl = 30L
  private val bossLvl = necro.bossLvl(heroLvl) // 30 / 4 = 7
  private val atk     = necro.stats(bossLvl).atk // 150 × 7 = 1050

  private def weapon: Item =
    Item(50L, "Меч", 1L, Rarity.Blue, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  /** Пояс с одной флягой `kind` на один глоток. */
  private def belt(kind: FlaskKind): Item =
    Item(9L, "Пояс", 1L, Rarity.Blue, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Belt(List(BeltSlot.Flask(kind, Rarity.Gray, 1))))

  private def beltSlots(i: Item): List[BeltSlot] = i.details match {
    case b: ItemDetails.Belt => b.slots
    case _                   => Nil
  }

  /** Герой бьёт без промаха; брони у него с запасом — обычный удар Некроманта
    * уходит в неё, и HP меняет только проклятая энергия. */
  private def hero(belted: Option[Item] = None): Hero = {
    val base = TestFixtures.hero(userId)
    base.copy(
      lvl          = heroLvl,
      dungeonLevel = 30,
      fightStats   = FightStats(atk = 10, hp = 500000L, armor = 1000000L, defence = 0,
                                evasion = 0, accuracy = 9999, energy = 0),
      baseStats    = base.baseStats.copy(str = 1, vit = 50000),
      equipment    = TestFixtures.emptyEquipment.copy(weapon = weapon,
                       belt = belted.getOrElse(TestFixtures.emptyEquipment.belt)))
  }

  /** Тот же герой, но HP на `missing` ниже максимума — чтобы было что лечить. */
  private def wounded(h: Hero, missing: Long = 100000L): Hero =
    h.copy(fightStats = h.fightStats.copy(hp = h.effectiveMaxHp(0L) - missing))

  /** Некромант один на один с героем. Святая вода гасит его миазмы (см.
    * BattleState.holyClash): иначе они грызли бы HP героя каждый раунд, и в
    * числах было бы не разобрать, что сделал сам удар. */
  private def necroBattle(h: Hero, turn: Int, energy: Long = 500L): SoloPveBattle = {
    val stats = necro.stats(bossLvl)
    val mob   = Monster(0L, bossLvl, necro.race, MobRarity.Legendary, stats, customName = Some(necro.monsterName))
    SoloPveBattle.from(mob, h).copy(bossKind = Some(necro.entryName), bossTurn = turn, minionLvl = 20L,
      noKin = true, holyWater = true, monsterCurrentEnergy = energy)
  }

  private def withEffects(b: SoloPveBattle)(f: BattleEffects => BattleEffects): SoloPveBattle =
    b.copy(effects = f(b.effects))

  /** Наёмник с большим запасом HP и без брони: удары его не валят, а урон
    * виден по HP целиком. */
  private def sturdyAlly(pos: Int): BattleAlly = {
    val a = BattleAlly.of(Ally(AllyKind.Human, pos, 1L, 0L, 0L), heroLvl)
    a.copy(hp = 100000L, armor = 0L, stats = a.stats.copy(hp = 100000L, armor = 0L))
  }

  private def makeState(h: Hero, battle: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  /** Ход героя кнопкой `action`: герой после хода, бой после хода и весь лог. */
  private def turn(h: Hero, battle: SoloPveBattle, action: UserAction, seed: ZIO[Any, Nothing, Unit]) =
    for {
      t <- makeState(h, battle)
      (state, dao, r) = t
      _       <- seed
      _       <- state.action(testUser, action, r)
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

  /** Удар героя и попадание моба в паре; урон героя — без разброса. */
  private val pairedSeed: ZIO[Any, Nothing, Unit] = TestRandom.feedInts(60, 90) *> TestRandom.feedLongs(100L, 100L)

  private val MobHit = """наносит (\d+) урона""".r

  private def mobHitDamage(log: String): Long =
    MobHit.findFirstMatchIn(log).map(_.group(1).toLong).getOrElse(-1L)

  override def spec = suite("Некромант: удар проклятой энергией")(

    test("в паре: вся атака сразу в HP — броня его не принимает; проклятие ложится, круг идёт, энергия списана") {
      val h = hero()
      for {
        r <- turn(h, necroBattle(h, turn = 0), tap("Attack"), pairedSeed)
        (u, after, log) = r
      } yield assertTrue(log.contains(s"Удар проклятой энергией! Вы получили $atk урона. " +
                                      "До конца следующего раунда лечение на вас не действует.")) &&
              // HP убыло ровно на удар: обычный удар Некроманта ушёл в броню
              assertTrue(h.fightStats.hp - u.fightStats.hp == atk) &&
              assertTrue(h.fightStats.armor - u.fightStats.armor == mobHitDamage(log)) &&
              // легло на два конца раунда, один уже прошёл
              assertTrue(after.effects.heroCursedTurns.contains(HealCurse.Turns - 1)) &&
              assertTrue(after.bossTurn == 1) &&
              assertTrue(after.monsterCurrentEnergy ==
                500L - necro.CurseCostPerLvl * bossLvl + necro.energyRegen(bossLvl))
    },

    test("проклятие держит весь следующий раунд: реген не лечит; кончилось — реген снова лечит") {
      val h      = wounded(hero())
      val regen  = Regen(5)
      // Шаг Некроманта в бою один на один — пропуск: прятаться не за кем.
      val base   = withEffects(necroBattle(h, turn = 1))(_.copy(heroRegen = Some(regen)))
      val cursed = withEffects(base)(_.copy(heroCursedTurns = Some(1)))
      for {
        held <- turn(h, cursed, tap("Attack"), pairedSeed)
        (heldHero, heldAfter, heldLog) = held
        free <- turn(h, base, tap("Attack"), pairedSeed)
        (freeHero, _, freeLog) = free
      } yield assertTrue(heldHero.fightStats.hp == h.fightStats.hp && !heldLog.contains("Регенерация")) &&
              // проклятие сошло в конце раунда, а срок регена шёл и под ним
              assertTrue(heldAfter.effects.heroCursedTurns.isEmpty) &&
              assertTrue(heldAfter.effects.heroRegen == regen.decayed) &&
              assertTrue(freeHero.fightStats.hp == h.fightStats.hp + regen.healOn(h.effectiveMaxHp(0L))) &&
              assertTrue(freeLog.contains("💚 Регенерация: +"))
    },

    test("фляга целителя под проклятием: глоток уходит, кровь снимается, а HP не растёт") {
      val h = wounded(hero(Some(belt(FlaskKind.Healer))))
      val b = withEffects(necroBattle(h, turn = 1))(_.copy(heroCursedTurns = Some(1), heroBleed = Some(Bleed(2))))
      for {
        r <- turn(h, b, beltUse, pairedSeed)
        (u, after, log) = r
      } yield assertTrue(log.contains("🖤 Проклятие Некроманта: лечение на вас не действует.")) &&
              assertTrue(u.fightStats.hp == h.fightStats.hp) &&
              assertTrue(after.effects.heroBleed.isEmpty) &&
              assertTrue(beltSlots(u.equipment.belt) == List(BeltSlot.Flask(FlaskKind.Healer, Rarity.Gray, 0)))
    },

    test("фляга очищения снимает проклятие") {
      val h = hero(Some(belt(FlaskKind.Cleansing)))
      val b = withEffects(necroBattle(h, turn = 1))(_.copy(heroCursedTurns = Some(2)))
      for {
        r <- turn(h, b, beltUse, ZIO.unit)
        (_, after, log) = r
      } yield assertTrue(log.contains("Фляга очищения")) &&
              assertTrue(after.effects.heroCursedTurns.isEmpty && !after.effects.heroCursed)
    },

    test("бьёт на три места: союзник напротив — удар ему, мимо брони, и проклятие на нём") {
      val h    = hero()
      val ally = sturdyAlly(1).copy(armor = 5000L, stats = sturdyAlly(1).stats.copy(armor = 5000L))
      // Герой вторым, напротив Некроманта — союзник.
      val base = necroBattle(h, turn = 0)
      val b    = base.copy(group = base.group.copy(heroPos = 2, activePos = 1, allies = List(ally)))
      for {
        r <- turn(h, b, tap("Wait"), ZIO.unit)
        (u, after, log) = r
        dealt = """получает (\d+) урона""".r.findFirstMatchIn(log).map(_.group(1).toLong).getOrElse(-1L)
        hit   = after.group.allyAt(1)
      } yield assertTrue(log.contains(s"Удар проклятой энергией: ${ally.name} получает ")) &&
              assertTrue(dealt > 0L && hit.exists(_.hp <= ally.hp - dealt)) &&
              // проклятие на союзнике, к концу раунда на раунд короче; на герое его нет
              assertTrue(hit.exists(_.cursed == HealCurse.Turns - 1)) &&
              assertTrue(after.effects.heroCursedTurns.isEmpty && u.fightStats.hp == h.fightStats.hp)
    },

    test("дальше трёх мест никого — удара нет, энергия цела, круг идёт") {
      val h    = hero()
      val base = necroBattle(h, turn = 0)
      // Он спрятался на пятом месте, герой на первом: до него четыре места.
      val b    = base.copy(group = base.group.copy(heroPos = 1, activePos = 5))
      for {
        r <- turn(h, b, tap("Wait"), ZIO.unit)
        (u, after, log) = r
      } yield assertTrue(!log.contains("Удар проклятой энергией") && u.fightStats.hp == h.fightStats.hp) &&
              assertTrue(after.effects.heroCursedTurns.isEmpty) &&
              assertTrue(after.bossTurn == 1 && after.monsterCurrentEnergy >= 500L)
    },

    test("проклятый союзник свою флягу не тратит: лечиться ей нечем") {
      // Союзник на третьем месте ранен, бить ему некого, броня цела — из
      // умений к месту только фляга. Без проклятия он её пьёт, под ним — нет.
      val h     = hero()
      val base  = necroBattle(h, turn = 1)
      // Энергии — под самый потолок: потраченное видно сразу.
      val hurt  = { val a = sturdyAlly(3); a.copy(hp = 50000L, energy = a.stats.energy) }
      def withAlly(a: BattleAlly) = base.copy(group = base.group.copy(allies = List(a)))
      for {
        free <- turn(h, withAlly(hurt), tap("Attack"), pairedSeed)
        held <- turn(h, withAlly(hurt.copy(cursed = 1)), tap("Attack"), pairedSeed)
      } yield assertTrue(free._2.group.allies.headOption.exists(_.hp > hurt.hp)) &&
              assertTrue(held._2.group.allies.headOption.exists(a => a.hp == hurt.hp && a.energy == hurt.energy))
    },

    test("смена пары и ход моба из строя геройских эффектов не снимают: проклятие, паутина, яд сетеплёта, грань аметиста") {
      val mine  = BattleEffects(heroCursedTurns = Some(2), heroWebTurns = Some(1), heroSkillJammed = Some(true),
                    heroTrueStrikeTurns = 2)
      val other = BattleEffects(monsterRootedTurns = 3)
      val both  = mine.withMonsterPart(other)
      assertTrue(both.heroCursedTurns.contains(2) && both.heroWebTurns.contains(1)) &&
      assertTrue(both.heroSkillJammed.contains(true) && both.heroTrueStrikeTurns == 2) &&
      assertTrue(both.monsterRootedTurns == 3)
    },

    test("созидание поднимает мертвеца уровнем пещеры, а не своим") {
      val h = hero()
      for {
        r <- turn(h, necroBattle(h, turn = 2), tap("Attack"), pairedSeed)
        (_, after, log) = r
      } yield assertTrue(log.contains("из земли выбирается")) &&
              assertTrue(after.group.others.exists(s => s.lvl == 20L && s.undead)) &&
              assertTrue(!after.group.others.exists(_.lvl == bossLvl))
    },

    test("экран боя: проклятие — 🖤 у HP, а обещанного регена под ним нет") {
      val h     = wounded(hero())
      val base  = withEffects(necroBattle(h, turn = 1))(_.copy(heroRegen = Some(Regen(5))))
      for {
        calm   <- screenOf(h, base)
        cursed <- screenOf(h, withEffects(base)(_.copy(heroCursedTurns = Some(2))))
      } yield assertTrue(!calm.contains(HealCurse.Icon) && calm.contains("(+")) &&
              assertTrue(cursed.contains(HealCurse.Icon) && !cursed.contains("(+"))
    },

    test("старые записи читаются: без проклятия в эффектах и на союзнике — его нет") {
      val effects = BattleEffects(heroWebTurns = Some(1)).asJson.mapObject(_.remove("heroCursedTurns"))
      val ally    = sturdyAlly(1).asJson.mapObject(_.remove("cursed"))
      assertTrue(effects.as[BattleEffects].exists(e => e.heroCursedTurns.isEmpty && e.heroWebbed)) &&
      assertTrue(ally.as[BattleAlly].exists(_.cursed == 0))
    }
  )
}
