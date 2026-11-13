package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, Bleed, Burn, Formation, Poison, Regen, SoloPveBattle, Ward}
import pangea.model.hero.Hero
import pangea.model.squad.AllyKind
import pangea.model.item.{BeltSlot, BrewKind, BrewRates, FlaskKind, Item, ItemDetails, ItemType, MaterialKind, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.trauma.Trauma
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Отвары в ячейках пояса: в бою их достают из того же отдельного сообщения,
  * что фляги и пыль. Боевые отвары делают то, что делали склянки; прочие —
  * то же, что сделал бы глоток с карточки, только посреди сечи. */
object BeltBrewSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def useSlot(idx: Int): UserAction =
    UserAction("", Some(s"""{"action":"BeltUse","slot":"$idx"}"""))
  private def pick(idx: Int, what: String): UserAction =
    UserAction("", Some(s"""{"action":"BeltPick","slot":"$idx","pick":"$what"}"""))

  /** Пояс с ячейками: синий держит три. */
  private def belt(slots: BeltSlot*): Item =
    Item(9L, "Пояс", 1L, ItemRarity.Blue, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Belt(slots.toList))

  private def weapon: Item =
    Item(5L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  /** Герой с пробитым HP: лечить есть что, а в ответ ему не бьют — ход
    * расходником раунд не завершает. */
  private def hero(slots: BeltSlot*): Hero = {
    val h = TestFixtures.hero(userId)
    h.copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 50, hp = 10L, armor = 0, defence = 0,
                              evasion = 9999, accuracy = 9999, energy = 0),
      baseStats  = h.baseStats.copy(str = 1, vit = 100),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon, belt = belt(slots: _*)))
  }

  private def monster(hp: Long): Monster =
    Monster(0L, 10L, Race.Orc, Rarity.Common,
      FightStats(atk = 1, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  private def battle(h: Hero, hps: Long*): SoloPveBattle =
    SoloPveBattle.fromGroup(hps.toList.map(monster), h, Nil)

  private def makeState(h: Hero, b: SoloPveBattle,
                        inv: TestInventoryRepository = TestInventoryRepository.accepting) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, inv, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def slots(h: Hero): List[BeltSlot] = h.equipment.belt.details match {
    case b: ItemDetails.Belt => b.slots
    case _                   => Nil
  }

  override def spec = suite("Отвары на поясе")(

    test("зверобойный отвар лечит из ячейки; ячейка пустеет, но помнит отвар") {
      val h = hero(BeltSlot.Brew(BrewKind.WortDecoction))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _       <- state.action(testUser, useSlot(0), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.fightStats.hp > 10L) &&
              assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.WortDecoction, count = 0))) &&
              // брать из ячейки больше нечего — в списке пояса её нет
              assertTrue(BattleState.beltUsable(after).isEmpty) &&
              assertTrue(screens.contains(BrewKind.WortDecoction.label))
    },

    test("яростная настойка кладёт временный баф атаки на пять раундов") {
      val h = hero(BeltSlot.Brew(BrewKind.FuryTincture))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        after <- battleOf(dao)
        buffs  = after.heroBattleState.buffs
      } yield assertTrue(buffs.size == 1) &&
              assertTrue(buffs.head.atk > 0L) &&
              assertTrue(buffs.head.turnsLeft.contains(BrewRates.PotionBuffRounds))
    },

    test("жгучая вытяжка травит удары, дубильный настой чинит броню") {
      val hurt = hero(BeltSlot.Brew(BrewKind.StingingExtract), BeltSlot.Brew(BrewKind.TanningBrew))
      val dented = hurt.copy(fightStats = hurt.fightStats.copy(armor = 0L),
        equipment = hurt.equipment.copy(chestPlate =
          Item(6L, "Нагрудник", 1L, ItemRarity.Blue, ItemType.ChestPlate,
            attack = 0, accuracy = 0, energy = 0, armor = 200, defence = 0, evasion = 0)))
      for {
        t <- makeState(dented, battle(dented, 1000L))
        (state, dao, r) = t
        _      <- state.action(testUser, useSlot(0), r)
        poison <- battleOf(dao)
        // второй отвар — уже следующий раунд: бьём, чтобы раунд закрылся
        _      <- state.action(testUser, tap("Attack"), r)
        _      <- state.action(testUser, useSlot(1), r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(poison.effects.heroPoisonousAttacks) &&
              assertTrue(after.fightStats.armor > 0L)
    },

    test("костоправный отвар спрашивает, какую травму снять, и снимает её") {
      val hurt = hero(BeltSlot.Brew(BrewKind.BoneSetter)).copy(
        traumaUntil = Some(999999999L),
        traumaNames = List(Trauma.BruisedLeg.name, Trauma.SplitSkull.name))
      for {
        t <- makeState(hurt, battle(hurt, 1000L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        ask   <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, pick(0, Trauma.BruisedLeg.name), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield // вопрос — отдельным сообщением с кнопками внутри; тяжёлую не предлагают
              assertTrue(ask.inline && ask.choices.map(_.label) == List(Trauma.BruisedLeg.name)) &&
              // на вопрос ход не ушёл: склянка цела, пока травму не назвали
              assertTrue(after.traumaNames == List(Trauma.SplitSkull.name)) &&
              assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.BoneSetter, count = 0))) &&
              assertTrue(log.contains(Trauma.BruisedLeg.name))
    },

    test("лечить нечего — склянка цела, и раунд тоже") {
      val h = hero(BeltSlot.Brew(BrewKind.BoneSetter))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _      <- state.action(testUser, useSlot(0), r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
        fight  <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.BoneSetter))) &&
              assertTrue(!fight.consumableUsedThisRound) &&
              assertTrue(screens.contains("Лечить нечего"))
    },

    test("устаревший ответ склянку не тратит: отвар спросит снова") {
      val hurt = hero(BeltSlot.Brew(BrewKind.BoneSetter)).copy(
        traumaUntil = Some(999999999L), traumaNames = List(Trauma.BruisedLeg.name))
      for {
        t <- makeState(hurt, battle(hurt, 1000L))
        (state, dao, r) = t
        // такой травмы на герое нет — кнопка из старого сообщения
        _       <- state.action(testUser, pick(0, Trauma.BrokenNose.name), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        fight   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.BoneSetter))) &&
              assertTrue(after.traumaNames == List(Trauma.BruisedLeg.name)) &&
              assertTrue(!fight.consumableUsedThisRound) &&
              assertTrue(screens.contains("откройте пояс заново"))
    },

    test("часовой баф и быстрые отдыхи с пояса записываются вместе с ходом") {
      val h = hero(BeltSlot.Brew(BrewKind.ClearEye), BeltSlot.Brew(BrewKind.Invigorating))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        // второй отвар — уже следующий раунд: бьём, чтобы раунд закрылся
        _     <- state.action(testUser, tap("Attack"), r)
        _     <- state.action(testUser, useSlot(1), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        azat  <- dao.readAzatData(userId).map(_.flatMap(_.as[pangea.model.hero.AzatState].toOption))
      } yield assertTrue(after.statBoosts.hasActive("herb:agi", 0L)) &&
              assertTrue(azat.exists(_.instantRests == BrewRates.InstantRests)) &&
              assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.ClearEye, count = 0),
                                             BeltSlot.Brew(BrewKind.Invigorating, count = 0)))
    },

    test("грибная смесь из ячейки достаётся всем, кто стоит в поле") {
      val h = hero(BeltSlot.Brew(BrewKind.MushroomMix))
      for {
        t <- makeState(h, battle(h, 100000L, 100000L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        after <- battleOf(dao)
        blow   = BrewRates.MushroomDamagePerLvl * 10L
      } yield assertTrue(after.monsterCurrentHp == 100000L - blow) &&
              assertTrue(after.group.others.head.currentHp == 100000L - blow) &&
              assertTrue(after.effects.monsterPoison.isDefined)
    },

    test("живая вода спрашивает, какую флягу заправить, и заправляет выбранную, а не первую") {
      val full = FlaskKind.chargesFor(ItemRarity.Gray)
      val h = hero(BeltSlot.Brew(BrewKind.LivingWater),
                   BeltSlot.Flask(FlaskKind.Healer, ItemRarity.Gray, 1),
                   BeltSlot.Flask(FlaskKind.Smith, ItemRarity.Gray, 0))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        ask   <- r.sentScreens.map(_.last)
        asked <- dao.getHeroByUserId(userId).map(_.get)
        mid   <- battleOf(dao)
        _     <- state.action(testUser, pick(0, "2"), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        fight <- battleOf(dao)
      } yield // вопрос — отдельным сообщением; фляги с номером ячейки и зарядами
              assertTrue(ask.inline && ask.text.contains("Какую флягу заправить")) &&
              assertTrue(ask.choices.map(_.label) == List(
                s"2. ${FlaskKind.Healer.label} — 1/$full", s"3. ${FlaskKind.Smith.label} — 0/$full")) &&
              // на вопрос ни склянка, ни бонусное действие не ушли
              assertTrue(slots(asked) == slots(h) && !mid.consumableUsedThisRound) &&
              // заправлена фляга кузнеца из третьей ячейки, целителя — как была
              assertTrue(slots(after) == List(
                BeltSlot.Brew(BrewKind.LivingWater, count = 0),
                BeltSlot.Flask(FlaskKind.Healer, ItemRarity.Gray, 1),
                BeltSlot.Flask(FlaskKind.Smith, ItemRarity.Gray, full))) &&
              // заправка — бонусное действие раунда, как и всё с пояса
              assertTrue(fight.consumableUsedThisRound)
    },

    test("живая вода: заправлять некуда (полная фляга, фляга не того толка) — склянка цела, и раунд тоже") {
      val full = FlaskKind.chargesFor(ItemRarity.Gray)
      val h = hero(BeltSlot.Brew(BrewKind.LivingWater),
                   BeltSlot.Flask(FlaskKind.Poison, ItemRarity.Gray, 0),
                   BeltSlot.Flask(FlaskKind.Healer, ItemRarity.Gray, full))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _       <- state.action(testUser, useSlot(0), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        fight   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(after) == slots(h) && !fight.consumableUsedThisRound) &&
              assertTrue(screens.contains("Заправлять некуда"))
    },

    test("живая вода: кнопка устарела (фляга уже полна) — склянку не тратит, спросит снова") {
      val full = FlaskKind.chargesFor(ItemRarity.Gray)
      val h = hero(BeltSlot.Brew(BrewKind.LivingWater),
                   BeltSlot.Flask(FlaskKind.Healer, ItemRarity.Gray, full))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _       <- state.action(testUser, pick(0, "1"), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        fight   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(after) == slots(h) && !fight.consumableUsedThisRound) &&
              assertTrue(screens.contains("откройте пояс заново"))
    },

    test("пустая ячейка номеров не сдвигает: кнопка фляги знает своё место") {
      val h = hero(BeltSlot.Dust(MaterialKind.RubyDust),
                   BeltSlot.Flask(FlaskKind.Healer, ItemRarity.Gray, 2))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, _, r) = t
        // горсть ушла на оружие — первая ячейка опустела
        _    <- state.action(testUser, useSlot(0), r)
        list <- r.sentScreens.map(_.last)
      } yield assertTrue(list.choices.size == 1) &&
              assertTrue(list.text.contains("2. " + FlaskKind.Healer.label + " — 2")) &&
              // высыпанная горсть видна пустой и без кнопки
              assertTrue(list.text.contains("1. " + MaterialKind.RubyDust.displayName + " — пусто")) &&
              // кнопка несёт НАСТОЯЩИЙ номер ячейки, а не номер строки в списке
              assertTrue(list.choices.head.data.get("slot").contains("1"))
    },

    test("стопка: глоток тратит одну склянку, в списке — сколько из скольких; после боя доливается до полной") {
      // синий пояс: три ячейки — и по три склянки в ячейке
      val h     = hero(BeltSlot.Brew(BrewKind.WortDecoction, 3))
      val spare = (1L to 4L).toList.map(i => BrewKind.item(BrewKind.WortDecoction).copy(id = 40L + i))
      val inv   = TestInventoryRepository.withItems(spare)
      for {
        t <- makeState(h, battle(h, 1L), inv)
        (state, dao, r) = t
        _       <- state.action(testUser, tap("Belt"), r)
        list    <- r.sentScreens.map(_.last)
        _       <- state.action(testUser, useSlot(0), r)
        mid     <- dao.getHeroByUserId(userId).map(_.get)
        // моб с 1 HP: добьём его, бой кончится — пояс доберёт из сумки
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(list.text.contains(s"1. ${BrewKind.WortDecoction.label} — 3/3")) &&
              assertTrue(slots(mid) == List(BeltSlot.Brew(BrewKind.WortDecoction, 2))) &&
              assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.WortDecoction, 3))) &&
              assertTrue(inv.snapshot.size == 3) &&
              assertTrue(screens.contains("На пояс легло из сумки"))
    },

    test("после боя пустая ячейка сама берёт такой же отвар из сумки") {
      val h     = hero(BeltSlot.Brew(BrewKind.WortDecoction))
      val spare = BrewKind.item(BrewKind.WortDecoction).copy(id = 42L)
      val inv   = TestInventoryRepository.withItems(List(spare))
      for {
        // моб с 1 HP: добьём его атакой, бой кончится — пояс доберёт из сумки
        t <- makeState(h, battle(h, 1L), inv)
        (state, dao, r) = t
        _       <- state.action(testUser, useSlot(0), r)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.WortDecoction))) &&
              assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(screens.contains("На пояс легло из сумки"))
    },

    test("гнилой зов из ячейки пояса ставит запах, а к кому он ведёт — не говорит") {
      val h = hero(BeltSlot.Brew(BrewKind.RotCall))
      for {
        t <- makeState(h, battle(h, 1000L))
        (state, dao, r) = t
        _       <- state.action(testUser, useSlot(0), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.statBoosts.hasActive(BrewRates.RotCallBoost, 0L)) &&
              assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.RotCall, count = 0))) &&
              assertTrue(screens.contains("Узелок развязан") && !screens.contains("Джо"))
    },

    test("мерцающий настой показывает все свободные места, а в конце раунда ставит героя на выбранное") {
      val h = hero(BeltSlot.Brew(BrewKind.ShimmerBrew))
      for {
        t <- makeState(h, battle(h, 100000L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        ask   <- r.sentScreens.map(_.last)
        start <- battleOf(dao)
        _     <- state.action(testUser, pick(0, "5"), r)
        mid   <- battleOf(dao)
        midH  <- dao.getHeroByUserId(userId).map(_.get)
        // раунд закрывает удар — шаг случается в его конце
        _     <- state.action(testUser, tap("Attack"), r)
        after <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        free   = (1 to Formation.HeroPlaces).filterNot(_ == start.group.heroPos).map(p => s"Место $p").toList
      } yield // вопрос — все свободные места своей стороны; ход на него не ушёл
              assertTrue(ask.inline && ask.choices.map(_.label) == free) &&
              assertTrue(!start.consumableUsedThisRound) &&
              // выпит: место запомнено, но герой пока стоит где стоял
              assertTrue(mid.group.pendingShift.contains(5) && mid.group.heroPos == start.group.heroPos) &&
              assertTrue(slots(midH) == List(BeltSlot.Brew(BrewKind.ShimmerBrew, count = 0))) &&
              assertTrue(after.group.heroPos == 5 && after.group.pendingShift.isEmpty) &&
              assertTrue(log.contains("проявляетесь на месте 5"))
    },

    test("мерцающий настой: место заняли — герой встаёт на ближайшее свободное") {
      val h   = hero()
      // мобы на местах 1–6: крысе на пятом есть кого бить, и она стоит
      val b0  = battle(h, List.fill(6)(100000L): _*)
      val rat = BattleAlly(AllyKind.Rat, 5, 10L, 0L, 0L, FightStats(1, 10, 0, 0, 0, 1, 0), 1L)
      val b   = b0.copy(group = b0.group.copy(allies = List(rat), pendingShift = Some(5)))
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        // в досягаемости двое — бьём того, кто в паре, раунд закрывается
        _     <- state.action(testUser, UserAction("", Some(s"""{"action":"Attack","target":"${b.group.heroPos}"}""")), r)
        after <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield // четвёртое и шестое одинаково близко — встаёт правее
              assertTrue(after.group.heroPos == 6) &&
              assertTrue(log.contains("Место 5 заняли"))
    },

    test("защитные отвары: снимают своё с героя и держат до конца боя; вторая склянка того же толка не тратится") {
      val h  = hero(BeltSlot.Brew(BrewKind.SnakeSerum), BeltSlot.Brew(BrewKind.SnakeSerum))
      val b0 = battle(h, 100000L)
      val b  = b0.copy(effects = b0.effects.copy(heroPoison = Some(Poison(6))))
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        _      <- state.action(testUser, useSlot(0), r)
        mid    <- battleOf(dao)
        // следующий раунд: бьём, чтобы раунд закрылся, — и пробуем вторую склянку
        _      <- state.action(testUser, tap("Attack"), r)
        _      <- state.action(testUser, useSlot(1), r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
        fight  <- battleOf(dao)
        log    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(mid.effects.heroPoison.isEmpty) &&
              assertTrue(mid.effects.warded(Ward.Poison) && !mid.effects.warded(Ward.Bleed)) &&
              assertTrue(log.contains("яд снят, до конца боя новый не возьмёт")) &&
              assertTrue(log.contains("и так на вас")) &&
              assertTrue(slots(after) == List(BeltSlot.Brew(BrewKind.SnakeSerum, 0), BeltSlot.Brew(BrewKind.SnakeSerum))) &&
              assertTrue(fight.effects.warded(Ward.Poison))
    },

    test("паучья сыворотка: с пояса снимает паутину, яд и сорванное умение и держит до конца боя") {
      val h  = hero(BeltSlot.Brew(BrewKind.SpiderSerum))
      val b0 = battle(h, 100000L)
      val b  = b0.copy(effects = b0.effects.copy(heroWebTurns = Some(2), heroPoison = Some(Poison(6)),
                 heroSkillJammed = Some(true)))
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        _   <- state.action(testUser, useSlot(0), r)
        mid <- battleOf(dao)
        log <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(mid.effects.heroWebTurns.isEmpty && mid.effects.heroPoison.isEmpty && !mid.effects.skillJammed) &&
              assertTrue(mid.effects.warded(Ward.Spider) && !mid.effects.warded(Ward.Poison)) &&
              assertTrue(log.contains("паутина и паучий яд сняты, до конца боя сетеплёты их не наложат")) &&
              assertTrue(BrewKind.SpiderSerum.combat && BrewKind.SpiderSerum.beltable && !BrewKind.SpiderSerum.drinkable &&
                BrewKind.SpiderSerum.portions == 1)
    },

    test("каждая защита снимает своё: огонь, холод, кровь") {
      val fx = pangea.model.battle.BattleEffects(heroPoison = Some(Poison(6)), heroBurn = Some(Burn(5)),
                 heroColdDefenceCut = 10, heroBleed = Some(Bleed(4)))
      assertTrue(fx.withWard(Ward.Fire).heroBurn.isEmpty && fx.withWard(Ward.Fire).heroPoison.isDefined) &&
      assertTrue(fx.withWard(Ward.Cold).heroColdDefenceCut == 0 && !fx.withWard(Ward.Cold).coldOnHero) &&
      assertTrue(fx.withWard(Ward.Bleed).heroBleed.isEmpty) &&
      // защиты копятся, и при смене пары остаются при герое
      assertTrue(fx.withWard(Ward.Fire).withWard(Ward.Cold).heroWards.contains(Set[Ward](Ward.Fire, Ward.Cold))) &&
      assertTrue(fx.withWard(Ward.Bleed).heroPart.warded(Ward.Bleed)) &&
      // все четыре — боевые, из пояса, на редкой траве: по одной склянке за рецепт
      assertTrue(List(BrewKind.SnakeSerum, BrewKind.FireproofBrew, BrewKind.WarmBlood, BrewKind.ClotBrew)
        .forall(k => k.combat && k.beltable && !k.drinkable && k.portions == 1))
    },

    test("великанье зелье: предел здоровья +20% до конца боя, прибавка налита сразу; второе не тратится; после боя сходит") {
      val h  = hero(BeltSlot.Brew(BrewKind.GiantBrew), BeltSlot.Brew(BrewKind.GiantBrew))
      val max0 = h.effectiveMaxHp(0L)
      for {
        // моб с 1 HP: после глотка добьём его — бой кончится
        t <- makeState(h, battle(h, 1L))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        mid   <- dao.getHeroByUserId(userId).map(_.get)
        _     <- state.action(testUser, useSlot(1), r)
        twice <- dao.getHeroByUserId(userId).map(_.get)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        _     <- state.action(testUser, tap("Attack"), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(mid.effectiveMaxHp(0L) == max0 * (100L + BrewRates.GiantHpPct) / 100L) &&
              assertTrue(mid.fightStats.hp == h.fightStats.hp + (mid.effectiveMaxHp(0L) - max0)) &&
              assertTrue(log.contains("к наибольшему здоровью до конца боя")) &&
              // второе — то же бонусное действие раунда: склянка цела
              assertTrue(slots(twice).lift(1).contains(BeltSlot.Brew(BrewKind.GiantBrew))) &&
              // бой кончился: баф снят, здоровье не выше обычного предела
              assertTrue(!after.statBoosts.boosts.exists(_.name == BrewRates.GiantBoost)) &&
              assertTrue(after.fightStats.hp <= after.effectiveMaxHp(0L) && after.effectiveMaxHp(0L) == max0)
    },

    test("отвар лунного побега: 12% в раунд пять раундов, не слабея, и сходит разом") {
      val h = hero(BeltSlot.Brew(BrewKind.MoonBrew))
      val steps = Iterator.iterate(Option(Regen(BrewRates.MoonRegenPct, Some(BrewRates.MoonRegenRounds))))(_.flatMap(_.decayed))
                    .takeWhile(_.isDefined).flatten.toList
      for {
        t <- makeState(h, battle(h, 100000L))
        (state, dao, r) = t
        _   <- state.action(testUser, useSlot(0), r)
        mid <- battleOf(dao)
        log <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(mid.effects.heroRegen.contains(Regen(12, Some(5)))) &&
              assertTrue(log.contains("+12% HP в раунд, 5 раундов.")) &&
              // пять раундов лечения, все по 12%
              assertTrue(steps.size == 5 && steps.forall(_.pct == 12)) &&
              // живучий сбор слабеет, как раньше
              assertTrue(Regen.onDrink.decayed.contains(Regen(Regen.OnDrink - Regen.DecayPerRound))) &&
              assertTrue(List(BrewKind.GiantBrew, BrewKind.MoonBrew).forall(k => k.combat && k.portions == 1))
    },

    test("регенерация, записанная в бою до лунного побега, читается и слабеет по-старому") {
      val old = io.circe.Json.obj("pct" -> 10.asJson)
      assertTrue(old.as[Regen] == Right(Regen(10))) &&
      assertTrue(old.as[Regen].toOption.flatMap(_.decayed).contains(Regen(8)))
    },

    test("бой, записанный до защит, читается: защит нет") {
      val h    = hero()
      val json = battle(h, 1000L).asJson.hcursor.downField("effects").downField("heroWards").delete.top.get
      assertTrue(json.as[SoloPveBattle].toOption.exists(_.effects.heroWards.isEmpty))
    },

    test("мерцающий настой: пригвождённому с места не сойти — склянка цела, и раунд тоже") {
      val h  = hero(BeltSlot.Brew(BrewKind.ShimmerBrew))
      val b0 = battle(h, 100000L)
      val b  = b0.copy(effects = b0.effects.copy(heroRootedTurns = 2))
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        _       <- state.action(testUser, useSlot(0), r)
        after   <- dao.getHeroByUserId(userId).map(_.get)
        fight   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(slots(after) == slots(h) && !fight.consumableUsedThisRound) &&
              assertTrue(screens.contains("пригвождены"))
    },

    test("шнапс, самогон, сонный дурман и приговор в ячейку не ложатся") {
      assertTrue(BeltSlot.of(BrewKind.item(BrewKind.Schnapps)).isEmpty) &&
      assertTrue(BeltSlot.of(BrewKind.item(BrewKind.Moonshine)).isEmpty) &&
      assertTrue(BeltSlot.of(BrewKind.item(BrewKind.SleepingDope)).isEmpty) &&
      // приговор пьют до боя: с кем встретиться, решают не посреди сечи
      assertTrue(BeltSlot.of(BrewKind.item(BrewKind.Sentence)).isEmpty) &&
      // а всё прочее ложится
      assertTrue(BrewKind.values.filter(_.beltable)
        .forall(k => BeltSlot.of(BrewKind.item(k)).contains(BeltSlot.Brew(k))))
    }
  )
}
