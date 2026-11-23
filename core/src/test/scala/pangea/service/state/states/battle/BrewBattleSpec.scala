package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleEffects, MonsterSlot, SoloPveBattle}
import pangea.model.hero.{Hero, WeaponDust}
import pangea.model.item.{BrewKind, BrewRates, Item, ItemType}
import pangea.test.TestFixtures
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats, ParamsBuff, StatBoost, StatBoosts}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.schedule.Scheduler
import pangea.service.state.UserAction
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Отвары, которые работают в бою: грибная смесь из доп. слота, зеркальный
  * настой подставляет под удар копии. */
object BrewBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val lvl = 10L

  private def hero(dust: WeaponDust = WeaponDust.empty, offhand: Item = Item.NoItem): Hero =
    TestFixtures.hero(userId, state = StateType.Battle).copy(
      lvl        = lvl,
      baseStats  = BaseStats(agi = 10, vit = 10, str = 10, int = 10),
      fightStats = FightStats(atk = 20, hp = 5000, armor = 500, defence = 10, evasion = 5, accuracy = 20, energy = 100),
      weaponDust = dust,
      equipment  = TestFixtures.emptyEquipment.copy(additionalWeapon = offhand))

  private def monster(hp: Long, atk: Long = 100L): Monster =
    Monster(0L, lvl, Race.Goblin, Rarity.Common,
      FightStats(atk = atk, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 10000, energy = 0))

  /** Приговорённый — именной, со своим именем — и сородич, которого он позвал. */
  private val PreyName = "Грызь Кривозуб"
  private def named(hp: Long): Monster = monster(hp, atk = 1L).copy(rarity = Rarity.Legendary, customName = Some(PreyName))
  private val kinStats = FightStats(atk = 1, hp = 1000, armor = 0, defence = 0, evasion = 0, accuracy = 10000, energy = 0)
  private val kinSlot  = MonsterSlot(lvl, Race.Goblin.entryName, Rarity.Common.entryName, kinStats, 1000L, 0L,
    marked = false, currentEnergy = 0L, effects = BattleEffects.empty)
  private val preySlot = MonsterSlot(lvl, Race.Goblin.entryName, Rarity.Legendary.entryName, kinStats.copy(hp = 100000),
    100000L, 0L, marked = false, currentEnergy = 0L, effects = BattleEffects.empty, customName = Some(PreyName))

  private def makeState(h: Hero, battle: SoloPveBattle, bag: List[Item] = Nil) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeActiveBattle(userId, battle.asJson)
      inv  = TestInventoryRepository.withItems(bag)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, inv, TestItemRepository.make, c, Scheduler.none), dao, inv, r)

  private def battleOf(dao: TestHeroDao): Task[SoloPveBattle] =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def bubbled: Hero =
    hero().copy(
      fightStats = hero().fightStats.copy(hp = 1L, armor = 0L),
      statBoosts = StatBoosts.none.add(StatBoost(BrewRates.BubbleBoost, ParamsBuff.zero, Long.MaxValue), 0L))

  override def spec = suite("Отвары в бою")(

    test("пузырьковый нектар отменяет смерть, оставляет один HP и на этом кончается") {
      val deadly = monster(100000L, atk = 100000L)
      for {
        t <- makeState(bubbled, SoloPveBattle.from(deadly, bubbled))
        (state, dao, _, r) = t
        // герой промахивается, моб бьёт наверняка — такой удар должен был добить
        _     <- TestRandom.feedInts(1, 99) *> TestRandom.feedLongs(100L, 100L)
        res   <- state.action(testUser, tap("Attack"), r)
        hero  <- dao.getHeroByUserId(userId).map(_.get)
        said  <- texts(r)
      } yield assertTrue(res == StateType.Battle && said.contains("🫧 Плёнка лопнула: смертельный удар не прошёл, у вас 1 HP.")) &&
              assertTrue(hero.fightStats.hp == 1L) &&
              // плёнка лопнула: второй раз она не выручит
              assertTrue(!hero.statBoosts.hasActive(BrewRates.BubbleBoost, 0L))
    },

    test("приговорённый уходит, если не добить его за отпущенные раунды") {
      val prey = SoloPveBattle.from(monster(100000L), hero()).copy(escapesAfter = 2)
      for {
        t <- makeState(hero(), prey)
        (state, dao, _, r) = t
        // первый раунд: он ещё здесь
        _     <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
        first <- state.action(testUser, tap("Attack"), r)
        alive <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        // второй — уходит
        _     <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
        gone  <- state.action(testUser, tap("Attack"), r)
        after <- dao.readActiveBattle(userId)
        said  <- texts(r)
      } yield assertTrue(first == StateType.Battle && alive.exists(_.group.round == 1)) &&
              assertTrue(gone == StateType.Dungeon && after.forall(_.isNull)) &&
              assertTrue(said.contains("уходит в темноту"))
    },

    test("приговорённый в паре уходит один: призванный им сородич встаёт к герою, бой идёт дальше") {
      val prey = SoloPveBattle.from(named(100000L), hero())
        .withReinforcement(kinSlot).copy(escapesAfter = 1, preyName = Some(PreyName), noKin = true)
      for {
        t <- makeState(hero(), prey)
        (state, dao, _, r) = t
        _     <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
        out   <- state.action(testUser, tap("Attack\",\"target\":\"1"), r)
        after <- battleOf(dao)
        said  <- texts(r)
      } yield assertTrue(out == StateType.Battle && said.contains("остаются драться")) &&
              assertTrue(after.customName.isEmpty && after.monsterCurrentHp == 1000L) &&
              assertTrue(!after.monstersInOrder.exists(_.customName.contains(PreyName))) &&
              // ушёл живым — в павших его нет, а срок снят
              assertTrue(after.group.slain.isEmpty && after.escapesAfter == 0)
    },

    test("приговорённый в строю сбоку уходит один — моб в паре остаётся") {
      val b = SoloPveBattle.from(monster(1000L, atk = 1L), hero())
        .withReinforcement(preySlot).copy(escapesAfter = 1, preyName = Some(PreyName), noKin = true)
      for {
        t <- makeState(hero(), b)
        (state, dao, _, r) = t
        _     <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
        out   <- state.action(testUser, tap("Attack\",\"target\":\"1"), r)
        after <- battleOf(dao)
        said  <- texts(r)
      } yield assertTrue(out == StateType.Battle && said.contains(PreyName) && said.contains("остаются драться")) &&
              assertTrue(after.group.others.isEmpty && after.monsterCurrentHp > 0L && after.group.slain.isEmpty)
    },

    test("приговорённый ушёл последним, а сородичей уже перебили — победа над ними, с него добычи нет") {
      val prey = SoloPveBattle.from(named(100000L), hero()).copy(escapesAfter = 1, preyName = Some(PreyName),
        noKin = true)
      val b = prey.copy(group = prey.group.copy(slain = List(kinSlot.slain)))
      for {
        t <- makeState(hero(), b)
        (state, dao, _, r) = t
        _     <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
        out   <- state.action(testUser, tap("Attack"), r)
        loot  <- dao.readSceneData(userId)
        said  <- texts(r)
      } yield assertTrue(out == StateType.Loot && said.contains("больше не ждёт")) &&
              // добыча — с одного павшего сородича, а не с приговорённого
              assertTrue(loot.exists(j => !j.noSpaces.contains(PreyName)))
    },

    test("грибная смесь: кнопка броска только со склянкой в доп. слоте") {
      val mix = BrewKind.item(BrewKind.MushroomMix).copy(id = 1L)
      val b   = SoloPveBattle.from(monster(1000L), hero())
      for {
        armed <- makeState(hero(offhand = mix), b, Nil)
        (s1, _, _, r1) = armed
        _      <- s1.enter(testUser, r1)
        on     <- r1.sentScreens.map(_.last)
        plain_ <- makeState(hero(), b, List(mix))
        (s2, _, _, r2) = plain_
        _      <- s2.enter(testUser, r2)
        off    <- r2.sentScreens.map(_.last)
      } yield assertTrue(on.choices.exists(c => c.id == "ThrowMix")) &&
              assertTrue(!off.choices.exists(_.id == "ThrowMix"))
    },

    test("бросок из доп. слота: бьёт всех в поле, травит и снимает смесь со слота") {
      val mix   = BrewKind.item(BrewKind.MushroomMix).copy(id = 1L)
      val group = SoloPveBattle.fromGroup(List(monster(100000L), monster(100000L)),
        hero(offhand = mix), List(0L, 0L))
      for {
        t <- makeState(hero(offhand = mix), group, Nil)
        (state, dao, _, r) = t
        _      <- state.action(testUser, tap("ThrowMix"), r)
        after  <- battleOf(dao)
        h      <- dao.getHeroByUserId(userId).map(_.get)
        blow    = BrewRates.MushroomDamagePerLvl * lvl
      } yield assertTrue(h.equipment.additionalWeapon.itemType == ItemType.NoItem) &&
              assertTrue(after.monsterCurrentHp == 100000L - blow) &&
              assertTrue(after.group.others.forall(_.currentHp == 100000L - blow)) &&
              assertTrue(after.effects.monsterPoison.isDefined) &&
              assertTrue(after.consumableUsedThisRound)
    },

    test("зеркальный настой: первые удары уходят в копии, потом бьют героя") {
      // Копии выпиты до боя — они приезжают в бой вместе с героем.
      val drunk = hero(WeaponDust.empty.copy(mirrors = 2))
      val b     = SoloPveBattle.from(monster(100000L), drunk)
      for {
        t <- makeState(drunk, b, Nil)
        (state, dao, _, r) = t
        // герой промахивается, моб бьёт в ответ: 1-й удар — в копию
        _      <- TestRandom.feedInts(1, 99) *> TestRandom.feedLongs(100L, 100L)
        _      <- state.action(testUser, tap("Attack"), r)
        first  <- dao.getHeroByUserId(userId).map(_.get)
        mid    <- battleOf(dao)
        said   <- texts(r)
        // 2-й удар — во вторую копию, 3-й достаётся уже герою
        _      <- TestRandom.feedInts(1, 99) *> TestRandom.feedLongs(100L, 100L)
        _      <- state.action(testUser, tap("Attack"), r)
        second <- dao.getHeroByUserId(userId).map(_.get)
        _      <- TestRandom.feedInts(1, 99) *> TestRandom.feedLongs(100L, 100L)
        _      <- state.action(testUser, tap("Attack"), r)
        third  <- dao.getHeroByUserId(userId).map(_.get)
        last   <- battleOf(dao)
      } yield assertTrue(said.contains(" бьёт копию. Копий осталось: ")) &&
              assertTrue(first.fightStats.hp == drunk.fightStats.hp && first.fightStats.armor == drunk.fightStats.armor) &&
              assertTrue(mid.effects.heroMirrors == 1) &&
              assertTrue(second.fightStats.hp == drunk.fightStats.hp) &&
              // копии кончились — удар дошёл
              assertTrue(last.effects.heroMirrors == 0) &&
              assertTrue(third.fightStats.armor < drunk.fightStats.armor || third.fightStats.hp < drunk.fightStats.hp)
    }
  )
}
