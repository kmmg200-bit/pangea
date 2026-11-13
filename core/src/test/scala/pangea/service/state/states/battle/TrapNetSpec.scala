package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, TrapNet}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Ловчая сеть в доп. слоте: бросок не дальше двух мест от героя, опутанный
  * враг раунд не сходит с места; раз за раунд, и раунд бросок не завершает; на
  * третьем броске сеть рвётся. */
object TrapNetSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))

  private val lvl = 10L

  private def net(charges: Int): Item =
    TrapNet.item.copy(id = 7L, details = ItemDetails.Net(charges, TrapNet.Charges))

  private def hero(charges: Int = TrapNet.Charges): Hero = {
    val h = TestFixtures.hero(userId)
    h.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 20, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = h.baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(additionalWeapon = net(charges)))
  }

  private val monster: Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = 20, hp = 100000L, armor = 100, defence = 100, evasion = 0, accuracy = 9999, energy = 0))

  /** Строй: первый моб в паре с героем на первом месте, остальные — на `places`. */
  private def battle(h: Hero, places: List[Int]): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(List.fill(places.size + 1)(monster), h, Nil)
    b.copy(group = b.group.copy(places = places))
  }

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  /** Счётчик на опутанном: раунд плюс тик запаса (см. `BattleState.netAt`). */
  private val held = TrapNet.HoldRounds + 1

  override def spec = suite("Ловчая сеть")(

    test("кнопка сети — над «Сбежать», с числом оставшихся бросков") {
      val h = hero()
      for {
        t <- makeState(h, battle(h, List(2)))
        (state, _, r) = t
        _   <- state.enter(testUser, r)
        scr <- r.sentScreens.map(_.last)
        netBtn  = scr.choices.find(_.id == "ThrowNet")
        fleeBtn = scr.choices.find(_.id == "Flee")
      } yield assertTrue(netBtn.exists(_.label == "🕸 Ловчая сеть (3)")) &&
              assertTrue(netBtn.flatMap(_.row).exists(row => fleeBtn.flatMap(_.row).exists(_ > row))) &&
              assertTrue(scr.choices.forall(_.id != "UseDivine"))
    },

    test("в двух шагах один враг — сеть летит в него сразу: раунд он не сойдёт с места, бросков 2, раунд не кончился") {
      val h = hero()
      for {
        t <- makeState(h, battle(h, List(4)))
        (state, dao, r) = t
        result  <- state.action(testUser, tap("ThrowNet"), r)
        after   <- battleOf(dao)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        farFree  = after.monsterAt(4).exists(!_.effects.monsterRooted)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(after.effects.monsterRootedTurns == held && farFree) &&
              assertTrue(updated.equipment.additionalWeapon.net.contains(ItemDetails.Net(2, TrapNet.Charges))) &&
              assertTrue(after.divineUsedThisRound && after.group.round == 0) &&
              assertTrue(screens.contains("🕸 Ловчая сеть → ") && screens.contains("опутан, раунд не сойдёт с места."))
    },

    test("второй бросок за раунд не проходит, бросок не тратится; в новом раунде — снова можно") {
      val h = hero()
      for {
        t <- makeState(h, battle(h, List(4)))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("ThrowNet"), r)
        _       <- state.action(testUser, tap("ThrowNet"), r)
        twice   <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        _       <- TestRandom.feedInts(60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, aimed("Attack", 1), r)
        // за раунд дальний подошёл и тоже в двух шагах — цель называем сами
        _       <- state.action(testUser, aimed("ThrowNet", 1), r)
        third   <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(screens.contains("второй раз за раунд её не бросить")) &&
              assertTrue(twice.equipment.additionalWeapon.net.exists(_.charges == 2)) &&
              assertTrue(third.equipment.additionalWeapon.net.exists(_.charges == 1))
    },

    test("последний бросок рвёт сеть: доп. слот пустеет") {
      val h = hero(charges = 1)
      for {
        t <- makeState(h, battle(h, List(4)))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("ThrowNet"), r)
        after   <- battleOf(dao)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.effects.monsterRootedTurns == held) &&
              assertTrue(updated.equipment.additionalWeapon.itemType == ItemType.NoItem) &&
              assertTrue(screens.contains("🕸 Ловчая сеть порвалась — бросать больше нечего."))
    },

    test("врагов в двух шагах несколько — спросит, в кого; дальше двух не предлагает и не бросает") {
      val h = hero()
      for {
        t <- makeState(h, battle(h, List(2, 3, 4)))
        (state, dao, r) = t
        _       <- state.action(testUser, tap("ThrowNet"), r)
        ask     <- r.sentScreens.map(_.last)
        offered  = ask.choices.filter(_.id == "ThrowNet").flatMap(_.data.get("target")).toSet
        _       <- state.action(testUser, aimed("ThrowNet", 4), r)
        far     <- dao.getHeroByUserId(userId).map(_.get)
        _       <- state.action(testUser, aimed("ThrowNet", 3), r)
        after   <- battleOf(dao)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        sideHeld = after.monsterAt(3).exists(_.effects.monsterRootedTurns == held)
        restFree = !after.effects.monsterRooted && after.monsterAt(2).exists(!_.effects.monsterRooted)
      } yield assertTrue(offered == Set("1", "2", "3")) &&
              assertTrue(far.equipment.additionalWeapon.net.exists(_.charges == 3)) &&
              assertTrue(sideHeld && restFree) &&
              assertTrue(updated.equipment.additionalWeapon.net.exists(_.charges == 2))
    },

    test("опутанный в строю раунд стоит на месте, а неопутанный подходит к герою") {
      val h = hero()
      for {
        tn <- makeState(h, battle(h, List(3)))
        (netted, ndao, nr) = tn
        _  <- netted.action(testUser, aimed("ThrowNet", 3), nr)
        _  <- TestRandom.feedInts(60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _  <- netted.action(testUser, aimed("Attack", 1), nr)
        withNet <- battleOf(ndao)
        tf <- makeState(h, battle(h, List(3)))
        (free, fdao, fr) = tf
        _  <- TestRandom.feedInts(60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _  <- free.action(testUser, aimed("Attack", 1), fr)
        noNet <- battleOf(fdao)
      } yield assertTrue(withNet.group.round == 1 && noNet.group.round == 1) &&
              assertTrue(withNet.group.occupied(3) && !withNet.group.occupied(2)) &&
              assertTrue(noNet.group.occupied(2) && !noNet.group.occupied(3))
    }
  )
}
