package pangea.service.state.states.dungeon

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{BattleEffects, Bleed, Burn, Poison, SoloPveBattle}
import pangea.model.monster.Race
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer, TestScheduler}
import zio.test._
import zio.{Duration, ZIO}

/** Дракон глубин: на 26–50-м уровне лабиринта любой осмотр и выслеживание
  * выводят на предупреждение стражи — «Напасть» или «Уйти» (на 25-й). */
object DragonLairSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState(level: Int, max: Int = 150) =
    for {
      heroDao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, dungeonLevel = level, maxDungeonLevel = max))
      renderer  <- TestRenderer.make
      scheduler <- TestScheduler.make
      content   <- ZIO.attempt(SceneContent.load())
    } yield (DungeonState(heroDao, TestInventoryRepository.accepting, scheduler, content), heroDao, renderer, scheduler)

  private val Warning = "Стражники видели здесь дракона"

  private def battleOf(heroDao: TestHeroDao) =
    heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  override def spec = suite("Дракон глубин")(

    test("логово — с 26-го по 50-й уровень включительно") {
      assertTrue(!DungeonState.dragonLair(25) && DungeonState.dragonLair(26) &&
        DungeonState.dragonLair(50) && !DungeonState.dragonLair(51))
    },

    test("«Исследовать» на 26-м и 50-м — предупреждение стражи с «Напасть» и «Уйти», боя ещё нет") {
      ZIO.foreach(List(26, 50)) { level =>
        for {
          q <- makeState(level)
          (state, heroDao, renderer, _) = q
          res     <- state.action(testUser, tap("FindEvent"), renderer)
          screens <- renderer.sentScreens
          battle  <- battleOf(heroDao)
        } yield assertTrue(res == StateType.Dungeon) &&
                assertTrue(screens.size == 1 && screens.head.text.contains(Warning)) &&
                assertTrue(screens.head.choices.map(_.id) == List("DragonAttack", "DragonLeave")) &&
                assertTrue(battle.isEmpty)
      }.map(_.reduce(_ && _))
    },

    test("на 25-м и 51-м дракона нет — обычный осмотр") {
      ZIO.foreach(List(25, 51)) { level =>
        for {
          q <- makeState(level)
          (state, _, renderer, _) = q
          _       <- state.action(testUser, tap("FindEvent"), renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(!screens.exists(_.text.contains(Warning))) &&
                assertTrue(screens.exists(_.text.contains("коридорам")))
      }.map(_.reduce(_ && _))
    },

    test("дракон раньше обязательных встреч: волчий зов на 30-м не срабатывает и не сгорает") {
      import pangea.model.item.BrewRates
      import pangea.model.stats.{ParamsBuff, StatBoost, StatBoosts}
      val called = TestFixtures.hero(userId, dungeonLevel = 30, maxDungeonLevel = 150).copy(statBoosts =
        StatBoosts.none.add(StatBoost(BrewRates.WolfCallBoost, ParamsBuff.zero, BrewRates.WolfCallMs), 0L))
      for {
        heroDao   <- TestHeroDao.withHero(userId, called)
        renderer  <- TestRenderer.make
        scheduler <- TestScheduler.make
        content   <- ZIO.attempt(SceneContent.load())
        state      = DungeonState(heroDao, TestInventoryRepository.accepting, scheduler, content)
        _       <- state.action(testUser, tap("FindEvent"), renderer)
        screens <- renderer.sentScreens
        hero    <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(screens.exists(_.text.contains(Warning))) &&
              assertTrue(hero.statBoosts.hasActive(BrewRates.WolfCallBoost, 0L))
    },

    test("«Уйти» — герой на 25-м уровне, экран этажа") {
      for {
        q <- makeState(37)
        (state, heroDao, renderer, _) = q
        res     <- state.action(testUser, tap("DragonLeave"), renderer)
        hero    <- heroDao.getHeroByUserId(userId).map(_.get)
        screens <- renderer.sentScreens
      } yield assertTrue(res == StateType.Dungeon && hero.dungeonLevel == 25) &&
              assertTrue(screens.exists(_.text.contains("вернулись на 25-й уровень"))) &&
              assertTrue(screens.last.text.contains("Уровень: 25") && screens.last.choices.exists(_.id == "FindEvent"))
    },

    test("«Напасть» — бой с драконом: все показатели по 100 000, сородичей не зовёт") {
      for {
        q <- makeState(33)
        (state, heroDao, renderer, _) = q
        res    <- state.action(testUser, tap("DragonAttack"), renderer)
        battle <- battleOf(heroDao)
        scene  <- heroDao.readSceneData(userId)
        s       = battle.map(_.monsterStats)
      } yield assertTrue(res == StateType.Battle) &&
              assertTrue(battle.exists(b => b.monsterRace == Race.Dragon.entryName && b.monsterName == "Дракон" &&
                b.monsterLvl == 33L && b.noKin && b.monsterCurrentHp == 100000L &&
                b.monsterCurrentArmor == 100000L && b.monsterCurrentEnergy == 100000L && !b.cantFlee)) &&
              assertTrue(s.exists(f => List(f.atk, f.hp, f.armor, f.defence, f.evasion, f.accuracy, f.energy)
                .forall(_ == 100000L))) &&
              assertTrue(scene.contains(io.circe.Json.Null))
    },

    test("старые кнопки дракона вне логова — просто экран этажа: ни боя, ни переноса") {
      for {
        q <- makeState(20)
        (state, heroDao, renderer, _) = q
        a       <- state.action(testUser, tap("DragonAttack"), renderer)
        l       <- state.action(testUser, tap("DragonLeave"), renderer)
        battle  <- battleOf(heroDao)
        hero    <- heroDao.getHeroByUserId(userId).map(_.get)
        screens <- renderer.sentScreens
      } yield assertTrue(a == StateType.Dungeon && l == StateType.Dungeon) &&
              assertTrue(battle.isEmpty && hero.dungeonLevel == 20) &&
              assertTrue(screens.forall(_.text.contains("Уровень: 20")))
    },

    test("выслеживание в логове — сразу дракон, таймер не ставится") {
      for {
        q <- makeState(40, max = 40)
        (state, heroDao, renderer, scheduler) = q
        res       <- state.action(testUser, tap("GoDarker"), renderer)
        screens   <- renderer.sentScreens
        scheduled <- scheduler.scheduled
        hero      <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(res == StateType.Dungeon && hero.dungeonLevel == 40) &&
              assertTrue(screens.last.text.contains(Warning)) &&
              assertTrue(!scheduled.exists(_.kind == TaskKind.DarknessTracking))
    },

    test("выслеживание, начатое до логова, по таймеру выводит на дракона, а не на Отмеченного") {
      for {
        q <- makeState(30, max = 30)
        (state, heroDao, renderer, scheduler) = q
        _         <- heroDao.writeSceneData(userId, io.circe.Json.obj("darknessTrackingUntil" -> io.circe.Json.fromLong(1000L)))
        _         <- TestClock.adjust(Duration.fromMillis(2000L))
        res       <- state.action(testUser, tap("GoDarker"), renderer)
        screens   <- renderer.sentScreens
        battle    <- battleOf(heroDao)
        scene     <- heroDao.readSceneData(userId)
        cancelled <- scheduler.cancelled
      } yield assertTrue(res == StateType.Dungeon && battle.isEmpty) &&
              assertTrue(screens.last.text.contains(Warning)) &&
              assertTrue(scene.contains(io.circe.Json.Null)) &&
              assertTrue(cancelled.contains(userId -> TaskKind.DarknessTracking))
    },

    test("с 25-го на 26-й выслеживать можно, как раньше") {
      for {
        q <- makeState(25, max = 25)
        (state, _, renderer, scheduler) = q
        _         <- state.action(testUser, tap("GoDarker"), renderer)
        screens   <- renderer.sentScreens
        scheduled <- scheduler.scheduled
      } yield assertTrue(!screens.exists(_.text.contains(Warning))) &&
              assertTrue(scheduled.exists(_.kind == TaskKind.DarknessTracking))
    },

    test("дракона не отравить, не обескровить и не поджечь") {
      val hero   = TestFixtures.hero(userId)
      val battle = SoloPveBattle.from(MonsterGenerator.dragon(30), hero)
      val hit    = battle.withEffects(BattleEffects.empty.copy(
        monsterPoison = Some(Poison(10)), monsterBleed = Some(Bleed(10)), monsterBurn = Some(Burn(10))))
      assertTrue(hit.effects.monsterPoison.isEmpty && hit.effects.monsterBleed.isEmpty &&
        hit.effects.monsterBurn.isEmpty) &&
        assertTrue(Race.immuneToDots(Race.Dragon) && Race.immuneToBurn(Race.Dragon)) &&
        assertTrue(!Race.mortals.contains(Race.Dragon))
    }
  )
}
