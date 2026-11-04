package pangea.service.state.states.guild

import pangea.engine.SceneContent
import pangea.model.quest.{DailyKind, DailyNpc, DailyQuests, DailyRates, DailyTask}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{DailyQuestLog, UserAction}
import pangea.test.{TestBankRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

object MasterHornStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  // Первая прокачка любого стата стоит 5 (см. MasterHornState.cost, n=1).
  private def hero(reputation: Long, silver: Long) =
    TestFixtures.hero(userId).copy(guildReputation = reputation, silver = silver)

  private def makeState(reputation: Long, silver: Long) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero(reputation, silver))
      invRepo   = TestInventoryRepository.accepting
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (MasterHornState(heroDao, invRepo, content), heroDao, renderer)

  override def spec = suite("MasterHornState")(

    test("хватает и репутации, и серебра → списывает поровну обе валюты, характеристика растёт") {
      for {
        t <- makeState(reputation = 100L, silver = 100L)
        (state, heroDao, renderer) = t
        _    <- state.action(testUser, tap("ImproveArmor"), renderer)
        _    <- state.action(testUser, tap("ConfirmImprove"), renderer)
        hero <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(hero.guildReputation == 95L) &&
              assertTrue(hero.silver == 95L) &&
              assertTrue(hero.masterHornBoosts.armor == 3L)
    },

    test("серебра на руках мало — Горн добирает из ячейки, а «Осталось серебра» считает её тоже") {
      val bank = TestBankRepository.of(cells = 1, silver = 10L)
      for {
        heroDao  <- TestHeroDao.withHero(userId, hero(reputation = 100L, silver = 2L))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = MasterHornState(heroDao, TestInventoryRepository.accepting, content, Some(bank))
        _        <- state.action(testUser, tap("ImproveArmor"), renderer)
        _        <- state.action(testUser, tap("ConfirmImprove"), renderer)
        h        <- heroDao.getHeroByUserId(userId).map(_.get)
        screens  <- renderer.sentScreens
      } yield assertTrue(h.silver == 0L && bank.silverSnapshot == 7L) &&
              assertTrue(h.masterHornBoosts.armor == 3L) &&
              assertTrue(screens.exists(_.text.contains("Осталось репутации: 95, серебра: 7.")))
    },

    test("не хватает репутации → отказ, ни репутация, ни серебро не списаны") {
      for {
        t <- makeState(reputation = 4L, silver = 100L)
        (state, heroDao, renderer) = t
        _       <- state.action(testUser, tap("ImproveArmor"), renderer)
        _       <- state.action(testUser, tap("ConfirmImprove"), renderer)
        hero    <- heroDao.getHeroByUserId(userId).map(_.get)
        screens <- renderer.sentScreens
      } yield assertTrue(hero.guildReputation == 4L) &&
              assertTrue(hero.silver == 100L) &&
              assertTrue(hero.masterHornBoosts.armor == 0L) &&
              assertTrue(screens.exists(_.text.contains("Недостаточно репутации")))
    },

    test("хватает репутации, но не хватает серебра → отказ, ни репутация, ни серебро не списаны") {
      for {
        t <- makeState(reputation = 100L, silver = 4L)
        (state, heroDao, renderer) = t
        _       <- state.action(testUser, tap("ImproveArmor"), renderer)
        _       <- state.action(testUser, tap("ConfirmImprove"), renderer)
        hero    <- heroDao.getHeroByUserId(userId).map(_.get)
        screens <- renderer.sentScreens
      } yield assertTrue(hero.guildReputation == 100L) &&
              assertTrue(hero.silver == 4L) &&
              assertTrue(hero.masterHornBoosts.armor == 0L) &&
              assertTrue(screens.exists(_.text.contains("Недостаточно серебра")))
    },

    test("прокачка у Горна не съедает его наказ «набрать репутации»: мерка опускается на потраченное") {
      // Часы тестов стоят в нуле — это и есть «сегодня» для поручения.
      val task = DailyTask(DailyKind.HornReputation, DailyRates.dayOf(0L), from = 100L, goal = 60L, taken = true)
      for {
        t <- makeState(reputation = 100L, silver = 100L)
        (state, heroDao, renderer) = t
        _     <- DailyQuestLog.save(heroDao, userId, DailyQuests.empty.updated(DailyNpc.Horn, task))
        _     <- state.action(testUser, tap("ImproveArmor"), renderer)
        _     <- state.action(testUser, tap("ConfirmImprove"), renderer)
        hero  <- heroDao.getHeroByUserId(userId).map(_.get)
        saved <- DailyQuestLog.load(heroDao, userId).map(_.of(DailyNpc.Horn).get)
      } yield assertTrue(hero.guildReputation == 95L) &&
              assertTrue(saved.taken && saved.kind == DailyKind.HornReputation && saved.from == 95L) &&
              // наберёт теперь 60 — наказ готов: потраченное отрабатывать не нужно
              assertTrue(saved.withCounter(hero.guildReputation + 60L).ready)
    },

    test("LeaveMasterHorn → переход в TrainingHall") {
      for {
        t <- makeState(reputation = 0L, silver = 0L)
        (state, _, renderer) = t
        result <- state.action(testUser, tap("LeaveMasterHorn"), renderer)
      } yield assertTrue(result == StateType.TrainingHall)
    }
  )
}
