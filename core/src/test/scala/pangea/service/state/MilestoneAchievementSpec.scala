package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.hero.{Achievement, Hero}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.states.GlobalMapState
import pangea.test._
import zio.ZIO
import zio.test._

/** Вехи — убитые за всю жизнь и уровень героя: перевалил порог — достижение
  * приходит после хода, откуда бы ни пришли убитые и уровень. */
object MilestoneAchievementSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val vkId     = VkId("vk_test")
  private val testUser = User(userId, vkId, TelegramId("tg_test"))

  private val base = TestFixtures.hero(userId, state = StateType.GlobalMap)

  private def handler(h: Hero) =
    for {
      heroDao   <- TestHeroDao.withHero(userId, h)
      heroRepo  <- TestHeroRepository.withHero(userId, h)
      userRepo  <- TestUserRepository.withUser(testUser)
      api       <- TestApi.make
      content   <- ZIO.attempt(SceneContent.load())
      lock      <- PlayerLock.make
      payouts    = Payouts(TestPayoutDao.empty, heroDao, content)
      parcels    = Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
      transfers  = Transfers(TestInventoryRepository.accepting, userRepo, parcels, new TestPlayers, content,
                     heroDao, TestBankRepository.empty, payouts, lock)
      states     = Map[StateType, State](StateType.GlobalMap -> GlobalMapState(heroDao, content))
    } yield (new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers, states,
               AdminPanel.disabled, content, lock), heroDao, api)

  override def spec = suite("Вехи: убитые и уровень")(

    test("пороги: 1к/10к/100к/500к убитых и 10/25/50/75/100/125/150 уровень — по порядку, без уже полученных") {
      val h = base.copy(kills = 10000L, lvl = 50L, achievements = List(Achievement.Level10.entryName))
      assertTrue(Achievement.milestonesDue(h) == List(Achievement.Kills1k, Achievement.Kills10k,
                   Achievement.Level25, Achievement.Level50)) &&
      assertTrue(Achievement.milestonesDue(base.copy(kills = 999L, lvl = 9L)).isEmpty) &&
      assertTrue(Achievement.milestonesDue(base.copy(kills = 500000L, lvl = 150L)).size == 11)
    },

    test("сила за убитых складывается: 1, 2, 5, 10 — все четыре +18; у каждого уровня +100 HP") {
      val kills  = Achievement.KillTiers.map(_._2).foldLeft(base)(_.withAchievement(_))
      val levels = base.withAchievement(Achievement.Level10).withAchievement(Achievement.Level25)
      assertTrue(Achievement.KillTiers.map(_._3) == List(1L, 2L, 5L, 10L)) &&
      assertTrue(Achievement.strBonus(kills) == 18L && kills.nativeBaseStats.str == base.nativeBaseStats.str + 18L) &&
      assertTrue(Achievement.hpBonus(levels) == 200L && levels.effectiveMaxHp(0L) == base.effectiveMaxHp(0L) + 200L) &&
      assertTrue(Achievement.values.filter(a => Achievement.LevelTiers.exists(_._2 == a)).forall(_.bonusLine == "+100 к HP")) &&
      assertTrue(Achievement.Level10.title == "Великий путь начинается с первой десятки")
    },

    test("после хода — достижения за пройденные вехи со строкой о каждом; второй ход их не повторяет") {
      for {
        t <- handler(base.copy(kills = 1500L, lvl = 25L))
        (state, dao, api) = t
        _     <- state.makeActionVK(vkId, eventId = 1L, UserAction("", None))
        once  <- api.sentMessages.map(_.map(_._1))
        _     <- state.makeActionVK(vkId, eventId = 2L, UserAction("", None))
        twice <- api.sentMessages.map(_.map(_._1))
        after <- dao.getHeroByUserId(userId).map(_.get)
        got    = once.filter(_.contains("Вы получили достижение"))
      } yield assertTrue(after.achievements == List("Kills1k", "Level10", "Level25")) &&
              assertTrue(got.size == 3 && got.head.contains("«Тысяча за плечами»") && got.head.contains("+1 к силе")) &&
              assertTrue(twice.count(_.contains("Вы получили достижение")) == 3)
    },

    test("пока до вех не дошёл — ни строки, ни записи") {
      for {
        t <- handler(base.copy(kills = 999L, lvl = 9L))
        (state, dao, api) = t
        _     <- state.makeActionVK(vkId, eventId = 1L, UserAction("", None))
        said  <- api.sentMessages.map(_.map(_._1))
        after <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(after.achievements.isEmpty && !said.exists(_.contains("Вы получили достижение")))
    }
  )
}
