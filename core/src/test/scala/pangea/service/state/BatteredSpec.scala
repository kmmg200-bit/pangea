package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.hero.{Achievement, AchievementRates, Hero}
import pangea.model.state.StateType
import pangea.model.trauma.Trauma
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.states.GlobalMapState
import pangea.test._
import zio.ZIO
import zio.test._

/** «Добейте меня...»: на герое разом все травмы игры — достижение после хода
  * и +1% к HP навсегда. */
object BatteredSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val vkId     = VkId("vk_test")
  private val testUser = User(userId, vkId, TelegramId("tg_test"))

  private val base = TestFixtures.hero(userId, state = StateType.GlobalMap)
  private val all  = Trauma.all.map(_.name).toList

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

  override def spec = suite("«Добейте меня...»")(

    test("все двенадцать травм — веха; без одной — нет") {
      assertTrue(Trauma.all.size == 12) &&
      assertTrue(Achievement.milestonesDue(base.copy(traumaNames = all)) == List(Achievement.Battered)) &&
      assertTrue(Achievement.milestonesDue(base.copy(traumaNames = all.drop(1))).isEmpty) &&
      assertTrue(Achievement.milestonesDue(base.copy(traumaNames = all,
                   achievements = List(Achievement.Battered.entryName))).isEmpty)
    },

    test("+1% к HP — и без травм на себе") {
      val got = base.withAchievement(Achievement.Battered)
      val hp  = base.effectiveMaxHp(0L)
      assertTrue(AchievementRates.BatteredHpPct == 1L) &&
      assertTrue(got.effectiveMaxHp(0L) == hp * 101L / 100L && got.effectiveMaxHp(0L) > hp)
    },

    test("после хода со всеми травмами — строка о достижении и запись") {
      for {
        t <- handler(base.copy(traumaNames = all, traumaUntil = Some(Long.MaxValue)))
        (state, dao, api) = t
        _     <- state.makeActionVK(vkId, eventId = 1L, UserAction("", None))
        said  <- api.sentMessages.map(_.map(_._1))
        after <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(said.exists(_.contains("Вы получили достижение «Добейте меня...». +1% к HP."))) &&
              assertTrue(after.hasAchievement(Achievement.Battered))
    }
  )
}
