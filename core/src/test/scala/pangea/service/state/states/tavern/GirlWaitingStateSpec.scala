package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.item.ItemType
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.{Task, ZIO}
import zio.test._

/** Девушка с картой отца ждёт в таверне оплаты: кнопка в таверне, расчёт по
  * прежней цене, четыре часа ожидания. */
object GirlWaitingStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val deal = GirlDeal(price = 7500L, mapLvl = 10L, until = GirlDeal.WaitMs)

  // Продавца карт в таверне нет: его кнопка тесту не нужна.
  private val noSeller = CardSellerData(offerUntil = None, price = None, nextRollAt = Some(Long.MaxValue))

  private def make(silver: Long, inv: TestInventoryRepository = TestInventoryRepository.accepting,
                   withDeal: Option[GirlDeal] = Some(deal)) =
    for {
      dao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = StateType.Tavern).copy(lvl = 10L, silver = silver))
      _   <- ZIO.foreachDiscard(withDeal)(d => GirlDeal.save(dao, userId, d))
      _   <- dao.writeCardSellerData(userId, noSeller.asJson)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (GirlWaitingState(dao, inv, TestItemRepository.make, c), dao, inv, r, c)

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Девушка ждёт в таверне")(

    test("пока ждёт — в таверне её кнопка над «Персонаж»; ушла — кнопки нет") {
      for {
        t <- make(silver = 0L)
        (_, dao, _, r, c) = t
        sched  <- TestScheduler.make
        tavern  = TavernState(dao, sched, c)
        _      <- tavern.enter(testUser, r)
        first  <- r.sentScreens.map(_.last.choices)
        gRow    = first.find(_.id == "GirlWaiting").flatMap(_.row).getOrElse(99)
        cRow    = first.find(_.id == "OpenCharacter").flatMap(_.row).getOrElse(0)
        goes   <- tavern.action(testUser, tap("GirlWaiting"), r)
        _      <- TestClock.adjust(zio.Duration.fromMillis(GirlDeal.WaitMs))
        _      <- tavern.enter(testUser, r)
        later  <- r.sentScreens.map(_.last.choices.map(_.id))
      } yield assertTrue(first.exists(_.id == "GirlWaiting") && gRow < cRow) &&
              assertTrue(goes == StateType.GirlWaiting) &&
              assertTrue(!later.contains("GirlWaiting"))
    },

    test("экран: цена и сколько ещё ждать, кнопки «Заплатить» и «Позже»") {
      for {
        t <- make(silver = 0L)
        (state, _, _, r, _) = t
        _      <- TestClock.adjust(zio.Duration.fromMillis(60L * 60L * 1000L))
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
        later  <- state.action(testUser, tap("GirlLater"), r)
      } yield assertTrue(screen.text.contains("Принесли 7500 серебра?") && screen.text.contains("ещё 3ч.")) &&
              assertTrue(screen.choices.map(_.id) == List("PayGirl", "GirlLater")) &&
              assertTrue(later == StateType.Tavern)
    },

    test("хватает серебра — карта в сумке, серебро списано, она больше не ждёт") {
      for {
        t <- make(silver = 10000L)
        (state, dao, inv, r, _) = t
        res  <- state.action(testUser, tap("PayGirl"), r)
        hero <- dao.getHeroByUserId(userId).map(_.get)
        left <- GirlDeal.load(dao, userId)
        said <- texts(r)
      } yield assertTrue(res == StateType.Tavern) &&
              assertTrue(hero.silver == 10000L - 7500L) &&
              assertTrue(inv.snapshot.exists(_.itemType == ItemType.TreasureMap)) &&
              assertTrue(left.isEmpty && said.contains("Девушка отдаёт вам карту"))
    },

    test("серебра снова мало — ждёт дальше, срок прежний; сумка полна — ничего не списано") {
      for {
        t <- make(silver = 100L)
        (state, dao, inv, r, _) = t
        stay <- state.action(testUser, tap("PayGirl"), r)
        said <- texts(r)
        kept <- GirlDeal.load(dao, userId)
        t2 <- make(silver = 10000L, inv = TestInventoryRepository.full)
        (state2, dao2, _, r2, _) = t2
        full  <- state2.action(testUser, tap("PayGirl"), r2)
        hero2 <- dao2.getHeroByUserId(userId).map(_.get)
        kept2 <- GirlDeal.load(dao2, userId)
      } yield assertTrue(stay == StateType.GirlWaiting && said.contains("Здесь меньше, чем 7500")) &&
              assertTrue(kept.contains(deal) && inv.snapshot.isEmpty) &&
              assertTrue(full == StateType.Tavern && hero2.silver == 10000L && kept2.contains(deal))
    },

    test("срок вышел — её нет: заплатить нельзя, запись стёрта") {
      for {
        t <- make(silver = 10000L)
        (state, dao, inv, r, _) = t
        _    <- TestClock.adjust(zio.Duration.fromMillis(GirlDeal.WaitMs))
        res  <- state.action(testUser, tap("PayGirl"), r)
        said <- texts(r)
        hero <- dao.getHeroByUserId(userId).map(_.get)
        left <- GirlDeal.load(dao, userId)
      } yield assertTrue(res == StateType.Tavern && said.contains("не дождалась")) &&
              assertTrue(hero.silver == 10000L && inv.snapshot.isEmpty && left.isEmpty)
    }
  )
}
