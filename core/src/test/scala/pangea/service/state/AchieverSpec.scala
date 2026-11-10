package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.hero.{Achievement, Hero}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

/** «Достигатор» I–V: за 10, 20, 30, 40 и 50 достижений (сами «Достигаторы» тоже
  * в счёт), каждый ранг — +2% к получаемому опыту. */
object AchieverSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk"), TelegramId("tg"))
  private val base     = TestFixtures.hero(userId)

  /** Первые `n` достижений по порядку — первые 26 заведены задолго до вех. */
  private def with_(n: Int, h: Hero = base): Hero = Achievement.values.take(n).foldLeft(h)(_.withAchievement(_))

  private def grantAll(h: Hero) =
    for {
      dao  <- TestHeroDao.withHero(userId, h)
      r    <- TestRenderer.make
      c    <- ZIO.attempt(SceneContent.load())
      got  <- QuestSupport.grantMilestones(dao, c, testUser, h, r)
      kept <- dao.getHeroByUserId(userId).map(_.get)
      said <- r.sentScreens.map(_.map(_.text))
    } yield (got, kept, said)

  override def spec = suite("«Достигатор»")(

    test("пороги 10/20/30/40/50; названия и бонус") {
      assertTrue(Achievement.AchieverTiers.map(_._1) == List(10, 20, 30, 40, 50)) &&
      assertTrue(Achievement.milestonesDue(with_(9)).isEmpty) &&
      assertTrue(Achievement.milestonesDue(with_(10)) == List(Achievement.Achiever1)) &&
      assertTrue(Achievement.Achiever3.title == "Достигатор III" &&
        Achievement.Achiever1.bonusLine == "+2% к получаемому опыту")
    },

    test("выданная веха идёт в счёт: 18 достижений и тысячный убитый — «Тысяча за плечами», I и II ранги") {
      for {
        t <- grantAll(with_(18).copy(kills = 1000L))
        (got, kept, said) = t
      } yield assertTrue(got.hasAchievement(Achievement.Kills1k) && got.hasAchievement(Achievement.Achiever1) &&
                got.hasAchievement(Achievement.Achiever2) && !got.hasAchievement(Achievement.Achiever3)) &&
              assertTrue(kept.achievements == got.achievements && Achievement.gotCount(kept) == 21) &&
              assertTrue(said.count(_.contains("«Достигатор")) == 2)
    },

    test("опыт: +2% за ранг, вверх; поверх «выспался»; без рангов — как был") {
      val two = base.withAchievement(Achievement.Achiever1).withAchievement(Achievement.Achiever2)
      assertTrue(base.expFrom(100L, 0L) == 100L && two.expFrom(100L, 0L) == 104L && two.expFrom(10L, 0L) == 11L) &&
      assertTrue(two.copy(expFrozen = true).expFrom(100L, 0L) == 0L)
    }
  )
}
