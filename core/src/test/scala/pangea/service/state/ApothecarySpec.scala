package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.hero.Achievement
import pangea.model.item.{BrewKind, BrewRates}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

/** «Промышленный аптекарь» I–V: счёт сваренных зелий (`heroes.brews`) — 10,
  * 100, 250, 500, 1000; каждый ранг удлиняет срок отваров на 10%. */
object ApothecarySpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk"), TelegramId("tg"))
  private val base     = TestFixtures.hero(userId)

  private def kinds(n: Int): List[String] = List.fill(n)(BrewKind.rank1.head.entryName)

  override def spec = suite("«Промышленный аптекарь»")(

    test("пороги 10/100/250/500/1000 — по порядку, с вехами убитых и уровня в одном списке") {
      assertTrue(Achievement.BrewTiers.map(_._1) == List(10L, 100L, 250L, 500L, 1000L)) &&
      assertTrue(Achievement.milestonesDue(base.copy(brews = 9L)).isEmpty) &&
      assertTrue(Achievement.milestonesDue(base.copy(brews = 250L)) ==
        List(Achievement.Apothecary1, Achievement.Apothecary2, Achievement.Apothecary3)) &&
      assertTrue(Achievement.Apothecary5.title == "Промышленный аптекарь V" &&
        Achievement.Apothecary1.bonusLine == "+10% к сроку действия отваров")
    },

    test("срок отвара: +10% за ранг, все пять — +50%; без достижений — как был") {
      val two = base.withAchievement(Achievement.Apothecary1).withAchievement(Achievement.Apothecary2)
      val all = Achievement.BrewTiers.map(_._2).foldLeft(base)(_.withAchievement(_))
      val hr  = BrewRates.BoostDurationMs
      assertTrue(Achievement.brewMs(base, hr) == hr && Achievement.brewMs(two, hr) == hr * 12L / 10L &&
        Achievement.brewMs(all, hr) == hr * 15L / 10L)
    },

    test("сварено — счёт растёт на каждое зелье; десятое — «Промышленный аптекарь I» со строкой") {
      for {
        dao  <- TestHeroDao.withHero(userId, base)
        r    <- TestRenderer.make
        c    <- ZIO.attempt(SceneContent.load())
        _    <- QuestSupport.brewed(dao, c, testUser, kinds(4), r)
        mid  <- dao.getHeroByUserId(userId).map(_.get)
        _    <- QuestSupport.brewed(dao, c, testUser, kinds(6), r)
        _    <- QuestSupport.brewed(dao, c, testUser, Nil, r)
        end  <- dao.getHeroByUserId(userId).map(_.get)
        said <- r.sentScreens.map(_.map(_.text))
      } yield assertTrue(mid.brews == 4L && !mid.hasAchievement(Achievement.Apothecary1)) &&
              assertTrue(end.brews == 10L && end.hasAchievement(Achievement.Apothecary1)) &&
              assertTrue(said.count(_.contains("«Промышленный аптекарь I»")) == 1)
    }
  )
}
