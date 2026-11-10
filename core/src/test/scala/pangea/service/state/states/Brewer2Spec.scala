package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.{CubeCraft, MaterialGenerator}
import pangea.model.hero.{Achievement, AchievementRates, AzatState, CubeStatus, LoreData}
import pangea.model.item.{BrewKind, Item}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{HerbLore, UserAction}
import pangea.service.state.states.temple.CubeState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** «Зельевар II»: сварен хоть раз каждый отвар второго ранга — из трав, среди
  * которых есть редкая. +3 к интеллекту, как «Зельевар I» даёт +2. */
object Brewer2Spec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def herbsFor(k: BrewKind): List[Item] =
    k.recipe.zipWithIndex.map { case (h, j) => MaterialGenerator.item(h).copy(id = (j + 1).toLong) }

  /** Сварить в кубе `last`, когда в знаниях уже записаны `known`. */
  private def brewLast(known: List[BrewKind], last: BrewKind, already: List[String] = Nil) =
    for {
      dao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.Cube).copy(achievements = already))
      _     <- HerbLore.writeLore(dao, userId, LoreData().brewedAlso(known.map(_.entryName)))
      _     <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = 10, cubeItems = herbsFor(last)).asJson)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      cube   = CubeState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
      _     <- cube.action(testUser, tap("CubeActivate"), r)
      said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      after <- dao.getHeroByUserId(userId).map(_.get)
    } yield (said, after)

  override def spec = suite("«Зельевар II»")(

    test("второй ранг — тринадцать отваров из трав с редкой; без перегонки, гнилого зова и первого ранга") {
      val rank2 = BrewKind.rank2
      assertTrue(rank2.size == 13) &&
      assertTrue(rank2.forall(k => k.recipe.exists(_.herbRank == 2) && k.recipe.forall(_.isHerb))) &&
      assertTrue(!rank2.contains(BrewKind.RotCall) && !rank2.contains(BrewKind.Moonshine)) &&
      assertTrue(rank2.intersect(BrewKind.rank1).isEmpty) &&
      // каждый сходится в кубе своей кучей
      assertTrue(rank2.forall { k =>
        val r = CubeCraft.craft(herbsFor(k), charges = 5, Rng(3L))
        r.chargesUsed == 1 && r.items.forall(_.brew.contains(k))
      })
    },

    test("последний отвар второго ранга — достижение и +3 к интеллекту") {
      val rank2 = BrewKind.rank2.toList
      for {
        got <- brewLast(rank2.init, rank2.last)
        int  = TestFixtures.hero(userId).effectiveBaseStats(0L).int
      } yield assertTrue(got._1.contains("Вы получили достижение «Зельевар II». +3 к интеллекту.")) &&
              assertTrue(got._2.hasAchievement(Achievement.Brewer2) && !got._2.hasAchievement(Achievement.Brewer1)) &&
              assertTrue(AchievementRates.Brewer2IntBonus == 3L && got._2.effectiveBaseStats(0L).int == int + 3L)
    },

    test("без одного отвара — нет; полученное не повторяется") {
      val rank2 = BrewKind.rank2.toList
      for {
        short <- brewLast(rank2.drop(2), rank2.head)
        again <- brewLast(rank2.init, rank2.last, already = List(Achievement.Brewer2.entryName))
      } yield assertTrue(!short._2.hasAchievement(Achievement.Brewer2)) &&
              assertTrue(!again._1.contains("Вы получили достижение «Зельевар II»"))
    }
  )
}
