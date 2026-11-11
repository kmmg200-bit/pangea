package pangea.service.cube

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.hero.{Achievement, AchievementRates, AzatState, CubeStatus}
import pangea.model.home.{HomeData, RecipeNote}
import pangea.model.item.{GemKind, Item, MaterialKind}
import pangea.model.state.StateType
import pangea.model.stats.BaseStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.temple.CubeState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** «Артефактор» I–V: разных рецептов куба в книге рецептов — 10, 25, 50, 100,
  * 150; отвары не в счёт. Каждый ранг — +1 ко всем характеристикам. */
object ArtificerSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def herb(id: Long, kind: MaterialKind): Item = MaterialGenerator.item(kind).copy(id = id)
  /** Ромашка, календула, крапива — костоправный отвар. */
  private val boneSet = List(herb(1L, MaterialKind.Chamomile), herb(2L, MaterialKind.Calendula), herb(3L, MaterialKind.Nettle))

  private def ruby(id: Long): Item = GemGenerator.item(GemKind.Ruby, 1).copy(id = id)
  /** Три расколотых рубина — камень грейдом выше: новая запись в книге. */
  private val rubies = List(ruby(1L), ruby(2L), ruby(3L))

  /** `n` записей, которых куб не повторит. */
  private def notes(n: Int): List[RecipeNote] = List.tabulate(n)(i => RecipeNote(List(s"Что-то $i"), s"Итог $i", 1))

  /** Активировать куб в Храме с книгой из `known` записей: что сказали и каким стал герой. */
  private def activate(known: Int, items: List[Item] = rubies, already: List[String] = Nil) =
    activateWith(notes(known), items, already)

  /** То же с книгой как есть — хоть бы и со старыми записями об отварах. */
  private def activateWith(book: List[RecipeNote], items: List[Item], already: List[String]) =
    for {
      dao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.Cube).copy(achievements = already))
      _     <- dao.writeHomeData(userId, HomeData(recipes = book).asJson)
      _     <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = 10, cubeItems = items).asJson)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      cube   = CubeState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
      _     <- cube.action(testUser, tap("CubeActivate"), r)
      said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      after <- dao.getHeroByUserId(userId).map(_.get)
    } yield (said, after)

  override def spec = suite("«Артефактор»")(

    test("пороги 10/25/50/100/150, у каждого ранга +1 ко всем; все пять — +5") {
      val bare = TestFixtures.hero(userId)
      val five = Achievement.ArtificerTiers.map(_._2).foldLeft(bare)(_.withAchievement(_))
      assertTrue(Achievement.ArtificerTiers.map(_._1) == List(10, 25, 50, 100, 150)) &&
      assertTrue(Achievement.ArtificerTiers.map(_._2.title) ==
        List("Артефактор I", "Артефактор II", "Артефактор III", "Артефактор IV", "Артефактор V")) &&
      assertTrue(Achievement.ArtificerTiers.forall(_._2.bonusLine == "+1 ко всем характеристикам")) &&
      assertTrue(AchievementRates.ArtificerStatBonus == 1L) &&
      assertTrue(Achievement.artificerDue(bare, 49) == List(Achievement.Artificer1, Achievement.Artificer2)) &&
      assertTrue(bare.effectiveBaseStats(0L) == BaseStats(agi = 10, vit = 10, str = 10, int = 10)) &&
      assertTrue(five.effectiveBaseStats(0L) == BaseStats(agi = 15, vit = 15, str = 15, int = 15))
    },

    test("десятая запись в книге — «Артефактор I» со строкой; девятая — нет") {
      for {
        ten  <- activate(9)
        nine <- activate(8)
      } yield assertTrue(ten._1.contains("Вы получили достижение «Артефактор I». +1 ко всем характеристикам.")) &&
              assertTrue(ten._2.hasAchievement(Achievement.Artificer1)) &&
              assertTrue(!nine._1.contains("Вы получили достижение") && !nine._2.hasAchievement(Achievement.Artificer1))
    },

    test("книга, собранная раньше, засчитывается при активации — даже если ничего не вышло") {
      for {
        got <- activate(30, items = Nil)
      } yield assertTrue(got._2.hasAchievement(Achievement.Artificer1) && got._2.hasAchievement(Achievement.Artificer2)) &&
              assertTrue(!got._2.hasAchievement(Achievement.Artificer3))
    },

    test("отвар записью не считается: девять записей и сваренный отвар — «Артефактора» нет; старые записи об отварах не в счёт") {
      val oldBrew = RecipeNote(List("Ромашка", "Календула", "Крапива"), "Костоправный отвар", 2)
      for {
        brew <- activate(9, items = boneSet)
        old  <- activateWith(notes(9) :+ oldBrew, items = Nil, already = Nil)
      } yield assertTrue(!brew._1.contains("Вы получили достижение") && !brew._2.hasAchievement(Achievement.Artificer1)) &&
              assertTrue(!old._2.hasAchievement(Achievement.Artificer1))
    },

    test("полученный ранг не повторяется") {
      for {
        again <- activate(9, already = List(Achievement.Artificer1.entryName))
      } yield assertTrue(!again._1.contains("Вы получили достижение «Артефактор I»")) &&
              assertTrue(again._2.achievements.count(_ == Achievement.Artificer1.entryName) == 1)
    }
  )
}
