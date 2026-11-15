package pangea.service.miniapp

import io.circe.syntax.EncoderOps
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.generator.item.GemGenerator
import pangea.model.hero.{Achievement, AzatState, CubeStatus}
import pangea.model.home.{HomeData, RecipeNote}
import pangea.model.item.{GemKind, Item}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.cube.AzatCube
import pangea.service.state.QuestSupport
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.{Ref, Task, ZIO}

/** Куб из приложения: общий с ботом [[AzatCube]] и то, что приложение
  * добавляет сверху, — город и строки итога в ответ, а не в чат. */
object MiniAppCubeSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("1"), TelegramId("-1"))
  private val hero   = TestFixtures.hero(userId, state = StateType.MarketSquare)

  private def ruby(id: Long): Item = GemGenerator.item(GemKind.Ruby, 1).copy(id = id)

  private def setup(inCube: List[Item], bag: List[Item], charges: Int = 50) =
    for {
      dao <- TestHeroDao.withHero(userId, hero)
      _   <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = charges, cubeItems = inCube).asJson)
      inv  = TestInventoryRepository.withItems(bag)
      c   <- ZIO.attempt(SceneContent.load())
    } yield (new AzatCube(dao, inv, TestItemRepository.make, c), inv, c)

  def spec = suite("MiniApp: куб Азата")(
    test("перенос в куб: вещь уходит из сумки в куб; в полный куб не ложится и остаётся в сумке") {
      for {
        t1            <- setup(Nil, List(ruby(1L)))
        (cube, inv, _) = t1
        put           <- cube.deposit(hero, ruby(1L))
        azat          <- cube.loadAzat(userId)
        t2            <- setup(List.tabulate(AzatState.CubeCapacity)(i => ruby(10L + i)), List(ruby(1L)))
        (full, inv2, c) = t2
        refused       <- full.deposit(hero, ruby(1L))
      } yield assertTrue(
        put.isEmpty,
        azat.cubeItems.map(_.id) == List(1L),
        inv.snapshot.isEmpty,
        refused.contains(c.text("cube.full")),
        inv2.snapshot.map(_.id) == List(1L)
      )
    },
    test("активация: три камня — один выше, заряд потрачен, строка итога — в ответ приложению") {
      for {
        t             <- setup(List(ruby(1L), ruby(2L), ruby(3L)), Nil, charges = 5)
        (cube, _, c)   = t
        lines         <- Ref.make(Vector.empty[String])
        made          <- cube.activate(user, hero, new MiniApp.Collect(lines))
        azat          <- cube.loadAzat(userId)
        said          <- lines.get
      } yield assertTrue(
        made.size == 1,
        made.forall(_.id > 0L),
        azat.cubeItems.map(_.id) == made.map(_.id),
        azat.cubeCharges == 4,
        said == Vector(c.text("cube.activated"))
      )
    },
    test("достижение из приложения — строкой в ответ приложению и той же строкой в личку; итог активации в личку не идёт") {
      // В книге девять рецептов: десятый из куба даёт «Артефактор I».
      val nine = List.tabulate(9)(i => RecipeNote(List(s"Камень №$i"), s"Итог №$i", 1))
      for {
        dao   <- TestHeroDao.withHero(userId, hero)
        _     <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = 5,
                   cubeItems = List(ruby(1L), ruby(2L), ruby(3L))).asJson)
        _     <- dao.writeHomeData(userId, HomeData.empty.copy(recipes = nine).asJson)
        c     <- ZIO.attempt(SceneContent.load())
        cube   = new AzatCube(dao, TestInventoryRepository.withItems(Nil), TestItemRepository.make, c)
        lines <- Ref.make(Vector.empty[String])
        dm    <- TestRenderer.make
        _     <- cube.activate(user, hero, new MiniApp.Collect(lines, Some(dm)))
        said  <- lines.get
        told  <- dm.sentScreens.map(_.map(_.text))
        after <- dao.getHeroByUserId(userId).map(_.get)
        line   = c.format("marisa.achievement", "title" -> Achievement.Artificer1.title,
                   "bonus" -> Achievement.Artificer1.bonusLine)
      } yield assertTrue(
        after.hasAchievement(Achievement.Artificer1),
        said.headOption.contains(c.text("cube.activated")),
        said.contains(line),
        told.contains(line),
        !told.contains(c.text("cube.activated"))
      )
    },
    test("личка не ответила — действие в приложении не падает, строка о достижении в ответе есть") {
      val broken = new Renderer {
        def show(user: User, screen: Screen): Task[Unit] = ZIO.fail(new RuntimeException("ВК недоступен"))
      }
      for {
        dao   <- TestHeroDao.withHero(userId, hero)
        c     <- ZIO.attempt(SceneContent.load())
        lines <- Ref.make(Vector.empty[String])
        got   <- QuestSupport.grant(dao, c, user, hero, Achievement.Artificer1, new MiniApp.Collect(lines, Some(broken)))
        said  <- lines.get
      } yield assertTrue(got.hasAchievement(Achievement.Artificer1), said.size == 1, said.head.contains("Артефактор I"))
    },
    test("с кубом из приложения работают только в городе") {
      assertTrue(
        MiniApp.inCity(hero),
        MiniApp.inCity(hero.copy(state = StateType.Home)),
        !MiniApp.inCity(hero.copy(state = StateType.Dungeon)),
        !MiniApp.inCity(hero.copy(state = StateType.Battle))
      )
    }
  )
}
