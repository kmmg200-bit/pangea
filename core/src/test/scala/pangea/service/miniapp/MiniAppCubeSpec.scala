package pangea.service.miniapp

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.GemGenerator
import pangea.model.hero.{AzatState, CubeStatus}
import pangea.model.item.{GemKind, Item}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.cube.AzatCube
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository}
import zio.test._
import zio.{Ref, ZIO}

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
