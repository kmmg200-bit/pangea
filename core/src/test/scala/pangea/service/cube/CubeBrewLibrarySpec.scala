package pangea.service.cube

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.{CubeCraft, MaterialGenerator}
import pangea.model.hero.{AzatState, CubeStatus}
import pangea.model.home.{HomeData, RecipeNote}
import pangea.model.item.{BrewKind, MaterialKind}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

import java.nio.file.{Files, Paths}

/** Что варит только куб, — не стандартный отвар, а рецепт куба (решение
  * пользователя): он в книге рецептов библиотеки и в её счёте. Тем, кто варил
  * это, пока куб таких записей не делал, книгу дописала миграция — той же
  * записью, что пишет куб. */
object CubeBrewLibrarySpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("1"), TelegramId("-1"))
  private val hero   = TestFixtures.hero(userId, state = StateType.Home)

  private val Migration = "20261115180000_cube_brews_in_library.sql"

  /** Текст миграции: тесты идут из корня репозитория, а на всякий случай — и из модуля. */
  private def migration: String =
    List(Paths.get("migrations", Migration), Paths.get("..", "migrations", Migration))
      .find(p => Files.exists(p)).map(p => Files.readString(p)).getOrElse("")

  private def made(kinds: List[MaterialKind]) =
    kinds.zipWithIndex.map { case (m, i) => MaterialGenerator.item(m).copy(id = i.toLong + 1L) }

  /** Что куб пишет в книгу, сварив `kind` по его рецепту. */
  private def cubeNote(kind: BrewKind): Option[RecipeNote] =
    CubeCraft.craft(made(kind.recipe), charges = 5, Rng(7L)).applied.find(_.result.brew.contains(kind)).map(AzatCube.note)

  def spec = suite("Что варит только куб, — в книге рецептов")(
    test("только куб варит «Яд красного паука», «Паучью сыворотку» и «Гнилой зов»; травяные отвары и перегонка — стандартные") {
      assertTrue(
        BrewKind.values.filter(_.cubeOnly).toSet == Set[BrewKind](BrewKind.RedVenomSalve, BrewKind.SpiderSerum, BrewKind.RotCall),
        !BrewKind.BoneSetter.cubeOnly,
        !BrewKind.Moonshine.cubeOnly
      )
    },
    test("в книге их записи видны и считаются, а стандартных отваров — нет; рейтинг «Рецепты» отсеивает только стандартные") {
      val venom = RecipeNote(List("Железа красного паука", "Ядовитая железа"), "Яд красного паука", 1)
      val bone  = RecipeNote(List("Ромашка", "Календула", "Крапива"), "Костоправный отвар", 2)
      assertTrue(
        HomeData.empty.copy(recipes = List(venom, bone)).recipeBook == List(venom),
        BrewKind.values.filter(_.cubeOnly).forall(k => !RecipeNote.BrewNames.contains(k.label)),
        RecipeNote.BrewNames.contains(BrewKind.BoneSetter.label)
      )
    },
    test("куб, сварив «Яд красного паука», пишет его в книгу рецептов") {
      for {
        dao  <- TestHeroDao.withHero(userId, hero)
        _    <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = 5,
                  cubeItems = made(List(MaterialKind.RedSpiderGland, MaterialKind.VenomGland))).asJson)
        c    <- ZIO.attempt(SceneContent.load())
        cube  = new AzatCube(dao, TestInventoryRepository.withItems(Nil), TestItemRepository.make, c)
        r    <- TestRenderer.make
        _    <- cube.activate(user, hero, r)
        home <- cube.loadHome(userId)
      } yield assertTrue(home.recipeBook.map(_.result) == List(BrewKind.RedVenomSalve.label))
    },
    test("миграция дописывает тем, кто варил это раньше, ровно ту запись, что пишет куб") {
      val notes = BrewKind.values.filter(_.cubeOnly).toList.map(k => k -> cubeNote(k))
      assertTrue(
        migration.nonEmpty,
        notes.forall(_._2.isDefined),
        notes.forall { case (k, note) =>
          note.exists(n => migration.contains(n.asJson.noSpaces)) &&
            migration.contains(s"""'["${k.entryName}"]'""") && migration.contains(s"= '${k.label}'")
        }
      )
    }
  )
}
