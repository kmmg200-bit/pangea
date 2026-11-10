package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.{AzatState, CubeStatus}
import pangea.model.home.{HomeData, RecipeNote}
import pangea.model.item.{BrewKind, Item, MaterialKind}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.temple.{CubeState, HallAzatState}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.test._
import zio.{Task, ZIO}

/** Верстак ремесленника: вставленный куб Азата переезжает из Храма домой;
  * работает так же, сила — энергия дома (если есть алтарь Азата) или свои
  * заряды с пополнением прямо на кубе; всё, что вышло, — в книгу рецептов. */
object BenchSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def herb(id: Long, kind: MaterialKind): Item = MaterialGenerator.item(kind).copy(id = id)
  /** Ромашка, календула, крапива — костоправный отвар, две склянки. */
  private val boneSet = List(herb(1L, MaterialKind.Chamomile), herb(2L, MaterialKind.Calendula), herb(3L, MaterialKind.Nettle))

  private def make(home: HomeData, azat: AzatState, silver: Long = 0L, doubloons: Long = 0L) =
    for {
      dao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.Home).copy(silver = silver, doubloons = doubloons))
      _   <- dao.writeHomeData(userId, home.asJson)
      _   <- dao.writeAzatData(userId, azat.asJson)
      inv  = TestInventoryRepository.accepting
      sch <- TestScheduler.make
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (HomeState(dao, sch, c, inv, TestItemRepository.make), CubeState(dao, inv, TestItemRepository.make, c),
             HallAzatState(dao, c), dao, r)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def azatOf(dao: TestHeroDao): Task[AzatState] =
    dao.readAzatData(userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private val cottage   = HomeData(tier = 1, metJunona = true, clutterCleared = true)
  private val withBench = cottage.copy(furniture = List("artisan"))
  private val active    = AzatState(cube = CubeStatus.Active, cubeCharges = 10)

  override def spec = suite("Верстак ремесленника")(

    test("Юнона: верстак — 100 дублонов, одно место; второй — ни к чему") {
      for {
        t <- make(cottage, active, doubloons = 300L)
        (home, _, _, dao, r) = t
        _     <- home.action(testUser, tap("HomeFurnBuy", "f" -> "artisan"), r)
        once  <- homeOf(dao)
        _     <- home.action(testUser, tap("HomeFurnBuy", "f" -> "artisan"), r)
        twice <- homeOf(dao)
        h     <- dao.getHeroByUserId(userId).map(_.get)
        said  <- texts(r)
      } yield assertTrue(once.furniture == List("artisan") && twice.furniture == List("artisan") && h.doubloons == 200L) &&
              assertTrue(said.contains("верстак ремесленника") && said.contains("Второй ни к чему"))
    },

    test("вставить куб: нет куба, спит — так и говорит; активный — переезжает домой, в Храме его больше нет") {
      for {
        t1 <- make(withBench, AzatState.empty)
        (h1, _, _, dao1, r1) = t1
        _    <- h1.action(testUser, tap("HomeCubeInsert"), r1)
        none <- texts(r1)
        t2 <- make(withBench, AzatState(cube = CubeStatus.FoundInactive))
        (h2, _, _, _, r2) = t2
        _    <- h2.action(testUser, tap("HomeCubeInsert"), r2)
        asleep <- texts(r2)
        t3 <- make(withBench, active, silver = 20000L)
        (h3, _, hall, dao3, r3) = t3
        _    <- h3.action(testUser, tap("HomeBench"), r3)
        card <- r3.sentScreens.map(_.last)
        _    <- h3.action(testUser, tap("HomeCubeInsert"), r3)
        moved <- azatOf(dao3)
        open  <- r3.sentScreens.map(_.last)
        _    <- hall.action(testUser, tap("ApproachCube"), r3)
        temple <- r3.sentScreens.map(_.last)
        _    <- hall.action(testUser, tap("RechargeFull"), r3)
        priest <- texts(r3)
        a1   <- azatOf(dao1)
      } yield assertTrue(none.contains("Куба Азата у вас нет") && !a1.cubeHome) &&
              assertTrue(asleep.contains("ещё спит")) &&
              assertTrue(card.choices.map(_.id) == List("HomeCubeInsert", "HomeSlots")) &&
              assertTrue(moved.cubeHome && open.choices.map(_.id) == List("HomeCube", "HomeSlots")) &&
              assertTrue(temple.text.contains("Здесь его больше нет") && !temple.choices.exists(_.id == "OpenCube")) &&
              assertTrue(priest.contains("пополняйте его там"))
    },

    test("куб в домике (без алтаря): свои заряды и пополнение прямо на кубе; «Назад» — домой") {
      for {
        t <- make(withBench, active.copy(cubeHome = true), silver = 20000L)
        (home, cube, _, dao, r) = t
        out  <- home.action(testUser, tap("HomeCube"), r)
        _    <- cube.enter(testUser, r)
        menu <- r.sentScreens.map(_.last)
        _    <- cube.action(testUser, tap("CubeRefillFull"), r)
        a    <- azatOf(dao)
        h    <- dao.getHeroByUserId(userId).map(_.get)
        back <- cube.action(testUser, tap("LeaveCube"), r)
      } yield assertTrue(out == StateType.Cube) &&
              assertTrue(menu.text.contains("Зарядов: 10") && menu.choices.map(_.id).containsSlice(List("CubeRefillFull", "CubeRefillHalf"))) &&
              assertTrue(a.cubeCharges == 60 && h.silver == 10000L) &&
              assertTrue(back == StateType.Home)
    },

    test("куб в доме с алтарём: энергия дома, без кнопок пополнения; вышедшее — в книгу рецептов, книга — в библиотеке") {
      val townhouse = HomeData(tier = 2, metJunona = true, charges = 100, furniture = List("artisan"))
      for {
        t <- make(townhouse, active.copy(cubeHome = true, cubeItems = boneSet))
        (home, cube, _, dao, r) = t
        _     <- cube.enter(testUser, r)
        menu  <- r.sentScreens.map(_.last)
        _     <- cube.action(testUser, tap("CubeActivate"), r)
        after <- homeOf(dao)
        a     <- azatOf(dao)
        _     <- home.action(testUser, tap("HomeLibrary"), r)
        lib   <- r.sentScreens.map(_.last)
        _     <- home.action(testUser, tap("HomeRecipes", "p" -> "0"), r)
        book  <- r.sentScreens.map(_.last)
      } yield assertTrue(menu.text.contains("Энергия дома: 100/100") && !menu.choices.exists(_.id == "CubeRefillFull")) &&
              // сила ушла из дома, заряды куба нетронуты
              assertTrue(after.charges == 99 && a.cubeCharges == 10) &&
              assertTrue(a.cubeItems.flatMap(_.brew) == List(BrewKind.BoneSetter, BrewKind.BoneSetter)) &&
              assertTrue(after.recipes == List(RecipeNote(List("Ромашка", "Календула", "Крапива"), "Костоправный отвар", 2))) &&
              assertTrue(lib.text.contains("В книге рецептов записей: 1.") && lib.choices.exists(_.id == "HomeRecipes")) &&
              assertTrue(book.text.contains("• Ромашка + Календула + Крапива → Костоправный отвар ×2"))
    },

    test("куб в Храме: «Назад» — в зал Азата, книга рецептов пишется и здесь") {
      for {
        t <- make(withBench, active.copy(cubeItems = boneSet))
        (_, cube, _, dao, r) = t
        _    <- cube.action(testUser, tap("CubeActivate"), r)
        home <- homeOf(dao)
        a    <- azatOf(dao)
        back <- cube.action(testUser, tap("LeaveCube"), r)
      } yield assertTrue(home.recipes == List(RecipeNote(List("Ромашка", "Календула", "Крапива"), "Костоправный отвар", 2))) &&
              assertTrue(a.cubeCharges == 9) &&
              assertTrue(back == StateType.HallAzat) &&
              assertTrue(RecipeNote(List("Череп", "Череп", "Череп"), "Расколотый череп", 1).line == "Череп ×3 → Расколотый череп")
    }
  )
}
