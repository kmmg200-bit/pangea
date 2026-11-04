package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.{AzatState, CubeStatus, Equipment}
import pangea.model.home.{Furniture, HomeData, House}
import pangea.model.item.{GemKind, Item, ItemType, MaterialKind, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.generator.item.GemGenerator
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.test._
import zio.{Task, ZIO}

/** Кладовая дома: мест под обстановку нет, а вещь хотят купить или вернуть —
  * Юнона спрашивает, что спрятать. Спрятанное места не занимает и не работает,
  * возвращается через «Обстановку», а при переезде в дом просторнее встаёт само.
  * Прячут только пустое: стойку без комплекта, верстак без куба, сундук, без
  * которого полкам хватает места. */
object HomeStashSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def stash(what: Furniture, goal: String, target: Furniture): UserAction =
    tap("HomeStash", "f" -> what.key, "for" -> goal, "t" -> target.key)

  private def make(home: HomeData, doubloons: Long = 0L, azat: AzatState = AzatState.empty) =
    for {
      dao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.Home).copy(doubloons = doubloons))
      _   <- dao.writeHomeData(userId, home.asJson)
      _   <- dao.writeAzatData(userId, azat.asJson)
      inv  = TestInventoryRepository.accepting
      sch <- TestScheduler.make
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (HomeState(dao, sch, c, inv, TestItemRepository.make), dao, inv, r, c)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def doubloonsOf(dao: TestHeroDao): Task[Long] = dao.getHeroByUserId(userId).map(_.get.doubloons)
  private def last(r: TestRenderer)  = r.sentScreens.map(_.last)
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  /** Домик без хлама, оба места заняты: верстак и посыльный. */
  private val fullCottage = HomeData(tier = 1, metJunona = true, clutterCleared = true,
    furniture = List(Furniture.ArtisanBench.key, Furniture.Courier.key))

  private val helmet = Item(7L, "Шлем", 1L, Rarity.Blue, ItemType.Helmet,
    attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0)

  override def spec = suite("Кладовая дома")(

    test("мест нет — «Что спрячем…?»; спрятали посыльного — стойка куплена, посыльный в кладовой и не работает") {
      for {
        t <- make(fullCottage, doubloons = 300L)
        (state, dao, inv, r, _) = t
        _      <- state.action(testUser, tap("HomeFurnBuy", "f" -> Furniture.Rack.key), r)
        ask    <- last(r)
        _      <- state.action(testUser, stash(Furniture.Courier, "buy", Furniture.Rack), r)
        home   <- homeOf(dao)
        coins  <- doubloonsOf(dao)
        said   <- texts(r)
      } yield assertTrue(ask.text.contains("Что спрячем из текущей обстановки дома?")) &&
              // на выбор — то, что стоит в доме
              assertTrue(ask.choices.filter(_.id == "HomeStash").map(_.label) ==
                List("🛠 Верстак ремесленника", "🏃 Личный посыльный")) &&
              assertTrue(home.furniture == List(Furniture.ArtisanBench.key, Furniture.Rack.key)) &&
              assertTrue(home.stored == List(Furniture.Courier.key) && !home.hasCourier) &&
              assertTrue(home.racks.size == 1 && coins == 200L && inv.snapshot.size == 1) &&
              assertTrue(said.contains("📦 В кладовую: 🏃 Личный посыльный.") && said.contains("стойка №1"))
    },

    test("мест нет и дублонов не хватает — сперва об этом, прятать ничего не предлагают") {
      for {
        t <- make(fullCottage, doubloons = 50L)
        (state, dao, _, r, _) = t
        _    <- state.action(testUser, tap("HomeFurnBuy", "f" -> Furniture.Rack.key), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield assertTrue(!said.contains("Что спрячем") && home == fullCottage) &&
              assertTrue(said.contains("Не хватает 50 🟡"))
    },

    test("кнопка «спрятать» без дублонов на покупку ничего не прячет") {
      for {
        t <- make(fullCottage, doubloons = 50L)
        (state, dao, _, r, _) = t
        _    <- state.action(testUser, stash(Furniture.Courier, "buy", Furniture.Rack), r)
        home <- homeOf(dao)
      } yield assertTrue(home == fullCottage)
    },

    test("прячут только пустое: стойку с комплектом, верстак с кубом, сундуки при полных полках — нет") {
      val rackHome = HomeData(tier = 1, metJunona = true, clutterCleared = true,
        furniture = List(Furniture.Rack.key, Furniture.Courier.key),
        racks = List(TestFixtures.emptyEquipment.copy(helmet = helmet)))
      val herbs = (1 to 60).toList.map(i => MaterialGenerator.item(MaterialKind.Chamomile).copy(id = 100L + i))
      val chest = HomeData(tier = 2, metJunona = true, herbs = herbs,
        furniture = List(Furniture.ApothecaryChest.key, Furniture.Courier.key, Furniture.Rack.key, Furniture.ArtisanBench.key),
        racks = List(Equipment.empty))
      val gems = (1 to 51).toList.map(i => GemGenerator.item(GemKind.Ruby, 1).copy(id = 500L + i))
      val island = HomeData(tier = 3, metJunona = true, gems = gems,
        furniture = List.fill(9)(Furniture.ApothecaryChest.key) :+ Furniture.JewelerChest.key)
      for {
        t1 <- make(rackHome, doubloons = 300L)
        (s1, d1, _, r1, _) = t1
        _   <- s1.action(testUser, stash(Furniture.Rack, "buy", Furniture.ArtisanBench), r1)
        h1  <- homeOf(d1)
        w1  <- texts(r1)
        t2 <- make(fullCottage, doubloons = 300L, azat = AzatState(cube = CubeStatus.Active, cubeHome = true))
        (s2, d2, _, r2, _) = t2
        _   <- s2.action(testUser, stash(Furniture.ArtisanBench, "buy", Furniture.Rack), r2)
        h2  <- homeOf(d2)
        w2  <- texts(r2)
        t3 <- make(chest, doubloons = 300L)
        (s3, d3, _, r3, _) = t3
        _   <- s3.action(testUser, stash(Furniture.ApothecaryChest, "back", Furniture.Courier), r3)
        h3  <- homeOf(d3)
        w3  <- texts(r3)
        t4 <- make(island, doubloons = 300L)
        (s4, d4, _, r4, _) = t4
        _   <- s4.action(testUser, stash(Furniture.JewelerChest, "buy", Furniture.ApothecaryChest), r4)
        h4  <- homeOf(d4)
        w4  <- texts(r4)
      } yield assertTrue(h1 == rackHome && w1.contains("На стойке №1 висит комплект")) &&
              assertTrue(h2 == fullCottage && w2.contains("В верстаке стоит куб Азата")) &&
              assertTrue(h3 == chest && w3.contains("трав должно остаться не больше 50, отваров — не больше 25")) &&
              assertTrue(h4 == island && w4.contains("камней должно остаться не больше 50"))
    },

    test("кладовая: вернуть — есть место, встаёт сразу; места нет — спрятать взамен") {
      val cottage = HomeData(tier = 1, metJunona = true, clutterCleared = true,
        furniture = List(Furniture.ArtisanBench.key), stored = List(Furniture.Courier.key))
      for {
        t <- make(cottage)
        (state, dao, _, r, _) = t
        _      <- state.action(testUser, tap("HomeSlots"), r)
        slots  <- last(r)
        _      <- state.action(testUser, tap("HomeStore"), r)
        store  <- last(r)
        _      <- state.action(testUser, tap("HomeUnstash", "f" -> Furniture.Courier.key), r)
        back   <- homeOf(dao)
        // теперь мест нет: верстак — в кладовую, посыльный… уже стоит; вернём
        // верстак, спрятав посыльного
        _      <- dao.writeHomeData(userId, back.hidden(Furniture.ArtisanBench).copy(furniture =
                    List(Furniture.Courier.key, Furniture.Rack.key), racks = List(Equipment.empty)).asJson)
        _      <- state.action(testUser, tap("HomeUnstash", "f" -> Furniture.ArtisanBench.key), r)
        ask    <- last(r)
        _      <- state.action(testUser, stash(Furniture.Courier, "back", Furniture.ArtisanBench), r)
        swap   <- homeOf(dao)
        said   <- texts(r)
      } yield assertTrue(slots.text.contains("📦 В кладовой: 1.") && slots.choices.exists(_.label == "📦 Кладовая (1)")) &&
              assertTrue(store.text.contains("• 🏃 Личный посыльный") &&
                store.choices.exists(_.label == "Вернуть: 🏃 Личный посыльный")) &&
              assertTrue(back.stored.isEmpty && back.hasCourier) &&
              assertTrue(ask.text.contains("Что спрячем") && ask.choices.exists(_.label == "🏃 Личный посыльный")) &&
              assertTrue(swap.furniture.toSet == Set(Furniture.Rack.key, Furniture.ArtisanBench.key) &&
                swap.stored == List(Furniture.Courier.key)) &&
              assertTrue(said.contains("🪑 🛠 Верстак ремесленника — снова в доме."))
    },

    test("то, что в кладовой, второй раз не продают — его возвращают") {
      val cottage = HomeData(tier = 1, metJunona = true, clutterCleared = true, stored = List(Furniture.Courier.key))
      for {
        t <- make(cottage, doubloons = 300L)
        (state, dao, _, r, _) = t
        _     <- state.action(testUser, tap("HomeFurnBuy", "f" -> Furniture.Courier.key), r)
        home  <- homeOf(dao)
        coins <- doubloonsOf(dao)
        said  <- texts(r)
      } yield assertTrue(home == cottage && coins == 300L && said.contains("лежит в кладовой"))
    },

    test("стойка в кладовой — всегда старшая: её кнопки нет, ключ от неё ждёт, младшие работают") {
      val town = HomeData(tier = 2, metJunona = true,
        furniture = List(Furniture.Rack.key, Furniture.Rack.key, Furniture.Courier.key, Furniture.ArtisanBench.key),
        racks = List(Equipment.empty, Equipment.empty))
      for {
        t <- make(town, doubloons = 300L)
        (state, dao, inv, r, c) = t
        _      <- state.action(testUser, tap("HomeFurnBuy", "f" -> Furniture.ApothecaryChest.key), r)
        ask    <- last(r)
        _      <- state.action(testUser, stash(Furniture.Rack, "buy", Furniture.ApothecaryChest), r)
        home   <- homeOf(dao)
        _      <- state.action(testUser, tap("HomeSlots"), r)
        slots  <- last(r)
        key2   <- Racks(dao, inv, c).swap(testUser, 2, r)
        key1   <- Racks(dao, inv, c).swap(testUser, 1, r)
      } yield assertTrue(ask.choices.exists(_.label == "🧥 Магическая стойка №2")) &&
              assertTrue(home.activeRacks == 1 && home.racks.size == 2 && home.stored == List(Furniture.Rack.key)) &&
              assertTrue(home.count(Furniture.ApothecaryChest) == 1) &&
              assertTrue(slots.choices.count(_.id == "HomeRack") == 1) &&
              assertTrue(key2.left.exists(_.contains("Стойка №2 убрана в кладовую"))) &&
              // №1 на месте: с пустыми и стойкой, и героем — «меняться нечем», а не «убрана»
              assertTrue(!key1.left.exists(_.contains("кладовую")))
    },

    test("переезд в дом просторнее: из кладовой встаёт спрятанное первым — сколько хватит мест") {
      val cottage = HomeData(tier = 1, metJunona = true, clutterCleared = true,
        furniture = List(Furniture.ArtisanBench.key, Furniture.Courier.key),
        stored = List(Furniture.Rack.key, Furniture.ApothecaryChest.key, Furniture.JewelerChest.key),
        racks = List(Equipment.empty))
      for {
        t <- make(cottage, doubloons = 1000L)
        (state, dao, _, r, _) = t
        _    <- state.action(testUser, tap("HomeBuy", "h" -> House.Townhouse.key, "pay" -> HomeState.PayDoubloons), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield // четыре места: двое стояли, двое встали из кладовой, третий ждёт
              assertTrue(home.tier == 2 && home.furniture == List(Furniture.ArtisanBench.key, Furniture.Courier.key,
                Furniture.Rack.key, Furniture.ApothecaryChest.key)) &&
              assertTrue(home.stored == List(Furniture.JewelerChest.key) && home.activeRacks == 1) &&
              assertTrue(said.contains("📦 Из кладовой в новый дом встало: 🧥 Магическая стойка, 📦 Сундук аптекаря."))
    }
  )
}
