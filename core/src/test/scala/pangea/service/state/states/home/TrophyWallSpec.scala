package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.home.HomeData
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity, TrophyKind}
import pangea.model.monster.{NamedTrophy, Race}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.{PlayerLock, UserAction}
import pangea.service.state.states.guild.TrophyExchangeState
import pangea.service.state.states.merchant.MerchantState
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Стена трофеев на острове: уникальные трофеи именных вешают из сумки — по
  * одному или все разом, каждого именного один раз; Гильдия, Ришелье и
  * передача их не берут. */
object TrophyWallSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def trophy(id: Long, monster: String, race: Race): Item =
    NamedTrophy.item(monster, race.entryName, 5L).fold(Item.NoItem)(_.copy(id = id))

  private val ear    = trophy(11L, "Каркан Кровавый Топор", Race.Orc)
  private val braid  = trophy(12L, "Грумм Две Косы", Race.Orc)
  private val finger = trophy(13L, "Шныга Трёхпалый", Race.Goblin)
  private val ear2   = trophy(14L, "Каркан Кровавый Топор", Race.Orc)

  /** Обычный трофей — для Гильдии. */
  private val head = Item(20L, "Голова (Орк)", 5L, Rarity.Gray, ItemType.Trophy,
    attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
    details = ItemDetails.Trophy(Race.Orc.entryName, TrophyKind.Head))

  private val island    = HomeData(tier = 3, metJunona = true)
  private val townhouse = HomeData(tier = 2, metJunona = true)

  private def make(home: HomeData, items: List[Item]) =
    for {
      dao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.TrophyWall))
      _   <- dao.writeHomeData(userId, home.asJson)
      inv  = TestInventoryRepository.withItems(items)
      sch <- TestScheduler.make
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (TrophyWallState(dao, inv, c), HomeState(dao, sch, c, inv, TestItemRepository.make), dao, inv, r)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Стена трофеев")(

    test("стена — только на острове: кнопка в доме; в добротном доме её нет, и экран не открывается") {
      for {
        t1 <- make(island, Nil)
        (_, house1, _, _, r1) = t1
        _      <- house1.action(testUser, tap("HomeMain"), r1)
        inside <- r1.sentScreens.map(_.last)
        t2 <- make(townhouse, Nil)
        (wall2, house2, _, _, r2) = t2
        _      <- house2.action(testUser, tap("HomeMain"), r2)
        plain  <- r2.sentScreens.map(_.last)
        out    <- wall2.action(testUser, tap("WallMenu"), r2)
      } yield assertTrue(inside.choices.exists(c => c.id == "HomeWall" && c.label == "🏆 Стена трофеев")) &&
              assertTrue(!plain.choices.exists(_.id == "HomeWall") && out == StateType.Home)
    },

    test("экран: сколько собрано всего и по расам, расы кнопками; повесить можно то, чего на стене нет") {
      for {
        t <- make(island.copy(wall = List("Каркан Кровавый Топор")), List(braid, finger, ear2))
        (wall, _, _, _, r) = t
        _      <- wall.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(screen.text.contains("Собрано: 1 из 96.")) &&
              assertTrue(screen.text.contains("Орки — 1 из 12 · +1% урона против орков")) &&
              assertTrue(screen.text.contains("Гоблины — 0 из 12\n")) &&
              assertTrue(screen.choices.count(_.id == "WallRace") == 8) &&
              assertTrue(screen.choices.exists(c => c.id == "WallRace" && c.label == "Орки 1/12")) &&
              // ухо Каркана уже висит — второе в счёт не идёт
              assertTrue(screen.choices.exists(c => c.id == "WallHang" && c.label == "🪝 Повесить трофеи (2)"))
    },

    test("повесить один: из сумки на стену, прибавка растёт; второй такой же — «уже висит», остаётся в сумке") {
      for {
        t <- make(island, List(ear, ear2))
        (wall, _, dao, inv, r) = t
        _     <- wall.action(testUser, tap("WallHang"), r)
        list  <- r.sentScreens.map(_.last)
        _     <- wall.action(testUser, tap("WallPut", "id" -> "11"), r)
        once  <- homeOf(dao)
        bag1   = inv.snapshot.map(_.id)
        said1 <- texts(r)
        _     <- wall.action(testUser, tap("WallPut", "id" -> "14"), r)
        twice <- homeOf(dao)
        said2 <- texts(r)
      } yield assertTrue(list.choices.count(_.id == "WallPut") == 1 &&
                list.choices.exists(_.label == "🪝 Ухо Каркана Кровавого Топора")) &&
              assertTrue(once.wall == List("Каркан Кровавый Топор") && bag1 == List(14L)) &&
              assertTrue(said1.contains("Вы вешаете на стену: Ухо Каркана Кровавого Топора. Против орков теперь +1% урона.")) &&
              assertTrue(twice.wall == List("Каркан Кровавый Топор") && inv.snapshot.map(_.id) == List(14L)) &&
              assertTrue(said2.contains("уже висит на стене"))
    },

    test("повесить все: каждого именного по разу, повторы остаются в сумке") {
      for {
        t <- make(island, List(ear, ear2, braid, finger, head))
        (wall, _, dao, inv, r) = t
        _    <- wall.action(testUser, tap("WallPutAll"), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield assertTrue(home.wall == List("Каркан Кровавый Топор", "Грумм Две Косы", "Шныга Трёхпалый")) &&
              assertTrue(home.wallPct == Map(Race.Orc.entryName -> 2, Race.Goblin.entryName -> 1)) &&
              assertTrue(inv.snapshot.map(_.id) == List(14L, 20L)) &&
              assertTrue(said.contains("Вы развешиваете по стене: Ухо Каркана Кровавого Топора, Коса Грумма Двух Кос, Палец Шныги Трёхпалого."))
    },

    test("страница расы: висящие — по названию, на месте прочих — пустой крюк") {
      for {
        t <- make(island.copy(wall = List("Каркан Кровавый Топор")), Nil)
        (wall, _, _, _, r) = t
        _    <- wall.action(testUser, tap("WallRace", "r" -> Race.Orc.entryName), r)
        page <- r.sentScreens.map(_.last)
      } yield assertTrue(page.text.startsWith("🏆 Орки — 1 из 12 · +1% урона против орков")) &&
              assertTrue(page.text.contains("🏆 Ухо Каркана Кровавого Топора")) &&
              assertTrue(page.text.split("\n").count(_ == "❔ Пустой крюк") == 11) &&
              assertTrue(page.choices.map(_.id) == List("WallMenu"))
    },

    test("в сумке вешать нечего — так и говорит") {
      for {
        t <- make(island, List(head))
        (wall, _, _, _, r) = t
        _    <- wall.action(testUser, tap("WallHang"), r)
        said <- texts(r)
      } yield assertTrue(said.contains("В сумке нет уникальных трофеев, которых ещё нет на стене."))
    },

    test("Гильдия, Ришелье и передача уникальных трофеев не берут") {
      for {
        t <- make(island, List(head, ear))
        (_, _, dao, inv, r) = t
        c         <- ZIO.attempt(SceneContent.load())
        _         <- TrophyExchangeState(dao, inv, c).action(testUser, tap("SubmitTrophies"), r)
        userRepo  <- TestUserRepository.withUser(testUser)
        lock      <- PlayerLock.make
        transfers  = Transfers(TestInventoryRepository.accepting, userRepo,
                       Parcels(TestParcelDao.empty, TestBankRepository.empty, c), new TestPlayers, c, dao,
                       TestBankRepository.empty, Payouts(TestPayoutDao.empty, dao, c), lock)
      } yield assertTrue(inv.snapshot.map(_.id) == List(11L)) &&
              assertTrue(!MerchantState.isJunk(ear, MerchantState.JunkSaleSettings(rarities = Rarity.values.toSet, trophies = true))) &&
              assertTrue(!transfers.sendable(ear)) &&
              assertTrue(transfers.refusal(List(ear), "Ухо Каркана").contains("transfer.noNamedTrophies")) &&
              assertTrue(transfers.refusal(List(head), "Голова").contains("transfer.noTrophies"))
    }
  )
}
