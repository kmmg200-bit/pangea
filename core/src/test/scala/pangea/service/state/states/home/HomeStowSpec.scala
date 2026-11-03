package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.artifact.ArtifactKind
import pangea.model.home.{HomeData, HomeRates}
import pangea.model.item.{BrewKind, GemKind, Item, ItemDetails, ItemType, MaterialKind, Rarity}
import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestArtifactRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.test._
import zio.{Task, ZIO}

/** «Разложить всё» на главном экране дома: рунные камни — на алтарь, травы —
  * на полки верстака (из сумки и Живой сумки), камни — в хранилище (из сумки и
  * Ларца). Чего в доме нет, то пропускается; всё прочее остаётся при герое. */
object HomeStowSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val cunning = Rune.Active(Skill.CunningStrike)

  private def stone(id: Long, size: RuneStoneSize): Item = RuneStone.item(cunning, size).copy(id = id)
  private def herb(id: Long, kind: MaterialKind): Item  = MaterialGenerator.item(kind).copy(id = id)
  private def gem(id: Long, kind: GemKind): Item        = GemGenerator.item(kind, 1).copy(id = id)
  private def bottle(id: Long): Item                    = BrewKind.item(BrewKind.values.head).copy(id = id)
  private def sword(id: Long): Item =
    Item(id, "Меч", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0,
      evasion = 0, details = ItemDetails.Weapon(Skill.CunningStrike))

  private val cottage = HomeData(tier = 1, metJunona = true)
  private val island  = HomeData(tier = 3, metJunona = true)

  private def make(home: HomeData, items: List[Item], living: List[Item] = Nil, casket: List[Item] = Nil) =
    for {
      dao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.Home))
      _   <- dao.writeHomeData(userId, home.asJson)
      inv  = TestInventoryRepository.withItems(items)
      art  = TestArtifactRepository.of(
               casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1, items = casket),
               bag    = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1, items = living))
      sch <- TestScheduler.make
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (HomeState(dao, sch, c, inv, TestItemRepository.make, None, Some(art)), dao, inv, art, r)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Дом: «Разложить всё»")(

    test("на острове: руны — на алтарь, травы — на полки, камни — в хранилище; меч и отвар остаются") {
      for {
        t <- make(island,
               items  = List(stone(1L, RuneStoneSize.Big), stone(2L, RuneStoneSize.Small), herb(3L, MaterialKind.Sage),
                             gem(4L, GemKind.Ruby), sword(5L), bottle(6L)),
               living = List(herb(11L, MaterialKind.Chamomile), bottle(12L)),
               casket = List(gem(21L, GemKind.Topaz)))
        (state, dao, inv, art, r) = t
        _     <- state.enter(testUser, r)
        house <- r.sentScreens.map(_.last)
        out   <- state.action(testUser, tap("HomeStowAll"), r)
        home  <- homeOf(dao)
        said  <- texts(r)
        back  <- r.sentScreens.map(_.last)
      } yield assertTrue(house.choices.exists(_.id == "HomeStowAll")) &&
              assertTrue(home.runeCount(cunning.key) == 6L) &&
              assertTrue(home.herbs.map(_.id).toSet == Set(3L, 11L)) &&
              assertTrue(home.gems.map(_.id).toSet == Set(4L, 21L)) &&
              // отвары и всё прочее «Разложить всё» не трогает
              assertTrue(inv.snapshot.map(_.id).toSet == Set(5L, 6L)) &&
              assertTrue(art.snapshot.bag.items.data.map(_.id) == List(12L) && art.snapshot.casket.items.data.isEmpty) &&
              assertTrue(said.contains("Алтарь вбирает узоры") && said.contains("На полки легло трав: 2.") &&
                         said.contains("В хранилище легло камней: 2.")) &&
              // и обратно — на главный экран дома
              assertTrue(out == StateType.Home && back.choices.exists(_.id == "HomeStowAll"))
    },

    test("в домике нет ни верстака, ни хранилища: уходят только руны, травы и камни остаются") {
      for {
        t <- make(cottage, items = List(stone(1L, RuneStoneSize.Small), herb(2L, MaterialKind.Sage), gem(3L, GemKind.Ruby)),
                  living = List(herb(11L, MaterialKind.Chamomile)), casket = List(gem(21L, GemKind.Topaz)))
        (state, dao, inv, art, r) = t
        _    <- state.action(testUser, tap("HomeStowAll"), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield assertTrue(home.runeCount(cunning.key) == 1L && home.herbs.isEmpty && home.gems.isEmpty) &&
              assertTrue(inv.snapshot.map(_.id).toSet == Set(2L, 3L)) &&
              assertTrue(art.snapshot.bag.items.data.map(_.id) == List(11L) &&
                         art.snapshot.casket.items.data.map(_.id) == List(21L)) &&
              assertTrue(said.contains("Алтарь вбирает узоры") && !said.contains("На полки") && !said.contains("В хранилище"))
    },

    test("полки почти полны: ложится, сколько влезет, остальное ждёт при герое — и об этом сказано") {
      val crowded = island.copy(herbs = (1L to (island.herbShelves - 1).toLong).map(i => herb(1000L + i, MaterialKind.Sage)).toList)
      for {
        t <- make(crowded, items = List(herb(2L, MaterialKind.Sage)), living = List(herb(11L, MaterialKind.Chamomile)))
        (state, dao, inv, art, r) = t
        _    <- state.action(testUser, tap("HomeStowAll"), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield assertTrue(home.herbs.size == island.herbShelves && inv.snapshot.isEmpty) &&
              assertTrue(art.snapshot.bag.items.data.map(_.id) == List(11L)) &&
              assertTrue(said.contains("На полки легло трав: 1.") && said.contains("Не поместилось: 1"))
    },

    test("раскладывать нечего — так и сказано, дом не тронут") {
      for {
        t <- make(island, items = List(sword(5L)))
        (state, dao, inv, _, r) = t
        _    <- state.action(testUser, tap("HomeStowAll"), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield assertTrue(said.contains("Раскладывать нечего")) &&
              assertTrue(home == island && inv.snapshot.map(_.id) == List(5L)) &&
              assertTrue(HomeRates.GemVault == 50)
    }
  )
}
