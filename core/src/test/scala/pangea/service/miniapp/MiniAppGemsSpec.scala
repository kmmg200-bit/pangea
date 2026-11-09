package pangea.service.miniapp

import pangea.engine.SceneContent
import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Gem, GemKind, Item, ItemDetails, ItemType, Rarity}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.equipment.GemWork
import pangea.service.storage.ItemStore
import pangea.test.{TestArtifactRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository}
import zio.test._
import zio.{Ref, ZIO}

/** То, чего нет в экранах бота: камень берут не только из сумки, а итог идёт
  * в ответ приложению. Сами правила проверены через экраны — ломка в
  * EquipmentStateSpec, вставка в SocketingStateSpec, резец в RuneChiselSpec. */
object MiniAppGemsSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("1"), TelegramId("-1"))
  private val gem    = Gem(GemKind.Ruby, Gem.MinGrade)

  private def helm(sockets: Option[Gem]*): Item =
    Item(20L, "Шлем", 1L, Rarity.Blue, ItemType.Helmet, attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0,
      sockets = sockets.toList)

  private val stone: Item =
    Item(5L, "Надколотый рубин", 1L, Rarity.Gray, ItemType.Gem, attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Gem(gem))

  private def setup(worn: Item) =
    for {
      c     <- ZIO.attempt(SceneContent.load())
      hero   = TestFixtures.hero(userId).copy(equipment = TestFixtures.emptyEquipment.copy(helmet = worn))
      dao   <- TestHeroDao.withHero(userId, hero)
      bag    = TestInventoryRepository.withItems(Nil)
      arts   = new TestArtifactRepository(HeroArtifacts.empty(hero.id).copy(casket = Artifact(ArtifactKind.Casket, 1, 0, Items(List(stone)))))
      casket = new ItemStore.Artifact(arts, ArtifactKind.Casket, c)
      lines <- Ref.make(Vector.empty[String])
    } yield (hero, dao, bag, arts, casket, new GemWork(dao, bag, TestItemRepository.make, c), new MiniApp.Collect(lines), lines, c)

  def spec = suite("MiniApp: камни и резец")(
    test("камень из Ларца: лёг в гнездо, из Ларца ушёл, сумка не тронута") {
      for {
        t <- setup(helm(None))
        (hero, dao, bag, arts, casket, work, say, lines, c) = t
        ok     <- work.insert(user, hero, 20L, gem, casket.remove(hero, stone).unit, say)
        saved  <- dao.getHeroByUserId(userId).someOrFailException
        inside <- arts.get(hero.id).map(_.casket.items.data)
        said   <- lines.get
      } yield assertTrue(
        ok,
        saved.equipment.helmet.socketedGems == List(gem),
        inside.isEmpty,
        bag.snapshot.isEmpty,
        said == Vector(c.format("socketing.done", "gem" -> gem.displayName, "item" -> "Шлем"))
      )
    },
    test("свободного гнезда нет: не вставлено, камень на месте, ничего не сказано") {
      for {
        t <- setup(helm(Some(gem)))
        (hero, dao, _, arts, casket, work, say, lines, _) = t
        ok     <- work.insert(user, hero, 20L, gem, casket.remove(hero, stone).unit, say)
        saved  <- dao.getHeroByUserId(userId).someOrFailException
        inside <- arts.get(hero.id).map(_.casket.items.data)
        said   <- lines.get
      } yield assertTrue(!ok, saved.equipment.helmet.socketedGems == List(gem), inside.map(_.id) == List(5L), said.isEmpty)
    },
    test("ломка: пустое гнездо — ничего; камень — пыль в сумку, гнездо свободно") {
      for {
        t <- setup(helm(None, Some(gem)))
        (hero, dao, bag, _, _, work, say, lines, c) = t
        empty <- work.breakWorn(user, hero, hero.equipment.helmet, 0, say)
        silent <- lines.get
        done  <- work.breakWorn(user, hero, hero.equipment.helmet, 1, say)
        saved <- dao.getHeroByUserId(userId).someOrFailException
        said  <- lines.get
      } yield assertTrue(
        !empty && silent.isEmpty,
        done,
        saved.equipment.helmet.sockets == List(None, None),
        bag.snapshot.size == gem.dustYield,
        said == Vector(c.format("equipment.breakGemDone", "gem" -> gem.displayName,
          "count" -> gem.dustYield.toString, "dust" -> gem.dust.displayName))
      )
    },
    test("резец и вещь без узора, которого он не возьмёт: не переписано, резец цел") {
      val flask = helm().copy(itemType = ItemType.Flask)
      for {
        t <- setup(flask)
        (hero, _, bag, _, _, work, say, lines, _) = t
        inv  <- bag.get(hero.id).orElseFail(new Throwable("сумка"))
        ok   <- work.chisel(user, hero, inv, flask, worn = true, stone, say)
        said <- lines.get
      } yield assertTrue(!ok, said.isEmpty)
    }
  )
}
