package pangea.service.miniapp

import pangea.engine.SceneContent
import pangea.model.artifact.HeroArtifacts
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.hero.Knowledge
import pangea.model.home.HomeData
import pangea.model.inventory.Inventory
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Item, ItemType, QuestItemKind, Rarity}
import pangea.model.stats.BaseStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.HerbLore
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository}
import zio.test._
import zio.{Duration, Ref, ZIO}

/** «Читать» у трактата из приложения: тот же бросок и те же слова, что на его
  * карточке в боте, только сказанное идёт в ответ приложению. Сами правила
  * проверены через экран — в FlowerMeadowSpec. */
object MiniAppTreatiseSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("1"), TelegramId("-1"))
  private val book   = QuestItemKind.item(QuestItemKind.FlowerTreatise1).copy(id = 9L)
  private val helm   =
    Item(20L, "Шлем", 1L, Rarity.Blue, ItemType.Helmet, attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0)

  /** Интеллект 40 — книгу осиливают с броска 20 и ниже; в сумке — `items`. */
  private def reading(items: List[Item]) =
    for {
      c     <- ZIO.attempt(SceneContent.load())
      hero   = TestFixtures.hero(userId).copy(baseStats = BaseStats(agi = 10, vit = 10, str = 10, int = 40L))
      dao   <- TestHeroDao.withHero(userId, hero)
      bag    = TestInventoryRepository.withItems(items)
      lines <- Ref.make(Vector.empty[String])
    } yield (hero, dao, bag, new MiniApp.Collect(lines), lines, c)

  def spec = suite("MiniApp: трактаты")(
    test("осилил: знание по книге, книга уходит; слова — как в боте") {
      for {
        t <- reading(List(book))
        (hero, dao, bag, say, lines, c) = t
        _     <- TestRandom.feedInts(20)                      // 20 ≤ 40 ÷ 2
        found <- HerbLore.readTreatise(dao, bag, c, user, hero, book, say)
        lore  <- HerbLore.readLore(dao, userId)
        said  <- lines.get
      } yield assertTrue(
        found,
        lore.knows(Knowledge.FlowersRank1),
        bag.snapshot.isEmpty,
        said == Vector(c.text("knowledge.readSpent"),
          c.text("knowledge.readSuccess") + "\n" + c.format("knowledge.gained", "title" -> Knowledge.FlowersRank1.title))
      )
    },
    test("не осилил: книга при герое, час «переварить»; раньше — через сколько минут") {
      for {
        t <- reading(List(book))
        (hero, dao, bag, say, lines, c) = t
        _     <- TestRandom.feedInts(21)                      // 21 > 20
        first <- HerbLore.readTreatise(dao, bag, c, user, hero, book, say)
        lore  <- HerbLore.readLore(dao, userId)
        _     <- TestClock.adjust(Duration.fromMillis(HerbLore.ReadingCooldownMs - 60000L))
        again <- HerbLore.readTreatise(dao, bag, c, user, hero, book, say)
        said  <- lines.get
      } yield assertTrue(
        first && again,
        !lore.knows(Knowledge.FlowersRank1),
        lore.failuresOf(QuestItemKind.FlowerTreatise1.entryName) == 1,
        bag.snapshot.map(_.id) == List(9L),
        said == Vector(c.text("knowledge.readSpent"), c.text("knowledge.readFailed"),
          c.format("knowledge.readCooldown", "mins" -> "1"))
      )
    },
    test("не трактат — читать нечего, ничего не сказано") {
      for {
        t <- reading(List(helm))
        (hero, dao, bag, say, lines, c) = t
        found <- HerbLore.readTreatise(dao, bag, c, user, hero, helm, say)
        said  <- lines.get
      } yield assertTrue(!found, said.isEmpty, bag.snapshot.size == 1)
    },
    test("в сумке у трактата — «Читать»; у письма Марисы и у шлема — нет") {
      val letter = QuestItemKind.item(QuestItemKind.MarisaLetter).copy(id = 10L)
      for {
        c   <- ZIO.attempt(SceneContent.load())
        hero = TestFixtures.hero(userId)
        v    = MiniAppView.inventory(hero, Inventory(1L, hero.id, 20L, Items(List(book, letter, helm))),
                 MiniAppView.Stores(HeroArtifacts.empty(hero.id), Barrel.empty(hero.id), BankVault.empty(hero.id), HomeData.empty),
                 c, _.name, 0L)
      } yield assertTrue(
        v.bagActs.find(_.item == 9L).exists(_.read),
        v.bagActs.find(_.item == 10L).forall(!_.read),
        v.bagActs.find(_.item == 20L).forall(!_.read)
      )
    }
  )
}
