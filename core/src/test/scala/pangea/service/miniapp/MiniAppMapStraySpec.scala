package pangea.service.miniapp

import pangea.engine.SceneContent
import pangea.generator.item.{GemGenerator, TreasureMapGenerator}
import pangea.model.artifact.HeroArtifacts
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.hero.Hero
import pangea.model.home.HomeData
import pangea.model.inventory.Inventory
import pangea.model.item.{GemKind, ItemType, QuestItemKind}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.user.UserId
import pangea.service.equipment.{MapWork, StrayFeedWork}
import pangea.service.state.{NpcQuestLog, StrayCasketQuest}
import pangea.test.{TestArtifactRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository}
import zio.test._

/** Миниапп: половинки карт и кормление бродячего ларца — те же кнопки и правила,
  * что на карточке в боте. */
object MiniAppMapStraySpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val base: Hero = TestFixtures.hero(userId).copy(equipment = TestFixtures.emptyEquipment)
  private lazy val content: SceneContent = SceneContent.load()

  private val halfA = TreasureMapGenerator.create(dropLevel = 10, half = true).copy(id = 30L)
  private val halfB = TreasureMapGenerator.create(dropLevel = 10, half = true).copy(id = 31L)
  private val casket = QuestItemKind.item(QuestItemKind.StrayCasket).copy(id = 40L)

  private def view(items: List[pangea.model.item.Item], quests: NpcQuests = NpcQuests.empty,
                   feeds: Map[Long, List[(Int, String)]] = Map.empty): MiniAppView.InventoryView =
    MiniAppView.inventory(base, Inventory(1L, base.id, 20L, Inventory.Items(items)),
      MiniAppView.Stores(HeroArtifacts.empty(base.id), Barrel.empty(base.id), BankVault.empty(base.id), HomeData.empty),
      content, _.name, 0L, quests, feeds)

  override def spec = suite("MiniApp: карта клада и бродячий ларец")(
    test("у половинки карты в сумке — кнопка «Объединить»") {
      val acts = view(List(halfA, halfB)).bagActs
      assertTrue(acts.find(_.item == halfA.id).flatMap(_.combineMap).contains("Объединить"))
    },

    test("MapWork: две половинки одной зоны — целая карта") {
      val bag  = TestInventoryRepository.withItems(List(halfA, halfB))
      val work = new MapWork(bag, TestItemRepository.make, content)
      for {
        inv  <- bag.get(base.id).mapError(e => new Throwable(e.toString))
        note <- work.combine(base.id, inv, halfA.id)
        left <- bag.get(base.id).map(_.items.data)
      } yield assertTrue(note.exists(_.contains("сложили две половинки"))) &&
        assertTrue(left.size == 1 && left.head.itemType == ItemType.TreasureMap)
    },

    test("голодный ларец с камнем — кнопки кормления по качеству") {
      val hungry = NpcQuests.empty.take(NpcQuest.StrayCasket, 0L)
      val gem    = GemGenerator.item(GemKind.Ruby, 1).copy(id = 50L)
      val labels = List((1, "Надколотый (1)"))
      val acts   = view(List(casket, gem), hungry, Map(casket.id -> labels)).bagActs
      assertTrue(acts.find(_.item == casket.id).exists(_.strayFeed == List(MiniAppView.FeedGradeView(1, "Надколотый (1)"))))
    },

    test("StrayFeedWork: кормление записывает счётчик") {
      val bag   = TestInventoryRepository.withItems(List(casket, GemGenerator.item(GemKind.Ruby, 1).copy(id = 50L)))
      val arts  = TestArtifactRepository.empty
      val start = NpcQuests.empty.take(NpcQuest.StrayCasket, 0L)
      for {
        dao  <- TestHeroDao.withHero(userId, base)
        _    <- NpcQuestLog.save(dao, userId, start)
        work  = new StrayFeedWork(dao, bag, arts, content)
        note <- work.feed(userId, base.id, casket, 1)
        q    <- NpcQuestLog.load(dao, userId)
      } yield assertTrue(note.exists(_.contains("Скормлено: 1/10"))) &&
        assertTrue(StrayCasketQuest.fed(q) == 1L)
    }
  )
}