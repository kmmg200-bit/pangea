package pangea.service.storage

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.home.{HomeData, House}
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Item, ItemType, MaterialKind, Rarity}
import pangea.model.user.UserId
import pangea.test.{TestArtifactRepository, TestBankRepository, TestFixtures, TestHeroDao, TestInventoryRepository}
import zio.ZIO
import zio.test._

/** Перенос вещи между хранилищами: сперва туда, потом отсюда — откажет
  * новое место, вещь останется где была; сумка для этого не нужна. */
object ItemStoreSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val hero   = TestFixtures.hero(userId)
  private val gone   = "её уже нет"

  private def sword(id: Long): Item =
    Item(id, "Меч", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  private def herb(id: Long): Item = MaterialGenerator.item(MaterialKind.Chamomile).copy(id = id)

  /** Шкаф первой ступени — три места — и в нём `inside`. */
  private def wardrobe(inside: List[Item]): TestArtifactRepository =
    new TestArtifactRepository(HeroArtifacts.empty(hero.id).copy(
      wardrobe = Artifact(ArtifactKind.Wardrobe, 1, 0, Items(inside))))

  def spec = suite("ItemStore: перенос между хранилищами")(
    test("из ячейки в шкаф — напрямую, сумка не нужна и не тронута") {
      for {
        c     <- ZIO.attempt(SceneContent.load())
        bank   = new TestBankRepository(cells = 1, items = List(sword(5L)))
        arts   = wardrobe(Nil)
        bag    = TestInventoryRepository.full
        res   <- ItemStore.move(new ItemStore.Vault(bank, c), new ItemStore.Artifact(arts, ArtifactKind.Wardrobe, c),
                   hero, sword(5L), gone)
        inside <- arts.get(hero.id).map(_.wardrobe.items.data.map(_.id))
      } yield assertTrue(res.isEmpty, bank.itemsSnapshot.isEmpty, inside == List(5L), bag.snapshot.isEmpty)
    },
    test("новое место отказало — вещь осталась где была, и сказано почему") {
      for {
        c      <- ZIO.attempt(SceneContent.load())
        bank    = new TestBankRepository(cells = 1, items = List(sword(5L)))
        arts    = wardrobe(List(sword(1L), sword(2L), sword(3L)))
        res    <- ItemStore.move(new ItemStore.Vault(bank, c), new ItemStore.Artifact(arts, ArtifactKind.Wardrobe, c),
                    hero, sword(5L), gone)
        noShop  = new TestArtifactRepository(HeroArtifacts.empty(hero.id))
        res2   <- ItemStore.move(new ItemStore.Vault(bank, c), new ItemStore.Artifact(noShop, ArtifactKind.Wardrobe, c),
                    hero, sword(5L), gone)
      } yield assertTrue(
        res.contains(c.text("artifact.wardrobe.full")),
        res2.contains(c.text("artifact.wardrobe.notOwned")),
        bank.itemsSnapshot.map(_.id) == List(5L)
      )
    },
    test("вещи на прежнем месте уже нет — «её уже нет», новое место не тронуто") {
      for {
        c    <- ZIO.attempt(SceneContent.load())
        bank  = new TestBankRepository(cells = 1)
        arts  = wardrobe(Nil)
        res  <- ItemStore.move(new ItemStore.Vault(bank, c), new ItemStore.Artifact(arts, ArtifactKind.Wardrobe, c),
                  hero, sword(5L), gone)
        inside <- arts.get(hero.id).map(_.wardrobe.items.data)
      } yield assertTrue(res.contains(gone), inside.isEmpty)
    },
    test("полки дома: трава — на полку, пока есть место; полка полна или её нет — почему") {
      val townhouse = HomeData.empty.copy(tier = House.Townhouse.tier)
      for {
        c     <- ZIO.attempt(SceneContent.load())
        dao   <- TestHeroDao.withHero(userId, hero)
        shelf  = new ItemStore.HomeShelf(dao, ItemStore.HomeShelf.Herbs, c)
        _     <- dao.writeHomeData(userId, townhouse.copy(herbs = List.tabulate(townhouse.herbShelves - 1)(i => herb(100L + i))).asJson)
        bag    = TestInventoryRepository.withItems(List(herb(1L), herb(2L)))
        store  = new ItemStore.Bag(bag, _ => "в сумке нет места")
        first <- ItemStore.move(store, shelf, hero, herb(1L), gone)
        full  <- ItemStore.move(store, shelf, hero, herb(2L), gone)
        onIt  <- shelf.items(hero).map(_.map(_.id))
        _     <- dao.writeHomeData(userId, HomeData.empty.copy(tier = 1).asJson)
        none  <- ItemStore.move(store, shelf, hero, herb(2L), gone)
      } yield assertTrue(
        first.isEmpty && onIt.contains(1L) && onIt.size == townhouse.herbShelves,
        full.contains(c.text("miniapp.storages.herbs.full")),
        none.contains(c.text("miniapp.storages.herbs.locked")),
        bag.snapshot.map(_.id) == List(2L)
      )
    },
    test("что куда берут: в Ларец — камни, в шкаф — не невесомое, на полку трав — травы; сюжетное — никуда") {
      for {
        c <- ZIO.attempt(SceneContent.load())
        arts   = new TestArtifactRepository(HeroArtifacts.empty(hero.id))
        casket = new ItemStore.Artifact(arts, ArtifactKind.Casket, c)
        wr     = new ItemStore.Artifact(arts, ArtifactKind.Wardrobe, c)
        bank   = new ItemStore.Vault(new TestBankRepository(), c)
        dao   <- TestHeroDao.withHero(userId, hero)
        herbs  = new ItemStore.HomeShelf(dao, ItemStore.HomeShelf.Herbs, c)
        gem    = GemGenerator.item(pangea.model.item.GemKind.Ruby, 1).copy(id = 9L)
        quest  = sword(7L).copy(itemType = ItemType.QuestItem)
      } yield assertTrue(
        casket.accepts(gem) && !casket.accepts(sword(1L)),
        wr.accepts(sword(1L)),
        herbs.accepts(herb(1L)) && !herbs.accepts(gem),
        List(casket, wr, bank, herbs).forall(!_.accepts(quest))
      )
    },
    test("выбросить: одну — ровно её, стопку — всю; сюжетное не трогаем; вещи уже нет — так и сказано") {
      def done(d: ItemStore.Dropped): Option[(Long, Int)] = d match {
        case ItemStore.Dropped.Done(it, n) => Some(it.id -> n)
        case _                             => None
      }
      def kept(d: ItemStore.Dropped): Option[Long] = d match {
        case ItemStore.Dropped.Kept(it) => Some(it.id)
        case _                          => None
      }
      val townhouse = HomeData.empty.copy(tier = House.Townhouse.tier)
      for {
        c     <- ZIO.attempt(SceneContent.load())
        dao   <- TestHeroDao.withHero(userId, hero)
        _     <- dao.writeHomeData(userId, townhouse.copy(herbs = List(herb(1L), herb(2L), herb(3L))).asJson)
        shelf  = new ItemStore.HomeShelf(dao, ItemStore.HomeShelf.Herbs, c)
        one   <- ItemStore.drop(shelf, hero, 3L, all = false)
        left1 <- shelf.items(hero).map(_.map(_.id))
        all   <- ItemStore.drop(shelf, hero, 2L, all = true)
        left2 <- shelf.items(hero).map(_.map(_.id))
        bag    = TestInventoryRepository.withItems(List(sword(5L), sword(7L).asStoryGear, sword(8L).copy(itemType = ItemType.QuestItem)))
        store  = new ItemStore.Bag(bag, _ => "в сумке нет места")
        star  <- ItemStore.drop(store, hero, 7L, all = false)
        quest <- ItemStore.drop(store, hero, 8L, all = true)
        plain <- ItemStore.drop(store, hero, 5L, all = false)
        again <- ItemStore.drop(store, hero, 5L, all = false)
      } yield assertTrue(
        done(one).contains(3L -> 1) && left1 == List(1L, 2L),
        // вся стопка — обе ромашки, что остались
        done(all).contains(2L -> 2) && left2.isEmpty,
        kept(star).contains(7L) && kept(quest).contains(8L),
        done(plain).contains(5L -> 1) && again == ItemStore.Dropped.Gone,
        bag.snapshot.map(_.id) == List(7L, 8L)
      )
    }
  )
}
