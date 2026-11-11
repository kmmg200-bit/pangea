package pangea.service.storage

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.Hero
import pangea.model.home.{HomeData, House}
import pangea.model.item.Item
import pangea.repository.artifact.{ArtifactRepoError, ArtifactRepository}
import pangea.repository.bank.{BankRepoError, BankRepository}
import pangea.repository.barrel.{BarrelRepoError, BarrelRepository}
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.service.state.{InventoryFeedback, StackMove}
import zio.{Task, ZIO}

/** Место, где у героя лежат вещи: сумка, артефакт, бочка, ячейка, полки дома.
  * Перенос между любыми двумя — [[ItemStore.move]], выбросить из любого —
  * [[ItemStore.drop]]; переносом пользуются и экраны
  * бота, и Mini App, поэтому места, пределы и слова отказа — здесь, по одному
  * на хранилище. Всё меняет героя и зовётся под локом игрока. */
trait ItemStore {

  /** Что лежит внутри сейчас. */
  def items(hero: Hero): Task[List[Item]]

  /** Берёт ли хранилище такую вещь вообще — по её виду, без мест и замков.
    * Сюжетное из сумки не уходит никуда. */
  def accepts(item: Item): Boolean

  /** Положить: `None` — легла, иначе — почему нет (текст для игрока). */
  def put(hero: Hero, item: Item): Task[Option[String]]

  /** Убрать вещь, которая уже легла в другое место; `false` — её здесь нет. */
  def remove(hero: Hero, item: Item): Task[Boolean]
}

object ItemStore {

  /** Вещь из `from` в `to`: сперва туда и только потом отсюда — откажет `to`,
    * вещь останется где была. Вещи в `from` уже нет — «её уже нет» (`gone`),
    * ничего не тронуто. `None` — перенесли. */
  def move(from: ItemStore, to: ItemStore, hero: Hero, item: Item, gone: String): Task[Option[String]] =
    from.items(hero).map(_.exists(_.id == item.id)).flatMap {
      case false => ZIO.some(gone)
      case true =>
        to.put(hero, item).flatMap {
          case refused @ Some(_) => ZIO.succeed(refused)
          // Убрать не вышло — вещь теперь в двух местах: лишняя лучше пропавшей.
          case None =>
            from.remove(hero, item).flatMap(removed =>
              ZIO.unless(removed)(ZIO.logError(s"Вещь ${item.id} героя ${hero.id.value} легла, но не ушла с прежнего места"))
            ).as(None)
        }
    }

  /** Чем кончилось «Выбросить» ([[drop]]). */
  sealed trait Dropped
  object Dropped {
    /** Вещи там уже нет — ничего не тронуто. */
    case object Gone extends Dropped
    /** Сюжетное — предмет задания или экипировка ★ — не выбрасывают. */
    final case class Kept(item: Item) extends Dropped
    /** Выброшено `count` вещей стопки `item` (одна, если стопку не просили). */
    final case class Done(item: Item, count: Int) extends Dropped
  }

  /** Выбросить из `store` вещь `itemId` — одну или (`all`) всю её стопку, с
    * конца, как и переносят ([[StackMove.stack]]). Сюжетное не выбрасывают: на
    * карточке бота у него такой кнопки нет. */
  def drop(store: ItemStore, hero: Hero, itemId: Long, all: Boolean): Task[Dropped] =
    store.items(hero).flatMap { items =>
      items.find(_.id == itemId) match {
        case None                                               => ZIO.succeed(Dropped.Gone)
        case Some(item) if item.isQuestItem || item.isStoryGear => ZIO.succeed(Dropped.Kept(item))
        case Some(item) =>
          val doomed = if (all) StackMove.stack(items, item) else List(item)
          ZIO.foldLeft(doomed)(0)((n, it) => store.remove(hero, it).map(gone => if (gone) n + 1 else n))
            .map(n => if (n == 0) Dropped.Gone else Dropped.Done(item, n))
      }
    }

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)

  /** Хранилище по имени — так его зовёт приложение (`StorageView.id`), — и что
    * оно берёт: вид вещи, без мест и замков. Сюжетное из сумки не уходит
    * никуда. `carried` — герой носит его с собой: до него можно дотянуться и
    * вне города. */
  sealed abstract class Place(val id: String, val carried: Boolean) {
    def accepts(item: Item): Boolean
  }

  object Place {
    case object Bag extends Place("bag", carried = true) {
      def accepts(item: Item): Boolean = true
    }

    /** Ларец, Живая сумка или шкаф: что берёт — решает сам артефакт. */
    final case class Art(kind: ArtifactKind) extends Place(kind match {
      case ArtifactKind.Casket    => "casket"
      case ArtifactKind.LivingBag => "livingBag"
      case ArtifactKind.Wardrobe  => "wardrobe"
    }, carried = true) {
      def accepts(item: Item): Boolean = kind.accepts(item) && !item.isQuestItem
    }

    /** Бочку вскрывают воры — сюжетную экипировку туда не кладут: её не
      * теряют (см. `Item.isStoryGear`). */
    case object Barrel extends Place("barrel", carried = false) {
      def accepts(item: Item): Boolean = !item.isQuestItem && !item.isStoryGear
    }

    case object Vault extends Place("vault", carried = false) {
      def accepts(item: Item): Boolean = !item.isQuestItem
    }

    final case class Shelf(kind: HomeShelf.Kind) extends Place(kind.id, carried = false) {
      def accepts(item: Item): Boolean = kind.accepts(item) && !item.isQuestItem
    }

    val all: List[Place] = List(Bag, Art(ArtifactKind.Casket), Art(ArtifactKind.LivingBag), Art(ArtifactKind.Wardrobe),
      Barrel, Vault, Shelf(HomeShelf.Herbs), Shelf(HomeShelf.Brews), Shelf(HomeShelf.Gems))

    def byId(id: String): Option[Place] = all.find(_.id == id)

    /** Откуда приложение берёт камень для гнезда: Ларец сам собирает камни с
      * добычи, и у его владельцев в сумке их почти нет. */
    val gemSources: List[Place] = List(Bag, Art(ArtifactKind.Casket))
  }

  /** Все хранилища героя по имени — для того, кто переносит между любыми
    * (Mini App). Сумка отказывает одними словами, откуда бы ни несли. */
  final class All(
    heroDao:       HeroDao,
    inventoryRepo: InventoryRepository,
    artifacts:     ArtifactRepository,
    barrelRepo:    BarrelRepository,
    bankRepo:      BankRepository,
    content:       SceneContent
  ) {
    private val bag = new Bag(inventoryRepo, InventoryFeedback.refusalLine(content, _, storage = true))

    def apply(place: Place): ItemStore = place match {
      case Place.Bag         => bag
      case Place.Art(kind)   => new Artifact(artifacts, kind, content)
      case Place.Barrel      => new Barrel(barrelRepo, content)
      case Place.Vault       => new Vault(bankRepo, content)
      case Place.Shelf(kind) => new HomeShelf(heroDao, kind, content)
    }
  }

  /** Сумка. Места не хватило — `refusal`: слова у каждого хранилища свои, как
    * в боте. Пыли и малым рунам мешает только свой предел на вид. */
  final class Bag(inventoryRepo: InventoryRepository, refusal: Item => String) extends ItemStore {
    def items(hero: Hero): Task[List[Item]] = inventoryRepo.get(hero.id).mapError(asThrowable).map(_.items.data)

    def accepts(item: Item): Boolean = Place.Bag.accepts(item)

    def put(hero: Hero, item: Item): Task[Option[String]] =
      inventoryRepo.get(hero.id).mapError(asThrowable).flatMap { inv =>
        val noRoom = if (item.weightless) !inv.hasRoomForHoard(item) else inv.freeSlots <= 0
        if (noRoom) ZIO.some(refusal(item))
        else inventoryRepo.addItem(hero.id, item).either.flatMap {
          case Right(_) => ZIO.none
          case Left(InventoryRepoError.NoMorePlaceForItems | InventoryRepoError.DustLimitReached) => ZIO.some(refusal(item))
          case Left(e) => ZIO.fail(asThrowable(e))
        }
      }

    def remove(hero: Hero, item: Item): Task[Boolean] =
      inventoryRepo.removeItem(item.id, hero.id).either.flatMap {
        case Right(_)                                      => ZIO.succeed(true)
        case Left(InventoryRepoError.CantFindItemToRemove) => ZIO.succeed(false)
        case Left(e)                                       => ZIO.fail(asThrowable(e))
      }
  }

  /** Ларец Азата, Живая сумка или Миниатюрный шкаф: что принимает — решает
    * сам артефакт ([[ArtifactKind.accepts]]). */
  final class Artifact(artifacts: ArtifactRepository, kind: ArtifactKind, content: SceneContent) extends ItemStore {
    def items(hero: Hero): Task[List[Item]] = artifacts.get(hero.id).mapError(asThrowable).map(_.of(kind).items.data)

    def accepts(item: Item): Boolean = Place.Art(kind).accepts(item)

    def put(hero: Hero, item: Item): Task[Option[String]] =
      artifacts.put(hero.id, kind, item).either.flatMap {
        case Right(_)                         => ZIO.none
        case Left(ArtifactRepoError.NoRoom)   => ZIO.some(content.text(s"artifact.${kind.key}.full"))
        case Left(ArtifactRepoError.NotOwned) => ZIO.some(content.text(s"artifact.${kind.key}.notOwned"))
        case Left(e)                          => ZIO.fail(asThrowable(e))
      }

    def remove(hero: Hero, item: Item): Task[Boolean] =
      artifacts.take(hero.id, kind, item.id).either.flatMap {
        case Right(_)                           => ZIO.succeed(true)
        case Left(ArtifactRepoError.NoSuchItem) => ZIO.succeed(false)
        case Left(e)                            => ZIO.fail(asThrowable(e))
      }
  }

  /** Неприметная бочка. Вскрыта ли она ворами, решает вызывающий: бочка здесь
    * — только место. */
  final class Barrel(barrelRepo: BarrelRepository, content: SceneContent) extends ItemStore {
    def items(hero: Hero): Task[List[Item]] = barrelRepo.get(hero.id).mapError(asThrowable).map(_.items.data)

    def accepts(item: Item): Boolean = Place.Barrel.accepts(item)

    def put(hero: Hero, item: Item): Task[Option[String]] =
      barrelRepo.deposit(hero.id, item).either.flatMap {
        case Right(_)                               => ZIO.none
        case Left(BarrelRepoError.BarrelFull)       => ZIO.some(content.text("barrel.barrelFull"))
        // Пыли этого вида в бочке уже сотня — горсть остаётся где была.
        case Left(BarrelRepoError.DustLimitReached) => ZIO.some(InventoryFeedback.refusalLine(content, item, storage = true))
        case Left(e)                                => ZIO.fail(asThrowable(e))
      }

    def remove(hero: Hero, item: Item): Task[Boolean] =
      barrelRepo.withdraw(hero.id, item.id).either.flatMap {
        case Right(_)                                   => ZIO.succeed(true)
        case Left(BarrelRepoError.CantFindItemToRemove) => ZIO.succeed(false)
        case Left(e)                                    => ZIO.fail(asThrowable(e))
      }
  }

  /** Ячейка в хранилище Торгового дома. */
  final class Vault(bankRepo: BankRepository, content: SceneContent) extends ItemStore {
    def items(hero: Hero): Task[List[Item]] = bankRepo.get(hero.id).mapError(asThrowable).map(_.items.data)

    def accepts(item: Item): Boolean = Place.Vault.accepts(item)

    def put(hero: Hero, item: Item): Task[Option[String]] =
      bankRepo.deposit(hero.id, item).either.flatMap {
        case Right(_)                             => ZIO.none
        case Left(BankRepoError.VaultFull)        => ZIO.some(content.text("bank.vault.full"))
        case Left(BankRepoError.DustLimitReached) => ZIO.some(InventoryFeedback.refusalLine(content, item, storage = true))
        case Left(BankRepoError.NoVault)          => ZIO.some(content.text("bank.vault.none"))
        case Left(e)                              => ZIO.fail(asThrowable(e))
      }

    def remove(hero: Hero, item: Item): Task[Boolean] =
      bankRepo.withdraw(hero.id, item.id).either.flatMap {
        case Right(_)                                 => ZIO.succeed(true)
        case Left(BankRepoError.CantFindItemToRemove) => ZIO.succeed(false)
        case Left(e)                                  => ZIO.fail(asThrowable(e))
      }
  }

  /** Полка дома: травы или отвары на верстаке травника, камни в хранилище
    * острова. Есть она не в каждом доме (`stands`), мест — сколько даёт дом
    * (`room`); нет полки — `missing`, нет места — `full`. */
  final class HomeShelf(
    heroDao: HeroDao,
    shelf:   HomeShelf.Kind,
    content: SceneContent
  ) extends ItemStore {
    def items(hero: Hero): Task[List[Item]] = load(hero).map(shelf.get)

    def accepts(item: Item): Boolean = Place.Shelf(shelf).accepts(item)

    def put(hero: Hero, item: Item): Task[Option[String]] =
      load(hero).flatMap { home =>
        if (!home.owns) ZIO.some(content.text("miniapp.noHouse"))
        else if (!shelf.stands(home)) ZIO.some(content.text(shelf.missingKey))
        else if (shelf.get(home).size >= shelf.room(home)) ZIO.some(content.text(shelf.fullKey))
        else save(hero, shelf.set(home, shelf.get(home) :+ item)).as(None)
      }

    def remove(hero: Hero, item: Item): Task[Boolean] =
      load(hero).flatMap { home =>
        val list = shelf.get(home)
        list.indexWhere(_.id == item.id) match {
          case -1 => ZIO.succeed(false)
          case i  => save(hero, shelf.set(home, list.patch(i, Nil, 1))).as(true)
        }
      }

    private def load(hero: Hero): Task[HomeData] =
      heroDao.readHomeData(hero.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

    private def save(hero: Hero, home: HomeData): Task[Unit] =
      heroDao.writeHomeData(hero.userId, home.asJson)
  }

  object HomeShelf {

    /** Какая полка: что на ней лежит, сколько мест и в каком доме она есть. */
    sealed abstract class Kind(val id: String) {
      def missingKey: String = s"miniapp.storages.$id.locked"
      def fullKey: String    = s"miniapp.storages.$id.full"

      def get(home: HomeData): List[Item]
      def set(home: HomeData, items: List[Item]): HomeData
      def room(home: HomeData): Int
      def stands(home: HomeData): Boolean
      def accepts(item: Item): Boolean
    }

    /** Полки трав на верстаке травника — в доме со второго. */
    case object Herbs extends Kind("herbs") {
      def get(home: HomeData): List[Item]                   = home.herbs
      def set(home: HomeData, items: List[Item]): HomeData = home.copy(herbs = items)
      def room(home: HomeData): Int                         = home.herbShelves
      def stands(home: HomeData): Boolean                   = home.house.exists(_.tier >= House.Townhouse.tier)
      def accepts(item: Item): Boolean                      = item.material.exists(_.isHerb)
    }

    /** Полка отваров на том же верстаке. */
    case object Brews extends Kind("brews") {
      def get(home: HomeData): List[Item]                   = home.brews
      def set(home: HomeData, items: List[Item]): HomeData = home.copy(brews = items)
      def room(home: HomeData): Int                         = home.brewShelves
      def stands(home: HomeData): Boolean                   = home.house.exists(_.tier >= House.Townhouse.tier)
      def accepts(item: Item): Boolean                      = item.brew.isDefined
    }

    /** Хранилище камней — только в доме на парящем острове. */
    case object Gems extends Kind("gems") {
      def get(home: HomeData): List[Item]                   = home.gems
      def set(home: HomeData, items: List[Item]): HomeData = home.copy(gems = items)
      def room(home: HomeData): Int                         = home.gemShelves
      def stands(home: HomeData): Boolean                   = home.house.contains(House.Island)
      def accepts(item: Item): Boolean                      = item.gem.isDefined
    }
  }
}
