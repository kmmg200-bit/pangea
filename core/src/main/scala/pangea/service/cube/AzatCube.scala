package pangea.service.cube

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.generator.item.CubeCraft
import pangea.model.hero.{Achievement, AzatState, Hero}
import pangea.model.home.{HomeData, RecipeNote}
import pangea.model.item.{BrewKind, Item}
import pangea.model.user.{User, UserId}
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.repository.item.ItemRepository
import pangea.service.state.{AzatData, HerbLore, InventoryFeedback, MarisaQuest, StackMove}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Куб Азата без экрана: положить, забрать, активировать. Одно на бота
  * ([[pangea.service.state.states.temple.CubeState]]) и на Mini App — числа и
  * правила у них одни. Всё меняет героя, поэтому зовётся под локом игрока:
  * в боте его держит ход, в приложении — сам запрос. */
final class AzatCube(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepository: ItemRepository,
                     content: SceneContent) {
  import AzatCube._

  /** Вещь из сумки в куб. `None` — легла, иначе — почему нет. */
  def deposit(hero: Hero, item: Item): Task[Option[String]] =
    loadAzat(hero.userId).flatMap { azat =>
      if (item.isBundle) ZIO.some(content.text("cube.bundle"))
      else if (azat.cubeItems.length >= AzatState.CubeCapacity) ZIO.some(content.text("cube.full"))
      else
        inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable) *>
          saveAzat(hero.userId, azat.copy(cubeItems = azat.cubeItems :+ item)).as(None)
    }

  /** Вещь из куба в сумку. Пыль и малые руны места не занимают — им мешает
    * только свой предел. */
  def withdraw(hero: Hero, item: Item): Task[Option[String]] =
    for {
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat <- loadAzat(hero.userId)
      res  <- if (!azat.cubeItems.exists(_.id == item.id)) ZIO.some(content.text("stack.gone"))
              else if (if (item.weightless) !inv.hasRoomForHoard(item) else inv.freeSlots <= 0)
                ZIO.some(if (item.weightless) InventoryFeedback.refusalLine(content, item, storage = true)
                         else content.text("cube.inventoryFull"))
              else
                inventoryRepo.addItem(hero.id, item).either.flatMap {
                  case Right(_) => saveAzat(hero.userId, azat.copy(cubeItems = removeFirst(azat.cubeItems, item.id))).as(None)
                  case Left(InventoryRepoError.NoMorePlaceForItems | InventoryRepoError.DustLimitReached) =>
                    ZIO.some(InventoryFeedback.refusalLine(content, item, storage = true))
                  case Left(e) => ZIO.fail(asThrowable(e))
                }
    } yield res

  /** «Забрать всё»: куб целиком — в сумку, сколько влезет. Не влезла одна
    * вещь — пробуем следующую: пыли и рунам сумкино место не нужно. Итог —
    * строкой. */
  def withdrawEverything(hero: Hero): Task[String] =
    loadAzat(hero.userId).flatMap { azat =>
      if (azat.cubeItems.isEmpty) ZIO.succeed(content.text("cube.emptyCube"))
      else StackMove.run(azat.cubeItems, stopAtFirst = false)(withdraw(hero, _))
             .map(StackMove.summary(content, _, "cube.withdrawnAll"))
    }

  /** «Активировать»: рецепты от длинного к короткому ([[CubeCraft]]), сила —
    * с зарядов куба или энергии дома. Строка итога и достижение «Зельевар I»
    * идут в `renderer` по порядку. Возвращает то, что получилось, — уже с
    * номерами вещей. */
  def activate(user: User, hero: Hero, renderer: Renderer): Task[List[Item]] =
    for {
      azat <- loadAzat(user.userId)
      home <- loadHome(user.userId)
      power = AzatCube.power(azat, home)
      seed <- Random.nextLong
      result = CubeCraft.craft(azat.cubeItems, power.charges, Rng(seed))
      // Результаты рецептов (id <= 0) персистим — им нужен id для инвентаря.
      persisted <- ZIO.foreach(result.items) { it =>
        if (it.id > 0L) ZIO.succeed(it -> false)
        else itemRepository.persist(hero.id, it).map(_ -> true)
      }
      kept = persisted.map(_._1)
      // Сила уходит оттуда же, откуда взята: энергия дома или заряды куба.
      _ <- power match {
             case Power.Home(n, _) =>
               saveAzat(user.userId, azat.copy(cubeItems = kept)) *>
                 saveHome(user.userId, home.copy(charges = n - result.chargesUsed))
             case Power.Own(n) =>
               saveAzat(user.userId, azat.copy(cubeItems = kept, cubeCharges = n - result.chargesUsed))
           }
      // Дома всё, что получилось, — в книгу рецептов (пишется и без библиотеки).
      _ <- ZIO.when(azat.cubeHome && result.applied.nonEmpty)(
             loadHome(user.userId).flatMap(h => saveHome(user.userId, h.noted(result.applied.map(AzatCube.note)))))
      msgKey = if (result.anyApplied) "cube.activated" else "cube.nothing"
      _ <- renderer.show(user, Screen(content.text(msgKey), Nil))
      // Сваренные отвары идут в счёт «Зельевара I»: по одному каждого — достижение.
      brewedNow = result.items.filter(_.id <= 0L).flatMap(_.brew).map(_.entryName)
      _ <- ZIO.when(brewedNow.nonEmpty) {
             for {
               lore   <- HerbLore.readLore(heroDao, user.userId)
               updated = lore.brewedAlso(brewedNow)
               _      <- HerbLore.writeLore(heroDao, user.userId, updated)
               _      <- ZIO.when(BrewKind.rank1.forall(k => updated.brewed.contains(k.entryName)))(
                           MarisaQuest.grant(heroDao, content, user, hero, Achievement.Brewer1, renderer))
             } yield ()
           }
    } yield persisted.collect { case (item, true) => item }

  def loadAzat(userId: UserId): Task[AzatState] =
    ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS)).flatMap(now => AzatData.load(heroDao, userId, now))

  def saveAzat(userId: UserId, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(userId, azat.asJson)

  def loadHome(userId: UserId): Task[HomeData] =
    heroDao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

  def saveHome(userId: UserId, home: HomeData): Task[Unit] =
    heroDao.writeHomeData(userId, home.asJson)
}

object AzatCube {

  /** Что из сумки предлагают положить в куб: всё, кроме сюжетного и стопок —
    * рецептов со стопками нет, её сперва распаковывают (см. `Bundling`). */
  def accepts(item: Item): Boolean = !item.isQuestItem && !item.isBundle

  /** Откуда куб берёт силу. */
  sealed trait Power { def charges: Int }
  object Power {
    /** Свои заряды куба — в Храме и в доме без святилища. */
    final case class Own(charges: Int) extends Power
    /** Энергия дома — куб дома, и в доме есть алтарь Азата. */
    final case class Home(charges: Int, max: Int) extends Power
  }

  def power(azat: AzatState, home: HomeData): Power =
    if (azat.cubeHome && home.altarMax > 0) Power.Home(home.charges, home.altarMax) else Power.Own(azat.cubeCharges)

  /** Запись в книгу рецептов: названия того, что ушло, и что вышло. */
  def note(a: CubeCraft.Applied): RecipeNote = RecipeNote(a.ingredients, a.made, a.count)

  private def removeFirst(items: List[Item], itemId: Long): List[Item] =
    items.indexWhere(_.id == itemId) match {
      case -1 => items
      case i  => items.patch(i, Nil, 1)
    }

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}
