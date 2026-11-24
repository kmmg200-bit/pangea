package pangea.service.home

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.generator.item.CubeCraft
import pangea.model.hero.{Hero, LoreData}
import pangea.model.home.{HomeData, HomeRates, House}
import pangea.model.item.{BrewKind, Item, MaterialKind}
import pangea.model.user.{User, UserId}
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.repository.item.ItemRepository
import pangea.service.state.{ItemMenu, QuestSupport, StackMove}
import pangea.service.storage.ItemStore.HomeShelf
import zio.{Random, Task, ZIO}

/** Котелок верстака травника без экрана: бросить в него с полки, вернуть на
  * полку, сварить, вынуть всё и сварить изученный отвар числом. Одно на бота
  * ([[pangea.service.state.states.home.HerbWorkbenchState]]) и на Mini App —
  * числа и правила у них одни. Всё меняет дом героя, поэтому зовётся под локом
  * игрока: в боте его держит ход, в приложении — сам запрос. Что сказать по
  * ходу (итог варки, достижение), идёт в `renderer`. */
final class Cauldron(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository,
                     content: SceneContent) {
  import Cauldron._

  /** Вещь с полки — в котелок: траву с полок трав, склянку-основу с полки
    * отваров. `None` — легла, иначе — почему нет. */
  def put(userId: UserId, shelf: HomeShelf.Kind, item: Item): Task[Option[String]] =
    loadHome(userId).flatMap { home =>
      val list = shelf.get(home)
      if (!list.exists(_.id == item.id)) ZIO.some(content.text("stack.gone"))
      else if (home.cauldron.size >= HomeRates.CauldronSlots) ZIO.some(potFull)
      else saveHome(userId, shelf.set(home, without(list, item)).copy(cauldron = home.cauldron :+ item)).as(None)
    }

  /** Вещь с полок по номеру — с той полки, где она лежит. */
  def putById(userId: UserId, itemId: Long): Task[Option[String]] =
    loadHome(userId).flatMap { home =>
      Shelves.flatMap(k => k.get(home).find(_.id == itemId).map(k -> _)).headOption match {
        case None             => ZIO.some(content.text("stack.gone"))
        case Some((shelf, it)) => put(userId, shelf, it)
      }
    }

  /** Одну вещь из котелка — обратно на её полку: траву — на полки трав,
    * склянку — на полку отваров, а что не с полок — в сумку. Места нет —
    * вещь остаётся в котелке. */
  def back(hero: Hero, itemId: Long): Task[Option[String]] =
    loadHome(hero.userId).flatMap { home =>
      home.cauldron.find(_.id == itemId) match {
        case None => ZIO.some(content.text("stack.gone"))
        case Some(it) =>
          val rest = home.copy(cauldron = without(home.cauldron, it))
          Shelves.find(_.accepts(it)) match {
            case Some(shelf) if shelf.get(home).size >= shelf.room(home) => ZIO.some(content.text(shelf.fullKey))
            case Some(shelf) => saveHome(hero.userId, shelf.set(rest, shelf.get(home) :+ it)).as(None)
            case None =>
              inventoryRepo.addItem(hero.id, it).either.flatMap {
                case Right(_) => saveHome(hero.userId, rest).as(None)
                case Left(InventoryRepoError.NoMorePlaceForItems | InventoryRepoError.DustLimitReached) =>
                  ZIO.some(content.text("home.workbench.bagFull"))
                case Left(e) => ZIO.fail(asThrowable(e))
              }
          }
      }
    }

  /** Сварить, что в котелке: варит, как магия Живой сумки
    * (`CubeCraft.brewHerbs`: отвары из трав и перегонка склянки с травами), и
    * тратит энергию дома — по одной на каждую варку. Сваренное встаёт на
    * полку отваров, что не поместилось — ждёт в котелке. Возвращает сваренное
    * — уже с номерами. */
  def cook(user: User, hero: Hero, renderer: Renderer): Task[List[Item]] =
    loadHome(user.userId).flatMap { home =>
      if (home.charges <= 0) say(user, content.text("home.workbench.noCharges"), renderer).as(Nil)
      else Random.nextLong.flatMap { seed =>
        val result = CubeCraft.brewHerbs(home.cauldron, home.charges, Rng(seed))
        if (!result.anyApplied) say(user, content.text("home.workbench.nothing"), renderer).as(Nil)
        else {
          val (fresh, kept) = result.items.partition(_.id <= 0L)
          for {
            made <- ZIO.foreach(fresh)(itemRepo.persist(hero.id, _))
            room  = (home.brewShelves - home.brews.size).max(0)
            (shelf, queued) = made.splitAt(room)
            _    <- saveHome(user.userId, home.copy(brews = home.brews ++ shelf, cauldron = kept ++ queued,
                      charges = home.charges - result.chargesUsed))
            _    <- say(user, content.format("home.workbench.cooked", "n" -> result.chargesUsed.toString,
                      "items" -> StackMove.names(made)) +
                      (if (queued.nonEmpty) "\n" + content.format("home.workbench.cookedWait", "n" -> queued.size.toString) else ""),
                      renderer)
            _    <- brewed(user, made, renderer)
          } yield made
        }
      }
    }

  /** «Вынуть всё»: травы — на полки, отвары — на полку отваров, а где места
    * нет — в сумку; что и туда не влезло, остаётся в котелке. Итог строкой;
    * котелок и так пуст — `None`. */
  def empty(hero: Hero): Task[Option[String]] =
    loadHome(hero.userId).flatMap { home =>
      if (home.cauldron.isEmpty) ZIO.none
      else for {
        inv <- inventoryRepo.get(hero.id).mapError(asThrowable)
        (hShelf, bShelf, bag, stay) = {
          val hRoom = home.herbShelves - home.herbs.size
          val bRoom = home.brewShelves - home.brews.size
          val (hs, rest1) = home.cauldron.partition(HomeShelf.Herbs.accepts)
          val (bs, other) = rest1.partition(_.brew.isDefined)
          val (hIn, hOver) = hs.splitAt(hRoom.max(0))
          val (bIn, bOver) = bs.splitAt(bRoom.max(0))
          val over = hOver ++ bOver ++ other
          val (toBag, stayPot) = over.splitAt(inv.freeSlots.toInt.max(0))
          (hIn, bIn, toBag, stayPot)
        }
        _ <- ZIO.foreachDiscard(bag)(it => inventoryRepo.addItem(hero.id, it).mapError(asThrowable))
        _ <- saveHome(hero.userId, home.copy(herbs = home.herbs ++ hShelf, brews = home.brews ++ bShelf, cauldron = stay))
      } yield Some(content.text("home.workbench.emptied") +
        (if (stay.nonEmpty) "\n" + content.format("home.workbench.emptiedStay", "n" -> stay.size.toString) else ""))
    }

  /** Сварить `asked` изученных отваров с полок: варок — с округлением вверх
    * (из простых трав выходит по два), каждая — одна энергия дома. Не хватает
    * трав, основы или энергии — говорим, чего и сколько, и ничего не трогаем.
    * Сваренное — на полку отваров, что не поместилось — ждёт в котелке, как у
    * котелка. Возвращает сваренное — уже с номерами. */
  def brew(user: User, hero: Hero, kind: BrewKind, asked: Int, renderer: Renderer): Task[List[Item]] =
    loadHome(user.userId).flatMap { home =>
      val per = perBatch(kind)
      // Так, а не (asked + per - 1) / per: у огромного числа сумма переполнится,
      // варок выйдет меньше нуля — и энергия дома не убудет, а прибудет.
      val batches = (asked.max(1) - 1) / per + 1
      val total   = batches.toLong * per
      lacks(home, kind, batches) match {
        case Nil =>
          val (herbsLeft, brewsLeft) = consume(home, kind, batches)
          for {
            made <- ZIO.foreach(List.fill(total.toInt)(BrewKind.item(kind)))(itemRepo.persist(hero.id, _))
            room  = (home.brewShelves - brewsLeft.size).max(0)
            (shelf, queued) = made.splitAt(room)
            _    <- saveHome(user.userId, home.copy(herbs = herbsLeft, brews = brewsLeft ++ shelf,
                      cauldron = home.cauldron ++ queued, charges = home.charges - batches))
            _    <- say(user, List(
                      Some(content.format("home.workbench.crafted", "name" -> kind.label,
                        "n" -> total.toString, "batches" -> batches.toString)),
                      Option.when(total > asked)(content.format("home.workbench.roundedUp",
                        "portions" -> per.toString, "n" -> total.toString, "asked" -> asked.toString)),
                      Option.when(queued.nonEmpty)(content.format("home.workbench.cookedWait", "n" -> queued.size.toString))
                    ).flatten.mkString("\n"), renderer)
            _    <- brewed(user, made, renderer)
          } yield made
        case missing =>
          val lines = missing.map { l =>
            val name = l match {
              case HerbNeed(h, _, _) => h.displayName
              case BaseNeed(b, _, _) => s"«${b.label}»"
              case EnergyNeed(_, _)  => content.text("home.workbench.lackEnergy")
            }
            content.format("home.workbench.lackLine", "name" -> name,
              "lack" -> (l.need - l.have).toString, "have" -> l.have.toString, "need" -> l.need.toString)
          }
          say(user, content.format("home.workbench.lack", "n" -> total.toString, "lines" -> lines.mkString("\n")), renderer)
            .as(Nil)
      }
    }

  private def potFull: String =
    content.format("home.workbench.potFull", "max" -> HomeRates.CauldronSlots.toString)

  /** Сваренное в котелке идёт в тот же счёт «Зельевара I» и «Промышленного
    * аптекаря», что и в кубе. */
  private def brewed(user: User, made: List[Item], renderer: Renderer): Task[Unit] =
    QuestSupport.brewed(heroDao, content, user, made.flatMap(_.brew).map(_.entryName), renderer)

  private def say(user: User, text: String, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(text, Nil))

  private def loadHome(userId: UserId): Task[HomeData] =
    heroDao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

  private def saveHome(userId: UserId, home: HomeData): Task[Unit] =
    heroDao.writeHomeData(userId, home.asJson)

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object Cauldron {

  /** Полки, с которых кладут в котелок, — по порядку, в каком ищут вещь. */
  val Shelves: List[HomeShelf.Kind] = List(HomeShelf.Herbs, HomeShelf.Brews)

  /** Верстак травника с котелком — в доме со второго. */
  def stands(home: HomeData): Boolean = home.house.exists(_.tier >= House.Townhouse.tier)

  /** Изученные отвары, которые варятся на верстаке: всё, что герой уже варил,
    * — кроме собранного не из трав (узелок гнилого зова котелку не по зубам).
    * По алфавиту. */
  def learned(lore: LoreData): List[BrewKind] =
    lore.brewed.flatMap(BrewKind.withNameOption).distinct
      .filter(k => k.recipe.nonEmpty && k.recipe.forall(_.isHerb))
      .sortBy(_.label)(ItemMenu.Alphabet)

  /** Сколько бутылок выходит с одной варки — как в котелке: из простых трав
    * две, с редкой травой одна, перегонка (основа + травы) — тоже одна. */
  def perBatch(kind: BrewKind): Int = if (kind.base.isDefined) 1 else kind.portions

  /** Сколько каких трав уйдёт на `batches` варок. */
  def herbsFor(kind: BrewKind, batches: Int): Map[MaterialKind, Int] =
    kind.recipe.groupMapReduce(identity)(_ => batches)(_ + _)

  /** Чего на `batches` варок нужно и сколько есть: трава, основа, энергия. */
  sealed trait Need { def need: Int; def have: Int }
  final case class HerbNeed(kind: MaterialKind, need: Int, have: Int) extends Need
  final case class BaseNeed(kind: BrewKind, need: Int, have: Int)     extends Need
  final case class EnergyNeed(need: Int, have: Int)                   extends Need

  /** Чего не хватает на `batches` варок; пусто — можно варить. */
  def lacks(home: HomeData, kind: BrewKind, batches: Int): List[Need] =
    (herbsFor(kind, batches).toList.sortBy(_._1.displayName).map { case (h, n) => HerbNeed(h, n, herbCount(home, h)) } ++
      kind.base.map(b => BaseNeed(b, batches, baseCount(home, b))).toList :+
      EnergyNeed(batches, home.charges)).filter(n => n.have < n.need)

  /** На сколько варок хватит трав на полках, основы и энергии дома. */
  def maxBatches(home: HomeData, kind: BrewKind): Int =
    (herbsFor(kind, 1).toList.map { case (h, n) => herbCount(home, h) / n } ++
      kind.base.map(baseCount(home, _)).toList :+ home.charges).min.max(0)

  /** Полки после варки: без трав на `batches` варок и без основ, если рецепт с ней. */
  def consume(home: HomeData, kind: BrewKind, batches: Int): (List[Item], List[Item]) = {
    val herbs = herbsFor(kind, batches).foldLeft(home.herbs) { case (left, (h, n)) => removeN(left, _.material.contains(h), n) }
    val brews = kind.base.fold(home.brews)(b => removeN(home.brews, _.brew.contains(b), batches))
    (herbs, brews)
  }

  private def herbCount(home: HomeData, h: MaterialKind): Int = home.herbs.count(_.material.contains(h))

  private def baseCount(home: HomeData, b: BrewKind): Int = home.brews.count(_.brew.contains(b))

  /** Убрать первые `n` подходящих, остальное — как лежало. */
  private def removeN(items: List[Item], fits: Item => Boolean, n: Int): List[Item] =
    items.foldLeft((List.empty[Item], n)) { case ((kept, left), it) =>
      if (left > 0 && fits(it)) (kept, left - 1) else (it :: kept, left)
    }._1.reverse

  /** Убрать ровно эту вещь. */
  private def without(items: List[Item], it: Item): List[Item] =
    items.indexWhere(_.id == it.id) match {
      case -1 => items
      case i  => items.patch(i, Nil, 1)
    }
}
