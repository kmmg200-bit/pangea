package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.home.HomeData
import pangea.model.inventory.Inventory
import pangea.model.item.{Item, ItemType}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.QuestSupport
import pangea.service.state.states.InventoryState
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Магические стойки дома: обмен надетого на то, что висит на стойке. Зовут
  * сюда и кнопка стойки дома, и ключ из сумки (где угодно, кроме боя).
  *
  * Меняется весь комплект разом, место на место: пустое место на стойке —
  * пустое и на герое. Пояс едет вместе со своими склянками. Условия — как при
  * надевании: вещи со стойки по уровню, а сумка после смены вещей вмещает своё
  * (снятый «Тайник» её ужимает). */
final case class Racks(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent) {

  /** Обменяться со стойкой №`n`: `Left` — почему нельзя (текст для игрока). */
  def swap(user: User, n: Int, renderer: Renderer): Task[Either[String, Unit]] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- heroDao.getHeroByUserId(user.userId).someOrFail(new Throwable(s"No hero for user ${user.userId}"))
      home <- heroDao.readHomeData(user.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      // Стойки в кладовой — старшие по номеру: ключ от такой ждёт, пока её вернут.
      res  <- home.racks.lift(n - 1).filter(_ => n <= home.activeRacks) match {
        case None if n >= 1 && n <= home.racks.size =>
          ZIO.left(content.format("home.rack.stored", "n" -> n.toString))
        case None => ZIO.left(content.text("home.rack.gone"))
        case Some(rack) =>
          Racks.refusal(hero, rack, inv) match {
            case Some(Racks.Refusal.Empty)         => ZIO.left(content.text("home.rack.nothing"))
            case Some(Racks.Refusal.Level(items))  =>
              ZIO.left(content.format("home.rack.level", "items" -> items.map(i => s"${i.name} (${i.lvl})").mkString(", "),
                "lvl" -> hero.lvl.toString))
            case Some(Racks.Refusal.Bag)           => ZIO.left(content.text("home.rack.bag"))
            case None =>
              // Весь комплект одним шагом: запасы держат долю от нового
              // максимума (см. Hero.reequip), туда-обратно ничего не прибудет.
              val dressed = hero.reequip(rack, now)
              val delta   = Racks.bagDelta(hero, rack)
              for {
                _ <- heroDao.updateEquipmentAndFightStats(user.userId, rack, dressed.fightStats)
                _ <- heroDao.writeHomeData(user.userId, home.copy(racks = home.racks.updated(n - 1, hero.equipment)).asJson)
                _ <- ZIO.when(delta != 0L)(inventoryRepo.increaseCapacity(hero.id, delta).orElse(ZIO.unit))
                _ <- QuestSupport.grantFullSets(heroDao, content, user, dressed, renderer)
              } yield Right(())
          }
      }
    } yield res
}

object Racks {

  /** Почему обменяться нельзя. */
  sealed trait Refusal
  object Refusal {
    /** И на герое, и на стойке пусто — меняться нечем. */
    case object Empty extends Refusal
    /** Эти вещи со стойки герою пока не по уровню. */
    final case class Level(items: List[Item]) extends Refusal
    /** Сумка после смены вещей не вместит своё: уходит «Тайник». */
    case object Bag extends Refusal
  }

  /** Почему со стойкой `rack` не обменяться; `None` — можно. */
  def refusal(hero: Hero, rack: Equipment, inv: Inventory): Option[Refusal] = {
    val worn   = real(hero.equipment.allItems)
    val onRack = real(rack.allItems)
    val high   = onRack.filter(_.lvl > hero.lvl)
    if (worn.isEmpty && onRack.isEmpty) Some(Refusal.Empty)
    else if (high.nonEmpty) Some(Refusal.Level(high))
    else if (!InventoryState.fitsAfterCapacityChange(inv, bagDelta(hero, rack), 0)) Some(Refusal.Bag)
    else None
  }

  /** Как изменится сумка, если надеть `rack` вместо надетого: «Тайник» — по
    * всему комплекту, как при обычном надевании. */
  def bagDelta(hero: Hero, rack: Equipment): Long =
    InventoryState.equipmentStashDelta(hero.equipment, rack, hero.runes.brandedPassives.toSet)

  /** Вещи без пустых мест. */
  def real(items: List[Item]): List[Item] = items.filter(_.itemType != ItemType.NoItem)
}
