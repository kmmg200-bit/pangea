package pangea.service.equipment

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.item.{BeltSlot, Item, ItemDetails, ItemType, Rarity}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{BeltContents, InventoryFeedback}
import zio.{Task, ZIO}

/** Что лежит в ячейках пояса, без экрана. Одно на бота — карточки вещи и слота
  * — и на Mini App: правила у них одни. Всё меняет героя, поэтому зовётся под
  * локом игрока: в боте его держит ход, в приложении — сам запрос. Что сказать
  * игроку, идёт в `renderer`. */
final class BeltWork(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository,
                     content: SceneContent) {
  import BeltWork.{Put, sameInBag}

  /** Положить расходник на пояс: фляга — со своими зарядами, роза — со своими
    * раскрытиями, пыль, отвар и святая вода — стопкой: из сумки уходит сразу
    * столько таких же, сколько влезает в ячейку (см.
    * [[ItemDetails.Belt.stackFor]]), а неполная ячейка того же вида сперва
    * доливается. Ячеек у пояса столько, сколько раньше вмещалось склянок (см.
    * [[ItemDetails.Belt.capacityFor]]); нет пояса — вещь остаётся в сумке, нет
    * места — [[Put.Full]]: что снять, решает тот, кто спросил. */
  def put(user: User, hero: Hero, item: Item, renderer: Renderer): Task[Put] = {
    val belt  = hero.equipment.belt
    val cap   = ItemDetails.Belt.capacityFor(belt.rarity)
    val stack = ItemDetails.Belt.stackFor(belt.rarity)
    (BeltSlot.of(item), belt.details) match {
      case (None, _) => ZIO.succeed(Put.NotForBelt)
      case (Some(_), _) if belt.itemType != ItemType.Belt =>
        renderer.show(user, Screen(content.text("inventory.beltNone"), Nil)).as(Put.Done)
      case (Some(unit: BeltSlot.Stack), b: ItemDetails.Belt) =>
        inventoryRepo.get(hero.id).mapError(asThrowable).flatMap { inv =>
          val same = sameInBag(inv.items.data, unit, item)
          b.putStack(unit, same.size, cap, stack) match {
            case None => ZIO.succeed(Put.Full(unit, b))
            case Some((filled, n)) =>
              // В той ячейке, куда легло: она одна изменилась (или встала новой).
              val in = filled.slots.zipWithIndex.collectFirst {
                case (s: BeltSlot.Stack, i) if s.sameKind(unit) && !b.slots.lift(i).contains(s) => s.count
              }
              inventoryRepo.removeItems(same.take(n).map(_.id).toSet, hero.id).mapError(asThrowable) *>
                heroDao.updateEquipment(user.userId, hero.equipment.copy(belt = belt.copy(details = filled))) *>
                renderer.show(user, Screen(content.format("inventory.beltPutStack",
                  "name" -> unit.name, "count" -> n.toString,
                  "have" -> in.getOrElse(n).toString, "stack" -> stack.toString), Nil)).as(Put.Done)
          }
        }
      case (Some(slot), b: ItemDetails.Belt) =>
        b.put(slot, cap) match {
          case None => ZIO.succeed(Put.Full(slot, b))
          case Some(filled) =>
            inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable) *>
              heroDao.updateEquipment(user.userId, hero.equipment.copy(belt = belt.copy(details = filled))) *>
              renderer.show(user, Screen(content.format("inventory.beltPutDone",
                "name" -> slot.name, "used" -> filled.slots.size.toString,
                "cap" -> cap.toString), Nil)).as(Put.Done)
        }
      case _ => ZIO.succeed(Put.NotForBelt)
    }
  }

  /** Положить вещь на пояс вместо ячейки `idx`: снятое уходит в сумку вещами —
    * стопка вся, сколько в ней осталось (пустой ячейке возвращать нечего);
    * новая стопка набирается из сумки до полной. Сумке должно хватить места:
    * взятое освобождает своё, снятое занимает; пыли — не больше сотни горстей.
    * Место на поясе успело найтись — вещь просто ложится на пояс. */
  def swap(user: User, hero: Hero, item: Item, idx: Option[Int], renderer: Renderer): Task[Put] = {
    val belt  = hero.equipment.belt
    val stack = ItemDetails.Belt.stackFor(belt.rarity)
    (BeltSlot.of(item), belt.details, idx) match {
      case (Some(slot), b: ItemDetails.Belt, _) if belt.itemType == ItemType.Belt && BeltWork.fits(slot, b, belt.rarity) =>
        put(user, hero, item, renderer)
      case (Some(slot), b: ItemDetails.Belt, Some(i)) if belt.itemType == ItemType.Belt =>
        b.at(i) match {
          case None => ZIO.succeed(Put.NotForBelt)
          case Some(old) =>
            val back = BeltContents.items(old)
            inventoryRepo.get(hero.id).mapError(asThrowable).flatMap { inv =>
              // Что уходит из сумки на пояс: стопкой — сколько влезет, иначе — сама вещь.
              val (put, taken) = slot match {
                case unit: BeltSlot.Stack =>
                  val same = sameInBag(inv.items.data, unit, item).take(stack)
                  (unit.withCount(same.size), same)
                case other => (other, List(item))
              }
              val after = inv.withItems(inv.items.data.filterNot(i => taken.exists(_.id == i.id)))
              if (!BeltContents.fits(after, back))
                renderer.show(user, Screen(back.find(r => r.weightless && !BeltContents.fits(after, List(r)))
                  .map(InventoryFeedback.refusalLine(content, _)).getOrElse(content.text("common.swapNoRoom")), Nil))
                  .as(Put.Done)
              else
                for {
                  // Место есть — теперь и сохраняем снятое (см. BeltContents.persist).
                  saved <- BeltContents.persist(Some(itemRepo), hero.id, back)
                  _     <- inventoryRepo.removeItems(taken.map(_.id).toSet, hero.id).mapError(asThrowable)
                  _     <- heroDao.updateEquipment(user.userId,
                             hero.equipment.copy(belt = belt.copy(details = b.updated(i, put))))
                  _     <- ZIO.foreachDiscard(saved)(r => inventoryRepo.addItem(hero.id, r).mapError(asThrowable))
                  name   = BeltContents.names(taken)
                  _     <- renderer.show(user, Screen(
                             if (back.isEmpty) content.format("inventory.beltSwappedEmpty", "name" -> name, "old" -> old.name)
                             else content.format("inventory.beltSwapped", "name" -> name, "old" -> BeltContents.names(back)), Nil))
                } yield Put.Done
            }
        }
      case _ => ZIO.succeed(Put.NotForBelt)
    }
  }

  /** Вынуть расходник с пояса: фляга собирается обратно со своими зарядами,
    * пыль — горстью, отвар — склянкой, цветок — со своими раскрытиями. Ячейку
    * снимают целиком: стопка уходит в сумку вся, сколько в ней осталось, — и
    * только если места хватит на всё разом. В сумке нет места — ячейка остаётся
    * как была.
    *
    * Пустая ячейка помнит, чем была полна, но вернуть из неё нечего: горсть
    * высыпана, склянка выпита, цветок осыпался — такая ячейка просто
    * освобождается. `false` — такой ячейки нет, сказать нечего. */
  def take(user: User, hero: Hero, idx: Int, renderer: Renderer): Task[Boolean] = {
    val belt = hero.equipment.belt
    belt.details match {
      case b: ItemDetails.Belt =>
        b.at(idx) match {
          case None => ZIO.succeed(false)
          case Some(slot) =>
            val back  = BeltContents.items(slot)
            val freed = heroDao.updateEquipment(user.userId,
              hero.equipment.copy(belt = belt.copy(details = b.without(idx))))
            inventoryRepo.get(hero.id).mapError(asThrowable).flatMap { inv =>
              if (back.isEmpty)
                freed *> renderer.show(user, Screen(content.format("equipment.beltCleared", "name" -> slot.name), Nil))
              else if (!BeltContents.fits(inv, back))
                renderer.show(user, Screen(content.text("equipment.beltTakeFull"), Nil))
              else
                for {
                  saved <- BeltContents.persist(Some(itemRepo), hero.id, back)
                  _     <- freed
                  _     <- ZIO.foreachDiscard(saved)(it => inventoryRepo.addItem(hero.id, it).ignore)
                  _     <- renderer.show(user, Screen(content.format("equipment.beltTaken",
                             "name" -> BeltContents.names(back)), Nil))
                } yield ()
            }.as(true)
        }
      case _ => ZIO.succeed(false)
    }
  }

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object BeltWork {

  /** Чем кончилось «на пояс»: сказано всё, что надо; вещь не для пояса; или
    * ячейки заняты и надо решить, что снять. */
  sealed trait Put
  object Put {
    case object Done       extends Put
    case object NotForBelt extends Put
    final case class Full(slot: BeltSlot, belt: ItemDetails.Belt) extends Put
  }

  /** Такие же вещи в сумке, что и стопка `unit`, — выбранная первой: её-то
    * игрок и кладёт, остальные идут следом. */
  private def sameInBag(bag: List[Item], unit: BeltSlot.Stack, picked: Item): List[Item] = {
    val same = bag.filter(i => BeltSlot.of(i).exists(unit.sameKind))
    same.filter(_.id == picked.id) ++ same.filterNot(_.id == picked.id)
  }

  /** Ляжет ли вещь на пояс, ничего не снимая: есть свободная ячейка или — для
    * стопки — неполная ячейка того же вида. */
  def fits(slot: BeltSlot, b: ItemDetails.Belt, rarity: Rarity): Boolean =
    slot match {
      case unit: BeltSlot.Stack =>
        b.putStack(unit, 1, ItemDetails.Belt.capacityFor(rarity), ItemDetails.Belt.stackFor(rarity)).isDefined
      case _ => b.slots.sizeIs < ItemDetails.Belt.capacityFor(rarity)
    }
}
