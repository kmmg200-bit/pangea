package pangea.service.equipment

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.{EquipmentState, InventoryState}
import pangea.service.state.{BeltContents, InventoryFeedback, QuestSupport}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Надеть и снять без экрана. Одно на бота — карточку вещи в сумке
  * ([[InventoryState]]) и карточку слота ([[EquipmentState]]) — и на Mini App:
  * правила у них одни. Всё меняет героя, поэтому зовётся под локом игрока: в
  * боте его держит ход, в приложении — сам запрос. Что сказать игроку, идёт в
  * `renderer`: бот показывает в чате, приложение собирает в ответ. */
final class Outfit(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository,
                   content: SceneContent) {

  /** Надеть вещь из сумки. `pairSlot` — какое из колец или какую руку занять
    * (см. [[InventoryState.equipWith]]). Можно ли надеть её вообще — тип,
    * уровень — проверяет вызывающий: у экранов это проверяется по-своему.
    * Здесь — поместится ли в сумку снятое: нет — строка отказа, иначе
    * `say(итог)` и достижения за собранные наборы. `true` — надето. */
  def wear(user: User, hero: Hero, item: Item, pairSlot: Option[Int], renderer: Renderer)(
      say: InventoryState.Equipped => String): Task[Boolean] =
    for {
      now <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      inv <- inventoryRepo.get(hero.id).mapError(asThrowable)
      done = InventoryState.equipWith(hero, item, pairSlot, now)
      // Замена, снимающая последний «Тайник», ужмёт сумку — как и при снятии;
      // двуручное поверх двух одноручных вернёт в сумку на вещь больше, пояс
      // меньше прежнего — то, что в него не поместилось.
      capDelta = InventoryState.equipmentStashDelta(hero.equipment, done.equipment, hero.runes.brandedPassives.toSet)
      worn <- InventoryState.equipBlock(inv, capDelta, done.toBag, belt = item.itemType == ItemType.Belt) match {
        case Some(key) => renderer.show(user, Screen(content.format(key, "name" -> item.name), Nil)).as(false)
        case None =>
          heroDao.updateEquipmentAndFightStats(user.userId, done.equipment, done.fightStats) *>
            inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable) *>
            BeltContents.persist(Some(itemRepo), hero.id, done.spilled).flatMap(spilled =>
              ZIO.foreachDiscard(done.removed ++ spilled)(old => inventoryRepo.addItem(hero.id, old).ignore)) *>
            ZIO.when(capDelta != 0)(inventoryRepo.increaseCapacity(hero.id, capDelta).orElse(ZIO.unit)) *>
            renderer.show(user, Screen(say(done), Nil)) *>
            // Двенадцатая вещь набора — за собранный набор своё достижение.
            QuestSupport.grantFullSets(heroDao, content, user,
              hero.copy(equipment = done.equipment, fightStats = done.fightStats), renderer).as(true)
      }
    } yield worn

  /** Снять надетое в слоте — в сумку. Что вышло, строкой в `renderer`; `true` —
    * снято. */
  def takeOff(user: User, hero: Hero, slot: EquipmentState.Slot, renderer: Renderer): Task[Boolean] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      item  = slot.get(hero.equipment)
      newEq = slot.clear(hero.equipment)
      // Снятие последнего «Тайника» в экипировке (дубли не стакаются — см.
      // InventoryState.equipmentStashDelta) убирает +10 слотов: если сумка (с
      // возвращаемым предметом) не влезет в уменьшенную вместимость — блокируем.
      capDelta = InventoryState.equipmentStashDelta(hero.equipment, newEq, hero.runes.brandedPassives.toSet)
      blockedByStash = capDelta < 0 && !InventoryState.fitsAfterCapacityChange(inv, capDelta, returningItems = 1)
      done <- if (item.itemType == ItemType.NoItem)
                renderer.show(user, Screen(content.text("equipment.slotEmpty"), Nil)).as(false)
              else if (blockedByStash)
                renderer.show(user, Screen(content.text("equipment.stashBlocked"), Nil)).as(false)
              else {
                // Пояс уходит пустым: содержимое — герою, в сумку отдельными вещами.
                // Места нужно на сам пояс и на всё, что место занимает (пыль невесома).
                val (bare, contents) =
                  if (item.itemType == ItemType.Belt) BeltContents.strip(item) else (item, Nil)
                if (!inv.hasRoomFor(1L + contents.count(!_.weightless).toLong))
                  renderer.show(user, Screen(content.text("equipment.unequipInventoryFull"), Nil)).as(false)
                else
                  inventoryRepo.addItem(hero.id, bare).foldZIO(
                    _ => renderer.show(user, Screen(content.text("equipment.unequipInventoryFull"), Nil)).as(false),
                    _ => {
                      val newFight = hero.reequip(newEq, now).fightStats
                      for {
                        _      <- heroDao.updateEquipmentAndFightStats(user.userId, newEq, newFight)
                        _      <- ZIO.when(capDelta != 0)(
                                    inventoryRepo.increaseCapacity(hero.id, capDelta).orElse(ZIO.unit))
                        // Пыли сверх сотни горстей не унести — у неё своя строка отказа.
                        saved  <- BeltContents.persist(Some(itemRepo), hero.id, contents)
                        lost   <- ZIO.foreach(saved)(it => inventoryRepo.addItem(hero.id, it).either.map(it -> _))
                                    .map(_.collect { case (it, Left(_)) => InventoryFeedback.refusalLine(content, it) }.distinct)
                        stripped = if (contents.isEmpty) ""
                                   else "\n" + content.format("equipment.beltStripped",
                                     "names" -> BeltContents.names(contents))
                        refused  = if (lost.isEmpty) "" else "\n" + lost.mkString("\n")
                        slotsLine <- InventoryFeedback.freeSlotsLine(inventoryRepo, content, hero.id)
                        _      <- renderer.show(user, Screen(
                                    content.format("equipment.unequipped", "name" -> item.name) + stripped + refused +
                                      "\n" + slotsLine, Nil))
                      } yield true
                    }
                  )
              }
    } yield done

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object Outfit {
  /** «Надето» — как в карточке слота: надели в пустой слот или вместо чего, и
    * что переложили с прежнего пояса. */
  def wornLine(content: SceneContent, item: Item, done: InventoryState.Equipped): String =
    (if (done.removed.isEmpty) content.format("equipment.wornNow", "name" -> item.name)
     else content.format("equipment.wornSwapped", "name" -> item.name,
       "old" -> InventoryState.removedNames(done.removed))) +
      BeltContents.movedLine(content, done.beltMoved, done.spilled)
}
