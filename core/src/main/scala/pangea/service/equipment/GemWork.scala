package pangea.service.equipment

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.generator.item.MaterialGenerator
import pangea.model.battle.Element
import pangea.model.hero.{Equipment, Hero}
import pangea.model.inventory.Inventory
import pangea.model.item.{Chisel, Gem, GemBreaking, Item, ItemType, PassiveKind}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.BeltContents
import pangea.service.state.states.InventoryState
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Камни в гнёздах и резец без экрана. Одно на бота — карточки слота и сумки,
  * экран вставки — и на Mini App: правила у них одни. Всё меняет героя, поэтому
  * зовётся под локом игрока: в боте его держит ход, в приложении — сам запрос.
  * Что сказать игроку, идёт в `renderer`: бот показывает в чате, приложение
  * собирает в ответ. */
final class GemWork(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository,
                    content: SceneContent) {

  /** Сломать камень из гнезда `socket` надетой вещи: камень крошится в пыль
    * своего вида, пыль — в сумку. Места под неё нет — сказать и ничего не
    * трогать. `false` — такого камня в гнезде нет, сказать нечего. */
  def breakWorn(user: User, hero: Hero, item: Item, socket: Int, renderer: Renderer): Task[Boolean] =
    GemBreaking.breakSocket(item, socket) match {
      case None => ZIO.succeed(false)
      case Some((emptied, gem)) =>
        for {
          now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
          inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
          count  = gem.dustYield
          // Рубин уходит из гнезда — максимум здоровья падает, и запасы с ним.
          broken = hero.reequip(hero.equipment.replacing(emptied), now)
          _ <-
            if (inv.freeSlots < count)
              renderer.show(user, Screen(content.format("equipment.breakGemNoRoom", "count" -> count.toString), Nil))
            else
              heroDao.updateEquipmentAndFightStats(user.userId, broken.equipment, broken.fightStats) *>
                // Каждая горсть — со своим номером из базы: без него горсти
                // разных видов делили −1, и снятие одной убирало из сумки все.
                BeltContents.persist(Some(itemRepo), hero.id, List.fill(count)(MaterialGenerator.item(gem.dust)))
                  .flatMap(ZIO.foreachDiscard(_)(d => inventoryRepo.addItem(hero.id, d).mapError(asThrowable))) *>
                renderer.show(user, Screen(content.format("equipment.breakGemDone",
                  "gem" -> gem.displayName, "count" -> count.toString, "dust" -> gem.dust.displayName), Nil))
        } yield true
    }

  /** Вставить камень `gem` в надетую вещь `targetId`, в первое свободное гнездо.
    * `spend` убирает камень оттуда, где он лежал. `false` — вещи нет или
    * свободного гнезда в ней нет; камень цел. */
  def insert(user: User, hero: Hero, targetId: Long, gem: Gem, spend: => Task[Unit], renderer: Renderer): Task[Boolean] =
    ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS)).flatMap { now =>
      hero.equipment.allItems.find(i => i.id == targetId && i.itemType != ItemType.NoItem) match {
        // Огонь и Холод в одном оружии не уживаются: вставляемый камень и
        // ПРОТИВОПОЛОЖНЫЙ ему обращаются в пыль. Одинаковые стихии при этом
        // спокойно стакаются — два рубина усиливают друг друга по грейдам.
        case Some(item) if isWeapon(item) && hasOpposite(item, gem) =>
          annihilate(user, hero, item, gem, spend, now, renderer).as(true)
        case _ =>
          socketInto(hero.equipment, targetId, gem) match {
            case None => ZIO.succeed(false)
            // Рубин в надетой вещи поднимает максимум здоровья — запасы держат долю.
            case Some((newEq, item)) =>
              heroDao.updateEquipmentAndFightStats(user.userId, newEq, hero.reequip(newEq, now).fightStats) *>
                spend *>
                renderer.show(user, Screen(
                  content.format("socketing.done", "gem" -> gem.displayName, "item" -> item.name), Nil)).as(true)
          }
      }
    }

  /** Огонь и Холод в одном оружии не уживаются: оба камня рассыпаются. От каждого
    * остаётся РОВНО одна горсть своей пыли — вспышка съедает камни целиком, грейд
    * тут ничего не добавляет (в отличие от спокойной ломки, см. `GemBreaking`).
    *
    * Пыли нужно две ячейки. Считаем их уже после того, как вставляемый камень ушёл
    * оттуда, где лежал: из сумки он сам освобождает одну. Не хватило — камни всё
    * равно потеряны, но пыль высыпается мимо сумки, и об этом честно сообщаем. */
  private def annihilate(
      user:     User,
      hero:     Hero,
      item:     Item,
      inserted: Gem,
      spend:    => Task[Unit],
      nowMs:    Long,
      renderer: Renderer
  ): Task[Unit] = {
    val cleaned = removeOpposite(item, inserted)
    val after   = hero.reequip(hero.equipment.replacing(cleaned), nowMs)
    val lost    = item.socketedGems.find(isOpposite(_, inserted))
    val dusts   = (inserted :: lost.toList).map(_.dust)
    for {
      _   <- heroDao.updateEquipmentAndFightStats(user.userId, after.equipment, after.fightStats)
      _   <- spend
      inv <- inventoryRepo.get(hero.id).mapError(asThrowable)
      room = inv.freeSlots >= dusts.size
      // Каждая горсть — со своим номером из базы: без него горсти делили −1, и
      // снятие одной (в куб, в ячейку) убирало из сумки все.
      _   <- ZIO.when(room)(ZIO.foreachDiscard(dusts)(kind =>
               itemRepo.persist(hero.id, MaterialGenerator.item(kind))
                 .flatMap(d => inventoryRepo.addItem(hero.id, d).mapError(asThrowable))))
      line = if (room)
               content.format("socketing.annihilate", "dust" -> dusts.map(_.displayName).mkString(", "))
             else content.text("socketing.annihilateNoRoom")
      _   <- renderer.show(user, Screen(line, Nil))
    } yield ()
  }

  private def isWeapon(item: Item): Boolean =
    item.itemType == ItemType.Weapon || item.itemType == ItemType.AdditionalWeapon

  /** Противоположная стихия: огонь ↔ холод. У остальных стихий пары нет. */
  private def opposite(e: Element): Option[Element] = e match {
    case Element.Fire => Some(Element.Cold)
    case Element.Cold => Some(Element.Fire)
    case _            => None
  }

  private def isOpposite(gem: Gem, to: Gem): Boolean =
    (Element.of(gem.kind), Element.of(to.kind).flatMap(opposite)) match {
      case (Some(a), Some(b)) => a == b
      case _                  => false
    }

  private def hasOpposite(item: Item, gem: Gem): Boolean =
    item.socketedGems.exists(isOpposite(_, gem))

  // Гасит первый камень противоположной стихии (гнездо становится свободным).
  private def removeOpposite(item: Item, gem: Gem): Item =
    item.sockets.indexWhere(_.exists(isOpposite(_, gem))) match {
      case -1  => item
      case idx => item.copy(sockets = item.sockets.updated(idx, None))
    }

  // Вставка камня в предмет `targetId`: возвращает обновлённое снаряжение и предмет.
  private def socketInto(eq: Equipment, targetId: Long, gem: Gem): Option[(Equipment, Item)] =
    eq.allItems.find(i => i.id == targetId && i.itemType != ItemType.NoItem && i.hasFreeSocket).map { item =>
      val updated = item.socketGem(gem)
      (eq.replacing(updated), updated)
    }

  /** Как зовётся узор вещи для экрана: умение, пассивка или «узора нет». */
  def chiselPattern(item: Item): String =
    Chisel.pattern(item).getOrElse(content.text("inventory.chisel.noPattern"))

  /** Сколько мест освободить в сумке, чтобы переписать вещь. Резец снимает с
    * надетой вещи «Тайник» наверняка (нынешний узор в розыгрыш не входит), и
    * без него сумка может не вместить своего. Резец, уходя, своё место
    * освобождает. 0 — переписывать можно. */
  def chiselShortage(hero: Hero, inv: Inventory, item: Item, worn: Boolean): Long =
    if (!worn || !item.passive.contains(PassiveKind.Stash)) 0L
    else Chisel.variants(item).headOption.fold(0L) { v =>
      val capDelta = InventoryState.equipmentStashDelta(hero.equipment, hero.equipment.replacing(v),
                       hero.runes.brandedPassives.toSet)
      (inv.occupied - 1L - (inv.maxItems + capDelta)).max(0L)
    }

  /** Отказ: сначала освободить место — с вещи уйдёт «Тайник». */
  def chiselStashLine(item: Item, shortage: Long): String =
    content.format("inventory.chisel.stashBlocked", "name" -> item.name, "count" -> shortage.toString)

  /** Резец сводит узор в случайный другой из тех, что бывают у такой вещи, и
    * рассыпается. С надетой вещи мог уйти или прийти «Тайник» — вместимость
    * сумки следует за ним; уйти ему, когда сумка без него переполнится,
    * нельзя: тогда ничего не трогаем и резец цел. `false` — не переписано. */
  def chisel(user: User, hero: Hero, inv: Inventory, item: Item, worn: Boolean, chisel: Item,
             renderer: Renderer): Task[Boolean] =
    Chisel.variants(item) match {
      case Nil => ZIO.succeed(false)
      case variants =>
        Random.nextIntBounded(variants.size).flatMap { i =>
          val rewritten = variants(i)
          val done      = content.format("inventory.chisel.done", "name" -> item.name,
                            "before" -> chiselPattern(item), "after" -> chiselPattern(rewritten))
          val spend     = inventoryRepo.removeItem(chisel.id, hero.id).mapError(asThrowable)
          if (worn) {
            val newEq    = hero.equipment.replacing(rewritten)
            val capDelta = InventoryState.equipmentStashDelta(hero.equipment, newEq, hero.runes.brandedPassives.toSet)
            // Подтвердили до того, как сумка наполнилась (старая кнопка), — та же проверка.
            val shortage = chiselShortage(hero, inv, item, worn)
            if (shortage > 0L) renderer.show(user, Screen(chiselStashLine(item, shortage), Nil)).as(false)
            else
              spend *> heroDao.updateEquipment(user.userId, newEq) *>
                ZIO.when(capDelta != 0)(inventoryRepo.increaseCapacity(hero.id, capDelta).orElse(ZIO.unit)) *>
                renderer.show(user, Screen(done, Nil)).as(true)
          } else
            inventoryRepo.updateItem(hero.id, rewritten).mapError(asThrowable) *> spend *>
              renderer.show(user, Screen(done, Nil)).as(true)
        }
    }

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}
