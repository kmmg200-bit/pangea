package pangea.service.equipment

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.generator.item.MaterialGenerator
import pangea.model.battle.Element
import pangea.model.hero.{Equipment, Hero, WeaponDust}
import pangea.model.inventory.Inventory
import pangea.model.item.{Chisel, Gem, GemBreaking, Item, ItemType, MaterialKind, PassiveKind}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.BeltContents
import pangea.service.state.states.InventoryState
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Камни в гнёздах, их пыль на оружии и резец без экрана. Одно на бота —
  * карточки слота и сумки, экран вставки — и на Mini App: правила у них одни.
  * Всё меняет героя, поэтому зовётся под локом игрока: в боте его держит ход,
  * в приложении — сам запрос. Что сказать игроку, идёт в `renderer`: бот
  * показывает в чате, приложение собирает в ответ. */
final class GemWork(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository,
                    content: SceneContent) {

  /** Сломать камень из гнезда `socket` надетой вещи: камень крошится в пыль
    * своего вида, пыль — в сумку. Пыли этого вида под предел ([[dustFull]]) —
    * сказать и ничего не трогать. `false` — такого камня в гнезде нет, сказать
    * нечего. */
  def breakWorn(user: User, hero: Hero, item: Item, socket: Int, renderer: Renderer): Task[Boolean] =
    GemBreaking.breakSocket(item, socket) match {
      case None => ZIO.succeed(false)
      case Some((emptied, gem)) =>
        for {
          now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
          inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
          // Рубин уходит из гнезда — максимум здоровья падает, и запасы с ним.
          broken = hero.reequip(hero.equipment.replacing(emptied), now)
          _ <-
            if (dustFull(inv, gem)) renderer.show(user, Screen(dustFullLine(gem), Nil))
            else
              heroDao.updateEquipmentAndFightStats(user.userId, broken.equipment, broken.fightStats) *>
                grantDust(hero, gem) *>
                renderer.show(user, Screen(content.format("equipment.breakGemDone",
                  "gem" -> gem.displayName, "count" -> gem.dustYield.toString, "dust" -> gem.dust.displayName), Nil))
        } yield true
    }

  /** Расколоть камень, лежащий в сумке отдельной вещью: он крошится в пыль
    * своего вида, пыль — в сумку. Пыли этого вида под предел ([[dustFull]]) —
    * камень цел, и сказано почему; камень уходит только вместе с выданной
    * пылью. `false` — это не камень, сказать нечего. */
  def crush(user: User, hero: Hero, item: Item, renderer: Renderer): Task[Boolean] =
    item.gem match {
      case None => ZIO.succeed(false)
      case Some(gem) =>
        for {
          inv <- inventoryRepo.get(hero.id).mapError(asThrowable)
          _ <-
            if (dustFull(inv, gem)) renderer.show(user, Screen(dustFullLine(gem), Nil))
            else
              inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable) *> grantDust(hero, gem) *>
                renderer.show(user, Screen(content.format("inventory.breakGemDone",
                  "gem" -> gem.displayName, "count" -> gem.dustYield.toString, "dust" -> gem.dust.displayName), Nil))
        } yield true
    }

  /** Посыпать надетое оружие горстью пыли `dust` — с её карточки в боте и в
    * приложении. Горсть тратится в любом случае — даже когда магия даёт
    * всполох или покрытие идёт четвёртым слоем. Сколько слоёв уже на оружии,
    * игрок держит в голове сам — не считаем за него. Оружие не надето —
    * сказать и ничего не трогать. `false` — это не пыль, сказать нечего. */
  def sprinkle(user: User, hero: Hero, dust: Item, renderer: Renderer): Task[Boolean] =
    dust.material.filter(_.gem.isDefined) match {
      case None => ZIO.succeed(false)
      case Some(_) if hero.equipment.unarmed =>
        renderer.show(user, Screen(content.text("inventory.dustNoWeapon"), Nil)).as(true)
      case Some(kind) =>
        val outcome = WeaponDust.sprinkle(kind, hero.weaponDust, hero.equipment.handGems)
        val line = outcome match {
          case WeaponDust.Outcome.Applied(_) =>
            content.format("inventory.dustApplied",
              "dust"   -> kind.displayName,
              "weapon" -> hero.equipment.mainWeapon.name)
          case WeaponDust.Outcome.Clash(_) =>
            content.format("inventory.dustClash", "dust" -> kind.displayName,
              "pct" -> WeaponDust.PenaltyPct.toString)
          case WeaponDust.Outcome.Overload(_) =>
            content.format("inventory.dustOverload", "pct" -> WeaponDust.PenaltyPct.toString)
        }
        inventoryRepo.removeItem(dust.id, hero.id).mapError(asThrowable) *>
          heroDao.updateWeaponDust(user.userId, outcome.next) *>
          renderer.show(user, Screen(line, Nil)).as(true)
    }

  /** Пыль места в сумке не занимает (см. `Inventory.occupied`), но одного вида
    * её не больше [[Item.HoardLimit]] горстей: если столько, сколько даст камень,
    * уже не унести, камень не трогаем. */
  def dustFull(inv: Inventory, gem: Gem): Boolean = hoardFull(inv, gem.dust, gem.dustYield)

  /** Ещё `count` горстей пыли `dust` не унести: её этого вида уже под предел. */
  private def hoardFull(inv: Inventory, dust: MaterialKind, count: Int): Boolean =
    MaterialGenerator.item(dust).hoardKey.exists(k => inv.hoardCount(k) + count > Item.HoardLimit)

  /** Отказ: пыли этого вида под предел, камень цел. */
  def dustFullLine(gem: Gem): String =
    content.format("inventory.gemDustFull", "dust" -> gem.dust.displayName, "n" -> Item.HoardLimit.toString)

  /** Пыль за камень — в сумку. Каждая горсть — со своим номером из базы: без
    * него горсти разных видов делили −1, и снятие одной убирало из сумки все. */
  def grantDust(hero: Hero, gem: Gem): Task[Unit] =
    BeltContents.persist(Some(itemRepo), hero.id, List.fill(gem.dustYield)(MaterialGenerator.item(gem.dust)))
      .flatMap(ZIO.foreachDiscard(_)(d => inventoryRepo.addItem(hero.id, d).mapError(asThrowable)))

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
    * Пыль места в сумке не занимает, но одного вида её не больше
    * [[Item.HoardLimit]] горстей — как и при ломке: если горсть одного из двух
    * видов уже не унести, вспышки нет, оба камня целы, и сказано почему. */
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
    val gems    = inserted :: item.socketedGems.find(isOpposite(_, inserted)).toList
    inventoryRepo.get(hero.id).mapError(asThrowable).flatMap { inv =>
      gems.find(g => hoardFull(inv, g.dust, 1)) match {
        case Some(full) => renderer.show(user, Screen(dustFullLine(full), Nil))
        case None =>
          for {
            _ <- heroDao.updateEquipmentAndFightStats(user.userId, after.equipment, after.fightStats)
            _ <- spend
            // Каждая горсть — со своим номером из базы: без него горсти делили −1, и
            // снятие одной (в куб, в ячейку) убирало из сумки все.
            _ <- ZIO.foreachDiscard(gems)(g =>
                   itemRepo.persist(hero.id, MaterialGenerator.item(g.dust))
                     .flatMap(d => inventoryRepo.addItem(hero.id, d).mapError(asThrowable)))
            _ <- renderer.show(user, Screen(content.format("socketing.annihilate",
                   "dust" -> gems.map(_.dust.displayName).mkString(", ")), Nil))
          } yield ()
      }
    }
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
