package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Journal, Renderer, SceneContent, Screen, Target}
import pangea.service.storage.ItemTrail
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.{GemBreaking, Item, ItemDetails, ItemType}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.equipment.{BeltWork, GemWork, Outfit}
import pangea.service.state.states.EquipmentState._
import pangea.service.state.{ItemMenu, State, UiScene, UserAction}
import zio.{Task, ZIO}

case class EquipmentState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  // Вещи с пояса и пыль от ломки сохраняются, прежде чем лечь в сумку: у
  // каждой свой номер из базы (см. BeltContents.persist).
  itemRepo:      ItemRepository,
  // След вещей: что сошло с пояса (см. ItemTrail).
  journal:       Option[Journal] = None
) extends State {

  /** Надеть и снять — общее с карточкой вещи в сумке и с Mini App. */
  private val outfit = new Outfit(heroDao, inventoryRepo, itemRepo, content)

  /** Сломать камень — общее с Mini App. */
  private val gemWork = new GemWork(heroDao, inventoryRepo, itemRepo, content)

  /** Вынуть с пояса — общее с Mini App. */
  private val beltWork = new BeltWork(heroDao, inventoryRepo, itemRepo, content, new ItemTrail(journal, ItemTrail.Bot))

  private val branch = new Branch(
    routes = Map(
      // Назад из карточки — на ту же страницу, откуда ушли.
      "EquipmentList" -> Target.Run { (u, _, r) => backToList(u) *> showList(u, r).as(StateType.Equipment) },
      "EquipmentPrev" -> Target.Run { (u, _, r) => navigate(u, r, -1) },
      "EquipmentNext" -> Target.Run { (u, _, r) => navigate(u, r, +1) },
      "Unequip"       -> Target.Run { (u, _, r) => unequipSelected(u, r) },
      // Примерка прямо из карточки слота: кнопка несёт id вещи из сумки.
      "EquipWear"     -> Target.Run { (u, ua, r) => wear(u, ua, r) },
      "BeltTake"      -> Target.Run { (u, ua, r) => takeFromBelt(u, ua, r) },
      "EquipPickPrev" -> Target.Run { (u, _,  r) => pickPage(u, r, -1) },
      "EquipPickNext" -> Target.Run { (u, _,  r) => pickPage(u, r, +1) },
      "BreakWorn"     -> Target.Run { (u, _,  r) => offerBreak(u, r) },
      "BreakWornPick" -> Target.Run { (u, ua, r) => confirmBreak(u, ua, r) },
      "BreakWornDo"   -> Target.Run { (u, ua, r) => doBreak(u, ua, r) },
      "BackFromEquip" -> Target.Goto(StateType.HeroStats)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = Set(StateType.HeroStats, StateType.Equipment)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    writeScene(user, EquipmentScene(page = Some(0))) *> showList(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Список слотов ──────────────────────────────────────────────────────────

  private def showList(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      scene <- readScene(user)
      // одна кнопка = один слот. Делаем индексы слотов сразу с предметами, чтобы
      // потом легко мапнуть нажатие на slot index.
      indexed = slots.zipWithIndex
      (pageSlots, totalPages, page) = ItemMenu.page(indexed, scene.page.getOrElse(0))
      header = s"🎽 ${content.text("equipment.header")}" + (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
      slotBtns = pageSlots.zipWithIndex.map { case ((slot, slotIdx), rowIdx) =>
                   val label = ItemMenu.truncate(slotLabel(slot, hero.equipment))
                   Choice(s"$SlotPrefix$slotIdx", label, row = Some(rowIdx))
                 }
      nav = navRow(page, totalPages, "EquipmentPrev", "EquipmentNext", "BackFromEquip")
      _   <- renderer.show(user, Screen(header, slotBtns ++ nav))
    } yield StateType.Equipment

  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      scene  <- readScene(user)
      (_, totalPages, _) = ItemMenu.page(slots.zipWithIndex, 0)
      cur    = scene.page.getOrElse(0)
      np     = (cur + delta).max(0).min(totalPages - 1)
      _      <- writeScene(user, scene.copy(page = Some(np)))
      res    <- showList(user, renderer)
    } yield res

  // ── Детальный экран слота ──────────────────────────────────────────────────

  private def showSlot(user: User, slotIdx: Int, renderer: Renderer): Task[StateType] =
    if (slotIdx < 0 || slotIdx >= slots.size) showList(user, renderer)
    else for {
      hero  <- getHero(user)
      scene <- readScene(user)
      slot   = slots(slotIdx)
      item   = slot.get(hero.equipment)
      isEmpty = item.itemType == ItemType.NoItem
      // В тексте — надетое: характеристики смотрят на то, что на герое сейчас.
      // Левая рука под двуручным пуста, но занята — говорим чем.
      itemLine = if (slot.busy(hero.equipment))
                   s"${LeftBusyText.capitalize}: ${hero.equipment.weapon.displayTitle}"
                 else if (isEmpty) content.text("equipment.empty") else EquipmentState.itemStats(item)
      // Рядом кнопками — всё, что подходит в этот слот и по плечу по уровню.
      fitting <- fittingItems(hero, slot)
      (pageItems, totalPages, page) = ItemMenu.page(fitting, scene.pickPage.getOrElse(0), PickPerPage)
      fitLine = if (fitting.isEmpty) ""
                else "\n\n" + content.format("equipment.fits", "count" -> fitting.size.toString) +
                     (if (totalPages > 1)
                        " " + content.format("equipment.fitsPage",
                          "page" -> (page + 1).toString, "total" -> totalPages.toString)
                      else "")
      text   = s"🎽 ${slot.name}\n$itemLine$fitLine"
      wearBtns = pageItems.zipWithIndex.map { case (it, idx) =>
                   Choice("EquipWear",
                     ItemMenu.truncate(content.format("equipment.wearBtn", "name" -> it.displayTitle)),
                     data = Map("id" -> it.id.toString), row = Some(idx + 1))
                 }
      choices = List(
        Option.when(!isEmpty)(content.choice("Unequip", "equipment.unequipBtn").copy(row = Some(0))),
        // Камни ломаются прямо на надетой вещи — снимать её для этого не нужно.
        Option.when(GemBreaking.hasGems(item))(
          content.choice("BreakWorn", "equipment.breakGem").copy(color = ChoiceColor.Negative, row = Some(0)))
      ).flatten ++ beltButtons(item) ++ wearBtns ++ pickNav(page, totalPages)
      _ <- writeScene(user, scene.copy(selectedSlot = Some(slotIdx), pickPage = Some(page)))
      _ <- renderer.show(user, Screen(text, choices))
    } yield StateType.Equipment

  /** Кнопки содержимого пояса: каждую ячейку можно вынуть обратно в сумку.
    * Что в них лежит, видно в описании самого пояса ([[Item.statsLines]]). */
  private def beltButtons(item: Item): List[Choice] = item.details match {
    case b: ItemDetails.Belt =>
      b.slots.zipWithIndex.map { case (slot, idx) =>
        Choice("BeltTake", ItemMenu.truncate(content.format("equipment.beltTake", "name" -> slot.name)),
          data = Map("slot" -> idx.toString), color = ChoiceColor.Negative, row = Some(0))
      }
    case _ => Nil
  }

  /** Вынуть расходник с пояса ([[BeltWork.take]]) — и обратно на карточку слота. */
  private def takeFromBelt(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    (readScene(user) <*> getHero(user)).flatMap { case (scene, hero) =>
      payloadField(ua, "slot").flatMap(_.toIntOption)
        .fold(ZIO.succeed(false): Task[Boolean])(beltWork.take(user, hero, _, renderer)).flatMap {
          case false => showList(user, renderer)
          case true  => scene.selectedSlot.fold(showList(user, renderer))(showSlot(user, _, renderer))
        }
    }

  /** Что из сумки встанет в этот слот: тип подходит, уровень по плечу. Сверху —
    * старшие по уровню: там обычно и прибавка больше. */
  private def fittingItems(hero: Hero, slot: Slot): Task[List[Item]] =
    inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      .map(inv => EquipmentState.fitting(hero, slot, inv.items.data))

  /** Ряд навигации карточки: «Назад» и страницы подбора, если их больше одной. */
  private def pickNav(page: Int, totalPages: Int): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice("EquipmentList", "equipment.back").copy(row = Some(row))),
      Option.when(page > 0)(Choice("EquipPickPrev", content.text("common.prev"), row = Some(row))),
      Option.when(page < totalPages - 1)(Choice("EquipPickNext", content.text("common.next"), row = Some(row)))
    ).flatten
  }

  private def pickPage(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    readScene(user).flatMap { scene =>
      scene.selectedSlot match {
        case None      => showList(user, renderer)
        case Some(idx) =>
          writeScene(user, scene.copy(pickPage = Some((scene.pickPage.getOrElse(0) + delta).max(0)))) *>
            showSlot(user, idx, renderer)
      }
    }

  // ── Примерка из карточки слота ─────────────────────────────────

  /** Надеть вещь, на кнопку которой нажали, — в тот слот, что открыт. */
  private def wear(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    readScene(user).flatMap { scene =>
      (scene.selectedSlot.filter(i => i >= 0 && i < slots.size),
       payloadField(ua, "id").flatMap(_.toLongOption)) match {
        case (Some(idx), Some(id)) => wearInto(user, idx, id, renderer)
        case _                     => showList(user, renderer)
      }
    }

  private def wearInto(user: User, slotIdx: Int, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      slot  = slots(slotIdx)
      _ <- inv.items.data.find(i => i.id == itemId && slot.accepts.contains(i.itemType)) match {
        // Сумку могли разобрать с другого экрана — тогда кнопка уже ни о чём.
        case None => renderer.show(user, Screen(content.text("equipment.wearGone"), Nil))
        case Some(it) if it.lvl > hero.lvl =>
          renderer.show(user, Screen(content.format("inventory.tooHighLevel",
            "required" -> it.lvl.toString, "current" -> hero.lvl.toString), Nil))
        case Some(it) =>
          outfit.wear(user, hero, it, slot.pairSlot, renderer)(Outfit.wornLine(content, it, _)).unit
      }
      // Остаёмся в карточке: видно новые характеристики и что ещё есть примерить.
      res <- showSlot(user, slotIdx, renderer)
    } yield res

  // ── Снятие выбранного ──────────────────────────────────────────────────────

  private def unequipSelected(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      res <- scene.selectedSlot match {
        case None => showList(user, renderer)
        case Some(idx) => doUnequip(user, idx, renderer)
      }
    } yield res

  private def doUnequip(user: User, slotIdx: Int, renderer: Renderer): Task[StateType] =
    if (slotIdx < 0 || slotIdx >= slots.size) showList(user, renderer)
    else getHero(user).flatMap(outfit.takeOff(user, _, slots(slotIdx), renderer)) *> showList(user, renderer)

  // ── Ломка камней в надетом ─────────────────────────────────────────────────

  /** Один камень — сразу подтверждение, несколько — сначала выбор. */
  private def offerBreak(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, _) =>
      GemBreaking.socketed(item) match {
        case Nil             => showList(user, renderer)
        case (idx, _) :: Nil => breakConfirmScreen(user, item, idx, renderer)
        case gems =>
          val choices = gems.map { case (idx, gem) =>
            Choice(
              "BreakWornPick",
              content.format("equipment.breakGemOption", "gem" -> gem.displayName),
              data  = Map("slot" -> idx.toString),
              color = ChoiceColor.Negative,
              row   = Some(0))
          } :+ content.choice("EquipmentList", "equipment.back").copy(row = Some(1))
          renderer.show(user, Screen(
            content.format("equipment.breakGemWhich", "item" -> item.name), choices)).as(StateType.Equipment)
      }
    }

  private def confirmBreak(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    payloadField(ua, "slot").flatMap(_.toIntOption) match {
      case None      => showList(user, renderer)
      case Some(idx) => withSelected(user, renderer)((item, _) => breakConfirmScreen(user, item, idx, renderer))
    }

  private def breakConfirmScreen(user: User, item: Item, idx: Int, renderer: Renderer): Task[StateType] =
    item.sockets.lift(idx).flatten match {
      case None => showList(user, renderer)
      case Some(gem) =>
        renderer.show(user, Screen(
          content.format("equipment.breakGemConfirm",
            "gem"   -> gem.displayName,
            "dust"  -> gem.dust.displayName,
            "count" -> gem.dustYield.toString),
          List(
            Choice("BreakWornDo", content.text("equipment.breakGemYes"),
              data = Map("slot" -> idx.toString), color = ChoiceColor.Negative, row = Some(0)),
            Choice("EquipmentList", content.text("equipment.breakGemNo"), row = Some(1))
          ))).as(StateType.Equipment)
    }

  private def doBreak(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    payloadField(ua, "slot").flatMap(_.toIntOption) match {
      case None => showList(user, renderer)
      case Some(idx) =>
        withSelected(user, renderer) { (item, hero) =>
          gemWork.breakWorn(user, hero, item, idx, renderer) *> showList(user, renderer)
        }
    }

  /** Предмет выбранного в сцене слота вместе с героем. */
  private def withSelected(user: User, renderer: Renderer)(
      f: (Item, Hero) => Task[StateType]): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      res <- scene.selectedSlot.filter(i => i >= 0 && i < slots.size).map(i => slots(i).get(hero.equipment)) match {
        case Some(item) if item.itemType != ItemType.NoItem => f(item, hero)
        case _                                                                => showList(user, renderer)
      }
    } yield res

  private def payloadField(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  // ── Fallback: динамические id EquipSlot_<idx> ──────────────────────────────

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(SlotPrefix) =>
        a.drop(SlotPrefix.length).toIntOption.fold(showList(user, renderer)) { idx =>
          // Каждый слот открывается с первой страницы подбора.
          readScene(user).flatMap(s => writeScene(user, s.copy(pickPage = Some(0)))) *>
            showSlot(user, idx, renderer)
        }
      case _ => showList(user, renderer)
    }

  // ── Хелперы ─────────────────────────────────────────────────────────────────

  private def slotLabel(slot: Slot, eq: Equipment): String = {
    val item = slot.get(eq)
    if (slot.busy(eq)) s"${slot.name}: $LeftBusyText"
    else if (item.itemType == ItemType.NoItem) s"${slot.name}: —"
    else s"${slot.name}: ${item.displayTitle}"
  }

  private def navRow(page: Int, totalPages: Int, prevId: String, nextId: String, backId: String): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice(backId, "equipment.back").copy(row = Some(row))),
      Option.when(page > 0)(Choice(prevId, content.text("common.prev"), row = Some(row))),
      Option.when(page < totalPages - 1)(Choice(nextId, content.text("common.next"), row = Some(row)))
    ).flatten
  }

  /** Закрыть карточку, не трогая страницу списка. */
  private def backToList(user: User): Task[Unit] =
    readScene(user).flatMap(scene => writeScene(user, scene.copy(selectedSlot = None)))

  private def readScene(user: User): Task[EquipmentScene] =
    UiScene.read(heroDao, user.userId, UiScene.Equipment, EquipmentScene())

  private def writeScene(user: User, scene: EquipmentScene): Task[Unit] =
    UiScene.write(heroDao, user.userId, UiScene.Equipment, scene)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object EquipmentState {

  val SlotPrefix = "EquipSlot_"

  /** Руки: правая держит одноручное или двуручное, левая — второе одноручное. */
  val RightHandName = "Правая рука"
  val LeftHandName  = "Левая рука"

  /** Левая рука при двуручном в правой: пуста, но занята. */
  val LeftBusyText = "занята двуручным"

  /** Что берут в руку: оружие или щит — в любую, в любом сочетании. */
  val HandTypes: Set[ItemType] = Set(ItemType.Weapon, ItemType.Shield)

  /** Сколько вещей на примерку в карточке: ряд 0 занят «Снять»/«Сломать», а
    * последний — навигацией, так что на кнопки вещей остаётся семь рядов. */
  val PickPerPage: Int = 7

  case class EquipmentScene(
    page:         Option[Int] = None,
    selectedSlot: Option[Int] = None,
    // Страница списка вещей, подходящих в открытый слот.
    pickPage:     Option[Int] = None
  )
  object EquipmentScene {
    implicit val encoder: Encoder[EquipmentScene] = deriveEncoder
    implicit val decoder: Decoder[EquipmentScene] = deriveDecoder
  }

  /** Слот снаряжения: как достать надетое, как освободить, что в него встаёт
    * (`accepts`) и, для колец и рук, какой это из двух слотов (`pairSlot` —
    * чтобы примерка меняла именно открытое кольцо или руку, а не первое
    * свободное). `busy` — слот пуст, но занят чужой вещью: левая рука при
    * двуручном в правой. */
  case class Slot(
    name:     String,
    get:      Equipment => Item,
    clear:    Equipment => Equipment,
    accepts:  Set[ItemType],
    pairSlot: Option[Int]          = None,
    busy:     Equipment => Boolean = _ => false
  )

  /** Что из `items` встанет в этот слот: тип подходит, уровень по плечу. Сверху
    * — старшие по уровню: там обычно и прибавка больше. Розы, выкованные до
    * переезда в пояс, лежат в сумке вещами доп. слота — примерять их больше
    * некуда, место цветка теперь в ячейке. Общее для карточки слота в боте и
    * для «Снаряжения» в Mini App. */
  def fitting(hero: Hero, slot: Slot, items: List[Item]): List[Item] =
    items.filter(i => slot.accepts.contains(i.itemType) && i.rose.isEmpty && i.lvl <= hero.lvl)
      .sortBy(i => (-i.lvl, i.name))

  val slots: List[Slot] = List(
    Slot("Шлем",        _.helmet,       _.copy(helmet       = Item.NoItem), Set(ItemType.Helmet)),
    Slot("Наплечники",  _.shoulderPads, _.copy(shoulderPads = Item.NoItem), Set(ItemType.ShoulderPads)),
    Slot("Нагрудник",   _.chestPlate,   _.copy(chestPlate   = Item.NoItem), Set(ItemType.ChestPlate)),
    Slot("Браслеты",    _.bracelets,    _.copy(bracelets    = Item.NoItem), Set(ItemType.Bracelets)),
    Slot("Перчатки",    _.gloves,       _.copy(gloves       = Item.NoItem), Set(ItemType.Gloves)),
    // Поножи идут в тот же слот, что штаны (см. InventoryState.withSlot).
    Slot("Штаны",       _.pants,        _.copy(pants        = Item.NoItem), Set(ItemType.Pants, ItemType.Leggings)),
    Slot("Сапоги",      _.boots,        _.copy(boots        = Item.NoItem), Set(ItemType.Boots)),
    Slot("Амулет",      _.amulet,       _.copy(amulet       = Item.NoItem), Set(ItemType.Amulet)),
    Slot("Кольцо 1",    _.firstRing,    _.copy(firstRing    = Item.NoItem), Set(ItemType.Ring), Some(1)),
    Slot("Кольцо 2",    _.secondRing,   _.copy(secondRing   = Item.NoItem), Set(ItemType.Ring), Some(2)),
    Slot("Пояс",        _.belt,         _.copy(belt         = Item.NoItem), Set(ItemType.Belt)),
    // Руки — пара слотов, как кольца (см. InventoryState.equip): в каждую —
    // оружие или щит; двуручное встаёт в правую и занимает обе.
    Slot(RightHandName, _.weapon,       _.copy(weapon       = Item.NoItem), HandTypes, Some(1)),
    Slot(LeftHandName,  _.leftHand,     _.copy(leftHand     = Item.NoItem), HandTypes, Some(2),
         busy = _.leftBusy),
    Slot("Доп. оружие", _.additionalWeapon, _.copy(additionalWeapon = Item.NoItem), Set(ItemType.AdditionalWeapon))
  )

  def itemStats(item: Item): String = {
    val stats    = item.statsLines
    val statsStr = if (stats.isEmpty) "Нет характеристик" else stats.mkString("\n")
    s"${item.displayTitle}\n$statsStr"
  }
}
