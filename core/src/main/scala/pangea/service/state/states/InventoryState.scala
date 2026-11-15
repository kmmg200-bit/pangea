package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.item.{BrewEffect, BrewKind}
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.TreasureMapGenerator
import pangea.model.hero.{Equipment, Hero}
import pangea.model.inventory.Inventory
import pangea.model.item.{BeltSlot, Bundling, Chisel, Gem, GemBreaking, Item, ItemDetails, ItemStack, ItemType, MaterialKind, QuestItemKind, RefillSource, TrophyKind}
import pangea.model.monster.Race
import pangea.model.quest.{Difficulty, NpcQuest, NpcQuests}
import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.equipment.{BeltWork, BrewWork, BundleWork, GemWork, Outfit}
import pangea.service.state.states.InventoryState._
import pangea.service.state.states.home.Racks
import pangea.service.state.{BarrelThievesQuest, BeltContents, BigDealQuest, BramblQuest, HedgeSongQuest, HerbLore, HotGoodsQuest, ItemMenu,
  JorgenQuest,
  FaelinQuest, MarisaQuest, MurlocQuest, NpcQuestLog, PlukhQuest, QuestCard, QuestGoods, SlotChoice, StackMove, State,
  StoryBrandQuest, StoryFaceQuest, StoryListQuest, StoryMarkQuest, StoryMorningQuest, StoryNewcomerQuest, StoryPowderQuest,
  StoryToolQuest, StrayCasketQuest, UiScene, UserAction, Villages}
import zio.{Random, Task, ZIO}

case class InventoryState(
  heroDao:        HeroDao,
  inventoryRepo:  InventoryRepository,
  itemRepository: ItemRepository,
  content:        SceneContent,
  artifacts:      Option[ArtifactRepository] = None
) extends State {

  /** Надеть — общее с карточкой слота и с Mini App. */
  private val outfit = new Outfit(heroDao, inventoryRepo, itemRepository, content)

  /** Распаковать стопку — общее с Mini App. */
  private val bundles = new BundleWork(inventoryRepo, itemRepository, content)

  /** Резец — общее с Mini App. */
  private val gemWork = new GemWork(heroDao, inventoryRepo, itemRepository, content)

  /** Отвары и пояс — общее с Mini App. */
  private val brewWork = new BrewWork(heroDao, inventoryRepo, content)

  /** Кнопки заданий на карточках сюжетных предметов — общее с Mini App. */
  private val questCard = new QuestCard(heroDao, inventoryRepo, itemRepository, content)
  private val beltWork = new BeltWork(heroDao, inventoryRepo, itemRepository, content)

  private val branch = new Branch(
    routes = Map(
      // Есть хранилища — назад в рюкзак, откуда сюда и зашли; нет — сразу в
      // «Персонаж»: рюкзак такому герою не показывают. Зашли с экрана добычи или
      // находки — обратно туда, есть хранилища или нет.
      "BackFromInventory" -> Target.Run { (u, _, _) => backTarget(u) },
      // Возврат из карточки к списку: страницу держим ту же, с которой ушли, —
      // иначе после каждого взгляда на вещь приходится листать заново. Сбрасываем
      // только выбранную вещь: карточку мы закрыли.
      "InventoryList"     -> Target.Run { (u, _, r) => backToList(u) *> showList(u, r).as(StateType.Inventory) },
      // Отказ в подтверждении или выборе, начатом с карточки, — назад на неё,
      // а не в список: у стопки остались ещё вещи.
      "ItemCard"          -> Target.Run { (u, _, r) => backToCard(u, r) },
      "InventoryPrev"     -> Target.Run { (u, _, r) => navigate(u, r, -1) },
      "InventoryNext"     -> Target.Run { (u, _, r) => navigate(u, r, +1) },
      "Equip"             -> Target.Run { (u, _, r) => equipSelected(u, r) },
      "EquipRing"         -> Target.Run { (u, ua, r) => equipChosenSlot(u, ua, r) },
      "EquipHand"         -> Target.Run { (u, ua, r) => equipChosenSlot(u, ua, r) },
      "Drop"              -> Target.Run { (u, _, r) => dropSelected(u, r) },
      // Стопка одинаковых — выбросить разом, но сперва спросить.
      "DropAll"           -> Target.Run { (u, _, r) => offerDropAll(u, r) },
      "DropAllYes"        -> Target.Run { (u, ua, r) => dropAll(u, ua, r) },
      // Стопка: пять одинаковых — в одну вещь на одном месте, и обратно.
      "Pack"              -> Target.Run { (u, _, r) => packSelected(u, r) },
      "Unpack"            -> Target.Run { (u, _, r) => unpackSelected(u, r) },
      "CombineMap"        -> Target.Run { (u, _, r) => combineSelected(u, r) },
      "CombineRune"       -> Target.Run { (u, _, r) => combineRuneSelected(u, r) },
      "SocketInsert"      -> Target.Run { (u, _, r) => startSocketing(u, r) },
      "BeltPut"           -> Target.Run { (u, _, r) => putOnBelt(u, r) },
      // Пояс полон: что снять ради новой вещи — и «Назад» на её карточку.
      "BeltSwap"          -> Target.Run { (u, ua, r) => swapOnBelt(u, ua, r) },
      "BeltSwapBack"      -> Target.Run { (u, _, r) => backToCard(u, r) },
      "PurseReturn"       -> Target.Run { (u, _,  r) => returnPurse(u, r) },
      "PurseOpen"         -> Target.Run { (u, _,  r) => openPurse(u, r) },
      // «Резец рунописца»: чей узор переписать, подтверждение, переписать.
      "ChiselUse"         -> Target.Run { (u, ua, r) => offerChisel(u, ua, r) },
      "ChiselPick"        -> Target.Run { (u, ua, r) => confirmChisel(u, ua, r) },
      "ChiselDo"          -> Target.Run { (u, ua, r) => doChisel(u, ua, r) },
      "BreakGem"          -> Target.Run { (u, _,  r) => offerBreak(u, r) },
      "BreakGemPick"      -> Target.Run { (u, ua, r) => confirmBreak(u, ua, r) },
      "BreakGemDo"        -> Target.Run { (u, ua, r) => doBreak(u, ua, r) },
      "CrushGem"          -> Target.Run { (u, _,  r) => offerCrush(u, r) },
      "CrushGemDo"        -> Target.Run { (u, _,  r) => doCrush(u, r) },
      "DustWeapon"        -> Target.Run { (u, _,  r) => sprinkleDust(u, r) },
      "DrinkBrew"         -> Target.Run { (u, _,  r) => drinkBrew(u, r) },
      "RefillByBrew"      -> Target.Run { (u, _,  r) => refillByBrew(u, r) },
      // Продать отвар с карточки — только в городе.
      "SellBrew"          -> Target.Run { (u, _,  r) => sellBrew(u, r) },
      "RackKeyUse"        -> Target.Run { (u, ua, r) => useRackKey(u, ua, r) },
      "RefillFlaskPick"   -> Target.Run { (u, ua, r) => refillFlask(u, ua, r) },
      "CoatWeapon"        -> Target.Run { (u, _,  r) => coatWeapon(u, r) },
      "CureTraumaPick"    -> Target.Run { (u, ua, r) => cureTrauma(u, ua, r) },
      "SentencePick"      -> Target.Run { (u, ua, r) => sentenceRace(u, ua, r) },
      // «Бродячий ларец»: скормить камень выбранного качества (сытый ведёт к
      // хозяину — кнопкой задания, см. ниже).
      "StrayFeed"         -> Target.Run { (u, ua, r) => feedStray(u, ua, r) },
      // Трактаты Густаво: читать, пока не осилишь.
      "ReadTreatise"      -> Target.Run { (u, _,  r) => readTreatise(u, r) },
      "IdentifyFlower"    -> Target.Run { (u, _,  r) => identifyFlower(u, r) },
      "IdentifyAll"       -> Target.Run { (u, _,  r) => identifyAllFlowers(u, r) }
    ) ++
      // Кнопки заданий с карточек сюжетных предметов — письмо и карты Марисы,
      // карта мурлоков, купоны, ларец, бочка, наёмники, главы сюжета, Барти —
      // общие с Mini App (см. QuestCard).
      QuestCard.Ids.map(id => id -> (Target.Run { (u, _, r) => questGo(u, r, id) }: Target)).toMap,
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.HeroStats, StateType.Backpack, StateType.Inventory, StateType.Socketing,
        StateType.MarisaHunt, StateType.MurlocVillage, StateType.RaceQuest, StateType.NameChange,
        StateType.StrayCasket, StateType.BarrelThieves, StateType.StoryCache, StateType.StoryWarehouse,
        StateType.StoryMeeting, StateType.StoryBridge, StateType.StorySojka, StateType.StoryArchive, StateType.StoryBindery,
        StateType.StoryGraveyard, StateType.BartiHouse, StateType.BartiCaravan, StateType.JorgenLair, StateType.VillageTrail,
        StateType.PlukhHunt, StateType.FaelinRoots, StateType.Village, StateType.VillageBandits, StateType.VillageSpiders,
        StateType.VillageCaravan, StateType.StoryDen, StateType.StoryCellar) ++ BackpackOrigins

  // С экрана добычи и с находки сюда ведёт «🎒 Рюкзак» — он и кладёт свой экран
  // в return_state.
  private def backTarget(user: User): Task[StateType] =
    for {
      hero <- getHero(user)
      keeps <- artifacts.fold(ZIO.succeed(false): Task[Boolean])(
                 _.get(hero.id).map(_.anyOwned).orElseSucceed(false))
      from  <- heroDao.readReturnState(user.userId)
    } yield from.filter(BackpackOrigins.contains)
              .getOrElse(if (keeps) StateType.Backpack else StateType.HeroStats)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    readScene(user).flatMap { scene =>
      scene.selectedId.filter(_ => scene.cardOnReturn.contains(true)) match {
        // Вернулись со вставки камня — на карточку той же стопки: камни в ней,
        // может, ещё остались.
        case Some(id) => writeScene(user, scene.copy(cardOnReturn = None)) *> showItem(user, id, renderer).unit
        case None     => writeScene(user, InventoryScene(page = Some(0))) *> showList(user, renderer).unit
      }
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Список ──────────────────────────────────────────────────────────────────

  /** Список сумки. `note` — что только что случилось (выбросили вещь): строкой
    * над списком, а не отдельным сообщением. */
  private def showList(user: User, renderer: Renderer, note: Option[String] = None): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items  = inv.items.data
      scene <- readScene(user)
      _ <- if (items.isEmpty) renderer.show(user, emptyScreen(hero, note))
           else {
             // Одинаковые камни, материалы и трофеи показываются одной кнопкой,
             // но места в сумке занимают по-прежнему поштучно. Ключи стоек —
             // связкой, когда их два и больше.
             val stacks = InventoryState.listed(items)
             val ring   = InventoryState.keyRing(items).isDefined
             val (pageItems, totalPages, page) = ItemMenu.page(stacks, scene.page.getOrElse(0))
             val header   = s"📦 Инвентарь${if (totalPages > 1) s" (${page + 1}/$totalPages)" else ""} | 🪙 ${hero.silver} | 🟡 ${hero.doubloons}"
             val itemBtns = ItemMenu.stackButtons(pageItems, ItemActionPrefix, label = (it, n) =>
               Option.when(ring && InventoryState.isRackKey(it))(InventoryState.keyRingLabel(content, n)))
             val nav      = navRow(page, totalPages, "InventoryPrev", "InventoryNext", "BackFromInventory")
             renderer.show(user, Screen(StackMove.withNote(note, header), itemBtns ++ nav))
           }
    } yield StateType.Inventory

  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      (_, totalPages, _) = ItemMenu.page(InventoryState.listed(inv.items.data), 0)
      scene <- readScene(user)
      cur    = scene.page.getOrElse(0)
      np     = (cur + delta).max(0).min(totalPages - 1)
      _     <- writeScene(user, scene.copy(page = Some(np)))
      res   <- showList(user, renderer)
    } yield res

  // ── Детальный экран выбранного предмета ────────────────────────────────────

  /** Карточка вещи. У стопки показываем ПЕРВУЮ её вещь, а тратим ПОСЛЕДНЮЮ
    * (см. [[withSelected]]): показанная остаётся в сумке, пока в стопке есть
    * что-то ещё, и после действия игрок возвращается на ту же карточку — с
    * меньшим счётчиком, — а не в список. */
  private def showItem(user: User, itemId: Long, renderer: Renderer, note: Option[String] = None): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      scene <- readScene(user)
      shown  = inv.items.data.find(_.id == itemId).map(i => stackOf(inv.items.data, i).headOption.getOrElse(i))
      res <- shown match {
        case None => showList(user, renderer, note)
        case Some(item) if item.isQuestItem =>
          writeScene(user, scene.copy(selectedId = Some(item.id))) *> showQuestItem(user, item, renderer)
        case Some(item @ InventoryState.Brewed(kind)) =>
          writeScene(user, scene.copy(selectedId = Some(item.id))) *> showBrew(user, item, kind, inv.items.data, renderer, note)
        case Some(item) if item.isBundle =>
          writeScene(user, scene.copy(selectedId = Some(item.id))) *> showBundle(user, item, hero, inv.items.data, renderer, note)
        case Some(item) =>
          // Сколько таких же лежит в сумке — счётчик над описанием; кнопки при
          // этом трогают ровно один предмет из стопки.
          val count    = ItemStack.countOf(inv.items.data, item)
          val text     = StackMove.withNote(note, itemDetail(item, hero, hero.silver) + stackLine(count))
          // Розу, выкованную до её переезда в пояс, носить в доп. слоте больше
          // незачем: раскрывается она из ячейки. Фляге тоже место на поясе — у
          // неё «На пояс» вместо «Надеть».
          val canEquip = ItemType.equippable.contains(item.itemType) && item.rose.isEmpty &&
                         item.itemType != ItemType.Flask
          val canCombine = item.itemType == ItemType.TreasureMapHalf
          // Пять малых рун одного узора складываются в большую — прямо в сумке.
          val canFoldRune = item.isSmallRune
          val canSocket  = item.gem.isDefined
          // Ломать можно и камни в гнёздах вещи, и сам камень, лежащий в сумке.
          val canBreak   = GemBreaking.hasGems(item)
          // Краденый кошелёк: вернуть хозяину через гильдию или развязать самому.
          val isPurse    = item.material.contains(MaterialKind.StolenPurse)
          val choices  = List(
            Option.when(canEquip)(content.choice("Equip", "inventory.equip").copy(row = Some(0))),
            Option.when(canCombine)(content.choice("CombineMap", "inventory.combineMap").copy(color = ChoiceColor.Positive, row = Some(0))),
            Option.when(canFoldRune)(content.choice("CombineRune", "inventory.combineRune").copy(color = ChoiceColor.Positive, row = Some(0))),
            Option.when(canSocket)(content.choice("SocketInsert", "inventory.socket").copy(color = ChoiceColor.Positive, row = Some(0))),
            Option.when(isPurse)(content.choice("PurseReturn", "purse.return").copy(color = ChoiceColor.Positive, row = Some(0))),
            Option.when(isPurse)(content.choice("PurseOpen", "purse.open").copy(row = Some(0))),
            Option.when(item.material.contains(MaterialKind.RuneChisel))(
              content.choice("ChiselUse", "inventory.chisel.use").copy(row = Some(0))),
            Option.when(canBreak)(content.choice("BreakGem", "inventory.breakGem").copy(color = ChoiceColor.Negative, row = Some(0))),
            Option.when(canSocket)(content.choice("CrushGem", "inventory.crushGem").copy(color = ChoiceColor.Negative, row = Some(0))),
            // Неопознанный цветок — приглядеться к нему ещё раз (см. HerbLore.identify).
            Option.when(item.material.exists(MaterialKind.isUnknownFlower))(
              content.choice("IdentifyFlower", "knowledge.identifyLabel").copy(color = ChoiceColor.Positive, row = Some(0))),
            // Таких цветов несколько — опознать их разом (см. HerbLore.identifyAll).
            Option.when(item.material.exists(MaterialKind.isUnknownFlower) && count > 1)(
              content.choice("IdentifyAll", "knowledge.identifyAllLabel", "n" -> count.toString)
                .copy(color = ChoiceColor.Positive, row = Some(0))),
            // Пыль сыплется на оружие: разовое покрытие на один бой.
            Option.when(item.material.exists(_.gem.isDefined))(
              content.choice("DustWeapon", "inventory.dustWeapon").copy(color = ChoiceColor.Positive, row = Some(0))),
            // Фляга, пыль и цветок ложатся на пояс: оттуда их достают в бою.
            // У отвара своя карточка и своя такая же кнопка (см. showBrew).
            Option.when(BeltSlot.of(item).isDefined)(
              content.choice("BeltPut", "inventory.beltPut").copy(color = ChoiceColor.Positive, row = Some(0))),
            // Сюжетную экипировку не выбрасывают: её не теряют (см. Item.isStoryGear).
            Option.when(!item.isStoryGear)(content.choice("Drop", "inventory.drop").copy(color = ChoiceColor.Negative, row = Some(0))),
            packChoice(item, count),
            if (item.isStoryGear) None else dropAllChoice(count),
            Some(content.choice("InventoryList", "inventory.exit").copy(row = Some(1), color = ChoiceColor.Negative))
          ).flatten
          writeScene(user, scene.copy(selectedId = Some(item.id))) *>
            renderer.show(user, Screen(text, choices)).as(StateType.Inventory)
      }
    } yield res

  // ── Отвары из трав ─────────────────────────────────────────────────────────

  /** Карточка отвара: описание, рецепт, сколько таких в сумке. Кнопки — по делу:
    * «Использовать» у того, что применяют с карточки; «На пояс» у того, чем пользуются в бою;
    * «Заправить флягу», если на поясе есть фляга такого толка; «Смазать
    * оружие» у смазок; «Продать» — у всех, кроме выпивки (продаёт только в
    * городе, см. [[sellBrew]]). Шнапс и самогон уходят Трактирщику, и дороже:
    * кнопок у них нет.
    *
    * Боевой отвар с карточки не пьётся вовсе — об этом говорим прямо, иначе
    * игрок будет искать у него кнопку «Использовать». */
  private def showBrew(user: User, item: Item, kind: BrewKind, all: List[Item], renderer: Renderer,
                       note: Option[String]): Task[StateType] =
    getHero(user).flatMap { hero =>
      val count   = ItemStack.countOf(all, item)
      val text    = StackMove.withNote(note, InventoryState.brewText(content, item, kind) + stackLine(count))
      val flaskFits = InventoryState.brewFlasks(hero).exists(f => kind.refills.contains(f.source))
      val coats = kind.effect match { case BrewEffect.Coat(_, _) => true; case _ => false }
      val choices = List(
        Option.when(kind.drinkable)(content.choice("DrinkBrew", "brew.drink").copy(color = ChoiceColor.Positive, row = Some(0))),
        // Склянка ложится в ячейку пояса — оттуда её достают в бою.
        Option.when(BeltSlot.of(item).isDefined)(
          content.choice("BeltPut", "inventory.beltPut").copy(color = ChoiceColor.Positive, row = Some(0))),
        Option.when(coats)(content.choice("CoatWeapon", "brew.coat").copy(color = ChoiceColor.Positive, row = Some(0))),
        Option.when(flaskFits)(content.choice("RefillByBrew", "brew.refill").copy(color = ChoiceColor.Positive, row = Some(0))),
        BrewKind.salePrice(kind).map(price =>
          Choice("SellBrew", content.format("brew.sell", "price" -> price.toString), row = Some(0))),
        Some(content.choice("Drop", "inventory.drop").copy(color = ChoiceColor.Negative, row = Some(0))),
        packChoice(item, count),
        dropAllChoice(count),
        Some(content.choice("InventoryList", "inventory.exit").copy(row = Some(1), color = ChoiceColor.Negative))
      ).flatten
      renderer.show(user, Screen(text, choices)).as(StateType.Inventory)
    }

  /** Продать отвар с карточки — только в городе («Персонаж» открыт из него), по
    * цене его ранга ([[BrewKind.salePrice]]). У стопки уходит одна бутылка, и
    * игрок остаётся на той же карточке. Не из города — отказ над карточкой,
    * отвар цел. Выпивку так не продать (кнопка из старого сообщения) — просто
    * карточка: её берёт Трактирщик. */
  private def sellBrew(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      item.brew.flatMap(BrewKind.salePrice) match {
        case None => showItem(user, item.id, renderer)
        case Some(price) =>
          heroDao.readReturnState(user.userId).flatMap { from =>
            if (!from.exists(StateType.cityStates.contains))
              backToCard(user, renderer, Some(content.text("brew.sellOnlyInCity")))
            else
              inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
                heroDao.updateSilver(user.userId, hero.silver + price) *>
                backToCard(user, renderer, Some(content.format("brew.sold",
                  "name" -> item.name, "price" -> price.toString)))
          }
      }
    }

  // ── Стопки ─────────────────────────────────────────────────────────────────

  /** «📦 В стопку» — когда таких вещей в сумке хватает на стопку. Ряд — с
    * «Выбросить все» и «Выйти»: первый на карточке камня и так полон. */
  private def packChoice(item: Item, count: Int): Option[Choice] =
    Option.when(Bundling.packable(item) && count >= Bundling.Size)(
      content.choice("Pack", "inventory.bundle.pack").copy(row = Some(1)))

  /** Карточка стопки: что в ней — и «Распаковать». */
  private def showBundle(user: User, item: Item, hero: Hero, all: List[Item], renderer: Renderer,
                         note: Option[String]): Task[StateType] = {
    val count   = ItemStack.countOf(all, item)
    val text    = StackMove.withNote(note, InventoryState.bundleText(content, item, hero) + stackLine(count))
    val choices = List(
      Some(content.choice("Unpack", "inventory.bundle.unpack").copy(row = Some(0))),
      Some(content.choice("Drop", "inventory.drop").copy(color = ChoiceColor.Negative, row = Some(0))),
      dropAllChoice(count),
      Some(content.choice("InventoryList", "inventory.exit").copy(row = Some(1), color = ChoiceColor.Negative))
    ).flatten
    renderer.show(user, Screen(text, choices)).as(StateType.Inventory)
  }

  /** Собрать стопку (см. [[BundleWork.pack]]): итог — над карточкой. Игрок
    * возвращается на карточку показанной вещи — она в сумке, пока таких
    * больше пяти. */
  private def packSelected(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res   <- scene.selectedId.flatMap(id => inv.items.data.find(_.id == id)) match {
                 case None       => showList(user, renderer)
                 case Some(item) => bundles.pack(hero.id, inv, item).flatMap(note => backToCard(user, renderer, note))
               }
    } yield res

  /** Распаковать стопку (см. [[BundleWork.unpack]]): итог — над карточкой. */
  private def unpackSelected(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res   <- scene.selectedId.flatMap(id => inv.items.data.find(_.id == id))
                 .map(i => stackOf(inv.items.data, i).lastOption.getOrElse(i)) match {
                 case None       => showList(user, renderer)
                 case Some(item) => bundles.unpack(hero.id, inv, item).flatMap(note => backToCard(user, renderer, note))
               }
    } yield res

  /** Заправить флягу отваром: только флягу своего толка и только неполную.
    * Какую — выбирает игрок: таких на поясе может быть несколько (живая вода
    * берёт и флягу целителя, и кузнеца, и стихийные), а угадать, какая ему
    * нужнее, нельзя. Спрашиваем и тогда, когда фляга одна, — как и травму у
    * костоправного отвара. */
  private def refillByBrew(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      val flasks  = InventoryState.brewFlasks(hero)
      val fitting = item.brew.toList.flatMap(kind => flasks.filter(f => kind.refills.contains(f.source)))
      fitting.filter(f => f.charges < f.maxCharges) match {
        case Nil =>
          val why =
            if (fitting.nonEmpty) "brew.flaskFull"
            else if (flasks.nonEmpty && item.brew.isDefined) "brew.wrongFlask"
            else "brew.noFlask"
          renderer.show(user, Screen(content.text(why), Nil)) *> showItem(user, item.id, renderer)
        case open =>
          val choices = open.zipWithIndex.map { case (f, i) =>
            Choice("RefillFlaskPick", Choice.fit(content.format("brew.flaskPick",
              "n" -> (f.slot + 1).toString, "name" -> f.name,
              "charges" -> f.charges.toString, "max" -> f.maxCharges.toString)),
              data = Map("slot" -> f.slot.toString), color = ChoiceColor.Positive, row = Some(i))
          } :+ content.choice("ItemCard", "inventory.exit").copy(row = Some(open.size), color = ChoiceColor.Negative)
          renderer.show(user, Screen(content.text("brew.whichFlask"), choices)).as(StateType.Inventory)
      }
    }

  /** Фляга выбрана: заправляем её, если она всё ещё своего толка и неполная.
    * Кнопка из старого сообщения (флягу уже долили или сняли) — спрашиваем
    * заново, отвар не тратим. */
  private def refillFlask(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      payloadField(ua, "slot").flatMap(_.toIntOption)
        .fold(ZIO.succeed(false): Task[Boolean])(brewWork.refill(user, hero, item, _, renderer)).flatMap {
          case true  => backToCard(user, renderer)
          case false => refillByBrew(user, renderer)
        }
    }

  /** Смазать оружие: что и как — у [[BrewWork]], общего с Mini App. */
  private def coatWeapon(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer)((item, hero) =>
      brewWork.coat(user, hero, item, renderer).flatMap(afterBrew(user, item, renderer)))

  /** Склянка ушла — на карточку: в стопке могут быть ещё; цела — на карточку этого отвара. */
  private def afterBrew(user: User, item: Item, renderer: Renderer)(spent: Boolean): Task[StateType] =
    if (spent) backToCard(user, renderer) else showItem(user, item.id, renderer)

  /** Глоток: по виду отвара. Костоправный сперва спрашивает, какую травму
    * лечить, приговор — какой род назвать. */
  private def drinkBrew(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      item.brew.map(_.effect) match {
        case Some(BrewEffect.CureTrauma) => offerCure(user, item, hero, renderer)
        case Some(BrewEffect.Sentence)   => offerSentence(user, renderer)
        case _ => brewWork.drink(user, hero, item, renderer).flatMap(afterBrew(user, item, renderer))
      }
    }

  /** Кого приговорить: восемь родов кнопками, по трое в ряд. Склянка при этом
    * ещё цела — она уйдёт, когда род назовут. */
  private def offerSentence(user: User, renderer: Renderer): Task[StateType] = {
    val races = Race.mortals.toList
    val buttons = races.zipWithIndex.map { case (race, i) =>
      Choice("SentencePick", race.toString, data = Map("race" -> race.entryName),
        color = ChoiceColor.Positive, row = Some(i / InventoryState.RacesPerRow))
    }
    val rows = (races.size + InventoryState.RacesPerRow - 1) / InventoryState.RacesPerRow
    renderer.show(user, Screen(content.text("brew.whichRace"),
      buttons :+ content.choice("ItemCard", "inventory.exit").copy(row = Some(rows), color = ChoiceColor.Negative)))
      .as(StateType.Inventory)
  }

  /** Род назван: имя ложится в баф, склянка пустеет. */
  private def sentenceRace(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      payloadField(ua, "race").flatMap(Race.withNameOption).filter(Race.mortals.contains) match {
        case None       => showItem(user, item.id, renderer)
        case Some(race) => brewWork.sentence(user, hero, item, race, renderer).flatMap(afterBrew(user, item, renderer))
      }
    }

  /** Какую травму лечить: лёгкие и средние по кнопке, тяжёлые — только перечислены. */
  private def offerCure(user: User, item: Item, hero: Hero, renderer: Renderer): Task[StateType] =
    for {
      now <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      active   = hero.activeTraumas(now)
      curable  = BrewWork.curable(hero, now)
      heavy    = BrewWork.heavy(hero, now)
      res <-
        if (active.isEmpty)
          renderer.show(user, Screen(content.text("brew.noTraumas"), Nil)) *> showItem(user, item.id, renderer)
        else if (curable.isEmpty)
          renderer.show(user, Screen(content.text("brew.onlyHeavy"), Nil)) *> showItem(user, item.id, renderer)
        else {
          val heavyLine = if (heavy.isEmpty) "" else "\n" + content.format("brew.heavyStay", "names" -> heavy.map(_.name).mkString(", "))
          val choices = curable.map(t =>
            Choice("CureTraumaPick", t.name, data = Map("trauma" -> t.name), color = ChoiceColor.Positive, row = Some(0))
          ) :+ content.choice("ItemCard", "inventory.exit").copy(row = Some(1), color = ChoiceColor.Negative)
          renderer.show(user, Screen(content.text("brew.whichTrauma") + heavyLine, choices)).as(StateType.Inventory)
        }
    } yield res

  private def cureTrauma(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      payloadField(ua, "trauma").fold(ZIO.succeed(false): Task[Boolean])(brewWork.cure(user, hero, item, _, renderer))
        .flatMap(afterBrew(user, item, renderer))
    }

  // ── Сюжетные предметы («Письмо Марисе») ────────────────────────────────────

  /** Письмо: описание и «Вскрыть» (после вскрытия — «Перечитать»); карта:
    * описание и «Отправиться по карте». Ни выбросить, ни продать. Ключ стойки,
    * когда ключей два и больше, — карточка связки ([[showKeyRing]]). */
  private def showQuestItem(user: User, item: Item, renderer: Renderer): Task[StateType] =
    keyRingOf(user, item).flatMap {
      case Some(keys) => showKeyRing(user, keys, renderer)
      case None       => showPlainQuestItem(user, item, renderer)
    }

  /** Связка — если это ключ стойки и ключей стоек при герое два и больше. */
  private def keyRingOf(user: User, item: Item): Task[Option[List[(Int, Item)]]] =
    if (!InventoryState.isRackKey(item)) ZIO.none
    else
      for {
        hero <- getHero(user)
        inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      } yield InventoryState.keyRing(inv.items.data)

  /** Связка ключей: какие ключи на кольце и кнопка на каждый — повернуть его.
    * Кнопки — по [[InventoryState.KeyRingPerRow]] в ряд, выход — рядом ниже. */
  private def showKeyRing(user: User, keys: List[(Int, Item)], renderer: Renderer): Task[StateType] = {
    val perRow = InventoryState.KeyRingPerRow
    val turns  = keys.zipWithIndex.map { case ((n, _), i) =>
      Choice("RackKeyUse", content.format("inventory.keyRing.turn", "n" -> n.toString),
        color = ChoiceColor.Positive, data = Map("n" -> n.toString), row = Some(i / perRow))
    }
    val exit = content.choice("InventoryList", "inventory.exit")
      .copy(color = ChoiceColor.Negative, row = Some((keys.size + perRow - 1) / perRow))
    renderer.show(user, Screen(InventoryState.keyRingText(content, keys.map(_._1)), turns :+ exit)).as(StateType.Inventory)
  }

  private def showPlainQuestItem(user: User, item: Item, renderer: Renderer): Task[StateType] =
    for {
      // План Барти: шаг «Ходового товара» — заново по кубу и сумке, чтобы
      // карточка сказала, что делать сейчас.
      quests <- if (item.questItem.contains(QuestItemKind.BartiPlan))
                  getHero(user).flatMap(h => HotGoodsQuest.refresh(heroDao, inventoryRepo, user.userId, h))
                else NpcQuestLog.load(heroDao, user.userId)
      // Бродячий ларец, пока голоден, — кнопка на каждое качество камней при
      // герое, по ряду на кнопку; выход — рядом ниже.
      feed   <- if (item.questItem.contains(QuestItemKind.StrayCasket) && StrayCasketQuest.hungry(quests))
                  getHero(user).flatMap(h => QuestGoods.all(inventoryRepo, artifacts, h.id)).map(strayFeedChoices)
                else ZIO.succeed(Nil)
      exit    = content.choice("InventoryList", "inventory.exit")
                  .copy(row = Some(if (feed.isEmpty) 1 else feed.size), color = ChoiceColor.Negative)
      // Что с предметом сделать — своя кнопка у каждого вида. Кнопки заданий —
      // общие с Mini App (см. QuestCard), текст карточки — тоже (см.
      // InventoryState.questText).
      use     = item.questItem match {
        // Ключ магической стойки: повернул — надетое и стойка дома меняются местами.
        case Some(key) if QuestItemKind.rackOf(key).isDefined =>
          Some(Choice("RackKeyUse", content.text("home.rack.useKey"), color = ChoiceColor.Positive, row = Some(0)))
        case Some(book) if HerbLore.isTreatise(book) =>
          Some(content.choice("ReadTreatise", "knowledge.readLabel").copy(color = ChoiceColor.Positive, row = Some(0)))
        case _ => QuestCard.choice(content, item, quests)
      }
      hero   <- getHero(user)
      text    = InventoryState.questText(content, item, quests, hero.lvl) +
                  (if (item.questItem.contains(QuestItemKind.StrayCasket) && StrayCasketQuest.hungry(quests) && feed.isEmpty)
                     "\n" + content.text("strayCasket.card.noGems") else "")
      _ <- renderer.show(user, Screen(text, use.toList ++ feed :+ exit))
    } yield StateType.Inventory

  // ── «Бродячий ларец»: кормление с карточки ─────────────────────────────────

  /** Кнопки кормления: по одной на каждое качество камней при герое — в сумке и
    * в своём ларце, — с числом. Нажатая скармливает один камень этого качества. */
  private def strayFeedChoices(goods: List[QuestGoods.Good]): List[Choice] = {
    val labels = content.list("strayCasket.feed.grades")
    StrayCasketQuest.grades(goods).zipWithIndex.flatMap { case ((grade, n), i) =>
      labels.lift(grade - 1).map(l => Choice("StrayFeed", Choice.fit(l.replace("{n}", n.toString)),
        data = Map("grade" -> grade.toString), color = ChoiceColor.Positive, row = Some(i)))
    }
  }

  /** Скормить ларцу камень выбранного качества: строка о съеденном, каждое
    * второе — воспоминание прежнего хозяина, десятое — ларец сыт. Дальше та же
    * карточка: покормить ещё. */
  private def feedStray(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      val grade = payloadField(ua, "grade").flatMap(_.toIntOption)
      for {
        quests <- NpcQuestLog.load(heroDao, user.userId)
        goods  <- QuestGoods.all(inventoryRepo, artifacts, hero.id)
        chosen  = grade.flatMap(StrayCasketQuest.pick(goods, _))
        after  <- (item.questItem, chosen) match {
                    case (Some(QuestItemKind.StrayCasket), Some(good)) if StrayCasketQuest.hungry(quests) =>
                      StrayCasketQuest.feed(heroDao, inventoryRepo, artifacts, user.userId, hero.id, good)
                        .map(_.map(good -> _))
                    case _ => ZIO.none
                  }
        res    <- after match {
                    case None => showItem(user, item.id, renderer)
                    case Some((good, next)) =>
                      val n      = StrayCasketQuest.fed(next)
                      val eaten  = content.format("strayCasket.feed.eaten", "gem" -> good.item.name,
                                     "fed" -> n.toString, "goal" -> StrayCasketQuest.GemsGoal.toString)
                      val memory = StrayCasketQuest.memoryAfter(n).flatMap(i => content.list("strayCasket.memories").lift(i - 1))
                      val sated  = Option.when(StrayCasketQuest.leads(next))(content.text("strayCasket.sated"))
                      renderer.show(user, Screen(eaten, Nil)) *>
                        ZIO.foreachDiscard(memory.toList ++ sated.toList)(t => renderer.show(user, Screen(t, Nil))) *>
                        showItem(user, item.id, renderer)
                  }
      } yield res
    }

  /** Кнопка задания с карточки сюжетного предмета — общим с Mini App
    * [[QuestCard]]: где герой, говорит `return_state` («Персонаж» открыт
    * оттуда). Шаг не тот — снова карточка; отказ — сказать и на карточку (у
    * карт и купонов — к списку); вопрос — показать; дело здесь — сделать и к
    * списку; сцена — подготовить и уйти в неё. */
  private def questGo(user: User, renderer: Renderer, id: String): Task[StateType] =
    for {
      hero   <- getHero(user)
      from   <- heroDao.readReturnState(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- questCard.go(id, hero, quests, from) match {
                  case QuestCard.Go.Stale => backToCard(user, renderer)
                  case QuestCard.Go.Refused(text, toList) =>
                    renderer.show(user, Screen(text, Nil)) *>
                      (if (toList) showList(user, renderer) else backToCard(user, renderer))
                  case QuestCard.Go.Ask(screen)        => renderer.show(user, screen).as(StateType.Inventory)
                  case QuestCard.Go.Here(run)          => run(user, renderer) *> showList(user, renderer)
                  case QuestCard.Go.Enter(scene, prep) => prep(user, renderer).as(scene)
                }
    } yield res

  /** Повернуть ключ стойки: магия меняет надетое местами с тем, что висит на
    * стойке дома, — где угодно, кроме боя (из боя в сумку не попасть). Со
    * связки номер ключа приходит в кнопке — и такой ключ должен быть при герое;
    * с карточки одного ключа — это сам ключ. */
  private def useRackKey(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      val chosen = payloadField(ua, "n").flatMap(_.toIntOption) match {
        case Some(n) =>
          inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
            .map(inv => Option.when(InventoryState.rackKeys(inv.items.data).exists(_._1 == n))(n))
        case None => ZIO.succeed(item.questItem.flatMap(QuestItemKind.rackOf))
      }
      chosen.flatMap {
        case None => showItem(user, item.id, renderer)
        case Some(n) =>
          Racks(heroDao, inventoryRepo, content).swap(user, n, renderer).flatMap {
            case Left(why) => renderer.show(user, Screen(why, Nil)) *> showItem(user, item.id, renderer)
            case Right(_)  =>
              renderer.show(user, Screen(content.format("home.rack.keyTurned", "n" -> n.toString), Nil)) *> showList(user, renderer)
          }
      }
    }

  /** Трактат: бросок интеллект ÷ 2 процентов (+2 % за каждую прошлую неудачу);
    * осилил — знание и книга уходит, нет — час на переварить, потом снова.
    * Общее с Mini App — [[HerbLore.readTreatise]]; потом — к списку. */
  private def readTreatise(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      scene <- readScene(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      _     <- ZIO.foreachDiscard(scene.selectedId.flatMap(id => inv.items.data.find(_.id == id)))(book =>
                 HerbLore.readTreatise(heroDao, inventoryRepo, content, user, hero, book, renderer))
      res   <- showList(user, renderer)
    } yield res

  // ── Действия с выбранным предметом ─────────────────────────────────────────

  private def equipSelected(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      res <- scene.selectedId match {
        case None => showList(user, renderer)
        case Some(id) => equipById(user, id, renderer, pairSlot = None)
      }
    } yield res

  // Игрок выбрал, какое из двух надетых колец снять или вместо какого оружия
  // взять новое (кнопки экранов выбора).
  private def equipChosenSlot(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    (payloadField(ua, "id").flatMap(_.toLongOption), payloadField(ua, "slot").flatMap(_.toIntOption)) match {
      case (Some(id), Some(slot)) => equipById(user, id, renderer, pairSlot = Some(slot))
      case _                      => showList(user, renderer)
    }

  /** Экран выбора кольца или руки: что из двух надетых снять ради новой вещи
    * (общий с «Забрать и надеть», см. [[SlotChoice]]). */
  private def slotChoiceScreen(item: Item, eq: Equipment, itemId: Long): Screen =
    SlotChoice.screen(content, item, eq,
      action = if (item.itemType == ItemType.Ring) "EquipRing" else "EquipHand",
      data   = Map("id" -> itemId.toString),
      back   = content.choice("InventoryList", "inventory.exit"))

  private def equipById(user: User, itemId: Long, renderer: Renderer, pairSlot: Option[Int]): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      // true — остаёмся на показанном экране (выбор кольца ждёт нажатия и
      // перерисовывать поверх него список нельзя).
      keepScreen <- inv.items.data.find(_.id == itemId) match {
        case None => ZIO.succeed(false)
        case Some(item) =>
          if (!ItemType.equippable.contains(item.itemType))
            renderer.show(user, Screen(content.text("inventory.notEquippable"), Nil)).as(false)
          // Кнопка со старой карточки фляги: в слот её больше не надевают.
          else if (item.itemType == ItemType.Flask)
            renderer.show(user, Screen(content.text("inventory.flaskToBelt"), Nil)).as(false)
          else if (item.lvl > hero.lvl)
            renderer.show(user, Screen(
              content.format("inventory.tooHighLevel",
                "required" -> item.lvl.toString,
                "current"  -> hero.lvl.toString), Nil)).as(false)
          // Оба слота колец заняты, или обе руки заняты одноручным или щитом, а
          // берут ещё одно, и слот ещё не выбран — спрашиваем, что снять.
          else if (pairSlot.isEmpty && SlotChoice.needed(hero.equipment, item))
            renderer.show(user, slotChoiceScreen(item, hero.equipment, itemId)).as(true)
          else
            outfit.wear(user, hero, item, pairSlot, renderer) { done =>
              content.format("inventory.equipped",
                "name" -> item.name,
                "old"  -> InventoryState.removedNames(done.removed)) +
                BeltContents.movedLine(content, done.beltMoved, done.spilled)
            }.as(false)
      }
      res <- if (keepScreen) ZIO.succeed(StateType.Inventory) else showList(user, renderer)
    } yield res

  // ── Краденый кошелёк ───────────────────────────────────────────────────────

  /** Вернуть кошелёк хозяину: гильдия берётся его найти и благодарит так же,
    * как за реликвию того же уровня. */
  private def returnPurse(user: User, renderer: Renderer): Task[StateType] =
    withPurse(user, renderer) { (item, hero) =>
      val gained = TrophyKind.reputationFor(TrophyKind.Relic.coef, item.lvl)
      val total  = hero.guildReputation + gained
      for {
        _ <- inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString))
        _ <- heroDao.updateGuildReputation(user.userId, total)
        _ <- NpcQuestLog.onReputation(heroDao, user.userId, gained)
        _ <- renderer.show(user, Screen(content.format("purse.returned",
               "reputation" -> gained.toString, "total" -> total.toString), Nil))
        r <- backToCard(user, renderer)
      } yield r
    }

  /** Развязать самому: горсть серебра, а изредка — то, чего воры и сами не
    * поняли. */
  private def openPurse(user: User, renderer: Renderer): Task[StateType] =
    withPurse(user, renderer) { (item, hero) =>
      for {
        roll   <- Random.nextIntBetween(1, InventoryState.PurseJackpotIn + 1)
        jackpot = roll == 1
        amount <- if (jackpot) ZIO.succeed(InventoryState.PurseJackpot)
                  else Random.nextLongBetween(InventoryState.PurseMinSilver, InventoryState.PurseMaxSilver + 1L)
        _ <- inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString))
        _ <- heroDao.updateSilver(user.userId, hero.silver + amount)
        _ <- ZIO.when(jackpot)(renderer.show(user, Screen(content.text("purse.jackpot"), Nil)))
        _ <- renderer.show(user, Screen(content.format("purse.opened", "silver" -> amount.toString), Nil))
        r <- backToCard(user, renderer)
      } yield r
    }

  private def withPurse(user: User, renderer: Renderer)(f: (Item, Hero) => Task[StateType]): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      if (item.material.contains(MaterialKind.StolenPurse)) f(item, hero)
      else showList(user, renderer)
    }

  private def dropSelected(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      if (item.isStoryGear) backToCard(user, renderer, Some(content.text("inventory.storyNoDrop")))
      else inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
        backToCard(user, renderer, Some(content.format("inventory.dropped", "name" -> item.name)))
    }

  /** «🗑 Выбросить все N» на карточке стопки. */
  private def dropAllChoice(count: Int): Option[Choice] =
    Option.when(count > 1)(Choice("DropAll", content.format("inventory.dropAll", "n" -> count.toString),
      color = ChoiceColor.Negative, row = Some(1)))

  /** Выбросить стопку целиком — сперва спросить: вернуть её будет нельзя.
    * Сколько обещали, едет в кнопке: больше этого не выбросим. */
  private def offerDropAll(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res   <- scene.selectedId.flatMap(id => inv.items.data.find(_.id == id)) match {
                 case None => showList(user, renderer)
                 case Some(item) =>
                   val n = stackOf(inv.items.data, item).size
                   renderer.show(user, Screen(
                     content.format("inventory.dropAllConfirm", "name" -> item.name, "n" -> n.toString),
                     List(
                       Choice("DropAllYes", content.text("inventory.dropAllYes"), color = ChoiceColor.Negative,
                         data = Map("n" -> n.toString), row = Some(0)),
                       Choice("ItemCard", content.text("inventory.dropAllNo"), row = Some(1))
                     ))).as(StateType.Inventory)
               }
    } yield res

  private def dropAll(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      limit  = ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption)
                 .flatMap(_.get("n")).flatMap(_.toIntOption).getOrElse(1)
      res   <- scene.selectedId.flatMap(id => inv.items.data.find(_.id == id)) match {
                 case None => showList(user, renderer)
                 case Some(item) if item.isStoryGear => backToCard(user, renderer, Some(content.text("inventory.storyNoDrop")))
                 case Some(item) =>
                   val gone = stackOf(inv.items.data, item).takeRight(limit)
                   inventoryRepo.removeItems(gone.map(_.id).toSet, hero.id).mapError(e => new Throwable(e.toString)) *>
                     backToCard(user, renderer, Some(content.format("inventory.droppedAll",
                       "name" -> item.name, "n" -> gone.size.toString)))
               }
    } yield res

  /** Назад на карточку выбранной вещи. Потратили последнюю вещь стопки (или
    * вещь была одна) — карточки больше нет, и [[showItem]] уводит в список.
    * `note` — что только что случилось: строкой над карточкой или списком. */
  private def backToCard(user: User, renderer: Renderer, note: Option[String] = None): Task[StateType] =
    readScene(user).flatMap(_.selectedId.fold(showList(user, renderer, note))(showItem(user, _, renderer, note)))

  /** Вещи той же стопки, в порядке сумки; у вещи без стопки — она одна. */
  private def stackOf(items: List[Item], item: Item): List[Item] =
    ItemStack.key(item) match {
      case Some(k) => items.filter(i => ItemStack.key(i).contains(k))
      case None    => List(item)
    }

  // ── Объединение половинок карты ────────────────────────────────────────────

  private def combineSelected(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      res <- scene.selectedId match {
        case None     => showList(user, renderer)
        case Some(id) => combineById(user, id, renderer)
      }
    } yield res

  // Две половинки одной зоны → одна целая карта этой зоны (уровень — больший из двух).
  private def combineById(user: User, halfId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items  = inv.items.data
      res <- items.find(_.id == halfId) match {
        case Some(first) if first.itemType == ItemType.TreasureMapHalf =>
          val firstZone = zoneOf(first)
          val second = items.find(i =>
            i.id != first.id && i.itemType == ItemType.TreasureMapHalf && zoneOf(i) == firstZone)
          (firstZone, second) match {
            case (Some(zone), Some(other)) =>
              val full = TreasureMapGenerator.full(zone)
              for {
                _         <- inventoryRepo.removeItems(Set(first.id, other.id), hero.id).mapError(e => new Throwable(e.toString))
                persisted <- itemRepository.persist(hero.id, full)
                _         <- inventoryRepo.addItem(hero.id, persisted).mapError(e => new Throwable(e.toString))
                _         <- renderer.show(user, Screen(content.format("inventory.combined", "name" -> full.name), Nil))
                back      <- showList(user, renderer)
              } yield back
            case _ =>
              val name = firstZone.map(_.treasureName).getOrElse("")
              renderer.show(user, Screen(content.format("inventory.combineNeedSecond", "name" -> name), Nil)) *>
                showList(user, renderer)
          }
        case _ => showList(user, renderer)
      }
    } yield res

  private def combineRuneSelected(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      res <- scene.selectedId match {
        case None     => showList(user, renderer)
        case Some(id) => combineRuneById(user, id, renderer)
      }
    } yield res

  /** Пять малых рун одного узора → одна большая. Малые места не занимают, а
    * большая занимает слот — в полную сумку её не сложить. */
  private def combineRuneById(user: User, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items  = inv.items.data
      res <- items.find(i => i.id == itemId && i.isSmallRune).flatMap(i => i.runeStone.map(i -> _)) match {
        case None => showList(user, renderer)
        case Some((first, details)) =>
          // Складываем последние пять: показанная первой остаётся на карточке,
          // если малых рун этого узора больше пяти.
          val pieces = items.filter(_.runeStone.exists(d =>
            d.size == RuneStoneSize.Small && d.runeKey == details.runeKey)).takeRight(RuneStone.PiecesPerBig)
          val rune   = Rune.byKey(details.runeKey)
          (pieces.sizeIs == RuneStone.PiecesPerBig, rune) match {
            case (false, _) =>
              renderer.show(user, Screen(content.format("inventory.runeNeedMore",
                "name" -> first.name, "have" -> pieces.size.toString, "need" -> RuneStone.PiecesPerBig.toString), Nil)) *>
                showItem(user, itemId, renderer)
            case (_, None) => showList(user, renderer)
            case (true, Some(_)) if inv.freeSlots <= 0 =>
              renderer.show(user, Screen(content.text("common.inventoryFull"), Nil)) *> showItem(user, itemId, renderer)
            case (true, Some(r)) =>
              val big = RuneStone.item(r, RuneStoneSize.Big)
              for {
                _         <- inventoryRepo.removeItems(pieces.map(_.id).toSet, hero.id).mapError(e => new Throwable(e.toString))
                persisted <- itemRepository.persist(hero.id, big)
                _         <- inventoryRepo.addItem(hero.id, persisted).mapError(e => new Throwable(e.toString))
                _         <- renderer.show(user, Screen(content.format("inventory.runeFolded", "name" -> big.name), Nil))
                back      <- backToCard(user, renderer)
              } yield back
          }
      }
    } yield res

  // ── Резец рунописца ────────────────────────────────────────────────────────

  /** Вещи, чей узор резец возьмёт (см. [[Chisel]]): сперва надетые, потом из
    * сумки. Второе в паре — надета ли вещь. */
  private def chiselTargets(hero: Hero, bag: List[Item]): List[(Item, Boolean)] =
    hero.equipment.allItems.filter(Chisel.fits).distinctBy(_.id).map(_ -> true) ++
      bag.filter(Chisel.fits).map(_ -> false)

  /** Список вещей под резец — по [[InventoryState.ChiselPage]] на страницу.
    * Надетые вещи с «Тайником», которые сейчас не переписать, — под списком
    * строкой о том, сколько мест освободить. */
  private def offerChisel(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      all   = chiselTargets(hero, inv.items.data)
      last  = ((all.size - 1) / ChiselPage).max(0)
      page  = payloadField(ua, "p").flatMap(_.toIntOption).getOrElse(0).max(0).min(last)
      back  = Choice("ItemCard", content.text("inventory.chisel.back"), row = Some(ChiselPage + 1))
      res  <-
        if (all.isEmpty)
          renderer.show(user, Screen(content.text("inventory.chisel.none"), List(back))).as(StateType.Inventory)
        else {
          val picks = all.slice(page * ChiselPage, (page + 1) * ChiselPage).zipWithIndex.map { case ((item, worn), i) =>
            Choice("ChiselPick",
              Choice.fit(content.format(if (worn) "inventory.chisel.worn" else "inventory.chisel.bag",
                "name" -> item.name, "pattern" -> gemWork.chiselPattern(item))),
              data = Map("item" -> item.id.toString), row = Some(i))
          }
          val nav = List(
            Option.when(page > 0)(Choice("ChiselUse", "◀", data = Map("p" -> (page - 1).toString), row = Some(ChiselPage))),
            Option.when(page < last)(Choice("ChiselUse", "▶", data = Map("p" -> (page + 1).toString), row = Some(ChiselPage)))
          ).flatten
          val notes = all.collect { case (item, worn) if gemWork.chiselShortage(hero, inv, item, worn) > 0L =>
                        gemWork.chiselStashLine(item, gemWork.chiselShortage(hero, inv, item, worn)) }
          renderer.show(user, Screen((content.text("inventory.chisel.which") :: notes).mkString("\n"),
            picks ++ nav :+ back)).as(StateType.Inventory)
        }
    } yield res

  /** Выбранная под резец вещь — там же, где была, и резец ещё в сумке. Нет
    * резца — к списку сумки; нет вещи — сказать и снова дать выбрать. */
  private def withChiselTarget(user: User, ua: UserAction, renderer: Renderer)(
      f: (Hero, Inventory, Item, Boolean, Item) => Task[StateType]): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      id    = payloadField(ua, "item").flatMap(_.toLongOption)
      res  <- (inv.items.data.find(_.material.contains(MaterialKind.RuneChisel)),
               id.flatMap(i => chiselTargets(hero, inv.items.data).find(_._1.id == i))) match {
                case (None, _) => showList(user, renderer)
                case (Some(_), None) =>
                  renderer.show(user, Screen(content.text("inventory.chisel.gone"), Nil)) *> offerChisel(user, ua, renderer)
                case (Some(chisel), Some((item, worn))) => f(hero, inv, item, worn, chisel)
              }
    } yield res

  private def confirmChisel(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withChiselTarget(user, ua, renderer) { (hero, inv, item, worn, _) =>
      val shortage = gemWork.chiselShortage(hero, inv, item, worn)
      if (shortage > 0L)
        renderer.show(user, Screen(gemWork.chiselStashLine(item, shortage), Nil)) *> offerChisel(user, ua, renderer)
      else renderer.show(user, Screen(
        content.format("inventory.chisel.confirm", "name" -> item.name, "pattern" -> gemWork.chiselPattern(item)),
        List(
          Choice("ChiselDo", content.text("inventory.chisel.yes"),
            data = Map("item" -> item.id.toString), color = ChoiceColor.Positive, row = Some(0)),
          Choice("ChiselUse", content.text("inventory.chisel.back"), row = Some(1))
        ))).as(StateType.Inventory)
    }

  /** Что сделает резец и что будет с «Тайником» — у [[GemWork]], общего с Mini App. */
  private def doChisel(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withChiselTarget(user, ua, renderer) { (hero, inv, item, worn, chisel) =>
      gemWork.chisel(user, hero, inv, item, worn, chisel, renderer).flatMap {
        case true  => showList(user, renderer)
        case false => offerChisel(user, ua, renderer)
      }
    }

  // ── Ломка камней ───────────────────────────────────────────────────────────

  /** Первый шаг: если камень в вещи один — сразу спрашиваем подтверждение, если
    * несколько — сначала даём выбрать, какой именно ломать. */
  private def offerBreak(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, _) =>
      GemBreaking.socketed(item) match {
        case Nil             => showList(user, renderer)
        case (idx, _) :: Nil => breakConfirmScreen(user, item, idx, renderer)
        case gems =>
          val choices = gems.map { case (idx, gem) =>
            Choice(
              "BreakGemPick",
              content.format("inventory.breakGemOption", "gem" -> gem.displayName),
              data  = Map("slot" -> idx.toString),
              color = ChoiceColor.Negative,
              row   = Some(0))
          } :+ content.choice("ItemCard", "inventory.exit").copy(row = Some(1))
          renderer.show(user, Screen(
            content.format("inventory.breakGemWhich", "item" -> item.name), choices)).as(StateType.Inventory)
      }
    }

  private def confirmBreak(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    payloadField(ua, "slot").flatMap(_.toIntOption) match {
      case None      => showList(user, renderer)
      case Some(idx) => withSelected(user, renderer)((item, _) => breakConfirmScreen(user, item, idx, renderer))
    }

  /** Красное подтверждение: сколько пыли выйдет и что камень пропадёт навсегда. */
  private def breakConfirmScreen(user: User, item: Item, idx: Int, renderer: Renderer): Task[StateType] =
    item.sockets.lift(idx).flatten match {
      case None => showList(user, renderer)
      case Some(gem) =>
        renderer.show(user, Screen(
          content.format("inventory.breakGemConfirm",
            "gem"   -> gem.displayName,
            "dust"  -> gem.dust.displayName,
            "count" -> gem.dustYield.toString),
          List(
            Choice("BreakGemDo", content.text("inventory.breakGemYes"),
              data = Map("slot" -> idx.toString), color = ChoiceColor.Negative, row = Some(0)),
            Choice("ItemCard", content.text("inventory.breakGemNo"), row = Some(1))
          ))).as(StateType.Inventory)
    }

  private def doBreak(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    payloadField(ua, "slot").flatMap(_.toIntOption) match {
      case None => showList(user, renderer)
      case Some(idx) =>
        withSelected(user, renderer) { (item, hero) =>
          GemBreaking.breakSocket(item, idx) match {
            case None => showList(user, renderer)
            case Some((emptied, gem)) =>
              grantDust(user, hero, gem, renderer,
                inventoryRepo.updateItem(hero.id, emptied).mapError(e => new Throwable(e.toString)),
                showList(user, renderer), backToCard(user, renderer))
          }
        }
    }

  /** Камень, лежащий в сумке отдельным предметом: крошим его целиком. */
  private def offerCrush(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, _) =>
      item.gem match {
        case None => showList(user, renderer)
        case Some(gem) =>
          renderer.show(user, Screen(
            content.format("inventory.crushGemConfirm",
              "gem"   -> gem.displayName,
              "dust"  -> gem.dust.displayName,
              "count" -> gem.dustYield.toString),
            List(
              Choice("CrushGemDo", content.text("inventory.breakGemYes"), color = ChoiceColor.Negative, row = Some(0)),
              Choice("ItemCard", content.text("inventory.breakGemNo"), row = Some(1))
            ))).as(StateType.Inventory)
      }
    }

  /** Правила и слова — общие с приложением ([[GemWork.crush]]). */
  private def doCrush(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      gemWork.crush(user, hero, item, renderer).flatMap(found =>
        if (found) backToCard(user, renderer) else showList(user, renderer))
    }

  /** Выдаёт пыль за камень из гнезда вещи в сумке. Пыль места не занимает, но
    * одного вида её не больше предела ([[GemWork.dustFull]]): тогда камень цел —
    * `spend` (выбить его из гнезда) не случается. `andThen` — куда уйти после (в
    * список), `refused` — куда, если не вышло (на ту же карточку). */
  private def grantDust(user: User, hero: Hero, gem: Gem, renderer: Renderer, spend: Task[Unit],
                        andThen: => Task[StateType], refused: => Task[StateType]): Task[StateType] =
    for {
      inv <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res <-
        if (gemWork.dustFull(inv, gem)) renderer.show(user, Screen(gemWork.dustFullLine(gem), Nil)) *> refused
        else
          spend *> gemWork.grantDust(hero, gem) *>
            renderer.show(user, Screen(content.format("inventory.breakGemDone",
              "gem" -> gem.displayName, "count" -> gem.dustYield.toString, "dust" -> gem.dust.displayName), Nil)) *>
            andThen
    } yield res

  /** Общая обвязка «взять выбранный в сцене предмет и героя». У стопки это
    * ПОСЛЕДНЯЯ её вещь, хотя на карточке — первая (см. [[showItem]]): так
    * показанная остаётся на месте, пока в стопке есть что-то ещё. */
  private def withSelected(user: User, renderer: Renderer)(
      f: (Item, Hero) => Task[StateType]): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res <- scene.selectedId.flatMap(id => inv.items.data.find(_.id == id)) match {
        case None       => showList(user, renderer)
        case Some(item) => f(stackOf(inv.items.data, item).lastOption.getOrElse(item), hero)
      }
    } yield res

  // ── Опознать цветок ────────────────────────────────────────────────────────

  /** Странный или необычный цветок: знаешь ранг — он становится травой и
    * показывается её карточка; не знаешь — рассыпается, а бросок на догадку идёт
    * тот же, что при сборе. */
  private def identifyFlower(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      item.material.filter(MaterialKind.isUnknownFlower) match {
        case None => showItem(user, item.id, renderer)
        case Some(kind) =>
          for {
            now <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
            res <- HerbLore.identify(heroDao, inventoryRepo, content, user, hero, item, kind, now)
            _   <- renderer.show(user, Screen(res.lines.mkString("\n"), Nil))
            s   <- res.herb.fold(showList(user, renderer))(h => showItem(user, h.id, renderer))
          } yield s
      }
    }

  /** «Опознать все»: те же броски, что у «Опознать», по всем таким цветкам в
    * сумке подряд. Знаешь ранг — все становятся травами; не знаешь — рассыпаются
    * по одному, пока не придёт догадка или пока не кончатся. Цветы остались —
    * снова их карточка: теперь их можно опознать; не осталось — список. */
  private def identifyAllFlowers(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res   <- scene.selectedId.flatMap(id => inv.items.data.find(_.id == id)) match {
                 case None => showList(user, renderer)
                 case Some(item) =>
                   item.material.filter(MaterialKind.isUnknownFlower) match {
                     case None => showItem(user, item.id, renderer)
                     case Some(kind) =>
                       for {
                         now  <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
                         // С конца стопки: её место в списке держит первый цветок (см. ItemStack.grouped).
                         done <- HerbLore.identifyAll(heroDao, inventoryRepo, content, user, hero,
                                   stackOf(inv.items.data, item).reverse, kind, now)
                         _    <- renderer.show(user, Screen(done.lines.mkString("\n"), Nil))
                         s    <- done.left.headOption.fold(showList(user, renderer))(f => showItem(user, f.id, renderer))
                       } yield s
                   }
               }
    } yield res

  // ── Пыль на оружие ─────────────────────────────────────────────────────────

  /** Посыпает надетое оружие выбранной пылью — общим с Mini App
    * [[GemWork.sprinkle]]: горсть тратится в любом случае. Не пыль — к списку. */
  private def sprinkleDust(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      gemWork.sprinkle(user, hero, item, renderer).flatMap(found =>
        if (found) backToCard(user, renderer) else showList(user, renderer))
    }

  /** Положить расходник на пояс: что и как — у [[BeltWork]], общего с Mini App;
    * нет места — спрашиваем, что снять ([[beltSwapScreen]]). */
  private def putOnBelt(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer)((item, hero) =>
      beltWork.put(user, hero, item, renderer).flatMap(afterBelt(user, renderer, showList(user, renderer))))

  private def afterBelt(user: User, renderer: Renderer, notForBelt: => Task[StateType]): BeltWork.Put => Task[StateType] = {
    case BeltWork.Put.Done          => backToCard(user, renderer)
    case BeltWork.Put.NotForBelt    => notForBelt
    case BeltWork.Put.Full(slot, b) => renderer.show(user, beltSwapScreen(slot, b)).as(StateType.Inventory)
  }

  /** Пояс полон: по кнопке на ячейку — что снять ради новой вещи, и «Назад».
    * Пустая ячейка помнит, чем была полна, — так и подписана. */
  private def beltSwapScreen(slot: BeltSlot, belt: ItemDetails.Belt): Screen =
    Screen(content.format("inventory.beltSwapChoice", "name" -> slot.name),
      belt.slots.zipWithIndex.map { case (s, i) =>
        Choice(
          id    = "BeltSwap",
          label = ItemMenu.truncate(content.format(
                    if (s.uses > 0) "inventory.beltSwapSlot" else "inventory.beltSwapSlotEmpty", "name" -> s.name)),
          data  = Map("slot" -> i.toString),
          row   = Some(i))
      } :+ content.choice("BeltSwapBack", "common.back").copy(row = Some(belt.slots.size)))

  /** Положить выбранную вещь на пояс вместо ячейки `slot` ([[BeltWork.swap]]). */
  private def swapOnBelt(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer)((item, hero) =>
      beltWork.swap(user, hero, item, payloadField(ua, "slot").flatMap(_.toIntOption), renderer)
        .flatMap(afterBelt(user, renderer, backToCard(user, renderer))))

  // ── Вставка камня: уходим на экран Socketing с id выбранного камня ─────────

  private def startSocketing(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      gem    = scene.selectedId.flatMap(id => inv.items.data.find(i => i.id == id && i.gem.isDefined))
      res <- gem match {
        case None => showList(user, renderer)
        case Some(shown) =>
          // Вставляем последний камень стопки, а вернёмся на её карточку: если
          // камни ещё остались, вставлять их можно дальше, не листая список.
          val toInsert = stackOf(inv.items.data, shown).lastOption.getOrElse(shown)
          writeScene(user, scene.copy(cardOnReturn = Some(true))) *>
            UiScene.write(heroDao, user.userId, UiScene.Socketing, SocketingState.Scene(toInsert.id))
              .as(StateType.Socketing)
      }
    } yield res

  // ── Fallback: динамические id вида ItemAction_<id> ─────────────────────────

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(ItemActionPrefix) =>
        a.drop(ItemActionPrefix.length).toLongOption.fold(showList(user, renderer))(showItem(user, _, renderer))
      case _ => showList(user, renderer)
    }

  // ── Хелперы ─────────────────────────────────────────────────────────────────

  private def emptyScreen(hero: Hero, note: Option[String]): Screen = {
    val base = content.screen("inventory.empty")
    Screen(StackMove.withNote(note, s"📦 Инвентарь пуст | 🪙 ${hero.silver} | 🟡 ${hero.doubloons}"), base.choices)
  }

  private def navRow(page: Int, totalPages: Int, prevId: String, nextId: String, backId: String): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice(backId, "inventory.exit").copy(row = Some(row), color = ChoiceColor.Negative)),
      Option.when(page > 0)(Choice(prevId, content.text("common.prev"), row = Some(row))),
      Option.when(page < totalPages - 1)(Choice(nextId, content.text("common.next"), row = Some(row)))
    ).flatten
  }

  /** Приписка «в сумке: N шт» — только когда таких вещей больше одной. */
  private def stackLine(count: Int): String =
    if (count > 1) "\n\n" + content.format("inventory.stackCount", "count" -> count.toString) else ""

  private def itemDetail(item: Item, hero: Hero, silver: Long): String =
    s"🪙 $silver\n\n${itemText(item, hero.equipment, Some(hero))}"

  /** Закрыть карточку, не трогая страницу списка. */
  private def backToList(user: User): Task[Unit] =
    readScene(user).flatMap(scene => writeScene(user, scene.copy(selectedId = None)))

  private def readScene(user: User): Task[InventoryScene] =
    UiScene.read(heroDao, user.userId, UiScene.Inventory, InventoryScene())

  private def writeScene(user: User, scene: InventoryScene): Task[Unit] =
    UiScene.write(heroDao, user.userId, UiScene.Inventory, scene)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  // Произвольное поле payload кнопки (напр. выбранный слот кольца).
  private def payloadField(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object InventoryState {

  /** Отвар — со своим видом: экран отвара получает вид из сопоставления, а не
    * достаёт его заново из предмета. */
  private object Brewed {
    def unapply(item: Item): Option[BrewKind] = item.brew
  }

  /** Текст карточки вещи, как в сумке бота, — без кнопок и без строки с
    * серебром: у сюжетного предмета — описание, у отвара — что он делает и где
    * работает, у прочего — характеристики и сравнение с надетым. Им же
    * описывает вещи Mini App. `quests` — ход заданий героя: вскрыто ли письмо
    * Марисы, сколько съел бродячий ларец. */
  def cardText(content: SceneContent, item: Item, hero: Hero, quests: NpcQuests): String = item match {
    case _ if item.isQuestItem => questText(content, item, quests, hero.lvl)
    case _ if item.isBundle    => bundleText(content, item, hero)
    case Brewed(kind)          => brewText(content, item, kind)
    case _                     => itemText(item, hero.equipment, Some(hero))
  }

  // ── Связка ключей ──────────────────────────────────────────────────────────

  /** Связка: с этого числа ключей стоек при герое они в сумке одной строкой, а
    * в её карточке — кнопка на каждый. В базе ключи остаются отдельными вещами:
    * связка — только то, как их показывают. */
  val KeyRingMin: Int = 2

  /** Кнопок ключей в ряд на карточке связки. */
  val KeyRingPerRow: Int = 3

  /** Ключ ли это от стойки дома. */
  def isRackKey(item: Item): Boolean = item.questItem.flatMap(QuestItemKind.rackOf).isDefined

  /** Ключи стоек среди вещей — с номерами, по порядку номеров. */
  def rackKeys(items: List[Item]): List[(Int, Item)] =
    items.flatMap(i => i.questItem.flatMap(QuestItemKind.rackOf).map(_ -> i)).sortBy(_._1)

  /** Связка — ключи стоек, если их хотя бы [[KeyRingMin]]. */
  def keyRing(items: List[Item]): Option[List[(Int, Item)]] =
    Some(rackKeys(items)).filter(_.sizeIs >= KeyRingMin)

  /** Сумка строками, как её показывают: одинаковое — стопкой
    * ([[ItemStack.grouped]]), ключи стоек — связкой: она встаёт туда, где лежит
    * первый из них, и её счётчик — сколько ключей на кольце. */
  def listed(items: List[Item]): List[(Item, Int)] =
    keyRing(items) match {
      case None => ItemStack.grouped(items)
      case Some(keys) =>
        val ids   = keys.map(_._2.id).toSet
        val first = items.find(i => ids.contains(i.id)).map(_.id)
        ItemStack.grouped(items.filter(i => !ids.contains(i.id) || first.contains(i.id)))
          .map { case (it, n) => if (first.contains(it.id)) (it, keys.size) else (it, n) }
    }

  /** Строка связки в сумке: «★ Связка ключей (3)». */
  def keyRingLabel(content: SceneContent, n: Int): String =
    s"${Item.QuestMark} " + content.format("inventory.keyRing.button", "n" -> n.toString)

  /** Карточка связки — какие ключи на кольце и что они делают; ею же связку
    * описывает Mini App. */
  def keyRingText(content: SceneContent, numbers: List[Int]): String =
    s"${Item.QuestMark} ${content.text("inventory.keyRing.name")}\n\n" +
      content.format("inventory.keyRing.text", "keys" -> numbers.map(n => s"№$n").mkString(", "))

  /** Стопка: значок и сколько внутри, а под ними — карточка того, что в ней
    * лежит: вещи в стопке одинаковые, и одной карточки на всех хватает. */
  def bundleText(content: SceneContent, item: Item, hero: Hero): String = {
    val inside = Bundling.contents(item)
    val head   = s"${item.displayTitle}\n${content.format("inventory.bundle.about", "n" -> inside.size.toString)}"
    inside.headOption.fold(head) {
      case first @ Brewed(kind) => s"$head\n\n${brewText(content, first, kind)}"
      case first                => s"$head\n\n${itemText(first, hero.equipment, Some(hero))}"
    }
  }

  /** Сюжетный предмет: описание, у письма, карты, бродячего ларца и улики из
    * бочки — ещё и сложность (у улики — по уровню героя: банда его уровня); к
    * невскрытому письму — подсказка, у кого спросить; у ларца и улики — что с
    * ними сейчас. */
  def questText(content: SceneContent, item: Item, quests: NpcQuests, heroLvl: Long): String = {
    def difficulty(n: Int) = content.format("marisa.difficulty", "difficulty" -> Difficulty.render(n))
    val letterOpened = quests.of(NpcQuest.Marisa).bonus
    item.questItem match {
      case Some(QuestItemKind.MarisaLetter) =>
        s"${item.displayTitle}\n${difficulty(MarisaQuest.LetterDifficulty)}\n\n${QuestItemKind.MarisaLetter.description}" +
          (if (letterOpened) "" else "\n\n" + content.text("marisa.letterHint"))
      case Some(QuestItemKind.MurlocVillageMap) =>
        s"${item.displayTitle}\n${difficulty(MurlocQuest.MapDifficulty)}\n\n${QuestItemKind.MurlocVillageMap.description}"
      case Some(QuestItemKind.StrayCasket) =>
        val now = StrayCasketQuest.cardLine(content, quests)
        s"${item.displayTitle}\n${difficulty(StrayCasketQuest.Level)}\n\n${QuestItemKind.StrayCasket.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.HedgeBranch) =>
        s"${item.displayTitle}\n${difficulty(HedgeSongQuest.Level)}\n\n${QuestItemKind.HedgeBranch.description}"
      // Долг Йоргена: список ведёт к логову, жетоны — к Горну.
      case Some(QuestItemKind.JorgenRoster) =>
        s"${item.displayTitle}\n${difficulty(JorgenQuest.Level)}\n\n${QuestItemKind.JorgenRoster.description}" +
          (if (JorgenQuest.toLair(quests)) "\n\n" + content.format("jorgen.card.lairHint",
            "level" -> JorgenQuest.LairDungeonLevel.toString) else "")
      case Some(QuestItemKind.JorgenTokens) =>
        s"${item.displayTitle}\n${difficulty(JorgenQuest.Level)}\n\n${QuestItemKind.JorgenTokens.description}"
      // Замок для предков: слепок — к отцу Брамбла на Стройку, замок — в Зал
      // предков через серебряную жилу.
      case Some(QuestItemKind.BramblCast) =>
        s"${item.displayTitle}\n${difficulty(BramblQuest.Level)}\n\n${QuestItemKind.BramblCast.description}"
      case Some(QuestItemKind.FamilyLock) =>
        s"${item.displayTitle}\n${difficulty(BramblQuest.Level)}\n\n${QuestItemKind.FamilyLock.description}"
      // Отданный лук: срубленное — вернуть изгороди, к старым корням.
      case Some(QuestItemKind.CutBow) =>
        s"${item.displayTitle}\n${difficulty(FaelinQuest.Level)}\n\n${QuestItemKind.CutBow.description}" +
          (if (FaelinQuest.toRoots(quests)) "\n\n" + content.text("faelinBow.card.hint") else "")
      // Чужой багор: печать — к Густаво, потом с карточки — позвать ловчих.
      case Some(QuestItemKind.SnowwalkerSeal) =>
        val now = if (PlukhQuest.toGustavo(quests)) content.text("plukhGaff.card.toGustavo")
                  else if (PlukhQuest.toHunt(quests)) content.text("plukhGaff.card.toHunt")
                  else ""
        s"${item.displayTitle}\n${difficulty(PlukhQuest.Level)}\n\n${QuestItemKind.SnowwalkerSeal.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.BarrelClue) =>
        val now =
          if (BarrelThievesQuest.onTrail(quests))
            content.format("barrelThieves.card.onTrail", "gang" -> BarrelThievesQuest.gangName(content, quests))
          else if (BarrelThievesQuest.askAround(quests)) content.text("barrelThieves.askAround")
          else ""
        s"${item.displayTitle}\n${difficulty(heroLvl.toInt)}\n\n${QuestItemKind.BarrelClue.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.StoryCloak) =>
        val now = StoryMarkQuest.cardLine(content, quests)
        s"${item.displayTitle}\n${difficulty(StoryMarkQuest.Level)}\n\n${QuestItemKind.StoryCloak.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.StoryCipher) =>
        val now = StoryPowderQuest.cardLine(content, quests)
        s"${item.displayTitle}\n${difficulty(StoryPowderQuest.Level)}\n\n${QuestItemKind.StoryCipher.description}" +
          s"\n\n${content.text("story.storyMark.cipher")}" + (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.StorySketch) =>
        val now = StoryBrandQuest.cardLine(content, quests)
        s"${item.displayTitle}\n${difficulty(StoryBrandQuest.Level)}\n\n${QuestItemKind.StorySketch.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.StoryPage) =>
        val now = StoryListQuest.cardLine(content, quests)
        s"${item.displayTitle}\n${difficulty(StoryListQuest.Level)}\n\n${QuestItemKind.StoryPage.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.StoryArchiveKey) =>
        val now = StoryFaceQuest.cardLine(content, quests)
        s"${item.displayTitle}\n${difficulty(StoryFaceQuest.Level)}\n\n${QuestItemKind.StoryArchiveKey.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.StoryCopies) =>
        val now = StoryFaceQuest.cardLine(content, quests)
        s"${item.displayTitle}\n\n${QuestItemKind.StoryCopies.description}" + (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.StoryNote) =>
        val now = StoryToolQuest.cardLine(content, quests)
        s"${item.displayTitle}\n${difficulty(StoryToolQuest.Level)}\n\n${QuestItemKind.StoryNote.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      // Знаки пути с Рубежа I: у Щита и одиночки они же ведут «Наутро» —
      // пока глава идёт, на карточке её сложность и что делать сейчас.
      case Some(kind @ (QuestItemKind.StoryShadowRing | QuestItemKind.StoryCouncilWrit | QuestItemKind.StoryCellBook)) =>
        val now = StoryMorningQuest.cardLine(content, quests, kind)
        if (now.isEmpty) s"${item.displayTitle}\n\n${kind.description}"
        else s"${item.displayTitle}\n${difficulty(StoryMorningQuest.Level)}\n\n${kind.description}\n\n$now"
      // Акт II, «Наутро», путь Тени: поручение Э. — где писарь и что дальше.
      case Some(QuestItemKind.StoryShadowOrder) =>
        val now = StoryMorningQuest.cardLine(content, quests, QuestItemKind.StoryShadowOrder)
        s"${item.displayTitle}\n${difficulty(StoryMorningQuest.Level)}\n\n${QuestItemKind.StoryShadowOrder.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      // Опись урожая — зацепка главы 7: пока глава не открыта — «с обновлением».
      case Some(QuestItemKind.StoryHarvest) =>
        val now = StoryNewcomerQuest.cardLine(content, quests)
        s"${item.displayTitle}\n\n${QuestItemKind.StoryHarvest.description}" + (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.BartiPlan) =>
        val now = HotGoodsQuest.cardLine(content, quests)
        s"${item.displayTitle}\n${difficulty(HotGoodsQuest.Level)}\n\n${QuestItemKind.BartiPlan.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      case Some(QuestItemKind.BartiAddress) =>
        val now = BigDealQuest.cardLine(content, quests)
        s"${item.displayTitle}\n${difficulty(BigDealQuest.Level)}\n\n${QuestItemKind.BartiAddress.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
      // Контракт деревни: сложность — по уровню задания; чья деревня, о чём
      // просили и куда идти — по ходу контракта.
      case Some(QuestItemKind.VillageContract) =>
        val now = Villages.cardLine(content, quests)
        s"${item.displayTitle}" + quests.village.fold("")(c => "\n" + difficulty(c.level.max(1))) +
          s"\n\n${QuestItemKind.VillageContract.description}" + (if (now.isEmpty) "" else "\n\n" + now)
      // Голова главаря разбойников: чья она и сколько за неё доплатит староста.
      case Some(QuestItemKind.BanditHead) =>
        val now = Villages.headLine(content, quests)
        s"${item.displayTitle}\n\n${QuestItemKind.BanditHead.description}" + (if (now.isEmpty) "" else "\n\n" + now)
      case Some(kind @ (QuestItemKind.KelvinMap | QuestItemKind.RaceCoupon | QuestItemKind.NameCoupon)) =>
        s"${item.displayTitle}\n\n${kind.description}"
      case Some(kind) if QuestItemKind.rackOf(kind).isDefined || HerbLore.isTreatise(kind) =>
        s"${item.displayTitle}\n\n${kind.description}"
      case _ => item.displayTitle
    }
  }

  /** Отвар: что делает; боевой — с пометкой, что работает из ячейки пояса. */
  def brewText(content: SceneContent, item: Item, kind: BrewKind): String =
    s"${item.displayTitle}\n${item.statsLines.mkString("\n")}" +
      (if (kind.combat) "\n" + content.text("brew.beltOnly") else "")

  /** Краденый кошелёк, если его развязать: от горсти серебра до увесистой.
    * Один из двухсот оказывается набит редкой монетой — воры и сами этого не
    * поняли. */
  val PurseMinSilver: Long = 10L
  val PurseMaxSilver: Long = 800L
  val PurseJackpotIn: Int  = 200   // 0,5%
  val PurseJackpot:   Long = 100000L


  /** Сколько родов помещается в ряд на экране приговора. */
  val RacesPerRow: Int = 3

  /** Фляга, которую можно заправить отваром: её ячейка на поясе (с нуля), толк,
    * заряды и снаряжение, в котором она уже полна. */
  final case class BrewFlask(slot: Int, name: String, source: RefillSource, charges: Int, maxCharges: Int,
                             refilled: Equipment)

  /** Фляги героя на поясе — по ячейкам. */
  def brewFlasks(hero: Hero): List[BrewFlask] = {
    val equip = hero.equipment
    equip.belt.details match {
      case b: ItemDetails.Belt =>
        b.slots.zipWithIndex.collect { case (f: BeltSlot.Flask, i) =>
          BrewFlask(i, f.name, f.kind.effect.refillSource, f.charges, f.maxCharges,
            equip.copy(belt = equip.belt.copy(details = b.updated(i, f.refilled))))
        }
      case _ => Nil
    }
  }


  val ItemActionPrefix = "InventoryItem_"

  /** Экраны, откуда в сумку заглядывают, не бросая добычу или находку
    * («🎒 Рюкзак»): выход из сумки ведёт обратно на них. */
  val BackpackOrigins: Set[StateType] = Set(StateType.Loot, StateType.FoundItem)

  /** `cardOnReturn` — вернуться на карточку `selectedId`, когда игрок придёт
    * обратно с другого экрана (вставки камня), а не в список. */
  case class InventoryScene(page: Option[Int] = None, selectedId: Option[Long] = None,
                            cardOnReturn: Option[Boolean] = None)
  object InventoryScene {
    implicit val encoder: Encoder[InventoryScene] = deriveEncoder
    implicit val decoder: Decoder[InventoryScene] = deriveDecoder
  }

  /** Оба слота колец заняты — новое кольцо можно надеть только вместо одного из
    * них, поэтому слот спрашиваем у игрока (см. `ringChoiceScreen`). */
  def ringSlotsFull(eq: Equipment): Boolean =
    eq.firstRing.itemType != ItemType.NoItem && eq.secondRing.itemType != ItemType.NoItem

  /** Итог надевания: снаряжение, боевые статы, снятое (`removed`) и то, что с
    * прежнего пояса не поместилось в новый (`spilled`, см. [[BeltContents.swap]]);
    * `beltMoved` — было ли с пояса что перекладывать. В сумку уходит и снятое, и
    * не поместившееся ([[toBag]]). */
  final case class Equipped(equipment: Equipment, fightStats: FightStats, removed: List[Item],
                            spilled: List[Item] = Nil, beltMoved: Boolean = false) {
    def toBag: List[Item] = removed ++ spilled
  }

  /** Надеть `item`. `pairSlot` — какой из двух парных слотов выбрал игрок:
    * кольцо 1 или 2, правая (1) или левая (2) рука; имеет смысл, когда оба
    * заняты. None — обычное правило слотов: первый свободный, а если свободных
    * нет, второй (у рук — правая). Двуручное снимает оба одноручных разом. Пояс
    * поверх пояса забирает содержимое прежнего, а тот уходит пустым. Статы — по
    * [[Hero.reequip]]: запасы сохраняют долю от нового максимума. */
  def equipWith(hero: Hero, item: Item, pairSlot: Option[Int], nowMs: Long): Equipped = {
    val laid = lay(hero, item, pairSlot)
    laid.copy(fightStats = hero.reequip(laid.equipment, nowMs).fightStats)
  }

  /** Куда встанет `item` и что снимется — как в [[equipWith]], но статы
    * остаются прежними: для раскладки (на стойку, примерка нескольких вещей
    * подряд), где статы считают потом одним шагом или не считают вовсе. */
  def lay(hero: Hero, item: Item, pairSlot: Option[Int]): Equipped = {
    val eq = hero.equipment
    val (newEq, out, belt) = (item.itemType, pairSlot) match {
      case (ItemType.Ring, Some(1)) => (eq.copy(firstRing = item), List(eq.firstRing), None)
      case (ItemType.Ring, Some(2)) => (eq.copy(secondRing = item), List(eq.secondRing), None)
      case (ItemType.Weapon | ItemType.Shield, hand) =>
        val (held, gone) = wield(eq, item, hand)
        (held, gone, None)
      case (ItemType.Belt, _) if eq.belt.itemType != ItemType.NoItem =>
        val swap = BeltContents.swap(eq.belt, item)
        (eq.copy(belt = swap.next), List(swap.worn), Some(swap))
      case _ => (withSlot(eq, item), List(equippedIn(eq, item.itemType)), None)
    }
    val removed  = out.filter(_.itemType != ItemType.NoItem)
    Equipped(newEq, hero.fightStats, removed, belt.fold(List.empty[Item])(_.spilled), belt.exists(_.moved))
  }

  /** То же, что [[equipWith]], одним кортежем: снаряжение, статы и всё, что
    * уходит в сумку. */
  def equip(hero: Hero, item: Item, pairSlot: Option[Int], nowMs: Long): (Equipment, FightStats, List[Item]) = {
    val done = equipWith(hero, item, pairSlot, nowMs)
    (done.equipment, done.fightStats, done.toBag)
  }

  /** Оружие или щит в руки. Двуручное встаёт в правую и снимает всё, что было
    * в обеих. Одноручное или щит поверх двуручного снимает двуручное и встаёт в
    * выбранную руку
    * (по умолчанию в правую). Иначе — в выбранную руку, а без выбора в
    * свободную, сперва правую; обе заняты — меняется правая (руку у игрока
    * спрашивают раньше, см. `Equipment.handsFull`). */
  private def wield(eq: Equipment, item: Item, hand: Option[Int]): (Equipment, List[Item]) =
    if (item.twoHanded) (eq.copy(weapon = item, leftHand = Item.NoItem), List(eq.weapon, eq.leftHand))
    else if (eq.leftBusy) {
      if (hand.contains(2)) (eq.copy(weapon = Item.NoItem, leftHand = item), List(eq.weapon))
      else (eq.copy(weapon = item), List(eq.weapon))
    } else hand match {
      case Some(2)                                         => (eq.copy(leftHand = item), List(eq.leftHand))
      case Some(_)                                         => (eq.copy(weapon = item), List(eq.weapon))
      case None if eq.weapon.itemType == ItemType.NoItem   => (eq.copy(weapon = item), Nil)
      case None if eq.leftHand.itemType == ItemType.NoItem => (eq.copy(leftHand = item), Nil)
      case None                                            => (eq.copy(weapon = item), List(eq.weapon))
    }

  /** Почему надеть нельзя — ключ текста, или None, если можно. Вещь уходит из
    * сумки, снятое возвращается: двуручное поверх двух одноручных кладёт в сумку
    * на вещь больше, чем забирает, и второму может не найтись места; так же и
    * пояс меньше прежнего (`belt`) — с не поместившимся в него. Замена,
    * снимающая последний «Тайник», сумку ужимает. */
  def equipBlock(inv: Inventory, capDelta: Long, removed: List[Item], belt: Boolean = false): Option[String] = {
    val returning = (removed.size - 1).max(0)
    if (capDelta >= 0 && returning == 0) None
    else if (fitsAfterCapacityChange(inv, capDelta, returning)) None
    else if (returning > 0) Some(if (belt) "inventory.beltNoRoom" else "inventory.handsNoRoom")
    else Some("equipment.stashBlocked")
  }

  /** Что ушло в сумку при надевании — для строки «Прежний: …». */
  def removedNames(removed: List[Item]): String =
    if (removed.isEmpty) "ничего" else removed.map(_.name).mkString(", ")

  /** Бонус вместимости сумки от пассивки «Тайник» на предмете (0, если её нет).
   *  Хелпер для одного предмета — для итогового изменения вместимости при
   *  надевании/снятии используй [[equipmentStashDelta]] (он дедуплицирует
   *  несколько «Тайников» так же, как остальные пассивки). */
  def stashBonus(item: Item): Long =
    if (item.passive.contains(pangea.model.item.PassiveKind.Stash)) pangea.model.item.PassiveKind.Stash.ExtraSlots
    else 0L

  /** Изменение вместимости сумки при переходе от `oldEq` к `newEq`: разница
   *  бонусов «Тайника», посчитанная по ПОЛНОМУ набору пассивок экипировки
   *  (`Equipment.passiveKinds` — множество, дубли схлопнуты), а не по одному
   *  заменяемому предмету. Поэтому надевание второго «Тайника», пока первый уже
   *  на герое, не даёт delta (бонус уже был учтён) — как и остальные пассивки,
   *  «работает только одна». Положительное — надет первый «Тайник» в экипировке,
   *  отрицательное — снят последний. */
  /** Сколько вещей под резец рунописца показывать на одной странице. */
  val ChiselPage: Int = 8

  def equipmentStashDelta(oldEq: Equipment, newEq: Equipment, body: Set[pangea.model.item.PassiveKind] = Set.empty): Long =
    pangea.model.hero.HeroPassives(newEq.passiveKinds ++ body).extraInventorySlots -
      pangea.model.hero.HeroPassives(oldEq.passiveKinds ++ body).extraInventorySlots

  /** Хватит ли места в сумке, если её вместимость изменится на `capDelta` (может
   *  быть отрицательным — например, теряем бонус «Тайника»), с учётом
   *  `returningItems` предметов, которые вернутся в сумку. Ложь → изменение
   *  переполнит сумку, и его надо блокировать. */
  def fitsAfterCapacityChange(inv: Inventory, capDelta: Long, returningItems: Int): Boolean =
    inv.occupied + returningItems <= inv.maxItems + capDelta

  /** Зона карты клада (для целой карты и её половинки); None у прочих предметов. */
  private def zoneOf(item: Item): Option[pangea.model.item.MapZone] = item.details match {
    case ItemDetails.TreasureMap(zone) => Some(zone)
    case _                             => None
  }

  /** Подробное представление предмета (статы + надетое в том же слоте). Используется
   *  и в детальном экране инвентаря, и снаружи (например, регистрация). */
  def itemText(item: Item, eq: Equipment, hero: Option[Hero] = None): String = item.mapDescription match {
    // Карта клада: только имя (без уровня) и текст-описание, без статов и сравнения
    // слотов — карта не надевается (см. также [[ItemType.equippable]]).
    case Some(desc) => s"${item.displayTitle}\n\n$desc"
    case None       => gearText(item, eq, hero)
  }

  private def gearText(item: Item, eq: Equipment, hero: Option[Hero]): String = {
    def equipped(prefix: String, cur: Item): String =
      if (cur.itemType == ItemType.NoItem) s"$prefix: свободен"
      else cur.equippedComparison(prefix)
    // Разделитель между сравниваемым предметом и тем, что уже надето.
    val sep = Item.ComparisonSeparator
    // У рук род женский: «Правая рука: свободна».
    def hand(prefix: String, cur: Item): String =
      if (cur.itemType == ItemType.NoItem) s"$prefix: свободна" else cur.equippedComparison(prefix)
    val slotInfo = item.itemType match {
      case ItemType.Trophy => "\nТрофей"
      case ItemType.NamedTrophy => "\nУникальный трофей. В Гильдию не сдаётся — его вешают на Стену трофеев."
      case ItemType.Ring   => s"\n$sep\n${equipped("Слот 1", eq.firstRing)}\n${equipped("Слот 2", eq.secondRing)}"
      case ItemType.Weapon | ItemType.Shield =>
        val left =
          if (eq.leftBusy) s"${EquipmentState.LeftHandName}: ${EquipmentState.LeftBusyText}"
          else hand(EquipmentState.LeftHandName, eq.leftHand)
        s"\n$sep\n${hand(EquipmentState.RightHandName, eq.weapon)}\n$left"
      case _ =>
        val cur = equippedIn(eq, item.itemType)
        if (cur.itemType == ItemType.NoItem) ""
        else s"\n$sep\n${equipped("Сейчас надет", cur)}"
    }
    // Сюжетная экипировка: что с ней можно и чего нельзя — строкой под статами.
    val story = if (item.isStoryGear) s"\n\n${Item.StoryGearNote}" else ""
    s"${item.displayTitle}\n${gearBody(item, hero)}$story$slotInfo"
  }

  /** Что вещь даёт сама по себе: статы и описания умения и пассивки — без
    * названия и без сравнения с надетым. Это тело карточки вещи; его же
    * «Снаряжение» в Mini App показывает у надетой вещи. */
  def gearBody(item: Item, hero: Option[Hero]): String = {
    val stats    = item.statsLines
    val statsStr = if (stats.isEmpty) "Нет характеристик" else stats.mkString("\n")
    // Описание активного навыка (с подставленной стоимостью энергии от уровня героя).
    val skillStr = (hero, item.activeSkill) match {
      case (Some(h), Some(s)) => s"\n\n${s.describe(h, Some(item))}"
      case _                  => ""
    }
    // Описание пассивного навыка (статичный текст — без стоимости/кулдауна).
    val passiveStr = item.passive.map(k => s"\n\n${k.describe}").getOrElse("")
    s"$statsStr$skillStr$passiveStr"
  }

  // Возвращает предмет, который будет вытеснен в инвентарь при надевании
  private def equippedIn(eq: Equipment, itemType: ItemType): Item = itemType match {
    case ItemType.Helmet           => eq.helmet
    case ItemType.ShoulderPads     => eq.shoulderPads
    case ItemType.ChestPlate       => eq.chestPlate
    case ItemType.Bracelets        => eq.bracelets
    case ItemType.Gloves           => eq.gloves
    case ItemType.Pants            => eq.pants
    case ItemType.Leggings         => eq.pants
    case ItemType.Boots            => eq.boots
    case ItemType.Amulet           => eq.amulet
    // Кольцо: ничего не вытесняется пока есть свободный слот; иначе вытесняется второй
    case ItemType.Ring             =>
      if (eq.firstRing.itemType == ItemType.NoItem || eq.secondRing.itemType == ItemType.NoItem) Item.NoItem
      else eq.secondRing
    case ItemType.Belt             => eq.belt
    case ItemType.Flask            => Item.NoItem // фляги носят на поясе, не в снаряжении
    case ItemType.Weapon           => eq.weapon
    case ItemType.Shield           => eq.weapon // руки — см. wield
    case ItemType.AdditionalWeapon => eq.additionalWeapon
    case ItemType.Trophy           => Item.NoItem // трофей не экипируется
    case ItemType.NamedTrophy      => Item.NoItem
    case ItemType.TreasureMap      => Item.NoItem // карта не экипируется
    case ItemType.TreasureMapHalf  => Item.NoItem // половинка карты не экипируется
    case ItemType.Gem              => Item.NoItem // камень не экипируется
    case ItemType.Material         => Item.NoItem // материал не экипируется
    case ItemType.QuestItem        => Item.NoItem // сюжетный предмет не экипируется
    case ItemType.Brew             => Item.NoItem // отвар не экипируется
    case ItemType.Rose             => Item.NoItem // роза ложится в ячейку пояса
    case ItemType.RuneStone        => Item.NoItem // рунный камень не экипируется
    case ItemType.Bundle           => Item.NoItem // стопка не экипируется
    case ItemType.NoItem           => Item.NoItem
  }

  private def withSlot(eq: Equipment, item: Item): Equipment = item.itemType match {
    case ItemType.Helmet           => eq.copy(helmet = item)
    case ItemType.ShoulderPads     => eq.copy(shoulderPads = item)
    case ItemType.ChestPlate       => eq.copy(chestPlate = item)
    case ItemType.Bracelets        => eq.copy(bracelets = item)
    case ItemType.Gloves           => eq.copy(gloves = item)
    case ItemType.Pants            => eq.copy(pants = item)
    case ItemType.Leggings         => eq.copy(pants = item)
    case ItemType.Boots            => eq.copy(boots = item)
    case ItemType.Amulet           => eq.copy(amulet = item)
    // Кольцо: первый свободный слот; если оба заняты — второй
    case ItemType.Ring             =>
      if (eq.firstRing.itemType == ItemType.NoItem) eq.copy(firstRing = item)
      else eq.copy(secondRing = item)
    case ItemType.Belt             => eq.copy(belt = item)
    case ItemType.Flask            => eq // фляга ложится в ячейку пояса
    case ItemType.Weapon           => eq.copy(weapon = item)
    case ItemType.Shield           => eq.copy(weapon = item) // руки — см. wield
    case ItemType.AdditionalWeapon => eq.copy(additionalWeapon = item)
    case ItemType.Trophy           => eq // трофей не экипируется
    case ItemType.NamedTrophy      => eq
    case ItemType.TreasureMap      => eq // карта не экипируется
    case ItemType.TreasureMapHalf  => eq // половинка карты не экипируется
    case ItemType.Gem              => eq // камень не экипируется
    case ItemType.Material         => eq // материал не экипируется
    case ItemType.QuestItem        => eq // сюжетный предмет не экипируется
    case ItemType.Brew             => eq // отвар не экипируется
    case ItemType.Rose             => eq // роза ложится в ячейку пояса
    case ItemType.RuneStone        => eq // рунный камень не экипируется
    case ItemType.Bundle           => eq // стопка не экипируется
    case ItemType.NoItem           => eq
  }

  /** Статы вместе с вещами: запасы растут на всю броню и энергию вещи. Только
    * для стартового снаряжения новичка (`RegistrationState`): оно приходит
    * полным. Смена надетого — через [[Hero.reequip]], на разницу запасы не
    * двигают. */
  def applyDelta(base: FightStats, added: Item, removed: Item): FightStats =
    base.copy(
      atk           = (base.atk + added.attack - removed.attack).max(0L),
      armor         = (base.armor + added.armor - removed.armor).max(0L),
      defence       = (base.defence + added.defence - removed.defence).max(0L),
      evasion       = (base.evasion + added.evasion - removed.evasion).max(0L),
      accuracy      = (base.accuracy + added.accuracy - removed.accuracy).max(0L),
      energy        = (base.energy + added.energy - removed.energy).max(0L)
    )
}
