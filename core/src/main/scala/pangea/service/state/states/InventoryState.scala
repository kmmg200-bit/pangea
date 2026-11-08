package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import pangea.model.item.{BrewEffect, BrewKind, BrewRates}
import pangea.model.stats.{ParamsBuff, StatBoost}
import pangea.model.trauma.Trauma
import pangea.service.state.AzatData
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.{MaterialGenerator, TreasureMapGenerator}
import pangea.model.hero.{Equipment, Hero, WeaponCoat, WeaponDust}
import pangea.model.inventory.Inventory
import pangea.model.item.{BeltSlot, Chisel, Gem, GemBreaking, Item, ItemDetails, ItemStack, ItemType, MaterialKind, QuestItemKind, RefillSource, TrophyKind}
import pangea.model.monster.Race
import pangea.model.quest.{Difficulty, NpcQuest, NpcQuests}
import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.equipment.Outfit
import pangea.service.state.states.InventoryState._
import pangea.service.state.states.home.Racks
import pangea.service.state.states.marisa.MarisaHuntState
import pangea.service.state.states.murloc.MurlocVillageState
import pangea.service.state.states.race.RaceQuestState
import pangea.service.state.{BarrelThievesQuest, BeltContents, HedgeSongQuest, HerbLore, InventoryFeedback, ItemMenu, MarisaQuest, MurlocQuest, NpcQuestLog, QuestGoods,
  SlotChoice, StackMove, State,
  StrayCasketQuest, UiScene, UserAction}
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
      "RackKeyUse"        -> Target.Run { (u, _,  r) => useRackKey(u, r) },
      "RefillFlaskPick"   -> Target.Run { (u, ua, r) => refillFlask(u, ua, r) },
      "CoatWeapon"        -> Target.Run { (u, _,  r) => coatWeapon(u, r) },
      "CureTraumaPick"    -> Target.Run { (u, ua, r) => cureTrauma(u, ua, r) },
      "SentencePick"      -> Target.Run { (u, ua, r) => sentenceRace(u, ua, r) },
      // «Письмо Марисе»: вскрыть письмо, отправиться по карте Кельвина.
      "OpenLetter"        -> Target.Run { (u, _,  r) => openLetter(u, r) },
      "UseKelvinMap"      -> Target.Run { (u, _,  r) => useKelvinMap(u, r) },
      "MapWithMarisa"     -> Target.Run { (u, _,  r) => startHunt(u, r, withMarisa = true) },
      "MapAlone"          -> Target.Run { (u, _,  r) => startHunt(u, r, withMarisa = false) },
      // «Деревня Мурлоков»: по карте старейшины — к развилке (налёт или помощь).
      "UseMurlocMap"      -> Target.Run { (u, _,  r) => useMurlocMap(u, r) },
      // «Бродячий ларец»: скормить камень выбранного качества; сытого — отпустить,
      // чтобы вёл к хозяину.
      "StrayFeed"         -> Target.Run { (u, ua, r) => feedStray(u, ua, r) },
      "StrayGo"           -> Target.Run { (u, _,  r) => goStray(u, r) },
      // «Вскрытая бочка»: по следу банды, что вскрыла бочку, — только из города.
      "BarrelTrail"       -> Target.Run { (u, _,  r) => followTrail(u, r) },
      // Трактаты Густаво: читать, пока не осилишь.
      "ReadTreatise"      -> Target.Run { (u, _,  r) => readTreatise(u, r) },
      "IdentifyFlower"    -> Target.Run { (u, _,  r) => identifyFlower(u, r) },
      // Купон на смену расы: квест начинается сразу — только из города.
      "RaceChange"        -> Target.Run { (u, _,  r) => startRaceChange(u, r) },
      // Купон на смену имени: новое имя пишут текстом — тоже только из города.
      "NameChange"        -> Target.Run { (u, _,  r) => startNameChange(u, r) }
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.HeroStats, StateType.Backpack, StateType.Inventory, StateType.Socketing,
        StateType.MarisaHunt, StateType.MurlocVillage, StateType.RaceQuest, StateType.NameChange,
        StateType.StrayCasket, StateType.BarrelThieves) ++ BackpackOrigins

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
             // но места в сумке занимают по-прежнему поштучно.
             val stacks = ItemStack.grouped(items)
             val (pageItems, totalPages, page) = ItemMenu.page(stacks, scene.page.getOrElse(0))
             val header   = s"📦 Инвентарь${if (totalPages > 1) s" (${page + 1}/$totalPages)" else ""} | 🪙 ${hero.silver} | 🟡 ${hero.doubloons}"
             val itemBtns = ItemMenu.stackButtons(pageItems, ItemActionPrefix)
             val nav      = navRow(page, totalPages, "InventoryPrev", "InventoryNext", "BackFromInventory")
             renderer.show(user, Screen(StackMove.withNote(note, header), itemBtns ++ nav))
           }
    } yield StateType.Inventory

  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      (_, totalPages, _) = ItemMenu.page(inv.items.data, 0)
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
            // Пыль сыплется на оружие: разовое покрытие на один бой.
            Option.when(item.material.exists(_.gem.isDefined))(
              content.choice("DustWeapon", "inventory.dustWeapon").copy(color = ChoiceColor.Positive, row = Some(0))),
            // Фляга, пыль и цветок ложатся на пояс: оттуда их достают в бою.
            // У отвара своя карточка и своя такая же кнопка (см. showBrew).
            Option.when(BeltSlot.of(item).isDefined)(
              content.choice("BeltPut", "inventory.beltPut").copy(color = ChoiceColor.Positive, row = Some(0))),
            Some(content.choice("Drop", "inventory.drop").copy(color = ChoiceColor.Negative, row = Some(0))),
            dropAllChoice(count),
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
    * оружие» у смазок. Шнапс уходит Трактирщику,
    * кнопок у него нет.
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
        Some(content.choice("Drop", "inventory.drop").copy(color = ChoiceColor.Negative, row = Some(0))),
        dropAllChoice(count),
        Some(content.choice("InventoryList", "inventory.exit").copy(row = Some(1), color = ChoiceColor.Negative))
      ).flatten
      renderer.show(user, Screen(text, choices)).as(StateType.Inventory)
    }

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
      val slot   = payloadField(ua, "slot").flatMap(_.toIntOption)
      val chosen = item.brew.toList.flatMap(kind => InventoryState.brewFlasks(hero)
                     .filter(f => kind.refills.contains(f.source) && f.charges < f.maxCharges))
                     .find(f => slot.contains(f.slot))
      chosen match {
        case Some(f) =>
          heroDao.updateEquipment(user.userId, f.refilled) *>
            consume(user, item, hero, content.format("brew.refilled", "flask" -> f.name), renderer)
        case None => refillByBrew(user, renderer)
      }
    }

  /** Смазать оружие: на ближайший бой удары по HP травят или пускают кровь.
    * Новая смазка сменяет прежнюю; без оружия мазать нечего. */
  private def coatWeapon(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      item.brew.map(_.effect) match {
        case Some(BrewEffect.Coat(coat, _)) if hero.equipment.unarmed =>
          renderer.show(user, Screen(content.text("brew.noWeapon"), Nil)) *> showItem(user, item.id, renderer)
        case Some(BrewEffect.Coat(coat, _)) =>
          val key = coat match { case WeaponCoat.Poison => "brew.coatedPoison"; case WeaponCoat.Bleed => "brew.coatedBleed" }
          heroDao.updateWeaponDust(user.userId, hero.weaponDust.copy(coat = Some(coat))) *>
            consume(user, item, hero, content.format(key, "weapon" -> hero.equipment.mainWeapon.name), renderer)
        case _ => showItem(user, item.id, renderer)
      }
    }

  /** Глоток: по виду отвара. Костоправный сперва спрашивает, какую травму лечить. */
  private def drinkBrew(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      item.brew.map(_.effect) match {
        case Some(BrewEffect.CureTrauma) => offerCure(user, item, hero, renderer)
        case Some(BrewEffect.InstantRest) =>
          for {
            now  <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
            azat <- AzatData.load(heroDao, user.userId, now)
            _    <- heroDao.writeAzatData(user.userId, azat.copy(instantRests = azat.instantRests + BrewRates.InstantRests).asJson)
            res  <- consume(user, item, hero, content.format("brew.rests",
                      "count" -> BrewRates.InstantRests.toString,
                      "total" -> (azat.instantRests + BrewRates.InstantRests).toString), renderer)
          } yield res
        // Пузырьковый нектар: плёнка ждёт своего часа и срока не имеет.
        case Some(BrewEffect.Bubble) =>
          for {
            _   <- heroDao.updateStatBoosts(user.userId, hero.statBoosts.add(
                     StatBoost(BrewRates.BubbleBoost, ParamsBuff.zero, Long.MaxValue), 0L))
            res <- consume(user, item, hero, content.text("brew.bubble"), renderer)
          } yield res
        // Приговор: сперва герой называет род, и только потом пьёт.
        case Some(BrewEffect.Sentence) => offerSentence(user, renderer)
        // Зеркальный настой: копии ждут ближайшего боя там же, где пыль и
        // смазка, — и сходят вместе с ними, когда бой кончится.
        case Some(BrewEffect.Mirror(copies)) =>
          heroDao.updateWeaponDust(user.userId, hero.weaponDust.copy(mirrors = copies)) *>
            consume(user, item, hero, content.format("brew.mirrors", "count" -> copies.toString), renderer)
        // Волчий зов: запах держится час, но сгорает на первой же встрече.
        case Some(BrewEffect.WolfCall) =>
          for {
            now <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
            _   <- heroDao.updateStatBoosts(user.userId, hero.statBoosts.add(
                     StatBoost(BrewRates.WolfCallBoost, ParamsBuff.zero, now + BrewRates.WolfCallMs), now))
            res <- consume(user, item, hero, content.text("brew.wolfCall"), renderer)
          } yield res
        // Гнилой зов — узелок: так же час, только смрад ведёт к Гнилому Джо.
        case Some(BrewEffect.RotCall) =>
          for {
            now <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
            _   <- heroDao.updateStatBoosts(user.userId, hero.statBoosts.add(
                     StatBoost(BrewRates.RotCallBoost, ParamsBuff.zero, now + BrewRates.RotCallMs), now))
            res <- consume(user, item, hero, content.text("brew.rotCall"), renderer)
          } yield res
        case Some(BrewEffect.Boost(name, buff, label)) =>
          for {
            now <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
            _   <- heroDao.updateStatBoosts(user.userId,
                     hero.statBoosts.add(StatBoost(name, buff, now + BrewRates.BoostDurationMs), now))
            res <- consume(user, item, hero, content.format("brew.boosted",
                     "brew" -> item.name, "pct" -> BrewRates.BoostPct.toString, "stat" -> label), renderer)
          } yield res
        case _ => showItem(user, item.id, renderer)
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
        case None => showItem(user, item.id, renderer)
        case Some(race) =>
          for {
            now <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
            _   <- heroDao.updateStatBoosts(user.userId, hero.statBoosts.add(
                     StatBoost(BrewRates.SentenceBoost + race.entryName, ParamsBuff.zero,
                       now + BrewRates.SentenceMs), now))
            res <- consume(user, item, hero, content.format("brew.sentenced", "race" -> race.toString), renderer)
          } yield res
      }
    }

  /** Какую травму лечить: лёгкие и средние по кнопке, тяжёлые — только перечислены. */
  private def offerCure(user: User, item: Item, hero: Hero, renderer: Renderer): Task[StateType] =
    for {
      now <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      active   = hero.activeTraumas(now)
      curable  = active.filter(_.severity != Trauma.Heavy)
      heavy    = active.filter(_.severity == Trauma.Heavy)
      res <-
        if (active.isEmpty)
          renderer.show(user, Screen(content.text("brew.noTraumas"), Nil)) *> showItem(user, item.id, renderer)
        else if (curable.isEmpty)
          renderer.show(user, Screen(content.text("brew.onlyHeavy"), Nil)) *> showItem(user, item.id, renderer)
        else {
          val heavyLine = if (heavy.isEmpty) "" else "\n" + content.format("brew.heavyStay", "names" -> heavy.map(_.name).mkString(", "))
          val choices = curable.distinct.map(t =>
            Choice("CureTraumaPick", t.name, data = Map("trauma" -> t.name), color = ChoiceColor.Positive, row = Some(0))
          ) :+ content.choice("ItemCard", "inventory.exit").copy(row = Some(1), color = ChoiceColor.Negative)
          renderer.show(user, Screen(content.text("brew.whichTrauma") + heavyLine, choices)).as(StateType.Inventory)
        }
    } yield res

  private def cureTrauma(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      for {
        now <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
        chosen = payloadField(ua, "trauma").flatMap(Trauma.byName)
                   .filter(t => t.severity != Trauma.Heavy && hero.activeTraumas(now).contains(t))
        res <- chosen match {
          case None => showItem(user, item.id, renderer)
          case Some(t) =>
            val names = removeFirst(hero.traumaNames, t.name)
            val until = if (names.isEmpty) None else hero.traumaUntil
            heroDao.updateTrauma(user.userId, until, names) *>
              consume(user, item, hero, content.format("brew.cured", "trauma" -> t.name), renderer)
        }
      } yield res
    }

  private def removeFirst(names: List[String], name: String): List[String] = {
    val i = names.indexOf(name)
    if (i < 0) names else names.patch(i, Nil, 1)
  }

  /** Отвар выпит: предмет уходит, сообщение, и — если в стопке ещё есть — обратно на карточку. */
  private def consume(user: User, item: Item, hero: Hero, line: String, renderer: Renderer): Task[StateType] =
    inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
      renderer.show(user, Screen(line, Nil)) *>
      backToCard(user, renderer)

  // ── Сюжетные предметы («Письмо Марисе») ────────────────────────────────────

  /** Письмо: описание и «Вскрыть» (после вскрытия — «Перечитать»); карта:
    * описание и «Отправиться по карте». Ни выбросить, ни продать. */
  private def showQuestItem(user: User, item: Item, renderer: Renderer): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      opened  = quests.of(NpcQuest.Marisa).bonus
      // Бродячий ларец, пока голоден, — кнопка на каждое качество камней при
      // герое, по ряду на кнопку; выход — рядом ниже.
      feed   <- if (item.questItem.contains(QuestItemKind.StrayCasket) && StrayCasketQuest.hungry(quests))
                  getHero(user).flatMap(h => QuestGoods.all(inventoryRepo, artifacts, h.id)).map(strayFeedChoices)
                else ZIO.succeed(Nil)
      exit    = content.choice("InventoryList", "inventory.exit")
                  .copy(row = Some(if (feed.isEmpty) 1 else feed.size), color = ChoiceColor.Negative)
      // Что с предметом сделать — своя кнопка у каждого вида; текст карточки —
      // общий с Mini App (см. InventoryState.questText).
      use     = item.questItem match {
        case Some(QuestItemKind.MarisaLetter) =>
          Some(content.choice("OpenLetter", if (opened) "marisa.readLetterLabel" else "marisa.openLetterLabel")
            .copy(color = ChoiceColor.Positive, row = Some(0)))
        case Some(QuestItemKind.KelvinMap) =>
          Some(content.choice("UseKelvinMap", "marisa.mapUseLabel").copy(color = ChoiceColor.Positive, row = Some(0)))
        case Some(QuestItemKind.MurlocVillageMap) =>
          Some(content.choice("UseMurlocMap", "murlocVillage.map.useLabel").copy(color = ChoiceColor.Positive, row = Some(0)))
        // Купон на смену расы: «Сменить расу» — квест начнётся сразу.
        case Some(QuestItemKind.RaceCoupon) =>
          Some(content.choice("RaceChange", "raceQuest.useLabel").copy(row = Some(0)))
        case Some(QuestItemKind.NameCoupon) =>
          Some(content.choice("NameChange", "nameChange.useLabel").copy(row = Some(0)))
        // Ключ магической стойки: повернул — надетое и стойка дома меняются местами.
        case Some(key) if QuestItemKind.rackOf(key).isDefined =>
          Some(Choice("RackKeyUse", content.text("home.rack.useKey"), color = ChoiceColor.Positive, row = Some(0)))
        case Some(book) if HerbLore.isTreatise(book) =>
          Some(content.choice("ReadTreatise", "knowledge.readLabel").copy(color = ChoiceColor.Positive, row = Some(0)))
        // Сытый бродячий ларец: «Идти за ларцом» — он поведёт к хозяину.
        case Some(QuestItemKind.StrayCasket) if StrayCasketQuest.leads(quests) =>
          Some(content.choice("StrayGo", "strayCasket.goLabel").copy(row = Some(0)))
        // Улика из бочки, место известно: «Идти по следу».
        case Some(QuestItemKind.BarrelClue) if BarrelThievesQuest.onTrail(quests) =>
          Some(content.choice("BarrelTrail", "barrelThieves.trailLabel").copy(row = Some(0)))
        case _ => None
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

  /** Отпустить сытый ларец: он ведёт к хозяину — только в лабиринте («Персонаж»
    * открыт из него), на любом уровне. В городе и в прочих сценах — отказ, как
    * у сюжетных карт, только наоборот. */
  private def goStray(user: User, renderer: Renderer): Task[StateType] =
    for {
      from   <- heroDao.readReturnState(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!StrayCasketQuest.leads(quests)) backToCard(user, renderer)
                else if (!from.contains(StateType.Dungeon))
                  renderer.show(user, Screen(content.text("strayCasket.card.onlyInLabyrinth"), Nil)) *>
                    backToCard(user, renderer)
                else ZIO.succeed(StateType.StrayCasket)
    } yield res

  /** По следу банды, что вскрыла бочку: только из города (как сюжетные карты)
    * и только когда Ришелье назвал место. */
  private def followTrail(user: User, renderer: Renderer): Task[StateType] =
    for {
      from   <- heroDao.readReturnState(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!BarrelThievesQuest.onTrail(quests)) backToCard(user, renderer)
                else if (!from.exists(StateType.cityStates.contains))
                  renderer.show(user, Screen(content.text("barrelThieves.onlyInCity"), Nil)) *> backToCard(user, renderer)
                else ZIO.succeed(StateType.BarrelThieves)
    } yield res

  /** Повернуть ключ стойки: магия меняет надетое местами с тем, что висит на
    * стойке дома, — где угодно, кроме боя (из боя в сумку не попасть). */
  private def useRackKey(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, _) =>
      item.questItem.flatMap(QuestItemKind.rackOf) match {
        case None => showItem(user, item.id, renderer)
        case Some(n) =>
          Racks(heroDao, inventoryRepo, content).swap(user, n, renderer).flatMap {
            case Left(why) => renderer.show(user, Screen(why, Nil)) *> showItem(user, item.id, renderer)
            case Right(_)  =>
              renderer.show(user, Screen(content.format("home.rack.keyTurned", "n" -> n.toString), Nil)) *> showList(user, renderer)
          }
      }
    }

  /** Вскрыть письмо: текст Кельвина и карта с обратной стороны. */
  private def openLetter(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      given <- MarisaQuest.revealMap(heroDao, inventoryRepo, itemRepository, user.userId, hero)
      _     <- renderer.show(user, Screen(content.text("marisa.letterText"), Nil))
      _     <- ZIO.when(given)(renderer.show(user, Screen(
                 content.format("marisa.questItemAdded", "item" -> QuestItemKind.KelvinMap.displayName), Nil)))
      res   <- showList(user, renderer)
    } yield res

  /** Трактат: бросок интеллект ÷ 2 процентов (+2 % за каждую прошлую неудачу);
    * осилил — знание и книга уходит, нет — час на переварить, потом снова. */
  private def readTreatise(user: User, renderer: Renderer): Task[StateType] =
    for {
      now   <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      hero  <- getHero(user)
      scene <- readScene(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      // Трактат — та книга, по которой есть чему научиться.
      book   = for {
                 item      <- scene.selectedId.flatMap(id => inv.items.data.find(_.id == id))
                 kind      <- item.questItem
                 knowledge <- HerbLore.knowledgeOf(kind)
               } yield (item, kind, knowledge)
      res <- book match {
        case Some((item, kind, knowledge)) =>
          for {
            lore <- HerbLore.readLore(heroDao, user.userId)
            until = lore.bookCooldowns.getOrElse(kind.entryName, 0L)
            _ <- if (now < until)
                   renderer.show(user, Screen(content.format("knowledge.readCooldown",
                     "mins" -> (((until - now) + 59999L) / 60000L).max(1L).toString), Nil))
                 else for {
                   roll <- Random.nextIntBetween(1, 101)
                   _    <- renderer.show(user, Screen(content.text("knowledge.readSpent"), Nil))
                   _    <- if (roll <= HerbLore.readingChance(hero, now, lore.failuresOf(kind.entryName)))
                             HerbLore.writeLore(heroDao, user.userId, lore.learn(knowledge, alone = false).bookMastered(kind.entryName)) *>
                               inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
                               renderer.show(user, Screen(
                                 content.text("knowledge.readSuccess") + "\n" +
                                   content.format("knowledge.gained", "title" -> knowledge.title), Nil))
                           else
                             HerbLore.writeLore(heroDao, user.userId, lore.bookFailed(kind.entryName, now + HerbLore.ReadingCooldownMs)) *>
                               renderer.show(user, Screen(content.text("knowledge.readFailed"), Nil))
                 } yield ()
            r <- showList(user, renderer)
          } yield r
        case _ => showList(user, renderer)
      }
    } yield res

  /** Карта Кельвина: только из города; если Мариса ждёт — спросить, брать ли её. */
  private def useKelvinMap(user: User, renderer: Renderer): Task[StateType] =
    for {
      from   <- heroDao.readReturnState(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res <- if (!from.exists(StateType.cityStates.contains))
               renderer.show(user, Screen(content.text("marisa.mapOnlyInCity"), Nil)) *> showList(user, renderer)
             else if (quests.onStep(NpcQuest.Marisa, 3))
               renderer.show(user, content.screen("marisa.mapTakeMarisa")).as(StateType.Inventory)
             else startHunt(user, renderer, withMarisa = false)
    } yield res

  /** Купон на смену расы: из города — к выбору расы, дальше квест ведёт
    * RaceQuestState; не из города — отказ, как у сюжетных карт. */
  private def startRaceChange(user: User, renderer: Renderer): Task[StateType] =
    for {
      from <- heroDao.readReturnState(user.userId)
      res  <- if (!from.exists(StateType.cityStates.contains))
                renderer.show(user, Screen(content.text("raceQuest.onlyInCity"), Nil)) *> showList(user, renderer)
              else
                heroDao.writeSceneData(user.userId, RaceQuestState.Progress.start.asJson).as(StateType.RaceQuest)
    } yield res

  /** Купон на смену имени: из города — к выбору имени (см. NameChangeState);
    * не из города — отказ, как у купона расы. */
  private def startNameChange(user: User, renderer: Renderer): Task[StateType] =
    heroDao.readReturnState(user.userId).flatMap { from =>
      if (from.exists(StateType.cityStates.contains)) ZIO.succeed(StateType.NameChange)
      else renderer.show(user, Screen(content.text("nameChange.onlyInCity"), Nil)) *> showList(user, renderer)
    }

  /** Карта деревни мурлоков: только из города; дальше развилку показывает сама деревня. */
  private def useMurlocMap(user: User, renderer: Renderer): Task[StateType] =
    for {
      from <- heroDao.readReturnState(user.userId)
      res  <- if (!from.exists(StateType.cityStates.contains))
                renderer.show(user, Screen(content.text("marisa.mapOnlyInCity"), Nil)) *> showList(user, renderer)
              else
                heroDao.writeSceneData(user.userId,
                  MurlocVillageState.Progress(MurlocVillageState.Step.Route).asJson).as(StateType.MurlocVillage)
    } yield res

  private def startHunt(user: User, renderer: Renderer, withMarisa: Boolean): Task[StateType] =
    for {
      from <- heroDao.readReturnState(user.userId)
      res  <- if (!from.exists(StateType.cityStates.contains))
                renderer.show(user, Screen(content.text("marisa.mapOnlyInCity"), Nil)) *> showList(user, renderer)
              else
                heroDao.writeSceneData(user.userId,
                  MarisaHuntState.Progress(MarisaHuntState.Step.Road, withMarisa).asJson).as(StateType.MarisaHunt)
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
      inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
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

  /** Как зовётся узор вещи для экрана: умение, пассивка или «узора нет». */
  private def chiselPattern(item: Item): String =
    Chisel.pattern(item).getOrElse(content.text("inventory.chisel.noPattern"))

  /** Сколько мест освободить в сумке, чтобы переписать вещь. Резец снимает с
    * надетой вещи «Тайник» наверняка (нынешний узор в розыгрыш не входит), и
    * без него сумка может не вместить своего. Резец, уходя, своё место
    * освобождает. 0 — переписывать можно. */
  private def chiselShortage(hero: Hero, inv: Inventory, item: Item, worn: Boolean): Long =
    if (!worn || !item.passive.contains(pangea.model.item.PassiveKind.Stash)) 0L
    else Chisel.variants(item).headOption.fold(0L) { v =>
      val capDelta = InventoryState.equipmentStashDelta(hero.equipment, hero.equipment.replacing(v),
                       hero.runes.brandedPassives.toSet)
      (inv.occupied - 1L - (inv.maxItems + capDelta)).max(0L)
    }

  /** Отказ: сначала освободить место — с вещи уйдёт «Тайник». */
  private def chiselStashLine(item: Item, shortage: Long): String =
    content.format("inventory.chisel.stashBlocked", "name" -> item.name, "count" -> shortage.toString)

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
                "name" -> item.name, "pattern" -> chiselPattern(item))),
              data = Map("item" -> item.id.toString), row = Some(i))
          }
          val nav = List(
            Option.when(page > 0)(Choice("ChiselUse", "◀", data = Map("p" -> (page - 1).toString), row = Some(ChiselPage))),
            Option.when(page < last)(Choice("ChiselUse", "▶", data = Map("p" -> (page + 1).toString), row = Some(ChiselPage)))
          ).flatten
          val notes = all.collect { case (item, worn) if chiselShortage(hero, inv, item, worn) > 0L =>
                        chiselStashLine(item, chiselShortage(hero, inv, item, worn)) }
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
      val shortage = chiselShortage(hero, inv, item, worn)
      if (shortage > 0L)
        renderer.show(user, Screen(chiselStashLine(item, shortage), Nil)) *> offerChisel(user, ua, renderer)
      else renderer.show(user, Screen(
        content.format("inventory.chisel.confirm", "name" -> item.name, "pattern" -> chiselPattern(item)),
        List(
          Choice("ChiselDo", content.text("inventory.chisel.yes"),
            data = Map("item" -> item.id.toString), color = ChoiceColor.Positive, row = Some(0)),
          Choice("ChiselUse", content.text("inventory.chisel.back"), row = Some(1))
        ))).as(StateType.Inventory)
    }

  /** Резец сводит узор в случайный другой из тех, что бывают у такой вещи, и
    * рассыпается. С надетой вещи мог уйти или прийти «Тайник» — вместимость
    * сумки следует за ним; уйти ему, когда сумка без него переполнится,
    * нельзя: тогда ничего не трогаем и резец цел. */
  private def doChisel(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withChiselTarget(user, ua, renderer) { (hero, inv, item, worn, chisel) =>
      val variants = Chisel.variants(item)
      Random.nextIntBounded(variants.size).flatMap { i =>
        val rewritten = variants(i)
        val done      = content.format("inventory.chisel.done", "name" -> item.name,
                          "before" -> chiselPattern(item), "after" -> chiselPattern(rewritten))
        val spend     = inventoryRepo.removeItem(chisel.id, hero.id).mapError(e => new Throwable(e.toString))
        if (worn) {
          val newEq    = hero.equipment.replacing(rewritten)
          val capDelta = InventoryState.equipmentStashDelta(hero.equipment, newEq, hero.runes.brandedPassives.toSet)
          // Подтвердили до того, как сумка наполнилась (старая кнопка), — та же проверка.
          val shortage = chiselShortage(hero, inv, item, worn)
          if (shortage > 0L)
            renderer.show(user, Screen(chiselStashLine(item, shortage), Nil)) *> offerChisel(user, ua, renderer)
          else
            spend *> heroDao.updateEquipment(user.userId, newEq) *>
              ZIO.when(capDelta != 0)(inventoryRepo.increaseCapacity(hero.id, capDelta).orElse(ZIO.unit)) *>
              renderer.show(user, Screen(done, Nil)) *> showList(user, renderer)
        } else
          inventoryRepo.updateItem(hero.id, rewritten).mapError(e => new Throwable(e.toString)) *> spend *>
            renderer.show(user, Screen(done, Nil)) *> showList(user, renderer)
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
              inventoryRepo.updateItem(hero.id, emptied).mapError(e => new Throwable(e.toString)) *>
                grantDust(user, hero, gem, renderer, showList(user, renderer), backToCard(user, renderer))
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

  private def doCrush(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      item.gem match {
        case None => showList(user, renderer)
        case Some(gem) =>
          inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
            grantDust(user, hero, gem, renderer, backToCard(user, renderer), backToCard(user, renderer))
      }
    }

  /** Выдаёт пыль за сломанный камень. Пыль кладётся отдельными предметами, поэтому
    * место в сумке проверяем заранее: лучше отказать, чем потерять часть пыли.
    * `andThen` — куда уйти после: раскрошенный камень из стопки возвращает на
    * карточку стопки, камень из гнезда — в список. `noRoom` — куда уйти, если
    * пыль не влезла: на ту же карточку, ничего не потеряв. */
  private def grantDust(user: User, hero: Hero, gem: Gem, renderer: Renderer,
                        andThen: => Task[StateType], noRoom: => Task[StateType]): Task[StateType] =
    for {
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      count  = gem.dustYield
      free   = inv.freeSlots
      res <-
        if (free < count)
          renderer.show(user, Screen(content.format("inventory.breakGemNoRoom", "count" -> count.toString), Nil)) *>
            noRoom
        else
          ZIO.foreachDiscard(1 to count) { _ =>
            itemRepository.persist(hero.id, MaterialGenerator.item(gem.dust))
              .flatMap(persisted => inventoryRepo.addItem(hero.id, persisted).mapError(e => new Throwable(e.toString)))
          } *>
            renderer.show(user, Screen(content.format("inventory.breakGemDone",
              "gem" -> gem.displayName, "count" -> count.toString, "dust" -> gem.dust.displayName), Nil)) *>
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

  // ── Пыль на оружие ─────────────────────────────────────────────────────────

  /** Посыпает надетое оружие выбранной пылью. Горсть тратится в любом случае —
    * даже когда магия даёт всполох или покрытие идёт четвёртым слоем. */
  private def sprinkleDust(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      item.material.filter(_.gem.isDefined) match {
        case None => showList(user, renderer)
        case Some(_) if hero.equipment.unarmed =>
          renderer.show(user, Screen(content.text("inventory.dustNoWeapon"), Nil)) *> backToCard(user, renderer)
        case Some(dust) =>
          val outcome = WeaponDust.sprinkle(dust, hero.weaponDust, hero.equipment.handGems)
          val line = outcome match {
            // Сколько слоёв уже на оружии, игрок держит в голове сам — не считаем за него.
            case WeaponDust.Outcome.Applied(_) =>
              content.format("inventory.dustApplied",
                "dust"   -> dust.displayName,
                "weapon" -> hero.equipment.mainWeapon.name)
            case WeaponDust.Outcome.Clash(_) =>
              content.format("inventory.dustClash", "dust" -> dust.displayName,
                "pct" -> WeaponDust.PenaltyPct.toString)
            case WeaponDust.Outcome.Overload(_) =>
              content.format("inventory.dustOverload", "pct" -> WeaponDust.PenaltyPct.toString)
          }
          inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
            heroDao.updateWeaponDust(user.userId, outcome.next) *>
            renderer.show(user, Screen(line, Nil)) *>
            backToCard(user, renderer)
      }
    }

  /** Положить расходник на пояс: фляга — со своими зарядами, роза — со своими
    * раскрытиями, пыль, отвар и святая вода — стопкой: из сумки уходит сразу
    * столько таких же, сколько влезает в ячейку (см.
    * [[ItemDetails.Belt.stackFor]]), а неполная ячейка того же вида сперва
    * доливается. Ячеек у пояса столько, сколько раньше вмещалось склянок (см.
    * [[ItemDetails.Belt.capacityFor]]); нет пояса — вещь остаётся в сумке, нет
    * места — спрашиваем, что снять ([[beltSwapScreen]]). */
  private def putOnBelt(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      val belt  = hero.equipment.belt
      val cap   = ItemDetails.Belt.capacityFor(belt.rarity)
      val stack = ItemDetails.Belt.stackFor(belt.rarity)
      (BeltSlot.of(item), belt.details) match {
        case (None, _) => showList(user, renderer)
        case (Some(_), _) if belt.itemType != ItemType.Belt =>
          renderer.show(user, Screen(content.text("inventory.beltNone"), Nil)) *> backToCard(user, renderer)
        case (Some(unit: BeltSlot.Stack), b: ItemDetails.Belt) =>
          inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
            val same = sameInBag(inv.items.data, unit, item)
            b.putStack(unit, same.size, cap, stack) match {
              case None =>
                renderer.show(user, beltSwapScreen(unit, b)).as(StateType.Inventory)
              case Some((filled, n)) =>
                // В той ячейке, куда легло: она одна изменилась (или встала новой).
                val in = filled.slots.zipWithIndex.collectFirst {
                  case (s: BeltSlot.Stack, i) if s.sameKind(unit) && !b.slots.lift(i).contains(s) => s.count
                }
                inventoryRepo.removeItems(same.take(n).map(_.id).toSet, hero.id).mapError(e => new Throwable(e.toString)) *>
                  heroDao.updateEquipment(user.userId,
                    hero.equipment.copy(belt = belt.copy(details = filled))) *>
                  renderer.show(user, Screen(content.format("inventory.beltPutStack",
                    "name" -> unit.name, "count" -> n.toString,
                    "have" -> in.getOrElse(n).toString, "stack" -> stack.toString), Nil)) *>
                  backToCard(user, renderer)
            }
          }
        case (Some(slot), b: ItemDetails.Belt) =>
          b.put(slot, cap) match {
            case None =>
              renderer.show(user, beltSwapScreen(slot, b)).as(StateType.Inventory)
            case Some(filled) =>
              inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
                heroDao.updateEquipment(user.userId,
                  hero.equipment.copy(belt = belt.copy(details = filled))) *>
                renderer.show(user, Screen(content.format("inventory.beltPutDone",
                  "name" -> slot.name, "used" -> filled.slots.size.toString,
                  "cap" -> cap.toString), Nil)) *>
                backToCard(user, renderer)
          }
        case _ => showList(user, renderer)
      }
    }

  /** Такие же вещи в сумке, что и стопка `unit`, — выбранная первой: её-то
    * игрок и кладёт, остальные идут следом. */
  private def sameInBag(bag: List[Item], unit: BeltSlot.Stack, picked: Item): List[Item] = {
    val same = bag.filter(i => BeltSlot.of(i).exists(unit.sameKind))
    same.filter(_.id == picked.id) ++ same.filterNot(_.id == picked.id)
  }

  /** Ляжет ли вещь на пояс, ничего не снимая: есть свободная ячейка или — для
    * стопки — неполная ячейка того же вида. */
  private def fitsOnBelt(slot: BeltSlot, b: ItemDetails.Belt, rarity: pangea.model.item.Rarity): Boolean =
    slot match {
      case unit: BeltSlot.Stack =>
        b.putStack(unit, 1, ItemDetails.Belt.capacityFor(rarity), ItemDetails.Belt.stackFor(rarity)).isDefined
      case _ => b.slots.sizeIs < ItemDetails.Belt.capacityFor(rarity)
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

  /** Положить выбранную вещь на пояс вместо ячейки `slot`: снятое уходит в сумку
    * вещами — стопка вся, сколько в ней осталось (пустой ячейке возвращать
    * нечего); новая стопка набирается из сумки до полной. Сумке должно хватить
    * места: взятое освобождает своё, снятое занимает; пыли — не больше сотни
    * горстей. Место на поясе успело найтись — вещь просто ложится на пояс. */
  private def swapOnBelt(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      val belt  = hero.equipment.belt
      val stack = ItemDetails.Belt.stackFor(belt.rarity)
      (BeltSlot.of(item), belt.details, payloadField(ua, "slot").flatMap(_.toIntOption)) match {
        case (Some(slot), b: ItemDetails.Belt, _) if belt.itemType == ItemType.Belt && fitsOnBelt(slot, b, belt.rarity) =>
          putOnBelt(user, renderer)
        case (Some(slot), b: ItemDetails.Belt, Some(idx)) if belt.itemType == ItemType.Belt =>
          b.at(idx) match {
            case None => backToCard(user, renderer)
            case Some(old) =>
              val back = BeltContents.items(old)
              inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
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
                    .map(InventoryFeedback.refusalLine(content, _)).getOrElse(content.text("common.swapNoRoom")), Nil)) *>
                    backToCard(user, renderer)
                else
                  for {
                    // Место есть — теперь и сохраняем снятое (см. BeltContents.persist).
                    saved <- BeltContents.persist(Some(itemRepository), hero.id, back)
                    _     <- inventoryRepo.removeItems(taken.map(_.id).toSet, hero.id).mapError(e => new Throwable(e.toString))
                    _     <- heroDao.updateEquipment(user.userId,
                               hero.equipment.copy(belt = belt.copy(details = b.updated(idx, put))))
                    _     <- ZIO.foreachDiscard(saved)(r => inventoryRepo.addItem(hero.id, r).mapError(e => new Throwable(e.toString)))
                    name   = BeltContents.names(taken)
                    _     <- renderer.show(user, Screen(
                               if (back.isEmpty) content.format("inventory.beltSwappedEmpty", "name" -> name, "old" -> old.name)
                               else content.format("inventory.beltSwapped", "name" -> name, "old" -> BeltContents.names(back)), Nil))
                    res   <- backToCard(user, renderer)
                  } yield res
              }
          }
        case _ => backToCard(user, renderer)
      }
    }

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
    case Brewed(kind)          => brewText(content, item, kind)
    case _                     => itemText(item, hero.equipment, Some(hero))
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
      case Some(QuestItemKind.BarrelClue) =>
        val now =
          if (BarrelThievesQuest.onTrail(quests))
            content.format("barrelThieves.card.onTrail", "gang" -> BarrelThievesQuest.gangName(content, quests))
          else if (BarrelThievesQuest.askAround(quests)) content.text("barrelThieves.askAround")
          else ""
        s"${item.displayTitle}\n${difficulty(heroLvl.toInt)}\n\n${QuestItemKind.BarrelClue.description}" +
          (if (now.isEmpty) "" else "\n\n" + now)
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
    s"${item.displayTitle}\n${gearBody(item, hero)}$slotInfo"
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
