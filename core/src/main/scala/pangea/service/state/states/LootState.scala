package pangea.service.state.states

import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Journal, Renderer, SceneContent, Screen, Target}
import pangea.model.GameEvent
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{Item, ItemType}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.repository.artifact.ArtifactRepository
import pangea.service.artifact.{ArtifactIntake, Intake}
import pangea.service.parcel.Parcels
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{InventoryFeedback, ItemMenu, QuestSupport, SlotChoice, State, StoryGearPost, TakeAndEquip,
  UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Промежуточный экран добычи после победы. `BattleState.victory` уже прокатал лут
 * (чистый `LootGenerator`) и положил его в scene_data; здесь мы «осматриваем добычу»:
 * показываем, что выпало, и двумя кнопками спрашиваем
 * «Забрать»/«Оставить». Забрать → серебро в кошелёк, предметы в инвентарь (переполнен →
 * предмет теряется). Оставить → добыча выбрасывается. Оба исхода ведут в Dungeon.
 * Если что-то из добычи встаёт в пустой слот, есть и третья кнопка — «Забрать и
 * надеть»: такие вещи сразу на героя, мимо сумки (см. [[TakeAndEquip]]). На
 * каждую вещь, которая встанет вместо надетой, — своя кнопка «Надеть: …»: всё
 * забирается, эта вещь надевается, снятое уходит в сумку.
 *
 * После группового боя добыча идёт по одному павшему за экран (`queue`): каждый
 * исход показывает следующего, пока очередь не опустеет, и только тогда — уход.
 * Павшие без вещей — одной сводкой в самом начале: кого обыскали и сколько
 * нашли на всех (см. [[showSearched]]); по одному идут только те, у кого вещи.
 *
 * Над «Оставить» — «🎒 Рюкзак»: заглянуть в сумку, освободить место и вернуться
 * к той же добыче (см. [[openBackpack]]).
 *
 * Сюжетную экипировку (см. `Item.isStoryGear`) не теряют: не влезла в сумку —
 * уходит посылкой (`parcels`, см. [[StoryGearPost]]), а «Оставить» её не
 * бросает — она идёт с героем, как при «Забрать».
 */
case class LootState(
  heroDao:             HeroDao,
  inventoryRepository: InventoryRepository,
  itemRepository:      ItemRepository,
  journal:             Journal,
  content:             SceneContent,
  artifacts:           Option[ArtifactRepository] = None,
  parcels:             Option[Parcels] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "Take"     -> Target.Run { (user, _, renderer) => claimLoot(user, renderer, wear = false) },
      "TakeWear" -> Target.Run { (user, _, renderer) => claimLoot(user, renderer, wear = true) },
      // «Надеть: …» — и кнопки выбора кольца или руки после него
      "TakeSwap" -> Target.Run { (user, ua, renderer) => swapLoot(user, ua, renderer) },
      "SwapBack" -> Target.Run { (user, _, renderer) => showPreview(user, renderer) },
      "Leave"    -> Target.Run { (user, _, renderer) => leaveLoot(user, renderer) },
      "Backpack" -> Target.Run { (user, _, _) => openBackpack(user) },
      "Continue" -> Target.Run { (user, _, renderer) => readLoot(user).flatMap(finish(user, _, renderer)) }
    ),
    // Написал текст или нажал старую кнопку — добычу не пропускаем: тот же
    // экран ещё раз, пока не нажата одна из его кнопок (см. repeat).
    fallback = Target.Run { (user, _, renderer) => repeat(user, renderer) }
  )

  // Добыча этого павшего разобрана. Если в очереди есть следующий — его экран
  // (в то же состояние переход не ведёт, поэтому `enter` зовём сами). Иначе —
  // куда уйти после добычи: экран сам рулит возвратом. Перед переходом кладём
  // eventData в scene_data (чтобы состояние-получатель прочитало свой прогресс),
  // иначе чистим. По умолчанию — обратно в лабиринт.
  private def finish(user: User, loot: LootData, renderer: Renderer): Task[StateType] =
    loot.queue match {
      case Nil =>
        heroDao.writeSceneData(user.userId, loot.eventData.getOrElse(Json.Null))
          .as(loot.returnState.getOrElse(StateType.Dungeon))
      case next :: rest =>
        val following = loot.copy(
          items = next.items, silvers = next.silvers, doubloons = next.doubloons,
          monsterName = Some(next.monsterName), queue = rest, credited = false)
        heroDao.writeSceneData(user.userId, following.asJson) *> enter(user, renderer).as(StateType.Loot)
    }

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.GlobalMap, StateType.TreasureMobsFight, StateType.TreasureSchron, StateType.Loot, StateType.Girl,
        StateType.MarisaHunt, StateType.FlowerMeadow, StateType.MonsterCave, StateType.Caravan,
        StateType.MurlocElder, StateType.MurlocVillage, StateType.Home, StateType.SquadMine, StateType.StrayCasket,
        StateType.HedgeSong, StateType.BarrelReturn, StateType.StoryCache, StateType.StoryWarehouse, StateType.StoryBridge,
        StateType.StoryBindery, StateType.StoryGraveyard, StateType.StoryDen, StateType.StoryCellar, StateType.BartiHouse, StateType.BartiShop, StateType.StorySquare,
        StateType.BartiMeet, StateType.BartiCaravan, StateType.Construction,
        StateType.JorgenLair, StateType.BramblHall, StateType.PlukhHunt, StateType.FaelinRoots,
        StateType.Village, StateType.VillageBandits, StateType.VillageSpiders, StateType.VillageCaravan,
        StateType.VillageTrail, StateType.Inventory)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)
      _    <- LootState.searched(loot).fold(show(user, renderer, hero, loot))(showSearched(user, renderer, hero, _))
    } yield ()

  /** Экран одной добычи: деньги — в кошелёк, вещей нет — сообщение, есть —
    * превью с кнопками. */
  private def show(user: User, renderer: Renderer, hero: Hero, loot: LootData): Task[Unit] = {
    // серебро и дублоны забираются всегда и сразу, без выбора — но один раз:
    // из рюкзака игрок возвращается сюда же, а деньги уже в кошельке
    val silverTotal = if (loot.credited) 0L else silvers(hero, loot.silvers).sum
    val doubloons   = if (loot.credited) 0L else loot.doubloons
    for {
      _ <- ZIO.when(silverTotal > 0L || doubloons > 0L)(
             heroDao.writeSceneData(user.userId, loot.copy(credited = true).asJson))
      _ <- ZIO.when(silverTotal > 0L)(heroDao.updateSilver(user.userId, hero.silver + silverTotal))
      _ <- ZIO.when(doubloons > 0L)(heroDao.updateDoubloons(user.userId, hero.doubloons + doubloons))
      _ <- if (loot.items.isEmpty) {
             // выбирать нечего — только серебро/дублоны (или совсем пусто)
             val text = noItemsText(loot, currencyLines(hero, loot))
             journal.append(GameEvent(user.userId, "loot_claimed",
               Json.obj("silver" -> silverTotal.asJson, "items" -> loot.items.map(_.name).asJson))) *>
               // В группе, пока павшие ещё есть, решать нечего — сообщение и сразу следующий.
               (if (loot.queue.nonEmpty) renderer.show(user, Screen(text, Nil)) *> finish(user, loot, renderer).unit
                else renderer.show(user, Screen(text, content.screen("loot.enter").choices)))
           } else
             // серебро/дублоны уже в кошельке; по предметам спрашиваем
             renderer.show(user, previewScreen(hero, loot))
    } yield ()
  }

  /** После группового боя павшие без вещей — одной сводкой в начале: кого
    * обыскали и сколько нашли на всех (см. [[LootState.searched]]). Их деньги —
    * сразу в кошелёк. Дальше — первый павший с вещами своим экраном, как и
    * раньше; таких нет — сводка ждёт «Продолжить». */
  private def showSearched(user: User, renderer: Renderer, hero: Hero, s: LootState.Searched): Task[Unit] = {
    val silver    = s.credit.map(m => silvers(hero, m.silvers).sum).sum
    val doubloons = s.credit.map(_.doubloons).sum
    val text      = noItemsText(s.summary, currencyLines(hero, s.summary))
    for {
      // Сведённых в scene_data больше нет ещё до начисления: повторный вход
      // их деньги второй раз не даст.
      _ <- heroDao.writeSceneData(user.userId, s.next.getOrElse(s.summary).asJson)
      _ <- ZIO.when(silver > 0L)(heroDao.updateSilver(user.userId, hero.silver + silver))
      _ <- ZIO.when(doubloons > 0L)(heroDao.updateDoubloons(user.userId, hero.doubloons + doubloons))
      _ <- ZIO.foreachDiscard(s.credit)(m => journal.append(GameEvent(user.userId, "loot_claimed",
             Json.obj("silver" -> silvers(hero, m.silvers).sum.asJson, "items" -> List.empty[String].asJson))))
      _ <- s.next match {
             // деньги сводки уже в кошельке — следующему экрану нужен свежий герой
             case Some(next) =>
               renderer.show(user, Screen(text, Nil)) *> getHero(user).flatMap(show(user, renderer, _, next))
             case None => renderer.show(user, Screen(text, content.screen("loot.enter").choices))
           }
    } yield ()
  }

  /** Экран добычи без вещей: только серебро и дублоны — или совсем пусто. У
    * сводки — кого обыскали: один — как павший в группе, больше — списком
    * (см. [[LootState.bodies]]). */
  private def noItemsText(loot: LootData, currency: List[String]): String = {
    val (found, nothing) = loot.searched match {
      case Nil =>
        (header(loot), loot.monsterName.fold(content.text("loot.empty"))(n => content.format("loot.groupEmpty", "monster" -> n)))
      case one :: Nil =>
        (content.format("loot.groupHeader", "monster" -> one), content.format("loot.groupEmpty", "monster" -> one))
      case many =>
        val names = LootState.bodies(many)
        (content.format("loot.searchedHeader", "monsters" -> names), content.format("loot.searchedEmpty", "monsters" -> names))
    }
    if (currency.nonEmpty) found + "\n\n" + currency.mkString("\n") else nothing
  }

  /** Тот же экран добычи ещё раз — на посторонний ввод: текст, чужая или старая
    * кнопка. Вещи ждут решения, пока игрок не нажмёт «Забрать», «Оставить» или
    * «Продолжить»; серебро второй раз не начисляется. */
  private def repeat(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)
      _    <- if (loot.items.nonEmpty) renderer.show(user, previewScreen(hero, loot))
              else renderer.show(user, Screen(noItemsText(loot, currencyLines(hero, loot)), content.screen("loot.enter").choices))
    } yield StateType.Loot

  /** Серебро с добычи: «Спаситель Марисы» получает на десятую больше. */
  private def silvers(hero: Hero, drops: List[Long]): List[Long] =
    drops.map(_ * Achievement.silverPct(hero) / 100L)

  private def currencyLines(hero: Hero, loot: LootData): List[String] = {
    val found = silvers(hero, loot.silvers)
    // У сводки обысканных серебро одним числом на всех, у павшего — по выпадениям.
    val shown = if (loot.searched.nonEmpty) List(found.sum).filter(_ > 0L) else found
    shown.map(s => content.format("loot.silver", "amount" -> s.toString)) ++
      (if (loot.doubloons > 0L) List(content.format("loot.doubloons", "amount" -> loot.doubloons.toString)) else Nil)
  }

  // В группе над добычей стоит имя павшего — чтобы было видно, чья она.
  private def header(loot: LootData): String = loot.header.getOrElse(loot.monsterName match {
    case Some(name) => content.format("loot.groupHeader", "monster" -> name)
    case None       => content.text("loot.header")
  })

  /** Превью добычи: валюта, вещи с надетым для сравнения и кнопки. Валюта уже в
    * кошельке, строки о ней — только для глаз, поэтому превью можно показать ещё
    * раз ([[showPreview]]): после «Назад» с выбора кольца или отказа «сумка полна».
    * Сверху «Забрать», под ним «Забрать и надеть» — если что-то встаёт в пустой
    * слот — и «Надеть: …» на каждую вещь, которая встанет вместо надетой;
    * последней — «Оставить», над ней — «🎒 Рюкзак». */
  private def previewScreen(hero: Hero, loot: LootData): Screen = {
    val preview = currencyLines(hero, loot) ++ loot.items.map(it => ItemMenu.withEquipped(itemLine(it), it, hero))
    val wearAll = Option.when(TakeAndEquip.pick(hero, loot.items).nonEmpty)(content.choice("TakeWear", "loot.takeWearLabel"))
    val swaps   = loot.items.zipWithIndex.collect { case (item, i) if TakeAndEquip.replaces(hero, item) =>
      content.choice("TakeSwap", "loot.swapLabel", "name" -> item.name).copy(data = swapData(i, item))
    }
    Screen(header(loot) + "\n\n" + preview.mkString("\n"),
      List(content.choice("Take", "loot.takeLabel")) ++ wearAll.toList ++ swaps ++
        List(content.choice("Backpack", "loot.backpackLabel"), content.choice("Leave", "loot.leaveLabel")))
  }

  /** Показать превью ещё раз — без начисления серебра. Вещей уже нет — выбирать
    * нечего, идём дальше. */
  private def showPreview(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)
      next <- if (loot.items.isEmpty) finish(user, loot, renderer)
              else renderer.show(user, previewScreen(hero, loot)).as(StateType.Loot)
    } yield next

  /** «🎒 Рюкзак»: заглянуть в сумку, не бросая добычу, — сразу в инвентарь, и у
    * владельца хранилищ тоже. Добыча ждёт в scene_data — сумка держит там только
    * свои ключи. return_state — Loot: по нему выход из сумки ведёт обратно сюда,
    * а карты из сумки не уводят в путь (они только из города). Вернувшись, игрок
    * видит ту же добычу: `enter` покажет её снова, а деньги второй раз не
    * начислит (`credited`). */
  private def openBackpack(user: User): Task[StateType] =
    heroDao.writeReturnState(user.userId, Some(StateType.Loot)).as(StateType.Inventory)

  /** Данные кнопки «Надеть: …»: номер вещи и её имя — старая кнопка не наденет
    * чужую вещь, если добыча на экране уже другая. */
  private def swapData(i: Int, item: Item): Map[String, String] = Map("i" -> i.toString, "n" -> item.name)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // «Забрать»: предметы в инвентарь (переполнен → предмет теряется). Серебро уже забрано в enter.
  // «Забрать и надеть» (`wear`): что встаёт в пустой слот, сразу на героя, мимо сумки.
  private def claimLoot(user: User, renderer: Renderer, wear: Boolean): Task[StateType] =
    for {
      hero <- getHero(user)
      loot <- readLoot(user)
      next <- claim(user, renderer, hero, loot, if (wear) TakeAndEquip.pick(hero, loot.items) else Nil, slot = None)
    } yield next

  // «Надеть: X»: всё забираем, а X — вместо надетой. Оба кольца или обе руки
  // заняты — сперва спрашиваем, что снять (та же кнопка, но с номером слота);
  // снятому нет места в сумке — не надеваем и показываем добычу снова. Кнопка со
  // старого экрана, где вещь уже другая, тоже просто показывает добычу.
  private def swapLoot(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      loot  <- readLoot(user)
      inv   <- inventoryRepository.get(hero.id).mapError(e => new Throwable(e.toString))
      slot   = payload(ua, "slot").flatMap(_.toIntOption).filter(s => s == 1 || s == 2)
      chosen = payload(ua, "i").flatMap(_.toIntOption).flatMap(i => loot.items.lift(i).map(i -> _))
                 .filter { case (_, item) => payload(ua, "n").contains(item.name) && TakeAndEquip.replaces(hero, item) }
      next  <- chosen match {
                 case None => showPreview(user, renderer)
                 case Some((i, item)) if slot.isEmpty && SlotChoice.needed(hero.equipment, item) =>
                   renderer.show(user, SlotChoice.screen(content, item, hero.equipment, "TakeSwap", swapData(i, item),
                     content.choice("SwapBack", "common.back"))).as(StateType.Loot)
                 case Some((i, item)) =>
                   TakeAndEquip.noRoom(hero, inv, item, slot) match {
                     case Some(key) =>
                       renderer.show(user, Screen(content.format(key, "name" -> item.name), Nil)) *> showPreview(user, renderer)
                     case None => claim(user, renderer, hero, loot, List(i), slot)
                   }
               }
    } yield next

  /** Забрать добычу: вещи с номерами `wearIdx` — сразу на героя (`slot` — для
    * одной вещи, см. `TakeAndEquip.wear`), остальное — в артефакты и сумку. */
  private def claim(user: User, renderer: Renderer, hero: Hero, loot: LootData,
                    wearIdx: List[Int], slot: Option[Int]): Task[StateType] =
    for {
      persisted <- ZIO.foreach(loot.items)(itemRepository.persist(hero.id, _))
      wornItems  = wearIdx.flatMap(persisted.lift)
      // Сперва надеваем: «Тайник» на надетом прибавит сумке места до того, как
      // в неё ляжет остальное, а снятое ляжет раньше добычи — место под него
      // проверено заранее.
      worn      <- TakeAndEquip.wear(heroDao, inventoryRepository, user, hero, wornItems, slot, Some(itemRepository))

      // Камни летят в Ларец Азата, травы и отвары — в Живую сумку; что не
      // взяли артефакты, идёт в сумку героя, как и раньше.
      kept     = persisted.zipWithIndex.collect { case (item, i) if !wearIdx.contains(i) => item }
      results <- ZIO.foreach(kept) { item =>
                   ArtifactIntake.accept(artifacts, inventoryRepository, hero.id, item).map(item -> _)
                 }

      _ <- journal.append(GameEvent(user.userId, "loot_claimed",
             Json.obj("silver" -> loot.silvers.sum.asJson, "items" -> loot.items.map(_.name).asJson,
                      "worn" -> wornItems.map(_.name).asJson, "removed" -> worn.removed.map(_.name).asJson)))

      takenLines = wornItems.map(itemLine) ++ results.collect {
        case (item, Intake.ToInventory)             => itemLine(item)
        case (item, Intake.ToArtifact(kind, free))  => ArtifactIntake.line(content, item, kind, free)
      }
      // Сюжетная экипировка, что не влезла, — посылкой: её не теряют.
      posted    <- post(hero, results.collect { case (item, Intake.Refused) if item.isStoryGear => item })
      // У каждой непринятой вещи своя причина: сумка полна или пыли уже сотня.
      lostLines  = results.collect {
                     case (item, Intake.Refused) if !posted.sent(item) => InventoryFeedback.refusalLine(content, item)
                   }.distinct
      slots     <- InventoryFeedback.freeSlotsLine(inventoryRepository, content, hero.id)
      taken      = if (takenLines.isEmpty) content.text("loot.empty")
                   else content.text("loot.claimed") + "\n\n" + takenLines.mkString("\n")
      wornLine   = if (wornItems.isEmpty) ""
                   else "\n\n" + content.format("loot.worn", "names" -> wornItems.map(_.name).mkString(", ")) +
                     TakeAndEquip.takenOffLine(content, worn)
      full       = (if (lostLines.isEmpty) "" else "\n\n" + lostLines.mkString("\n")) + posted.line.fold("")("\n\n" + _)
      // Добыча разобрана — до показа: ВК может не принять сообщение и после
      // повторов, ход тогда обрывается здесь, а герой остаётся на экране добычи.
      // Повторное «Забрать» выдало бы те же вещи ещё раз; с пустой добычей оно
      // просто доведёт ход до конца — в лабиринт, в событие или к следующему.
      _ <- heroDao.writeSceneData(user.userId, loot.copy(items = Nil).asJson)
      _ <- renderer.show(user, Screen(taken + wornLine + full + "\n\n" + slots, Nil))
      // Двенадцатая вещь набора — за собранный набор своё достижение.
      _ <- QuestSupport.grantFullSets(heroDao, content, user, worn.hero, renderer)
      next <- finish(user, loot, renderer)
    } yield next

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  // «Оставить»: предметы выбрасываются (серебро уже забрано в enter). Сюжетную
  // экипировку не бросают: она идёт с героем — в сумку, а не влезла — посылкой.
  private def leaveLoot(user: User, renderer: Renderer): Task[StateType] =
    for {
      loot <- readLoot(user)
      hero <- getHero(user)
      (story, left) = loot.items.partition(_.isStoryGear)
      _    <- journal.append(GameEvent(user.userId, "loot_left",
                Json.obj("silver" -> loot.silvers.sum.asJson, "items" -> left.map(_.name).asJson)))
      kept <- keepStory(user, hero, loot, story)
      _    <- renderer.show(user, Screen((content.text("loot.left") :: kept).mkString("\n\n"), Nil))
      next <- finish(user, loot, renderer)
    } yield next

  /** Сюжетная экипировка с оставленной добычи: в сумку, не влезла — посылкой.
    * Добыча разобрана до показа, как и в [[claim]]: повторное «Оставить» её
    * второй раз не выдаст. Строки — что куда легло. */
  private def keepStory(user: User, hero: Hero, loot: LootData, story: List[Item]): Task[List[String]] =
    if (story.isEmpty) ZIO.succeed(Nil)
    else
      for {
        persisted <- ZIO.foreach(story)(itemRepository.persist(hero.id, _))
        results   <- ZIO.foreach(persisted)(item =>
                       ArtifactIntake.accept(artifacts, inventoryRepository, hero.id, item).map(item -> _))
        posted    <- post(hero, results.collect { case (item, Intake.Refused) => item })
        _         <- heroDao.writeSceneData(user.userId, loot.copy(items = Nil).asJson)
        bagged     = results.collect { case (item, Intake.ToInventory) => item.displayTitle }
        lost       = results.collect {
                       case (item, Intake.Refused) if !posted.sent(item) => InventoryFeedback.refusalLine(content, item)
                     }.distinct
      } yield Option.when(bagged.nonEmpty)(content.format("loot.storyKept", "items" -> bagged.mkString(", "))).toList ++
        lost ++ posted.line.toList

  /** Сюжетную экипировку, что не влезла в сумку, — посылкой (см.
    * [[StoryGearPost]]). Без почты (в тестах) отправить нечем — вещь тогда
    * пропадает, как и прочая добыча, что не влезла. */
  private def post(hero: Hero, items: List[Item]): Task[LootState.Posted] =
    parcels match {
      case Some(p) if items.nonEmpty =>
        ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
          .flatMap(now => StoryGearPost.send(p, hero, content.text("loot.storyFrom"), items, now))
          .map(line => LootState.Posted(items.map(_.id).toSet, line))
      case _ => ZIO.succeed(LootState.Posted(Set.empty, None))
    }

  private def readLoot(user: User): Task[LootData] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[LootData].toOption).getOrElse(LootData(Nil, Nil)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def itemLine(item: Item): String =
    if (item.itemType == ItemType.Trophy)
      s"🎁 ${item.displayTitle} (трофей)" // у трофеев нет редкости — кружка в заголовке не будет
    else if (item.isNamedTrophy) s"🎁 ${item.displayTitle} (уникальный трофей)"
    else if (item.itemType == ItemType.Gem || item.itemType == ItemType.Material || item.itemType == ItemType.RuneStone)
      s"🎁 ${item.displayTitle}" // описания камня, ингредиента, пыли и рунной плиты при выпадении не показываем — только в инвентаре
    else {
      val lines = item.statsLines
      val tail  = if (lines.isEmpty) "" else "\n" + lines.mkString("\n")
      s"🎁 ${item.displayTitle}$tail"
    }
}

object LootState {

  /** Что из сюжетной экипировки ушло посылкой (номера) и строка о том, куда. */
  final case class Posted(ids: Set[Long], line: Option[String]) {
    def sent(item: Item): Boolean = ids.contains(item.id)
  }
  // Содержимое scene_data между victory и экраном добычи: непросохранённые предметы
  // (id = -1) и список серебряных выпадений. Плюс обобщённый «роутинг»:
  //   - doubloons   — дублоны к выдаче (премиум-валюта схрона);
  //   - returnState — куда уйти после добычи (default Dungeon); экран сам рулит;
  //   - eventData   — непрозрачный блоб для состояния-получателя (его пишут в
  //                   scene_data перед переходом, чтобы событие прочитало свой
  //                   прогресс — напр. ChainData цепочки боёв);
  //   - monsterName — чья это добыча (только после группового боя);
  //   - queue       — добыча остальных павших, по одному экрану на каждого;
  //   - won         — это добыча за ПОБЕДУ (пишет BattleState.applyVictory), а не
  //                   роутинг, положенный сценой перед боем. По ней бой узнаёт
  //                   свою же не доехавшую до экрана победу (см. BattleState.recover);
  //   - credited    — серебро и дублоны этого павшего уже в кошельке: из рюкзака
  //                   игрок возвращается на тот же экран, и второй раз их не дают;
  //   - searched    — это сводка обысканных после группового боя: кто (по имени
  //                   на павшего); их серебро и дублоны — в silvers и doubloons.
  final case class LootData(
    items:       List[Item],
    silvers:     List[Long],
    doubloons:   Long              = 0L,
    returnState: Option[StateType] = None,
    eventData:   Option[Json]      = None,
    monsterName: Option[String]    = None,
    queue:       List[MonsterLoot] = Nil,
    won:         Boolean           = false,
    // Свой заголовок экрана — у находки не с тела (хлам в домике).
    header:      Option[String]    = None,
    credited:    Boolean           = false,
    searched:    List[String]      = Nil
  )
  object LootData {
    implicit val encoder: Encoder[LootData] = deriveEncoder[LootData]
    // Рукописный декодер: добыча лежит в scene_data, и новое поле не должно
    // стирать уже накатанную (см. память о производных декодерах).
    implicit val decoder: Decoder[LootData] = (c: HCursor) =>
      for {
        items       <- c.get[List[Item]]("items")
        silvers     <- c.get[List[Long]]("silvers")
        doubloons   <- c.getOrElse[Long]("doubloons")(0L)
        returnState <- c.getOrElse[Option[StateType]]("returnState")(None)
        eventData   <- c.getOrElse[Option[Json]]("eventData")(None)
        monsterName <- c.getOrElse[Option[String]]("monsterName")(None)
        queue       <- c.getOrElse[List[MonsterLoot]]("queue")(Nil)
        won         <- c.getOrElse[Boolean]("won")(false)
        header      <- c.getOrElse[Option[String]]("header")(None)
        credited    <- c.getOrElse[Boolean]("credited")(false)
        searched    <- c.getOrElse[List[String]]("searched")(Nil)
      } yield LootData(items, silvers, doubloons, returnState, eventData, monsterName, queue, won, header, credited,
          searched)
  }

  /** Сводка групповой добычи (см. LootState.showSearched): `summary` — сама
    * сводка, кого обыскали и всё их серебро с дублонами (уже в кошельке —
    * `credited`); `credit` — чьи деньги начислить сейчас; `next` — первый
    * павший с вещами, с него экран идёт дальше, за ним — остальные с вещами. */
  final case class Searched(summary: LootData, credit: List[MonsterLoot], next: Option[LootData])

  /** Павших без вещей — в сводку. None — сводить нечего: добыча не с группы
    * (имени павшего нет — и у самой сводки его нет) или вещи у всех. */
  def searched(loot: LootData): Option[Searched] =
    loot.monsterName.flatMap { name =>
      val own            = MonsterLoot(name, loot.items, loot.silvers, loot.doubloons)
      val (empty, found) = (own :: loot.queue).partition(_.items.isEmpty)
      Option.when(empty.nonEmpty) {
        val summary = loot.copy(items = Nil, silvers = empty.flatMap(_.silvers),
          doubloons = empty.map(_.doubloons).sum, monsterName = None, queue = Nil,
          searched = empty.map(_.monsterName), credited = true)
        val next = found match {
          // Первым с вещами идёт сам этот павший, если вещи у него, — и его
          // деньги могли уже лечь в кошелёк.
          case first :: rest => Some(loot.copy(items = first.items, silvers = first.silvers,
            doubloons = first.doubloons, monsterName = Some(first.monsterName), queue = rest,
            credited = loot.credited && loot.items.nonEmpty))
          case Nil => None
        }
        // Без вещей этот павший — первый в сводке; его деньги могли лечь раньше.
        Searched(summary, if (loot.credited && loot.items.isEmpty) empty.drop(1) else empty, next)
      }
    }

  /** Кого обыскали: одинаковые — одной записью со счётом («Орк раб ×5»), в
    * порядке гибели. */
  def bodies(names: List[String]): String =
    names.distinct.map { n =>
      val k = names.count(_ == n)
      if (k > 1) s"$n ×$k" else n
    }.mkString(", ")

  /** Добыча с одного павшего в группе: ждёт своей очереди на экран. */
  final case class MonsterLoot(monsterName: String, items: List[Item], silvers: List[Long], doubloons: Long)
  object MonsterLoot {
    implicit val encoder: Encoder[MonsterLoot] = deriveEncoder[MonsterLoot]
    implicit val decoder: Decoder[MonsterLoot] = (c: HCursor) =>
      for {
        monsterName <- c.getOrElse[String]("monsterName")("")
        items       <- c.getOrElse[List[Item]]("items")(Nil)
        silvers     <- c.getOrElse[List[Long]]("silvers")(Nil)
        doubloons   <- c.getOrElse[Long]("doubloons")(0L)
      } yield MonsterLoot(monsterName, items, silvers, doubloons)
  }
}
