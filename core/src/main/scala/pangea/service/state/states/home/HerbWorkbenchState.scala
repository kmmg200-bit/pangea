package pangea.service.state.states.home

import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.Hero
import pangea.model.home.{HomeData, HomeRates}
import pangea.model.item.{BrewKind, Item, ItemStack, MaterialKind}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.repository.item.ItemRepository
import pangea.service.home.Cauldron
import pangea.service.state.{HerbLore, ItemMenu, State, UiScene, UserAction}
import pangea.service.storage.ItemStore
import zio.{Task, ZIO}

/** Верстак травника — со второго дома. Полки под травы (всего
  * [[HomeData.herbShelves]], не по видам), котелок на [[HomeRates.CauldronSlots]]
  * мест и полка готовых отваров на [[HomeData.brewShelves]]: на острове и с
  * сундуками аптекаря полки шире.
  *
  * Травы ложатся на полки одной кнопкой — из сумки и из Живой сумки, пока есть
  * место. Траву с полки можно бросить в котелок или забрать в сумку — по одной
  * кнопкой или сразу сколько написано числом. Котелок
  * варит, как магия Живой сумки, и тратит энергию дома — по одной на каждую
  * варку (общий с Mini App [[Cauldron]]). Сваренное встаёт на полку отваров,
  * что не поместилось — ждёт в котелке. На полке отваров — то же: сложить все из сумки и Живой сумки,
  * забрать в сумку — по одной кнопкой или сразу сколько написано числом, —
  * бросить в котелок основой для перегонки. Всё живёт в `home_data`.
  *
  * «Изученные» — всё, что герой уже варил (общий счёт «Зельевара»: котелок,
  * куб Азата, Живая сумка) и что варится на верстаке. Выбрал отвар — пишет
  * числом, сколько приготовить: хватает трав на полках и энергии — варится
  * сразу, нет — верстак говорит, чего и сколько не хватает. Чего верстак ждёт
  * числом — сварить отвар, забрать траву с полок или отвар с полки, — лежит в
  * `scene_data` ([[HerbWorkbenchState.WbScene]]). */
case class HerbWorkbenchState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  artifacts:     Option[ArtifactRepository] = None
) extends State {

  private val stow     = HomeStow(inventoryRepo, artifacts, content)
  private val cauldron = new Cauldron(heroDao, inventoryRepo, itemRepo, content)

  private val branch = new Branch(
    routes = Map(
      "WbMenu"      -> Target.Run { (u, ua, r) => menu(u, page(ua), r) },
      "WbHerb"      -> Target.Run { (u, ua, r) => herb(u, ua, r) },
      "WbHerbPot"   -> Target.Run { (u, ua, r) => herbToPot(u, ua, r) },
      "WbHerbBag"   -> Target.Run { (u, ua, r) => herbToBag(u, ua, r) },
      "WbPutHerbs"  -> Target.Run { (u, _, r) => putHerbs(u, r) },
      "WbPot"       -> Target.Run { (u, _, r) => pot(u, r) },
      "WbCook"      -> Target.Run { (u, _, r) => cook(u, r) },
      "WbEmptyPot"  -> Target.Run { (u, _, r) => emptyPot(u, r) },
      "WbBrews"     -> Target.Run { (u, ua, r) => brews(u, page(ua), r) },
      "WbBrew"      -> Target.Run { (u, ua, r) => brew(u, ua, r) },
      "WbBrewBag"   -> Target.Run { (u, ua, r) => brewToBag(u, ua, r) },
      "WbBrewPot"   -> Target.Run { (u, ua, r) => brewToPot(u, ua, r) },
      "WbPutBrews"  -> Target.Run { (u, _, r) => putBrews(u, r) },
      "WbKnown"     -> Target.Run { (u, ua, r) => known(u, page(ua), r) },
      "WbRecipe"    -> Target.Run { (u, ua, r) => recipeOf(u, ua, r) },
      "LeaveWb"     -> Target.Goto(StateType.Home)
    ),
    // Текст — число, если верстак его ждёт (сварить изученный, забрать с полок);
    // иначе — к верстаку.
    fallback = Target.Run { (u, ua, r) => onText(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    clearPending(user) *> menu(user, 0, renderer).unit

  /** Любая кнопка, кроме выбора отвара, снимает ожидание числа: иначе число,
    * написанное потом у полок, сварило бы отвар, о котором уже забыли. Экраны
    * стопок — на полках трав и на полке отваров — ставят ожидание заново,
    * когда показываются. */
  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    ZIO.when(ua.payload.isDefined && !payload(ua, "action").contains("WbRecipe"))(clearPending(user)) *>
      branch.act(user, ua, renderer)

  // ── Полки трав ──────────────────────────────────────────────────────────────

  /** Верстак: что на полках, сколько в котелке, энергия дома; травы — стопками
    * по страницам. */
  private def menu(user: User, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      val text = content.format("home.workbench.text",
        "herbs" -> home.herbs.size.toString, "herbsMax" -> home.herbShelves.toString,
        "pot" -> home.cauldron.size.toString, "potMax" -> HomeRates.CauldronSlots.toString,
        "brewsMax" -> home.brewShelves.toString,
        "charges" -> home.charges.toString, "chargesMax" -> home.altarMax.toString)
      val tail = List(
        Choice("WbPutHerbs", content.text("home.workbench.putHerbs"), color = ChoiceColor.Positive, row = Some(ItemMenu.NavRow + 1)),
        Choice("WbPot", content.text("home.workbench.pot"), row = Some(ItemMenu.NavRow + 1)),
        Choice("WbBrews", content.text("home.workbench.brews"), row = Some(ItemMenu.NavRow + 1)),
        Choice("WbKnown", content.text("home.workbench.known"), row = Some(ItemMenu.NavRow + 1)),
        Choice("LeaveWb", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow + 1))
      )
      renderer.show(user, Screen(text, stacks(ItemMenu.byName(home.herbs), p, "WbHerb", "WbMenu") ++ tail))
        .as(StateType.HerbWorkbench)
    }

  private def herb(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.herbs, ua) match {
        case None     => menu(user, page(ua), renderer)
        case Some(it) => herbScreen(user, home, it, page(ua), renderer)
      }
    }

  /** Трава на полках: сколько её, «В котелок» и «В сумку» по одной. Её больше
    * одной — ждём ещё и числа: сколько забрать в сумку разом (см.
    * [[takeHerbs]]). */
  private def herbScreen(user: User, home: HomeData, it: Item, p: Int, renderer: Renderer): Task[StateType] = {
    val n    = same(home.herbs, it)
    val data = Map("id" -> it.id.toString, "p" -> p.toString)
    val text = content.format("home.workbench.herb", "name" -> it.name, "n" -> n.toString) +
      (if (n > 1) "\n" + content.text("home.workbench.herbAsk") else "")
    val pending = it.material.filter(_ => n > 1) match {
      case Some(kind) => writeScene(user, HerbWorkbenchState.WbScene(herb = Some(kind.entryName), page = p))
      case None       => clearPending(user)
    }
    pending *> renderer.show(user, Screen(text, List(
      Choice("WbHerbPot", content.text("home.workbench.toPot"), color = ChoiceColor.Positive, data = data, row = Some(0)),
      Choice("WbHerbBag", content.text("home.workbench.toBag"), data = data, row = Some(0)),
      Choice("WbMenu", content.text("home.house.back"), color = ChoiceColor.Negative, data = Map("p" -> p.toString), row = Some(1))
    ))).as(StateType.HerbWorkbench)
  }

  /** Стопка этой травы — если на полках она ещё есть; нет — сам верстак. */
  private def herbOf(user: User, kind: MaterialKind, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      home.herbs.findLast(_.material.contains(kind)) match {
        case None     => menu(user, p, renderer)
        case Some(it) => herbScreen(user, home, it, p, renderer)
      }
    }

  /** Забрать с полок в сумку `asked` трав этого вида — сколько написали числом.
    * Больше, чем лежит, — сколько есть; в сумке тесно — сколько влезет, прочее
    * ждёт на полках. Берём с конца стопки, как и по одной: показанная трава
    * уходит последней. */
  private def takeHerbs(user: User, kind: MaterialKind, asked: Int, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      val stack = home.herbs.filter(_.material.contains(kind))
      if (stack.isEmpty) say(user, content.text("stack.gone"), renderer) *> menu(user, p, renderer)
      else for {
        hero  <- getHero(user)
        inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
        want   = asked.min(stack.size)
        taken <- addToBag(hero, stack.takeRight(want.min(inv.freeSlots.toInt.max(0))))
        ids    = taken.map(_.id).toSet
        _     <- ZIO.when(taken.nonEmpty)(saveHome(user, home.copy(herbs = home.herbs.filterNot(i => ids.contains(i.id)))))
        _     <- say(user,
                   if (taken.isEmpty) content.text("home.workbench.bagFull")
                   else content.format("home.workbench.herbTaken", "name" -> kind.displayName, "n" -> taken.size.toString) +
                     (if (taken.size < want) "\n" + content.format("home.workbench.herbTakenShort", "n" -> (want - taken.size).toString)
                      else ""),
                   renderer)
        s     <- if (stack.size > taken.size) herbOf(user, kind, p, renderer) else menu(user, p, renderer)
      } yield s
    }

  private def herbToPot(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.herbs, ua) match {
        case None => menu(user, page(ua), renderer)
        case Some(it) =>
          cauldron.put(user.userId, ItemStore.HomeShelf.Herbs, it).flatMap {
            case Some(why) => say(user, why, renderer) *> herb(user, ua, renderer)
            case None =>
              say(user, content.format("home.workbench.potIn", "name" -> it.name), renderer) *>
                afterTake(user, ua, home.herbs, it, renderer, herb, menu)
          }
      }
    }

  private def herbToBag(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.herbs, ua) match {
        case None => menu(user, page(ua), renderer)
        case Some(it) =>
          toBag(user, herbShelf, it).flatMap {
            case Some(why) => say(user, why, renderer) *> herb(user, ua, renderer)
            case None =>
              say(user, content.format("home.workbench.toBagDone", "name" -> it.name), renderer) *>
                afterTake(user, ua, home.herbs, it, renderer, herb, menu)
          }
      }
    }

  /** Все травы — из сумки и из Живой сумки — на полки, пока есть место. */
  private def putHerbs(user: User, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      gather(user, isHerb, home.herbShelves - home.herbs.size).flatMap {
        case (Nil, 0) => say(user, content.text("home.workbench.noHerbs"), renderer) *> menu(user, 0, renderer)
        case (taken, left) =>
          saveHome(user, home.copy(herbs = home.herbs ++ taken)) *>
            say(user, stow.herbsText(taken.size, left), renderer) *>
            menu(user, 0, renderer)
      }
    }

  // ── Котелок ─────────────────────────────────────────────────────────────────

  private def pot(user: User, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      val lines = ItemStack.grouped(ItemMenu.byName(home.cauldron)).map { case (it, n) =>
                    s"• ${it.name}${ItemStack.countSuffix(n)}" }
      val text  =
        if (home.cauldron.isEmpty) content.text("home.workbench.potEmpty")
        else content.format("home.workbench.potList", "n" -> home.cauldron.size.toString,
          "max" -> HomeRates.CauldronSlots.toString, "lines" -> lines.mkString("\n"))
      renderer.show(user, Screen(text + "\n" + content.format("home.workbench.energy",
        "n" -> home.charges.toString, "max" -> home.altarMax.toString), List(
        Choice("WbCook", content.text("home.workbench.cook"), color = ChoiceColor.Positive, row = Some(0)),
        Choice("WbEmptyPot", content.text("home.workbench.emptyPot"), row = Some(0)),
        Choice("WbMenu", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(1))
      ))).as(StateType.HerbWorkbench)
    }

  /** Сварить ([[Cauldron.cook]]) и снова котелок. */
  private def cook(user: User, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { _ =>
      getHero(user).flatMap(cauldron.cook(user, _, renderer)) *> pot(user, renderer)
    }

  /** Вынуть всё ([[Cauldron.empty]]) и снова котелок. */
  private def emptyPot(user: User, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { _ =>
      getHero(user).flatMap(cauldron.empty).flatMap(ZIO.foreachDiscard(_)(say(user, _, renderer))) *> pot(user, renderer)
    }

  // ── Полка отваров ───────────────────────────────────────────────────────────

  private def brews(user: User, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      val text =
        if (home.brews.isEmpty) content.text("home.workbench.brewsEmpty")
        else content.format("home.workbench.brewsText", "n" -> home.brews.size.toString, "max" -> home.brewShelves.toString)
      val tail = List(
        Choice("WbPutBrews", content.text("home.workbench.putBrews"), color = ChoiceColor.Positive, row = Some(ItemMenu.NavRow + 1)),
        Choice("WbMenu", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow + 1))
      )
      renderer.show(user, Screen(text, stacks(ItemMenu.byName(home.brews), p, "WbBrew", "WbBrews") ++ tail))
        .as(StateType.HerbWorkbench)
    }

  private def brew(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.brews, ua) match {
        case None     => brews(user, page(ua), renderer)
        case Some(it) => brewScreen(user, home, it, page(ua), renderer)
      }
    }

  /** Стопка отваров на полке: сколько их, «В сумку» и «В котелок» по одной. Их
    * больше одной — ждём ещё и числа: сколько забрать в сумку разом (см.
    * [[takeBrews]]). */
  private def brewScreen(user: User, home: HomeData, it: Item, p: Int, renderer: Renderer): Task[StateType] = {
    val n    = same(home.brews, it)
    val data = Map("id" -> it.id.toString, "p" -> p.toString)
    val text = content.format("home.workbench.brewItem", "name" -> it.name, "n" -> n.toString) +
      (if (n > 1) "\n" + content.text("home.workbench.brewAsk") else "")
    val pending = it.brew.filter(_ => n > 1) match {
      case Some(kind) => writeScene(user, HerbWorkbenchState.WbScene(take = Some(kind.entryName), page = p))
      case None       => clearPending(user)
    }
    pending *> renderer.show(user, Screen(text, List(
      Choice("WbBrewBag", content.text("home.workbench.toBag"), color = ChoiceColor.Positive, data = data, row = Some(0)),
      Choice("WbBrewPot", content.text("home.workbench.toPot"), data = data, row = Some(0)),
      Choice("WbBrews", content.text("home.house.back"), color = ChoiceColor.Negative, data = Map("p" -> p.toString), row = Some(1))
    ))).as(StateType.HerbWorkbench)
  }

  /** Стопка этого отвара — если на полке он ещё есть; нет — сама полка. */
  private def brewOf(user: User, kind: BrewKind, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      home.brews.findLast(_.brew.contains(kind)) match {
        case None     => brews(user, p, renderer)
        case Some(it) => brewScreen(user, home, it, p, renderer)
      }
    }

  /** Забрать с полки в сумку `asked` бутылок этого отвара — сколько написали
    * числом. Больше, чем стоит, — сколько есть; в сумке тесно — сколько влезет,
    * прочее ждёт на полке. Берём с конца стопки, как и по одной: показанная
    * бутылка уходит последней. */
  private def takeBrews(user: User, kind: BrewKind, asked: Int, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      val stack = home.brews.filter(_.brew.contains(kind))
      if (stack.isEmpty) say(user, content.text("stack.gone"), renderer) *> brews(user, p, renderer)
      else for {
        hero  <- getHero(user)
        inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
        want   = asked.min(stack.size)
        taken <- addToBag(hero, stack.takeRight(want.min(inv.freeSlots.toInt.max(0))))
        ids    = taken.map(_.id).toSet
        _     <- ZIO.when(taken.nonEmpty)(saveHome(user, home.copy(brews = home.brews.filterNot(i => ids.contains(i.id)))))
        _     <- say(user,
                   if (taken.isEmpty) content.text("home.workbench.bagFull")
                   else content.format("home.workbench.taken", "name" -> kind.label, "n" -> taken.size.toString) +
                     (if (taken.size < want) "\n" + content.format("home.workbench.takenShort", "n" -> (want - taken.size).toString)
                      else ""),
                   renderer)
        s     <- if (stack.size > taken.size) brewOf(user, kind, p, renderer) else brews(user, p, renderer)
      } yield s
    }

  /** Положить в сумку по порядку, пока она берёт; что легло — то и вернём. */
  private def addToBag(hero: Hero, items: List[Item], added: List[Item] = Nil): Task[List[Item]] =
    items match {
      case Nil => ZIO.succeed(added.reverse)
      case it :: rest =>
        inventoryRepo.addItem(hero.id, it).either.flatMap {
          case Right(_) => addToBag(hero, rest, it :: added)
          case Left(InventoryRepoError.NoMorePlaceForItems | InventoryRepoError.DustLimitReached) => ZIO.succeed(added.reverse)
          case Left(e) => ZIO.fail(asThrowable(e))
        }
    }

  private def brewToBag(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.brews, ua) match {
        case None => brews(user, page(ua), renderer)
        case Some(it) =>
          toBag(user, brewShelf, it).flatMap {
            case Some(why) => say(user, why, renderer) *> brew(user, ua, renderer)
            case None =>
              say(user, content.format("home.workbench.toBagDone", "name" -> it.name), renderer) *>
                afterTake(user, ua, home.brews, it, renderer, brew, brews)
          }
      }
    }

  /** Склянку — основой в котелок: из неё и трав выходит перегонка. */
  private def brewToPot(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      itemOf(home.brews, ua) match {
        case None => brews(user, page(ua), renderer)
        case Some(it) =>
          cauldron.put(user.userId, ItemStore.HomeShelf.Brews, it).flatMap {
            case Some(why) => say(user, why, renderer) *> brew(user, ua, renderer)
            case None =>
              say(user, content.format("home.workbench.potIn", "name" -> it.name), renderer) *>
                afterTake(user, ua, home.brews, it, renderer, brew, brews)
          }
      }
    }

  /** Все отвары — из сумки и из Живой сумки — на полку, пока есть место. */
  private def putBrews(user: User, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      gather(user, HomeStow.isBrew, home.brewShelves - home.brews.size).flatMap {
        case (Nil, 0) => say(user, content.text("home.workbench.noBrews"), renderer) *> brews(user, 0, renderer)
        case (taken, left) =>
          saveHome(user, home.copy(brews = home.brews ++ taken)) *>
            say(user, stow.brewsText(taken.size, left), renderer) *>
            brews(user, 0, renderer)
      }
    }

  // ── Изученные отвары ────────────────────────────────────────────────────────

  /** Изученные отвары по страницам: всё, что герой уже варил и что варится
    * на верстаке. */
  private def known(user: User, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { _ =>
      HerbLore.readLore(heroDao, user.userId).flatMap { lore =>
        val kinds = Cauldron.learned(lore)
        val (pageKinds, totalPages, cur) = ItemMenu.page(kinds, p)
        val buttons = pageKinds.zipWithIndex.map { case (k, i) =>
          Choice("WbRecipe", ItemMenu.truncate(k.label), data = Map("k" -> k.entryName, "p" -> cur.toString), row = Some(i))
        }
        val nav = List(
          Option.when(cur > 0)(Choice("WbKnown", content.text("common.prev"), data = Map("p" -> (cur - 1).toString), row = Some(ItemMenu.NavRow))),
          Option.when(cur < totalPages - 1)(Choice("WbKnown", content.text("common.next"), data = Map("p" -> (cur + 1).toString), row = Some(ItemMenu.NavRow)))
        ).flatten
        val text = content.text(if (kinds.isEmpty) "home.workbench.knownEmpty" else "home.workbench.knownText")
        renderer.show(user, Screen(text, buttons ++ nav :+
          Choice("WbMenu", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow + 1))))
          .as(StateType.HerbWorkbench)
      }
    }

  /** Отвар из кнопки — только изученный: чужую кнопку сюда не подсунешь. */
  private def recipeOf(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    HerbLore.readLore(heroDao, user.userId).flatMap { lore =>
      payload(ua, "k").flatMap(BrewKind.withNameOption).filter(Cauldron.learned(lore).contains) match {
        case None       => known(user, page(ua), renderer)
        case Some(kind) => recipe(user, kind, page(ua), renderer)
      }
    }

  /** Рецепт: из чего, сколько выходит с варки, на сколько хватит полок и
    * энергии — и ждём число: сколько приготовить. */
  private def recipe(user: User, kind: BrewKind, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { home =>
      val base = kind.base.fold("")(b => content.format("home.workbench.recipeBase", "name" -> b.label))
      val can  = Cauldron.maxBatches(home, kind) * Cauldron.perBatch(kind)
      writeScene(user, HerbWorkbenchState.WbScene(Some(kind.entryName), p)) *>
        renderer.show(user, Screen(content.format("home.workbench.recipe",
          "name" -> kind.label, "herbs" -> kind.recipe.map(_.displayName).mkString(", "), "base" -> base,
          "portions" -> Cauldron.perBatch(kind).toString, "can" -> can.toString,
          "charges" -> home.charges.toString, "chargesMax" -> home.altarMax.toString), List(
          Choice("WbKnown", content.text("home.house.back"), color = ChoiceColor.Negative, data = Map("p" -> p.toString), row = Some(0))
        ))).as(StateType.HerbWorkbench)
    }

  /** Написанное у верстака: число — для того, чего верстак ждёт (сварить
    * изученный отвар, забрать отвары с полки или травы с полок); ждать нечего —
    * сам верстак. */
  private def onText(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    readScene(user).flatMap { scene =>
      val number = ua.text.filterNot(_.isWhitespace).toIntOption.filter(_ > 0)
      (scene.recipe.flatMap(BrewKind.withNameOption), scene.take.flatMap(BrewKind.withNameOption),
       scene.herb.flatMap(MaterialKind.withNameOption)) match {
        case (Some(kind), _, _) =>
          number match {
            case None    => say(user, content.text("home.workbench.notNumber"), renderer) *> recipe(user, kind, scene.page, renderer)
            case Some(n) => craft(user, kind, n, scene.page, renderer)
          }
        case (None, Some(kind), _) =>
          number match {
            case None    => say(user, content.text("home.workbench.takeNotNumber"), renderer) *> brewOf(user, kind, scene.page, renderer)
            case Some(n) => takeBrews(user, kind, n, scene.page, renderer)
          }
        case (None, None, Some(kind)) =>
          number match {
            case None    => say(user, content.text("home.workbench.herbNotNumber"), renderer) *> herbOf(user, kind, scene.page, renderer)
            case Some(n) => takeHerbs(user, kind, n, scene.page, renderer)
          }
        case _ => menu(user, 0, renderer)
      }
    }

  /** Сварить `asked` изученных отваров ([[Cauldron.brew]]) и снова рецепт. */
  private def craft(user: User, kind: BrewKind, asked: Int, p: Int, renderer: Renderer): Task[StateType] =
    withWorkbench(user) { _ =>
      getHero(user).flatMap(cauldron.brew(user, _, kind, asked, renderer)) *> recipe(user, kind, p, renderer)
    }

  private def readScene(user: User): Task[HerbWorkbenchState.WbScene] =
    UiScene.read(heroDao, user.userId, UiScene.Workbench, HerbWorkbenchState.WbScene())

  private def writeScene(user: User, scene: HerbWorkbenchState.WbScene): Task[Unit] =
    UiScene.write(heroDao, user.userId, UiScene.Workbench, scene)

  /** Снять ожидание числа — если оно было: лишней записи не делаем. */
  private def clearPending(user: User): Task[Unit] =
    readScene(user).flatMap(s =>
      ZIO.when(s.recipe.isDefined || s.take.isDefined || s.herb.isDefined)(
        UiScene.clear(heroDao, user.userId, UiScene.Workbench)).unit)

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  /** Верстак есть со второго дома; нет — обратно в дом. */
  private def withWorkbench(user: User)(f: HomeData => Task[StateType]): Task[StateType] =
    loadHome(user).flatMap(home =>
      if (Cauldron.stands(home)) f(home) else ZIO.succeed(StateType.Home))

  /** Стопки одинакового по страницам, листание и номер страницы в кнопках.
    * Полки и котелок показывают по алфавиту ([[ItemMenu.byName]]). */
  private def stacks(items: List[Item], p: Int, pick: String, self: String): List[Choice] = {
    val (pageItems, totalPages, cur) = ItemMenu.page(ItemStack.grouped(items), p)
    val buttons = pageItems.zipWithIndex.map { case ((it, n), i) =>
      Choice(pick, ItemMenu.truncate(it.name + ItemStack.countSuffix(n)),
        data = Map("id" -> it.id.toString, "p" -> cur.toString), row = Some(i))
    }
    val nav = List(
      Option.when(cur > 0)(Choice(self, content.text("common.prev"), data = Map("p" -> (cur - 1).toString), row = Some(ItemMenu.NavRow))),
      Option.when(cur < totalPages - 1)(Choice(self, content.text("common.next"), data = Map("p" -> (cur + 1).toString), row = Some(ItemMenu.NavRow)))
    ).flatten
    buttons ++ nav
  }

  /** Сколько таких же, как `it`, лежит в `items` (вместе с ним). */
  private def same(items: List[Item], it: Item): Int =
    ItemStack.key(it).fold(1)(k => items.count(i => ItemStack.key(i).contains(k)))

  /** Вещь по id из кнопки — если она ещё здесь. */
  private def itemOf(items: List[Item], ua: UserAction): Option[Item] =
    payload(ua, "id").flatMap(_.toLongOption).flatMap(id => items.find(_.id == id))

  /** Убрать ровно эту вещь. */
  private def without(items: List[Item], it: Item): List[Item] =
    items.indexWhere(_.id == it.id) match {
      case -1 => items
      case i  => items.patch(i, Nil, 1)
    }

  /** После того как одну взяли: остались ещё такие — экран той же стопки (по
    * следующей её вещи), нет — список. */
  private def afterTake(
      user: User, ua: UserAction, before: List[Item], it: Item, renderer: Renderer,
      stack: (User, UserAction, Renderer) => Task[StateType], list: (User, Int, Renderer) => Task[StateType]
  ): Task[StateType] =
    ItemStack.key(it).flatMap(k => without(before, it).findLast(i => ItemStack.key(i).contains(k))) match {
      case Some(next) =>
        stack(user, UserAction(ua.text, Some(Map("action" -> "", "id" -> next.id.toString, "p" -> page(ua).toString).asJson.noSpaces)), renderer)
      case None => list(user, page(ua), renderer)
    }

  /** Положить в сумку, если есть место. */
  private def toBag(user: User, shelf: ItemStore, it: Item): Task[Option[String]] =
    getHero(user).flatMap(ItemStore.move(shelf, bag, _, it, content.text("stack.gone")))

  private val bag       = new ItemStore.Bag(inventoryRepo, _ => content.text("home.workbench.bagFull"))
  private val herbShelf = new ItemStore.HomeShelf(heroDao, ItemStore.HomeShelf.Herbs, content)
  private val brewShelf = new ItemStore.HomeShelf(heroDao, ItemStore.HomeShelf.Brews, content)

  /** Собрать вещи из сумки, а следом из Живой сумки — не больше `room`.
    * Возвращает взятое и сколько подходящего осталось за бортом. */
  private def gather(user: User, accepts: Item => Boolean, room: Int): Task[(List[Item], Int)] =
    getHero(user).flatMap(hero => stow.gather(hero.id, accepts, ArtifactKind.LivingBag, room))

  private def isHerb(it: Item): Boolean = HomeStow.isHerb(it)

  private def say(user: User, text: String, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(text, Nil))

  private def page(ua: UserAction): Int = payload(ua, "p").flatMap(_.toIntOption).getOrElse(0)

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def loadHome(user: User): Task[HomeData] =
    heroDao.readHomeData(user.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

  private def saveHome(user: User, home: HomeData): Task[Unit] =
    heroDao.writeHomeData(user.userId, home.asJson)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object HerbWorkbenchState {

  /** Чего верстак ждёт числом: сварить изученный отвар (`recipe`), забрать с
    * полки в сумку отвары этого вида (`take`) или с полок траву этого вида
    * (`herb`) — все `entryName`, — и с какой страницы списка их выбрали: туда
    * ведёт «Назад». В записях постарше `take` и `herb` нет — значит, забирать
    * ничего не ждут. */
  final case class WbScene(recipe: Option[String] = None, page: Int = 0, take: Option[String] = None,
                           herb: Option[String] = None)
  object WbScene {
    implicit val encoder: Encoder[WbScene] = deriveEncoder
    implicit val decoder: Decoder[WbScene] = (c: HCursor) =>
      for {
        recipe <- c.getOrElse[Option[String]]("recipe")(None)
        page   <- c.getOrElse[Int]("page")(0)
        take   <- c.getOrElse[Option[String]]("take")(None)
        herb   <- c.getOrElse[Option[String]]("herb")(None)
      } yield WbScene(recipe, page, take, herb)
  }
}
