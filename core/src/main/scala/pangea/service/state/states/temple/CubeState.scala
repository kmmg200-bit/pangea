package pangea.service.state.states.temple

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, jawn}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.CubeCraft
import pangea.model.hero.{Achievement, AzatState, Hero}
import pangea.model.home.{HomeData, RecipeNote}
import pangea.model.item.{BrewKind, Item, ItemStack}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.purse.Purse
import pangea.service.state.ItemMenu
import pangea.service.state.states.temple.CubeState._
import pangea.service.state.{AzatData, HerbLore, InventoryFeedback, MarisaQuest, State, UserAction}
import java.util.concurrent.TimeUnit
import zio.{Random, Task, ZIO}

/** Крафт в кубе Азата: по образцу «Неприметной бочки» игрок кладёт в куб до
 *  [[AzatState.CubeCapacity]] предметов и забирает их. Кнопка «Активировать»
 *  просчитывает рецепты ([[CubeCraft]]); успешные применения тратят заряды.
 *  Содержимое куба и заряды живут в `azat_data`; пагинация — в `scene_data`.
 *
 *  Куб может переехать домой — в гнездо верстака ремесленника
 *  (`AzatState.cubeHome`). Тогда «Назад» ведёт домой, всё, что получилось,
 *  пишется в книгу рецептов дома, а сила — смотря по дому: со святилищем (алтарь
 *  Азата) куб берёт энергию дома, без него — свои заряды, и пополнить их можно
 *  прямо на кубе, по цене жреца. */
case class CubeState(
  heroDao:        HeroDao,
  inventoryRepo:  InventoryRepository,
  itemRepository: ItemRepository,
  content:        SceneContent,
  bank:           Option[BankRepository] = None
) extends State {

  /** Кошель: своё серебро, а следом — ячейка в Торговом доме. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "CubeMenu"         -> Target.Run { (u, _, r) => resetScene(u) *> showMenu(u, r).as(StateType.Cube) },
      "CubeDeposit"      -> Target.Run { (u, _, r) => writeScene(u, CubeScene(depositPage = Some(0))) *> showDeposit(u, r).as(StateType.Cube) },
      "CubeDepositPrev"  -> Target.Run { (u, _, r) => navigateDeposit(u, r, -1).as(StateType.Cube) },
      "CubeDepositNext"  -> Target.Run { (u, _, r) => navigateDeposit(u, r, +1).as(StateType.Cube) },
      "CubeWithdraw"     -> Target.Run { (u, _, r) => writeScene(u, CubeScene(withdrawPage = Some(0))) *> showWithdraw(u, r).as(StateType.Cube) },
      "CubeWithdrawPrev" -> Target.Run { (u, _, r) => navigateWithdraw(u, r, -1).as(StateType.Cube) },
      "CubeWithdrawNext" -> Target.Run { (u, _, r) => navigateWithdraw(u, r, +1).as(StateType.Cube) },
      "CubeActivate"     -> Target.Run { (u, _, r) => activate(u, r).as(StateType.Cube) },
      "CubeRefillFull"   -> Target.Run { (u, _, r) => refill(u, r, HallAzatState.RechargeFullSilver, HallAzatState.RechargeFullCharges).as(StateType.Cube) },
      "CubeRefillHalf"   -> Target.Run { (u, _, r) => refill(u, r, HallAzatState.RechargeHalfSilver, HallAzatState.RechargeHalfCharges).as(StateType.Cube) },
      // Куб дома — назад домой, в Храме — в зал Азата.
      "LeaveCube"        -> Target.Run { (u, _, _) => loadAzat(u).map(a => if (a.cubeHome) StateType.Home else StateType.HallAzat) }
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets ++ Set(StateType.Home, StateType.HallAzat)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    resetScene(user) *> showMenu(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // --- Меню куба ---

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      azat <- loadAzat(user)
      home <- loadHome(user)
      power = CubeState.power(azat, home)
      text  = power match {
                case Power.Home(n, max) => content.format("cube.menu.textHome",
                  "items" -> azat.cubeItems.length.toString, "capacity" -> AzatState.CubeCapacity.toString,
                  "charges" -> n.toString, "max" -> max.toString)
                case Power.Own(n) => content.format("cube.menu.text",
                  "items" -> azat.cubeItems.length.toString, "capacity" -> AzatState.CubeCapacity.toString,
                  "charges" -> n.toString)
              }
      // Пополнить на самом кубе — когда он дома и живёт на своих зарядах.
      refills = if (azat.cubeHome && power.isInstanceOf[Power.Own]) List(
                  Choice("CubeRefillFull", content.format("cube.refill", "charges" -> HallAzatState.RechargeFullCharges.toString,
                    "price" -> HallAzatState.RechargeFullSilver.toString), row = Some(2)),
                  Choice("CubeRefillHalf", content.format("cube.refill", "charges" -> HallAzatState.RechargeHalfCharges.toString,
                    "price" -> HallAzatState.RechargeHalfSilver.toString), row = Some(2)))
                else Nil
      choices = List(
        Choice("CubeDeposit",  content.text("cube.menu.deposit"),  row = Some(0)),
        Choice("CubeWithdraw", content.text("cube.menu.withdraw"), row = Some(0)),
        Choice("CubeActivate", content.text("cube.menu.activate"), color = ChoiceColor.Positive, row = Some(1))
      ) ++ refills :+ Choice("LeaveCube", content.text("cube.menu.leave"), color = ChoiceColor.Negative, row = Some(3))
      _ <- renderer.show(user, Screen(text, choices))
    } yield ()

  /** Пополнить куб прямо на нём — по цене жреца; только дома и без святилища. */
  private def refill(user: User, renderer: Renderer, cost: Long, charges: Int): Task[Unit] =
    for {
      hero   <- getHero(user)
      azat   <- loadAzat(user)
      home   <- loadHome(user)
      wallet <- purse.wallet(hero)
      _ <- if (!azat.cubeHome || !CubeState.power(azat, home).isInstanceOf[Power.Own]) ZIO.unit
           else if (azat.cubeCharges >= AzatState.MaxCharges)
             renderer.show(user, Screen(content.text("cube.refillFull"), Nil))
           else if (!wallet.canAfford(cost))
             renderer.show(user, Screen(content.format("cube.refillLack", "price" -> cost.toString), Nil))
           else {
             val next = (azat.cubeCharges + charges).min(AzatState.MaxCharges)
             purse.charge(user.userId, hero, cost) *>
               saveAzat(user, azat.copy(cubeCharges = next)) *>
               renderer.show(user, Screen(content.format("cube.refilled", "charges" -> next.toString), Nil))
           }
      _ <- showMenu(user, renderer)
    } yield ()

  // --- Списки ---

  private def showDeposit(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat  <- loadAzat(user)
      scene <- readScene(user)
      items  = inv.items.data.filterNot(_.isQuestItem) // сюжетное в куб не кладётся
      free   = AzatState.CubeCapacity - azat.cubeItems.length
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.text("cube.emptyInventory"), backRow))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(ItemStack.grouped(items), scene.depositPage.getOrElse(0))
             val header  = content.format("cube.depositHeader", "free" -> free.toString) +
                           (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val btns    = ItemMenu.stackButtons(pageItems, DepositPrefix)
             val nav     = navRow("CubeMenu",
                             Option.when(page > 0)(Choice("CubeDepositPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
                             Option.when(page < totalPages - 1)(Choice("CubeDepositNext", content.text("common.next"), row = Some(ItemMenu.NavRow))))
             renderer.show(user, Screen(header, btns ++ nav))
           }
    } yield ()

  private def showWithdraw(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat  <- loadAzat(user)
      scene <- readScene(user)
      items  = azat.cubeItems
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.text("cube.emptyCube"), backRow))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(ItemStack.grouped(items), scene.withdrawPage.getOrElse(0))
             val header  = content.format("cube.withdrawHeader", "free" -> inv.freeSlots.toString) +
                           (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val btns    = ItemMenu.stackButtons(pageItems, WithdrawPrefix)
             val nav     = navRow("CubeMenu",
                             Option.when(page > 0)(Choice("CubeWithdrawPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
                             Option.when(page < totalPages - 1)(Choice("CubeWithdrawNext", content.text("common.next"), row = Some(ItemMenu.NavRow))))
             renderer.show(user, Screen(header, btns ++ nav))
           }
    } yield ()

  private def navigateDeposit(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      (_, totalPages, _) = ItemMenu.page(inv.items.data, 0)
      newPage = (scene.depositPage.getOrElse(0) + delta).max(0).min(totalPages - 1)
      _ <- writeScene(user, scene.copy(depositPage = Some(newPage)))
      _ <- showDeposit(user, renderer)
    } yield ()

  private def navigateWithdraw(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    for {
      scene <- readScene(user)
      azat  <- loadAzat(user)
      (_, totalPages, _) = ItemMenu.page(azat.cubeItems, 0)
      newPage = (scene.withdrawPage.getOrElse(0) + delta).max(0).min(totalPages - 1)
      _ <- writeScene(user, scene.copy(withdrawPage = Some(newPage)))
      _ <- showWithdraw(user, renderer)
    } yield ()

  private def backRow: List[Choice] =
    List(Choice("CubeMenu", content.text("cube.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def navRow(backAction: String, prev: Option[Choice], next: Option[Choice]): List[Choice] =
    List(Some(Choice(backAction, content.text("cube.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))), prev, next).flatten

  // --- Транзакции ---

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(DepositPrefix) =>
        a.drop(DepositPrefix.length).toLongOption.fold(showMenu(user, renderer))(depositItem(user, _, renderer)).as(StateType.Cube)
      case Some(a) if a.startsWith(WithdrawPrefix) =>
        a.drop(WithdrawPrefix.length).toLongOption.fold(showMenu(user, renderer))(withdrawItem(user, _, renderer)).as(StateType.Cube)
      case _ => showMenu(user, renderer).as(StateType.Cube)
    }

  private def depositItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat <- loadAzat(user)
      _ <- inv.items.data.find(i => i.id == itemId && !i.isQuestItem) match {
        case None => showDeposit(user, renderer)
        case Some(item) =>
          if (azat.cubeItems.length >= AzatState.CubeCapacity)
            renderer.show(user, Screen(content.text("cube.full"), Nil)) *> showDeposit(user, renderer)
          else
            inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable) *>
              saveAzat(user, azat.copy(cubeItems = azat.cubeItems :+ item)) *>
              renderer.show(user, Screen(content.format("cube.deposited", "name" -> item.name), Nil)) *>
              showDeposit(user, renderer)
      }
    } yield ()

  private def withdrawItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat <- loadAzat(user)
      _ <- azat.cubeItems.find(_.id == itemId) match {
        case None => showWithdraw(user, renderer)
        case Some(item) =>
          // Пыль и малые руны места не занимают — им мешает только свой предел.
          if (if (item.weightless) !inv.hasRoomForHoard(item) else inv.freeSlots <= 0)
            renderer.show(user, Screen(
              if (item.weightless) InventoryFeedback.refusalLine(content, item, storage = true)
              else content.text("cube.inventoryFull"), Nil)) *> showWithdraw(user, renderer)
          else
            inventoryRepo.addItem(hero.id, item).mapError(asThrowable) *>
              saveAzat(user, azat.copy(cubeItems = removeFirst(azat.cubeItems, itemId))) *>
              renderer.show(user, Screen(content.format("cube.withdrawn", "name" -> item.name), Nil)) *>
              showWithdraw(user, renderer)
      }
    } yield ()

  // --- Активация: просчёт рецептов ---

  private def activate(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      azat <- loadAzat(user)
      home <- loadHome(user)
      power = CubeState.power(azat, home)
      seed <- Random.nextLong
      result = CubeCraft.craft(azat.cubeItems, power.charges, Rng(seed))
      // Результаты рецептов (id <= 0) персистим — им нужен id для инвентаря.
      persisted <- ZIO.foreach(result.items) { it =>
        if (it.id > 0L) ZIO.succeed(it)
        else itemRepository.persist(hero.id, it)
      }
      // Сила уходит оттуда же, откуда взята: энергия дома или заряды куба.
      _ <- power match {
             case Power.Home(n, _) =>
               saveAzat(user, azat.copy(cubeItems = persisted)) *>
                 saveHome(user, home.copy(charges = n - result.chargesUsed))
             case Power.Own(n) =>
               saveAzat(user, azat.copy(cubeItems = persisted, cubeCharges = n - result.chargesUsed))
           }
      // Дома всё, что получилось, — в книгу рецептов (пишется и без библиотеки).
      _ <- ZIO.when(azat.cubeHome && result.applied.nonEmpty)(
             loadHome(user).flatMap(h => saveHome(user, h.noted(result.applied.map(CubeState.note)))))
      msgKey = if (result.anyApplied) "cube.activated" else "cube.nothing"
      _ <- renderer.show(user, Screen(content.text(msgKey), Nil))
      // Сваренные отвары идут в счёт «Зельевара I»: по одному каждого — достижение.
      brewedNow = result.items.filter(_.id <= 0L).flatMap(_.brew).map(_.entryName)
      _ <- ZIO.when(brewedNow.nonEmpty) {
             for {
               lore   <- HerbLore.readLore(heroDao, user.userId)
               updated = lore.brewedAlso(brewedNow)
               _      <- HerbLore.writeLore(heroDao, user.userId, updated)
               _      <- ZIO.when(BrewKind.rank1.forall(k => updated.brewed.contains(k.entryName)))(
                           MarisaQuest.grant(heroDao, content, user, hero, Achievement.Brewer1, renderer))
             } yield ()
           }
      _ <- showMenu(user, renderer)
    } yield ()

  // --- Вспомогательное ---

  private def removeFirst(items: List[Item], itemId: Long): List[Item] =
    items.indexWhere(_.id == itemId) match {
      case -1 => items
      case i  => items.patch(i, Nil, 1)
    }

  private def loadAzat(user: User): Task[AzatState] =
    ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      .flatMap(now => AzatData.load(heroDao, user.userId, now))

  private def saveAzat(user: User, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(user.userId, azat.asJson)

  private def loadHome(user: User): Task[HomeData] =
    heroDao.readHomeData(user.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

  private def saveHome(user: User, home: HomeData): Task[Unit] =
    heroDao.writeHomeData(user.userId, home.asJson)

  private def readScene(user: User): Task[CubeScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[CubeScene].toOption).getOrElse(CubeScene()))

  private def writeScene(user: User, scene: CubeScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def resetScene(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object CubeState {
  val DepositPrefix  = "CubeDeposit_"
  val WithdrawPrefix = "CubeWithdraw_"

  /** Откуда куб берёт силу. */
  sealed trait Power { def charges: Int }
  object Power {
    /** Свои заряды куба — в Храме и в доме без святилища. */
    final case class Own(charges: Int) extends Power
    /** Энергия дома — куб дома, и в доме есть алтарь Азата. */
    final case class Home(charges: Int, max: Int) extends Power
  }

  def power(azat: AzatState, home: HomeData): Power =
    if (azat.cubeHome && home.altarMax > 0) Power.Home(home.charges, home.altarMax) else Power.Own(azat.cubeCharges)

  /** Запись в книгу рецептов: названия того, что ушло, и что вышло. */
  def note(a: CubeCraft.Applied): RecipeNote = RecipeNote(a.consumed.map(_.name), a.result.name, a.count)

  case class CubeScene(depositPage: Option[Int] = None, withdrawPage: Option[Int] = None)
  object CubeScene {
    implicit val encoder: Encoder[CubeScene] = deriveEncoder
    implicit val decoder: Decoder[CubeScene] = deriveDecoder
  }
}
