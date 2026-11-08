package pangea.service.state.states.temple

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{AzatState, Hero}
import pangea.model.home.HomeData
import pangea.model.item.ItemStack
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.cube.AzatCube
import pangea.service.cube.AzatCube.Power
import pangea.service.purse.Purse
import pangea.service.state.ItemMenu
import pangea.service.state.states.temple.CubeState._
import pangea.service.state.{StackMove, State, UserAction}
import zio.{Task, ZIO}

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

  /** Сам куб — положить, забрать, активировать; то же зовёт и Mini App. */
  private val cube = new AzatCube(heroDao, inventoryRepo, itemRepository, content)

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
      "CubeTakeAll"      -> Target.Run { (u, _, r) => withdrawEverything(u, r).as(StateType.Cube) },
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

  /** Меню куба. `note` — что только что случилось («Забрать всё»): строкой
    * над меню, а не отдельным сообщением. */
  private def showMenu(user: User, renderer: Renderer, note: Option[String] = None): Task[Unit] =
    for {
      azat <- loadAzat(user)
      home <- loadHome(user)
      power = AzatCube.power(azat, home)
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
      // Куб выгребается и одной кнопкой — как почта.
      takeAll = Option.when(azat.cubeItems.nonEmpty)(Choice("CubeTakeAll", content.text("stack.takeAll"), row = Some(0)))
      choices = List(
        Choice("CubeDeposit",  content.text("cube.menu.deposit"),  row = Some(0)),
        Choice("CubeWithdraw", content.text("cube.menu.withdraw"), row = Some(0))
      ) ++ takeAll.toList ++ List(
        Choice("CubeActivate", content.text("cube.menu.activate"), color = ChoiceColor.Positive, row = Some(1))
      ) ++ refills :+ Choice("LeaveCube", content.text("cube.menu.leave"), color = ChoiceColor.Negative, row = Some(3))
      _ <- renderer.show(user, Screen(StackMove.withNote(note, text), choices))
    } yield ()

  /** Пополнить куб прямо на нём — по цене жреца; только дома и без святилища. */
  private def refill(user: User, renderer: Renderer, cost: Long, charges: Int): Task[Unit] =
    for {
      hero   <- getHero(user)
      azat   <- loadAzat(user)
      home   <- loadHome(user)
      wallet <- purse.wallet(hero)
      _ <- if (!azat.cubeHome || !AzatCube.power(azat, home).isInstanceOf[Power.Own]) ZIO.unit
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

  /** Список сумки, из которой кладут. `note` — что только что случилось: оно
    * идёт строкой над списком, а не отдельным сообщением. `hot` — стопка, из
    * которой только что положили одну: рядом с ней «⏬ все N». */
  private def showDeposit(user: User, renderer: Renderer, note: Option[String] = None,
                          hot: Option[String] = None): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat  <- loadAzat(user)
      scene <- readScene(user)
      items  = inv.items.data.filter(AzatCube.accepts) // сюжетное и стопки в куб не кладутся
      free   = AzatState.CubeCapacity - azat.cubeItems.length
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(StackMove.withNote(note, content.text("cube.emptyInventory")), backRow))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(ItemStack.grouped(items), scene.depositPage.getOrElse(0))
             val header  = content.format("cube.depositHeader", "free" -> free.toString) +
                           (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val btns    = ItemMenu.stackButtons(pageItems, DepositPrefix) ++
                           StackMove.allButton(pageItems, hot, DepositAllPrefix, content)
             val nav     = navRow("CubeMenu",
                             Option.when(page > 0)(Choice("CubeDepositPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
                             Option.when(page < totalPages - 1)(Choice("CubeDepositNext", content.text("common.next"), row = Some(ItemMenu.NavRow))))
             renderer.show(user, Screen(StackMove.withNote(note, header), btns ++ nav))
           }
    } yield ()

  /** Список куба, из которого забирают; `note` и `hot` — как у [[showDeposit]]. */
  private def showWithdraw(user: User, renderer: Renderer, note: Option[String] = None,
                           hot: Option[String] = None): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat  <- loadAzat(user)
      scene <- readScene(user)
      items  = azat.cubeItems
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(StackMove.withNote(note, content.text("cube.emptyCube")), backRow))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(ItemStack.grouped(items), scene.withdrawPage.getOrElse(0))
             val header  = content.format("cube.withdrawHeader", "free" -> inv.freeSlots.toString) +
                           (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val btns    = ItemMenu.stackButtons(pageItems, WithdrawPrefix) ++
                           StackMove.allButton(pageItems, hot, WithdrawAllPrefix, content)
             val nav     = navRow("CubeMenu",
                             Option.when(page > 0)(Choice("CubeWithdrawPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
                             Option.when(page < totalPages - 1)(Choice("CubeWithdrawNext", content.text("common.next"), row = Some(ItemMenu.NavRow))))
             renderer.show(user, Screen(StackMove.withNote(note, header), btns ++ nav))
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
      case Some(a) if a.startsWith(DepositAllPrefix) =>
        a.drop(DepositAllPrefix.length).toLongOption.fold(showMenu(user, renderer))(depositAll(user, _, renderer)).as(StateType.Cube)
      case Some(a) if a.startsWith(WithdrawAllPrefix) =>
        a.drop(WithdrawAllPrefix.length).toLongOption.fold(showMenu(user, renderer))(withdrawAll(user, _, renderer)).as(StateType.Cube)
      case _ => showMenu(user, renderer).as(StateType.Cube)
    }

  private def depositItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      _ <- inv.items.data.find(i => i.id == itemId && !i.isQuestItem) match {
        case None => showDeposit(user, renderer)
        case Some(item) =>
          cube.deposit(hero, item).flatMap {
            case None         => showDeposit(user, renderer,
                                   Some(content.format("cube.deposited", "name" -> item.name)), StackMove.hotOf(item))
            case Some(reason) => showDeposit(user, renderer, Some(reason))
          }
      }
    } yield ()

  private def withdrawItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      azat <- loadAzat(user)
      _ <- azat.cubeItems.find(_.id == itemId) match {
        case None => showWithdraw(user, renderer)
        case Some(item) =>
          cube.withdraw(hero, item).flatMap {
            case None         => showWithdraw(user, renderer,
                                   Some(content.format("cube.withdrawn", "name" -> item.name)), StackMove.hotOf(item))
            case Some(reason) => showWithdraw(user, renderer, Some(reason))
          }
      }
    } yield ()

  /** «⏬ все N» у стопки сумки: остаток — в куб, пока есть место. */
  private def depositAll(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      items  = inv.items.data.filter(AzatCube.accepts)
      _ <- items.find(_.id == itemId) match {
        case None => showDeposit(user, renderer)
        case Some(item) =>
          StackMove.run(StackMove.stack(items, item))(cube.deposit(hero, _)).flatMap(moved =>
            showDeposit(user, renderer, Some(StackMove.summary(content, moved, "cube.depositedAll"))))
      }
    } yield ()

  /** «⏬ все N» у стопки в кубе: остаток — в сумку, сколько влезет. */
  private def withdrawAll(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      azat <- loadAzat(user)
      _ <- azat.cubeItems.find(_.id == itemId) match {
        case None => showWithdraw(user, renderer)
        case Some(item) =>
          StackMove.run(StackMove.stack(azat.cubeItems, item))(cube.withdraw(hero, _)).flatMap(moved =>
            showWithdraw(user, renderer, Some(StackMove.summary(content, moved, "cube.withdrawnAll"))))
      }
    } yield ()

  /** «⏬ Забрать всё»: куб целиком — в сумку, сколько влезет. */
  private def withdrawEverything(user: User, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap(cube.withdrawEverything).flatMap(note => showMenu(user, renderer, Some(note)))

  // --- Активация: просчёт рецептов ---

  private def activate(user: User, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap(cube.activate(user, _, renderer)) *> showMenu(user, renderer)

  // --- Вспомогательное ---

  private def loadAzat(user: User): Task[AzatState] = cube.loadAzat(user.userId)

  private def saveAzat(user: User, azat: AzatState): Task[Unit] = cube.saveAzat(user.userId, azat)

  private def loadHome(user: User): Task[HomeData] = cube.loadHome(user.userId)

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
  // «⏬ все N»: за префиксом — id любой вещи стопки.
  val DepositAllPrefix  = "CubeDepositAll_"
  val WithdrawAllPrefix = "CubeWithdrawAll_"

  case class CubeScene(depositPage: Option[Int] = None, withdrawPage: Option[Int] = None)
  object CubeScene {
    implicit val encoder: Encoder[CubeScene] = deriveEncoder
    implicit val decoder: Decoder[CubeScene] = deriveDecoder
  }
}
