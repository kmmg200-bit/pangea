package pangea.service.state.states.bank

import io.circe.{Decoder, Encoder, Json, jawn}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.bank.{BankVault, StowGroup, StowSettings}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemStack}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.bank.{BankRepoError, BankRepository}
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.service.state.ItemMenu
import pangea.service.state.states.bank.BankVaultState._
import pangea.service.state.{CityExit, InventoryFeedback, StackMove, State, UserAction}
import zio.{Task, ZIO}

/** «Моё хранилище» в Торговом доме: ячейки, выкупленные у Рахадима. Работает
 *  как неприметная бочка, только вместимость растёт с числом ячеек (см.
 *  [[BankVault]]), а серебро отсюда идёт в дело, когда кончится своё. Режим
 *  ввода суммы и страницы списков живут в `heroes.scene_data`. */
case class BankVaultState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  bankRepo:      BankRepository,
  content:       SceneContent,
  artifacts:     Option[ArtifactRepository] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "VaultMenu"           -> Target.Run { (u, _, r) => resetScene(u) *> showMenu(u, r).as(StateType.BankVault) },
      "VaultDepositItems"   -> Target.Run { (u, _, r) => writeScene(u, VaultScene(depositPage = Some(0))) *> showDepositItems(u, r).as(StateType.BankVault) },
      "VaultDepositPrev"    -> Target.Run { (u, _, r) => navigateDeposit(u, r, -1).as(StateType.BankVault) },
      "VaultDepositNext"    -> Target.Run { (u, _, r) => navigateDeposit(u, r, +1).as(StateType.BankVault) },
      "VaultWithdrawItems"  -> Target.Run { (u, _, r) => writeScene(u, VaultScene(withdrawPage = Some(0))) *> showWithdrawItems(u, r).as(StateType.BankVault) },
      "VaultWithdrawPrev"   -> Target.Run { (u, _, r) => navigateWithdraw(u, r, -1).as(StateType.BankVault) },
      "VaultWithdrawNext"   -> Target.Run { (u, _, r) => navigateWithdraw(u, r, +1).as(StateType.BankVault) },
      "VaultDepositSilver"  -> Target.Run { (u, _, r) => writeScene(u, VaultScene(vaultMode = Some(ModeDepositSilver))) *> showDepositSilver(u, r).as(StateType.BankVault) },
      "VaultDepositAll"     -> Target.Run { (u, _, r) => depositAllSilver(u, r).as(StateType.BankVault) },
      "VaultWithdrawSilver" -> Target.Run { (u, _, r) => writeScene(u, VaultScene(vaultMode = Some(ModeWithdrawSilver))) *> showWithdrawSilver(u, r).as(StateType.BankVault) },
      "VaultWithdrawAll"    -> Target.Run { (u, _, r) => withdrawAllSilver(u, r).as(StateType.BankVault) },
      "VaultStowAll"        -> Target.Run { (u, _, r) => stowAll(u, r).as(StateType.BankVault) },
      "VaultStowSettings"   -> Target.Run { (u, _, r) => showStowSettings(u, r).as(StateType.BankVault) },
      "LeaveVault"          -> Target.Goto(StateType.TradeHouse),
      CityExit.route
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    resetScene(user) *> showMenu(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // --- Меню хранилища ---

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      vault <- getVault(user)
      _     <- if (!vault.open) renderer.show(user, Screen(content.text("bank.vault.none"), leaveRow))
               else {
                 val text = content.format("bank.vault.menu.text",
                   "cells"     -> vault.cells.toString,
                   "items"     -> vault.occupied.toString,
                   "maxItems"  -> vault.maxItems.toString,
                   "silver"    -> vault.silver.toString,
                   "maxSilver" -> vault.maxSilver.toString)
                 val choices = List(
                   Choice("VaultDepositItems",   content.text("bank.vault.menu.depositItems"),   row = Some(0)),
                   Choice("VaultWithdrawItems",  content.text("bank.vault.menu.withdrawItems"),  row = Some(0)),
                   Choice("VaultDepositSilver",  content.text("bank.vault.menu.depositSilver"),  row = Some(1)),
                   Choice("VaultWithdrawSilver", content.text("bank.vault.menu.withdrawSilver"), row = Some(1)),
                   Choice("VaultStowAll",        content.text("bank.vault.stow.label"),
                     color = ChoiceColor.Positive, row = Some(2)),
                   Choice("VaultStowSettings",   content.text("bank.vault.stow.settingsLabel"),
                     color = ChoiceColor.Secondary, row = Some(2)),
                   Choice("LeaveVault",          content.text("bank.vault.menu.leave"), color = ChoiceColor.Negative, row = Some(3)),
                   CityExit.button(content, Some(3))
                 )
                 renderer.show(user, Screen(text, choices))
               }
    } yield ()

  // --- Списки предметов ---

  /** Список сумки, из которой кладут. `note` — что только что случилось: оно
    * идёт строкой над списком, а не отдельным сообщением. `hot` — стопка, из
    * которой только что положили одну: рядом с ней «⏬ все N». */
  private def showDepositItems(user: User, renderer: Renderer, note: Option[String] = None,
                               hot: Option[String] = None): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      vault <- getVault(user)
      scene <- readScene(user)
      items  = inv.items.data.filterNot(_.isQuestItem) // сюжетное в хранилище не кладётся
      _ <- if (items.isEmpty) renderer.show(user, Screen(StackMove.withNote(note, content.text("bank.vault.emptyInventory")), backRow))
           else {
             val (pageItems, totalPages, page) =
               ItemMenu.page(ItemStack.grouped(items), scene.depositPage.getOrElse(0))
             val header = content.format("bank.vault.depositHeader",
               "free" -> vault.freeSlots.toString, "page" -> (page + 1).toString, "total" -> totalPages.toString)
             val buttons = ItemMenu.stackButtons(pageItems, DepositItemPrefix) ++
                           StackMove.allButton(pageItems, hot, DepositAllPrefix, content)
             val nav = navRow(
               back = Some(Choice("VaultMenu", content.text("bank.vault.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))),
               prev = Option.when(page > 0)(Choice("VaultDepositPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
               next = Option.when(page < totalPages - 1)(Choice("VaultDepositNext", content.text("common.next"), row = Some(ItemMenu.NavRow))))
             renderer.show(user, Screen(StackMove.withNote(note, header), buttons ++ nav))
           }
    } yield ()

  private def navigateDeposit(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      (_, totalPages, _) = ItemMenu.page(ItemStack.grouped(inv.items.data), 0)
      page   = (scene.depositPage.getOrElse(0) + delta).max(0).min(totalPages - 1)
      _     <- writeScene(user, scene.copy(depositPage = Some(page)))
      _     <- showDepositItems(user, renderer)
    } yield ()

  /** Список ячейки, из которой забирают; `note` и `hot` — как у [[showDepositItems]]. */
  private def showWithdrawItems(user: User, renderer: Renderer, note: Option[String] = None,
                                hot: Option[String] = None): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      vault <- getVault(user)
      scene <- readScene(user)
      items  = vault.items.data
      _ <- if (items.isEmpty) renderer.show(user, Screen(StackMove.withNote(note, content.text("bank.vault.emptyVault")), backRow))
           else {
             val (pageItems, totalPages, page) =
               ItemMenu.page(ItemStack.grouped(items), scene.withdrawPage.getOrElse(0))
             val header = content.format("bank.vault.withdrawHeader", "free" -> inv.freeSlots.toString) +
                          (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val buttons = ItemMenu.stackButtons(pageItems, WithdrawItemPrefix) ++
                           StackMove.allButton(pageItems, hot, WithdrawAllPrefix, content)
             val nav = navRow(
               back = Some(Choice("VaultMenu", content.text("bank.vault.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))),
               prev = Option.when(page > 0)(Choice("VaultWithdrawPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
               next = Option.when(page < totalPages - 1)(Choice("VaultWithdrawNext", content.text("common.next"), row = Some(ItemMenu.NavRow))))
             renderer.show(user, Screen(StackMove.withNote(note, header), buttons ++ nav))
           }
    } yield ()

  private def navigateWithdraw(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    for {
      scene <- readScene(user)
      vault <- getVault(user)
      (_, totalPages, _) = ItemMenu.page(ItemStack.grouped(vault.items.data), 0)
      page   = (scene.withdrawPage.getOrElse(0) + delta).max(0).min(totalPages - 1)
      _     <- writeScene(user, scene.copy(withdrawPage = Some(page)))
      _     <- showWithdrawItems(user, renderer)
    } yield ()

  // --- «Положить всё» и его настройка ---

  /** Сгрузить в ячейку всё отмеченное разом — и из сумки, и из других
    * хранилищ, если герой их отметил. Отчёт показываем одной сводкой: по
    * вещи на экран здесь никому не нужно. */
  private def stowAll(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero     <- getHero(user)
      vault    <- getVault(user)
      settings <- VaultStow.read(heroDao, user.userId)
      _ <- if (!vault.open) renderer.show(user, Screen(content.text("bank.vault.none"), leaveRow))
           else
             for {
               report <- VaultStow.run(heroDao, inventoryRepo, bankRepo, artifacts,
                           user.userId, hero, settings)
               _      <- renderer.show(user, Screen(stowReportText(report), Nil))
               _      <- showMenu(user, renderer)
             } yield ()
    } yield ()

  /** Сводка укладки: сперва что легло, потом откуда, и сколько места осталось. */
  private def stowReportText(report: StowReport): String =
    // «Нечего класть» — только когда и правда нечего: забитая ячейка это
    // другая беда, и говорить о ней надо своими словами.
    if (report.empty && !report.full) content.text("bank.vault.stow.nothing")
    else {
      val kinds = StowGroup.itemGroups.flatMap { g =>
        report.byGroup.get(g).map(n => s"${content.text(s"bank.vault.stow.name.${g.id}")} $n")
      }
      val head = Option.when(report.items > 0)(content.format("bank.vault.stow.done",
        "count" -> report.items.toString,
        "kinds" -> (if (kinds.isEmpty) "" else kinds.mkString(" (", ", ", ")"))))
      val silver = Option.when(report.silver > 0L)(
        content.format("bank.vault.stow.silverLine", "silver" -> report.silver.toString))
      val fromStorages = VaultStow.storages.flatMap { case (group, kind) =>
        report.fromArtifact.get(kind).map(n =>
          s"${content.text(s"bank.vault.stow.name.${group.id}")} $n")
      }
      val storages = Option.when(fromStorages.nonEmpty)(
        content.format("bank.vault.stow.fromStorages", "list" -> fromStorages.mkString(" · ")))
      val full = Option.when(report.full)(content.text("bank.vault.stow.full"))
      List(head, silver, storages, full).flatten.mkString("\n")
    }

  private def showStowSettings(user: User, renderer: Renderer): Task[Unit] =
    VaultStow.read(heroDao, user.userId)
      .flatMap(s => renderer.show(user, stowSettingsScreen(s, content.text("bank.vault.stow.header"))))

  /** Экран настройки: переключатели по трое в ряд — так все четырнадцать
    * укладываются в пять рядов, и остаётся место под «Назад» (у ВК рядов
    * всего десять, и лишний он не прощает — отклоняет весь экран). */
  private def stowSettingsScreen(s: StowSettings, text: String): Screen = {
    val groups = StowGroup.itemGroups ++ List(StowGroup.Silver) ++ StowGroup.storageGroups
    val buttons = groups.zipWithIndex.map { case (g, i) =>
      val on = s.on(g)
      Choice(
        id    = s"$StowTogglePrefix${g.id}",
        label = content.format(g.key, "state" ->
                  content.text(if (on) "bank.vault.stow.on" else "bank.vault.stow.off")),
        color = if (on) ChoiceColor.Positive else ChoiceColor.Negative,
        row   = Some(i / StowTogglesPerRow))
    }
    val rows = (groups.size + StowTogglesPerRow - 1) / StowTogglesPerRow
    Screen(text,
      buttons :+ Choice("VaultMenu", content.text("bank.vault.stow.back"),
        color = ChoiceColor.Negative, row = Some(rows)))
  }

  /** Щелчок по переключателю: длинное объяснение герой уже прочитал, когда
    * сюда зашёл, — теперь хватит строчки о том, что именно поменялось. */
  private def toggleStow(user: User, group: StowGroup, renderer: Renderer): Task[Unit] =
    VaultStow.toggle(heroDao, user.userId, group).flatMap { s =>
      val name = content.format(group.key, "state" -> "").trim
      val line = content.format(
        if (s.on(group)) "bank.vault.stow.toggledOn" else "bank.vault.stow.toggledOff",
        "what" -> name)
      renderer.show(user, stowSettingsScreen(s, line))
    }

  private def backRow: List[Choice] =
    List(Choice("VaultMenu", content.text("bank.vault.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def leaveRow: List[Choice] =
    List(Choice("LeaveVault", content.text("bank.vault.menu.leave"), color = ChoiceColor.Negative, row = Some(0)))

  private def navRow(back: Option[Choice], prev: Option[Choice], next: Option[Choice]): List[Choice] =
    List(back, prev, next).flatten

  // --- Ввод серебра ---

  private def showDepositSilver(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      vault <- getVault(user)
      text   = content.format("bank.vault.depositSilverPrompt",
                 "heroSilver" -> hero.silver.toString, "free" -> vault.freeSilverSpace.toString)
      _ <- renderer.show(user, Screen(text, allSilverRow("VaultDepositAll", "bank.vault.depositSilverAll")))
    } yield ()

  private def showWithdrawSilver(user: User, renderer: Renderer): Task[Unit] =
    for {
      vault <- getVault(user)
      text   = content.format("bank.vault.withdrawSilverPrompt", "vaultSilver" -> vault.silver.toString)
      _ <- renderer.show(user, Screen(text, allSilverRow("VaultWithdrawAll", "bank.vault.withdrawSilverAll")))
    } yield ()

  private def allSilverRow(allAction: String, allTextKey: String): List[Choice] =
    List(
      Choice(allAction, content.text(allTextKey), row = Some(0)),
      Choice("VaultMenu", content.text("bank.vault.cancelSilver"), color = ChoiceColor.Negative, row = Some(0))
    )

  private def cancelSilverRow: List[Choice] =
    List(Choice("VaultMenu", content.text("bank.vault.cancelSilver"), color = ChoiceColor.Negative, row = Some(0)))

  // --- Fallback: id предметов и текстовый ввод суммы ---

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(DepositItemPrefix) =>
        a.drop(DepositItemPrefix.length).toLongOption.fold(showMenu(user, renderer))(depositItem(user, _, renderer))
          .as(StateType.BankVault)
      case Some(a) if a.startsWith(WithdrawItemPrefix) =>
        a.drop(WithdrawItemPrefix.length).toLongOption.fold(showMenu(user, renderer))(withdrawItem(user, _, renderer))
          .as(StateType.BankVault)
      case Some(a) if a.startsWith(DepositAllPrefix) =>
        a.drop(DepositAllPrefix.length).toLongOption.fold(showMenu(user, renderer))(depositAll(user, _, renderer))
          .as(StateType.BankVault)
      case Some(a) if a.startsWith(WithdrawAllPrefix) =>
        a.drop(WithdrawAllPrefix.length).toLongOption.fold(showMenu(user, renderer))(withdrawAll(user, _, renderer))
          .as(StateType.BankVault)
      case Some(a) if a.startsWith(StowTogglePrefix) =>
        StowGroup.withId(a.drop(StowTogglePrefix.length))
          .fold(showStowSettings(user, renderer))(toggleStow(user, _, renderer))
          .as(StateType.BankVault)
      case _ =>
        readScene(user).flatMap { scene =>
          scene.vaultMode match {
            case Some(mode) => handleSilverText(user, mode, ua.text.trim, renderer).as(StateType.BankVault)
            case None       => showMenu(user, renderer).as(StateType.BankVault)
          }
        }
    }

  private def handleSilverText(user: User, mode: String, text: String, renderer: Renderer): Task[Unit] =
    text.toLongOption match {
      case None              => renderer.show(user, Screen(content.text("bank.vault.silverNotANumber"), cancelSilverRow))
      case Some(n) if n <= 0 => renderer.show(user, Screen(content.text("bank.vault.silverNonPositive"), cancelSilverRow))
      case Some(amount)      =>
        if (mode == ModeDepositSilver) doDepositSilver(user, amount, renderer)
        else                           doWithdrawSilver(user, amount, renderer)
    }

  // --- Транзакции ---

  private def depositItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      _ <- inv.items.data.find(i => i.id == itemId && !i.isQuestItem) match {
        case None => showDepositItems(user, renderer)
        case Some(item) =>
          depositOne(hero, item).flatMap {
            case None         => showDepositItems(user, renderer,
                                   Some(content.format("bank.vault.itemDeposited", "name" -> item.name)), StackMove.hotOf(item))
            case Some(reason) => showDepositItems(user, renderer, Some(reason))
          }
      }
    } yield ()

  private def withdrawItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      vault <- getVault(user)
      _ <- vault.items.data.find(_.id == itemId) match {
        case None => showWithdrawItems(user, renderer)
        case Some(item) =>
          withdrawOne(hero, item).flatMap {
            case None         => showWithdrawItems(user, renderer,
                                   Some(content.format("bank.vault.itemWithdrawn", "name" -> item.name)), StackMove.hotOf(item))
            case Some(reason) => showWithdrawItems(user, renderer, Some(reason))
          }
      }
    } yield ()

  /** «⏬ все N» у стопки сумки: остаток — в ячейку, пока принимает. */
  private def depositAll(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      items  = inv.items.data.filterNot(_.isQuestItem)
      _ <- items.find(_.id == itemId) match {
        case None => showDepositItems(user, renderer)
        case Some(item) =>
          StackMove.run(StackMove.stack(items, item))(depositOne(hero, _)).flatMap(moved =>
            showDepositItems(user, renderer, Some(StackMove.summary(content, moved, "bank.vault.itemsDeposited"))))
      }
    } yield ()

  /** «⏬ все N» у стопки в ячейке: остаток — в сумку, сколько влезет. */
  private def withdrawAll(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      vault <- getVault(user)
      _ <- vault.items.data.find(_.id == itemId) match {
        case None => showWithdrawItems(user, renderer)
        case Some(item) =>
          StackMove.run(StackMove.stack(vault.items.data, item))(withdrawOne(hero, _)).flatMap(moved =>
            showWithdrawItems(user, renderer, Some(StackMove.summary(content, moved, "bank.vault.itemsWithdrawn"))))
      }
    } yield ()

  /** Вещь из сумки в ячейку: сперва в ячейку и только потом из сумки — если
    * ячейка откажет, вещь останется при герое. `None` — легла, иначе — почему нет. */
  private def depositOne(hero: Hero, item: Item): Task[Option[String]] =
    bankRepo.deposit(hero.id, item).either.flatMap {
      case Right(_)                             => inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable).as(None)
      case Left(BankRepoError.VaultFull)        => ZIO.some(content.text("bank.vault.full"))
      case Left(BankRepoError.DustLimitReached) => ZIO.some(InventoryFeedback.refusalLine(content, item, storage = true))
      case Left(BankRepoError.NoVault)          => ZIO.some(content.text("bank.vault.none"))
      case Left(e)                              => ZIO.fail(asThrowable(e))
    }

  /** Вещь из ячейки в сумку. Пыль и малые руны места в сумке не занимают — им
    * мешает только свой предел. Не легла в сумку — возвращается в ячейку. */
  private def withdrawOne(hero: Hero, item: Item): Task[Option[String]] =
    inventoryRepo.get(hero.id).mapError(asThrowable).flatMap { inv =>
      val noRoom = if (item.weightless) !inv.hasRoomForHoard(item) else inv.freeSlots <= 0
      if (noRoom) ZIO.some(InventoryFeedback.refusalLine(content, item, storage = true))
      else
        bankRepo.withdraw(hero.id, item.id).either.flatMap {
          case Left(BankRepoError.CantFindItemToRemove) => ZIO.some(content.text("stack.gone"))
          case Left(e)                                  => ZIO.fail(asThrowable(e))
          case Right(got) =>
            inventoryRepo.addItem(hero.id, got).either.flatMap {
              case Right(_) => ZIO.none
              // редкая гонка: пока проверяли — сумку успели забить. Возвращаем в ячейку.
              case Left(InventoryRepoError.NoMorePlaceForItems | InventoryRepoError.DustLimitReached) =>
                bankRepo.deposit(hero.id, got).mapError(asThrowable).as(Some(InventoryFeedback.refusalLine(content, got, storage = true)))
              case Left(e) => ZIO.fail(asThrowable(e))
            }
        }
    }

  private def depositAllSilver(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      vault <- getVault(user)
      amount = hero.silver.min(vault.freeSilverSpace)
      _ <- if (amount <= 0) renderer.show(user, Screen(content.text("bank.vault.silverNothingToDeposit"), allSilverRow("VaultDepositAll", "bank.vault.depositSilverAll")))
           else doDepositSilver(user, amount, renderer)
    } yield ()

  private def withdrawAllSilver(user: User, renderer: Renderer): Task[Unit] =
    for {
      vault <- getVault(user)
      _ <- if (vault.silver <= 0) renderer.show(user, Screen(content.text("bank.vault.silverNothingToWithdraw"), allSilverRow("VaultWithdrawAll", "bank.vault.withdrawSilverAll")))
           else doWithdrawSilver(user, vault.silver, renderer)
    } yield ()

  private def doDepositSilver(user: User, amount: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      _ <- if (amount > hero.silver)
             renderer.show(user, Screen(content.text("bank.vault.silverNotEnoughHero"), cancelSilverRow))
           else
             bankRepo.depositSilver(hero.id, amount).foldZIO(
               {
                 case BankRepoError.SilverOverflow    => renderer.show(user, Screen(content.text("bank.vault.silverOverflow"), cancelSilverRow))
                 case BankRepoError.NonPositiveAmount => renderer.show(user, Screen(content.text("bank.vault.silverNonPositive"), cancelSilverRow))
                 case BankRepoError.NoVault           => renderer.show(user, Screen(content.text("bank.vault.none"), leaveRow))
                 case e                               => ZIO.fail(asThrowable(e))
               },
               _ => heroDao.updateSilver(user.userId, hero.silver - amount) *>
                    renderer.show(user, Screen(content.format("bank.vault.silverDeposited", "amount" -> amount.toString), Nil)) *>
                    resetScene(user) *> showMenu(user, renderer)
             )
    } yield ()

  private def doWithdrawSilver(user: User, amount: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      _ <- bankRepo.withdrawSilver(hero.id, amount).foldZIO(
             {
               case BankRepoError.NotEnoughSilver   => renderer.show(user, Screen(content.text("bank.vault.silverNotEnoughVault"), cancelSilverRow))
               case BankRepoError.NonPositiveAmount => renderer.show(user, Screen(content.text("bank.vault.silverNonPositive"), cancelSilverRow))
               case e                               => ZIO.fail(asThrowable(e))
             },
             _ => heroDao.updateSilver(user.userId, hero.silver + amount) *>
                  renderer.show(user, Screen(content.format("bank.vault.silverWithdrawn", "amount" -> amount.toString), Nil)) *>
                  resetScene(user) *> showMenu(user, renderer)
           )
    } yield ()

  // --- Вспомогательное ---

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def getVault(user: User): Task[BankVault] =
    getHero(user).flatMap(h => bankRepo.get(h.id).mapError(asThrowable))

  private def readScene(user: User): Task[VaultScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[VaultScene].toOption).getOrElse(VaultScene()))

  private def writeScene(user: User, scene: VaultScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def resetScene(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object BankVaultState {
  val DepositItemPrefix  = "VaultPut_"
  val WithdrawItemPrefix = "VaultTake_"
  // «⏬ все N»: за префиксом — id любой вещи стопки.
  val DepositAllPrefix   = "VaultPutAll_"
  val WithdrawAllPrefix  = "VaultTakeAll_"
  val StowTogglePrefix   = "VaultStow_"
  val ModeDepositSilver  = "vaultDepositSilver"
  val ModeWithdrawSilver = "vaultWithdrawSilver"

  /** По трое в ряд: столько переключателей в десять рядов иначе не уложить, а
    * подписи под такую ширину короткие — название и значок. */
  val StowTogglesPerRow: Int = 3

  case class VaultScene(
    vaultMode:    Option[String] = None,
    depositPage:  Option[Int]    = None,
    withdrawPage: Option[Int]    = None
  )
  object VaultScene {
    implicit val encoder: Encoder[VaultScene] = deriveEncoder
    implicit val decoder: Decoder[VaultScene] = deriveDecoder
  }
}
