package pangea.service.state.states.artifact

import io.circe.{Decoder, Encoder, jawn}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.domain.Rng
import pangea.generator.item.CubeCraft
import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{BrewKind, Item, ItemStack}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.{ArtifactRepoError, ArtifactRepository}
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.repository.item.ItemRepository
import pangea.service.state.states.artifact.ArtifactState._
import pangea.service.state.{HerbLore, InventoryFeedback, ItemMenu, MarisaQuest, StackMove, State, UiScene, UserAction}
import zio.{Random, Task, ZIO}

/** Экран сборного артефакта: Ларца Азата или Живой сумки (вид задаётся при
 *  сборке состояния). Внутрь можно заглянуть, забрать своё и доложить новое, а
 *  «магия Азата» за заряд делает то, ради чего артефакт и держат: плавит три
 *  одинаковых камня в один категорией выше или варит отвар из трав. */
case class ArtifactState(
  kind:          ArtifactKind,
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  artifacts:     ArtifactRepository,
  content:       SceneContent
) extends State {

  private val self = kind.state

  private val branch = new Branch(
    routes = Map(
      "ArtifactMenu"  -> Target.Run { (u, _, r) => resetScene(u) *> showMenu(u, r).as(self) },
      "ArtifactPut"   -> Target.Run { (u, _, r) => writeScene(u, ArtifactScene(putPage = Some(0))) *> showPutList(u, r).as(self) },
      "ArtifactPutPrev"  -> Target.Run { (u, _, r) => turnPut(u, r, -1).as(self) },
      "ArtifactPutNext"  -> Target.Run { (u, _, r) => turnPut(u, r, +1).as(self) },
      "ArtifactTake"  -> Target.Run { (u, _, r) => writeScene(u, ArtifactScene(takePage = Some(0))) *> showTakeList(u, r).as(self) },
      "ArtifactTakePrev" -> Target.Run { (u, _, r) => turnTake(u, r, -1).as(self) },
      "ArtifactTakeNext" -> Target.Run { (u, _, r) => turnTake(u, r, +1).as(self) },
      "ArtifactMagic" -> Target.Run { (u, _, r) => magic(u, r).as(self) },
      "ArtifactTakeAll" -> Target.Run { (u, _, r) => takeEverything(u, r).as(self) },
      "LeaveArtifact" -> Target.Goto(StateType.Backpack)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    resetScene(user) *> showMenu(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Меню артефакта ─────────────────────────────────────────────────────────

  /** Меню артефакта. `note` — что только что случилось («Забрать всё»):
    * строкой над меню, а не отдельным сообщением. */
  private def showMenu(user: User, renderer: Renderer, note: Option[String] = None): Task[Unit] =
    mine(user).flatMap { a =>
      if (!a.owned) renderer.show(user, Screen(content.text(s"artifact.$key.notOwned"), leaveRow))
      else renderer.show(user, Screen(StackMove.withNote(note, menuText(a)), List(
        Some(Choice("ArtifactPut",   content.text("artifact.putLabel"),  row = Some(0))),
        Some(Choice("ArtifactTake",  content.text("artifact.takeLabel"), row = Some(0))),
        // Выгрести артефакт целиком — как почту.
        Option.when(a.items.data.nonEmpty)(Choice("ArtifactTakeAll", content.text("stack.takeAll"), row = Some(0))),
        Option.when(kind.hasMagic)(Choice("ArtifactMagic", content.text(s"artifact.$key.magicLabel"),
          color = ChoiceColor.Positive, row = Some(1))),
        Some(Choice("LeaveArtifact", content.text("artifact.back"), color = ChoiceColor.Negative, row = Some(2)))
      ).flatten))
    }

  private def menuText(a: Artifact): String =
    // У шкафа зарядов нет — и строки про них тоже.
    content.format(if (kind.hasMagic) "artifact.menu" else "artifact.menuPlain",
      "title"      -> content.text(s"artifact.$key.title"),
      "tier"       -> a.tier.toString,
      "maxTier"    -> HeroArtifacts.MaxTier.toString,
      "items"      -> a.occupied.toString,
      "capacity"   -> a.capacity.toString,
      "charges"    -> a.charges.toString,
      "maxCharges" -> HeroArtifacts.MaxCharges.toString)

  // ── Положить / забрать ─────────────────────────────────────────────────────

  /** Что можно положить. `note` — что только что случилось: оно идёт строкой
    * над списком, а не отдельным сообщением. `hot` — стопка, из которой только
    * что положили одну: рядом с ней «⏬ все N». */
  private def showPutList(user: User, renderer: Renderer, note: Option[String] = None,
                          hot: Option[String] = None): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      a     <- mine(user)
      scene <- readScene(user)
      items  = inv.items.data.filter(i => kind.accepts(i) && !i.isQuestItem)
      _ <- if (items.isEmpty) renderer.show(user, Screen(StackMove.withNote(note, content.text(s"artifact.$key.nothingToPut")), backRow))
           else {
             val (pageItems, pages, p) = ItemMenu.page(ItemStack.grouped(items), scene.putPage.getOrElse(0))
             val header = content.format("artifact.putHeader",
               "free" -> a.freeSlots.toString, "page" -> (p + 1).toString, "total" -> pages.toString)
             renderer.show(user, Screen(StackMove.withNote(note, header),
               ItemMenu.stackButtons(pageItems, PutPrefix) ++
                 StackMove.allButton(pageItems, hot, PutAllPrefix, content) ++
                 navRow("ArtifactPutPrev", "ArtifactPutNext", p, pages)))
           }
    } yield ()

  /** Что лежит внутри; `note` и `hot` — как у [[showPutList]]. */
  private def showTakeList(user: User, renderer: Renderer, note: Option[String] = None,
                           hot: Option[String] = None): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      a     <- mine(user)
      scene <- readScene(user)
      _ <- if (a.items.data.isEmpty) renderer.show(user, Screen(StackMove.withNote(note, content.text(s"artifact.$key.empty")), backRow))
           else {
             val (pageItems, pages, p) = ItemMenu.page(ItemStack.grouped(a.items.data), scene.takePage.getOrElse(0))
             val header = content.format("artifact.takeHeader",
               "free" -> inv.freeSlots.toString, "page" -> (p + 1).toString, "total" -> pages.toString)
             renderer.show(user, Screen(StackMove.withNote(note, header),
               ItemMenu.stackButtons(pageItems, TakePrefix) ++
                 StackMove.allButton(pageItems, hot, TakeAllPrefix, content) ++
                 navRow("ArtifactTakePrev", "ArtifactTakeNext", p, pages)))
           }
    } yield ()

  private def turnPut(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    readScene(user).flatMap(s => writeScene(user, s.copy(putPage = Some((s.putPage.getOrElse(0) + delta).max(0))))) *>
      showPutList(user, renderer)

  private def turnTake(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    readScene(user).flatMap(s => writeScene(user, s.copy(takePage = Some((s.takePage.getOrElse(0) + delta).max(0))))) *>
      showTakeList(user, renderer)

  private def putItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      _ <- inv.items.data.find(i => i.id == itemId && kind.accepts(i) && !i.isQuestItem) match {
        case None => showPutList(user, renderer)
        case Some(item) =>
          putOne(hero, item).flatMap {
            case Right(a)     => showPutList(user, renderer, Some(content.format("artifact.putDone",
                                   "name" -> item.displayTitle, "free" -> a.freeSlots.toString)), StackMove.hotOf(item))
            case Left(reason) => showPutList(user, renderer, Some(reason))
          }
      }
    } yield ()

  private def takeItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      a    <- mine(user)
      _ <- a.items.data.find(_.id == itemId) match {
        case None => showTakeList(user, renderer)
        case Some(item) =>
          takeOne(hero, item).flatMap {
            case None         => showTakeList(user, renderer, Some(content.format("artifact.takeDone",
                                   "name" -> item.displayTitle)), StackMove.hotOf(item))
            case Some(reason) => showTakeList(user, renderer, Some(reason))
          }
      }
    } yield ()

  /** «⏬ все N» у стопки сумки: остаток — внутрь, пока есть место. */
  private def putAll(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      items  = inv.items.data.filter(i => kind.accepts(i) && !i.isQuestItem)
      _ <- items.find(_.id == itemId) match {
        case None => showPutList(user, renderer)
        case Some(item) =>
          for {
            moved <- StackMove.run(StackMove.stack(items, item))(putOne(hero, _).map(_.left.toOption))
            a     <- mine(user)
            _     <- showPutList(user, renderer,
                       Some(StackMove.summary(content, moved, "artifact.putAll", "free" -> a.freeSlots.toString)))
          } yield ()
      }
    } yield ()

  /** «⏬ все N» у стопки внутри: остаток — в сумку, сколько влезет. */
  private def takeAll(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      a    <- mine(user)
      _ <- a.items.data.find(_.id == itemId) match {
        case None => showTakeList(user, renderer)
        case Some(item) =>
          StackMove.run(StackMove.stack(a.items.data, item))(takeOne(hero, _)).flatMap(moved =>
            showTakeList(user, renderer, Some(StackMove.summary(content, moved, "artifact.takenAll"))))
      }
    } yield ()

  /** «⏬ Забрать всё»: артефакт целиком — в сумку, сколько влезет. Не влезла
    * одна вещь — пробуем следующую: пыли и рунам сумкино место не нужно. */
  private def takeEverything(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      a    <- mine(user)
      note <- if (a.items.data.isEmpty) ZIO.succeed(content.text(s"artifact.$key.empty"))
              else StackMove.run(a.items.data, stopAtFirst = false)(takeOne(hero, _))
                     .map(StackMove.summary(content, _, "artifact.takenAll"))
      _    <- showMenu(user, renderer, Some(note))
    } yield ()

  /** Вещь из сумки внутрь: сперва внутрь и только потом из сумки — откажет
    * артефакт, вещь останется при герое. Справа — артефакт после, слева — почему нет. */
  private def putOne(hero: Hero, item: Item): Task[Either[String, Artifact]] =
    artifacts.put(hero.id, kind, item).either.flatMap {
      case Right(a)                         => inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable).as(Right(a))
      case Left(ArtifactRepoError.NoRoom)   => ZIO.left(content.text(s"artifact.$key.full"))
      case Left(e)                          => ZIO.fail(asThrowable(e))
    }

  /** Вещь изнутри в сумку. Не легла в сумку — возвращается в артефакт. */
  private def takeOne(hero: Hero, item: Item): Task[Option[String]] =
    inventoryRepo.get(hero.id).mapError(asThrowable).flatMap { inv =>
      val noRoom = if (item.weightless) !inv.hasRoomForHoard(item) else inv.freeSlots <= 0
      if (noRoom) ZIO.some(InventoryFeedback.refusalLine(content, item))
      else
        artifacts.take(hero.id, kind, item.id).either.flatMap {
          case Left(_) => ZIO.some(content.text("stack.gone"))
          case Right(got) =>
            inventoryRepo.addItem(hero.id, got).either.flatMap {
              case Right(_) => ZIO.none
              // Сумку успели забить — возвращаем вещь артефакту.
              case Left(InventoryRepoError.NoMorePlaceForItems | InventoryRepoError.DustLimitReached) =>
                artifacts.put(hero.id, kind, got).mapError(asThrowable).as(Some(InventoryFeedback.refusalLine(content, got)))
              case Left(e) => ZIO.fail(asThrowable(e))
            }
        }
    }

  // ── Магия Азата ────────────────────────────────────────────────────────────

  private def magic(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      a    <- mine(user)
      _ <- if (!a.owned || !kind.hasMagic) renderer.show(user, Screen(content.text(s"artifact.$key.notOwned"), leaveRow))
           else if (a.charges <= 0)
             renderer.show(user, Screen(content.format("artifact.noCharges",
               "cost" -> HeroArtifacts.RechargeSilver.toString), Nil)) *> showMenu(user, renderer)
           else runMagic(user, hero, a, renderer)
    } yield ()

  private def runMagic(user: User, hero: Hero, a: Artifact, renderer: Renderer): Task[Unit] =
    for {
      seed  <- Random.nextLong
      result = kind match {
                 case ArtifactKind.Casket    => CubeCraft.upgradeGems(a.items.data, a.charges, Rng(seed))
                 case ArtifactKind.LivingBag => CubeCraft.brewHerbs(a.items.data, a.charges, Rng(seed))
                 // У шкафа магии нет — сюда он не попадает (кнопки тоже нет).
                 case ArtifactKind.Wardrobe  => CubeCraft.Result(a.items.data, 0, Rng(seed))
               }
      // Свежесозданному (id <= 0) нужен свой id — как и результатам куба.
      persisted <- ZIO.foreach(result.items)(i => if (i.id > 0L) ZIO.succeed(i) else itemRepo.persist(hero.id, i))
      _ <- if (!result.anyApplied)
             renderer.show(user, Screen(content.text(s"artifact.$key.magicNothing"), Nil)) *> showMenu(user, renderer)
           else
             artifacts.applyMagic(hero.id, kind, persisted, result.chargesUsed).mapError(asThrowable) *>
               renderer.show(user, Screen(content.format(s"artifact.$key.magicDone",
                 "count" -> result.chargesUsed.toString), Nil)) *>
               brewedAchievement(user, hero, result.items, renderer) *>
               showMenu(user, renderer)
    } yield ()

  /** Сваренное в сумке идёт в тот же счёт «Зельевара I», что и сваренное в кубе. */
  private def brewedAchievement(user: User, hero: Hero, made: List[pangea.model.item.Item], renderer: Renderer): Task[Unit] = {
    val brewed = made.filter(_.id <= 0L).flatMap(_.brew).map(_.entryName)
    ZIO.when(kind == ArtifactKind.LivingBag && brewed.nonEmpty) {
      for {
        lore   <- HerbLore.readLore(heroDao, user.userId)
        updated = lore.brewedAlso(brewed)
        _      <- HerbLore.writeLore(heroDao, user.userId, updated)
        _      <- ZIO.when(BrewKind.rank1.forall(k => updated.brewed.contains(k.entryName)))(
                    MarisaQuest.grant(heroDao, content, user, hero, Achievement.Brewer1, renderer))
      } yield ()
    }.unit
  }

  // ── Вспомогательное ────────────────────────────────────────────────────────

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(PutPrefix) =>
        a.drop(PutPrefix.length).toLongOption.fold(showMenu(user, renderer))(putItem(user, _, renderer)).as(self)
      case Some(a) if a.startsWith(TakePrefix) =>
        a.drop(TakePrefix.length).toLongOption.fold(showMenu(user, renderer))(takeItem(user, _, renderer)).as(self)
      case Some(a) if a.startsWith(PutAllPrefix) =>
        a.drop(PutAllPrefix.length).toLongOption.fold(showMenu(user, renderer))(putAll(user, _, renderer)).as(self)
      case Some(a) if a.startsWith(TakeAllPrefix) =>
        a.drop(TakeAllPrefix.length).toLongOption.fold(showMenu(user, renderer))(takeAll(user, _, renderer)).as(self)
      case _ => showMenu(user, renderer).as(self)
    }

  private def navRow(prevId: String, nextId: String, page: Int, pages: Int): List[Choice] =
    List(
      Some(Choice("ArtifactMenu", content.text("artifact.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))),
      Option.when(page > 0)(Choice(prevId, content.text("common.prev"), row = Some(ItemMenu.NavRow))),
      Option.when(page < pages - 1)(Choice(nextId, content.text("common.next"), row = Some(ItemMenu.NavRow)))
    ).flatten

  private def backRow: List[Choice] =
    List(Choice("ArtifactMenu", content.text("artifact.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def leaveRow: List[Choice] =
    List(Choice("LeaveArtifact", content.text("artifact.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def key: String = kind.key

  private def mine(user: User): Task[Artifact] =
    getHero(user).flatMap(h => artifacts.get(h.id).mapError(asThrowable)).map(_.of(kind))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  // Страницы хранилища живут в своём ключе сцены. Раньше они занимали колонку
  // целиком и стирали собой событие, в котором стоит игрок: сходить в Ларец из
  // «Персонажа» перед караваном значило вернуться к другому каравану — та же беда,
  // что когда-то была с логовом элементаля (см. [[UiScene]]).
  private def readScene(user: User): Task[ArtifactScene] =
    UiScene.read(heroDao, user.userId, UiScene.Artifact, ArtifactScene())

  private def writeScene(user: User, scene: ArtifactScene): Task[Unit] =
    UiScene.write(heroDao, user.userId, UiScene.Artifact, scene)

  private def resetScene(user: User): Task[Unit] =
    UiScene.clear(heroDao, user.userId, UiScene.Artifact)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object ArtifactState {
  val PutPrefix  = "ArtPut_"
  val TakePrefix = "ArtTake_"
  // «⏬ все N»: за префиксом — id любой вещи стопки.
  val PutAllPrefix  = "ArtPutAll_"
  val TakeAllPrefix = "ArtTakeAll_"

  case class ArtifactScene(putPage: Option[Int] = None, takePage: Option[Int] = None)
  object ArtifactScene {
    implicit val encoder: Encoder[ArtifactScene] = deriveEncoder
    implicit val decoder: Decoder[ArtifactScene] = deriveDecoder
  }
}
