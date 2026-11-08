package pangea.service.miniapp

import pangea.dao.artifact.ArtifactDao
import pangea.dao.bank.BankVaultDao
import pangea.dao.barrel.BarrelDao
import pangea.dao.hero.HeroDao
import pangea.dao.top.TopDao
import pangea.engine.{Players, Renderer, SceneContent, Screen}
import pangea.model.artifact.HeroArtifacts
import pangea.model.auction.AuctionLot
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.home.HomeData
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.model.state.StateType
import pangea.model.top.{TopBoard, TopKind}
import pangea.model.user.User
import pangea.repository.auction.AuctionRepository
import pangea.repository.bank.BankRepository
import pangea.repository.hero.HeroRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.repository.user.UserRepository
import pangea.service.auction.AuctionPurchase
import pangea.service.cube.AzatCube
import pangea.service.equipment.Outfit
import pangea.service.miniapp.MiniAppView.{AuctionActionView, AuctionView, CharacterView, CraftActionView, CraftView,
  InventoryActionView, InventoryView, QuestsView, SquadActionView, SquadView, TopView}
import pangea.service.payout.Payouts
import pangea.service.squad.SquadOrders
import pangea.service.state.{AzatData, HerbLore, NpcQuestLog, PlayerLock, QuestJournal, SquadDuty}
import pangea.service.state.states.{EquipmentState, InventoryState}
import pangea.service.state.states.home.Racks
import zio.{Ref, Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** VK Mini App: персонаж, задания, отряд, инвентарь, крафт, топ и аукцион вне
  * чата. Просмотр безопасен из любого места, даже из боя: сцены, лок игрока и
  * `hero.state` не трогаются, а данные только читаются. Что меняет игру, идёт
  * под локом игрока — тем же, что и ходы в чате: куб Азата и покупка на
  * аукционе — только когда герой в городе, снаряжение и отряд — когда он не
  * в бою. */
trait MiniApp {

  /** Игрок по параметрам запуска приложения. Героя здесь не заводим: игру
    * начинают в сообщениях сообщества. */
  def player(launchParams: String): Task[Either[MiniApp.Denied, Hero]]

  def character(hero: Hero): Task[CharacterView]

  def quests(hero: Hero): Task[QuestsView]

  def squad(hero: Hero): Task[SquadView]

  /** Приказ отряду — как в «Отряде» в боте. Ответ — строй после него и что
    * сказать. */
  def squadOrder(hero: Hero, act: MiniApp.SquadAct): Task[SquadActionView]

  def inventory(hero: Hero): Task[InventoryView]

  def auction(hero: Hero): Task[AuctionView]

  /** Купить лот — как в Торговом доме: вещь в сумку. Ответ — витрина после
    * покупки и что сказать. */
  def buy(hero: Hero, lotId: Long): Task[AuctionActionView]

  /** Топ: первые [[MiniApp.TopSize]] и место самого героя. */
  def top(hero: Hero, kind: TopKind): Task[TopView]

  /** Надеть или снять — как в карточке слота в боте. Ответ — вещи после
    * этого и что сказать. */
  def gear(hero: Hero, act: MiniApp.GearAct): Task[InventoryActionView]

  /** Надеть комплект со стойки №`n` — обменяться с ней надетым, как кнопкой
    * стойки дома. Только в городе. */
  def rack(hero: Hero, n: Int): Task[InventoryActionView]

  /** «Крафт»: куб Азата и сумка, из которой в него кладут. */
  def craft(hero: Hero): Task[CraftView]

  /** Действие с кубом и куб после него. */
  def cube(hero: Hero, act: MiniApp.CubeAct): Task[CraftActionView]
}

object MiniApp {

  sealed trait Denied
  object Denied {
    /** Ключ приложения не задан — API выключен. */
    case object Off extends Denied
    final case class Rejected(why: VkLaunchParams.Rejected) extends Denied
    /** Подпись верна, но героя у этого игрока нет. */
    case object NoHero extends Denied
  }

  /** Сколько лотов витрины отдаём разом: приложение фильтрует и сортирует их
    * само, а больше игрок и не пролистает. */
  val MaxLots: Int = 500

  /** Сколько героев показывает топ. */
  val TopSize: Int = 100

  /** Сколько снимок топа живёт в памяти: сколько бы игроков его ни открыли,
    * база считает его раз в столько. */
  val TopTtlMs: Long = 60L * 1000L

  /** Снимок топа, снятый в `at`. */
  final case class CachedTop(at: Long, board: TopBoard)

  /** Что делают с кубом из приложения: переносят вещь пальцем туда или
    * обратно, забирают всё, активируют. */
  sealed trait CubeAct
  object CubeAct {
    final case class Deposit(itemId: Long)  extends CubeAct
    final case class Withdraw(itemId: Long) extends CubeAct
    case object TakeAll                     extends CubeAct
    case object Activate                    extends CubeAct
  }

  /** Что делают со снаряжением: надевают вещь из сумки в слот или снимают
    * надетое. Слот — номер в [[EquipmentState.slots]]. */
  sealed trait GearAct
  object GearAct {
    final case class Wear(slot: Int, itemId: Long) extends GearAct
    final case class TakeOff(slot: Int)            extends GearAct
  }

  /** Приказы отряду: переставить союзника (с места на место) или героя,
    * выгнать союзника, решить про восставшего — занять им место союзника или
    * отпустить. */
  sealed trait SquadAct
  object SquadAct {
    final case class Move(from: Int, to: Int) extends SquadAct
    final case class MoveHero(to: Int)        extends SquadAct
    final case class Dismiss(at: Int)         extends SquadAct
    final case class TakeRisen(at: Int)       extends SquadAct
    case object DismissRisen                  extends SquadAct
  }

  /** Чем кончилось действие: что сказать и номера вещей, что получились. */
  private[miniapp] final case class Outcome(note: Option[String], made: List[Long])

  /** Строки, которые куб показал бы в чате (итог активации, достижение), —
    * в ответ приложению, а не в беседу: там свой экран со своими кнопками. */
  private[miniapp] final class Collect(lines: zio.Ref[Vector[String]]) extends Renderer {
    def show(user: User, screen: Screen): Task[Unit] = lines.update(_ :+ screen.text)
  }

  /** Герой в городе — только тогда из приложения можно работать с кубом,
    * покупать на аукционе и перекладывать вещи по хранилищам, как и
    * передавать их. */
  def inCity(hero: Hero): Boolean = StateType.cityStates.contains(hero.state)

  /** Герой в бою — тогда снаряжение и строй из приложения не меняют: ход боя
    * считается на надетом и на том, кто где стоит. Любой бой — в лабиринте,
    * сюжетный, на арене — это [[StateType.Battle]]. */
  def inBattle(hero: Hero): Boolean = hero.state == StateType.Battle

  val live: ZLayer[MiniAppConfig with UserRepository with HeroRepository with InventoryRepository with AuctionRepository
                     with HeroDao with ArtifactDao with BarrelDao with BankVaultDao with TopDao
                     with ItemRepository with BankRepository with Payouts with Players with PlayerLock
                     with SceneContent, Nothing, MiniApp] =
    ZLayer {
      for {
        config      <- ZIO.service[MiniAppConfig]
        users       <- ZIO.service[UserRepository]
        heroes      <- ZIO.service[HeroRepository]
        inventories <- ZIO.service[InventoryRepository]
        lots        <- ZIO.service[AuctionRepository]
        heroDao     <- ZIO.service[HeroDao]
        artifacts   <- ZIO.service[ArtifactDao]
        barrels     <- ZIO.service[BarrelDao]
        vaults      <- ZIO.service[BankVaultDao]
        tops        <- ZIO.service[TopDao]
        items       <- ZIO.service[ItemRepository]
        bank        <- ZIO.service[BankRepository]
        payouts     <- ZIO.service[Payouts]
        players     <- ZIO.service[Players]
        lock        <- ZIO.service[PlayerLock]
        content     <- ZIO.service[SceneContent]
        topCache    <- Ref.make(Map.empty[TopKind, CachedTop])
      } yield new MiniAppLive(config, users, heroes, inventories, lots, heroDao, artifacts, barrels, vaults, content,
        tops, topCache, new AzatCube(heroDao, inventories, items, content),
        new AuctionPurchase(heroDao, inventories, lots, users, bank, payouts, players, content),
        new Outfit(heroDao, inventories, items, content), new SquadOrders(heroDao, content), lock)
    }
}

final class MiniAppLive(
  config:      MiniAppConfig,
  users:       UserRepository,
  heroes:      HeroRepository,
  inventories: InventoryRepository,
  lots:        AuctionRepository,
  heroDao:     HeroDao,
  artifacts:   ArtifactDao,
  barrels:     BarrelDao,
  vaults:      BankVaultDao,
  content:     SceneContent,
  tops:        TopDao,
  topCache:    Ref[Map[TopKind, MiniApp.CachedTop]],
  azatCube:    AzatCube,
  purchase:    AuctionPurchase,
  outfit:      Outfit,
  orders:      SquadOrders,
  lock:        PlayerLock
) extends MiniApp {
  import MiniApp.{CubeAct, Denied, GearAct, Outcome, SquadAct}

  override def player(launchParams: String): Task[Either[Denied, Hero]] =
    config.vkSecret.map(_.trim).filter(_.nonEmpty) match {
      case None => ZIO.left(Denied.Off)
      case Some(secret) =>
        now.flatMap(ms => VkLaunchParams.verify(launchParams, secret, ms) match {
          case Left(why) => ZIO.left(Denied.Rejected(why))
          case Right(vkId) =>
            users.getUserByVkId(vkId).flatMap {
              case None       => ZIO.left(Denied.NoHero)
              case Some(user) => heroes.getHero(user.userId).map(_.toRight(Denied.NoHero))
            }
        })
    }

  /** Отдыхи, которые задолжало благословение, бот доначислит и запишет сам при
    * первом же ходе ([[AzatData.load]]). Приложение ничего не пишет, поэтому
    * считает их на голом снимке и показывает уже с ними. */
  override def character(hero: Hero): Task[CharacterView] =
    for {
      ms   <- now
      azat <- AzatData.read(heroDao, hero.userId)
      lore <- HerbLore.readLore(heroDao, hero.userId)
    } yield MiniAppView.character(hero, azat.withDailyRests(ms), lore, content, ms)

  override def quests(hero: Hero): Task[QuestsView] =
    for {
      ms      <- now
      journal <- QuestJournal.load(heroDao, content, hero, ms)
    } yield MiniAppView.quests(hero, journal, content)

  override def squad(hero: Hero): Task[SquadView] =
    now.map(ms => MiniAppView.squad(hero, content, ms))

  /** Под локом игрока, героя перечитываем уже под ним. В бою — ничего не
    * трогаем, говорим почему. Сначала ушедшие уходят по-настоящему
    * ([[SquadDuty.settle]]): приложение уже показывало строй без них, и
    * приказ должен лечь на тот же строй. */
  override def squadOrder(hero: Hero, act: SquadAct): Task[SquadActionView] =
    lock.withLock(hero.userId) {
      for {
        fresh <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        note  <- if (MiniApp.inBattle(fresh)) ZIO.succeed(content.text("miniapp.squad.inBattle"))
                 else order(fresh, act)
      } yield fresh -> note
    }.flatMap { case (fresh, note) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(squad)
        .map(SquadActionView(_, Option.when(note.nonEmpty)(note)))
    }

  private def order(hero: Hero, act: SquadAct): Task[String] =
    for {
      user    <- users.getUserById(hero.userId).someOrFail(new Throwable(s"Нет игрока ${hero.userId.value}"))
      lines   <- Ref.make(Vector.empty[String])
      say      = new MiniApp.Collect(lines)
      ms      <- now
      settled <- SquadDuty.settle(heroDao, content, user, hero, ms, say)
      _       <- act match {
                   case SquadAct.Move(from, to) => orders.move(user, settled, from, to, say)
                   case SquadAct.MoveHero(to)   => orders.moveHero(user, settled, to, say)
                   case SquadAct.Dismiss(at)    => orders.dismiss(user, settled, at, say)
                   case SquadAct.TakeRisen(at)  => orders.takeRisenPlace(user, settled, at, say)
                   case SquadAct.DismissRisen   => orders.dismissRisen(user, settled, say)
                 }
      said    <- lines.get
    } yield said.mkString("\n\n")

  /** Хранилища читаются без записи: чего у героя ещё не заводили, того и нет —
    * пустая бочка, ни одной ячейки, ни одного артефакта. */
  override def inventory(hero: Hero): Task[InventoryView] =
    for {
      inv      <- inventories.get(hero.id).mapError(e => new Throwable(e.toString))
      arts     <- artifacts.find(hero.id).map(_.getOrElse(HeroArtifacts.empty(hero.id)))
      barrel   <- barrels.find(hero.id).map(_.getOrElse(Barrel.empty(hero.id)))
      vault    <- vaults.find(hero.id).map(_.getOrElse(BankVault.empty(hero.id)))
      home     <- heroDao.readHomeData(hero.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
      describe <- describer(hero)
      ms       <- now
    } yield MiniAppView.inventory(hero, inv, MiniAppView.Stores(arts, barrel, vault, home), content, describe, ms)

  /** С витриной — чем платить и куда класть: серебро на руках вместе с
    * ячейкой (её читаем без записи) и свободные места в сумке. */
  override def auction(hero: Hero): Task[AuctionView] =
    for {
      ms     <- now
      page   <- lots.page(ms, 0, MiniApp.MaxLots).mapError(e => new Throwable(e.toString))
      (onSale, _, _) = page
      mine   <- lots.mine(hero.id, AuctionLot.MaxLots.toLong).mapError(e => new Throwable(e.toString))
      vault  <- vaults.find(hero.id).map(_.fold(0L)(_.silver))
      inv    <- inventories.get(hero.id).mapError(e => new Throwable(e.toString))
      describe <- describer(hero)
    } yield MiniAppView.auction(hero, onSale, mine, ms, describe,
      MiniAppView.Funds(hero.silver + vault, inv.freeSlots), content)

  /** Под локом покупателя, героя перечитываем уже под ним. Не в городе —
    * ничего не трогаем, говорим почему. */
  override def buy(hero: Hero, lotId: Long): Task[AuctionActionView] =
    lock.withLock(hero.userId) {
      for {
        fresh <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        said  <- if (!MiniApp.inCity(fresh)) ZIO.succeed(content.text("miniapp.auction.away") -> false)
                 else users.getUserById(fresh.userId).someOrFail(new Throwable(s"Нет игрока ${fresh.userId.value}"))
                        .flatMap(purchase.buy(_, fresh, lotId)).map(purchaseNote)
      } yield fresh -> said
    }.flatMap { case (fresh, (note, bought)) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(auction)
        .map(AuctionActionView(_, note, bought))
    }

  /** Под локом игрока, героя перечитываем уже под ним. В бою — ничего не
    * трогаем, говорим почему. */
  override def gear(hero: Hero, act: GearAct): Task[InventoryActionView] =
    lock.withLock(hero.userId) {
      for {
        fresh <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        note  <- if (MiniApp.inBattle(fresh)) ZIO.succeed(content.text("miniapp.equipment.inBattle"))
                 else gearAct(fresh, act)
      } yield fresh -> note
    }.flatMap { case (fresh, note) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(inventory)
        .map(InventoryActionView(_, Option.when(note.nonEmpty)(note)))
    }

  /** Те же проверки и слова, что у карточки слота в боте: в слот — только то,
    * что в него встаёт ([[EquipmentState.fitting]]). */
  private def gearAct(hero: Hero, act: GearAct): Task[String] =
    for {
      user  <- users.getUserById(hero.userId).someOrFail(new Throwable(s"Нет игрока ${hero.userId.value}"))
      lines <- Ref.make(Vector.empty[String])
      say    = new MiniApp.Collect(lines)
      gone   = say.show(user, Screen(content.text("equipment.wearGone"), Nil))
      _     <- act match {
                 case GearAct.Wear(idx, id) =>
                   EquipmentState.slots.lift(idx) match {
                     case None => gone
                     case Some(slot) =>
                       inventories.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
                         EquipmentState.fitting(hero, slot, inv.items.data).find(_.id == id) match {
                           case None       => gone
                           case Some(item) => outfit.wear(user, hero, item, slot.pairSlot, say)(Outfit.wornLine(content, item, _)).unit
                         }
                       }
                   }
                 case GearAct.TakeOff(idx) =>
                   EquipmentState.slots.lift(idx) match {
                     case None       => say.show(user, Screen(content.text("equipment.slotEmpty"), Nil))
                     case Some(slot) => outfit.takeOff(user, hero, slot, say).unit
                   }
               }
      said  <- lines.get
    } yield said.mkString("\n\n")

  /** Проверки и слова — у [[Racks]], как у кнопки стойки дома: что не так
    * (не по уровню, сумка не вместит), а вышло — что поменялись. */
  override def rack(hero: Hero, n: Int): Task[InventoryActionView] =
    inCityAct(hero) { (user, say) =>
      racks.swap(user, n, say).flatMap {
        case Left(why) => say.show(user, Screen(why, Nil))
        case Right(_)  => say.show(user, Screen(content.format("home.rack.swapped", "n" -> n.toString), Nil))
      }
    }

  private val racks = Racks(heroDao, inventories, content)

  /** Действие с хранилищами: под локом игрока, героя перечитываем уже под
    * ним; не в городе — ничего не трогаем, говорим почему. Что сказал бот,
    * собирается в заметку, а ответ — вещи после действия. */
  private def inCityAct(hero: Hero)(act: (User, Renderer) => Task[Unit]): Task[InventoryActionView] =
    lock.withLock(hero.userId) {
      for {
        fresh <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        note  <- if (!MiniApp.inCity(fresh)) ZIO.succeed(content.text("miniapp.storages.away"))
                 else for {
                   user  <- users.getUserById(fresh.userId).someOrFail(new Throwable(s"Нет игрока ${fresh.userId.value}"))
                   lines <- Ref.make(Vector.empty[String])
                   _     <- act(user, new MiniApp.Collect(lines))
                   said  <- lines.get
                 } yield said.mkString("\n\n")
      } yield fresh -> note
    }.flatMap { case (fresh, note) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(inventory)
        .map(InventoryActionView(_, Option.when(note.nonEmpty)(note)))
    }

  /** Те же слова, что у Рахадима в боте, и куплено ли. */
  private def purchaseNote(result: AuctionPurchase.Result): (String, Boolean) = result match {
    case AuctionPurchase.Result.Gone         => content.text("bank.auction.lotGone") -> false
    case AuctionPurchase.Result.Own          => content.text("bank.auction.ownLot") -> false
    case AuctionPurchase.Result.NoMoney(lot) => content.format("bank.auction.noMoney", "price" -> lot.priceLine) -> false
    case AuctionPurchase.Result.NoRoom       => content.text("bank.auction.noRoom") -> false
    case AuctionPurchase.Result.Bought(lot)  =>
      content.format("bank.auction.bought", "name" -> lot.item.displayTitle, "price" -> lot.priceLine) -> true
  }

  /** Список и место героя — из одного снимка в памяти: база на открытие топа
    * не ходит. */
  override def top(hero: Hero, kind: TopKind): Task[TopView] =
    now.flatMap(cachedTop(kind, _)).map { board =>
      MiniAppView.top(hero, kind, board, content.text("miniapp.top.noName"))
    }

  /** Снимок из памяти, а устарел — заново из базы. Двое, попавшие на
    * устаревший разом, снимут его оба: это дешевле, чем держать замок. */
  private def cachedTop(kind: TopKind, ms: Long): Task[TopBoard] =
    topCache.get.map(_.get(kind).filter(c => ms - c.at < MiniApp.TopTtlMs)).flatMap {
      case Some(fresh) => ZIO.succeed(fresh.board)
      case None =>
        tops.board(kind, MiniApp.TopSize).tap(board => topCache.update(_ + (kind -> MiniApp.CachedTop(ms, board))))
    }

  /** Куб читается без записи, как и всё в просмотре: отдыхи благословения
    * бот доначислит сам. */
  override def craft(hero: Hero): Task[CraftView] =
    for {
      ms       <- now
      azat     <- AzatData.read(heroDao, hero.userId).map(_.withDailyRests(ms))
      home     <- azatCube.loadHome(hero.userId)
      inv      <- inventories.get(hero.id).mapError(e => new Throwable(e.toString))
      describe <- describer(hero)
    } yield MiniAppView.craft(hero, azat, home, inv, content, describe)

  /** Под локом игрока: героя перечитываем уже под ним — пока запрос ждал,
    * ход в чате мог его поменять. Куб не ожил или герой не в городе — ничего
    * не трогаем, говорим почему. */
  override def cube(hero: Hero, act: CubeAct): Task[CraftActionView] =
    lock.withLock(hero.userId) {
      for {
        fresh   <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        azat    <- azatCube.loadAzat(fresh.userId)
        outcome <- if (!azat.hasCube) ZIO.succeed(Outcome(Some(content.text("miniapp.craft.noCube")), Nil))
                   else if (!MiniApp.inCity(fresh)) ZIO.succeed(Outcome(Some(content.text("miniapp.craft.away")), Nil))
                   else cubeAct(fresh, azat.cubeItems, act)
      } yield fresh -> outcome
    }.flatMap { case (fresh, outcome) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(craft)
        .map(CraftActionView(_, outcome.note, outcome.made))
    }

  private def cubeAct(hero: Hero, inCube: List[Item], act: CubeAct): Task[Outcome] = {
    val gone = Outcome(Some(content.text("stack.gone")), Nil)
    act match {
      case CubeAct.Deposit(id) =>
        inventories.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
          inv.items.data.find(i => i.id == id && !i.isQuestItem) match {
            case None       => ZIO.succeed(gone)
            case Some(item) => azatCube.deposit(hero, item).map(Outcome(_, Nil))
          }
        }
      case CubeAct.Withdraw(id) =>
        inCube.find(_.id == id) match {
          case None       => ZIO.succeed(gone)
          case Some(item) => azatCube.withdraw(hero, item).map(Outcome(_, Nil))
        }
      case CubeAct.TakeAll =>
        azatCube.withdrawEverything(hero).map(note => Outcome(Some(note), Nil))
      case CubeAct.Activate =>
        for {
          user  <- users.getUserById(hero.userId).someOrFail(new Throwable(s"Нет игрока ${hero.userId.value}"))
          lines <- zio.Ref.make(Vector.empty[String])
          made  <- azatCube.activate(user, hero, new MiniApp.Collect(lines))
          said  <- lines.get
        } yield Outcome(Option.when(said.nonEmpty)(said.mkString("\n\n")), made.map(_.id))
    }
  }

  /** Описание вещи — как в карточке бота. Что сказать на карточке сюжетного
    * предмета (вскрыто ли письмо Марисы, сыт ли бродячий ларец), знает журнал
    * заданий. */
  private def describer(hero: Hero): Task[Item => String] =
    NpcQuestLog.load(heroDao, hero.userId).map { quests =>
      (item: Item) => InventoryState.cardText(content, item, hero, quests)
    }

  private val now = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
}
