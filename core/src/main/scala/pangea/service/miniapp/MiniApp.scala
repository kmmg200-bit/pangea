package pangea.service.miniapp

import pangea.dao.artifact.ArtifactDao
import pangea.dao.bank.BankVaultDao
import pangea.dao.barrel.BarrelDao
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.dao.top.TopDao
import pangea.engine.{Journal, Players, Renderer, SceneContent, Screen}
import pangea.model.artifact.HeroArtifacts
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.home.HomeData
import pangea.model.hero.Hero
import pangea.model.item.{Chisel, Item, ItemType, MaterialKind}
import pangea.model.monster.Race
import pangea.model.quest.NpcQuests
import pangea.model.state.StateType
import pangea.model.top.{TopBoard, TopKind}
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.auction.AuctionRepository
import pangea.repository.bank.BankRepository
import pangea.repository.barrel.BarrelRepository
import pangea.repository.hero.HeroRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.repository.user.UserRepository
import pangea.service.auction.{AuctionPurchase, AuctionSale}
import pangea.service.cube.AzatCube
import pangea.service.equipment.{BeltWork, BrewWork, BundleWork, GemWork, Outfit}
import pangea.service.miniapp.MiniAppView.{HeroNamesView, TradeActionView, TradeView, AuctionActionView, AuctionSellView, AuctionView, CharacterView,
  CraftActionView, CraftView,
  InventoryActionView, InventoryView, QuestActionView, QuestsView, SquadActionView, SquadView, TopView}
import pangea.service.parcel.Transfers
import pangea.service.payout.Payouts
import pangea.service.squad.SquadOrders
import pangea.service.storage.{ItemStore, ItemTrail}
import pangea.service.storage.ItemStore.Place
import pangea.service.state.{AzatData, BarrelRobbery, HerbLore, NpcQuestLog, PlayerLock, QuestCard, QuestJournal, QuestsDone,
  SceneDoor, SquadDuty, StackMove}
import pangea.service.state.states.{EquipmentState, InventoryState}
import pangea.service.state.states.home.Racks
import pangea.service.trade.Trades
import zio.{Ref, Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** VK Mini App: персонаж, задания, отряд, инвентарь, крафт, топ и аукцион вне
  * чата. Просмотр безопасен из любого места, даже из боя: сцены, лок игрока и
  * `hero.state` не трогаются, а данные только читаются. Что меняет игру, идёт
  * под локом игрока — тем же, что и ходы в чате: куб Азата, покупка и
  * выставление на аукционе — только когда герой в городе, снаряжение и отряд —
  * когда он не в бою. */
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

  /** Обмен: открытые предложения, что при герое можно положить, правила. */
  def trade(hero: Hero): Task[TradeView]

  /** Имена героев — подсказки, кому предложить обмен; себя в списке нет. */
  def heroNames(hero: Hero): Task[HeroNamesView]

  /** Действие с обменом — под локом игрока, как ход в чате; правила — у
    * [[pangea.service.trade.Trades]]. */
  def tradeAct(hero: Hero, act: MiniApp.TradeAct): Task[TradeActionView]

  /** Купить лот — как в Торговом доме: вещь в сумку. Ответ — витрина после
    * покупки и что сказать. */
  def buy(hero: Hero, lotId: Long): Task[AuctionActionView]

  /** Выставить вещь `itemId` из сумки за `price` в монете `currency` — как у
    * Рахадима в боте, с тем же сбором и объявлением в торговой беседе. Ответ —
    * витрина после этого и что сказать. */
  def sell(hero: Hero, itemId: Long, currency: AuctionCurrency, price: Long): Task[AuctionSellView]

  /** Топ: первые [[MiniApp.TopSize]] и место самого героя. */
  def top(hero: Hero, kind: TopKind): Task[TopView]

  /** Надеть или снять — как в карточке слота в боте. Ответ — вещи после
    * этого и что сказать. */
  def gear(hero: Hero, act: MiniApp.GearAct): Task[InventoryActionView]

  /** Отвар из сумки — как с его карточки в боте; не в бою. */
  def brew(hero: Hero, itemId: Long, act: MiniApp.BrewAct): Task[InventoryActionView]

  /** Распаковать стопку `itemId` из сумки — как кнопкой на её карточке в
    * боте; не в бою. */
  def unpack(hero: Hero, itemId: Long): Task[InventoryActionView]

  /** Собрать в стопку пять вещей, как `itemId`, из сумки — как кнопкой
    * «В стопку» на её карточке в боте; не в бою. */
  def pack(hero: Hero, itemId: Long): Task[InventoryActionView]

  /** Расколоть камень `itemId` из сумки — как кнопкой «Расколоть камень» на
    * его карточке в боте; не в бою. */
  def crush(hero: Hero, itemId: Long): Task[InventoryActionView]

  /** Посыпать надетое оружие горстью пыли `itemId` из сумки — как кнопкой
    * «Применить на оружие» на её карточке в боте; не в бою. */
  def dust(hero: Hero, itemId: Long): Task[InventoryActionView]

  /** Прочитать трактат `itemId` из сумки — как кнопкой «Читать» на его
    * карточке в боте; не в бою. */
  def read(hero: Hero, itemId: Long): Task[InventoryActionView]

  /** Кнопка задания на карточке сюжетного предмета `itemId` из сумки — та же,
    * что в боте: `act` — её имя у бота или ответ на её вопрос; не в бою.
    * Задание пошло дальше в чате с ботом — так и сказано (`chat`). */
  def quest(hero: Hero, itemId: Long, act: String): Task[QuestActionView]

  /** Надеть комплект со стойки №`n` — обменяться с ней надетым, как кнопкой
    * стойки дома. Только в городе. */
  def rack(hero: Hero, n: Int): Task[InventoryActionView]

  /** Переложить вещь `itemId` из хранилища `from` в `to` (имена — как у
    * [[MiniAppView.StorageView]], сумка — `bag`); `all` — всю её стопку.
    * Только в городе. */
  def move(hero: Hero, from: String, to: String, itemId: Long, all: Boolean): Task[InventoryActionView]

  /** Выбросить вещь `itemId` из хранилища `from` (имена — как у `move`); `all`
    * — всю её стопку. Хранилище должно быть под рукой ([[MiniApp.dropBarred]]);
    * сюжетное не выбрасывают. */
  def drop(hero: Hero, from: String, itemId: Long, all: Boolean): Task[InventoryActionView]

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
    /** Сломать камень из гнезда `socket` надетой вещи. */
    final case class BreakGem(slot: Int, socket: Int) extends GearAct
    /** Вставить камень `itemId`, лежащий в `from` (сумка или Ларец), в надетую вещь. */
    final case class InsertGem(slot: Int, from: String, itemId: Long) extends GearAct
    /** Переписать резцом узор надетой вещи. */
    final case class UseChisel(slot: Int) extends GearAct
    /** Вещь из сумки — на пояс; `slot` — вместо какой ячейки, когда пояс полон. */
    final case class BeltPut(itemId: Long, slot: Option[Int]) extends GearAct
    /** Вынуть ячейку пояса в сумку целиком. */
    final case class BeltTake(slot: Int) extends GearAct
  }

  /** Что делают с обменом: предлагают герою по имени, отвечают своей стороной,
    * соглашаются с условиями, подтверждают (`Accept` — когда партнёр уже
    * согласился), отзывают или отклоняют. */
  sealed trait TradeAct
  object TradeAct {
    final case class Propose(partner: String, draft: Trades.Draft) extends TradeAct
    final case class Answer(id: Long, draft: Trades.Draft)         extends TradeAct
    final case class Agree(id: Long)                               extends TradeAct
    final case class Accept(id: Long)                              extends TradeAct
    final case class Cancel(id: Long)                              extends TradeAct

    /** Номера вещей из запроса: «11,12,15»; мусор отбрасывается. */
    def items(raw: Option[String]): List[Long] =
      raw.toList.flatMap(_.split(',').toList).flatMap(_.trim.toLongOption)
  }

  /** Что делают с отваром из сумки. На что тратить — травму, род, ячейку фляги —
    * игрок выбрал из того, что прислал бот ([[MiniAppView.BrewAskView]]). */
  sealed trait BrewAct
  object BrewAct {
    case object Drink                       extends BrewAct
    case object Coat                        extends BrewAct
    final case class Cure(trauma: String)   extends BrewAct
    final case class Sentence(race: String) extends BrewAct
    final case class Refill(slot: Int)      extends BrewAct

    /** Из запроса: `how` — это [[MiniAppView.BrewAskView.action]]. */
    def parse(how: String, arg: Option[String]): Option[BrewAct] = (how, arg) match {
      case ("drink", _)          => Some(Drink)
      case ("coat", _)           => Some(Coat)
      case ("cure", Some(t))     => Some(Cure(t))
      case ("sentence", Some(r)) => Some(Sentence(r))
      case ("refill", Some(n))   => n.toIntOption.map(Refill)
      case _                     => None
    }
  }

  /** Приказы отряду: переставить союзника (с места на место) или героя,
    * выгнать союзника, решить про восставшего — занять им место союзника или
    * отпустить. Резерв: отправить туда союзника из строя, выставить из резерва
    * в строй (мест нет — вместо союзника с места `swapAt`) или выгнать. В
    * резерве союзника адресуют номером в нём (`i`) и именем (`who`): список мог
    * сдвинуться, пока игрок смотрел. */
  sealed trait SquadAct
  object SquadAct {
    final case class Move(from: Int, to: Int) extends SquadAct
    final case class MoveHero(to: Int)        extends SquadAct
    final case class Dismiss(at: Int)         extends SquadAct
    final case class TakeRisen(at: Int)       extends SquadAct
    case object DismissRisen                  extends SquadAct
    final case class ToReserve(at: Int)       extends SquadAct
    final case class Deploy(i: Int, who: String, swapAt: Option[Int]) extends SquadAct
    final case class DismissReserve(i: Int, who: String)            extends SquadAct
  }

  /** Чем кончилось действие: что сказать и номера вещей, что получились. */
  private[miniapp] final case class Outcome(note: Option[String], made: List[Long])

  /** Строки, которые куб показал бы в чате (итог активации, достижение), —
    * в ответ приложению, а не в беседу: там свой экран со своими кнопками.
    *
    * Строка о новом достижении — ещё и в личку `dm` (чат игрока с ботом), как
    * в боте: приложение могли закрыть, не дочитав. Не ушла в личку — действие
    * от этого не падает: в ответе приложению строка всё равно есть. Без `dm`
    * (в тестах) — только в ответ. */
  private[miniapp] final class Collect(lines: zio.Ref[Vector[String]], dm: Option[Renderer] = None) extends Renderer {
    def show(user: User, screen: Screen): Task[Unit] = lines.update(_ :+ screen.text)

    override def achieved(user: User, screen: Screen): Task[Unit] =
      show(user, screen) *> ZIO.foreachDiscard(dm)(_.show(user, screen).catchAll(err =>
        ZIO.logWarning(s"miniapp: строка о достижении не ушла в личку игроку ${user.userId.value}: ${err.getMessage}")))
  }

  /** Герой в городе — только тогда из приложения можно работать с кубом,
    * выставлять на аукционе и трогать хранилища города и дома, как и передавать
    * вещи. Покупать на аукционе можно и вне города, если дома есть личный
    * посыльный — тогда платим серебром только из ячейки. */
  def inCity(hero: Hero): Boolean = StateType.cityStates.contains(hero.state)

  def canAuctionBuy(hero: Hero, hasCourier: Boolean): Boolean = inCity(hero) || hasCourier

  /** Сколько серебра показывать на витрине аукциона: в городе — руки и ячейка,
    * вне города — только ячейка (посыльный не носит карманное серебро). */
  def auctionSilver(hand: Long, vault: Long, hero: Hero): Long =
    if (inCity(hero)) hand + vault else vault

  /** Герой в бою — тогда снаряжение, комплект со стойки и строй из приложения
    * не меняют: ход боя считается на надетом и на том, кто где стоит. Любой бой
    * — в лабиринте, сюжетный — это [[StateType.Battle]]. */
  def inBattle(hero: Hero): Boolean = hero.state == StateType.Battle

  /** До каких хранилищ герой сейчас дотянется: в городе — до всех, вне города
    * — только до того, что носит с собой, в бою — ни до каких. */
  def reach(hero: Hero): Place => Boolean =
    if (inBattle(hero)) _ => false
    else if (inCity(hero)) _ => true
    else _.carried

  /** Почему дотянуться удастся не до всех хранилищ; `None` — до всех. */
  def storesAway(content: SceneContent, hero: Hero): Option[String] =
    if (inBattle(hero)) Some(content.text("miniapp.storages.inBattle"))
    else Option.unless(inCity(hero))(content.text("miniapp.storages.away"))

  /** Почему вещь из `from` в `to` сейчас не переложить: под рукой должны быть
    * оба хранилища. `None` — можно. */
  def moveBarred(content: SceneContent, hero: Hero, from: Place, to: Place): Option[String] =
    storesAway(content, hero).filterNot(_ => reach(hero)(from) && reach(hero)(to))

  /** Почему из `from` сейчас ничего не выбросить: хранилище должно быть под
    * рукой, как и для переноса ([[reach]]). `None` — можно. */
  def dropBarred(content: SceneContent, hero: Hero, from: Place): Option[String] =
    if (reach(hero)(from)) None
    else Some(content.text(if (inBattle(hero)) "miniapp.storages.dropInBattle" else "miniapp.storages.dropAway"))

  val live: ZLayer[MiniAppConfig with UserRepository with HeroRepository with InventoryRepository with AuctionRepository
                     with HeroDao with ArtifactDao with BarrelDao with BankVaultDao with TopDao with QuestOutcomeDao
                     with ItemRepository with BankRepository with ArtifactRepository with BarrelRepository
                     with Payouts with Players with PlayerLock with SceneDoor
                     with SceneContent with Trades with Transfers with Journal, Nothing, MiniApp] =
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
        outcomes    <- ZIO.service[QuestOutcomeDao]
        items       <- ZIO.service[ItemRepository]
        bank        <- ZIO.service[BankRepository]
        artifactRepo <- ZIO.service[ArtifactRepository]
        barrelRepo  <- ZIO.service[BarrelRepository]
        payouts     <- ZIO.service[Payouts]
        players     <- ZIO.service[Players]
        lock        <- ZIO.service[PlayerLock]
        door        <- ZIO.service[SceneDoor]
        trades      <- ZIO.service[Trades]
        transfers   <- ZIO.service[Transfers]
        content     <- ZIO.service[SceneContent]
        journal     <- ZIO.service[Journal]
        trail        = new ItemTrail(Some(journal), ItemTrail.App)
        topCache    <- Ref.make(Map.empty[TopKind, CachedTop])
      } yield new MiniAppLive(config, users, heroes, inventories, lots, heroDao, artifacts, barrels, vaults, content,
        tops, topCache, outcomes, new AzatCube(heroDao, inventories, items, content),
        new AuctionPurchase(heroDao, inventories, lots, users, bank, payouts, players, content),
        new AuctionSale(heroDao, inventories, lots, bank, players),
        new Outfit(heroDao, inventories, items, content), new GemWork(heroDao, inventories, items, content),
        new BrewWork(heroDao, inventories, content), new BeltWork(heroDao, inventories, items, content, trail),
        new BundleWork(inventories, items, content), new SquadOrders(heroDao, content),
        new ItemStore.All(heroDao, inventories, artifactRepo, barrelRepo, bank, content), trail,
        new BarrelRobbery(heroDao, barrelRepo, items, content), lock,
        new AppQuests(heroDao, inventories, new QuestCard(heroDao, inventories, items, content), content, door),
        trades, transfers, door.chat)
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
  outcomes:    QuestOutcomeDao,
  azatCube:    AzatCube,
  purchase:    AuctionPurchase,
  sale:        AuctionSale,
  outfit:      Outfit,
  gemWork:     GemWork,
  brewWork:    BrewWork,
  beltWork:    BeltWork,
  bundleWork:  BundleWork,
  orders:      SquadOrders,
  places:      ItemStore.All,
  // След вещей: перенос и «Выбросить» (см. ItemTrail).
  trail:       ItemTrail,
  robbery:     BarrelRobbery,
  lock:        PlayerLock,
  appQuests:   AppQuests,
  trades:      Trades,
  transfers:   Transfers,
  // Чат игрока с ботом: строка о достижении, полученном в приложении, — и туда.
  dm:          Renderer
) extends MiniApp {
  import MiniApp.{BrewAct, CubeAct, Denied, GearAct, Outcome, SquadAct, TradeAct}

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
      done    <- QuestsDone.load(heroDao, outcomes, content, hero)
    } yield MiniAppView.quests(hero, journal, content, done)

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
      say      = new MiniApp.Collect(lines, Some(dm))
      ms      <- now
      settled <- SquadDuty.settle(heroDao, content, user, hero, ms, say)
      _       <- act match {
                   case SquadAct.Move(from, to) => orders.move(user, settled, from, to, say)
                   case SquadAct.MoveHero(to)   => orders.moveHero(user, settled, to, say)
                   case SquadAct.Dismiss(at)    => orders.dismiss(user, settled, at, say)
                   case SquadAct.TakeRisen(at)  => orders.takeRisenPlace(user, settled, at, say)
                   case SquadAct.DismissRisen   => orders.dismissRisen(user, settled, say)
                   case SquadAct.ToReserve(at)  => orders.toReserve(user, settled, at, say)
                   case SquadAct.Deploy(i, who, swapAt) => orders.deploy(user, settled, i, who, swapAt, say)
                   case SquadAct.DismissReserve(i, who) => orders.dismissReserve(user, settled, i, who, say)
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
      quests   <- NpcQuestLog.load(heroDao, hero.userId)
      ms       <- now
    } yield MiniAppView.inventory(hero, inv, MiniAppView.Stores(arts, barrel, vault, home), content,
      describeWith(hero, quests), ms, quests)

  /** С витриной — чем платить и куда класть: в городе серебро на руках вместе с
    * ячейкой, вне города — только ячейка; свободные места в сумке и что из
    * сумки можно выставить. */
  override def auction(hero: Hero): Task[AuctionView] =
    for {
      ms       <- now
      page     <- lots.page(ms, 0, MiniApp.MaxLots).mapError(e => new Throwable(e.toString))
      (onSale, _, _) = page
      mine     <- lots.mine(hero.id, AuctionLot.MaxLots.toLong).mapError(e => new Throwable(e.toString))
      vaultAmt <- vaults.find(hero.id).map(_.fold(0L)(_.silver))
      home     <- heroDao.readHomeData(hero.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
      inv      <- inventories.get(hero.id).mapError(e => new Throwable(e.toString))
      describe <- describer(hero)
      silver    = MiniApp.auctionSilver(hero.silver, vaultAmt, hero)
    } yield MiniAppView.auction(hero, onSale, mine, ms, inv.items.data, describe,
      MiniAppView.Funds(silver, inv.freeSlots), content, home.hasCourier)

  /** Обмен: открытые предложения с именами партнёров, что при герое передаётся
    * (из этого кладут свою сторону) и правила. */
  override def trade(hero: Hero): Task[TradeView] =
    for {
      offers   <- trades.open(hero.userId)
      names    <- ZIO.foreach(offers)(o => trades.nameOf(o.partnerOf(hero.userId)).map(o.id -> _)).map(_.toMap)
      held     <- transfers.carried(hero)
      describe <- describer(hero)
      ms       <- now
    } yield MiniAppView.trade(hero, offers, names, held.filter(transfers.sendable), content, describe, trades.describe, ms)

  override def heroNames(hero: Hero): Task[HeroNamesView] =
    heroDao.namedHeroes.map(MiniAppView.heroNames(hero.userId, _))

  /** Под локом игрока, героя перечитываем уже под ним. Отказ сервиса — словами
    * в ответе; сделано — что сделано. */
  override def tradeAct(hero: Hero, act: TradeAct): Task[TradeActionView] =
    lock.withLock(hero.userId) {
      for {
        fresh <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        user  <- users.getUserById(fresh.userId).someOrFail(new Throwable(s"Нет игрока ${fresh.userId.value}"))
        ms    <- now
        said  <- act match {
                   case TradeAct.Propose(partner, draft) =>
                     trades.propose(user, fresh, partner, draft, ms).map(_.map(_ => content.text("trade.done.sent")))
                   case TradeAct.Answer(id, draft) =>
                     trades.answer(user, fresh, id, draft, ms).map(_.map(_ => content.text("trade.done.answered")))
                   case TradeAct.Agree(id) =>
                     trades.agree(user, fresh, id, ms).map(_.map(_ => content.text("trade.done.agreed")))
                   case TradeAct.Accept(id) =>
                     trades.confirm(user, fresh, id, ms).map(_.map(o => content.format("trade.done.accepted",
                       "got" -> trades.describe(o.sideOf(o.partnerOf(user.userId))))))
                   case TradeAct.Cancel(id) =>
                     trades.cancel(user, id, ms).map(_.map(trades.closedNote(user.userId, _)))
                 }
      } yield fresh -> said
    }.flatMap { case (fresh, said) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(trade)
        .map(TradeActionView(_, Some(said.merge), said.isRight))
    }

  /** Под локом покупателя, героя перечитываем уже под ним. Вне города — только
    * с личным посыльным и серебром из ячейки. */
  override def buy(hero: Hero, lotId: Long): Task[AuctionActionView] =
    lock.withLock(hero.userId) {
      for {
        fresh    <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        home     <- heroDao.readHomeData(fresh.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
        inCity    = MiniApp.inCity(fresh)
        canBuy    = MiniApp.canAuctionBuy(fresh, home.hasCourier)
        said     <- if (!canBuy) ZIO.succeed(content.text("miniapp.auction.away") -> false)
                    else users.getUserById(fresh.userId).someOrFail(new Throwable(s"Нет игрока ${fresh.userId.value}"))
                           .flatMap(purchase.buy(_, fresh, lotId,
                             vaultSilverOnly = !inCity, deliverToVault = !inCity)).map(purchaseNote(_, !inCity))
      } yield fresh -> said
    }.flatMap { case (fresh, (note, bought)) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(auction)
        .map(AuctionActionView(_, note, bought))
    }

  /** Под локом продавца, героя перечитываем уже под ним. Не в городе —
    * ничего не трогаем, говорим почему. Правила и слова — те же, что у
    * Рахадима в боте ([[AuctionSale]]). */
  override def sell(hero: Hero, itemId: Long, currency: AuctionCurrency, price: Long): Task[AuctionSellView] =
    lock.withLock(hero.userId) {
      for {
        fresh <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        said  <- if (!MiniApp.inCity(fresh)) ZIO.succeed(content.text("miniapp.auction.sellAway") -> false)
                 else users.getUserById(fresh.userId).someOrFail(new Throwable(s"Нет игрока ${fresh.userId.value}"))
                        .flatMap(sale.list(_, fresh, itemId, currency, price)).map(saleNote(currency, _))
      } yield fresh -> said
    }.flatMap { case (fresh, (note, listed)) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(auction)
        .map(AuctionSellView(_, note, listed))
    }

  override def gear(hero: Hero, act: GearAct): Task[InventoryActionView] =
    notInBattle(hero, content.text("miniapp.equipment.inBattle"))(gearAct(_, act))

  override def brew(hero: Hero, itemId: Long, act: BrewAct): Task[InventoryActionView] =
    notInBattle(hero, content.text("battle.belt.notInBattle"))(brewAct(_, itemId, act))

  /** Те же правила и слова, что у карточки стопки в боте ([[BundleWork]]).
    * Стопки уже нет — так и сказать. */
  override def unpack(hero: Hero, itemId: Long): Task[InventoryActionView] =
    notInBattle(hero, content.text("miniapp.storages.inBattle")) { fresh =>
      inventories.get(fresh.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
        inv.items.data.find(_.id == itemId) match {
          case None       => ZIO.succeed(content.text("stack.gone"))
          case Some(item) => bundleWork.unpack(fresh.id, inv, item).map(_.getOrElse(content.text("stack.gone")))
        }
      }
    }

  /** Те же правила и слова, что у «В стопку» на карточке вещи в боте
    * ([[BundleWork.pack]]). Вещи уже нет или она в стопку не собирается — так
    * и сказать. */
  override def pack(hero: Hero, itemId: Long): Task[InventoryActionView] =
    notInBattle(hero, content.text("miniapp.storages.inBattle")) { fresh =>
      inventories.get(fresh.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
        inv.items.data.find(_.id == itemId) match {
          case None       => ZIO.succeed(content.text("stack.gone"))
          case Some(item) => bundleWork.pack(fresh.id, inv, item).map(_.getOrElse(content.text("stack.gone")))
        }
      }
    }

  /** Те же правила и слова, что у карточки камня в боте ([[GemWork.crush]]).
    * Камня уже нет — так и сказать. */
  override def crush(hero: Hero, itemId: Long): Task[InventoryActionView] =
    bagAct(hero, itemId, content.text("socketing.gemGone"))(gemWork.crush)

  /** Те же правила и слова, что у карточки пыли в боте ([[GemWork.sprinkle]]).
    * Горсти уже нет — так и сказать. */
  override def dust(hero: Hero, itemId: Long): Task[InventoryActionView] =
    bagAct(hero, itemId, content.text("stack.gone"))(gemWork.sprinkle)

  /** Тот же бросок и те же слова, что у карточки трактата в боте
    * ([[HerbLore.readTreatise]]). Книги уже нет — так и сказать. */
  override def read(hero: Hero, itemId: Long): Task[InventoryActionView] =
    bagAct(hero, itemId, content.text("stack.gone"))(HerbLore.readTreatise(heroDao, inventories, content, _, _, _, _))

  /** Под локом игрока, героя перечитываем уже под ним. В бою — ничего не
    * трогаем: те же слова, что у сумки в бою. Ответ — вещи после кнопки и что
    * вышло ([[AppQuests]]). */
  override def quest(hero: Hero, itemId: Long, act: String): Task[QuestActionView] =
    lock.withLock(hero.userId) {
      for {
        fresh <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        user  <- users.getUserById(fresh.userId).someOrFail(new Throwable(s"Нет игрока ${fresh.userId.value}"))
        out   <- if (MiniApp.inBattle(fresh)) ZIO.succeed(AppQuests.Outcome.said(content.text("battle.belt.notInBattle")))
                 else appQuests.run(user, fresh, itemId, act)
      } yield fresh -> out
    }.flatMap { case (fresh, out) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(inventory)
        .map(QuestActionView(_, out.note, out.chat, out.ask))
    }

  /** Вещь `itemId` из сумки — в дело `act`, общее с её карточкой в боте; не в
    * бою. Вещи уже нет или дело не про неё (`act` ответил `false`) — сказать
    * `gone`. */
  private def bagAct(hero: Hero, itemId: Long, gone: String)(
      act: (User, Hero, Item, Renderer) => Task[Boolean]): Task[InventoryActionView] =
    notInBattle(hero, content.text("battle.belt.notInBattle")) { fresh =>
      for {
        user  <- users.getUserById(fresh.userId).someOrFail(new Throwable(s"Нет игрока ${fresh.userId.value}"))
        lines <- Ref.make(Vector.empty[String])
        say    = new MiniApp.Collect(lines, Some(dm))
        tell   = say.show(user, Screen(gone, Nil))
        inv   <- inventories.get(fresh.id).mapError(e => new Throwable(e.toString))
        _     <- inv.items.data.find(_.id == itemId) match {
                   case None       => tell
                   case Some(item) => act(user, fresh, item, say).flatMap(found => ZIO.unless(found)(tell).unit)
                 }
        said  <- lines.get
      } yield said.mkString("\n\n")
    }

  /** Под локом игрока, героя перечитываем уже под ним. В бою — ничего не
    * трогаем, говорим почему. */
  private def notInBattle(hero: Hero, refused: String)(act: Hero => Task[String]): Task[InventoryActionView] =
    lock.withLock(hero.userId) {
      for {
        fresh <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        note  <- if (MiniApp.inBattle(fresh)) ZIO.succeed(refused) else act(fresh)
      } yield fresh -> note
    }.flatMap { case (fresh, note) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(inventory)
        .map(InventoryActionView(_, Option.when(note.nonEmpty)(note)))
    }

  /** Те же правила и слова, что у карточки отвара в боте ([[BrewWork]]). Стало
    * нельзя, пока игрок смотрел, и причину никто не назвал — сказать это. */
  private def brewAct(hero: Hero, itemId: Long, act: BrewAct): Task[String] =
    for {
      user  <- users.getUserById(hero.userId).someOrFail(new Throwable(s"Нет игрока ${hero.userId.value}"))
      lines <- Ref.make(Vector.empty[String])
      say    = new MiniApp.Collect(lines, Some(dm))
      inv   <- inventories.get(hero.id).mapError(e => new Throwable(e.toString))
      _     <- inv.items.data.find(_.id == itemId) match {
                 case None => say.show(user, Screen(content.text("stack.gone"), Nil))
                 case Some(item) =>
                   (act match {
                     case BrewAct.Drink        => brewWork.drink(user, hero, item, say)
                     case BrewAct.Coat         => brewWork.coat(user, hero, item, say)
                     case BrewAct.Cure(t)      => brewWork.cure(user, hero, item, t, say)
                     case BrewAct.Refill(slot) => brewWork.refill(user, hero, item, slot, say)
                     case BrewAct.Sentence(r)  =>
                       Race.withNameOption(r).filter(Race.mortals.contains)
                         .fold(ZIO.succeed(false): Task[Boolean])(brewWork.sentence(user, hero, item, _, say))
                   }) *> lines.get.flatMap(said =>
                     ZIO.when(said.isEmpty)(say.show(user, Screen(content.text("miniapp.brew.stale"), Nil))).unit)
               }
      said  <- lines.get
    } yield said.mkString("\n\n")

  /** Те же проверки и слова, что у карточки слота в боте: в слот — только то,
    * что в него встаёт ([[EquipmentState.fitting]]). */
  private def gearAct(hero: Hero, act: GearAct): Task[String] =
    for {
      user  <- users.getUserById(hero.userId).someOrFail(new Throwable(s"Нет игрока ${hero.userId.value}"))
      lines <- Ref.make(Vector.empty[String])
      say    = new MiniApp.Collect(lines, Some(dm))
      gone   = say.show(user, Screen(content.text("equipment.wearGone"), Nil))
      empty  = say.show(user, Screen(content.text("equipment.slotEmpty"), Nil))
      wornAt = (idx: Int) => EquipmentState.slots.lift(idx).map(_.get(hero.equipment)).filter(_.itemType != ItemType.NoItem)
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
                     case None       => empty
                     case Some(slot) => outfit.takeOff(user, hero, slot, say).unit
                   }
                 case GearAct.BreakGem(idx, socket) =>
                   wornAt(idx) match {
                     case None => empty
                     case Some(item) =>
                       gemWork.breakWorn(user, hero, item, socket, say).flatMap(found =>
                         ZIO.unless(found)(say.show(user, Screen(content.text("stack.gone"), Nil))).unit)
                   }
                 case GearAct.InsertGem(idx, from, id) =>
                   (wornAt(idx), Place.byId(from).filter(Place.gemSources.contains)) match {
                     case (Some(target), Some(place)) => insertGem(user, hero, target, places(place), id, say)
                     case _                           => empty
                   }
                 case GearAct.UseChisel(idx) =>
                   inventories.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
                     (wornAt(idx).filter(Chisel.fits), inv.items.data.find(_.material.contains(MaterialKind.RuneChisel))) match {
                       case (Some(item), Some(chisel)) => gemWork.chisel(user, hero, inv, item, worn = true, chisel, say).unit
                       case _                          => say.show(user, Screen(content.text("inventory.chisel.gone"), Nil))
                     }
                   }
                 case GearAct.BeltPut(id, slot) =>
                   inventories.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap(_.items.data.find(_.id == id) match {
                     case None => say.show(user, Screen(content.text("stack.gone"), Nil))
                     case Some(item) =>
                       (if (slot.isEmpty) beltWork.put(user, hero, item, say)
                        else beltWork.swap(user, hero, item, slot, say)).flatMap {
                         case BeltWork.Put.Done       => ZIO.unit
                         case BeltWork.Put.NotForBelt => say.show(user, Screen(content.text("stack.gone"), Nil))
                         case BeltWork.Put.Full(_, _) => say.show(user, Screen(content.text("inventory.beltFull"), Nil))
                       }
                   })
                 case GearAct.BeltTake(idx) =>
                   beltWork.take(user, hero, idx, say).flatMap(found =>
                     ZIO.unless(found)(say.show(user, Screen(content.text("battle.belt.gone"), Nil))).unit)
               }
      said  <- lines.get
    } yield said.mkString("\n\n")

  /** Камень из `store` — в надетую вещь. Слова — те же, что на экране вставки
    * в боте; Огонь и Холод в оружии рассыпаются молча, как и там. */
  private def insertGem(user: User, hero: Hero, target: Item, store: ItemStore, id: Long, say: Renderer): Task[Unit] =
    store.items(hero).flatMap(items => items.find(_.id == id).flatMap(i => i.gem.map(i -> _)) match {
      case None => say.show(user, Screen(content.text("socketing.gemGone"), Nil))
      case Some((stone, gem)) =>
        gemWork.insert(user, hero, target.id, gem, store.remove(hero, stone).unit, say).flatMap(done =>
          ZIO.unless(done)(say.show(user, Screen(content.text("socketing.noTargets"), Nil))).unit)
    })

  /** Смена комплекта издалека — как ключом из сумки в боте: где угодно, кроме
    * боя, и слова те же. Что не так (не по уровню, сумка не вместит), скажет
    * [[Racks]]. */
  override def rack(hero: Hero, n: Int): Task[InventoryActionView] =
    storesAct(hero) { (user, fresh, say) =>
      if (MiniApp.inBattle(fresh)) say.show(user, Screen(content.text("miniapp.equipment.inBattle"), Nil))
      else racks.swap(user, n, say).flatMap {
        case Left(why) => say.show(user, Screen(why, Nil))
        case Right(_)  => say.show(user, Screen(content.format("home.rack.keyTurned", "n" -> n.toString), Nil))
      }
    }

  private val racks = Racks(heroDao, inventories, content)

  /** Перенос — общим [[ItemStore.move]], слова отказов — те же, что у экранов
    * бота. Стопка — по одной, пока принимает новое место ([[StackMove]]). Оба
    * хранилища должны быть герою под рукой ([[MiniApp.moveBarred]]) — жребий
    * бочки бросается только после этого. */
  override def move(hero: Hero, from: String, to: String, itemId: Long, all: Boolean): Task[InventoryActionView] =
    storesAct(hero) { (user, fresh, say) =>
      def tell(text: String) = say.show(user, Screen(text, Nil))
      val gone = content.text("stack.gone")
      (Place.byId(from), Place.byId(to)) match {
        case (Some(src), Some(dst)) if src != dst =>
          MiniApp.moveBarred(content, fresh, src, dst).fold(barrelGuard(user, fresh, src, dst))(ZIO.some(_)).flatMap {
            case Some(why) => tell(why)
            case None =>
              val (source, target) = (places(src), places(dst))
              val where            = content.text(s"miniapp.storages.${dst.id}.title")
              source.items(fresh).flatMap { items =>
                items.find(_.id == itemId) match {
                  case None => tell(gone)
                  case Some(item) if !target.accepts(item) =>
                    tell(content.format("miniapp.storages.wrongPlace", "name" -> item.displayTitle, "to" -> where))
                  case Some(item) if all =>
                    StackMove.run(StackMove.stack(items, item))(ItemStore.move(source, target, fresh, _, gone))
                      .tap(moved => trail.moved(user.userId, src.id, dst.id, moved.items))
                      .flatMap(moved => tell(StackMove.summary(content, moved, "miniapp.storages.movedAll", "to" -> where)))
                  case Some(item) =>
                    ItemStore.move(source, target, fresh, item, gone).flatMap(why =>
                      ZIO.when(why.isEmpty)(trail.moved(user.userId, src.id, dst.id, List(item))) *>
                        tell(why.getOrElse(content.format("miniapp.storages.moved", "name" -> item.displayTitle, "to" -> where))))
                }
              }
          }
        case _ => tell(gone)
      }
    }

  /** «Выбросить» — общим [[ItemStore.drop]]. Хранилище должно быть под рукой
    * ([[MiniApp.dropBarred]]); бочка — та же, что при переносе: вскрытая воры
    * ничего не отдаёт, а первое за сутки дело с ней — жребий. Сюжетное не
    * выбрасывают — словами бота. */
  override def drop(hero: Hero, from: String, itemId: Long, all: Boolean): Task[InventoryActionView] =
    storesAct(hero) { (user, fresh, say) =>
      def tell(text: String) = say.show(user, Screen(text, Nil))
      Place.byId(from) match {
        case None => tell(content.text("stack.gone"))
        case Some(src) =>
          MiniApp.dropBarred(content, fresh, src).fold(barrelGuard(user, fresh, src, src))(ZIO.some(_)).flatMap {
            case Some(why) => tell(why)
            case None =>
              ItemStore.drop(places(src), fresh, itemId, all).flatMap {
                case ItemStore.Dropped.Gone => tell(content.text("stack.gone"))
                case ItemStore.Dropped.Kept(item) =>
                  tell(content.text(if (item.isStoryGear) "inventory.storyNoDrop" else "miniapp.storages.questNoDrop"))
                case ItemStore.Dropped.Done(item, gone) =>
                  trail.dropped(user.userId, src.id, gone) *>
                    tell(if (all) content.format("miniapp.storages.droppedAll", "name" -> item.displayTitle, "n" -> gone.size.toString)
                         else content.format("miniapp.storages.dropped", "name" -> item.displayTitle))
              }
          }
      }
    }

  /** Бочку вскрыли воры — ни класть, ни брать. Первое за сутки дело с ней —
    * жребий, как при открытии бочки в боте ([[BarrelRobbery]]). */
  private def barrelGuard(user: User, hero: Hero, src: Place, dst: Place): Task[Option[String]] =
    if (src != Place.Barrel && dst != Place.Barrel) ZIO.none
    else barrels.find(hero.id).flatMap {
      case Some(b) if b.robbed => ZIO.some(content.text("miniapp.storages.barrel.robbed"))
      case _ => robbery.roll(user, hero).map(now => Option.when(now)(BarrelRobbery.text(content, hero, robbedNow = true)))
    }

  /** Действие с хранилищами: под локом игрока, героя перечитываем уже под
    * ним — где он сейчас, решает само действие. Что сказал бот, собирается в
    * заметку, а ответ — вещи после действия. */
  private def storesAct(hero: Hero)(act: (User, Hero, Renderer) => Task[Unit]): Task[InventoryActionView] =
    lock.withLock(hero.userId) {
      for {
        fresh <- heroes.getHero(hero.userId).someOrFail(new Throwable(s"Нет героя у игрока ${hero.userId.value}"))
        user  <- users.getUserById(fresh.userId).someOrFail(new Throwable(s"Нет игрока ${fresh.userId.value}"))
        lines <- Ref.make(Vector.empty[String])
        _     <- act(user, fresh, new MiniApp.Collect(lines, Some(dm)))
        said  <- lines.get
        note   = said.mkString("\n\n")
      } yield fresh -> note
    }.flatMap { case (fresh, note) =>
      heroes.getHero(fresh.userId).map(_.getOrElse(fresh)).flatMap(inventory)
        .map(InventoryActionView(_, Option.when(note.nonEmpty)(note)))
    }

  /** Те же слова, что у Рахадима в боте, и куплено ли. */
  private def purchaseNote(result: AuctionPurchase.Result, courier: Boolean): (String, Boolean) = result match {
    case AuctionPurchase.Result.Gone         => content.text("bank.auction.lotGone") -> false
    case AuctionPurchase.Result.Own          => content.text("bank.auction.ownLot") -> false
    case AuctionPurchase.Result.NoMoney(lot) => content.format("bank.auction.noMoney", "price" -> lot.priceLine) -> false
    case AuctionPurchase.Result.NoRoom       =>
      (if (courier) content.text("bank.auction.noVaultRoom") else content.text("bank.auction.noRoom")) -> false
    case AuctionPurchase.Result.Bought(lot)  =>
      val key = if (courier) "bank.auction.boughtVault" else "bank.auction.bought"
      content.format(key, "name" -> lot.title, "price" -> lot.priceLine) -> true
  }

  /** Те же слова, что у Рахадима в боте, и выставлено ли. */
  private def saleNote(c: AuctionCurrency, result: AuctionSale.Result): (String, Boolean) = result match {
    case AuctionSale.Result.Gone         => content.text("stack.gone") -> false
    case AuctionSale.Result.PriceTooLow  => content.format("bank.auction.priceTooLow", "min" -> AuctionLot.MinPrice.toString) -> false
    case AuctionSale.Result.PriceTooHigh => content.format("bank.auction.priceTooHigh", "max" -> AuctionLot.MaxPrice.toString) -> false
    case AuctionSale.Result.TooManyLots  => content.format("bank.auction.tooManyLots", "max" -> AuctionLot.MaxLots.toString) -> false
    case AuctionSale.Result.NoFee(fee)   => content.format("bank.auction.noFee", "fee" -> s"${c.emoji} $fee") -> false
    case AuctionSale.Result.Failed       => content.text("bank.auction.listFailed") -> false
    case AuctionSale.Result.Listed(lot, fee) =>
      content.format("bank.auction.listed", "id" -> lot.id.toString, "name" -> lot.title,
        "price" -> lot.priceLine, "fee" -> s"${c.emoji} $fee") -> true
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
        tops.board(kind, MiniApp.TopSize, ms).tap(board => topCache.update(_ + (kind -> MiniApp.CachedTop(ms, board))))
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
          made  <- azatCube.activate(user, hero, new MiniApp.Collect(lines, Some(dm)))
          said  <- lines.get
        } yield Outcome(Option.when(said.nonEmpty)(said.mkString("\n\n")), made.map(_.id))
    }
  }

  /** Описание вещи — как в карточке бота. Что сказать на карточке сюжетного
    * предмета (вскрыто ли письмо Марисы, сыт ли бродячий ларец), знает журнал
    * заданий. */
  private def describer(hero: Hero): Task[Item => String] =
    NpcQuestLog.load(heroDao, hero.userId).map(describeWith(hero, _))

  private def describeWith(hero: Hero, quests: NpcQuests): Item => String =
    (item: Item) => InventoryState.cardText(content, item, hero, quests)

  private val now = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
}
