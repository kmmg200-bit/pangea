package pangea.service.miniapp

import pangea.dao.artifact.ArtifactDao
import pangea.dao.bank.BankVaultDao
import pangea.dao.barrel.BarrelDao
import pangea.dao.hero.HeroDao
import pangea.dao.top.TopDao
import pangea.engine.SceneContent
import pangea.model.artifact.HeroArtifacts
import pangea.model.auction.AuctionLot
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.home.HomeData
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.model.top.{TopBoard, TopKind}
import pangea.repository.auction.AuctionRepository
import pangea.repository.hero.HeroRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.user.UserRepository
import pangea.service.miniapp.MiniAppView.{AuctionView, CharacterView, InventoryView, QuestsView, SquadView, TopView}
import pangea.service.state.{AzatData, HerbLore, NpcQuestLog, QuestJournal}
import pangea.service.state.states.InventoryState
import zio.{Ref, Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** VK Mini App: персонаж, задания, отряд, инвентарь, топ и аукцион вне чата. Пока только просмотр — он
  * безопасен из любого места, даже из боя: сцены, лок игрока и `hero.state`
  * не трогаются, а данные только читаются. */
trait MiniApp {

  /** Игрок по параметрам запуска приложения. Героя здесь не заводим: игру
    * начинают в сообщениях сообщества. */
  def player(launchParams: String): Task[Either[MiniApp.Denied, Hero]]

  def character(hero: Hero): Task[CharacterView]

  def quests(hero: Hero): Task[QuestsView]

  def squad(hero: Hero): Task[SquadView]

  def inventory(hero: Hero): Task[InventoryView]

  def auction(hero: Hero): Task[AuctionView]

  /** Топ: первые [[MiniApp.TopSize]] и место самого героя. */
  def top(hero: Hero, kind: TopKind): Task[TopView]
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

  val live: ZLayer[MiniAppConfig with UserRepository with HeroRepository with InventoryRepository with AuctionRepository
                     with HeroDao with ArtifactDao with BarrelDao with BankVaultDao with TopDao
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
        content     <- ZIO.service[SceneContent]
        topCache    <- Ref.make(Map.empty[TopKind, CachedTop])
      } yield new MiniAppLive(config, users, heroes, inventories, lots, heroDao, artifacts, barrels, vaults, content,
        tops, topCache)
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
  topCache:    Ref[Map[TopKind, MiniApp.CachedTop]]
) extends MiniApp {
  import MiniApp.Denied

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

  override def auction(hero: Hero): Task[AuctionView] =
    for {
      ms     <- now
      page   <- lots.page(ms, 0, MiniApp.MaxLots).mapError(e => new Throwable(e.toString))
      (onSale, _, _) = page
      mine   <- lots.mine(hero.id, AuctionLot.MaxLots.toLong).mapError(e => new Throwable(e.toString))
      describe <- describer(hero)
    } yield MiniAppView.auction(hero, onSale, mine, ms, describe)

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

  /** Описание вещи — как в карточке бота. Что сказать на карточке сюжетного
    * предмета (вскрыто ли письмо Марисы, сыт ли бродячий ларец), знает журнал
    * заданий. */
  private def describer(hero: Hero): Task[Item => String] =
    NpcQuestLog.load(heroDao, hero.userId).map { quests =>
      (item: Item) => InventoryState.cardText(content, item, hero, quests)
    }

  private val now = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
}
