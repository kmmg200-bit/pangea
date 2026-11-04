package pangea.service.miniapp

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.auction.AuctionLot
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.repository.auction.AuctionRepository
import pangea.repository.hero.HeroRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.user.UserRepository
import pangea.service.miniapp.MiniAppView.{AuctionView, InventoryView}
import pangea.service.state.NpcQuestLog
import pangea.service.state.states.InventoryState
import zio.{Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** VK Mini App: инвентарь и аукцион вне чата. Пока только просмотр — он
  * безопасен из любого места, даже из боя: сцены, лок игрока и `hero.state`
  * не трогаются, а данные только читаются. */
trait MiniApp {

  /** Игрок по параметрам запуска приложения. Героя здесь не заводим: игру
    * начинают в сообщениях сообщества. */
  def player(launchParams: String): Task[Either[MiniApp.Denied, Hero]]

  def inventory(hero: Hero): Task[InventoryView]

  def auction(hero: Hero): Task[AuctionView]
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

  val live: ZLayer[MiniAppConfig with UserRepository with HeroRepository with InventoryRepository with AuctionRepository
                     with HeroDao with SceneContent, Nothing, MiniApp] =
    ZLayer.fromFunction(new MiniAppLive(_, _, _, _, _, _, _))
}

final class MiniAppLive(
  config:      MiniAppConfig,
  users:       UserRepository,
  heroes:      HeroRepository,
  inventories: InventoryRepository,
  lots:        AuctionRepository,
  heroDao:     HeroDao,
  content:     SceneContent
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

  override def inventory(hero: Hero): Task[InventoryView] =
    for {
      inv      <- inventories.get(hero.id).mapError(e => new Throwable(e.toString))
      describe <- describer(hero)
    } yield MiniAppView.inventory(hero, inv, describe)

  override def auction(hero: Hero): Task[AuctionView] =
    for {
      ms     <- now
      page   <- lots.page(ms, 0, MiniApp.MaxLots).mapError(e => new Throwable(e.toString))
      (onSale, _, _) = page
      mine   <- lots.mine(hero.id, AuctionLot.MaxLots.toLong).mapError(e => new Throwable(e.toString))
      describe <- describer(hero)
    } yield MiniAppView.auction(hero, onSale, mine, ms, describe)

  /** Описание вещи — как в карточке бота. Что сказать на карточке сюжетного
    * предмета (вскрыто ли письмо Марисы, сыт ли бродячий ларец), знает журнал
    * заданий. */
  private def describer(hero: Hero): Task[Item => String] =
    NpcQuestLog.load(heroDao, hero.userId).map { quests =>
      (item: Item) => InventoryState.cardText(content, item, hero, quests)
    }

  private val now = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
}
