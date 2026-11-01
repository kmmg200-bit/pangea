package pangea.service.miniapp

import pangea.model.auction.AuctionLot
import pangea.model.hero.Hero
import pangea.repository.auction.AuctionRepository
import pangea.repository.hero.HeroRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.user.UserRepository
import pangea.service.miniapp.MiniAppView.{AuctionView, InventoryView}
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

  val live: ZLayer[MiniAppConfig with UserRepository with HeroRepository with InventoryRepository with AuctionRepository,
                   Nothing, MiniApp] =
    ZLayer.fromFunction(new MiniAppLive(_, _, _, _, _))
}

final class MiniAppLive(
  config:      MiniAppConfig,
  users:       UserRepository,
  heroes:      HeroRepository,
  inventories: InventoryRepository,
  lots:        AuctionRepository
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
    inventories.get(hero.id).mapError(e => new Throwable(e.toString))
      .map(MiniAppView.inventory(hero, _))

  override def auction(hero: Hero): Task[AuctionView] =
    for {
      ms     <- now
      page   <- lots.page(ms, 0, MiniApp.MaxLots).mapError(e => new Throwable(e.toString))
      (onSale, _, _) = page
      mine   <- lots.mine(hero.id, AuctionLot.MaxLots.toLong).mapError(e => new Throwable(e.toString))
    } yield MiniAppView.auction(hero, onSale, mine, ms)

  private val now = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
}
