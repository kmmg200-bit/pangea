package app

import pangea.dao.activity.ActivityDao
import pangea.dao.arena.ArenaDao
import pangea.dao.Transactor
import pangea.dao.config.PostgresConfig
import pangea.dao.admin.AdminDao
import pangea.dao.artifact.ArtifactDao
import pangea.dao.parcel.ParcelDao
import pangea.dao.auction.AuctionDao
import pangea.dao.payment.PaymentDao
import pangea.dao.payout.PayoutDao
import pangea.dao.referral.ReferralDao
import pangea.dao.promo.PromoDao
import pangea.dao.bank.BankVaultDao
import pangea.dao.barrel.BarrelDao
import pangea.dao.hero.HeroDao
import pangea.dao.inventory.InventoryDao
import pangea.dao.item.ItemDao
import pangea.dao.journal.JournalLive
import pangea.dao.schedule.ScheduledTaskDao
import pangea.dao.sendfailure.SendFailureDao
import pangea.dao.user.UserDao
import pangea.engine.{Players, SceneContent}
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.auction.AuctionRepository
import pangea.repository.bank.BankRepository
import pangea.repository.barrel.BarrelRepository
import pangea.repository.hero.HeroRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.repository.user.UserRepository
import pangea.client.tbank.{TBankClient, TBankConfig}
import pangea.service.activity.ActivityContest
import pangea.service.admin.{AdminConfig, AdminPanel}
import pangea.service.donation.Donations
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.referral.Referrals
import pangea.service.promo.PromoCodes
import pangea.service.schedule.{Scheduler, SchedulerPoller}
import pangea.service.sender.Api
import pangea.service.sender.vk.config.VkConfig
import pangea.service.sender.vk.VkPlayers
import pangea.service.state.{PlayerLock, StateHandler}
import pangea.service.state.states.StatesMap
import server.Server
import server.model.ServerConfig
import zio._
import zio.logging.backend.SLF4J

import java.util.concurrent.TimeUnit

object Main extends ZIOAppDefault {

  // Необработанная ошибка любого fiber'а (фоновые задачи, forkDaemon) — в лог
  // уровнем ERROR со всей причиной: по умолчанию ZIO пишет её в DEBUG, а его в
  // логе не видно.
  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Unit] =
    (Runtime.removeDefaultLoggers >>> SLF4J.slf4j) ++ Runtime.setUnhandledErrorLogLevel(LogLevel.Error)

  private type Env = Server with SchedulerPoller with Donations with ActivityContest with Transfers

  private val env =
    ZLayer.make[Env](
      VkConfig.live,
      PostgresConfig.live,
      Transactor.live,
      Api.vk,
      ZLayer.fromZIO(ZIO.service[Api].map(VkPlayers(_))).asInstanceOf[ZLayer[Api, Nothing, Players]],
      HeroDao.live,
      UserDao.live,
      InventoryDao.live,
      BarrelDao.live,
      BankVaultDao.live,
      ArenaDao.live,
      AuctionDao.live,
      ArtifactDao.live,
      PayoutDao.live,
      ActivityDao.live,
      ReferralDao.live,
      PromoDao.live,
      PaymentDao.live,
      ParcelDao.live,
      ItemDao.live,
      AdminDao.live,
      AdminConfig.live,
      AdminPanel.live,
      ScheduledTaskDao.live,
      SendFailureDao.live,
      ItemRepository.live,
      JournalLive.live,
      SceneContent.live,
      StatesMap.live,
      HeroRepository.live,
      UserRepository.live,
      InventoryRepository.live,
      BarrelRepository.live,
      BankRepository.live,
      AuctionRepository.live,
      ArtifactRepository.live,
      Payouts.live,
      ActivityContest.live,
      Referrals.live,
      PromoCodes.live,
      TBankConfig.live,
      TBankClient.live,
      Donations.live,
      Parcels.live,
      PlayerLock.live,
      Transfers.live,
      StateHandler.live,
      Scheduler.live,
      SchedulerPoller.live,
      ServerConfig.live,
      Server.live
    )

  /** Сверка платежей: добирает потерянные нотификации и застрявшие выдачи.
    * Нотификация обычно приходит за секунды, так что это страховка, а не
    * основной путь — раз в минуту более чем достаточно. */
  private val reconcileLoop =
    ZIO.serviceWithZIO[Donations] { donations =>
      (ZIO
        .clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        .flatMap(donations.reconcile)
        .catchAllCause(cause => ZIO.logErrorCause("Сверка платежей упала", cause)) *>
        ZIO.sleep(1.minute)).forever
    }

  /** Итоги недели конкурса активности: раз в минуту смотрим, не началась ли
    * новая неделя и подведена ли прошлая. Подведённая повторно ничего не
    * платит — минута тут лишь задержка до наград после полуночи понедельника. */
  private val contestLoop =
    ZIO.serviceWithZIO[ActivityContest] { contest =>
      (ZIO
        .clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        .flatMap(contest.settle)
        .catchAllCause(cause => ZIO.logErrorCause("Итоги конкурса активности упали", cause)) *>
        ZIO.sleep(1.minute)).forever
    }

  /** Почта: срок хранения вышел — всё, что лежит на почте, сгорает, и хозяину
    * уходит письмо; посылки, что не разложились сразу, раскладываются.
    * Хода хозяина не ждём; минута — лишь задержка после срока. */
  private val mailLoop =
    ZIO.serviceWithZIO[Transfers] { transfers =>
      (ZIO
        .clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        .flatMap(transfers.mailRound)
        .catchAllCause(cause => ZIO.logErrorCause("Обход почты упал", cause)) *>
        ZIO.sleep(1.minute)).forever
    }

  private val program =
    for {
      poller <- ZIO.service[SchedulerPoller]
      _      <- poller.start.forkDaemon
      _      <- reconcileLoop.forkDaemon
      _      <- contestLoop.forkDaemon
      _      <- mailLoop.forkDaemon
      _      <- ZIO.serviceWithZIO[Server](_.run())
    } yield ()

  // Сервер упал целиком (не поднялся слой, не открылся порт) — причина в лог,
  // а не только в консоль.
  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    program.provideLayer(env).tapErrorCause(cause => ZIO.logErrorCause("Сервер остановился с ошибкой", cause))
}
