package app

import cats.syntax.semigroupk._
import org.http4s.HttpRoutes
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
import pangea.dao.vk.VkInboxDao
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
import pangea.service.{Background, Leader}
import pangea.dao.lock.AdvisoryLocks
import pangea.metrics.GameMetrics
import pangea.service.inbox.InboxWorker
import pangea.service.activity.ActivityContest
import pangea.service.admin.{AdminConfig, AdminPanel, NameBackfill}
import pangea.service.donation.Donations
import pangea.service.miniapp.{MiniApp, MiniAppConfig}
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.referral.Referrals
import pangea.service.promo.PromoCodes
import pangea.service.schedule.{Scheduler, SchedulerPoller, SchedulerPollerLive}
import pangea.service.sender.Api
import pangea.service.sender.vk.config.VkConfig
import pangea.service.sender.vk.{VkLongPoll, VkPlayers}
import pangea.service.state.{PlayerLock, StateHandler}
import pangea.service.state.states.StatesMap
import server.{MetricsServer, Server, ServerLive, VkInboxHandler, VkIngest}
import server.model.{Role, ServerConfig}
import zio._
import zio.interop.catz._
import zio.logging.backend.SLF4J
import zio.metrics.connectors.prometheus.PrometheusPublisher
import zio.metrics.connectors.{MetricsConfig, prometheus}
import zio.metrics.jvm.DefaultJvmMetrics

import java.util.concurrent.TimeUnit

object Main extends ZIOAppDefault {

  // Необработанная ошибка любого fiber'а (фоновые задачи, forkDaemon) — в лог
  // уровнем ERROR со всей причиной: по умолчанию ZIO пишет её в DEBUG, а его в
  // логе не видно.
  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Unit] =
    (Runtime.removeDefaultLoggers >>> SLF4J.slf4j) ++ Runtime.setUnhandledErrorLogLevel(LogLevel.Error)

  /** Бот (и общий процесс): игра целиком. */
  private type Env =
    SchedulerPoller with Donations with ActivityContest with Transfers with Background with PrometheusPublisher
      with VkConfig with VkLongPoll with VkInboxDao with InboxWorker with VkIngest with ServerConfig with AdminConfig
      with MiniApp with Leader

  /** Сборщик: только приём событий ВК в очередь — без игры. */
  private type CollectorEnv =
    VkIngest with VkConfig with VkLongPoll with ServerConfig with Background with PrometheusPublisher

  /** Метрики: реестр ZIO раз в 5 секунд переписывается в текст для Prometheus
    * (его отдаёт MetricsServer); плюс метрики JVM — память, GC, потоки, CPU.
    * Что именно пишет игра — см. GameMetrics. */
  private val metrics: ZLayer[Any, Throwable, PrometheusPublisher] =
    ZLayer.scoped {
      for {
        env <- ((ZLayer.succeed(MetricsConfig(5.seconds)) ++ prometheus.publisherLayer) >+> prometheus.prometheusLayer).build
        _   <- DefaultJvmMetrics.live.build
      } yield env.get[PrometheusPublisher]
    }

  private val env =
    ZLayer.make[Env](
      metrics,
      Background.live,
      VkConfig.live,
      PostgresConfig.live,
      Transactor.live,
      Api.vk,
      VkLongPoll.live,
      VkInboxDao.live,
      VkInboxHandler.live,
      InboxWorker.live,
      // Имя героя — из игры, а нет его — из ВК (см. VkPlayers).
      ZLayer.fromFunction((api: Api, heroDao: HeroDao) => VkPlayers(api, heroDao): Players),
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
      NameBackfill.live,
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
      MiniAppConfig.live,
      MiniApp.live,
      Parcels.live,
      AdvisoryLocks.live,
      PlayerLock.live,
      Leader.live,
      Transfers.live,
      StateHandler.live,
      Scheduler.live,
      SchedulerPoller.live,
      ServerConfig.live,
      VkIngest.live
    )

  private val collectorEnv =
    ZLayer.make[CollectorEnv](
      metrics,
      Background.live,
      VkConfig.live,
      PostgresConfig.live,
      Transactor.live,
      VkInboxDao.live,
      VkLongPoll.live,
      AdminConfig.live,
      ServerConfig.live,
      VkIngest.live
    )

  /** Сверка платежей: добирает потерянные нотификации и застрявшие выдачи.
    * Нотификация обычно приходит за секунды, так что это страховка, а не
    * основной путь — раз в минуту более чем достаточно. */
  private def reconcileLoop(background: Background) =
    ZIO.serviceWithZIO[Donations] { donations =>
      leaderRepeat(background, 1.minute, "Сверка платежей упала")(now.flatMap(donations.reconcile))
    }

  /** Оплата билетов за активность: раз в минуту смотрим, не началась ли
    * новая неделя и подведена ли прошлая. Подведённая повторно ничего не
    * платит — минута тут лишь задержка до наград после полуночи понедельника. */
  private def contestLoop(background: Background) =
    ZIO.serviceWithZIO[ActivityContest] { contest =>
      leaderRepeat(background, 1.minute, "Оплата билетов за активность упала")(now.flatMap(contest.settle))
    }

  /** Почта: срок хранения вышел — всё, что лежит на почте, сгорает, и хозяину
    * уходит письмо; посылки, что не разложились сразу, раскладываются.
    * Хода хозяина не ждём; минута — лишь задержка после срока. */
  private def mailLoop(background: Background) =
    ZIO.serviceWithZIO[Transfers] { transfers =>
      leaderRepeat(background, 1.minute, "Обход почты упал")(now.flatMap(transfers.mailRound))
    }

  /** Отложенные события: готовые задачи из очереди — каждые пять секунд. */
  private def pollerLoop(background: Background) =
    ZIO.serviceWithZIO[SchedulerPoller] { poller =>
      background.repeat(SchedulerPollerLive.PollInterval, "Scheduler poll failed")(poller.poll)
    }

  /** Long Poll — второй путь сообщений от ВК рядом с Callback (см. VkLongPoll):
    * включается `VK_LONG_POLL=true` и тумблером в настройках сообщества. */
  private def longPollLoop(background: Background) =
    ZIO.serviceWithZIO[VkConfig] { config =>
      ZIO.when(config.longPoll)(
        ZIO.serviceWithZIO[VkLongPoll](longPoll =>
          ZIO.serviceWithZIO[VkIngest](ingest => background.forkUntilShutdown(longPoll.run(ingest.longPoll)))))
    }

  /** Очередь входящих событий ВК (см. InboxWorker): разбор — до остановки;
    * при остановке новых событий не берём, начатые ходы дожидается фон. */
  private def inboxLoop(background: Background) =
    ZIO.serviceWithZIO[InboxWorker](worker => background.forkUntilShutdown(worker.run))

  /** Обработанные события ВК старше недели — забыть; и сколько ждёт в очереди —
    * в метрику. */
  private def inboxCleanupLoop(background: Background) =
    ZIO.serviceWithZIO[VkInboxDao] { inbox =>
      leaderRepeat(background, 1.hour, "Уборка очереди ВК упала")(
        now.flatMap(t => inbox.forget(t - InboxWorker.Keep.toMillis).unit)) *>
        background.repeat(5.seconds, "Очередь ВК: не посчитать ждущие")(
          inbox.pending.flatMap(n => GameMetrics.vkInboxPending.set(n.toDouble)))
    }

  private val now = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  /** Цикл, витки которого делает только ведущий бот (см. Leader): одиночные
    * дела — два бота разом сделали бы их дважды. Остальные боты пропускают
    * виток и пробуют на следующем — вдруг ведущим стали они. */
  private def leaderRepeat(background: Background, every: Duration, failed: String)(tick: Task[Unit]) =
    ZIO.serviceWithZIO[Leader](leader => background.repeat(every, failed)(leader.ifLeader(tick)))

  /** HTTP-сервер с этими маршрутами и сервер метрик для Prometheus. */
  private def serve(routes: HttpRoutes[Task]) =
    ZIO.serviceWithZIO[ServerConfig](config => (new ServerLive(config, routes): Server).run()) <&>
      ZIO.serviceWithZIO[PrometheusPublisher](MetricsServer.run)

  /** Маршруты бота: касса, Mini App, логи. */
  private val botRoutes =
    for {
      donations  <- ZIO.service[Donations]
      background <- ZIO.service[Background]
      admin      <- ZIO.service[AdminConfig]
      miniApp    <- ZIO.service[MiniApp]
    } yield Server.botRoutes(donations, background, admin, miniApp)

  /** Бот: разбор очереди ВК, таймеры, фоновые циклы, касса, Mini App, логи.
    * `vk` — ещё и приём событий ВК (колбэк и Long Poll): общий процесс, `ROLE=all`.
    *
    * Остановка (SIGTERM от `docker stop` → прерывание главного fiber'а): сервер
    * закрывает порт, затем фон дожидается начатых ходов и витков циклов — и лишь
    * потом закрываются слои: пул базы, клиенты ВК и банка. Раньше фон шёл через
    * `forkDaemon`, и пул закрывался прямо под ним. */
  private def botProgram(vk: Boolean) =
    ZIO.serviceWithZIO[Background] { background =>
      (pollerLoop(background) *>
        reconcileLoop(background) *>
        contestLoop(background) *>
        mailLoop(background) *>
        inboxLoop(background) *>
        inboxCleanupLoop(background) *>
        ZIO.when(vk)(longPollLoop(background)) *>
        botRoutes.flatMap(routes =>
          ZIO.serviceWithZIO[VkIngest](ingest => serve(if (vk) ingest.routes <+> routes else routes))))
        .ensuring(background.shutdown)
    }

  /** Сборщик: колбэк ВК и Long Poll — в очередь, больше ничего. */
  private val collectorProgram =
    ZIO.serviceWithZIO[Background] { background =>
      (longPollLoop(background) *> ZIO.serviceWithZIO[VkIngest](ingest => serve(ingest.routes)))
        .ensuring(background.shutdown)
    }

  // Сервер упал целиком (не поднялся слой, не открылся порт) — причина в лог,
  // а не только в консоль.
  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    Role.fromEnv.flatMap { role =>
      ZIO.logInfo(s"Роль процесса: ${role.name}") *> (role match {
        case Role.Collector => collectorProgram.provideLayer(collectorEnv)
        case Role.Bot       => botProgram(vk = false).provideLayer(env)
        case Role.All       => botProgram(vk = true).provideLayer(env)
      })
    }.tapErrorCause(cause => ZIO.logErrorCause("Сервер остановился с ошибкой", cause))
}
