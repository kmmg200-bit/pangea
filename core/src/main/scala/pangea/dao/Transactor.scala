package pangea.dao

import cats.implicits.catsSyntaxOptionId
import doobie.hikari.{Config, HikariTransactor}
import doobie.util.transactor.{Transactor => DTransactor}
import pangea.dao.config.PostgresConfig
import pangea.metrics.DbPoolMetrics
import zio.{Schedule, Task, ZIO, ZLayer, durationInt}
import zio.interop.catz._

object Transactor {
  val live: ZLayer[PostgresConfig, Throwable, DTransactor[Task]] =
    ZLayer.scoped {
      for {
        config <- ZIO.service[PostgresConfig]

        daoconfig = Config(
          jdbcUrl = config.jdbcUrl,
          username = config.username.some,
          password = config.password.some
        )

        // Пул — в метрики: ожидание и занятость соединений (см. DbPoolMetrics).
        pool = new DbPoolMetrics
        xa <- HikariTransactor.fromConfig[Task](daoconfig, metricsTrackerFactory = Some(pool)).toScopedZIO
        _  <- pool.sample.repeat(Schedule.spaced(5.seconds)).forkScoped
      } yield xa
    }
}
