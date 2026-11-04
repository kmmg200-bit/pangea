package pangea.dao.config

import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.{ZIO, ZLayer}

/** @param lockPoolSize сколько соединений у пула локов игроков (см.
  *                     `AdvisoryLocks`): столько ходов разом может идти под
  *                     локом, следующий ждёт соединения. */
case class PostgresConfig(
  jdbcUrl: String,
  username: String,
  password: String,
  lockPoolSize: Int
)

object PostgresConfig {
  private val loadConfig =
    ZIO.attempt(
      ConfigSource.default.at("postgres").loadOrThrow[PostgresConfig]
    )

  val live: ZLayer[Any, Throwable, PostgresConfig] =
    ZLayer.fromZIO(loadConfig)
}
