package pangea.service.miniapp

import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.{ZIO, ZLayer}

/** @param vkSecret защищённый ключ VK Mini App (`VK_APP_SECRET`, настройки
  *                 приложения ВК): им ВК подписывает параметры запуска. Не
  *                 задан — API Mini App выключен: без ключа игрока не узнать. */
case class MiniAppConfig(vkSecret: Option[String] = None)

object MiniAppConfig {
  private val loadConfig =
    ZIO.attempt(ConfigSource.default.at("mini-app").loadOrThrow[MiniAppConfig])

  val live: ZLayer[Any, Throwable, MiniAppConfig] =
    ZLayer.fromZIO(loadConfig)
}
