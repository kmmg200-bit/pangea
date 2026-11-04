package server

import io.circe.Json
import pangea.dao.vk.VkInboxDao
import pangea.service.Background
import pangea.service.admin.AdminConfig
import pangea.service.donation.Donations
import pangea.service.miniapp.MiniApp
import server.model.ServerConfig
import zio._

trait Server {
  def run(): UIO[Unit]

  /** Сообщения, которые бот сам забрал у ВК по Long Poll: в очередь — тем же
    * путём, что и по Callback. Ходов не ждёт; не легли в очередь — ошибка, и
    * Long Poll запросит ту же пачку ещё раз. */
  def longPoll(updates: List[Json]): Task[Unit]
}

object Server {
  val live: ZLayer[ServerConfig with AdminConfig with Donations with Background with VkInboxDao with MiniApp,
                   Nothing, Server] =
    ZLayer {
      for {
        config      <- ZIO.service[ServerConfig]
        adminConfig <- ZIO.service[AdminConfig]
        donations   <- ZIO.service[Donations]
        background  <- ZIO.service[Background]
        inbox       <- ZIO.service[VkInboxDao]
        miniApp     <- ZIO.service[MiniApp]
      } yield new ServerLive(config, adminConfig, donations, background, inbox, miniApp)
    }
}
