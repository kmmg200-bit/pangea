package server

import io.circe.Json
import pangea.dao.vk.SeenMessageDao
import pangea.service.Background
import pangea.service.admin.AdminConfig
import pangea.service.donation.Donations
import pangea.service.state.StateHandler
import server.model.ServerConfig
import zio._

trait Server {
  def run(): UIO[Unit]

  /** Сообщения, которые бот сам забрал у ВК по Long Poll: в ход — тем же
    * путём, что и по Callback. Ходов не ждёт. */
  def longPoll(updates: List[Json]): UIO[Unit]
}

object Server {
  val live: ZLayer[ServerConfig with StateHandler with AdminConfig with Donations with Background with SeenMessageDao, Nothing, Server] =
    ZLayer {
      for {
        config       <- ZIO.service[ServerConfig]
        stateHandler <- ZIO.service[StateHandler]
        adminConfig  <- ZIO.service[AdminConfig]
        donations    <- ZIO.service[Donations]
        background   <- ZIO.service[Background]
        seen         <- ZIO.service[SeenMessageDao]
      } yield new ServerLive(config, stateHandler, adminConfig, donations, background, seen)
    }
}
