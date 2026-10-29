package server

import pangea.service.Background
import pangea.service.admin.AdminConfig
import pangea.service.donation.Donations
import pangea.service.state.StateHandler
import server.model.ServerConfig
import zio._

trait Server {
  def run(): UIO[Unit]
}

object Server {
  val live: ZLayer[ServerConfig with StateHandler with AdminConfig with Donations with Background, Nothing, Server] =
    ZLayer {
      for {
        config       <- ZIO.service[ServerConfig]
        stateHandler <- ZIO.service[StateHandler]
        adminConfig  <- ZIO.service[AdminConfig]
        donations    <- ZIO.service[Donations]
        background   <- ZIO.service[Background]
      } yield new ServerLive(config, stateHandler, adminConfig, donations, background)
    }
}
