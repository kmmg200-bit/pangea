package server

import cats.syntax.semigroupk._
import org.http4s.HttpRoutes
import pangea.service.Background
import pangea.service.admin.AdminConfig
import pangea.service.donation.Donations
import pangea.service.miniapp.MiniApp
import zio._
import zio.interop.catz._

trait Server {
  def run(): UIO[Unit]
}

object Server {

  /** Маршруты бота: проверка жив ли, вебхук кассы, Mini App, выдача логов.
    * Колбэка ВК среди них нет — его принимает [[VkIngest]] (у сборщика или в
    * общем процессе). */
  def botRoutes(donations: Donations, background: Background, adminConfig: AdminConfig, miniApp: MiniApp): HttpRoutes[Task] =
    HealthRoutes.routes <+>
      new PaymentRoutes(donations, background).routes <+>
      new LogsRoutes(adminConfig).routes <+>
      new MiniAppRoutes(miniApp).routes
}
