package pangea.client

import fs2.io.net.Network
import org.http4s.client.{Client => HClient}
import org.http4s.ember.client.EmberClientBuilder
import zio.{Scope, Task, ZIO}
import zio.interop.catz._

import scala.concurrent.duration.DurationInt

object Client {
  // Явный Network[Task] вместо устаревшего неявного implicitForAsync (http4s 3.7.0).
  private implicit val network: Network[Task] = Network.forAsync[Task]

  /** HTTP-клиент на время жизни области — у слоя это всё время работы сервера.
    * Пул соединений живёт столько же, и соединение с ВК переиспользуется.
    * Раньше область закрывалась сразу после создания: клиент работал, но каждое
    * сообщение игроку открывало новое TCP- и TLS-соединение.
    *
    * Тайм-аут — на один запрос: ВК отвечает за доли секунды, а зависший запрос
    * без него держал бы ход — и лок игрока — слишком долго. */
  def scoped: ZIO[Scope, Throwable, HClient[Task]] =
    EmberClientBuilder
      .default[Task]
      .withTimeout(10.seconds)
      .build
      .toScopedZIO
}
