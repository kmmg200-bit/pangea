package pangea.engine

import pangea.model.user.User
import zio.Task

trait Renderer {
  def show(user: User, screen: Screen): Task[Unit]

  /** Строка о новом достижении (см. QuestSupport.grant). Обычно — такой же
    * показ, как все; приложение, которое собирает строки себе в ответ, шлёт её
    * ещё и в личку (см. MiniApp.Collect). */
  def achieved(user: User, screen: Screen): Task[Unit] = show(user, screen)
}
