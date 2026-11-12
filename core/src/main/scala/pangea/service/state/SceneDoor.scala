package pangea.service.state

import pangea.engine.Renderer
import pangea.model.state.StateType
import pangea.model.user.User
import zio.Task

/** Вход в сцену бота не ходом в чате, а со стороны — из приложения: экран
  * сцены уходит игроку в чат, и герой записывается в неё — тем же переходом,
  * что и после хода, со сквозными сценами (см. `StateHandler`). Лок игрока
  * держит тот, кто входит. */
trait SceneDoor {

  /** Чат игрока с ботом: что сказано перед сценой («Новое задание»), — туда же. */
  def chat: Renderer

  /** Войти из сцены `from`, где герой сейчас, в сцену `to`. */
  def enter(user: User, from: StateType, to: StateType): Task[Unit]
}
