package pangea.test

import pangea.engine.{Renderer, Screen}
import pangea.model.user.User
import zio.{Ref, Task, ZIO}

class TestRenderer(screensRef: Ref[List[Screen]]) extends Renderer {
  def show(user: User, screen: Screen): Task[Unit] = screensRef.update(_ :+ screen)
  def sentScreens: Task[List[Screen]]              = screensRef.get

  /** Забыть показанное: удобно, когда в тесте важно только то, что пришло
    * после определённого шага. */
  def reset: Task[Unit] = screensRef.set(Nil)
}

object TestRenderer {
  def make: Task[TestRenderer] =
    Ref.make(List.empty[Screen]).map(new TestRenderer(_))

  /** ВК не принимает сообщение со словами `marker` и после повторов (как
    * ошибка 10 «Internal server error»): показ падает, ход обрывается на нём.
    * Прочие экраны проходят молча. */
  def rejecting(marker: String): Renderer = new Renderer {
    def show(user: User, screen: Screen): Task[Unit] =
      if (screen.text.contains(marker)) ZIO.fail(new Throwable("vk: Internal server error"))
      else ZIO.unit
  }
}
