package pangea.service.state.states.arena

import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

/** Арена закрыта: бои между игроками перестраиваются заново. Пока — заглушка:
  * что закрыто, и назад в Центр города. Сюда же попадает и тот, кто стоял на
  * арене до закрытия. */
case class ArenaState(content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map("LeaveArena" -> Target.Goto(StateType.CityCenter)),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.Arena) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.Arena

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.text("arena.closed"), List(
      content.choice("LeaveArena", "arena.leave").copy(color = ChoiceColor.Negative, row = Some(0)))))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
