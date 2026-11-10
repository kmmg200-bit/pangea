package pangea.service.state.states.story

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{NpcQuestLog, State, StoryFaceQuest, StoryLine, UserAction}
import zio.Task

/** «Лицо из прошлого», шаг 1: архив канцелярии ночью (см. StoryFaceQuest).
  * Сюда ведёт карточка ключа — «Открыть архив», из города. Книга отмеченных
  * в свежем переплёте: ярлычок «Переплётная Брея, у рыбного рынка» и заметки
  * мастера на полях — рукой того, кто пережил пролог (своя у Элорина, своя у
  * Элиаса, обе — если живы оба). Глава — на втором шаге: в переплётную. */
case class StoryArchiveState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map("ArchiveLeave" -> Target.Goto(StateType.GlobalMap)),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryArchive) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      quests    <- NpcQuestLog.load(heroDao, user.userId)
      survivors <- StoryLine.survivorsOf(heroDao, user.userId)
      _         <- if (!StoryFaceQuest.toArchive(quests))
                     renderer.show(user, Screen(content.text("story.storyFace.archive.gone"), List(leave)))
                   else NpcQuestLog.save(heroDao, user.userId, StoryFaceQuest.step(quests, 2)) *>
                     renderer.show(user, Screen(
                       List("story.storyFace.archive.text", s"story.storyFace.archive.hand${survivors.suffix}",
                         "story.storyFace.archive.end").map(k => content.text(k).trim).mkString("\n\n"), List(leave)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave = content.choice("ArchiveLeave", "story.storyFace.archive.leaveLabel")
}
