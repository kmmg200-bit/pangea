package pangea.service.state.states.story

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.QuestItemKind
import pangea.model.quest.NpcQuest
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{NpcQuestLog, QuestSupport, State, StoryLine, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Первая зацепка основной линии: чёрный плащ у изгороди (глава «Метка под
  * меткой», см. StoryMarkQuest). Встречу назначает линия — когда «Встать на
  * ноги» засчитано дома, на пятом исследовании уровня лабиринта
  * (StoryLine.firstClueDue). Сцена — сама находка: плащ ложится в сумку, глава
  * берётся, дальше её ведёт карточка плаща. Пришли, когда находить уже
  * нечего, — просто дальше в лабиринт. */
case class StoryCloakState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map("CloakOn" -> Target.Goto(StateType.Dungeon)),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryCloak) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      lore   <- heroDao.readLoreData(user.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))
      due     = StoryLine.clueReady(lore) && StoryLine.startable(quests, lore).exists(_.quest == NpcQuest.StoryMark)
      _      <- if (!due) renderer.show(user, Screen(content.text("story.storyMark.cloak.nothing"), List(onward)))
                else for {
                  now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
                  _     <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StoryCloak)
                  taken  = quests.take(NpcQuest.StoryMark, now)
                  _     <- NpcQuestLog.save(heroDao, user.userId, taken)
                  _     <- renderer.show(user, Screen(
                             content.text("story.storyMark.cloak.found") + "\n\n" +
                               StoryLine.takenLine(content, NpcQuest.StoryMark, taken), List(onward)))
                } yield ()
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def onward = content.choice("CloakOn", "story.storyMark.cloak.onwardLabel")

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
