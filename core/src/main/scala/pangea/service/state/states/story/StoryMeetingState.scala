package pangea.service.state.states.story

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.NpcQuest
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{NpcQuestDialog, NpcQuestLog, QuestSupport, State, StoryBrandQuest, UserAction}
import zio.{Task, ZIO}

/** «Что горит на груди», шаг 4: встреча с Сойкой — вторым отмеченным Кинэта —
  * за дальним столом таверны (см. StoryBrandQuest). Сюда ведёт карточка
  * наброска узора — «Идти на встречу», из города. Сойка говорит о Клейме и о
  * Братстве и кладёт на стол страницу из списка отмеченных — с новым именем
  * героя; «Взять страницу» закрывает главу: набросок уходит, страница —
  * зацепка следующей главы — в сумку. */
case class StoryMeetingState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val chapter = NpcQuestDialog(heroDao, content, NpcQuest.StoryBrand, "StoryBrand")

  private val branch = new Branch(
    routes = Map(
      "MeetTake"  -> Target.Run { (u, _, r) => take(u, r) },
      "MeetLeave" -> Target.Goto(StateType.Tavern)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryMeeting) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (StoryBrandQuest.toMeeting(quests))
        renderer.show(user, Screen(content.text("story.storyBrand.meeting.text"), List(
          content.choice("MeetTake", "story.storyBrand.meeting.takeLabel").copy(color = ChoiceColor.Positive))))
      else renderer.show(user, Screen(content.text("story.storyBrand.meeting.gone"), List(leave)))
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave = content.choice("MeetLeave", "story.storyBrand.meeting.leaveLabel")

  /** Страница у героя: набросок уходит, глава закрыта опытом. На странице —
    * его новое имя. */
  private def take(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      hero   <- getHero(user)
      _      <- if (!StoryBrandQuest.toMeeting(quests)) enter(user, renderer)
                else for {
                  _    <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StorySketch))
                  _    <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StoryPage)
                  done <- chapter.complete(user, hero, identity)
                  (_, expLine) = done
                  _    <- renderer.show(user, Screen(
                            content.format("story.storyBrand.meeting.page", "name" -> hero.name.getOrElse("")), Nil))
                  _    <- renderer.show(user, Screen(expLine + "\n\n" + content.text("story.storyBrand.questDone"), List(leave)))
                } yield ()
    } yield StateType.StoryMeeting

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
