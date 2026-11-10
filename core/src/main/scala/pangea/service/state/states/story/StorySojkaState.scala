package pangea.service.state.states.story

import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.NpcQuest
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.{NpcQuestDialog, NpcQuestLog, QuestOutcomeLog, QuestSupport, State, StoryListQuest, UserAction}
import zio.{Task, ZIO}

/** «Список»: Сойка за дальним столом таверны (см. StoryListQuest). Сюда ведёт
  * карточка страницы списка, из города, — дважды. На втором шаге — рассказать
  * ей о канцелярии: она обещает показать того, кто носит оттуда бумаги, ночью
  * у Мельничного моста («Договорились» — глава на третьем шаге). На четвёртом
  * — после засады: Сойка не отпирается; простить или порвать с ней — выбор
  * закрывает главу: страница уходит, ключ от архива канцелярии (зацепка
  * следующей главы) — в сумку, награда, исход — в подсчёт «как и вы». */
case class StorySojkaState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  outcomes:      Option[QuestOutcomeDao] = None
) extends State {

  private val chapter = NpcQuestDialog(heroDao, content, NpcQuest.StoryList, "StoryList")

  private val branch = new Branch(
    routes = Map(
      "SojkaAgree"   -> Target.Run { (u, _, r) => agree(u, r) },
      "SojkaForgive" -> Target.Run { (u, _, r) => decide(u, r, SojkaFate.Forgave) },
      "SojkaBreak"   -> Target.Run { (u, _, r) => decide(u, r, SojkaFate.Broke) },
      "SojkaLeave"   -> Target.Goto(StateType.Tavern)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StorySojka) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (StoryListQuest.toSojka(quests))
        renderer.show(user, Screen(content.text("story.storyList.sojka.talk"), List(
          content.choice("SojkaAgree", "story.storyList.sojka.agreeLabel").copy(color = ChoiceColor.Positive))))
      else if (StoryListQuest.toConfront(quests))
        renderer.show(user, Screen(content.text("story.storyList.sojka.confront"), List(
          content.choice("SojkaForgive", "story.storyList.sojka.forgiveLabel").copy(color = ChoiceColor.Positive, row = Some(0)),
          content.choice("SojkaBreak", "story.storyList.sojka.breakLabel").copy(color = ChoiceColor.Negative, row = Some(0)))))
      else renderer.show(user, Screen(content.text("story.storyList.sojka.gone"), List(leave)))
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave = content.choice("SojkaLeave", "story.storyList.sojka.leaveLabel")

  /** Договорились о встрече у моста: глава на третьем шаге. */
  private def agree(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (!StoryListQuest.toSojka(quests)) enter(user, renderer)
                else NpcQuestLog.save(heroDao, user.userId, StoryListQuest.step(quests, 3)) *>
                  renderer.show(user, Screen(content.text("story.storyList.sojka.agreed"), List(leave)))
    } yield StateType.StorySojka

  /** Простить или порвать: глава закрыта выбором, страница уходит, ключ от
    * архива — в сумку, награда, исход — в подсчёт. */
  private def decide(user: User, renderer: Renderer, fate: SojkaFate): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      hero   <- getHero(user)
      _      <- if (!StoryListQuest.toConfront(quests)) enter(user, renderer)
                else for {
                  _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StoryPage))
                  _      <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StoryArchiveKey)
                  silver  = StoryListQuest.silver
                  _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
                  done   <- chapter.complete(user, hero.copy(silver = hero.silver + silver),
                              q => q.update(NpcQuest.StoryList)(_.copy(counter = fate.n)))
                  (_, expLine) = done
                  stats  <- QuestOutcomeLog.recordAndCount(outcomes, content, hero, fate.outcome)
                  _      <- renderer.show(user, Screen(
                              content.text(s"story.storyList.sojka.${fate.key}").trim + "\n\n" +
                                content.text("story.storyList.key").trim, Nil))
                  _      <- renderer.show(user, Screen(
                              content.format("story.storyList.reward", "silver" -> silver.toString, "exp" -> expLine) +
                                "\n\n" + QuestOutcomeLog.withStats(content.text("story.storyList.questDone"), stats), List(leave)))
                } yield ()
    } yield StateType.StorySojka

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
