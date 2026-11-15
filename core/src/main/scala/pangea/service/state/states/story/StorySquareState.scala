package pangea.service.state.states.story

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.{Difficulty, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.StoryNewcomerQuest.Fate
import pangea.service.state.{NpcQuestDialog, NpcQuestLog, QuestOutcomeLog, QuestSupport, State, StoryLine,
  StoryNewcomerQuest, StoryToolQuest, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Новенький», шаги 1 и 2: главная площадь (см. StoryNewcomerQuest). Сюда ведёт
  * карточка описи урожая, из города. В фонтане сидит Мило — новый отмеченный, —
  * а рядом уже те, кто пришёл за ним раньше героя: у Тени — стража совета, у
  * остальных — ловцы Братства с сетями. «Драться» — сюжетный бой, без
  * «Сбежать», награда как за обычный бой; добыча вернёт сюда с отметкой победы
  * — глава на втором шаге: куда девать новенького. Выбор закрывает главу: опись
  * уходит, пропуск на просеку — в сумку, исход — в подсчёт «как и вы», глава 8
  * взята и висит до обновления. Ушёл или погиб — те, кто пришёл за Мило, никуда
  * не делись, опись поведёт снова. */
case class StorySquareState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  outcomes:      Option[QuestOutcomeDao] = None
) extends State {
  import StorySquareState._

  private val chapter = NpcQuestDialog(heroDao, content, NpcQuest.StoryNewcomer, "StoryNewcomer")

  private val branch = new Branch(
    routes = Map(
      "SquareFight"       -> Target.Run { (u, _, r) => fight(u, r) },
      "SquareBrotherhood" -> Target.Run { (u, _, r) => decide(u, r, Fate.Brotherhood) },
      "SquareCouncil"     -> Target.Run { (u, _, r) => decide(u, r, Fate.Council) },
      "SquareHedge"       -> Target.Run { (u, _, r) => decide(u, r, Fate.Hedge) },
      "SquareLeave"       -> Target.Goto(StateType.CityCenter)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StorySquare) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StorySquare)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      data   <- heroDao.readSceneData(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (StoryFight.won(data, WonKey) && StoryNewcomerQuest.toSquare(quests)) {
                  val next = StoryNewcomerQuest.step(quests, 2)
                  heroDao.writeSceneData(user.userId, Json.Null) *> NpcQuestLog.save(heroDao, user.userId, next) *>
                    renderer.show(user, fateScreen(next))
                }
                else if (StoryNewcomerQuest.toSquare(quests)) renderer.show(user, squareScreen(quests))
                else if (StoryNewcomerQuest.toFate(quests)) renderer.show(user, fateScreen(quests))
                else renderer.show(user, Screen(content.text("story.storyNewcomer.square.gone"), List(leave.copy(row = Some(0)))))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave: Choice =
    content.choice("SquareLeave", "story.storyNewcomer.square.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(1))

  /** Путь Рубежа I — ключ текстов: у кого какая площадь. */
  private def pathKey(q: NpcQuests): String = StoryToolQuest.choice(q).fold(StoryToolQuest.Path.Alone.key)(_.key)

  /** Площадь: новенький в фонтане и те, кто успел к нему раньше. */
  private def squareScreen(q: NpcQuests): Screen =
    Screen(
      List("story.storyNewcomer.square.text", s"story.storyNewcomer.square.${pathKey(q)}")
        .map(k => content.text(k).trim).mkString("\n\n") + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryNewcomerQuest.Level)),
      List(content.choice("SquareFight", "story.storyNewcomer.square.fightLabel").copy(row = Some(0)), leave.copy(row = Some(0))))

  /** После боя: Мило, слова своего пути и вопрос — куда его. */
  private def fateScreen(q: NpcQuests): Screen = {
    val wonPath = s"story.storyNewcomer.square.won${pathKey(q).capitalize}"
    Screen(
      List("story.storyNewcomer.square.won", wonPath, "story.storyNewcomer.square.question")
        .map(k => content.text(k).trim).mkString("\n\n"),
      List(
        content.choice("SquareBrotherhood", "story.storyNewcomer.square.brotherhoodLabel").copy(row = Some(0)),
        content.choice("SquareCouncil", "story.storyNewcomer.square.councilLabel").copy(row = Some(0)),
        content.choice("SquareHedge", "story.storyNewcomer.square.hedgeLabel").copy(row = Some(1)),
        leave.copy(row = Some(1))))
  }

  /** Бой: у Тени — сержант стражи совета и четверо, у остальных — старший ловец
    * и четверо с сетями. Добыча вернёт сюда с отметкой победы. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryNewcomerQuest.toSquare(quests)) enter(user, renderer).as(StateType.StorySquare)
      else {
        val guards = StoryNewcomerQuest.guards(quests)
        val leader = content.text(if (guards) "story.storyNewcomer.square.sergeantName" else "story.storyNewcomer.square.catcherName")
        val story  = if (guards) StoryNewcomerQuest.GuardStory else StoryNewcomerQuest.CatchersStory
        val start  = if (guards) "story.storyNewcomer.square.fightStartShadow" else "story.storyNewcomer.square.fightStart"
        for {
          hero <- getHero(user)
          _    <- StoryFight.start(heroDao, user, hero, StoryNewcomerQuest.foes(leader), Nil, story, StateType.StorySquare, WonKey)
          _    <- renderer.show(user, Screen(content.text(start), Nil))
        } yield StateType.Battle
      }
    }

  /** Куда девать новенького: глава закрыта выбором — опись уходит, пропуск на
    * просеку в сумку, серебро и опыт, исход — в подсчёт; глава 8 взята и висит. */
  private def decide(user: User, renderer: Renderer, fate: Fate): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryNewcomerQuest.toFate(quests)) enter(user, renderer)
      else for {
        now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        hero   <- getHero(user)
        _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StoryHarvest))
        _      <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StoryPass)
        silver  = StoryNewcomerQuest.silver
        _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
        done   <- chapter.complete(user, hero.copy(silver = hero.silver + silver),
                    q => q.update(NpcQuest.StoryNewcomer)(_.copy(counter = fate.n)).take(NpcQuest.StoryClearing, now))
        (_, expLine) = done
        stats  <- QuestOutcomeLog.recordAndCount(outcomes, content, hero, fate.outcome)
        after  <- NpcQuestLog.load(heroDao, user.userId)
        _      <- renderer.show(user, Screen((fateKeys(quests, fate) :+ passKey(quests))
                    .map(k => content.text(k).trim).mkString("\n\n"), Nil))
        _      <- renderer.show(user, Screen(List(
                    content.format(s"story.storyNewcomer.reward.${fate.key}", "silver" -> silver.toString, "exp" -> expLine),
                    QuestOutcomeLog.withStats(content.text("story.storyNewcomer.questDone"), stats),
                    StoryLine.takenLine(content, NpcQuest.StoryClearing, after)).mkString("\n\n"), List(leave.copy(row = Some(0)))))
      } yield ()
    }.as(StateType.StorySquare)

  /** Что стало с Мило — по выбору, а у Тени — ещё и как это видит Братство. */
  private def fateKeys(q: NpcQuests, fate: Fate): List[String] = {
    val shadow = StoryNewcomerQuest.guards(q)
    fate match {
      case Fate.Brotherhood =>
        List(if (shadow) "story.storyNewcomer.fate.brotherhood.shadow" else "story.storyNewcomer.fate.brotherhood.other")
      case Fate.Council =>
        "story.storyNewcomer.fate.council.main" :: Option.when(shadow)("story.storyNewcomer.fate.council.shadow").toList
      case Fate.Hedge => List("story.storyNewcomer.fate.hedge.main")
    }
  }

  /** Пропуск на просеку — у главаря тех, кто пришёл за Мило. */
  private def passKey(q: NpcQuests): String =
    if (StoryNewcomerQuest.guards(q)) "story.storyNewcomer.pass.guard" else "story.storyNewcomer.pass.catcher"

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StorySquareState {

  /** Отметка в `scene_data`: те, кто пришёл за новеньким, побиты. */
  val WonKey: String = "storySquareWon"
}
