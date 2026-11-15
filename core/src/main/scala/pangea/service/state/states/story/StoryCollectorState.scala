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
import pangea.service.state.StoryCollectorQuest.Deal
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.{NpcQuestDialog, NpcQuestLog, QuestOutcomeLog, QuestSupport, State, StoryCollectorQuest, StoryLine,
  UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Сборщик» (см. StoryCollectorQuest): одна сцена на два места главы. Пока
  * склад не куплен — ночь своего пути: у Тени Э. и люди Сборщика, у Щита
  * гильдейский зал и стража совета, у одиночки Барти с задатком и проверка
  * Сборщика. «Драться» — сюжетный бой, без «Сбежать», награда как за обычный;
  * добыча вернёт сюда с отметкой победы — глава на шаге покупки склада
  * (Торговый дом). Сюда ведёт карточка книги печи, из города.
  *
  * Склад куплен — дегтярный склад: Сойка (по главе 4) и глава Братства с
  * договором. Выбор закрывает главу: книга печи уходит, купчая остаётся
  * зацепкой Рубежа II; серебро и опыт, договор — в главу и в «как и вы»,
  * Рубеж II взят и висит. Сюда ведёт карточка купчей, из города. */
case class StoryCollectorState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  outcomes:      Option[QuestOutcomeDao] = None
) extends State {
  import StoryCollectorState._

  private val chapter = NpcQuestDialog(heroDao, content, NpcQuest.StoryCollector, "StoryCollector")

  private val branch = new Branch(
    routes = Map(
      "CollectorFight"  -> Target.Run { (u, _, r) => fight(u, r) },
      "CollectorPlace"  -> Target.Run { (u, _, r) => decide(u, r, Deal.Place) },
      "CollectorPeace"  -> Target.Run { (u, _, r) => decide(u, r, Deal.Peace) },
      "CollectorRefuse" -> Target.Run { (u, _, r) => decide(u, r, Deal.Refuse) },
      "CollectorLeave"  -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryCollector) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryCollector)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      data   <- heroDao.readSceneData(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (StoryFight.won(data, WonKey) && StoryCollectorQuest.toFight(quests)) {
                  val next = StoryCollectorQuest.step(quests, StoryCollectorQuest.buyStep(quests))
                  heroDao.writeSceneData(user.userId, Json.Null) *> NpcQuestLog.save(heroDao, user.userId, next) *>
                    renderer.show(user, Screen(content.text(s"story.storyCollector.night.won${key(quests)}").trim,
                      List(leave.copy(row = Some(0)))))
                }
                else if (StoryCollectorQuest.toFight(quests)) renderer.show(user, fightScreen(quests))
                else if (StoryCollectorQuest.toWarehouse(quests)) renderer.show(user, warehouseScreen(quests))
                else renderer.show(user, Screen(content.text("story.storyCollector.gone"), List(leave.copy(row = Some(0)))))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave: Choice =
    content.choice("CollectorLeave", "story.storyCollector.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(2))

  private def path(q: NpcQuests): Path = StoryCollectorQuest.path(q)

  /** Ключ пути с большой буквы — для `…Shadow`, `…Shield`, `…Alone`. */
  private def key(q: NpcQuests): String = path(q).key.capitalize

  private def difficulty: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryCollectorQuest.Level))

  /** Ночь своего пути — перед боем. */
  private def fightScreen(q: NpcQuests): Screen =
    Screen(content.text(s"story.storyCollector.night.arrive${key(q)}").trim + "\n\n" + difficulty,
      List(content.choice("CollectorFight", "story.storyCollector.fightLabel").copy(row = Some(0)), leave.copy(row = Some(0))))

  /** Дегтярный склад: Сойка — по главе 4, — глава Братства и его договор. */
  private def warehouseScreen(q: NpcQuests): Screen = {
    val sojka = if (StoryCollectorQuest.sojkaForgiven(q)) "story.storyCollector.warehouse.sojkaForgave"
                else "story.storyCollector.warehouse.sojkaBroke"
    val keys  = List("story.storyCollector.warehouse.arrive", sojka, "story.storyCollector.warehouse.oldMan",
                  s"story.storyCollector.warehouse.offer${key(q)}")
    Screen(keys.map(k => content.text(k).trim).mkString("\n\n"),
      List(
        content.choice("CollectorPlace", "story.storyCollector.warehouse.placeLabel").copy(row = Some(0)),
        content.choice("CollectorPeace", "story.storyCollector.warehouse.peaceLabel").copy(row = Some(0)),
        content.choice("CollectorRefuse", "story.storyCollector.warehouse.refuseLabel").copy(row = Some(1)),
        leave))
  }

  /** Бой своего пути: враги и гости — по пути. Добыча вернёт сюда с отметкой
    * победы. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryCollectorQuest.toFight(quests)) enter(user, renderer).as(StateType.StoryCollector)
      else for {
        hero <- getHero(user)
        _    <- StoryFight.start(heroDao, user, hero,
                  StoryCollectorQuest.foes(content.text(s"story.storyCollector.leader${key(quests)}")),
                  StoryCollectorQuest.guests(content, quests), StoryCollectorQuest.fightStory(quests),
                  StateType.StoryCollector, WonKey)
        _    <- renderer.show(user, Screen(content.text(s"story.storyCollector.night.fightStart${key(quests)}").trim, Nil))
      } yield StateType.Battle
    }

  /** Договор с главой Братства закрывает главу: книга печи уходит, купчая
    * остаётся; серебро и опыт, договор — в главу и в подсчёт «как и вы»,
    * Рубеж II взят и висит. */
  private def decide(user: User, renderer: Renderer, deal: Deal): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryCollectorQuest.toWarehouse(quests)) enter(user, renderer)
      else for {
        now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        hero   <- getHero(user)
        _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StoryLedger))
        silver  = StoryCollectorQuest.silver
        _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
        done   <- chapter.complete(user, hero.copy(silver = hero.silver + silver),
                    q => q.update(NpcQuest.StoryCollector)(_.copy(counter = deal.n)).take(NpcQuest.StoryFeed, now))
        (_, expLine) = done
        stats  <- QuestOutcomeLog.recordAndCount(outcomes, content, hero, deal.outcome)
        after  <- NpcQuestLog.load(heroDao, user.userId)
        extra   = Option.when(deal == Deal.Place && path(quests) != Path.Alone)(
                    content.text(s"story.storyCollector.warehouse.placeFor${key(quests)}").trim)
        _      <- renderer.show(user, Screen((content.text(s"story.storyCollector.warehouse.${deal.key}").trim :: extra.toList)
                    .mkString("\n\n"), Nil))
        _      <- renderer.show(user, Screen(List(
                    content.format(s"story.storyCollector.reward.${path(quests).key}", "silver" -> silver.toString, "exp" -> expLine),
                    QuestOutcomeLog.withStats(content.text("story.storyCollector.questDone"), stats),
                    StoryLine.takenLine(content, NpcQuest.StoryFeed, after)).mkString("\n\n"), List(leave.copy(row = Some(0)))))
      } yield ()
    }.as(StateType.StoryCollector)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryCollectorState {

  /** Отметка в `scene_data`: люди Сборщика (или стража совета) побиты. */
  val WonKey: String = "storyCollectorWon"
}
