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
import pangea.service.state.StoryFurnaceQuest.Fork
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.{NpcQuestDialog, NpcQuestLog, QuestOutcomeLog, QuestSupport, State, StoryFurnaceQuest, StoryLine,
  UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Печь»: Загон, старая солеварня (см. StoryFurnaceQuest). Сюда ведёт
  * карточка накладной, из города. Тень приезжает курьером с урожаем Братства —
  * легенда держится, и сразу развилка. Щит идёт на штурм с Горном и асессором
  * Штилем, одиночка — ночью через крышу, с капитанами каравана: «Драться» —
  * сюжетный бой, без «Сбежать», награда как за обычный; добыча вернёт сюда с
  * отметкой победы — глава на шаге развилки.
  *
  * У печи — что делают с Клеймом, пустые, новенький из главы 7 (если его отдали
  * Братству или совету), Смотритель-приказчик (если его отпустили в главе 2) и
  * книга печи. Развилка — разбить печь или оставить: накладная уходит, книга
  * печи — в сумку. У Тени и одиночки глава закрыта здесь: серебро и опыт,
  * исход — в подсчёт «как и вы», глава 10 взята и висит; Щит несёт книгу
  * Юноне (HomeState). Ушёл или погиб — Загон никуда не делся, накладная
  * поведёт снова. */
case class StoryFurnaceState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  outcomes:      Option[QuestOutcomeDao] = None
) extends State {
  import StoryFurnaceState._

  private val chapter = NpcQuestDialog(heroDao, content, NpcQuest.StoryFurnace, "StoryFurnace")

  private val branch = new Branch(
    routes = Map(
      "FurnaceFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "FurnaceBreak" -> Target.Run { (u, _, r) => decide(u, r, Fork.Broken) },
      "FurnaceKeep"  -> Target.Run { (u, _, r) => decide(u, r, Fork.Kept) },
      "FurnaceLeave" -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryFurnace) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryFurnace)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      data   <- heroDao.readSceneData(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (StoryFight.won(data, WonKey) && StoryFurnaceQuest.toFight(quests)) {
                  val next = StoryFurnaceQuest.step(quests, StoryFurnaceQuest.forkStep(quests))
                  heroDao.writeSceneData(user.userId, Json.Null) *> NpcQuestLog.save(heroDao, user.userId, next) *>
                    renderer.show(user, forkScreen(next, Some(s"story.storyFurnace.pen.won${key(quests)}")))
                }
                else if (StoryFurnaceQuest.toFight(quests)) renderer.show(user, fightScreen(quests))
                else if (StoryFurnaceQuest.toFork(quests)) renderer.show(user, forkScreen(quests, arrival(quests)))
                else renderer.show(user, Screen(content.text("story.storyFurnace.pen.gone"), List(leave.copy(row = Some(0)))))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave: Choice =
    content.choice("FurnaceLeave", "story.storyFurnace.pen.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(2))

  private def path(q: NpcQuests): Path = StoryFurnaceQuest.path(q)

  /** Ключ пути с большой буквы — для `…Shadow`, `…Shield`, `…Alone`. */
  private def key(q: NpcQuests): String = path(q).key.capitalize

  private def difficulty: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryFurnaceQuest.Level))

  /** Тень приезжает к печи сразу — её приезд и есть начало сцены. */
  private def arrival(q: NpcQuests): Option[String] =
    Option.when(path(q) == Path.Shadow)("story.storyFurnace.pen.arriveShadow")

  /** Перед боем: штурм (Щит) или ночь на крыше (одиночка). */
  private def fightScreen(q: NpcQuests): Screen =
    Screen(content.text(s"story.storyFurnace.pen.arrive${key(q)}").trim + "\n\n" + difficulty,
      List(content.choice("FurnaceFight", "story.storyFurnace.pen.fightLabel").copy(row = Some(0)), leave.copy(row = Some(0))))

  /** Печь: что в Загоне делают с Клеймом, кто сидит в клетях, кто служит при
    * печи и что в книге; дальше — что с печью делать. `lead` — с чего начать. */
  private def forkScreen(q: NpcQuests, lead: Option[String]): Screen = {
    val newcomer = if (StoryFurnaceQuest.newcomerHere(q)) "story.storyFurnace.pen.newcomerHere" else "story.storyFurnace.pen.newcomerGone"
    val warden   = Option.when(StoryFurnaceQuest.wardenHere(q))("story.storyFurnace.pen.warden")
    val keys     = lead.toList ++ List("story.storyFurnace.pen.inside", newcomer) ++ warden.toList ++
                     List("story.storyFurnace.pen.book", "story.storyFurnace.pen.question")
    // Сложность — Тени, у которой боя не было: у остальных она была перед боем.
    val tail = if (path(q) == Path.Shadow) "\n\n" + difficulty else ""
    Screen(keys.map(k => content.text(k).trim).mkString("\n\n") + tail,
      List(
        content.choice("FurnaceBreak", s"story.storyFurnace.fork.break${key(q)}").copy(row = Some(0)),
        content.choice("FurnaceKeep", s"story.storyFurnace.fork.keep${key(q)}").copy(row = Some(1)),
        leave))
  }

  /** Бой: у Щита — охрана Загона, у одиночки — ночная стража; гости по пути.
    * Добыча вернёт сюда с отметкой победы. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryFurnaceQuest.toFight(quests)) enter(user, renderer).as(StateType.StoryFurnace)
      else {
        val shield = path(quests) == Path.Shield
        val leader = content.text(if (shield) "story.storyFurnace.guardName" else "story.storyFurnace.watchName")
        val story  = if (shield) StoryFurnaceQuest.RaidStory else StoryFurnaceQuest.NightStory
        for {
          hero <- getHero(user)
          _    <- StoryFight.start(heroDao, user, hero, StoryFurnaceQuest.foes(leader), StoryFurnaceQuest.guests(content, quests),
                    story, StateType.StoryFurnace, WonKey)
          _    <- renderer.show(user, Screen(content.text(s"story.storyFurnace.pen.fightStart${key(quests)}"), Nil))
        } yield StateType.Battle
      }
    }

  /** Развилка у печи: накладная уходит, книга печи — в сумку, что с печью —
    * в главу. Тень и одиночка закрывают главу здесь: серебро и опыт, исход — в
    * подсчёт, глава 10 взята; Щит несёт книгу Юноне. */
  private def decide(user: User, renderer: Renderer, fork: Fork): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryFurnaceQuest.toFork(quests)) enter(user, renderer)
      else for {
        now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        hero   <- getHero(user)
        _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StoryWaybill))
        _      <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StoryLedger)
        marked  = quests.update(NpcQuest.StoryFurnace)(_.copy(counter = fork.n))
        result  = content.text(s"story.storyFurnace.fork.${fork.key}${key(quests)}").trim
        _      <- if (path(quests) == Path.Shield)
                    NpcQuestLog.save(heroDao, user.userId, StoryFurnaceQuest.step(marked, 4)) *>
                      renderer.show(user, Screen(result + "\n\n" + content.text("story.storyFurnace.fork.toJunona").trim,
                        List(leave.copy(row = Some(0)))))
                  else for {
                    _      <- NpcQuestLog.save(heroDao, user.userId, marked)
                    silver  = StoryFurnaceQuest.silver
                    _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
                    done   <- chapter.complete(user, hero.copy(silver = hero.silver + silver), _.take(NpcQuest.StoryCollector, now))
                    (_, expLine) = done
                    stats  <- QuestOutcomeLog.recordAndCount(outcomes, content, hero, fork.outcome)
                    after  <- NpcQuestLog.load(heroDao, user.userId)
                    _      <- renderer.show(user, Screen(result, Nil))
                    _      <- renderer.show(user, Screen(List(
                                content.format(s"story.storyFurnace.reward.${path(quests).key}", "silver" -> silver.toString,
                                  "exp" -> expLine),
                                QuestOutcomeLog.withStats(content.text("story.storyFurnace.questDone"), stats),
                                StoryLine.takenLine(content, NpcQuest.StoryCollector, after)).mkString("\n\n"),
                                List(leave.copy(row = Some(0)))))
                  } yield ()
      } yield ()
    }.as(StateType.StoryFurnace)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryFurnaceState {

  /** Отметка в `scene_data`: охрана Загона (или ночная стража) побита. */
  val WonKey: String = "storyFurnaceWon"
}
