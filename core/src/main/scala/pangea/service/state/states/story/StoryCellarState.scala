package pangea.service.state.states.story

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.quest.Difficulty
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{NpcQuestLog, State, StoryLine, StoryMorningQuest, UserAction}
import zio.{Task, ZIO}

/** «Наутро», путь Щита, шаг 3: ночь в подвале совета (см. StoryMorningQuest).
  * Сюда ведёт карточка грамоты совета, из города. Брей за решёткой, Горн с
  * бутылкой и двое его искателей; около полуночи приходят чистильщики
  * Братства — за Бреем. «Драться» — сюжетный бой, без «Сбежать», награда как за
  * обычный бой; Горн и искатели — в строю гостями. Добыча вернёт сюда с
  * отметкой победы: Брей заговорил — глава на четвёртом шаге, к Юноне. Ушёл
  * или погиб — чистильщики вернутся следующей ночью, грамота поведёт снова. */
case class StoryCellarState(heroDao: HeroDao, content: SceneContent) extends State {
  import StoryCellarState._

  private val branch = new Branch(
    routes = Map(
      "CellarFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "CellarLeave" -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryCellar) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryCellar)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      data   <- heroDao.readSceneData(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (StoryFight.won(data, WonKey) && StoryMorningQuest.toCellar(quests)) testimony(user, renderer)
                else if (StoryMorningQuest.toCellar(quests)) renderer.show(user, watchScreen)
                else renderer.show(user, Screen(content.text("story.storyMorning.cellar.gone"), List(leave)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave = content.choice("CellarLeave", "story.storyMorning.cellar.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(0))

  /** Ночь у решётки: бутылка, сыр, разговоры — и шаги наверху. */
  private def watchScreen: Screen =
    Screen(
      content.text("story.storyMorning.cellar.text").trim + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryMorningQuest.Level)),
      List(content.choice("CellarFight", "story.storyMorning.cellar.fightLabel").copy(row = Some(0)), leave))

  /** Чистильщики: главарь и его люди; Горн и двое искателей — в строю. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!StoryMorningQuest.toCellar(quests)) enter(user, renderer).as(StateType.StoryCellar)
                else for {
                  hero <- getHero(user)
                  _    <- StoryFight.start(heroDao, user, hero,
                            StoryMorningQuest.gang(content.text("story.storyMorning.cellar.leaderName")),
                            StoryMorningQuest.cellarGuests(content), StoryMorningQuest.CellarStory, StateType.StoryCellar, WonKey)
                  _    <- renderer.show(user, Screen(content.text("story.storyMorning.cellar.fightStart"), Nil))
                } yield StateType.Battle
    } yield res

  /** Отбились: Брей понял, что его пришли убить свои, и готов говорить. Глава
    * на четвёртом шаге — рассказать Юноне. */
  private def testimony(user: User, renderer: Renderer): Task[Unit] =
    for {
      _         <- heroDao.writeSceneData(user.userId, Json.Null)
      quests    <- NpcQuestLog.load(heroDao, user.userId)
      survivors <- StoryLine.survivorsOf(heroDao, user.userId)
      _         <- NpcQuestLog.save(heroDao, user.userId, StoryMorningQuest.step(quests, 4))
      _         <- renderer.show(user, Screen(
                     List("story.storyMorning.cellar.won", s"story.storyMorning.cellar.brey${StoryMorningQuest.breyVoice(survivors)}",
                       "story.storyMorning.cellar.end").map(k => content.text(k).trim).mkString("\n\n"), List(leave)))
    } yield ()

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryCellarState {

  /** Отметка в `scene_data`: чистильщики в подвале совета побиты. */
  val WonKey: String = "storyCellarWon"
}
