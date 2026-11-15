package pangea.service.state.states.story

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.quest.Difficulty
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.purse.Purse
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.{NpcQuestLog, State, StoryMorningQuest, UserAction}
import zio.{Task, ZIO}

/** «Наутро», путь Тени, шаг 2: «Три кости» — игорный дом у порта (см.
  * StoryMorningQuest). Сюда ведёт карточка поручения Э., из города. Писарь
  * Горна сидит в задней комнате в счёт долга. Долг можно заплатить — серебром
  * с рук, а нехватку кошель доберёт из ячейки Торгового дома, — или драться
  * с хозяином и вышибалами: сюжетный бой, без «Сбежать», награда как за
  * обычный бой; добыча вернёт сюда с отметкой победы. Писарь свободен —
  * лодка, опись урожая, глава закрыта. Ушёл или погиб — писарь ждёт, поручение
  * поведёт снова. */
case class StoryDenState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  bank:          Option[BankRepository] = None
) extends State {
  import StoryDenState._

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "DenPay"   -> Target.Run { (u, _, r) => pay(u, r) },
      "DenFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "DenLeave" -> Target.Goto(StateType.HarborQuarter)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryDen) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryDen)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      data   <- heroDao.readSceneData(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (StoryFight.won(data, WonKey) && StoryMorningQuest.toDen(quests)) freed(user, "story.storyMorning.den.won", renderer)
                else if (StoryMorningQuest.toDen(quests)) renderer.show(user, denScreen)
                else renderer.show(user, Screen(content.text("story.storyMorning.den.gone"), List(leave)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave = content.choice("DenLeave", "story.storyMorning.den.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(1))

  private def owner: String = content.text("story.storyMorning.den.ownerName")

  /** Задняя комната: писарь в залоге, долг, вышибалы у двери — заплатить,
    * драться или уйти. */
  private def denScreen: Screen =
    Screen(
      content.format("story.storyMorning.den.text", "owner" -> owner, "debt" -> StoryMorningQuest.debt.toString).trim +
        "\n\n" + content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryMorningQuest.Level)),
      List(
        content.choice("DenPay", "story.storyMorning.den.payLabel", "debt" -> StoryMorningQuest.debt.toString)
          .copy(color = ChoiceColor.Positive, row = Some(0)),
        content.choice("DenFight", "story.storyMorning.den.fightLabel").copy(row = Some(0)),
        leave))

  /** Заплатить долг писаря: хватает — писарь свободен, нет — так и сказать. */
  private def pay(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      hero   <- getHero(user)
      _      <- if (!StoryMorningQuest.toDen(quests)) enter(user, renderer)
                else purse.charge(user.userId, hero, StoryMorningQuest.debt).flatMap {
                  case Some(_) => freed(user, "story.storyMorning.den.paid", renderer)
                  case None    => renderer.show(user, denScreen.copy(text = content.format("story.storyMorning.den.notEnough",
                                    "debt" -> StoryMorningQuest.debt.toString) + "\n\n" + denScreen.text))
                }
    } yield StateType.StoryDen

  /** Бой с хозяином и вышибалами: добыча вернёт сюда с отметкой победы. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!StoryMorningQuest.toDen(quests)) enter(user, renderer).as(StateType.StoryDen)
                else for {
                  hero <- getHero(user)
                  _    <- StoryFight.start(heroDao, user, hero, StoryMorningQuest.denFoes(owner), Nil,
                            StoryMorningQuest.DenStory, StateType.StoryDen, WonKey)
                  _    <- renderer.show(user, Screen(content.text("story.storyMorning.den.fightStart"), Nil))
                } yield StateType.Battle
    } yield res

  /** Писарь свободен — откупом или боем: лодка, опись урожая, глава закрыта. */
  private def freed(user: User, leadKey: String, renderer: Renderer): Task[Unit] =
    for {
      _    <- heroDao.writeSceneData(user.userId, Json.Null)
      hero <- getHero(user)
      _    <- StoryMorningQuest.finish(heroDao, inventoryRepo, itemRepo, content, user, hero, Path.Shadow,
                List(leadKey, "story.storyMorning.den.out").map(k => content.text(k).trim).mkString("\n\n"),
                List(leave.copy(row = Some(0))), renderer)
    } yield ()

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryDenState {

  /** Отметка в `scene_data`: хозяин «Трёх костей» и вышибалы побиты. */
  val WonKey: String = "storyDenWon"
}
