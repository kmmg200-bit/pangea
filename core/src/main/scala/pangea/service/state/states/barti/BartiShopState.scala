package pangea.service.state.states.barti

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.{Difficulty, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.BartiRoofQuest.Deal
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.story.StoryFight
import pangea.service.state.{BartiRoofQuest, NpcQuestLog, QuestSupport, State, StoryToolQuest, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Лавка Барти на Торговой площади — «Крыша» (см. BartiRoofQuest). Сюда ведёт
  * кнопка с площади, пока задание доступно или идёт, и карточка квитанции —
  * из города. Впервые — Барти показывает лавку и квитанцию ночной стражи:
  * задание взято на первом шаге своего пути, квитанция — в сумку. Щиту — к
  * Юноне (в лавке ждать нечего). Тени Братство предлагает решить вопрос само:
  * согласился — задание закрыто без боя, а Барти теперь должен Братству;
  * отказался — ночью в лавку. Ночью приходит капитан ночной стражи с четырьмя:
  * «Драться» — сюжетный бой, без «Сбежать», награда как за обычный бой, Барти
  * прячется под прилавком; добыча вернёт сюда с отметкой победы — задание
  * закрыто. Ушёл или погиб — стража придёт следующей ночью. «Уйти» — на
  * Торговую площадь. */
case class BartiShopState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {
  import BartiShopState._

  private val branch = new Branch(
    routes = Map(
      "RoofBrotherhood" -> Target.Run { (u, _, r) => letBrotherhood(u, r) },
      "RoofSelf"        -> Target.Run { (u, _, r) => bySelf(u, r) },
      "RoofFight"       -> Target.Run { (u, _, r) => fight(u, r) },
      "RoofLeave"       -> Target.Goto(StateType.MarketSquare)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.BartiShop) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.BartiShop)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      data   <- heroDao.readSceneData(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (StoryFight.won(data, WonKey) && BartiRoofQuest.toNight(quests)) won(user, quests, renderer)
                else if (BartiRoofQuest.available(quests)) begin(user, quests, renderer)
                else renderer.show(user, stepScreen(quests, None))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave: Choice =
    content.choice("RoofLeave", "barti.roof.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(1))

  private def difficulty: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(BartiRoofQuest.Level))

  /** Впервые: лавка, квитанция стражи — в сумку, задание взято на первом шаге
    * своего пути; дальше — экран этого шага. */
  private def begin(user: User, quests: NpcQuests, renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero  <- getHero(user)
      _     <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.BartiReceipt)
      taken  = BartiRoofQuest.take(quests, now)
      _     <- NpcQuestLog.save(heroDao, user.userId, taken)
      line   = content.format("barti.taken", "title" -> content.text("journal.quests.bartiRoof.title"),
                 "step" -> content.list("journal.quests.bartiRoof.steps").lift(taken.of(NpcQuest.BartiRoof).step - 1)
                   .getOrElse(""))
      // Одиночке ждать некого и спрашивать некого — Барти сразу говорит, когда
      // приходит стража.
      alone  = Option.when(StoryToolQuest.choice(quests).contains(Path.Alone))(content.text("barti.roof.alone").trim)
      _     <- renderer.show(user, stepScreen(taken, Some((List(content.text("barti.roof.pitch").trim) ++ alone.toList :+ line)
                 .mkString("\n\n"))))
    } yield ()

  /** Экран шага: Щиту — к Юноне, Тени — предложение Братства, ночью — стража у
    * двери; задания нет — Барти торгует. `lead` — что сказать перед этим. */
  private def stepScreen(quests: NpcQuests, lead: Option[String]): Screen = {
    def text(body: String) = (lead.toList :+ body).mkString("\n\n")
    if (BartiRoofQuest.toJunona(quests)) Screen(text(content.text("barti.roof.shield").trim), List(leave.copy(row = Some(0))))
    else if (BartiRoofQuest.toOffer(quests))
      Screen(text(content.text("barti.roof.offer").trim + "\n\n" + difficulty), List(
        content.choice("RoofBrotherhood", "barti.roof.brotherhoodLabel").copy(row = Some(0)),
        content.choice("RoofSelf", "barti.roof.selfLabel").copy(row = Some(0)),
        leave))
    else if (BartiRoofQuest.toNight(quests))
      Screen(text(content.text("barti.roof.night").trim + "\n\n" + difficulty),
        List(content.choice("RoofFight", "barti.roof.fightLabel").copy(row = Some(0)), leave.copy(row = Some(0))))
    else Screen(text(content.text("barti.roof.idle")), List(leave.copy(row = Some(0))))
  }

  /** Тень: пусть решает Братство — без боя; задание закрыто, Барти должен
    * Братству. Не тот шаг — экран шага. */
  private def letBrotherhood(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!BartiRoofQuest.toOffer(quests)) renderer.show(user, stepScreen(quests, None))
      else for {
        hero   <- getHero(user)
        reward <- BartiRoofQuest.finish(heroDao, inventoryRepo, content, user, hero, Some(Deal.Brotherhood))
        _      <- renderer.show(user, Screen(content.text("barti.roof.brotherhood").trim + "\n\n" + reward, List(leave.copy(row = Some(0)))))
      } yield ()
    }.as(StateType.BartiShop)

  /** Тень: разобраться самому — ночью в лавку. Не тот шаг — экран шага. */
  private def bySelf(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!BartiRoofQuest.toOffer(quests)) renderer.show(user, stepScreen(quests, None))
      else {
        val next = BartiRoofQuest.step(quests, 3)
        NpcQuestLog.save(heroDao, user.userId, next) *>
          renderer.show(user, stepScreen(next, Some(content.text("barti.roof.self").trim)))
      }
    }.as(StateType.BartiShop)

  /** Ночью: капитан ночной стражи и четверо; Барти — под прилавком. Добыча
    * вернёт сюда с отметкой победы. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!BartiRoofQuest.toNight(quests)) renderer.show(user, stepScreen(quests, None)).as(StateType.BartiShop)
      else for {
        hero <- getHero(user)
        _    <- StoryFight.start(heroDao, user, hero, BartiRoofQuest.foes(content.text("barti.roof.captainName")), Nil,
                  BartiRoofQuest.FightStory, StateType.BartiShop, WonKey)
        _    <- renderer.show(user, Screen(content.text("barti.roof.fightStart"), Nil))
      } yield StateType.Battle
    }

  /** Отбились: что стало с капитаном — по пути; задание закрыто. */
  private def won(user: User, quests: NpcQuests, renderer: Renderer): Task[Unit] =
    for {
      _      <- heroDao.writeSceneData(user.userId, Json.Null)
      hero   <- getHero(user)
      path    = StoryToolQuest.choice(quests)
      deal    = Option.when(path.contains(Path.Shadow))(Deal.Self)
      coda    = content.text(s"barti.roof.coda.${path.fold(Path.Alone.key)(_.key)}").trim
      reward <- BartiRoofQuest.finish(heroDao, inventoryRepo, content, user, hero, deal)
      _      <- renderer.show(user, Screen(List(content.text("barti.roof.won").trim, coda, reward).mkString("\n\n"),
                  List(leave.copy(row = Some(0)))))
    } yield ()

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object BartiShopState {

  /** Отметка в `scene_data`: ночная стража у лавки Барти побита. */
  val WonKey: String = "bartiRoofWon"
}
