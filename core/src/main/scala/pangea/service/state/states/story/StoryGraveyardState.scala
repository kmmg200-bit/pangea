package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.domain.Rng
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.quest.{Difficulty, NpcQuest}
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.parcel.Parcels
import pangea.service.state.StoryLine.Survivors
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{NpcQuestDialog, NpcQuestLog, QuestOutcomeLog, QuestSupport, State, StoryGearPost, StoryLine,
  StoryToolQuest, UserAction}
import zio.{Random, Task, ZIO}

/** Рубеж I «Чьё ты орудие»: старое кладбище за городской стеной в полночь
  * (см. StoryToolQuest). Сюда ведёт карточка записки «Э.» — «Идти на
  * кладбище», из города. Ждёт тот, кто пережил пролог (или оба): он узнал
  * героя и предлагает войти в Братство самому; из-за надгробий встают люди
  * ячейки. «Драться» — сюжетный бой с отрядом героя, без «Сбежать», награда
  * как за обычный бой; добыча возвращает сюда с отметкой победы
  * ([[StoryGraveyardState.Won]]) — рубеж на втором шаге: выбор пути. Путь
  * закрывает рубеж: записка уходит, снаряжение пути и его знак — в сумку
  * (снаряжение, что не влезло, — посылкой, `parcels`), серебро, своё
  * достижение, исход — в подсчёт «как и вы». Ушёл или погиб — встреча ждёт,
  * записка поведёт снова. */
case class StoryGraveyardState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  outcomes:      Option[QuestOutcomeDao] = None,
  parcels:       Option[Parcels] = None
) extends State {

  private val milestone = NpcQuestDialog(heroDao, content, NpcQuest.StoryTool, "StoryTool")

  private val branch = new Branch(
    routes = Map(
      "GraveFight"  -> Target.Run { (u, _, r) => fight(u, r) },
      "GraveShadow" -> Target.Run { (u, _, r) => decide(u, r, Path.Shadow) },
      "GraveShield" -> Target.Run { (u, _, r) => decide(u, r, Path.Shield) },
      "GraveAlone"  -> Target.Run { (u, _, r) => decide(u, r, Path.Alone) },
      "GraveLeave"  -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryGraveyard) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryGraveyard)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      won       <- heroDao.readSceneData(user.userId).map(_.exists(_.as[StoryGraveyardState.Won].exists(_.won)))
      quests    <- NpcQuestLog.load(heroDao, user.userId)
      survivors <- StoryLine.survivorsOf(heroDao, user.userId)
      _         <- if (won && StoryToolQuest.toGraveyard(quests))
                     heroDao.writeSceneData(user.userId, Json.Null) *>
                       NpcQuestLog.save(heroDao, user.userId, StoryToolQuest.step(quests, 2)) *>
                       renderer.show(user, choiceScreen(survivors))
                   else if (StoryToolQuest.toChoice(quests)) renderer.show(user, choiceScreen(survivors))
                   else if (StoryToolQuest.toGraveyard(quests)) renderer.show(user, meetScreen(survivors))
                   else renderer.show(user, Screen(content.text("story.storyTool.graveyard.gone"), List(leave)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave = content.choice("GraveLeave", "story.storyTool.graveyard.leaveLabel")

  private def joined(keys: String*): String = keys.map(k => content.text(k).trim).mkString("\n\n")

  /** Снаряжение пути в строке награды: что легло в сумку — по строке на вещь,
    * что ушло посылкой — строкой почты. */
  private def gearLines(bagged: List[Item], posted: Option[String]): String =
    bagged.map(i => "\n" + content.format("story.storyTool.gearLine", "name" -> i.displayTitle)).mkString +
      posted.fold("")("\n\n" + _)

  /** Встреча: кладбище, тот (или те), кто ждёт, его сделка — и люди за
    * надгробиями. */
  private def meetScreen(survivors: Survivors): Screen =
    Screen(
      joined("story.storyTool.graveyard.text", s"story.storyTool.graveyard.meet${survivors.suffix}",
        "story.storyTool.graveyard.answer") + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryToolQuest.Level)),
      List(
        content.choice("GraveFight", "story.storyTool.graveyard.fightLabel").copy(row = Some(0)),
        content.choice("GraveLeave", "story.storyTool.graveyard.leaveLabel").copy(row = Some(0))))

  /** После боя: вопрос рубежа и три пути — по ряду на каждый. */
  private def choiceScreen(survivors: Survivors): Screen =
    Screen(
      joined(s"story.storyTool.choice.with${survivors.suffix}", "story.storyTool.choice.question"),
      List(
        content.choice("GraveShadow", "story.storyTool.choice.shadowLabel").copy(row = Some(0)),
        content.choice("GraveShield", "story.storyTool.choice.shieldLabel").copy(color = ChoiceColor.Positive, row = Some(1)),
        content.choice("GraveAlone", "story.storyTool.choice.aloneLabel").copy(color = ChoiceColor.Negative, row = Some(2))))

  /** Бой с ячейкой: выживший (или оба) и люди ячейки уровня задания, отряд с
    * героем. Добыча вернёт сюда — с отметкой, что люди ячейки лежат. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero      <- getHero(user)
      quests    <- NpcQuestLog.load(heroDao, user.userId)
      survivors <- StoryLine.survivorsOf(heroDao, user.userId)
      res       <- if (!StoryToolQuest.toGraveyard(quests)) enter(user, renderer).as(StateType.StoryGraveyard)
                   else for {
                     foes     <- ZIO.succeed(StoryToolQuest.foes(survivors, content))
                     energies <- ZIO.foreach(foes)(m =>
                                   Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                     .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                     battle    = SoloPveBattle.fromGroup(foes, hero, energies).copy(story = Some(StoryToolQuest.FightStory))
                     routing   = LootData(Nil, Nil, returnState = Some(StateType.StoryGraveyard),
                                   eventData = Some(StoryGraveyardState.WonMark.asJson))
                     _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                     _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                     _        <- renderer.show(user, Screen(content.text("story.storyTool.graveyard.fightStart"), Nil))
                   } yield StateType.Battle
    } yield res

  /** Путь выбран: рубеж закрыт, записка уходит, знак пути — в сумку,
    * снаряжение пути — туда же (не влезло — посылкой), серебро, достижение
    * пути, исход — в подсчёт «как и вы». */
  private def decide(user: User, renderer: Renderer, path: Path): Task[StateType] =
    for {
      quests    <- NpcQuestLog.load(heroDao, user.userId)
      hero      <- getHero(user)
      survivors <- StoryLine.survivorsOf(heroDao, user.userId)
      _         <- if (!StoryToolQuest.toChoice(quests)) enter(user, renderer)
                   else for {
                     _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StoryNote))
                     _      <- ZIO.foreachDiscard(path.token)(QuestSupport.giveItem(inventoryRepo, itemRepo, hero, _))
                     seed   <- Random.nextLong
                     now    <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
                     given  <- StoryGearPost.give(inventoryRepo, itemRepo, parcels, hero,
                                 content.text(s"story.storyTool.gearFrom.${path.key}"), StoryToolQuest.gear(path, Rng(seed)), now)
                     (bagged, posted) = given
                     silver  = StoryToolQuest.silver
                     _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
                     done   <- milestone.complete(user, hero.copy(silver = hero.silver + silver),
                                 q => q.update(NpcQuest.StoryTool)(_.copy(counter = path.n)))
                     (leveled, expLine) = done
                     stats  <- QuestOutcomeLog.recordAndCount(outcomes, content, hero, path.outcome)
                     _      <- renderer.show(user, Screen(
                                 joined(List(s"story.storyTool.path.${path.key}.with${survivors.suffix}",
                                   s"story.storyTool.path.${path.key}.end") ++
                                   Option.when(path.gearLine)(s"story.storyTool.path.${path.key}.gear"): _*), Nil))
                     _      <- QuestSupport.grant(heroDao, content, user, leveled, path.achievement, renderer)
                     _      <- renderer.show(user, Screen(
                                 content.format(s"story.storyTool.reward.${path.key}", "silver" -> silver.toString, "exp" -> expLine) +
                                   gearLines(bagged, posted) +
                                   "\n\n" + QuestOutcomeLog.withStats(content.text("story.storyTool.questDone"), stats), List(leave)))
                   } yield ()
    } yield StateType.StoryGraveyard

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryGraveyardState {

  /** Отметка в scene_data: люди ячейки на кладбище лежат. */
  final case class Won(won: Boolean = true)

  object Won {
    implicit val encoder: Encoder[Won] = (w: Won) => Json.obj("storyGraveyardWon" -> w.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры.
    implicit val decoder: Decoder[Won] = (c: HCursor) => c.get[Boolean]("storyGraveyardWon").map(Won(_))
  }

  val WonMark: Won = Won(won = true)
}
