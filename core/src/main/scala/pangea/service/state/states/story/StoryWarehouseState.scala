package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.{Difficulty, NpcQuest}
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{NpcQuestDialog, NpcQuestLog, QuestOutcomeLog, QuestSupport, State, StoryPowderQuest, UserAction}
import zio.{Random, Task, ZIO}

/** «Порох у причала»: дегтярный склад у причала (см. StoryPowderQuest). Сюда
  * ведёт карточка шифровки — «Идти в порт», из города. Смотритель узнаёт
  * печать на футляре, но не курьера: «Драться» — сюжетный бой с его людьми и
  * отрядом героя, без «Сбежать», награда как за обычный бой; добыча
  * возвращает сюда с отметкой победы ([[StoryWarehouseState.Won]]) — глава на
  * втором шаге, Смотритель на полу. Его судьба — три кнопки; выбор закрывает
  * главу: шифровка уходит, набросок узора (зацепка следующей главы) — в
  * сумку, касса Смотрителя — в кошель, исход — в подсчёт «как и вы». Ушёл или
  * погиб — склад ждёт, шифровка поведёт снова. */
case class StoryWarehouseState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  outcomes:      Option[QuestOutcomeDao] = None
) extends State {

  private val chapter = NpcQuestDialog(heroDao, content, NpcQuest.StoryPowder, "StoryPowder")

  private val branch = new Branch(
    routes = Map(
      "WarehouseFight"   -> Target.Run { (u, _, r) => fight(u, r) },
      "WarehouseLeave"   -> Target.Goto(StateType.HarborQuarter),
      "WardenKill"       -> Target.Run { (u, _, r) => decide(u, r, WardenFate.Killed) },
      "WardenRelease"    -> Target.Run { (u, _, r) => decide(u, r, WardenFate.Released) },
      "WardenHand"       -> Target.Run { (u, _, r) => decide(u, r, WardenFate.Handed) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryWarehouse) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryWarehouse)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      won    <- heroDao.readSceneData(user.userId).map(_.exists(_.as[StoryWarehouseState.Won].exists(_.won)))
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (won && StoryPowderQuest.toWarehouse(quests))
                  heroDao.writeSceneData(user.userId, Json.Null) *>
                    NpcQuestLog.save(heroDao, user.userId, StoryPowderQuest.won(quests)) *>
                    renderer.show(user, fateScreen)
                else if (StoryPowderQuest.beaten(quests)) renderer.show(user, fateScreen)
                else if (StoryPowderQuest.toWarehouse(quests)) renderer.show(user, doorScreen)
                else renderer.show(user, Screen(content.text("story.storyPowder.warehouse.gone"), List(leave)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def doorScreen: Screen =
    Screen(
      content.text("story.storyPowder.warehouse.door") + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryPowderQuest.Level)),
      List(
        content.choice("WarehouseFight", "story.storyPowder.warehouse.fightLabel").copy(row = Some(0)),
        content.choice("WarehouseLeave", "story.storyPowder.warehouse.leaveLabel").copy(row = Some(0))))

  private def fateScreen: Screen =
    Screen(content.text("story.storyPowder.fate.text"), List(
      content.choice("WardenKill", "story.storyPowder.fate.killLabel").copy(color = ChoiceColor.Negative, row = Some(0)),
      content.choice("WardenRelease", "story.storyPowder.fate.releaseLabel").copy(row = Some(1)),
      content.choice("WardenHand", "story.storyPowder.fate.handLabel").copy(color = ChoiceColor.Positive, row = Some(2))))

  private def leave = content.choice("WarehouseLeave", "story.storyPowder.warehouse.leaveLabel")

  /** Бой с людьми Смотрителя: он сам и трое уровня задания, отряд с ним.
    * Добыча вернёт сюда — с отметкой, что склад взят. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!StoryPowderQuest.toWarehouse(quests)) enter(user, renderer).as(StateType.StoryWarehouse)
                else for {
                  guards   <- ZIO.succeed(StoryPowderQuest.guards(content.text("story.storyPowder.wardenName")))
                  energies <- ZIO.foreach(guards)(m =>
                                Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                  .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                  battle    = SoloPveBattle.fromGroup(guards, hero, energies).copy(story = Some(StoryPowderQuest.FightStory))
                  routing   = LootData(Nil, Nil, returnState = Some(StateType.StoryWarehouse),
                                eventData = Some(StoryWarehouseState.WonMark.asJson))
                  _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _        <- renderer.show(user, Screen(content.text("story.storyPowder.warehouse.fightStart"), Nil))
                } yield StateType.Battle
    } yield res

  /** Судьба Смотрителя: глава закрыта выбором, шифровка уходит, набросок
    * узора — в сумку, касса — в кошель, исход — в подсчёт. */
  private def decide(user: User, renderer: Renderer, fate: WardenFate): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      hero   <- getHero(user)
      _      <- if (!StoryPowderQuest.beaten(quests)) enter(user, renderer)
                else for {
                  _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StoryCipher))
                  _      <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StorySketch)
                  silver  = StoryPowderQuest.silver
                  _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
                  done   <- chapter.complete(user, hero.copy(silver = hero.silver + silver),
                              q => q.update(NpcQuest.StoryPowder)(_.copy(counter = fate.n)))
                  (_, expLine) = done
                  stats  <- QuestOutcomeLog.recordAndCount(outcomes, content, hero, fate.outcome)
                  _      <- renderer.show(user, Screen(
                              content.text(s"story.storyPowder.fate.${fate.key}").trim + "\n\n" +
                                content.text("story.storyPowder.sketch").trim, Nil))
                  _      <- renderer.show(user, Screen(
                              content.format("story.storyPowder.reward", "silver" -> silver.toString, "exp" -> expLine) +
                                "\n\n" + QuestOutcomeLog.withStats(content.text("story.storyPowder.questDone"), stats), List(leave)))
                } yield ()
    } yield StateType.StoryWarehouse

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryWarehouseState {

  /** Отметка в scene_data: люди Смотрителя перебиты. */
  final case class Won(won: Boolean = true)

  object Won {
    implicit val encoder: Encoder[Won] = (w: Won) => Json.obj("storyWarehouseWon" -> w.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры.
    implicit val decoder: Decoder[Won] = (c: HCursor) => c.get[Boolean]("storyWarehouseWon").map(Won(_))
  }

  val WonMark: Won = Won(won = true)
}
