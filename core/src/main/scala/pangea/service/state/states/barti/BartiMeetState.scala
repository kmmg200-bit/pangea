package pangea.service.state.states.barti

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.{Difficulty, NpcQuest}
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{HotGoodsQuest, NpcQuestDialog, NpcQuestLog, QuestSupport, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Барти у изгороди (см. HotGoodsQuest). Сюда ведёт осмотр уровня лабиринта
  * (DungeonState): впервые — наугад, с 4-го уровня героя; второй раз — сам,
  * когда куб есть и товар в сумке.
  *
  * Впервые: Барти зажали звери. «Спасти Барти» — сюжетный бой с отрядом героя,
  * без «Сбежать», награда как за обычный бой; добыча возвращает сюда с
  * отметкой победы ([[BartiMeetState.Won]]) — разговор: «любой грамотный и
  * способный человек раскрутится», рецепты живой воды и шнапса, листок с
  * планом, задание взято. «Пройти мимо» или смерть задание не начинают.
  * Второй раз: Барти видит товар — адрес его дома, должок за шалфей, задание
  * закрыто. */
case class BartiMeetState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.HotGoods, "HotGoods")

  private val branch = new Branch(
    routes = Map(
      "BartiFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "BartiPass"  -> Target.Run { (u, _, r) => pass(u, r) },
      "BartiOut"   -> Target.Goto(StateType.Dungeon)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.BartiMeet) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.BartiMeet, StateType.Dungeon)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      won    <- heroDao.readSceneData(user.userId).map(_.exists(_.as[BartiMeetState.Won].exists(_.won)))
      hero   <- getHero(user)
      quests <- HotGoodsQuest.refresh(heroDao, inventoryRepo, user.userId, hero)
      _      <- if (won && HotGoodsQuest.fresh(quests)) talk(user, hero, renderer)
                else if (HotGoodsQuest.fresh(quests)) renderer.show(user, meetScreen)
                else if (HotGoodsQuest.ready(quests)) again(user, hero, renderer)
                else renderer.show(user, Screen(content.text("barti.meet.gone"), List(out)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def out = content.choice("BartiOut", "barti.meet.onwardLabel")

  private def meetScreen: Screen =
    Screen(
      content.text("barti.meet.text").trim + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(HotGoodsQuest.Level)),
      List(
        content.choice("BartiFight", "barti.meet.fightLabel").copy(row = Some(0)),
        content.choice("BartiPass", "barti.meet.passLabel").copy(row = Some(0))))

  /** «Пройти мимо»: задание не начато, Барти встретится снова. */
  private def pass(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("barti.meet.passed"), Nil)).as(StateType.Dungeon)

  /** Бой со зверями уровня задания, отряд с героем. Добыча вернёт сюда — с
    * отметкой, что звери отбиты. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!HotGoodsQuest.fresh(quests)) enter(user, renderer).as(StateType.BartiMeet)
                else for {
                  beasts   <- ZIO.succeed(HotGoodsQuest.beasts)
                  energies <- ZIO.foreach(beasts)(m =>
                                Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                  .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                  battle    = SoloPveBattle.fromGroup(beasts, hero, energies).copy(story = Some(HotGoodsQuest.FightStory))
                  routing   = LootData(Nil, Nil, returnState = Some(StateType.BartiMeet),
                                eventData = Some(BartiMeetState.WonMark.asJson))
                  _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _        <- renderer.show(user, Screen(content.text("barti.meet.fightStart"), Nil))
                } yield StateType.Battle
    } yield res

  /** Звери отбиты: разговор, рецепты, листок с планом — задание взято. Шаг —
    * сразу по кубу и сумке: у кого всё уже есть, того Барти найдёт сам. */
  private def talk(user: User, hero: Hero, renderer: Renderer): Task[Unit] =
    for {
      _      <- heroDao.writeSceneData(user.userId, Json.Null)
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      _      <- NpcQuestLog.modify(heroDao, user.userId)(q =>
                  HotGoodsQuest.Recipes.foldLeft(q.take(NpcQuest.HotGoods, now))(_.withRecipe(_)))
      given  <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.BartiPlan)
      quests <- HotGoodsQuest.refresh(heroDao, inventoryRepo, user.userId, hero)
      _      <- renderer.show(user, Screen(content.text("barti.meet.talk").trim, Nil))
      _      <- renderer.show(user, Screen(
                  content.text("barti.meet.plan").trim + "\n\n" +
                    content.format("barti.taken", "title" -> content.text("journal.quests.hotGoods.title"),
                      "step" -> content.list("journal.quests.hotGoods.steps")
                        .lift(quests.of(NpcQuest.HotGoods).step - 1).getOrElse("")) +
                    (if (given) "\n" + content.format("marisa.questItemAdded", "item" -> QuestItemKind.BartiPlan.displayName)
                     else ""),
                  List(out)))
    } yield ()

  /** Второй раз: товар в сумке — адрес дома на Торговой площади, должок за
    * шалфей, задание закрыто. План своё отслужил — уходит из сумки. */
  private def again(user: User, hero: Hero, renderer: Renderer): Task[Unit] =
    for {
      _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.BartiPlan))
      _      <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.BartiAddress)
      silver  = HotGoodsQuest.silver
      _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
      done   <- quest.complete(user, hero.copy(silver = hero.silver + silver), identity)
      (_, expLine) = done
      _      <- renderer.show(user, Screen(content.text("barti.again.text").trim, Nil))
      _      <- renderer.show(user, Screen(
                  content.format("barti.again.reward", "silver" -> silver.toString, "exp" -> expLine) + "\n" +
                    content.format("marisa.questItemAdded", "item" -> QuestItemKind.BartiAddress.displayName) + "\n\n" +
                    content.text("barti.again.questDone"), List(out)))
    } yield ()

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object BartiMeetState {

  /** Отметка в scene_data: звери у изгороди отбиты. */
  final case class Won(won: Boolean = true)

  object Won {
    implicit val encoder: Encoder[Won] = (w: Won) => Json.obj("bartiHedgeWon" -> w.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры.
    implicit val decoder: Decoder[Won] = (c: HCursor) => c.get[Boolean]("bartiHedgeWon").map(Won(_))
  }

  val WonMark: Won = Won(won = true)
}
