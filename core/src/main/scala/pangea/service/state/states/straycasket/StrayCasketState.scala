package pangea.service.state.states.straycasket

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.Difficulty
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.straycasket.StrayCasketState._
import pangea.service.state.{NpcQuestLog, QuestGoods, State, StrayCasketQuest, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Бродячий ларец» в лабиринте. Сюда ведут два пути:
  *
  * Редкая встреча с осмотра уровня (RareQuests, см. DungeonState): чужой Ларец
  * Азата кружит рядом. «Протянуть камень» — ларец выхватывает самый простой
  * камень (если он есть при герое) и увязывается следом: в сумке — его
  * карточка, кормят его оттуда. «Уйти» — задание не начато, ларец может
  * встретиться снова.
  *
  * Сытый ларец отпускают с его карточки («Идти за ларцом», только в лабиринте,
  * см. InventoryState), и он ведёт к хозяину: Ансельм Вейл, поднятый мертвец
  * уровня задания. Бой сюжетный (`story = strayCasket`): без «Сбежать» и без
  * отряда, добычи по таблицам нет. Победа возвращает сюда (`Progress(AfterFight)`
  * через добычу): хозяин упокоен, ларец нести Фету. Погиб или ушёл до боя —
  * задание ждёт на том же шаге, ларец снова отпускают с карточки. */
case class StrayCasketState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  artifacts:     Option[ArtifactRepository] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "StrayOffer" -> Target.Run { (u, _, r) => offer(u, r) },
      "StrayShoo"  -> Target.Run { (u, _, r) => shoo(u, r) },
      "StrayFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "StrayLeave" -> Target.Goto(StateType.Dungeon)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StrayCasket) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StrayCasket)

  /** Вход: после боя (добыча положила `Progress(AfterFight)`) — хозяин упокоен;
    * ларец сыт — дорога к хозяину; задания ещё не было — встреча. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      quests   <- NpcQuestLog.load(heroDao, user.userId)
      progress <- heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Progress].toOption))
      _        <- if (progress.exists(_.step == Step.AfterFight) && StrayCasketQuest.leads(quests))
                    heroDao.writeSceneData(user.userId, Json.Null) *>
                      StrayCasketQuest.laidToRest(heroDao, user.userId) *>
                      renderer.show(user, Screen(content.text("strayCasket.rest"), List(leaveChoice)))
                  else if (StrayCasketQuest.leads(quests)) renderer.show(user, ownerScreen)
                  else if (StrayCasketQuest.fresh(quests)) renderer.show(user, meetScreen)
                  else renderer.show(user, Screen(content.text("strayCasket.gone"), List(leaveChoice)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Встреча ─────────────────────────────────────────────────────────────────

  /** Ларец кружит рядом и щёлкает зубами на камни героя; сложность — сразу. */
  private def meetScreen: Screen =
    Screen(content.text("strayCasket.meet") + "\n\n" + difficultyLine, List(
      content.choice("StrayOffer", "strayCasket.offerLabel").copy(row = Some(0)),
      content.choice("StrayShoo", "strayCasket.shooLabel").copy(row = Some(0))))

  private def difficultyLine: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StrayCasketQuest.Level))

  /** «Протянуть камень»: задание начато, ларец — в сумке. Самый простой камень
    * при герое он выхватывает сразу; камней нет — всё равно увязывается следом. */
  private def offer(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!StrayCasketQuest.fresh(quests)) enter(user, renderer).as(StateType.StrayCasket)
                else for {
                  _      <- StrayCasketQuest.start(heroDao, inventoryRepo, itemRepo, user.userId, hero, now)
                  goods  <- QuestGoods.all(inventoryRepo, artifacts, hero.id)
                  first   = StrayCasketQuest.cheapest(goods)
                  after  <- ZIO.foreach(first)(g =>
                              StrayCasketQuest.feed(heroDao, inventoryRepo, artifacts, user.userId, hero.id, g)).map(_.flatten)
                  eaten   = first.filter(_ => after.isDefined)
                  left    = after.fold(StrayCasketQuest.GemsGoal)(StrayCasketQuest.left)
                  _      <- renderer.show(user, Screen(eaten.fold(content.text("strayCasket.offerEmpty"))(g =>
                              content.format("strayCasket.offerFed", "gem" -> g.item.name)), Nil))
                  _      <- renderer.show(user, Screen(
                              content.format("marisa.questItemAdded", "item" -> QuestItemKind.StrayCasket.displayName) + "\n" +
                                content.format("strayCasket.feedHint", "left" -> left.toString), Nil))
                } yield StateType.Dungeon
    } yield res

  /** «Уйти»: задание не начато, ларец остаётся в изгороди. */
  private def shoo(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StrayCasketQuest.fresh(quests)) enter(user, renderer).as(StateType.StrayCasket)
      else renderer.show(user, Screen(content.text("strayCasket.shooed"), Nil)).as(StateType.Dungeon)
    }

  // ── Хозяин ларца ────────────────────────────────────────────────────────────

  /** Сытый ларец привёл к хозяину: бой или назад в лабиринт. */
  private def ownerScreen: Screen =
    Screen(content.text("strayCasket.owner") + "\n\n" + difficultyLine, List(
      content.choice("StrayFight", "strayCasket.fightLabel").copy(row = Some(0)),
      content.choice("StrayLeave", "strayCasket.leaveLabel").copy(row = Some(0))))

  private def leaveChoice = content.choice("StrayLeave", "strayCasket.toDungeonLabel")

  /** Бой с хозяином: один на один — отряд в сюжетный бой не берут; добыча
    * вернёт сюда, к развязке. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!StrayCasketQuest.leads(quests)) enter(user, renderer).as(StateType.StrayCasket)
                else for {
                  pct    <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                  battle  = SoloPveBattle.from(StrayCasketQuest.owner, hero, squad = false).withStartEnergy(pct)
                              .copy(story = Some(StrayCasketQuest.FightStory))
                  routing = LootData(Nil, Nil, returnState = Some(StateType.StrayCasket),
                              eventData = Some(Progress(Step.AfterFight).asJson))
                  _      <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _      <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _      <- renderer.show(user, Screen(content.text("strayCasket.fightStart"), Nil))
                } yield StateType.Battle
    } yield res

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StrayCasketState {

  /** Где мы: развязка после боя с хозяином ларца. */
  object Step {
    val AfterFight: String = "afterFight"
  }

  final case class Progress(step: String)

  object Progress {
    implicit val encoder: Encoder[Progress] = (p: Progress) => Json.obj("strayCasketStep" -> p.step.asJson)
    // Ключ свой, а не общий «step»: в scene_data лежат и чужие структуры, и
    // случайное совпадение поля не должно читаться как наш прогресс.
    implicit val decoder: Decoder[Progress] = (c: HCursor) => c.get[String]("strayCasketStep").map(Progress(_))
  }
}
