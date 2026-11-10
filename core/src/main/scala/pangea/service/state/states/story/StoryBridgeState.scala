package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.quest.Difficulty
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{NpcQuestLog, State, StoryListQuest, UserAction}
import zio.{Random, Task, ZIO}

/** «Список», шаг 3: Мельничный мост ночью (см. StoryListQuest). Сюда ведёт
  * карточка страницы списка — «Идти к Мельничному мосту», из города. Сойки
  * нет — есть чистильщики Братства. «Драться» — сюжетный бой с отрядом героя,
  * без «Сбежать», награда как за обычный бой; добыча возвращает сюда с
  * отметкой победы ([[StoryBridgeState.Won]]) — на главаре приказ: героя
  * привела «птица». Глава на четвёртом шаге: найти Сойку. Ушёл или погиб —
  * засада ждёт, страница поведёт снова. */
case class StoryBridgeState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "BridgeFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "BridgeLeave" -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryBridge) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryBridge)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      won    <- heroDao.readSceneData(user.userId).map(_.exists(_.as[StoryBridgeState.Won].exists(_.won)))
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (won && StoryListQuest.toBridge(quests))
                  heroDao.writeSceneData(user.userId, Json.Null) *>
                    NpcQuestLog.save(heroDao, user.userId, StoryListQuest.step(quests, 4)) *>
                    renderer.show(user, Screen(content.text("story.storyList.bridge.order"), List(leave)))
                else if (StoryListQuest.toBridge(quests)) renderer.show(user, ambushScreen)
                else renderer.show(user, Screen(content.text("story.storyList.bridge.gone"), List(leave)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def ambushScreen: Screen =
    Screen(
      content.text("story.storyList.bridge.ambush") + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryListQuest.Level)),
      List(
        content.choice("BridgeFight", "story.storyList.bridge.fightLabel").copy(row = Some(0)),
        content.choice("BridgeLeave", "story.storyList.bridge.leaveLabel").copy(row = Some(0))))

  private def leave = content.choice("BridgeLeave", "story.storyList.bridge.leaveLabel")

  /** Бой с чистильщиками: главарь и трое уровня задания, отряд с героем.
    * Добыча вернёт сюда — с отметкой, что засада отбита. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!StoryListQuest.toBridge(quests)) enter(user, renderer).as(StateType.StoryBridge)
                else for {
                  guards   <- ZIO.succeed(StoryListQuest.guards(content.text("story.storyList.leaderName")))
                  energies <- ZIO.foreach(guards)(m =>
                                Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                  .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                  battle    = SoloPveBattle.fromGroup(guards, hero, energies).copy(story = Some(StoryListQuest.FightStory))
                  routing   = LootData(Nil, Nil, returnState = Some(StateType.StoryBridge),
                                eventData = Some(StoryBridgeState.WonMark.asJson))
                  _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _        <- renderer.show(user, Screen(content.text("story.storyList.bridge.fightStart"), Nil))
                } yield StateType.Battle
    } yield res

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryBridgeState {

  /** Отметка в scene_data: засада у моста отбита. */
  final case class Won(won: Boolean = true)

  object Won {
    implicit val encoder: Encoder[Won] = (w: Won) => Json.obj("storyBridgeWon" -> w.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры.
    implicit val decoder: Decoder[Won] = (c: HCursor) => c.get[Boolean]("storyBridgeWon").map(Won(_))
  }

  val WonMark: Won = Won(won = true)
}
