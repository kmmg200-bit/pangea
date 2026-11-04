package pangea.service.state.states.barrelthieves

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
import pangea.service.state.{BarrelThievesQuest, NpcQuestLog, State, UserAction}
import zio.{Random, Task, ZIO}

/** «Вскрытая бочка»: старый склад у причала, куда ведёт улика после Ришелье
  * (см. InventoryState, «Идти по следу»). Там банда гоблинов-рабов перешивает
  * украденное. «Напасть» — сюжетный бой (`story = barrelThieves`) с отрядом
  * героя: четверо гоблинов его уровня, без «Сбежать», награда как за обычный
  * бой. Победа ведёт через добычу к возврату украденного (BarrelReturnState),
  * погиб или ушёл — задание ждёт на том же шаге, по следу можно идти снова. У
  * героя-гоблина банда говорит иначе (`<ключ>Kin`). */
case class BarrelThievesState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "ThievesFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "ThievesLeave" -> Target.Goto(StateType.HarborQuarter)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.BarrelThieves) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.BarrelThieves)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (BarrelThievesQuest.onTrail(quests))
                  renderer.show(user, Screen(
                    content.format("barrelThieves." + BarrelThievesQuest.key(hero, "lair"),
                      "gang" -> BarrelThievesQuest.gangName(content, quests)) + "\n\n" +
                      content.format("marisa.difficulty", "difficulty" -> Difficulty.render(hero.lvl.toInt)),
                    List(
                      content.choice("ThievesFight", "barrelThieves.fightLabel").copy(row = Some(0)),
                      content.choice("ThievesLeave", "barrelThieves.leaveLabel").copy(row = Some(0)))))
                else renderer.show(user, Screen(content.text("barrelThieves.gone"),
                       List(content.choice("ThievesLeave", "barrelThieves.leaveLabel"))))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Бой с бандой: четверо гоблинов-рабов уровня героя, отряд с ним. Добыча
    * вернёт к бочке — с отметкой, что банда разбита. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!BarrelThievesQuest.onTrail(quests)) enter(user, renderer).as(StateType.BarrelThieves)
                else for {
                  gang     <- ZIO.succeed(BarrelThievesQuest.gang(hero.lvl))
                  energies <- ZIO.foreach(gang)(m =>
                                Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                  .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                  battle    = SoloPveBattle.fromGroup(gang, hero, energies).copy(story = Some(BarrelThievesQuest.FightStory))
                  routing   = LootData(Nil, Nil, returnState = Some(StateType.BarrelReturn),
                                eventData = Some(BarrelThievesState.WonMark.asJson))
                  _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _        <- renderer.show(user, Screen(content.text("barrelThieves.fightStart"), Nil))
                } yield StateType.Battle
    } yield res

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object BarrelThievesState {

  /** Отметка в scene_data: банда разбита, украденное можно возвращать. */
  final case class Won(won: Boolean = true)

  object Won {
    implicit val encoder: Encoder[Won] = (w: Won) => Json.obj("barrelThievesWon" -> w.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры.
    implicit val decoder: Decoder[Won] = (c: HCursor) => c.get[Boolean]("barrelThievesWon").map(Won(_))
  }

  val WonMark: Won = Won(won = true)
}
