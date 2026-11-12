package pangea.service.state.states.events.treasure

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

/**
 * Событие «мобы, выкопавшие сокровище» (5%). Интро: встречаем 2–3 мобов одной
 * случайной расы. Две кнопки — «Атаковать» (начать цепочку боёв) и «Уйти»
 * (вернуться в лабиринт). Прогресс цепочки (раса/число боёв/диапазон дублонов)
 * кладём в `scene_data` как [[TreasureMobsChain]]; спавном боёв заведует
 * [[TreasureMobsFightState]]. Чужая кнопка или текст — тот же экран ещё раз,
 * с теми же мобами: сундук не пропадает.
 */
case class TreasureMobsState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "AttackMobs" -> Target.Goto(StateType.TreasureMobsFight),
      "LeaveMobs"  -> Target.Run { (user, _, renderer) =>
        heroDao.writeSceneData(user.userId, Json.Null) *>
          renderer.show(user, Screen(content.text("treasureMobs.left"), Nil)).as(StateType.Dungeon)
      }
    ),
    // Написал текст или нажал старую кнопку — тот же экран ещё раз, с теми же
    // мобами из сцены: раньше героя выбрасывало в лабиринт, и сундук пропадал.
    fallback = Target.Run { (user, _, renderer) => again(user, renderer) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets + StateType.Dungeon + StateType.TreasureMobsFight

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      raceIdx <- Random.nextIntBounded(Race.mortals.size)
      race     = Race.mortals(raceIdx)
      count   <- Random.nextIntBetween(2, 4) // 2 или 3
      chain    = TreasureMobsChain(race.entryName, remaining = count, doubloonMin = 2, doubloonMax = 3)
      _       <- heroDao.writeSceneData(user.userId, chain.asJson)
      _       <- show(user, chain, race, renderer)
    } yield ()

  /** Экран мобов у сундука ещё раз — по цепочке из сцены, мобы не выбираются
    * заново. Цепочки нет (сцену стёрли) — показывать нечего: в лабиринт. */
  private def again(user: User, renderer: Renderer): Task[StateType] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[TreasureMobsChain].toOption)).flatMap { chain =>
      chain.flatMap(c => Race.withNameOption(c.race).map(c -> _)) match {
        case Some((c, race)) => show(user, c, race, renderer).as(StateType.TreasureMobs)
        case None            => ZIO.succeed(StateType.Dungeon)
      }
    }

  private def show(user: User, chain: TreasureMobsChain, race: Race, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(
      content.format("treasureMobs.enter.text",
        "count" -> chain.remaining.toString,
        "race"  -> race.toString),
      content.screen("treasureMobs.enter").choices))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
