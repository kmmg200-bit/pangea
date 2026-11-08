package pangea.service.squad

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.squad.Squad
import pangea.model.user.User
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Приказы отряду без экрана: переставить союзника или героя, выгнать, решить,
  * кем пожертвовать ради восставшего. Одно на бота
  * ([[pangea.service.state.states.hero.SquadState]]) и на Mini App — правила и
  * слова у них одни. Всё меняет героя, поэтому зовётся под локом игрока: в
  * боте его держит ход, в приложении — сам запрос. Что сказать игроку, идёт в
  * `renderer`. Каждый приказ возвращает строй после него; `None` — приказ ни о
  * чём (союзника на том месте уже нет, восставший уже не ждёт), и ничего не
  * записано. */
final class SquadOrders(heroDao: HeroDao, content: SceneContent) {

  /** Союзника с места `from` — на место `to`: занято другим — меняются местами,
    * занято героем — герой встаёт на прежнее место союзника. */
  def move(user: User, hero: Hero, from: Int, to: Int, renderer: Renderer): Task[Option[Squad]] =
    hero.squad.allyAt(from) match {
      case None => ZIO.none
      case Some(a) =>
        val moved = hero.squad.moveAt(from, to)
        save(user, moved, content.format("squad.moved", "name" -> a.name, "n" -> to.toString), renderer)
    }

  /** Герой — на место `to`: союзник оттуда встаёт на прежнее место героя. Уже
    * стоит там — ни о чём. */
  def moveHero(user: User, hero: Hero, to: Int, renderer: Renderer): Task[Option[Squad]] = {
    val moved = hero.squad.moveHero(to)
    if (moved == hero.squad) ZIO.none
    else save(user, moved, content.format("squad.heroMoved", "n" -> to.toString), renderer)
  }

  def dismiss(user: User, hero: Hero, at: Int, renderer: Renderer): Task[Option[Squad]] =
    hero.squad.allyAt(at) match {
      case None    => ZIO.none
      case Some(a) => save(user, hero.squad.dismissAt(at), content.format("squad.dismissed", "name" -> a.name), renderer)
    }

  /** Восставший занимает место союзника с места `at` — тот покидает отряд. */
  def takeRisenPlace(user: User, hero: Hero, at: Int, renderer: Renderer): Task[Option[Squad]] =
    hero.squad.pendingRisen match {
      case None => ZIO.none
      case Some(form) =>
        ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS)).flatMap { now =>
          val gone = hero.squad.allyAt(at).map(_.name).getOrElse("")
          save(user, hero.squad.takeRisenPlace(at, hero.lvl, now),
            content.format("squad.risenTook", "name" -> form.name, "gone" -> gone), renderer)
        }
    }

  /** Восставшего отпускают: тело оседает прахом. */
  def dismissRisen(user: User, hero: Hero, renderer: Renderer): Task[Option[Squad]] =
    if (hero.squad.pendingRisen.isEmpty) ZIO.none
    else save(user, hero.squad.dismissRisen, content.text("squad.risenDust"), renderer)

  private def save(user: User, squad: Squad, line: String, renderer: Renderer): Task[Option[Squad]] =
    heroDao.updateSquad(user.userId, squad) *> renderer.show(user, Screen(line, Nil)).as(Some(squad))
}
