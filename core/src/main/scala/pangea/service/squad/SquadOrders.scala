package pangea.service.squad

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.squad.{AllyRates, Squad}
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

  /** Восставший занимает место союзника с места `at` — тот покидает отряд.
    * Ждёт Тёмный волк, а свой в отряде уже есть, — названный остаётся: новый
    * волк вцепляется в прежнего и встаёт на его место (см. Squad.takeRisenPlace). */
  def takeRisenPlace(user: User, hero: Hero, at: Int, renderer: Renderer): Task[Option[Squad]] =
    hero.squad.pendingRisen match {
      case None => ZIO.none
      case Some(form) =>
        ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS)).flatMap { now =>
          // Волки дерутся, только если прежний стоит в строю: из резерва его
          // место названному союзнику не освободить.
          val line = hero.squad.wolfRival(form).filter(_.position > 0) match {
            case Some(_) => content.format("squad.wolfFight", "name" -> form.name)
            case None =>
              val gone = hero.squad.allyAt(at).map(_.name).getOrElse("")
              content.format("squad.risenTook", "name" -> form.name, "gone" -> gone)
          }
          save(user, hero.squad.takeRisenPlace(at, hero.lvl, now), line, renderer)
        }
    }

  /** Восставшего отпускают: тело оседает прахом. */
  def dismissRisen(user: User, hero: Hero, renderer: Renderer): Task[Option[Squad]] =
    if (hero.squad.pendingRisen.isEmpty) ZIO.none
    else save(user, hero.squad.dismissRisen, content.text("squad.risenDust"), renderer)

  // ── Резерв ──────────────────────────────────────────────────────────────────

  /** Союзник с места `at` — в резерв. Резерв полон — сказать об этом и ничего
    * не менять; крысу туда не берут — ни о чём. */
  def toReserve(user: User, hero: Hero, at: Int, renderer: Renderer): Task[Option[Squad]] =
    hero.squad.allyAt(at).filter(hero.squad.reservable) match {
      case None => ZIO.none
      case Some(_) if hero.squad.reserveFull =>
        renderer.show(user, Screen(content.format("squad.reserveFull", "max" -> AllyRates.ReserveSize.toString), Nil))
          .as(None)
      case Some(a) => save(user, hero.squad.toReserve(at), content.format("squad.toReserveDone", "name" -> a.name), renderer)
    }

  /** Из резерва — в строй: на первое свободное место, а мест нет — вместо
    * союзника с места `swapAt`, тот уходит в резерв. `i` — номер в резерве,
    * `name` — кто там должен стоять: список мог сдвинуться, пока игрок думал.
    * Мест нет и меняться не с кем — ни о чём: тогда спрашивают, кого отправить
    * в резерв взамен. */
  def deploy(user: User, hero: Hero, i: Int, name: String, swapAt: Option[Int],
             renderer: Renderer): Task[Option[Squad]] =
    hero.squad.reserve.lift(i).filter(_.name == name) match {
      case None => ZIO.none
      case Some(a) =>
        (hero.squad.freePosition, swapAt.flatMap(hero.squad.allyAt).filter(hero.squad.reservable)) match {
          case (Some(p), _) =>
            save(user, hero.squad.deploy(i), content.format("squad.deployed", "name" -> a.name, "n" -> p.toString), renderer)
          case (None, Some(b)) =>
            save(user, hero.squad.swapIn(i, b.position),
              content.format("squad.swappedIn", "name" -> a.name, "gone" -> b.name, "n" -> b.position.toString), renderer)
          case _ => ZIO.none
        }
    }

  /** Выгнать из резерва (`i` и `name` — как в [[deploy]]). */
  def dismissReserve(user: User, hero: Hero, i: Int, name: String, renderer: Renderer): Task[Option[Squad]] =
    hero.squad.reserve.lift(i).filter(_.name == name) match {
      case None    => ZIO.none
      case Some(a) => save(user, hero.squad.dismissReserve(i), content.format("squad.dismissed", "name" -> a.name), renderer)
    }

  private def save(user: User, squad: Squad, line: String, renderer: Renderer): Task[Option[Squad]] =
    heroDao.updateSquad(user.userId, squad) *> renderer.show(user, Screen(line, Nil)).as(Some(squad))
}
