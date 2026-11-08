package pangea.dao.top

import cats.data.NonEmptyList
import doobie.Fragments
import doobie.implicits._
import doobie.util.fragment.Fragment
import doobie.util.transactor
import pangea.model.hero.HeroId
import pangea.model.state.StateType
import pangea.model.top.{TopBoard, TopKind, TopRow}
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Топы героев. В топ идут только те, кто прошёл регистрацию; админов в нём
  * нет — их герои часто тестовые. При равенстве выше тот, у кого выше уровень
  * и опыт, а дальше — кто раньше начал (меньший номер героя). */
trait TopDao {

  /** Снимок топа: весь порядок героев одним запросом — по нему место любого
    * героя — и строки первых `size`. */
  def board(kind: TopKind, size: Int): Task[TopBoard]
}

final class TopDaoLive(xa: transactor.Transactor[Task]) extends TopDao {
  import TopDaoLive._

  override def board(kind: TopKind, size: Int): Task[TopBoard] =
    (for {
      order   <- (fr"select h.id," ++ ranked ++ fr"," ++ valueColumn(kind) ++ fr"from heroes h order by" ++ orderBy(kind))
                   .query[(HeroId, Boolean, Long)].to[Vector]
      details <- NonEmptyList.fromList(TopBoard.firsts(order, size)) match {
                   case None      => doobie.free.connection.pure(List.empty[TopRow])
                   case Some(ids) =>
                     (fr"select h.id, h.user_id, u.vk_id, h.name, h.race, h.lvl," ++ valueColumn(kind) ++
                       fr"from heroes h join users u on u.id = h.user_id where" ++ Fragments.in(fr"h.id", ids.map(_.value)))
                       .query[TopRow].to[List]
                 }
    } yield TopBoard.build(order, details, size)).transact(xa)
}

object TopDaoLive {

  /** В топе ли герой: прошёл регистрацию и не админ. */
  private val ranked: Fragment =
    fr"(h.state <> ${StateType.Registration: StateType} and not exists" ++
      fr"(select 1 from admins a where a.user_id = h.user_id))"

  /** Чем топ меряет героя. Имена колонок — свои, снаружи не приходят. Из
    * достижений и книги рецептов берётся только число: что в них, топ не
    * читает. */
  private def valueColumn(kind: TopKind): Fragment = kind match {
    case TopKind.Level        => fr"h.lvl"
    case TopKind.Depth        => fr"h.max_dungeon_level"
    case TopKind.Kills        => fr"h.kills"
    // Имена через запятую; пустая строка — ни одного.
    case TopKind.Achievements => fr"coalesce(cardinality(string_to_array(nullif(h.achievements, ''), ',')), 0)"
    case TopKind.Alchemy      => jsonCount(fr"h.lore_data -> 'brewed'")
    case TopKind.Recipes      => jsonCount(fr"h.home_data -> 'recipes'")
  }

  /** Сколько в JSON-списке записей; нет списка (старая запись, дома нет) — ноль. */
  private def jsonCount(list: Fragment): Fragment =
    fr"(case when jsonb_typeof(" ++ list ++ fr") = 'array' then jsonb_array_length(" ++ list ++ fr") else 0 end)"

  /** Порядок топа: по мерке, затем уровень и опыт, затем кто раньше начал. */
  private def orderBy(kind: TopKind): Fragment = kind match {
    case TopKind.Level => fr"h.lvl desc, h.exp desc, h.id asc"
    case other         => valueColumn(other) ++ fr" desc, h.lvl desc, h.exp desc, h.id asc"
  }
}

object TopDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, TopDao] =
    ZLayer.fromFunction(new TopDaoLive(_))
}
