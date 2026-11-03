package pangea.test

import pangea.dao.activity.{ActivityDao, TicketPrize, WeekTickets}
import pangea.model.user.UserId
import zio.{Task, ZIO}

/** Билеты за активность в памяти. Выплаты за них ложатся в тот же склад
  * отложенных выплат, что и в проде (`pending_payouts`), — здесь это
  * [[TestPayoutDao]]. */
class TestActivityDao(payouts: TestPayoutDao) extends ActivityDao {
  private var posts: Map[Long, Long]                       = Map.empty
  private var tickets_ : Map[(UserId, String, Long), Long] = Map.empty  // → week
  private var awardedWeeks: Set[Long]                      = Set.empty

  def recordPost(postId: Long, postedAt: Long): Task[Unit] =
    ZIO.succeed { if (!posts.contains(postId)) posts = posts.updated(postId, postedAt) }

  def postedAt(postId: Long): Task[Option[Long]] = ZIO.succeed(posts.get(postId))

  def recordTicket(userId: UserId, kind: String, slot: Long, week: Long, at: Long): Task[Boolean] =
    ZIO.succeed {
      val key = (userId, kind, slot)
      if (tickets_.contains(key)) false
      else { tickets_ = tickets_.updated(key, week); true }
    }

  def tickets(week: Long): Task[List[WeekTickets]] =
    ZIO.succeed {
      tickets_.toList
        .collect { case ((user, kind, _), w) if w == week => user -> kind }
        .groupBy(_._1).toList
        .map { case (user, rows) =>
          WeekTickets(user, rows.count(_._2 == "gold").toLong, rows.count(_._2 == "silver").toLong)
        }
        .sortBy(_.userId.value)
    }

  def ticketsOf(userId: UserId, week: Long): Task[WeekTickets] =
    tickets(week).map(_.find(_.userId == userId).getOrElse(WeekTickets(userId, 0L, 0L)))

  def awarded(week: Long): Task[Boolean] = ZIO.succeed(awardedWeeks.contains(week))

  def award(week: Long, prizes: List[TicketPrize], now: Long): Task[Boolean] =
    if (awardedWeeks.contains(week)) ZIO.succeed(false)
    else
      ZIO.succeed { awardedWeeks += week } *>
        ZIO.foreachDiscard(prizes)(p => payouts.add(p.hero, p.silver, p.doubloons)).as(true)

  def awardedSnapshot: Set[Long] = awardedWeeks
}
