package pangea.dao.activity

import cats.syntax.foldable._
import cats.syntax.functor._
import doobie.free.{connection => FC}
import doobie.implicits._
import doobie.util.transactor
import doobie.util.transactor.Transactor
import pangea.model.hero.HeroId
import pangea.model.user.UserId
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Билеты игрока за неделю: золотые и серебряные. */
final case class WeekTickets(userId: UserId, gold: Long, silver: Long)

/** Награда за неделю одному герою — в отложенные выплаты. */
final case class TicketPrize(hero: HeroId, silver: Long, doubloons: Long)

/** Билеты за активность в группе: посты группы, выданные билеты, подведённые
  * недели. */
trait ActivityDao {
  /** Запомнить вышедший пост. Повтор события ничего не портит. */
  def recordPost(postId: Long, postedAt: Long): Task[Unit]

  /** Когда вышел пост; `None` — такого поста мы не видели. */
  def postedAt(postId: Long): Task[Option[Long]]

  /** Выдать билет. `slot` — за что он: пост для золотого, сутки для
    * серебряного. `false` — за этот `slot` билет такого вида уже выдан. */
  def recordTicket(userId: UserId, kind: String, slot: Long, week: Long, at: Long): Task[Boolean]

  /** Билеты недели по игрокам. */
  def tickets(week: Long): Task[List[WeekTickets]]

  /** Билеты недели одного игрока — для письма о новом билете. */
  def ticketsOf(userId: UserId, week: Long): Task[WeekTickets]

  /** Подведена ли уже неделя. */
  def awarded(week: Long): Task[Boolean]

  /** Оплатить неделю одним заходом: отметка «подведена» и деньги за билеты
    * (в отложенные выплаты, см. `Payouts`) — в одной транзакции. `false` —
    * неделю уже подвели, и никто ничего не получил второй раз. */
  def award(week: Long, prizes: List[TicketPrize], now: Long): Task[Boolean]
}

class ActivityDaoLive(xa: Transactor[Task]) extends ActivityDao {

  override def recordPost(postId: Long, postedAt: Long): Task[Unit] =
    sql"""insert into group_posts(post_id, posted_at) values($postId, $postedAt)
          on conflict (post_id) do nothing"""
      .update.run.transact(xa).unit

  override def postedAt(postId: Long): Task[Option[Long]] =
    sql"select posted_at from group_posts where post_id = $postId"
      .query[Long].option.transact(xa)

  override def recordTicket(userId: UserId, kind: String, slot: Long, week: Long, at: Long): Task[Boolean] =
    sql"""insert into activity_tickets(user_id, kind, slot, week, at)
          values(${userId.value}, $kind, $slot, $week, $at)
          on conflict (user_id, kind, slot) do nothing"""
      .update.run.transact(xa).map(_ == 1)

  override def tickets(week: Long): Task[List[WeekTickets]] =
    sql"""select user_id,
                 count(*) filter (where kind = 'gold'),
                 count(*) filter (where kind = 'silver')
          from activity_tickets
          where week = $week
          group by user_id
          order by user_id"""
      .query[(Long, Long, Long)].to[List].transact(xa)
      .map(_.map { case (user, gold, silver) => WeekTickets(UserId(user), gold, silver) })

  override def ticketsOf(userId: UserId, week: Long): Task[WeekTickets] =
    sql"""select count(*) filter (where kind = 'gold'),
                 count(*) filter (where kind = 'silver')
          from activity_tickets
          where user_id = ${userId.value} and week = $week"""
      .query[(Long, Long)].unique.transact(xa)
      .map { case (gold, silver) => WeekTickets(userId, gold, silver) }

  override def awarded(week: Long): Task[Boolean] =
    sql"select count(*) from activity_prizes where week = $week"
      .query[Long].unique.transact(xa).map(_ > 0L)

  override def award(week: Long, prizes: List[TicketPrize], now: Long): Task[Boolean] = {
    val mark =
      sql"""insert into activity_prizes(week, paid_at) values($week, $now)
            on conflict (week) do nothing""".update.run

    def pay(prize: TicketPrize) =
      sql"""insert into pending_payouts(hero_id, silver, doubloons)
            values(${prize.hero.value}, ${prize.silver}, ${prize.doubloons})
            on conflict (hero_id) do update
            set silver    = pending_payouts.silver + ${prize.silver},
                doubloons = pending_payouts.doubloons + ${prize.doubloons}""".update.run

    mark.flatMap {
      case 0 => FC.pure(false)
      case _ => prizes.traverse_(pay).as(true)
    }.transact(xa)
  }
}

object ActivityDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, ActivityDao] =
    ZLayer.fromFunction(new ActivityDaoLive(_))
}
