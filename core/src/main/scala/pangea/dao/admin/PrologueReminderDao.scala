package pangea.dao.admin

import doobie.Update
import doobie.implicits._
import doobie.util.transactor
import pangea.model.user.UserId
import zio.interop.catz._
import zio.{Duration, Task, ZLayer}

/** Учёт напоминаний о прологе (см. [[pangea.service.admin.PrologueReminder]]):
  * рассылки, их адресаты и что адресат сделал после — вернулся, выбрал расу,
  * прошёл пролог. Таблицы — `prologue_reminders` и `prologue_reminder_recipients`. */
trait PrologueReminderDao {
  import PrologueReminderDao._

  /** Кому напоминали за последние `within`: рассылка их пропускает. */
  def remindedWithin(within: Duration): Task[Set[UserId]]

  /** Завести рассылку: кто запустил, порог простоя, текст, адресаты (письма
    * ещё в пути) и сколько пропущено. Ответ — номер рассылки. */
  def open(admin: UserId, idle: Duration, message: String, recipients: List[UserId], skipped: Int): Task[Long]

  /** Чем кончилось письмо адресату. */
  def outcome(reminder: Long, user: UserId, outcome: Outcome): Task[Unit]

  /** Рассылка кончилась: итоги. */
  def close(reminder: Long, sent: Int, closed: Int, failed: Int): Task[Unit]

  /** Последняя рассылка и что было после неё. `None` — рассылок не было. */
  def last: Task[Option[Last]]

  // Отметки ставит игра — последней рассылке игрока, если она не старше
  // [[Attribution]]; каждую один раз, первым разом.

  /** Игрок что-то нажал в прологе. */
  def returned(user: UserId): Task[Unit]

  /** Игрок выбрал расу. */
  def choseRace(user: UserId): Task[Unit]

  /** Игрок прошёл пролог — вышел из сторожки. */
  def completed(user: UserId): Task[Unit]
}

object PrologueReminderDao {

  /** Сколько после рассылки возвращение игрока засчитывается ей. */
  val Attribution: Duration = Duration.fromSeconds(7L * 24 * 3600)

  /** Чем кончилось письмо: строка — то, что лежит в базе (`outcome`). */
  sealed abstract class Outcome(val db: String)
  object Outcome {
    case object Sent   extends Outcome("sent")
    /** Игрок закрыл сообщения от сообщества (900–902). */
    case object Closed extends Outcome("closed")
    case object Failed extends Outcome("failed")
  }

  /** Последняя рассылка: когда начата (мс эпохи), скольким ушла, скольким
    * дошло, сколько закрыли сообщения, и сколько после неё вернулись, выбрали
    * расу и прошли пролог. */
  final case class Last(startedAt: Long, recipients: Int, sent: Int, closed: Int, returned: Int, race: Int, completed: Int)

  val live: ZLayer[transactor.Transactor[Task], Nothing, PrologueReminderDao] =
    ZLayer.fromFunction(new PrologueReminderDaoLive(_))
}

class PrologueReminderDaoLive(xa: transactor.Transactor[Task]) extends PrologueReminderDao {
  import PrologueReminderDao._

  override def remindedWithin(within: Duration): Task[Set[UserId]] =
    sql"""select distinct p.user_id from prologue_reminder_recipients p
          join prologue_reminders r on r.id = p.reminder_id
          where r.started_at > now() - ${within.toSeconds} * interval '1 second'"""
      .query[UserId].to[List].transact(xa).map(_.toSet)

  override def open(admin: UserId, idle: Duration, message: String, recipients: List[UserId], skipped: Int): Task[Long] =
    (for {
      id <- sql"""insert into prologue_reminders (admin_user_id, idle_minutes, message, skipped)
                  values (${admin.value}, ${idle.toMinutes.toInt}, $message, $skipped)"""
              .update.withUniqueGeneratedKeys[Long]("id")
      _  <- Update[(Long, Long)]("insert into prologue_reminder_recipients (reminder_id, user_id) values (?, ?)")
              .updateMany(recipients.map(u => (id, u.value)))
    } yield id).transact(xa)

  override def outcome(reminder: Long, user: UserId, outcome: Outcome): Task[Unit] =
    sql"""update prologue_reminder_recipients set outcome = ${outcome.db}
          where reminder_id = $reminder and user_id = ${user.value}"""
      .update.run.transact(xa).unit

  override def close(reminder: Long, sent: Int, closed: Int, failed: Int): Task[Unit] =
    sql"""update prologue_reminders set finished_at = now(), sent = $sent, closed = $closed, failed = $failed
          where id = $reminder"""
      .update.run.transact(xa).unit

  override def last: Task[Option[Last]] =
    sql"""select (extract(epoch from r.started_at) * 1000)::bigint,
                 count(p.user_id)::int,
                 (count(*) filter (where p.outcome = ${Outcome.Sent.db}))::int,
                 (count(*) filter (where p.outcome = ${Outcome.Closed.db}))::int,
                 count(p.returned_at)::int, count(p.race_at)::int, count(p.completed_at)::int
          from prologue_reminders r
          left join prologue_reminder_recipients p on p.reminder_id = r.id
          where r.id = (select max(id) from prologue_reminders)
          group by r.id, r.started_at"""
      .query[Last].option.transact(xa)

  override def returned(user: UserId): Task[Unit]  = mark(user, "returned_at")
  override def choseRace(user: UserId): Task[Unit] = mark(user, "race_at")
  override def completed(user: UserId): Task[Unit] = mark(user, "completed_at")

  /** Отметить `column` в последней рассылке игрока, если её ещё нет и рассылка
    * не старше [[Attribution]]. Колонка — одна из трёх наших, не снаружи. */
  private def mark(user: UserId, column: String): Task[Unit] = {
    val col = doobie.Fragment.const(column)
    (fr"update prologue_reminder_recipients p set" ++ col ++ fr"= now()" ++
      fr"where p.user_id = ${user.value} and p." ++ col ++ fr"is null" ++
      fr"""and p.reminder_id = (select max(q.reminder_id) from prologue_reminder_recipients q where q.user_id = ${user.value})
           and exists (select 1 from prologue_reminders r
                       where r.id = p.reminder_id and r.started_at > now() - ${Attribution.toSeconds} * interval '1 second')""")
      .update.run.transact(xa).unit
  }
}
