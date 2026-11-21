package pangea.test

import pangea.dao.admin.PrologueReminderDao
import pangea.dao.admin.PrologueReminderDao.{Last, Outcome}
import pangea.model.user.UserId
import zio.{Duration, Ref, Task, UIO, ZIO}

/** Учёт напоминаний о прологе в памяти: рассылки — как их завёл
  * `PrologueReminder`, отметки — как их поставила игра. `recent` — кому
  * «напоминали за неделю», `lastRun` — что отдать как прошлую рассылку;
  * `broken` — отметки падают, будто база недоступна. */
final class TestPrologueReminderDao(
    runsRef:  Ref[Vector[TestPrologueReminderDao.Run]],
    marksRef: Ref[Vector[(String, UserId)]],
    recent:   Set[UserId],
    lastRun:  Option[Last],
    broken:   Boolean
) extends PrologueReminderDao {
  import TestPrologueReminderDao._

  def remindedWithin(within: Duration): Task[Set[UserId]] = ZIO.succeed(recent)

  def open(admin: UserId, idle: Duration, message: String, recipients: List[UserId], skipped: Int): Task[Long] =
    runsRef.modify(rs => (rs.size + 1L, rs :+ Run(admin, idle, message, recipients, skipped, Map.empty, None)))

  def outcome(reminder: Long, user: UserId, outcome: Outcome): Task[Unit] =
    change(reminder)(r => r.copy(outcomes = r.outcomes.updated(user, outcome)))

  def close(reminder: Long, sent: Int, closed: Int, failed: Int): Task[Unit] =
    change(reminder)(_.copy(totals = Some((sent, closed, failed))))

  def last: Task[Option[Last]] = ZIO.succeed(lastRun)

  def returned(user: UserId): Task[Unit]  = mark(Returned, user)
  def choseRace(user: UserId): Task[Unit] = mark(Race, user)
  def completed(user: UserId): Task[Unit] = mark(Completed, user)

  /** Заведённые рассылки — по порядку. */
  def runs: UIO[Vector[Run]] = runsRef.get

  /** Отметки игры: какая и кому — по порядку. */
  def marks: UIO[Vector[(String, UserId)]] = marksRef.get

  private def change(reminder: Long)(f: Run => Run): Task[Unit] =
    runsRef.update(rs => rs.lift((reminder - 1L).toInt).fold(rs)(r => rs.updated((reminder - 1L).toInt, f(r))))

  private def mark(kind: String, user: UserId): Task[Unit] =
    if (broken) ZIO.fail(new RuntimeException("база недоступна")) else marksRef.update(_ :+ (kind -> user))
}

object TestPrologueReminderDao {

  /** Рассылка: кто запустил, порог, текст, адресаты, пропущено, исходы писем и
    * итоги (дошло, закрыли, сбой) — когда закрыта. */
  final case class Run(
      admin:      UserId,
      idle:       Duration,
      message:    String,
      recipients: List[UserId],
      skipped:    Int,
      outcomes:   Map[UserId, Outcome],
      totals:     Option[(Int, Int, Int)]
  )

  val Returned  = "returned"
  val Race      = "race"
  val Completed = "completed"

  def make(recent: Set[UserId] = Set.empty, last: Option[Last] = None, broken: Boolean = false): UIO[TestPrologueReminderDao] =
    for {
      runs  <- Ref.make(Vector.empty[Run])
      marks <- Ref.make(Vector.empty[(String, UserId)])
    } yield new TestPrologueReminderDao(runs, marks, recent, last, broken)

  /** Ничего не помнит и ничего не отмечает — где учёт напоминаний ни при чём. */
  val silent: PrologueReminderDao = new PrologueReminderDao {
    def remindedWithin(within: Duration): Task[Set[UserId]] = ZIO.succeed(Set.empty)
    def open(admin: UserId, idle: Duration, message: String, recipients: List[UserId], skipped: Int): Task[Long] = ZIO.succeed(1L)
    def outcome(reminder: Long, user: UserId, outcome: Outcome): Task[Unit] = ZIO.unit
    def close(reminder: Long, sent: Int, closed: Int, failed: Int): Task[Unit] = ZIO.unit
    def last: Task[Option[Last]] = ZIO.none
    def returned(user: UserId): Task[Unit]  = ZIO.unit
    def choseRace(user: UserId): Task[Unit] = ZIO.unit
    def completed(user: UserId): Task[Unit] = ZIO.unit
  }
}
