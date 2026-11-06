package pangea.test

import pangea.engine.Journal
import pangea.model.GameEvent
import pangea.model.user.UserId
import zio.{Clock, Ref, Task}

import java.util.concurrent.TimeUnit

/** Журнал в памяти: событие и когда оно записано (по часам теста). */
class TestJournal(ref: Ref[List[(GameEvent, Long)]]) extends Journal {
  override def append(event: GameEvent): Task[Unit] =
    Clock.currentTime(TimeUnit.MILLISECONDS).flatMap(at => ref.update(_ :+ (event -> at)))

  override def count(userId: UserId, eventType: String, sinceMs: Long): Task[Long] =
    ref.get.map(_.count { case (e, at) => e.userId == userId && e.eventType == eventType && at >= sinceMs }.toLong)

  def logged: Task[List[GameEvent]] = ref.get.map(_.map(_._1))
}

object TestJournal {
  def make: Task[TestJournal] = Ref.make(List.empty[(GameEvent, Long)]).map(new TestJournal(_))
}
