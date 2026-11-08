package pangea.dao.quest

import doobie.implicits._
import doobie.util.transactor
import doobie.util.transactor.Transactor
import pangea.model.hero.HeroId
import pangea.model.quest.QuestOutcome
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Чем герои закончили задания с развилкой (`quest_outcomes`, см.
  * [[QuestOutcome]]): одна запись на героя и задание. */
trait QuestOutcomeDao {

  /** Запомнить, чем герой закончил задание. `at` — когда, мс. Повтор
    * перезаписывает: в подсчёте герой один. */
  def record(heroId: HeroId, outcome: QuestOutcome, at: Long): Task[Unit]
}

class QuestOutcomeDaoLive(xa: Transactor[Task]) extends QuestOutcomeDao {

  override def record(heroId: HeroId, outcome: QuestOutcome, at: Long): Task[Unit] =
    sql"""insert into quest_outcomes(hero_id, quest, outcome, at)
          values(${heroId.value}, ${outcome.quest.key}, ${outcome.entryName}, $at)
          on conflict (hero_id, quest) do update
          set outcome = excluded.outcome, at = excluded.at"""
      .update.run.transact(xa).unit
}

object QuestOutcomeDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, QuestOutcomeDao] =
    ZLayer.fromFunction(new QuestOutcomeDaoLive(_))
}
