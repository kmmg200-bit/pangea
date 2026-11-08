package pangea.dao.quest

import doobie.implicits._
import doobie.util.transactor
import doobie.util.transactor.Transactor
import pangea.model.hero.HeroId
import pangea.model.quest.{NpcQuest, QuestOutcome}
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Чем герои закончили задания с развилкой (`quest_outcomes`, см.
  * [[QuestOutcome]]): одна запись на героя и задание. */
trait QuestOutcomeDao {

  /** Запомнить, чем герой закончил задание. `at` — когда, мс. Повтор
    * перезаписывает: в подсчёте герой один. */
  def record(heroId: HeroId, outcome: QuestOutcome, at: Long): Task[Unit]

  /** Сколько героев закончили задание каждым исходом. Строки, которых
    * [[QuestOutcome]] не знает, не считаются. */
  def tally(quest: NpcQuest): Task[Map[QuestOutcome, Long]]
}

class QuestOutcomeDaoLive(xa: Transactor[Task]) extends QuestOutcomeDao {

  override def record(heroId: HeroId, outcome: QuestOutcome, at: Long): Task[Unit] =
    sql"""insert into quest_outcomes(hero_id, quest, outcome, at)
          values(${heroId.value}, ${outcome.quest.key}, ${outcome.entryName}, $at)
          on conflict (hero_id, quest) do update
          set outcome = excluded.outcome, at = excluded.at"""
      .update.run.transact(xa).unit

  override def tally(quest: NpcQuest): Task[Map[QuestOutcome, Long]] =
    sql"""select outcome, count(*) from quest_outcomes
          where quest = ${quest.key}
          group by outcome"""
      .query[(String, Long)].to[List].transact(xa)
      .map(_.flatMap { case (name, n) => QuestOutcome.find(quest, name).map(_ -> n) }.toMap)
}

object QuestOutcomeDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, QuestOutcomeDao] =
    ZLayer.fromFunction(new QuestOutcomeDaoLive(_))
}
