package pangea.test

import pangea.dao.quest.QuestOutcomeDao
import pangea.model.hero.HeroId
import pangea.model.quest.{NpcQuest, QuestOutcome}
import zio.{Ref, Task, UIO}

/** Исходы заданий в памяти — как `quest_outcomes`: одна запись на героя и
  * задание, повтор перезаписывает. */
class TestQuestOutcomeDao(ref: Ref[Map[(HeroId, NpcQuest), (QuestOutcome, Long)]]) extends QuestOutcomeDao {

  override def record(heroId: HeroId, outcome: QuestOutcome, at: Long): Task[Unit] =
    ref.update(_.updated(heroId -> outcome.quest, outcome -> at))

  /** Исход героя в задании, если записан. */
  def of(heroId: HeroId, quest: NpcQuest): UIO[Option[QuestOutcome]] = ref.get.map(_.get(heroId -> quest).map(_._1))

  /** Все записанные исходы. */
  def all: UIO[List[QuestOutcome]] = ref.get.map(_.values.map(_._1).toList)
}

object TestQuestOutcomeDao {
  def make: UIO[TestQuestOutcomeDao] =
    Ref.make(Map.empty[(HeroId, NpcQuest), (QuestOutcome, Long)]).map(new TestQuestOutcomeDao(_))
}
