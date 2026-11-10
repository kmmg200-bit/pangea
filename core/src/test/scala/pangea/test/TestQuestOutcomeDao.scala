package pangea.test

import pangea.dao.quest.QuestOutcomeDao
import pangea.model.hero.HeroId
import pangea.model.quest.{NpcQuest, QuestOutcome}
import zio.{Ref, Task, UIO, ZIO}

/** Исходы заданий в памяти — как `quest_outcomes`: одна запись на героя и
  * задание, повтор перезаписывает. */
class TestQuestOutcomeDao(ref: Ref[Map[(HeroId, String), (QuestOutcome, Long)]]) extends QuestOutcomeDao {

  override def record(heroId: HeroId, outcome: QuestOutcome, at: Long): Task[Unit] =
    ref.update(_.updated(heroId -> outcome.questKey, outcome -> at))

  override def tally(questKey: String): Task[Map[QuestOutcome, Long]] =
    ref.get.map(_.values.map(_._1).filter(_.questKey == questKey).groupBy(identity).map { case (o, all) => o -> all.size.toLong })

  override def ofHero(heroId: HeroId): Task[Map[String, QuestOutcome]] =
    ref.get.map(_.collect { case ((id, quest), (o, _)) if id == heroId => quest -> o })

  /** Исход героя в задании, если записан. */
  def of(heroId: HeroId, questKey: String): UIO[Option[QuestOutcome]] = ref.get.map(_.get(heroId -> questKey).map(_._1))

  def of(heroId: HeroId, quest: NpcQuest): UIO[Option[QuestOutcome]] = of(heroId, quest.key)

  /** Все записанные исходы. */
  def all: UIO[List[QuestOutcome]] = ref.get.map(_.values.map(_._1).toList)

  /** Другие герои уже закончили задание: по `n` героев на исход (id с 1000 —
    * не пересекаются с героем теста). */
  def seed(counts: (QuestOutcome, Int)*): UIO[Unit] =
    ZIO.foreachDiscard(counts.toList.flatMap { case (o, n) => List.fill(n)(o) }.zipWithIndex) { case (o, i) =>
      ref.update(_.updated(HeroId(1000L + i) -> o.questKey, o -> 0L))
    }
}

object TestQuestOutcomeDao {
  def make: UIO[TestQuestOutcomeDao] =
    Ref.make(Map.empty[(HeroId, String), (QuestOutcome, Long)]).map(new TestQuestOutcomeDao(_))
}
