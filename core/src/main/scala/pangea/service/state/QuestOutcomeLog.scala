package pangea.service.state

import pangea.dao.quest.QuestOutcomeDao
import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.quest.QuestOutcome
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Запись исхода задания с развилкой ([[QuestOutcome]]) — в развязке, рядом с
  * закрытием задания. Запись — не часть задания: не удалась — задание всё
  * равно закрыто, только в подсчёт этот герой не попадёт; ошибка уходит в лог.
  * Без хранилища исходов (`None`, как в старых тестах сцен) — ничего. */
object QuestOutcomeLog {

  def record(outcomes: Option[QuestOutcomeDao], hero: Hero, outcome: QuestOutcome): Task[Unit] =
    ZIO.foreachDiscard(outcomes) { dao =>
      ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        .flatMap(now => dao.record(hero.id, outcome, now))
        .catchAll(e => ZIO.logWarning(
          s"Исход задания не записан: герой ${hero.id.value}, ${outcome.questKey}/${outcome.entryName}: ${e.getMessage}"))
    }

  /** Записать исход и сказать, как задание закончили другие (см.
    * [[QuestOutcomeStats]]): строки для игрока — сцена ставит их после «Квест …
    * завершён». Без хранилища исходов — ничего. */
  def recordAndCount(outcomes: Option[QuestOutcomeDao], content: SceneContent, hero: Hero,
                     outcome: QuestOutcome): Task[Option[String]] =
    record(outcomes, hero, outcome) *> QuestOutcomeStats.text(outcomes, content, outcome)

  /** Строка «Квест … завершён» и, если есть, статистика под ней — одним сообщением. */
  def withStats(line: String, stats: Option[String]): String = (line :: stats.toList).mkString("\n\n")
}
