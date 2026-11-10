package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.engine.SceneContent
import pangea.model.hero.{Hero, LoreData}
import pangea.model.quest.{FirstSteps, NpcQuest, NpcQuests, QuestOutcome}
import zio.{Task, ZIO}

/** Пройденные задания героя — в Mini App, «Задания» → «Пройденные»: сюжет
  * («Встать на ноги» и главы основной линии) и задания горожан, которые герой
  * закончил. У заданий с развилкой — как их закончили другие герои, теми же
  * строками, что бот пишет после развязки ([[QuestOutcomeStats]]); свой
  * исход помечен. Только читает. */
object QuestsDone {

  /** Пройденное задание. `stats` — подсчёт исходов, от большей доли к меньшей;
    * пусто, если у задания нет развилки или закончивших пока мало. Тогда
    * `first` — «вы среди первых», если герой свой исход записал. */
  final case class Done(title: String, story: Boolean, first: Option[String], stats: List[QuestOutcomeStats.Row])

  /** Ключи пройденного: «Встать на ноги» — когда засчитано дома, задания
    * горожан и главы — с отметкой `done`. */
  def doneKeys(lore: LoreData, quests: NpcQuests): List[String] =
    Option.when(lore.firstStepsDone)(FirstSteps.Key).toList ++
      NpcQuest.values.toList.filter(quests.isDone).map(_.key)

  /** Что герой закончил, по порядку: «Встать на ноги» и главы основной линии,
    * потом задания горожан — как в журнале. `mine` — его исходы по ключу
    * задания, `tallies` — подсчёты по тем же ключам; нет подсчёта — нет и
    * процентов. */
  def build(content: SceneContent, lore: LoreData, quests: NpcQuests,
            mine: Map[String, QuestOutcome], tallies: Map[String, Map[QuestOutcome, Long]]): List[Done] = {
    def done(key: String, title: String, story: Boolean): Done = {
      val own = mine.get(key)
      tallies.get(key).filter(_ => QuestOutcome.of(key).nonEmpty)
        .map(t => QuestOutcomeStats.rows(content, key, own, t)) match {
        case Some(Some(rows)) => Done(title, story, None, rows)
        case Some(None)       => Done(title, story, own.map(_ => content.format("questStats.first", "title" -> title)), Nil)
        case None             => Done(title, story, None, Nil)
      }
    }
    val firstSteps = Option.when(lore.firstStepsDone)(
      done(FirstSteps.Key, content.text("firstSteps.title"), story = true))
    val chapters = StoryLine.Chapters.map(_.quest).filter(quests.isDone)
      .map(q => done(q.key, StoryLine.title(content, q), story = true))
    val npcs = NpcQuest.values.toList.filter(q => quests.isDone(q) && !StoryLine.isChapter(q))
      .map(q => done(q.key, content.text(s"journal.quests.${q.key}.title"), story = false))
    firstSteps.toList ++ chapters ++ npcs
  }

  /** Пройденные задания героя из базы. Подсчёт — только у пройденных заданий
    * с развилкой. База исходов не ответила — список всё равно, без процентов. */
  def load(heroDao: HeroDao, outcomes: QuestOutcomeDao, content: SceneContent, hero: Hero): Task[List[Done]] =
    for {
      lore   <- heroDao.readLoreData(hero.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))
      quests <- NpcQuestLog.load(heroDao, hero.userId)
      forked  = doneKeys(lore, quests).filter(k => QuestOutcome.of(k).nonEmpty)
      counted <- (for {
                   mine    <- outcomes.ofHero(hero.id)
                   tallies <- ZIO.foreach(forked)(k => outcomes.tally(k).map(k -> _))
                 } yield (mine, tallies.toMap))
                   .catchAll(e => ZIO.logWarning(s"Пройденные задания без процентов: герой ${hero.id.value}: ${e.getMessage}")
                     .as((Map.empty[String, QuestOutcome], Map.empty[String, Map[QuestOutcome, Long]])))
    } yield build(content, lore, quests, counted._1, counted._2)
}
