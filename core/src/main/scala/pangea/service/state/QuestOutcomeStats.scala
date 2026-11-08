package pangea.service.state

import pangea.dao.quest.QuestOutcomeDao
import pangea.engine.SceneContent
import pangea.model.quest.QuestOutcome
import zio.{Task, ZIO}

/** Как задание с развилкой закончили другие герои — строки после «Квест …
  * завершён». Проценты — среди всех, кто задание закончил (герой уже среди
  * них), от большей доли к меньшей; своя строка — «как и вы». Пока
  * закончивших меньше [[MinHeroes]], проценты ничего не говорят — вместо них
  * «вы — среди первых». */
object QuestOutcomeStats {

  /** С какого числа закончивших показываем проценты. */
  val MinHeroes: Long = 10L

  /** Текст по подсчёту `tally`; свой исход — `mine`. Себя герой видит в
    * подсчёте, даже если его запись не дошла до базы. */
  def render(content: SceneContent, mine: QuestOutcome, tally: Map[QuestOutcome, Long]): String = {
    val counts = if (tally.getOrElse(mine, 0L) > 0L) tally else tally.updated(mine, 1L)
    val total  = counts.values.sum
    val title  = content.text(s"journal.quests.${mine.quest.key}.title")
    if (total < MinHeroes) content.format("questStats.first", "title" -> title)
    else {
      val rows = QuestOutcome.of(mine.quest)
        .flatMap(o => counts.get(o).filter(_ > 0L).map(o -> _))
        .sortBy { case (o, n) => (-n, QuestOutcome.indexOf(o)) }
      val lines = rows.map { case (o, n) =>
        content.format(if (o == mine) "questStats.mine" else "questStats.line",
          "pct" -> pct(content, n, total), "label" -> content.text(s"questStats.${o.quest.key}.${o.entryName}"))
      }
      (content.format("questStats.header", "title" -> title) :: lines).mkString("\n")
    }
  }

  /** Целый процент. Ненулевая доля меньше половины процента — «меньше 1%»;
    * пока исход выбрали не все — не больше 99%. */
  def pct(content: SceneContent, n: Long, total: Long): String = {
    val p = math.round(n * 100.0 / total)
    if (p < 1L) content.text("questStats.lessThanOne")
    else s"${if (n < total) p.min(99L) else p}%"
  }

  /** Подсчёт из базы — и текст. Без хранилища исходов или при ошибке базы —
    * ничего: статистика не должна мешать развязке. */
  def text(outcomes: Option[QuestOutcomeDao], content: SceneContent, mine: QuestOutcome): Task[Option[String]] =
    ZIO.foreach(outcomes)(_.tally(mine.quest).map(render(content, mine, _)))
      .catchAll(e => ZIO.logWarning(
        s"Статистика исходов не посчитана: ${mine.quest.key}: ${e.getMessage}").as(None))
}
