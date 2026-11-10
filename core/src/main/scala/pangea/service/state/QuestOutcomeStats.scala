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

  /** Строка подсчёта: доля, чем закончили и тот ли это исход, что у героя. */
  final case class Row(pct: String, label: String, mine: Boolean)

  /** Подсчёт задания `questKey` строками — от большей доли к меньшей. `None`
    * — закончивших меньше [[MinHeroes]], процентам верить рано. Свой исход
    * `mine` герой видит в подсчёте, даже если его запись не дошла до базы;
    * исход неизвестен (герой закончил задание до подсчёта) — строки без
    * пометки. Ими пишет бот после развязки и показывает Mini App. */
  def rows(content: SceneContent, questKey: String, mine: Option[QuestOutcome],
           tally: Map[QuestOutcome, Long]): Option[List[Row]] = {
    val counts = mine.fold(tally)(m => if (tally.getOrElse(m, 0L) > 0L) tally else tally.updated(m, 1L))
    val total  = counts.values.sum
    Option.when(total >= MinHeroes)(
      QuestOutcome.of(questKey)
        .flatMap(o => counts.get(o).filter(_ > 0L).map(o -> _))
        .sortBy { case (o, n) => (-n, QuestOutcome.indexOf(o)) }
        .map { case (o, n) => Row(pct(content, n, total), label(content, o), mine.contains(o)) })
  }

  /** Как исход звучит в подсчёте: «спасли Марису». */
  def label(content: SceneContent, o: QuestOutcome): String =
    content.text(s"questStats.${o.questKey}.${o.entryName}")

  /** Текст по подсчёту `tally`; свой исход — `mine`. */
  def render(content: SceneContent, mine: QuestOutcome, tally: Map[QuestOutcome, Long]): String = {
    val title = content.text(mine.titleKey)
    rows(content, mine.questKey, Some(mine), tally) match {
      case None => content.format("questStats.first", "title" -> title)
      case Some(rs) =>
        val lines = rs.map(r => content.format(if (r.mine) "questStats.mine" else "questStats.line",
          "pct" -> r.pct, "label" -> r.label))
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
    ZIO.foreach(outcomes)(_.tally(mine.questKey).map(render(content, mine, _)))
      .catchAll(e => ZIO.logWarning(
        s"Статистика исходов не посчитана: ${mine.questKey}: ${e.getMessage}").as(None))
}
