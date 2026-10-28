package pangea.util

/** Сколько осталось — коротко и одинаково везде: «6д 23ч», «5ч 20мин», «40мин».
  * Сутки и больше — дни с часами, меньше суток — часы с минутами. Меньше минуты
  * не пишем: «1мин», вот-вот. */
object TimeLeft {

  def format(ms: Long): String = {
    val mins = (ms / 60000L).max(1L)
    val d    = mins / 1440L
    val h    = mins % 1440L / 60L
    val m    = mins % 60L
    if (d > 0) s"${d}д ${h}ч" else if (h > 0) s"${h}ч ${m}мин" else s"${m}мин"
  }
}
