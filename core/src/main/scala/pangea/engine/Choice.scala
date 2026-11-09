package pangea.engine

/** `row` — индекс ряда кнопок в клавиатуре. Кнопки с одинаковым `row` оказываются
 *  в одной строке клавиатуры; ряды отсортированы по возрастанию `row`. Сами кнопки
 *  внутри ряда — в порядке появления в списке. По умолчанию у всех кнопок `row = 0`,
 *  и каждая кнопка идёт в новой строке (см. VkRenderer для пояснений).
 *
 *  `app` — номер мини-приложения ВК: такая кнопка не шлёт боту действие, а
 *  открывает приложение прямо в ВК (см. [[pangea.service.state.AppButton]]). */
case class Choice(
  id:    String,
  label: String,
  data:  Map[String, String] = Map.empty,
  color: ChoiceColor          = ChoiceColor.Primary,
  row:   Option[Int]          = None,
  app:   Option[Long]         = None
)

object Choice {
  /** Самая длинная подпись, которую принимает клавиатура ВК; длиннее — ошибка
    * отправки и «зависший» экран у игрока. */
  val MaxLabelLength: Int = 40

  /** Сколько кнопок ВК пускает в ряд с кнопкой приложения (`app`). Больше —
    * ошибка 911 «of type open_app can contain only 2 button(s)», и сообщение
    * не уходит целиком. На dev.vk.com такой предел описан только для кнопки
    * location, но для open_app ВК его тоже проверяет. */
  val MaxAppRowButtons: Int = 2

  /** Подпись, укладывающаяся в лимит: лишнее срезается с многоточием. */
  def fit(label: String): String =
    if (label.length <= MaxLabelLength) label else label.take(MaxLabelLength - 1) + "…"
}
