package pangea.service.state

import pangea.engine.{Choice, SceneContent}

/** Кнопка «📱 Приложение»: открывает мини-приложение игры прямо в ВК
  * (vk.ru/app54870345). Боту нажатие ничего не шлёт — сцене её обрабатывать не
  * нужно. Стоит в «Персонаже», в городе и на всех экранах ожидания по таймеру:
  * пока герой занят, игрок может заглянуть в приложение. */
object AppButton {

  /** Номер мини-приложения игры во ВКонтакте. Не секрет — он есть в ссылке. */
  val AppId: Long = 54870345L

  /** Сама кнопка. `row` — ряд клавиатуры; без него кнопка встаёт в нижний ряд. */
  def choice(content: SceneContent, row: Option[Int] = None): Choice =
    Choice("OpenApp", content.text("common.openApp"), row = row, app = Some(AppId))

  /** Экран ожидания, где из кнопок только «Уйти»: кнопка приложения встаёт
    * слева, в один ряд с ней. Кнопок больше — они рядом ниже: в ряд с кнопкой
    * приложения ВК пускает не больше двух ([[Choice.MaxAppRowButtons]]). */
  def inRow(content: SceneContent, choices: List[Choice]): List[Choice] = {
    val row = if (choices.sizeIs < Choice.MaxAppRowButtons) 0 else 1
    choice(content, row = Some(0)) :: choices.map(_.copy(row = Some(row)))
  }

  /** Есть ли на экране кнопка приложения — для проверок. */
  def on(choices: List[Choice]): Boolean = choices.exists(_.app.contains(AppId))
}
