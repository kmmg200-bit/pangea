package pangea.engine

import zio.test._

/** Заметка над экраном: «что только что случилось» и сам экран — одним
  * сообщением, а не двумя подряд. */
object ScreenNoteSpec extends ZIOSpecDefault {

  private val floor = Screen("Этаж 3", List(Choice("FindEvent", "Искать")), image = Some("photo-1_2"))

  override def spec = suite("Заметка над экраном")(

    test("заметка встаёт над текстом экрана, кнопки и картинка — прежние") {
      val noted = floor.noted("Выше уже некуда.")
      assertTrue(noted.text == "Выше уже некуда.\n\nЭтаж 3") &&
      assertTrue(noted.choices == floor.choices && noted.image == floor.image)
    },

    test("пустая заметка экран не трогает; у пустого экрана заметка — весь текст") {
      assertTrue(floor.noted("") == floor) &&
      assertTrue(Screen("", Nil).noted("Пусто").text == "Пусто")
    },

    test("несколько заметок — абзацами, пустые пропускаются") {
      assertTrue(Screen.joined("Победа.", "", "Объявление закрыто.") == "Победа.\n\nОбъявление закрыто.") &&
      assertTrue(Screen.joined("", "") == "")
    }
  )
}
