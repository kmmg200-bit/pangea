package pangea.service.chat

import pangea.model.vk.keyboard.{Action, Button, Keyboard}
import pangea.service.state.AppButton

/** Кнопки под полем ввода в общей и торговой беседе: Mini App и статья-туториал.
  * Уходят вместе с каждым сообщением бота в эти две беседы (см.
  * `VkApi.sendToChat`): ВК держит клавиатуру беседы, пока её не сменят. В ряду с
  * кнопкой приложения ВК пускает не больше двух кнопок — их и две. */
object ChatButtons {

  /** Та же подпись, что у кнопки приложения в игре (`common.openApp`). */
  val AppLabel: String = "📱 Приложение"

  val TutorialLabel: String = "📖 Туториал"

  val keyboard: Keyboard = Keyboard(
    List(List(
      Button.withAction(Action.OpenApp(AppLabel, AppButton.AppId)),
      Button.withAction(Action.OpenLink(TutorialLabel, Links.Tutorial)))),
    one_time = false,
    inline = false)
}
