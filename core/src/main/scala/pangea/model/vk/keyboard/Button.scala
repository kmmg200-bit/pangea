package pangea.model.vk.keyboard

import io.circe.{Encoder, Json}
import io.circe.syntax.EncoderOps
import pangea.model.vk.keyboard.ButtonColor.Primary

case class Button(action: Action, color: ButtonColor) {
  def withColor(color: ButtonColor): Button = this.copy(color = color)
}

object Button {
  def withAction(action: Action): Button = Button(action, Primary)

  /** Цвет ВК принимает только у кнопок text и callback — прочим его не шлём. */
  implicit val encoder: Encoder[Button] = (b: Button) =>
    b.action match {
      case _: Action.OpenApp => Json.obj("action" -> b.action.asJson)
      case _                 => Json.obj("action" -> b.action.asJson, "color" -> b.color.asJson)
    }
}
