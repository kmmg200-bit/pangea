package pangea.model.vk.keyboard

import io.circe.{Encoder, Json}
import io.circe.syntax.EncoderOps

sealed trait Action {
  def `type`: String
  def label: String
  def payload: Option[Json]
}

object Action {
  val payloadEmpty: Json = Json.Null

  case class Text(label_ : String, payload_ : Option[Json]) extends Action {
    override def `type`: String        = "text"
    override def label: String         = label_
    override def payload: Option[Json] = payload_
  }

  case class Callback(label_ : String, payload_ : Json) extends Action {
    override def `type`: String        = "callback"
    override def label: String         = label_
    override def payload: Option[Json] = Some(payload_)
  }

  /** Открывает мини-приложение ВК по его номеру — прямо в ВК, без браузера.
    * Боту нажатие ничего не шлёт, поэтому и payload ей не нужен. Цвета у такой
    * кнопки нет: ВК задаёт цвет только кнопкам text и callback (см. [[Button]]). */
  case class OpenApp(label_ : String, appId: Long) extends Action {
    override def `type`: String        = "open_app"
    override def label: String         = label_
    override def payload: Option[Json] = None
  }

  implicit val encoder: Encoder[Action] =
    (action: Action) => {
      val base = Json.obj("type" -> action.`type`.asJson, "label" -> action.label.asJson)
      // Номер приложения ВК ждёт числом, а payload — строкой с JSON внутри.
      val extra = action match {
        case OpenApp(_, appId) => Json.obj("app_id" -> appId.asJson)
        case other             => other.payload.fold(Json.obj())(p => Json.obj("payload" -> p.toString.asJson))
      }
      base.deepMerge(extra)
    }
}
