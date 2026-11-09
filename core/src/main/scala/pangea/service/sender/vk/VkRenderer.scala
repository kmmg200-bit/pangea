package pangea.service.sender.vk

import io.circe.syntax.EncoderOps
import pangea.engine.{Choice, ChoiceColor, Renderer, Screen}
import pangea.model.user.User
import pangea.model.vk.Attachment
import pangea.model.vk.keyboard.{Action => VkAction, Button, ButtonColor, Keyboard}
import pangea.service.sender.Api
import zio.Task

class VkRenderer(api: Api) extends Renderer {
  def show(user: User, screen: Screen): Task[Unit] = {
    val kbOpt =
      if (screen.choices.isEmpty)
        // Без кнопок: либо явно скрываем прошлую клавиатуру (пустая bot-клавиатура),
        // либо вообще не отправляем `keyboard` — тогда ВК сохраняет прежнюю.
        if (screen.hideKeyboard) Some(Keyboard.empty) else None
      else {
        // Если хотя бы у одной кнопки задан `row` — рендерим ряды: choices группируются
        // по row, ряды отсортированы по возрастанию. Кнопки без row идут каждая в свою
        // строку (старое поведение — сохранено для совместимости).
        val hasRows = screen.choices.exists(_.row.isDefined)
        val kbInit  = Keyboard.empty.withInline(screen.inline)
        val rows =
          if (!hasRows) screen.choices.map(List(_))
          else
            screen.choices.zipWithIndex
              .groupBy { case (c, _) => c.row.getOrElse(Int.MaxValue) }
              .toList
              .sortBy(_._1)
              .map { case (_, items) => items.sortBy(_._2).map(_._1) }
        // Страховка общая для обеих раскладок: столбик из одиннадцати кнопок ВК
        // отклоняет так же, как и одиннадцать рядов.
        val kb = VkRenderer.fit(rows).foldLeft(kbInit) { (acc, rowChoices) =>
          rowChoices.foldLeft(acc.addRow()) { (k, choice) => k.addButton(toButton(choice)) }
        }
        Some(kb)
      }
    // Картинка — уже загруженное фото из альбома группы: ВК прикрепляет его по ссылке.
    api.sendMessage(user, screen.text, screen.image.flatMap(Attachment.parse).toList, kbOpt)
  }

  private def toButton(choice: Choice): Button =
    choice.app match {
      // Кнопка приложения открывает его прямо в ВК, боту она ничего не шлёт.
      case Some(appId) => Button.withAction(VkAction.OpenApp(Choice.fit(choice.label), appId))
      case None =>
        val payload = (Map("action" -> choice.id) ++ choice.data).asJson
        // Подпись длиннее лимита ВК роняет отправку — режем здесь, чтобы ни одна
        // сцена не могла уронить экран.
        Button.withAction(VkAction.Text(Choice.fit(choice.label), Some(payload)))
          .withColor(VkRenderer.toButtonColor(choice.color))
    }
}

object VkRenderer {
  def apply(api: Api): VkRenderer = new VkRenderer(api)

  /** Сколько рядов и кнопок в ряду принимает клавиатура ВК. Сообщение с более
    * чем [[MaxRows]] рядами ВК отклоняет целиком, и игрок вместо экрана видит
    * «Произошла ошибка». */
  val MaxRows: Int           = 10
  val MaxButtonsPerRow: Int  = 5

  /** Ужимает раскладку под лимит ВК: если рядов больше, чем принимает
    * клавиатура, кнопки перекладываются плотно — по пять в ряд. Это страховка:
    * сцены и так считают ряды сами, но одна забытая кнопка не должна ронять
    * экран целиком. Совсем не влезшее (свыше пятидесяти кнопок) отсекается —
    * лучше показать экран без хвоста, чем не показать ничего.
    *
    * Кнопку приложения, у которой в ряду больше [[Choice.MaxAppRowButtons]]
    * кнопок, выносим отдельным рядом сразу под ним: такой ряд ВК отклоняет
    * вместе со всем сообщением. */
  def fit(rows: List[List[Choice]]): List[List[Choice]] = {
    val split = appRows(rows)
    if (split.sizeIs <= MaxRows) split
    else {
      // Плотно, по пять в ряд, — а кнопка приложения отдельным рядом в конце.
      val (apps, rest) = rows.flatten.partition(_.app.isDefined)
      (rest.grouped(MaxButtonsPerRow).toList ++ apps.map(List(_))).take(MaxRows)
    }
  }

  /** Ряд, где рядом с кнопкой приложения больше [[Choice.MaxAppRowButtons]]
    * кнопок, — без неё, а она сама отдельным рядом следом. */
  private def appRows(rows: List[List[Choice]]): List[List[Choice]] =
    rows.flatMap { row =>
      if (row.sizeIs <= Choice.MaxAppRowButtons || !row.exists(_.app.isDefined)) List(row)
      else {
        val (apps, rest) = row.partition(_.app.isDefined)
        (rest :: apps.map(List(_))).filter(_.nonEmpty)
      }
    }

  def toButtonColor(color: ChoiceColor): ButtonColor = color match {
    case ChoiceColor.Negative  => ButtonColor.Negative
    case ChoiceColor.Positive  => ButtonColor.Positive
    case ChoiceColor.Secondary => ButtonColor.Secondary
    case ChoiceColor.Primary   => ButtonColor.Primary
  }
}
