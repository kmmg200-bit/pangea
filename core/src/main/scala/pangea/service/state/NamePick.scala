package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.hero.HeroName
import pangea.model.user.User
import zio.{Task, ZIO}

/** Выбор имени героя — общий для конца пролога и купона Фета: правила
  * [[HeroName]], занятость и тексты отказа (`heroName.*` в scenes.yaml).
  * Сцены сами решают, как спрашивать и куда идти дальше; имя на подтверждении
  * они держат в своём `scene_data` под [[NamePick.PendingKey]]. */
final case class NamePick(heroDao: HeroDao, content: SceneContent) {

  /** Имя набрано текстом, а не кнопкой: только такое и разбираем. Подпись
    * старой кнопки («Дальше») — не имя. */
  def typed(ua: UserAction): Option[String] =
    Option.when(ua.payload.isEmpty && ua.text.trim.nonEmpty)(ua.text)

  /** Ввод игрока → имя на подтверждение или текст отказа. Своё же имя — не
    * занято: его можно взять снова. */
  def check(user: User, raw: String): Task[Either[String, String]] =
    HeroName.validate(raw) match {
      case Left(problem) => ZIO.succeed(Left(refusal(problem)))
      case Right(name)   => heroDao.nameTaken(user.userId, name).map(taken => if (taken) Left(takenText(name)) else Right(name))
    }

  /** Записать имя. `Left` — пока игрок подтверждал, его занял другой. */
  def claim(user: User, name: String): Task[Either[String, Unit]] =
    heroDao.updateName(user.userId, name).map(ok => if (ok) Right(()) else Left(takenText(name)))

  private def takenText(name: String): String = content.format("heroName.taken", "name" -> name)

  private def refusal(problem: HeroName.Problem): String = problem match {
    case HeroName.Problem.Length =>
      content.format("heroName.length", "min" -> HeroName.MinLength.toString, "max" -> HeroName.MaxLength.toString)
    case HeroName.Problem.Letters  => content.text("heroName.letters")
    case HeroName.Problem.Reserved => content.text("heroName.reserved")
  }
}

object NamePick {
  /** Ключ в `scene_data`: имя, которое игрок ввёл и ещё не подтвердил. */
  val PendingKey: String = "pendingName"

  /** Ключ в `scene_data`: сцена ждёт имя текстом. */
  val AwaitKey: String = "awaitingName"
}
