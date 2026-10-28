package pangea.service.admin

import pangea.dao.hero.HeroDao
import pangea.model.hero.HeroName
import pangea.model.user.{User, UserId}
import pangea.repository.user.UserRepository
import pangea.service.sender.Api
import zio.{Duration, Ref, Task, ZIO, ZLayer, durationInt}

/** Имена из ВК для героев, прошедших пролог до имён. Запускается из админ-панели
  * и идёт в фоне: по одному герою, с паузой между запросами к ВК, чтобы не
  * упереться в лимиты. Каждому ставится первое из [[NameBackfill.candidates]],
  * что проходит правила [[HeroName]] и ещё свободно. Не подошло ничего —
  * герой остаётся без имени и по-прежнему виден по имени из ВК. Повторный
  * запуск трогает только тех, кто так и остался без имени. */
final class NameBackfill(heroDao: HeroDao, userRepo: UserRepository, api: Api, running: Ref[Boolean], pause: Duration) {
  import NameBackfill._

  /** Начать в фоне; по окончании — сводка админу `admin`. `false` — уже идёт. */
  def start(admin: User): Task[Boolean] =
    running.getAndSet(true).flatMap {
      case true  => ZIO.succeed(false)
      case false =>
        run
          .foldCauseZIO(
            cause => ZIO.logErrorCause("Имена из ВК не проставлены", cause) *> notify(admin, Failed),
            report => notify(admin, summary(report)))
          .ensuring(running.set(false))
          .forkDaemon
          .as(true)
    }

  /** Пройти всех безымянных героев. */
  def run: Task[Report] =
    heroDao.unnamedUserIds.flatMap(ids =>
      ZIO.foldLeft(ids)(Report(0, 0, 0)) { (report, id) =>
        fill(id).map(report.add) <* ZIO.sleep(pause)
      })

  private def fill(id: UserId): Task[Outcome] =
    userRepo.getUserById(id).flatMap {
      case None => ZIO.succeed(Outcome.Skipped)
      case Some(user) =>
        api.getName(user).map(_.response.headOption).flatMap {
          case None     => ZIO.succeed(Outcome.Skipped)
          case Some(vk) => claimFirst(id, candidates(vk.firstName, vk.lastName))
        }
    }.catchAll(e => ZIO.logWarning(s"Имя из ВК для игрока ${id.value} не получено: ${e.getMessage}").as(Outcome.Failed))

  /** Первое свободное из кандидатов. Свободно ли — решает уникальный индекс. */
  private def claimFirst(id: UserId, names: List[String]): Task[Outcome] =
    names match {
      case Nil          => ZIO.succeed(Outcome.Skipped)
      case name :: rest =>
        heroDao.updateName(id, name).flatMap(ok => if (ok) ZIO.succeed(Outcome.Named) else claimFirst(id, rest))
    }

  private def notify(admin: User, text: String): Task[Unit] =
    api.sendMessage(admin, text, Nil, None).unit
}

object NameBackfill {

  /** Пауза между игроками: ВК пускает сообщество не чаще 20 запросов в
    * секунду, а бот в это время ещё и играет. */
  val Pause: Duration = 350.millis

  /** Что пробовать в имени героя по имени из ВК — по порядку: «Имя Фамилия»,
    * «Имя», «Имя Ф». Только то, что проходит правила [[HeroName]]. */
  def candidates(first: String, last: String): List[String] =
    List(s"$first $last", first, s"$first ${last.take(1)}")
      .flatMap(HeroName.validate(_).toOption)
      .distinct

  sealed trait Outcome
  object Outcome {
    case object Named   extends Outcome
    case object Skipped extends Outcome
    case object Failed  extends Outcome
  }

  /** Сколько героев получили имя, сколько осталось без него (ничего не
    * подошло), на скольких ВК не ответил. */
  final case class Report(named: Int, skipped: Int, failed: Int) {
    def add(o: Outcome): Report = o match {
      case Outcome.Named   => copy(named = named + 1)
      case Outcome.Skipped => copy(skipped = skipped + 1)
      case Outcome.Failed  => copy(failed = failed + 1)
    }
  }

  // Тексты — служебные, как и у панели (см. AdminPanel).
  val Started: String = "🏷 Проставляю имена из ВК тем, кто прошёл пролог до имён. Это в фоне: пришлю сводку, когда закончу."
  val Busy: String    = "🏷 Имена из ВК уже проставляются — дождитесь сводки."
  val Failed: String  = "🏷 Имена из ВК: прервалось, причина — в логе. Запустите ещё раз: уже названных не тронет."

  def summary(r: Report): String =
    s"""🏷 Имена из ВК проставлены.
       |  получили имя: ${r.named}
       |  без имени (ничего не подошло или занято): ${r.skipped}
       |  ВК не ответил: ${r.failed}""".stripMargin

  val live: ZLayer[HeroDao with UserRepository with Api, Nothing, NameBackfill] =
    ZLayer.fromZIO(
      for {
        heroDao  <- ZIO.service[HeroDao]
        userRepo <- ZIO.service[UserRepository]
        api      <- ZIO.service[Api]
        running  <- Ref.make(false)
      } yield new NameBackfill(heroDao, userRepo, api, running, Pause))
}
