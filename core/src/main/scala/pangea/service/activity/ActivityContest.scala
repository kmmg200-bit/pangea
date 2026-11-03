package pangea.service.activity

import pangea.dao.activity.{ActivityDao, TicketPrize, WeekTickets}
import pangea.dao.hero.HeroDao
import pangea.engine.{Players, SceneContent}
import pangea.model.hero.{AzatState, Hero}
import pangea.model.user.{User, VkId}
import pangea.repository.user.UserRepository
import zio.{Random, Task, ZIO, ZLayer}

/** Билеты за активность в группе ВК. Лайк свежего поста — золотой билет, по
  * одному на пост; комментарий под свежим постом — серебряный, по одному в
  * сутки. О каждом новом билете игроку сразу приходит письмо — шуточная
  * находка и счёт за неделю. В конце недели каждый золотой оборачивается
  * дублоном, каждый серебряный — сотней серебра, и об этом ещё одно письмо.
  *
  * Свежий пост — вышедший не раньше суток назад. Сутки и неделя — московские
  * (неделя с понедельника 00:00). Билеты идут только игрокам с героем. */
final case class ActivityContest(
  dao:      ActivityDao,
  userRepo: UserRepository,
  heroDao:  HeroDao,
  players:  Players,
  content:  SceneContent
) {
  import ActivityContest._

  /** Событие группы: пост запоминаем, за лайк и комментарий даём билет. */
  def handle(event: GroupEvent, now: Long): Task[Unit] = event match {
    case GroupEvent.PostPublished(post, at) => dao.recordPost(post, at)
    case GroupEvent.Liked(vk, post)         => ticket(vk, post, Kind.Gold, post, now)
    case GroupEvent.Commented(vk, post)     => ticket(vk, post, Kind.Silver, dayStart(now), now)
  }

  private def ticket(vk: Long, post: Long, kind: Kind, slot: Long, now: Long): Task[Unit] =
    dao.postedAt(post).flatMap {
      case Some(at) if now - at <= PostWindowMs =>
        player(VkId(vk.toString)).flatMap {
          case Some((user, _)) =>
            val week = weekStart(now)
            // Письмо — только о новом билете: повтор события или второй лайк
            // того же поста ничего не дают, и писать не о чем.
            dao.recordTicket(user.userId, kind.key, slot, week, now)
              .flatMap(fresh => ZIO.when(fresh)(notice(user, kind, week))).unit
          case None            => ZIO.unit
        }
      // Пост старый или мы его не видели (вышел до того, как бот стал их
      // запоминать) — билета за него нет.
      case _ => ZIO.unit
    }

  /** Письмо о новом билете: случайная находка и сколько билетов за неделю.
    * Закрытые сообщения игрока билету не мешают — письмо просто не дойдёт. */
  private def notice(user: User, kind: Kind, week: Long): Task[Unit] = {
    val lines = content.list("activity.noticeLines")
    for {
      t   <- dao.ticketsOf(user.userId, week)
      idx <- Random.nextIntBounded(lines.size.max(1))
      ticket = content.text(s"activity.ticket.${kind.key}")
      line   = lines.lift(idx).getOrElse("").replace("{ticket}", ticket)
      _   <- players.notify(user, content.format("activity.notice",
               "emoji" -> content.text(s"activity.emoji.${kind.key}"), "line" -> line,
               "reason" -> content.text(s"activity.reason.${kind.key}"),
               "gold" -> t.gold.toString, "silver" -> t.silver.toString,
               "perGold" -> DoubloonsPerTicket.toString, "perSilver" -> SilverPerTicket.toString)).ignore
    } yield ()
  }

  /** Игрок с героем по его странице ВК. */
  private def player(vk: VkId): Task[Option[(User, Hero)]] =
    userRepo.getUserByVkId(vk).flatMap {
      case Some(user) => heroDao.getHeroByUserId(user.userId).map(_.map(user -> _))
      case None       => ZIO.none
    }

  /** Оплатить билеты прошедшей недели, если её ещё не подводили: деньги — в
    * отложенные выплаты (дойдут с ближайшим действием, где бы игрок ни был),
    * каждому — одно письмо, в беседу — сколько роздано всего. Зовётся раз в
    * минуту: подведённая неделя повторно ничего не платит. */
  def settle(now: Long): Task[Unit] = {
    val week = weekStart(now) - WeekMs
    dao.awarded(week).flatMap {
      case true  => ZIO.unit
      case false =>
        for {
          tickets <- dao.tickets(week)
          found   <- ZIO.foreach(tickets)(t => byUserId(t).map(_.map { case (u, h) => (t, u, h) }))
          winners  = found.flatten
          prizes   = winners.map { case (t, _, hero) => TicketPrize(hero.id, t.silver * SilverPerTicket, t.gold * DoubloonsPerTicket) }
          paid    <- dao.award(week, prizes, now)
          _       <- ZIO.when(paid && winners.nonEmpty)(congratulate(winners.map { case (t, u, _) => t -> u }))
        } yield ()
    }
  }

  private def byUserId(t: WeekTickets): Task[Option[(User, Hero)]] =
    for {
      user <- userRepo.getUserById(t.userId).option.map(_.flatten)
      hero <- heroDao.getHeroByUserId(t.userId)
    } yield for { u <- user; h <- hero } yield (u, h)

  private def congratulate(winners: List[(WeekTickets, User)]): Task[Unit] = {
    val doubloons = winners.map(_._1.gold * DoubloonsPerTicket).sum
    val silver    = winners.map(_._1.silver * SilverPerTicket).sum
    for {
      _ <- ZIO.foreachDiscard(winners) { case (t, user) => players.notify(user, letter(t)).ignore }
      _ <- players.announce(content.format("activity.results",
             "doubloons" -> doubloons.toString, "silver" -> silver.toString)).ignore
    } yield ()
  }

  /** Письмо игроку: что принесли билеты каждого вида — сами билеты не считаем. */
  private def letter(t: WeekTickets): String = {
    val gold   = Option.when(t.gold > 0L)(content.format("activity.goldLine",
                   "doubloons" -> (t.gold * DoubloonsPerTicket).toString))
    val silver = Option.when(t.silver > 0L)(content.format("activity.silverLine",
                   "silver" -> (t.silver * SilverPerTicket).toString))
    content.format("activity.yourTickets", "lines" -> (gold.toList ++ silver).mkString("\n"))
  }
}

object ActivityContest {

  /** Вид билета. */
  sealed abstract class Kind(val key: String)
  object Kind {
    case object Gold   extends Kind("gold")
    case object Silver extends Kind("silver")
  }

  /** Что даёт билет в конце недели. */
  val DoubloonsPerTicket: Long = 1L
  val SilverPerTicket: Long    = 100L

  private val DayMs: Long = 24L * 60L * 60L * 1000L
  val WeekMs: Long        = 7L * DayMs

  /** Билеты идут за посты не старше этого. */
  val PostWindowMs: Long = DayMs

  /** Начало московских суток (мс) — серебряный билет даётся раз за них. */
  def dayStart(nowMs: Long): Long =
    Math.floorDiv(nowMs + AzatState.MoscowOffsetMs, DayMs) * DayMs - AzatState.MoscowOffsetMs

  /** Начало недели (мс): понедельник, 00:00 по Москве — как и прочие сутки игры
    * (см. [[AzatState.midnightsBetween]]). */
  def weekStart(nowMs: Long): Long = {
    val day    = Math.floorDiv(nowMs + AzatState.MoscowOffsetMs, DayMs)
    // 1 января 1970-го — четверг: понедельник той же недели на три дня раньше.
    val monday = day - Math.floorMod(day + 3L, 7L)
    monday * DayMs - AzatState.MoscowOffsetMs
  }

  val live: ZLayer[ActivityDao with UserRepository with HeroDao with Players with SceneContent, Nothing, ActivityContest] =
    ZLayer.fromFunction(ActivityContest(_, _, _, _, _))
}
