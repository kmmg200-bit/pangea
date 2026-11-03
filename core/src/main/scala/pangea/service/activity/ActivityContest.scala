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
  * двумя дублонами, каждый серебряный — серебром от 100 до 300, и об этом ещё
  * одно письмо.
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
               "gold" -> t.gold.toString, "silver" -> t.silver.toString)).ignore
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
          winners <- ZIO.foreach(found.flatten) { case (t, user, hero) =>
                       silverFor(t.silver).map(silver => TicketPrize(hero.id, silver, t.gold * DoubloonsPerTicket) -> user)
                     }
          paid    <- dao.award(week, winners.map(_._1), now)
          _       <- ZIO.when(paid && winners.nonEmpty)(congratulate(winners))
        } yield ()
    }
  }

  /** Серебро за `count` серебряных билетов: каждый бросается отдельно. */
  private def silverFor(count: Long): Task[Long] =
    ZIO.foldLeft(1L to count)(0L)((sum, _) => Random.nextLongBetween(SilverMin, SilverMax + 1L).map(sum + _))

  private def byUserId(t: WeekTickets): Task[Option[(User, Hero)]] =
    for {
      user <- userRepo.getUserById(t.userId).option.map(_.flatten)
      hero <- heroDao.getHeroByUserId(t.userId)
    } yield for { u <- user; h <- hero } yield (u, h)

  private def congratulate(winners: List[(TicketPrize, User)]): Task[Unit] = {
    val doubloons = winners.map(_._1.doubloons).sum
    val silver    = winners.map(_._1.silver).sum
    for {
      _ <- ZIO.foreachDiscard(winners) { case (prize, user) => players.notify(user, letter(prize)).ignore }
      _ <- players.announce(content.format("activity.results",
             "doubloons" -> doubloons.toString, "silver" -> silver.toString)).ignore
    } yield ()
  }

  /** Письмо игроку: что принесли билеты каждого вида — сами билеты не считаем. */
  private def letter(prize: TicketPrize): String = {
    val gold   = Option.when(prize.doubloons > 0L)(content.format("activity.goldLine",
                   "doubloons" -> prize.doubloons.toString))
    val silver = Option.when(prize.silver > 0L)(content.format("activity.silverLine",
                   "silver" -> prize.silver.toString))
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

  /** Что даёт билет в конце недели: золотой — ровно столько дублонов,
    * серебряный — серебро в этих пределах, у каждого билета своё. */
  val DoubloonsPerTicket: Long = 2L
  val SilverMin: Long          = 100L
  val SilverMax: Long          = 300L

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
