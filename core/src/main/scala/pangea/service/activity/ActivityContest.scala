package pangea.service.activity

import pangea.dao.activity.{ActivityDao, TicketPrize, WeekTickets}
import pangea.dao.hero.HeroDao
import pangea.engine.{Players, SceneContent}
import pangea.model.hero.{AzatState, Hero}
import pangea.model.user.{User, VkId}
import pangea.repository.user.UserRepository
import zio.{Random, Task, UIO, ZIO, ZLayer}

import scala.annotation.tailrec

/** Билеты за активность в группе ВК. Лайк свежего поста — золотой билет, по
  * одному на пост; комментарий под свежим постом — серебряный, по одному в
  * сутки. О каждом новом билете игроку сразу приходит письмо — шуточная
  * находка и счёт за неделю. В конце недели каждый золотой оборачивается
  * двумя дублонами, каждый серебряный — серебром от 100 до 300, и сверх того
  * каждый билет бросается на выигрыш (так решил пользователь): золотой — 10%
  * на 10 дублонов и 0,5% на 100, серебряный — 10% на 1000 серебра и 0,5% на
  * 10 000. Об итогах — ещё одно письмо, с выигрышами отдельной строкой.
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
          winners <- ZIO.foreach(found.flatten) { case (t, user, hero) => winningsOf(t, hero).map(_ -> user) }
          paid    <- dao.award(week, winners.map(_._1.prize), now)
          _       <- ZIO.when(paid && winners.nonEmpty)(congratulate(winners))
        } yield ()
    }
  }

  /** Что принесли билеты одного игрока: каждый — свою долю (золотой — дублоны,
    * серебряный — серебро, у каждого своё) и свой бросок на выигрыш сверх неё. */
  private def winningsOf(t: WeekTickets, hero: Hero): Task[Winnings] =
    for {
      gold   <- ZIO.foreach(List.range(0L, t.gold))(_ => winRoll(GoldWins))
      silver <- ZIO.foreach(List.range(0L, t.silver))(_ =>
                  Random.nextLongBetween(SilverMin, SilverMax + 1L) <*> winRoll(SilverWins))
    } yield Winnings(
      TicketPrize(hero.id, silver.map { case (share, won) => share + won }.sum, t.gold * DoubloonsPerTicket + gold.sum),
      gold.filter(_ > 0L), silver.map(_._2).filter(_ > 0L))

  /** Бросок одного билета на выигрыш: сколько он выиграл сверх своей доли. */
  private def winRoll(wins: List[Win]): UIO[Long] = Random.nextIntBounded(WinScale).map(win(wins, _))

  private def byUserId(t: WeekTickets): Task[Option[(User, Hero)]] =
    for {
      user <- userRepo.getUserById(t.userId).option.map(_.flatten)
      hero <- heroDao.getHeroByUserId(t.userId)
    } yield for { u <- user; h <- hero } yield (u, h)

  private def congratulate(winners: List[(Winnings, User)]): Task[Unit] = {
    val doubloons = winners.map(_._1.prize.doubloons).sum
    val silver    = winners.map(_._1.prize.silver).sum
    for {
      _ <- ZIO.foreachDiscard(winners) { case (prize, user) => players.notify(user, letter(prize)).ignore }
      _ <- players.announce(content.format("activity.results",
             "doubloons" -> doubloons.toString, "silver" -> silver.toString)).ignore
    } yield ()
  }

  /** Письмо игроку: что принесли билеты каждого вида — сами билеты не считаем;
    * под строкой вида — что из этого выиграно сверх доли. */
  private def letter(w: Winnings): String = {
    val gold   = Option.when(w.prize.doubloons > 0L)(content.format("activity.goldLine",
                   "doubloons" -> w.prize.doubloons.toString) + winsLine(w.goldWins, "activity.goldWin"))
    val silver = Option.when(w.prize.silver > 0L)(content.format("activity.silverLine",
                   "silver" -> w.prize.silver.toString) + winsLine(w.silverWins, "activity.silverWin"))
    content.format("activity.yourTickets", "lines" -> (gold.toList ++ silver).mkString("\n"))
  }

  /** Строка выигрышей: «🎉 Из них выигрыш: 🟡 100, 🟡 10 ×2» — крупные первыми,
    * одинаковые — одной записью со счётом. Выигрышей нет — ничего. */
  private def winsLine(wins: List[Long], key: String): String =
    if (wins.isEmpty) ""
    else "\n" + content.format("activity.winsLine", "wins" ->
      wins.groupBy(identity).toList.sortBy { case (amount, _) => -amount }.map { case (amount, all) =>
        content.format(key, "amount" -> amount.toString) + (if (all.size > 1) s" ×${all.size}" else "")
      }.mkString(", "))
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

  /** Выигрыш билета сверх его доли: шанс — на тысячу (`perMille`), и сколько. */
  final case class Win(perMille: Int, amount: Long)

  /** Бросок на выигрыш — из стольких. */
  val WinScale: Int = 1000

  /** Золотой билет: 0,5% — 100 дублонов, 10% — 10 (так решил пользователь). */
  val GoldWins: List[Win] = List(Win(5, 100L), Win(100, 10L))

  /** Серебряный билет: 0,5% — 10 000 серебра, 10% — 1000. */
  val SilverWins: List[Win] = List(Win(5, 10000L), Win(100, 1000L))

  /** Что выиграл бросок `roll` (от 0 до [[WinScale]] − 1): выигрыши идут подряд,
    * каждый своей долей; не попал ни в один — без выигрыша. */
  def win(wins: List[Win], roll: Int): Long = {
    @tailrec
    def go(rest: List[Win], from: Int): Long = rest match {
      case w :: tail => if (roll < from + w.perMille) w.amount else go(tail, from + w.perMille)
      case Nil       => 0L
    }
    go(wins, 0)
  }

  /** Итог недели одного игрока: выплата и что из неё выиграли его билеты сверх
    * доли (по выигрышу на билет). */
  final case class Winnings(prize: TicketPrize, goldWins: List[Long], silverWins: List[Long])

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
