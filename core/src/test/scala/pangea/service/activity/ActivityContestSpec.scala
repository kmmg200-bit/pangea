package pangea.service.activity

import java.time.{ZoneId, ZonedDateTime}
import pangea.engine.SceneContent
import pangea.model.hero.HeroId
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.chat.ChatCommand
import pangea.test._
import zio.ZIO
import zio.test._

/** Билеты за активность в группе: события группы, билеты недели, их оплата. */
object ActivityContestSpec extends ZIOSpecDefault {

  private val moscow = ZoneId.of("Europe/Moscow")
  private def msk(y: Int, mo: Int, d: Int, h: Int, mi: Int = 0): Long =
    ZonedDateTime.of(y, mo, d, h, mi, 0, 0, moscow).toInstant.toEpochMilli

  // Вторник, 13 октября 2026-го, 15:00 по Москве.
  private val now  = msk(2026, 10, 13, 15)
  private val hour = 60L * 60L * 1000L
  private val day  = 24L * hour

  private def user(n: Int): User = User(UserId(n.toLong), VkId((100 + n).toString), TelegramId(s"tg$n"))

  /** Билеты с игроками 1..`players` (у каждого герой с тем же номером); у игрока
    * 9 аккаунт есть, а героя нет. */
  private def contest(players: Int = 3) =
    for {
      heroDao  <- TestHeroDao.make
      _        <- ZIO.foreachDiscard(1 to players)(n =>
                    heroDao.insertHero(TestFixtures.hero(UserId(n.toLong)).copy(id = HeroId(n.toLong))))
      userRepo <- TestUserRepository.withUsers(((1 to players).map(user) :+ user(9)): _*)
      content  <- ZIO.attempt(SceneContent.load())
      payouts   = TestPayoutDao.empty
      dao       = new TestActivityDao(payouts)
      players_  = new TestPlayers
    } yield (ActivityContest(dao, userRepo, heroDao, players_, content), dao, payouts, players_)

  private def json(s: String) = io.circe.parser.parse(s).toOption.get

  private def post(id: Long, at: Long)   = GroupEvent.PostPublished(id, at)
  private def like(n: Int, post: Long)    = GroupEvent.Liked(100L + n, post)
  private def comment(n: Int, post: Long) = GroupEvent.Commented(100L + n, post)

  private val week = ActivityContest.weekStart(now)
  private def ticketsOf(dao: TestActivityDao) =
    dao.tickets(week).map(_.map(t => t.userId -> (t.gold, t.silver)).toMap)

  override def spec = suite("Билеты за активность")(

    test("неделя — с понедельника 00:00 по Москве, сутки — с полуночи по Москве") {
      val monday = msk(2026, 10, 12, 0)
      assertTrue(ActivityContest.weekStart(now) == monday) &&
      assertTrue(ActivityContest.weekStart(monday) == monday) &&
      // воскресенье 23:59 — ещё прошлая неделя
      assertTrue(ActivityContest.weekStart(msk(2026, 10, 11, 23, 59)) == msk(2026, 10, 5, 0)) &&
      assertTrue(ActivityContest.dayStart(now) == msk(2026, 10, 13, 0)) &&
      assertTrue(ActivityContest.dayStart(msk(2026, 10, 13, 23, 59)) == msk(2026, 10, 13, 0))
    },

    test("разбор событий группы: пост, лайк на посте, комментарий игрока — и только они") {
      val published = json("""{"type":"wall_post_new","object":{"id":28,"date":1760000000,"post_type":"post","owner_id":-1},"secret":"s"}""")
      val postponed = json("""{"type":"wall_post_new","object":{"id":29,"date":1760000000,"post_type":"postpone"}}""")
      val liked     = json("""{"type":"like_add","object":{"liker_id":5,"object_type":"post","object_owner_id":-1,"object_id":28}}""")
      val onPhoto   = json("""{"type":"like_add","object":{"liker_id":5,"object_type":"photo","object_id":28}}""")
      val replied   = json("""{"type":"wall_reply_new","object":{"id":3,"from_id":7,"post_id":28,"owner_id":-1,"date":1}}""")
      val byGroup   = json("""{"type":"wall_reply_new","object":{"id":4,"from_id":-1,"post_id":28}}""")
      val other     = json("""{"type":"group_join","object":{"user_id":5}}""")
      assertTrue(GroupEvent.parse(published).contains(GroupEvent.PostPublished(28L, 1760000000000L))) &&
      assertTrue(GroupEvent.secretOf(published).contains("s")) &&
      assertTrue(GroupEvent.parse(postponed).isEmpty) &&
      assertTrue(GroupEvent.parse(liked).contains(GroupEvent.Liked(5L, 28L))) &&
      assertTrue(GroupEvent.parse(onPhoto).isEmpty) &&
      assertTrue(GroupEvent.parse(replied).contains(GroupEvent.Commented(7L, 28L))) &&
      assertTrue(GroupEvent.parse(byGroup).isEmpty) &&
      assertTrue(GroupEvent.parse(other).isEmpty && !GroupEvent.Types.contains("group_join"))
    },

    test("лайк свежего поста — золотой билет, один на пост, сколько ни ставь заново") {
      for {
        t <- contest()
        (c, dao, _, _) = t
        _       <- c.handle(post(1L, now - hour), now)
        _       <- c.handle(post(2L, now - 23 * hour), now)
        _       <- c.handle(like(1, 1L), now)
        _       <- c.handle(like(1, 1L), now + 1000L)      // сняла и поставила снова
        _       <- c.handle(like(1, 2L), now)               // пост вышел почти сутки назад — ещё свежий
        _       <- c.handle(like(2, 1L), now)
        tickets <- ticketsOf(dao)
      } yield assertTrue(tickets == Map(UserId(1L) -> (2L, 0L), UserId(2L) -> (1L, 0L)))
    },

    test("комментарий под свежим постом — серебряный билет, один за московские сутки") {
      for {
        t <- contest()
        (c, dao, _, _) = t
        _       <- c.handle(post(1L, now - hour), now)
        _       <- c.handle(post(2L, now), now)
        _       <- c.handle(comment(1, 1L), now)
        _       <- c.handle(comment(1, 1L), now + hour)     // ещё один под тем же постом
        _       <- c.handle(comment(1, 2L), now + 2 * hour) // под другим, но в те же сутки
        // назавтра, после полуночи по Москве, — новый билет
        _       <- c.handle(post(3L, now + day - hour), now + day)
        _       <- c.handle(comment(1, 3L), now + day)
        tickets <- ticketsOf(dao)
      } yield assertTrue(tickets == Map(UserId(1L) -> (0L, 2L)))
    },

    test("пост старше суток, незнакомый пост, игрок без героя и чужак — без билетов") {
      for {
        t <- contest()
        (c, dao, _, _) = t
        _       <- c.handle(post(1L, now - day - 1000L), now)  // вышел больше суток назад
        _       <- c.handle(like(1, 1L), now)
        _       <- c.handle(comment(1, 1L), now)
        _       <- c.handle(like(2, 77L), now)                 // такого поста бот не видел
        _       <- c.handle(post(2L, now - hour), now)
        _       <- c.handle(like(9, 2L), now)                  // аккаунт есть, героя нет
        _       <- c.handle(GroupEvent.Liked(555L, 2L), now)   // не игрок вовсе
        tickets <- ticketsOf(dao)
      } yield assertTrue(tickets.isEmpty)
    },

    test("конец недели: золотой — два дублона, серебряный — 100–300 серебра каждый, каждому одно письмо, в беседу — сводка, и всё один раз") {
      for {
        t <- contest()
        (c, _, payouts, players) = t
        _ <- c.handle(post(1L, now - hour), now)
        _ <- c.handle(post(2L, now - hour), now)
        _ <- c.handle(like(1, 1L), now)
        _ <- c.handle(like(1, 2L), now)
        _ <- c.handle(comment(1, 1L), now)
        _ <- c.handle(like(2, 1L), now)
        _ <- c.handle(post(3L, now + day - hour), now + day)
        _ <- c.handle(comment(3, 3L), now + day)
        _ <- c.handle(comment(1, 3L), now + day)       // у первого — второй серебряный
        // наступила следующая неделя; серебро бросается на каждый билет:
        // первому 120 и 300, третьему 100
        nextWeek = week + ActivityContest.WeekMs + hour
        _ <- TestRandom.feedLongs(120L, 300L, 100L)
        _ <- c.settle(nextWeek)
        _ <- c.settle(nextWeek + 60000L)               // через минуту — уже подведено
        // письма о каждом билете тут не в счёт — только итоги
        weekly  = players.sentLetters.filter(_._2.contains("Итоги недели"))
        letters = weekly.toMap
      } yield assertTrue(payouts.snapshot == Map(
                HeroId(1L) -> (420L, 4L), HeroId(2L) -> (0L, 2L), HeroId(3L) -> (100L, 0L))) &&
              assertTrue(weekly.size == 3) &&
              assertTrue(letters(UserId(1L)).contains("Золотые билеты принесли вам 🟡 4") &&
                         letters(UserId(1L)).contains("Серебряные билеты принесли вам 🪙 420")) &&
              // у кого серебряных нет, про них и не пишем
              assertTrue(!letters(UserId(2L)).contains("Серебряные")) &&
              assertTrue(!letters(UserId(3L)).contains("Золотые")) &&
              assertTrue(players.announced == List(
                "🎫 Участники лотереи разделили между собой 🟡 6 и 🪙 520, спасибо всем за участие!"))
    },

    test("неделя без билетов подводится молча: ни выплат, ни писем, ни сводки") {
      for {
        t <- contest()
        (c, dao, payouts, players) = t
        _ <- c.settle(now)
      } yield assertTrue(payouts.snapshot.isEmpty && players.announced.isEmpty && players.sentLetters.isEmpty) &&
              assertTrue(dao.awardedSnapshot.contains(week - ActivityContest.WeekMs))
    },

    test("о каждом новом билете — письмо: находка, за что он и счёт недели; о повторе и отказе — молчок") {
      for {
        t <- contest()
        (c, _, _, players) = t
        _       <- c.handle(post(1L, now - hour), now)
        _       <- c.handle(post(2L, now - 2 * day), now)
        _       <- c.handle(like(1, 1L), now)
        _       <- c.handle(like(1, 1L), now + 1000L)       // повтор — билета нет
        _       <- c.handle(comment(1, 1L), now + 2000L)
        _       <- c.handle(comment(1, 1L), now + 3000L)    // серебряный за сутки уже есть
        _       <- c.handle(like(1, 2L), now)               // пост старше суток
        letters  = players.sentLetters.map(_._2)
      } yield assertTrue(letters.size == 2) &&
              assertTrue(letters.head.startsWith("🎫 ") && letters.head.contains("золотой билетик") &&
                         letters.head.contains("золотых билетов — 1, серебряных — 0")) &&
              assertTrue(letters(1).startsWith("🎟 ") && letters(1).contains("серебряный билетик") &&
                         letters(1).contains("золотых билетов — 1, серебряных — 1")) &&
              assertTrue(letters.forall(l => l.contains("Рахадим подведёт итоги лотереи и обязательно вас найдёт для приза. Где бы вы ни спрятались.") &&
                                             !l.contains("{")))
    },

    test("золотой билет — два дублона, серебряный — от 100 до 300 серебра") {
      assertTrue(ActivityContest.DoubloonsPerTicket == 2L) &&
      assertTrue(ActivityContest.SilverMin == 100L && ActivityContest.SilverMax == 300L)
    },

    test("находок десять, и в каждой есть место для билетика") {
      for {
        content <- ZIO.attempt(SceneContent.load())
        lines    = content.list("activity.noticeLines")
      } yield assertTrue(lines.size == 10 && lines.distinct.size == 10) &&
              assertTrue(lines.forall(_.contains("{ticket}")))
    },

    test("«Рейтинга» в беседе больше нет") {
      assertTrue(ChatCommand.selfCommand("Рейтинг").isEmpty) &&
      assertTrue(ChatCommand.selfCommand("рейтинг активности").isEmpty)
    }
  )
}
