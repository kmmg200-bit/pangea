package pangea.service.sender.vk

import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpApp, UrlForm}
import pangea.dao.sendfailure.SendFailureDao
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.vk.Community
import pangea.service.sender.ChatTarget
import pangea.service.sender.vk.config.VkConfig
import zio.interop.catz._
import zio.test._
import zio.{Clock, Fiber, Ref, Task, ZIO, durationInt}

import java.util.concurrent.TimeUnit

object VkRateLimitSpec extends ZIOSpecDefault {

  private val dsl = Http4sDsl[Task]
  import dsl._

  private val now = Clock.currentTime(TimeUnit.MILLISECONDS)

  private val limit = VkRateLimit.PerSecond

  /** Дождаться, пока файбер встанет: так очередь к окну — в порядке запуска. */
  private def parked(fiber: Fiber.Runtime[_, _]) =
    fiber.status.repeatUntil {
      case _: Fiber.Status.Suspended => true
      case _                         => false
    }

  /** Дождаться, пока файбер прошёл окно или встал к нему: только тогда двигать
    * часы — иначе под нагрузкой он дойдёт до окна, когда они уже ушли вперёд. */
  private def settled(fiber: Fiber.Runtime[_, _]) =
    fiber.status.repeatUntil {
      case Fiber.Status.Done | _: Fiber.Status.Suspended => true
      case _                                             => false
    }

  def spec = suite("Окно частоты ВК")(
    test("за секунду — 20 вызовов сразу, 21-й — когда из окна выйдет первый") {
      for {
        window <- VkRateLimit.make(Community.Main)
        done   <- Ref.make(0)
        _      <- ZIO.foreachDiscard(1 to limit)(_ => window(done.update(_ + 1)))
        first  <- done.get
        late   <- window(done.update(_ + 1)).fork
        _      <- TestClock.adjust(999.millis)
        early  <- late.poll
        _      <- TestClock.adjust(1.milli)
        _      <- late.join
        all    <- done.get
      } yield assertTrue(first == limit, early.isEmpty, all == limit + 1)
    },
    test("пятьдесят разом: за любую секунду — не больше 20, и дождались все") {
      for {
        window <- VkRateLimit.make(Community.Main)
        times  <- Ref.make(Vector.empty[Long])
        fibers <- ZIO.foreach(1 to 50)(_ => window(now.flatMap(t => times.update(_ :+ t))).fork)
        _      <- ZIO.foreachDiscard(fibers)(settled)
        // Часы — по секунде, и каждый раз дожидаемся, пока места этой секунды
        // разобраны: ждущие проходят семафор по одному, и один сдвиг на 3 с под
        // нагрузкой полного прогона уходил дальше раньше, чем они успевали.
        _      <- TestClock.adjust(1.second) *> times.get.repeatUntil(_.size == 2 * limit)
        _      <- TestClock.adjust(1.second) *> times.get.repeatUntil(_.size == 50)
        _      <- ZIO.foreachDiscard(fibers)(_.join)
        seen   <- times.get.map(_.sorted)
      } yield assertTrue(seen.size == 50) &&
              assertTrue(seen.forall(t => seen.count(u => u >= t && u < t + 1000L) <= limit)) &&
              assertTrue(seen.groupBy(identity).view.mapValues(_.size).toMap == Map(0L -> 20, 1000L -> 20, 2000L -> 10))
    },
    // Место окно выдаёт по очереди, а сам вызов идёт уже после, вне неё: трое,
    // получившие место в одну миллисекунду, могут выполниться в любом порядке.
    // Поэтому окно заполнено по вызову в миллисекунду — места освобождаются в
    // 1000, 1001 и 1002, и порядок их выдачи виден по времени.
    test("ждущие получают места по порядку прихода") {
      for {
        window <- VkRateLimit.make(Community.Main)
        order  <- Ref.make(Vector.empty[(String, Long)])
        _      <- ZIO.foreachDiscard(1 to limit)(i =>
                    window(ZIO.unit) *> ZIO.when(i < limit)(TestClock.adjust(1.milli)))
        mark    = (name: String) => window(now.flatMap(t => order.update(_ :+ (name -> t))))
        a      <- mark("a").fork
        _      <- parked(a)
        b      <- mark("b").fork
        _      <- parked(b)
        c      <- mark("c").fork
        _      <- parked(c)
        _      <- TestClock.adjust(1.second)
        _      <- a.join *> b.join *> c.join
        seen   <- order.get.map(_.sortBy(_._2))
      } yield assertTrue(seen == Vector("a" -> 1000L, "b" -> 1001L, "c" -> 1002L))
    },
    test("пик за секунду: двадцать разом — 20; держится 20 секунд, потом забыт") {
      for {
        window <- VkRateLimit.make(Community.Main)
        _      <- ZIO.foreachDiscard(1 to limit)(_ => window(ZIO.unit))
        burst  <- window.peak
        _      <- TestClock.adjust(VkRateLimit.PeakSpan)
        kept   <- window.peak
        _      <- TestClock.adjust(1.second)
        gone   <- window.peak
      } yield assertTrue(burst == limit, kept == limit, gone == 0)
    },
    test("пик — за любую секунду, не за календарную: 10 в конце одной и 10 в начале другой — 20") {
      for {
        window <- VkRateLimit.make(Community.Main)
        _      <- TestClock.adjust(500.millis)
        _      <- ZIO.foreachDiscard(1 to 10)(_ => window(ZIO.unit))
        _      <- TestClock.adjust(700.millis)
        _      <- ZIO.foreachDiscard(1 to 10)(_ => window(ZIO.unit))
        peak   <- window.peak
      } yield assertTrue(peak == 20)
    },
    test("ВК ответил «слишком часто» — окно закрыто на секунду") {
      for {
        window <- VkRateLimit.make(Community.Main)
        _      <- window.tooFast
        late   <- window(ZIO.unit).fork
        _      <- TestClock.adjust(999.millis)
        early  <- late.poll
        _      <- TestClock.adjust(1.milli)
        _      <- late.join
      } yield assertTrue(early.isEmpty)
    },
    test("6 от ВК: повтор основным ключом — через секунду, а гильдейский пишет сразу") {
      for {
        calls <- Ref.make(Vector.empty[(String, Long)])
        api   <- VkApi.make(tooFastOnce(calls), config, NoFailures)
        main  <- api.sendMessage(player, "экран", Nil, None).fork
        // Ответ 6 разобран, окно закрыто, повтор уснул — только теперь можно
        // двигать часы: раньше повтор уснул бы уже после сдвига и не проснулся.
        _     <- TestClock.sleeps.repeatUntil(_.nonEmpty)
        _     <- api.sendToChat(ChatTarget.Peer(2000000001L, Community.Guilds), "гильдии")
        _     <- TestClock.adjust(1.second)
        _     <- main.join
        seen  <- calls.get
      } yield assertTrue(seen.filter(_._1 == "t").map(_._2) == Vector(0L, 1000L)) &&
              assertTrue(seen.filter(_._1 == "g").map(_._2) == Vector(0L))
    }
  ) @@ TestAspect.timeout(30.seconds)

  private val player = User(UserId(1L), VkId("7"), TelegramId("tg"))

  private val config = VkConfig("t", guildsToken = Some("g"))

  private object NoFailures extends SendFailureDao {
    override def record(userId: Option[UserId], vkId: String, messageText: String, keyboardJson: Option[String],
                        errorCode: Option[Int], errorMessage: String, attempts: Int, createdAt: Long): Task[Unit] =
      ZIO.unit
  }

  /** ВК, что записывает ключ и время каждого вызова, а на первый вызов
    * основным ключом отвечает «слишком часто». */
  private def tooFastOnce(calls: Ref[Vector[(String, Long)]]): Client[Task] =
    Client.fromHttpApp(HttpApp[Task] { req =>
      req.as[UrlForm].flatMap { form =>
        val token = form.getFirst("access_token").getOrElse("")
        now.flatMap(t => calls.modify(c => (c.count(_._1 == token), c :+ (token -> t)))).flatMap {
          case 0 if token == "t" => Ok("""{"error":{"error_code":6,"error_msg":"Too many requests per second"}}""")
          case _                 => Ok("""{"response":1}""")
        }
      }
    })
}
