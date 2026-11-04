package pangea.service.sender.vk

import io.circe.parser.parse
import io.circe.{ACursor, Json}
import org.http4s.client.Client
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.{Method, Request, Uri}
import pangea.dao.vk.VkInboxDao
import pangea.service.sender.vk.config.VkConfig
import zio.interop.catz._
import zio.{Duration, Ref, Task, UIO, ZIO, ZLayer, durationInt}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

/** Bots Long Poll — второй путь событий сообщества рядом с Callback API: тут
  * бот сам спрашивает ВК «что нового?» и ждёт ответа до 25 секунд. Когда ВК
  * задерживает Callback, сообщение может прийти этим путём раньше; в ход идёт
  * то, что пришло первым (см. [[pangea.dao.vk.VkInboxDao]]).
  *
  * Берём только новые сообщения: события конкурса активности остаются на
  * Callback — там их подлинность заверяет секрет.
  *
  * Связь оборвалась — переподключаемся с того же места (`ts`): ВК хранит
  * очередь, и пропущенное за обрыв приходит следом. Место хранится и вне
  * процесса ([[LongPollCursor]]): после перезапуска чтение продолжается с него,
  * и пришедшее за паузу забирается сразу. Двигаемся дальше, только когда
  * `onUpdates` сохранил пачку: не вышло — та же пачка придёт ещё раз. */
final class VkLongPoll(client: Client[Task], config: VkConfig, cursor: LongPollCursor, pauses: VkLongPoll.Pauses) {
  import VkLongPoll._

  /** Слушать до прерывания. `onUpdates` получает пачку сообщений и сохраняет
    * её — ходов не ждёт: следующий запрос к ВК идёт сразу за ним. */
  def run(onUpdates: List[Json] => Task[Unit]): UIO[Nothing] =
    cursor.load
      .catchAllCause(cause => ZIO.logWarningCause("Long Poll: не прочитать, где остановились, — начну с текущего", cause).as(None))
      .flatMap(Ref.make(_)).flatMap { lastTs =>
      connect(lastTs).foldCauseZIO(
        cause =>
          ZIO.logWarning(s"Long Poll: ВК не дал подключиться (${reason(cause)}) — повтор через ${pauses.refused.getSeconds} с") *>
            ZIO.sleep(pauses.refused),
        { case (groupId, session) =>
          ZIO.logInfo("Long Poll: слушаю ВК") *>
            listen(groupId, session, lastTs, onUpdates).catchAllCause(cause =>
              ZIO.logWarning(s"Long Poll: связь с ВК прервалась (${reason(cause)}) — переподключаюсь") *>
                ZIO.sleep(pauses.dropped))
        }
      ).forever
    }

  private def connect(lastTs: Ref[Option[String]]): Task[(Long, Session)] =
    for {
      groupId <- method("groups.getById").flatMap(json => ZIO.fromEither(parseGroupId(json)).mapError(new RuntimeException(_)))
      ts      <- lastTs.get
      session <- open(groupId, ts)
    } yield (groupId, session)

  /** Адрес и ключ очереди. `ts` — с какого события читать; нет — с текущего. */
  private def open(groupId: Long, ts: Option[String]): Task[Session] =
    method("groups.getLongPollServer", "group_id" -> groupId.toString)
      .flatMap(json => ZIO.fromEither(parseSession(json, ts)).mapError(new RuntimeException(_)))

  private def listen(groupId: Long, session: Session, lastTs: Ref[Option[String]],
                     onUpdates: List[Json] => Task[Unit]): Task[Nothing] =
    lastTs.set(Some(session.ts)) *> check(session).flatMap {
      case Reply.Events(ts, updates) =>
        val messages = updates.filter(isMessage)
        ZIO.when(messages.nonEmpty)(onUpdates(messages)) *> remember(session.ts, ts) *>
          listen(groupId, session.copy(ts = ts), lastTs, onUpdates)
      // Часть очереди ВК уже выбросил: читаем с того места, что он назвал.
      // Выпавшее мимо нас не прошло — его принёс Callback.
      case Reply.Missed(ts) =>
        ZIO.logWarning("Long Poll: ВК потерял часть очереди — читаю дальше") *> remember(session.ts, ts) *>
          listen(groupId, session.copy(ts = ts), lastTs, onUpdates)
      case Reply.KeyExpired =>
        open(groupId, Some(session.ts)).flatMap(listen(groupId, _, lastTs, onUpdates))
      case Reply.Lost =>
        open(groupId, None).flatMap(listen(groupId, _, lastTs, onUpdates))
    }

  /** Запомнить новое место; пустой ответ места не двигает — писать незачем. */
  private def remember(was: String, ts: String): Task[Unit] =
    ZIO.when(ts != was)(ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS)).flatMap(cursor.save(ts, _))).unit

  private def check(session: Session): Task[Reply] = {
    val uri = session.server.withQueryParams(Map(
      "act"  -> "a_check",
      "key"  -> session.key,
      "ts"   -> session.ts,
      "wait" -> WaitSeconds.toString
    ))
    client.run(Request[Task](Method.GET, uri)).use { res =>
      res.bodyText.compile.string.flatMap { body =>
        if (!res.status.isSuccess) ZIO.fail(new RuntimeException(s"HTTP ${res.status.code}"))
        else ZIO.fromEither(parse(body).left.map(_.getMessage).flatMap(parseReply)).mapError(new RuntimeException(_))
      }
    }
  }

  /** Метод API ВК от имени сообщества — его ответ (`response`). */
  private def method(name: String, params: (String, String)*): Task[Json] = {
    val uri = (uri"https://api.vk.com/method" / name)
      .withQueryParams(params.toMap ++ Map("access_token" -> config.token, "v" -> VkApi.Version))
    client.run(Request[Task](Method.GET, uri)).use(res =>
      res.bodyText.compile.string.flatMap(VkApi.responseOf(res.status, _)))
  }

  private def reason(cause: zio.Cause[Throwable]): String =
    cause.failureOption.orElse(cause.dieOption).map(t => Option(t.getMessage).getOrElse(t.toString))
      .getOrElse(cause.prettyPrint.take(200))
}

object VkLongPoll {

  /** Сколько ВК держит запрос, если новых событий нет. Больше — дольше висит
    * один запрос; меньше — чаще спрашиваем впустую. */
  val WaitSeconds = 25

  /** Пауза перед повтором: `dropped` — связь оборвалась посреди работы,
    * `refused` — ВК не дал подключиться (например, Long Poll выключен в
    * настройках сообщества): тут спешить некуда, а лог не должен забиваться. */
  final case class Pauses(dropped: Duration, refused: Duration)

  val live: ZLayer[VkConfig with VkInboxDao, Throwable, VkLongPoll] = ZLayer.scoped(for {
    config <- ZIO.service[VkConfig]
    cursor <- ZIO.service[VkInboxDao]
    // Тайм-аут запроса — с запасом к ожиданию ВК: иначе пустой ответ через 25
    // секунд считался бы обрывом.
    client <- pangea.client.Client.scoped(FiniteDuration(WaitSeconds + 15L, TimeUnit.SECONDS))
  } yield new VkLongPoll(client, config, cursor, Pauses(dropped = 5.seconds, refused = 1.minute)))

  /** Адрес очереди, ключ к ней и номер, с которого читать. */
  final case class Session(server: Uri, key: String, ts: String)

  /** Ответ очереди. `failed` 1 — часть истории потеряна, 2 — ключ устарел,
    * 3 — потеряно всё: и ключ, и место в очереди. */
  sealed trait Reply
  object Reply {
    final case class Events(ts: String, updates: List[Json]) extends Reply
    final case class Missed(ts: String) extends Reply
    case object KeyExpired extends Reply
    case object Lost extends Reply
  }

  /** Какие события берём: только новые сообщения. */
  val Types: Set[String] = Set("message_new")

  private def isMessage(update: Json): Boolean =
    update.hcursor.get[String]("type").toOption.exists(Types.contains)

  /** `id` сообщества из ответа `groups.getById`: с версии 5.194 — объект с
    * полем `groups`, раньше — сразу массив. */
  private[vk] def parseGroupId(response: Json): Either[String, Long] = {
    val c = response.hcursor
    c.downField("groups").downN(0).get[Long]("id")
      .orElse(c.downN(0).get[Long]("id"))
      .left.map(_ => s"groups.getById: нет id сообщества в ответе ${response.noSpaces.take(200)}")
  }

  /** Ответ `groups.getLongPollServer`; `ts` — продолжить с прежнего места. */
  private[vk] def parseSession(response: Json, ts: Option[String]): Either[String, Session] = {
    val c = response.hcursor
    for {
      server <- c.get[String]("server").left.map(_ => s"getLongPollServer: нет server в ${response.noSpaces.take(200)}")
      uri    <- Uri.fromString(server).left.map(_ => s"getLongPollServer: кривой server $server")
      key    <- c.get[String]("key").left.map(_ => "getLongPollServer: нет key")
      from   <- ts.toRight(()).orElse(tsOf(c)).left.map(_ => "getLongPollServer: нет ts")
    } yield Session(uri, key, from)
  }

  private[vk] def parseReply(json: Json): Either[String, Reply] = {
    val c = json.hcursor
    c.get[Int]("failed").toOption match {
      case Some(1)     => tsOf(c).map(Reply.Missed).left.map(_ => "failed 1 без ts")
      case Some(2)     => Right(Reply.KeyExpired)
      case Some(3)     => Right(Reply.Lost)
      case Some(other) => Left(s"неизвестный failed $other")
      case None =>
        for {
          ts      <- tsOf(c).left.map(_ => s"ответ без ts: ${json.noSpaces.take(200)}")
          updates <- c.getOrElse[List[Json]]("updates")(Nil).left.map(_ => "updates — не массив")
        } yield Reply.Events(ts, updates)
    }
  }

  /** `ts` ВК присылает то строкой, то числом. */
  private def tsOf(c: ACursor): Either[Unit, String] =
    c.get[String]("ts").orElse(c.get[Long]("ts").map(_.toString)).left.map(_ => ())
}
