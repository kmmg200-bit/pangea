package pangea.service.sender.vk

import cats.data.Chain
import io.circe.Json
import io.circe.parser.parse
import org.http4s.client.Client
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.{Method, Request, Status, UrlForm}
import pangea.metrics.GameMetrics
import pangea.dao.sendfailure.SendFailureDao
import pangea.model.user.{User, VkId}
import pangea.model.vk.{Attachment, Community}
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.{UserResponse, User => VkUser}
import pangea.service.ErrorLog
import pangea.service.chat.ChatButtons
import pangea.service.sender.{Api, ChatTarget}
import pangea.service.sender.vk.config.VkConfig
import zio.interop.catz._
import zio.{Duration, Ref, Schedule, Task, ZIO, durationInt}

import java.util.concurrent.TimeUnit
import scala.util.Random

class VkApi(client: Client[Task], config: VkConfig, failures: SendFailureDao) extends Api {

  private val uri = uri"https://api.vk.com/method/"
  private def auth(token: String): Map[String, List[String]] = Map(
    "access_token" -> List(token),
    "v"            -> List(VkApi.Version)
  )

  override def sendMessage(
      user: User,
      message: String,
      attachments: List[Attachment],
      keyboard: Option[Keyboard]
  ): Task[Unit] = {
    val fields =
      Map(
        "message"   -> List(message),
        "user_id"   -> List(user.vkId.value),
        "random_id" -> List(randomId.toString)
      ) ++
        keyboard.map(k => "keyboard" -> List(k.getJson)).toMap ++
        Option.when(attachments.nonEmpty)("attachment" -> List(attachments.mkString(","))).toMap

    // Счётчик попыток для записи в журнал инцидентов.
    for {
      attempts <- Ref.make(0)
      _ <- (attempts.update(_ + 1) *> call(config.token, VkApi.SendMethod, fields))
             .retry(VkApi.SendRetry)
             .tapError(err => recordFailure(user, message, keyboard.map(_.getJson), err, attempts).orElse(ZIO.unit))
    } yield ()
  }

  // Сообщение в беседу: тот же messages.send, только адресат — peer_id
  // беседы (см. VkApi.chatPeer). Без настроенной беседы молчим: объявления и
  // ответы в беседу не должны ломать ход игрока, поэтому и ошибки здесь не
  // роняют вызов.
  //
  // В общую и торговую беседу сообщение уходит с кнопками приложения и
  // туториала под полем ввода (см. VkApi.chatKeyboard). ВК их не принял — то же
  // сообщение без них: объявление важнее кнопок. random_id у обеих попыток
  // один, поэтому дважды сообщение не уйдёт.
  //
  // Пишем токеном того сообщества, чья это беседа (см. VkApi.tokenFor).
  override def sendToChat(to: ChatTarget, message: String): Task[Unit] =
    (VkApi.chatPeer(config, to), VkApi.tokenFor(config, to)) match {
      case (None, _) => ZIO.unit
      case (Some(peerId), None) =>
        ZIO.logWarning(s"Беседа $peerId гильдейского сообщества: VK_TOKEN_FOR_GROUP_CHATS не задан — писать нечем")
      case (Some(peerId), Some(token)) =>
        val fields = Map(
          "message"   -> List(message),
          "peer_id"   -> List(peerId.toString),
          "random_id" -> List(randomId.toString)
        )

        def send(fields: Map[String, List[String]]): Task[Unit] =
          call(token, VkApi.SendMethod, fields).unit.retry(VkApi.SendRetry)

        VkApi.chatKeyboard(config, to)
          .fold(send(fields)) { keys =>
            send(fields + ("keyboard" -> List(keys.getJson))).catchAll(err =>
              ZIO.logWarning(s"vk chat keyboard refused, sending without it: ${err.getMessage}") *> send(fields))
          }
          .tapErrorCause(ErrorLog.logged("vk chat announce failed"))
          .orElse(ZIO.unit)
    }

  // Пересылка /home и /restart админам. Пересылку ВК может и не принять — тогда
  // шлём хотя бы подпись: в ней ссылка на игрока, и админ всё равно узнает, кто это.
  override def forwardToUser(to: VkId, message: String, forwarded: Option[Long]): Task[Unit] = {
    val letter = Map("message" -> List(message), "user_id" -> List(to.value))
    forwarded.filter(_ > 0L) match {
      case Some(id) =>
        post(letter + ("forward_messages" -> List(id.toString)))
          .tapError(err => ZIO.logWarning(s"vk forward failed, sending caption only: ${err.getMessage}"))
          .orElse(post(letter))
      case None => post(letter)
    }
  }

  /** messages.send основным токеном с этими полями, своим random_id (один на
    * все повторы, чтобы повтор не задвоил письмо) и теми же повторами. */
  private def post(fields: Map[String, List[String]]): Task[Unit] =
    call(config.token, VkApi.SendMethod, fields + ("random_id" -> List(randomId.toString))).unit.retry(VkApi.SendRetry)

  /** Одна попытка вызвать `method` токеном `token`; ответ — поле `response`.
    * Ошибка ВК, ответ не 2xx или не JSON и сбой сети — поломка, и каждая — в
    * метрику ошибок по коду ([[VkApi.errorCode]]). Успешный ответ не логируем
    * (шумно), ошибочный — да. Повторы — у вызывающего: каждый идёт сюда же.
    *
    * Параметры уходят телом POST (form-urlencoded), а не в query string: длинные
    * экраны (лавка Ришелье — 3 предмета со сравнением с надетым + клавиатура) в
    * percent-encoding раздувались до ~7-8 КБ и упирались в лимит строки запроса
    * на стороне ВК; ответ 414 приходил HTML-ом и молча считался успехом. */
  private def call(token: String, method: String, fields: Map[String, List[String]]): Task[Json] = {
    val request = Request[Task](
      method = Method.POST,
      uri = uri / method
    ).withEntity(UrlForm((fields ++ auth(token)).map { case (k, vs) => k -> Chain.fromSeq(vs) }))

    GameMetrics.vkCall(method)(
      client
        .run(request)
        .use(res => res.bodyText.compile.string.map(res.status -> _))
        .tapError(_ => GameMetrics.vkError(method, VkApi.NoResponse))
        .flatMap { case (status, body) =>
          VkApi.responseOf(status, body).tapError(err =>
            ZIO.logError(s"vk $method response: ${VkApi.snippet(body)}") *>
              GameMetrics.vkError(method, VkApi.errorCode(status, err)))
        })
  }

  private def recordFailure(
    user:         User,
    messageText:  String,
    keyboardJson: Option[String],
    err:          Throwable,
    attemptsRef:  Ref[Int]
  ): Task[Unit] =
    for {
      now      <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      attempts <- attemptsRef.get
      (code, msg) = err match {
        case VkApi.VkApiError(c, m) => (Some(c), m)
        case other                  => (None, Option(other.getMessage).getOrElse(other.toString))
      }
      _ <- GameMetrics.vkSendFailures.increment
      _ <- failures.record(
             userId       = Option(user.userId).filter(_.value > 0L),
             vkId         = user.vkId.value,
             messageText  = messageText,
             keyboardJson = keyboardJson,
             errorCode    = code,
             errorMessage = msg,
             attempts     = attempts,
             createdAt    = now
           )
    } yield ()

  override def getName(user: User): Task[UserResponse] =
    call(config.token, "users.get", Map("user_ids" -> List(user.vkId.value)))
      .flatMap(response => ZIO.fromEither(response.as[List[VkUser]]).map(UserResponse(_)))

  private def randomId = Random.between(Int.MinValue, Int.MaxValue)
}

object VkApi {

  /** `peer_id` беседы `to`. Торговая не настроена — пишем в общую: объявления
    * не должны пропадать из-за пустой настройки. */
  def chatPeer(config: VkConfig, to: ChatTarget): Option[Long] = to match {
    case ChatTarget.General      => config.chatPeerId
    case ChatTarget.Trade        => config.tradeChatPeerId.orElse(config.chatPeerId)
    case ChatTarget.Peer(peerId, _) => Some(peerId)
  }

  /** Чьим токеном писать: в беседы гильдейского сообщества — его, а нет его —
    * нечем; всё прочее — основного. */
  def tokenFor(config: VkConfig, to: ChatTarget): Option[String] = to match {
    case ChatTarget.Peer(_, Community.Guilds) => config.guilds
    case _                                    => Some(config.token)
  }

  /** Кнопки под полем ввода — только в общей и торговой беседе основного
    * сообщества (см. [[ChatButtons]]); в прочих беседах — без кнопок. Беседы
    * гильдейского — всегда без них: номер его беседы может совпасть с общей. */
  def chatKeyboard(config: VkConfig, to: ChatTarget): Option[Keyboard] = {
    def ours(peerId: Long) = config.chatPeerId.contains(peerId) || config.tradeChatPeerId.contains(peerId)
    val shown = to match {
      case ChatTarget.Peer(_, Community.Guilds) => false
      case other                                => chatPeer(config, other).exists(ours)
    }
    Option.when(shown)(ChatButtons.keyboard)
  }

  /** Метод отправки — метка в метриках вызовов ВК. */
  private val SendMethod = "messages.send"

  /** Версия API ВК: от неё зависит вид событий и ответов. */
  val Version = "5.199"

  /** Ошибка API VK: тело ответа содержит объект `error`. */
  final case class VkApiError(code: Int, message: String) extends RuntimeException(s"VK API error $code: $message")

  /** Игрок не принимает сообщения от сообщества: 900 — сообщество у него в
    * чёрном списке, 901 — сообщения не разрешены, 902 — закрыты настройками
    * приватности. Пока игрок сам их не откроет, повтор не поможет. */
  val UnreachableCodes: Set[Int] = Set(900, 901, 902)

  /** Через сколько ещё раз отправить, когда ВК ответил «игрок закрыл
    * сообщения» (900–902). ВК порой отвечает так ложно, в сбое: 3 ноября трое
    * игроков получили 901/902 посреди игры, а следующее же их нажатие дошло.
    * Одна попытка через паузу отличает сбой от закрытых сообщений; не дошло и
    * тогда — больше не повторяем. */
  val UnreachableRetryDelay: Duration = 5.seconds

  // Транзиентные сбои VK/сети — лечим повтором: до 3 попыток с задержкой
  // 250 → 500 мс. «Игрок закрыл сообщения» — одна попытка через
  // UnreachableRetryDelay. После — пишем инцидент в send_failures и
  // пробрасываем ошибку, чтобы catchAll в StateHandler мог отработать.
  private[vk] val SendRetry: Schedule[Any, Throwable, Any] =
    ((Schedule.recurs(2) && Schedule.exponential(250.millis)) +++
      (Schedule.recurs(1) && Schedule.spaced(UnreachableRetryDelay)))
      .contramap[Any, Throwable](err => if (Api.unreachable(err)) Right(err) else Left(err))

  /** Сколько символов тела ответа кладём в текст ошибки (HTML-страницы прокси
    * бывают на десятки килобайт, а ошибка едет в лог и в `send_failures`). */
  private val BodySnippetLimit = 300

  /**
   * Разбирает ответ VK API. Успех — только 2xx с JSON-телом без поля `error`.
   * Не-2xx (например 414 от прокси при слишком длинном запросе) и тело, которое
   * не является JSON, — это недоставленное сообщение: фейлим, чтобы отработали
   * повтор, журнал `send_failures` и `catchAll` в StateHandler. Раньше такой
   * ответ считался успехом, и игрок молча оставался без экрана.
   */
  def failOnBadResponse(status: Status, body: String): Task[Unit] =
    responseOf(status, body).unit

  /** То же, но с телом ответа — полем `response` (нет его — `null`). */
  def responseOf(status: Status, body: String): Task[Json] =
    if (!status.isSuccess) ZIO.fail(VkApiError(status.code, snippet(body)))
    else
      parse(body) match {
        case Left(_) => ZIO.fail(new RuntimeException(s"VK non-JSON response: ${snippet(body)}"))
        case Right(json) =>
          json.hcursor.downField("error").focus match {
            case None => ZIO.succeed(json.hcursor.downField("response").focus.getOrElse(Json.Null))
            case Some(err) =>
              val code = err.hcursor.get[Int]("error_code").getOrElse(-1)
              val msg  = err.hcursor.get[String]("error_msg").getOrElse(err.noSpaces)
              ZIO.fail(VkApiError(code, msg))
          }
      }

  private def snippet(body: String): String =
    if (body.length <= BodySnippetLimit) body else body.take(BodySnippetLimit) + "…"

  /** Код ошибки для метрики (см. [[GameMetrics.vkApiErrors]]): ответ не 2xx —
    * `http_<статус>`; 2xx с полем `error` — номер ошибки ВК; 2xx не JSON —
    * `not_json`. */
  def errorCode(status: Status, err: Throwable): String =
    if (!status.isSuccess) s"http_${status.code}"
    else err match {
      case VkApiError(code, _) => code.toString
      case _                   => "not_json"
    }

  /** Код ошибки для метрики, когда ответа нет вовсе: сеть, тайм-аут. */
  val NoResponse = "net"
}
