package pangea.service.sender.vk

import io.circe.Json
import pangea.metrics.GameMetrics
import zio.stm.{STM, TRef}
import zio.{Chunk, Exit, Promise, Ref, Scope, Task, UIO, URIO, ZIO}

import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec

/** Пачки писем одного ключа сообщества. ВК пускает на ключ 20 запросов в
  * секунду ([[VkRateLimit]]), а в один `execute` кладётся до 25 вызовов API:
  * письма, скопившиеся, пока окно частоты полно, уходят одним запросом — до
  * 500 писем в секунду вместо 20.
  *
  * Письмо ([[send]]) встаёт в очередь; разбирает её один цикл ([[VkBatcher.make]]):
  * ждёт первое письмо, занимает место в окне частоты и забирает из очереди всё,
  * что успело накопиться, — не больше [[VkBatcher.MaxLetters]] писем и
  * [[VkBatcher.MaxBytes]] байт тела запроса ([[VkBatcher.cut]]). Одно письмо уходит обычным
  * `messages.send`, несколько — одним `execute`; ответа цикл не ждёт и сразу
  * берётся за следующее. Нарочно письма не копим: пока в окне есть место,
  * каждое уходит сразу и само по себе, а пачки складываются сами — только когда
  * окно полно.
  *
  * Исход у каждого письма свой: `execute` отвечает массивом — номер письма или
  * `false`, — а ошибки не ушедших по порядку лежат в `execute_errors`
  * ([[VkBatcher.outcomes]]). Не прошёл запрос целиком — его ошибку получает
  * каждое письмо пачки. Повторы — у вызывающего ([[VkApi]]). */
final class VkBatcher private (
    label: String,
    queue: TRef[Chunk[VkBatcher.Letter]],
    open:  TRef[Boolean],
    peaks: Ref[Map[Long, Int]]
) {
  import VkBatcher._

  /** Отправить письмо: `messages.send` с полями `fields` токеном `token` — в
    * пачке с прочими. Одна попытка; ответ — номер письма. */
  def send(token: String, fields: Map[String, String]): Task[Json] =
    for {
      done   <- Promise.make[Throwable, Json]
      since  <- GameMetrics.now
      letter  = Letter(token, fields, sizeOf(fields), since, done)
      queued <- open.get.flatMap(o => if (o) queue.modify(q => (Some(q.size + 1), q :+ letter)) else STM.succeed(None)).commit
      json   <- queued match {
                  case Some(size) => mark(size) *> done.await
                  case None       => ZIO.fail(Stopped)
                }
    } yield json

  /** Пик очереди за секунду — в метрику; бот зовёт раз в секунду (см.
    * [[VkApi.publishPeaks]]). */
  def publish: UIO[Unit] =
    peak.flatMap(n => GameMetrics.vkBatchQueued.tagged("community", label).set(n.toDouble))

  /** Больше всего писем в очереди за последние [[VkRateLimit.PeakSpan]]:
    * сколько их стало, когда письмо встало в очередь, — наибольшее по
    * секундам. Сама очередь живёт миллисекунды, и Prometheus, опрашивающий раз
    * в 15 с, увидел бы в ней почти всегда ноль. */
  private[vk] def peak: UIO[Int] =
    now.flatMap(t =>
      peaks.modify { bySecond =>
        val fresh = bySecond.filter { case (second, _) => second * 1000L >= t - VkRateLimit.PeakSpan.toMillis }
        (fresh.values.maxOption.getOrElse(0), fresh)
      })

  /** Письмо встало в очередь, в ней теперь `size` писем: в пик секунды. */
  private def mark(size: Int): UIO[Unit] =
    now.flatMap(t => peaks.update(bySecond => bySecond.updated(t / 1000L, bySecond.getOrElse(t / 1000L, 0) max size)))

  /** Разбирать очередь: место в окне частоты — `slot`, запрос — `post`. Пачку
    * из очереди забираем и отправку запускаем неразрывно: прерванный цикл не
    * потеряет писем — они либо в очереди, либо уже в отправке. Сколько письмо
    * простояло в очереди — в метрику, когда пачка забрана. */
  private def loop(slot: UIO[Unit], post: Post): UIO[Nothing] =
    (queue.get.retryUntil(_.nonEmpty).commit *> slot *>
      ZIO.uninterruptibleMask(restore =>
        queue.modify(letters => cut(letters)(_.token, _.bytes)).commit.flatMap(batch =>
          ZIO.foreachDiscard(batch)(l => GameMetrics.since(GameMetrics.vkBatchWaitSeconds.tagged("community", label), l.since)) *>
            restore(deliver(batch, post)).onInterrupt(failAll(batch, Stopped)).fork))).forever

  /** Отправить пачку и раздать исходы. */
  private def deliver(batch: Chunk[Letter], post: Post): UIO[Unit] =
    GameMetrics.vkBatchLetters.tagged("community", label).update(batch.size.toDouble).when(batch.nonEmpty) *>
      (batch.toList match {
        case Nil => ZIO.unit
        case one :: Nil =>
          post(one.token, VkApi.SendMethod, one.fields).map(VkApi.response).exit.flatMap(one.done.done).unit
        case first :: _ =>
          val (code, args) = script(batch.map(_.fields))
          post(first.token, ExecuteMethod, args + ("code" -> code)).exit.flatMap {
            case Exit.Success(body) =>
              ZIO.foreachDiscard(batch.zip(outcomes(body, batch.size))) {
                case (letter, Right(id)) => letter.done.succeed(id)
                case (letter, Left(err)) => refused(err) *> letter.done.fail(err)
              }
            case Exit.Failure(cause) => ZIO.foreachDiscard(batch)(_.done.failCause(cause))
          }
      })

  /** Письмо из пачки ВК не принял: в лог и в метрику ошибок — как одиночную
    * попытку `messages.send`. */
  private def refused(err: VkApi.VkApiError): UIO[Unit] =
    ZIO.logError(s"vk ${VkApi.SendMethod} in $ExecuteMethod: ${err.getMessage}") *>
      GameMetrics.vkError(VkApi.SendMethod, err.code.toString)

  /** Слой закрылся: очередь — закрыта, письма в ней — не ушли. */
  private def stop: UIO[Unit] =
    (open.set(false) *> queue.getAndSet(Chunk.empty)).commit.flatMap(failAll(_, Stopped))
}

object VkBatcher {

  /** Одна попытка вызвать метод — токен, метод, поля; место в окне частоты уже
    * занято. Ответ — тело целиком: у `execute` рядом с `response` лежит и
    * `execute_errors`. */
  type Post = (String, String, Map[String, String]) => Task[Json]

  /** Письмо в очереди: токен, поля `messages.send`, их размер, когда встало в
    * очередь ([[GameMetrics.now]]) и куда отдать исход. */
  private[vk] final case class Letter(
      token:  String,
      fields: Map[String, String],
      bytes:  Int,
      since:  Long,
      done:   Promise[Throwable, Json]
  )

  /** Пачки ключа `label` (метка в метриках), пока жив `Scope` слоя. Слой
    * закрылся — письма в очереди и в отправке получают [[Stopped]], а новые
    * получают её сразу. */
  def make(label: String, slot: UIO[Unit], post: Post): URIO[Scope, VkBatcher] =
    for {
      queue   <- TRef.make(Chunk.empty[Letter]).commit
      open    <- TRef.make(true).commit
      peaks   <- Ref.make(Map.empty[Long, Int])
      batcher  = new VkBatcher(label, queue, open, peaks)
      _       <- ZIO.addFinalizer(batcher.stop)
      _       <- batcher.loop(slot, post).forkScoped
    } yield batcher

  /** Сколько вызовов API ВК пускает в один `execute` (dev.vk.com/ru/method/execute). */
  val MaxLetters: Int = 25

  /** Предел пачки — сколько её письма займут в теле запроса (см. [[sizeOf]]).
    * ВК принимает тело до 256 КБ: проверено 21.11.2026 пробными `execute` —
    * 255 КБ прошли, 257 КБ получили 413 от прокси ВК (HTML, не JSON). Берём
    * 240 КБ: остальное — токен, версия и оболочка кода. Кириллица в теле
    * запроса — шесть байт на букву (`я` — `%D1%8F`): экран в 1000 букв с
    * клавиатурой — 8–10 КБ, и 25 таких влезают. Письмо больше предела уходит
    * одно, `messages.send`. */
  val MaxBytes: Int = 240 * 1024

  /** Метод пачки. */
  val ExecuteMethod = "execute"

  /** Бот останавливается: письмо не ушло и уже не уйдёт. */
  case object Stopped extends RuntimeException("VK sender stopped")

  /** Ответа на письмо в `execute` нет вовсе — ни номера, ни ошибки. */
  private[vk] val NoResult: Int = -1

  /** Сколько письмо займёт в теле запроса `execute` — не меньше, чем на деле:
    * его параметры (`a<i>_<ключ>=<значение>&`) и его вызов в коде (с запятой),
    * всё в percent-encoding ([[encoded]]); номер письма — самый длинный. */
  private[vk] def sizeOf(fields: Map[String, String]): Int = {
    val last   = MaxLetters - 1
    val params = fields.foldLeft(0) { case (n, (key, value)) => n + encoded(arg(last, key)) + encoded(value) + 2 }
    params + encoded(call(last, fields)) + 1
  }

  /** Длина строки в теле запроса (form-urlencoded) — с запасом: каждый байт
    * UTF-8, кроме незарезервированных (`A-Za-z0-9-._~`), — как три (`%XX`),
    * пробел тоже, хоть он и бывает `+`. */
  private[vk] def encoded(text: String): Int =
    text.getBytes(StandardCharsets.UTF_8).foldLeft(0)((n, b) => n + (if (unreserved(b)) 1 else 3))

  private def unreserved(b: Byte): Boolean =
    (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9') || b == '-' || b == '.' || b == '_' || b == '~'

  /** Имя параметра: поле `key` письма `i`. */
  private def arg(i: Int, key: String): String = s"a${i}_$key"

  /** Вызов `messages.send` письма `i` в коде: поля — по алфавиту, значения —
    * ссылками на параметры. */
  private def call(i: Int, fields: Map[String, String]): String =
    fields.keys.toList.sorted.map(key => s""""$key":Args.${arg(i, key)}""").mkString("API.messages.send({", ",", "})")

  /** Пачка из начала очереди `queue` и остаток. Первое письмо — всегда, даже
    * если оно одно больше [[MaxBytes]]; следом — по порядку, пока токен тот же,
    * писем не больше [[MaxLetters]], а байт (`bytes`, см. [[sizeOf]]) — не
    * больше [[MaxBytes]].
    * Остаток — в том же порядке. */
  private[vk] def cut[A](queue: Chunk[A])(token: A => String, bytes: A => Int): (Chunk[A], Chunk[A]) =
    queue.headOption match {
      case None => (Chunk.empty, Chunk.empty)
      case Some(first) =>
        @tailrec def fits(n: Int, size: Int): Int =
          if (n >= queue.size || n >= MaxLetters) n
          else {
            val next  = queue(n)
            val total = size + bytes(next)
            if (token(next) != token(first) || total > MaxBytes) n else fits(n + 1, total)
          }
        queue.splitAt(fits(1, bytes(first)))
    }

  /** Код `execute` для писем `letters` и его параметры. Код — одни вызовы
    * `messages.send` со ссылками на параметры: поле `message` письма `i` —
    * `Args.a<i>_message`. Сами тексты в код не попадают, и экранировать нечего;
    * ключи полей — наши (`message`, `user_id`, `keyboard`…). Ответ — массив
    * ответов по порядку писем. */
  private[vk] def script(letters: Chunk[Map[String, String]]): (String, Map[String, String]) = {
    val numbered = letters.zipWithIndex
    val code     = numbered.map { case (fields, i) => call(i, fields) }.mkString("return [", ",", "];")
    (code, numbered.flatMap { case (fields, i) => fields.map { case (key, value) => arg(i, key) -> value } }.toMap)
  }

  /** Исход каждого из `size` писем по ответу `execute` (`body` — тело целиком).
    * В `response` — массив по порядку писем: номер письма — ушло; `false` — не
    * ушло, и его ошибка — очередная из `execute_errors`: ВК кладёт туда ошибки
    * в порядке вызовов. `messages.send` отвечает номером, так что `false` — всегда
    * сбой. Ответа на письмо нет вовсе (массив короче, ошибок не хватило) — тоже
    * не ушло: ошибка [[NoResult]]. */
  private[vk] def outcomes(body: Json, size: Int): Chunk[Either[VkApi.VkApiError, Json]] = {
    val results = Chunk.fromIterable(body.hcursor.downField("response").values.getOrElse(Nil))
    val errors = body.hcursor.downField("execute_errors").values.getOrElse(Nil).toList.map { err =>
      VkApi.VkApiError(err.hcursor.get[Int]("error_code").getOrElse(NoResult), err.hcursor.get[String]("error_msg").getOrElse(err.noSpaces))
    }
    def missing = VkApi.VkApiError(NoResult, "VK execute: no result for the letter")
    Chunk.fromIterable(0 until size).mapAccum(errors) { (left, i) =>
      results.lift(i) match {
        case Some(result) if result != Json.False => (left, Right(result))
        case Some(_) =>
          left match {
            case err :: rest => (rest, Left(err))
            case Nil         => (Nil, Left(missing))
          }
        case None => (left, Left(missing))
      }
    }._2
  }

  private val now: UIO[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def failAll(letters: Chunk[Letter], err: Throwable): UIO[Unit] =
    ZIO.foreachDiscard(letters)(_.done.fail(err))
}
