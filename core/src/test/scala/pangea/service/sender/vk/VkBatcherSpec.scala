package pangea.service.sender.vk

import io.circe.Json
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.{Charset, HttpApp, Status, UrlForm}
import pangea.dao.sendfailure.SendFailureDao
import pangea.metrics.GameMetrics
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.vk.Community
import pangea.service.sender.ChatTarget
import pangea.service.sender.vk.config.VkConfig
import zio.interop.catz._
import zio.test._
import zio.{Chunk, Clock, Exit, Fiber, Ref, Scope, Task, UIO, ZIO, durationInt}

import java.util.concurrent.TimeUnit

object VkBatcherSpec extends ZIOSpecDefault {

  // По порядку: метрики глобальные, а часть тестов считает их приращение.
  def spec = suite("Пачки писем ВК")(cutSpec, sizeSpec, scriptSpec, outcomesSpec, loopSpec, stopSpec, apiSpec) @@
    TestAspect.sequential @@ TestAspect.timeout(60.seconds)

  // ── Поддельный ВК ───────────────────────────────────────────────────────────

  /** Запрос к ВК: метод, токен, время и письма — у `messages.send` одно, у
    * `execute` — собранные по коду и параметрам, как собрал бы ВК. */
  private final case class Hit(method: String, token: String, at: Long, letters: List[Map[String, String]])

  /** Что ВК отвечает на письмо, пришедшее методом `messages.send` или в
    * `execute`: номер или ошибка (код, текст). */
  private type Answer = (String, Map[String, String]) => Either[(Int, String), Int]

  /** Всё уходит; номер письма — его поле `message`, если оно число, иначе 1. */
  private val delivered: Answer = (_, letter) => Right(letter.get("message").flatMap(_.toIntOption).getOrElse(1))

  /** VKScript пачки, как его выполнит ВК: код — `return [` вызовы `];`, вызов —
    * `API.messages.send({"ключ":Args.имя,…})`. Код не того вида — `Left`. */
  private object VkScript {
    private val Call  = """API\.messages\.send\(\{([^{}]*)\}\)""".r
    private val Field = """"([a-z_]+)":Args\.([A-Za-z0-9_]+)""".r

    def letters(code: String, args: Map[String, String]): Either[String, List[Map[String, String]]] = {
      val calls = Call.findAllMatchIn(code).toList
      val shell = Call.replaceAllIn(code, "")
      if (calls.isEmpty || shell != calls.map(_ => "").mkString("return [", ",", "];")) Left(code)
      else
        calls.foldRight[Either[String, List[Map[String, String]]]](Right(Nil)) { (call, acc) =>
          val fields = call.group(1).split(",").toList.map {
            case Field(key, arg) => args.get(arg).map(key -> _).toRight(s"нет параметра $arg")
            case other           => Left(s"не поле: $other")
          }
          for {
            rest   <- acc
            letter <- fields.foldRight[Either[String, List[(String, String)]]](Right(Nil))((f, a) => a.flatMap(l => f.map(_ :: l)))
          } yield letter.toMap :: rest
        }
    }
  }

  private def error(code: Int, msg: String): Json =
    Json.obj("error_code" -> Json.fromInt(code), "error_msg" -> Json.fromString(msg))

  /** Тело ответа ВК на вызов `method` с полями `fields`. `execute` — по
    * письмам из кода: номер или `false`, ошибки — по порядку в
    * `execute_errors`; `whole` — ошибка всего `execute`. */
  private def answerOf(method: String, fields: Map[String, String], answer: Answer, whole: Option[(Int, String)])
      : (List[Map[String, String]], Json) =
    method match {
      case "messages.send" =>
        val letter = fields -- List("access_token", "v")
        List(letter) -> answer(method, letter).fold(
          { case (code, msg) => Json.obj("error" -> error(code, msg)) },
          id => Json.obj("response" -> Json.fromInt(id)))
      case "execute" =>
        val args = fields -- List("access_token", "v", "code")
        VkScript.letters(fields.getOrElse("code", ""), args) match {
          case Left(bad) => Nil -> Json.obj("error" -> error(12, s"Unable to compile code: $bad"))
          case Right(letters) =>
            whole match {
              case Some((code, msg)) => letters -> Json.obj("error" -> error(code, msg))
              case None =>
                val results = letters.map(answer(method, _))
                val errors  = results.collect { case Left((code, msg)) => error(code, msg).deepMerge(Json.obj("method" -> Json.fromString("messages.send"))) }
                letters -> Json.obj(
                  "response"       -> Json.arr(results.map(_.fold(_ => Json.False, Json.fromInt)): _*),
                  "execute_errors" -> Json.arr(errors: _*)
                ).mapObject(o => if (errors.isEmpty) o.remove("execute_errors") else o)
            }
        }
      case _ => Nil -> Json.obj("response" -> Json.arr(Json.obj(
        "id" -> Json.fromInt(1), "first_name" -> Json.fromString("Лео"), "last_name" -> Json.fromString("Вейн"))))
    }

  private val now: UIO[Long] = Clock.currentTime(TimeUnit.MILLISECONDS)

  /** Поддельная попытка вызова для [[VkBatcher]]: запрос — в `hits`, ответ
    * разбирает настоящий [[VkApi.bodyOf]]. */
  private def fakePost(hits: Ref[Vector[Hit]], answer: Answer = delivered, whole: Option[(Int, String)] = None): VkBatcher.Post =
    (token, method, fields) =>
      now.flatMap { t =>
        val (letters, body) = answerOf(method, fields, answer, whole)
        hits.update(_ :+ Hit(method, token, t, letters)) *> VkApi.bodyOf(Status.Ok, body.noSpaces)
      }

  private val dsl = Http4sDsl[Task]
  import dsl._

  private def methodOf(path: String): String = path.split('/').lastOption.getOrElse("")

  /** Поддельный ВК по HTTP — для [[VkApi]] целиком. */
  private def fakeVk(hits: Ref[Vector[Hit]], answer: Answer = delivered, whole: Option[(Int, String)] = None): Client[Task] =
    Client.fromHttpApp(HttpApp[Task] { req =>
      req.as[UrlForm].flatMap { form =>
        val fields = form.values.map { case (k, vs) => k -> vs.toList.mkString }
        val method = methodOf(req.uri.path.renderString)
        val (letters, body) = answerOf(method, fields, answer, whole)
        now.flatMap(t => hits.update(_ :+ Hit(method, fields.getOrElse("access_token", ""), t, letters))) *> Ok(body.noSpaces)
      }
    })

  // ── Помощники ───────────────────────────────────────────────────────────────

  private val limit = VkRateLimit.PerSecond

  /** Окно частоты, в котором уже нет места до 1000-й миллисекунды. */
  private val fullWindow: UIO[VkRateLimit] =
    VkRateLimit.make(Community.Main).tap(window => ZIO.foreachDiscard(1 to limit)(_ => window(ZIO.unit)))

  /** Письмо игроку: номер письма — `n` (так его вернёт поддельный ВК). */
  private def letter(n: Int, user: String = "7"): Map[String, String] =
    Map("message" -> n.toString, "user_id" -> user, "random_id" -> (1000 + n).toString)

  /** Дождаться, пока файбер встанет: письмо в очереди и ждёт исхода. */
  private def parked(fiber: Fiber.Runtime[_, _]) =
    fiber.status.repeatUntil {
      case _: Fiber.Status.Suspended => true
      case _                         => false
    }

  /** Дождаться, пока файбер встанет или кончится: письмо в очереди или уже
    * ушло. Письма, что уйдут сразу, не ждут часов — [[parked]] ждал бы их
    * вечно. */
  private def settled(fiber: Fiber.Runtime[_, _]) =
    fiber.status.repeatUntil {
      case _: Fiber.Status.Suspended | Fiber.Status.Done => true
      case _                                              => false
    }

  /** Отправить письма по одному, каждое — в очередь вслед за предыдущим. */
  private def queued(batcher: VkBatcher, token: String, letters: Seq[Map[String, String]]) =
    ZIO.foreach(letters)(l => batcher.send(token, l).either.fork.tap(parked))

  /** Пачки уходят параллельно, и к ВК могут прийти не в том порядке, в каком
    * собраны, — раскладываем их по первому письму (номер — поле `message`). */
  private def inOrder(hits: Vector[Hit]) =
    hits.sortBy(_.letters.headOption.flatMap(_.get("message")).flatMap(_.toIntOption).getOrElse(Int.MaxValue))

  private def methods(hits: Vector[Hit]) = inOrder(hits).map(_.method)
  private def sizes(hits: Vector[Hit])   = inOrder(hits).map(_.letters.size)
  private def messages(hits: Vector[Hit]) = inOrder(hits).flatMap(_.letters).flatMap(_.get("message"))

  // ── Нарезка пачки ───────────────────────────────────────────────────────────

  private def cut(queue: (String, Int)*) = VkBatcher.cut(Chunk.fromIterable(queue))(_._1, _._2)

  private def cutSpec = suite("Пачка из очереди")(
    test("пустая очередь — пустая пачка") {
      assertTrue(cut() == (Chunk.empty, Chunk.empty))
    },
    test("одно письмо — пачка из одного") {
      assertTrue(cut("t" -> 10) == (Chunk("t" -> 10), Chunk.empty))
    },
    test("30 писем — 25 в пачке, 5 остаются в том же порядке") {
      val queue        = (1 to 30).map(i => "t" -> i)
      val (batch, rest) = cut(queue: _*)
      assertTrue(batch.map(_._2) == Chunk.fromIterable(1 to 25), rest.map(_._2) == Chunk.fromIterable(26 to 30))
    },
    test("ровно 25 — все в пачке") {
      val (batch, rest) = cut((1 to VkBatcher.MaxLetters).map(i => "t" -> i): _*)
      assertTrue(batch.size == VkBatcher.MaxLetters, rest.isEmpty)
    },
    test("другой токен обрывает пачку: он и всё после — в следующую") {
      assertTrue(cut("t" -> 1, "t" -> 2, "g" -> 3, "t" -> 4) == (Chunk("t" -> 1, "t" -> 2), Chunk("g" -> 3, "t" -> 4)))
    },
    test("по байтам: до предела включительно — в пачке, сверх — в следующую") {
      val half = VkBatcher.MaxBytes / 2
      assertTrue(cut("t" -> half, "t" -> half, "t" -> 1) == (Chunk("t" -> half, "t" -> half), Chunk("t" -> 1)))
    },
    test("письмо больше предела — уходит одно") {
      val big = VkBatcher.MaxBytes * 2
      assertTrue(cut("t" -> big, "t" -> 1) == (Chunk("t" -> big), Chunk("t" -> 1)))
    },
    test("большое письмо не первым — ждёт следующей пачки, порядок не меняется") {
      val big = VkBatcher.MaxBytes
      assertTrue(cut("t" -> 1, "t" -> big, "t" -> 2) == (Chunk("t" -> 1), Chunk("t" -> big, "t" -> 2)))
    }
  )

  /** Тело запроса execute с письмами `letters` — ровно как его соберёт
    * [[VkApi.attempt]] (form-urlencoded, http4s), в байтах. */
  private def wire(letters: Chunk[Map[String, String]], token: String): Int = {
    val (code, args) = VkBatcher.script(letters)
    val form         = UrlForm((args ++ Map("code" -> code, "access_token" -> token, "v" -> VkApi.Version)).toSeq: _*)
    UrlForm.encodeString(Charset.`UTF-8`)(form).getBytes("UTF-8").length
  }

  /** Письмо с текстом `text`: всё, что бывает в экранах, — кириллица, эмодзи,
    * пробелы, переносы, кавычки, звёздочки — и клавиатура. */
  private def screen(n: Int, text: String) = letter(n) ++ Map(
    "message"  -> text,
    "keyboard" -> s"""{"buttons":[[{"action":{"type":"text","label":"⚔️ Бой * $n ~ «вперёд»"},"color":"primary"}]],"one_time":false}""")

  /** Токен сообщества ВК — около 220 символов; берём с запасом. */
  private val longToken = "x" * 256

  private def sizeSpec = suite("Размер письма в запросе")(
    test("латиница, цифры и -._~ — по байту, кириллица — шесть байт на букву, эмодзи 🦴 — двенадцать") {
      def extra(text: String) = VkBatcher.sizeOf(Map("message" -> text)) - VkBatcher.sizeOf(Map("message" -> ""))
      assertTrue(extra("abcXYZ019-._~") == 13, extra("абв") == 18, extra("🦴") == 12, extra(" ") == 3, extra("*") == 3)
    },
    test("оценка письма — не меньше, чем оно займёт на деле: сумма по 25 письмам покрывает тело запроса") {
      val letters = Chunk.fromIterable((0 until VkBatcher.MaxLetters).map(n =>
        screen(n, s"Экран $n: ⚔️ бой с «волком» — 🦴 x${n * 7}\nвторая строка * звёздочки ~ тильда \"кавычки\" ") ))
      val total = letters.map(VkBatcher.sizeOf).sum
      val real  = wire(letters, token = "")
      // Сверх писем в теле — только оболочка кода, code=, access_token= и v.
      assertTrue(total + 64 >= real, total < real * 2)
    },
    test("пачка на пределе — тело запроса не больше 255 КБ, что ВК принял 21.11, даже с длинным токеном") {
      val queue = Chunk.fromIterable((1 to 60).map(n => screen(n, ("Длинный экран лавки " * 200).take(1500 + n * 37))))
      val (batch, rest) = VkBatcher.cut(queue)(_ => "t", VkBatcher.sizeOf)
      val used = batch.map(VkBatcher.sizeOf).sum
      assertTrue(batch.nonEmpty, rest.nonEmpty) &&
      assertTrue(wire(batch, longToken) <= 255 * 1024) &&
      assertTrue(used + rest.headOption.map(VkBatcher.sizeOf).getOrElse(0) > VkBatcher.MaxBytes)
    },
    test("25 экранов по 1000 букв с клавиатурой — одной пачкой") {
      val queue = Chunk.fromIterable((1 to VkBatcher.MaxLetters).map(n => screen(n, "я" * 1000)))
      assertTrue(VkBatcher.cut(queue)(_ => "t", VkBatcher.sizeOf)._1.size == VkBatcher.MaxLetters)
    },
    test("самое длинное письмо ВК (4096 букв) с клавиатурой меньше предела — уходит в пачке, а не одно") {
      assertTrue(VkBatcher.sizeOf(screen(1, "я" * 4096)) < VkBatcher.MaxBytes / 4)
    }
  )

  // ── Код execute ─────────────────────────────────────────────────────────────

  private def scriptSpec = suite("Код execute")(
    test("по вызову messages.send на письмо, по порядку; поля — по алфавиту; ответ — массив") {
      val (code, args) = VkBatcher.script(Chunk(
        Map("user_id" -> "7", "message" -> "привет", "random_id" -> "5"),
        Map("peer_id" -> "2000000001", "message" -> "итоги", "random_id" -> "6", "keyboard" -> "{}")))
      assertTrue(code ==
        """return [API.messages.send({"message":Args.a0_message,"random_id":Args.a0_random_id,"user_id":Args.a0_user_id}),""" +
          """API.messages.send({"keyboard":Args.a1_keyboard,"message":Args.a1_message,"peer_id":Args.a1_peer_id,"random_id":Args.a1_random_id})];""") &&
      assertTrue(args == Map(
        "a0_message" -> "привет", "a0_random_id" -> "5", "a0_user_id" -> "7",
        "a1_keyboard" -> "{}", "a1_message" -> "итоги", "a1_peer_id" -> "2000000001", "a1_random_id" -> "6"))
    },
    test("тексты писем — только в параметрах: кавычки, скобки и Args в тексте код не трогают") {
      val nasty = List(
        """"}); API.wall.post({"message":"взлом""",
        "строка\nвторая \\ \"кавычки\" ' ` ${x}",
        "Args.a0_message + API.messages.delete()",
        """{"buttons":[[{"action":{"type":"text","label":"⚔️ Бой"}}]],"one_time":false}""")
      val letters      = Chunk.fromIterable(nasty.zipWithIndex.map { case (text, i) => Map("message" -> text, "keyboard" -> text, "user_id" -> i.toString) })
      val (code, args) = VkBatcher.script(letters)
      assertTrue(nasty.forall(text => !code.contains(text))) &&
      assertTrue(nasty.zipWithIndex.forall { case (text, i) => args.get(s"a${i}_message").contains(text) && args.get(s"a${i}_keyboard").contains(text) }) &&
      assertTrue(code.forall(c => c < 128)) &&
      assertTrue(VkScript.letters(code, args) == Right(letters.toList))
    },
    test("параметры не задевают поля самого execute: code, access_token, v") {
      val (_, args) = VkBatcher.script(Chunk(letter(1), letter(2)))
      assertTrue(args.keySet.intersect(Set("code", "access_token", "v", "lang", "test_mode")).isEmpty) &&
      assertTrue(args.keySet.forall(_.matches("a[0-9]+_[a-z_]+")))
    },
    test("клавиатура, вложение, пересылка — каждое своим параметром, у писем без них их нет") {
      val withAll = letter(1) ++ Map("keyboard" -> "{}", "attachment" -> "photo1_2", "forward_messages" -> "77")
      val (_, args) = VkBatcher.script(Chunk(withAll, letter(2)))
      assertTrue(Set("a0_keyboard", "a0_attachment", "a0_forward_messages").subsetOf(args.keySet)) &&
      assertTrue(!args.keySet.exists(k => k.startsWith("a1_") && Set("keyboard", "attachment", "forward_messages")(k.drop(3))))
    },
    test("25 писем: ВК по коду и параметрам соберёт ровно их, в том же порядке") {
      val letters      = Chunk.fromIterable((1 to VkBatcher.MaxLetters).map(i => letter(i, user = (100 + i).toString)))
      val (code, args) = VkBatcher.script(letters)
      assertTrue(VkScript.letters(code, args) == Right(letters.toList)) &&
      assertTrue(args.size == letters.map(_.size).sum)
    },
    test("длина кода от текстов не зависит: 25 писем по 4000 букв — тот же код, что с пустыми, меньше 4 КБ") {
      def code(text: String) =
        VkBatcher.script(Chunk.fromIterable((1 to VkBatcher.MaxLetters).map(i => letter(i) ++ Map("message" -> text, "keyboard" -> text))))._1
      assertTrue(code("я" * 4000) == code(""), code("").length < 4 * 1024)
    }
  )

  // ── Ответ execute ───────────────────────────────────────────────────────────

  private def body(json: String): Json = io.circe.parser.parse(json).getOrElse(Json.Null)

  private def outcomesSpec = suite("Ответ execute")(
    test("все ушли — каждому свой номер") {
      assertTrue(VkBatcher.outcomes(body("""{"response":[11,12,13]}"""), 3) ==
        Chunk(Right(Json.fromInt(11)), Right(Json.fromInt(12)), Right(Json.fromInt(13))))
    },
    test("false — не ушло; ошибка — очередная из execute_errors") {
      val json = body(
        """{"response":[11,false,13,false],"execute_errors":[
          |{"method":"messages.send","error_code":901,"error_msg":"Can't send messages for users without permission"},
          |{"method":"messages.send","error_code":902,"error_msg":"privacy"}]}""".stripMargin)
      assertTrue(VkBatcher.outcomes(json, 4) == Chunk(
        Right(Json.fromInt(11)),
        Left(VkApi.VkApiError(901, "Can't send messages for users without permission")),
        Right(Json.fromInt(13)),
        Left(VkApi.VkApiError(902, "privacy"))))
    },
    // Ответ настоящего ВК 21.11.2026 на пробную пачку из трёх писем админам:
    // два ушли, у третьего — сломанная клавиатура. Лишние поля ошибки (view,
    // i18n_title) разбору не мешают.
    test("настоящий ответ ВК: два номера, а третьему — его ошибка 100 из execute_errors") {
      val json = body(
        """{"response":[18296811,18296814,false],"execute_errors":[{"method":"messages.send","error_code":100,""" +
          """"error_msg":"One of the parameters specified was missing or invalid: keyboard contains incorrect JSON","view":null,"i18n_title":null}]}""")
      assertTrue(VkBatcher.outcomes(json, 3) == Chunk(
        Right(Json.fromInt(18296811)),
        Right(Json.fromInt(18296814)),
        Left(VkApi.VkApiError(100, "One of the parameters specified was missing or invalid: keyboard contains incorrect JSON"))))
    },
    test("ошибок меньше, чем false, — недостающим «нет ответа»") {
      val json = body("""{"response":[false,false],"execute_errors":[{"method":"messages.send","error_code":10,"error_msg":"x"}]}""")
      assertTrue(VkBatcher.outcomes(json, 2).map(_.left.map(_.code)) == Chunk(Left(10), Left(VkBatcher.NoResult)))
    },
    test("ответ короче пачки — остальным «нет ответа»") {
      assertTrue(VkBatcher.outcomes(body("""{"response":[5]}"""), 3).map(_.left.map(_.code)) ==
        Chunk(Right(Json.fromInt(5)), Left(VkBatcher.NoResult), Left(VkBatcher.NoResult)))
    },
    test("response не массив или его нет — никому нет ответа") {
      assertTrue(VkBatcher.outcomes(body("""{"response":1}"""), 2).forall(_.isLeft)) &&
      assertTrue(VkBatcher.outcomes(body("""{}"""), 2).forall(_.isLeft)) &&
      assertTrue(VkBatcher.outcomes(body("""{"response":1}"""), 2).size == 2)
    },
    test("ошибка без кода и текста — код «нет ответа», текст — сама ошибка") {
      val json = body("""{"response":[false],"execute_errors":[{"method":"messages.send"}]}""")
      assertTrue(VkBatcher.outcomes(json, 1) == Chunk(Left(VkApi.VkApiError(VkBatcher.NoResult, """{"method":"messages.send"}"""))))
    }
  )

  // ── Цикл пачек ──────────────────────────────────────────────────────────────

  private def loopSpec = suite("Цикл пачек")(
    test("в окне есть место — каждое письмо уходит само, messages.send, без execute") {
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        window  <- VkRateLimit.make(Community.Main)
        batcher <- VkBatcher.make("spec", window.slot, fakePost(hits))
        ids     <- ZIO.foreach(1 to 5)(n => batcher.send("t", letter(n)))
        seen    <- hits.get
      } yield assertTrue(ids == (1 to 5).map(Json.fromInt)) &&
              assertTrue(methods(seen) == Vector.fill(5)("messages.send")) &&
              assertTrue(seen.flatMap(_.letters) == (1 to 5).map(n => letter(n)).toVector) &&
              assertTrue(seen.forall(h => h.token == "t" && h.at == 0L))
    },
    test("окно полно — письма копятся и уходят одним execute, когда место появится; каждому — свой номер") {
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        window  <- fullWindow
        batcher <- VkBatcher.make("spec", window.slot, fakePost(hits))
        fibers  <- queued(batcher, "t", (1 to 10).map(n => letter(n)))
        before  <- hits.get
        _       <- TestClock.adjust(1.second)
        results <- ZIO.foreach(fibers)(_.join)
        seen    <- hits.get
      } yield assertTrue(before.isEmpty) &&
              assertTrue(results == (1 to 10).map(n => Right(Json.fromInt(n)))) &&
              assertTrue(methods(seen) == Vector("execute"), seen.map(_.at) == Vector(1000L)) &&
              assertTrue(seen.flatMap(_.letters) == (1 to 10).map(n => letter(n)).toVector)
    },
    test("60 писем при полном окне — три запроса: 25, 25 и 10, по порядку прихода") {
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        window  <- fullWindow
        batcher <- VkBatcher.make("spec", window.slot, fakePost(hits))
        fibers  <- queued(batcher, "t", (1 to 60).map(n => letter(n)))
        _       <- TestClock.adjust(1.second)
        results <- ZIO.foreach(fibers)(_.join)
        seen    <- hits.get
      } yield assertTrue(sizes(seen) == Vector(25, 25, 10), methods(seen).forall(_ == "execute")) &&
              assertTrue(messages(seen) == (1 to 60).map(_.toString).toVector) &&
              assertTrue(results.forall(_.isRight))
    },
    test("пачка — одно место в окне: 26 писем при полном окне — два запроса, а не 26") {
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        window  <- fullWindow
        batcher <- VkBatcher.make("spec", window.slot, fakePost(hits))
        fibers  <- queued(batcher, "t", (1 to 26).map(n => letter(n)))
        _       <- TestClock.adjust(1.second)
        _       <- ZIO.foreachDiscard(fibers)(_.join)
        seen    <- hits.get
        peak    <- window.peak
      } yield assertTrue(sizes(seen) == Vector(25, 1), methods(seen) == Vector("execute", "messages.send")) &&
              assertTrue(peak == limit)
    },
    test("500 писем разом: все уходят за секунду, в любую секунду — не больше 20 запросов, в пачке — не больше 25") {
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        window  <- VkRateLimit.make(Community.Main)
        batcher <- VkBatcher.make("spec", window.slot, fakePost(hits))
        fibers  <- ZIO.foreach(1 to 500)(n => batcher.send("t", letter(n)).either.fork)
        // Часы — только когда все 500 в очереди: опоздавшее письмо ушло бы на
        // пятой секунде, хотя ждать ему было нечего.
        _       <- ZIO.foreachDiscard(fibers)(settled)
        _       <- TestClock.adjust(5.seconds)
        results <- ZIO.foreach(fibers)(_.join)
        seen    <- hits.get
        times    = seen.map(_.at)
      } yield assertTrue(results.forall(_.isRight), results.size == 500) &&
              assertTrue(seen.map(_.letters.size).sum == 500) &&
              assertTrue(seen.flatMap(_.letters).flatMap(_.get("message")).sorted == (1 to 500).map(_.toString).sorted.toVector) &&
              assertTrue(times.forall(_ <= 1000L), seen.size <= 2 * limit) &&
              assertTrue(times.forall(t => times.count(u => u >= t && u < t + 1000L) <= limit)) &&
              assertTrue(seen.forall(_.letters.size <= VkBatcher.MaxLetters))
    },
    test("одно письмо из пачки не ушло — ошибку получает только оно, с кодом ВК") {
      val answer: Answer = (m, l) => if (l.get("message").contains("2")) Left(901 -> "Can't send messages") else delivered(m, l)
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        window  <- fullWindow
        batcher <- VkBatcher.make("spec", window.slot, fakePost(hits, answer))
        fibers  <- queued(batcher, "t", (1 to 3).map(n => letter(n)))
        _       <- TestClock.adjust(1.second)
        results <- ZIO.foreach(fibers)(_.join)
      } yield assertTrue(results == Seq(
        Right(Json.fromInt(1)), Left(VkApi.VkApiError(901, "Can't send messages")), Right(Json.fromInt(3))))
    },
    test("не прошёл весь execute — его ошибку получает каждое письмо пачки") {
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        window  <- fullWindow
        batcher <- VkBatcher.make("spec", window.slot, fakePost(hits, whole = Some(12 -> "Unable to compile code")))
        fibers  <- queued(batcher, "t", (1 to 4).map(n => letter(n)))
        _       <- TestClock.adjust(1.second)
        results <- ZIO.foreach(fibers)(_.join)
      } yield assertTrue(results.forall(_ == Left(VkApi.VkApiError(12, "Unable to compile code"))), results.size == 4)
    },
    test("одиночное письмо не ушло — его ошибка как есть") {
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        window  <- VkRateLimit.make(Community.Main)
        batcher <- VkBatcher.make("spec", window.slot, fakePost(hits, (_, _) => Left(914 -> "Message is too long")))
        result  <- batcher.send("t", letter(1)).either
      } yield assertTrue(result == Left(VkApi.VkApiError(914, "Message is too long")))
    },
    test("ВК не ответил (сеть) — ошибка сети у каждого письма пачки") {
      val down: VkBatcher.Post = (_, _, _) => ZIO.fail(new RuntimeException("connection reset"))
      for {
        window  <- fullWindow
        batcher <- VkBatcher.make("spec", window.slot, down)
        fibers  <- queued(batcher, "t", (1 to 3).map(n => letter(n)))
        _       <- TestClock.adjust(1.second)
        results <- ZIO.foreach(fibers)(_.join)
      } yield assertTrue(results.forall(_.left.exists(_.getMessage == "connection reset")))
    },
    test("письма разных токенов — разными запросами, каждый своим токеном") {
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        window  <- fullWindow
        batcher <- VkBatcher.make("spec", window.slot, fakePost(hits))
        a       <- queued(batcher, "t", List(letter(1), letter(2)))
        b       <- queued(batcher, "g", List(letter(3), letter(4)))
        c       <- queued(batcher, "t", List(letter(5)))
        _       <- TestClock.adjust(1.second)
        _       <- ZIO.foreachDiscard(a ++ b ++ c)(_.join)
        seen    <- hits.get
      } yield assertTrue(inOrder(seen).map(h => h.token -> h.letters.flatMap(_.get("message"))) ==
        Vector("t" -> List("1", "2"), "g" -> List("3", "4"), "t" -> List("5")))
    },
    test("большие письма не влезают в одну пачку — делятся по байтам, порядок прежний") {
      val big = "я" * (VkBatcher.MaxBytes / 2 / 6 + 1) // больше половины предела: кириллица в запросе — шесть байт
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        window  <- fullWindow
        batcher <- VkBatcher.make("spec", window.slot, fakePost(hits))
        fibers  <- queued(batcher, "t", (1 to 3).map(n => letter(n) ++ Map("keyboard" -> big)))
        _       <- TestClock.adjust(1.second)
        _       <- ZIO.foreachDiscard(fibers)(_.join)
        seen    <- hits.get
      } yield assertTrue(sizes(seen) == Vector(1, 1, 1)) &&
              assertTrue(messages(seen) == Vector("1", "2", "3"))
    },
    test("ожидание письма в очереди — в метрике у каждого письма; пик очереди держится 20 секунд, потом забыт") {
      def waits  = GameMetrics.vkBatchWaitSeconds.tagged("community", "spec-wait").value.map(_.count)
      def inQueue = GameMetrics.vkBatchQueued.tagged("community", "spec-wait").value.map(_.value)
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        before  <- waits
        window  <- fullWindow
        batcher <- VkBatcher.make("spec-wait", window.slot, fakePost(hits))
        fibers  <- queued(batcher, "t", (1 to 10).map(n => letter(n)))
        _       <- batcher.publish
        stuck   <- inQueue
        early   <- waits
        _       <- TestClock.adjust(1.second)
        _       <- ZIO.foreachDiscard(fibers)(_.join)
        _       <- batcher.publish
        kept    <- inQueue
        after   <- waits
        _       <- TestClock.adjust(VkRateLimit.PeakSpan)
        _       <- batcher.publish
        gone    <- inQueue
      } yield assertTrue(stuck == 10.0, early == before, after - before == 10L, kept == 10.0, gone == 0.0)
    },
    test("в метрике — по значению на запрос, сумма — сколько писем") {
      def state = GameMetrics.vkBatchLetters.tagged("community", "spec-metric").value
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        before  <- state
        window  <- fullWindow
        batcher <- VkBatcher.make("spec-metric", window.slot, fakePost(hits))
        fibers  <- queued(batcher, "t", (1 to 10).map(n => letter(n)))
        _       <- TestClock.adjust(1.second)
        _       <- ZIO.foreachDiscard(fibers)(_.join)
        _       <- batcher.send("t", letter(11))
        after   <- state
      } yield assertTrue(after.count - before.count == 2L, after.sum - before.sum == 11.0)
    }
  )

  // ── Остановка ───────────────────────────────────────────────────────────────

  private def stopSpec = suite("Остановка")(
    test("слой закрылся — письма в очереди не ушли и получили Stopped, новые — сразу Stopped") {
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        scope   <- Scope.make
        window  <- fullWindow
        batcher <- scope.extend[Any](VkBatcher.make("spec", window.slot, fakePost(hits)))
        fibers  <- queued(batcher, "t", (1 to 3).map(n => letter(n)))
        _       <- scope.close(Exit.unit)
        results <- ZIO.foreach(fibers)(_.join)
        late    <- batcher.send("t", letter(4)).either
        _       <- TestClock.adjust(1.second)
        seen    <- hits.get
      } yield assertTrue(results.forall(_ == Left(VkBatcher.Stopped)), late == Left(VkBatcher.Stopped), seen.isEmpty)
    },
    test("слой закрылся посреди отправки — письма этой пачки получают Stopped, а не висят") {
      val hang: VkBatcher.Post = (_, _, _) => ZIO.never
      for {
        scope   <- Scope.make
        window  <- VkRateLimit.make(Community.Main)
        batcher <- scope.extend[Any](VkBatcher.make("spec", window.slot, hang))
        fiber   <- batcher.send("t", letter(1)).either.fork
        _       <- parked(fiber)
        _       <- scope.close(Exit.unit)
        result  <- fiber.join
      } yield assertTrue(result == Left(VkBatcher.Stopped))
    }
  )

  // ── VkApi целиком ───────────────────────────────────────────────────────────

  private val config = VkConfig("t", guildsToken = Some("g"))

  private def player(n: Int) = User(UserId(n.toLong), VkId(n.toString), TelegramId("tg"))

  /** Сбои отправки: игрок, код и число попыток. */
  private final class Failures(seen: Ref[Vector[(String, Option[Int], Int)]]) extends SendFailureDao {
    override def record(userId: Option[UserId], vkId: String, messageText: String, keyboardJson: Option[String],
                        errorCode: Option[Int], errorMessage: String, attempts: Int, createdAt: Long): Task[Unit] =
      seen.update(_ :+ ((vkId, errorCode, attempts)))
  }

  private object NoFailures extends SendFailureDao {
    override def record(userId: Option[UserId], vkId: String, messageText: String, keyboardJson: Option[String],
                        errorCode: Option[Int], errorMessage: String, attempts: Int, createdAt: Long): Task[Unit] =
      ZIO.unit
  }

  /** Занять окно основного ключа до 1000-й миллисекунды: 20 запросов имени. */
  private def fill(api: VkApi) = ZIO.foreachDiscard(1 to limit)(n => api.getName(player(n)))

  private def letters(hits: Vector[Hit], method: String) = hits.filter(_.method == method).flatMap(_.letters)

  private def errorsOf(method: String, code: String) =
    GameMetrics.vkApiErrors.tagged("method", method).tagged("code", code).value.map(_.count)

  private def apiSpec = suite("VkApi с пачками")(
    test("сорок игроков разом при полном окне — два execute, у каждого своё письмо и свой random_id") {
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        api     <- VkApi.make(fakeVk(hits), config, NoFailures)
        _       <- fill(api)
        fibers  <- ZIO.foreach(1 to 40)(n => api.sendMessage(player(n), s"экран $n", Nil, None).fork)
        _       <- TestClock.adjust(1.second)
        _       <- ZIO.foreachDiscard(fibers)(_.join)
        seen    <- hits.get
        sent     = letters(seen, "execute")
      } yield assertTrue(seen.count(_.method == "users.get") == limit) &&
              assertTrue(seen.filter(_.method == "execute").map(_.letters.size).sorted == Vector(15, 25)) &&
              assertTrue(seen.filter(_.method == "execute").forall(h => h.token == "t" && h.at == 1000L)) &&
              assertTrue(!seen.exists(_.method == "messages.send")) &&
              assertTrue(sent.flatMap(l => l.get("user_id").zip(l.get("message"))).toSet ==
                (1 to 40).map(n => n.toString -> s"экран $n").toSet, sent.size == 40) &&
              assertTrue(sent.flatMap(_.get("random_id")).distinct.size == 40)
    },
    test("клавиатура, вложение и пересылка админу едут в пачке своими полями") {
      import pangea.model.vk.keyboard.Keyboard
      val keys = Keyboard(buttons = Nil, one_time = false, inline = true)
      for {
        hits   <- Ref.make(Vector.empty[Hit])
        api    <- VkApi.make(fakeVk(hits), config, NoFailures)
        _      <- fill(api)
        a      <- api.sendMessage(player(1), "с кнопками", Nil, Some(keys)).fork
        _      <- parked(a)
        b      <- api.forwardToUser(VkId("99"), "игрок просит /home", Some(555L)).fork
        _      <- parked(b)
        _      <- TestClock.adjust(1.second)
        _      <- a.join *> b.join
        seen   <- hits.get
        sent    = letters(seen, "execute")
      } yield assertTrue(sent.size == 2) &&
              assertTrue(sent.headOption.flatMap(_.get("keyboard")).contains(keys.getJson)) &&
              assertTrue(sent.lift(1).exists(l => l.get("forward_messages").contains("555") && l.get("user_id").contains("99")))
    },
    test("письмо, не принятое в пачке, повторяется отдельным messages.send с тем же random_id") {
      // В пачке письмо игрока 13 не ушло (сбой ВК), отдельным messages.send — уходит.
      val answer: Answer = (m, l) =>
        if (m == "execute" && l.get("user_id").contains("13")) Left(10 -> "Internal server error") else delivered(m, l)
      for {
        hits   <- Ref.make(Vector.empty[Hit])
        api    <- VkApi.make(fakeVk(hits, answer), config, NoFailures)
        _      <- fill(api)
        fibers <- ZIO.foreach(List(12, 13, 14))(n => api.sendMessage(player(n), s"экран $n", Nil, None).fork)
        _      <- TestClock.adjust(5.seconds)
        _      <- ZIO.foreachDiscard(fibers)(_.join)
        seen   <- hits.get
        batch   = letters(seen, "execute")
        alone   = letters(seen, "messages.send")
      } yield assertTrue(batch.size == 3, alone.size == 1) &&
              assertTrue(alone.flatMap(_.get("user_id")) == Vector("13")) &&
              assertTrue(alone.flatMap(_.get("random_id")) == batch.filter(_.get("user_id").contains("13")).flatMap(_.get("random_id")))
    },
    test("ВК не принял execute целиком (код 12) — каждое письмо уходит своим messages.send, и все доходят") {
      for {
        hits   <- Ref.make(Vector.empty[Hit])
        api    <- VkApi.make(fakeVk(hits, whole = Some(12 -> "Unable to compile code")), config, NoFailures)
        _      <- fill(api)
        fibers <- ZIO.foreach(1 to 5)(n => api.sendMessage(player(n), s"экран $n", Nil, None).either.fork)
        _      <- TestClock.adjust(10.seconds)
        done   <- ZIO.foreach(fibers)(_.join)
        seen   <- hits.get
        batch   = letters(seen, "execute")
        alone   = letters(seen, "messages.send")
      } yield assertTrue(done.forall(_.isRight)) &&
              assertTrue(seen.count(_.method == "execute") == 1, batch.size == 5) &&
              assertTrue(alone.size == 5, alone.toSet == batch.toSet)
    },
    test("закрытые сообщения в пачке: повтор через 5 секунд отдельно, не дошло — в send_failures с двумя попытками") {
      val answer: Answer = (m, l) => if (l.get("user_id").contains("66")) Left(901 -> "Can't send messages") else delivered(m, l)
      for {
        hits    <- Ref.make(Vector.empty[Hit])
        failed  <- Ref.make(Vector.empty[(String, Option[Int], Int)])
        before  <- errorsOf("messages.send", "901")
        api     <- VkApi.make(fakeVk(hits, answer), config, new Failures(failed))
        _       <- fill(api)
        fibers  <- ZIO.foreach(List(65, 66, 67))(n => api.sendMessage(player(n), s"экран $n", Nil, None).either.fork)
        _       <- TestClock.adjust(1.second)
        early   <- hits.get
        _       <- TestClock.adjust(10.seconds)
        results <- ZIO.foreach(fibers)(_.join)
        seen    <- hits.get
        after   <- errorsOf("messages.send", "901")
        journal <- failed.get
      } yield assertTrue(results.map(_.isRight) == List(true, false, true)) &&
              assertTrue(results.lift(1).exists(_.left.exists(_ == VkApi.VkApiError(901, "Can't send messages")))) &&
              assertTrue(!early.exists(_.method == "messages.send")) &&
              assertTrue(seen.filter(_.method == "messages.send").map(_.at) == Vector(6000L)) &&
              assertTrue(journal == Vector(("66", Some(901), 2))) &&
              assertTrue(after - before == 2.0)
    },
    test("письма основного и гильдейского сообществ не смешиваются: у каждого ключа свои пачки") {
      val guild = ChatTarget.Peer(2000000005L, Community.Guilds)
      for {
        hits   <- Ref.make(Vector.empty[Hit])
        api    <- VkApi.make(fakeVk(hits), config, NoFailures)
        main   <- ZIO.foreach(1 to 30)(n => api.sendMessage(player(n), s"экран $n", Nil, None).fork)
        chats  <- ZIO.foreach(1 to 30)(n => api.sendToChat(guild, s"гильдии $n").fork)
        _      <- TestClock.adjust(5.seconds)
        _      <- ZIO.foreachDiscard(main ++ chats)(_.join)
        seen   <- hits.get
        byKey   = seen.groupBy(_.token).view.mapValues(_.flatMap(_.letters)).toMap
      } yield assertTrue(byKey.keySet == Set("t", "g")) &&
              assertTrue(byKey.get("t").exists(ls => ls.size == 30 && ls.forall(_.contains("user_id")))) &&
              assertTrue(byKey.get("g").exists(ls => ls.size == 30 && ls.forall(_.get("peer_id").contains("2000000005")))) &&
              assertTrue(seen.forall(_.letters.size <= VkBatcher.MaxLetters))
    },
    test("6 на execute — окно ключа закрыто на секунду, письма пачки уходят повтором после паузы") {
      for {
        hits   <- Ref.make(Vector.empty[Hit])
        execs  <- Ref.make(0)
        vk      = fakeVk(hits)
        // Первый execute — «слишком часто», прочее — как обычно.
        api    <- VkApi.make(Client.fromHttpApp(HttpApp[Task] { req =>
                    if (methodOf(req.uri.path.renderString) != "execute") vk.toHttpApp.run(req)
                    else execs.getAndUpdate(_ + 1).flatMap {
                      case 0 =>
                        now.flatMap(t => hits.update(_ :+ Hit("execute-6", "t", t, Nil))) *>
                          Ok("""{"error":{"error_code":6,"error_msg":"Too many requests per second"}}""")
                      case _ => vk.toHttpApp.run(req)
                    }
                  }), config, NoFailures)
        _      <- fill(api)
        fibers <- ZIO.foreach(1 to 3)(n => api.sendMessage(player(n), s"экран $n", Nil, None).fork)
        _      <- TestClock.adjust(10.seconds)
        _      <- ZIO.foreachDiscard(fibers)(_.join)
        seen   <- hits.get
        retry   = seen.filter(_.method == "messages.send")
      } yield assertTrue(seen.filter(_.method == "execute-6").map(_.at) == Vector(1000L)) &&
              assertTrue(retry.size == 3, retry.forall(_.at >= 2000L))
    }
  )
}
