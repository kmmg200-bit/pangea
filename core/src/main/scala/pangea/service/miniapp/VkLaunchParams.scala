package pangea.service.miniapp

import org.http4s.Query
import pangea.model.user.VkId

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/** Параметры запуска VK Mini App. ВК дописывает их к адресу приложения и
  * подписывает защищённым ключом приложения, а приложение пересылает их нам с
  * каждым запросом. Сошлась подпись — это и правда игрок `vk_user_id`: без
  * ключа её не подделать.
  *
  * Подпись — HMAC-SHA256 от параметров `vk_*`, упорядоченных по имени и
  * собранных в строку запроса, в base64url без `=` на конце (документация ВК,
  * «Подпись параметров запуска»). */
object VkLaunchParams {

  /** Сколько годятся параметры: в `vk_ts` ВК ставит время запуска. Утёкшая
    * ссылка с ними дольше суток не служит; кто держит приложение открытым
    * дольше, просто откроет его заново. */
  val TtlSeconds: Long = 24L * 60L * 60L

  sealed trait Rejected
  object Rejected {
    /** Нет подписи, игрока или времени запуска. */
    case object Incomplete extends Rejected
    case object BadSign    extends Rejected
    case object Expired    extends Rejected
  }

  /** Чьи это параметры. `raw` — строка запроса адреса приложения, с `?` или без. */
  def verify(raw: String, secret: String, nowMs: Long): Either[Rejected, VkId] = {
    val params = Query.unsafeFromString(raw.stripPrefix("?")).pairs.toList
      .map { case (k, v) => k -> v.getOrElse("") }
    // Повтор имени — берём первое: подписаны и так только `vk_*`, а ВК их не повторяет.
    val first = params.foldLeft(Map.empty[String, String]) { case (acc, (k, v)) =>
      if (acc.contains(k)) acc else acc + (k -> v)
    }
    val vk = first.toList.filter(_._1.startsWith("vk_")).sortBy(_._1)
    for {
      given <- first.get("sign").toRight(Rejected.Incomplete)
      user  <- first.get("vk_user_id").filter(_.nonEmpty).toRight(Rejected.Incomplete)
      ts    <- first.get("vk_ts").flatMap(_.toLongOption).toRight(Rejected.Incomplete)
      _     <- Either.cond(MessageDigest.isEqual(sign(vk, secret).getBytes(UTF_8), given.getBytes(UTF_8)),
                 (), Rejected.BadSign)
      _     <- Either.cond(nowMs / 1000L - ts <= TtlSeconds, (), Rejected.Expired)
    } yield VkId(user)
  }

  /** Подпись упорядоченных параметров `vk_*` — так же, как её считает ВК. */
  private[miniapp] def sign(vk: List[(String, String)], secret: String): String = {
    val line = vk.map { case (k, v) => s"${escape(k)}=${escape(v)}" }.mkString("&")
    val mac  = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(secret.getBytes(UTF_8), "HmacSHA256"))
    Base64.getUrlEncoder.withoutPadding.encodeToString(mac.doFinal(line.getBytes(UTF_8)))
  }

  /** Кодирование значения, как у `querystring.stringify` в Node — им ВК и
    * считает подпись: без процентов остаются латиница, цифры и `-_.!~*'()`.
    * `URLEncoder` не подходит: он пишет пробел плюсом и кодирует `~`. */
  private def escape(s: String): String =
    s.getBytes(UTF_8).map { b =>
      val c = (b & 0xff).toChar
      if (c < 128 && (c.isLetterOrDigit || "-_.!~*'()".indexOf(c.toInt) >= 0)) c.toString
      else f"%%${b & 0xff}%02X"
    }.mkString
}
