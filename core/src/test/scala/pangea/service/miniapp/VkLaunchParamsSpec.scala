package pangea.service.miniapp

import pangea.model.user.VkId
import pangea.service.miniapp.VkLaunchParams.Rejected
import zio.test._

/** Подпись параметров запуска — та же, что считает ВК: пример из его
  * документации и свой, с запятой в значении и временем запуска. */
object VkLaunchParamsSpec extends ZIOSpecDefault {

  /** Подписан ключом `test-secret` (посчитано `querystring` и `crypto` Node —
    * как в документации ВК). Порядок — как присылает ВК, не по алфавиту. */
  private val Ts     = 1793534400L
  private val Signed =
    "vk_access_token_settings=friends%2Cstatus&vk_app_id=51000000&vk_are_notifications_enabled=0" +
      "&vk_is_app_user=1&vk_is_favorite=0&vk_language=ru&vk_platform=mobile_android&vk_ref=other" +
      s"&vk_user_id=123456&vk_ts=$Ts&odr_enabled=1&sign=fEsuJYF8F2SY9CuNlxzVOcMbGM4DEvQfSNddww4LtIA"

  private val Secret = "test-secret"
  private val AnHourLater = (Ts + 3600L) * 1000L

  override def spec = suite("Параметры запуска VK Mini App")(

    test("подпись — как в примере из документации ВК") {
      val vk = List(
        "vk_access_token_settings"     -> "",
        "vk_app_id"                    -> "6736218",
        "vk_are_notifications_enabled" -> "1",
        "vk_is_app_user"               -> "1",
        "vk_language"                  -> "ru",
        "vk_platform"                  -> "android",
        "vk_user_id"                   -> "494075"
      )
      assertTrue(VkLaunchParams.sign(vk, "wvl68m4dR1UpLrVRli") == "htQFduJpLxz7ribXRZpDFUH-XEUhC9rBPTJkjUFEkRA")
    },

    test("подписанные — это игрок vk_user_id; с `?` впереди и без, чужие параметры не мешают") {
      assertTrue(
        VkLaunchParams.verify(Signed, Secret, AnHourLater) == Right(VkId("123456")),
        VkLaunchParams.verify("?" + Signed, Secret, AnHourLater) == Right(VkId("123456"))
      )
    },

    test("подменили игрока или ключ не тот — подпись не сходится") {
      assertTrue(
        VkLaunchParams.verify(Signed.replace("vk_user_id=123456", "vk_user_id=123457"), Secret, AnHourLater) ==
          Left(Rejected.BadSign),
        VkLaunchParams.verify(Signed, "other-secret", AnHourLater) == Left(Rejected.BadSign)
      )
    },

    test("сутки после запуска — ещё годится, позже — нет") {
      val ttl = VkLaunchParams.TtlSeconds
      assertTrue(
        VkLaunchParams.verify(Signed, Secret, (Ts + ttl) * 1000L) == Right(VkId("123456")),
        VkLaunchParams.verify(Signed, Secret, (Ts + ttl + 1L) * 1000L) == Left(Rejected.Expired)
      )
    },

    test("без подписи, игрока или времени запуска — не принимаем") {
      assertTrue(
        VkLaunchParams.verify(Signed.replaceAll("&sign=[^&]*", ""), Secret, AnHourLater) == Left(Rejected.Incomplete),
        VkLaunchParams.verify(Signed.replace("vk_user_id=123456&", ""), Secret, AnHourLater) == Left(Rejected.Incomplete),
        VkLaunchParams.verify(Signed.replace(s"vk_ts=$Ts&", ""), Secret, AnHourLater) == Left(Rejected.Incomplete),
        VkLaunchParams.verify("", Secret, AnHourLater) == Left(Rejected.Incomplete)
      )
    }
  )
}
