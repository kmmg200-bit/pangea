package pangea.service.donation

import pangea.engine.SceneContent
import pangea.model.hero.Achievement
import pangea.model.payment.DonationTiers

/** Откуда у героя деньги: после зачисления доната Ришелье рассказывает
  * историю — свою на каждую сумму. Ту же сумму во второй раз он уже не
  * расписывает (`donation.again`). Донат закрыл последнюю некупленную из семи
  * сумм — последней строкой «Любимец Проекта»: само достижение ставит выдача
  * (см. `PaymentDaoLive.grantOnce`). */
object DonationStory {

  /** Сумма доната в копейках → ключ истории в `donation.stories`. */
  val Stories: Map[Long, String] = DonationTiers.Amounts.zip(
    List("r200", "r500", "r1000", "r2000", "r4000", "r6000", "r10000")).toMap

  /** Письмо о донате на `amount` копеек. `granted` — суммы всех выданных игроку
    * донатов, этот уже в их числе. `None` — сказать нечего. */
  def text(content: SceneContent, amount: Long, granted: List[Long]): Option[String] = {
    val first = granted.count(_ == amount) <= 1
    val story = Stories.get(amount).map { key =>
      val body = if (first) content.text(s"donation.stories.$key") else content.text("donation.again")
      content.text("donation.header") + "\n" + body
    }
    // Набор закрывает только первая покупка недостающей суммы: повтор уже
    // купленной его не меняет.
    val favorite = Option.when(story.nonEmpty && first && DonationTiers.complete(granted))(
      content.format("marisa.achievement",
        "title" -> Achievement.ProjectFavorite.title, "bonus" -> Achievement.ProjectFavorite.bonusLine))
    story.map(s => (s :: favorite.toList).mkString("\n\n"))
  }
}
