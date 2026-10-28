package pangea.model.promo

import pangea.model.item.{BrewKind, Item, QuestItemKind}
import pangea.model.quest.DailyRates

import java.time.LocalDate

/** Что даёт промокод: вещи, благословение Азата и дублоны. Благословение идёт
  * от остатка, если оно уже есть (см. `AzatState.blessedFor`). */
final case class PromoReward(items: List[Item], blessingMs: Long, doubloons: Long)

/** Промокод: слово, которое пишут боту в личку с любого экрана, и срок. Каждый
  * игрок вводит код один раз (см. `PromoCodes`). `key` — имя кода в базе, под
  * ним запоминается, кто его уже ввёл; `untilMs` — когда код перестаёт
  * действовать: в эту миллисекунду он уже не принимается. */
final case class PromoCode(key: String, word: String, untilMs: Long, reward: PromoReward)

object PromoCode {

  private val HourMs: Long = 60L * 60L * 1000L

  /** «Живём»: до 00:00 понедельника 2 ноября 2026 года по Москве. */
  val Zhivyom: PromoCode = PromoCode(
    key     = "zhivyom",
    word    = "Живём",
    untilMs = moscowMidnight(LocalDate.of(2026, 11, 2)),
    reward  = PromoReward(
      items = List(
        QuestItemKind.item(QuestItemKind.RaceCoupon),
        BrewKind.item(BrewKind.LivingWater),
        BrewKind.item(BrewKind.Schnapps)),
      blessingMs = 24L * HourMs,
      doubloons  = 50L))

  val all: List[PromoCode] = List(Zhivyom)

  /** Код, который набран этим текстом. Регистр, «ё» и «е», пробелы, кавычки и
    * знаки препинания не важны: «живем!» — тот же «Живём». */
  def find(text: String): Option[PromoCode] = {
    val typed = normalize(text)
    if (typed.isEmpty) None else all.find(code => normalize(code.word) == typed)
  }

  private def normalize(text: String): String =
    text.toLowerCase.replace('ё', 'е').filter(_.isLetterOrDigit)

  /** Начало этого дня по Москве: сутки игры кончаются в полночь по ней. */
  def moscowMidnight(day: LocalDate): Long =
    day.toEpochDay * DailyRates.DayMs - DailyRates.MoscowOffsetMs
}
