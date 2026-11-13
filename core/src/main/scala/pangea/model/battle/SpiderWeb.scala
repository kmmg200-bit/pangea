package pangea.model.battle

import pangea.model.monster.Breed

/** Паутина сетеплётов — у взрослого и у красного (см. [[Breed]]): удар с шансом
  * опутывает цель, героя или союзника. Опутанный не сходит с места — ни шагом,
  * ни Тараном, ни Прыжком, ни мерцающим настоем, — а удары по нему идут мимо
  * брони: [[ArmorShare]] в броню, остальное — прямо в HP (кончилась броня — и её
  * доля уходит в HP). Держит остаток раунда, в котором легла, и весь
  * следующий: ставится на [[Turns]] и снимается по одному в самом конце раунда.
  *
  * Шанс решил пользователь: (уровень паука − ловкость + 100 / (уклонение −
  * защита)) × 5; не больше нуля — [[MinPct]]%, потолок — [[MaxPct]]%. Ловкость —
  * с учётом снаряжения и бафов, как на экране героя; уклонение равно защите —
  * делить на ноль нельзя, разность считается за 1. У союзника ловкости нет —
  * вместо неё его уровень. */
object SpiderWeb {

  /** На сколько концов раунда паутина ложится. */
  val Turns: Int = 2

  /** Доля удара по опутанному, что принимает броня; остальное — в HP. */
  val ArmorShare: Double = 0.7

  val MinPct: Long = 5L
  val MaxPct: Long = 95L
  val Mult: Double = 5.0

  /** Значок паутины в строке удара и у HP. */
  val Icon: String = "🕸"

  /** Плетёт ли паутину моб этой породы. */
  def weaves(breed: Option[Breed]): Boolean = breed.exists(b => b == Breed.AdultSpider || b == Breed.RedSpider)

  /** Шанс опутать, в процентах. */
  def chancePct(spiderLvl: Long, agility: Long, evasion: Long, defence: Long): Long = {
    val diff = evasion - defence
    val raw  = spiderLvl.toDouble - agility.toDouble + 100.0 / (if (diff == 0L) 1L else diff).toDouble
    if (raw <= 0.0) MinPct else math.round(raw * Mult).max(MinPct).min(MaxPct)
  }

  /** Сколько от удара принимает броня: по опутанному — [[ArmorShare]], иначе вся. */
  def armorShare(webbed: Boolean): Double = if (webbed) ArmorShare else 1.0
}
