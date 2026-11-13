package pangea.model.battle

import pangea.model.monster.Breed

/** Красный сетеплёт — редкий и именной (см. [[Breed.RedSpider]]); так его
  * описал пользователь. Десятая часть каждой его атаки всегда идёт прямо в
  * HP; удар травит с шансом [[PoisonPct]]% — и если отравил, с шансом
  * [[JamPct]]% следующее активное умение цели срывается: энергия уходит, умения
  * нет. Паутину он плетёт тоже (см. [[SpiderWeb]]). Всё это — и по героям, и по
  * союзникам. */
object RedSpider {

  /** Доля его удара, что принимает броня: десятая часть всегда в HP. */
  val ArmorShare: Double = 0.9

  /** Шанс отравить ударом, в процентах. */
  val PoisonPct: Long = 20L

  /** Шанс, что отравленный не сможет применить следующее умение, в процентах. */
  val JamPct: Long = 50L

  def is(breed: Option[Breed]): Boolean = breed.contains(Breed.RedSpider)

  /** Сколько от удара принимает броня: по опутанному — меньше (паутина), у удара
    * красного — без десятой части; совпало — из двух меньшее. */
  def armorShare(webbed: Boolean, attacker: Option[Breed]): Double =
    math.min(SpiderWeb.armorShare(webbed), if (is(attacker)) ArmorShare else 1.0)
}
