package pangea.model.hero

import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.trauma.Trauma

/** Карточка «Персонажа» — числами, а не текстом. Считает её герой
  * ([[Hero.card]]), а показывают двое: бот печатает текстом ([[Hero.getInfo]]),
  * Mini App раскладывает по экрану. Так два экрана не разойдутся в цифрах.
  *
  * Благословения и быстрых отдыхов здесь нет: они живут в `azat_data`, а не в
  * герое (см. [[AzatState]]).
  *
  * @param race          как раса зовётся сейчас ([[Hero.raceName]]: с «Некромантом» — «Нежить-Орк»)
  * @param restedLeftMs  сколько ещё держится «выспался» после сна дома
  * @param stats         сила, тело, ловкость и ум сейчас — со всем, что действует
  * @param native        они же родные: до расы, травм и зелий ([[Hero.nativeBaseStats]])
  * @param armor         текущая броня, не выше потолка с травмами
  * @param energy        текущая энергия, не выше потолка
  * @param fight         боевые статы сейчас: атака, защита, точность, уклонение
  * @param traumas       действующие травмы
  * @param traumaLeftMs  сколько ещё до снятия травм
  */
final case class HeroCard(
  race:          String,
  lvl:           Long,
  exp:           Long,
  neededExp:     Long,
  kills:         Long,
  expFrozen:     Boolean,
  restedLeftMs:  Option[Long],
  stats:         BaseStats,
  native:        BaseStats,
  hp:            Long,
  maxHp:         Long,
  armor:         Long,
  maxArmor:      Long,
  energy:        Long,
  maxEnergy:     Long,
  fight:         FightStats,
  upgradePoints: Long,
  traumas:       List[Trauma],
  traumaLeftMs:  Option[Long]
)

object HeroCard {

  /** Сколько ещё «выспался»: «3 ч 20 мин», меньше часа — «40 мин». */
  def restedLeft(ms: Long): String = {
    val mins = (ms / 60000L).max(1L)
    if (mins >= 60L) s"${mins / 60L} ч ${mins % 60L} мин" else s"$mins мин"
  }

  /** Сколько ещё до снятия травм: «5ч 20мин». */
  def traumaLeft(ms: Long): String = {
    val secs = ms / 1000L
    s"${secs / 3600L}ч ${(secs % 3600L) / 60L}мин"
  }
}
