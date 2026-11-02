package pangea.model.hero

import enumeratum._
import pangea.model.item.ItemSet

/** Достижения героя: выдаются сюжетом один раз и навсегда, каждое несёт свой
  * бонус. Хранятся ключами в `heroes.achievements`. */
sealed abstract class Achievement(val title: String, val bonusLine: String) extends EnumEntry

/** Числа достижений, которые читают и сами варианты, — отдельно от компаньона:
  * вариант enum не должен ходить в свой же компаньон, пока тот собирает
  * `values` (иначе инициализация класса встаёт в дедлок). */
object AchievementRates {

  /** На сколько процентов достижение за собранный набор поднимает его главный стат. */
  val SetStatPct: Long = 3L

  /** Сколько одной характеристики даёт достижение за малый расовый набор: по два
    * набора на каждую из четырёх, так что все восемь — ровно +2 ко всем. */
  val RaceSetStatBonus: Long = 1L
}

object Achievement extends Enum[Achievement] {
  val values: IndexedSeq[Achievement] = findValues

  /** Довёл долг Кельвина до конца — Мариса может спать спокойно. */
  case object MarisaSavior extends Achievement("Спаситель Марисы", "+10% к выпадаемому серебру в событиях лабиринта")

  /** Оставил Марису посреди материка с пустыми руками. */
  case object Scoundrel extends Achievement("Мерзавец", "+5% к наносимому и получаемому урону в бою")

  /** Сварил в кубе хотя бы по разу каждый отвар первого ранга. */
  case object Brewer1 extends Achievement("Зельевар I", "+2 к интеллекту")

  /** Довёз в деревню мурлоков двадцать единиц снаряжения. */
  case object MurlocFavorite extends Achievement("Любимец Мурлоков", "+1 к ловкости")

  /** Вырезал деревню мурлоков. */
  case object MurlocBane extends Achievement("Гроза Мурлоков", "+1 к силе")

  /** Вывел из-под Серебряной жилы гномью дружину «Двенадцать фонарей». */
  case object SquadFriend extends Achievement("Друг подгорного народа", "+1 к телосложению")

  /** Забрал груз дружины «Двенадцать фонарей» и оставил её у фонаря. */
  case object MineLooter extends Achievement("Расхититель штолен", "+10% серебра с серебряных жил")

  /** Больше десяти раз поднимал наёмников из-за стола за серебро, не дав им отдохнуть. */
  case object Overtime extends Achievement("Переработки наше всё", "+100 к HP")

  // ── Собранные наборы ────────────────────────────────────────────────────────
  // За каждый набор, надетый целиком — все двенадцать вещей разом, — своё
  // достижение: прибавка к главному стату набора, та же, что у его двойки, но
  // меньше и навсегда, даже когда набор снят.

  case object StoneGuardSet extends Achievement("Живая крепость",
    s"+${AchievementRates.SetStatPct}% к защите")

  case object WildFlameSet extends Achievement("Укротитель пламени",
    s"+${AchievementRates.SetStatPct}% к атаке")

  case object GhoulSet extends Achievement("Кровопийца",
    s"+${AchievementRates.SetStatPct}% к уклонению")

  case object HunterSet extends Achievement("Зоркий глаз",
    s"+${AchievementRates.SetStatPct}% к точности")

  case object RatSet extends Achievement("Свой среди крыс",
    s"+${AchievementRates.SetStatPct}% к броне")

  case object NecromancerSet extends Achievement("Повелитель мёртвых",
    s"+${AchievementRates.SetStatPct}% к интеллекту")

  // Малые расовые наборы: +1 к одной из четырёх характеристик, по два набора
  // на каждую. Набор надет целиком — достижение, какой бы расы ни был герой:
  // скрытый шаг набора от расы зависит, достижение — нет.

  case object PioneerSet extends Achievement("Новый берег",
    s"+${AchievementRates.RaceSetStatBonus} к телосложению")

  case object QuietGroveSet extends Achievement("Голос рощи",
    s"+${AchievementRates.RaceSetStatBonus} к интеллекту")

  case object DeepMotherSet extends Achievement("Дитя глубин",
    s"+${AchievementRates.RaceSetStatBonus} к интеллекту")

  case object HordeSet extends Achievement("Вождь орды",
    s"+${AchievementRates.RaceSetStatBonus} к силе")

  case object TricksterSet extends Achievement("Ловкие пальцы",
    s"+${AchievementRates.RaceSetStatBonus} к ловкости")

  case object UnderworldSet extends Achievement("Огонь преисподней",
    s"+${AchievementRates.RaceSetStatBonus} к силе")

  case object MountainMasterSet extends Achievement("Подгорный кузнец",
    s"+${AchievementRates.RaceSetStatBonus} к телосложению")

  case object DesertSet extends Achievement("Гость Пустынь",
    s"+${AchievementRates.RaceSetStatBonus} к ловкости")

  /** Достижение за набор, собранный целиком. */
  def forSet(set: ItemSet): Option[Achievement] = set match {
    case ItemSet.StoneGuard     => Some(StoneGuardSet)
    case ItemSet.WildFlame      => Some(WildFlameSet)
    case ItemSet.Ghoul          => Some(GhoulSet)
    case ItemSet.Hunter         => Some(HunterSet)
    case ItemSet.Rat            => Some(RatSet)
    case ItemSet.Necromancer    => Some(NecromancerSet)
    case ItemSet.Pioneer        => Some(PioneerSet)
    case ItemSet.QuietGrove     => Some(QuietGroveSet)
    case ItemSet.DeepMother     => Some(DeepMotherSet)
    case ItemSet.Horde          => Some(HordeSet)
    case ItemSet.Trickster      => Some(TricksterSet)
    case ItemSet.Underworld     => Some(UnderworldSet)
    case ItemSet.MountainMaster => Some(MountainMasterSet)
    case ItemSet.Desert         => Some(DesertSet)
  }

  /** Сколько вещей набора надо надеть разом, чтобы он считался собранным. */
  val FullSetPieces: Int = ItemSet.Thresholds.max

  private def setPct(hero: Hero, a: Achievement): Long =
    if (hero.hasAchievement(a)) AchievementRates.SetStatPct else 0L

  /** +% к защите, атаке, уклонению и точности от собранных наборов. */
  def defencePct(hero: Hero): Long  = setPct(hero, StoneGuardSet)
  def attackPct(hero: Hero): Long   = setPct(hero, WildFlameSet)
  def evasionPct(hero: Hero): Long  = setPct(hero, GhoulSet)
  def accuracyPct(hero: Hero): Long = setPct(hero, HunterSet)

  /** +% к потолку брони от «Своего среди крыс» — как и у двойки «Крысы», в
    * потолок, иначе прибавку срезала бы первая же починка. */
  def armorPct(hero: Hero): Long = setPct(hero, RatSet)

  /** +% к интеллекту от «Повелителя мёртвых». */
  def intPct(hero: Hero): Long = setPct(hero, NecromancerSet)

  /** Сколько интеллекта прибавляет «Зельевар I». */
  val BrewerIntBonus: Long = 2L

  /** Сколько ловкости прибавляет «Любимец Мурлоков» и силы — «Гроза Мурлоков». */
  val MurlocFavoriteAgiBonus: Long = 1L
  val MurlocBaneStrBonus: Long     = 1L

  /** По +1 за каждое из достижений за малые расовые наборы. */
  private def raceSets(hero: Hero, sets: Achievement*): Long =
    sets.count(hero.hasAchievement).toLong * AchievementRates.RaceSetStatBonus

  /** Плоская прибавка к интеллекту от достижений героя. */
  def intBonus(hero: Hero): Long =
    (if (hero.hasAchievement(Brewer1)) BrewerIntBonus else 0L) + raceSets(hero, QuietGroveSet, DeepMotherSet)

  /** Плоская прибавка к ловкости от достижений героя. */
  def agiBonus(hero: Hero): Long =
    (if (hero.hasAchievement(MurlocFavorite)) MurlocFavoriteAgiBonus else 0L) + raceSets(hero, TricksterSet, DesertSet)

  /** Плоская прибавка к силе от достижений героя. */
  def strBonus(hero: Hero): Long =
    (if (hero.hasAchievement(MurlocBane)) MurlocBaneStrBonus else 0L) + raceSets(hero, HordeSet, UnderworldSet)

  /** Сколько телосложения прибавляет «Друг подгорного народа». */
  val SquadFriendVitBonus: Long = 1L

  /** Плоская прибавка к телосложению от достижений героя. */
  def vitBonus(hero: Hero): Long =
    (if (hero.hasAchievement(SquadFriend)) SquadFriendVitBonus else 0L) + raceSets(hero, PioneerSet, MountainMasterSet)

  /** Сколько HP прибавляет «Переработки наше всё». */
  val OvertimeHpBonus: Long = 100L

  /** Плоская прибавка к максимуму HP от достижений героя. */
  def hpBonus(hero: Hero): Long =
    if (hero.hasAchievement(Overtime)) OvertimeHpBonus else 0L

  /** Сколько процентов сверху к серебру с серебряной жилы даёт «Расхититель штолен». */
  val MineLooterVeinPct: Long = 10L

  /** Множитель серебра с серебряной жилы с учётом достижений героя. */
  def veinPct(hero: Hero): Long =
    100L + (if (hero.hasAchievement(MineLooter)) MineLooterVeinPct else 0L)

  /** Сколько процентов сверху к серебру, выпадающему в лабиринте. */
  val SaviorSilverBonusPct: Long = 10L

  /** На сколько процентов Мерзавец бьёт сильнее — и получает сильнее. */
  val ScoundrelDamageBonusPct: Long = 5L

  /** Множитель серебра из лабиринта с учётом достижений героя. */
  def silverPct(hero: Hero): Long =
    100L + (if (hero.hasAchievement(MarisaSavior)) SaviorSilverBonusPct else 0L)

  /** Множитель урона в бою (в обе стороны) с учётом достижений героя. */
  def damagePct(hero: Hero): Long =
    100L + (if (hero.hasAchievement(Scoundrel)) ScoundrelDamageBonusPct else 0L)
}
