package pangea.model.hero

import enumeratum._
import pangea.model.item.{ItemSet, Rarity}
import pangea.model.trauma.Trauma

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

  /** На сколько процентов достижение за путь, выбранный на рубеже основной
    * линии, поднимает свой стат. */
  val PathStatPct: Long = 5L

  /** Сколько силы даёт каждая веха убитых: 1 000, 10 000, 100 000 и 500 000.
    * Складываются: все четыре — +18. */
  val Kills1kStr: Long   = 1L
  val Kills10kStr: Long  = 2L
  val Kills100kStr: Long = 5L
  val Kills500kStr: Long = 10L

  /** Сколько HP даёт каждое достижение за уровень героя. */
  val LevelHpBonus: Long = 100L

  /** Сколько каждой из четырёх характеристик даёт «Торопыга». */
  val HastyStatBonus: Long = 1L

  /** На сколько процентов каждый ранг «Промышленного аптекаря» удлиняет срок
    * отваров: все пять — +50%. */
  val ApothecaryDurationPct: Long = 10L

  /** На сколько процентов больше опыта даёт каждый ранг «Достигатора»: все
    * пять — +10%. */
  val AchieverExpPct: Long = 2L

  /** Сколько каждой из четырёх характеристик даёт «Любимец Проекта». */
  val ProjectFavoriteStatBonus: Long = 1L

  /** На сколько процентов «Весь в золоте» поднимает атаку, защиту, уклонение,
    * точность и потолок брони. */
  val AllGoldStatPct: Long = 2L

  /** На сколько процентов «Добейте меня...» поднимает максимум HP. */
  val BatteredHpPct: Long = 1L

  /** Сколько каждой из четырёх характеристик даёт каждый ранг «Артефактора»:
    * все пять — +5. */
  val ArtificerStatBonus: Long = 1L

  /** Сколько интеллекта даёт «Зельевар II». */
  val Brewer2IntBonus: Long = 3L
}

object Achievement extends Enum[Achievement] {
  val values: IndexedSeq[Achievement] = findValues

  /** Довёл долг Кельвина до конца — Мариса может спать спокойно. */
  case object MarisaSavior extends Achievement("Спаситель Марисы", "+10% к выпадаемому серебру в событиях лабиринта")

  /** Оставил Марису посреди материка с пустыми руками. */
  case object Scoundrel extends Achievement("Мерзавец", "+5% к наносимому и получаемому урону в бою")

  /** Сварил в кубе хотя бы по разу каждый отвар первого ранга. */
  case object Brewer1 extends Achievement("Зельевар I", "+2 к интеллекту")

  /** Сварил хотя бы по разу каждый отвар второго ранга (`BrewKind.rank2`). */
  case object Brewer2 extends Achievement("Зельевар II",
    s"+${AchievementRates.Brewer2IntBonus} к интеллекту")

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

  /** Своё вернул: нашёл гоблинов, что вскрыли бочку в порту (см. BarrelThievesQuest). */
  case object OwnBack extends Achievement("Своё не отдам", "+1 к ловкости")

  // ── Рубежи основной линии ───────────────────────────────────────────────────
  // Путь на рубеже — своё достижение. На следующем рубеже путь можно сменить,
  // но полученное остаётся: достижения — навсегда.

  /** Рубеж I, путь Тени: вошёл в Братство под новым лицом (см. StoryToolQuest). */
  case object ShadowPath extends Achievement("Тень Братства",
    s"+${AchievementRates.PathStatPct}% к уклонению")

  /** Рубеж I, путь Щита: сдал ячейку Братства совету и Гильдии. */
  case object ShieldPath extends Achievement("Щит Кинэта",
    s"+${AchievementRates.PathStatPct}% к защите")

  /** Рубеж I, путь «Сам по себе»: свёл счёты и забрал тайник ячейки. */
  case object OwnPath extends Achievement("Ничьё орудие",
    s"+${AchievementRates.PathStatPct}% к атаке")

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

  // ── Вехи: убитые за всю жизнь ──────────────────────────────────────────────
  // Выдаются после хода, на котором герой перевалил порог (см. [[KillTiers]]).

  case object Kills1k extends Achievement("Тысяча за плечами",
    s"+${AchievementRates.Kills1kStr} к силе")

  case object Kills10k extends Achievement("Мясник лабиринта",
    s"+${AchievementRates.Kills10kStr} к силе")

  case object Kills100k extends Achievement("Жнец",
    s"+${AchievementRates.Kills100kStr} к силе")

  case object Kills500k extends Achievement("Сама Смерть",
    s"+${AchievementRates.Kills500kStr} к силе")

  // ── Вехи: уровень героя ─────────────────────────────────────────────────────
  // Каждое — +100 HP. Выдаются, как и вехи убитых, после хода с порогом.

  case object Level10 extends Achievement("Великий путь начинается с первой десятки",
    s"+${AchievementRates.LevelHpBonus} к HP")

  case object Level25 extends Achievement("Четверть сотни",
    s"+${AchievementRates.LevelHpBonus} к HP")

  case object Level50 extends Achievement("Полпути до сотни",
    s"+${AchievementRates.LevelHpBonus} к HP")

  case object Level75 extends Achievement("Бывалый",
    s"+${AchievementRates.LevelHpBonus} к HP")

  case object Level100 extends Achievement("Сотник",
    s"+${AchievementRates.LevelHpBonus} к HP")

  case object Level125 extends Achievement("Живая легенда",
    s"+${AchievementRates.LevelHpBonus} к HP")

  case object Level150 extends Achievement("Вершина",
    s"+${AchievementRates.LevelHpBonus} к HP")

  // ── Вехи: сваренные зелья ───────────────────────────────────────────────────
  // Счёт — `heroes.brews`: каждое зелье из куба, Живой сумки и с верстака.

  case object Apothecary1 extends Achievement("Промышленный аптекарь I",
    s"+${AchievementRates.ApothecaryDurationPct}% к сроку действия отваров")

  case object Apothecary2 extends Achievement("Промышленный аптекарь II",
    s"+${AchievementRates.ApothecaryDurationPct}% к сроку действия отваров")

  case object Apothecary3 extends Achievement("Промышленный аптекарь III",
    s"+${AchievementRates.ApothecaryDurationPct}% к сроку действия отваров")

  case object Apothecary4 extends Achievement("Промышленный аптекарь IV",
    s"+${AchievementRates.ApothecaryDurationPct}% к сроку действия отваров")

  case object Apothecary5 extends Achievement("Промышленный аптекарь V",
    s"+${AchievementRates.ApothecaryDurationPct}% к сроку действия отваров")

  // ── Вехи: сколько всего достижений ──────────────────────────────────────────
  // Считаются все полученные, и сами «Достигаторы» тоже.

  case object Achiever1 extends Achievement("Достигатор I",
    s"+${AchievementRates.AchieverExpPct}% к получаемому опыту")

  case object Achiever2 extends Achievement("Достигатор II",
    s"+${AchievementRates.AchieverExpPct}% к получаемому опыту")

  case object Achiever3 extends Achievement("Достигатор III",
    s"+${AchievementRates.AchieverExpPct}% к получаемому опыту")

  case object Achiever4 extends Achievement("Достигатор IV",
    s"+${AchievementRates.AchieverExpPct}% к получаемому опыту")

  case object Achiever5 extends Achievement("Достигатор V",
    s"+${AchievementRates.AchieverExpPct}% к получаемому опыту")

  // ── От админа ───────────────────────────────────────────────────────────────

  /** Выдаёт «Админ 1» в беседе: «Дать Торопыга» ответом на сообщение игрока
    * (см. [[ChatGrantable]]). Других путей нет. */
  case object Hasty extends Achievement("Торопыга",
    s"+${AchievementRates.HastyStatBonus} ко всем характеристикам")

  /** Купил хоть раз каждую из семи сумм доната (`DonationTiers`). Ставит его сама
    * выдача дублонов — той же транзакцией, что и начисление (см.
    * `PaymentDaoLive.grantOnce`), поэтому не теряется, даже если оплата пришла
    * посреди чужого хода. */
  case object ProjectFavorite extends Achievement("Любимец Проекта",
    s"+${AchievementRates.ProjectFavoriteStatBonus} ко всем характеристикам")

  /** Всё надетое — легендарное (см. [[allGold]]). Проверяется при надевании,
    * как и собранный набор. */
  case object AllGold extends Achievement("Весь в золоте",
    s"+${AchievementRates.AllGoldStatPct}% к атаке, защите, уклонению, точности и броне")

  /** На герое разом все травмы, что есть в игре (`Trauma.all`). Копятся они по
    * тирам — лёгкие, средние, тяжёлые, — так что «получил все» и «все разом»
    * здесь одно. Веха: проверяется после каждого хода. */
  case object Battered extends Achievement("Добейте меня...",
    s"+${AchievementRates.BatteredHpPct}% к HP")

  /** «Артефактор» I–V: разных рецептов куба Азата в книге рецептов — 10, 25,
    * 50, 100, 150 (см. [[ArtificerTiers]]). */
  case object Artificer1 extends Achievement("Артефактор I",
    s"+${AchievementRates.ArtificerStatBonus} ко всем характеристикам")
  case object Artificer2 extends Achievement("Артефактор II",
    s"+${AchievementRates.ArtificerStatBonus} ко всем характеристикам")
  case object Artificer3 extends Achievement("Артефактор III",
    s"+${AchievementRates.ArtificerStatBonus} ко всем характеристикам")
  case object Artificer4 extends Achievement("Артефактор IV",
    s"+${AchievementRates.ArtificerStatBonus} ко всем характеристикам")
  case object Artificer5 extends Achievement("Артефактор V",
    s"+${AchievementRates.ArtificerStatBonus} ко всем характеристикам")

  /** Какие достижения «Админ 1» выдаёт словом «Дать» в беседе. Остальные — только
    * за свои условия. */
  val ChatGrantable: List[Achievement] = List(Hasty)

  /** Вехи убитых: сколько надо убить и сколько силы даёт достижение. */
  val KillTiers: List[(Long, Achievement, Long)] = List(
    (1000L, Kills1k, AchievementRates.Kills1kStr),
    (10000L, Kills10k, AchievementRates.Kills10kStr),
    (100000L, Kills100k, AchievementRates.Kills100kStr),
    (500000L, Kills500k, AchievementRates.Kills500kStr)
  )

  /** Вехи уровня героя. */
  val LevelTiers: List[(Long, Achievement)] = List(
    10L -> Level10, 25L -> Level25, 50L -> Level50, 75L -> Level75,
    100L -> Level100, 125L -> Level125, 150L -> Level150
  )

  /** Вехи сваренных зелий. */
  val BrewTiers: List[(Long, Achievement)] = List(
    10L -> Apothecary1, 100L -> Apothecary2, 250L -> Apothecary3, 500L -> Apothecary4, 1000L -> Apothecary5
  )

  /** Вехи, до которых герой уже дошёл, а достижения за них ещё нет, — по
    * порядку. Порог проверяется по самому герою, поэтому неважно, откуда
    * пришли уровень, убитые или зелья. */
  def milestonesDue(hero: Hero): List[Achievement] =
    (KillTiers.collect { case (n, a, _) if hero.kills >= n => a } ++
      LevelTiers.collect { case (n, a) if hero.lvl >= n => a } ++
      BrewTiers.collect { case (n, a) if hero.brews >= n => a } ++
      AchieverTiers.collect { case (n, a) if gotCount(hero) >= n => a } ++
      Option.when(allTraumas(hero))(Battered)).filterNot(hero.hasAchievement)

  /** Все травмы игры на герое разом. Имена после срока не стираются до новой
    * травмы, но собраны они всё равно были. */
  def allTraumas(hero: Hero): Boolean = Trauma.all.forall(t => hero.traumaNames.contains(t.name))

  /** +% к максимуму HP от «Добейте меня...». */
  def hpPct(hero: Hero): Long =
    if (hero.hasAchievement(Battered)) AchievementRates.BatteredHpPct else 0L

  /** Вехи «Артефактора»: сколько разных рецептов куба записано в книге. */
  val ArtificerTiers: List[(Int, Achievement)] = List(
    10 -> Artificer1, 25 -> Artificer2, 50 -> Artificer3, 100 -> Artificer4, 150 -> Artificer5
  )

  /** Ранги «Артефактора», до которых дошла книга из `known` рецептов, а у героя
    * их ещё нет. */
  def artificerDue(hero: Hero, known: Int): List[Achievement] =
    ArtificerTiers.collect { case (n, a) if known >= n => a }.filterNot(hero.hasAchievement)

  /** Вехи «Достигатора»: сколько достижений надо иметь. */
  val AchieverTiers: List[(Int, Achievement)] = List(
    10 -> Achiever1, 20 -> Achiever2, 30 -> Achiever3, 40 -> Achiever4, 50 -> Achiever5
  )

  /** Сколько достижений у героя. */
  def gotCount(hero: Hero): Int = values.count(hero.hasAchievement)

  /** +% к опыту от «Достигатора»: +2% за ранг. */
  def expPct(hero: Hero): Long =
    AchieverTiers.count { case (_, a) => hero.hasAchievement(a) }.toLong * AchievementRates.AchieverExpPct

  /** Срок отвара с учётом «Промышленного аптекаря»: +10% за каждый ранг. */
  def brewMs(hero: Hero, ms: Long): Long =
    ms * (100L + BrewTiers.count { case (_, a) => hero.hasAchievement(a) }.toLong *
      AchievementRates.ApothecaryDurationPct) / 100L

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

  private def pathPct(hero: Hero, a: Achievement): Long =
    if (hero.hasAchievement(a)) AchievementRates.PathStatPct else 0L

  private def goldPct(hero: Hero): Long =
    if (hero.hasAchievement(AllGold)) AchievementRates.AllGoldStatPct else 0L

  /** Надето ли легендарное везде: все одиннадцать мест на теле и обе руки
    * (двуручное закрывает обе). Доп. слот не в счёт. */
  def allGold(equipment: Equipment): Boolean = {
    val body = List(equipment.helmet, equipment.shoulderPads, equipment.chestPlate, equipment.bracelets,
      equipment.gloves, equipment.pants, equipment.boots, equipment.amulet, equipment.firstRing,
      equipment.secondRing, equipment.belt, equipment.weapon)
    val hands = if (equipment.leftBusy) Nil else List(equipment.leftHand)
    (body ++ hands).forall(_.rarity == Rarity.Orange)
  }

  /** +% к защите, атаке, уклонению и точности от собранных наборов, путей,
    * выбранных на рубежах, и «Всего в золоте». */
  def defencePct(hero: Hero): Long  = setPct(hero, StoneGuardSet) + pathPct(hero, ShieldPath) + goldPct(hero)
  def attackPct(hero: Hero): Long   = setPct(hero, WildFlameSet) + pathPct(hero, OwnPath) + goldPct(hero)
  def evasionPct(hero: Hero): Long  = setPct(hero, GhoulSet) + pathPct(hero, ShadowPath) + goldPct(hero)
  def accuracyPct(hero: Hero): Long = setPct(hero, HunterSet) + goldPct(hero)

  /** +% к потолку брони от «Своего среди крыс» и «Всего в золоте» — как и у
    * двойки «Крысы», в потолок, иначе прибавку срезала бы первая же починка. */
  def armorPct(hero: Hero): Long = setPct(hero, RatSet) + goldPct(hero)

  /** +% к интеллекту от «Повелителя мёртвых». */
  def intPct(hero: Hero): Long = setPct(hero, NecromancerSet)

  /** Сколько интеллекта прибавляет «Зельевар I». */
  val BrewerIntBonus: Long = 2L

  /** Сколько ловкости прибавляет «Любимец Мурлоков» и силы — «Гроза Мурлоков». */
  val MurlocFavoriteAgiBonus: Long = 1L
  val MurlocBaneStrBonus: Long     = 1L

  /** Сколько ловкости прибавляет «Своё не отдам». */
  val OwnBackAgiBonus: Long = 1L

  /** По +1 за каждое из достижений за малые расовые наборы. */
  private def raceSets(hero: Hero, sets: Achievement*): Long =
    sets.count(hero.hasAchievement).toLong * AchievementRates.RaceSetStatBonus

  /** Плоская прибавка к интеллекту от достижений героя. */
  def intBonus(hero: Hero): Long =
    (if (hero.hasAchievement(Brewer1)) BrewerIntBonus else 0L) +
      (if (hero.hasAchievement(Brewer2)) AchievementRates.Brewer2IntBonus else 0L) +
      raceSets(hero, QuietGroveSet, DeepMotherSet) +
      hasty(hero)

  /** «Торопыга», «Любимец Проекта» и каждый ранг «Артефактора» — по +1 к
    * каждой из четырёх характеристик. */
  private def hasty(hero: Hero): Long =
    (if (hero.hasAchievement(Hasty)) AchievementRates.HastyStatBonus else 0L) +
      (if (hero.hasAchievement(ProjectFavorite)) AchievementRates.ProjectFavoriteStatBonus else 0L) +
      ArtificerTiers.count { case (_, a) => hero.hasAchievement(a) }.toLong * AchievementRates.ArtificerStatBonus

  /** Плоская прибавка к ловкости от достижений героя. */
  def agiBonus(hero: Hero): Long =
    (if (hero.hasAchievement(MurlocFavorite)) MurlocFavoriteAgiBonus else 0L) +
      (if (hero.hasAchievement(OwnBack)) OwnBackAgiBonus else 0L) + raceSets(hero, TricksterSet, DesertSet) + hasty(hero)

  /** Плоская прибавка к силе от достижений героя. */
  def strBonus(hero: Hero): Long =
    (if (hero.hasAchievement(MurlocBane)) MurlocBaneStrBonus else 0L) + raceSets(hero, HordeSet, UnderworldSet) +
      KillTiers.collect { case (_, a, str) if hero.hasAchievement(a) => str }.sum + hasty(hero)

  /** Сколько телосложения прибавляет «Друг подгорного народа». */
  val SquadFriendVitBonus: Long = 1L

  /** Плоская прибавка к телосложению от достижений героя. */
  def vitBonus(hero: Hero): Long =
    (if (hero.hasAchievement(SquadFriend)) SquadFriendVitBonus else 0L) + raceSets(hero, PioneerSet, MountainMasterSet) + hasty(hero)

  /** Сколько HP прибавляет «Переработки наше всё». */
  val OvertimeHpBonus: Long = 100L

  /** Плоская прибавка к максимуму HP от достижений героя. */
  def hpBonus(hero: Hero): Long =
    (if (hero.hasAchievement(Overtime)) OvertimeHpBonus else 0L) +
      LevelTiers.count { case (_, a) => hero.hasAchievement(a) }.toLong * AchievementRates.LevelHpBonus

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
