package pangea.generator.monster

import pangea.domain.Rng
import pangea.model.monster.Rarity._
import pangea.model.monster.{Monster, MonsterRaceFactor, Race, Rarity}
import pangea.model.skill.MonsterEnergy
import pangea.model.stats.FightStats

/** Ставки башни со стрелком — за уровень. */
object TowerRates {
  val AtkPerLvl: Long      = 20L
  val AccuracyPerLvl: Long = 15L
  val DefencePerLvl: Long  = 20L
  val ArmorPerLvl: Long    = 150L
  val HpPerLvl: Long       = 45L
}

/** Дракон глубин: каждый показатель — атака, HP, броня, защита, уклонение,
  * меткость и энергия. */
object DragonRates {
  val Stat: Long = 100000L
}

/** Глубина (решение пользователя): с [[DeepFrom]]-го уровня к каждой базовой
  * ставке моба — атаке, HP, броне, защите, уклонению, точности — ещё
  * +[[DeepBonus]], до умножения на уровень, редкость и расу (как `ArmorBonus` и
  * `DefenceBonus`). По уровню самого моба — у всех, кто считается этой
  * формулой: в лабиринте (вернулся на 25-й — новые мобы обычные), в заданиях,
  * сюжете, контрактах и событиях; и у союзников, чья сила — «как у моба того же
  * уровня». У минибоссов и дракона ставки свои — их это не касается. */
object DepthRates {
  val DeepFrom: Int     = 26
  val DeepBonus: Double = 2.0

  /** Прибавка к базовым ставкам моба этого уровня. */
  def bonus(level: Int): Double = if (level >= DeepFrom) DeepBonus else 0.0
}

object MonsterGenerator {

  private val N = 1.1

  // Прибавки к базовым ставкам защиты и брони: идут ДО умножения на уровень,
  // редкость и расу, поэтому растут вместе с ними и чувствуются на всю игру.
  private val DefenceBonus = 2.0
  private val ArmorBonus   = 1.0

  // Модификатор «Отмеченный тьмой»: 7% шанс, +20% ко всем показателям.
  // Доступен только мобам редкости Rare и выше — обычные/необычные «отмеченными»
  // не бывают.
  private val MarkedChance                = 7L
  private val MarkedMultiplier            = 1.2
  private val MarkedRarities: Set[Rarity] = Set(Rare, Mythical, Legendary)

  // Weighted pool: 50% Common, 28% Uncommon, 17% Rare, 4% Mythical, 1% Legendary
  private val rarityPool: List[Rarity] =
    List.fill(50)(Common) ++
      List.fill(28)(Uncommon) ++
      List.fill(17)(Rare) ++
      List.fill(4)(Mythical) ++
      List.fill(1)(Legendary)

  // Пул редкостей для гарантированно отмеченного моба: только Rare+ в тех же
  // относительных весах, что и в общем пуле (17 : 4 : 1).
  private val markedRarityPool: List[Rarity] =
    rarityPool.filter(MarkedRarities.contains)

  /** Башня со стрелком — сооружение с собственными ставками: стоит крепко
    * (броня и HP), бьёт метко, но небыстро, и ни энергии, ни уклонения у неё
    * нет. Редкость средняя нарочно: легендарные и мифические зовут сородичей,
    * а звать башне некого. */
  def tower(lvl: Int): Monster = {
    val l = lvl.toLong.max(1L)
    Monster(0L, l, Race.Construct, Rarity.Rare, FightStats(
      atk      = TowerRates.AtkPerLvl * l,
      hp       = TowerRates.HpPerLvl * l,
      armor    = TowerRates.ArmorPerLvl * l,
      defence  = TowerRates.DefencePerLvl * l,
      evasion  = 0L,
      accuracy = TowerRates.AccuracyPerLvl * l,
      energy   = 0L))
  }

  /** Дракон глубин (26–50-й уровень лабиринта): все показатели — по
    * [[DragonRates.Stat]], на любом уровне. Сородичей не зовёт — бой с ним
    * идёт с `noKin` (см. DungeonState). */
  def dragon(lvl: Int): Monster = {
    val s = DragonRates.Stat
    Monster(0L, lvl.toLong.max(1L), Race.Dragon, Rarity.Legendary,
      FightStats(atk = s, hp = s, armor = s, defence = s, evasion = s, accuracy = s, energy = s))
  }

  def generate(dungeonLevel: Int, rng: Rng): (Monster, Rng) = {
    val (race, rng1) = rng.pick(Race.mortals.toList)
    generateOfRace(dungeonLevel, race, rng1)
  }

  /** Генерация моба фиксированной расы (раса задаётся снаружи — например, в
    * цепочке боёв события «мобы с сокровищем» все бои одной расы). Редкость и
    * «отмеченность» по-прежнему роллятся как обычно.
    */
  def generateOfRace(
      dungeonLevel: Int,
      race: Race,
      rng: Rng
  ): (Monster, Rng) = {
    val (rarity, rng2)   = rng.pick(rarityPool)
    val stats            = buildStats(dungeonLevel, rarity, race)
    val (markRoll, rng3) = rng2.between(0L, 100L)
    val marked     = MarkedRarities.contains(rarity) && markRoll < MarkedChance
    val finalStats = if (marked) boost(stats, MarkedMultiplier) else stats
    val (name, rng4) = legendaryName(race, rarity, rng3)
    (Monster(0L, dungeonLevel.toLong, race, rarity, finalStats, marked, name), rng4)
  }

  /** Имя легендарного: из списка его расы, наугад. У прочих редкостей имени
    * нет — их зовут по расе и тиру (см. [[Monster.namesByRaceRarity]]).
    * Бросок тратится только на легендарных: у остальных RNG не трогается, и
    * порядок бросков в боевых тестах не плывёт. */
  def legendaryName(race: Race, rarity: Rarity, rng: Rng): (Option[String], Rng) =
    if (rarity != Legendary) (None, rng)
    else Monster.legendaryNames.get(race).filter(_.nonEmpty) match {
      case None        => (None, rng)
      case Some(names) => val (n, next) = rng.pick(names); (Some(n), next)
    }

  /** То же, но когда имя нужно само по себе — например, чтобы запомнить его в
    * сцене и показать до боя тем же, кем он потом и выйдет. */
  def legendaryName(race: Race, rng: Rng): (Option[String], Rng) =
    legendaryName(race, Legendary, rng)

  /** Моб заданных расы и редкости — когда сюжет решает сам, кто вышел навстречу
    * (трое бандитов первых трёх тиров, брат девицы). Метки тьмы у таких нет. */
  def generateOfRaceAndRarity(
      dungeonLevel: Int, race: Race, rarity: Rarity, name: Option[String] = None
  ): Monster =
    Monster(0L, dungeonLevel.toLong, race, rarity, buildStats(dungeonLevel, rarity, race),
      customName = name)

  /** Гарантированно «Отмеченный тьмой» моб заданного уровня — для механики
    * выслеживания прохода вглубь. Редкость роллится среди Rare+ (в тех же
    * относительных весах, что и обычный ролл), статы усилены
    * `MarkedMultiplier`. Уровень сюда передаётся ЦЕЛЕВОЙ (куда игрок хочет
    * спуститься), а не текущий.
    */
  def generateMarked(dungeonLevel: Int, rng: Rng): (Monster, Rng) = {
    val (race, rng1)   = rng.pick(Race.mortals.toList)
    val (rarity, rng2) = rng1.pick(markedRarityPool)
    val stats = boost(buildStats(dungeonLevel, rarity, race), MarkedMultiplier)
    val (name, rng3) = legendaryName(race, rarity, rng2)
    (Monster(0L, dungeonLevel.toLong, race, rarity, stats, marked = true, name), rng3)
  }

  // +X% ко всем показателям (атака/HP зажаты снизу единицей, как в buildStats).
  private def boost(s: FightStats, factor: Double): FightStats =
    FightStats(
      atk = (s.atk * factor).toLong.max(1L),
      hp = (s.hp * factor).toLong.max(1L),
      armor = (s.armor * factor).toLong,
      defence = (s.defence * factor).toLong,
      evasion = (s.evasion * factor).toLong,
      accuracy = (s.accuracy * factor).toLong,
      // Энергию «отмеченность» не трогает: цены умений от неё не зависят.
      energy = s.energy
    )

  /** `deep` — прибавка глубины к каждой базовой ставке (атака, HP, броня,
    * защита, уклонение, точность) до умножения на уровень, редкость и расу — с
    * [[DepthRates.DeepFrom]]-го уровня моба. Энергию она не трогает: цены умений
    * от неё не зависят. */
  private def buildStats(level: Int, rarity: Rarity, race: Race): FightStats = {
    val deep = DepthRates.bonus(level)
    val base = level.toDouble * rarity.factor * N
    val f    = MonsterRaceFactor.of(race)
    FightStats(
      atk = ((10.0 + deep) * base * f.attackFactor).toLong.max(1L),
      hp = ((40.0 + deep) * base * f.hpFactor).toLong.max(1L),
      armor = ((22.0 + ArmorBonus + deep) * base * f.armorFactor).toLong,
      // Защита — процентное снижение урона, третий слой поверх брони и HP.
      // Растёт линейно по уровню, как и пробитие героя, — поэтому доля
      // срезанного держится ровной всю игру (см. BattleState.pierce).
      defence = ((5.0 + DefenceBonus + deep) * base * f.defenceFactor).toLong,
      evasion = ((16.25 + deep) * base * f.evasionFactor).toLong,
      // Точность у всех вдвое меньше, чем была, а в попадании весит вдвое
      // больше (см. BattleState.dodgeChance) — бьют мобы так же метко.
      accuracy = ((8.25 + deep) * base * f.accuracyFactor).toLong,
      // Потолок энергии — из него моб платит за свои умения (см. MonsterEnergy).
      energy = MonsterEnergy.maxEnergy(level.toLong)
    )
  }
}
