package pangea.model.hero

import pangea.model.hero.Achievement
import pangea.model.item.Item
import pangea.model.trauma.TraumaPenalties

/** Раскладка числа на экране «Персонаж»: откуда сложился итог. Считает в том же
  * порядке, что [[Hero.effectiveFightStats]] и [[Hero.effectiveBaseStats]], чтобы
  * сумма строк сходилась с показанным значением (кроме отсечения снизу единицей
  * после травмы в бою — его отражаем отдельной строкой). */
object HeroStatBreakdown {

  final case class Line(label: String, value: Long)
  final case class View(current: Long, lines: List[Line])

  private def line(label: String, value: Long, acc: List[Line]): List[Line] =
    if (value != 0L) acc :+ Line(label, value) else acc

  private val equipSlots: List[(String, Equipment => Item)] = List(
    "Шлем"        -> (_.helmet),
    "Наплечники"  -> (_.shoulderPads),
    "Нагрудник"   -> (_.chestPlate),
    "Браслеты"    -> (_.bracelets),
    "Перчатки"    -> (_.gloves),
    "Штаны"       -> (_.pants),
    "Сапоги"      -> (_.boots),
    "Амулет"      -> (_.amulet),
    "Кольцо 1"    -> (_.firstRing),
    "Кольцо 2"    -> (_.secondRing),
    "Пояс"        -> (_.belt),
    "Правая рука" -> (_.weapon),
    "Левая рука"  -> (_.leftHand),
    "Доп. оружие" -> (_.additionalWeapon)
  )

  private def fight(
    hero:     Hero,
    nowMs:    Long,
    stored:   Long,
    itemStat: Item => Long,
    horn:     Long,
    trauma:   TraumaPenalties => Double,
    passive:  Option[Long => Long],
    gemPct:   Long,
    setPct:   Long,
    achPct:   Long,
    current:  Long
  ): View = {
    val eqSum = hero.equipment.allItems.map(itemStat).sum
  val own     = stored - eqSum
    var lines   = line("Собственная", own, Nil)
    lines = equipSlots.foldLeft(lines) { case (ls, (name, pick)) =>
      line(name, itemStat(pick(hero.equipment)), ls)
    }
    var acc = stored
    lines = line("Мастер Горн", horn, lines)
    acc += horn
    val beforeTrauma = acc
    val traumaPct    = trauma(hero.combinedPenalties(nowMs))
    val afterTrauma  = (beforeTrauma * (1.0 - traumaPct)).toLong.max(1L)
    lines = line("Травмы", afterTrauma - beforeTrauma, lines)
    acc = afterTrauma
    passive.foreach { add =>
      val d = add(acc)
      lines = line("Пассивки", d, lines)
      acc += d
    }
    if (gemPct != 0L) {
      val d = acc * gemPct / 100L
      lines = line("Камни", d, lines)
      acc += d
    }
    if (setPct != 0L) {
      val d = acc * setPct / 100L
      lines = line("Наборы", d, lines)
      acc += d
    }
    if (achPct != 0L) {
      val d = acc * achPct / 100L
      lines = line("Достижения", d, lines)
      acc += d
    }
    View(current, lines)
  }

  def attack(hero: Hero, nowMs: Long): View =
    fight(
      hero, nowMs, hero.fightStats.atk, _.attack, hero.masterHornBoosts.attack,
      _.atkPct, None, 0L, hero.sets.attackBonusPct, Achievement.attackPct(hero),
      hero.effectiveFightStats(nowMs).atk)

  def defence(hero: Hero, nowMs: Long): View =
    fight(
      hero, nowMs, hero.fightStats.defence, _.defence, hero.masterHornBoosts.defence,
      _.defPct, Some(hero.passives.defenceFlatBonus), hero.gems.defenceBonusPct, hero.sets.defenceBonusPct,
      Achievement.defencePct(hero), hero.effectiveFightStats(nowMs).defence)

  def accuracy(hero: Hero, nowMs: Long): View = {
    val g = hero.gems
    val gemPct = g.accuracyBonusPct + g.airEvasionAccuracyBonusPct
    fight(
      hero, nowMs, hero.fightStats.accuracy, _.accuracy, hero.masterHornBoosts.accuracy,
      _.accPct, Some(hero.passives.accuracyFlatBonus), gemPct, hero.sets.accuracyBonusPct,
      Achievement.accuracyPct(hero), hero.effectiveFightStats(nowMs).accuracy)
  }

  def evasion(hero: Hero, nowMs: Long): View = {
    val g = hero.gems
    val gemPct = g.evasionBonusPct + g.airEvasionAccuracyBonusPct
    fight(
      hero, nowMs, hero.fightStats.evasion, _.evasion, hero.masterHornBoosts.evasion,
      _.evasionPct, None, gemPct, hero.sets.evasionBonusPct, Achievement.evasionPct(hero),
      hero.effectiveFightStats(nowMs).evasion)
  }

  /** Раскладка силы, тела, ловкости или ума — как в [[Hero.effectiveBaseStats]]. */
  def base(hero: Hero, nowMs: Long, which: String): View = {
    val p   = hero.combinedPenalties(nowMs)
    val b   = HeroRaceBuff.of(hero.race)
    val s   = hero.sets
    val cur = which match {
      case "str" => hero.effectiveBaseStats(nowMs).str
      case "vit" => hero.effectiveBaseStats(nowMs).vit
      case "agi" => hero.effectiveBaseStats(nowMs).agi
      case "int" => hero.effectiveBaseStats(nowMs).int
      case _     => 1L
    }
    val (baseVal, achFlat, traumaPct, potionFactor, setPct, achPct) = which match {
      case "str" =>
        (hero.baseStats.str, Achievement.strBonus(hero), p.strPct, hero.statBoosts.strFactor(nowMs), s.strBonusPct, 0L)
      case "vit" =>
        (hero.baseStats.vit, Achievement.vitBonus(hero), p.vitPct, hero.statBoosts.vitFactor(nowMs), s.vitBonusPct, 0L)
      case "agi" =>
        (hero.baseStats.agi, Achievement.agiBonus(hero), 0.0, hero.statBoosts.agiFactor(nowMs), s.agiBonusPct, 0L)
      case "int" =>
        (hero.baseStats.int, Achievement.intBonus(hero), p.intPct, hero.statBoosts.intFactor(nowMs), s.intBonusPct,
          Achievement.intPct(hero))
      case _ => (0L, 0L, 0.0, 1.0, 0L, 0L)
    }
    var lines = line("Собственная", baseVal, Nil)
    lines = line("Достижения", achFlat, lines)
    val withAch = baseVal + achFlat
    val afterRace = which match {
      case "str" => b.applyStr(withAch)
      case "vit" => b.applyVit(withAch)
      case "agi" => b.applyAgi(withAch)
      case "int" => b.applyInt(withAch)
      case _     => withAch
    }
    lines = line("Раса", afterRace - withAch, lines)
    var acc = afterRace.toDouble
    if (traumaPct > 0.0) {
      val before = acc.toLong
      acc = acc * (1.0 - traumaPct)
      lines = line("Травмы", acc.toLong - before, lines)
    }
    if (potionFactor != 1.0) {
      val before = acc.toLong
      acc = acc * potionFactor
      lines = line("Зелья", acc.toLong - before, lines)
    }
    if (setPct != 0L) {
      val before = acc.toLong
      acc = acc * (100L + setPct) / 100.0
      lines = line("Наборы", acc.toLong - before, lines)
    }
    if (achPct != 0L) {
      val before = acc.toLong
      acc = acc * (100L + achPct) / 100.0
      lines = line("Достижения (%)", acc.toLong - before, lines)
    }
    View(cur, lines)
  }
}