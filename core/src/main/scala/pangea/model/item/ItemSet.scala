package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.monster.Race

/** Набор («сет») снаряжения. Предмет из набора носит его имя вместо титула —
 *  третьим словом названия: «🔵 Прочный Шлем Каменного стража» (поэтому
 *  [[ItemSet.title]] стоит в родительном падеже, как обычные титулы).
 *
 *  Бонусы выдаются по порогам числа надетых предметов набора: у больших наборов
 *  2/4/6/8/10/12, у малых расовых — 3/6/9 и 12 (см. [[race]]). Пороги независимы
 *  между наборами — можно носить 12 предметов из шести разных наборов и
 *  получить шесть двойных бонусов. Считаются только 12 «сетовых» слотов (см.
 *  `Equipment.setItems`): фляга и доп. оружие в наборы не входят.
 *
 *  Числовые ставки живут ЗДЕСЬ, на варианте — как у [[PassiveKind]] и
 *  [[GemKind]]; точки применения читают готовый модификатор через
 *  `pangea.model.hero.HeroSets`. */
sealed abstract class ItemSet(val label: String, val title: String) extends EnumEntry {

  /** Бонусы набора по порогам, от 2 предметов и выше. */
  def bonuses: List[SetBonus]

  /** Бонус конкретного порога, если он у набора описан. */
  def bonusAt(pieces: Int): Option[SetBonus] = bonuses.find(_.pieces == pieces)

  /** Чей это набор: у малого расового — его раса (её легендарные мобы роняют
   *  вещи набора, ей одной открыт шаг 12). У больших наборов расы нет. */
  def race: Option[Race] = None
}

/** Один бонус набора: порог, текст для игрока и признак «механики ещё нет».
 *
 *  `active = false` — порог описан, показывается игроку, но в бою пока ничего не
 *  делает (как броневая грань Сапфира). Так помечены бонусы, под которые в игре
 *  нет системы: стихийный урон мобов и негативные эффекты на герое.
 *
 *  `ownRaceOnly` — скрытый шаг малого расового набора: он есть только у героя
 *  расы набора, а чужой расе его не видно вовсе (см. `HeroSets.visible`). */
final case class SetBonus(pieces: Int, text: String, active: Boolean = true, ownRaceOnly: Boolean = false)

/** Общие для всех наборов числа. Живут ОТДЕЛЬНО от компаньона [[ItemSet]]
 *  намеренно: если бы `case object`-ы читали их из своего же компаньона, его
 *  инициализация пошла бы изнутри их конструкторов — а компаньон в этот момент
 *  сам инициализирует их через `findValues`. Такой цикл на одном потоке кладёт в
 *  `values` null (см. регрессию в PassiveKindSpec), а на нескольких — намертво
 *  заклинивает инициализацию класса. */
object SetRates {

  /** Пороги, на которых набор выдаёт бонус. */
  val Thresholds: List[Int] = List(2, 4, 6, 8, 10, 12)

  /** Прибавка «двойного» порога у каждого набора — в % к своему стату. */
  val StatBonusPct: Long = 5L

  /** Общий для всех наборов бонус восьмого порога. */
  val HpFlatBonus: Long   = 300L
  val HpPctBonus: Long    = 10L
  val HpBonusText: String = s"+$HpFlatBonus к HP и +$HpPctBonus% к HP."

  /** Прибавка третьего порога малого расового набора — в % к своему стату:
   *  шагов у малого набора вдвое меньше, и каждый чуть сильнее, чем у большого. */
  val RaceStatPct: Long = 8L
}

object ItemSet extends Enum[ItemSet] {

  val values: IndexedSeq[ItemSet] = findValues

  // Реэкспорт ставок для потребителей — сами варианты читают их из SetRates.
  val Thresholds: List[Int] = SetRates.Thresholds
  val HpFlatBonus: Long     = SetRates.HpFlatBonus
  val HpPctBonus: Long      = SetRates.HpPctBonus
  val StatBonusPct: Long    = SetRates.StatBonusPct

  // ── Каменный страж ──────────────────────────────────────────────────────────
  case object StoneGuard extends ItemSet("Каменный страж", "Каменного стража") {
    val DefencePct: Long          = SetRates.StatBonusPct
    val ElementalTakenCutPct: Long = 20L
    val ArmorDamageCutPct: Long    = 20L
    val IgniteResistPct: Long      = 30L
    val LowHpThresholdPct: Long    = 30L
    val ArmorRestorePct: Long      = 30L

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$DefencePct% к защите."),
      SetBonus(4,  s"Любой стихийный урон на $ElementalTakenCutPct% слабее по вам."),
      SetBonus(6,  s"Урон по вашей броне дополнительно снижен на $ArmorDamageCutPct%."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, s"Шанс поджечь Вас снижен на $IgniteResistPct%."),
      SetBonus(12, s"При получении урона, после которого у вас остаётся менее $LowHpThresholdPct% HP, " +
                   s"вы восстанавливаете $ArmorRestorePct% брони.")
    )
  }

  // ── Дикое пламя ─────────────────────────────────────────────────────────────
  case object WildFlame extends ItemSet("Дикое пламя", "Дикого пламени") {
    val AttackPct: Long        = SetRates.StatBonusPct
    val FireDamageBonusPct: Long = 10L
    val BurnGrowthMult: Long   = 2L
    val IgniteChanceBonusPct: Long = 30L

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$AttackPct% к атаке."),
      SetBonus(4,  s"Стихийный урон огнём по HP и броне увеличен на $FireDamageBonusPct%."),
      SetBonus(6,  s"Рост урона от огня за раунд увеличен в $BurnGrowthMult раза."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, s"Шанс поджечь врага увеличен на $IgniteChanceBonusPct%."),
      SetBonus(12, "Ваши активные умения всегда поджигают врага. Каждый процент огня дополнительно " +
                   "снижает защиту противника на столько же процентов.")
    )
  }

  // ── Упырь ───────────────────────────────────────────────────────────────────
  case object Ghoul extends ItemSet("Упырь", "Упыря") {
    val EvasionPct: Long      = SetRates.StatBonusPct
    val LifestealPct: Long    = 2L
    val BleedChancePct: Long  = 30L
    val BleedPct: Int         = 4
    val KillHpRestorePct: Long    = 10L
    val KillArmorRestorePct: Long = 10L

    /** Сколько чужой крови достанется одному вампиру (порог 10).
      *
      * `bleedOnOtherSide` — урон, который кровотечение нанесло за раунд
      * ЧУЖОЙ стороне; `vampires` — сколько вампиров на СВОЕЙ. Кровь
      * делится ровно между ними: двое — каждому половина.
      *
      * Счёт идёт по сторонам, а не по всем кровоточащим сразу: вампиры
      * одной стороны пьют кровь только соперников, и наоборот; кто именно
      * заставил кровоточить, значения не имеет.
      *
      * Сегодня вампир в бою всегда один — сам герой, и правило отдаёт ему
      * всю кровь. Написано оно под будущее, где вампиров на стороне может
      * оказаться несколько. */
    def bleedHealShare(bleedOnOtherSide: Long, vampires: Int): Long =
      if (vampires <= 0 || bleedOnOtherSide <= 0L) 0L else bleedOnOtherSide / vampires.toLong

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$EvasionPct% к уклонению."),
      SetBonus(4,  s"+$LifestealPct% вампиризма."),
      SetBonus(6,  s"$BleedChancePct% шанс, что ваша атака, нанёсшая урон по HP, вызовет кровотечение $BleedPct%."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, "Урон от кровотечения врага также восстанавливает ваше HP."),
      SetBonus(12, "Ваши активные умения, наносящие урон, всегда накладывают кровотечение. " +
                   s"Каждый убитый враг тут же, в бою, восстанавливает вам $KillHpRestorePct% HP и $KillArmorRestorePct% брони.")
    )
  }

  // ── Охотник ─────────────────────────────────────────────────────────────────
  case object Hunter extends ItemSet("Охотник", "Охотника") {
    val AccuracyPct: Long      = SetRates.StatBonusPct
    val EnergyPct: Long        = 10L
    val AgiRegenMult: Long     = 2L
    val RepeatChancePct: Long  = 25L
    val BlockSkillChancePct: Long = 10L
    val SkillCritBonusPct: Long = 10L

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$AccuracyPct% к точности."),
      SetBonus(4,  s"+$EnergyPct% к энергии. Ловкость восстанавливает в $AgiRegenMult раза больше энергии за раунд."),
      SetBonus(6,  s"$RepeatChancePct% шанс повторить атаку при промахе по врагу. Один раз за раунд."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, s"$BlockSkillChancePct% шанс заблокировать урон от атакующего умения противника."),
      SetBonus(12, s"+$SkillCritBonusPct% к шансу критического применения всех атакующих умений.")
    )
  }

  // ── Крыса ───────────────────────────────────────────────────────────────────
  case object Rat extends ItemSet("Крыса", "Крысы") {
    val ArmorPct: Long        = SetRates.StatBonusPct
    val PoisonChancePct: Long = 30L
    val PoisonPct: Int        = 3
    val SummonChancePct: Long = 5L
    val DotTakenCutPct: Long  = 50L

    /** На сколько п.п. за раунд слабеет кровотечение на герое (порог 10): до
      * набора оно не затухало вовсе — только лечение снимало его целиком. Шаг
      * тот же, что у яда (`Poison.DecayPerRound`), чтобы правило читалось одно на
      * оба эффекта; число здесь своё — вариант enum не ходит за ним в другой
      * пакет (см. комментарий к [[SetRates]] про циклы инициализации). */
    val BleedDecayPerRound: Int = 2

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$ArmorPct% к броне."),
      SetBonus(4,  s"$PoisonChancePct% шанс, что ваша атака, нанёсшая урон по HP, отравит врага на $PoisonPct%."),
      SetBonus(6,  s"$SummonChancePct% шанс за раунд, что из-под ваших ног выскочит крыса и встанет в строй. " +
                   "После боя она остаётся при вас на сутки."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, s"Яд и кровотечение на вас снимают вдвое меньше HP, а кровотечение вдобавок затухает " +
                   s"на $BleedDecayPerRound% в раунд, как яд."),
      SetBonus(12, "Из-под ваших ног выскакивают Чумные крысы. Если в начале боя при вас нет ни одной крысы, " +
                   "одна приходит сразу.")
    )
  }

  // ── Некромант ───────────────────────────────────────────────────────────────
  case object Necromancer extends ItemSet("Некромант", "Некроманта") {
    val IntPct: Long         = 10L
    val UndeadBoostPct: Long = 20L
    val EnergyCutPct: Long   = 10L

    /** Во сколько раз дольше держится поднятый с алтаря (порог 6). */
    val UndeadLastsMult: Long = 2L

    // ── Миазмы тьмы (порог 10) ────────────────────────────────────────────────
    // Сама механика общая — аура, которая держит всё поле боя. Числа у неё свои
    // (pangea.model.battle.AuraRates), здесь они нужны только для описания.

    /** Герой сам становится нежитью: кровь и яд его не берут, зато огонь берёт
      * сильнее на столько процентов, а горение на нём растёт во столько раз. */
    val UndeadFireTakenPct: Long   = 25L
    val UndeadBurnGrowthMult: Long = 2L

    // ── Восставшие (порог 12) ─────────────────────────────────────────────────
    val RiseChancePct: Long = 5L
    val RiseHpPct: Long     = 25L

    def bonuses: List[SetBonus] = List(
      SetBonus(2,  s"+$IntPct% к интеллекту."),
      SetBonus(4,  s"Ожившие союзники на $UndeadBoostPct% сильнее. " +
                   s"Ваша максимальная энергия на $EnergyCutPct% ниже."),
      SetBonus(6,  s"Поднятый с алтаря служит в $UndeadLastsMult раза дольше и выходит в бой целым."),
      SetBonus(8,  SetRates.HpBonusText),
      SetBonus(10, s"Вы сами становитесь нежитью: кровь и яд вас больше не берут, зато огонь бьёт на " +
                   s"$UndeadFireTakenPct% сильнее, а горите вы в $UndeadBurnGrowthMult раза жарче.\n" +
                   s"Вы держите аурой всё поле боя — миазмы тьмы. В конце раунда на них уходит " +
                   s"${pangea.model.battle.AuraRates.MiasmaEnergyCostPct}% вашей энергии, и на столько же, " +
                   s"сверх ${pangea.model.battle.AuraRates.MiasmaPct}% максимума HP, вся нежить на поле " +
                   s"лечится, а всё живое — теряет. Энергии меньше " +
                   s"${pangea.model.battle.AuraRates.MiasmaMinEnergy} — аура оседает."),
      SetBonus(12, s"Враг, добитый вами, с шансом $RiseChancePct% восстаёт нежитью и идёт за вами двое " +
                   s"суток: с $RiseHpPct% HP и той бронёй, что на нём осталась. Элементали, Гнилой Джо " +
                   "и Крысиный король не восстают — в них некого поднимать.")
    )
  }

  // ── Малые расовые наборы ────────────────────────────────────────────────────
  // По одному на каждую смертную расу. Слабее больших: пороги 3/6/9 открыты
  // всем, а скрытый 12 — только герою расы набора, и чужой расе его не видно
  // вовсе. Вещи роняет каждый легендарный моб этой расы — ровно одну (см.
  // LootGenerator.raceSetGear). Бонусы с `active = false` ждут своей механики.

  // ── Первопроходец (люди) ────────────────────────────────────────────────────
  case object Pioneer extends ItemSet("Первопроходец", "Первопроходца") {
    override def race: Option[Race] = Some(Race.Human)

    /** Люди ни в чём не выделяются — и ни в чём не уступают: понемногу ко всем
      * четырём характеристикам. */
    val BaseStatsPct: Long = 3L

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$BaseStatsPct% к силе, телосложению, ловкости и интеллекту."),
      SetBonus(6,  "Каждый раунд боя +1% к атаке и защите, до +6%.", active = false),
      SetBonus(9,  "Травмы проходят вдвое быстрее, а отдых после смерти на треть короче.", active = false),
      SetBonus(12, "Раз за бой удар, который убил бы вас, оставляет вам 1 HP и снимает с вас яд, " +
                   "кровотечение и пламя.", active = false, ownRaceOnly = true)
    )
  }

  // ── Тихая роща (эльфы) ──────────────────────────────────────────────────────
  case object QuietGrove extends ItemSet("Тихая роща", "Тихой рощи") {
    override def race: Option[Race] = Some(Race.Elf)
    val AccuracyPct: Long = SetRates.RaceStatPct

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$AccuracyPct% к точности."),
      SetBonus(6,  "Первый ваш удар в каждом бою всегда попадает и бьёт в полтора раза сильнее.", active = false),
      SetBonus(9,  "25% шанс, что ваша атака, нанёсшая урон по HP, ослабит врага: −15% к его атаке " +
                   "на 2 раунда.", active = false),
      SetBonus(12, "40% шанс, что ваша атака, нанёсшая урон по HP, отравит врага на 3%, и ваш яд " +
                   "не слабеет со временем.", active = false, ownRaceOnly = true)
    )
  }

  // ── Мать Глубин (мурлоки) ───────────────────────────────────────────────────
  case object DeepMother extends ItemSet("Мать Глубин", "Матери Глубин") {
    override def race: Option[Race] = Some(Race.Murloc)
    val EvasionPct: Long = SetRates.RaceStatPct

    /** Сколько HP и брони возвращается в конце раунда (порог 9), в % от максимума. */
    val RegenPct: Long = 2L

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$EvasionPct% к уклонению."),
      SetBonus(6,  "15% шанс, что попавший по вам обычный удар соскользнёт по чешуе и пройдёт вполовину.",
               active = false),
      SetBonus(9,  s"В конце каждого раунда вы восстанавливаете $RegenPct% HP и $RegenPct% брони."),
      SetBonus(12, "В начале каждого боя из воды выходит мурлок-собрат и встаёт в строй; против трёх " +
                   "и больше врагов — двое.", active = false, ownRaceOnly = true)
    )
  }

  // ── Орда (орки) ─────────────────────────────────────────────────────────────
  case object Horde extends ItemSet("Орда", "Орды") {
    override def race: Option[Race] = Some(Race.Orc)
    val HpPct: Long = SetRates.RaceStatPct

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$HpPct% к HP."),
      SetBonus(6,  "Удар, снявший разом больше 20% вашего HP, проходит на 25% слабее.", active = false),
      SetBonus(9,  "Пока у вас меньше половины HP, +15% к атаке.", active = false),
      SetBonus(12, "+3% к атаке за каждые 10% недостающего HP, до +30%; пока у вас меньше половины HP — " +
                   "ещё и 3% вампиризма.", active = false, ownRaceOnly = true)
    )
  }

  // ── Мародёр (гоблины) ───────────────────────────────────────────────────────
  case object Marauder extends ItemSet("Мародёр", "Мародёра") {
    override def race: Option[Race] = Some(Race.Goblin)
    val AttackPct: Long = SetRates.RaceStatPct

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$AttackPct% к атаке."),
      SetBonus(6,  "20% шанс, что ваша атака, нанёсшая урон по HP, выжжет врагу 30% энергии — как " +
                   "Молния, даже без неё в оружии.", active = false),
      SetBonus(9,  "+20% серебра с боёв.", active = false),
      SetBonus(12, "С каждого добитого вами врага 10% шанс на лишнюю вещь в добыче, а окружить вас " +
                   "при бегстве вдвое труднее.", active = false, ownRaceOnly = true)
    )
  }

  // ── Преисподняя (демоны) ────────────────────────────────────────────────────
  case object Underworld extends ItemSet("Преисподняя", "Преисподней") {
    override def race: Option[Race] = Some(Race.Demon)
    val IntPct: Long = SetRates.RaceStatPct

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$IntPct% к интеллекту."),
      SetBonus(6,  "Шанс поджечь врага увеличен на 10%, а по горящему врагу вы бьёте на 10% сильнее.",
               active = false),
      SetBonus(9,  "Активные умения стоят на 15% меньше энергии.", active = false),
      SetBonus(12, "Каждый добитый вами враг возвращает вам 20% энергии.", active = false, ownRaceOnly = true)
    )
  }

  // ── Подгорный мастер (гномы) ────────────────────────────────────────────────
  case object MountainMaster extends ItemSet("Подгорный мастер", "Подгорного мастера") {
    override def race: Option[Race] = Some(Race.Gnome)

    /** Прибавка к ПОТОЛКУ брони (порог 3) — как у двойки «Крысы». */
    val ArmorPct: Long = SetRates.RaceStatPct

    /** Сколько брони возвращается в конце раунда (порог 6), в % от максимума. */
    val ArmorRegenPct: Long = 4L

    /** Прибавка к шансу прока Холода, в п.п. (порог 9). */
    val ColdChanceBonusPct: Long = 15L

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$ArmorPct% к броне."),
      SetBonus(6,  s"В конце каждого раунда ваша броня восстанавливается на $ArmorRegenPct%."),
      SetBonus(9,  s"Шанс сковать врага холодом увеличен на $ColdChanceBonusPct%."),
      SetBonus(12, "Пока у вас есть броня, вас нельзя оглушить, сбить с ног или пригвоздить, и удары " +
                   "не оставляют на вас травм.", active = false, ownRaceOnly = true)
    )
  }

  // ── Караван (каджиты) ───────────────────────────────────────────────────────
  case object Caravan extends ItemSet("Караван", "Каравана") {
    override def race: Option[Race] = Some(Race.Khajiit)
    val AgiPct: Long = SetRates.RaceStatPct

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$AgiPct% к ловкости."),
      SetBonus(6,  "Шанс прока Воздуха увеличен на 10%, а попутный ветер держится на раунд дольше.",
               active = false),
      SetBonus(9,  "Торговцы платят вам на 10% больше, а продают на 10% дешевле.", active = false),
      SetBonus(12, "Поход по карте клада на четверть короче. +5% к уклонению.", active = false,
               ownRaceOnly = true)
    )
  }

  /** Набор по имени-титулу из названия предмета (третье слово). */
  def byTitle(title: String): Option[ItemSet] = values.find(_.title == title)

  /** Малый набор расы: у каждой смертной — свой, у прочих (элементали, нежить,
   *  звери, сооружения) — нет. */
  def ofRace(race: Race): Option[ItemSet] = values.find(_.race.contains(race))

  implicit val encoder: Encoder[ItemSet] = (s: ItemSet) => s.entryName.asJson
  implicit val decoder: Decoder[ItemSet] = (c: HCursor) => c.as[String].map(ItemSet.withName)
}
