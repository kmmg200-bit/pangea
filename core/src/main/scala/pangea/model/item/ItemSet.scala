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

    /** Приноровился (порог 6): +% к атаке и защите за каждый прошедший раунд
      * боя, до потолка. */
    val RoundPct: Long    = 1L
    val RoundCapPct: Long = 6L

    /** Порог 9: на сколько % короче травмы и отдых после смерти. */
    val TraumaCutPct: Long    = 50L
    val DeathRestCutPct: Long = 50L

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$BaseStatsPct% к силе, телосложению, ловкости и интеллекту."),
      SetBonus(6,  s"Каждый раунд боя +$RoundPct% к атаке и защите, до +$RoundCapPct%."),
      SetBonus(9,  "Травмы проходят вдвое быстрее, а отдых после смерти вдвое короче."),
      SetBonus(12, "Раз за бой удар, который убил бы вас, оставляет вам 1 HP и снимает с вас яд, " +
                   "кровотечение и пламя.", ownRaceOnly = true)
    )
  }

  // ── Тихая роща (эльфы) ──────────────────────────────────────────────────────
  case object QuietGrove extends ItemSet("Тихая роща", "Тихой рощи") {
    override def race: Option[Race] = Some(Race.Elf)
    val AccuracyPct: Long = SetRates.RaceStatPct

    /** Удар из засады (порог 6): сила первого удара боя, в % от обычного. */
    val AmbushDamagePct: Long = 150L

    /** Порог 9: шанс ослабить врага ударом по HP, на сколько % и на сколько раундов. */
    val WeakenChancePct: Long = 25L
    val WeakenPct: Long       = 15L
    val WeakenTurns: Int      = 2

    /** Глаз лучника (порог 12): на сколько мест дальше бьют лук и арбалет и на
      * сколько % слабее их штраф вблизи. */
    val RangedReachBonus: Int     = 1
    val ClosePenaltyCutPct: Long  = 50L

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$AccuracyPct% к точности."),
      SetBonus(6,  "Первый ваш удар в каждом бою всегда попадает и бьёт в полтора раза сильнее."),
      SetBonus(9,  s"$WeakenChancePct% шанс, что ваша атака, нанёсшая урон по HP, ослабит врага: " +
                   s"−$WeakenPct% к его атаке на $WeakenTurns раунда."),
      SetBonus(12, s"+$RangedReachBonus к досягаемости лука и арбалета, а штраф за выстрел вблизи вдвое меньше.",
               ownRaceOnly = true)
    )
  }

  // ── Мать Глубин (мурлоки) ───────────────────────────────────────────────────
  case object DeepMother extends ItemSet("Мать Глубин", "Матери Глубин") {
    override def race: Option[Race] = Some(Race.Murloc)
    val EvasionPct: Long = SetRates.RaceStatPct

    /** Скользкая чешуя (порог 6): с каким шансом попавший обычный удар
      * соскальзывает и на сколько % слабее проходит. */
    val SlipChancePct: Long = 15L
    val SlipCutPct: Long    = 50L

    /** Сколько HP и брони возвращается в конце раунда (порог 9), в % от максимума. */
    val RegenPct: Long = 2L

    /** Зов глубин (порог 12): сколько собратьев выходит в начале боя и против
      * скольких врагов — на одного больше. */
    val KinCount: Int     = 1
    val KinManyFoes: Int  = 3

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$EvasionPct% к уклонению."),
      SetBonus(6,  s"$SlipChancePct% шанс, что попавший по вам обычный удар соскользнёт по чешуе и пройдёт вполовину."),
      SetBonus(9,  s"В конце каждого раунда вы восстанавливаете $RegenPct% HP и $RegenPct% брони."),
      SetBonus(12, "В начале каждого боя из воды выходит мурлок-собрат и встаёт в строй; против трёх " +
                   "и больше врагов — двое.", ownRaceOnly = true)
    )
  }

  // ── Орда (орки) ─────────────────────────────────────────────────────────────
  case object Horde extends ItemSet("Орда", "Орды") {
    override def race: Option[Race] = Some(Race.Orc)
    val HpPct: Long = SetRates.RaceStatPct

    /** Упрямство (порог 6): удар, снявший разом больше этой доли HP (в % от
      * максимума), проходит на столько % слабее. */
    val StubbornHitPct: Long = 20L
    val StubbornCutPct: Long = 25L

    /** Ярость (порог 9): пока HP ниже этой доли максимума — +% к атаке. Та же
      * черта включает и вампиризм порога 12. */
    val RageBelowPct: Long  = 50L
    val RageAttackPct: Long = 15L

    /** Орочья кровь (порог 12): +% к атаке за каждый полный шаг недостающего
      * HP, до потолка, и вампиризм ниже `RageBelowPct`. */
    val BloodStepPct: Long      = 10L
    val BloodAttackPct: Long    = 3L
    val BloodCapPct: Long       = 30L
    val BloodLifestealPct: Long = 3L

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$HpPct% к HP."),
      SetBonus(6,  s"Удар, снявший разом больше $StubbornHitPct% вашего HP, проходит на $StubbornCutPct% слабее."),
      SetBonus(9,  s"Пока у вас меньше половины HP, +$RageAttackPct% к атаке."),
      SetBonus(12, s"+$BloodAttackPct% к атаке за каждые $BloodStepPct% недостающего HP, до +$BloodCapPct%; " +
                   s"пока у вас меньше половины HP — ещё и $BloodLifestealPct% вампиризма.", ownRaceOnly = true)
    )
  }

  // ── Пройдоха (гоблины) ──────────────────────────────────────────────────────
  case object Trickster extends ItemSet("Пройдоха", "Пройдохи") {
    override def race: Option[Race] = Some(Race.Goblin)
    val AttackPct: Long = SetRates.RaceStatPct

    /** Грязный приём (порог 6): с каким шансом удар по HP выжигает энергию и
      * сколько — в % от ПОТОЛКА энергии врага, как Молния. */
    val EnergyBurnChancePct: Long = 20L
    val EnergyBurnPct: Long       = 30L

    /** Сколько серебра сверху с боёв (порог 9), в %. */
    val SilverBonusPct: Long = 20L

    /** Карманы победителей (порог 12): шанс лишней вещи с каждого добитого
      * героем врага и на сколько % реже его окружают при бегстве. */
    val ExtraGearChancePct: Long = 10L
    val SurroundCutPct: Long     = 50L

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$AttackPct% к атаке."),
      SetBonus(6,  s"$EnergyBurnChancePct% шанс, что ваша атака, нанёсшая урон по HP, выжжет врагу " +
                   s"$EnergyBurnPct% энергии — как Молния, даже без неё в оружии."),
      SetBonus(9,  s"+$SilverBonusPct% серебра с боёв."),
      SetBonus(12, s"С каждого добитого вами врага $ExtraGearChancePct% шанс на лишнюю вещь в добыче, а " +
                   "окружить вас при бегстве вдвое труднее.", ownRaceOnly = true)
    )
  }

  // ── Преисподняя (демоны) ────────────────────────────────────────────────────
  case object Underworld extends ItemSet("Преисподняя", "Преисподней") {
    override def race: Option[Race] = Some(Race.Demon)
    val IntPct: Long = SetRates.RaceStatPct

    /** Порог 6: прибавка к шансу прока Огня (п.п.) и к урону по горящему врагу (%). */
    val IgniteChanceBonusPct: Long  = 10L
    val BurningDamageBonusPct: Long = 10L

    /** На сколько % дешевле активные умения (порог 9). */
    val SkillEnergyCutPct: Long = 15L

    /** Напор (порог 12): сколько % максимума энергии возвращает каждый добитый враг. */
    val KillEnergyPct: Long = 20L

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$IntPct% к интеллекту."),
      SetBonus(6,  s"Шанс поджечь врага увеличен на $IgniteChanceBonusPct%, а по горящему врагу вы бьёте " +
                   s"на $BurningDamageBonusPct% сильнее."),
      SetBonus(9,  s"Активные умения стоят на $SkillEnergyCutPct% меньше энергии."),
      SetBonus(12, s"Каждый добитый вами враг возвращает вам $KillEnergyPct% энергии.", ownRaceOnly = true)
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
                   "не оставляют на вас травм.", ownRaceOnly = true)
    )
  }

  // ── Пустынь (каджиты) ───────────────────────────────────────────────────────
  case object Desert extends ItemSet("Пустынь", "Пустынь") {
    override def race: Option[Race] = Some(Race.Khajiit)
    val AgiPct: Long = SetRates.RaceStatPct

    /** Порог 6: прибавка к шансу прока Воздуха (п.п.) и на сколько раундов
      * дольше держится попутный ветер. */
    val AirChanceBonusPct: Long = 10L
    val AirTurnsBonus: Int      = 1

    /** Порог 9: на сколько % дороже Ришелье покупает и дешевле продаёт. */
    val TradePct: Long = 10L

    /** Порог 12: на сколько % короче поход по карте клада и прибавка к уклонению. */
    val MapHuntCutPct: Long  = 25L
    val RoadEvasionPct: Long = 5L

    def bonuses: List[SetBonus] = List(
      SetBonus(3,  s"+$AgiPct% к ловкости."),
      SetBonus(6,  s"Шанс прока Воздуха увеличен на $AirChanceBonusPct%, а попутный ветер держится на раунд дольше."),
      SetBonus(9,  s"Торговец Ришелье платит вам на $TradePct% больше, а продаёт на $TradePct% дешевле."),
      SetBonus(12, s"Поход по карте клада на четверть короче. +$RoadEvasionPct% к уклонению.", ownRaceOnly = true)
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
