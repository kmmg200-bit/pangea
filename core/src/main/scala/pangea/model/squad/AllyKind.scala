package pangea.model.squad

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.battle.{Element, GroupState}
import pangea.model.item.GripRates
import pangea.model.monster.Race
import pangea.model.stats.FightStats

/** Наёмник-союзник: кто он, чем бьёт и сколько стоит. Сила растёт с уровнем
 *  героя (`AliensLvL` = уровень героя): статы — ставка × уровень, но только до
 *  своего потолка ([[maxLvl]]): дальше наёмник не растёт, и герой уходит
 *  вперёд сам. Умения у всех одни и те же ([[AllySkill]]), различаются лишь
 *  числа и стихия удара. */
sealed abstract class AllyKind(
  val name:     String,
  val race:     Race,
  /** Чем бьёт: прок этой стихии роллится на его ударах. None — ничем, просто
    * зубами (крыса, прибежавшая на набор «Крыса»). */
  val element:  Option[Element],
  /** Выше этого уровня наёмник не становится сильнее. */
  val maxLvl:   Long,
  val hpPerLvl:       Long,
  val armorPerLvl:    Long,
  val atkPerLvl:      Long,
  val energyPerLvl:   Long,
  val accuracyPerLvl: Long,
  val defencePerLvl:  Long,
  val evasionPerLvl:  Long
) extends EnumEntry {

  /** Сколько мест в обе стороны достаёт его удар: соседей, как у всех
    * ([[GroupState.Reach]]), — а лучник дальше. */
  def reach: Int = GroupState.Reach

  /** Стреляет: вблизи — по тому, кто напротив, и по соседям — бьёт слабее,
    * как лук героя ([[GripRates.RangedClosePenaltyPct]]). */
  def ranged: Boolean = false

  /** Шанс (в %), что его удар, дошедший до HP, оставит в ране яд. */
  def poisonPct: Long = 0L

  /** Пользуется ли умениями ([[AllySkill]]). Крыса не умеет ничего, кроме
    *  укуса: ни фляги, ни починки брони, ни дробящего удара. */
  def usesSkills: Boolean = true

  /** Уровень, по которому считаются статы: не выше потолка наёмника. `grown` —
    * на сколько этот потолок поднялся у героя после личного задания наёмника
    * (см. [[AllyRates.QuestGrowth]], `MercQuests`). */
  def effectiveLvl(lvl: Long, grown: Long = 0L): Long = lvl.max(1L).min(maxLvl + grown.max(0L))

  def stats(lvl: Long, grown: Long = 0L): FightStats = {
    val l = effectiveLvl(lvl, grown)
    FightStats(
      atk      = atkPerLvl * l,
      hp       = hpPerLvl * l,
      armor    = armorPerLvl * l,
      defence  = defencePerLvl * l,
      evasion  = evasionPerLvl * l,
      accuracy = accuracyPerLvl * l,
      energy   = energyPerLvl * l
    )
  }

  /** Сколько энергии восстанавливает за раунд — тоже по своему потолку. */
  def energyRegen(lvl: Long, grown: Long = 0L): Long = AllyRates.EnergyRegenPerLvl * effectiveLvl(lvl, grown)
}

/** Числа союзников — отдельно от компаньона (см. FlaskRates). */
object AllyRates {
  val EnergyRegenPerLvl: Long = 7L

  /** Найм Йоргена: столько дублонов, сколько бы герой ни был силён. */
  val HumanDoubloons: Long = 10L
  /** Найм Плюха, Брамбла и Фаэлина: отваров — уровень героя на это, но не
    * меньше одного. */
  val BrewPerLevels: Long = 5L

  /** Найм Фаэлина: сверх смазки — столько дублонов, сколько бы герой ни был силён. */
  val ElfDoubloons: Long = 5L

  /** На сколько уровней поднимается потолок наёмника у героя, выполнившего его
    * личное задание (см. `MercQuests`): Йорген растёт до 13-го, а не до 5-го. */
  val QuestGrowth: Long = 8L

  /** Как часто яд Фаэлина остаётся в ране — как у эльфов в лабиринте: через раз. */
  val ElfPoisonPct: Long = pangea.model.battle.Poison.WeaponChancePct

  /** Сколько союзник отсутствует после свитка (сутки). */
  val AwayMs: Long = 24L * 60L * 60L * 1000L

  /** На сколько нанимается наёмник — и сколько потом отдыхает в отлучке. */
  val HireMs: Long = 12L * 60L * 60L * 1000L
  val OffDutyMs: Long = 12L * 60L * 60L * 1000L

  /** Насколько хватает тёмной силы алтаря: сутки с минуты, когда кости
    * поднялись. Дальше они рассыпаются, и ждать этого союзника неоткуда. */
  val UndeadMs: Long = 24L * 60L * 60L * 1000L

  /** Сколько крыса, выскочившая на набор «Крыса», держится при герое после боя:
   *  сутки, потом убегает обратно под землю. Ждать её неоткуда — придёт новая. */
  val RatMs: Long = 24L * 60L * 60L * 1000L

  /** Восставший по «Некроманту» (порог 12) служит двое суток — вдвое дольше
   *  поднятого с алтаря. Порог 6 этот срок уже не удлиняет: он про алтарь. */
  val RisenLasts: Long = 2L

  /** Сколько бывший пленник с заставы разбойников держится при герое: 48 часов
   *  (так решил пользователь), потом уходит домой. */
  val CaptiveMs: Long = 48L * 60L * 60L * 1000L

  /** Мест в строю вместе с героем: герой и до десяти союзников. Число берём из
    * единой схемы строя — у врагов мест больше, и за одиннадцатым начинается
    * хвост, куда отряду не дотянуться (см. [[pangea.model.battle.Formation]]). */
  val Positions: Int = pangea.model.battle.Formation.HeroPlaces

  /** Сколько союзников ждёт в резерве — при герое, но не в строю. */
  val ReserveSize: Int = 10
}

object AllyKind extends Enum[AllyKind] {

  /** Йорген Кремень: воин, бьёт огнём. Растёт до пятого уровня — он и сам
    * говорит, что помнит только строй, а не подвиги. */
  case object Human extends AllyKind("Йорген Кремень", Race.Human, Some(Element.Fire), maxLvl = 5L,
    hpPerLvl = 125L, armorPerLvl = 150L, atkPerLvl = 20L, energyPerLvl = 100L,
    accuracyPerLvl = 100L, defencePerLvl = 40L, evasionPerLvl = 100L)

  /** Плюх: мурлок-ловкач, бьёт молнией. Тянется дальше всех — до десятого. */
  case object Murloc extends AllyKind("Плюх", Race.Murloc, Some(Element.Lightning), maxLvl = 10L,
    hpPerLvl = 100L, armorPerLvl = 100L, atkPerLvl = 30L, energyPerLvl = 100L,
    accuracyPerLvl = 150L, defencePerLvl = 30L, evasionPerLvl = 150L)

  /** Брамбл Медноус: гном-воин, бьёт холодом. Потолок — седьмой уровень. */
  case object Gnome extends AllyKind("Брамбл Медноус", Race.Gnome, Some(Element.Cold), maxLvl = 7L,
    hpPerLvl = 80L, armorPerLvl = 175L, atkPerLvl = 20L, energyPerLvl = 100L,
    accuracyPerLvl = 100L, defencePerLvl = 50L, evasionPerLvl = 60L)

  /** Фаэлин Тихострел: эльф-лучник. Стихии нет — вместо неё яд на
    * наконечниках, через раз. Тоньше Плюха, зато бьёт издалека, как лук героя,
    * и растёт так же — до десятого. На цветочной поляне с ним цветов больше
    * (см. `FlowerMeadowState.ElfBonusFlowers`). */
  case object Elf extends AllyKind("Фаэлин Тихострел", Race.Elf, None, maxLvl = 10L,
    hpPerLvl = 70L, armorPerLvl = 60L, atkPerLvl = 35L, energyPerLvl = 100L,
    accuracyPerLvl = 170L, defencePerLvl = 20L, evasionPerLvl = 170L) {
    override def reach: Int      = GripRates.RangedReach
    override def ranged: Boolean = true
    override def poisonPct: Long = AllyRates.ElfPoisonPct
  }

  /** Дружинник «Пропавшей дружины»: гном-предводитель, который идёт с героем
    * только на бой у завала (см. LostSquadQuest.escortAllies). Имя, уровень и
    * статы у каждого свои — они лежат на самом союзнике ([[UndeadForm]]); нанять
    * его негде, а после боя он уходит со своими наверх и в отряд не встаёт.
    * Бьёт холодом, как вся его раса. */
  case object SquadGnome extends AllyKind("Дружинник", Race.Gnome, Some(Element.Cold), maxLvl = 150L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L)

  /** Поднятый с алтаря: раса, имя и статы у него свои — они лежат на самом
    * союзнике ([[UndeadForm]]), а не на виде. Здесь только то, что общее у всей
    * нежити: она бьёт холодом и в таверне не сидит. */
  case object Undead extends AllyKind("Поднятый", Race.Undead, Some(Element.Cold), maxLvl = 1L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L)

  /** Крыса, выскочившая из-под ног на набор «Крыса»: имя, уровень и статы у неё
    * свои — она тот же моб, что водится в канализации, только на стороне героя
    * (см. [[UndeadForm]] и `Squad.summonRat`). Стихии у неё нет, зубы и всё;
    * нанять её негде, а держится она сутки и потом убегает. */
  case object Rat extends AllyKind("Крыса", Race.Animal, None, maxLvl = 1L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L) {
    override def usesSkills: Boolean = false
  }

  /** Мурлок-собрат, вышедший из воды на набор «Мать Глубин» (порог 12): статы
    * у него свои — редкий мурлок уровня героя (см. [[UndeadForm]] и
    * `BattleState.kinAlly`). Он идёт с героем только на этот бой, а после боя
    * уходит обратно в воду и в отряд не встаёт. Стихии нет — бьёт как мурлок. */
  case object MurlocKin extends AllyKind("Мурлок-собрат", Race.Murloc, None, maxLvl = 1L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L)

  /** Элиар из «Того, кто уговаривает деревья»: эльф, который поёт изгороди и
    * бьётся рядом с героем, пока на песню идут орки, — по силе именной эльф
    * уровня задания. Имя, уровень и статы лежат на самом союзнике
    * ([[UndeadForm]], см. HedgeSongQuest.elf); нанять его негде, после боя он
    * в отряд не встаёт. Стихии нет. */
  case object HedgeElf extends AllyKind("Эльф-певец", Race.Elf, None, maxLvl = 150L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L)

  /** Капитан каджитского каравана из «Большого дела»: когда на сделку пришёл
    * собрат-каджит, двое капитанов встают рядом с ним против ночной стражи — по
    * силе редкий каджит уровня задания. Имя, уровень и статы лежат на самом
    * союзнике ([[UndeadForm]], см. BigDealQuest.captain); нанять негде, после
    * боя в отряд не встаёт. Стихии нет. */
  case object CaravanKhajiit extends AllyKind("Каджит-капитан", Race.Khajiit, None, maxLvl = 150L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L)

  /** Караванщик обоза деревни (контракт «Сопровождение каравана»): в засаде он
    * бьётся рядом с героем — по силе редкий той же расы, что деревня, уровня
    * задания. Имя, уровень и статы лежат на самом союзнике ([[UndeadForm]], см.
    * Villages.caravanGuard); нанять негде, после боя в отряд не встаёт. Стихии
    * нет. */
  case object CaravanGuard extends AllyKind("Караванщик", Race.Human, None, maxLvl = 150L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L)
  /** Бывший пленник с заставы разбойников (контракт «Логово разбойников»): его
    * освободили, и он попросился в отряд. По силе — редкий своей расы уровня
    * задания; раса в имени, а имя, уровень и статы лежат на самом союзнике
    * ([[UndeadForm]], см. Villages.freedCaptive). Нанять негде; держится при
    * герое двое суток ([[AllyRates.CaptiveMs]], так решил пользователь) и
    * уходит домой. Стихии нет. */
  case object FreedCaptive extends AllyKind("Бывший пленник", Race.Human, None, maxLvl = 150L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L)

  /** Мастер Горн в подвале совета (глава 6 «Наутро», путь Щита): встаёт рядом
    * с героем против чистильщиков Братства — по силе легендарный человек уровня
    * задания. Имя, уровень и статы лежат на самом союзнике ([[UndeadForm]], см.
    * StoryMorningQuest.horn); нанять негде, после боя в отряд не встаёт.
    * Стихии нет. */
  case object GuildMaster extends AllyKind("Мастер Гильдии", Race.Human, None, maxLvl = 150L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L)

  /** Искатель, которого Горн привёл в подвал совета (там же): по силе редкий
    * человек уровня задания; имя и статы — на самом союзнике, после боя в
    * отряд не встаёт. Стихии нет. */
  case object GuildSeeker extends AllyKind("Искатель Гильдии", Race.Human, None, maxLvl = 150L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L)

  /** Барти со сковородой (глава 6, путь «Сам по себе»): дерётся у себя дома
    * против людей Братства — плохо, по силе необычный человек уровня задания.
    * Имя и статы — на самом союзнике, после боя в отряд не встаёт. Стихии нет. */
  case object Informant extends AllyKind("Осведомитель", Race.Human, None, maxLvl = 150L,
    hpPerLvl = 0L, armorPerLvl = 0L, atkPerLvl = 0L, energyPerLvl = 0L,
    accuracyPerLvl = 0L, defencePerLvl = 0L, evasionPerLvl = 0L)

  val values: IndexedSeq[AllyKind] = findValues

  /** Кого можно нанять за столом таверны. Нежить и крысы туда не садятся. */
  val hireable: List[AllyKind] = List(Human, Murloc, Gnome, Elf)

  /** Сколько отваров берут Плюх, Брамбл и Фаэлин на этом уровне героя. */
  def brewsFor(lvl: Long): Long = (lvl / AllyRates.BrewPerLevels).max(1L)

  implicit val encoder: Encoder[AllyKind] = (k: AllyKind) => k.entryName.asJson
  implicit val decoder: Decoder[AllyKind] = (c: HCursor) => c.as[String].map(AllyKind.withName)
}

/** Умения союзника — одни на всех: применяются случайно из тех, на что хватает
 *  энергии и что сейчас к месту. */
sealed abstract class AllySkill(val costPerLvl: Long) extends EnumEntry {
  def cost(lvl: Long): Long = costPerLvl * lvl
}

object AllySkill extends Enum[AllySkill] {
  /** Быстрый удар: половина атаки. */
  case object QuickStrike     extends AllySkill(5L)
  /** Исцеляющая фляга: 30% макс. HP, только раненому. */
  case object HealingFlask    extends AllySkill(8L)
  /** Экстренная починка: 20% макс. брони, только с побитой бронёй. */
  case object EmergencyRepair extends AllySkill(10L)
  /** Дробящий удар: 0,4 атаки прямо в HP, мимо брони и защиты. */
  case object CrushingStrike  extends AllySkill(16L)

  val values: IndexedSeq[AllySkill] = findValues

  val QuickStrikeFactor: Double    = 0.5
  val HealPct: Long                = 30L
  val RepairPct: Long              = 20L
  val CrushingStrikeFactor: Double = 0.4
}
