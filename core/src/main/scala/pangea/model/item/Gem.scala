package pangea.model.item

import enumeratum._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Вид камня-усилителя (череп/аметист/…). Как и [[PassiveKind]], все числовые
 *  ставки эффекта живут ЗДЕСЬ, на самом варианте — и текст описания, и боевая
 *  математика ([[pangea.model.hero.HeroGems]]) читают одни и те же константы.
 *
 *  Эффект камня зависит от того, во что он вставлен: в оружии — «оружейная» грань,
 *  в остальном снаряжении — «броневая». Стихийные грани (преобразование урона в
 *  холод/огонь/воздух/молнию и «эффективность магии») ОТЛОЖЕНЫ: системы стихий в
 *  бою пока нет, поэтому такие грани помечены как временно недействующие
 *  (см. [[GemKind.inertWeapon]]/[[inertArmor]]). */
sealed abstract class GemKind(val baseName: String) extends EnumEntry {

  /** Что даёт камень этого грейда в гнезде оружия — без приставки: так он
    * подписан в карточке вещи, где гнездо и так видно. */
  def weaponEffect(grade: Int): String

  /** Что даёт камень в гнезде прочего снаряжения (и щита) — без приставки. */
  def armorEffect(grade: Int): String

  /** Описание оружейной грани для данного грейда (инвентарь/дроп). */
  def weaponEffectText(grade: Int): String = s"В гнезде оружия: ${weaponEffect(grade)}"

  /** Описание броневой грани для данного грейда. */
  def armorEffectText(grade: Int): String = s"В гнезде снаряжения: ${armorEffect(grade)}"
}

object GemKind extends Enum[GemKind] {

  val values: IndexedSeq[GemKind] = findValues

  // ── Череп ─────────────────────────────────────────────────────────────────
  case object Skull extends GemKind("Череп") {
    val WeaponVampPctPerGrade: Long          = 1L  // вампиризм: % нанесённого по HP урона в лечение
    val WeaponEnergyRegenPctPerGrade: Long   = 1L  // +% к восстановлению энергии в бою
    val ArmorHpRegenPerMillePerGrade: Long   = 5L  // 0.5% макс.HP реген/раунд (в промилле)
    def weaponEffect(g: Int): String =
      s"вампиризм ${WeaponVampPctPerGrade * g}% нанесённого урона в HP и +${WeaponEnergyRegenPctPerGrade * g}% к восстановлению энергии."
    def armorEffect(g: Int): String =
      s"+${ArmorHpRegenPerMillePerGrade * g / 10.0}% макс. HP регенерация каждый раунд."
  }

  // ── Аметист ───────────────────────────────────────────────────────────────
  case object Amethyst extends GemKind("Аметист") {
    val WeaponAccuracyPctPerGrade: Long = 5L
    val ArmorDefencePctPerGrade: Long   = 1L
    def weaponEffect(g: Int): String = s"+${WeaponAccuracyPctPerGrade * g}% к итоговой точности."
    def armorEffect(g: Int): String  = s"+${ArmorDefencePctPerGrade * g}% к итоговой защите."
  }

  // ── Сапфир (стихия холода; броневая грань «магия» — отложена) ────────────────
  case object Sapphire extends GemKind("Сапфир") {
    def weaponEffect(g: Int): String = s"стихия Холода, +${2 * g}% к стихийному урону. +10% урона по броне."
    def armorEffect(g: Int): String  = s"+${1 * g}% к эффективности магии. (пока не действует)"
  }

  // ── Изумруд ──────────────────────────────────────────────────────────────
  case object Emerald extends GemKind("Изумруд") {
    val WeaponPoisonTenthPctPerGrade: Long = 15L // 1.5% яда за грейд (в десятых долях %)
    val ArmorEvasionPctPerGrade: Long      = 1L
    def weaponEffect(g: Int): String =
      s"при уроне по HP накладывает ${WeaponPoisonTenthPctPerGrade * g / 10.0}% яда."
    def armorEffect(g: Int): String = s"+${ArmorEvasionPctPerGrade * g}% к итоговому уклонению."
  }

  // ── Рубин (стихия огня; броневая грань — макс. HP) ───────────────────────────
  case object Ruby extends GemKind("Рубин") {
    val ArmorMaxHpPctPerGrade: Long   = 1L
    val ArmorFlatHpBase: Long         = 10L
    val ArmorFlatHpPerExtraGrade: Long = 30L
    def flatHp(g: Int): Long = ArmorFlatHpBase + ArmorFlatHpPerExtraGrade * (g - 1)
    def weaponEffect(g: Int): String = s"стихия Огня, +${2 * g}% к стихийному урону. -20% по броне, +10% по HP; 30% шанс поджечь."
    def armorEffect(g: Int): String  = s"+${ArmorMaxHpPctPerGrade * g}% к макс. HP и +${flatHp(g)} к макс. HP."
  }

  // ── Бриллиант (стихия воздуха; броневая грань — энергия) ─────────────────────
  case object Diamond extends GemKind("Бриллиант") {
    val ArmorEnergyPctPerGrade: Long = 1L
    def weaponEffect(g: Int): String = s"стихия Воздуха, +${2 * g}% к стихийному урону. -10% урона, +5% уклонения/точности; 30% шанс усилить их."
    def armorEffect(g: Int): String  = s"+${ArmorEnergyPctPerGrade * g}% к Энергии."
  }

  // ── Топаз (стихия молнии; броневая грань — шанс экипировки) ───────────────────
  case object Topaz extends GemKind("Топаз") {
    val ArmorGearChancePctPerGrade: Long = 1L
    def weaponEffect(g: Int): String = s"стихия Молнии, +${2 * g}% к стихийному урону. -20% по броне, -10% по HP; 20% урона игнорирует броню противника."
    def armorEffect(g: Int): String  = s"+${ArmorGearChancePctPerGrade * g}% к шансу получения экипировки."
  }

  implicit val encoder: Encoder[GemKind] = (k: GemKind) => k.entryName.asJson
  implicit val decoder: Decoder[GemKind] = (c: HCursor) => c.as[String].map(GemKind.withName)
}

/** Камень-усилитель конкретного грейда (1..5). Лежит в инвентаре как предмет
 *  ([[ItemType.Gem]] + [[ItemDetails.Gem]]) и вставляется в гнездо снаряжения. */
case class Gem(kind: GemKind, grade: Int) {

  /** Имя с учётом грейда: «Надколотый череп», «Череп», «Идеальный череп». */
  def displayName: String = Gem.gradeName(kind, grade)

  /** Сколько пыли останется, если камень расколоть или сломать из гнезда: по
    * [[Gem.DustPerGrade]] горсти на грейд — надколотый даёт 2, безупречный 8. */
  def dustYield: Int = Gem.DustPerGrade * grade.max(Gem.MinGrade).min(Gem.MaxGrade)

  /** Вид пыли, в которую крошится этот камень. */
  def dust: MaterialKind = MaterialKind.dustOf(kind)

  /** Строка эффекта для гнезда в оружии/снаряжении. */
  def weaponEffectText: String = kind.weaponEffectText(grade)
  def armorEffectText: String  = kind.armorEffectText(grade)

  /** Что даёт камень в гнезде оружия / прочего снаряжения — без приставки. */
  def weaponEffect: String = kind.weaponEffect(grade)
  def armorEffect: String  = kind.armorEffect(grade)
}

object Gem {

  val MinGrade: Int = 1
  val MaxGrade: Int = 5

  /** Горстей пыли за каждый грейд расколотого или сломанного камня. */
  val DustPerGrade: Int = 2

  /** Приставки грейдов (индекс = grade-1). Грейд 3 — базовое имя без приставки. */
  private val gradePrefix: Vector[String] =
    Vector("Надколотый", "Поврежденный", "", "Безупречный", "Идеальный")

  /** Имя камня грейда: приставка + строчное базовое имя, кроме грейда 3 (базовое). */
  def gradeName(kind: GemKind, grade: Int): String = {
    val g = grade.max(MinGrade).min(MaxGrade)
    val prefix = gradePrefix(g - 1)
    if (prefix.isEmpty) kind.baseName else s"$prefix ${kind.baseName.toLowerCase}"
  }

  /** Редкость в сумке для сортировки (миниапп): грейд 1 — белая, …, 5 — оранжевая;
    * ниже — только пыль ([[MaterialKind.inventoryRarity]]). */
  def inventoryRarity(grade: Int): Rarity = {
    val g = grade.max(MinGrade).min(MaxGrade)
    Vector(Rarity.White, Rarity.Green, Rarity.Blue, Rarity.Purple, Rarity.Orange)(g - 1)
  }

  implicit val encoder: Encoder[Gem] = deriveEncoder[Gem]
  implicit val decoder: Decoder[Gem] = deriveDecoder[Gem]
}
