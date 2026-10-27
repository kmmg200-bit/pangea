package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Сюжетные предметы. Лежат в инвентаре как [[ItemType.QuestItem]], места не
  * занимают и никуда, кроме сюжета, не деваются. */
sealed abstract class QuestItemKind(val displayName: String, val description: String) extends EnumEntry

object QuestItemKind extends Enum[QuestItemKind] {
  val values: IndexedSeq[QuestItemKind] = findValues

  /** Письмо с семьдесят пятого убитого моба — начало «Письма Марисе». */
  case object MarisaLetter extends QuestItemKind(
    "Старое письмо Марисе",
    "Старое заляпанное кровью письмо с восковой печатью с символом монеты, адресованное Марисе Кальдере, проживающей в городе Кинэт."
  )

  /** Карта с обратной стороны письма: тайник Кельвина под городом. */
  case object KelvinMap extends QuestItemKind(
    "Карта Кельвина",
    "Карта с обратной стороны письма Кельвина: путь к тайнику недалеко от Кинэта. Отправиться по ней можно только из города."
  )

  /** Карта от старейшины мурлоков: путь к деревне от ворот Кинэта. */
  case object MurlocVillageMap extends QuestItemKind(
    "Деревня Мурлоков",
    "Кусок рыбьей кожи, на котором старейшина нацарапал путь: от ворот Кинэта на закат до сломанного столба, к реке и вниз по течению до трёх камней. Пахнет тиной. Отправиться по карте можно только из города."
  )

  /** Фонарь старшины гномьей дружины «Двенадцать фонарей» — с пятисотого
    * убитого, начало «Пропавшей дружины». */
  case object ForemanLantern extends QuestItemKind(
    "Фонарь старшины",
    "Латунный гномий фонарь, стекло треснуло. На ободе выбито: «Двенадцать фонарей · Двенадцатый». Масло в нём свежее. На внутренней стороне крышки нацарапано: «Нижний горизонт. Семеро живы. Вода прибывает»."
  )

  /** Груз дружины «Двенадцать фонарей», забранный из штольни. */
  case object SquadCrate extends QuestItemKind(
    "Ящик дружины",
    "Тяжёлый гномий ящик, окованный медью: руда и самородки, за которыми «Двенадцать фонарей» спускались под Серебряную жилу. Купит его только тот, кто дружину нанимал, — прораб Гуннар Отвес на Стройке."
  )

  /** Первый урок Густаво о травах: читается, пока не осилишь (см. Knowledge.FlowersRank1). */
  case object FlowerTreatise1 extends QuestItemKind(
    "Трактат о цветах, часть 1",
    "Потрёпанная книжица с гербарием между страниц. Крапива, календула, шалфей — и почерк Густаво на полях."
  )

  /** Второй трактат — для изысканных знатоков (см. Knowledge.FlowersRank2). */
  case object FlowerTreatise2 extends QuestItemKind(
    "Трактат о цветах, часть 2",
    "Тонкая тетрадь в кожаном переплёте. Лилии в пузырях, цветы-миражи и чёрный лотос — с пометкой «не нюхать»."
  )

  /** Ключи магических стоек дома: ключ №N меняет надетое местами с тем, что
    * висит на стойке №N (см. `Racks`). Выдаются вместе со стойкой. */
  case object RackKey1 extends QuestItemKind(
    "Ключ стойки №1",
    "Тяжёлый ключ с биркой «1». Повернёшь — и магия поменяет надетое местами с тем, что висит на стойке №1 у тебя дома. Работает где угодно, кроме боя."
  )

  case object RackKey2 extends QuestItemKind(
    "Ключ стойки №2",
    "Тяжёлый ключ с биркой «2». Повернёшь — и магия поменяет надетое местами с тем, что висит на стойке №2 у тебя дома. Работает где угодно, кроме боя."
  )

  case object RackKey3 extends QuestItemKind(
    "Ключ стойки №3",
    "Тяжёлый ключ с биркой «3». Повернёшь — и магия поменяет надетое местами с тем, что висит на стойке №3 у тебя дома. Работает где угодно, кроме боя."
  )

  case object RackKey4 extends QuestItemKind(
    "Ключ стойки №4",
    "Тяжёлый ключ с биркой «4». Повернёшь — и магия поменяет надетое местами с тем, что висит на стойке №4 у тебя дома. Работает где угодно, кроме боя."
  )

  /** Ключи по номерам стоек: №1 — первый. */
  val rackKeys: List[QuestItemKind] = List(RackKey1, RackKey2, RackKey3, RackKey4)

  /** Номер стойки, если это её ключ. */
  def rackOf(kind: QuestItemKind): Option[Int] = rackKeys.indexOf(kind) match {
    case -1 => None
    case i  => Some(i + 1)
  }

  /** Предмет-шаблон (id = -1, присвоит персист). */
  def item(kind: QuestItemKind): Item =
    Item(
      id       = -1L,
      name     = kind.displayName,
      lvl      = 0L,
      rarity   = Rarity.Orange,
      itemType = ItemType.QuestItem,
      attack   = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details  = ItemDetails.Quest(kind)
    )

  implicit val encoder: Encoder[QuestItemKind] = (k: QuestItemKind) => k.entryName.asJson
  implicit val decoder: Decoder[QuestItemKind] = (c: HCursor) => c.as[String].map(QuestItemKind.withName)
}
