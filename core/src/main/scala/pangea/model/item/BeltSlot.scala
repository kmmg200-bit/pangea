package pangea.model.item

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/** Что лежит в ячейке пояса. Пояс — не предмет с зарядами, а сумка на поясе:
  * сколько в ней ячеек, решает редкость самого пояса ([[ItemDetails.Belt]]), а
  * в ячейке лежит фляга со своими зарядами, раскрывшаяся роза со своими
  * раскрытиями или стопка ([[BeltSlot.Stack]]): горсти пыли, склянки отвара,
  * склянки святой воды — одного вида, и столько, сколько у пояса ячеек.
  *
  * Внутрь кладутся не целые предметы, а их суть: вид и редкость. По ним вещь
  * собирается заново, когда игрок достаёт её обратно, — у фляг, пыли, отваров
  * и цветков кроме вида с запасом ничего своего и нет. */
sealed trait BeltSlot {

  /** Название для списка расходников в бою. */
  def name: String

  /** Сколько раз этим можно воспользоваться: заряды фляги, горсть пыли — один. */
  def uses: Int

  /** Ячейка после того, как из неё взяли одно: у фляги и розы — заряд, прочее
    * — целиком. Пустая ячейка помнит, чем была полна. */
  def taken: BeltSlot
}

object BeltSlot {

  /** Фляга в ячейке. Редкость хранится, чтобы знать её запас зарядов и собрать
    * вещь обратно такой, какой её положили. */
  final case class Flask(kind: FlaskKind, rarity: Rarity, charges: Int) extends BeltSlot {
    def name: String      = kind.label
    def uses: Int         = charges
    def maxCharges: Int   = FlaskKind.chargesFor(rarity)
    def spent: Flask      = copy(charges = (charges - 1).max(0))
    def refilled: Flask   = copy(charges = maxCharges)
    def empty: Boolean    = charges <= 0
    def taken: BeltSlot   = spent
  }

  /** Ячейка-стопка: горсти одной пыли, склянки одного отвара или святой воды.
    * Сколько их влезает, решает пояс: столько же, сколько у него ячеек (см.
    * [[ItemDetails.Belt.stackFor]]) — у белого одна, у легендарного пять.
    * Берут по одной; опустевшая ячейка помнит, чем была полна, и после боя
    * сама добирает такие же из сумки до полной. Снимают её целиком — со всем,
    * что в ней осталось. */
  sealed trait Stack extends BeltSlot {
    def count: Int
    def withCount(n: Int): Stack
    def uses: Int       = count
    def spent: Boolean  = count <= 0
    def taken: BeltSlot = withCount((count - 1).max(0))

    /** Та же ли это вещь, что и в ячейке `other` (сколько их — не важно). */
    def sameKind(other: BeltSlot): Boolean = other match {
      case s: Stack => s.withCount(0) == withCount(0)
      case _        => false
    }
  }

  /** Горсти пыли. */
  final case class Dust(kind: MaterialKind, count: Int = 1) extends Stack {
    def name: String            = kind.displayName
    def withCount(n: Int): Dust = copy(count = n)
  }

  /** Склянки отвара; стопка в сумке остаётся стопкой. */
  final case class Brew(kind: BrewKind, count: Int = 1) extends Stack {
    def name: String            = kind.label
    def withCount(n: Int): Brew = copy(count = n)
  }

  /** Раскрывшаяся роза в ячейке: цветок ложится целиком, со всеми своими
    * раскрытиями. Осыпавшаяся ячейка тоже помнит цветок — после боя доберёт
    * такой же из сумки, если он там есть. */
  final case class Rose(kind: RoseKind, charges: Int) extends BeltSlot {
    def name: String      = kind.itemName
    def uses: Int         = charges
    def maxCharges: Int   = RoseRates.Charges
    def spent: Rose       = copy(charges = (charges - 1).max(0))
    def refilled: Rose    = copy(charges = maxCharges)
    def withered: Boolean = charges <= 0
    def taken: BeltSlot   = spent
  }

  /** Нальёт ли ячейку ручей: только флягу с водой (см. [[FlaskEffect.refillsAtSpring]]). */
  def refillsAtSpring(slot: BeltSlot): Boolean = slot match {
    case f: Flask => f.kind.effect.refillsAtSpring
    case _        => false
  }

  /** Склянки святой воды. */
  final case class HolyWater(count: Int = 1) extends Stack {
    def name: String                 = MaterialKind.HolyWater.displayName
    def withCount(n: Int): HolyWater = copy(count = n)
  }

  /** Что из сумки вообще можно положить на пояс: фляга, пыль, отвар (кроме
    * того, что в бою ни к чему — см. [[BrewKind.beltable]]) или цветок. */
  def of(item: Item): Option[BeltSlot] = item.details match {
    case f: ItemDetails.Flask =>
      FlaskKind.values.find(_.effect == f.effect).map(kind => Flask(kind, item.rarity, f.charges))
    case ItemDetails.Material(MaterialKind.HolyWater)     => Some(HolyWater())
    case ItemDetails.Material(kind) if kind.gem.isDefined => Some(Dust(kind))
    case ItemDetails.Brew(kind) if kind.beltable          => Some(Brew(kind))
    case r: ItemDetails.Rose                              => Some(Rose(r.kind, r.charges))
    case _                                                => None
  }

  private val flaskEnc: Encoder[Flask] = (f: Flask) =>
    Json.obj("type" -> "Flask".asJson, "kind" -> f.kind.entryName.asJson,
      "rarity" -> f.rarity.asJson, "charges" -> f.charges.asJson)

  private val dustEnc: Encoder[Dust] = (d: Dust) =>
    Json.obj("type" -> "Dust".asJson, "kind" -> d.kind.entryName.asJson,
      "count" -> d.count.asJson)

  private val brewEnc: Encoder[Brew] = (b: Brew) =>
    Json.obj("type" -> "Brew".asJson, "kind" -> b.kind.entryName.asJson,
      "count" -> b.count.asJson)

  private val holyEnc: Encoder[HolyWater] = (h: HolyWater) =>
    Json.obj("type" -> "Holy".asJson, "count" -> h.count.asJson)

  /** Сколько в стопке. Записи до стопок знали одну вещь на ячейку и писали
    * только, потрачена ли она: потрачена — ноль, нет — одна. */
  private def countOf(c: HCursor): Decoder.Result[Int] =
    c.get[Option[Int]]("count").flatMap {
      case Some(n) => Right(n)
      case None    => c.getOrElse[Boolean]("spent")(false).map(spent => if (spent) 0 else 1)
    }

  private val roseEnc: Encoder[Rose] = (r: Rose) =>
    Json.obj("type" -> "Rose".asJson, "kind" -> r.kind.entryName.asJson,
      "charges" -> r.charges.asJson)

  implicit val encoder: Encoder[BeltSlot] = Encoder.instance {
    case f: Flask => flaskEnc(f)
    case d: Dust  => dustEnc(d)
    case b: Brew  => brewEnc(b)
    case r: Rose  => roseEnc(r)
    case h: HolyWater => holyEnc(h)
  }

  implicit val decoder: Decoder[BeltSlot] = (c: HCursor) =>
    c.get[String]("type").flatMap {
      case "Flask" =>
        for {
          kind    <- c.get[String]("kind").map(FlaskKind.withName)
          rarity  <- c.get[Rarity]("rarity")
          charges <- c.getOrElse[Int]("charges")(0)
        } yield Flask(kind, rarity, charges)
      case "Dust" =>
        for {
          kind  <- c.get[String]("kind").map(MaterialKind.withName)
          count <- countOf(c)
        } yield Dust(kind, count)
      case "Brew" =>
        for {
          kind  <- c.get[String]("kind").map(BrewKind.withName)
          count <- countOf(c)
        } yield Brew(kind, count)
      case "Rose" =>
        for {
          kind    <- c.get[String]("kind").map(RoseKind.withName)
          charges <- c.getOrElse[Int]("charges")(0)
        } yield Rose(kind, charges)
      case "Holy" => countOf(c).map(HolyWater(_))
      case other  => Left(io.circe.DecodingFailure(s"Неизвестное содержимое пояса: $other", c.history))
    }
}
