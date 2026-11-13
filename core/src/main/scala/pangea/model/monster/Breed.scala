package pangea.model.monster

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Особая порода моба: у неё свои умения в бою и своя добыча — сетеплёты из
  * гнезда у деревни (см. Villages). У обычных мобов породы нет. Имя — у самой
  * породы, по нему моба и зовут в бою. `entryName` лежит в бою
  * (`active_battle`): не переименовывать и не удалять. */
sealed abstract class Breed(val displayName: String) extends EnumEntry

object Breed extends Enum[Breed] with DoobieEnum[Breed] {
  val values: IndexedSeq[Breed] = findValues

  /** Молодой сетеплёт: удар, дошедший до HP, травит. */
  case object YoungSpider extends Breed("Молодой сетеплёт")

  /** Взрослый сетеплёт: опутывает паутиной. */
  case object AdultSpider extends Breed("Взрослый сетеплёт")

  /** Матка сетеплётов: рождает новорождённых. */
  case object SpiderMother extends Breed("Матка сетеплётов")

  /** Красный сетеплёт: редкий и опасный, с него — его железа. */
  case object RedSpider extends Breed("Красный сетеплёт")

  /** Новорождённый сетеплёт: его рождает матка в бою; ни опыта, ни добычи. */
  case object NewbornSpider extends Breed("Новорождённый сетеплёт")

  /** Сетеплёты — все пауки гнезда. */
  val spiders: Set[Breed] = Set(YoungSpider, AdultSpider, SpiderMother, RedSpider, NewbornSpider)

  /** Сетеплёт ли это — по породе моба (у обычных мобов её нет). */
  def isSpider(breed: Option[Breed]): Boolean = breed.exists(spiders.contains)

  implicit val encoder: Encoder[Breed] = (b: Breed) => b.entryName.asJson

  /** Порода из поля боя — мягко: чужое имя читается как «породы нет», и бой
    * из-за него не теряется. */
  def read(c: HCursor, field: String): Decoder.Result[Option[Breed]] =
    c.getOrElse[Option[String]](field)(None).map(_.flatMap(withNameOption))
}
