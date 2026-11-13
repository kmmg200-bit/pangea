package pangea.model.village

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/** Что герой пустил в дело у входа в гнездо сетеплётов (см.
  * `VillageLairState`), — как на пороге пещеры. Живёт в `scene_data`, пока он
  * стоит у входа: своими ключами (`nest…`) и с меткой `nest` — там лежат и чужие
  * структуры, и чужое за наше читаться не должно. Бой её съедает: добыча пишет
  * туда свою отметку. Уйти от оврага — подготовка пропадает вместе со сценой.
  *
  * @param weakened пауки одурманены: атака и энергия срезаны (дымная фляга,
  *                 сонный дурман)
  * @param poisoned пауки встретят героя уже отравленными (глефовый гриб)
  * @param burning  пауки начинают бой горящими — пламя
  *                 [[VillageRates.NestBurnPct]]% (огненная фляга, рубиновая пыль)
  * @param lured    паучий цвет увёл выводок: в гнезде только матка и
  *                 [[VillageRates.LuredRedSpiders]] красных
  * @param page     страница скарба на экране «Использовать предмет»
  */
final case class NestPrep(
  weakened: Boolean = false,
  poisoned: Boolean = false,
  burning:  Boolean = false,
  lured:    Boolean = false,
  page:     Int     = 0
)

object NestPrep {

  val empty: NestPrep = NestPrep()

  implicit val encoder: Encoder[NestPrep] = (p: NestPrep) => Json.obj(
    "nest"         -> true.asJson,
    "nestWeakened" -> p.weakened.asJson,
    "nestPoisoned" -> p.poisoned.asJson,
    "nestBurning"  -> p.burning.asJson,
    "nestLured"    -> p.lured.asJson,
    "nestPage"     -> p.page.asJson)

  /** Без метки `nest` — не наша запись: это добыча, отметка победы или чужая сцена. */
  implicit val decoder: Decoder[NestPrep] = (c: HCursor) =>
    for {
      _        <- c.get[Boolean]("nest")
      weakened <- c.getOrElse[Boolean]("nestWeakened")(false)
      poisoned <- c.getOrElse[Boolean]("nestPoisoned")(false)
      burning  <- c.getOrElse[Boolean]("nestBurning")(false)
      lured    <- c.getOrElse[Boolean]("nestLured")(false)
      page     <- c.getOrElse[Int]("nestPage")(0)
    } yield NestPrep(weakened, poisoned, burning, lured, page)
}
