package pangea.model.battle

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/** Повозка обоза на стороне героя (караван деревни). Стоит «под» строем: место
  * у неё своё, но занять его можно — и тогда удары по этому месту принимает
  * тот, кто на нём стоит (так решил пользователь). Своих действий у повозки нет;
  * может гореть — пламя тикает в конце раунда, а кто стоит на ней в это
  * время, тот его сбивает.
  *
  * @param position место в ряду героя
  * @param burn     пламя на ней; после боя его тушат караванщики (см. [[cooled]])
  */
final case class Wagon(position: Int, hp: Long, armor: Long, maxHp: Long, maxArmor: Long, burn: Option[Burn] = None) {
  def alive: Boolean = hp > 0L

  def hpPct: Long    = if (maxHp <= 0L) 0L else hp * 100L / maxHp
  def armorPct: Long = if (maxArmor <= 0L) 0L else armor * 100L / maxArmor

  /** Удар по повозке: сперва броня, остаток — в HP. Увернуться ей нечем. */
  def struck(damage: Long): Wagon = {
    val toArmor = damage.max(0L).min(armor)
    copy(armor = armor - toArmor, hp = (hp - (damage.max(0L) - toArmor)).max(0L))
  }

  /** Загорелась: свежее пламя — [[Convoy.BurnStart]]% HP за раунд (дерево
    * горит сильнее живого), по горящей — разгорается, как всякое пламя. */
  def ignited: Wagon = copy(burn = Some(burn.fold(Burn(Convoy.BurnStart))(_.reignited)))

  /** Пламя потушено. */
  def cooled: Wagon = copy(burn = None)
}

object Wagon {
  implicit val encoder: Encoder[Wagon] = (w: Wagon) => Json.obj(
    "position" -> w.position.asJson,
    "hp"       -> w.hp.asJson,
    "armor"    -> w.armor.asJson,
    "maxHp"    -> w.maxHp.asJson,
    "maxArmor" -> w.maxArmor.asJson,
    "burn"     -> w.burn.asJson)

  implicit val decoder: Decoder[Wagon] = (c: HCursor) =>
    for {
      position <- c.get[Int]("position")
      hp       <- c.get[Long]("hp")
      armor    <- c.getOrElse[Long]("armor")(0L)
      maxHp    <- c.getOrElse[Long]("maxHp")(hp)
      maxArmor <- c.getOrElse[Long]("maxArmor")(armor)
      burn     <- c.getOrElse[Option[Burn]]("burn")(None)
    } yield Wagon(position, hp, armor, maxHp, maxArmor, burn)
}

/** Обоз в бою (засада на караван деревни): его повозки и как с ними обходится
  * засада — бьёт ли она повозки вообще и с каким шансом удар поджигает.
  * Лежит в `GroupState.convoy`; в прочих боях обоза нет. */
final case class Convoy(wagons: List[Wagon], hitsWagons: Boolean, ignitePct: Long) {

  /** Целая повозка на месте `pos`. */
  def wagonAt(pos: Int): Option[Wagon] = wagons.find(w => w.alive && w.position == pos)

  /** Сколько повозок ещё цело. */
  def intact: Int = wagons.count(_.alive)

  def updated(w: Wagon): Convoy = copy(wagons = wagons.map(x => if (x.position == w.position) w else x))
}

object Convoy {

  /** Пламя на повозке начинается с 10% её HP за раунд и разгорается дальше,
    * как всякое пламя (так решил пользователь). */
  val BurnStart: Int = 10

  /** Под этим ключом в `eventData` добычи бой возвращает повозки после боя —
    * туда, откуда начался (караван держит их под тем же ключом). Пламя к тому
    * времени потушено. */
  val ReturnKey: String = "convoyWagons"

  /** Повозки после боя — в данные того, кто вёл к бою (`eventData` добычи). */
  def returned(eventData: Json, convoy: Convoy): Json =
    eventData.deepMerge(Json.obj(ReturnKey -> convoy.wagons.map(_.cooled).asJson))

  implicit val encoder: Encoder[Convoy] = (c: Convoy) => Json.obj(
    "wagons"     -> c.wagons.asJson,
    "hitsWagons" -> c.hitsWagons.asJson,
    "ignitePct"  -> c.ignitePct.asJson)

  implicit val decoder: Decoder[Convoy] = (c: HCursor) =>
    for {
      wagons <- c.getOrElse[List[Wagon]]("wagons")(Nil)
      hits   <- c.getOrElse[Boolean]("hitsWagons")(true)
      ignite <- c.getOrElse[Long]("ignitePct")(0L)
    } yield Convoy(wagons, hits, ignite)
}
