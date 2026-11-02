package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.model.user.UserId
import zio.Task

/** Девушка с картой отца ждёт в таверне: серебра на карту у героя не хватило,
  * и она согласилась подождать. Цена и уровень карты — те, о которых
  * договорились; ждёт она до `until`, потом уходит. Живёт в `heroes.girl_deal`:
  * `scene_data` перезаписало бы первое же событие в лабиринте. */
final case class GirlDeal(price: Long, mapLvl: Long, until: Long) {
  def waiting(nowMs: Long): Boolean = nowMs < until
}

object GirlDeal {

  /** Сколько она ждёт: четыре часа. */
  val WaitMs: Long = 4L * 60L * 60L * 1000L

  implicit val encoder: Encoder[GirlDeal] = (d: GirlDeal) => Json.obj(
    "price"  -> d.price.asJson,
    "mapLvl" -> d.mapLvl.asJson,
    "until"  -> d.until.asJson
  )

  // Рукописный, с запасными значениями: без срока запись читается как уже ушедшая.
  implicit val decoder: Decoder[GirlDeal] = (c: HCursor) =>
    for {
      price  <- c.getOrElse[Long]("price")(0L)
      mapLvl <- c.getOrElse[Long]("mapLvl")(1L)
      until  <- c.getOrElse[Long]("until")(0L)
    } yield GirlDeal(price, mapLvl, until)

  def load(heroDao: HeroDao, userId: UserId): Task[Option[GirlDeal]] =
    heroDao.readGirlDeal(userId).map(_.flatMap(_.as[GirlDeal].toOption))

  /** Ждёт ли она сейчас. */
  def waiting(heroDao: HeroDao, userId: UserId, nowMs: Long): Task[Option[GirlDeal]] =
    load(heroDao, userId).map(_.filter(_.waiting(nowMs)))

  def save(heroDao: HeroDao, userId: UserId, deal: GirlDeal): Task[Unit] =
    heroDao.writeGirlDeal(userId, deal.asJson)

  def clear(heroDao: HeroDao, userId: UserId): Task[Unit] =
    heroDao.writeGirlDeal(userId, Json.Null)
}
