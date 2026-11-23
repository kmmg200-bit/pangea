package pangea.model

import io.circe.Json
import pangea.model.user.UserId

import java.util.UUID

case class GameEvent(
  userId:    UserId,
  eventType: String,
  payload:   Json,
  traceId:   Option[UUID] = None
)

object GameEvent {

  /** Беспроигрышная лотерея Фета: три большие руны за дублоны. По этим записям
    * админ-панель считает, сколько раз в неё сыграли (см. `AdminDao.stats`). */
  val FetRuneLottery: String = "fet_rune_lottery"

  /** Куплен Набор успешного искателя у Фета: он один на игрока, и по этой
    * строке лавка его больше не предлагает (см. FetShopState). */
  val FetStarterPack: String = "fet_starter_pack"

  /** Куплен мешок наудачу у Барти — серебряная лотерея. По этим записям лимит
    * мешков на сутки и учёт в админ-панели (см. BartiLottery, `AdminDao.stats`). */
  val BartiLottery: String = "barti_lottery"

  /** След вещей (см. `ItemTrail`): перенос между хранилищами в Mini App. */
  val ItemMoved: String = "item_moved"

  /** След вещей: «Выбросить» в Mini App. */
  val ItemDropped: String = "item_dropped"

  /** След вещей: вещь легла на пояс — и что сошло с него вместо неё. */
  val BeltPut: String = "belt_put"

  /** След вещей: снято с пояса в сумку. */
  val BeltTaken: String = "belt_taken"
}
