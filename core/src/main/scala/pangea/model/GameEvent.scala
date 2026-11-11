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
}
