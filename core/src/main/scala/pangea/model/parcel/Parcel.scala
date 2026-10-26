package pangea.model.parcel

import pangea.model.hero.HeroId
import pangea.model.item.Item

/** Одна переданная вещь в пути. `fromName` — имя отправителя на момент
 *  отправки: получателю важно, кто передал, а не чей это был id. `mailedAt` —
 *  когда вещь легла на почту, потому что ячейка её не приняла; `None` — она
 *  ещё в пути (см. [[pangea.service.parcel.Parcels]]). */
final case class Parcel(id: Long, heroId: HeroId, fromName: String, item: Item, sentAt: Long,
                        mailedAt: Option[Long] = None)
