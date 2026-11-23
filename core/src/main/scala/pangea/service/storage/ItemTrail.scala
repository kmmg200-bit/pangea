package pangea.service.storage

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.Journal
import pangea.model.GameEvent
import pangea.model.item.Item
import pangea.model.user.UserId
import zio.{UIO, ZIO}

/** След вещей в журнале событий (`event_log`): куда вещь делась там, где
  * других следов не остаётся. Откуда вещи берутся, журнал знает по добыче
  * (`loot_claimed`); а здесь пишется:
  *
  *  - перенос и «Выбросить» в Mini App — в боте те же действия видны по
  *    нажатым кнопкам во входящих (`vk_inbox`, неделю), а запросы приложения
  *    не видны нигде;
  *  - пояс, из бота и из приложения: положенная на него вещь теряет свой
  *    номер, а снятая собирается заново с новым (см. `BeltContents`), и без
  *    записи цепочку номеров не восстановить.
  *
  * У каждой вещи — номер, название и редкость: у фиолетовой и пурпурной
  * значок один, 🟣, по названию их не различить. `via` — откуда действие
  * ([[ItemTrail.Bot]] или [[ItemTrail.App]]). Журнала нет (тесты) — ничего не
  * пишется. Записать не вышло — действие игрока уже сделано и не страдает:
  * только предупреждение в лог. */
final class ItemTrail(journal: Option[Journal], via: String) {

  /** Перенесено из хранилища `from` в `to` (имена хранилищ — как у
    * [[ItemStore.Place]]). */
  def moved(user: UserId, from: String, to: String, items: List[Item]): UIO[Unit] =
    write(user, GameEvent.ItemMoved, items, "from" -> from.asJson, "to" -> to.asJson, "items" -> ItemTrail.list(items))

  /** Выброшено из хранилища `from`. */
  def dropped(user: UserId, from: String, items: List[Item]): UIO[Unit] =
    write(user, GameEvent.ItemDropped, items, "from" -> from.asJson, "items" -> ItemTrail.list(items))

  /** `put` легло на пояс (их номеров больше нет), `off` — что сошло с пояса
    * вместо них, уже с новыми номерами. */
  def beltPut(user: UserId, put: List[Item], off: List[Item]): UIO[Unit] =
    write(user, GameEvent.BeltPut, put, "put" -> ItemTrail.list(put), "off" -> ItemTrail.list(off))

  /** Снято с пояса в сумку — с новыми номерами. */
  def beltTaken(user: UserId, items: List[Item]): UIO[Unit] =
    write(user, GameEvent.BeltTaken, items, "items" -> ItemTrail.list(items))

  private def write(user: UserId, kind: String, items: List[Item], fields: (String, Json)*): UIO[Unit] =
    ZIO.foreachDiscard(journal.filter(_ => items.nonEmpty)) { j =>
      j.append(GameEvent(user, kind, Json.obj(("via" -> via.asJson) +: fields: _*)))
        .catchAllCause(cause => ZIO.logWarningCause(s"След вещей ($kind) игрока ${user.value} не записан", cause))
    }
}

object ItemTrail {

  /** Действие пришло из бота. */
  val Bot: String = "bot"

  /** Действие пришло из Mini App. */
  val App: String = "app"

  /** Без журнала — ничего не пишет (тесты, где след не проверяют). */
  val silent: ItemTrail = new ItemTrail(None, Bot)

  /** Вещь в записи: номер, название, редкость. */
  def entry(item: Item): Json =
    Json.obj("id" -> item.id.asJson, "name" -> item.name.asJson, "rarity" -> item.rarity.entryName.asJson)

  def list(items: List[Item]): Json = Json.arr(items.map(entry): _*)
}
