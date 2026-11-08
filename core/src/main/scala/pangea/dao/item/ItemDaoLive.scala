package pangea.dao.item

import doobie.ConnectionIO
import doobie.implicits._
import doobie.util.transactor.Transactor
import pangea.model.item.Item
import zio.Task
import zio.interop.catz._

class ItemDaoLive(xa: Transactor[Task]) extends ItemDao {

  override def insert(heroId: Long, item: Item): Task[Long] =
    ItemDaoLive.insertIo(heroId, item).transact(xa)
}

object ItemDaoLive {
  /** Номер для вещи — внутри чужой транзакции (см. [[pangea.dao.auction.AuctionDaoLive]]). */
  def insertIo(heroId: Long, item: Item): ConnectionIO[Long] =
    sql"""INSERT INTO items (hero_id, name, lvl, rarity, item_type, attack, accuracy, energy, armor, defence, evasion)
          VALUES ($heroId, ${item.name}, ${item.lvl}, ${item.rarity.entryName}, ${item.itemType.entryName},
                  ${item.attack}, ${item.accuracy}, ${item.energy}, ${item.armor}, ${item.defence}, ${item.evasion})
       """.update
      .withUniqueGeneratedKeys[Long]("id")
}
