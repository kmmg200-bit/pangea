package pangea.dao.inventory

import doobie.ConnectionIO
import doobie.postgres.circe.json.implicits._
import doobie.implicits._
import doobie.util.transactor.Transactor
import io.circe.syntax.EncoderOps
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import zio.Task
import zio.interop.catz._
import pangea.model.inventory.Inventory.{meta, Items}
import pangea.model.item.Item

class InventoryDaoLive(xa: Transactor[Task]) extends InventoryDao {

  override def create(heroId: HeroId): Task[Unit] =
    sql"insert into inventories(hero_id, max_items, items) values($heroId, ${Inventory.DefaultCapacity}, ${Items(List.empty[Item])})".update.run
      .transact(xa)
      .unit

  override def get(heroId: HeroId): Task[Inventory] =
    sql"select * from inventories where hero_id = $heroId"
      .query[Inventory]
      .unique
      .transact(xa)

  override def update(inventory: Inventory): Task[Unit] =
    InventoryDaoLive.updateIo(inventory).transact(xa).unit
}

/** Сумка внутри чужой транзакции: вещь переходит между сумкой и лотом
  * аукциона целиком или не переходит вовсе (см. [[pangea.dao.auction.AuctionDaoLive]]). */
object InventoryDaoLive {
  /** Сумка с замком на строку до конца транзакции. */
  def lockedIo(heroId: HeroId): ConnectionIO[Inventory] =
    sql"select * from inventories where hero_id = $heroId for update"
      .query[Inventory]
      .unique

  def updateIo(inventory: Inventory): ConnectionIO[Int] =
    sql"update inventories set max_items = ${inventory.maxItems}, items = ${inventory.items.asJson} where id = ${inventory.id}".update.run
}
