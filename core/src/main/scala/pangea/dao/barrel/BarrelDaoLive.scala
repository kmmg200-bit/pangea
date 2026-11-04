package pangea.dao.barrel

import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import doobie.util.transactor.Transactor
import io.circe.syntax.EncoderOps
import pangea.model.barrel.Barrel
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.{meta, Items}
import pangea.model.item.Item
import zio.Task
import zio.interop.catz._

class BarrelDaoLive(xa: Transactor[Task]) extends BarrelDao {

  override def getOrCreate(heroId: HeroId): Task[Barrel] =
    (for {
      _ <- sql"insert into barrels(hero_id, items, silver) values($heroId, ${Items(List.empty[Item])}, 0) on conflict (hero_id) do nothing".update.run
      b <- sql"select id, hero_id, items, silver, thieves from barrels where hero_id = $heroId".query[Barrel].unique
    } yield b).transact(xa)

  /** Вещи, серебро и воры — одной записью: кража и возврат переносят добро
    * между ними неразрывно, и половина его не потеряется на полпути. */
  override def update(barrel: Barrel): Task[Unit] =
    sql"update barrels set items = ${barrel.items.asJson}, silver = ${barrel.silver}, thieves = ${barrel.thieves.map(_.asJson)} where id = ${barrel.id}".update.run
      .transact(xa)
      .unit
}
