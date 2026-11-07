package pangea.dao.artifact

import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import doobie.util.transactor
import doobie.util.transactor.Transactor
import io.circe.syntax.EncoderOps
import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.{meta, Items}
import pangea.model.item.Item
import zio.interop.catz._
import zio.{Task, ZLayer}

trait ArtifactDao {
  /** Гарантирует запись артефактов героя и возвращает её. */
  def getOrCreate(heroId: HeroId): Task[HeroArtifacts]
  /** Артефакты героя, если запись уже заводили, — без записи (для Mini App). */
  def find(heroId: HeroId): Task[Option[HeroArtifacts]]
  def update(artifacts: HeroArtifacts): Task[Unit]
}

class ArtifactDaoLive(xa: Transactor[Task]) extends ArtifactDao {

  override def getOrCreate(heroId: HeroId): Task[HeroArtifacts] =
    (for {
      _ <- sql"""insert into hero_artifacts(hero_id, casket_items, bag_items, wardrobe_items)
                 values($heroId, ${Items(List.empty[Item])}, ${Items(List.empty[Item])}, ${Items(List.empty[Item])})
                 on conflict (hero_id) do nothing""".update.run
      row <- select(heroId).unique
    } yield toArtifacts(row)).transact(xa)

  override def find(heroId: HeroId): Task[Option[HeroArtifacts]] =
    select(heroId).option.map(_.map(toArtifacts)).transact(xa)

  private type Row = (HeroId, Int, Int, Items, Int, Int, Items, Int, Items)

  private def select(heroId: HeroId) =
    sql"""select hero_id, casket_tier, casket_charges, casket_items,
                 bag_tier, bag_charges, bag_items,
                 wardrobe_tier, wardrobe_items
          from hero_artifacts where hero_id = $heroId""".query[Row]

  private def toArtifacts(row: Row): HeroArtifacts = {
    val (id, cTier, cCharges, cItems, bTier, bCharges, bItems, wTier, wItems) = row
    HeroArtifacts(id,
      Artifact(ArtifactKind.Casket, cTier, cCharges, cItems),
      Artifact(ArtifactKind.LivingBag, bTier, bCharges, bItems),
      // У шкафа магии нет, поэтому и зарядов он не держит.
      Artifact(ArtifactKind.Wardrobe, wTier, 0, wItems))
  }

  override def update(a: HeroArtifacts): Task[Unit] =
    sql"""update hero_artifacts set
            casket_tier = ${a.casket.tier}, casket_charges = ${a.casket.charges}, casket_items = ${a.casket.items.asJson},
            bag_tier = ${a.bag.tier}, bag_charges = ${a.bag.charges}, bag_items = ${a.bag.items.asJson},
            wardrobe_tier = ${a.wardrobe.tier}, wardrobe_items = ${a.wardrobe.items.asJson}
          where hero_id = ${a.heroId}"""
      .update.run.transact(xa).unit
}

object ArtifactDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, ArtifactDao] =
    ZLayer.fromFunction(new ArtifactDaoLive(_))
}
