package pangea.repository.barrel

import pangea.dao.barrel.BarrelDao
import pangea.model.barrel.{Barrel, StolenGoods}
import pangea.model.hero.HeroId
import pangea.model.item.Item
import zio.{IO, ZLayer}

trait BarrelRepository {
  def get(heroId: HeroId): IO[BarrelRepoError, Barrel]
  def deposit(heroId: HeroId, item: Item): IO[BarrelRepoError, Unit]
  def withdraw(heroId: HeroId, itemId: Long): IO[BarrelRepoError, Item]
  def depositSilver(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit]
  def withdrawSilver(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit]

  /** Жребий на кражу брошен — запомнить когда: чаще раза в сутки его не бросают. */
  def markRolled(heroId: HeroId, nowMs: Long): IO[BarrelRepoError, Unit]

  /** Кража: все вещи (кроме сюжетных) и серебро уходят ворам, в бочке остаётся
    * `clue`. Одной записью — добро не потеряется на полпути. Возвращает, что
    * унесли. */
  def steal(heroId: HeroId, clue: Item, nowMs: Long): IO[BarrelRepoError, StolenGoods]

  /** Украденное — обратно в бочку: вещи после `transform` (воры их
    * перешивают) и серебро; сюжетные вещи из бочки уходят — это улика. Не
    * крали — ничего. Возвращает, что вернулось. */
  def restore(heroId: HeroId, transform: List[Item] => List[Item]): IO[BarrelRepoError, Option[StolenGoods]]
}

object BarrelRepository {
  val live: ZLayer[BarrelDao, Nothing, BarrelRepository] =
    ZLayer.fromFunction(new BarrelRepositoryLive(_))
}
