package pangea.repository.barrel

import pangea.dao.barrel.BarrelDao
import pangea.model.barrel.{Barrel, BarrelThieves, StolenGoods}
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.Item
import zio.{IO, ZIO}

final class BarrelRepositoryLive(dao: BarrelDao) extends BarrelRepository {

  def get(heroId: HeroId): IO[BarrelRepoError, Barrel] =
    dao.getOrCreate(heroId).orElseFail(BarrelRepoError.CantFindBarrel)

  def deposit(heroId: HeroId, item: Item): IO[BarrelRepoError, Unit] =
    for {
      barrel <- get(heroId)
      // Пыль и малые руны места не занимают, но их не больше сотни на вид.
      _      <- ZIO.when(item.weightless && !barrel.hasRoomForHoard(item))(
                  ZIO.fail(BarrelRepoError.DustLimitReached))
      _      <- ZIO.when(!item.weightless && barrel.freeSlots <= 0)(ZIO.fail(BarrelRepoError.BarrelFull))
      _      <- dao.update(barrel.addItem(item)).orElseFail(BarrelRepoError.CantUpdateBarrel)
    } yield ()

  def withdraw(heroId: HeroId, itemId: Long): IO[BarrelRepoError, Item] =
    for {
      barrel <- get(heroId)
      item   <- ZIO.fromOption(barrel.items.data.find(_.id == itemId))
                  .orElseFail(BarrelRepoError.CantFindItemToRemove)
      rest    = barrel.items.withoutOne(itemId)
      _      <- dao.update(barrel.withItems(rest)).orElseFail(BarrelRepoError.CantUpdateBarrel)
    } yield item

  def depositSilver(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit] =
    for {
      _      <- ZIO.when(amount <= 0)(ZIO.fail(BarrelRepoError.NonPositiveAmount))
      barrel <- get(heroId)
      _      <- ZIO.when(amount > barrel.freeSilverSpace)(ZIO.fail(BarrelRepoError.SilverOverflow))
      _      <- dao.update(barrel.copy(silver = barrel.silver + amount)).orElseFail(BarrelRepoError.CantUpdateBarrel)
    } yield ()

  def withdrawSilver(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit] =
    for {
      _      <- ZIO.when(amount <= 0)(ZIO.fail(BarrelRepoError.NonPositiveAmount))
      barrel <- get(heroId)
      _      <- ZIO.when(amount > barrel.silver)(ZIO.fail(BarrelRepoError.NotEnoughSilver))
      _      <- dao.update(barrel.copy(silver = barrel.silver - amount)).orElseFail(BarrelRepoError.CantUpdateBarrel)
    } yield ()

  def markRolled(heroId: HeroId, nowMs: Long): IO[BarrelRepoError, Unit] =
    get(heroId).flatMap(b => dao.update(BarrelThefts.rolled(b, nowMs)).orElseFail(BarrelRepoError.CantUpdateBarrel))

  def steal(heroId: HeroId, clue: Item, nowMs: Long): IO[BarrelRepoError, StolenGoods] =
    for {
      barrel <- get(heroId)
      (robbed, goods) = BarrelThefts.steal(barrel, clue, nowMs)
      _      <- dao.update(robbed).orElseFail(BarrelRepoError.CantUpdateBarrel)
    } yield goods

  def restore(heroId: HeroId, transform: List[Item] => List[Item]): IO[BarrelRepoError, Option[StolenGoods]] =
    for {
      barrel <- get(heroId)
      back    = BarrelThefts.restore(barrel, transform)
      _      <- ZIO.foreachDiscard(back) { case (b, _) => dao.update(b).orElseFail(BarrelRepoError.CantUpdateBarrel) }
    } yield back.map(_._2)
}

/** Кража и возврат как чистые функции над бочкой — их делят прод и тесты. */
object BarrelThefts {

  /** Жребий брошен сейчас. */
  def rolled(barrel: Barrel, nowMs: Long): Barrel =
    barrel.copy(thieves = Some(barrel.thieves.getOrElse(BarrelThieves()).copy(rolledAt = nowMs)))

  /** Всё, кроме сюжетного, — ворам; в бочке остаётся улика. */
  def steal(barrel: Barrel, clue: Item, nowMs: Long): (Barrel, StolenGoods) = {
    val (kept, taken) = barrel.items.data.partition(_.isQuestItem)
    val goods         = StolenGoods(taken, barrel.silver)
    (barrel.copy(items = Items(kept :+ clue), silver = 0L,
      thieves = Some(BarrelThieves(rolledAt = nowMs, stolen = Some(goods)))), goods)
  }

  /** Украденное — обратно, улика — прочь. Не крали — None. */
  def restore(barrel: Barrel, transform: List[Item] => List[Item]): Option[(Barrel, StolenGoods)] =
    barrel.thieves.flatMap(t => t.stolen.map(t -> _)).map { case (t, goods) =>
      val back = goods.copy(items = transform(goods.items))
      (barrel.copy(
        items   = Items(barrel.items.data.filterNot(_.isQuestItem) ++ back.items),
        silver  = (barrel.silver + back.silver).min(Barrel.MaxSilver),
        thieves = Some(t.copy(stolen = None))), back)
    }
}
