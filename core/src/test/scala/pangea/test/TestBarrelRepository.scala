package pangea.test

import pangea.model.barrel.{Barrel, BarrelThieves, StolenGoods}
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.Item
import pangea.repository.barrel.{BarrelRepoError, BarrelRepository, BarrelThefts}
import zio.{IO, ZIO}

class TestBarrelRepository(private var items: List[Item] = Nil, private var silver: Long = 0L,
                           private var thieves: Option[BarrelThieves] = None)
    extends BarrelRepository {

  private val heroId = HeroId(1L)
  private def barrel: Barrel = Barrel(1L, heroId, Items(items), silver, thieves)

  private def save(b: Barrel): Unit = { items = b.items.data; silver = b.silver; thieves = b.thieves }

  def get(heroId: HeroId): IO[BarrelRepoError, Barrel] = ZIO.succeed(barrel)

  // Как в проде: пыль и малые руны места не занимают, но их не больше сотни на вид.
  def deposit(heroId: HeroId, item: Item): IO[BarrelRepoError, Unit] =
    if (item.weightless && !barrel.hasRoomForHoard(item)) ZIO.fail(BarrelRepoError.DustLimitReached)
    else if (!item.weightless && barrel.freeSlots <= 0) ZIO.fail(BarrelRepoError.BarrelFull)
    else ZIO.succeed { items = items :+ item }

  def withdraw(heroId: HeroId, itemId: Long): IO[BarrelRepoError, Item] =
    items.find(_.id == itemId) match {
      case None => ZIO.fail(BarrelRepoError.CantFindItemToRemove)
      case Some(it) =>
        items = items.filterNot(_.id == itemId)
        ZIO.succeed(it)
    }

  def depositSilver(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit] =
    if (amount <= 0)                             ZIO.fail(BarrelRepoError.NonPositiveAmount)
    else if (amount + silver > Barrel.MaxSilver) ZIO.fail(BarrelRepoError.SilverOverflow)
    else                                         ZIO.succeed { silver += amount }

  def withdrawSilver(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit] =
    if (amount <= 0)          ZIO.fail(BarrelRepoError.NonPositiveAmount)
    else if (amount > silver) ZIO.fail(BarrelRepoError.NotEnoughSilver)
    else                      ZIO.succeed { silver -= amount }

  def markRolled(heroId: HeroId, nowMs: Long): IO[BarrelRepoError, Unit] =
    ZIO.succeed(save(BarrelThefts.rolled(barrel, nowMs)))

  def steal(heroId: HeroId, clue: Item, nowMs: Long): IO[BarrelRepoError, StolenGoods] =
    ZIO.succeed { val (b, goods) = BarrelThefts.steal(barrel, clue, nowMs); save(b); goods }

  def restore(heroId: HeroId, transform: List[Item] => List[Item]): IO[BarrelRepoError, Option[StolenGoods]] =
    ZIO.succeed(BarrelThefts.restore(barrel, transform).map { case (b, goods) => save(b); goods })

  def itemsSnapshot: List[Item]              = items
  def silverSnapshot: Long                   = silver
  def thievesSnapshot: Option[BarrelThieves] = thieves
}

object TestBarrelRepository {
  def empty: TestBarrelRepository = new TestBarrelRepository()
  def withItems(items: List[Item]): TestBarrelRepository = new TestBarrelRepository(items = items)
  def withSilver(silver: Long): TestBarrelRepository = new TestBarrelRepository(silver = silver)
  def of(items: List[Item], silver: Long): TestBarrelRepository = new TestBarrelRepository(items, silver)
}
