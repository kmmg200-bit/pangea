package pangea.service.auction

import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.HeroId
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test._
import zio.test._

/** Выставление лота — общее у бота и Mini App: вещь из сумки в лот, сбор —
  * десятина в монете лота, объявление — в торговую беседу; отказ ничего не
  * стоит. */
object AuctionSaleSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("vk_seller"), TelegramId("tg_seller"))
  private val heroId = HeroId(1L)

  private def gear(id: Long, name: String = "Шлем"): Item =
    Item(id, name, lvl = 3L, Rarity.Blue, ItemType.Weapon,
      attack = 7, accuracy = 2, energy = 0, armor = 0, defence = 0, evasion = 0)

  private val trophy = Item(9L, "Мешок с пожитками (Орк)", lvl = 10L, Rarity.Gray, ItemType.Trophy,
    attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private def ownLot(id: Long): AuctionLot =
    AuctionLot(id, heroId, gear(50L + id, s"Топор $id"), 100L, AuctionCurrency.Silver, 0L, AuctionLot.LifetimeMs)

  private case class Fix(sale: AuctionSale, heroDao: TestHeroDao, inv: TestInventoryRepository,
                         lots: TestAuctionRepository, bank: TestBankRepository, players: TestPlayers)

  private def setup(bag: List[Item], silver: Long, doubloons: Long = 0L, vaultSilver: Long = 0L,
                    lots: List[AuctionLot] = Nil) =
    for {
      heroDao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = silver, doubloons = doubloons))
      inv      = TestInventoryRepository.withItems(bag)
      aucRepo  = new TestAuctionRepository(lots, inv)
      bank     = TestBankRepository.of(cells = if (vaultSilver > 0L) 1 else 0, silver = vaultSilver)
      players  = new TestPlayers
    } yield Fix(new AuctionSale(heroDao, inv, aucRepo, bank, players), heroDao, inv, aucRepo, bank, players)

  private def seller(f: Fix) = f.heroDao.getHeroByUserId(userId).someOrFailException

  override def spec = suite("Выставление на аукционе")(

    test("за серебро: вещь в лот, сбор с рук, нехватку добирает ячейка; объявление — в торговую беседу") {
      for {
        f      <- setup(List(gear(42L)), silver = 30L, vaultSilver = 100L)
        hero   <- seller(f)
        result <- f.sale.list(user, hero, 42L, AuctionCurrency.Silver, 500L)
        after  <- seller(f)
        lots    = f.lots.snapshot
      } yield assertTrue(
        lots.map(l => (l.item.id, l.price, l.currency, l.sellerId)) == List((42L, 500L, AuctionCurrency.Silver, heroId)),
        result == AuctionSale.Result.Listed(lots.head, 50L),
        f.inv.snapshot.isEmpty,
        after.silver == 0L && f.bank.silverSnapshot == 80L,
        f.players.announcedTrade.exists(m => m.contains("Выставлен лот номер 1") && m.contains("Шлем"))
      )
    },

    test("за дублоны: сбор — дублонами с рук, серебро не трогают") {
      for {
        f      <- setup(List(gear(42L)), silver = 1000L, doubloons = 100L)
        hero   <- seller(f)
        result <- f.sale.list(user, hero, 42L, AuctionCurrency.Doubloons, 300L)
        after  <- seller(f)
      } yield assertTrue(
        result.isInstanceOf[AuctionSale.Result.Listed],
        f.lots.snapshot.map(_.currency) == List(AuctionCurrency.Doubloons),
        after.doubloons == 70L && after.silver == 1000L
      )
    },

    test("отказы: вещи нет, трофей, цена вне границ, десять лотов, нечем заплатить сбор — ничего не списано") {
      val ten = (1L to AuctionLot.MaxLots.toLong).toList.map(ownLot)
      for {
        f      <- setup(List(gear(42L), trophy), silver = 1000L)
        hero   <- seller(f)
        gone   <- f.sale.list(user, hero, 77L, AuctionCurrency.Silver, 500L)
        troph  <- f.sale.list(user, hero, 9L, AuctionCurrency.Silver, 500L)
        low    <- f.sale.list(user, hero, 42L, AuctionCurrency.Silver, AuctionLot.MinPrice - 1L)
        high   <- f.sale.list(user, hero, 42L, AuctionCurrency.Silver, AuctionLot.MaxPrice + 1L)
        // Серебра полно, а сбор за лот в дублонах платят дублонами.
        noFee  <- f.sale.list(user, hero, 42L, AuctionCurrency.Doubloons, 500L)
        after  <- seller(f)
        full   <- setup(List(gear(42L)), silver = 1000L, lots = ten)
        fullH  <- seller(full)
        many   <- full.sale.list(user, fullH, 42L, AuctionCurrency.Silver, 500L)
        fullA  <- seller(full)
      } yield assertTrue(
        gone == AuctionSale.Result.Gone && troph == AuctionSale.Result.Gone,
        low == AuctionSale.Result.PriceTooLow && high == AuctionSale.Result.PriceTooHigh,
        noFee == AuctionSale.Result.NoFee(50L),
        f.lots.snapshot.isEmpty && f.inv.snapshot.map(_.id) == List(42L, 9L) && after.silver == 1000L,
        many == AuctionSale.Result.TooManyLots,
        full.lots.snapshot.size == AuctionLot.MaxLots && full.inv.snapshot.map(_.id) == List(42L) && fullA.silver == 1000L,
        f.players.announcedTrade.isEmpty
      )
    }
  )
}
