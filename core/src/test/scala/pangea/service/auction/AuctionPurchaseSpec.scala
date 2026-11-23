package pangea.service.auction

import pangea.engine.SceneContent
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.HeroId
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.payout.Payouts
import pangea.test._
import zio.ZIO
import zio.test._

/** Покупка лота — общая у бота и Mini App: вещь в сумку, деньги продавцу, а
  * при отказе ничего не списано и лот на месте. */
object AuctionPurchaseSpec extends ZIOSpecDefault {

  private val userId     = UserId(1L)
  private val buyerUser  = User(userId, VkId("vk_buyer"), TelegramId("tg_buyer"))
  private val seller     = HeroId(2L)
  private val sellerUs   = UserId(2L)
  private val sellerUser = User(sellerUs, VkId("vk_seller"), TelegramId("tg_seller"))

  private def gear(id: Long, name: String): Item =
    Item(id, name, lvl = 3L, Rarity.Blue, ItemType.Weapon,
      attack = 7, accuracy = 2, energy = 0, armor = 0, defence = 0, evasion = 0)

  private def lotOf(id: Long, price: Long, currency: AuctionCurrency = AuctionCurrency.Silver,
                    owner: HeroId = seller): AuctionLot =
    AuctionLot(id, owner, gear(50L + id, s"Топор $id"), price, currency, 0L, AuctionLot.LifetimeMs)

  private case class Fix(purchase: AuctionPurchase, heroDao: TestHeroDao, inv: TestInventoryRepository,
                         lots: TestAuctionRepository, bank: TestBankRepository, payouts: TestPayoutDao,
                         players: TestPlayers)

  private def setup(lots: List[AuctionLot], silver: Long = 0L, doubloons: Long = 0L, vaultSilver: Long = 0L,
                    bag: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = silver, doubloons = doubloons))
      _        <- heroDao.insertHero(TestFixtures.hero(sellerUs).copy(id = seller, silver = 0L, doubloons = 0L))
      inv       = TestInventoryRepository.withItems(bag)
      // Ячейка одна на всех в тестовом банке; продавцу без своей выручка ждёт в городе.
      bank      = TestBankRepository.of(cells = if (vaultSilver > 0L) 1 else 0, silver = vaultSilver)
      aucRepo   = new TestAuctionRepository(lots, inv, vault = Some(bank))
      payoutDao = TestPayoutDao.empty
      users    <- TestUserRepository.withUsers(buyerUser, sellerUser)
      players   = new TestPlayers
      content  <- ZIO.attempt(SceneContent.load())
      purchase  = new AuctionPurchase(heroDao, inv, aucRepo, users, bank,
                    Payouts(payoutDao, heroDao, content), players, content)
    } yield Fix(purchase, heroDao, inv, aucRepo, bank, payoutDao, players)

  private def buyer(f: Fix) = f.heroDao.getHeroByUserId(userId).someOrFailException

  override def spec = suite("Покупка на аукционе")(

    test("за серебро: вещь в сумку, нехватку добирает ячейка, продавцу — колокольчик") {
      for {
        f      <- setup(List(lotOf(1L, price = 300L)), silver = 100L, vaultSilver = 250L)
        hero   <- buyer(f)
        result <- f.purchase.buy(buyerUser, hero, 1L)
        after  <- buyer(f)
      } yield assertTrue(
        result == AuctionPurchase.Result.Bought(lotOf(1L, price = 300L)),
        f.inv.snapshot.map(_.name) == List("Топор 1"),
        after.silver == 0L,
        // Тестовая ячейка одна на всех: сняли 200 у покупателя, легли 300 продавцу.
        f.bank.silverSnapshot == 350L,
        f.lots.snapshot.isEmpty,
        f.players.sentLetters.map(_._1) == List(sellerUs)
      )
    },

    test("за дублоны: с рук, выручка продавца ждёт в городе") {
      for {
        f      <- setup(List(lotOf(1L, price = 80L, currency = AuctionCurrency.Doubloons)), doubloons = 100L)
        hero   <- buyer(f)
        result <- f.purchase.buy(buyerUser, hero, 1L)
        after  <- buyer(f)
      } yield assertTrue(
        result.isInstanceOf[AuctionPurchase.Result.Bought],
        after.doubloons == 20L,
        f.payouts.snapshot.get(seller).contains((0L, 80L))
      )
    },

    test("вне города: серебро только из ячейки — на руках хватает, но без ячейки не купит") {
      for {
        f      <- setup(List(lotOf(1L, price = 300L)), silver = 500L, vaultSilver = 250L)
        hero   <- buyer(f)
        result <- f.purchase.buy(buyerUser, hero, 1L, vaultSilverOnly = true)
        after  <- buyer(f)
      } yield assertTrue(
        result == AuctionPurchase.Result.NoMoney(lotOf(1L, price = 300L)),
        after.silver == 500L && f.bank.silverSnapshot == 250L && f.lots.snapshot.size == 1
      )
    },

    test("вне города: серебро только из ячейки — покупка списывает ячейку, руки не трогает") {
      for {
        f      <- setup(List(lotOf(1L, price = 300L)), silver = 500L, vaultSilver = 300L)
        hero   <- buyer(f)
        result <- f.purchase.buy(buyerUser, hero, 1L, vaultSilverOnly = true, deliverToVault = true)
        after  <- buyer(f)
      } yield assertTrue(
        result == AuctionPurchase.Result.Bought(lotOf(1L, price = 300L)),
        after.silver == 500L,
        f.bank.silverSnapshot == 300L,
        f.inv.snapshot.isEmpty,
        f.bank.itemsSnapshot.map(_.name) == List("Топор 1")
      )
    },

    test("вне города: купленное — в хранилище, даже если сумка забита") {
      val full = (1L to 20L).toList.map(i => gear(100L + i, s"Хлам $i"))
      for {
        f      <- setup(List(lotOf(1L, price = 300L)), silver = 500L, vaultSilver = 300L, bag = full)
        hero   <- buyer(f)
        result <- f.purchase.buy(buyerUser, hero, 1L, vaultSilverOnly = true, deliverToVault = true)
      } yield assertTrue(
        result == AuctionPurchase.Result.Bought(lotOf(1L, price = 300L)),
        f.inv.snapshot.size == 20,
        f.bank.itemsSnapshot.map(_.name) == List("Топор 1")
      )
    },

    test("отказы: свой лот, не хватает денег, нет места, лота нет — ничего не списано, лот на месте") {
      val full = (1L to 20L).toList.map(i => gear(100L + i, s"Хлам $i"))
      for {
        own     <- setup(List(lotOf(1L, price = 300L, owner = HeroId(1L))), silver = 1000L)
        ownRes  <- buyer(own).flatMap(own.purchase.buy(buyerUser, _, 1L))
        poor    <- setup(List(lotOf(1L, price = 300L)), silver = 100L, vaultSilver = 150L)
        poorRes <- buyer(poor).flatMap(poor.purchase.buy(buyerUser, _, 1L))
        poorH   <- buyer(poor)
        cramped <- setup(List(lotOf(1L, price = 300L)), silver = 1000L, bag = full)
        noRoom  <- buyer(cramped).flatMap(cramped.purchase.buy(buyerUser, _, 1L))
        roomH   <- buyer(cramped)
        gone    <- setup(Nil, silver = 1000L)
        goneRes <- buyer(gone).flatMap(gone.purchase.buy(buyerUser, _, 7L))
      } yield assertTrue(
        ownRes == AuctionPurchase.Result.Own && own.lots.snapshot.size == 1,
        poorRes == AuctionPurchase.Result.NoMoney(lotOf(1L, price = 300L)),
        poorH.silver == 100L && poor.bank.silverSnapshot == 150L && poor.lots.snapshot.size == 1,
        noRoom == AuctionPurchase.Result.NoRoom && roomH.silver == 1000L && cramped.lots.snapshot.size == 1,
        goneRes == AuctionPurchase.Result.Gone
      )
    }
  )
}
