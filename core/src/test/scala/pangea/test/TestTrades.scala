package pangea.test

import pangea.engine.SceneContent
import pangea.model.hero.{Hero, HeroId}
import pangea.model.inventory.Inventory
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.service.Background
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.PlayerLock
import pangea.service.trade.Trades
import zio.{IO, Task, ZIO, durationInt}

/** Двое для обмена: Алиса и Борис, у каждого своя сумка. */
object TestTrades {

  val alice: User = User(UserId(1L), VkId("1"), TelegramId("t1"))
  val bob: User   = User(UserId(2L), VkId("2"), TelegramId("t2"))
  val aHero: HeroId = HeroId(1L)
  val bHero: HeroId = HeroId(2L)

  def item(id: Long, name: String, kind: ItemType = ItemType.Weapon): Item =
    Item(id, name, 3L, Rarity.Blue, kind, attack = 7, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  val sword: Item  = item(11L, "Меч")
  val axe: Item    = item(12L, "Топор")
  val ring: Item   = item(13L, "Кольцо", ItemType.Ring)
  val fang: Item   = item(14L, "Клык волка", ItemType.Trophy)
  val shield: Item = item(21L, "Щит", ItemType.Shield)

  /** Сумка у каждого героя своя. */
  final class Bags(start: Map[HeroId, List[Item]]) extends InventoryRepository {
    private var bags = start
    def of(hero: HeroId): List[Item] = bags.getOrElse(hero, Nil)
    def get(heroId: HeroId): IO[InventoryRepoError, Inventory] =
      ZIO.succeed(Inventory(heroId.value, heroId, 20L, Inventory.Items(of(heroId))))
    def addItem(heroId: HeroId, item: Item): IO[InventoryRepoError, Unit] =
      if (of(heroId).size >= 20) ZIO.fail(InventoryRepoError.NoMorePlaceForItems)
      else ZIO.succeed { bags += heroId -> (of(heroId) :+ item) }
    def removeItem(itemId: Long, heroId: HeroId): IO[InventoryRepoError, Unit] =
      ZIO.succeed { bags += heroId -> of(heroId).filterNot(_.id == itemId) }
    def removeItems(itemIds: Set[Long], heroId: HeroId): IO[InventoryRepoError, Unit] =
      ZIO.succeed { bags += heroId -> of(heroId).filterNot(i => itemIds.contains(i.id)) }
    def updateItem(heroId: HeroId, item: Item): IO[InventoryRepoError, Unit]                    = ZIO.unit
    def swap(heroId: HeroId, remove: List[Item], add: List[Item]): IO[InventoryRepoError, Unit] = ZIO.unit
    def refillFlasks(heroId: HeroId): IO[InventoryRepoError, Unit]                              = ZIO.unit
    def increaseCapacity(heroId: HeroId, delta: Long): IO[InventoryRepoError, Unit]             = ZIO.unit
  }

  final case class Fx(trades: Trades, dao: TestTradeDao, heroes: TestHeroDao, users: TestUserRepository,
                      transfers: Transfers, bags: Bags, parcels: TestParcelDao, payouts: TestPayoutDao,
                      players: TestPlayers, content: SceneContent) {
    def hero(user: User): Task[Hero] = heroes.getHeroByUserId(user.userId).someOrFailException
    def sentTo(hero: HeroId): List[String] = parcels.snapshot.filter(_.heroId == hero).map(_.item.name)
    def letters(user: User): List[String] = players.sentLetters.collect { case (u, t) if u == user.userId => t }
  }

  /** Оба в Торговом доме. У Алисы меч, топор, кольцо и клык, 1000 серебра и 200
    * дублонов; у Бориса щит, 500 и 100. Ячеек нет: серебро ждёт в городе, вещи —
    * на почте. */
  def fx(aliceState: StateType = StateType.TradeHouse): Task[Fx] =
    for {
      heroes   <- TestHeroDao.withHero(alice.userId, TestFixtures.hero(alice.userId, state = aliceState)
                    .copy(id = aHero, name = Some("Алиса"), silver = 1000L, doubloons = 200L))
      _        <- heroes.insertHero(TestFixtures.hero(bob.userId, state = StateType.TradeHouse)
                    .copy(id = bHero, name = Some("Борис"), silver = 500L, doubloons = 100L))
      users    <- TestUserRepository.withUsers(alice, bob)
      content  <- ZIO.attempt(SceneContent.load())
      bags      = new Bags(Map(aHero -> List(sword, axe, ring, fang), bHero -> List(shield)))
      parcelDao = TestParcelDao.empty
      payoutDao = TestPayoutDao.empty
      players   = new TestPlayers
      bank      = TestBankRepository.empty
      parcels   = Parcels(parcelDao, bank, content)
      payouts   = Payouts(payoutDao, heroes, content)
      lock     <- PlayerLock.make
      bg       <- Background.make(1.second)
      transfers = Transfers(bags, users, parcels, players, content, heroes, bank, payouts, lock)
      dao       = new TestTradeDao
    } yield Fx(new Trades(dao, transfers, parcels, bank, payouts, heroes, users, players, content, lock, bg),
               dao, heroes, users, transfers, bags, parcelDao, payoutDao, players, content)
}
