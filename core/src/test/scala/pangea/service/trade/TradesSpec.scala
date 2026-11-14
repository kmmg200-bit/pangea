package pangea.service.trade

import io.circe.parser.decode
import pangea.engine.SceneContent
import pangea.model.hero.{Hero, HeroId}
import pangea.model.inventory.Inventory
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.trade.{TradeSide, TradeStatus}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.service.Background
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.PlayerLock
import pangea.test._
import zio.test._
import zio.{IO, Task, ZIO, durationInt}

/** Обмен предложением: своё — в заклад, ответ — своим, принимает тот, чей ход;
  * отозвать и отклонить — когда угодно, и тогда всё возвращается целиком. */
object TradesSpec extends ZIOSpecDefault {

  private val alice = User(UserId(1L), VkId("1"), TelegramId("t1"))
  private val bob   = User(UserId(2L), VkId("2"), TelegramId("t2"))
  private val aHero = HeroId(1L)
  private val bHero = HeroId(2L)
  private val now   = 1000000L

  private def item(id: Long, name: String, kind: ItemType = ItemType.Weapon): Item =
    Item(id, name, 3L, Rarity.Blue, kind, attack = 7, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private val sword  = item(11L, "Меч")
  private val axe    = item(12L, "Топор")
  private val ring   = item(13L, "Кольцо", ItemType.Ring)
  private val shield = item(21L, "Щит", ItemType.Shield)
  private val fang   = item(14L, "Клык волка", ItemType.Trophy)

  /** Сумка у каждого героя своя. */
  private final class Bags(start: Map[HeroId, List[Item]]) extends InventoryRepository {
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
    def updateItem(heroId: HeroId, item: Item): IO[InventoryRepoError, Unit]                = ZIO.unit
    def swap(heroId: HeroId, remove: List[Item], add: List[Item]): IO[InventoryRepoError, Unit] = ZIO.unit
    def refillFlasks(heroId: HeroId): IO[InventoryRepoError, Unit]                          = ZIO.unit
    def increaseCapacity(heroId: HeroId, delta: Long): IO[InventoryRepoError, Unit]         = ZIO.unit
  }

  private final case class Fx(trades: Trades, dao: TestTradeDao, heroes: TestHeroDao, bags: Bags,
                              parcels: TestParcelDao, payouts: TestPayoutDao, players: TestPlayers, content: SceneContent) {
    def hero(user: User): Task[Hero] = heroes.getHeroByUserId(user.userId).someOrFailException
    def sentTo(hero: HeroId): List[String] = parcels.snapshot.filter(_.heroId == hero).map(_.item.name)
    def letters(user: User): List[String] = players.sentLetters.collect { case (u, t) if u == user.userId => t }
  }

  /** В городе у Алисы меч, топор, кольцо и клык, 1000 серебра и 200 дублонов;
    * у Бориса щит, 500 и 100. Ячеек нет: серебро ждёт в городе, вещи — на почте. */
  private def fx(aliceState: StateType = StateType.TradeHouse) =
    for {
      heroes  <- TestHeroDao.withHero(alice.userId, TestFixtures.hero(alice.userId, state = aliceState)
                   .copy(id = aHero, name = Some("Алиса"), silver = 1000L, doubloons = 200L))
      _       <- heroes.insertHero(TestFixtures.hero(bob.userId, state = StateType.TradeHouse)
                   .copy(id = bHero, name = Some("Борис"), silver = 500L, doubloons = 100L))
      users   <- TestUserRepository.withUsers(alice, bob)
      content <- ZIO.attempt(SceneContent.load())
      bags     = new Bags(Map(aHero -> List(sword, axe, ring, fang), bHero -> List(shield)))
      parcelDao = TestParcelDao.empty
      payoutDao = TestPayoutDao.empty
      players  = new TestPlayers
      bank     = TestBankRepository.empty
      parcels  = Parcels(parcelDao, bank, content)
      payouts  = Payouts(payoutDao, heroes, content)
      lock    <- PlayerLock.make
      bg      <- Background.make(1.second)
      transfers = Transfers(bags, users, parcels, players, content, heroes, bank, payouts, lock)
      dao      = new TestTradeDao
    } yield Fx(new Trades(dao, transfers, parcels, bank, payouts, heroes, users, players, content, lock, bg),
               dao, heroes, bags, parcelDao, payoutDao, players, content)

  private def draft(items: List[Item] = Nil, silver: Long = 0L, doubloons: Long = 0L, note: Option[String] = None) =
    Trades.Draft(items.map(_.id), silver, doubloons, note)

  /** Посылки получателю и письма идут отдельным волокном — ждём по живым часам. */
  private def eventually[A](get: => A)(ok: A => Boolean): Task[A] =
    Live.live((ZIO.sleep(10.millis) *> ZIO.succeed(get)).repeatUntil(ok)
      .timeoutFail(new Throwable("не дождались"))(5.seconds))

  private def offerOf(e: Either[String, pangea.model.trade.TradeOffer]) = e.toOption.getOrElse(throw new Exception(e.toString))

  override def spec = suite("Обмен между игроками")(

    test("предложение: своё — в заклад, серебро с комиссией; ход и письмо — партнёру; срок — трое суток") {
      for {
        f     <- fx()
        a     <- f.hero(alice)
        res   <- f.trades.propose(alice, a, "борис", draft(List(sword), silver = 100L, note = Some("Хочу щит")), now)
        offer  = offerOf(res)
        a2    <- f.hero(alice)
      } yield assertTrue(
        offer.from == alice.userId, offer.to == bob.userId, offer.turn == bob.userId,
        offer.status == TradeStatus.Open, offer.expiresAt == now + Trades.LifetimeMs,
        offer.fromSide.items == List(sword), offer.fromSide.silver == 100L, offer.fromSide.silverFee == 10L,
        offer.fromSide.note.contains("Хочу щит"),
        f.bags.of(aHero) == List(axe, ring, fang), a2.silver == 890L,
        f.letters(bob) == List(f.content.format("trade.offered", "name" -> "Иван Иванов"))
      )
    },

    test("отказы — и ничего не тронуто: не в городе, нет героя, сам с собой, пусто, трофей, чужая вещь, мало денег, длинная записка") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        far    <- fx(StateType.Dungeon).flatMap(g => g.hero(alice).flatMap(h => g.trades.propose(alice, h, "Борис", draft(List(sword)), now)))
        nobody <- f.trades.propose(alice, a, "Некто", draft(List(sword)), now)
        self   <- f.trades.propose(alice, a, "Алиса", draft(List(sword)), now)
        empty  <- f.trades.propose(alice, a, "Борис", draft(), now)
        trophy <- f.trades.propose(alice, a, "Борис", draft(List(fang)), now)
        alien  <- f.trades.propose(alice, a, "Борис", draft(List(shield)), now)
        little <- f.trades.propose(alice, a, "Борис", draft(silver = 10L), now)
        poor   <- f.trades.propose(alice, a, "Борис", draft(silver = 5000L), now)
        long   <- f.trades.propose(alice, a, "Борис", draft(note = Some("а" * (Trades.MaxNote + 1))), now)
        a2     <- f.hero(alice)
        c       = f.content
      } yield assertTrue(
        far == Left(c.text("trade.notInCity")),
        nobody == Left(c.format("trade.noPartner", "name" -> "Некто")),
        self == Left(c.text("trade.self")),
        empty == Left(c.text("trade.empty")),
        trophy == Left(c.format("trade.notTradable", "name" -> "Клык волка")),
        alien == Left(c.text("trade.itemGone")),
        little == Left(c.format("trade.tooLittle", "min" -> "🪙 50")),
        poor == Left(c.format("trade.notEnoughSilver", "need" -> "🪙 5500", "fee" -> "🪙 500", "have" -> "🪙 1000")),
        long == Left(c.format("trade.noteTooLong", "max" -> Trades.MaxNote.toString)),
        f.bags.of(aHero) == List(sword, axe, ring, fang), a2.silver == 1000L, f.dao.snapshot.isEmpty
      )
    },

    test("открытых предложений — не больше пяти") {
      for {
        f     <- fx()
        a     <- f.hero(alice)
        five  <- ZIO.foreach(1 to Trades.MaxOpen)(_ => f.trades.propose(alice, a, "Борис", draft(note = Some("?")), now))
        sixth <- f.trades.propose(alice, a, "Борис", draft(note = Some("?")), now)
      } yield assertTrue(five.forall(_.isRight), sixth == Left(f.content.format("trade.tooMany", "max" -> "5")))
    },

    test("ответ: своё — в заклад, ход и письмо — тому, кто предложил; не в свой ход — ни ответить, ни принять") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword), silver = 100L), now).map(offerOf)
        b      <- f.hero(bob)
        reply  <- f.trades.answer(bob, b, offer.id, draft(List(shield), doubloons = 50L), now + 5L).map(offerOf)
        b2     <- f.hero(bob)
        again  <- f.trades.answer(bob, b2, offer.id, draft(), now + 6L)
        early  <- f.trades.accept(bob, b2, offer.id, now + 6L)
      } yield assertTrue(
        reply.turn == alice.userId, reply.toSide.items == List(shield), reply.toSide.doubloonsFee == 5L,
        reply.expiresAt == now + 5L + Trades.LifetimeMs,
        f.bags.of(bHero).isEmpty, b2.doubloons == 45L,
        f.letters(alice) == List(f.content.format("trade.answered", "name" -> "Иван Иванов")),
        again == Left(f.content.text("trade.notYourTurn")), early == Left(f.content.text("trade.notYourTurn"))
      )
    },

    test("принять: каждому — заклад другого посылкой и деньгами, комиссия сгорела; партнёру — что ему досталось") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword), silver = 100L), now).map(offerOf)
        b      <- f.hero(bob)
        _      <- f.trades.answer(bob, b, offer.id, draft(List(shield), doubloons = 50L), now)
        a2     <- f.hero(alice)
        done   <- f.trades.accept(alice, a2, offer.id, now + 1L).map(offerOf)
        mail   <- eventually(f.letters(bob))(_.size >= 2)
        late   <- f.trades.cancel(bob, offer.id, now + 2L)
        a3     <- f.hero(alice)
        b3     <- f.hero(bob)
      } yield assertTrue(
        done.status == TradeStatus.Accepted,
        f.sentTo(bHero) == List("Меч"), f.sentTo(aHero) == List("Щит"),
        f.payouts.snapshot.get(bHero).contains((100L, 0L)), f.payouts.snapshot.get(aHero).contains((0L, 50L)),
        a3.silver == 890L, b3.doubloons == 45L,
        mail.lastOption.exists(_.startsWith(f.content.format("trade.accepted", "name" -> "Иван Иванов", "got" -> (sword.displayTitle + ", 🪙 100")))),
        late == Left(f.content.text("trade.closed"))
      )
    },

    test("правка своей стороны: убранное — назад в сумку, добавленное — в заклад, деньги — разницей") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword, axe), silver = 100L), now).map(offerOf)
        b      <- f.hero(bob)
        _      <- f.trades.answer(bob, b, offer.id, draft(note = Some("Без топора, но с кольцом")), now)
        a2     <- f.hero(alice)
        edited <- f.trades.answer(alice, a2, offer.id, draft(List(sword, ring), silver = 50L), now).map(offerOf)
        a3     <- f.hero(alice)
      } yield assertTrue(
        edited.fromSide.items == List(sword, ring), edited.fromSide.silver == 50L, edited.fromSide.silverFee == 5L,
        f.bags.of(aHero) == List(fang, axe), a3.silver == 890L,
        f.payouts.snapshot.get(aHero).contains((55L, 0L)), edited.turn == bob.userId
      )
    },

    test("запись опоздала — положенное возвращается, сказано заново открыть") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword)), now).map(offerOf)
        b      <- f.hero(bob)
        _       = f.dao.loseNextUpdate()
        stale  <- f.trades.answer(bob, b, offer.id, draft(List(shield), doubloons = 50L), now)
        b2     <- f.hero(bob)
      } yield assertTrue(
        stale == Left(f.content.text("trade.stale")),
        f.bags.of(bHero) == List(shield), b2.doubloons == 100L,
        f.dao.snapshot.headOption.exists(o => o.toSide.isEmpty && o.turn == bob.userId)
      )
    },

    test("отозвать можно в любой момент — и не в свой ход, и вне города: своё — в сумку, деньги целиком, с комиссией") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword), silver = 100L, doubloons = 50L), now).map(offerOf)
        _      <- f.heroes.updateState(alice.userId, StateType.Dungeon)
        done   <- f.trades.cancel(alice, offer.id, now + 1L).map(offerOf)
        a2     <- f.hero(alice)
      } yield assertTrue(
        done.status == TradeStatus.Cancelled,
        f.bags.of(aHero).toSet == Set(sword, axe, ring, fang),
        f.payouts.snapshot.get(aHero).contains((110L, 0L)), a2.doubloons == 200L,
        f.letters(bob).lastOption.contains(f.content.format("trade.cancelled", "name" -> "Иван Иванов"))
      )
    },

    test("отклонить: заклад предложившего — ему посылкой и деньгами, ему письмо") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword), silver = 100L), now).map(offerOf)
        done   <- f.trades.cancel(bob, offer.id, now + 1L).map(offerOf)
        mail   <- eventually(f.letters(alice))(_.nonEmpty)
      } yield assertTrue(
        done.status == TradeStatus.Declined,
        f.sentTo(aHero) == List("Меч"), f.payouts.snapshot.get(aHero).contains((110L, 0L)),
        mail.headOption.exists(_.startsWith(f.content.format("trade.declined", "name" -> "Иван Иванов")))
      )
    },

    test("срок вышел — закрыто, заклады хозяевам, обоим письмо; свежее не тронуто") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        old    <- f.trades.propose(alice, a, "Борис", draft(List(sword)), now).map(offerOf)
        b      <- f.hero(bob)
        _      <- f.trades.answer(bob, b, old.id, draft(List(shield)), now)
        a2     <- f.hero(alice)
        fresh  <- f.trades.propose(alice, a2, "Борис", draft(List(axe)), now + Trades.LifetimeMs).map(offerOf)
        _      <- f.trades.expire(now + Trades.LifetimeMs + 1L)
        toA    <- eventually(f.letters(alice))(_.nonEmpty)
        toB    <- eventually(f.letters(bob))(_.exists(_.startsWith(f.content.format("trade.expired", "name" -> "Иван Иванов"))))
        all    <- ZIO.succeed(f.dao.snapshot)
      } yield assertTrue(
        all.find(_.id == old.id).exists(_.status == TradeStatus.Expired),
        all.find(_.id == fresh.id).exists(_.status == TradeStatus.Open),
        f.sentTo(aHero) == List("Меч"), f.sentTo(bHero) == List("Щит"),
        toA.exists(_.startsWith(f.content.format("trade.expired", "name" -> "Иван Иванов"))),
        toB.exists(_.startsWith(f.content.format("trade.expired", "name" -> "Иван Иванов")))
      )
    },

    test("сторона из старой записи без новых полей читается: недостающее — нулём и пусто") {
      assertTrue(decode[TradeSide]("""{"items":[],"silver":100}""") == Right(TradeSide(Nil, 100L, 0L, 0L, 0L, None)))
    }
  ) @@ TestAspect.sequential
}
