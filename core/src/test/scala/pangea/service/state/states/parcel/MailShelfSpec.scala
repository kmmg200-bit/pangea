package pangea.service.state.states.parcel

import pangea.engine.SceneContent
import pangea.model.bank.BankVault
import pangea.model.hero.{Hero, HeroId}
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.parcel.{Parcels, TransferTarget, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.{PlayerLock, State, StateHandler, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

import java.time.Instant

/** Посылка ложится сразу при отправке: в банковскую ячейку, а что не влезло —
  * на почту. Почта хранит вещи 7 дней с того, как на неё легла первая, — потом
  * всё, что на ней лежит, сгорает само, и хозяину приходит письмо. О каждой
  * посылке получателю пишут сразу; про срок — только когда он пошёл. */
object MailShelfSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val sender   = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val friendUs = UserId(2L)
  private val friend   = HeroId(2L)
  private val receiver = User(friendUs, VkId("vk_friend"), TelegramId("tg_friend"))
  private val target   = TransferTarget(friend, friendUs, "Пётр")

  private val hour = 60L * 60L * 1000L
  private val day  = 24L * hour

  private val shelf = " Заберите в течение 7 дней, иначе всё, что лежит на почте, сгорит."

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def item(id: Long, name: String): Item =
    Item(id, name, 3L, Rarity.Blue, ItemType.Weapon, attack = 7, accuracy = 2, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  private val sword  = item(10L, "Меч")
  private val shield = item(11L, "Щит")

  private def at(ms: Long) = TestClock.setTime(Instant.ofEpochMilli(ms))

  /** Отправитель — игрок 1, получатель — игрок 2 со своим героем. Ячейка в
    * тестах одна на всех: по ней видно, что легло получателю. */
  private final case class Post(transfers: Transfers, parcels: Parcels, dao: TestParcelDao, players: TestPlayers,
                                renderer: TestRenderer, hero: Hero, theirs: Hero, mail: MailState,
                                handler: StateHandler) {
    /** Отдать и сразу доставить: лок отправителя тест не держит. */
    def give(items: Item*): Task[Unit] =
      transfers.send(sender, hero, target, items.toList, 0L).flatMap(_.handOver)
    def letters: List[String] = players.sentLetters.collect { case (`friendUs`, text) => text }
  }

  private def post(bank: TestBankRepository = TestBankRepository.empty): Task[Post] =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = StateType.TradeHouse))
      _        <- heroDao.insertHero(TestFixtures.hero(friendUs).copy(id = friend))
      userRepo <- TestUserRepository.withUsers(sender, receiver)
      content  <- ZIO.attempt(SceneContent.load())
      renderer <- TestRenderer.make
      api      <- TestApi.make
      lock     <- PlayerLock.make
      dao       = TestParcelDao.empty
      inv       = TestInventoryRepository.withItems(List(sword, shield))
      players   = new TestPlayers
      payouts   = Payouts(TestPayoutDao.empty, heroDao, content)
      parcels   = Parcels(dao, bank, content)
      transfers = Transfers(inv, userRepo, parcels, players, content, heroDao, bank, payouts, lock)
      handler   = new StateHandler(api, userRepo, new DaoBackedTestHeroRepository(heroDao), heroDao, payouts,
                    parcels, transfers, Map.empty[StateType, State], AdminPanel.disabled, content, lock)
      hero     <- heroDao.getHeroByUserId(userId).map(_.get)
      theirs   <- heroDao.getHeroByUserId(friendUs).map(_.get)
    } yield Post(transfers, parcels, dao, players, renderer, hero, theirs,
                 MailState(heroDao, inv, parcels, content), handler)

  /** Ячейка, где свободно одно место. */
  private def oneFreeSlot: TestBankRepository =
    TestBankRepository.of(1, List.tabulate(BankVault.ItemsPerCell.toInt - 1)(i => item(100L + i, "Камень")))

  override def spec = suite("Почта: посылка сразу, срок хранения")(

    test("есть место в ячейке — вещь ложится туда сразу, и получателю письмо") {
      val bank = TestBankRepository.withCells(1)
      for {
        p <- post(bank)
        _ <- p.give(sword)
      } yield assertTrue(bank.itemsSnapshot.map(_.id) == List(10L) && p.dao.snapshot.isEmpty) &&
              assertTrue(p.letters == List("📦 Иван Иванов передал(а) вам: [Ур.3] Меч ×1. Уже в вашей банковской ячейке."))
    },

    test("ячейки нет — вещь сразу на почте, срок пошёл с отправки, в письме — 7 дней") {
      for {
        p <- post()
        _ <- at(5 * hour)
        _ <- p.give(sword)
      } yield assertTrue(p.dao.snapshot.map(_.mailedAt) == List(Some(5 * hour))) &&
              assertTrue(p.letters == List("📦 Иван Иванов передал(а) вам: [Ур.3] Меч ×1. Ждёт на почте Торгового дома." + shelf))
    },

    test("на почте уже что-то лежит — о новой посылке пишут без срока, и срок у неё тот же") {
      for {
        p <- post()
        _ <- p.give(sword)
        _ <- at(2 * day)
        _ <- p.give(shield)
      } yield assertTrue(p.letters.size == 2) &&
              assertTrue(p.letters.last == "📦 Иван Иванов передал(а) вам: [Ур.3] Щит ×1. Ждёт на почте Торгового дома.") &&
              assertTrue(p.dao.snapshot.map(_.mailedAt) == List(Some(0L), Some(0L)))
    },

    test("в ячейку влезло не всё — в письме, сколько где") {
      val bank = oneFreeSlot
      for {
        p <- post(bank)
        _ <- p.give(sword, item(12L, "Меч"))
      } yield assertTrue(bank.itemsSnapshot.exists(_.id == 10L) && p.dao.snapshot.map(_.item.id) == List(12L)) &&
              assertTrue(p.letters == List("📦 Иван Иванов передал(а) вам: [Ур.3] Меч ×2. В банковскую ячейку легло 1, " +
                "остальное (1) ждёт на почте Торгового дома." + shelf))
    },

    test("через 7 дней почта сгорает сама, без хода хозяина, — и ему письмо") {
      for {
        p      <- post()
        _      <- p.give(sword)
        _      <- at(2 * day)
        _      <- p.give(shield)
        _      <- p.transfers.mailRound(7 * day - 1L)
        before  = (p.dao.snapshot.size, p.letters.size)
        _      <- p.transfers.mailRound(7 * day)
      } yield assertTrue(before == (2, 2)) &&
              // щит пролежал пять дней, но срок у почты общий — сгорел вместе с мечом
              assertTrue(p.dao.snapshot.isEmpty) &&
              assertTrue(p.letters.last ==
                "🔥 Срок хранения на почте вышел — всё, что там лежало, сгорело: [Ур.3] Меч (Иван Иванов), [Ур.3] Щит (Иван Иванов).")
    },

    test("посылка в пути не сгорает вместе с почтой — фон раскладывает её со свежим сроком") {
      for {
        p <- post()
        _ <- p.give(sword)
        _ <- p.parcels.send(friend, "Иван", shield, 6 * day)   // не разложилась сразу
        _ <- p.transfers.mailRound(7 * day)
      } yield assertTrue(p.dao.snapshot.map(x => (x.item.id, x.mailedAt)) == List((11L, Some(7 * day)))) &&
              // одним письмом: что сгорело и что пришло взамен
              assertTrue(p.letters.last ==
                "🔥 Срок хранения на почте вышел — всё, что там лежало, сгорело: [Ур.3] Меч (Иван Иванов).\n\n" +
                "📮 На почту Торгового дома пришло: [Ур.3] Щит (Иван)." + shelf)
    },

    test("не разложилось сразу — через минуту фон разложит само, без хода получателя, и напишет") {
      val bank = TestBankRepository.withCells(1)
      for {
        p     <- post(bank)
        _     <- p.parcels.send(friend, "Иван", sword, 0L)
        _     <- p.transfers.mailRound(30L * 1000L)                    // ещё рано: вдруг доставка в пути
        early  = (p.dao.snapshot.size, p.letters.size)
        _     <- p.transfers.mailRound(2L * 60L * 1000L)
      } yield assertTrue(early == (1, 0)) &&
              assertTrue(bank.itemsSnapshot.map(_.id) == List(10L) && p.dao.snapshot.isEmpty) &&
              assertTrue(p.letters == List("📦 Вам передали: [Ур.3] Меч (Иван). Всё убрано в банковскую ячейку."))
    },

    test("срок вышел, а фон ещё не успел — новая посылка не наследует его: старое сгорает, срок свежий") {
      for {
        p <- post()
        _ <- p.give(sword)
        _ <- at(7 * day + hour)
        _ <- p.give(shield)
      } yield assertTrue(p.dao.snapshot.map(x => (x.item.id, x.mailedAt)) == List((11L, Some(7 * day + hour)))) &&
              assertTrue(p.letters.last ==
                "🔥 Срок хранения на почте вышел — всё, что там лежало, сгорело: [Ур.3] Меч (Иван Иванов).\n\n" +
                "📦 Иван Иванов передал(а) вам: [Ур.3] Щит ×1. Ждёт на почте Торгового дома." + shelf)
    },

    test("забрал с почты всё — срок сброшен, и о следующей посылке снова пишут про 7 дней") {
      for {
        p <- post()
        _ <- p.give(sword)
        _ <- at(1 * day)
        _ <- p.mail.action(receiver, tap("MailAll"), p.renderer)
        _ <- at(3 * day)
        _ <- p.give(shield)
      } yield assertTrue(p.letters.last.endsWith(shelf)) &&
              assertTrue(p.dao.snapshot.map(_.mailedAt) == List(Some(3 * day)))
    },

    test("что получатель успел разложить своим ходом, доставка не трогает и второй раз не пишет") {
      for {
        p        <- post()
        shipment <- p.transfers.send(sender, p.hero, target, List(sword), 0L)
        _        <- p.parcels.deliver(receiver, p.theirs, p.renderer)
        screens  <- p.renderer.sentScreens
        _        <- shipment.handOver
      } yield assertTrue(screens.map(_.text).exists(_.contains("На почту Торгового дома пришло: [Ур.3] Меч"))) &&
              assertTrue(p.letters.isEmpty && p.dao.snapshot.map(_.mailedAt) == List(Some(0L)))
    },

    test("не разложилось сразу — разложится на ходу получателя; о второй посылке — без срока") {
      for {
        p      <- post()
        _      <- p.parcels.send(friend, "Иван", sword, 0L)
        _      <- p.parcels.deliver(receiver, p.theirs, p.renderer)
        first  <- p.renderer.sentScreens.map(_.map(_.text))
        _      <- p.renderer.reset
        _      <- p.parcels.send(friend, "Иван", shield, 0L)
        _      <- p.parcels.deliver(receiver, p.theirs, p.renderer)
        second <- p.renderer.sentScreens.map(_.map(_.text))
      } yield assertTrue(first == List("📮 На почту Торгового дома пришло: [Ур.3] Меч (Иван)." + shelf)) &&
              assertTrue(second == List("📮 На почту Торгового дома пришло: [Ур.3] Щит (Иван)."))
    },

    test("«Передать» из беседы — вещь у получателя сразу, письмо тоже") {
      val bank = TestBankRepository.withCells(1)
      for {
        p <- post(bank)
        _ <- p.handler.transferFromChat(sender.vkId, receiver.vkId, "меч", -1L)
      } yield assertTrue(bank.itemsSnapshot.map(_.id) == List(10L) && p.dao.snapshot.isEmpty) &&
              assertTrue(p.letters == List("📦 Иван Иванов передал(а) вам: [Ур.3] Меч ×1. Уже в вашей банковской ячейке."))
    },

    test("на почте видно, сколько осталось до сгорания") {
      for {
        p      <- post()
        _      <- p.give(sword)
        _      <- at(1 * hour)
        _      <- p.mail.enter(receiver, p.renderer)
        screen <- p.renderer.sentScreens.map(_.last)
      } yield assertTrue(screen.text.contains("⏳ Через 6д 23ч всё, что лежит на почте, сгорит."))
    },

    test("остаток срока: дни с часами, под конец — часы с минутами") {
      assertTrue(Parcels.left(Parcels.ShelfMs) == "7д 0ч") &&
      assertTrue(Parcels.left(25 * hour) == "1д 1ч") &&
      assertTrue(Parcels.left(5 * hour + 20 * 60000L) == "5ч 20мин") &&
      assertTrue(Parcels.left(40 * 60000L) == "40мин") &&
      assertTrue(Parcels.left(0L) == "1мин")
    }
  )
}
