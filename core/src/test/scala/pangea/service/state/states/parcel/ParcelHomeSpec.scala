package pangea.service.state.states.parcel

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.bank.BankVault
import pangea.model.hero.{Hero, HeroId}
import pangea.model.home.HomeData
import pangea.model.item.{Item, ItemType, MaterialKind, Rarity}
import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.parcel.{Parcels, TransferTarget, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.PlayerLock
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Переданное игроку, что берёт его дом, ложится сразу туда: руны — на алтарь,
  * травы и отвары — на верстак травника, камни — в хранилище на острове, пока
  * есть место. Остальное — как раньше: в банковскую ячейку, а не влезло — на
  * почту. В письме получателю — где что. */
object ParcelHomeSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val sender   = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val friendUs = UserId(2L)
  private val friend   = HeroId(2L)
  private val receiver = User(friendUs, VkId("vk_friend"), TelegramId("tg_friend"))
  private val target   = TransferTarget(friend, friendUs, "Пётр")

  private val shelf = " Заберите в течение 7 дней, иначе всё, что лежит на почте, сгорит."

  private def herb(id: Long): Item = MaterialGenerator.item(MaterialKind.Chamomile).copy(id = id)
  private def rune(id: Long): Item = RuneStone.item(Rune.Active(Skill.CunningStrike), RuneStoneSize.Big).copy(id = id)
  private def stone(id: Long): Item =
    Item(id, "Камень", 3L, Rarity.Blue, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  private val herbs     = List(herb(10L), herb(11L), herb(12L))
  private val title     = herb(10L).displayTitle
  private val gave      = s"📦 Иван Иванов передал(а) вам: $title ×3."
  private val cottage   = HomeData(tier = 1, metJunona = true)
  private val townhouse = HomeData(tier = 2, metJunona = true)
  /** Добротный дом, где на полках трав свободно одно место. */
  private val oneShelf  = townhouse.copy(herbs = List.tabulate(townhouse.herbShelves - 1)(i => herb(1000L + i)))

  /** Отправитель — игрок 1, получатель — игрок 2 со своим героем и домом.
    * Ячейка в тестах одна на всех: по ней видно, что легло получателю. */
  private final case class Post(transfers: Transfers, parcels: Parcels, dao: TestParcelDao, heroDao: TestHeroDao,
                                players: TestPlayers, hero: Hero, theirs: Hero) {
    def give(items: List[Item]): Task[Unit] =
      transfers.send(sender, hero, target, items, 0L).flatMap(_.handOver)
    def letters: List[String] = players.sentLetters.collect { case (`friendUs`, text) => text }
    def home: Task[HomeData] =
      heroDao.readHomeData(friendUs).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  }

  private def post(home: Option[HomeData], bank: TestBankRepository = TestBankRepository.empty,
                   items: List[Item] = herbs): Task[Post] =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = StateType.TradeHouse))
      _        <- heroDao.insertHero(TestFixtures.hero(friendUs).copy(id = friend))
      _        <- ZIO.foreachDiscard(home)(h => heroDao.writeHomeData(friendUs, h.asJson))
      userRepo <- TestUserRepository.withUsers(sender, receiver)
      content  <- ZIO.attempt(SceneContent.load())
      lock     <- PlayerLock.make
      dao       = TestParcelDao.empty
      inv       = TestInventoryRepository.withItems(items)
      players   = new TestPlayers
      payouts   = Payouts(TestPayoutDao.empty, heroDao, content)
      parcels   = Parcels(dao, bank, content, Some(heroDao))
      transfers = Transfers(inv, userRepo, parcels, players, content, heroDao, bank, payouts, lock)
      hero     <- heroDao.getHeroByUserId(userId).map(_.get)
      theirs   <- heroDao.getHeroByUserId(friendUs).map(_.get)
    } yield Post(transfers, parcels, dao, heroDao, players, hero, theirs)

  override def spec = suite("Посылки — сразу в дом получателя")(

    test("травы — сразу на полки верстака травника; ячейка и почта пусты") {
      val bank = TestBankRepository.withCells(1)
      for {
        p    <- post(Some(townhouse), bank)
        _    <- p.give(herbs)
        home <- p.home
      } yield assertTrue(home.herbs.map(_.id) == List(10L, 11L, 12L)) &&
              assertTrue(bank.itemsSnapshot.isEmpty && p.dao.snapshot.isEmpty) &&
              assertTrue(p.letters == List(gave + " Всё уже в вашем доме: на полках верстака травника."))
    },

    test("на полках одно место — одна трава туда, остальные — в ячейку") {
      val bank = TestBankRepository.withCells(1)
      for {
        p    <- post(Some(oneShelf), bank)
        _    <- p.give(herbs)
        home <- p.home
      } yield assertTrue(home.herbs.size == townhouse.herbShelves && home.herbs.lastOption.map(_.id).contains(10L)) &&
              assertTrue(bank.itemsSnapshot.map(_.id) == List(11L, 12L) && p.dao.snapshot.isEmpty) &&
              assertTrue(p.letters == List(gave +
                " В дом легло 1 (на полки верстака травника), остальное (2) — в банковскую ячейку."))
    },

    test("и в ячейке тесно — в письме дом, ячейка и почта, и срок почты") {
      val bank = TestBankRepository.of(1, List.tabulate(BankVault.ItemsPerCell.toInt - 1)(i => stone(100L + i)))
      for {
        p    <- post(Some(oneShelf), bank)
        _    <- p.give(herbs)
        home <- p.home
      } yield assertTrue(home.herbs.size == townhouse.herbShelves) &&
              assertTrue(bank.itemsSnapshot.exists(_.id == 11L) && p.dao.snapshot.map(_.item.id) == List(12L)) &&
              assertTrue(p.letters == List(gave + " В дом легло 1 (на полки верстака травника), в банковскую ячейку — 1, " +
                "остальное (1) ждёт на почте Торгового дома." + shelf))
    },

    test("ячейки нет — что не легло в дом, ждёт на почте") {
      for {
        p    <- post(Some(oneShelf))
        _    <- p.give(herbs)
        home <- p.home
      } yield assertTrue(home.herbs.size == townhouse.herbShelves && p.dao.snapshot.map(_.item.id) == List(11L, 12L)) &&
              assertTrue(p.letters == List(gave +
                " В дом легло 1 (на полки верстака травника), остальное (2) ждёт на почте Торгового дома." + shelf))
    },

    test("руны — на алтарь в любом доме, даже в домике") {
      for {
        p    <- post(Some(cottage), TestBankRepository.withCells(1), items = List(rune(20L)))
        _    <- p.give(List(rune(20L)))
        home <- p.home
      } yield assertTrue(home.runeCount(Rune.Active(Skill.CunningStrike).key) == 5L && p.dao.snapshot.isEmpty) &&
              assertTrue(p.letters == List(s"📦 Иван Иванов передал(а) вам: ${rune(20L).displayTitle} ×1. " +
                "Всё уже в вашем доме: на руническом алтаре."))
    },

    test("в домике верстака нет, а без дома — тем более: травы — в ячейку, как раньше") {
      val inCottage = TestBankRepository.withCells(1)
      val homeless  = TestBankRepository.withCells(1)
      for {
        p1   <- post(Some(cottage), inCottage)
        _    <- p1.give(herbs)
        home <- p1.home
        p2   <- post(None, homeless)
        _    <- p2.give(herbs)
      } yield assertTrue(home.herbs.isEmpty && inCottage.itemsSnapshot.map(_.id) == List(10L, 11L, 12L)) &&
              assertTrue(homeless.itemsSnapshot.map(_.id) == List(10L, 11L, 12L)) &&
              assertTrue(p1.letters == List(gave + " Уже в вашей банковской ячейке.") && p2.letters == p1.letters)
    },

    test("на почте трава, а на полке освободилось место — на ходу героя она ложится туда") {
      val full = townhouse.copy(herbs = List.tabulate(townhouse.herbShelves)(i => herb(1000L + i)))
      for {
        p      <- post(Some(full))
        _      <- p.give(List(herb(10L)))
        mailed  = p.dao.snapshot.map(_.item.id)
        _      <- p.heroDao.writeHomeData(friendUs, full.copy(herbs = full.herbs.drop(1)).asJson)
        r      <- TestRenderer.make
        _      <- p.parcels.deliver(receiver, p.theirs, r)
        home   <- p.home
        said   <- r.sentScreens.map(_.map(_.text))
      } yield assertTrue(mailed == List(10L) && p.dao.snapshot.isEmpty) &&
              assertTrue(home.herbs.size == townhouse.herbShelves && home.herbs.exists(_.id == 10L)) &&
              assertTrue(said == List(s"🏠 Вам передали: $title (Иван Иванов). Всё уже в вашем доме: на полках верстака травника."))
    },

    test("отправителю: травы лягут «в дом или в банковскую ячейку», меч — «в банковскую ячейку», как раньше") {
      for {
        p <- post(Some(townhouse))
      } yield assertTrue(p.transfers.sentText(herbs, "Пётр") ==
                s"$title ×3 отправлено игроку Пётр. Вещь ляжет к нему в дом или в банковскую ячейку.") &&
              assertTrue(p.transfers.sentText(List(stone(1L)), "Пётр") ==
                "[Ур.3] Камень ×1 отправлено игроку Пётр. Вещь ляжет к нему в банковскую ячейку.")
    },

    test("посыльный несёт траву из хранилища — тоже «в дом или в банковскую ячейку»") {
      val bank = TestBankRepository.of(1, List(herb(30L)))
      for {
        p   <- post(Some(townhouse), bank)
        out <- p.transfers.courierSend(sender, p.hero, target, herb(30L).name, 1, 0L)
      } yield assertTrue(out match {
        case Transfers.Outcome.Sent(text, _) =>
          text.startsWith("🏃 Посыльный отнёс") && text.endsWith(" Вещь ляжет к нему в дом или в банковскую ячейку.")
        case _ => false
      })
    },

    test("посылку успели забрать раньше — в доме она не задваивается") {
      // Забрать посылку не выходит: её будто взяли между раскладкой и `claim`.
      val taken = new TestParcelDao() {
        override def claim(id: Long, heroId: HeroId): Task[Boolean] = ZIO.succeed(false)
      }
      for {
        content <- ZIO.attempt(SceneContent.load())
        heroDao <- TestHeroDao.withHero(friendUs, TestFixtures.hero(friendUs).copy(id = friend))
        _       <- heroDao.writeHomeData(friendUs, townhouse.asJson)
        parcels  = Parcels(taken, TestBankRepository.empty, content, Some(heroDao))
        sent    <- parcels.send(friend, "Иван Иванов", herb(10L), 0L)
        landed  <- parcels.arrive(friend, friendUs, List(sent.id), 0L)
        home    <- heroDao.readHomeData(friendUs).map(_.flatMap(_.as[HomeData].toOption))
      } yield assertTrue(landed.homed.isEmpty && home.exists(h => h.herbs.isEmpty && h.tier == 2))
    }
  )
}
