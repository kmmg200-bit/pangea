package pangea.service.state.states.parcel

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.{Hero, HeroId}
import pangea.model.home.{Furniture, HomeData}
import pangea.model.item.{GemKind, Item, ItemType, MaterialKind, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.chat.ChatCommand
import pangea.service.parcel.{Parcels, TransferTarget, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.{PlayerLock, State, StateHandler}
import pangea.test._
import zio.ZIO
import zio.test._

/** Вещи с личным посыльным: вне города «Передать …» берёт вещь из хранилища
  * Торгового дома, а нет там — с полок дома; сумку не трогает и экрана выбора
  * не открывает — несёт лишь то, что понятно без выбора. В городе — сперва
  * сумка, как без посыльного, а нет там — хранилище, потом дом. */
object CourierItemsSpec extends ZIOSpecDefault {

  private val userId     = UserId(1L)
  private val sender     = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val friendUs   = UserId(2L)
  private val friendHero = HeroId(2L)
  private val friend     = User(friendUs, VkId("vk_friend"), TelegramId("tg_friend"))

  private def mithril(id: Long): Item = MaterialGenerator.item(MaterialKind.Mithril).copy(id = id)

  private def herb(id: Long): Item = MaterialGenerator.item(MaterialKind.Chamomile).copy(id = id)

  /** Рубин: 1 — «Надколотый рубин», 3 — просто «Рубин», 4 — «Безупречный рубин». */
  private def gem(id: Long, grade: Int): Item = GemGenerator.item(GemKind.Ruby, grade).copy(id = id)

  private def sword(id: Long, attack: Int): Item =
    Item(id, "Острый меч", lvl = 3L, Rarity.Blue, ItemType.Weapon,
      attack = attack, accuracy = 2, energy = 0, armor = 0, defence = 0, evasion = 0)

  /** Трофей с моба: такое не передаётся. */
  private def trophy(id: Long): Item =
    Item(id, "Клык белого волка", 1L, Rarity.Gray, ItemType.Trophy,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private val target = TransferTarget(friendHero, friendUs, "Пётр")

  private case class Fix(handler: StateHandler, dao: TestHeroDao, bag: TestInventoryRepository,
                         mine: TestBankRepository, theirs: TestBankRepository, players: TestPlayers, api: TestApi,
                         transfers: Transfers)

  /** Отправитель там, где `state`; хранилище у него своё, у получателя — своё:
    * по ним видно, откуда ушло и куда легло. `house` — дом отправителя с тем,
    * что лежит на его полках. */
  private def chat(state: StateType = StateType.Dungeon, courier: Boolean = true, stored: Boolean = false,
                   bag: List[Item] = Nil, vault: List[Item] = Nil, house: HomeData = HomeData(tier = 1),
                   arts: TestArtifactRepository = TestArtifactRepository.empty) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = state))
      _        <- heroDao.insertHero(TestFixtures.hero(friendUs).copy(id = friendHero))
      // Личный посыльный — обстановка домика; спрятанный в кладовую не работает.
      _        <- ZIO.when(courier)(heroDao.writeHomeData(userId,
                    (if (stored) house.copy(stored = List(Furniture.Courier.key))
                     else house.copy(furniture = List(Furniture.Courier.key))).asJson))
      userRepo <- TestUserRepository.withUsers(sender, friend)
      api      <- TestApi.make
      content  <- ZIO.attempt(SceneContent.load())
      lock     <- PlayerLock.make
      mine      = TestBankRepository.of(cells = 1, items = vault)
      theirs    = TestBankRepository.of(cells = 1)
      payouts   = Payouts(TestPayoutDao.empty, heroDao, content)
      players   = new TestPlayers
      inv       = TestInventoryRepository.withItems(bag)
      parcels   = Parcels(TestParcelDao.empty, theirs, content)
      transfers = Transfers(inv, userRepo, parcels, players, content, heroDao, mine, payouts, lock, Some(arts))
      handler   = new StateHandler(api, userRepo, new DaoBackedTestHeroRepository(heroDao), heroDao, payouts,
                    parcels, transfers, Map.empty[StateType, State], AdminPanel.disabled, content, lock)
    } yield Fix(handler, heroDao, inv, mine, theirs, players, api, transfers)

  private def say(f: Fix, text: String) =
    f.handler.transferFromChat(sender.vkId, friend.vkId, ChatCommand.transferQuery(text).getOrElse(""), -1L)

  private def sent(f: Fix) = f.api.sentMessages.map(_.map(_._1).mkString("\n"))

  private def heroOf(dao: TestHeroDao): zio.Task[Hero] = dao.getHeroByUserId(userId).someOrFail(new Throwable("no hero"))

  private def homeOf(dao: TestHeroDao): zio.Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

  override def spec = suite("Посыльный и вещи")(

    test("из лабиринта: вещи уходят из хранилища, сумка не тронута, сцена героя — тоже") {
      val scene = Json.obj("darknessTrackingUntil" -> Json.fromLong(123L))
      for {
        f     <- chat(bag = List(mithril(21L)), vault = List(mithril(11L), mithril(12L), mithril(13L)))
        _     <- f.dao.writeSceneData(userId, scene)
        _     <- say(f, "Передать мифрил 2 шт")
        text  <- sent(f)
        hero  <- heroOf(f.dao)
        after <- f.dao.readSceneData(userId)
      } yield assertTrue(f.mine.itemsSnapshot.map(_.id) == List(13L)) &&
              assertTrue(f.bag.snapshot.map(_.id) == List(21L)) &&
              assertTrue(f.theirs.itemsSnapshot.map(_.id).sorted == List(11L, 12L)) &&
              assertTrue(text.contains(s"🏃 Посыльный отнёс ${mithril(1L).displayTitle} ×2 из вашего хранилища Торгового дома игроку")) &&
              assertTrue(f.players.announcedTrade.exists(_.contains("×2"))) &&
              assertTrue(hero.state == StateType.Dungeon && after.contains(scene))
    },

    test("одна подходящая вещь — уходит сразу, даже экипировка") {
      for {
        f    <- chat(state = StateType.TreasureDig, vault = List(sword(31L, attack = 7), mithril(11L)))
        _    <- say(f, "Передать острый меч")
        text <- sent(f)
      } yield assertTrue(f.mine.itemsSnapshot.map(_.id) == List(11L)) &&
              assertTrue(f.theirs.itemsSnapshot.map(_.id) == List(31L)) &&
              assertTrue(text.contains("🏃 Посыльный отнёс"))
    },

    test("подходит несколько разных — посыльный просит назвать точнее: ничего не ушло, экрана выбора нет") {
      for {
        f    <- chat(vault = List(sword(31L, attack = 7), sword(32L, attack = 9)))
        _    <- say(f, "Передать острый меч")
        text <- sent(f)
        hero <- heroOf(f.dao)
      } yield assertTrue(text.contains("Посыльный не понял, что нести: в хранилище под «острый меч» подходит несколько вещей")) &&
              assertTrue(text.contains("Назовите точнее")) &&
              assertTrue(f.mine.itemsSnapshot.size == 2 && f.theirs.itemsSnapshot.isEmpty) &&
              assertTrue(f.players.announcedTrade.isEmpty) &&
              assertTrue(hero.state == StateType.Dungeon)
    },

    test("в хранилище пусто, а в сумке есть — сумку посыльный не трогает") {
      for {
        f    <- chat(bag = List(mithril(21L)))
        _    <- say(f, "Передать мифрил")
        text <- sent(f)
      } yield assertTrue(text.contains("Посыльный ищет вещь в хранилище Торгового дома, а потом дома")) &&
              assertTrue(text.contains("Ничего похожего на «мифрил» он не нашёл")) &&
              assertTrue(f.bag.snapshot.map(_.id) == List(21L) && f.theirs.itemsSnapshot.isEmpty)
    },

    test("в хранилище нет — несёт с полок дома: травы уходят с верстака травника, сумка и хранилище не тронуты") {
      for {
        f    <- chat(bag = List(herb(21L)), vault = List(mithril(11L)),
                  house = HomeData(tier = 2, herbs = List(herb(41L), herb(42L), herb(43L))))
        _    <- say(f, "Передать ромашка 2 шт")
        text <- sent(f)
        home <- homeOf(f.dao)
      } yield assertTrue(home.herbs.map(_.id) == List(43L)) &&
              assertTrue(f.theirs.itemsSnapshot.map(_.id).sorted == List(41L, 42L)) &&
              assertTrue(f.mine.itemsSnapshot.map(_.id) == List(11L) && f.bag.snapshot.map(_.id) == List(21L)) &&
              assertTrue(text.contains(s"🏃 В хранилище Торгового дома такого не нашлось — посыльный взял ${herb(1L).displayTitle} ×2 с полок верстака травника у вас дома и отнёс игроку")) &&
              assertTrue(text.contains("Вещь ляжет к нему в дом или в банковскую ячейку.")) &&
              assertTrue(f.players.announcedTrade.exists(_.contains("×2")))
    },

    test("есть и в хранилище, и дома — несёт из хранилища, полки не тронуты") {
      for {
        f    <- chat(vault = List(herb(11L)), house = HomeData(tier = 2, herbs = List(herb(41L))))
        _    <- say(f, "Передать ромашка")
        text <- sent(f)
        home <- homeOf(f.dao)
      } yield assertTrue(f.mine.itemsSnapshot.isEmpty && f.theirs.itemsSnapshot.map(_.id) == List(11L)) &&
              assertTrue(home.herbs.map(_.id) == List(41L)) &&
              assertTrue(text.contains("из вашего хранилища Торгового дома"))
    },

    test("названное ровно бьёт похожее: «рубин» — из хранилища камней дома, «надколотый рубин» — из хранилища Торгового дома") {
      val island = HomeData(tier = 3, gems = List(gem(51L, 3)))
      for {
        f     <- chat(vault = List(gem(12L, 1)), house = island)
        _     <- say(f, "Передать рубин")
        text  <- sent(f)
        home  <- homeOf(f.dao)
        g     <- chat(vault = List(gem(12L, 1)), house = island)
        _     <- say(g, "Передать надколотый рубин")
        other <- homeOf(g.dao)
      } yield assertTrue(f.theirs.itemsSnapshot.map(_.id) == List(51L) && home.gems.isEmpty) &&
              assertTrue(f.mine.itemsSnapshot.map(_.id) == List(12L)) &&
              assertTrue(text.contains("из хранилища камней у вас дома")) &&
              assertTrue(g.theirs.itemsSnapshot.map(_.id) == List(12L) && g.mine.itemsSnapshot.isEmpty) &&
              assertTrue(other.gems.map(_.id) == List(51L))
    },

    test("дома подходит несколько разных — посыльный просит назвать точнее: ничего не ушло") {
      for {
        f    <- chat(house = HomeData(tier = 3, gems = List(gem(51L, 1), gem(52L, 4))))
        _    <- say(f, "Передать рубин")
        text <- sent(f)
        home <- homeOf(f.dao)
      } yield assertTrue(text.contains("в хранилище Торгового дома такого нет, а дома под «рубин» подходит несколько вещей")) &&
              assertTrue(text.contains("Назовите точнее")) &&
              assertTrue(home.gems.size == 2 && f.theirs.itemsSnapshot.isEmpty && f.players.announcedTrade.isEmpty)
    },

    test("полки, которой в этом доме нет, посыльный не смотрит") {
      for {
        f    <- chat(house = HomeData(tier = 1, herbs = List(herb(41L))))
        _    <- say(f, "Передать ромашка")
        text <- sent(f)
        home <- homeOf(f.dao)
      } yield assertTrue(text.contains("Ничего похожего на «ромашка» он не нашёл")) &&
              assertTrue(home.herbs.map(_.id) == List(41L) && f.theirs.itemsSnapshot.isEmpty)
    },

    test("без посыльного вещи из лабиринта не передать — и подсказка про посыльного") {
      for {
        f    <- chat(courier = false, vault = List(mithril(11L)))
        _    <- say(f, "Передать мифрил")
        text <- sent(f)
      } yield assertTrue(text.contains("только в городе")) &&
              assertTrue(text.contains("вещи из хранилища Торгового дома и с полок дома передаст откуда угодно личный посыльный")) &&
              assertTrue(f.mine.itemsSnapshot.size == 1)
    },

    test("посыльный, спрятанный в кладовую, вещей не носит") {
      for {
        f    <- chat(stored = true, vault = List(mithril(11L)))
        _    <- say(f, "Передать мифрил")
        text <- sent(f)
      } yield assertTrue(text.contains("только в городе") && f.mine.itemsSnapshot.size == 1)
    },

    test("в городе — сперва сумка: есть там — уходит из сумки, как без посыльного; хранилище не тронуто") {
      for {
        f    <- chat(state = StateType.TradeHouse, bag = List(mithril(21L)), vault = List(mithril(11L)))
        _    <- say(f, "Передать мифрил")
        text <- sent(f)
      } yield assertTrue(f.bag.snapshot.isEmpty && f.mine.itemsSnapshot.map(_.id) == List(11L)) &&
              assertTrue(f.theirs.itemsSnapshot.map(_.id) == List(21L)) &&
              assertTrue(text.contains(s"${mithril(1L).displayTitle} ×1 отправлено игроку") && !text.contains("🏃"))
    },

    test("в городе в сумке нет — посыльный несёт из хранилища Торгового дома") {
      for {
        f    <- chat(state = StateType.TradeHouse, vault = List(mithril(11L), mithril(12L)))
        _    <- say(f, "Передать мифрил 2 шт")
        text <- sent(f)
      } yield assertTrue(f.mine.itemsSnapshot.isEmpty && f.theirs.itemsSnapshot.map(_.id).sorted == List(11L, 12L)) &&
              assertTrue(text.contains(s"🏃 При вас такого не нашлось — посыльный отнёс ${mithril(1L).displayTitle} ×2 из вашего хранилища Торгового дома игроку")) &&
              assertTrue(f.players.announcedTrade.exists(_.contains("×2")))
    },

    test("в городе нет ни в сумке, ни в хранилище — посыльный берёт с полок дома") {
      for {
        f    <- chat(state = StateType.Home, bag = List(mithril(21L)), vault = List(mithril(11L)),
                  house = HomeData(tier = 2, herbs = List(herb(41L), herb(42L))))
        _    <- say(f, "Передать ромашка")
        text <- sent(f)
        home <- homeOf(f.dao)
      } yield assertTrue(home.herbs.map(_.id) == List(42L) && f.theirs.itemsSnapshot.map(_.id) == List(41L)) &&
              assertTrue(f.bag.snapshot.map(_.id) == List(21L) && f.mine.itemsSnapshot.map(_.id) == List(11L)) &&
              assertTrue(text.contains(s"🏃 Ни при вас, ни в хранилище Торгового дома такого не нашлось — посыльный взял ${herb(1L).displayTitle} ×1 с полок верстака травника у вас дома и отнёс игроку")) &&
              assertTrue(text.contains("Вещь ляжет к нему в дом или в банковскую ячейку."))
    },

    test("в городе названное ровно бьёт похожее: «рубин» — из хранилища, хотя в сумке «надколотый рубин»") {
      for {
        f <- chat(state = StateType.TradeHouse, bag = List(gem(21L, 1)), vault = List(gem(11L, 3)))
        _ <- say(f, "Передать рубин")
        g <- chat(state = StateType.TradeHouse, bag = List(gem(21L, 1)), vault = List(gem(11L, 3)))
        _ <- say(g, "Передать надколотый рубин")
      } yield assertTrue(f.theirs.itemsSnapshot.map(_.id) == List(11L) && f.bag.snapshot.map(_.id) == List(21L)) &&
              assertTrue(g.theirs.itemsSnapshot.map(_.id) == List(21L) && g.mine.itemsSnapshot.map(_.id) == List(11L))
    },

    test("в городе в сумке подходит несколько разных — выбор кнопками, как без посыльного; хранилище не тронуто") {
      for {
        f    <- chat(state = StateType.TradeHouse, bag = List(sword(21L, attack = 7), sword(22L, attack = 9)),
                  vault = List(sword(11L, attack = 5)))
        hero <- heroOf(f.dao)
        out  <- f.transfers.quickSend(sender, hero, target, "острый меч", 1, 0L, None, courier = true)
      } yield assertTrue(out == Transfers.Outcome.NeedPick) &&
              assertTrue(f.mine.itemsSnapshot.map(_.id) == List(11L) && f.theirs.itemsSnapshot.isEmpty)
    },

    test("в городе в хранилище подходит несколько разных — посыльный просит назвать точнее") {
      for {
        f    <- chat(state = StateType.TradeHouse, vault = List(sword(31L, attack = 7), sword(32L, attack = 9)))
        _    <- say(f, "Передать острый меч")
        text <- sent(f)
      } yield assertTrue(text.contains("Посыльный не понял, что нести: при вас такого нет, а в хранилище под «острый меч» подходит несколько вещей")) &&
              assertTrue(text.contains("заберите нужную из хранилища и передайте из сумки") && !text.contains("в городе")) &&
              assertTrue(f.mine.itemsSnapshot.size == 2 && f.theirs.itemsSnapshot.isEmpty)
    },

    test("в городе нигде нет — посыльный говорит, где искал; трофей в сумке — прежний отказ") {
      for {
        f    <- chat(state = StateType.TradeHouse, bag = List(mithril(21L)))
        _    <- say(f, "Передать ромашка")
        none <- sent(f)
        g    <- chat(state = StateType.TradeHouse, bag = List(trophy(22L)), vault = List(mithril(11L)))
        _    <- say(g, "Передать клык")
        fang <- sent(g)
      } yield assertTrue(none.contains("При вас нет ничего похожего на «ромашка», и посыльный не нашёл такого ни в хранилище Торгового дома, ни дома")) &&
              assertTrue(f.bag.snapshot.map(_.id) == List(21L) && f.theirs.itemsSnapshot.isEmpty) &&
              assertTrue(fang.contains("Трофеи из рук в руки не ходят") && g.mine.itemsSnapshot.size == 1)
    },

    test("в городе без посыльного — только из сумки, как раньше; безымянное «Передать» хранилище не трогает") {
      for {
        f    <- chat(state = StateType.TradeHouse, courier = false, vault = List(mithril(11L)))
        _    <- say(f, "Передать мифрил")
        no   <- sent(f)
        g    <- chat(state = StateType.TradeHouse, stored = true, vault = List(mithril(11L)))
        _    <- say(g, "Передать мифрил")
        hid  <- sent(g)
        h    <- chat(state = StateType.TradeHouse, vault = List(mithril(11L)))
        _    <- say(h, "Передать")
        bare <- sent(h)
      } yield assertTrue(no.contains("При вас нет ничего похожего на «мифрил»") && f.mine.itemsSnapshot.size == 1) &&
              assertTrue(hid.contains("При вас нет ничего похожего на «мифрил»") && g.mine.itemsSnapshot.size == 1) &&
              assertTrue(!bare.contains("🏃") && h.mine.itemsSnapshot.size == 1 && h.theirs.itemsSnapshot.isEmpty)
    },

    test("в городе ларец — тоже «при вас»: рубин уходит из ларца, хранилище не тронуто, посыльный не нужен") {
      val arts = TestArtifactRepository.of(casket =
        TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1, items = List(gem(21L, 3))))
      for {
        f    <- chat(state = StateType.TradeHouse, vault = List(gem(11L, 3)), arts = arts)
        _    <- say(f, "Передать рубин")
        text <- sent(f)
      } yield assertTrue(f.theirs.itemsSnapshot.map(_.id) == List(21L) && arts.snapshot.casket.items.data.isEmpty) &&
              assertTrue(f.mine.itemsSnapshot.map(_.id) == List(11L)) &&
              assertTrue(text.contains("отправлено игроку") && !text.contains("🏃"))
    },

    test("вне города посыльный не трогает ни сумку, ни ларец, ни шкаф") {
      val arts = TestArtifactRepository.of(wardrobe =
        TestArtifactRepository.artifact(ArtifactKind.Wardrobe, tier = 1, items = List(mithril(31L))))
      for {
        f    <- chat(bag = List(mithril(21L)), arts = arts)
        _    <- say(f, "Передать мифрил")
        text <- sent(f)
      } yield assertTrue(text.contains("Ничего похожего на «мифрил» он не нашёл")) &&
              assertTrue(arts.snapshot.wardrobe.items.data.map(_.id) == List(31L) && f.bag.snapshot.map(_.id) == List(21L)) &&
              assertTrue(f.theirs.itemsSnapshot.isEmpty)
    },

    test("перечень подходящего: стопки со счётом, не больше пяти, остальное — «и ещё N»") {
      val items = List(mithril(1L), mithril(2L)) ++ (3L to 8L).toList.map(i => sword(i, attack = i.toInt))
      val line  = Transfers.namesOf(items)
      assertTrue(line.startsWith(s"${mithril(1L).displayTitle} (2 шт), ${sword(3L, attack = 3).displayTitle}")) &&
        assertTrue(line.endsWith(" и ещё 2"))
    }
  )
}
