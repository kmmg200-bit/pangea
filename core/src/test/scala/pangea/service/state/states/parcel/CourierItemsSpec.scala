package pangea.service.state.states.parcel

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.{Hero, HeroId}
import pangea.model.home.{Furniture, HomeData}
import pangea.model.item.{Item, ItemType, MaterialKind, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.chat.ChatCommand
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.{PlayerLock, State, StateHandler}
import pangea.test._
import zio.ZIO
import zio.test._

/** Вещи с личным посыльным: вне города «Передать …» берёт вещь только из
  * хранилища Торгового дома, сумку не трогает и экрана выбора не открывает —
  * несёт лишь то, что понятно без выбора. В городе всё как раньше: только из
  * сумки. */
object CourierItemsSpec extends ZIOSpecDefault {

  private val userId     = UserId(1L)
  private val sender     = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val friendUs   = UserId(2L)
  private val friendHero = HeroId(2L)
  private val friend     = User(friendUs, VkId("vk_friend"), TelegramId("tg_friend"))

  private def mithril(id: Long): Item = MaterialGenerator.item(MaterialKind.Mithril).copy(id = id)

  private def sword(id: Long, attack: Int): Item =
    Item(id, "Острый меч", lvl = 3L, Rarity.Blue, ItemType.Weapon,
      attack = attack, accuracy = 2, energy = 0, armor = 0, defence = 0, evasion = 0)

  private case class Fix(handler: StateHandler, dao: TestHeroDao, bag: TestInventoryRepository,
                         mine: TestBankRepository, theirs: TestBankRepository, players: TestPlayers, api: TestApi)

  /** Отправитель там, где `state`; хранилище у него своё, у получателя — своё:
    * по ним видно, откуда ушло и куда легло. */
  private def chat(state: StateType = StateType.Dungeon, courier: Boolean = true, stored: Boolean = false,
                   bag: List[Item] = Nil, vault: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = state))
      _        <- heroDao.insertHero(TestFixtures.hero(friendUs).copy(id = friendHero))
      // Личный посыльный — обстановка домика; спрятанный в кладовую не работает.
      _        <- ZIO.when(courier)(heroDao.writeHomeData(userId,
                    (if (stored) HomeData(tier = 1, stored = List(Furniture.Courier.key))
                     else HomeData(tier = 1, furniture = List(Furniture.Courier.key))).asJson))
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
      transfers = Transfers(inv, userRepo, parcels, players, content, heroDao, mine, payouts, lock)
      handler   = new StateHandler(api, userRepo, new DaoBackedTestHeroRepository(heroDao), heroDao, payouts,
                    parcels, transfers, Map.empty[StateType, State], AdminPanel.disabled, content, lock)
    } yield Fix(handler, heroDao, inv, mine, theirs, players, api)

  private def say(f: Fix, text: String) =
    f.handler.transferFromChat(sender.vkId, friend.vkId, ChatCommand.transferQuery(text).getOrElse(""), -1L)

  private def sent(f: Fix) = f.api.sentMessages.map(_.map(_._1).mkString("\n"))

  private def heroOf(dao: TestHeroDao): zio.Task[Hero] = dao.getHeroByUserId(userId).someOrFail(new Throwable("no hero"))

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
      } yield assertTrue(text.contains("Посыльный носит вещи только из хранилища Торгового дома, а там нет ничего похожего на «мифрил»")) &&
              assertTrue(f.bag.snapshot.map(_.id) == List(21L) && f.theirs.itemsSnapshot.isEmpty)
    },

    test("без посыльного вещи из лабиринта не передать — и подсказка про посыльного") {
      for {
        f    <- chat(courier = false, vault = List(mithril(11L)))
        _    <- say(f, "Передать мифрил")
        text <- sent(f)
      } yield assertTrue(text.contains("только в городе")) &&
              assertTrue(text.contains("вещи из хранилища Торгового дома передаст откуда угодно личный посыльный")) &&
              assertTrue(f.mine.itemsSnapshot.size == 1)
    },

    test("посыльный, спрятанный в кладовую, вещей не носит") {
      for {
        f    <- chat(stored = true, vault = List(mithril(11L)))
        _    <- say(f, "Передать мифрил")
        text <- sent(f)
      } yield assertTrue(text.contains("только в городе") && f.mine.itemsSnapshot.size == 1)
    },

    test("в городе посыльный ничего не меняет: вещи — только из сумки, хранилище не трогается") {
      for {
        f    <- chat(state = StateType.TradeHouse, bag = List(mithril(21L)), vault = List(mithril(11L)))
        _    <- say(f, "Передать мифрил")
        text <- sent(f)
        g    <- chat(state = StateType.TradeHouse, vault = List(mithril(11L)))
        _    <- say(g, "Передать мифрил")
        none <- sent(g)
      } yield assertTrue(f.bag.snapshot.isEmpty && f.mine.itemsSnapshot.map(_.id) == List(11L)) &&
              assertTrue(f.theirs.itemsSnapshot.map(_.id) == List(21L) && !text.contains("Посыльный")) &&
              assertTrue(none.contains("В сумке нет ничего похожего на «мифрил»") && g.mine.itemsSnapshot.size == 1)
    },

    test("перечень подходящего: стопки со счётом, не больше пяти, остальное — «и ещё N»") {
      val items = List(mithril(1L), mithril(2L)) ++ (3L to 8L).toList.map(i => sword(i, attack = i.toInt))
      val line  = Transfers.namesOf(items)
      assertTrue(line.startsWith(s"${mithril(1L).displayTitle} (2 шт), ${sword(3L, attack = 3).displayTitle}")) &&
        assertTrue(line.endsWith(" и ещё 2"))
    }
  )
}
