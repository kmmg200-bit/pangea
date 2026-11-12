package pangea.service.state.states.parcel

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.home.{Furniture, HomeData}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.chat.ChatCommand
import pangea.service.chat.ChatCommand.Money
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.parcel.Transfers.{FeePct, MinMoney}
import pangea.service.payout.Payouts
import pangea.service.state.{PlayerLock, State, StateHandler}
import pangea.test._
import zio.ZIO
import zio.test._

/** Деньги из рук в руки: «Передать серебро 100» и «Передать дублоны 5» в
  * ответ на сообщение игрока в общей беседе. В сумке их не ищут — серебро
  * ложится получателю в ячейку (или ждёт его в городе), а дублоны, которые не
  * теряются нигде, отдают откуда угодно, и в руки они идут сразу. Меньше 50 не
  * передают, а сверху суммы отправитель платит 10% комиссии — она сгорает. */
object MoneyTransferSpec extends ZIOSpecDefault {

  private val userId     = UserId(1L)
  private val sender     = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val friendUs   = UserId(2L)
  private val friendHero = HeroId(2L)
  private val friend     = User(friendUs, VkId("vk_friend"), TelegramId("tg_friend"))

  /** Беседа целиком: отправитель в Торговом доме, у получателя свой герой.
    * Ячейка в тестах одна на всех — по ней видно, что серебро легло получателю. */
  private def chat(silver: Long = 0L, doubloons: Long = 0L, vaultCells: Int = 1,
                   state: StateType = StateType.TradeHouse, vaultSilver: Long = 0L, courier: Boolean = false) =
    for {
      heroDao  <- TestHeroDao.withHero(userId,
                    TestFixtures.hero(userId, state = state).copy(silver = silver, doubloons = doubloons))
      _        <- heroDao.insertHero(TestFixtures.hero(friendUs).copy(id = friendHero))
      // Личный посыльный — обстановка домика (см. Furniture.Courier).
      _        <- ZIO.when(courier)(heroDao.writeHomeData(userId,
                    HomeData(tier = 1, furniture = List(Furniture.Courier.key)).asJson))
      userRepo <- TestUserRepository.withUsers(sender, friend)
      api      <- TestApi.make
      content  <- ZIO.attempt(SceneContent.load())
      lock     <- PlayerLock.make
      bank      = TestBankRepository.of(cells = vaultCells, silver = vaultSilver)
      payoutDao = TestPayoutDao.empty
      payouts   = Payouts(payoutDao, heroDao, content)
      players   = new TestPlayers
      inv       = TestInventoryRepository.accepting
      parcels   = Parcels(TestParcelDao.empty, bank, content)
      transfers = Transfers(inv, userRepo, parcels, players, content, heroDao, bank, payouts, lock)
      handler   = new StateHandler(api, userRepo, new DaoBackedTestHeroRepository(heroDao), heroDao, payouts,
                    parcels, transfers, Map.empty[StateType, State], AdminPanel.disabled, content, lock)
    } yield (handler, heroDao, bank, payoutDao, players, api)

  /** Что после «Передать …» написали в беседе: текст после первого слова. */
  private def say(handler: StateHandler, text: String, eventId: Long) =
    handler.transferFromChat(sender.vkId, friend.vkId,
      ChatCommand.transferQuery(text).getOrElse(""), eventId)

  private def heroOf(dao: TestHeroDao, id: UserId): zio.Task[Hero] =
    dao.getHeroByUserId(id).map(_.get)

  override def spec = suite("Передача денег")(

    test("разбор: валюта по слову или значку, сумма — числом, «1 000» — тысяча") {
      assertTrue(ChatCommand.moneyQuery("серебро 100").contains(Money.Silver -> Some(100L))) &&
      assertTrue(ChatCommand.moneyQuery("100 серебра").contains(Money.Silver -> Some(100L))) &&
      assertTrue(ChatCommand.moneyQuery("🪙 1 000").contains(Money.Silver -> Some(1000L))) &&
      assertTrue(ChatCommand.moneyQuery("Дублоны 5").contains(Money.Doubloons -> Some(5L))) &&
      assertTrue(ChatCommand.moneyQuery("5 дублонов").contains(Money.Doubloons -> Some(5L))) &&
      // суммы нет — спросим, сколько
      assertTrue(ChatCommand.moneyQuery("серебро").contains(Money.Silver -> None)) &&
      assertTrue(ChatCommand.moneyQuery("дублоны 0").contains(Money.Doubloons -> None)) &&
      // «серебряное кольцо» — вещь, а не деньги; обе валюты разом не разобрать
      assertTrue(ChatCommand.moneyQuery("серебряное кольцо").isEmpty) &&
      assertTrue(ChatCommand.moneyQuery("надколотый череп 2 штуки").isEmpty) &&
      assertTrue(ChatCommand.moneyQuery("серебро и дублоны").isEmpty)
    },

    test("комиссия — десятая часть сверху, с округлением вверх; меньше 50 не передают") {
      assertTrue(MinMoney == 50L && FeePct == 10L) &&
      assertTrue(Transfers.fee(50L) == 5L && Transfers.fee(100L) == 10L) &&
      // не меньше десятой части: с 55 — шесть, а не пять
      assertTrue(Transfers.fee(55L) == 6L && Transfers.fee(51L) == 6L)
    },

    test("строка о передаче денег — в ту беседу, где написали «Передать», а не в торговую") {
      val general = pangea.service.sender.ChatTarget.Peer(2000000001L, pangea.model.vk.Community.Main)
      for {
        t <- chat(silver = 500L, doubloons = 100L)
        (handler, _, _, _, players, _) = t
        _ <- handler.transferFromChat(sender.vkId, friend.vkId,
               ChatCommand.transferQuery("Передать серебро 100").getOrElse(""), -1L, Some(general))
        _ <- handler.transferFromChat(sender.vkId, friend.vkId,
               ChatCommand.transferQuery("Передать дублоны 50").getOrElse(""), -2L, Some(general))
      } yield assertTrue(players.announcedTrade.isEmpty) &&
              assertTrue(players.announcedAt.map(_._1) == List(general, general)) &&
              assertTrue(players.announcedAt.exists(_._2.contains("передал 🪙 100"))) &&
              assertTrue(players.announcedAt.exists(_._2.contains("передал 🟡 50")))
    },

    test("серебро: с отправителя списано, получателю легло в ячейку, беседа знает") {
      for {
        t <- chat(silver = 500L)
        (handler, dao, bank, payoutDao, players, api) = t
        _      <- say(handler, "Передать серебро 100", -1L)
        me     <- heroOf(dao, userId)
        sent   <- api.sentMessages
      } yield // сто ушло получателю, десять сгорело комиссией
              assertTrue(me.silver == 390L) &&
              assertTrue(bank.silverSnapshot == 100L && payoutDao.snapshot.isEmpty) &&
              assertTrue(sent.exists(m => m._1.contains("🪙 100 отправлено игроку") && m._1.contains("Комиссия — 🪙 10"))) &&
              assertTrue(players.announcedTrade.exists(m => m.contains("передал 🪙 100") &&
                m.contains("[id" + sender.vkId.value) && m.contains("[id" + friend.vkId.value))) &&
              // получатель знает, где теперь его деньги
              assertTrue(players.sentLetters.exists { case (to, text) =>
                to == friendUs && text.contains("🪙 100") && text.contains("ячейке") })
    },

    test("ячейки у получателя нет — серебро ждёт его в городе") {
      for {
        t <- chat(silver = 500L, vaultCells = 0)
        (handler, dao, bank, payoutDao, players, _) = t
        _  <- say(handler, "Передать 250 серебра", -1L)
        me <- heroOf(dao, userId)
      } yield assertTrue(me.silver == 500L - 250L - 25L && bank.silverSnapshot == 0L) &&
              assertTrue(payoutDao.snapshot.get(friendHero).contains((250L, 0L))) &&
              assertTrue(players.sentLetters.exists { case (to, text) =>
                to == friendUs && text.contains("в городе") })
    },

    test("дублоны идут сразу в руки: ни ячейки, ни ожидания города") {
      for {
        t <- chat(doubloons = 100L)
        (handler, dao, bank, payoutDao, players, api) = t
        _      <- say(handler, "Передать дублоны 50", -1L)
        me     <- heroOf(dao, userId)
        friend <- heroOf(dao, friendUs)
        sent   <- api.sentMessages
      } yield // получатель получает ровно названное, комиссия сгорает
              assertTrue(me.doubloons == 45L && friend.doubloons == 50L) &&
              assertTrue(bank.silverSnapshot == 0L && payoutDao.snapshot.isEmpty) &&
              assertTrue(sent.exists(m => m._1.contains("🟡 50 отправлено игроку") && m._1.contains("Комиссия — 🟡 5"))) &&
              assertTrue(players.sentLetters.exists { case (to, text) =>
                to == friendUs && text.contains("🟡 50") && text.contains("уже у вас") })
    },

    test("дублоны отдают и из лабиринта: они не теряются нигде") {
      for {
        t <- chat(doubloons = 100L, state = StateType.Dungeon)
        (handler, dao, _, _, _, api) = t
        _      <- say(handler, "Передать 60 дублонов", -1L)
        me     <- heroOf(dao, userId)
        friend <- heroOf(dao, friendUs)
        sent   <- api.sentMessages.map(_.map(_._1).mkString("\n"))
      } yield assertTrue(me.doubloons == 100L - 60L - 6L && friend.doubloons == 60L) &&
              assertTrue(!sent.contains("только в городе"))
    },

    test("не хватает с комиссией, меньше минимума, без суммы — ничего не списано, вещей не ищем") {
      for {
        t <- chat(silver = 100L, doubloons = 50L)
        (handler, dao, bank, payoutDao, players, api) = t
        // ровно сто есть, но с комиссией нужно сто десять
        _    <- say(handler, "Передать серебро 100", -1L)
        _    <- say(handler, "Передать дублоны 50", -2L)
        _    <- say(handler, "Передать серебро 49", -3L)
        _    <- say(handler, "Передать 30 дублонов", -4L)
        _    <- say(handler, "Передать серебро", -5L)
        me   <- heroOf(dao, userId)
        sent <- api.sentMessages.map(_.map(_._1).mkString("\n"))
      } yield assertTrue(me.silver == 100L && me.doubloons == 50L) &&
              assertTrue(bank.silverSnapshot == 0L && payoutDao.snapshot.isEmpty) &&
              assertTrue(players.announcedTrade.isEmpty) &&
              assertTrue(sent.contains("Не хватает серебра: нужно 🪙 110 вместе с комиссией 🪙 10")) &&
              assertTrue(sent.contains("Не хватает дублонов: нужно 🟡 55 вместе с комиссией 🟡 5")) &&
              assertTrue(sent.contains("Меньше 🪙 50 не передать") && sent.contains("Меньше 🟡 50 не передать")) &&
              assertTrue(sent.contains("Сколько передать?")) &&
              // в сумке «серебро» не искали
              assertTrue(!sent.contains("В сумке нет ничего похожего"))
    },

    test("серебро из лабиринта не передать — только в городе") {
      for {
        t <- chat(silver = 500L, state = StateType.Dungeon)
        (handler, dao, bank, _, _, api) = t
        _    <- say(handler, "Передать серебро 100", -1L)
        me   <- heroOf(dao, userId)
        sent <- api.sentMessages.map(_.map(_._1).mkString("\n"))
      } yield assertTrue(me.silver == 500L && bank.silverSnapshot == 0L) &&
              assertTrue(sent.contains("только в городе")) &&
              // в отказе — подсказка про посыльного
              assertTrue(sent.contains("личный посыльный") && sent.contains("Обустройстве"))
    },

    test("посыльный: из лабиринта серебро уходит из ячейки, а то, что при себе, остаётся") {
      for {
        t <- chat(silver = 500L, state = StateType.Dungeon, vaultSilver = 300L, courier = true)
        (handler, dao, bank, payoutDao, players, api) = t
        _    <- say(handler, "Передать серебро 100", -1L)
        me   <- heroOf(dao, userId)
        sent <- api.sentMessages.map(_.map(_._1).mkString("\n"))
      } yield // при себе — нетронуто; ячейка в тестах общая: −110 у отправителя, +100 получателю
              assertTrue(me.silver == 500L && bank.silverSnapshot == 300L - 110L + 100L) &&
              assertTrue(payoutDao.snapshot.isEmpty) &&
              assertTrue(sent.contains("🏃 Посыльный отнёс 🪙 100 из вашей ячейки Торгового дома") &&
                         sent.contains("Комиссия — 🪙 10")) &&
              assertTrue(!sent.contains("только в городе")) &&
              assertTrue(players.announcedTrade.exists(_.contains("передал 🪙 100")))
    },

    test("посыльный: в ячейке мало — отказ, ничего не снято; при себе серебро он не берёт") {
      for {
        t <- chat(silver = 500L, state = StateType.TreasureDig, vaultSilver = 100L, courier = true)
        (handler, dao, bank, _, players, api) = t
        _    <- say(handler, "Передать серебро 100", -1L)
        me   <- heroOf(dao, userId)
        sent <- api.sentMessages.map(_.map(_._1).mkString("\n"))
      } yield assertTrue(me.silver == 500L && bank.silverSnapshot == 100L) &&
              assertTrue(sent.contains("Посыльный носит серебро только из ячейки Торгового дома: нужно 🪙 110 вместе с комиссией 🪙 10, а в ячейке 🪙 100.")) &&
              assertTrue(players.announcedTrade.isEmpty)
    },

    test("вещь из лабиринта посыльный ищет в хранилище (см. CourierItemsSpec): нет её там — так и говорит, серебро не тронуто") {
      for {
        t <- chat(silver = 500L, state = StateType.Dungeon, vaultSilver = 300L, courier = true)
        (handler, _, bank, _, _, api) = t
        _    <- say(handler, "Передать надколотый рубин", -1L)
        sent <- api.sentMessages.map(_.map(_._1).mkString("\n"))
      } yield assertTrue(sent.contains("Посыльный носит вещи только из хранилища Торгового дома")) &&
              assertTrue(!sent.contains("только в городе") && bank.silverSnapshot == 300L)
    },

    test("посыльный в городе ничего не меняет: сперва то, что при себе, нехватка — из ячейки") {
      for {
        t <- chat(silver = 60L, vaultSilver = 300L, courier = true)
        (handler, dao, bank, _, _, api) = t
        _    <- say(handler, "Передать серебро 100", -1L)
        me   <- heroOf(dao, userId)
        sent <- api.sentMessages.map(_.map(_._1).mkString("\n"))
      } yield // 60 при себе ушли целиком, 50 — из ячейки; получателю +100 в ту же ячейку
              assertTrue(me.silver == 0L && bank.silverSnapshot == 300L - 50L + 100L) &&
              assertTrue(sent.contains("🪙 100 отправлено игроку") && !sent.contains("Посыльный"))
    },

    test("в городе серебро передают с любого экрана; из «Персонажа» и посреди задания — нет") {
      def from(state: StateType) =
        for {
          t <- chat(silver = 500L, state = state)
          (handler, dao, _, _, _, api) = t
          _    <- say(handler, "Передать серебро 100", -1L)
          me   <- heroOf(dao, userId)
          sent <- api.sentMessages.map(_.map(_._1).mkString("\n"))
        } yield (me.silver < 500L, sent.contains("только в городе"))
      for {
        herbs   <- from(StateType.GustavoHerbs)
        mercs   <- from(StateType.Mercenaries)
        gate    <- from(StateType.Outskirts)
        bag     <- from(StateType.Inventory)
        thieves <- from(StateType.Thieves)
      } yield assertTrue(herbs == (true, false) && mercs == (true, false) && gate == (true, false)) &&
              // меню «Персонаж» и сцена задания — отказ, серебро на месте
              assertTrue(bag == (false, true) && thieves == (false, true))
    }
  )
}
