package pangea.service.state.states.parcel

import pangea.engine.SceneContent
import pangea.model.artifact.HeroArtifacts
import pangea.model.hero.HeroId
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.Background
import pangea.service.chat.ChatCommand
import pangea.service.parcel.{Parcels, TransferTarget, Transfers}
import pangea.service.state.{PlayerLock, UserAction}
import pangea.test._
import zio.test._
import zio.{ZIO, durationInt}

/** Передача вещей между игроками: команда из общей беседы, выбор в личке и
  * посылка, которая сама ложится получателю в банковскую ячейку. */
object TransferSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val heroId   = HeroId(1L)
  private val friendUs = UserId(2L)
  private val friend   = HeroId(2L)
  private val friendUser = User(friendUs, VkId("vk_friend"), TelegramId("tg_friend"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private val target = TransferTarget(friend, friendUs, "Пётр")

  /** Из лички доставка идёт отдельным волокном: ждём письмо получателю по
    * живым часам, а не по тестовым. */
  private def lettersOf(players: TestPlayers) =
    Live.live((ZIO.sleep(10.millis) *> ZIO.succeed(players.sentLetters)).repeatUntil(_.nonEmpty)
      .timeoutFail(new Throwable("письмо получателю не дошло"))(5.seconds))

  /** «Надколотый череп» — камень-усилитель первого грейда. */
  private def skull(id: Long): Item =
    pangea.generator.item.GemGenerator.item(pangea.model.item.GemKind.Skull, 1).copy(id = id)

  /** Трофей с моба: такое не передаётся. */
  private def trophy(id: Long): Item =
    Item(id, "Клык белого волка", 1L, Rarity.Gray, ItemType.Trophy, attack = 0, accuracy = 0,
      energy = 0, armor = 0, defence = 0, evasion = 0)

  private def sword(id: Long, name: String = "Меч"): Item =
    Item(id, name, 3L, Rarity.Blue, ItemType.Weapon, attack = 7, accuracy = 2, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  private def scene(query: String, count: Int) =
    TransferState.TransferScene(
      toHeroId = friend.value, toUserId = friendUs.value, toName = "Пётр", query = query, count = count)

  /** Сервис передачи и всё, что к нему прилагается. */
  private def fixture(items: List[Item]) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = StateType.TradeHouse))
      invRepo   = TestInventoryRepository.withItems(items)
      userRepo <- TestUserRepository.withUsers(testUser, friendUser)
      parcelDao = TestParcelDao.empty
      content  <- ZIO.attempt(SceneContent.load())
      players   = new TestPlayers
      bank      = TestBankRepository.withCells(1)
      parcels   = Parcels(parcelDao, bank, content)
      payouts   = pangea.service.payout.Payouts(TestPayoutDao.empty, heroDao, content)
      lock     <- PlayerLock.make
      hero     <- heroDao.getHeroByUserId(userId).map(_.get)
    } yield (Transfers(invRepo, userRepo, parcels, players, content, heroDao, bank, payouts, lock),
             hero, invRepo, parcelDao, players)

  private def transfer(items: List[Item], query: String, count: Int = 1) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = StateType.TradeHouse))
      _        <- heroDao.insertHero(TestFixtures.hero(friendUs).copy(id = friend))
      _        <- heroDao.writeSceneData(userId, io.circe.syntax.EncoderOps(scene(query, count)).asJson)
      invRepo   = TestInventoryRepository.withItems(items)
      userRepo <- TestUserRepository.withUsers(testUser, friendUser)
      parcelDao = TestParcelDao.empty
      bank      = TestBankRepository.withCells(1)
      content  <- ZIO.attempt(SceneContent.load())
      players   = new TestPlayers
      renderer <- TestRenderer.make
      parcels   = Parcels(parcelDao, bank, content)
      lock     <- PlayerLock.make
      transfers = Transfers(invRepo, userRepo, parcels, players, content, heroDao, bank,
                    pangea.service.payout.Payouts(TestPayoutDao.empty, heroDao, content), lock)
      bg       <- Background.make(Background.Grace)
    } yield (TransferState(heroDao, invRepo, transfers, content, bg),
             invRepo, parcelDao, bank, players, renderer)

  override def spec = suite("Передача вещей")(

    test("разбор фразы: что передают и сколько") {
      assertTrue(ChatCommand.transferQuery("Передать расколотый череп 2 штук").contains("расколотый череп 2 штук")) &&
      assertTrue(ChatCommand.transferQuery("привет").isEmpty) &&
      assertTrue(ChatCommand.count("расколотый череп 2 штук") == 2) &&
      assertTrue(ChatCommand.count("меч х3") == 3 && ChatCommand.count("меч") == 1) &&
      assertTrue(ChatCommand.itemQuery("расколотый череп 2 штук") == "расколотый череп") &&
      assertTrue(ChatCommand.matches("череп", "Расколотый череп")) &&
      assertTrue(ChatCommand.matches("меч", "⚔ [Ур.3] Меч") && !ChatCommand.matches("щит", "Меч")) &&
      // пустой запрос подходит всему: игрок выберет вещь кнопкой
      assertTrue(ChatCommand.matches("", "Что угодно"))
    },

    suite("Без уточнения")(

      test("точное название и не экипировка — уходит сразу, экран не нужен") {
        for {
          f <- fixture((1L to 3L).toList.map(skull))
          (transfers, hero, inv, parcelDao, players) = f
          out <- transfers.quickSend(testUser, hero, target, "надколотый Череп", 2, 0L)
        } yield assertTrue(out.isInstanceOf[Transfers.Outcome.Sent]) &&
                // регистр не важен, ушли ровно две
                assertTrue(parcelDao.snapshot.size == 2 && inv.snapshot.map(_.id) == List(3L)) &&
                assertTrue(players.announcedTrade.exists(m =>
                  m.contains("[id" + testUser.vkId.value) && m.contains("[id" + friendUser.vkId.value) &&
                  m.contains("Надколотый череп")))
      },

      test("экипировка в единственном числе тоже уходит сразу") {
        for {
          f <- fixture(List(sword(1L)))
          (transfers, hero, inv, parcelDao, _) = f
          out <- transfers.quickSend(testUser, hero, target, "меч", 1, 0L)
        } yield assertTrue(out.isInstanceOf[Transfers.Outcome.Sent]) &&
                assertTrue(parcelDao.snapshot.map(_.item.id) == List(1L) && inv.snapshot.isEmpty)
      },

      test("двух мечей уже не спутать вслепую — уточняем кнопками") {
        for {
          f <- fixture(List(sword(1L).copy(attack = 12), sword(2L).copy(attack = 9)))
          (transfers, hero, inv, parcelDao, _) = f
          out <- transfers.quickSend(testUser, hero, target, "меч", 1, 0L)
        } yield assertTrue(out == Transfers.Outcome.NeedPick) &&
                assertTrue(parcelDao.snapshot.isEmpty && inv.snapshot.size == 2)
      },

      test("подходит несколько разных вещей — тоже уточняем") {
        for {
          f <- fixture(List(skull(1L), skull(2L).copy(name = "Надколотый череп волка")))
          (transfers, hero, _, parcelDao, _) = f
          out <- transfers.quickSend(testUser, hero, target, "череп", 1, 0L)
        } yield assertTrue(out == Transfers.Outcome.NeedPick && parcelDao.snapshot.isEmpty)
      },

      test("трофеи не передаются") {
        for {
          f <- fixture(List(trophy(1L)))
          (transfers, hero, inv, parcelDao, _) = f
          out <- transfers.quickSend(testUser, hero, target, "клык", 1, 0L)
        } yield assertTrue(out.isInstanceOf[Transfers.Outcome.Forbidden]) &&
                assertTrue(parcelDao.snapshot.isEmpty && inv.snapshot.size == 1)
      },

      test("стопку передают целиком и только со словом: «стопку надколотый череп» — стопку, без слова — сам череп") {
        val bundle = pangea.model.item.Bundling.pack((11L to 15L).toList.map(skull)).get.copy(id = 7L)
        for {
          f   <- fixture(List(bundle, skull(1L)))
          (transfers, hero, inv, parcelDao, _) = f
          one <- transfers.quickSend(testUser, hero, target, "надколотый череп", 1, 0L)
          two <- transfers.quickSend(testUser, hero, target, "Стопку надколотый череп", 1, 0L)
        } yield assertTrue(one.isInstanceOf[Transfers.Outcome.Sent] && two.isInstanceOf[Transfers.Outcome.Sent]) &&
                assertTrue(parcelDao.snapshot.map(_.item.id) == List(1L, 7L) && inv.snapshot.isEmpty) &&
                assertTrue(parcelDao.snapshot.exists(_.item == bundle))
      },

      test("без слова «стопку» стопку не отдают, даже когда отдельных таких вещей нет, — и подсказывают, как") {
        val bundle = pangea.model.item.Bundling.pack((11L to 15L).toList.map(skull)).get.copy(id = 7L)
        for {
          f   <- fixture(List(bundle))
          (transfers, hero, inv, parcelDao, _) = f
          out <- transfers.quickSend(testUser, hero, target, "надколотый череп", 1, 0L)
        } yield assertTrue(out == Transfers.Outcome.Forbidden(
                  "📦 «надколотый череп» у вас только в стопке. Чтобы передать стопку, так и скажите: " +
                  "«Передать стопку надколотый череп».")) &&
                assertTrue(parcelDao.snapshot.isEmpty && inv.snapshot == List(bundle))
      },

      test("строка о передаче — в беседу, где её затеяли; стопка в ней, в ответе и в письме — «стопку «…»»") {
        val bundle  = pangea.model.item.Bundling.pack((11L to 15L).toList.map(skull)).get.copy(id = 7L)
        val general = 2000000001L
        for {
          f    <- fixture(List(bundle))
          (transfers, hero, _, _, players) = f
          out  <- transfers.quickSend(testUser, hero, target, "стопку надколотый череп", 1, 0L, Some(general))
          sent  = Some(out).collect { case s: Transfers.Outcome.Sent => s }
          _    <- ZIO.foreachDiscard(sent)(_.handOver)
        } yield assertTrue(players.announcedTrade.isEmpty) &&
                assertTrue(players.announcedAt == List(general ->
                  s"📦 Игрок [id${testUser.vkId.value}|Иван Иванов] передал стопку «Надколотый череп» игроку [id${friendUser.vkId.value}|Пётр]")) &&
                assertTrue(sent.map(_.message).contains(
                  "📦 Вы передали стопку «Надколотый череп» игроку Пётр. Всё ляжет к нему в банковскую ячейку.")) &&
                assertTrue(players.sentLetters.exists { case (to, text) =>
                  to == friendUs && text.startsWith("📦 Иван Иванов передал(а) вам: стопку «Надколотый череп».") })
      },

      test("стопки в строках о передаче — по-русски: стопку, 2 стопки, 5 стопок, 21 стопку; прочее — «×N»") {
        val bundle = pangea.model.item.Bundling.pack((11L to 15L).toList.map(skull)).get.copy(id = 7L)
        for {
          f <- fixture(Nil)
          (transfers, _, _, _, _) = f
        } yield assertTrue(transfers.what(bundle, 1) == "стопку «Надколотый череп»") &&
                assertTrue(transfers.what(bundle, 2) == "2 стопки «Надколотый череп»") &&
                assertTrue(transfers.what(bundle, 5) == "5 стопок «Надколотый череп»") &&
                assertTrue(transfers.what(bundle, 12) == "12 стопок «Надколотый череп»") &&
                assertTrue(transfers.what(bundle, 21) == "21 стопку «Надколотый череп»") &&
                assertTrue(transfers.what(skull(1L), 2) == "Надколотый череп ×2")
      },

      test("«стопка» и «стопку» — оба слова годятся; одно слово — любая стопка, а вся сумка — со стопками") {
        val bundle = pangea.model.item.Bundling.pack((11L to 15L).toList.map(skull)).get.copy(id = 7L)
        for {
          f <- fixture(Nil)
          (transfers, _, _, _, _) = f
          items = List(bundle, skull(1L), sword(2L))
        } yield assertTrue(transfers.matching(items, "стопка надколотый череп") == List(bundle)) &&
                assertTrue(transfers.matching(items, "стопку надколотый череп") == List(bundle)) &&
                assertTrue(transfers.matching(items, "стопку") == List(bundle)) &&
                assertTrue(transfers.matching(items, "череп") == List(skull(1L))) &&
                assertTrue(transfers.matching(items, "").toSet == items.toSet)
      },

      test("в сумке ничего похожего — так и говорим") {
        for {
          f <- fixture(List(sword(1L)))
          (transfers, hero, _, _, _) = f
          out <- transfers.quickSend(testUser, hero, target, "череп", 1, 0L)
        } yield assertTrue(out == Transfers.Outcome.Empty)
      }
    ),

    test("список показывает только подходящее, одинаковые названия — разными кнопками") {
      val two = List(sword(1L).copy(attack = 12), sword(2L).copy(attack = 9), skull(3L))
      for {
        t <- transfer(two, query = "меч")
        (state, _, _, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        ids      = screens.last.choices.map(_.id)
      } yield assertTrue(ids.contains("Give_1") && ids.contains("Give_2") && !ids.contains("Give_3")) &&
              assertTrue(screens.last.text.contains("Пётр"))
    },

    test("на пустой запрос показываем всю сумку, кроме того, что не передаётся") {
      for {
        t <- transfer(List(sword(1L), trophy(2L)), query = "")
        (state, _, _, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        ids      = screens.last.choices.map(_.id)
      } yield assertTrue(ids.contains("Give_1") && !ids.contains("Give_2"))
    },

    test("выбор и подтверждение: вещи сразу у получателя в ячейке, ему письмо") {
      val skulls = (1L to 5L).toList.map(skull)
      for {
        t <- transfer(skulls, query = "череп", count = 2)
        (state, inv, parcelDao, bank, players, renderer) = t
        _       <- state.action(testUser, tap("Give_1"), renderer)
        confirm <- renderer.sentScreens.map(_.last)
        next    <- state.action(testUser, tap("TransferYes"), renderer)
        letters <- lettersOf(players)
      } yield assertTrue(confirm.inline && confirm.text.contains("×2")) &&
              // ушли ровно две из пяти — и обе легли получателю в ячейку
              assertTrue(inv.snapshot.map(_.id) == List(3L, 4L, 5L)) &&
              assertTrue(bank.itemsSnapshot.map(_.id) == List(1L, 2L) && parcelDao.snapshot.isEmpty) &&
              assertTrue(letters.map(_._1) == List(friendUs)) &&
              assertTrue(letters.head._2.contains("Надколотый череп ×2. Уже в вашей банковской ячейке.")) &&
              // возвращаемся туда, откуда пришли
              assertTrue(next == StateType.GlobalMap)
    },

    test("посылка сама ложится в банковскую ячейку получателя") {
      for {
        heroDao  <- TestHeroDao.withHero(friendUs, TestFixtures.hero(friendUs).copy(id = friend))
        parcelDao = TestParcelDao.empty
        bank      = TestBankRepository.withCells(1)
        content  <- ZIO.attempt(SceneContent.load())
        renderer <- TestRenderer.make
        parcels   = Parcels(parcelDao, bank, content)
        hero     <- heroDao.getHeroByUserId(friendUs).map(_.get)
        _        <- parcels.send(friend, "Иван", sword(10L), 0L)
        _        <- parcels.deliver(friendUser, hero, renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(bank.itemsSnapshot.map(_.id) == List(10L)) &&
              assertTrue(parcelDao.snapshot.isEmpty) &&
              assertTrue(screens.last.text.contains("банковскую ячейку"))
    },

    test("ячейки нет или она полна — посылка ждёт на почте, а герою сказано про 7 дней") {
      for {
        heroDao  <- TestHeroDao.withHero(friendUs, TestFixtures.hero(friendUs).copy(id = friend))
        parcelDao = TestParcelDao.empty
        bank      = TestBankRepository.empty            // ячейка не куплена
        content  <- ZIO.attempt(SceneContent.load())
        renderer <- TestRenderer.make
        parcels   = Parcels(parcelDao, bank, content)
        hero     <- heroDao.getHeroByUserId(friendUs).map(_.get)
        _        <- parcels.send(friend, "Иван", sword(10L), 0L)
        _        <- parcels.deliver(friendUser, hero, renderer)
        screens  <- renderer.sentScreens
        waiting  <- parcels.waitingCount(friend)
      } yield assertTrue(waiting == 1L && bank.itemsSnapshot.isEmpty) &&
              // срок пошёл с этой минуты
              assertTrue(parcelDao.snapshot.map(_.mailedAt) == List(Some(0L))) &&
              assertTrue(screens.size == 1) &&
              assertTrue(screens.head.text.contains("На почту Торгового дома пришло: [Ур.3] Меч (Иван)")) &&
              assertTrue(screens.head.text.contains(s"${Parcels.ShelfDays} дней"))
    },

    test("почта: забрать одну и забрать всё, что влезет") {
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        parcelDao = TestParcelDao.empty
        content  <- ZIO.attempt(SceneContent.load())
        renderer <- TestRenderer.make
        parcels   = Parcels(parcelDao, TestBankRepository.empty, content)
        invRepo   = TestInventoryRepository.accepting
        state     = MailState(heroDao, invRepo, parcels, content)
        _        <- parcels.send(heroId, "Иван", sword(10L), 0L)
        _        <- parcels.send(heroId, "Иван", sword(11L), 0L)
        _        <- state.enter(testUser, renderer)
        list     <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, tap("MailTake", "id" -> "1"), renderer)
        afterOne  = (invRepo.snapshot.map(_.id), parcelDao.snapshot.size)
        _        <- state.action(testUser, tap("MailAll"), renderer)
      } yield assertTrue(list.choices.map(_.id).count(_ == "MailTake") == 2) &&
              assertTrue(afterOne == (List(10L), 1)) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(10L, 11L) && parcelDao.snapshot.isEmpty)
    },

    test("в Торговом доме почта видна, только когда на ней что-то есть") {
      import pangea.service.state.states.bank.TradeHouseState
      def house(withMail: Boolean) =
        for {
          heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
          parcelDao = TestParcelDao.empty
          content  <- ZIO.attempt(SceneContent.load())
          renderer <- TestRenderer.make
          bank      = TestBankRepository.withCells(1)
          parcels   = Parcels(parcelDao, bank, content)
          _        <- ZIO.when(withMail)(parcels.send(heroId, "Иван", sword(10L), 0L).unit)
          userRepo <- TestUserRepository.withUser(testUser)
          state     = TradeHouseState(heroDao, userRepo, bank, parcels, TestInventoryRepository.accepting,
                        TestDonations.off, content)
          _        <- state.enter(testUser, renderer)
          screens  <- renderer.sentScreens
        } yield screens.last.choices.map(_.id)
      for {
        without <- house(withMail = false)
        with_   <- house(withMail = true)
      } yield assertTrue(!without.contains("Mail")) && assertTrue(with_.contains("Mail")) &&
              assertTrue(HeroArtifacts.MaxTier == 4)   // артефакты на месте, ничего не поехало
    }
  )
}
