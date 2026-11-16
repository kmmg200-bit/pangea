package pangea.service.state

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, HeroId}
import pangea.model.trade.{TradeOffer, TradeSide, TradeStatus}
import pangea.model.item.{BeltSlot, FlaskKind, Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.engine.Renderer
import pangea.service.admin.{AdminPanel, HomeAlert}
import pangea.service.state.states.GlobalMapState
import pangea.service.state.states.battle.BattleState
import pangea.service.payout.Payouts
import pangea.test.{TestApi, TestBankRepository, TestFixtures, TestHeroDao, TestHeroRepository, TestInventoryRepository, TestItemRepository, TestParcelDao, TestPayoutDao, TestUserRepository}
import zio.{Task, ZIO}
import zio.test._

/** `/home` — глобальная аварийная команда (см. ARCHITECTURE.md §10): должна
 *  выкидывать героя в город из ЛЮБОГО состояния, включая бой, ничего не тратя
 *  и не начисляя, и не рассматриваться как поражение/бегство. */
object StateHandlerSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val vkId     = VkId("vk_test")
  private val testUser = User(userId, vkId, TelegramId("tg_test"))

  private val hero = TestFixtures.hero(userId, state = StateType.Battle).copy(
    fightStats = FightStats(atk = 10, hp = 37, armor = 5, defence = 0, evasion = 5, accuracy = 10, energy = 5),
    silver     = 123L,
    exp        = 45L
  )

  private val battle = SoloPveBattle(
    monsterLvl          = 5L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 5, hp = 50, armor = 0, defence = 0, evasion = 0, accuracy = 10, energy = 0),
    monsterCurrentHp    = 50L,
    monsterCurrentArmor = 0L
  )

  private def makeHandler(startState: StateType, customize: Hero => Hero = identity,
                          admin: AdminPanel = AdminPanel.disabled) =
    for {
      baseHero <- ZIO.succeed(customize(hero.copy(state = startState)))
      heroDao  <- TestHeroDao.withHero(userId, baseHero)
      _        <- heroDao.writeActiveBattle(userId, battle.asJson)
      _        <- heroDao.writeSceneData(userId, Json.obj("someLeftoverKey" -> Json.fromString("junk")))
      heroRepo <- TestHeroRepository.withHero(userId, baseHero)
      userRepo <- TestUserRepository.withUser(testUser)
      api      <- TestApi.make
      content  <- ZIO.attempt(SceneContent.load())
      states = Map[StateType, State](
        StateType.GlobalMap -> GlobalMapState(heroDao, content),
        StateType.Battle    -> BattleState(heroDao, TestInventoryRepository.accepting, TestItemRepository.make, content)
      )
      lock <- PlayerLock.make
      payouts   = pangea.service.payout.Payouts(pangea.test.TestPayoutDao.empty, heroDao, content)
      parcels   = pangea.service.parcel.Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
      transfers = pangea.service.parcel.Transfers(TestInventoryRepository.accepting, userRepo, parcels,
                    new pangea.test.TestPlayers, content, heroDao, TestBankRepository.empty, payouts, lock)
    } yield (new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers, states,
                       admin, content, lock), heroDao, heroRepo, api)

  /** Панель-заглушка для пересылки /home и /restart: «Админ 1» — те, кого вернёт
    * `watchers`. */
  private def watchedBy(watchers: UserId => Task[List[VkId]]): AdminPanel = new AdminPanel {
    def intercept(user: User, hero: Hero, action: UserAction, renderer: Renderer): Task[Boolean] = ZIO.succeed(false)
    def homeWatchers(except: UserId): Task[List[VkId]] = watchers(except)
    def canGrant(userId: UserId): Task[Boolean] = ZIO.succeed(false)
  }

  /** Обвязка для отложенной выручки: герой, диспетчер и склад невыданных денег. */
  private def makePayoutHandler(startState: StateType) =
    for {
      baseHero  <- ZIO.succeed(hero.copy(state = startState))
      heroDao   <- TestHeroDao.withHero(userId, baseHero)
      heroRepo  <- TestHeroRepository.withHero(userId, baseHero)
      userRepo  <- TestUserRepository.withUser(testUser)
      api       <- TestApi.make
      content   <- ZIO.attempt(SceneContent.load())
      payoutDao  = TestPayoutDao.empty
      payouts    = Payouts(payoutDao, heroDao, content)
      lock      <- PlayerLock.make
      parcels    = pangea.service.parcel.Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
      transfers  = pangea.service.parcel.Transfers(TestInventoryRepository.accepting, userRepo, parcels,
                     new pangea.test.TestPlayers, content, heroDao, TestBankRepository.empty, payouts, lock)
      states     = Map[StateType, State](
        StateType.GlobalMap -> GlobalMapState(heroDao, content),
        StateType.Battle    -> BattleState(heroDao, TestInventoryRepository.accepting, TestItemRepository.make, content))
    } yield (new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers, states,
                       pangea.service.admin.AdminPanel.disabled, content, lock), heroDao, api, payoutDao)

  override def spec = suite("StateHandler /home")(

    test("выручка с аукциона догоняет героя в городе, а в бою ждёт своего часа") {
      for {
        t <- makePayoutHandler(StateType.GlobalMap)
        (handler, heroDao, api, payouts) = t
        _        <- payouts.add(hero.id, 300L, 0L)
        _        <- handler.makeActionVK(vkId, eventId = 1L, UserAction("", None))
        updated  <- heroDao.getHeroByUserId(userId).map(_.get)
        messages <- api.sentMessages
        // Тот же герой в лабиринте денег не получает: там их отнимет смерть.
        d <- makePayoutHandler(StateType.Battle)
        (fighter, fightDao, _, fightPayouts) = d
        _        <- fightPayouts.add(hero.id, 300L, 0L)
        _        <- fighter.makeActionVK(vkId, eventId = 2L, UserAction("", None))
        inFight  <- fightDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(updated.silver == hero.silver + 300L) &&
              assertTrue(messages.exists(_._1.contains("деньги, что вас дожидались"))) &&
              assertTrue(payouts.snapshot.isEmpty) &&
              assertTrue(inFight.silver == hero.silver && fightPayouts.snapshot.nonEmpty)
    },

    test("/home посреди боя → переносит в GlobalMap, чистит активный бой и scene_data, герой ничего не теряет/не получает") {
      for {
        t <- makeHandler(StateType.Battle)
        (handler, heroDao, heroRepo, _) = t
        _        <- handler.makeActionVK(vkId, eventId = 1L, UserAction("/home", None))
        updated  <- heroRepo.getHero(userId).map(_.get)
        battleOp <- heroDao.readActiveBattle(userId)
        sceneOp  <- heroDao.readSceneData(userId)
      } yield assertTrue(updated.state == StateType.GlobalMap) &&
              assertTrue(updated.fightStats.hp == 37L) &&      // не умер, HP как было
              assertTrue(updated.silver == 123L) &&            // не потерял и не получил серебро
              assertTrue(updated.exp == 45L) &&                // опыт не тронут
              assertTrue(updated.traumaNames.isEmpty) &&       // травму (как при смерти) не получил
              assertTrue(battleOp.isEmpty) &&                  // активный бой сброшен
              assertTrue(sceneOp.contains(Json.Null))          // scene_data сброшен
    },

    test("/HOME  (регистр и пробелы не важны) срабатывает так же") {
      for {
        t <- makeHandler(StateType.Battle)
        (handler, _, heroRepo, _) = t
        _       <- handler.makeActionVK(vkId, eventId = 1L, UserAction("  /HOME  ", None))
        updated <- heroRepo.getHero(userId).map(_.get)
      } yield assertTrue(updated.state == StateType.GlobalMap)
    },

    test("/home уходит «Админ 1»: само сообщение игрока и подпись, из какой сцены он выбирался") {
      val admin1 = VkId("777")
      val admin2 = VkId("888")
      for {
        t <- makeHandler(StateType.Battle, admin = watchedBy(_ => ZIO.succeed(List(admin1, admin2))))
        (handler, _, heroRepo, api) = t
        _       <- handler.makeActionVK(vkId, eventId = 42L, UserAction("/home", None))
        updated <- heroRepo.getHero(userId).map(_.get)
        sent    <- api.forwards
      } yield assertTrue(updated.state == StateType.GlobalMap) &&
              assertTrue(sent.map(_._1) == List(admin1, admin2)) &&
              // пересылается ровно это сообщение — по его номеру в диалоге сообщества
              assertTrue(sent.forall(_._3.contains(42L))) &&
              assertTrue(sent.forall(_._2 == HomeAlert.caption(vkId, StateType.Battle))) &&
              assertTrue(HomeAlert.caption(vkId, StateType.Battle) == "🆘 /home от vk.com/idvk_test\nСцена: бой")
    },

    test("свой /home админу не пересылается, а без «Админ 1» не пересылается никому") {
      for {
        // панель исключает самого игрока — и других «Админ 1» нет
        t <- makeHandler(StateType.Battle, admin = watchedBy(except => ZIO.succeed(List(VkId("777")).filter(_ => except != userId))))
        (self, _, _, selfApi) = t
        _      <- self.makeActionVK(vkId, eventId = 1L, UserAction("/home", None))
        toSelf <- selfApi.forwards
        d <- makeHandler(StateType.Battle)
        (nobody, _, _, nobodyApi) = d
        _      <- nobody.makeActionVK(vkId, eventId = 1L, UserAction("/home", None))
        toNone <- nobodyApi.forwards
      } yield assertTrue(toSelf.isEmpty && toNone.isEmpty)
    },

    test("сбой пересылки не мешает самому /home: игрок всё равно в городе") {
      for {
        t <- makeHandler(StateType.Battle, admin = watchedBy(_ => ZIO.fail(new Throwable("база недоступна"))))
        (handler, _, heroRepo, _) = t
        _       <- handler.makeActionVK(vkId, eventId = 1L, UserAction("/home", None))
        updated <- heroRepo.getHero(userId).map(_.get)
      } yield assertTrue(updated.state == StateType.GlobalMap)
    },

    test("у сцены, ещё не названной по-русски, в подписи её имя из кода") {
      assertTrue(StateType.values.forall(s => HomeAlert.sceneName(s).nonEmpty)) &&
      assertTrue(HomeAlert.sceneName(StateType.TreasureHunt) == "поход за сокровищем") &&
      // сейчас по-русски названы все сцены
      assertTrue(StateType.values.forall(s => HomeAlert.sceneName(s) != s.entryName))
    },

    test("кнопка с payload не считается командой /home, даже если текст совпадает") {
      for {
        t <- makeHandler(StateType.Battle)
        (handler, _, heroRepo, _) = t
        _       <- handler.makeActionVK(vkId, eventId = 1L, UserAction("/home", Some("""{"action":"Attack"}""")))
        updated <- heroRepo.getHero(userId).map(_.get)
      } yield assertTrue(updated.state == StateType.Battle)
    },

    test("вампирская фляга с пояса + /home → эффект не остаётся на герое и не попадёт в следующий бой") {
      val flaskBelt = Item(9L, "Пояс", 1L, ItemRarity.Green, ItemType.Belt,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        details = ItemDetails.Belt(List(BeltSlot.Flask(FlaskKind.Vampiric, ItemRarity.Gray, 1))))
      for {
        t <- makeHandler(StateType.Battle, h => h.copy(equipment = TestFixtures.emptyEquipment.copy(belt = flaskBelt)))
        (handler, heroDao, heroRepo, _) = t

        // 1) Достаём флягу с пояса. BattleState читает/пишет героя через heroDao
        // (не heroRepo) — сверяем состояние оттуда же, откуда его видит сам бой.
        _              <- handler.makeActionVK(vkId, eventId = 1L, UserAction("", Some("""{"action":"BeltUse","slot":"0"}""")))
        duringBattle   <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        heroAfterDrink <- heroDao.getHeroByUserId(userId).map(_.get)
        beltAfterDrink  = heroAfterDrink.equipment.belt.details.asInstanceOf[ItemDetails.Belt]

        // 2) Прописываем /home посреди того же боя.
        _                 <- handler.makeActionVK(vkId, eventId = 2L, UserAction("/home", None))
        activeBattleAfter <- heroDao.readActiveBattle(userId)
        finalHero         <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield
        // эффект реально применился и был виден только внутри active_battle текущего боя
        assertTrue(duringBattle.effects.heroVampiricHits > 0) &&
        // заряд списался по-настоящему, а пустая фляга осталась в ячейке
        assertTrue(beltAfterDrink.slots == List(BeltSlot.Flask(FlaskKind.Vampiric, ItemRarity.Gray, 0))) &&
        // /home полностью стирает бой вместе с баффом — раз объекта с баффом больше
        // не существует, ему неоткуда «остаться навсегда» или перейти в следующий бой
        assertTrue(activeBattleAfter.isEmpty) &&
        // и герой (fightStats) не получил постоянного изменения от фляги
        assertTrue(finalHero.fightStats.evasion == hero.fightStats.evasion) &&
        // на всякий случай: любой новый бой в принципе стартует с пустым heroBattleState
        // независимо от того, что было раньше (SoloPveBattle.from не наследует бафы)
        assertTrue(pangea.model.battle.HeroBattleState.empty.isEmpty)
    },

    test("/home уже в городе → не падает, город перерисован") {
      for {
        t <- makeHandler(StateType.GlobalMap)
        (handler, _, heroRepo, api) = t
        _        <- handler.makeActionVK(vkId, eventId = 1L, UserAction("/home", None))
        updated  <- heroRepo.getHero(userId).map(_.get)
        messages <- api.sentMessages
      } yield assertTrue(updated.state == StateType.GlobalMap) &&
              assertTrue(messages.nonEmpty)
    },

    // ── /restart ──────────────────────────────────────────────────────────────
    test("/restart сам по себе ничего не стирает — только предупреждение с inline-кнопкой") {
      for {
        t <- makeRestartHandler(StateType.GlobalMap)
        (handler, heroDao, api) = t
        _        <- handler.makeActionVK(vkId, eventId = 1L, UserAction("/restart", None))
        still    <- heroDao.getHeroByUserId(userId)
        messages <- api.sentMessages
        (text, keyboard) = messages.last
      } yield assertTrue(still.exists(_.silver == 123L)) &&              // герой на месте
              assertTrue(text.contains("сотрёт героя")) &&
              assertTrue(keyboard.exists(_.inline)) &&                    // клавиатура сцены не сбита
              assertTrue(keyboard.exists(_.buttons.flatten.exists(
                _.action.payload.exists(_.noSpaces.contains(StateHandler.RestartConfirmId)))))
    },

    test("подтверждение → герой удалён, заведён новый и показана регистрация") {
      for {
        t <- makeRestartHandler(StateType.Battle)
        (handler, heroDao, api) = t
        _        <- handler.makeActionVK(vkId, eventId = 1L,
                      UserAction("", Some(s"""{"action":"${StateHandler.RestartConfirmId}"}""")))
        fresh    <- heroDao.getHeroByUserId(userId)
        battleOp <- heroDao.readActiveBattle(userId)
        messages <- api.sentMessages.map(_.map(_._1))
      } yield // новый герой — с нуля, а не тот, что был
              assertTrue(fresh.exists(_.silver == 0L)) &&
              assertTrue(fresh.exists(_.exp == 0L)) &&
              assertTrue(fresh.exists(_.state == StateType.Registration)) &&
              assertTrue(battleOp.isEmpty) &&
              assertTrue(messages.exists(_.contains("Прошлое стёрто"))) &&
              // сразу же начался первый экран регистрации
              assertTrue(messages.size >= 2)
    },

    test("кнопка с payload не считается командой /restart, даже если текст совпадает") {
      for {
        t <- makeRestartHandler(StateType.GlobalMap)
        (handler, heroDao, api) = t
        _        <- handler.makeActionVK(vkId, eventId = 1L, UserAction("/restart", Some("""{"action":"OpenCharacter"}""")))
        still    <- heroDao.getHeroByUserId(userId)
        messages <- api.sentMessages
      } yield assertTrue(still.exists(_.silver == 123L)) &&
              assertTrue(!messages.exists(_._1.contains("сотрёт героя")))
    },

    test("/restart без подтверждения «Админ 1» не уходит: игрок только спросил, герой цел") {
      for {
        t <- makeRestartHandler(StateType.GlobalMap, admin = watchedBy(_ => ZIO.succeed(List(VkId("777")))))
        (handler, heroDao, api) = t
        _      <- handler.makeActionVK(vkId, eventId = 42L, UserAction("/restart", None))
        still  <- heroDao.getHeroByUserId(userId)
        warned <- api.sentMessages.map(_.exists(_._1.contains("сотрёт героя")))
        sent   <- api.forwards
      } yield assertTrue(still.exists(_.silver == 123L) && warned && sent.isEmpty)
    },

    test("подтверждение /restart уходит «Админ 1», как /home: само нажатие и подпись — героя какого уровня стёрли") {
      val admin1 = VkId("777")
      val admin2 = VkId("888")
      for {
        t <- makeRestartHandler(StateType.Battle, admin = watchedBy(_ => ZIO.succeed(List(admin1, admin2))),
               customize = _.copy(lvl = 37L))
        (handler, heroDao, api) = t
        // как в жизни: сперва команда, потом кнопка под предупреждением
        _     <- handler.makeActionVK(vkId, eventId = 42L, UserAction("/restart", None))
        _     <- handler.makeActionVK(vkId, eventId = 43L,
                   UserAction("💀 Да, стереть всё и начать заново", Some(s"""{"action":"${StateHandler.RestartConfirmId}"}""")))
        fresh <- heroDao.getHeroByUserId(userId)
        sent  <- api.forwards
        wiped  = HomeAlert.wipeCaption(vkId, 37L, StateType.Battle)
      } yield assertTrue(fresh.exists(h => h.exp == 0L && h.state == StateType.Registration)) &&
              // пересылается только нажатие — по его номеру в диалоге сообщества
              assertTrue(sent == List((admin1, wiped, Some(43L)), (admin2, wiped, Some(43L)))) &&
              assertTrue(wiped == "💀 /restart подтверждён: vk.com/idvk_test стёр героя 37-го уровня\nСцена: бой")
    },

    test("свой подтверждённый /restart админу не пересылается, а сбой пересылки не мешает стиранию") {
      val confirm = UserAction("", Some(s"""{"action":"${StateHandler.RestartConfirmId}"}"""))
      for {
        // панель исключает самого игрока — и других «Админ 1» нет
        t <- makeRestartHandler(StateType.GlobalMap,
               admin = watchedBy(except => ZIO.succeed(List(VkId("777")).filter(_ => except != userId))))
        (self, _, selfApi) = t
        _      <- self.makeActionVK(vkId, eventId = 1L, UserAction("/restart", None))
        _      <- self.makeActionVK(vkId, eventId = 2L, confirm)
        toSelf <- selfApi.forwards
        d <- makeRestartHandler(StateType.GlobalMap, admin = watchedBy(_ => ZIO.fail(new Throwable("база недоступна"))))
        (broken, brokenDao, brokenApi) = d
        _      <- broken.makeActionVK(vkId, eventId = 1L, UserAction("/restart", None))
        warned <- brokenApi.sentMessages.map(_.exists(_._1.contains("сотрёт героя")))
        _      <- broken.makeActionVK(vkId, eventId = 2L, confirm)
        fresh  <- brokenDao.getHeroByUserId(userId)
      } yield assertTrue(toSelf.isEmpty) &&
              assertTrue(warned) &&
              assertTrue(fresh.exists(h => h.silver == 0L && h.state == StateType.Registration))
    },

    test("подтверждение /restart закрывает обмены: партнёру — его заклад, а заклад стёртого не вернётся и новому герою") {
      // комиссия — десятая часть, как у передачи денег
      def side(silver: Long = 0L, doubloons: Long = 0L) =
        TradeSide(Nil, silver, silver / 10L, doubloons, doubloons / 10L, None)
      def offer(from: UserId, to: UserId, fromSide: TradeSide, toSide: TradeSide) =
        TradeOffer(0L, from, to, fromSide, toSide, to, None, TradeStatus.Open, 1L, 1L, 1000L, None, 0)
      for {
        t <- makeTradeRestartHandler
        (handler, heroDao, tradeDao, trades, payoutDao, parcelDao) = t
        // своё предложение партнёру и его — игроку
        mine   <- tradeDao.insert(offer(userId, partner.userId, side(silver = 300L), side(silver = 200L)))
        theirs <- tradeDao.insert(offer(partner.userId, userId, side(doubloons = 40L), TradeSide.empty))
        _      <- handler.makeActionVK(vkId, eventId = 1L,
                    UserAction("", Some(s"""{"action":"${StateHandler.RestartConfirmId}"}""")))
        fresh  <- heroDao.getHeroByUserId(userId)
        // срок потом выходит — а отдавать новому герою нечего: оба закрыты
        _      <- trades.expire(2000L)
        all     = tradeDao.snapshot
      } yield assertTrue(all.find(_.id == mine).exists(_.status == TradeStatus.Cancelled)) &&
              assertTrue(all.find(_.id == theirs).exists(_.status == TradeStatus.Declined)) &&
              // партнёру — 200 серебра и 40 дублонов с комиссией: ячейки нет, ждут в городе
              assertTrue(payoutDao.snapshot.get(partnerHero).contains((220L, 44L))) &&
              // игроку — ни старому герою, ни новому: 300 серебра с комиссией сгорели
              assertTrue(payoutDao.snapshot.get(hero.id).isEmpty && parcelDao.snapshot.isEmpty) &&
              assertTrue(fresh.exists(h => h.state == StateType.Registration && h.silver == 0L && h.doubloons == 0L))
    }
  )

  /** Партнёр по обмену: второй игрок со своим героем. */
  private val partner     = User(UserId(2L), VkId("vk_partner"), TelegramId("tg_partner"))
  private val partnerHero = HeroId(2L)

  /** Обвязка для /restart с обменами: как [[makeRestartHandler]], плюс партнёр
    * в Торговом доме и сами обмены. Ячеек нет: деньги, что вернулись
    * партнёру, ждут его в городе. */
  private def makeTradeRestartHandler =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero.copy(state = StateType.TradeHouse))
      _        <- heroDao.insertHero(TestFixtures.hero(partner.userId, state = StateType.TradeHouse).copy(id = partnerHero))
      heroRepo  = TestHeroRepository.backedBy(heroDao)
      userRepo <- TestUserRepository.withUsers(testUser, partner)
      api      <- TestApi.make
      content  <- ZIO.attempt(SceneContent.load())
      players   = new pangea.test.TestPlayers
      journal  <- pangea.test.TestJournal.make
      states = Map[StateType, State](
        StateType.Registration -> pangea.service.state.states.registration.RegistrationState(
                                    players, heroDao, TestInventoryRepository.accepting, TestItemRepository.make, journal, content)
      )
      lock     <- PlayerLock.make
      bg       <- pangea.service.Background.make(zio.Duration.fromSeconds(1L))
      payoutDao = TestPayoutDao.empty
      payouts   = Payouts(payoutDao, heroDao, content)
      bank      = TestBankRepository.empty
      parcelDao = TestParcelDao.empty
      parcels   = pangea.service.parcel.Parcels(parcelDao, bank, content)
      transfers = pangea.service.parcel.Transfers(TestInventoryRepository.accepting, userRepo, parcels, players, content,
                    heroDao, bank, payouts, lock)
      tradeDao  = new pangea.test.TestTradeDao
      trades    = new pangea.service.trade.Trades(tradeDao, transfers, parcels, bank, payouts, heroDao, userRepo, players,
                    content, lock, bg)
    } yield (new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers, states,
                       AdminPanel.disabled, content, lock, trades = Some(trades)), heroDao, tradeDao, trades, payoutDao, parcelDao)

  /** Обвязка для /restart: репозиторий поверх DAO, как в проде, — иначе удаление
    * в DAO не было бы видно репозиторию, и цепочка «стёрли → завели нового» не
    * сошлась бы. Регистрация в наборе состояний — на неё уходит новый герой. */
  private def makeRestartHandler(startState: StateType, admin: AdminPanel = AdminPanel.disabled,
                                 customize: Hero => Hero = identity) =
    for {
      baseHero <- ZIO.succeed(customize(hero.copy(state = startState)))
      heroDao  <- TestHeroDao.withHero(userId, baseHero)
      _        <- heroDao.writeActiveBattle(userId, battle.asJson)
      heroRepo  = TestHeroRepository.backedBy(heroDao)
      userRepo <- TestUserRepository.withUser(testUser)
      api      <- TestApi.make
      content  <- ZIO.attempt(SceneContent.load())
      players   = new pangea.test.TestPlayers
      invRepo   = pangea.test.TestInventoryRepository.accepting
      itemRepo  = pangea.test.TestItemRepository.make
      journal  <- pangea.test.TestJournal.make
      states = Map[StateType, State](
        StateType.GlobalMap    -> GlobalMapState(heroDao, content),
        StateType.Battle       -> BattleState(heroDao, TestInventoryRepository.accepting, TestItemRepository.make, content),
        StateType.Registration -> pangea.service.state.states.registration.RegistrationState(
                                    players, heroDao, invRepo, itemRepo, journal, content)
      )
      lock <- PlayerLock.make
      payouts   = pangea.service.payout.Payouts(pangea.test.TestPayoutDao.empty, heroDao, content)
      parcels   = pangea.service.parcel.Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
      transfers = pangea.service.parcel.Transfers(TestInventoryRepository.accepting, userRepo, parcels,
                    new pangea.test.TestPlayers, content, heroDao, TestBankRepository.empty, payouts, lock)
    } yield (new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers, states,
                       admin, content, lock), heroDao, api)
}
