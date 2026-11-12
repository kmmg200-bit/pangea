package pangea.service.state

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.item.{Item, ItemType, QuestItemKind, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.service.admin.AdminPanel
import pangea.service.sender.vk.VkRenderer
import pangea.service.state.states.LootState.{LootData, MonsterLoot}
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.events.item.FoundItemState
import pangea.service.state.states.events.item.FoundItemState.FoundItemData
import pangea.service.state.states.{GlobalMapState, InventoryState, LootState}
import pangea.test._
import zio.test._
import zio.{IO, Task, ZIO}

/** «🎒 Рюкзак» на экранах добычи и находки — через настоящий диспетчер
  * состояний: в сумку и обратно, на тот же экран, с той же добычей и той же
  * очередью павших (деньги второй раз не начисляются) или с той же находкой
  * (новая вещь не катится). */
object LootBackpackSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val vkId     = VkId("vk_test")
  private val testUser = User(userId, vkId, TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def select(id: Long): UserAction = tap(s"${InventoryState.ItemActionPrefix}$id")

  private def gear(name: String, kind: ItemType = ItemType.Helmet): Item =
    Item(-1L, name, 5L, Rarity.Green, kind, attack = 0, accuracy = 0, energy = 0, armor = 7, defence = 1, evasion = 0)

  private def junk(id: Long): Item = gear(s"Хлам $id", ItemType.Boots).copy(id = id)

  private val onLoot  = TestFixtures.hero(userId, state = StateType.Loot).copy(silver = 100L)
  private val onFound = TestFixtures.hero(userId, state = StateType.FoundItem)

  /** Сумка на `cap` вещей: в полную, как и в проде, ничего не ложится. */
  private final class TightBag(cap: Long, start: List[Item]) extends TestInventoryRepository(canAdd = true, start) {
    override def get(heroId: HeroId): IO[InventoryRepoError, Inventory] =
      super.get(heroId).map(_.copy(maxItems = cap))
    override def addItem(heroId: HeroId, item: Item): IO[InventoryRepoError, Unit] =
      if (!item.weightless && !item.isQuestItem && snapshot.count(i => !i.weightless && !i.isQuestItem) >= cap)
        ZIO.fail(InventoryRepoError.NoMorePlaceForItems)
      else super.addItem(heroId, item)
  }

  /** Диспетчер с добычей, сумкой и городом — куда уходит добыча.
    * `menuFrom` — откуда герой в последний раз открывал «Персонаж» (return_state
    * до добычи). */
  private def make(loot: LootData, inv: InventoryRepository, artifacts: TestArtifactRepository,
                   menuFrom: Option[StateType]) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, onLoot)
      // Состояние героя диспетчер ведёт в репозитории — в тестовом DAO его смена пустая.
      heroRepo <- TestHeroRepository.withHero(userId, onLoot)
      _        <- heroDao.writeSceneData(userId, loot.asJson)
      _        <- heroDao.writeReturnState(userId, menuFrom)
      api      <- TestApi.make
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      itemRepo  = TestItemRepository.make
      owned     = Some(artifacts)
      lootState = LootState(heroDao, inv, itemRepo, journal, content, owned)
      press    <- dispatcher(heroDao, heroRepo, api, content, Map[StateType, State](
                    StateType.Loot      -> lootState,
                    StateType.Inventory -> InventoryState(heroDao, inv, itemRepo, content, owned),
                    StateType.GlobalMap -> GlobalMapState(heroDao, content)))
      // Экран добычи, как его показывает победа: бой отдал Loot, диспетчер позвал enter.
      _        <- lootState.enter(testUser, VkRenderer(api))
    } yield (press, heroDao, heroRepo, api)

  /** Диспетчер с находкой, сумкой и лабиринтом — куда уходит находка. */
  private def makeFound(inv: InventoryRepository) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, onFound)
      heroRepo <- TestHeroRepository.withHero(userId, onFound)
      api      <- TestApi.make
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      sch      <- TestScheduler.make
      itemRepo  = TestItemRepository.make
      found     = FoundItemState(heroDao, inv, itemRepo, journal, content)
      press    <- dispatcher(heroDao, heroRepo, api, content, Map[StateType, State](
                    StateType.FoundItem -> found,
                    StateType.Inventory -> InventoryState(heroDao, inv, itemRepo, content),
                    StateType.Dungeon   -> DungeonState(heroDao, inv, sch, content)))
      // Находка, как её показывает исследование этажа: диспетчер позвал enter.
      _        <- found.enter(testUser, VkRenderer(api))
    } yield (press, heroDao, heroRepo, api)

  /** Настоящий диспетчер над этими состояниями; нажатие — как сообщение игрока. */
  private def dispatcher(heroDao: TestHeroDao, heroRepo: TestHeroRepository, api: TestApi, content: SceneContent,
                         states: Map[StateType, State]) =
    for {
      userRepo <- TestUserRepository.withUser(testUser)
      lock     <- PlayerLock.make
      payouts   = pangea.service.payout.Payouts(TestPayoutDao.empty, heroDao, content)
      parcels   = pangea.service.parcel.Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
      transfers = pangea.service.parcel.Transfers(TestInventoryRepository.accepting, userRepo, parcels,
                    new TestPlayers, content, heroDao, TestBankRepository.empty, payouts, lock)
      handler   = new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels,
                    transfers, states, AdminPanel.disabled, content, lock)
      events   <- zio.Ref.make(0L)
    } yield (ua: UserAction) => events.updateAndGet(_ + 1L).flatMap(handler.makeActionVK(vkId, _, ua))

  private def stateOf(heroRepo: TestHeroRepository): Task[Option[StateType]] = heroRepo.getHero(userId).map(_.map(_.state))
  private def lootOf(heroDao: TestHeroDao): Task[Option[LootData]] =
    heroDao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
  private def foundOf(heroDao: TestHeroDao): Task[Option[Item]] =
    heroDao.readSceneData(userId).map(_.flatMap(_.as[FoundItemData].toOption).map(_.item))
  private def money(heroDao: TestHeroDao): Task[Option[(Long, Long)]] =
    heroDao.getHeroByUserId(userId).map(_.map(h => h.silver -> h.doubloons))
  private def lastText(api: TestApi): Task[String] = api.sentMessages.map(_.lastOption.fold("")(_._1))
  private def lastLabels(api: TestApi): Task[List[String]] =
    api.sentMessages.map(_.lastOption.flatMap(_._2).toList.flatMap(_.buttons.flatten.map(_.action.label)))

  override def spec = suite("«🎒 Рюкзак» с добычи и с находки")(

    test("группа: дважды в сумку и обратно — тот же павший, вещи и очередь целы, серебро и дублоны по разу, дальше по маршруту") {
      val loot = LootData(
        items = List(gear("Шлем орка")), silvers = List(10L),
        monsterName = Some("Орк-первый"),
        returnState = Some(StateType.GlobalMap), eventData = Some(Json.obj("chain" -> 2.asJson)),
        queue = List(
          MonsterLoot("Орк-второй", List(gear("Сапоги орка", ItemType.Boots)), List(5L), 1L),
          MonsterLoot("Орк-третий", Nil, List(7L), 0L)))
      val bag = new TightBag(3L, List(junk(101L), junk(102L), junk(103L)))
      for {
        t <- make(loot, bag, TestArtifactRepository.empty, menuFrom = Some(StateType.Dungeon))
        (press, heroDao, heroRepo, api) = t
        firstLabels <- lastLabels(api)
        opened      <- api.sentMessages.map(_.map(_._1).mkString("\n"))
        // сумка полна — идём освобождать место
        _       <- press(tap("Backpack"))
        inBag   <- stateOf(heroRepo)
        back    <- heroDao.readReturnState(userId)
        waiting <- lootOf(heroDao)
        _       <- press(select(101L)) *> press(tap("Drop"))
        _       <- press(tap("BackFromInventory"))
        onLoot1 <- stateOf(heroRepo)
        screen1 <- lastText(api)
        labels1 <- lastLabels(api)
        money1  <- money(heroDao)
        // шлем лёг на место хлама, на экране — второй павший со своими деньгами
        _       <- press(tap("Take"))
        took1    = bag.snapshot.map(_.name)
        screen2 <- lastText(api)
        money2  <- money(heroDao)
        // второй раз в сумку — уже со второго павшего
        _       <- press(tap("Backpack"))
        _       <- press(select(102L)) *> press(tap("Drop"))
        _       <- press(tap("BackFromInventory"))
        onLoot2 <- stateOf(heroRepo)
        again2  <- lastText(api)
        money3  <- money(heroDao)
        queue2  <- lootOf(heroDao)
        _       <- press(tap("Take"))
        money4  <- money(heroDao)
        after   <- stateOf(heroRepo)
        scene   <- heroDao.readSceneData(userId)
      } yield assertTrue(firstLabels == List("Забрать", "🎒 Рюкзак", "Оставить")) &&
              // третий без вещей — сводкой в самом начале, его серебро сразу в кошельке
              assertTrue(opened.contains("Вы осматриваете тело: Орк-третий") && opened.contains("🪙 Вы нашли 7 серебра")) &&
              assertTrue(inBag.contains(StateType.Inventory) && back.contains(StateType.Loot)) &&
              assertTrue(waiting.exists(l => l.monsterName.contains("Орк-первый") && l.items.size == 1 && l.queue.size == 1)) &&
              // вернулись на тот же экран: тот же павший, те же кнопки, серебро не задвоилось
              assertTrue(onLoot1.contains(StateType.Loot) && screen1.contains("Орк-первый") && screen1.contains("Шлем орка")) &&
              assertTrue(labels1 == firstLabels && money1.contains(117L -> 0L)) &&
              assertTrue(took1 == List("Хлам 102", "Хлам 103", "Шлем орка")) &&
              assertTrue(screen2.contains("Орк-второй") && money2.contains(122L -> 1L)) &&
              assertTrue(onLoot2.contains(StateType.Loot) && again2 == screen2 && money3.contains(122L -> 1L)) &&
              assertTrue(queue2.exists(l => l.monsterName.contains("Орк-второй") && l.queue.isEmpty)) &&
              assertTrue(bag.snapshot.map(_.name) == List("Хлам 103", "Шлем орка", "Сапоги орка")) &&
              assertTrue(money4.contains(122L -> 1L)) &&
              // добыча разобрана — с последним павшим с вещами сразу дальше по её
              // маршруту, с данными события
              assertTrue(after.contains(StateType.GlobalMap) && scene.contains(Json.obj("chain" -> 2.asJson)))
    },

    test("с ларцом — тоже сразу в сумку и из неё обратно к добыче; карта Кельвина из такой сумки в путь не уводит") {
      val map    = QuestItemKind.item(QuestItemKind.KelvinMap).copy(id = 12L)
      val casket = TestArtifactRepository.of(casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1))
      val loot   = LootData(items = List(gear("Шлем")), silvers = List(30L), returnState = Some(StateType.GlobalMap))
      for {
        // последний раз «Персонаж» открывали в городе — карта сочла бы, что герой там
        t <- make(loot, TestInventoryRepository.withItems(List(map)), casket, menuFrom = Some(StateType.GlobalMap))
        (press, heroDao, heroRepo, api) = t
        first     <- lastText(api)
        _         <- press(tap("Backpack"))
        inBag     <- stateOf(heroRepo)
        _         <- press(select(12L)) *> press(tap("UseKelvinMap"))
        refused   <- api.sentMessages.map(_.map(_._1).mkString(" | "))
        stayed    <- stateOf(heroRepo)
        waiting   <- lootOf(heroDao)
        _         <- press(tap("BackFromInventory"))
        onLoot    <- stateOf(heroRepo)
        again     <- lastText(api)
        silver    <- money(heroDao)
      } yield assertTrue(inBag.contains(StateType.Inventory)) &&
              assertTrue(refused.contains("только из города") && stayed.contains(StateType.Inventory)) &&
              assertTrue(waiting.exists(_.items.map(_.name) == List("Шлем"))) &&
              assertTrue(onLoot.contains(StateType.Loot) && again == first && silver.contains(130L -> 0L))
    },

    test("находка, сумка полна: рюкзак → выбросить хлам → назад к той же вещи (новая не катится) → забрать — она в сумке") {
      val bag = new TightBag(3L, List(junk(101L), junk(102L), junk(103L)))
      for {
        t <- makeFound(bag)
        (press, heroDao, heroRepo, api) = t
        first   <- lastText(api)
        labels  <- lastLabels(api)
        item    <- foundOf(heroDao)
        _       <- press(tap("FoundBackpack"))
        inBag   <- stateOf(heroRepo)
        back    <- heroDao.readReturnState(userId)
        _       <- press(select(101L)) *> press(tap("Drop"))
        _       <- press(tap("BackFromInventory"))
        onFound <- stateOf(heroRepo)
        again   <- lastText(api)
        same    <- foundOf(heroDao)
        _       <- press(tap("TakeItem"))
        after   <- stateOf(heroRepo)
      } yield assertTrue(labels.takeRight(2) == List("🎒 Рюкзак", "Оставить")) &&
              assertTrue(inBag.contains(StateType.Inventory) && back.contains(StateType.FoundItem)) &&
              assertTrue(onFound.contains(StateType.FoundItem) && again == first) &&
              assertTrue(item.isDefined && same.map(_.id) == item.map(_.id)) &&
              assertTrue(item.exists(i => bag.snapshot.map(_.id) == List(102L, 103L, i.id))) &&
              assertTrue(after.contains(StateType.Dungeon))
    }
  )
}
