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
import pangea.service.state.states.artifact.BackpackState
import pangea.service.state.states.{GlobalMapState, InventoryState, LootState}
import pangea.test._
import zio.test._
import zio.{IO, Task, ZIO}

/** «🎒 Рюкзак» на экране добычи — через настоящий диспетчер состояний: из
  * добычи в сумку и обратно, на тот же экран, с той же добычей и той же
  * очередью павших; деньги второй раз не начисляются. */
object LootBackpackSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val vkId     = VkId("vk_test")
  private val testUser = User(userId, vkId, TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def select(id: Long): UserAction = tap(s"${InventoryState.ItemActionPrefix}$id")

  private def gear(name: String, kind: ItemType = ItemType.Helmet): Item =
    Item(-1L, name, 5L, Rarity.Green, kind, attack = 0, accuracy = 0, energy = 0, armor = 7, defence = 1, evasion = 0)

  private def junk(id: Long): Item = gear(s"Хлам $id", ItemType.Boots).copy(id = id)

  private val onLoot = TestFixtures.hero(userId, state = StateType.Loot).copy(silver = 100L)

  /** Сумка на `cap` вещей: в полную, как и в проде, ничего не ложится. */
  private final class TightBag(cap: Long, start: List[Item]) extends TestInventoryRepository(canAdd = true, start) {
    override def get(heroId: HeroId): IO[InventoryRepoError, Inventory] =
      super.get(heroId).map(_.copy(maxItems = cap))
    override def addItem(heroId: HeroId, item: Item): IO[InventoryRepoError, Unit] =
      if (!item.weightless && !item.isQuestItem && snapshot.count(i => !i.weightless && !i.isQuestItem) >= cap)
        ZIO.fail(InventoryRepoError.NoMorePlaceForItems)
      else super.addItem(heroId, item)
  }

  /** Диспетчер с добычей, сумкой, рюкзаком и городом — куда уходит добыча.
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
      userRepo <- TestUserRepository.withUser(testUser)
      api      <- TestApi.make
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      itemRepo  = TestItemRepository.make
      owned     = Some(artifacts)
      lootState = LootState(heroDao, inv, itemRepo, journal, content, owned)
      states    = Map[StateType, State](
                    StateType.Loot      -> lootState,
                    StateType.Inventory -> InventoryState(heroDao, inv, itemRepo, content, owned),
                    StateType.Backpack  -> BackpackState(heroDao, inv, artifacts, content),
                    StateType.GlobalMap -> GlobalMapState(heroDao, content))
      lock     <- PlayerLock.make
      payouts   = pangea.service.payout.Payouts(TestPayoutDao.empty, heroDao, content)
      parcels   = pangea.service.parcel.Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
      transfers = pangea.service.parcel.Transfers(TestInventoryRepository.accepting, userRepo, parcels,
                    new TestPlayers, content, heroDao, TestBankRepository.empty, payouts, lock)
      handler   = new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels,
                    transfers, states, AdminPanel.disabled, content, lock)
      // Экран добычи, как его показывает победа: бой отдал Loot, диспетчер позвал enter.
      _        <- lootState.enter(testUser, VkRenderer(api))
      events   <- zio.Ref.make(0L)
      press     = (ua: UserAction) => events.updateAndGet(_ + 1L).flatMap(handler.makeActionVK(vkId, _, ua))
    } yield (press, heroDao, heroRepo, api)

  private def stateOf(heroRepo: TestHeroRepository): Task[Option[StateType]] = heroRepo.getHero(userId).map(_.map(_.state))
  private def lootOf(heroDao: TestHeroDao): Task[Option[LootData]] =
    heroDao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
  private def money(heroDao: TestHeroDao): Task[Option[(Long, Long)]] =
    heroDao.getHeroByUserId(userId).map(_.map(h => h.silver -> h.doubloons))
  private def lastText(api: TestApi): Task[String] = api.sentMessages.map(_.lastOption.fold("")(_._1))
  private def lastLabels(api: TestApi): Task[List[String]] =
    api.sentMessages.map(_.lastOption.flatMap(_._2).toList.flatMap(_.buttons.flatten.map(_.action.label)))

  override def spec = suite("Добыча: «🎒 Рюкзак»")(

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
        screen3 <- lastText(api)
        money4  <- money(heroDao)
        _       <- press(tap("Continue"))
        after   <- stateOf(heroRepo)
        scene   <- heroDao.readSceneData(userId)
      } yield assertTrue(firstLabels == List("Забрать", "🎒 Рюкзак", "Оставить")) &&
              assertTrue(inBag.contains(StateType.Inventory) && back.contains(StateType.Loot)) &&
              assertTrue(waiting.exists(l => l.monsterName.contains("Орк-первый") && l.items.size == 1 && l.queue.size == 2)) &&
              // вернулись на тот же экран: тот же павший, те же кнопки, серебро не задвоилось
              assertTrue(onLoot1.contains(StateType.Loot) && screen1.contains("Орк-первый") && screen1.contains("Шлем орка")) &&
              assertTrue(labels1 == firstLabels && money1.contains(110L -> 0L)) &&
              assertTrue(took1 == List("Хлам 102", "Хлам 103", "Шлем орка")) &&
              assertTrue(screen2.contains("Орк-второй") && money2.contains(115L -> 1L)) &&
              assertTrue(onLoot2.contains(StateType.Loot) && again2 == screen2 && money3.contains(115L -> 1L)) &&
              assertTrue(queue2.exists(l => l.monsterName.contains("Орк-второй") && l.queue.map(_.monsterName) == List("Орк-третий"))) &&
              assertTrue(bag.snapshot.map(_.name) == List("Хлам 103", "Шлем орка", "Сапоги орка")) &&
              assertTrue(screen3.contains("Орк-третий") && money4.contains(122L -> 1L)) &&
              // добыча разобрана — дальше по её маршруту, с данными события
              assertTrue(after.contains(StateType.GlobalMap) && scene.contains(Json.obj("chain" -> 2.asJson)))
    },

    test("с ларцом — через рюкзак: сумка → рюкзак → добыча; карта Кельвина из такой сумки в путь не уводит") {
      val map    = QuestItemKind.item(QuestItemKind.KelvinMap).copy(id = 12L)
      val casket = TestArtifactRepository.of(casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1))
      val loot   = LootData(items = List(gear("Шлем")), silvers = List(30L), returnState = Some(StateType.GlobalMap))
      for {
        // последний раз «Персонаж» открывали в городе — карта сочла бы, что герой там
        t <- make(loot, TestInventoryRepository.withItems(List(map)), casket, menuFrom = Some(StateType.GlobalMap))
        (press, heroDao, heroRepo, api) = t
        first     <- lastText(api)
        _         <- press(tap("Backpack"))
        inPack    <- stateOf(heroRepo)
        _         <- press(tap("OpenBag"))
        _         <- press(select(12L)) *> press(tap("UseKelvinMap"))
        refused   <- api.sentMessages.map(_.map(_._1).mkString(" | "))
        stayed    <- stateOf(heroRepo)
        waiting   <- lootOf(heroDao)
        _         <- press(tap("BackFromInventory"))
        packAgain <- stateOf(heroRepo)
        _         <- press(tap("BackFromBackpack"))
        onLoot    <- stateOf(heroRepo)
        again     <- lastText(api)
        silver    <- money(heroDao)
      } yield assertTrue(inPack.contains(StateType.Backpack)) &&
              assertTrue(refused.contains("только из города") && stayed.contains(StateType.Inventory)) &&
              assertTrue(waiting.exists(_.items.map(_.name) == List("Шлем"))) &&
              assertTrue(packAgain.contains(StateType.Backpack)) &&
              assertTrue(onLoot.contains(StateType.Loot) && again == first && silver.contains(130L -> 0L))
    }
  )
}
