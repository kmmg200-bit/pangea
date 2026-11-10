package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests, QuestOutcome}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.parcel.Parcels
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.bank.TradeHouseState
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryListQuest, StoryPowderQuest, UserAction}
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Основная линия, глава 4 «Список»: страница ведёт к Рахадиму (с 17-го
  * уровня), к Сойке, к засаде у Мельничного моста и снова к Сойке —
  * простить или порвать; ключ от архива канцелярии — зацепка главы 5. */
object StoryListSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 17L): Hero = TestFixtures.hero(userId).copy(lvl = lvl, silver = 100L)

  /** Главы 1–3 закрыты. */
  private val afterBrand: NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    StoryPowderQuest.finish(base, WardenFate.Released).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
  }

  /** Глава 4 на шаге `step`. */
  private def list(step: Int): NpcQuests = StoryListQuest.step(afterBrand.take(NpcQuest.StoryList, 0L), step)

  private val ready: LoreData = LoreData(firstSteps = true, firstStepsDone = true, cluePasses = 5)

  private def dao(h: Hero, q: NpcQuests) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- d.writeLoreData(userId, ready.asJson)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def pageItem: Item = QuestItemKind.item(QuestItemKind.StoryPage).copy(id = 903L)

  private def openCard(state: InventoryState, item: Item, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${item.id}"}""")), r)

  private def tradeHouse(d: TestHeroDao, items: List[Item], c: SceneContent) =
    for {
      userRepo <- TestUserRepository.withUser(testUser)
      bankRepo  = TestBankRepository.of(0)
      parcels   = Parcels(TestParcelDao.empty, bankRepo, c)
    } yield TradeHouseState(d, userRepo, bankRepo, parcels, TestInventoryRepository.withItems(items), TestDonations.off, c)

  override def spec = suite("Основная линия: «Список»")(

    test("карточка страницы: сразу после главы 3 — к Рахадиму, на любом уровне; по шагам — к Сойке, к мосту, снова к Сойке") {
      def card(lvl: Long, q: NpcQuests) =
        for {
          d     <- dao(hero(lvl), q)
          _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(List(pageItem)), TestItemRepository.make, c)
          _     <- openCard(state, pageItem, r)
          shown <- r.sentScreens.map(_.last)
        } yield shown
      for {
        low    <- card(1L, afterBrand)
        open   <- card(17L, afterBrand)
        tell   <- card(17L, list(2))
        bridge <- card(17L, list(3))
        after  <- card(17L, list(4))
      } yield assertTrue(low.text.contains("Рахадим") && open.text.contains("Рахадим") && !low.text.contains("окрепнуть")) &&
              assertTrue(low.text.contains(Difficulty.render(StoryListQuest.Level))) &&
              assertTrue(tell.choices.exists(_.id == "StorySojkaGo") && bridge.choices.exists(_.id == "StoryBridgeGo") &&
                after.choices.exists(_.id == "StorySojkaGo") && after.text.contains("птица"))
    },

    test("Рахадим: кнопка — со страницей при герое, на любом уровне; бумага канцелярии совета, глава берётся и сразу ведёт к Сойке") {
      for {
        c     <- content
        young <- dao(hero(16L), afterBrand)
        st0   <- tradeHouse(young, List(pageItem), c)
        r0    <- TestRenderer.make
        _     <- st0.enter(testUser, r0)
        none  <- r0.sentScreens.map(_.last)
        d     <- dao(hero(), afterBrand)
        st    <- tradeHouse(d, List(pageItem), c)
        r     <- TestRenderer.make
        _     <- st.enter(testUser, r)
        menu  <- r.sentScreens.map(_.last)
        _     <- st.action(testUser, tap("RakStory"), r)
        said  <- texts(r)
        q     <- questOf(d)
      } yield assertTrue(none.choices.exists(_.id == "RakStory") && menu.choices.exists(_.id == "RakStory")) &&
              assertTrue(said.contains("канцелярии совета") && said.contains("📜 Новое задание: «Список»")) &&
              assertTrue(StoryListQuest.toSojka(q))
    },

    test("Сойка: с карточки, из города; «Договорились» — к мосту; из лабиринта — отказ") {
      for {
        d     <- dao(hero(), list(2))
        _     <- d.writeReturnState(userId, Some(StateType.Dungeon))
        r     <- TestRenderer.make
        c     <- content
        card   = InventoryState(d, TestInventoryRepository.withItems(List(pageItem)), TestItemRepository.make, c)
        _     <- openCard(card, pageItem, r)
        deep  <- card.action(testUser, tap("StorySojkaGo"), r)
        _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
        go    <- card.action(testUser, tap("StorySojkaGo"), r)
        state  = StorySojkaState(d, TestInventoryRepository.withItems(List(pageItem)), TestItemRepository.make, c)
        _     <- state.enter(testUser, r)
        talk  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("SojkaAgree"), r)
        q     <- questOf(d)
      } yield assertTrue(deep == StateType.Inventory && go == StateType.StorySojka) &&
              assertTrue(talk.text.contains("Мельничного моста") && talk.choices.map(_.id) == List("SojkaAgree")) &&
              assertTrue(StoryListQuest.toBridge(q))
    },

    test("мост: засада, сложность; бой — главарь чистильщиков и трое людей уровня задания, без бегства; после — приказ «птица»") {
      for {
        d      <- dao(hero(20L), list(3))
        r      <- TestRenderer.make
        c      <- content
        state   = StoryBridgeState(d, c)
        _      <- state.enter(testUser, r)
        ambush <- r.sentScreens.map(_.last)
        _      <- TestRandom.feedLongs(50L, 50L, 50L, 50L)
        res    <- state.action(testUser, tap("BridgeFight"), r)
        battle <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot   <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        _      <- d.writeSceneData(userId, StoryBridgeState.WonMark.asJson)
        r2     <- TestRenderer.make
        _      <- state.enter(testUser, r2)
        order  <- texts(r2)
        q      <- questOf(d)
      } yield assertTrue(ambush.text.contains("Чистильщики Братства") && ambush.text.contains(Difficulty.render(StoryListQuest.Level))) &&
              assertTrue(ambush.choices.map(_.id) == List("BridgeFight", "BridgeLeave") && res == StateType.Battle) &&
              assertTrue(battle.exists(b => b.story.contains(StoryListQuest.FightStory) && b.cantFlee &&
                b.monsterName.contains("Чистильщик") && b.monsterRarity == Rarity.Rare.entryName &&
                b.monsterLvl == StoryListQuest.Level.toLong && b.group.others.size == 3)) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.StoryBridge)) && BattleState.RaidStories.contains(StoryListQuest.FightStory)) &&
              assertTrue(order.contains("приведёт птица") && StoryListQuest.toConfront(q))
    },

    test("Сойка после засады: простить или порвать — страница уходит, ключ от архива в сумке, награда, исход записан") {
      def decide(fate: SojkaFate, action: String) =
        for {
          d    <- dao(hero(), list(4))
          inv   = TestInventoryRepository.withItems(List(pageItem))
          o    <- TestQuestOutcomeDao.make
          r    <- TestRenderer.make
          c    <- content
          st    = StorySojkaState(d, inv, TestItemRepository.make, c, Some(o))
          _    <- st.enter(testUser, r)
          ask  <- r.sentScreens.map(_.last)
          _    <- st.action(testUser, tap(action), r)
          said <- texts(r)
          q    <- questOf(d)
          h    <- heroOf(d)
          mine <- o.of(h.id, NpcQuest.StoryList)
        } yield (ask, said, q, h, inv.snapshot, mine, fate)
      for {
        forgave <- decide(SojkaFate.Forgave, "SojkaForgive")
        broke   <- decide(SojkaFate.Broke, "SojkaBreak")
      } yield assertTrue(forgave._1.choices.map(_.id) == List("SojkaForgive", "SojkaBreak") && forgave._1.text.contains("Ты всю жизнь так делал")) &&
              assertTrue(forgave._2.contains("Я тебе должна") && broke._2.contains("Как знаешь")) &&
              assertTrue(List(forgave, broke).forall { case (_, said, q, h, bag, mine, fate) =>
                said.contains("Архив. Не выносить") && said.contains("Квест «Список» завершён") &&
                  said.contains("Вы — среди первых, кто закончил «Список»") &&
                  q.isDone(NpcQuest.StoryList) && StoryListQuest.choice(q).contains(fate) &&
                  bag.exists(_.questItem.contains(QuestItemKind.StoryArchiveKey)) && !bag.exists(_.questItem.contains(QuestItemKind.StoryPage)) &&
                  h.silver == 100L + StoryListQuest.silver && h.exp > 0L && mine.contains(fate.outcome)
              })
    },

    test("десятый, кто решил с Сойкой, видит проценты; смерть у моста — проснуться в городе; журнал — глава сюжетным заданием") {
      val mob = Monster(0L, 17L, Race.Human, Rarity.Uncommon,
        FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      for {
        d     <- dao(hero(), list(4))
        o     <- TestQuestOutcomeDao.make
        _     <- o.seed(QuestOutcome.ListForgave -> 3, QuestOutcome.ListBroke -> 6)
        r     <- TestRenderer.make
        c     <- content
        _     <- StorySojkaState(d, TestInventoryRepository.withItems(List(pageItem)), TestItemRepository.make, c, Some(o))
                   .action(testUser, tap("SojkaForgive"), r)
        said  <- texts(r)
        dead  <- dao(hero().copy(exp = 50L), list(3))
        _     <- dead.writeActiveBattle(userId, SoloPveBattle.from(mob, hero()).copy(story = Some(StoryListQuest.FightStory)).asJson)
        r2    <- TestRenderer.make
        _     <- TestRandom.feedInts(1)
        _     <- DeathState(dead, TestInventoryRepository.withItems(Nil), c).enter(testUser, r2)
        scene <- dead.readSceneData(userId)
        text   = QuestJournal.render(c, hero(), ready, list(3), DailyQuests.empty, None, 0L)
      } yield assertTrue(said.contains(List(
                "📊 Как закончили «Список» герои Пангеи:",
                "• 60% порвали с Сойкой",
                "• 40% простили Сойку — как и вы").mkString("\n"))) &&
              assertTrue(scene.exists(_.hcursor.get[String]("wakeTo").toOption.contains("GlobalMap"))) &&
              assertTrue(scene.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("story.storyList.bridge.death")))) &&
              assertTrue(text.contains("📌 «Список»") && text.contains("Мельничному мосту"))
    }
  )
}
