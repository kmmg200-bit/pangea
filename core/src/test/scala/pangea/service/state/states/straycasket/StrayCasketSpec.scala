package pangea.service.state.states.straycasket

import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.GemGenerator
import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Gem, GemKind, Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.artifact.FetShopState
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.{DeathState, InventoryState, LootState}
import pangea.service.state.{NpcQuestLog, QuestGoods, QuestJournal, RareQuests, StrayCasketQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Бродячий ларец»: редкая встреча с 5-го уровня лабиринта, кормление с
  * карточки в сумке и воспоминания, бой с хозяином и повтор после смерти,
  * развязка у Фета — новый ларец или ступень к своему, потолок 5 и съеденные
  * камни обратно. */
object StrayCasketSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def hero(dungeonLevel: Int = 5): Hero = TestFixtures.hero(userId, dungeonLevel = dungeonLevel)

  private def gem(id: Long, kind: GemKind, grade: Int): Item = GemGenerator.item(kind, grade).copy(id = id)

  private def casketItem: Item = QuestItemKind.item(QuestItemKind.StrayCasket).copy(id = 900L)

  /** Задание на шаге `step` со скормленным `fed` и съеденными камнями `eaten`. */
  private def quest(step: Int, fed: Long = 0L, eaten: List[Gem] = Nil): NpcQuests =
    NpcQuests.empty.take(NpcQuest.StrayCasket, 0L)
      .update(NpcQuest.StrayCasket)(_.copy(step = step, counter = fed)).copy(strayGems = eaten)

  private def dao(h: Hero, q: Option[NpcQuests] = None) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def stray(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent,
                    arts: TestArtifactRepository = TestArtifactRepository.empty) =
    StrayCasketState(d, inv, TestItemRepository.make, c, Some(arts))

  /** Открыть карточку бродячего ларца в сумке. */
  private def openCard(state: InventoryState, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${casketItem.id}"}""")), r)

  override def spec = suite("Бродячий ларец")(

    test("бросок: с 5-го уровня лабиринта, полпроцента, из тех двух заданий, которых у героя ещё не было") {
      val strayTaken = quest(1)
      val bothDone   = NpcQuests.empty.take(NpcQuest.StrayCasket, 0L).finish(NpcQuest.StrayCasket)
                         .take(NpcQuest.HedgeSong, 0L).finish(NpcQuest.HedgeSong)
      for {
        _    <- TestRandom.feedDoubles(0.0049, 0.0049, 0.005, 0.9)
        // открыты оба — второе по счёту; открыто одно — выбирать не из чего
        _    <- TestRandom.feedInts(1)
        both <- RareQuests.roll(NpcQuests.empty, 5)
        one  <- RareQuests.roll(strayTaken, 5)
        edge <- RareQuests.roll(NpcQuests.empty, 5)
        miss <- RareQuests.roll(NpcQuests.empty, 150)
      } yield assertTrue(RareQuests.open(NpcQuests.empty, 4).isEmpty) &&
              assertTrue(RareQuests.open(NpcQuests.empty, 5) == List(StateType.StrayCasket, StateType.HedgeSong)) &&
              assertTrue(RareQuests.open(strayTaken, 10) == List(StateType.HedgeSong) && RareQuests.open(bothDone, 10).isEmpty) &&
              assertTrue(both.contains(StateType.HedgeSong) && one.contains(StateType.HedgeSong)) &&
              assertTrue(edge.isEmpty && miss.isEmpty) &&
              assertTrue(RareQuests.Chance == 0.005 && RareQuests.MinDungeonLevel == 5)
    },

    test("осмотр уровня: выпал бросок — встреча с ларцом или эльфом; на 4-м бросок не идёт, событие — из пула") {
      def explore(lvl: Int, q: Option[NpcQuests] = None) =
        for {
          d   <- dao(hero(lvl), q)
          _   <- d.writeSceneData(userId, Json.obj("hedgeSongCleared" -> Json.fromInt(2)))
          r   <- TestRenderer.make
          sch <- TestScheduler.make
          c   <- content
          res <- DungeonState(d, TestInventoryRepository.withItems(Nil), sch, c).action(testUser, tap("FindEvent"), r)
          sd  <- d.readSceneData(userId)
        } yield (res, sd)
      // билет 50 — находка и на 4-м, и на 5-м, и на 10-м уровне
      val found     = List(4, 5, 10).map(StateType.eventsAt(_, 1.0)(50))
      val bothTaken = quest(1).take(NpcQuest.HedgeSong, 0L)
      for {
        // билет пула на каждый осмотр, а за выпавшим броском — какое из двух
        _        <- TestRandom.feedInts(50, 0, 50, 50, 1, 50, 50)
        _        <- TestRandom.feedDoubles(0.001, 0.001, 0.9)
        met      <- explore(5)
        // на 4-м бросок не идёт: следующая выпавшая встреча — снова на 5-м
        shallow  <- explore(4)
        metElf   <- explore(5)
        missed   <- explore(5)
        // оба задания уже взяты — бросок не идёт вовсе
        again    <- explore(10, Some(bothTaken))
      } yield assertTrue(found.forall(_ == StateType.FoundItem)) &&
              assertTrue(met._1 == StateType.StrayCasket && metElf._1 == StateType.HedgeSong) &&
              // встреча начинается с чистого листа: старые волны эльфа из scene_data стёрты
              assertTrue(met._2.forall(_.isNull) && metElf._2.forall(_.isNull)) &&
              assertTrue(List(shallow, missed, again).forall(_._1 == StateType.FoundItem))
    },

    test("сытый ларец сам не ведёт: осмотр — обычное событие; «Идти за ларцом» с карточки — к хозяину, только в лабиринте") {
      for {
        d     <- dao(hero(1), Some(quest(2, fed = 10L)))
        r     <- TestRenderer.make
        sch   <- TestScheduler.make
        c     <- content
        _     <- TestRandom.feedInts(50)
        res   <- DungeonState(d, TestInventoryRepository.withItems(Nil), sch, c).action(testUser, tap("FindEvent"), r)
        inv    = TestInventoryRepository.withItems(List(casketItem))
        state  = InventoryState(d, inv, TestItemRepository.make, c, Some(TestArtifactRepository.empty))
        _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
        _     <- openCard(state, r)
        card  <- r.sentScreens.map(_.last)
        city  <- state.action(testUser, tap("StrayGo"), r)
        said  <- texts(r)
        _     <- d.writeReturnState(userId, Some(StateType.Dungeon))
        went  <- state.action(testUser, tap("StrayGo"), r)
      } yield assertTrue(res == StateType.FoundItem) &&
              assertTrue(card.choices.exists(_.id == "StrayGo") && !card.choices.exists(_.id == "StrayFeed")) &&
              assertTrue(city == StateType.Inventory && said.contains("отпустить его можно только в лабиринте")) &&
              assertTrue(went == StateType.StrayCasket)
    },

    test("встреча: ларец щёлкает зубами на камни героя, сложность — сразу, «Протянуть камень» или «Уйти»") {
      for {
        d      <- dao(hero())
        r      <- TestRenderer.make
        c      <- content
        _      <- stray(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(screen.text.contains("щёлкает зубами, чуя камни у вас при себе")) &&
              assertTrue(screen.text.contains(Difficulty.render(7)) && Difficulty.render(7) == "сложность 7 ур.") &&
              assertTrue(screen.choices.map(_.id) == List("StrayOffer", "StrayShoo"))
    },

    test("«Протянуть камень»: ларец съедает самый простой камень и ложится в сумку; задание на первом шаге") {
      val bag = List(gem(1001L, GemKind.Ruby, 3), gem(1002L, GemKind.Topaz, 1), gem(1003L, GemKind.Skull, 2))
      for {
        d   <- dao(hero())
        inv  = TestInventoryRepository.withItems(bag)
        r   <- TestRenderer.make
        c   <- content
        out <- stray(d, inv, c).action(testUser, tap("StrayOffer"), r)
        q   <- questOf(d)
        said <- texts(r)
      } yield assertTrue(out == StateType.Dungeon) &&
              assertTrue(StrayCasketQuest.hungry(q) && StrayCasketQuest.fed(q) == 1L) &&
              assertTrue(q.strayGems == List(Gem(GemKind.Topaz, 1))) &&
              // надколотый топаз съеден, прочие при герое; ларец — в сумке
              assertTrue(!inv.snapshot.exists(_.id == 1002L) && inv.snapshot.exists(_.id == 1001L)) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.StrayCasket))) &&
              assertTrue(said.contains("Надколотый топаз") && said.contains("«Бродячий ларец»") && said.contains(": 9."))
    },

    test("камней нет — ларец всё равно увязывается следом; «Уйти» — задание не начато") {
      for {
        d1   <- dao(hero())
        inv1  = TestInventoryRepository.withItems(Nil)
        r    <- TestRenderer.make
        c    <- content
        _    <- stray(d1, inv1, c).action(testUser, tap("StrayOffer"), r)
        q1   <- questOf(d1)
        said <- texts(r)
        d2   <- dao(hero())
        out  <- stray(d2, TestInventoryRepository.withItems(Nil), c).action(testUser, tap("StrayShoo"), r)
        q2   <- questOf(d2)
      } yield assertTrue(StrayCasketQuest.hungry(q1) && StrayCasketQuest.fed(q1) == 0L && q1.strayGems.isEmpty) &&
              assertTrue(inv1.snapshot.exists(_.questItem.contains(QuestItemKind.StrayCasket))) &&
              assertTrue(said.contains("Камня при вас не нашлось")) &&
              assertTrue(out == StateType.Dungeon && StrayCasketQuest.fresh(q2))
    },

    test("какой камень съест: того вида, которого больше, и сперва из сумки; качества — по порядку") {
      val goods = List(
        QuestGoods.Good(gem(1L, GemKind.Ruby, 1), Some(ArtifactKind.Casket)),
        QuestGoods.Good(gem(2L, GemKind.Ruby, 1), None),
        QuestGoods.Good(gem(3L, GemKind.Emerald, 1), None),
        QuestGoods.Good(gem(4L, GemKind.Sapphire, 4), None),
        QuestGoods.Good(casketItem, None))
      assertTrue(StrayCasketQuest.pick(goods, 1).map(_.item.id).contains(2L)) &&
      assertTrue(StrayCasketQuest.pick(goods, 4).map(_.item.id).contains(4L)) &&
      assertTrue(StrayCasketQuest.pick(goods, 3).isEmpty) &&
      assertTrue(StrayCasketQuest.grades(goods) == List(1 -> 3, 4 -> 1)) &&
      assertTrue(StrayCasketQuest.cheapest(goods).map(_.item.id).contains(2L)) &&
      assertTrue((1L to 10L).toList.map(StrayCasketQuest.memoryAfter) ==
        List(None, Some(1), None, Some(2), None, Some(3), None, Some(4), None, Some(5)))
    },

    test("карточка в сумке: сложность, сколько съедено и кнопки по качеству камней — из сумки и из ларца") {
      val arts = TestArtifactRepository.of(casket = Artifact(ArtifactKind.Casket, 1, 0,
                   Items(List(gem(1101L, GemKind.Amethyst, 2)))))
      for {
        d      <- dao(hero(), Some(quest(1, fed = 3L)))
        inv     = TestInventoryRepository.withItems(List(casketItem, gem(1102L, GemKind.Ruby, 1), gem(1103L, GemKind.Ruby, 1)))
        r      <- TestRenderer.make
        c      <- content
        state   = InventoryState(d, inv, TestItemRepository.make, c, Some(arts))
        _      <- openCard(state, r)
        card   <- r.sentScreens.map(_.last)
      } yield assertTrue(card.text.contains(Difficulty.render(7)) && card.text.contains("Скормлено камней: 3/10")) &&
              assertTrue(card.choices.filter(_.id == "StrayFeed").map(_.label) == List("Надколотый (2)", "Повреждённый (1)")) &&
              assertTrue(card.choices.exists(_.id == "InventoryList"))
    },

    test("кормление: каждый второй камень — воспоминание, десятый — ларец сыт и помнит дорогу") {
      val rubies = (1 to 3).map(i => gem(1200L + i, GemKind.Ruby, 1)).toList
      val arts   = TestArtifactRepository.of(casket = Artifact(ArtifactKind.Casket, 1, 0,
                     Items(List(gem(1300L, GemKind.Skull, 2)))))
      for {
        d      <- dao(hero(), Some(quest(1, fed = 7L)))
        inv     = TestInventoryRepository.withItems(casketItem :: rubies)
        r      <- TestRenderer.make
        c      <- content
        state   = InventoryState(d, inv, TestItemRepository.make, c, Some(arts))
        _      <- openCard(state, r)
        _      <- state.action(testUser, tap("StrayFeed", "grade" -> "1"), r)
        eighth <- texts(r)
        _      <- state.action(testUser, tap("StrayFeed", "grade" -> "1"), r)
        // десятый — из ларца: повреждённый череп лежит только там
        _      <- state.action(testUser, tap("StrayFeed", "grade" -> "2"), r)
        all    <- texts(r)
        card   <- r.sentScreens.map(_.last)
        q      <- questOf(d)
        // сытому больше не скормить: старая кнопка камень не трогает
        _      <- state.action(testUser, tap("StrayFeed", "grade" -> "1"), r)
      } yield assertTrue(eighth.contains("Скормлено: 8/10") && eighth.contains("Тот же костёр, но у огня один Ансельм") == false) &&
              assertTrue(eighth.contains("так далеко к тьме") == false && eighth.contains("Из-за поворота выходит что-то большое")) &&
              assertTrue(all.contains("Последняя вспышка") && all.contains("Ларец сыт")) &&
              assertTrue(StrayCasketQuest.leads(q) && StrayCasketQuest.fed(q) == 10L) &&
              assertTrue(q.strayGems.takeRight(3) == List(Gem(GemKind.Ruby, 1), Gem(GemKind.Ruby, 1), Gem(GemKind.Skull, 2))) &&
              assertTrue(arts.snapshot.casket.items.data.isEmpty) &&
              assertTrue(inv.snapshot.count(_.gem.isDefined) == 1) &&
              assertTrue(card.text.contains("он помнит дорогу") && !card.choices.exists(_.id == "StrayFeed"))
    },

    test("хозяин ларца: бой один на один с Ансельмом Вейлом — поднятый человек-именной 7-го уровня, без «Сбежать»") {
      for {
        d      <- dao(hero().copy(lvl = 40L), Some(quest(2, fed = 10L)))
        r      <- TestRenderer.make
        c      <- content
        state   = stray(d, TestInventoryRepository.withItems(List(casketItem)), c)
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
        out    <- state.action(testUser, tap("StrayFight"), r)
        b      <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot   <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(screen.text.contains("Это Ансельм") && screen.choices.map(_.id) == List("StrayFight", "StrayLeave")) &&
              assertTrue(out == StateType.Battle) &&
              assertTrue(b.exists(x => x.story.contains(StrayCasketQuest.FightStory) && x.cantFlee)) &&
              assertTrue(b.exists(x => x.monsterLvl == 7L && x.monsterRace == Race.Human.entryName &&
                x.monsterRarity == Rarity.Legendary.entryName && x.monsterUndead && x.monsterName == "Ансельм Вейл")) &&
              assertTrue(b.exists(_.group.allies.isEmpty)) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.StrayCasket)))
    },

    test("настоящий бой: победа без добычи по таблицам, «Продолжить» — назад к ларцу, хозяин упокоен") {
      // хозяин в один удар — проверяется дорога после боя, а не сам бой
      val base = SoloPveBattle.from(StrayCasketQuest.owner, hero(), squad = false)
      val weak = base.copy(
        monsterStats = base.monsterStats.copy(hp = 1L, armor = 0L, defence = 0L, evasion = 0L, atk = 1L),
        monsterCurrentHp = 1L, monsterCurrentArmor = 0L, story = Some(StrayCasketQuest.FightStory))
      val strong = hero().copy(fightStats = FightStats(atk = 500, hp = 2000, armor = 0, defence = 0,
        evasion = 9999, accuracy = 9999, energy = 0))
      for {
        d     <- dao(strong, Some(quest(2, fed = 10L)))
        _     <- d.writeActiveBattle(userId, weak.asJson)
        _     <- d.writeSceneData(userId, LootData(Nil, Nil, returnState = Some(StateType.StrayCasket),
                   eventData = Some(StrayCasketState.Progress(StrayCasketState.Step.AfterFight).asJson)).asJson)
        inv    = TestInventoryRepository.withItems(List(casketItem))
        r     <- TestRenderer.make
        c     <- content
        won   <- BattleState(d, inv, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        j     <- TestJournal.make
        back  <- LootState(d, inv, TestItemRepository.make, j, c).action(testUser, tap("Continue"), r)
        _     <- stray(d, inv, c).enter(testUser, r)
        rest  <- r.sentScreens.map(_.last)
        q     <- questOf(d)
      } yield assertTrue(won == StateType.Loot) &&
              assertTrue(loot.exists(l => l.items.isEmpty && l.silvers.isEmpty && l.returnState.contains(StateType.StrayCasket))) &&
              assertTrue(back == StateType.StrayCasket) &&
              assertTrue(rest.text.contains("Ансельм оседает") && StrayCasketQuest.toFet(q))
    },

    test("после победы: Ансельм упокоен, ларец нести Фету; погиб — проснуться в лабиринте, задание ждёт на том же шаге") {
      for {
        d1    <- dao(hero(), Some(quest(2, fed = 10L)))
        _     <- d1.writeSceneData(userId, StrayCasketState.Progress(StrayCasketState.Step.AfterFight).asJson)
        r     <- TestRenderer.make
        c     <- content
        _     <- stray(d1, TestInventoryRepository.withItems(List(casketItem)), c).enter(testUser, r)
        rest  <- r.sentScreens.map(_.last)
        q1    <- questOf(d1)
        d2    <- dao(hero(), Some(quest(2, fed = 10L)))
        battle = SoloPveBattle.from(StrayCasketQuest.owner, hero(), squad = false).copy(story = Some(StrayCasketQuest.FightStory))
        _     <- d2.writeActiveBattle(userId, battle.asJson)
        _     <- DeathState(d2, TestInventoryRepository.withItems(List(casketItem)), c).enter(testUser, r)
        wake  <- d2.readSceneData(userId)
        q2    <- questOf(d2)
      } yield assertTrue(rest.text.contains("«Ансельму — от Ф.»") && rest.choices.map(_.id) == List("StrayLeave")) &&
              assertTrue(StrayCasketQuest.toFet(q1)) &&
              assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("strayCasket.death")))) &&
              assertTrue(wake.exists(_.hcursor.downField("wakeTo").focus.isEmpty)) &&
              assertTrue(StrayCasketQuest.leads(q2)) &&
              assertTrue(c.text("strayCasket.death").contains("отпустите ларец с карточки"))
    },

    test("Фет: ларца нет — даёт новый, заряженный, и кладёт в него съеденные камни; задание закрыто, потолок ларца — 5") {
      val eaten = List.fill(8)(Gem(GemKind.Ruby, 1)) ++ List(Gem(GemKind.Topaz, 2), Gem(GemKind.Skull, 1))
      for {
        d     <- dao(hero(), Some(quest(3, fed = 10L, eaten = eaten)))
        inv    = TestInventoryRepository.withItems(List(casketItem))
        arts   = TestArtifactRepository.empty
        r     <- TestRenderer.make
        c     <- content
        state  = FetShopState(d, arts, inv, c, Some(TestItemRepository.make))
        _     <- state.enter(testUser, r)
        menu  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("FetStray"), r)
        said  <- texts(r)
        q     <- questOf(d)
        after <- r.sentScreens.map(_.last)
        casket = arts.snapshot.casket
      } yield assertTrue(menu.choices.exists(_.id == "FetStray")) &&
              assertTrue(casket.tier == 1 && casket.charges == HeroArtifacts.MaxCharges) &&
              assertTrue(casket.items.data.flatMap(_.gem) == eaten) &&
              assertTrue(q.isDone(NpcQuest.StrayCasket) && q.strayGems.isEmpty) &&
              assertTrue(HeroArtifacts.maxTier(ArtifactKind.Casket, q) == 5) &&
              assertTrue(HeroArtifacts.maxTier(ArtifactKind.Wardrobe, q) == HeroArtifacts.MaxTier) &&
              assertTrue(!inv.snapshot.exists(_.questItem.contains(QuestItemKind.StrayCasket))) &&
              assertTrue(said.contains("Этот я оставлю себе — на память о друге")) &&
              assertTrue(said.contains("новый 💠 Ларец Азата: ступень 1/5, мест внутри: 15")) &&
              assertTrue(said.contains("Надколотый рубин ×8") && said.contains("Получено")) &&
              assertTrue(said.contains("📜 Квест «Бродячий ларец» завершён.")) &&
              assertTrue(!after.choices.exists(_.id == "FetStray"))
    },

    test("Фет: свой ларец собран до конца — пятая ступень, 75 мест, камни внутри; дальше за дублоны не растёт") {
      val full  = (1 to 60).map(i => gem(2000L + i, GemKind.Amethyst, 1)).toList
      val eaten = List.fill(10)(Gem(GemKind.Emerald, 3))
      for {
        d    <- dao(hero().copy(doubloons = 1000L), Some(quest(3, fed = 10L, eaten = eaten)))
        arts  = TestArtifactRepository.of(casket = Artifact(ArtifactKind.Casket, HeroArtifacts.MaxTier, 5, Items(full)))
        r    <- TestRenderer.make
        c    <- content
        state = FetShopState(d, arts, TestInventoryRepository.withItems(List(casketItem)), c, Some(TestItemRepository.make))
        _    <- state.action(testUser, tap("FetStray"), r)
        said <- texts(r)
        _    <- state.action(testUser, tap("FetGoods", "kind" -> "Casket"), r)
        goods <- r.sentScreens.map(_.last)
        casket = arts.snapshot.casket
      } yield assertTrue(casket.tier == 5 && casket.capacity == 75 && casket.items.data.size == 70) &&
              assertTrue(said.contains("Фет колдует над вашим ларцом") && said.contains("ступень 5/5, мест внутри: 75")) &&
              assertTrue(said.contains("Изумруд ×10")) &&
              assertTrue(!goods.choices.exists(_.id == "FetBuy"))
    },

    test("до развязки у Фета кнопки ларца нет; ларец сыт, но хозяин не упокоен — тоже") {
      for {
        c  <- content
        shown <- ZIO.foreach(List(quest(1, fed = 4L), quest(2, fed = 10L))) { q =>
                   for {
                     d <- dao(hero(), Some(q))
                     r <- TestRenderer.make
                     _ <- FetShopState(d, TestArtifactRepository.empty, TestInventoryRepository.withItems(List(casketItem)), c,
                            Some(TestItemRepository.make)).enter(testUser, r)
                     m <- r.sentScreens.map(_.last)
                   } yield m.choices.exists(_.id == "FetStray")
                 }
      } yield assertTrue(shown == List(false, false))
    },

    test("журнал: шаг задания со счётчиком; у ларца в Mini App — тот же текст карточки") {
      for {
        c   <- content
        text = QuestJournal.render(c, hero(), LoreData.empty, quest(1, fed = 4L), DailyQuests.empty, None, 0L)
        card = InventoryState.cardText(c, casketItem, hero(), quest(2, fed = 10L))
      } yield assertTrue(text.contains("«Бродячий ларец» — скормить бродячему ларцу камни (4/10)")) &&
              assertTrue(card.contains(Difficulty.render(7)) && card.contains("он помнит дорогу"))
    },

    test("старая запись заданий без съеденных камней читается; новая — туда и обратно") {
      val old = parse("""{"quests":{"fet":{"step":1,"counter":0,"since":0,"bonus":false,"done":true}},"recipes":["dustAssembly"]}""")
        .getOrElse(Json.Null)
      val fresh = quest(1, fed = 2L, eaten = List(Gem(GemKind.Ruby, 1), Gem(GemKind.Diamond, 5)))
      assertTrue(old.as[NpcQuests].toOption.exists(q => q.isDone(NpcQuest.Fet) && q.strayGems.isEmpty &&
        q.recipes == List(NpcQuests.DustAssemblyRecipe))) &&
      assertTrue(fresh.asJson.as[NpcQuests].toOption.contains(fresh))
    }
  )
}
