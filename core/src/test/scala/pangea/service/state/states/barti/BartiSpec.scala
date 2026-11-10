package pangea.service.state.states.barti

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{AzatState, CubeStatus, Hero}
import pangea.model.item.{BrewKind, Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.BigDealQuest.Stock
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{AzatData, BigDealQuest, HotGoodsQuest, NpcQuestLog, QuestJournal, UserAction}
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Побочная линия Барти: «Ходовой товар» — встреча у изгороди наугад с 4-го
  * уровня героя, бой, рецепты и план, свой куб и товар, вторая встреча и
  * адрес; «Большое дело» — дом Барти, товар хоть по одной склянке, ожидание
  * каравана. */
object BartiSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 4L): Hero = TestFixtures.hero(userId, dungeonLevel = 2).copy(lvl = lvl, silver = 100L)

  private def dao(h: Hero, q: NpcQuests = NpcQuests.empty, cube: Boolean = false) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- ZIO.when(cube)(AzatData.save(d, userId, AzatState(cube = CubeStatus.Active)))
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def planItem: Item    = QuestItemKind.item(QuestItemKind.BartiPlan).copy(id = 950L)
  private def addressItem: Item = QuestItemKind.item(QuestItemKind.BartiAddress).copy(id = 951L)
  private def brew(kind: BrewKind, id: Long): Item = BrewKind.item(kind).copy(id = id)
  private def goods: List[Item] = List(brew(BrewKind.LivingWater, 960L), brew(BrewKind.Schnapps, 961L))

  /** «Ходовой товар» взят. */
  private val taken: NpcQuests = NpcQuests.empty.take(NpcQuest.HotGoods, 0L)

  /** «Ходовой товар» закрыт. */
  private val goodsDone: NpcQuests = taken.finish(NpcQuest.HotGoods)

  private def openCard(state: InventoryState, item: Item, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${item.id}"}""")), r)

  private def explore(d: TestHeroDao, inv: TestInventoryRepository = TestInventoryRepository.withItems(Nil)) =
    for {
      r   <- TestRenderer.make
      sch <- TestScheduler.make
      c   <- content
      res <- DungeonState(d, inv, sch, c).action(testUser, tap("FindEvent"), r)
    } yield res

  override def spec = suite("Побочная линия Барти")(

    test("встреча наугад: с 4-го уровня героя, 3% на осмотр, после редких заданий; взятое или закрытое задание — больше нет") {
      for {
        // билет пула на каждый осмотр (50 — находка); на 2-м повороте редкий бросок не идёт — дробный только у Барти
        _      <- TestRandom.feedInts(List.fill(4)(50): _*)
        _      <- TestRandom.feedDoubles(0.02, 0.9)
        met    <- dao(hero()).flatMap(explore(_))
        miss   <- dao(hero()).flatMap(explore(_))
        young  <- dao(hero(lvl = 3L)).flatMap(explore(_))
        busy   <- dao(hero(), goodsDone).flatMap(explore(_))
      } yield assertTrue(met == StateType.BartiMeet && miss == StateType.FoundItem) &&
              assertTrue(young == StateType.FoundItem && busy == StateType.FoundItem) &&
              assertTrue(HotGoodsQuest.Chance == 0.03 && HotGoodsQuest.MinHeroLvl == 4L)
    },

    test("у изгороди: Барти зажали звери, сложность; бой — двое зверей уровня задания, без бегства; «Пройти мимо» — задание не взято") {
      for {
        d      <- dao(hero())
        r      <- TestRenderer.make
        c      <- content
        state   = BartiMeetState(d, TestInventoryRepository.withItems(Nil), TestItemRepository.make, c)
        _      <- state.enter(testUser, r)
        meet   <- r.sentScreens.map(_.last)
        _      <- TestRandom.feedLongs(50L, 50L)
        res    <- state.action(testUser, tap("BartiFight"), r)
        battle <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot   <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        p      <- dao(hero())
        out    <- BartiMeetState(p, TestInventoryRepository.withItems(Nil), TestItemRepository.make, c).action(testUser, tap("BartiPass"), r)
        q      <- questOf(p)
      } yield assertTrue(meet.text.contains("Это мой шалфей") && meet.text.contains(Difficulty.render(HotGoodsQuest.Level)) &&
                meet.choices.map(_.id) == List("BartiFight", "BartiPass")) &&
              assertTrue(res == StateType.Battle && battle.exists(b => b.story.contains(HotGoodsQuest.FightStory) && b.cantFlee &&
                b.monsterLvl == HotGoodsQuest.Level.toLong && b.group.others.size == 1)) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.BartiMeet)) && BattleState.RaidStories.contains(HotGoodsQuest.FightStory)) &&
              assertTrue(out == StateType.Dungeon && HotGoodsQuest.fresh(q))
    },

    test("звери отбиты: «любой грамотный и способный», рецепты записаны, план в сумке, задание взято — шаг по кубу") {
      for {
        d     <- dao(hero())
        inv    = TestInventoryRepository.withItems(Nil)
        r     <- TestRenderer.make
        c     <- content
        _     <- d.writeSceneData(userId, BartiMeetState.WonMark.asJson)
        _     <- BartiMeetState(d, inv, TestItemRepository.make, c).enter(testUser, r)
        said  <- texts(r)
        last  <- r.sentScreens.map(_.last)
        q     <- questOf(d)
        sd    <- d.readSceneData(userId)
        known  = HotGoodsQuest.Recipes.forall(k => q.recipes.toSet(k))
      } yield assertTrue(said.contains("любой грамотный и способный человек раскрутится") && said.contains("Для друзей — Барти") &&
                said.contains("шалфей, ромашка, крапива") && said.contains("📜 Новое задание: «Ходовой товар»") &&
                said.contains("обзавестись своим кубом Азата")) &&
              assertTrue(q.isTaken(NpcQuest.HotGoods) && q.of(NpcQuest.HotGoods).step == 1 && known) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.BartiPlan)) && sd.forall(_.isNull) &&
                last.choices.map(_.id) == List("BartiOut"))
    },

    test("карточка плана: нет куба — завести; куб есть — сварить товар; всё есть — Барти найдёт сам; шаг пишется в журнал") {
      def card(cube: Boolean, bag: List[Item]) =
        for {
          d     <- dao(hero(), taken, cube)
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(planItem :: bag), TestItemRepository.make, c)
          _     <- openCard(state, planItem, r)
          shown <- r.sentScreens.map(_.last)
          q     <- questOf(d)
        } yield (shown.text, q.of(NpcQuest.HotGoods).step)
      for {
        noCube <- card(cube = false, goods)
        noBrew <- card(cube = true, Nil)
        ready  <- card(cube = true, goods)
        c      <- content
        text    = QuestJournal.render(c, hero(), pangea.model.hero.LoreData.empty, taken.update(NpcQuest.HotGoods)(_.copy(step = 2)),
                    DailyQuests.empty, None, 0L)
      } yield assertTrue(noCube._1.contains("без своего куба") && noCube._2 == 1 &&
                noCube._1.contains(Difficulty.render(HotGoodsQuest.Level)) && noCube._1.contains("Деловой план Барти")) &&
              assertTrue(noBrew._1.contains("Куб есть") && noBrew._2 == 2) &&
              assertTrue(ready._1.contains("найдёт") && ready._2 == 3) &&
              assertTrue(text.contains("🧭 «Ходовой товар» — сварить в кубе живую воду и шнапс"))
    },

    test("товар готов: ближайший осмотр — Барти сам; адрес, должок за шалфей, опыт, задание закрыто, план уходит") {
      for {
        d     <- dao(hero(), taken, cube = true)
        inv    = TestInventoryRepository.withItems(planItem :: goods)
        _     <- TestRandom.feedInts(50)
        res   <- explore(d, inv)
        r     <- TestRenderer.make
        c     <- content
        _     <- BartiMeetState(d, inv, TestItemRepository.make, c).enter(testUser, r)
        said  <- texts(r)
        q     <- questOf(d)
        h     <- heroOf(d)
      } yield assertTrue(res == StateType.BartiMeet) &&
              assertTrue(said.contains("заходи ко мне на Торговую площадь") && said.contains("📜 Квест «Ходовой товар» завершён")) &&
              assertTrue(q.isDone(NpcQuest.HotGoods) && h.silver == 100L + HotGoodsQuest.silver && h.exp > 0L) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.BartiAddress)) &&
                !inv.snapshot.exists(_.questItem.contains(QuestItemKind.BartiPlan)) && HotGoodsQuest.silver == 450L)
    },

    test("смерть у изгороди: проснуться в лабиринте, задание так и не начато") {
      val mob = Monster(0L, 3L, Race.Animal, Rarity.Uncommon,
        FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      for {
        d     <- dao(hero().copy(exp = 50L))
        _     <- d.writeActiveBattle(userId, SoloPveBattle.from(mob, hero()).copy(story = Some(HotGoodsQuest.FightStory)).asJson)
        r     <- TestRenderer.make
        c     <- content
        _     <- TestRandom.feedInts(1)
        _     <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        scene <- d.readSceneData(userId)
        q     <- questOf(d)
      } yield assertTrue(scene.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("barti.meet.death")))) &&
              assertTrue(scene.exists(_.hcursor.downField("wakeTo").focus.forall(_.isNull))) &&
              assertTrue(HotGoodsQuest.fresh(q))
    },

    test("адрес ведёт к Барти — только из города; впервые — затея с каджитами, «Большое дело» взято") {
      for {
        d     <- dao(hero(), goodsDone)
        inv    = TestInventoryRepository.withItems(List(addressItem))
        r     <- TestRenderer.make
        c     <- content
        card   = InventoryState(d, inv, TestItemRepository.make, c)
        _     <- d.writeReturnState(userId, Some(StateType.Dungeon))
        _     <- openCard(card, addressItem, r)
        shown <- r.sentScreens.map(_.last)
        deep  <- card.action(testUser, tap("BartiHouseGo"), r)
        _     <- d.writeReturnState(userId, Some(StateType.MarketSquare))
        go    <- card.action(testUser, tap("BartiHouseGo"), r)
        r2    <- TestRenderer.make
        _     <- BartiHouseState(d, inv, c).enter(testUser, r2)
        said  <- texts(r2)
        last  <- r2.sentScreens.map(_.last)
        q     <- questOf(d)
      } yield assertTrue(shown.text.contains("дом с синей дверью") && shown.text.contains(Difficulty.render(BigDealQuest.Level)) &&
                shown.choices.exists(_.id == "BartiHouseGo")) &&
              assertTrue(deep == StateType.Inventory && go == StateType.BartiHouse) &&
              assertTrue(said.contains("Контрабанда") && said.contains("Ра'Шидд") && said.contains("📜 Новое задание: «Большое дело»") &&
                said.contains("шнапса — 0 из 20")) &&
              assertTrue(BigDealQuest.collecting(q) && last.choices.map(_.id) == List("BartiLeave"))
    },

    test("Барти берёт товар хоть по одной склянке и не больше, чем нужно; собрано всё — ниже 11-го «рано», с 11-го — ждёт караван") {
      val almost = BigDealQuest.withStock(goodsDone.take(NpcQuest.BigDeal, 0L), Stock(19, 18))
      def give(lvl: Long, q: NpcQuests, bag: List[Item]) =
        for {
          d     <- dao(hero(lvl), q)
          inv    = TestInventoryRepository.withItems(addressItem :: bag)
          r     <- TestRenderer.make
          c     <- content
          state  = BartiHouseState(d, inv, c)
          _     <- state.enter(testUser, r)
          menu  <- r.sentScreens.map(_.last)
          _     <- state.action(testUser, tap("BartiGive"), r)
          said  <- texts(r)
          quest <- questOf(d)
        } yield (menu, said, quest, inv.snapshot)
      val partial = goodsDone.take(NpcQuest.BigDeal, 0L)
      val bag     = List(brew(BrewKind.Schnapps, 970L), brew(BrewKind.Schnapps, 971L), brew(BrewKind.LivingWater, 972L),
                      brew(BrewKind.LivingWater, 973L), brew(BrewKind.LivingWater, 974L), brew(BrewKind.Moonshine, 975L))
      for {
        one   <- give(4L, partial, List(brew(BrewKind.Schnapps, 976L)))
        early <- give(4L, almost, bag)
        late  <- give(11L, almost, bag)
      } yield assertTrue(one._1.choices.map(_.id) == List("BartiGive", "BartiLeave") && one._2.contains("Шнапса — 1, живой воды — 0") &&
                BigDealQuest.stock(one._3) == Stock(1, 0) && BigDealQuest.collecting(one._3)) &&
              // взял один шнапс и две живые воды — сколько не хватало; самогон и лишнее остались в сумке
              assertTrue(BigDealQuest.stock(early._3) == Stock(20, 20) && BigDealQuest.waiting(early._3) &&
                early._4.count(_.brew.contains(BrewKind.Schnapps)) == 1 && early._4.count(_.brew.contains(BrewKind.LivingWater)) == 1 &&
                early._4.exists(_.brew.contains(BrewKind.Moonshine))) &&
              assertTrue(early._2.contains("Не-е. Рано") && !early._2.contains("🔜")) &&
              assertTrue(late._2.contains("Караван ещё не пришёл") && late._2.contains("🔜"))
    },

    test("сколько у Барти — одним числом в счётчике задания; журнал «Большого дела» — без чисел") {
      val s = Stock(12, 7)
      for {
        c   <- content
        text = QuestJournal.render(c, hero(), pangea.model.hero.LoreData.empty, goodsDone.take(NpcQuest.BigDeal, 0L),
                 DailyQuests.empty, None, 0L)
      } yield assertTrue(s.counter == 1207L && Stock.fromCounter(1207L) == s && Stock.fromCounter(0L) == Stock.empty) &&
              assertTrue(Stock(19, 20).plus(BrewKind.Schnapps, 5) == Stock(20, 20) && Stock(20, 20).full && !Stock(20, 19).full) &&
              assertTrue(text.contains("🧭 «Большое дело» — принести Барти двадцать бутылок шнапса"))
    }
  )
}
