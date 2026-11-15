package pangea.service.state.states.barti

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{BrewKind, Item}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.story.StoryFight
import pangea.service.state.states.{ConstructionState, DeathState, MarketSquareState}
import pangea.service.state.{BartiDeliveryQuest, NpcQuestLog, QuestJournal, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Большая поставка» — побочное задание Барти в Акте II, все пути: заказ на
  * десять бутылей самогона в лагерь Чернолома у Старой вырубки, плата вперёд —
  * досками. Лавка — кнопкой с Торговой площади после «Крыши». Самогон носят
  * Барти по бутыли, потом везут в лагерь — бой с пьяной бригадой, Барти под
  * телегой; остаток — тоже досками, и доски берёт прораб Гуннар: платит
  * золотом. */
object BartiDeliverySpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, state = StateType.MarketSquare).copy(lvl = 36L, silver = 100L, doubloons = 5L)

  /** Акт I пройден, «Большое дело» и «Крыша» закрыты. */
  private def ready: NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    val face = StoryPowderQuest.finish(base, WardenFate.Killed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = SojkaFate.Broke.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
    StoryToolQuest.finish(face.take(NpcQuest.StoryTool, 0L), Path.Shield)
      .take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods).take(NpcQuest.BigDeal, 0L).finish(NpcQuest.BigDeal)
      .take(NpcQuest.BartiRoof, 0L).finish(NpcQuest.BartiRoof)
  }

  /** «Поставка» на шаге `n`, у Барти сложено `have`. */
  private def delivery(n: Int, have: Int = 0): NpcQuests =
    BartiDeliveryQuest.withStock(BartiDeliveryQuest.step(ready.take(NpcQuest.BartiDelivery, 0L), n), have)

  private def dao(q: NpcQuests) =
    for {
      d <- TestHeroDao.withHero(userId, hero)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- d.writeLoreData(userId, LoreData(firstSteps = true, firstStepsDone = true).asJson)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def brew(kind: BrewKind, id: Long): Item = BrewKind.item(kind).copy(id = id)
  private def moonshine(n: Int, from: Long): List[Item] = (0 until n).toList.map(i => brew(BrewKind.Moonshine, from + i))

  private def shop(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    BartiShopState(d, inv, TestItemRepository.make, c)

  override def spec = suite("«Большая поставка»: самогон для Чернолома, плата досками")(

    test("Торговая площадь: «Лавка Барти» — после «Крыши», пока «Поставка» доступна или идёт") {
      def buttons(q: NpcQuests) =
        for {
          c <- content
          d <- dao(q)
          r <- TestRenderer.make
          _ <- MarketSquareState(c, Some(d)).enter(testUser, r)
          s <- r.sentScreens.map(_.last)
        } yield s.choices.map(_.id)
      for {
        open   <- buttons(ready)
        going  <- buttons(delivery(3))
        closed <- buttons(delivery(3).finish(NpcQuest.BartiDelivery))
      } yield assertTrue(open.contains("BartiShop") && going.contains("BartiShop") && !closed.contains("BartiShop"))
    },

    test("впервые: доски вместо платы и рецепт самогона — задание взято, рецепт в книге") {
      for {
        c <- content
        d <- dao(ready)
        r <- TestRenderer.make
        _ <- shop(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        s <- r.sentScreens.map(_.last)
        q <- questOf(d)
      } yield assertTrue(s.text.contains("доска — та же монета, только длиннее") && s.text.contains("Не те, что у просеки") &&
                s.text.contains("с полынью и шалфеем") && s.text.contains("📜 Новое задание: «Большая поставка»") &&
                s.text.contains("У Барти в ящике — 0 из 10 бутылей самогона") &&
                s.text.contains(Difficulty.render(BartiDeliveryQuest.Level))) &&
              assertTrue(BartiDeliveryQuest.toCollect(q) && q.recipes.contains(BartiDeliveryQuest.Recipe) &&
                !s.choices.exists(_.id == "DeliveryGive"))
    },

    test("самогон — по бутыли, шнапс не в счёт; набралось десять — телега, пора в лагерь") {
      def give(have: Int, bag: List[Item]) =
        for {
          c    <- content
          d    <- dao(delivery(1, have))
          inv   = TestInventoryRepository.withItems(bag)
          r    <- TestRenderer.make
          st    = shop(d, inv, c)
          _    <- st.enter(testUser, r)
          shown <- r.sentScreens.map(_.last)
          _    <- st.action(testUser, tap("DeliveryGive"), r)
          said <- r.sentScreens.map(_.last)
          q    <- questOf(d)
        } yield (shown, said, q, inv.snapshot)
      for {
        part  <- give(0, moonshine(4, 900L) :+ brew(BrewKind.Schnapps, 950L))
        full  <- give(4, moonshine(7, 910L))
        empty <- give(2, List(brew(BrewKind.Schnapps, 951L)))
      } yield assertTrue(part._1.choices.exists(_.id == "DeliveryGive") && part._2.text.contains("«4 из 10.") &&
                BartiDeliveryQuest.stock(part._3) == 4 && BartiDeliveryQuest.toCollect(part._3) &&
                part._4.count(_.brew.contains(BrewKind.Schnapps)) == 1 && !part._4.exists(_.brew.contains(BrewKind.Moonshine))) &&
              assertTrue(full._2.text.contains("«10 из 10.") && full._2.text.contains("Поехали, пока я не передумал") &&
                full._2.choices.exists(_.id == "DeliveryGo") && BartiDeliveryQuest.toCamp(full._3) &&
                full._4.count(_.brew.contains(BrewKind.Moonshine)) == 1) &&
              assertTrue(!empty._1.choices.exists(_.id == "DeliveryGive") && empty._2.text.contains("Шнапс — это ещё не самогон") &&
                BartiDeliveryQuest.stock(empty._3) == 2)
    },

    test("в лагерь: пьяная бригада — Хряп Топорище и четверо гоблинов; без бегства, Барти не в строю; добыча вернёт в лавку") {
      for {
        c    <- content
        d    <- dao(delivery(2, BartiDeliveryQuest.Goal))
        r    <- TestRenderer.make
        res  <- shop(d, TestInventoryRepository.withItems(Nil), c).action(testUser, tap("DeliveryGo"), r)
        said <- texts(r)
        b    <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(res == StateType.Battle && said.contains("Сам снимает ящик, сам открывает, сам пробует")) &&
              assertTrue(b.exists(x => x.story.contains(BartiDeliveryQuest.FightStory) && x.cantFlee &&
                x.monsterName.contains("Хряп Топорище") && x.monsterRace == Race.Goblin.entryName &&
                x.monsterRarity == Rarity.Mythical.entryName && x.monsterLvl == BartiDeliveryQuest.Level.toLong &&
                x.group.others.size == 4 && x.group.others.forall(m => m.race == Race.Goblin.entryName && m.rarity == Rarity.Rare.entryName) &&
                x.group.allies.isEmpty && x.kegs == 0)) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.BartiShop)) && BattleState.RaidStories.contains(BartiDeliveryQuest.FightStory))
    },

    test("отбились — остаток досками, Барти шлёт к Гуннару; Гуннар берёт живые доски и платит 80 дублонов — задание закрыто") {
      for {
        c      <- content
        d      <- dao(delivery(2, BartiDeliveryQuest.Goal))
        _      <- d.writeSceneData(userId, StoryFight.mark(BartiShopState.DeliveryWonKey))
        r      <- TestRenderer.make
        _      <- shop(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        back   <- r.sentScreens.map(_.last.text)
        q1     <- questOf(d)
        sch    <- TestScheduler.make
        site    = ConstructionState(d, sch, c)
        r2     <- TestRenderer.make
        _      <- site.enter(testUser, r2)
        menu   <- r2.sentScreens.map(_.last)
        _      <- site.action(testUser, tap("BartiPlanks"), r2)
        paid   <- r2.sentScreens.map(_.last.text)
        q      <- questOf(d)
        h      <- heroOf(d)
      } yield assertTrue(back.contains("«Остаток, — говорит он") && back.contains("история с известью") && BartiDeliveryQuest.toGunnar(q1)) &&
              assertTrue(menu.choices.exists(_.id == "BartiPlanks")) &&
              assertTrue(paid.contains("«Живая, — говорит он. — Из изгороди.") && paid.contains("такой топят печи") &&
                paid.contains(s"🟡 +${BartiDeliveryQuest.Doubloons} — Гуннар платит за доски золотом") &&
                paid.contains("📜 Квест «Большая поставка» завершён.")) &&
              assertTrue(q.isDone(NpcQuest.BartiDelivery) && h.doubloons == 5L + BartiDeliveryQuest.Doubloons && h.exp > 0L)
    },

    test("смерть в лагере — проснуться на Торговой площади, самогон цел, шаг тот же; журнал — что делать на каждом шаге") {
      for {
        c     <- content
        d     <- dao(delivery(2, BartiDeliveryQuest.Goal))
        fight  = SoloPveBattle.fromGroup(BartiDeliveryQuest.foes("Хряп Топорище"), hero, List.fill(5)(0L))
                   .copy(story = Some(BartiDeliveryQuest.FightStory))
        _     <- d.writeActiveBattle(userId, fight.asJson)
        r     <- TestRenderer.make
        _     <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        wake  <- d.readSceneData(userId)
        q     <- questOf(d)
        line   = (n: Int) => QuestJournal.render(c, hero, LoreData.empty, delivery(n), DailyQuests.empty, None, 0L)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("barti.delivery.death"))) &&
                wake.exists(_.hcursor.get[StateType]("wakeTo").toOption.contains(StateType.MarketSquare)) &&
                BartiDeliveryQuest.toCamp(q) && BartiDeliveryQuest.stock(q) == BartiDeliveryQuest.Goal) &&
              assertTrue(line(1).contains("«Большая поставка» — принести Барти десять бутылей самогона из красавки") &&
                line(2).contains("отвезти с Барти самогон в лагерь Чернолома у Старой вырубки") &&
                line(3).contains("сбыть доски Барти прорабу Гуннару"))
    }
  )
}
