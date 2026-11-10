package pangea.service.state.states.barti

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.BigDealQuest.Stock
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.tavern.MercenariesState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{BigDealQuest, NpcQuestLog, QuestJournal, UserAction}
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** «Большое дело», караван: с 11-го уровня героя караван приходит, ночью у
  * Сухого колодца — подозрительные каджиты, засада ночной стражи, плата
  * дублонами; пал — задание всё равно закрыто, меньшей платой. Герой-каджит —
  * свой текст и двое капитанов каравана в строю. */
object BartiDealSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 11L, race: Race = Race.Human): Hero =
    TestFixtures.hero(userId).copy(lvl = lvl, race = race, silver = 100L, doubloons = 10L)

  /** «Ходовой товар» закрыт, «Большое дело» на шаге `step`, товар собран. */
  private def deal(step: Int): NpcQuests =
    BigDealQuest.step(BigDealQuest.withStock(NpcQuests.empty.take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods)
      .take(NpcQuest.BigDeal, 0L), Stock(BigDealQuest.Goal, BigDealQuest.Goal)), step)

  private def dao(h: Hero, q: NpcQuests) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, q)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def addressItem: Item = QuestItemKind.item(QuestItemKind.BartiAddress).copy(id = 951L)

  private def openCard(state: InventoryState, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${addressItem.id}"}""")), r)

  /** «Поставить ящики» у каравана: что сказано и какой бой. */
  private def ambush(h: Hero) =
    for {
      d      <- dao(h, deal(3))
      r      <- TestRenderer.make
      c      <- content
      _      <- TestRandom.feedLongs(50L, 50L, 50L, 50L)
      res    <- BartiCaravanState(d, TestInventoryRepository.withItems(List(addressItem)), c).action(testUser, tap("CaravanBoxes"), r)
      said   <- texts(r)
      battle <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      loot   <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
    } yield (res, said, battle, loot)

  override def spec = suite("Барти: «Большое дело» — караван")(

    test("товар у Барти: ниже 11-го — «рано», шаг тот же; с 11-го — караван пришёл, задание ведёт к нему") {
      def visit(lvl: Long) =
        for {
          d    <- dao(hero(lvl), deal(2))
          r    <- TestRenderer.make
          c    <- content
          _    <- BartiHouseState(d, TestInventoryRepository.withItems(List(addressItem)), c).enter(testUser, r)
          said <- texts(r)
          q    <- questOf(d)
        } yield (said, q)
      for {
        young <- visit(10L)
        grown <- visit(11L)
      } yield assertTrue(young._1.contains("Не-е. Рано") && BigDealQuest.waiting(young._2)) &&
              assertTrue(grown._1.contains("Караван стоит на Западной дороге, у Сухого колодца") && BigDealQuest.toCaravan(grown._2))
    },

    test("карточка адреса: караван пришёл — «Идти к каравану», только из города; домой к Барти уже не ведёт") {
      for {
        d     <- dao(hero(), deal(3))
        r     <- TestRenderer.make
        c     <- content
        card   = InventoryState(d, TestInventoryRepository.withItems(List(addressItem)), TestItemRepository.make, c)
        _     <- d.writeReturnState(userId, Some(StateType.Dungeon))
        _     <- openCard(card, r)
        shown <- r.sentScreens.map(_.last)
        deep  <- card.action(testUser, tap("BartiCaravanGo"), r)
        _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
        go    <- card.action(testUser, tap("BartiCaravanGo"), r)
        house <- card.action(testUser, tap("BartiHouseGo"), r)
      } yield assertTrue(shown.text.contains("Сухого колодца") && shown.choices.exists(_.id == "BartiCaravanGo") &&
                !shown.choices.exists(_.id == "BartiHouseGo")) &&
              assertTrue(deep == StateType.Inventory && go == StateType.BartiCaravan && house == StateType.Inventory)
    },

    test("у колодца: каджиты будто готовят подвох, Барти везёт и свой запас; сложность задания; каджиту — свой текст") {
      def meet(h: Hero) =
        for {
          d     <- dao(h, deal(3))
          r     <- TestRenderer.make
          c     <- content
          _     <- BartiCaravanState(d, TestInventoryRepository.withItems(List(addressItem)), c).enter(testUser, r)
          shown <- r.sentScreens.map(_.last)
        } yield shown
      for {
        human <- meet(hero())
        cat   <- meet(hero(race = Race.Khajiit))
      } yield assertTrue(human.text.contains("Каджит всегда платит. Не сейчас") && human.text.contains("ящик самогона") &&
                human.text.contains(Difficulty.render(BigDealQuest.Level)) &&
                human.choices.map(_.id) == List("CaravanBoxes", "CaravanLeave")) &&
              assertTrue(cat.text.contains("Здешние пески холодны") && cat.text.contains("собрат-каджит") &&
                cat.text.contains("ящик самогона") && !cat.text.contains("Не сейчас"))
    },

    test("засада ночной стражи: виселица на главной площади, бой уровня задания без бегства; каджиту — двое капитанов в строю") {
      for {
        human <- ambush(hero())
        cat   <- ambush(hero(race = Race.Khajiit))
        (res, said, battle, loot) = human
      } yield assertTrue(res == StateType.Battle && said.contains("повешу на главной площади — как беглых преступников")) &&
              assertTrue(battle.exists(b => b.story.contains(BigDealQuest.AmbushStory) && b.cantFlee &&
                b.monsterName.contains("Капитан ночной стражи") && b.monsterRarity == Rarity.Rare.entryName &&
                b.monsterLvl == BigDealQuest.Level.toLong && b.group.others.size == 3 &&
                !b.group.allies.exists(_.kind == AllyKind.CaravanKhajiit))) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.BartiCaravan)) && BattleState.RaidStories.contains(BigDealQuest.AmbushStory)) &&
              assertTrue(cat._2.contains("Брата не бросают") && cat._3.exists(b =>
                b.group.allies.count(_.kind == AllyKind.CaravanKhajiit) == 2 &&
                  b.group.allies.flatMap(_.undead.map(_.name)).toSet == Set("Ра'Тхар", "Ма'Кесс"))) &&
              assertTrue(MercenariesState.key(AllyKind.CaravanKhajiit) == "caravanKhajiit")
    },

    test("стража перебита: каджиты платят сполна — 125 дублонов, Барти проговаривается в сторонке; адрес уходит, задание закрыто") {
      def win(h: Hero) =
        for {
          d    <- dao(h, deal(3))
          inv   = TestInventoryRepository.withItems(List(addressItem))
          r    <- TestRenderer.make
          c    <- content
          _    <- d.writeSceneData(userId, BartiCaravanState.WonMark.asJson)
          _    <- BartiCaravanState(d, inv, c).enter(testUser, r)
          said <- texts(r)
          q    <- questOf(d)
          after <- heroOf(d)
        } yield (said, q, after, inv.snapshot)
      for {
        human <- win(hero())
        cat   <- win(hero(race = Race.Khajiit))
      } yield assertTrue(human._1.contains("Каджит всегда платит. Даже когда никто не верит") &&
                human._1.contains("Мы отходим от костров") && human._1.contains("Это ведь не из-за меня?") &&
                human._1.contains("🟡 +125") && human._1.contains("📜 Квест «Большое дело» завершён")) &&
              assertTrue(cat._1.contains("Брату — тем более")) &&
              assertTrue(List(human, cat).forall { case (_, q, h, bag) =>
                q.isDone(NpcQuest.BigDeal) && h.doubloons == 10L + BigDealQuest.WonDoubloons && h.exp > 0L &&
                  !bag.exists(_.questItem.contains(QuestItemKind.BartiAddress))
              })
    },

    test("пал в засаде — задание всё равно закрыто: 85 дублонов, рассказ Барти, проснуться на Торговой площади") {
      val mob = Monster(0L, 11L, Race.Human, Rarity.Uncommon,
        FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      for {
        d     <- dao(hero().copy(exp = 50L), deal(3))
        inv    = TestInventoryRepository.withItems(List(addressItem))
        _     <- d.writeActiveBattle(userId, SoloPveBattle.from(mob, hero()).copy(story = Some(BigDealQuest.AmbushStory)).asJson)
        r     <- TestRenderer.make
        c     <- content
        _     <- TestRandom.feedInts(1)
        _     <- DeathState(d, inv, c).enter(testUser, r)
        scene <- d.readSceneData(userId)
        q     <- questOf(d)
        h     <- heroOf(d)
        note   = scene.flatMap(_.hcursor.get[String]("wakeNote").toOption).getOrElse("")
        story  = c.text("barti.caravan.fallen")
      } yield assertTrue(scene.exists(_.hcursor.get[String]("wakeTo").toOption.contains("MarketSquare"))) &&
              assertTrue(scene.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("barti.caravan.fallen")))) &&
              assertTrue(note.contains("🟡 +85") && note.contains("📜 Квест «Большое дело» завершён")) &&
              assertTrue(story.contains("Сабли из-под тюков") && story.contains("доля за охрану")) &&
              assertTrue(q.isDone(NpcQuest.BigDeal) && h.doubloons == 10L + BigDealQuest.FallenDoubloons &&
                !inv.snapshot.exists(_.questItem.contains(QuestItemKind.BartiAddress)))
    },

    test("журнал: караван пришёл — шаг к Сухому колодцу") {
      for {
        c   <- content
        text = QuestJournal.render(c, hero(), LoreData.empty, deal(3), DailyQuests.empty, None, 0L)
      } yield assertTrue(text.contains("🧭 «Большое дело» — прийти ночью к каравану у Сухого колодца"))
    }
  )
}
