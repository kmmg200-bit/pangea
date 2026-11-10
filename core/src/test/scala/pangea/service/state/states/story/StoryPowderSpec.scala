package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, FirstSteps, NpcQuest, NpcQuests, QuestOutcome}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryPowderQuest, UserAction}
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Основная линия, глава 2 «Порох у причала»: шифровка ведёт в порт с 9-го
  * уровня, дегтярный склад, бой с людьми Смотрителя, его судьба — три исхода
  * с подсчётом «как и вы», набросок узора — зацепка главы 3. */
object StoryPowderSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 9L): Hero = TestFixtures.hero(userId).copy(lvl = lvl, silver = 100L)

  /** Глава 1 закрыта. */
  private val afterMark: NpcQuests =
    FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)

  /** Глава 2 на шаге `step`. */
  private def powder(step: Int): NpcQuests =
    afterMark.take(NpcQuest.StoryPowder, 0L).update(NpcQuest.StoryPowder)(_.copy(step = step))

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

  private def cipherItem: Item = QuestItemKind.item(QuestItemKind.StoryCipher).copy(id = 900L)

  private def openCard(state: InventoryState, item: Item, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${item.id}"}""")), r)

  override def spec = suite("Основная линия: «Порох у причала»")(

    test("карточка шифровки: «Идти в порт» сразу после главы 1 — на любом уровне, «рано» не говорим; из лабиринта — отказ; из города — глава взята, на склад") {
      def card(lvl: Long, from: StateType) =
        for {
          d     <- dao(hero(lvl), afterMark)
          _     <- d.writeReturnState(userId, Some(from))
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(List(cipherItem)), TestItemRepository.make, c)
          _     <- openCard(state, cipherItem, r)
          shown <- r.sentScreens.map(_.last)
          go    <- state.action(testUser, tap("StoryPortGo"), r)
          said  <- texts(r)
          q     <- questOf(d)
        } yield (shown, go, said, q)
      for {
        low   <- card(1L, StateType.GlobalMap)
        deep  <- card(9L, StateType.Dungeon)
        city  <- card(9L, StateType.GlobalMap)
      } yield assertTrue(low._1.text.contains("Пора наведаться в порт") && low._1.choices.exists(_.id == "StoryPortGo")) &&
              assertTrue(low._1.text.contains(pangea.model.quest.Difficulty.render(StoryPowderQuest.Level)) && !low._1.text.contains("окрепнуть")) &&
              assertTrue(low._2 == StateType.StoryWarehouse && StoryPowderQuest.toWarehouse(low._4)) &&
              assertTrue(deep._1.choices.exists(_.id == "StoryPortGo") && deep._2 == StateType.Inventory && deep._3.contains("из города")) &&
              assertTrue(city._1.text.contains("Пора наведаться в порт") && city._2 == StateType.StoryWarehouse) &&
              assertTrue(StoryPowderQuest.toWarehouse(city._4) && city._3.contains("📜 Новое задание: «Порох у причала»"))
    },

    test("склад: дверь, сложность, «Драться»/«Уйти»; бой — Смотритель и трое людей уровня задания (не героя), без бегства, добыча вернёт сюда") {
      for {
        d      <- dao(hero(11L), powder(1))
        r      <- TestRenderer.make
        c      <- content
        state   = StoryWarehouseState(d, TestInventoryRepository.withItems(List(cipherItem)), TestItemRepository.make, c)
        _      <- state.enter(testUser, r)
        door   <- r.sentScreens.map(_.last)
        _      <- TestRandom.feedLongs(50L, 50L, 50L, 50L)
        res    <- state.action(testUser, tap("WarehouseFight"), r)
        battle <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot   <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(door.text.contains("Курьер не тот") && door.text.contains(pangea.model.quest.Difficulty.render(StoryPowderQuest.Level))) &&
              assertTrue(door.choices.map(_.id) == List("WarehouseFight", "WarehouseLeave") && res == StateType.Battle) &&
              assertTrue(battle.exists(b => b.story.contains(StoryPowderQuest.FightStory) && b.cantFlee)) &&
              assertTrue(battle.exists(b => b.monsterName.contains("Смотритель") && b.monsterRarity == Rarity.Rare.entryName &&
                b.monsterRace == Race.Human.entryName && b.monsterLvl == StoryPowderQuest.Level.toLong)) &&
              assertTrue(battle.exists(_.group.others.forall(s => s.lvl == StoryPowderQuest.Level.toLong && s.rarity == Rarity.Uncommon.entryName)) &&
                battle.exists(_.group.others.size == 3)) &&
              assertTrue(loot.exists(l => l.returnState.contains(StateType.StoryWarehouse) &&
                l.eventData.flatMap(_.as[StoryWarehouseState.Won].toOption).exists(_.won))) &&
              assertTrue(BattleState.RaidStories.contains(StoryPowderQuest.FightStory))
    },

    test("люди Смотрителя перебиты: он на полу, три кнопки — добить, отпустить, сдать совету; вернулся позже — те же кнопки") {
      for {
        d     <- dao(hero(), powder(1))
        _     <- d.writeSceneData(userId, StoryWarehouseState.WonMark.asJson)
        r     <- TestRenderer.make
        c     <- content
        state  = StoryWarehouseState(d, TestInventoryRepository.withItems(List(cipherItem)), TestItemRepository.make, c)
        _     <- state.enter(testUser, r)
        fate  <- r.sentScreens.map(_.last)
        q     <- questOf(d)
        sd    <- d.readSceneData(userId)
        _     <- state.enter(testUser, r)
        again <- r.sentScreens.map(_.last)
      } yield assertTrue(fate.text.contains("Нам велели ждать отмеченного")) &&
              assertTrue(fate.choices.map(_.id) == List("WardenKill", "WardenRelease", "WardenHand") && again.choices == fate.choices) &&
              assertTrue(StoryPowderQuest.beaten(q) && sd.forall(_.isNull))
    },

    test("судьба Смотрителя: выбор закрывает главу — шифровка уходит, набросок узора в сумке, касса, опыт; исход записан") {
      def decide(fate: WardenFate, action: String) =
        for {
          d    <- dao(hero(10L), powder(2))
          inv   = TestInventoryRepository.withItems(List(cipherItem))
          o    <- TestQuestOutcomeDao.make
          r    <- TestRenderer.make
          c    <- content
          _    <- StoryWarehouseState(d, inv, TestItemRepository.make, c, Some(o)).action(testUser, tap(action), r)
          said <- texts(r)
          q    <- questOf(d)
          h    <- heroOf(d)
          mine <- o.of(h.id, NpcQuest.StoryPowder)
        } yield (said, q, h, inv.snapshot, mine, fate)
      for {
        killed   <- decide(WardenFate.Killed, "WardenKill")
        released <- decide(WardenFate.Released, "WardenRelease")
        handed   <- decide(WardenFate.Handed, "WardenHand")
        all       = List(killed, released, handed)
      } yield assertTrue(killed._1.contains("хороший ученик") && released._1.contains("Первым кораблём") && handed._1.contains("посту стражи")) &&
              assertTrue(all.forall { case (said, q, h, bag, mine, fate) =>
                said.contains("Отмеченный. Брать живым") && said.contains("Квест «Порох у причала» завершён") &&
                  said.contains("Вы — среди первых, кто закончил «Порох у причала»") &&
                  q.isDone(NpcQuest.StoryPowder) && StoryPowderQuest.choice(q).contains(fate) &&
                  bag.exists(_.questItem.contains(QuestItemKind.StorySketch)) && !bag.exists(_.questItem.contains(QuestItemKind.StoryCipher)) &&
                  h.silver == 100L + StoryPowderQuest.Level * StoryPowderQuest.SilverPerLvl && h.exp > 0L && mine.contains(fate.outcome)
              })
    },

    test("десятый, кто решил судьбу Смотрителя, видит проценты — своя строка «как и вы»") {
      for {
        d    <- dao(hero(), powder(2))
        o    <- TestQuestOutcomeDao.make
        _    <- o.seed(QuestOutcome.PowderKilled -> 5, QuestOutcome.PowderReleased -> 2, QuestOutcome.PowderHanded -> 2)
        r    <- TestRenderer.make
        c    <- content
        _    <- StoryWarehouseState(d, TestInventoryRepository.withItems(List(cipherItem)), TestItemRepository.make, c, Some(o))
                  .action(testUser, tap("WardenHand"), r)
        said <- texts(r)
      } yield assertTrue(said.contains(List(
                "📊 Как закончили «Порох у причала» герои Пангеи:",
                "• 50% добили Смотрителя",
                "• 30% сдали Смотрителя совету — как и вы",
                "• 20% отпустили Смотрителя").mkString("\n")))
    },

    test("смерть на складе: проснуться в порту, глава на том же шаге; журнал — глава сюжетным заданием") {
      val mob = Monster(0L, 9L, Race.Human, Rarity.Uncommon,
        FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      for {
        d     <- dao(hero().copy(exp = 50L), powder(1))
        _     <- d.writeActiveBattle(userId, SoloPveBattle.from(mob, hero()).copy(story = Some(StoryPowderQuest.FightStory)).asJson)
        r     <- TestRenderer.make
        c     <- content
        _     <- TestRandom.feedInts(1)
        _     <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        scene <- d.readSceneData(userId)
        q     <- questOf(d)
        text   = QuestJournal.render(c, hero(), ready, powder(1), DailyQuests.empty, None, 0L)
      } yield assertTrue(scene.exists(_.hcursor.get[String]("wakeTo").toOption.contains("HarborQuarter"))) &&
              assertTrue(scene.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("story.storyPowder.warehouse.death")))) &&
              assertTrue(StoryPowderQuest.toWarehouse(q)) &&
              assertTrue(text.contains("📌 «Порох у причала»") && text.contains("дегтярный склад"))
    }
  )
}
