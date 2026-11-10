package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.home.HomeData
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.guild.MasterHornState
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.states.{DeathState, InventoryState, LootState}
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryLine, StoryMarkQuest, UserAction}
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Основная линия, глава 1 «Метка под меткой»: ведёт её чёрный плащ — первая
  * зацепка, найденная у изгороди после «Встать на ноги»; Ришелье спарывает
  * метку, Горн называет Сороку, тайник ищут с карточки плаща; в тайнике —
  * бой и следующая зацепка, шифровка «Э.». */
object StoryMarkSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 5L, dungeonLevel: Int = 5): Hero =
    TestFixtures.hero(userId, dungeonLevel = dungeonLevel).copy(lvl = lvl, silver = 100L)

  /** «Встать на ноги» пройдено по заданиям горожан. */
  private val cityDone: NpcQuests =
    FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))

  /** Глава на шаге `step`. */
  private def chapter(step: Int): NpcQuests =
    cityDone.take(NpcQuest.StoryMark, 0L).update(NpcQuest.StoryMark)(_.copy(step = step))

  private val house: HomeData = HomeData(tier = 1)

  /** «Встать на ноги» засчитано дома, исследований до первой зацепки хватило. */
  private val ready: LoreData = LoreData(firstSteps = true, firstStepsDone = true, cluePasses = StoryLine.FirstCluePasses)

  private def dao(h: Hero, q: NpcQuests, home: Option[HomeData] = Some(house), lore: LoreData = ready) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- ZIO.foreachDiscard(home)(x => d.writeHomeData(userId, x.asJson))
      _ <- d.writeLoreData(userId, lore.asJson)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def cloakItem: Item  = QuestItemKind.item(QuestItemKind.StoryCloak).copy(id = 901L)
  private def cipherItem: Item = QuestItemKind.item(QuestItemKind.StoryCipher).copy(id = 900L)

  /** Открыть карточку плаща в сумке. */
  private def openCard(state: InventoryState, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${cloakItem.id}"}""")), r)

  override def spec = suite("Основная линия: «Метка под меткой»")(

    test("первая зацепка: после «Встать на ноги» — на 5-м исследовании уровня; не засчитано или глава начата — нет") {
      def explore(d: TestHeroDao) =
        for {
          r   <- TestRenderer.make
          sch <- TestScheduler.make
          c   <- content
          res <- DungeonState(d, TestInventoryRepository.withItems(Nil), sch, c).action(testUser, tap("FindEvent"), r)
        } yield res
      val fresh = LoreData(firstSteps = true, firstStepsDone = true)
      for {
        // билет пула на каждый осмотр (50 — находка); редкий бросок мимо
        _      <- TestRandom.feedInts(List.fill(9)(50): _*)
        _      <- TestRandom.feedDoubles(List.fill(9)(0.9): _*)
        d      <- dao(hero(dungeonLevel = 1), cityDone, lore = fresh)
        first4 <- ZIO.foreach(1 to 4)(_ => explore(d))
        fifth  <- explore(d)
        passes <- d.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).map(_.cluePasses))
        notYet <- dao(hero(), cityDone, lore = LoreData(firstSteps = true)).flatMap(explore)
        taken  <- dao(hero(), chapter(1)).flatMap(explore)
      } yield assertTrue(first4.forall(_ == StateType.FoundItem) && fifth == StateType.StoryCloak) &&
              assertTrue(passes.contains(StoryLine.FirstCluePasses)) &&
              assertTrue(List(notYet, taken).forall(_ == StateType.FoundItem))
    },

    test("находка: плащ в сумке, глава на первом шаге и строка «новое задание»; второй раз — пусто") {
      for {
        d     <- dao(hero(), cityDone)
        inv    = TestInventoryRepository.withItems(Nil)
        r     <- TestRenderer.make
        c     <- content
        state  = StoryCloakState(d, inv, TestItemRepository.make, c)
        _     <- state.enter(testUser, r)
        found <- r.sentScreens.map(_.last)
        q     <- questOf(d)
        r2    <- TestRenderer.make
        _     <- state.enter(testUser, r2)
        again <- texts(r2)
        out   <- state.action(testUser, tap("CloakOn"), r2)
      } yield assertTrue(found.text.contains("чёрный плащ") && found.text.contains("📜 Новое задание: «Метка под меткой»") &&
                found.text.contains("показать чёрный плащ Ришелье") && found.choices.map(_.id) == List("CloakOn")) &&
              assertTrue(inv.snapshot.count(_.questItem.contains(QuestItemKind.StoryCloak)) == 1) &&
              assertTrue(StoryMarkQuest.toRichelieu(q) && StoryLine.active(q).contains(NpcQuest.StoryMark)) &&
              assertTrue(again.contains("У изгороди пусто") && out == StateType.Dungeon)
    },

    test("глава открыта, как только «Встать на ноги» засчитано, — уровень героя не важен; закрытая и взятая — больше не предлагаются") {
      val done = chapter(3).finish(NpcQuest.StoryMark)
      assertTrue(StoryLine.startable(cityDone, LoreData(firstSteps = true)).isEmpty) &&
      assertTrue(StoryLine.startable(cityDone, ready).map(_.quest).contains(NpcQuest.StoryMark)) &&
      assertTrue(!StoryLine.startable(done, ready).exists(_.quest == NpcQuest.StoryMark) && StoryLine.startable(chapter(1), ready).isEmpty)
    },

    test("Ришелье: кнопка — только на первом шаге и с плащом при герое; спорол метку — знак Братства, дальше к Горну") {
      for {
        empty <- dao(hero(), chapter(1))
        r0    <- TestRenderer.make
        c     <- content
        _     <- MerchantState(empty, TestInventoryRepository.withItems(Nil), TestItemRepository.make, c).enter(testUser, r0)
        none  <- r0.sentScreens.map(_.last)
        d     <- dao(hero(), chapter(1))
        r     <- TestRenderer.make
        state  = MerchantState(d, TestInventoryRepository.withItems(List(cloakItem)), TestItemRepository.make, c)
        _     <- state.enter(testUser, r)
        menu  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("RichStory"), r)
        said  <- texts(r)
        after <- r.sentScreens.map(_.last)
        q     <- questOf(d)
      } yield assertTrue(!none.choices.exists(_.id == "RichStory") && menu.choices.exists(_.id == "RichStory")) &&
              assertTrue(said.contains("круг, перечёркнутый клинком") && said.contains("записано у Горна")) &&
              assertTrue(StoryMarkQuest.toHorn(q) && !after.choices.exists(_.id == "RichStory"))
    },

    test("Горн: кнопка — только на втором шаге и с плащом при герое; номер с метки — Ульф Сорока, дальше тайник") {
      for {
        early <- dao(hero(), chapter(1))
        r0    <- TestRenderer.make
        c     <- content
        _     <- MasterHornState(early, TestInventoryRepository.withItems(List(cloakItem)), c).enter(testUser, r0)
        none  <- r0.sentScreens.map(_.last)
        d     <- dao(hero(), chapter(2))
        r     <- TestRenderer.make
        state  = MasterHornState(d, TestInventoryRepository.withItems(List(cloakItem)), c)
        _     <- state.enter(testUser, r)
        menu  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("HornStory"), r)
        said  <- texts(r)
        q     <- questOf(d)
      } yield assertTrue(!none.choices.exists(_.id == "HornStory") && menu.choices.exists(_.id == "HornStory")) &&
              assertTrue(said.contains("Ульф Сорока") && said.contains("тайники у изгороди")) &&
              assertTrue(StoryMarkQuest.toCache(q))
    },

    test("карточка плаща ведёт: текст по шагу; «Искать тайник» — с третьего шага, только в лабиринте и с 5-го поворота") {
      def card(step: Int, from: StateType, lvl: Int = 5) =
        for {
          d     <- dao(hero(dungeonLevel = lvl), chapter(step))
          _     <- d.writeReturnState(userId, Some(from))
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(List(cloakItem)), TestItemRepository.make, c)
          _     <- openCard(state, r)
          shown <- r.sentScreens.map(_.last)
          go    <- state.action(testUser, tap("StoryCacheGo"), r)
          said  <- texts(r)
        } yield (shown, go, said)
      for {
        first   <- card(1, StateType.Dungeon)
        third   <- card(3, StateType.Dungeon)
        city    <- card(3, StateType.GlobalMap)
        shallow <- card(3, StateType.Dungeon, lvl = 4)
      } yield assertTrue(first._1.text.contains("Чёрный плащ") && first._1.text.contains("Ришелье") &&
                !first._1.choices.exists(_.id == "StoryCacheGo") && first._2 != StateType.StoryCache) &&
              assertTrue(third._1.text.contains("Ульф Сорока") && third._1.choices.exists(_.id == "StoryCacheGo")) &&
              assertTrue(third._2 == StateType.StoryCache) &&
              assertTrue(city._2 == StateType.Inventory && city._3.contains("только в лабиринте")) &&
              assertTrue(shallow._2 == StateType.Inventory && shallow._3.contains("ближе 5-го поворота"))
    },

    test("тайник: сложность и две кнопки; «Отбить тайник» — бой с тремя мечеными зверями уровня задания (не героя), без бегства, добыча вернёт сюда") {
      for {
        d      <- dao(hero(lvl = 7L), chapter(3))
        r      <- TestRenderer.make
        c      <- content
        state   = StoryCacheState(d, TestInventoryRepository.withItems(List(cloakItem)), TestItemRepository.make, c)
        _      <- state.enter(testUser, r)
        lair   <- r.sentScreens.map(_.last)
        _      <- TestRandom.feedLongs(50L, 50L, 50L)
        res    <- state.action(testUser, tap("CacheFight"), r)
        battle <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot   <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(lair.text.contains("Сорока прятал хорошо") && lair.text.contains("Сложность")) &&
              assertTrue(lair.choices.map(_.id) == List("CacheFight", "CacheLeave")) &&
              assertTrue(res == StateType.Battle) &&
              assertTrue(battle.exists(b => b.story.contains(StoryMarkQuest.FightStory) && b.cantFlee)) &&
              assertTrue(battle.exists(b => b.monsterRace == Race.Animal.entryName && b.monsterLvl == StoryMarkQuest.Level.toLong &&
                b.group.others.size == 2 && b.group.others.forall(s => s.lvl == StoryMarkQuest.Level.toLong && s.marked) && b.monsterMarked)) &&
              assertTrue(lair.text.contains(pangea.model.quest.Difficulty.render(StoryMarkQuest.Level))) &&
              assertTrue(loot.exists(l => l.returnState.contains(StateType.StoryCache) &&
                l.eventData.flatMap(_.as[StoryCacheState.Won].toOption).exists(_.won))) &&
              assertTrue(BattleState.RaidStories.contains(StoryMarkQuest.FightStory))
    },

    test("настоящий бой у тайника: добыча как за обычный бой, «Продолжить» — назад к тайнику") {
      val weak = Monster(0L, 5L, Race.Animal, Rarity.Common,
        FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      val strong = hero().copy(fightStats = FightStats(atk = 500, hp = 2000, armor = 0, defence = 0,
        evasion = 9999, accuracy = 9999, energy = 0))
      for {
        d    <- dao(strong, chapter(3))
        _    <- d.writeActiveBattle(userId, SoloPveBattle.fromGroup(List(weak), strong, Nil, squad = false)
                  .copy(story = Some(StoryMarkQuest.FightStory)).asJson)
        _    <- d.writeSceneData(userId, LootData(Nil, Nil, returnState = Some(StateType.StoryCache),
                  eventData = Some(StoryCacheState.WonMark.asJson)).asJson)
        inv   = TestInventoryRepository.accepting
        r    <- TestRenderer.make
        c    <- content
        won  <- BattleState(d, inv, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
        h    <- heroOf(d)
        j    <- TestJournal.make
        back <- LootState(d, inv, TestItemRepository.make, j, c).action(testUser, tap("Continue"), r)
        mark <- d.readSceneData(userId).map(_.flatMap(_.as[StoryCacheState.Won].toOption))
      } yield assertTrue(won == StateType.Loot && h.exp > 0L && back == StateType.StoryCache && mark.exists(_.won))
    },

    test("тайник отбит: плащ уходит, следующая зацепка — шифровка — в сумке, заначка Сороки, глава закрыта, глава 2 висит; подпись «Э.» — по тому, кто жив") {
      def open(branch: Option[String]) =
        for {
          d    <- dao(hero(lvl = 6L), chapter(3), lore = LoreData(prologueBranch = branch))
          _    <- d.writeSceneData(userId, StoryCacheState.WonMark.asJson)
          inv   = TestInventoryRepository.withItems(List(cloakItem))
          r    <- TestRenderer.make
          c    <- content
          _    <- StoryCacheState(d, inv, TestItemRepository.make, c).enter(testUser, r)
          said <- texts(r)
          last <- r.sentScreens.map(_.last)
          q    <- questOf(d)
          h    <- heroOf(d)
          sd   <- d.readSceneData(userId)
        } yield (said, last, q, h, inv.snapshot, sd)
      for {
        human <- open(Some("Human"))
        elf   <- open(Some("Elf"))
        orc   <- open(Some("Orc"))
        none  <- open(None)
        (said, last, q, h, bag, sd) = human
      } yield assertTrue(said.contains("вощёный футляр курьера") && said.contains("Нужен живым") && said.contains("— Э.")) &&
              assertTrue(bag.exists(_.questItem.contains(QuestItemKind.StoryCipher)) &&
                !bag.exists(_.questItem.contains(QuestItemKind.StoryCloak))) &&
              assertTrue(h.silver == 100L + StoryMarkQuest.Level * StoryMarkQuest.SilverPerLvl && h.exp > 0L) &&
              assertTrue(q.isDone(NpcQuest.StoryMark) && StoryLine.active(q).contains(NpcQuest.StoryPowder) && sd.forall(_.isNull)) &&
              assertTrue(said.contains("Квест «Метка под меткой» завершён") && last.choices.map(_.id) == List("CacheLeave")) &&
              assertTrue(said.contains("📜 Новое задание: «Порох у причала»") && said.contains("дождаться, когда груз Братства придёт в порт")) &&
              // кто пережил пролог: человеком был Элиасом — жив Элорин; эльфом — наоборот; иначе — оба
              assertTrue(said.contains("значит, Элорин") && elf._1.contains("значит, Элиас") &&
                orc._1.contains("Взрыв пережили оба") && none._1.contains("Взрыв пережили оба"))
    },

    test("тайник уже отбит — пусто, кнопка в лабиринт; «Уйти» — в лабиринт") {
      for {
        d     <- dao(hero(), chapter(3).finish(NpcQuest.StoryMark))
        r     <- TestRenderer.make
        c     <- content
        state  = StoryCacheState(d, TestInventoryRepository.withItems(Nil), TestItemRepository.make, c)
        _     <- state.enter(testUser, r)
        gone  <- r.sentScreens.map(_.last)
        out   <- state.action(testUser, tap("CacheLeave"), r)
      } yield assertTrue(gone.text.contains("Тайник Сороки пуст") && out == StateType.Dungeon)
    },

    test("смерть у тайника: проснуться в лабиринте, глава на том же шаге") {
      val mob = Monster(0L, 5L, Race.Animal, Rarity.Uncommon,
        FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      for {
        d     <- dao(hero().copy(exp = 50L), chapter(3))
        _     <- d.writeActiveBattle(userId, SoloPveBattle.from(mob, hero()).copy(story = Some(StoryMarkQuest.FightStory)).asJson)
        r     <- TestRenderer.make
        c     <- content
        _     <- TestRandom.feedInts(1)
        _     <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        scene <- d.readSceneData(userId)
        q     <- questOf(d)
      } yield assertTrue(scene.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("story.storyMark.cache.death")))) &&
              assertTrue(scene.exists(_.hcursor.downField("wakeTo").focus.forall(_.isNull))) &&
              assertTrue(StoryMarkQuest.toCache(q))
    },

    test("журнал: глава — сюжетным заданием со своим шагом, среди заданий горожан её нет; после неё висит глава 2; карточка шифровки — с текстом") {
      val hung = chapter(3).finish(NpcQuest.StoryMark).take(NpcQuest.StoryPowder, 0L)
      for {
        c    <- content
        text  = QuestJournal.render(c, hero(), ready, chapter(2), DailyQuests.empty, None, 0L, house)
        done  = QuestJournal.render(c, hero(), ready, hung, DailyQuests.empty, None, 0L, house)
        card  = InventoryState.questText(c, cipherItem, NpcQuests.empty, 6L)
        wait  = InventoryState.questText(c, cipherItem, hung, 6L)
      } yield assertTrue(text.contains("📌 «Метка под меткой»") && text.contains("показать споротую метку Мастеру Горну")) &&
              assertTrue(!text.contains("🧭 «Метка под меткой»") && !done.contains("Метка под меткой")) &&
              assertTrue(done.contains("📌 «Порох у причала»") && done.contains("откроется с одним из ближайших обновлений") &&
                !done.contains("🧭 «Порох у причала»")) &&
              assertTrue(card.contains("Шифровка Братства") && card.contains("Нужен живым") && !card.contains("🔜")) &&
              assertTrue(wait.contains("🔜 Глава «Порох у причала» откроется"))
    }
  )
}
