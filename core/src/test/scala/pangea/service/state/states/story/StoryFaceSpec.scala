package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.guild.MasterHornState
import pangea.service.state.states.home.HomeState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryFaceQuest, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Основная линия, глава 5 «Лицо из прошлого»: ключ от архива ведёт в архив
  * канцелярии (рука на полях — того, кто пережил пролог), в переплётную днём
  * (лицо из прошлого) и ночью (ячейка, копии списка), к Горну и к Юноне; в
  * конце — записка «Э.», зацепка Рубежа I. */
object StoryFaceSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 21L): Hero = TestFixtures.hero(userId).copy(lvl = lvl, silver = 100L)

  /** Главы 1–4 закрыты; с Сойкой — как скажут. */
  private def afterList(sojka: SojkaFate = SojkaFate.Forgave): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    StoryPowderQuest.finish(base, WardenFate.Handed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList)
      .update(NpcQuest.StoryList)(_.copy(counter = sojka.n))
  }

  /** Глава 5 на шаге `step`. */
  private def face(step: Int, sojka: SojkaFate = SojkaFate.Forgave): NpcQuests =
    StoryFaceQuest.step(afterList(sojka).take(NpcQuest.StoryFace, 0L), step)

  /** Пролог: Human — жив Элорин, Elf — жив Элиас, без записи — оба. */
  private def lore(branch: Option[String]): LoreData =
    LoreData(firstSteps = true, firstStepsDone = true, cluePasses = 5, prologueBranch = branch)

  private def dao(h: Hero, q: NpcQuests, branch: Option[String] = Some("Human")) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- d.writeLoreData(userId, lore(branch).asJson)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def keyItem: Item    = QuestItemKind.item(QuestItemKind.StoryArchiveKey).copy(id = 904L)
  private def copiesItem: Item = QuestItemKind.item(QuestItemKind.StoryCopies).copy(id = 905L)

  private def openCard(state: InventoryState, item: Item, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${item.id}"}""")), r)

  override def spec = suite("Основная линия: «Лицо из прошлого»")(

    test("карточка ключа: сразу после главы 4 — в архив, на любом уровне, сложность задания; дальше — в переплётную, к Горну, к Юноне") {
      def card(lvl: Long, q: NpcQuests) =
        for {
          d     <- dao(hero(lvl), q)
          _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(List(keyItem)), TestItemRepository.make, c)
          _     <- openCard(state, keyItem, r)
          shown <- r.sentScreens.map(_.last)
        } yield shown
      for {
        low   <- card(1L, afterList())
        day   <- card(21L, face(2))
        night <- card(21L, face(3))
        horn  <- card(21L, face(4))
        june  <- card(21L, face(5))
      } yield assertTrue(low.text.contains("Архив канцелярии") && low.text.contains(Difficulty.render(StoryFaceQuest.Level)) &&
                low.choices.exists(_.id == "StoryArchiveGo")) &&
              assertTrue(day.choices.exists(_.id == "StoryBinderyGo") && night.choices.exists(_.id == "StoryBinderyGo") &&
                night.text.contains("Ночью их стерегут")) &&
              assertTrue(horn.text.contains("Мастеру Горну") && !horn.choices.exists(_.id.startsWith("Story")) &&
                june.text.contains("Юноне"))
    },

    test("архив: с карточки из города — глава берётся; из лабиринта — отказ; рука на полях — того, кто пережил пролог") {
      def archive(branch: Option[String]) =
        for {
          d    <- dao(hero(), afterList(), branch)
          _    <- d.writeReturnState(userId, Some(StateType.GlobalMap))
          r    <- TestRenderer.make
          c    <- content
          card  = InventoryState(d, TestInventoryRepository.withItems(List(keyItem)), TestItemRepository.make, c)
          _    <- openCard(card, keyItem, r)
          go   <- card.action(testUser, tap("StoryArchiveGo"), r)
          _    <- StoryArchiveState(d, c).enter(testUser, r)
          said <- texts(r)
          q    <- questOf(d)
        } yield (go, said, q)
      for {
        c        <- content
        deep     <- dao(hero(), afterList())
        _        <- deep.writeReturnState(userId, Some(StateType.Dungeon))
        r0       <- TestRenderer.make
        st0       = InventoryState(deep, TestInventoryRepository.withItems(List(keyItem)), TestItemRepository.make, c)
        _        <- openCard(st0, keyItem, r0)
        refused  <- st0.action(testUser, tap("StoryArchiveGo"), r0)
        q0       <- questOf(deep)
        elorin   <- archive(Some("Human"))
        elias    <- archive(Some("Elf"))
        both     <- archive(None)
      } yield assertTrue(refused == StateType.Inventory && !q0.isTaken(NpcQuest.StoryFace)) &&
              assertTrue(elorin._1 == StateType.StoryArchive && elorin._2.contains("📜 Новое задание: «Лицо из прошлого»")) &&
              assertTrue(List(elorin, elias, both).forall { case (_, said, q) =>
                said.contains("Переплётная Брея, у рыбного рынка") && StoryFaceQuest.toFace(q) }) &&
              assertTrue(elorin._2.contains("Так писал Элорин") && elias._2.contains("Так писал Элиас") &&
                both._2.contains("Руки две"))
    },

    test("переплётная днём — лицо из прошлого; ночью — знак Сойки, сложность; бой — старший и трое уровня задания, без бегства") {
      def day(branch: Option[String]) =
        for {
          d    <- dao(hero(), face(2), branch)
          r    <- TestRenderer.make
          c    <- content
          _    <- StoryBinderyState(d, TestInventoryRepository.withItems(List(keyItem)), TestItemRepository.make, c).enter(testUser, r)
          said <- texts(r)
          q    <- questOf(d)
        } yield (said, q)
      def door(sojka: SojkaFate) =
        for {
          d     <- dao(hero(), face(3, sojka))
          r     <- TestRenderer.make
          c     <- content
          _     <- StoryBinderyState(d, TestInventoryRepository.withItems(List(keyItem)), TestItemRepository.make, c).enter(testUser, r)
          shown <- r.sentScreens.map(_.last)
        } yield shown
      for {
        elorin  <- day(Some("Human"))
        elias   <- day(Some("Elf"))
        both    <- day(None)
        forgave <- door(SojkaFate.Forgave)
        broke   <- door(SojkaFate.Broke)
        d       <- dao(hero(24L), face(3))
        c       <- content
        state    = StoryBinderyState(d, TestInventoryRepository.withItems(List(keyItem)), TestItemRepository.make, c)
        r       <- TestRenderer.make
        _       <- TestRandom.feedLongs(50L, 50L, 50L, 50L)
        res     <- state.action(testUser, tap("BinderyFight"), r)
        battle  <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot    <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(elorin._1.contains("Элорин. Живой") && elias._1.contains("Элиас. Живой") && both._1.contains("Живы оба")) &&
              assertTrue(List(elorin, elias, both).forall { case (said, q) => said.contains("в кандалах") && StoryFaceQuest.toRaid(q) }) &&
              assertTrue(forgave.text.contains("возвращает долг") && broke.text.contains("продала меня снова") &&
                forgave.text.contains(Difficulty.render(StoryFaceQuest.Level))) &&
              assertTrue(forgave.choices.map(_.id) == List("BinderyFight", "BinderyLeave") && res == StateType.Battle) &&
              assertTrue(battle.exists(b => b.story.contains(StoryFaceQuest.FightStory) && b.cantFlee &&
                b.monsterName.contains("Старший ячейки") && b.monsterRarity == Rarity.Rare.entryName &&
                b.monsterLvl == StoryFaceQuest.Level.toLong && b.group.others.size == 3)) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.StoryBindery)) &&
                BattleState.RaidStories.contains(StoryFaceQuest.FightStory))
    },

    test("ячейка перебита — копии списка в сумке; Горн узнаёт свои номера (кнопка — только с копиями), дальше — к Юноне") {
      for {
        d      <- dao(hero(), face(3))
        inv     = TestInventoryRepository.withItems(List(keyItem))
        r      <- TestRenderer.make
        c      <- content
        _      <- d.writeSceneData(userId, StoryBinderyState.WonMark.asJson)
        _      <- StoryBinderyState(d, inv, TestItemRepository.make, c).enter(testUser, r)
        found  <- texts(r)
        q      <- questOf(d)
        bare   <- dao(hero(), face(4))
        rb     <- TestRenderer.make
        _      <- MasterHornState(bare, TestInventoryRepository.withItems(List(keyItem)), c).enter(testUser, rb)
        noBtn  <- rb.sentScreens.map(_.last)
        horn    = MasterHornState(d, TestInventoryRepository.withItems(List(keyItem, copiesItem)), c)
        r2     <- TestRenderer.make
        _      <- horn.enter(testUser, r2)
        menu   <- r2.sentScreens.map(_.last)
        _      <- horn.action(testUser, tap("HornFace"), r2)
        said   <- texts(r2)
        q2     <- questOf(d)
      } yield assertTrue(found.contains("Видели у моста. Жив") && StoryFaceQuest.toHorn(q) &&
                inv.snapshot.exists(_.questItem.contains(QuestItemKind.StoryCopies))) &&
              assertTrue(!noBtn.choices.exists(_.id == "HornFace") && menu.choices.exists(_.id == "HornFace")) &&
              assertTrue(said.contains("закрою его учёт сам") && StoryFaceQuest.toJunona(q2))
    },

    test("Юнона: бумага совета, стража будет; ключ и копии уходят, записка «Э.» — в сумке, награда, глава закрыта") {
      def junona(branch: Option[String]) =
        for {
          d     <- dao(hero(), face(5), branch)
          sch   <- TestScheduler.make
          inv    = TestInventoryRepository.withItems(List(keyItem, copiesItem))
          r     <- TestRenderer.make
          c     <- content
          state  = HomeState(d, sch, c, inv, TestItemRepository.make)
          _     <- state.action(testUser, tap("HomeOffice"), r)
          menu  <- r.sentScreens.map(_.last)
          _     <- state.action(testUser, tap("HomeJunonaFace"), r)
          said  <- texts(r)
          q     <- questOf(d)
          h     <- heroOf(d)
        } yield (menu, said, q, h, inv.snapshot)
      for {
        elorin <- junona(Some("Human"))
        elias  <- junona(Some("Elf"))
        both   <- junona(None)
      } yield assertTrue(elorin._1.choices.exists(_.id == "HomeJunonaFace")) &&
              assertTrue(List(elorin, elias, both).forall { case (_, said, q, h, bag) =>
                said.contains("весы и ключ") && said.contains("Старое кладбище за городской стеной") &&
                  said.contains("📜 Квест «Лицо из прошлого» завершён") && q.isDone(NpcQuest.StoryFace) &&
                  bag.exists(_.questItem.contains(QuestItemKind.StoryNote)) &&
                  !bag.exists(_.questItem.contains(QuestItemKind.StoryArchiveKey)) &&
                  !bag.exists(_.questItem.contains(QuestItemKind.StoryCopies)) &&
                  h.silver == 100L + StoryFaceQuest.silver && h.exp > 0L && StoryToolQuest.open(q)
              }) &&
              assertTrue(elorin._2.contains("закроем этот счёт") && elias._2.contains("Не знаю, как ты вылез") &&
                both._2.contains("— Э. и Э.»"))
    },

    test("смерть в переплётной — проснуться в городе; журнал — глава сюжетным заданием") {
      val mob = Monster(0L, 21L, Race.Human, Rarity.Uncommon,
        FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      for {
        c     <- content
        dead  <- dao(hero().copy(exp = 50L), face(3))
        _     <- dead.writeActiveBattle(userId, SoloPveBattle.from(mob, hero()).copy(story = Some(StoryFaceQuest.FightStory)).asJson)
        r     <- TestRenderer.make
        _     <- TestRandom.feedInts(1)
        _     <- DeathState(dead, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        scene <- dead.readSceneData(userId)
        text   = QuestJournal.render(c, hero(), lore(Some("Human")), face(4), DailyQuests.empty, None, 0L)
      } yield assertTrue(scene.exists(_.hcursor.get[String]("wakeTo").toOption.contains("GlobalMap"))) &&
              assertTrue(scene.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("story.storyFace.bindery.death")))) &&
              assertTrue(text.contains("📌 «Лицо из прошлого»") && text.contains("Мастеру Горну"))
    }
  )
}
