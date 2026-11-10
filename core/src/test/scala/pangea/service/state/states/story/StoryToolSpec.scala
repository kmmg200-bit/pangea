package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Achievement, Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests, QuestOutcome}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.parcel.Parcels
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryLine, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Основная линия, Рубеж I «Чьё ты орудие»: записка «Э.» ведёт на старое
  * кладбище — встреча с тем, кто пережил пролог, бой с ячейкой и выбор пути:
  * Тень, Щит или сам по себе. У пути — своё достижение, знак в сумке и строка
  * «как и вы»; путь записан. */
object StoryToolSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 25L): Hero = TestFixtures.hero(userId).copy(lvl = lvl, silver = 100L)

  /** Главы 1–5 закрыты. */
  private val afterFace: NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    StoryPowderQuest.finish(base, WardenFate.Killed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = SojkaFate.Broke.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
  }

  /** Рубеж на шаге `step`. */
  private def tool(step: Int): NpcQuests = StoryToolQuest.step(afterFace.take(NpcQuest.StoryTool, 0L), step)

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

  private def noteItem: Item = QuestItemKind.item(QuestItemKind.StoryNote).copy(id = 906L)

  private def openCard(state: InventoryState, item: Item, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${item.id}"}""")), r)

  /** Бой на кладбище: кто в нём стоит. */
  private def battleOf(branch: Option[String]) =
    for {
      d      <- dao(hero(), tool(1), branch)
      c      <- content
      r      <- TestRenderer.make
      _      <- TestRandom.feedLongs(50L, 50L, 50L, 50L)
      res    <- StoryGraveyardState(d, TestInventoryRepository.withItems(List(noteItem)), TestItemRepository.make, c)
                  .action(testUser, tap("GraveFight"), r)
      battle <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      loot   <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
    } yield (res, battle, loot)

  override def spec = suite("Основная линия: Рубеж I «Чьё ты орудие»")(

    test("карточка записки: сразу после главы 5 — на кладбище, сложность задания; с карточки из города рубеж берётся, из лабиринта — отказ") {
      for {
        c       <- content
        d       <- dao(hero(1L), afterFace)
        _       <- d.writeReturnState(userId, Some(StateType.Dungeon))
        r       <- TestRenderer.make
        card     = InventoryState(d, TestInventoryRepository.withItems(List(noteItem)), TestItemRepository.make, c)
        _       <- openCard(card, noteItem, r)
        shown   <- r.sentScreens.map(_.last)
        deep    <- card.action(testUser, tap("StoryGraveyardGo"), r)
        q0      <- questOf(d)
        _       <- d.writeReturnState(userId, Some(StateType.GlobalMap))
        go      <- card.action(testUser, tap("StoryGraveyardGo"), r)
        said    <- texts(r)
        q       <- questOf(d)
      } yield assertTrue(shown.text.contains("Старое кладбище") && shown.text.contains(Difficulty.render(StoryToolQuest.Level)) &&
                shown.choices.exists(_.id == "StoryGraveyardGo")) &&
              assertTrue(deep == StateType.Inventory && !q0.isTaken(NpcQuest.StoryTool)) &&
              assertTrue(go == StateType.StoryGraveyard && said.contains("📜 Новое задание: «Чьё ты орудие»") &&
                StoryToolQuest.toGraveyard(q) && StoryLine.active(q).contains(NpcQuest.StoryTool))
    },

    test("встреча: тот, кто пережил пролог, и его сделка; бой — без бегства, уровня задания; один — мифический при троих, двое — редкие") {
      def meet(branch: Option[String]) =
        for {
          d     <- dao(hero(), tool(1), branch)
          r     <- TestRenderer.make
          c     <- content
          _     <- StoryGraveyardState(d, TestInventoryRepository.withItems(List(noteItem)), TestItemRepository.make, c).enter(testUser, r)
          shown <- r.sentScreens.map(_.last)
        } yield shown
      for {
        elorin <- meet(Some("Human"))
        elias  <- meet(Some("Elf"))
        both   <- meet(None)
        solo   <- battleOf(Some("Human"))
        pair   <- battleOf(None)
      } yield assertTrue(elorin.text.contains("Я записал тебя в расходы") && elias.text.contains("Я тебя в шахту тащил") &&
                both.text.contains("Мы спорили, ты это или нет")) &&
              assertTrue(elorin.text.contains(Difficulty.render(StoryToolQuest.Level)) &&
                elorin.choices.map(_.id) == List("GraveFight", "GraveLeave")) &&
              assertTrue(solo._1 == StateType.Battle && solo._2.exists(b => b.story.contains(StoryToolQuest.FightStory) &&
                b.cantFlee && b.monsterName.contains("Элорин") && b.monsterRarity == Rarity.Mythical.entryName &&
                b.monsterLvl == StoryToolQuest.Level.toLong && b.group.others.size == 3)) &&
              assertTrue(pair._2.exists(b => b.monsterName.contains("Элорин") && b.monsterRarity == Rarity.Rare.entryName &&
                b.group.others.size == 3 && b.group.others.exists(_.customName.contains("Элиас")))) &&
              assertTrue(solo._3.exists(_.returnState.contains(StateType.StoryGraveyard)) &&
                BattleState.RaidStories.contains(StoryToolQuest.FightStory))
    },

    test("после боя — вопрос рубежа и три пути; путь закрывает рубеж: записка уходит, знак пути, серебро, достижение, исход записан") {
      def decide(path: Path, action: String, branch: Option[String]) =
        for {
          d     <- dao(hero(), tool(1), branch)
          inv    = TestInventoryRepository.withItems(List(noteItem))
          o     <- TestQuestOutcomeDao.make
          r     <- TestRenderer.make
          c     <- content
          st     = StoryGraveyardState(d, inv, TestItemRepository.make, c, Some(o))
          _     <- d.writeSceneData(userId, StoryGraveyardState.WonMark.asJson)
          _     <- st.enter(testUser, r)
          ask   <- r.sentScreens.map(_.last)
          _     <- st.action(testUser, tap(action), r)
          said  <- texts(r)
          q     <- questOf(d)
          h     <- heroOf(d)
          mine  <- o.of(h.id, NpcQuest.StoryTool)
        } yield (ask, said, q, h, inv.snapshot, mine, path)
      for {
        shadow <- decide(Path.Shadow, "GraveShadow", Some("Human"))
        shield <- decide(Path.Shield, "GraveShield", Some("Elf"))
        alone  <- decide(Path.Alone, "GraveAlone", None)
      } yield assertTrue(shadow._1.choices.map(_.id) == List("GraveShadow", "GraveShield", "GraveAlone") &&
                shadow._1.text.contains("Чьё ты орудие, Лео?") && shadow._1.text.contains("Предложение в силе")) &&
              assertTrue(shadow._2.contains("Знак своего") && shadow._2.contains("Знака они не дождутся") &&
                shield._2.contains("Свои не бьют ножом под рёбра") && shield._2.contains("Теперь я — его щит") &&
                alone._2.contains("Две мести") && alone._2.contains("Ничьё — своё")) &&
              assertTrue(shield._2.contains("Под вечер следующего дня по приказу Юноны") &&
                alone._2.contains("Её я тоже беру себе") && !shadow._2.contains("Под вечер следующего дня")) &&
              assertTrue(!shadow._5.exists(_.questItem.contains(QuestItemKind.StoryShadowRing))) &&
              assertTrue(List(shadow, shield, alone).forall { case (_, said, q, h, bag, mine, path) =>
                said.contains("Рубеж «Чьё ты орудие» пройден") && said.contains(path.achievement.title) &&
                  said.contains("Вы — среди первых, кто закончил «Чьё ты орудие»") &&
                  q.isDone(NpcQuest.StoryTool) && StoryToolQuest.choice(q).contains(path) &&
                  path.token.forall(t => bag.exists(_.questItem.contains(t))) && !bag.exists(_.questItem.contains(QuestItemKind.StoryNote)) &&
                  bag.filter(_.isStoryGear).map(_.name) == path.gear.map(_.name) &&
                  bag.filter(_.isStoryGear).forall(i => i.lvl == StoryToolQuest.GearLvl && i.rarity == StoryToolQuest.GearRarity) &&
                  bag.filter(_.isStoryGear).map(_.itemType) == path.gear.map(_.itemType) &&
                  said.contains(s"🎁 ★ [Ур.25] ${path.gear.map(_.name).headOption.getOrElse("")}") &&
                  h.silver == 100L + StoryToolQuest.silver && h.exp > 0L && h.hasAchievement(path.achievement) &&
                  mine.contains(path.outcome) && StoryLine.active(q).isEmpty
              })
    },

    test("снаряжение пути не влезло в сумку — уходит посылкой, на почте не сгорает; знак пути — в сумку всё равно") {
      val inv = TestInventoryRepository.full
      inv.put(noteItem)
      for {
        d     <- dao(hero(), tool(1), None)
        o     <- TestQuestOutcomeDao.make
        r     <- TestRenderer.make
        c     <- content
        post   = TestParcelDao.empty
        st     = StoryGraveyardState(d, inv, TestItemRepository.make, c, Some(o), Some(Parcels(post, TestBankRepository.empty, c)))
        _     <- d.writeSceneData(userId, StoryGraveyardState.WonMark.asJson)
        _     <- st.enter(testUser, r)
        _     <- st.action(testUser, tap("GraveShield"), r)
        said  <- texts(r)
      } yield assertTrue(post.snapshot.map(_.item.name) == Path.Shield.gear.map(_.name) && post.snapshot.forall(_.item.isStoryGear)) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.StoryCouncilWrit)) && !inv.snapshot.exists(_.isStoryGear)) &&
              assertTrue(said.contains("Сюжетное на почте не сгорает") && said.contains("(совет Кинэта)"))
    },

    test("достижения путей — свой стат: Тень — уклонение, Щит — защита, сам по себе — атака; рубежи их не отнимают") {
      val h = hero()
      assertTrue(Achievement.evasionPct(h.withAchievement(Achievement.ShadowPath)) == 5L &&
        Achievement.defencePct(h.withAchievement(Achievement.ShieldPath)) == 5L &&
        Achievement.attackPct(h.withAchievement(Achievement.OwnPath)) == 5L &&
        Achievement.evasionPct(h) == 0L && Achievement.defencePct(h) == 0L && Achievement.attackPct(h) == 0L) &&
        assertTrue(Achievement.ShadowPath.bonusLine == "+5% к уклонению" && Achievement.OwnPath.title == "Ничьё орудие")
    },

    test("десятый, кто выбрал путь, видит проценты; смерть на кладбище — проснуться в городе; журнал — рубеж сюжетным заданием") {
      val mob = Monster(0L, 25L, Race.Human, Rarity.Uncommon,
        FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      for {
        d     <- dao(hero(), tool(2))
        o     <- TestQuestOutcomeDao.make
        _     <- o.seed(QuestOutcome.ToolShadow -> 3, QuestOutcome.ToolShield -> 4, QuestOutcome.ToolAlone -> 2)
        r     <- TestRenderer.make
        c     <- content
        _     <- StoryGraveyardState(d, TestInventoryRepository.withItems(List(noteItem)), TestItemRepository.make, c, Some(o))
                   .action(testUser, tap("GraveShadow"), r)
        said  <- texts(r)
        dead  <- dao(hero().copy(exp = 50L), tool(1))
        _     <- dead.writeActiveBattle(userId, SoloPveBattle.from(mob, hero()).copy(story = Some(StoryToolQuest.FightStory)).asJson)
        r2    <- TestRenderer.make
        _     <- TestRandom.feedInts(1)
        _     <- DeathState(dead, TestInventoryRepository.withItems(Nil), c).enter(testUser, r2)
        scene <- dead.readSceneData(userId)
        text   = QuestJournal.render(c, hero(), lore(Some("Human")), tool(1), DailyQuests.empty, None, 0L)
      } yield assertTrue(said.contains(List(
                "📊 Как закончили «Чьё ты орудие» герои Пангеи:",
                "• 40% выбрали путь Тени — как и вы",
                "• 40% выбрали путь Щита",
                "• 20% пошли сами по себе").mkString("\n"))) &&
              assertTrue(scene.exists(_.hcursor.get[String]("wakeTo").toOption.contains("GlobalMap"))) &&
              assertTrue(scene.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("story.storyTool.graveyard.death")))) &&
              assertTrue(text.contains("📌 «Чьё ты орудие»") && text.contains("старое кладбище"))
    }
  )
}
