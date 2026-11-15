package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.{SceneContent, Screen}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Achievement, Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryCollectorQuest.Deal
import pangea.service.state.StoryFeedQuest.Brand
import pangea.service.state.StoryFurnaceQuest.Fork
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryFeedQuest, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Рубеж II «Чем кормить Клеймо»: третья ночь на дегтярном складе. Гости пути и
  * Сойка с крыши (если её простили), Сборщик и четверо его людей; Сборщик
  * тянет узор из героя. Связанный Сборщик — выбор: накормить, усмирить или
  * приручить Клеймо. У выбора — своё достижение, снаряжение ★ 50-го уровня и
  * строка «как и вы»; глава Братства помнит договор главы 10. Купчая
  * остаётся, глава 11 висит до обновления. */
object StoryFeedSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, state = StateType.GlobalMap).copy(lvl = 50L, silver = 100L)

  /** Главы 1–10 пройдены (путь `path`, Сойка в главе 4 — `sojka`, договор с
    * главой Братства — `deal`), рубеж на шаге `n`. */
  private def feed(path: Path, n: Int, sojka: SojkaFate = SojkaFate.Broke, deal: Deal = Deal.Refuse): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    val face = StoryPowderQuest.finish(base, WardenFate.Killed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = sojka.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
    val done = StoryToolQuest.finish(face.take(NpcQuest.StoryTool, 0L), path)
      .take(NpcQuest.StoryMorning, 0L).finish(NpcQuest.StoryMorning)
      .take(NpcQuest.StoryNewcomer, 0L).finish(NpcQuest.StoryNewcomer)
      .take(NpcQuest.StoryClearing, 0L).finish(NpcQuest.StoryClearing)
      .take(NpcQuest.StoryFurnace, 0L).finish(NpcQuest.StoryFurnace)
      .update(NpcQuest.StoryFurnace)(_.copy(counter = Fork.Broken.n))
      .take(NpcQuest.StoryCollector, 0L).finish(NpcQuest.StoryCollector)
      .update(NpcQuest.StoryCollector)(_.copy(counter = deal.n))
      .take(NpcQuest.StoryFeed, 0L)
    StoryFeedQuest.step(done, n)
  }

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

  private val deed: Item = QuestItemKind.item(QuestItemKind.StoryDeed).copy(id = 987L)

  private def scene(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent, o: Option[TestQuestOutcomeDao] = None) =
    StoryFeedState(d, inv, TestItemRepository.make, c, o)

  private def label(s: Screen, id: String) = s.choices.find(_.id == id).map(_.label)

  /** Карточка купчей из сумки — в городе — и «StoryFeedGo» на ней. */
  private def card(q: NpcQuests) =
    for {
      c     <- content
      d     <- dao(q)
      _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
      r     <- TestRenderer.make
      st     = InventoryState(d, TestInventoryRepository.withItems(List(deed)), TestItemRepository.make, c)
      _     <- st.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${deed.id}"}""")), r)
      shown <- r.sentScreens.map(_.last)
      to    <- st.action(testUser, tap("StoryFeedGo"), r)
    } yield (shown, to)

  override def spec = suite("Рубеж II «Чем кормить Клеймо»")(

    test("карточка купчей: ждать Сборщика, потом — к связанному; сложность рубежа; после рубежа — Акт III с обновлением") {
      for {
        night  <- card(feed(Path.Shadow, 1))
        bound  <- card(feed(Path.Alone, 2))
        passed <- card(feed(Path.Shield, 2).update(NpcQuest.StoryFeed)(_.copy(counter = Brand.Tame.n)).finish(NpcQuest.StoryFeed)
                    .take(NpcQuest.StoryScars, 0L))
      } yield assertTrue(night._1.text.contains("Сборщик придёт на склад на третью ночь") &&
                night._1.text.contains(Difficulty.render(StoryFeedQuest.Level)) &&
                label(night._1, "StoryFeedGo").contains("🌙 Ждать Сборщика") && night._2 == StateType.StoryFeed) &&
              assertTrue(bound._1.text.contains("Решить, чем кормить Клеймо") && label(bound._1, "StoryFeedGo").contains("🔗 К Сборщику") &&
                bound._2 == StateType.StoryFeed) &&
              assertTrue(passed._1.text.contains("Акт III «Шрамы мира» откроется с обновлением 🔜") &&
                !passed._1.text.contains(Difficulty.render(StoryFeedQuest.Level)) && label(passed._1, "StoryFeedGo").isEmpty &&
                passed._2 != StateType.StoryFeed)
    },

    test("ночь на складе: свои по пути, Сойка — если её простили; Сборщик и четверо легендарных, без бегства, узор тянет Сборщик") {
      def fight(q: NpcQuests) =
        for {
          c     <- content
          d     <- dao(q)
          r     <- TestRenderer.make
          st     = scene(d, TestInventoryRepository.withItems(List(deed)), c)
          _     <- st.enter(testUser, r)
          shown <- r.sentScreens.map(_.last)
          res   <- st.action(testUser, tap("FeedFight"), r)
          start <- r.sentScreens.map(_.last.text)
          b     <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
          loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        } yield (shown, res, start, b, loot)
      def allies(b: Option[SoloPveBattle]) =
        b.toList.flatMap(_.group.allies).map(a => (a.kind, a.lvl, a.undead.map(_.name))).sortBy(_._3)
      val lvl = StoryFeedQuest.Level.toLong
      for {
        shadow <- fight(feed(Path.Shadow, 1, sojka = SojkaFate.Forgave))
        shield <- fight(feed(Path.Shield, 1))
        alone  <- fight(feed(Path.Alone, 1, sojka = SojkaFate.Forgave))
      } yield assertTrue(shadow._1.text.contains("Э. сидит на бочке у двери") && shadow._1.text.contains("Сойка на месте") &&
                shield._1.text.contains("Горн устроился в сарае") && !shield._1.text.contains("Сойка на месте") &&
                alone._1.text.contains("Барти принёс сковороду")) &&
              assertTrue(List(shadow, shield, alone).forall { case (shown, res, start, b, loot) =>
                shown.text.contains("моё Клеймо отвечает ему, а не мне") &&
                  shown.text.contains(Difficulty.render(StoryFeedQuest.Level)) &&
                  shown.choices.map(_.id) == List("FeedFight", "FeedLeave") && res == StateType.Battle &&
                  start.contains("Люди Сборщика расходятся вдоль стен") &&
                  b.exists(x => x.cantFlee && x.story.contains(StoryFeedQuest.FightStory) && x.puller.contains("Сборщик") &&
                    x.customName.contains("Сборщик") && x.monsterRace == Race.Human.entryName &&
                    x.monsterRarity == Rarity.Legendary.entryName && x.monsterLvl == lvl && x.group.others.size == 4 &&
                    x.group.others.forall(_.rarity == Rarity.Legendary.entryName) &&
                    x.group.others.forall(!_.customName.contains("Сборщик"))) &&
                  loot.exists(_.returnState.contains(StateType.StoryFeed))
              }) &&
              assertTrue(allies(shadow._4) == List((AllyKind.Sojka, lvl, Some("Сойка")), (AllyKind.BrotherhoodAgent, lvl, Some("Э.")))) &&
              assertTrue(allies(shield._4) == List((AllyKind.GuildMaster, lvl, Some("Горн")), (AllyKind.GuildSeeker, lvl, Some("Сван")),
                (AllyKind.GuildSeeker, lvl, Some("Хильда")))) &&
              assertTrue(allies(alone._4) == List((AllyKind.Informant, lvl, Some("Барти")), (AllyKind.Sojka, lvl, Some("Сойка")))) &&
              assertTrue(BattleState.RaidStories.contains(StoryFeedQuest.FightStory))
    },

    test("после боя — Сборщик связан: «Чем кормить будешь?» и три выбора, рубеж на втором шаге") {
      for {
        c     <- content
        d     <- dao(feed(Path.Shadow, 1))
        _     <- d.writeSceneData(userId, StoryFight.mark(StoryFeedState.WonKey))
        r     <- TestRenderer.make
        _     <- scene(d, TestInventoryRepository.withItems(List(deed)), c).enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
        q     <- questOf(d)
      } yield assertTrue(shown.text.contains("«Ну, — говорит он. — Чем кормить будешь?»") &&
                shown.text.contains("Забрать украденное — Клеймо станет сильнее, а он пустым")) &&
              assertTrue(shown.choices.map(_.id) == List("FeedFeed", "FeedCalm", "FeedTame", "FeedLeave") &&
                label(shown, "FeedFeed").contains("🔥 Накормить: забрать узор") &&
                label(shown, "FeedCalm").contains("🧂 Усмирить: отдать живым") &&
                label(shown, "FeedTame").contains("🔗 Приручить: в подвал")) &&
              assertTrue(StoryFeedQuest.toChoice(q))
    },

    test("выбор закрывает рубеж: достижение, снаряжение ★ 50-го уровня, серебро, «как и вы», глава 11 висит; старик помнит договор") {
      def decide(path: Path, deal: Deal, brand: Brand) =
        for {
          c     <- content
          d     <- dao(feed(path, 2, deal = deal))
          inv    = TestInventoryRepository.withItems(List(deed))
          o     <- TestQuestOutcomeDao.make
          r     <- TestRenderer.make
          _     <- scene(d, inv, c, Some(o)).action(testUser, tap(s"Feed${brand.key.capitalize}"), r)
          said  <- texts(r)
          q     <- questOf(d)
          h     <- heroOf(d)
          mine  <- o.of(h.id, NpcQuest.StoryFeed)
        } yield (brand, said, q, h, inv.snapshot, mine)
      for {
        fed     <- decide(Path.Shadow, Deal.Place, Brand.Feed)
        calmed  <- decide(Path.Alone, Deal.Peace, Brand.Calm)
        council <- decide(Path.Shield, Deal.Place, Brand.Calm)
        tamed   <- decide(Path.Alone, Deal.Refuse, Brand.Tame)
        placed  <- decide(Path.Shadow, Deal.Place, Brand.Calm)
      } yield assertTrue(List(fed, calmed, council, tamed, placed).forall { case (brand, said, q, h, bag, mine) =>
                said.contains("📜 Рубеж «Чем кормить Клеймо» пройден.") && said.contains("📜 Новое задание: «Шрамы мира»") &&
                  said.contains(brand.achievement.title) && q.isDone(NpcQuest.StoryFeed) && StoryFeedQuest.choice(q).contains(brand) &&
                  q.isTaken(NpcQuest.StoryScars) && mine.contains(brand.outcome) && h.hasAchievement(brand.achievement) &&
                  h.silver == 100L + StoryFeedQuest.silver && h.exp > 0L &&
                  bag.exists(_.questItem.contains(QuestItemKind.StoryDeed)) &&
                  bag.filter(_.isStoryGear).map(_.name) == brand.gear.map(_.name) &&
                  bag.filter(_.isStoryGear).map(_.itemType) == brand.gear.map(_.itemType) &&
                  bag.filter(_.isStoryGear).forall(i => i.lvl == StoryFeedQuest.GearLvl && i.rarity == StoryFeedQuest.GearRarity) &&
                  said.contains(s"🎁 ★ [Ур.50] ${brand.gear.map(_.name).headOption.getOrElse("")}")
              }) &&
              assertTrue(fed._2.contains("Клеймо тянет из него узор") && fed._2.contains("«Живой, — говорит он наконец. — Я просил живого»") &&
                fed._2.contains("«Договаривались мы не об этом, — говорит он у двери. — Я запомню»")) &&
              assertTrue(calmed._2.contains("Впервые за много лет оно молчит") && calmed._2.contains("«Живой и целый") &&
                calmed._2.contains("«С этой минуты я вас не знаю»") && !calmed._2.contains("люди Юноны")) &&
              assertTrue(council._2.contains("Утром за ним приходят люди Юноны") && !council._2.contains("старик с палкой") &&
                council._2.contains("«Совету? Договаривались мы не об этом»")) &&
              assertTrue(tamed._2.contains("почему убитый просыпается в убийце") && tamed._2.contains("«Подвал, — говорит он.") &&
                !tamed._2.contains("Договаривались") && !tamed._2.contains("Место за вами")) &&
              assertTrue(placed._2.contains("«Место за вами, — говорит он у двери. — Остальное — как обещал»."))
    },

    test("достижения рубежа — свой стат: накормить — атака, усмирить — броня, приручить — интеллект") {
      val h = hero
      assertTrue(Achievement.attackPct(h.withAchievement(Achievement.FedBrand)) == 5L &&
        Achievement.armorPct(h.withAchievement(Achievement.CalmBrand)) == 5L &&
        Achievement.intPct(h.withAchievement(Achievement.TameBrand)) == 5L &&
        Achievement.attackPct(h) == 0L && Achievement.armorPct(h) == 0L && Achievement.intPct(h) == 0L) &&
      assertTrue(Achievement.FedBrand.title == "Сытое Клеймо" && Achievement.CalmBrand.title == "Тихое Клеймо" &&
        Achievement.TameBrand.title == "Ручное Клеймо" && Achievement.TameBrand.bonusLine == "+5% к интеллекту")
    },

    test("смерть на складе — проснуться в городе, шаг тот же; журнал рубежа; после — «Шрамы мира» висит") {
      for {
        c     <- content
        d     <- dao(feed(Path.Alone, 1))
        fight  = SoloPveBattle.fromGroup(StoryFeedQuest.foes("Сборщик"), hero, List.fill(5)(0L))
                   .copy(story = Some(StoryFeedQuest.FightStory), puller = Some("Сборщик"))
        _     <- d.writeActiveBattle(userId, fight.asJson)
        r     <- TestRenderer.make
        _     <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        wake  <- d.readSceneData(userId)
        q     <- questOf(d)
        line   = (x: NpcQuests) => QuestJournal.render(c, hero, LoreData.empty, x, DailyQuests.empty, None, 0L)
        closed = feed(Path.Alone, 2).finish(NpcQuest.StoryFeed).take(NpcQuest.StoryScars, 0L)
      } yield assertTrue(wake.flatMap(_.hcursor.get[List[String]]("wakeLines").toOption).contains(List("story.storyFeed.death")) &&
                wake.flatMap(_.hcursor.get[StateType]("wakeTo").toOption).contains(StateType.GlobalMap) && StoryFeedQuest.toFight(q)) &&
              assertTrue(line(feed(Path.Shadow, 1)).contains("встретить Сборщика на складе и взять живым") &&
                line(feed(Path.Shield, 2)).contains("решить, чем кормить Клеймо")) &&
              assertTrue(line(closed).contains("📌 «Шрамы мира»") && line(closed).contains("Акт III откроется с обновлением 🔜"))
    }
  )
}
