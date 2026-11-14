package pangea.service.state.states.village

import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.model.battle.{Convoy, SlainMonster, SoloPveBattle}
import pangea.model.caravan.CaravanScene
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.generator.monster.MonsterGenerator
import pangea.model.monster.{Breed, Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.{CaravanAmbush, VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.generator.loot.LootGenerator
import pangea.generator.item.MaterialGenerator
import pangea.model.item.MaterialKind
import pangea.service.state.{NpcQuestLog, QuestJournal, UserAction, VillageLair, Villages}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Деревня» — событие лабиринта: с 20-го поворота, один билет из ста, не
  * выпадает, пока при герое контракт деревни. Деревню можно разграбить
  * (сюжетный бой с жителями и сторожевой башней) или взять контракт
  * деревни — логово разбойников: контракт ведёт к логову, потом назад за
  * платой. */
object VillageSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val Level = 23
  // Задание старосты — на три уровня сложнее деревни (разброс от −1 до +4).
  private val QuestLevel = 26

  private def hero(dungeonLevel: Int = Level): Hero =
    TestFixtures.hero(userId, dungeonLevel = dungeonLevel).copy(lvl = 25L, silver = 100L)

  // Деревня орков, второе название в списке — «Дымный Лог»; рядом третий город
  // списка — Каэлис; плата выпала 1700, главарь назван.
  private val Silver      = 1700L
  private val scene       = VillageScene(Race.Orc, 1, VillageQuest.BanditLair, Level, QuestLevel, Silver, Some("Чёрный Виктор"),
                              leaderRace = Some(Race.Human), city = 2, land = 1)
  private val VillageName = "Дымный Лог"
  private val City        = "Каэлис"

  private def contract(step: Int = VillageContract.Taken): VillageContract = VillageContract.of(scene).copy(step = step)
  private def withContract(step: Int = VillageContract.Taken): NpcQuests = NpcQuests.empty.copy(village = Some(contract(step)))
  private def paper: Item = QuestItemKind.item(QuestItemKind.VillageContract).copy(id = 901L)

  private val content = ZIO.attempt(SceneContent.load())

  private def dao(h: Hero = hero(), q: Option[NpcQuests] = None, sc: Option[Json] = None) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
      _ <- ZIO.foreachDiscard(sc)(d.writeSceneData(userId, _))
    } yield d

  private def village(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    VillageState(d, inv, TestItemRepository.make, c)

  private def lair(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    VillageLairState(d, inv, TestItemRepository.make, c, VillageLair.Bandits)

  private def spiderLair(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    VillageLairState(d, inv, TestItemRepository.make, c, VillageLair.Spiders)

  // Та же деревня, только беда другая — гнездо сетеплётов, главаря нет.
  private val spiderScene = scene.copy(quest = VillageQuest.SpiderNests, leader = None, leaderRace = None)
  private def withSpiders(step: Int = VillageContract.Taken): NpcQuests =
    NpcQuests.empty.copy(village = Some(VillageContract.of(spiderScene).copy(step = step)))

  private def sceneOf(d: TestHeroDao): Task[Option[VillageScene]] =
    d.readSceneData(userId).map(_.flatMap(_.as[VillageScene].toOption))

  private def battleOf(d: TestHeroDao): Task[Option[SoloPveBattle]] =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)

  private def mortal(race: String): Boolean = Race.mortals.exists(_.entryName == race)

  private def wakeLines(sd: Option[Json]): List[String] =
    sd.flatMap(_.hcursor.get[List[String]]("wakeLines").toOption).getOrElse(Nil)

  override def spec = suite("Деревня")(

    test("npc_quests: старая запись без деревни читается, договор пишется и читается, битый договор не роняет остальное") {
      val old    = parse("""{"quests":{"marisa":{"step":2,"counter":0,"since":5,"bonus":false,"done":false}},"recipes":["dustAssembly"],"strayGems":[]}""")
      val broken = parse("""{"quests":{"marisa":{"step":2}},"village":{"quest":"Nope","race":"Орк"}}""")
      // Договор в самом скупом виде: без главаря и шага — шаг первый.
      val bare   = parse("""{"quest":"BanditLair","race":"Орк","name":1,"level":23,"silver":4600}""")
      val full   = withContract(VillageContract.Done)
      assertTrue(old.flatMap(_.as[NpcQuests]).exists(q => q.onStep(NpcQuest.Marisa, 2) && q.recipes == List("dustAssembly") && q.village.isEmpty)) &&
      assertTrue(full.asJson.as[NpcQuests].toOption.contains(full)) &&
      assertTrue(broken.flatMap(_.as[NpcQuests]).exists(q => q.onStep(NpcQuest.Marisa, 2) && q.village.isEmpty)) &&
      assertTrue(bare.flatMap(_.as[VillageContract]).toOption.contains(
        VillageContract(VillageQuest.BanditLair, Race.Orc, 1, 23, 4600L, None, VillageContract.Taken)))
    },

    test("сцена деревни — своими ключами: караван её не читает, она — караван, добыча с ней внутри — не деревня") {
      val caravan = CaravanScene(race = Race.Orc.entryName, guards = 12, towers = 1).asJson
      val routing = LootData(Nil, Nil, returnState = Some(StateType.Village), eventData = Some(scene.asJson)).asJson
      assertTrue(scene.asJson.as[VillageScene].toOption.contains(scene)) &&
      assertTrue(caravan.as[VillageScene].isLeft && scene.asJson.as[CaravanScene].isLeft) &&
      assertTrue(routing.as[VillageScene].isLeft)
    },

    test("пул: деревня — с 15-го уровня, один билет; выпала — с чистого листа; при контракте деревни не выпадает") {
      def roll(q: Option[NpcQuests]) =
        for {
          d         <- dao(TestFixtures.hero(userId, dungeonLevel = 20), q, Some(Json.obj("stale" -> Json.True)))
          renderer  <- TestRenderer.make
          scheduler <- TestScheduler.make
          c         <- content
          _         <- TestRandom.feedInts(StateType.events.indexOf(StateType.Village))
          res       <- DungeonState(d, TestInventoryRepository.accepting, scheduler, c).action(testUser, tap("FindEvent"), renderer)
          sd        <- d.readSceneData(userId)
        } yield (res, sd)
      for {
        free  <- roll(None)
        bound <- roll(Some(withContract()))
      } yield assertTrue(free._1 == StateType.Village && free._2.contains(Json.Null)) &&
              assertTrue(bound._1 != StateType.Village) &&
              assertTrue(StateType.eventsAt(14, 1.0).count(_ == StateType.Village) == 0 &&
                StateType.eventsAt(15, 1.0).count(_ == StateType.Village) == 1)
    },

    test("приход: новая деревня — раса, название из её списка, беда, главарь и сложность; вход ещё раз — та же деревня") {
      for {
        c     <- content
        d     <- dao()
        r     <- TestRenderer.make
        st     = village(d, TestInventoryRepository.withItems(Nil), c)
        _     <- st.enter(testUser, r)
        first <- r.sentScreens.map(_.last)
        s     <- sceneOf(d)
        _     <- st.enter(testUser, r)
        again <- r.sentScreens.map(_.last)
        typed <- st.action(testUser, UserAction("привет", None), r)
        third <- r.sentScreens.map(_.last)
      } yield assertTrue(s.exists(x => x.level == Level && x.step == VillageScene.Arrival &&
                VillageQuest.openAt(Level).contains(x.quest) && x.leader.isDefined == (x.quest == VillageQuest.BanditLair))) &&
              assertTrue(s.exists(x => x.quest != VillageQuest.BanditLair || x.leaderRace.exists(r => Race.mortals.contains(r) &&
                Monster.legendaryNames.get(r).exists(names => x.leader.exists(names.contains))))) &&
              assertTrue(s.exists(x => first.text.contains("«" + Villages.name(c, x.race, x.name) + "»") &&
                Villages.names(c, x.race).contains(Villages.name(c, x.race, x.name)))) &&
              assertTrue(s.exists(x => first.text.contains(c.text(s"village.trouble.${x.quest.key}").trim))) &&
              assertTrue(s.exists(x => Villages.lands(c, x.race).lift(x.land).exists(first.text.contains(_)))) &&
              assertTrue(s.exists(x => x.questLevel >= Level + VillageRates.QuestLevelMin && x.questLevel <= Level + VillageRates.QuestLevelMax)) &&
              assertTrue(s.exists(x => x.silver >= VillageRates.silver(x.questLevel, 80L) && x.silver <= VillageRates.silver(x.questLevel, 120L) &&
                Villages.cities(c).isDefinedAt(x.city))) &&
              assertTrue(s.exists(x => x.doubloons == VillageRates.doubloons(x.questLevel) && x.doubloons > 0L)) &&
              assertTrue(first.text.contains("Сложность: " + Difficulty.render(Level))) &&
              assertTrue(first.choices.map(_.id) == List("VillageHeadman", "VillageRaid", "OpenCharacter", "VillageLeave")) &&
              assertTrue(again.text == first.text && typed == StateType.Village && third.text == first.text)
    },

    test("налёт: подтверждение, потом сюжетный бой — жители деревни, именной, башня в хвосте строя, всё уровня деревни") {
      for {
        c       <- content
        d       <- dao(sc = Some(scene.asJson))
        r       <- TestRenderer.make
        st       = village(d, TestInventoryRepository.withItems(Nil), c)
        ask     <- st.action(testUser, tap("VillageRaid"), r)
        confirm <- r.sentScreens.map(_.last)
        go      <- st.action(testUser, tap("VillageRaidGo"), r)
        b       <- battleOf(d)
        loot    <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        slots    = b.toList.flatMap(x => x.group.others ++ x.group.queue)
        towers   = slots.filter(_.race == Race.Construct.entryName)
      } yield assertTrue(ask == StateType.Village && confirm.choices.map(_.id) == List("VillageRaidGo", "VillageBack")) &&
              assertTrue(go == StateType.Battle) &&
              assertTrue(b.exists(x => x.story.contains(Villages.RaidStory) && x.lootLevel.contains(Level.toLong) &&
                x.monsterRace == Race.Orc.entryName && x.cantFlee)) &&
              // строй, очередь и именной — 23 жителя, да башня
              assertTrue(slots.size + 1 == VillageRates.RaidFront + VillageRates.RaidQueue + 1 + VillageRates.RaidTowers) &&
              assertTrue(towers.size == VillageRates.RaidTowers) &&
              assertTrue(slots.filterNot(_.race == Race.Construct.entryName).forall(s => s.race == Race.Orc.entryName && s.lvl == Level.toLong)) &&
              assertTrue(slots.exists(s => s.rarity == Rarity.Legendary.entryName && s.customName.isDefined)) &&
              assertTrue(loot.exists(l => l.returnState.contains(StateType.Village) &&
                l.eventData.flatMap(_.as[VillageScene].toOption).exists(_.step == VillageScene.AfterRaid)))
    },

    test("налёт выигран: развязка один раз, дальше пепелище; «В лабиринт» стирает сцену") {
      for {
        c   <- content
        d   <- dao(sc = Some(scene.copy(step = VillageScene.AfterRaid).asJson))
        r   <- TestRenderer.make
        st   = village(d, TestInventoryRepository.withItems(Nil), c)
        _   <- st.enter(testUser, r)
        won <- r.sentScreens.map(_.last)
        _   <- st.enter(testUser, r)
        ash <- r.sentScreens.map(_.last)
        out <- st.action(testUser, tap("VillageOut"), r)
        sd  <- d.readSceneData(userId)
      } yield assertTrue(won.text.contains(s"Последний защитник деревни «$VillageName»") && won.choices.map(_.id) == List("VillageOut")) &&
              assertTrue(ash.text == c.text("village.razed")) &&
              assertTrue(out == StateType.Dungeon && sd.contains(Json.Null))
    },

    test("староста: кто он — отдельно, просьба — с главарём, платой и сложностью; «Взяться» — контракт в сумке, договор в журнале") {
      for {
        c     <- content
        d     <- dao(sc = Some(scene.asJson))
        inv    = TestInventoryRepository.accepting
        r     <- TestRenderer.make
        st     = village(d, inv, c)
        _     <- st.action(testUser, tap("VillageHeadman"), r)
        shown <- r.sentScreens
        s1    <- sceneOf(d)
        res   <- st.action(testUser, tap("VillageAccept"), r)
        last  <- r.sentScreens.map(_.last)
        q     <- questOf(d)
        sd    <- d.readSceneData(userId)
      } yield assertTrue(shown.size == 2 && shown.head.text == c.text(s"village.headman.${Race.Orc.entryName}")) &&
              assertTrue(shown(1).text.contains("Верховодит у них Чёрный Виктор") &&
                shown(1).text.contains(s"Последний обоз из города $City") && !shown(1).text.contains("Кинэт") &&
                shown(1).text.contains(s"Плата — $Silver серебра") &&
                shown(1).text.contains("Сложность: " + Difficulty.render(QuestLevel))) &&
              assertTrue(shown(1).choices.map(_.id) == List("VillageAccept", "VillageDecline")) &&
              assertTrue(s1.exists(_.step == VillageScene.Headman)) &&
              assertTrue(res == StateType.Dungeon && sd.contains(Json.Null)) &&
              assertTrue(q.village.contains(contract()) && contract().silver == Silver && contract().city == 2 &&
                contract().level == QuestLevel) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.VillageContract))) &&
              assertTrue(last.text.contains("Застава у старого тракта") && last.text.contains("«Контракт деревни»"))
    },

    test("«Отказаться» у старосты и «Уйти» у входа — в лабиринт без контракта и договора, сцена стёрта") {
      for {
        c    <- content
        r    <- TestRenderer.make
        d1   <- dao(sc = Some(scene.copy(step = VillageScene.Headman).asJson))
        inv1  = TestInventoryRepository.accepting
        no   <- village(d1, inv1, c).action(testUser, tap("VillageDecline"), r)
        q1   <- questOf(d1)
        sd1  <- d1.readSceneData(userId)
        d2   <- dao(sc = Some(scene.asJson))
        away <- village(d2, TestInventoryRepository.accepting, c).action(testUser, tap("VillageLeave"), r)
        sd2  <- d2.readSceneData(userId)
        said <- r.sentScreens.map(_.map(_.text))
      } yield assertTrue(no == StateType.Dungeon && q1.village.isEmpty && inv1.snapshot.isEmpty && sd1.contains(Json.Null)) &&
              assertTrue(away == StateType.Dungeon && sd2.contains(Json.Null)) &&
              assertTrue(said.exists(_.contains("Наёмников на дорогах больше")) && said.exists(_.contains(s"деревня «$VillageName» остаётся за спиной")))
    },

    test("старые кнопки: «Взяться» и «В лабиринт» у входа — та же деревня тем же экраном, ничего не взято") {
      for {
        c     <- content
        d     <- dao(sc = Some(scene.asJson))
        r     <- TestRenderer.make
        st     = village(d, TestInventoryRepository.accepting, c)
        took  <- st.action(testUser, tap("VillageAccept"), r)
        a     <- r.sentScreens.map(_.last)
        out   <- st.action(testUser, tap("VillageOut"), r)
        b     <- r.sentScreens.map(_.last)
        q     <- questOf(d)
        s     <- sceneOf(d)
      } yield assertTrue(took == StateType.Village && out == StateType.Village) &&
              assertTrue(a.choices.map(_.id) == List("VillageHeadman", "VillageRaid", "OpenCharacter", "VillageLeave") && b.text == a.text) &&
              assertTrue(q.village.isEmpty && s.contains(scene))
    },

    test("с контрактом при себе, пока беда не кончилась, староста ждёт") {
      for {
        c    <- content
        d    <- dao(q = Some(withContract()))
        r    <- TestRenderer.make
        _    <- village(d, TestInventoryRepository.withItems(List(paper)), c).enter(testUser, r)
        last <- r.sentScreens.map(_.last)
        q    <- questOf(d)
      } yield assertTrue(last.text.contains("Обозы так и не ходят") && last.choices.map(_.id) == List("VillageOut")) &&
              assertTrue(Villages.toBandits(q))
    },

    test("карточка контракта: сложность, деревня, главарь и плата; «Идти к логову» — из лабиринта с 15-го уровня; сделано — «Вернуться в деревню»") {
      def card(from: StateType, lvl: Int, step: Int) =
        for {
          d     <- dao(hero(lvl), Some(withContract(step)))
          _     <- d.writeReturnState(userId, Some(from))
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(List(paper)), TestItemRepository.make, c)
          _     <- state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${paper.id}"}""")), r)
          shown <- r.sentScreens.map(_.last)
          go    <- state.action(testUser, tap(if (step == VillageContract.Taken) "VillageGo" else "VillageReturn"), r)
          said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
          sd    <- d.readSceneData(userId)
        } yield (shown, go, said, sd)
      for {
        deep    <- card(StateType.Dungeon, 15, VillageContract.Taken)
        city    <- card(StateType.GlobalMap, 15, VillageContract.Taken)
        shallow <- card(StateType.Dungeon, 14, VillageContract.Taken)
        back    <- card(StateType.Dungeon, 15, VillageContract.Done)
      } yield assertTrue(deep._1.text.contains("Сложность: " + Difficulty.render(QuestLevel)) &&
                deep._1.text.contains(s"Деревня орков «$VillageName»") && deep._1.text.contains("верховодит Чёрный Виктор") &&
                deep._1.text.contains(s"Плата — $Silver серебра")) &&
              assertTrue(deep._1.choices.exists(_.id == "VillageGo") && deep._2 == StateType.VillageBandits && deep._4.contains(Json.Null)) &&
              assertTrue(city._2 == StateType.Inventory && city._3.contains("идти туда можно только из него")) &&
              assertTrue(shallow._2 == StateType.Inventory && shallow._3.contains("не ближе 15-го уровня")) &&
              assertTrue(back._1.choices.exists(_.id == "VillageReturn") && back._1.text.contains("Дело сделано") &&
                back._2 == StateType.Village)
    },

    test("логово: банда и главарь у входа; бой — пятнадцать бандитов уровня задания, расы вперемешку, главарь под своим именем, без бегства") {
      for {
        c     <- content
        d     <- dao(q = Some(withContract()))
        r     <- TestRenderer.make
        st     = lair(d, TestInventoryRepository.withItems(List(paper)), c)
        _     <- st.enter(testUser, r)
        intro <- r.sentScreens.map(_.last)
        res   <- st.action(testUser, tap("BanditsFight"), r)
        b     <- battleOf(d)
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        slots  = b.toList.flatMap(x => x.group.others ++ x.group.queue)
      } yield assertTrue(intro.text.contains("сидит Чёрный Виктор") && intro.text.contains("Сложность: " + Difficulty.render(QuestLevel))) &&
              assertTrue(intro.choices.map(_.id) == List("BanditsFight", "OpenCharacter", "BanditsLeave", "BanditsAbandon")) &&
              assertTrue(res == StateType.Battle) &&
              assertTrue(b.exists(x => x.story.contains(Villages.BanditsStory) && x.lootLevel.contains(QuestLevel.toLong) &&
                mortal(x.monsterRace) && x.cantFlee)) &&
              assertTrue(slots.size + 1 == VillageRates.BanditFront + VillageRates.BanditBack + 1) &&
              assertTrue(slots.forall(s => mortal(s.race) && s.lvl == QuestLevel.toLong)) &&
              assertTrue((b.toList.map(_.monsterRace) ++ slots.map(_.race)).distinct.size > 1) &&
              assertTrue(slots.exists(s => s.rarity == Rarity.Legendary.entryName && s.customName.contains("Чёрный Виктор") &&
                s.race == Race.Human.entryName)) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.VillageBandits)))
    },

    test("банда перебита: логово разбито — голова главаря в сумке, дальше к старосте; в логове больше пусто") {
      for {
        c     <- content
        d     <- dao(q = Some(withContract()), sc = Some(VillageLairState.wonMark(VillageLair.Bandits)))
        inv    = TestInventoryRepository.withItems(List(paper))
        r     <- TestRenderer.make
        st     = lair(d, inv, c)
        // сдавшихся и охотников за головами нет (они — VillageBanditsSpec, VillageSurvivorsSpec)
        _     <- TestRandom.feedInts(99, 99)
        _     <- st.enter(testUser, r)
        last  <- r.sentScreens.map(_.last)
        q     <- questOf(d)
        sd    <- d.readSceneData(userId)
        _     <- st.enter(testUser, r)
        again <- r.sentScreens.map(_.last)
      } yield assertTrue(last.text.contains("Чёрный Виктор падает у самого костра") && last.text.contains(s"тюки с клеймом города $City") &&
                last.text.contains("Голову главаря вы заворачиваете в мешковину") &&
                last.text.contains(s"Дорога у деревни «$VillageName» снова свободна") && last.choices.map(_.id) == List("BanditsOut")) &&
              assertTrue(Villages.toPay(q) && sd.contains(Json.Null)) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.BanditHead))) &&
              assertTrue(again.text == c.text("village.bandits.gone"))
    },

    test("плата: староста платит один раз — серебро, опыт, контракт уходит из сумки, договор закрыт; дальше прощание") {
      for {
        c    <- content
        d    <- dao(q = Some(withContract(VillageContract.Done)))
        inv   = TestInventoryRepository.withItems(List(paper))
        r    <- TestRenderer.make
        st    = village(d, inv, c)
        _    <- st.enter(testUser, r)
        paid <- r.sentScreens.map(_.last)
        h    <- d.getHeroByUserId(userId).map(_.get)
        q    <- questOf(d)
        _    <- st.enter(testUser, r)
        bye  <- r.sentScreens.map(_.last)
        h2   <- d.getHeroByUserId(userId).map(_.get)
        out  <- st.action(testUser, tap("VillageOut"), r)
      } yield assertTrue(paid.text.contains("Чёрный Виктор больше не будет кричать на дороге") &&
                paid.text.contains(s"Получено $Silver серебра") &&
                paid.text.contains(s"Получено ${VillageRates.exp(QuestLevel)} опыта") &&
                paid.text.contains(c.text("village.questDone"))) &&
              assertTrue(h.silver == 100L + Silver && h.exp == VillageRates.exp(QuestLevel)) &&
              assertTrue(q.village.isEmpty && inv.snapshot.isEmpty) &&
              assertTrue(bye.text.contains("кивает вам на прощание") && h2.silver == h.silver && h2.exp == h.exp) &&
              assertTrue(out == StateType.Dungeon)
    },

    test("плата с дублонами: староста называет их и платит — серебро, дублоны и опыт; карточка и журнал — с ними же") {
      val rich = NpcQuests.empty.copy(village = Some(contract(VillageContract.Done).copy(doubloons = 3L)))
      for {
        c    <- content
        d    <- dao(q = Some(rich))
        inv   = TestInventoryRepository.withItems(List(paper))
        r    <- TestRenderer.make
        card  = Villages.cardLine(c, rich)
        line  = Villages.journalStep(c, contract(VillageContract.Done).copy(doubloons = 3L))
        _    <- village(d, inv, c).enter(testUser, r)
        paid <- r.sentScreens.map(_.last)
        h    <- d.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(card.contains(s"ждёт вас с платой — $Silver серебра и 3 дублона.") &&
                line.contains(s"за платой — $Silver серебра и 3 дублона")) &&
              assertTrue(paid.text.contains(s"💰 Получено $Silver серебра и 3 дублона.")) &&
              assertTrue(h.silver == 100L + Silver && h.doubloons == hero().doubloons + 3L)
    },

    test("плата словами: серебро, с дублонами — «и N дублон/дублона/дублонов»") {
      for {
        c <- content
      } yield assertTrue(Villages.payLine(c, 350L, 0L) == "350 серебра") &&
              assertTrue(List(1L, 2L, 4L, 5L, 11L, 12L, 21L, 22L, 25L).map(n => Villages.payLine(c, 500L, n)) == List(
                "500 серебра и 1 дублон", "500 серебра и 2 дублона", "500 серебра и 4 дублона", "500 серебра и 5 дублонов",
                "500 серебра и 11 дублонов", "500 серебра и 12 дублонов", "500 серебра и 21 дублон",
                "500 серебра и 22 дублона", "500 серебра и 25 дублонов"))
    },

    test("старые сцена и контракт — без дублонов: обещано было одно серебро; новые пишутся и читаются с ними") {
      val oldScene = parse("""{"villageRace":"Орк","villageName":1,"villageQuest":"BanditLair","villageLevel":23,""" +
        """"villageQuestLevel":26,"villageSilver":1950,"villageStep":"headman"}""")
      val oldDeal  = parse("""{"quest":"BanditLair","race":"Орк","name":1,"level":26,"silver":1950,"step":2}""")
      val fresh    = scene.copy(doubloons = 3L)
      assertTrue(oldScene.flatMap(_.as[VillageScene]).exists(x => x.silver == 1950L && x.doubloons == 0L)) &&
      assertTrue(oldDeal.flatMap(_.as[VillageContract]).exists(x => x.silver == 1950L && x.doubloons == 0L && x.done)) &&
      assertTrue(fresh.asJson.as[VillageScene].toOption.contains(fresh)) &&
      assertTrue(VillageContract.of(fresh).doubloons == 3L &&
        VillageContract.of(fresh).asJson.as[VillageContract].toOption.contains(VillageContract.of(fresh)))
    },

    test("отказ от контракта в логове — с подтверждением: контракт и договор уходят") {
      for {
        c       <- content
        d       <- dao(q = Some(withContract()))
        inv      = TestInventoryRepository.withItems(List(paper))
        r       <- TestRenderer.make
        st       = lair(d, inv, c)
        ask     <- st.action(testUser, tap("BanditsAbandon"), r)
        confirm <- r.sentScreens.map(_.last)
        q0      <- questOf(d)
        done    <- st.action(testUser, tap("BanditsAbandonYes"), r)
        q       <- questOf(d)
      } yield assertTrue(ask == StateType.VillageBandits && confirm.choices.map(_.id) == List("BanditsAbandonYes", "BanditsBack") &&
                confirm.text.contains(s"деревня «$VillageName»") && q0.village.isDefined) &&
              assertTrue(done == StateType.Dungeon && q.village.isEmpty && inv.snapshot.isEmpty)
    },

    test("банда сборная: на любом броске — только смертные расы лабиринта, рас много, главарь последним — своей расы и со своим именем") {
      val orcChief = contract().copy(leader = Some("Каркан Кровавый Топор"), leaderRace = Some(Race.Orc))
      val gangs    = (1L to 50L).toList.map(seed => Villages.banditGang(orcChief, Rng(seed)))
      val races    = gangs.flatten.map(_.race).toSet
      assertTrue(gangs.forall(g => g.size == VillageRates.BanditFront + VillageRates.BanditBack + 1)) &&
      assertTrue(races.subsetOf(Race.mortals.toSet) && races.intersect(Race.bossRaces).isEmpty && races.size == Race.mortals.size) &&
      assertTrue(gangs.forall(_.lastOption.exists(l => l.race == Race.Orc && l.rarity == Rarity.Legendary &&
        l.customName.contains("Каркан Кровавый Топор")))) &&
      // Раса главаря не записана (так было до сборных банд) — человек, как раньше.
      assertTrue(Villages.banditGang(contract().copy(leaderRace = None), Rng(1L)).lastOption.exists(_.race == Race.Human))
    },

    test("пауки: староста про сетеплётов — свой разговор с городом и платой; «Взяться» — контракт на гнездо, без главаря") {
      for {
        c     <- content
        d     <- dao(sc = Some(spiderScene.asJson))
        inv    = TestInventoryRepository.accepting
        r     <- TestRenderer.make
        st     = village(d, inv, c)
        _     <- st.enter(testUser, r)
        come  <- r.sentScreens.map(_.last)
        _     <- st.action(testUser, tap("VillageHeadman"), r)
        shown <- r.sentScreens.map(_.takeRight(2))
        _     <- st.action(testUser, tap("VillageAccept"), r)
        last  <- r.sentScreens.map(_.last)
        q     <- questOf(d)
      } yield assertTrue(come.text.contains(c.text("village.trouble.spiders").trim)) &&
              assertTrue(shown.lastOption.exists(s => s.text.contains("вышли сетеплёты") && s.text.contains(s"знахарь из города $City") &&
                s.text.contains(s"Плата — $Silver серебра") && s.text.contains("Сложность: " + Difficulty.render(QuestLevel)))) &&
              assertTrue(last.text.contains("Овраг за деревней") && last.text.contains("«Контракт деревни»")) &&
              assertTrue(q.village.exists(cc => cc.quest == VillageQuest.SpiderNests && cc.leader.isEmpty && cc.level == QuestLevel)) &&
              assertTrue(Villages.lairOf(q).contains(VillageLair.Spiders) && !Villages.toBandits(q))
    },

    test("пауки: карточка ведёт к гнезду — «🕸 Идти к гнезду», из лабиринта") {
      for {
        d     <- dao(hero(20), Some(withSpiders()))
        _     <- d.writeReturnState(userId, Some(StateType.Dungeon))
        r     <- TestRenderer.make
        c     <- content
        state  = InventoryState(d, TestInventoryRepository.withItems(List(paper)), TestItemRepository.make, c)
        _     <- state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${paper.id}"}""")), r)
        shown <- r.sentScreens.map(_.last)
        go    <- state.action(testUser, tap("VillageGo"), r)
      } yield assertTrue(shown.text.contains("гнездо сетеплётов") &&
                shown.choices.exists(ch => ch.id == "VillageGo" && ch.label == c.choice("x", "village.card.spidersLabel").label)) &&
              assertTrue(go == StateType.VillageSpiders)
    },

    test("пауки: гнездо — выводок и матка у входа; бой — восемь молодых, шесть взрослых и матка уровня задания, у пород свои имена, без бегства") {
      for {
        c     <- content
        d     <- dao(q = Some(withSpiders()))
        r     <- TestRenderer.make
        st     = spiderLair(d, TestInventoryRepository.withItems(List(paper)), c)
        _     <- st.enter(testUser, r)
        intro <- r.sentScreens.map(_.last)
        res   <- st.action(testUser, tap("SpidersFight"), r)
        b     <- battleOf(d)
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        slots  = b.toList.flatMap(x => x.group.others ++ x.group.queue)
        names  = Breed.spiders.map(_.displayName)
        breeds = b.toList.flatMap(x => x.monsterBreed.toList ++ slots.flatMap(_.breed))
      } yield assertTrue(intro.text.contains("матка") && intro.text.contains("Сложность: " + Difficulty.render(QuestLevel))) &&
              assertTrue(intro.choices.map(_.id) == List("SpidersFight", "SpidersSupply", "OpenCharacter", "SpidersLeave", "SpidersAbandon")) &&
              assertTrue(res == StateType.Battle) &&
              assertTrue(b.exists(x => x.story.contains(Villages.SpidersStory) && x.lootLevel.contains(QuestLevel.toLong) &&
                x.cantFlee && x.monsterRace == Race.Animal.entryName && names.contains(x.monsterName))) &&
              // красный выползает раз на сотню — тогда их шестнадцать
              assertTrue(breeds.count(_ == Breed.YoungSpider) == VillageRates.SpiderYoung &&
                breeds.count(_ == Breed.AdultSpider) == VillageRates.SpiderAdult &&
                breeds.count(_ == Breed.SpiderMother) == 1 && breeds.count(_ == Breed.RedSpider) <= 1 &&
                breeds.size == slots.size + 1) &&
              assertTrue(slots.forall(s => s.race == Race.Animal.entryName && s.lvl == QuestLevel.toLong &&
                s.customName.exists(names.contains) && s.breed.exists(br => s.customName.contains(br.displayName)))) &&
              assertTrue(slots.exists(s => s.breed.contains(Breed.SpiderMother) && s.rarity == Rarity.Mythical.entryName &&
                s.customName.contains("Матка сетеплётов"))) &&
              // яд — у молодых, и только у них
              assertTrue(slots.forall(s => s.effects.monsterPoisonsOnHit == s.breed.contains(Breed.YoungSpider)) &&
                b.exists(x => x.effects.monsterPoisonsOnHit == x.monsterBreed.contains(Breed.YoungSpider))) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.VillageSpiders)))
    },

    test("пауки: выводок перебит — гнездо выжжено, дальше к старосте; плата по-паучьи; отметка победы у логов своя") {
      for {
        c    <- content
        d    <- dao(q = Some(withSpiders()), sc = Some(VillageLairState.wonMark(VillageLair.Spiders)))
        inv   = TestInventoryRepository.withItems(List(paper))
        r    <- TestRenderer.make
        _    <- spiderLair(d, inv, c).enter(testUser, r)
        won  <- r.sentScreens.map(_.last)
        q    <- questOf(d)
        _    <- village(d, inv, c).enter(testUser, r)
        paid <- r.sentScreens.map(_.last)
        q2   <- questOf(d)
      } yield assertTrue(won.text.contains("Гнезда больше нет") && won.choices.map(_.id) == List("SpidersOut")) &&
              assertTrue(Villages.toPay(q)) &&
              assertTrue(paid.text.contains("собаки лаяли на луну") && paid.text.contains(s"Получено $Silver серебра")) &&
              assertTrue(q2.village.isEmpty && inv.snapshot.isEmpty) &&
              assertTrue(!VillageLairState.won(VillageLair.Spiders, VillageLairState.wonMark(VillageLair.Bandits)) &&
                VillageLairState.won(VillageLair.Bandits, Json.obj("villageBanditsWon" -> Json.True)))
    },

    test("пауки: с них шёлк и ядовитые железы, крысиного нет; со зверей в прочих боях — крысиное, как было") {
      def kinds(drops: List[LootGenerator.LootDrop]): Set[MaterialKind] =
        drops.collect { case LootGenerator.LootDrop.Gear(i) => i.material }.flatten.toSet
      val spiders = (1L to 200L).toList.flatMap(seed =>
        LootGenerator.roll(Rarity.Rare, Race.Animal, 20L, Rng(seed), beastSpoils = LootGenerator.SpiderSpoils)._1)
      val rats    = (1L to 200L).toList.flatMap(seed => LootGenerator.roll(Rarity.Rare, Race.Animal, 20L, Rng(seed))._1)
      assertTrue(kinds(spiders) == Set[MaterialKind](MaterialKind.SpiderSilk, MaterialKind.VenomGland, MaterialKind.CocoonTreasure)) &&
      assertTrue(kinds(rats).contains(MaterialKind.RatPelt) &&
        kinds(rats).intersect(Set[MaterialKind](MaterialKind.SpiderSilk, MaterialKind.VenomGland)).isEmpty) &&
      assertTrue(BattleState.beastSpoils(Some(Villages.SpidersStory)) == LootGenerator.SpiderSpoils &&
        BattleState.beastSpoils(Some(Villages.BanditsStory)) == LootGenerator.RatSpoils &&
        BattleState.beastSpoils(None) == LootGenerator.RatSpoils)
    },

    test("пауки: сила — как у образцов: молодой — орк первого ранга, взрослый — каджит третьего, матка — каджит четвёртого, красный — именной эльф") {
      val lvl = QuestLevel
      def like(race: Race, rarity: Rarity) = MonsterGenerator.generateOfRaceAndRarity(lvl, race, rarity).fightStats
      val young  = Villages.spider(Breed.YoungSpider, lvl)
      val adult  = Villages.spider(Breed.AdultSpider, lvl)
      val mother = Villages.spider(Breed.SpiderMother, lvl)
      val red    = Villages.spider(Breed.RedSpider, lvl)
      assertTrue(young.fightStats == like(Race.Orc, Rarity.Common) && young.rarity == Rarity.Common) &&
      assertTrue(adult.fightStats == like(Race.Khajiit, Rarity.Rare) && adult.rarity == Rarity.Rare) &&
      assertTrue(mother.fightStats == like(Race.Khajiit, Rarity.Mythical) && mother.rarity == Rarity.Mythical) &&
      assertTrue(red.fightStats == like(Race.Elf, Rarity.Legendary) && red.rarity == Rarity.Legendary) &&
      assertTrue(List(young, adult, mother, red).forall(m => m.race == Race.Animal && m.lvl == lvl.toLong &&
        m.breed.exists(b => m.customName.contains(b.displayName))))
    },

    test("пауки: красный сетеплёт — раз на сотню, последним; с него всегда его железа; Ришелье берёт её за 5 дублонов") {
      val contract = VillageContract.of(spiderScene)
      val reds     = (1L to 2000L).count(seed => Villages.spiderBrood(contract, Rng(seed)).exists(_.breed.contains(Breed.RedSpider)))
      val withRed  = Villages.spiderBrood(contract, red = true)
      val gland    = BattleState.breedDrops(Some(Breed.RedSpider)).flatMap(_.itemOpt).flatMap(_.material)
      assertTrue(reds > 0 && reds < 60) &&
      assertTrue(withRed.size == VillageRates.SpiderYoung + VillageRates.SpiderAdult + 2 &&
        withRed.lastOption.exists(_.breed.contains(Breed.RedSpider)) &&
        Villages.spiderBrood(contract, red = false).forall(!_.breed.contains(Breed.RedSpider))) &&
      assertTrue(gland == List(MaterialKind.RedSpiderGland) &&
        Breed.values.filterNot(_ == Breed.RedSpider).forall(b => BattleState.breedDrops(Some(b)).isEmpty) &&
        BattleState.breedDrops(None).isEmpty) &&
      assertTrue(MaterialKind.RedSpiderGland.doubloonPrice == 5L && !MaterialKind.RedSpiderGland.worthless &&
        MaterialKind.CocoonTreasure.worthless && MaterialKind.CocoonTreasure.description.nonEmpty)
    },

    test("порода в бою: старые записи слота, павшего и боя без неё читаются, чужое имя — тоже, своя едет туда и обратно") {
      val hero0  = hero()
      val battle = Villages.withBreedTraits(SoloPveBattle.fromGroup(Villages.spiderBrood(VillageContract.of(spiderScene), red = false),
                     hero0, Nil))
      val slot   = battle.group.others.headOption
      val oldSlot   = slot.map(s => s.asJson.mapObject(_.remove("breed")))
      val oddSlot   = slot.map(s => s.asJson.mapObject(_.add("breed", Json.fromString("Тарантул"))))
      val oldBattle = battle.asJson.mapObject(_.remove("monsterBreed"))
      val dead      = battle.slainActive
      val oldDead   = dead.asJson.mapObject(_.remove("breed"))
      assertTrue(slot.exists(s => s.breed.isDefined && s.asJson.as[pangea.model.battle.MonsterSlot].toOption.contains(s))) &&
      assertTrue(oldSlot.exists(_.as[pangea.model.battle.MonsterSlot].exists(_.breed.isEmpty)) &&
        oddSlot.exists(_.as[pangea.model.battle.MonsterSlot].exists(_.breed.isEmpty))) &&
      assertTrue(battle.asJson.as[SoloPveBattle].exists(_.monsterBreed == battle.monsterBreed) && battle.monsterBreed.isDefined &&
        oldBattle.as[SoloPveBattle].exists(_.monsterBreed.isEmpty)) &&
      assertTrue(dead.breed == battle.monsterBreed && oldDead.as[pangea.model.battle.SlainMonster].exists(_.breed.isEmpty))
    },

    test("журнал: по контракту — к логову, потом за платой") {
      for {
        c <- content
        toLair = QuestJournal.render(c, hero(), LoreData.empty, withContract(), DailyQuests.empty, None, 0L)
        toPay  = QuestJournal.render(c, hero(), LoreData.empty, withContract(VillageContract.Done), DailyQuests.empty, None, 0L)
      } yield assertTrue(toLair.contains(s"«Контракт деревни» — разбить логово разбойников у деревни орков «$VillageName»")) &&
              assertTrue(toPay.contains(s"«Контракт деревни» — вернуться в деревню орков «$VillageName» за платой — $Silver серебра"))
    },

    test("смерть в налёте и в логове: проснуться в лабиринте со своей строкой, контракт на месте; оба боя награждаются как обычные") {
      for {
        c   <- content
        r   <- TestRenderer.make
        d1  <- dao()
        _   <- d1.writeActiveBattle(userId, SoloPveBattle.fromGroup(Villages.raidMonsters(scene, Rng(1L)), hero(), Nil,
                 towers = VillageRates.RaidTowers).copy(story = Some(Villages.RaidStory)).asJson)
        _   <- DeathState(d1, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        w1  <- d1.readSceneData(userId)
        d2  <- dao(q = Some(withContract()))
        _   <- d2.writeActiveBattle(userId, SoloPveBattle.fromGroup(Villages.banditGang(contract(), Rng(1L)), hero(), Nil)
                 .copy(story = Some(Villages.BanditsStory)).asJson)
        _   <- DeathState(d2, TestInventoryRepository.withItems(List(paper)), c).enter(testUser, r)
        w2  <- d2.readSceneData(userId)
        q2  <- questOf(d2)
        d3  <- dao(q = Some(withSpiders()))
        _   <- d3.writeActiveBattle(userId, SoloPveBattle.fromGroup(Villages.spiderBrood(VillageContract.of(spiderScene), Rng(1L)),
                 hero(), Nil).copy(story = Some(Villages.SpidersStory)).asJson)
        _   <- DeathState(d3, TestInventoryRepository.withItems(List(paper)), c).enter(testUser, r)
        w3  <- d3.readSceneData(userId)
      } yield assertTrue(wakeLines(w1) == List("village.raid.death") && wakeLines(w2) == List("village.bandits.death") &&
                wakeLines(w3) == List("village.spiders.death")) &&
              assertTrue(w1.exists(_.hcursor.downField("wakeTo").failed) && w2.exists(_.hcursor.downField("wakeTo").failed)) &&
              assertTrue(Villages.toBandits(q2)) &&
              assertTrue(BattleState.RaidStories.contains(Villages.RaidStory) && BattleState.RaidStories.contains(Villages.BanditsStory) &&
                BattleState.RaidStories.contains(Villages.SpidersStory))
    },

    test("вход в логово предупреждает: из этого боя не сбежать — у разбойников и у пауков") {
      for {
        c       <- content
        r       <- TestRenderer.make
        d1      <- dao(q = Some(withContract()))
        _       <- lair(d1, TestInventoryRepository.withItems(List(paper)), c).enter(testUser, r)
        bandits <- r.sentScreens.map(_.last)
        d2      <- dao(q = Some(withSpiders()))
        _       <- spiderLair(d2, TestInventoryRepository.withItems(List(paper)), c).enter(testUser, r)
        spiders <- r.sentScreens.map(_.last)
        warn     = c.text("village.lair.noEscape")
      } yield assertTrue(warn.contains("Сбежать из этого боя не получится")) &&
              assertTrue(bandits.text.endsWith(warn) && spiders.text.endsWith(warn))
    },

    test("награбленное: на заставе — 30% с каждого павшего, с разбойников в засаде на караван — вдвое реже; в обычном бою, при налёте, в гнезде и в прочих засадах — нет") {
      def kinds(drops: List[LootGenerator.LootDrop]): List[MaterialKind] =
        drops.collect { case LootGenerator.LootDrop.Gear(i) => i.material }.flatten
      def rolls(story: Option[String], convoy: Option[Convoy]): List[List[LootGenerator.LootDrop]] =
        (1L to 2000L).toList.map(seed => BattleState.plunder(story, convoy, Rng(seed))._1)
      def ambush(a: CaravanAmbush): Option[Convoy] = Some(Villages.convoy(Nil, a))
      val caravan = Some(Villages.CaravanStory)
      val lair    = rolls(Some(Villages.BanditsStory), None)
      val robbers = rolls(caravan, ambush(CaravanAmbush.Bandits))
      val rng     = Rng(7L)
      def none(story: Option[String], convoy: Option[Convoy]): Boolean = BattleState.plunder(story, convoy, rng) == ((Nil, rng))
      // обычный бой лабиринта, налёт на деревню, гнездо сетеплётов, засады Чернолома и ищеек,
      // засада из пути без повозок, прочие сюжеты
      val quiet = List(None, Some(Villages.RaidStory), Some(Villages.SpidersStory), caravan, Some("collector"))
                    .forall(s => none(s, None)) &&
                  none(caravan, ambush(CaravanAmbush.Chernolom)) && none(caravan, ambush(CaravanAmbush.Hounds))
      val onlyBandits = CaravanAmbush.all.filter(_.plunders) == List(CaravanAmbush.Bandits)
      // бой засады, записанный до этого, — без награбленного
      val oldConvoy = Json.obj("wagons" -> Json.arr(), "hitsWagons" -> Json.True, "ignitePct" -> Json.fromLong(25L))
        .as[Convoy].toOption
      val oldQuiet  = oldConvoy.exists(cv => !cv.plunders && cv.ignitePct == 25L)
      // 30% от двух тысяч — шестьсот, 15% — триста
      assertTrue(lair.count(_.nonEmpty) > 480 && lair.count(_.nonEmpty) < 720 && lair.forall(_.size <= 1)) &&
      assertTrue(robbers.count(_.nonEmpty) > 220 && robbers.count(_.nonEmpty) < 380 && robbers.forall(_.size <= 1)) &&
      assertTrue(LootGenerator.BanditSpoilsChancePct == 30L && LootGenerator.AmbushSpoilsChancePct == 15L) &&
      assertTrue(lair.flatMap(kinds).toSet == LootGenerator.BanditSpoils.map(_._1).toSet &&
        robbers.flatMap(kinds).toSet == LootGenerator.BanditSpoils.map(_._1).toSet &&
        LootGenerator.BanditSpoils.map(_._2).sum == 100 && LootGenerator.BanditSpoils.size == 7) &&
      assertTrue(quiet && onlyBandits && oldQuiet)
    },

    test("награбленное: цены у Ришелье — 100, 100, 200, 200, 250, 300, 400; на карточке — «(пока не используется)», кроме пеньки (она в ловчей сети), как у кокона; у прочих — нет") {
      val prices = Map[MaterialKind, Long](MaterialKind.Timber -> 100L, MaterialKind.HempRope -> 100L,
        MaterialKind.TannedLeather -> 200L, MaterialKind.WoolCloth -> 200L, MaterialKind.IronIngot -> 250L,
        MaterialKind.PineTar -> 300L, MaterialKind.CopperIngot -> 400L)
      // Ядовитая железа и железа красного паука уже в деле — идут в паучью
      // сыворотку и яд красного паука; паучий шёлк с пенькой — в ловчую сеть.
      // Пометки на них нет.
      val spider = List(MaterialKind.CocoonTreasure)
      val spoils = LootGenerator.BanditSpoils.map(_._1).filterNot(_ == MaterialKind.HempRope)
      val cards  = (spoils ++ spider).map(k => k -> MaterialGenerator.item(k).statsLines)
      assertTrue(LootGenerator.BanditSpoils.forall { case (k, _) => prices.get(k).contains(k.silverPrice) }) &&
      assertTrue(cards.forall { case (k, lines) =>
        k.notYetUsed && lines.contains(k.description) && lines.lastOption.contains(MaterialKind.NotYetUsedNote) }) &&
      assertTrue(MaterialKind.NotYetUsedNote == "(пока не используется)") &&
      assertTrue(List(MaterialKind.EverburningIron, MaterialKind.DiamondDust, MaterialKind.Mithril, MaterialKind.VenomGland,
        MaterialKind.RedSpiderGland, MaterialKind.SpiderSilk, MaterialKind.HempRope).forall(k =>
        !k.notYetUsed && !MaterialGenerator.item(k).statsLines.contains(MaterialKind.NotYetUsedNote)))
    },

    test("награбленное в настоящем бою: на заставе и с разбойников в засаде на караван падает; с гоблинов и нежити в обычном бою лабиринта и с Чернолома — нет") {
      val strong = hero().copy(fightStats = FightStats(atk = 1000000L, hp = 500000L, armor = 0, defence = 0, evasion = 0,
                     accuracy = 9999, energy = 0))
      def rogue(race: Race) = Monster(0L, QuestLevel.toLong, race, Rarity.Common,
                     FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      // `n` уже полегли: на заставе хоть с кого-то из шестнадцати да упадёт, в
      // засаде на караван (вдвое реже) павших берём больше
      def gang(race: Race, n: Int) = List.fill(n)(SlainMonster(QuestLevel.toLong, race.entryName, Rarity.Common.entryName,
                     marked = false, name = "Разбойник"))
      def materials(story: Option[String], race: Race, convoy: Option[Convoy] = None, n: Int = 15): Task[List[MaterialKind]] =
        for {
          c  <- content
          d  <- dao(strong)
          b   = SoloPveBattle.from(rogue(race), strong, squad = false)
          _  <- d.writeActiveBattle(userId, b.copy(noKin = true, story = story,
                  group = b.group.copy(slain = gang(race, n), convoy = convoy)).asJson)
          r  <- TestRenderer.make
          _  <- BattleState(d, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
          ld <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        } yield ld.toList.flatMap(l => l.items ++ l.queue.flatMap(_.items)).flatMap(_.material)
      val bandit = LootGenerator.BanditSpoils.map(_._1).toSet
      val caravan = Some(Villages.CaravanStory)
      for {
        lair    <- materials(Some(Villages.BanditsStory), Race.Human)
        plain   <- materials(None, Race.Undead)
        // смертные в обычном бою — как гоблины-рабы на 10-м уровне: награбленного с них нет
        goblins <- materials(None, Race.Goblin)
        // засада на караван: у разбойников есть, у Чернолома — нет
        robbers <- materials(caravan, Race.Human, Some(Villages.convoy(Nil, CaravanAmbush.Bandits)), n = 40)
        torches <- materials(caravan, Race.Goblin, Some(Villages.convoy(Nil, CaravanAmbush.Chernolom)), n = 40)
      } yield assertTrue(lair.exists(bandit.contains)) && assertTrue(!plain.exists(bandit.contains)) &&
              assertTrue(!goblins.exists(bandit.contains)) &&
              assertTrue(robbers.exists(bandit.contains) && !torches.exists(bandit.contains))
    },

    test("тексты: у каждой расы — названия, приход и староста; у каждой беды — всё, что ей нужно; первая беда — с порога деревень") {
      for {
        c <- content
        all = Race.mortals.toList.flatMap(Villages.names(c, _))
      } yield assertTrue(Race.mortals.forall(r => Villages.names(c, r).size >= 2 &&
                c.text(s"village.arrive.${r.entryName}").contains("{name}") && c.text(s"village.headman.${r.entryName}").nonEmpty)) &&
              assertTrue(all.distinct.size == all.size) &&
              assertTrue(Race.mortals.forall(r => Villages.lands(c, r).size >= 2)) &&
              assertTrue(Villages.cities(c).size >= 5 && Villages.cities(c).distinct == Villages.cities(c) &&
                !Villages.cities(c).contains("Кинэт")) &&
              assertTrue(VillageRates.silver(20) == 1000L && VillageRates.silver(20, 80L) == 800L && VillageRates.silver(20, 120L) == 1200L) &&
              // и по дублону за каждые полные семь уровней задания
              assertTrue(List(1, 6, 7, 13, 14, 26, 150).map(VillageRates.doubloons) == List(0L, 0L, 1L, 1L, 2L, 3L, 21L)) &&
              // уровень задания: от −1 до +4 от уровня деревни, не глубже 150-го
              assertTrue(VillageRates.QuestLevelMin == -1 && VillageRates.QuestLevelMax == 4 &&
                VillageRates.questLevel(20, -1) == 19 && VillageRates.questLevel(20, 4) == 24 &&
                VillageRates.questLevel(148, 4) == 150 && VillageRates.questLevel(150, 4) == 150 && VillageRates.questLevel(1, -1) == 1) &&
              assertTrue(VillageQuest.values.forall(q =>
                List(s"village.trouble.${q.key}", s"village.plea.${q.key}.text", s"village.plea.${q.key}.accepted",
                  s"village.waiting.${q.key}", s"village.card.${q.key}", s"journal.village.${q.key}")
                  .forall(k => c.text(k).nonEmpty))) &&
              // платит староста — за логова; за караван платят в городе, куда он дошёл
              assertTrue(VillageQuest.values.filterNot(_ == VillageQuest.CaravanEscort)
                .forall(q => c.text(s"village.pay.${q.key}").nonEmpty)) &&
              assertTrue(VillageQuest.BanditLair.minLevel == Villages.MinDungeonLevel && Villages.MinDungeonLevel == 15) &&
              assertTrue(VillageQuest.openAt(15) == List(VillageQuest.BanditLair) && VillageQuest.openAt(19) == List(VillageQuest.BanditLair) &&
                VillageQuest.openAt(20) == List(VillageQuest.BanditLair, VillageQuest.SpiderNests) &&
                VillageQuest.openAt(1) == List(VillageQuest.BanditLair)) &&
              assertTrue(VillageLair.all.forall(l =>
                List("intro", "fight", "victory", "left", "gone", "death").forall(k => c.text(s"village.${l.texts}.$k").nonEmpty) &&
                c.choice("x", s"village.${l.texts}.fightLabel").label.nonEmpty &&
                c.choice("x", s"village.card.${l.texts}Label").label.nonEmpty)) &&
              // у каждой беды, кроме каравана и артефакта, — своё логово (у каравана — дорога,
              // VillageCaravanSpec; у артефакта — след воров, VillageArtifactSpec), и у логов
              // разные сцены, бои, кнопки и отметки
              assertTrue(VillageQuest.values.filterNot(q => q == VillageQuest.CaravanEscort || q == VillageQuest.StolenArtifact)
                .forall(q => VillageLair.of(q).isDefined) &&
                VillageLair.of(VillageQuest.CaravanEscort).isEmpty && VillageLair.of(VillageQuest.StolenArtifact).isEmpty &&
                VillageLair.all.map(_.scene).distinct.size == VillageLair.all.size &&
                VillageLair.all.map(_.story).distinct.size == VillageLair.all.size &&
                VillageLair.all.map(_.ids).distinct.size == VillageLair.all.size &&
                VillageLair.all.map(_.wonKey).distinct.size == VillageLair.all.size) &&
              assertTrue(c.text("village.spiders.redSpider").nonEmpty) &&
              assertTrue(MaterialKind.SpiderSilk.description.nonEmpty && MaterialKind.VenomGland.description.nonEmpty &&
                MaterialKind.SpiderSilk.worthless && MaterialKind.VenomGland.worthless)
    }
  )
}
