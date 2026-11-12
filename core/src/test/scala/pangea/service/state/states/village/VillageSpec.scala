package pangea.service.state.states.village

import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.caravan.CaravanScene
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.{VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, QuestJournal, UserAction, Villages}
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
    VillageBanditsState(d, inv, c)

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

    test("пул: деревня — с 20-го уровня, один билет; выпала — с чистого листа; при контракте деревни не выпадает") {
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
              assertTrue(StateType.eventsAt(19, 1.0).count(_ == StateType.Village) == 0 &&
                StateType.eventsAt(20, 1.0).count(_ == StateType.Village) == 1)
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
                x.quest == VillageQuest.BanditLair && x.leader.isDefined)) &&
              assertTrue(s.exists(x => x.leaderRace.exists(r => Race.mortals.contains(r) &&
                Monster.legendaryNames.get(r).exists(names => x.leader.exists(names.contains)))) ) &&
              assertTrue(s.exists(x => first.text.contains("«" + Villages.name(c, x.race, x.name) + "»") &&
                Villages.names(c, x.race).contains(Villages.name(c, x.race, x.name)))) &&
              assertTrue(first.text.contains(c.text("village.trouble.bandits").trim)) &&
              assertTrue(s.exists(x => Villages.lands(c, x.race).lift(x.land).exists(first.text.contains(_)))) &&
              assertTrue(s.exists(x => x.questLevel >= Level + VillageRates.QuestLevelMin && x.questLevel <= Level + VillageRates.QuestLevelMax)) &&
              assertTrue(s.exists(x => x.silver >= VillageRates.silver(x.questLevel, 80L) && x.silver <= VillageRates.silver(x.questLevel, 120L) &&
                Villages.cities(c).isDefinedAt(x.city))) &&
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

    test("карточка контракта: сложность, деревня, главарь и плата; «Идти к логову» — из лабиринта с 20-го уровня; сделано — «Вернуться в деревню»") {
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
        deep    <- card(StateType.Dungeon, 20, VillageContract.Taken)
        city    <- card(StateType.GlobalMap, 20, VillageContract.Taken)
        shallow <- card(StateType.Dungeon, 19, VillageContract.Taken)
        back    <- card(StateType.Dungeon, 20, VillageContract.Done)
      } yield assertTrue(deep._1.text.contains("Сложность: " + Difficulty.render(QuestLevel)) &&
                deep._1.text.contains(s"Деревня орков «$VillageName»") && deep._1.text.contains("верховодит Чёрный Виктор") &&
                deep._1.text.contains(s"Плата — $Silver серебра")) &&
              assertTrue(deep._1.choices.exists(_.id == "VillageGo") && deep._2 == StateType.VillageBandits && deep._4.contains(Json.Null)) &&
              assertTrue(city._2 == StateType.Inventory && city._3.contains("идти туда можно только из него")) &&
              assertTrue(shallow._2 == StateType.Inventory && shallow._3.contains("не ближе 20-го уровня")) &&
              assertTrue(back._1.choices.exists(_.id == "VillageReturn") && back._1.text.contains("Логово разбито") &&
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

    test("банда перебита: логово разбито — дальше к старосте; в логове больше пусто") {
      for {
        c     <- content
        d     <- dao(q = Some(withContract()), sc = Some(VillageBanditsState.Won(won = true).asJson))
        r     <- TestRenderer.make
        st     = lair(d, TestInventoryRepository.withItems(List(paper)), c)
        _     <- st.enter(testUser, r)
        last  <- r.sentScreens.map(_.last)
        q     <- questOf(d)
        sd    <- d.readSceneData(userId)
        _     <- st.enter(testUser, r)
        again <- r.sentScreens.map(_.last)
      } yield assertTrue(last.text.contains("Чёрный Виктор падает в костёр") && last.text.contains(s"тюки с клеймом города $City") &&
                last.text.contains(s"Дорога у деревни «$VillageName» снова свободна") && last.choices.map(_.id) == List("BanditsOut")) &&
              assertTrue(Villages.toPay(q) && sd.contains(Json.Null)) &&
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
      } yield assertTrue(wakeLines(w1) == List("village.raid.death") && wakeLines(w2) == List("village.bandits.death")) &&
              assertTrue(w1.exists(_.hcursor.downField("wakeTo").failed) && w2.exists(_.hcursor.downField("wakeTo").failed)) &&
              assertTrue(Villages.toBandits(q2)) &&
              assertTrue(BattleState.RaidStories.contains(Villages.RaidStory) && BattleState.RaidStories.contains(Villages.BanditsStory))
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
              assertTrue(VillageRates.silver(20) == 1500L && VillageRates.silver(20, 80L) == 1200L && VillageRates.silver(20, 120L) == 1800L) &&
              // уровень задания: от −1 до +4 от уровня деревни, не глубже 150-го
              assertTrue(VillageRates.QuestLevelMin == -1 && VillageRates.QuestLevelMax == 4 &&
                VillageRates.questLevel(20, -1) == 19 && VillageRates.questLevel(20, 4) == 24 &&
                VillageRates.questLevel(148, 4) == 150 && VillageRates.questLevel(150, 4) == 150 && VillageRates.questLevel(1, -1) == 1) &&
              assertTrue(VillageQuest.values.forall(q =>
                List(s"village.trouble.${q.key}", s"village.plea.${q.key}.text", s"village.plea.${q.key}.accepted",
                  s"village.waiting.${q.key}", s"village.pay.${q.key}", s"village.card.${q.key}", s"journal.village.${q.key}")
                  .forall(k => c.text(k).nonEmpty))) &&
              assertTrue(VillageQuest.BanditLair.minLevel == Villages.MinDungeonLevel && Villages.MinDungeonLevel == 20) &&
              assertTrue(VillageQuest.openAt(20) == List(VillageQuest.BanditLair) && VillageQuest.openAt(1) == List(VillageQuest.BanditLair))
    }
  )
}
