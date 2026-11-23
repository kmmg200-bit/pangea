package pangea.service.state.states.hedgesong

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.inventory.Inventory.Items
import pangea.model.item.QuestItemKind
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.squad.{AllyKind, AllyRates}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.artifact.FetShopState
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.{DeathState, InventoryState, LootState}
import pangea.service.state.{HedgeSongQuest, NpcQuestLog, QuestJournal, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Тот, кто уговаривает деревья»: эльф поёт изгороди, герой стоит на страже —
  * три волны орков, каждая своим боем, с отрядом и Элиаром в строю; победа —
  * ветвь, Фету — за шкаф. «Не сейчас», уход между волнами и смерть задание не
  * начинают. Фаэлин при герое — младший брат Элиара: у него свои слова, а на
  * уход посреди песни он остаётся с братом. */
object HedgeSongSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def hero(race: Race = Race.Human): Hero = TestFixtures.hero(userId, dungeonLevel = 5).copy(race = race)

  private def branch = QuestItemKind.item(QuestItemKind.HedgeBranch).copy(id = 700L)

  private def onHands: NpcQuests = NpcQuests.empty.take(NpcQuest.HedgeSong, 0L)

  private def dao(h: Hero, q: Option[NpcQuests] = None) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def scene(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    HedgeSongState(d, inv, TestItemRepository.make, c)

  private def battleOf(d: TestHeroDao) =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  /** Волна `n`: начать её из сцены, где `cleared` уже отбито. */
  private def startWave(h: Hero, cleared: Int) =
    for {
      d   <- dao(h)
      _   <- ZIO.when(cleared > 0)(d.writeSceneData(userId, HedgeSongState.Progress(cleared).asJson))
      r   <- TestRenderer.make
      c   <- content
      out <- scene(d, TestInventoryRepository.withItems(Nil), c).action(testUser, tap("HedgeFight"), r)
      b   <- battleOf(d)
      l   <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      t   <- texts(r)
    } yield (out, b, l, t)

  /** Ранги первой и второй волны — как их пишет бой. */
  private val firstRanks: Set[String]  = Set(Rarity.Uncommon, Rarity.Rare).map(_.entryName)
  private val secondRanks: Set[String] = Set(Rarity.Rare, Rarity.Mythical).map(_.entryName)

  override def spec = suite("Тот, кто уговаривает деревья")(

    test("встреча: эльф поёт изгороди, сложность — сразу; «Встать на стражу» или «Не сейчас»; эльфу — свои слова") {
      for {
        c     <- content
        d1    <- dao(hero())
        r     <- TestRenderer.make
        _     <- scene(d1, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        human <- r.sentScreens.map(_.last)
        d2    <- dao(hero(Race.Elf))
        _     <- scene(d2, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        elf   <- r.sentScreens.map(_.last)
      } yield assertTrue(human.text.contains("Я Элиар") && human.text.contains("орки-лесорубы")) &&
              assertTrue(human.text.contains(Difficulty.render(7))) &&
              assertTrue(human.choices.map(_.id) == List("HedgeFight", "HedgeNotNow")) &&
              assertTrue(elf.text.contains("Постой за моей спиной, родич") && !human.text.contains("родич"))
    },

    test("«Не сейчас»: задание не начато, эльфа надо встретить снова") {
      for {
        d   <- dao(hero())
        r   <- TestRenderer.make
        c   <- content
        out <- scene(d, TestInventoryRepository.withItems(Nil), c).action(testUser, tap("HedgeNotNow"), r)
        q   <- questOf(d)
        sd  <- d.readSceneData(userId)
      } yield assertTrue(out == StateType.Dungeon && HedgeSongQuest.fresh(q) && sd.forall(_.isNull))
    },

    test("первая волна: четверо орков 7-го уровня — воины и живучие; отряд героя и Элиар в строю; бой сюжетный, награда по 7-му уровню") {
      val merc = hero().squad.hire(AllyKind.Human, 1L, 0L)
      for {
        w <- startWave(hero().copy(lvl = 40L, squad = merc), 0)
        (out, b, loot, said) = w
        orcs = b.toList.flatMap(x => x.monsterRarity :: x.group.others.map(_.rarity))
        elfs = b.toList.flatMap(_.group.allies.filter(_.kind == AllyKind.HedgeElf))
        named = MonsterGenerator.generateOfRaceAndRarity(7, Race.Elf, Rarity.Legendary).fightStats
      } yield assertTrue(out == StateType.Battle && said.contains("Первая волна: четверо")) &&
              assertTrue(orcs.size == 4 && orcs.forall(r => firstRanks.contains(r))) &&
              assertTrue(b.exists(x => x.monsterRace == Race.Orc.entryName && x.monsterLvl == 7L &&
                x.group.others.forall(s => s.race == Race.Orc.entryName && s.lvl == 7L))) &&
              assertTrue(b.exists(x => x.story.contains(HedgeSongQuest.WaveStory) && x.cantFlee && x.lootLevel.contains(7L))) &&
              // наёмник героя тоже здесь, а Элиар — по силе именной эльф 7-го уровня, на полных силах
              assertTrue(b.exists(_.group.allies.exists(_.kind == AllyKind.Human))) &&
              assertTrue(elfs.size == 1 && elfs.forall(e => e.lvl == 7L && e.stats == named && e.hp == named.hp &&
                e.undead.exists(_.name == HedgeSongQuest.ElfName))) &&
              assertTrue(loot.exists(l => l.returnState.contains(StateType.HedgeSong) &&
                l.eventData.flatMap(_.as[HedgeSongState.Progress].toOption).contains(HedgeSongState.Progress(1))))
    },

    test("вторая волна — живучие и командиры; третья — вожак Бокк Последний у Костра и три командира") {
      for {
        w2 <- startWave(hero(), 1)
        w3 <- startWave(hero(), 2)
        second = w2._2.toList.flatMap(x => x.monsterRarity :: x.group.others.map(_.rarity))
        third  = w3._2.toList.flatMap(x => (x.monsterRarity, x.customName) :: x.group.others.map(s => (s.rarity, s.customName)))
      } yield assertTrue(second.size == 4 && second.forall(r => secondRanks.contains(r))) &&
              assertTrue(third.count(_._1 == Rarity.Mythical.entryName) == 3) &&
              assertTrue(third.count(t => t._1 == Rarity.Legendary.entryName && t._2.contains(HedgeSongQuest.LeaderName)) == 1) &&
              assertTrue(third.size == 4 && w3._4.contains("Последняя волна")) &&
              assertTrue(w3._3.exists(_.eventData.flatMap(_.as[HedgeSongState.Progress].toOption).contains(HedgeSongState.Progress(3))))
    },

    test("в этом бою вожак и командиры не зовут сородичей после первого раунда — как во всяком сюжетном бою") {
      val boss = Monster(0L, 7L, Race.Orc, Rarity.Legendary,
        FightStats(atk = 20, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))
      val h = hero().copy(lvl = 10L, fightStats = FightStats(atk = 20, hp = 500000, armor = 0, defence = 0,
        evasion = 0, accuracy = 9999, energy = 0))
      def round(story: Option[String]) =
        for {
          d <- TestHeroDao.withHero(userId, h)
          _ <- d.writeActiveBattle(userId, SoloPveBattle.from(boss, h).copy(story = story).asJson)
          r <- TestRenderer.make
          c <- content
          // тот же раунд, что в GroupBattleSpec: герой попал, моб ответил, подкрепления нет, зов — трое
          _ <- TestRandom.feedInts(60, 99, 99, 3, 1, 0, 1) *> TestRandom.feedLongs(100L, 100L, 5L, 5L, 5L)
          _ <- BattleState(d, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
          b <- battleOf(d)
          t <- texts(r)
        } yield (b, t)
      for {
        plain <- round(None)
        hedge <- round(Some(HedgeSongQuest.WaveStory))
      } yield assertTrue(plain._2.contains("на зов сбегаются сородичи") && plain._1.exists(_.group.others.size == 3)) &&
              assertTrue(!hedge._2.contains("на зов сбегаются сородичи") && hedge._1.exists(_.group.others.isEmpty)) &&
              assertTrue(hedge._1.exists(_.group.round == 1))
    },

    test("между волнами: «К бою», «Персонаж» и «Уйти»; уход — задание не начато, волны забыты") {
      for {
        d    <- dao(hero())
        _    <- d.writeSceneData(userId, HedgeSongState.Progress(1).asJson)
        r    <- TestRenderer.make
        c    <- content
        st    = scene(d, TestInventoryRepository.withItems(Nil), c)
        _    <- st.enter(testUser, r)
        mid  <- r.sentScreens.map(_.last)
        menu <- st.action(testUser, tap("OpenCharacter"), r)
        back <- d.readReturnState(userId)
        _    <- st.enter(testUser, r)
        same <- r.sentScreens.map(_.last)
        out  <- st.action(testUser, tap("HedgeLeave"), r)
        q    <- questOf(d)
        sd   <- d.readSceneData(userId)
      } yield assertTrue(mid.text.contains("идёт вторая волна")) &&
              assertTrue(mid.choices.map(_.id) == List("HedgeFight", "OpenCharacter", "HedgeLeave")) &&
              assertTrue(menu == StateType.HeroStats && back.contains(StateType.HedgeSong)) &&
              // из «Персонажа» — к той же волне, а не к новой встрече
              assertTrue(same.text == mid.text) &&
              assertTrue(out == StateType.Dungeon && HedgeSongQuest.fresh(q) && sd.forall(_.isNull))
    },

    test("Фаэлин при герое — младший брат Элиара: свои слова на встрече, между волнами и в конце; был у изгороди — так и записано") {
      val withFaelin = hero().copy(squad = hero().squad.hire(AllyKind.Elf, 1L, 0L))
      def screen(h: Hero, cleared: Int) =
        for {
          d <- dao(h)
          _ <- ZIO.when(cleared > 0)(d.writeSceneData(userId, HedgeSongState.Progress(cleared).asJson))
          r <- TestRenderer.make
          c <- content
          _ <- scene(d, TestInventoryRepository.accepting, c).enter(testUser, r)
          t <- texts(r)
          q <- questOf(d)
        } yield (t, q)
      for {
        meet   <- screen(withFaelin, 0)
        plain  <- screen(hero(), 0)
        kin    <- screen(withFaelin.copy(race = Race.Elf), 0)
        first  <- screen(withFaelin, 1)
        second <- screen(withFaelin, 2)
        won    <- screen(withFaelin, 3)
        alone  <- screen(hero(), 3)
      } yield assertTrue(meet._1.contains("Я Элиар") && meet._1.contains("Мой старший брат. Так поёт только он.")) &&
              assertTrue(meet._1.indexOf("Мой старший брат") < meet._1.indexOf("Сложность:")) &&
              assertTrue(!plain._1.contains("Фаэлин")) &&
              // слова Фаэлина одни на любую кровь героя: эльфу Элиар говорит своё, а Фаэлин — то же
              assertTrue(kin._1.contains("родич") && kin._1.contains("Мой старший брат")) &&
              assertTrue(first._1.contains("идёт вторая волна") && first._1.contains("Сбивался всегда я.")) &&
              assertTrue(second._1.contains("Бокк Последний у Костра") && second._1.contains("Песней его не погасишь.")) &&
              assertTrue(won._1.contains("Изгородь вздыхает всей стеной") && won._1.contains("Петь ты так и не выучился.")) &&
              assertTrue(HedgeSongQuest.toFet(won._2) && HedgeSongQuest.faelinWasThere(won._2)) &&
              assertTrue(!alone._1.contains("Фаэлин") && HedgeSongQuest.toFet(alone._2) && !HedgeSongQuest.faelinWasThere(alone._2))
    },

    test("«Уйти» между волнами — Фаэлин остаётся с братом: из отряда вон, за стол через сутки, без реплики о свитке; «Не сейчас» — уходит с героем") {
      val withFaelin = hero().copy(squad = hero().squad.hire(AllyKind.Elf, 1L, 0L))
      for {
        c     <- content
        d1    <- dao(withFaelin)
        _     <- d1.writeSceneData(userId, HedgeSongState.Progress(1).asJson)
        r1    <- TestRenderer.make
        out1  <- scene(d1, TestInventoryRepository.withItems(Nil), c).action(testUser, tap("HedgeLeave"), r1)
        left  <- texts(r1)
        h1    <- d1.getHeroByUserId(userId).map(_.get)
        q1    <- questOf(d1)
        d2    <- dao(withFaelin)
        r2    <- TestRenderer.make
        out2  <- scene(d2, TestInventoryRepository.withItems(Nil), c).action(testUser, tap("HedgeNotNow"), r2)
        later <- texts(r2)
        h2    <- d2.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(out1 == StateType.Dungeon && HedgeSongQuest.fresh(q1)) &&
              assertTrue(left.contains("Я его одного не оставлю") && !left.contains("обрывается на полуслове")) &&
              assertTrue(!h1.squad.has(AllyKind.Elf) && h1.squad.offDuty.get(AllyKind.Elf.entryName).contains(AllyRates.AwayMs)) &&
              assertTrue(!h1.squad.away.contains(AllyKind.Elf.entryName)) &&
              assertTrue(out2 == StateType.Dungeon && later.contains("Как знаешь") && later.contains("Он допоёт и без нас.")) &&
              assertTrue(h2.squad.has(AllyKind.Elf))
    },

    test("настоящий бой третьей волны: добыча как за обычный бой, назад к эльфу — ветвь в сумке, задание взято; Элиар в отряд не встаёт") {
      val weakOrc = Monster(0L, 7L, Race.Orc, Rarity.Mythical,
        FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      val strong = hero().copy(fightStats = FightStats(atk = 500, hp = 2000, armor = 0, defence = 0,
        evasion = 9999, accuracy = 9999, energy = 0))
      val battle = HedgeSongQuest.withElf(SoloPveBattle.fromGroup(List(weakOrc), strong, Nil, squad = false)
        .copy(story = Some(HedgeSongQuest.WaveStory), lootLevel = Some(7L)))
      for {
        d     <- dao(strong)
        _     <- d.writeActiveBattle(userId, battle.asJson)
        _     <- d.writeSceneData(userId, LootData(Nil, Nil, returnState = Some(StateType.HedgeSong),
                   eventData = Some(HedgeSongState.Progress(3).asJson)).asJson)
        inv    = TestInventoryRepository.accepting
        r     <- TestRenderer.make
        c     <- content
        won   <- BattleState(d, inv, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        j     <- TestJournal.make
        back  <- LootState(d, inv, TestItemRepository.make, j, c).action(testUser, tap("Continue"), r)
        _     <- scene(d, inv, c).enter(testUser, r)
        said  <- texts(r)
        q     <- questOf(d)
        h     <- d.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(won == StateType.Loot && back == StateType.HedgeSong) &&
              // обычная награда: опыт за павшего и добыча по таблице — экран добычи не пустой
              assertTrue(h.exp > 0L && loot.exists(l => l.silvers.nonEmpty || l.items.nonEmpty)) &&
              assertTrue(said.contains("Изгородь вздыхает всей стеной") && said.contains("«Ветвь живой изгороди»")) &&
              assertTrue(HedgeSongQuest.toFet(q) && inv.snapshot.exists(_.questItem.contains(QuestItemKind.HedgeBranch))) &&
              assertTrue(!h.squad.has(AllyKind.HedgeElf))
    },

    test("смерть в волнах: проснуться в лабиринте, эльфа нет — задание не начато; упал Элиар — его прячет изгородь") {
      for {
        d    <- dao(hero())
        b     = SoloPveBattle.fromGroup(HedgeSongQuest.wave(1, pangea.domain.Rng(1L))._1, hero(), Nil)
                  .copy(story = Some(HedgeSongQuest.WaveStory))
        _    <- d.writeActiveBattle(userId, b.asJson)
        r    <- TestRenderer.make
        c    <- content
        _    <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        wake <- d.readSceneData(userId)
        q    <- questOf(d)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("hedgeSong.death")))) &&
              assertTrue(wake.exists(_.hcursor.downField("wakeTo").focus.isEmpty) && HedgeSongQuest.fresh(q)) &&
              assertTrue(BattleState.goneKey(HedgeSongQuest.elf) == "battle.ally.hedgeElfDown") &&
              assertTrue(c.text("battle.ally.hedgeElfDown").contains("изгородь обвивает его ветвями")) &&
              assertTrue(BattleState.RaidStories.contains(HedgeSongQuest.WaveStory))
    },

    test("Фет: шкафа нет — новый, ступень 1/5 и три места; задание закрыто, потолок шкафа — 5") {
      for {
        d     <- dao(hero(), Some(onHands))
        inv    = TestInventoryRepository.withItems(List(branch))
        arts   = TestArtifactRepository.empty
        r     <- TestRenderer.make
        c     <- content
        state  = FetShopState(d, arts, inv, c, Some(TestItemRepository.make))
        _     <- state.enter(testUser, r)
        menu  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("FetBranch"), r)
        said  <- texts(r)
        q     <- questOf(d)
        after <- r.sentScreens.map(_.last)
      } yield assertTrue(menu.choices.exists(_.id == "FetBranch")) &&
              assertTrue(arts.snapshot.wardrobe.tier == 1 && arts.snapshot.wardrobe.capacity == 3) &&
              assertTrue(q.isDone(NpcQuest.HedgeSong) && HeroArtifacts.maxTier(ArtifactKind.Wardrobe, q) == 5) &&
              assertTrue(!inv.snapshot.exists(_.questItem.contains(QuestItemKind.HedgeBranch))) &&
              assertTrue(said.contains("Элиар жив?") && said.contains("Новый 🚪 Миниатюрный шкаф — ваш: ступень 1/5, мест внутри: 3")) &&
              assertTrue(said.contains("Получено") && said.contains("📜 Квест «Тот, кто уговаривает деревья» завершён.")) &&
              assertTrue(!after.choices.exists(_.id == "FetBranch"))
    },

    test("Фет: шкаф собран до конца — пятая ступень, 15 мест; дальше за дублоны не растёт") {
      for {
        d     <- dao(hero().copy(doubloons = 1000L), Some(onHands))
        arts   = TestArtifactRepository.of(wardrobe = Artifact(ArtifactKind.Wardrobe, HeroArtifacts.MaxTier, 0, Items(Nil)))
        r     <- TestRenderer.make
        c     <- content
        state  = FetShopState(d, arts, TestInventoryRepository.withItems(List(branch)), c, Some(TestItemRepository.make))
        _     <- state.action(testUser, tap("FetBranch"), r)
        said  <- texts(r)
        _     <- state.action(testUser, tap("FetGoods", "kind" -> "Wardrobe"), r)
        goods <- r.sentScreens.map(_.last)
      } yield assertTrue(arts.snapshot.wardrobe.tier == 5 && arts.snapshot.wardrobe.capacity == 15) &&
              assertTrue(said.contains("врастает в него") && said.contains("ступень 5/5, мест внутри: 15")) &&
              assertTrue(!goods.choices.exists(_.id == "FetBuy"))
    },

    test("журнал и карточка ветви: что делать и сложность; без ветви на руках кнопки у Фета нет") {
      for {
        c    <- content
        text  = QuestJournal.render(c, hero(), LoreData.empty, onHands, DailyQuests.empty, None, 0L)
        card  = InventoryState.cardText(c, branch, hero(), onHands)
        d    <- dao(hero())
        r    <- TestRenderer.make
        _    <- FetShopState(d, TestArtifactRepository.empty, TestInventoryRepository.withItems(Nil), c,
                  Some(TestItemRepository.make)).enter(testUser, r)
        menu <- r.sentScreens.map(_.last)
      } yield assertTrue(text.contains("«Тот, кто уговаривает деревья» — отнести ветвь живой изгороди Фету в Торговый дом")) &&
              assertTrue(card.contains(Difficulty.render(7)) && card.contains(QuestItemKind.HedgeBranch.description)) &&
              assertTrue(!menu.choices.exists(_.id == "FetBranch"))
    }
  )
}
