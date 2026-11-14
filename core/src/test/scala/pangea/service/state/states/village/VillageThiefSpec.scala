package pangea.service.state.states.village

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.model.battle.{BattleEffects, MonsterSlot, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.NpcQuests
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.{ArtifactTrail, VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.service.schedule.Scheduler
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.{NpcQuestLog, UserAction, Villages}
import pangea.test._
import zio.test._
import zio.{Task, ZIO, durationInt}

/** Вор с артефактом в бою в стане не задерживается: стоит первым и, если не
  * свалить его за четыре раунда, уходит живым со свёртком, а банда дерётся
  * дальше. Победа над ней — и сразу переход по его следу, а за ним он сам,
  * загнанный, с двумя подручными: второй раз бежать ему некуда. */
object VillageThiefSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private val legDone: UserAction = UserAction("", Some(VillageTrailState.LegAction))

  private val Level      = 52
  private val QuestLevel = 54
  private val Silver     = 2700L

  // Деревня мурлоков (Илистая Заводь), святыня — Жемчужина Прилива.
  private val scene = VillageScene(Race.Murloc, 0, VillageQuest.StolenArtifact, Level, QuestLevel, Silver, city = 2)
  private val Name  = "Илистая Заводь"
  private val Pearl = "Жемчужина Прилива"
  private def contract(step: Int = VillageContract.Taken): VillageContract = VillageContract.of(scene).copy(step = step)
  private def quests(step: Int = VillageContract.Taken): NpcQuests = NpcQuests.empty.copy(village = Some(contract(step)))
  private def paper: Item = QuestItemKind.item(QuestItemKind.VillageContract).copy(id = 901L)

  private val Thief     = "Грызь Кривозуб"
  private val ThiefRace = Race.Goblin

  private val content = ZIO.attempt(SceneContent.load())

  private def walker: Hero = TestFixtures.hero(userId, dungeonLevel = Level).copy(lvl = 55L, silver = 100L)

  private def dao(q: Option[NpcQuests] = Some(quests()), data: Option[Json] = None, h: Hero = walker) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
      _ <- ZIO.foreachDiscard(data)(d.writeSceneData(userId, _))
    } yield d

  private def trail(d: TestHeroDao, c: SceneContent): Task[(VillageTrailState, TestScheduler)] =
    TestScheduler.make.map(s => (VillageTrailState(d, TestInventoryRepository.withItems(List(paper)), s, c), s))

  private def trailOf(d: TestHeroDao): Task[Option[ArtifactTrail]] =
    d.readSceneData(userId).map(_.flatMap(_.as[ArtifactTrail].toOption))

  private def battleOf(d: TestHeroDao): Task[Option[SoloPveBattle]] =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  /** Погоня у стана, где вора уже знают по имени. */
  private val atCamp: ArtifactTrail = ArtifactTrail(2, 3, 0L, thief = Some(Thief), thiefRace = Some(ThiefRace))

  /** Вор ушёл: один переход по его следу, он уже кончился. */
  private val chasing: ArtifactTrail = ArtifactTrail.after(Some(Thief), Some(ThiefRace), 60L * 1000L)

  // ── Бой ─────────────────────────────────────────────────────────────────────

  private def fighter: Hero =
    TestFixtures.hero(userId, state = StateType.Battle).copy(
      lvl        = 10L,
      baseStats  = BaseStats(agi = 10, vit = 10, str = 10, int = 10),
      fightStats = FightStats(atk = 20, hp = 5000, armor = 500, defence = 10, evasion = 5, accuracy = 20, energy = 100))

  private val crewStats = FightStats(atk = 1, hp = 1000, armor = 0, defence = 0, evasion = 0, accuracy = 10000, energy = 0)
  private val crewSlot  = MonsterSlot(10L, Race.Orc.entryName, Rarity.Common.entryName, crewStats, 1000L, 0L,
    marked = false, currentEnergy = 0L, effects = BattleEffects.empty)

  /** Вор в паре с героем: живуч, бьёт слабо — за раунд его не свалить. */
  private def thiefMob: Monster =
    Monster(0L, 10L, ThiefRace, Rarity.Legendary,
      FightStats(atk = 1, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 10000, energy = 0),
      customName = Some(Thief))

  /** Бой в стане, где вору пора уходить: срок — первый раунд. */
  private def thiefFight(b: SoloPveBattle): SoloPveBattle =
    b.copy(escapesAfter = 1, preyName = Some(Thief), noKin = true, story = Some(Villages.ArtifactStory))

  private def battleState(d: TestHeroDao, c: SceneContent): BattleState =
    BattleState(d, TestInventoryRepository.withItems(Nil), TestItemRepository.make, c, Scheduler.none)

  override def spec = suite("Вор уходит из боя")(

    test("у стана — подсказка: вор уйдёт, если не свалить его за четыре раунда") {
      for {
        c     <- content
        d     <- dao(data = Some(ArtifactTrail(2, 3, 0L).asJson))
        r     <- TestRenderer.make
        st    <- trail(d, c).map(_._1)
        _     <- st.enter(testUser, r)
        camp  <- r.sentScreens.map(_.last)
      } yield assertTrue(VillageRates.ThiefEscapeRounds == 4 &&
                camp.text.contains("Вор со свёртком драться не станет: не свалите его за 4 раунда"))
    },

    test("бой в стане: вор первым, с именем и сроком — четыре раунда; кто он — в погоне, она едет в данных события") {
      for {
        c    <- content
        d    <- dao(data = Some(ArtifactTrail(2, 3, 0L).asJson))
        r    <- TestRenderer.make
        st   <- trail(d, c).map(_._1)
        _    <- st.action(testUser, tap("VillageTrailFight"), r)
        b    <- battleOf(d)
        loot <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        back  = loot.flatMap(_.eventData)
        t     = back.flatMap(_.as[ArtifactTrail].toOption)
      } yield assertTrue(b.exists(x => x.escapesAfter == VillageRates.ThiefEscapeRounds && x.preyName.isDefined &&
                x.preyName == x.customName && x.monsterRarity == Rarity.Legendary.entryName)) &&
              assertTrue(t.exists(x => x.thief == b.flatMap(_.preyName) && x.thiefRace.isDefined && !x.escaped && x.atCamp)) &&
              assertTrue(back.exists(VillageTrailState.won) && !back.exists(SoloPveBattle.preyLeftIn))
    },

    test("вор в паре уходит: его строка, подручный встаёт к герою, бой идёт дальше, а бой помнит, что вор ушёл") {
      for {
        c     <- content
        d     <- dao(q = None, h = fighter)
        _     <- d.writeActiveBattle(userId, thiefFight(SoloPveBattle.from(thiefMob, fighter).withReinforcement(crewSlot)).asJson)
        r     <- TestRenderer.make
        _     <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
        out   <- battleState(d, c).action(testUser, tap("Attack\",\"target\":\"1"), r)
        after <- battleOf(d)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(out == StateType.Battle &&
                said.contains(s"💨 $Thief ныряет в кусты, прижимая к груди свёрток, — и уходит с артефактом. Подручные остаются драться.")) &&
              assertTrue(after.exists(b => b.preyLeft && b.customName.isEmpty && b.escapesAfter == 0 && b.group.slain.isEmpty))
    },

    test("вор ушёл последним, банду уже перебили — победа; добыча несёт в сцену отметку, что вор ушёл") {
      val alone = thiefFight(SoloPveBattle.from(thiefMob, fighter))
      for {
        c     <- content
        d     <- dao(q = None, h = fighter)
        _     <- d.writeActiveBattle(userId, alone.copy(group = alone.group.copy(slain = List(crewSlot.slain))).asJson)
        _     <- d.writeSceneData(userId, LootData(Nil, Nil, returnState = Some(StateType.VillageTrail),
                   eventData = Some(atCamp.asJson.deepMerge(VillageTrailState.wonMark))).asJson)
        r     <- TestRenderer.make
        _     <- TestRandom.feedInts(60, 99) *> TestRandom.feedLongs(100L, 100L)
        out   <- battleState(d, c).action(testUser, tap("Attack"), r)
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        back   = loot.flatMap(_.eventData)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(out == StateType.Loot && said.contains(s"💨 $Thief ныряет в кусты") && !said.contains("Подручные остаются")) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.VillageTrail)) &&
                back.exists(SoloPveBattle.preyLeftIn) && back.exists(VillageTrailState.won) &&
                back.flatMap(_.as[ArtifactTrail].toOption).exists(_.thief.contains(Thief)))
    },

    test("с добычи, а вор ушёл: сразу переход по его следу — кто вор, погоня помнит; таймер, контракт не тронут") {
      val back = atCamp.asJson.deepMerge(VillageTrailState.wonMark).deepMerge(SoloPveBattle.markPreyLeft(Json.obj()))
      for {
        c     <- content
        d     <- dao(data = Some(back))
        r     <- TestRenderer.make
        pair  <- trail(d, c)
        (st, sched) = pair
        _     <- st.enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
        t     <- trailOf(d)
        tasks <- sched.scheduled
        q     <- NpcQuestLog.load(d, userId)
      } yield assertTrue(shown.text.contains(s"Бой кончен, но свёртка нигде нет. $Thief бежит с ним через изгородь") &&
                shown.text.contains("Беглец не прячет следов")) &&
              assertTrue(t.exists(x => x.escaped && x.legs == 1 && x.leg == 1 && x.nextAt >= VillageRates.TrailLegMinMs &&
                x.thief.contains(Thief) && x.thiefRace.contains(ThiefRace))) &&
              assertTrue(tasks.map(_.kind) == List(TaskKind.VillageTrail)) &&
              assertTrue(q.village.exists(v => !v.done))
    },

    test("след беглеца кончился — загнанный вор: свой текст, без подсказки о бегстве; бой — он и двое, бежать некуда") {
      for {
        c     <- content
        d     <- dao(data = Some(chasing.asJson))
        r     <- TestRenderer.make
        st    <- trail(d, c).map(_._1)
        _     <- TestClock.adjust(10.minutes)
        _     <- st.action(testUser, legDone, r)
        camp  <- r.sentScreens.map(_.last)
        t     <- trailOf(d)
        res   <- st.action(testUser, tap("VillageTrailFight"), r)
        fight <- r.sentScreens.map(_.last)
        b     <- battleOf(d)
        races  = b.toList.flatMap(x => x.monsterRace :: (x.group.others ++ x.group.queue).map(_.race))
      } yield assertTrue(camp.text.contains(s"$Thief ждёт внизу, прижавшись спиной к осыпи") &&
                !camp.text.contains("драться не станет") && camp.text.contains("Сбежать из этого боя не получится") &&
                camp.choices.map(_.id).headOption.contains("VillageTrailFight")) &&
              assertTrue(t.exists(x => x.atCamp && x.escaped)) &&
              assertTrue(res == StateType.Battle && fight.text.contains(s"$Thief выхватывает нож")) &&
              assertTrue(b.exists(x => x.customName.contains(Thief) && x.monsterRace == ThiefRace.entryName &&
                x.escapesAfter == 0 && x.preyName.isEmpty && x.story.contains(Villages.ArtifactStory))) &&
              assertTrue(races.size == 1 + VillageRates.CorneredHenchmen)
    },

    test("загнанный вор повержен: артефакт у героя — своя развязка, контракт «сделано»") {
      for {
        c   <- content
        d   <- dao(data = Some(chasing.copy(leg = 2, nextAt = 0L).asJson.deepMerge(VillageTrailState.wonMark)))
        r   <- TestRenderer.make
        st  <- trail(d, c).map(_._1)
        _   <- st.enter(testUser, r)
        won <- r.sentScreens.map(_.last)
        q   <- NpcQuestLog.load(d, userId)
      } yield assertTrue(won.text.contains(s"Последний из троих падает у осыпи") &&
                won.text.contains(s"внутри — $Pearl. Вещь цела, только в глине") &&
                won.text.contains(s"старосте деревни «$Name»") && !won.text.contains("у костра")) &&
              assertTrue(q.village.exists(_.done))
    },

    test("загнанный вор: та же раса и имя, и двое подручных покрепче; не запомнили — бросок заново") {
      val gang  = Villages.cornered(contract(), Some(Thief), Some(ThiefRace), Rng(3L))
      val fresh = Villages.cornered(contract(), None, None, Rng(3L))
      assertTrue(gang.size == 1 + VillageRates.CorneredHenchmen && gang.forall(_.lvl == QuestLevel.toLong)) &&
      assertTrue(gang.headOption.exists(m => m.customName.contains(Thief) && m.race == ThiefRace && m.rarity == Rarity.Legendary)) &&
      assertTrue(gang.drop(1).forall(m => VillageRates.ThiefBackRarities.contains(m.rarity) && Race.mortals.contains(m.race))) &&
      assertTrue(fresh.headOption.exists(m => m.customName.exists(_.nonEmpty) && Race.mortals.contains(m.race) &&
        m.rarity == Rarity.Legendary))
    },

    test("записи: погоня с вором читается и без новых полей; бой без отметки — вор не уходил") {
      val t   = ArtifactTrail(1, 1, 5L, escaped = true, thief = Some(Thief), thiefRace = Some(ThiefRace))
      val old = SoloPveBattle.from(thiefMob, fighter).asJson.mapObject(_.remove("preyLeft"))
      assertTrue(t.asJson.as[ArtifactTrail] == Right(t)) &&
      assertTrue(Json.obj("trailLegs" -> 3.asJson, "trailLeg" -> 2.asJson).as[ArtifactTrail] == Right(ArtifactTrail(3, 2, 0L))) &&
      assertTrue(old.as[SoloPveBattle].exists(b => !b.preyLeft)) &&
      assertTrue(SoloPveBattle.preyLeftIn(SoloPveBattle.markPreyLeft(Json.obj("x" -> 1.asJson))) &&
        !SoloPveBattle.preyLeftIn(Json.obj()))
    }
  )
}
