package pangea.service.state.states.village

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{MiniBoss, Race}
import pangea.model.quest.NpcQuests
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.state.StateType
import pangea.model.village.{ArtifactOffer, ArtifactTrail, VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.service.state.{NpcQuestLog, UserAction, Villages}
import pangea.test._
import zio.test._
import zio.{Task, ZIO, durationInt}

/** Покупатели артефакта (вариант Battle Brothers). Воров застают при сделке —
  * у костра с ними Некромант, минибосс: он стоит первым, вор — за ним. После
  * победы из кустов может выйти маг Сноходцев — тот, кто заказал кражу, — и
  * предложить за артефакт дублоны: продал — контракт сорван, отказал — к
  * старосте. Был при сделке Некромант — мага нет. */
object VillageBuyersSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private val legDone: UserAction = UserAction("", Some(VillageTrailState.LegAction))

  private val Level      = 52
  private val QuestLevel = 54
  private val Silver     = 2700L
  private val Doubloons  = 7L

  // Деревня мурлоков (Илистая Заводь): 2700 серебра и 7 дублонов.
  private val scene = VillageScene(Race.Murloc, 0, VillageQuest.StolenArtifact, Level, QuestLevel, Silver, city = 2,
    doubloons = Doubloons)
  private val Name  = "Илистая Заводь"
  private def contract(step: Int = VillageContract.Taken): VillageContract = VillageContract.of(scene).copy(step = step)
  private def quests(step: Int = VillageContract.Taken): NpcQuests = NpcQuests.empty.copy(village = Some(contract(step)))
  private def paper: Item = QuestItemKind.item(QuestItemKind.VillageContract).copy(id = 901L)

  // Маг даёт по дублону за каждые 400 серебра (2700 → 7) и дублоны деревни сверху.
  private val Offer = 14L

  private def hero: Hero = TestFixtures.hero(userId, dungeonLevel = Level).copy(lvl = 55L, silver = 100L, doubloons = 3L)

  private val content = ZIO.attempt(SceneContent.load())

  private def dao(q: Option[NpcQuests] = Some(quests()), data: Option[Json]) =
    for {
      d <- TestHeroDao.withHero(userId, hero)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
      _ <- ZIO.foreachDiscard(data)(d.writeSceneData(userId, _))
    } yield d

  private def trail(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent): Task[VillageTrailState] =
    TestScheduler.make.map(s => VillageTrailState(d, inv, s, c))

  private def trailOf(d: TestHeroDao): Task[Option[ArtifactTrail]] =
    d.readSceneData(userId).map(_.flatMap(_.as[ArtifactTrail].toOption))

  private def battleOf(d: TestHeroDao): Task[Option[SoloPveBattle]] =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def won(t: ArtifactTrail): Json = t.asJson.deepMerge(VillageTrailState.wonMark)

  override def spec = suite("Покупатели артефакта")(

    test("вышли к стану — бросок сделки: застали — у костра Некромант; не застали — обычный стан") {
      def arrive(roll: Int) =
        for {
          c    <- content
          d    <- dao(data = Some(ArtifactTrail(2, 2, 60L * 1000L).asJson))
          r    <- TestRenderer.make
          st   <- trail(d, TestInventoryRepository.withItems(List(paper)), c)
          _    <- TestClock.adjust(10.minutes)
          _    <- TestRandom.feedInts(roll)
          _    <- st.action(testUser, legDone, r)
          camp <- r.sentScreens.map(_.last)
          t    <- trailOf(d)
        } yield (camp, t)
      for {
        deal  <- arrive(0)
        plain <- arrive(99)
      } yield assertTrue(VillageRates.NecromancerPct == 25) &&
              assertTrue(deal._1.text.contains("Некромант. Вы успели к самой сделке") &&
                deal._1.text.contains("Вор со свёртком драться не станет") && deal._2.exists(t => t.atCamp && t.necromancer)) &&
              assertTrue(plain._1.text.contains("У огня голов семь") && plain._2.exists(t => t.atCamp && !t.necromancer))
    },

    test("бой при сделке: Некромант первым — минибосс на полной энергии, поднимает уровнем задания и расы вора; вор вторым и уходит") {
      for {
        c     <- content
        d     <- dao(data = Some(ArtifactTrail(2, 3, 0L, necromancer = true).asJson))
        r     <- TestRenderer.make
        st    <- trail(d, TestInventoryRepository.withItems(List(paper)), c)
        res   <- st.action(testUser, tap("VillageTrailFight"), r)
        shown <- r.sentScreens.map(_.last)
        b     <- battleOf(d)
        lord   = Villages.dealer(contract())
        thief  = b.flatMap(_.group.others.headOption)
      } yield assertTrue(res == StateType.Battle && shown.text.contains("Некромант медленно поворачивается к вам")) &&
              assertTrue(lord.lvl == MiniBoss.Necromancer.bossLvl(QuestLevel.toLong) && lord.customName.contains("Некромант")) &&
              assertTrue(b.exists(x => x.bossKind.contains(MiniBoss.Necromancer.entryName) && x.customName.contains("Некромант") &&
                x.monsterCurrentEnergy == lord.fightStats.energy && x.minionLvl == QuestLevel.toLong && x.noKin &&
                x.story.contains(Villages.ArtifactStory))) &&
              assertTrue(b.exists(x => x.group.others.size + x.group.queue.size == VillageRates.ThiefFront + VillageRates.ThiefBack + 1)) &&
              // вор — за Некромантом: по имени его срок, по его расе — поднятые
              assertTrue(b.exists(x => x.escapesAfter == VillageRates.ThiefEscapeRounds && x.preyName.isDefined &&
                x.preyName == thief.flatMap(_.customName) && x.group.originRace == thief.map(_.race)))
    },

    test("победа без Некроманта — с шансом маг Сноходцев: дублоны за 400 серебра и дублоны деревни; контракт ещё не сделан") {
      for {
        c     <- content
        d     <- dao(data = Some(won(ArtifactTrail(2, 3, 0L))))
        r     <- TestRenderer.make
        st    <- trail(d, TestInventoryRepository.withItems(List(paper)), c)
        _     <- TestRandom.feedInts(0)
        _     <- st.enter(testUser, r)
        shown <- r.sentScreens
        q     <- NpcQuestLog.load(d, userId)
        sd    <- d.readSceneData(userId)
        offer  = shown.lastOption
        found  = shown.dropRight(1).lastOption
      } yield assertTrue(VillageRates.MageOfferPct == 30 && VillageRates.mageDoubloons(Silver, Doubloons) == Offer) &&
              assertTrue(found.exists(s => s.text.contains("Вы разворачиваете его: внутри — Жемчужина Прилива") && s.choices.isEmpty)) &&
              assertTrue(offer.exists(s => s.text.contains("глаз в снежинке: знак Сноходцев") &&
                s.text.contains(s"плачу я золотом: $Offer дублонов") &&
                s.choices.map(_.id) == List("VillageTrailSell", "VillageTrailRefuse", "OpenCharacter") &&
                s.choices.headOption.exists(_.label == s"💰 Продать за $Offer дублонов"))) &&
              assertTrue(q.village.exists(v => !v.done) && sd.flatMap(_.as[ArtifactOffer].toOption).contains(ArtifactOffer(Offer)))
    },

    test("продать магу: контракт сорван — береста и договор уходят, дублоны у героя") {
      for {
        c     <- content
        d     <- dao(data = Some(ArtifactOffer(Offer).asJson))
        inv    = TestInventoryRepository.withItems(List(paper))
        r     <- TestRenderer.make
        st    <- trail(d, inv, c)
        res   <- st.action(testUser, tap("VillageTrailSell"), r)
        shown <- r.sentScreens.map(_.last)
        h     <- d.getHeroByUserId(userId).map(_.get)
        q     <- NpcQuestLog.load(d, userId)
        sd    <- d.readSceneData(userId)
        again <- st.action(testUser, tap("VillageTrailSell"), r) *> d.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(res == StateType.VillageTrail && shown.text.contains("Сноходец взвешивает свёрток") &&
                shown.text.contains(s"Контракт деревни «$Name» сорван") && shown.text.contains(s"💰 Получено $Offer дублонов.") &&
                shown.choices.map(_.id) == List("VillageTrailOut")) &&
              assertTrue(h.doubloons == hero.doubloons + Offer && h.silver == hero.silver && h.exp == hero.exp) &&
              assertTrue(q.village.isEmpty && inv.snapshot.isEmpty && sd.contains(Json.Null)) &&
              // старая кнопка второй платы не даёт
              assertTrue(again.doubloons == h.doubloons)
    },

    test("отказать магу: свёрток у героя — контракт «сделано», к старосте; дублонов нет") {
      for {
        c     <- content
        d     <- dao(data = Some(ArtifactOffer(Offer).asJson))
        inv    = TestInventoryRepository.withItems(List(paper))
        r     <- TestRenderer.make
        st    <- trail(d, inv, c)
        res   <- st.action(testUser, tap("VillageTrailRefuse"), r)
        shown <- r.sentScreens.map(_.last)
        h     <- d.getHeroByUserId(userId).map(_.get)
        q     <- NpcQuestLog.load(d, userId)
      } yield assertTrue(res == StateType.VillageTrail && shown.text.contains("Сноходец пожимает плечами") &&
                shown.text.contains(c.text("village.lair.returnHint")) && shown.choices.map(_.id) == List("VillageTrailOut")) &&
              assertTrue(q.village.exists(_.done) && h.doubloons == hero.doubloons && inv.snapshot.nonEmpty)
    },

    test("маг ждёт ответа: из меню — снова его предложение; «Идти по следу», «Уйти» и «В лабиринт» — старые кнопки") {
      for {
        c     <- content
        d     <- dao(data = Some(ArtifactOffer(Offer).asJson))
        r     <- TestRenderer.make
        st    <- trail(d, TestInventoryRepository.withItems(List(paper)), c)
        _     <- st.enter(testUser, r)
        back  <- r.sentScreens.map(_.last)
        go    <- st.action(testUser, tap("VillageTrailGo"), r)
        leave <- st.action(testUser, tap("VillageTrailLeave"), r)
        out   <- st.action(testUser, tap("VillageTrailOut"), r)
        sd    <- d.readSceneData(userId)
        last  <- r.sentScreens.map(_.last)
      } yield assertTrue(back.text.contains("глаз в снежинке") && back.choices.map(_.id).contains("VillageTrailSell")) &&
              assertTrue(go == StateType.VillageTrail && leave == StateType.VillageTrail && out == StateType.VillageTrail) &&
              assertTrue(sd.flatMap(_.as[ArtifactOffer].toOption).contains(ArtifactOffer(Offer)) && last.text.contains("глаз в снежинке"))
    },

    test("был при сделке Некромант — мага нет: сразу к старосте; и в погоне за беглецом это помнится") {
      val necro = ArtifactTrail(2, 3, 0L, thief = Some("Грызь Кривозуб"), thiefRace = Some(Race.Goblin), necromancer = true)
      for {
        c     <- content
        d1    <- dao(data = Some(won(necro)))
        r     <- TestRenderer.make
        st1   <- trail(d1, TestInventoryRepository.withItems(List(paper)), c)
        _     <- TestRandom.feedInts(0)
        _     <- st1.enter(testUser, r)
        done  <- r.sentScreens.map(_.last)
        q1    <- NpcQuestLog.load(d1, userId)
        d2    <- dao(data = Some(SoloPveBattle.markPreyLeft(won(necro))))
        st2   <- trail(d2, TestInventoryRepository.withItems(List(paper)), c)
        _     <- st2.enter(testUser, r)
        chase <- trailOf(d2)
      } yield assertTrue(!done.text.contains("Сноходцев") && done.text.contains(c.text("village.lair.returnHint")) &&
                q1.village.exists(_.done)) &&
              assertTrue(chase.exists(t => t.escaped && t.necromancer && t.thief.contains("Грызь Кривозуб")))
    },

    test("плата мага: по дублону за 400 серебра с округлением, дублоны деревни — сверху, не меньше одного; дублоны словами") {
      for {
        c <- content
      } yield assertTrue(VillageRates.MageSilverPerDoubloon == 400L) &&
              assertTrue(VillageRates.mageDoubloons(2700L, 7L) == 14L && VillageRates.mageDoubloons(2600L, 0L) == 7L &&
                VillageRates.mageDoubloons(2500L, 0L) == 6L && VillageRates.mageDoubloons(100L, 0L) == 1L &&
                VillageRates.mageDoubloons(0L, 0L) == 1L) &&
              assertTrue(List(1L, 3L, 14L, 21L).map(Villages.doubloonsLine(c, _)) ==
                List("1 дублон", "3 дублона", "14 дублонов", "21 дублон"))
    },

    test("записи: предложение мага — своим ключом; погоня без флага Некроманта — без него") {
      assertTrue(ArtifactOffer(Offer).asJson.as[ArtifactOffer] == Right(ArtifactOffer(Offer))) &&
      assertTrue(ArtifactOffer(Offer).asJson.as[ArtifactTrail].isLeft && ArtifactTrail(2, 1, 0L).asJson.as[ArtifactOffer].isLeft) &&
      assertTrue(Json.obj("trailLegs" -> 2.asJson).as[ArtifactTrail].exists(t => !t.necromancer)) &&
      assertTrue(ArtifactTrail(2, 3, 0L, necromancer = true).asJson.as[ArtifactTrail].exists(_.necromancer))
    }
  )
}
