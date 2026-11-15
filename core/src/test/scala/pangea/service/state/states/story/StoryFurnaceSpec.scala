package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryFurnaceQuest.Fork
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryNewcomerQuest.Fate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.home.HomeState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryFurnaceQuest, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Акт II, глава 9 «Печь»: Загон, старая солеварня, где с живых снимают Клеймо.
  * Тень привозит урожай Братства курьером — легенда держится, боя нет; Щит
  * находит Загон через Юнону и берёт его штурмом с Горном и асессором Штилем;
  * одиночка с соляным караваном — ночью через крышу, с капитанами каравана.
  * Развилка у печи — разбить или оставить — у всех; исход — в «как и вы».
  * Помнятся новенький из главы 7 и Смотритель из главы 2. В сумке остаётся
  * книга печи, глава 10 висит до обновления. */
object StoryFurnaceSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, state = StateType.GlobalMap).copy(lvl = 41L, silver = 100L)

  /** Главы 1–8 пройдены (путь `path`, новенький — `newcomer`, Смотритель —
    * `warden`), «Печь» на шаге `n`; одиночке — «Соль» по `salt`. */
  private def furnace(path: Path, n: Int, newcomer: Fate = Fate.Hedge, warden: WardenFate = WardenFate.Killed,
                      salt: Boolean = true): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    val face = StoryPowderQuest.finish(base, warden).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = SojkaFate.Broke.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
    val done = StoryToolQuest.finish(face.take(NpcQuest.StoryTool, 0L), path)
      .take(NpcQuest.StoryMorning, 0L).finish(NpcQuest.StoryMorning)
      .take(NpcQuest.StoryNewcomer, 0L).finish(NpcQuest.StoryNewcomer)
      .update(NpcQuest.StoryNewcomer)(_.copy(counter = newcomer.n))
      .take(NpcQuest.StoryClearing, 0L).finish(NpcQuest.StoryClearing)
      .take(NpcQuest.StoryFurnace, 0L)
    val salted = if (salt) done.take(NpcQuest.BartiSalt, 0L).finish(NpcQuest.BartiSalt) else done
    StoryFurnaceQuest.step(salted, n)
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

  private val waybill: Item = QuestItemKind.item(QuestItemKind.StoryWaybill).copy(id = 985L)
  private val ledger: Item  = QuestItemKind.item(QuestItemKind.StoryLedger).copy(id = 986L)

  private def pen(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent, o: Option[TestQuestOutcomeDao] = None) =
    StoryFurnaceState(d, inv, TestItemRepository.make, c, o)

  override def spec = suite("Акт II, глава 9 «Печь»")(

    test("карточка накладной: Тени — урожай в Загон; Щиту — к Юноне, потом штурм; одиночке — после «Соли» с караваном; после боя — к печи") {
      def card(q: NpcQuests) =
        for {
          c     <- content
          d     <- dao(q)
          _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
          r     <- TestRenderer.make
          st     = InventoryState(d, TestInventoryRepository.withItems(List(waybill)), TestItemRepository.make, c)
          _     <- st.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${waybill.id}"}""")), r)
          shown <- r.sentScreens.map(_.last)
          go    <- st.action(testUser, tap("StoryFurnaceGo"), r)
        } yield (shown, go)
      def label(s: pangea.engine.Screen) = s.choices.find(_.id == "StoryFurnaceGo").map(_.label)
      for {
        shadow  <- card(furnace(Path.Shadow, 1))
        shield1 <- card(furnace(Path.Shield, 1))
        shield2 <- card(furnace(Path.Shield, 2))
        shield3 <- card(furnace(Path.Shield, 3))
        alone   <- card(furnace(Path.Alone, 1))
        noSalt  <- card(furnace(Path.Alone, 1, salt = false))
      } yield assertTrue(shadow._1.text.contains("Я теперь не только свой, но и курьер") &&
                shadow._1.text.contains(Difficulty.render(StoryFurnaceQuest.Level)) &&
                label(shadow._1).contains("🛒 Везти урожай") && shadow._2 == StateType.StoryFurnace) &&
              assertTrue(shield1._1.text.contains("Юнона просила зайти") && label(shield1._1).isEmpty && shield1._2 != StateType.StoryFurnace) &&
              assertTrue(shield2._1.text.contains("Горн и асессор Штиль ждут") && label(shield2._1).contains("⚔ В Загон") &&
                shield2._2 == StateType.StoryFurnace) &&
              assertTrue(label(shield3._1).contains("🔥 К печи")) &&
              assertTrue(alone._1.text.contains("Ра'Тхар нашёл меня в Кинэте") && label(alone._1).contains("🐪 С караваном") &&
                alone._2 == StateType.StoryFurnace) &&
              assertTrue(noSalt._1.text.contains("Барти знает всех") && label(noSalt._1).isEmpty &&
                noSalt._1.choices.exists(_.id == "BartiSaltAsk") && noSalt._2 != StateType.StoryFurnace)
    },

    test("Щит: Юнона находит Загон в бумагах совета — склад соли за монету, подпись не её; дальше штурм") {
      for {
        c      <- content
        d      <- dao(furnace(Path.Shield, 1))
        sch    <- TestScheduler.make
        home    = HomeState(d, sch, c, TestInventoryRepository.withItems(List(waybill)), TestItemRepository.make)
        r      <- TestRenderer.make
        _      <- home.action(testUser, tap("HomeOffice"), r)
        office <- r.sentScreens.map(_.last)
        _      <- home.action(testUser, tap("HomeJunonaPen"), r)
        said   <- texts(r)
        q      <- questOf(d)
      } yield assertTrue(office.choices.exists(_.id == "HomeJunonaPen")) &&
              assertTrue(said.contains("на сто лет, за одну монету в год") && said.contains("Подпись, как ты уже догадываешься, не моя") &&
                said.contains("асессор Штиль — вызвался сам")) &&
              assertTrue(StoryFurnaceQuest.toFight(q))
    },

    test("Тень: легенда держится — боя нет, у печи сразу развилка; Мило здесь, если его отдали Братству или совету; Смотритель — приказчик, если его отпустили") {
      def at(q: NpcQuests) =
        for {
          c <- content
          d <- dao(q)
          r <- TestRenderer.make
          _ <- pen(d, TestInventoryRepository.withItems(List(waybill)), c).enter(testUser, r)
          s <- r.sentScreens.map(_.last)
        } yield s
      for {
        sold  <- at(furnace(Path.Shadow, 1, newcomer = Fate.Brotherhood, warden = WardenFate.Released))
        resold <- at(furnace(Path.Shadow, 1, newcomer = Fate.Council))
        saved <- at(furnace(Path.Shadow, 1, newcomer = Fate.Hedge))
      } yield assertTrue(sold.text.contains("Курьеров тут не разглядывают") && sold.text.contains("Пустые сидят в дальнем углу") &&
                sold.text.contains("В третьей клети — Мило") && sold.text.contains("приказчик — Смотритель с дегтярного склада") &&
                sold.text.contains("«Сборщику лично — отмеченный из Кинэта»") && sold.text.contains(Difficulty.render(StoryFurnaceQuest.Level))) &&
              assertTrue(sold.choices.map(_.id) == List("FurnaceBreak", "FurnaceKeep", "FurnaceLeave") &&
                sold.choices.headOption.map(_.label).contains("Залить топку рассолом")) &&
              assertTrue(resold.text.contains("В третьей клети — Мило") && !resold.text.contains("приказчик")) &&
              assertTrue(saved.text.contains("Мило здесь нет. Это единственная хорошая новость за день."))
    },

    test("бой: Щит — Колосник и четверо, Горн и асессор Штиль в строю; одиночка — Кочерга и четверо, Ра'Тхар и Ма'Кесс; без бегства") {
      def fight(q: NpcQuests) =
        for {
          c     <- content
          d     <- dao(q)
          r     <- TestRenderer.make
          st     = pen(d, TestInventoryRepository.withItems(List(waybill)), c)
          _     <- st.enter(testUser, r)
          shown <- r.sentScreens.map(_.last)
          res   <- st.action(testUser, tap("FurnaceFight"), r)
          start <- r.sentScreens.map(_.last.text)
          b     <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
          loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        } yield (shown, res, start, b, loot)
      def allies(b: Option[SoloPveBattle]) =
        b.toList.flatMap(_.group.allies).map(a => (a.kind, a.home, a.lvl, a.undead.map(_.name))).sortBy(_._4)
      val lvl = StoryFurnaceQuest.Level.toLong
      for {
        shield <- fight(furnace(Path.Shield, 2))
        alone  <- fight(furnace(Path.Alone, 1))
      } yield assertTrue(shield._1.text.contains("«Склад закрыт на учёт»") && alone._1.text.contains("Через неё мы и лезем") &&
                List(shield, alone).forall { case (shown, res, _, b, loot) =>
                  shown.choices.map(_.id) == List("FurnaceFight", "FurnaceLeave") && res == StateType.Battle &&
                    b.exists(x => x.cantFlee && x.monsterRace == Race.Human.entryName && x.monsterRarity == Rarity.Mythical.entryName &&
                      x.monsterLvl == lvl && x.group.others.size == 4 && x.group.others.forall(_.rarity == Rarity.Rare.entryName)) &&
                    loot.exists(_.returnState.contains(StateType.StoryFurnace))
                }) &&
              assertTrue(shield._4.exists(x => x.story.contains(StoryFurnaceQuest.RaidStory) && x.monsterName.contains("Колосник")) &&
                shield._3.contains("Асессор Штиль поправляет очки")) &&
              assertTrue(allies(shield._4) == List((AllyKind.CouncilEnvoy, 0, lvl, Some("Асессор Штиль")),
                (AllyKind.GuildMaster, 0, lvl, Some("Горн")))) &&
              assertTrue(alone._4.exists(x => x.story.contains(StoryFurnaceQuest.NightStory) && x.monsterName.contains("Кочерга")) &&
                alone._3.contains("Ра'Тхар закрывает за нами дверь")) &&
              assertTrue(allies(alone._4) == List((AllyKind.CaravanKhajiit, 0, lvl, Some("Ма'Кесс")),
                (AllyKind.CaravanKhajiit, 0, lvl, Some("Ра'Тхар")))) &&
              assertTrue(StoryFurnaceQuest.FightStories.forall(BattleState.RaidStories.contains))
    },

    test("развилка у Тени и одиночки закрывает главу у печи: книга печи в сумке, накладная уходит, исход в «как и вы», «Сборщик» висит") {
      def decide(q: NpcQuests, action: String) =
        for {
          c    <- content
          d    <- dao(q)
          inv   = TestInventoryRepository.withItems(List(waybill))
          o    <- TestQuestOutcomeDao.make
          r    <- TestRenderer.make
          _    <- pen(d, inv, c, Some(o)).action(testUser, tap(action), r)
          said <- texts(r)
          after <- questOf(d)
          h    <- heroOf(d)
          mine <- o.of(h.id, NpcQuest.StoryFurnace)
        } yield (said, after, h, inv.snapshot, mine)
      for {
        shadow <- decide(furnace(Path.Shadow, 1), "FurnaceBreak")
        alone  <- decide(furnace(Path.Alone, 2), "FurnaceKeep")
      } yield assertTrue(shadow._1.contains("я лью в топку два ведра рассола") && shadow._1.contains("Братство платит курьеру")) &&
              assertTrue(alone._1.contains("Клеймо у меня своё") && alone._1.contains("из сундука мастера печи")) &&
              assertTrue(List((shadow, Fork.Broken), (alone, Fork.Kept)).forall { case ((said, q, h, bag, mine), fork) =>
                said.contains("📜 Квест «Печь» завершён.") && said.contains("📜 Новое задание: «Сборщик»") &&
                  q.isDone(NpcQuest.StoryFurnace) && StoryFurnaceQuest.fork(q).contains(fork) && q.isTaken(NpcQuest.StoryCollector) &&
                  mine.contains(fork.outcome) && h.silver == 100L + StoryFurnaceQuest.silver && h.exp > 0L &&
                  bag.exists(_.questItem.contains(QuestItemKind.StoryLedger)) && !bag.exists(_.questItem.contains(QuestItemKind.StoryWaybill))
              })
    },

    test("Щит: после штурма — развилка, Горн разбивает печь или её забирает совет; книгу — Юноне, глава закрыта у неё; Мило продали за её спиной") {
      def run(newcomer: Fate, action: String) =
        for {
          c      <- content
          d      <- dao(furnace(Path.Shield, 2, newcomer = newcomer))
          _      <- d.writeSceneData(userId, StoryFight.mark(StoryFurnaceState.WonKey))
          inv     = TestInventoryRepository.withItems(List(waybill))
          o      <- TestQuestOutcomeDao.make
          r      <- TestRenderer.make
          st      = pen(d, inv, c, Some(o))
          _      <- st.enter(testUser, r)
          won    <- r.sentScreens.map(_.last)
          q1     <- questOf(d)
          _      <- st.action(testUser, tap(action), r)
          forked <- r.sentScreens.map(_.last.text)
          q2     <- questOf(d)
          sch    <- TestScheduler.make
          home    = HomeState(d, sch, c, inv, TestItemRepository.make, None, None, Some(o))
          r2     <- TestRenderer.make
          _      <- home.action(testUser, tap("HomeOffice"), r2)
          office <- r2.sentScreens.map(_.last)
          _      <- home.action(testUser, tap("HomeJunonaPenTell"), r2)
          told   <- texts(r2)
          q      <- questOf(d)
          h      <- heroOf(d)
          mine   <- o.of(h.id, NpcQuest.StoryFurnace)
        } yield (won, q1, forked, q2, office, told, q, h, mine)
      for {
        broke <- run(Fate.Council, "FurnaceBreak")
        kept  <- run(Fate.Hedge, "FurnaceKeep")
      } yield assertTrue(broke._1.text.contains("Горн проходит в ворота первым") && broke._1.text.contains("Печь гудит. Что с ней делать?") &&
                broke._1.choices.headOption.map(_.label).contains("🔨 Пусть Горн разобьёт") && StoryFurnaceQuest.toFork(broke._2)) &&
              assertTrue(broke._3.contains("Горн отодвигает его рукой, как дверь") && broke._3.contains("Книгу печи я везу Юноне") &&
                StoryFurnaceQuest.toTell(broke._4) && StoryFurnaceQuest.fork(broke._4).contains(Fork.Broken) &&
                !broke._4.isDone(NpcQuest.StoryFurnace)) &&
              assertTrue(broke._5.choices.exists(_.id == "HomeJunonaPenTell")) &&
              assertTrue(broke._6.contains("Я записала его в канцелярию своей рукой") && broke._6.contains("а я его уже потеряла") &&
                broke._6.contains("Гильдия и Юнона, из своих") && broke._6.contains("📜 Квест «Печь» завершён.") &&
                broke._6.contains("📜 Новое задание: «Сборщик»")) &&
              assertTrue(broke._7.isDone(NpcQuest.StoryFurnace) && broke._7.isTaken(NpcQuest.StoryCollector) &&
                broke._9.contains(Fork.Broken.outcome) && broke._8.silver == 100L + StoryFurnaceQuest.silver) &&
              assertTrue(kept._3.contains("«Совет благодарит, — говорит он. — Дальше мы сами»") &&
                kept._6.contains("Асессор Штиль будет её изучать") && !kept._6.contains("Я записала его") &&
                kept._9.contains(Fork.Kept.outcome))
    },

    test("смерть у Загона — проснуться в городе, шаг тот же; журнал — по пути; книга печи у Щита — к Юноне, потом — «Сборщик» с обновлением") {
      def die(q: NpcQuests, story: String) =
        for {
          c    <- content
          d    <- dao(q)
          fight = SoloPveBattle.fromGroup(StoryFurnaceQuest.foes("Колосник"), hero, List.fill(5)(0L)).copy(story = Some(story))
          _    <- d.writeActiveBattle(userId, fight.asJson)
          r    <- TestRenderer.make
          _    <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
          wake <- d.readSceneData(userId)
          q2   <- questOf(d)
        } yield (wake.flatMap(_.hcursor.get[List[String]]("wakeLines").toOption),
                 wake.flatMap(_.hcursor.get[StateType]("wakeTo").toOption), StoryFurnaceQuest.toFight(q2))
      for {
        c      <- content
        raid   <- die(furnace(Path.Shield, 2), StoryFurnaceQuest.RaidStory)
        night  <- die(furnace(Path.Alone, 1), StoryFurnaceQuest.NightStory)
        line    = (q: NpcQuests) => QuestJournal.render(c, hero, LoreData.empty, q, DailyQuests.empty, None, 0L)
        toTell  = furnace(Path.Shield, 4).update(NpcQuest.StoryFurnace)(_.copy(counter = Fork.Broken.n))
        closed  = furnace(Path.Alone, 2).finish(NpcQuest.StoryFurnace).take(NpcQuest.StoryCollector, 0L)
        tellCard = InventoryState.questText(c, ledger, toTell, 41L)
        soonCard = InventoryState.questText(c, ledger, closed, 41L)
      } yield assertTrue(raid == ((Some(List("story.storyFurnace.pen.deathShield")), Some(StateType.GlobalMap), true))) &&
              assertTrue(night == ((Some(List("story.storyFurnace.pen.deathAlone")), Some(StateType.GlobalMap), true))) &&
              assertTrue(line(furnace(Path.Shadow, 1)).contains("отвезти урожай Братства в Загон") &&
                line(furnace(Path.Shield, 3)).contains("решить, что делать с печью") &&
                line(furnace(Path.Alone, 1)).contains("доехать с соляным караваном до Загона")) &&
              assertTrue(tellCard.contains("Книга печи") && tellCard.contains("Рассказать Юноне, что нашлось в Загоне")) &&
              assertTrue(soonCard.contains("станет ясно с обновлением 🔜") && line(closed).contains("📌 «Сборщик»"))
    }
  )
}
