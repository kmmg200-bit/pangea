package pangea.service.state.states.barti

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
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryNewcomerQuest.Fate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.story.StoryFight
import pangea.service.state.states.{DeathState, InventoryState, MarketSquareState}
import pangea.service.state.{BartiSaltQuest, NpcQuestLog, QuestJournal, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Соль» — побочное задание Барти в Акте II, только у пути «Сам по себе»: из
  * накладной герой знает слово «Загон», а Барти — что это старая солеварня и
  * что соль туда возит караван Та'Нахира. Ночью у Сухого колодца — к каравану;
  * на соляной колее дозор Загона: бой, капитаны Ра'Тхар и Ма'Кесс в строю.
  * Отбились — Та'Нахир платит дублонами и повезёт героя к печи, когда шум
  * уляжется. */
object BartiSaltSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, state = StateType.GlobalMap).copy(lvl = 39L, silver = 100L, doubloons = 5L)

  /** Главы 1–8 пройдены (путь `path`), «Печь» висит; «Большое дело» — по
    * `bigDeal`, «Синяя дверь» закрыта. */
  private def ready(path: Path, bigDeal: Boolean = true): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    val face = StoryPowderQuest.finish(base, WardenFate.Killed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = SojkaFate.Broke.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
    val act2 = StoryToolQuest.finish(face.take(NpcQuest.StoryTool, 0L), path)
      .take(NpcQuest.StoryMorning, 0L).finish(NpcQuest.StoryMorning)
      .take(NpcQuest.StoryNewcomer, 0L).finish(NpcQuest.StoryNewcomer)
      .update(NpcQuest.StoryNewcomer)(_.copy(counter = Fate.Hedge.n))
      .take(NpcQuest.StoryClearing, 0L).finish(NpcQuest.StoryClearing).take(NpcQuest.StoryFurnace, 0L)
      .take(NpcQuest.BartiDoor, 0L).finish(NpcQuest.BartiDoor)
    if (bigDeal) act2.take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods).take(NpcQuest.BigDeal, 0L).finish(NpcQuest.BigDeal)
    else act2
  }

  /** «Соль» взята: ночью — к каравану. */
  private def toCaravan: NpcQuests = ready(Path.Alone).take(NpcQuest.BartiSalt, 0L)

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

  private val waybill: Item = QuestItemKind.item(QuestItemKind.StoryWaybill).copy(id = 984L)

  private def caravan(d: TestHeroDao, c: SceneContent) =
    BartiCaravanState(d, TestInventoryRepository.withItems(Nil), c)

  override def spec = suite("«Соль»: соляной караван в Загон")(

    test("карточка накладной у одиночки: спросить Барти, потом к каравану, потом — глава 9 с караваном; у Щита — своя глава 9") {
      def card(q: NpcQuests, button: String) =
        for {
          c     <- content
          d     <- dao(q)
          _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
          r     <- TestRenderer.make
          st     = InventoryState(d, TestInventoryRepository.withItems(List(waybill)), TestItemRepository.make, c)
          _     <- st.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${waybill.id}"}""")), r)
          shown <- r.sentScreens.map(_.last)
          go    <- st.action(testUser, tap(button), r)
        } yield (shown, go)
      for {
        ask    <- card(ready(Path.Alone), "BartiSaltAsk")
        ride   <- card(toCaravan, "BartiSaltGo")
        done   <- card(toCaravan.finish(NpcQuest.BartiSalt), "BartiSaltGo")
        shield <- card(ready(Path.Shield), "BartiSaltAsk")
      } yield assertTrue(ask._1.text.contains("Барти знает всех, кто что куда возит") &&
                ask._1.choices.exists(_.id == "BartiSaltAsk") && ask._2 == StateType.BartiHouse) &&
              assertTrue(ride._1.text.contains("Та'Нахир ждёт ночью у Сухого колодца") &&
                ride._1.choices.exists(_.id == "BartiSaltGo") && ride._2 == StateType.BartiCaravan) &&
              assertTrue(done._1.text.contains("Ра'Тхар нашёл меня в Кинэте") && !done._1.choices.exists(_.id == "BartiSaltGo") &&
                done._1.choices.exists(_.id == "StoryFurnaceGo") && done._2 != StateType.BartiCaravan) &&
              assertTrue(shield._1.text.contains("Юнона просила зайти") && !shield._1.choices.exists(_.id.startsWith("BartiSalt")) &&
                shield._2 != StateType.BartiHouse)
    },

    test("«Дом Барти» на площади: до «Большого дела» — у одиночки, пока «Соль» ждёт разговора; после — у всех, а «📜 Соль» — на экране дома у одиночки") {
      def visit(q: NpcQuests) =
        for {
          c      <- content
          d      <- dao(q)
          r      <- TestRenderer.make
          _      <- MarketSquareState(c, Some(d)).enter(testUser, r)
          square <- r.sentScreens.map(_.last)
          r2     <- TestRenderer.make
          _      <- BartiHouseState(d, TestInventoryRepository.withItems(Nil), c, Some(TestItemRepository.make)).enter(testUser, r2)
          home   <- r2.sentScreens.map(_.last)
        } yield (square.choices.map(_.id), home.choices.map(_.id))
      for {
        alone    <- visit(ready(Path.Alone, bigDeal = false))
        shield   <- visit(ready(Path.Shield, bigDeal = false))
        taken    <- visit(ready(Path.Alone, bigDeal = false).take(NpcQuest.BartiSalt, 0L))
        settled  <- visit(ready(Path.Alone))
        shieldOk <- visit(ready(Path.Shield))
        going    <- visit(toCaravan)
      } yield assertTrue(alone._1.contains("BartiHome") && !shield._1.contains("BartiHome") && !taken._1.contains("BartiHome")) &&
              assertTrue(settled._1.contains("BartiHome") && shieldOk._1.contains("BartiHome") && going._1.contains("BartiHome")) &&
              assertTrue(settled._2.contains("BartiQuestSalt") && going._2.contains("BartiQuestSalt") &&
                !shieldOk._2.contains("BartiQuestSalt"))
    },

    test("у Барти: Загон — старая солеварня, соль туда возит Та'Нахир; задание взято; «Синяя дверь» подождёт; Та'Нахир помнит героя по «Большому делу»") {
      // После «Большого дела» — с экрана дома, кнопкой «📜 Соль»; до него — сразу.
      def talk(q: NpcQuests) =
        for {
          c <- content
          d <- dao(q)
          r <- TestRenderer.make
          st = BartiHouseState(d, TestInventoryRepository.withItems(Nil), c, Some(TestItemRepository.make))
          _ <- st.enter(testUser, r)
          _ <- ZIO.when(q.isDone(NpcQuest.BigDeal))(st.action(testUser, tap("BartiQuestSalt"), r))
          s <- r.sentScreens.map(_.last)
          q <- questOf(d)
        } yield (s, q)
      for {
        // «Синяя дверь» ещё не начата: Барти говорит о Загоне, а не о метке на двери.
        met      <- talk(ready(Path.Alone).update(NpcQuest.BartiDoor)(_.copy(step = 0, done = false)))
        stranger <- talk(ready(Path.Alone, bigDeal = false))
      } yield assertTrue(met._1.text.contains("Это солеварня, старая") && met._1.text.contains("А соль туда возит Та'Нахир") &&
                met._1.text.contains("📜 Новое задание: «Соль»") && met._1.text.contains("у колеса было хорошо сделано") &&
                met._1.text.contains(Difficulty.render(BartiSaltQuest.Level))) &&
              assertTrue(BartiSaltQuest.toCaravan(met._2) && !met._2.isTaken(NpcQuest.BartiDoor)) &&
              assertTrue(stranger._1.text.contains("А соль туда возит Та'Нахир") && !stranger._1.text.contains("у колеса") &&
                BartiSaltQuest.toCaravan(stranger._2))
    },

    test("у колодца: соляной караван; «Ехать с караваном» — дозор Загона, Сажа и четверо; Ра'Тхар и Ма'Кесс в строю; без бегства") {
      for {
        c     <- content
        d     <- dao(toCaravan)
        r     <- TestRenderer.make
        st     = caravan(d, c)
        _     <- st.enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
        res   <- st.action(testUser, tap("SaltRide"), r)
        said  <- texts(r)
        b     <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        allies = b.toList.flatMap(_.group.allies)
      } yield assertTrue(shown.text.contains("Соль везут в мешках") && shown.text.contains(Difficulty.render(BartiSaltQuest.Level)) &&
                shown.choices.map(_.id) == List("SaltRide", "SaltLeave")) &&
              assertTrue(res == StateType.Battle && said.contains("К печи чужих не возят")) &&
              assertTrue(b.exists(x => x.story.contains(BartiSaltQuest.FightStory) && x.cantFlee && x.monsterName.contains("Сажа") &&
                x.monsterRace == Race.Human.entryName && x.monsterRarity == Rarity.Mythical.entryName &&
                x.monsterLvl == BartiSaltQuest.Level.toLong && x.group.others.size == 4 &&
                x.group.others.forall(m => m.race == Race.Human.entryName && m.rarity == Rarity.Rare.entryName))) &&
              assertTrue(allies.map(a => (a.kind, a.home, a.lvl, a.undead.map(_.name))).sortBy(_._4) ==
                List((AllyKind.CaravanKhajiit, 0, BartiSaltQuest.Level.toLong, Some("Ма'Кесс")),
                  (AllyKind.CaravanKhajiit, 0, BartiSaltQuest.Level.toLong, Some("Ра'Тхар")))) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.BartiCaravan)) && BattleState.RaidStories.contains(BartiSaltQuest.FightStory))
    },

    test("отбились: Та'Нахир платит 50 дублонов и повезёт к печи, когда шум уляжется; задание закрыто") {
      for {
        c    <- content
        d    <- dao(toCaravan)
        _    <- d.writeSceneData(userId, StoryFight.mark(BartiCaravanState.SaltWonKey))
        r    <- TestRenderer.make
        _    <- caravan(d, c).enter(testUser, r)
        said <- texts(r)
        q    <- questOf(d)
        h    <- heroOf(d)
      } yield assertTrue(said.contains("Фонарь догорает в соли") && said.contains("Ра'Тхар найдёт тебя в Кинэте") &&
                said.contains(s"🟡 +${BartiSaltQuest.Doubloons} — Та'Нахир платит за охрану соли") &&
                said.contains("📜 Квест «Соль» завершён.")) &&
              assertTrue(q.isDone(NpcQuest.BartiSalt) && h.doubloons == 5L + BartiSaltQuest.Doubloons && h.exp > 0L)
    },

    test("смерть на соляной колее — проснуться в городе, шаг тот же; «Уйти» от колодца — караван подождёт; журнал") {
      for {
        c     <- content
        d     <- dao(toCaravan)
        fight  = SoloPveBattle.fromGroup(BartiSaltQuest.foes("Сажа"), hero, List.fill(5)(0L))
                   .copy(story = Some(BartiSaltQuest.FightStory))
        _     <- d.writeActiveBattle(userId, fight.asJson)
        r     <- TestRenderer.make
        _     <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        wake  <- d.readSceneData(userId)
        q     <- questOf(d)
        r2    <- TestRenderer.make
        left  <- caravan(d, c).action(testUser, tap("SaltLeave"), r2)
        bye   <- texts(r2)
        line   = QuestJournal.render(c, hero, LoreData.empty, toCaravan, DailyQuests.empty, None, 0L)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("barti.salt.death"))) &&
                wake.exists(_.hcursor.get[StateType]("wakeTo").toOption.contains(StateType.GlobalMap)) &&
                BartiSaltQuest.toCaravan(q)) &&
              assertTrue(left == StateType.GlobalMap && bye.contains("Караван подождёт ещё ночь")) &&
              assertTrue(line.contains("«Соль» — ночью к соляному каравану Та'Нахира у Сухого колодца"))
    }
  )
}
