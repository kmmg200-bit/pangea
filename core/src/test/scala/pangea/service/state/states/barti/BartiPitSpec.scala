package pangea.service.state.states.barti

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.BartiPitQuest.Way
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.home.HomeState
import pangea.service.state.states.story.StoryFight
import pangea.service.state.states.{DeathState, MarketSquareState}
import pangea.service.state.{BartiPitQuest, NpcQuestLog, QuestJournal, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Долговая яма» — побочное задание Барти в Акте II, все пути: Барти прогорел,
  * дом опечатан Торговым домом, сам он в долговой яме совета. Тени помогает
  * Братство — или заплатить самому; Щит идёт к Юноне — заплатить самому или
  * пусть она спишет долг; одиночка вытаскивает ночью, через надзирателя и
  * стражу. Выбор помним — и в подсчёте «как и вы». Напоследок Барти отдаёт
  * пояс надзирателя ★. */
object BartiPitSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(silver: Long = 100L): Hero =
    TestFixtures.hero(userId, state = StateType.MarketSquare).copy(lvl = 44L, silver = silver)

  /** Акт I пройден (путь `path`), «Большое дело», «Крыша» и «Большая поставка» закрыты. */
  private def ready(path: Path): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    val face = StoryPowderQuest.finish(base, WardenFate.Killed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = SojkaFate.Broke.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
    StoryToolQuest.finish(face.take(NpcQuest.StoryTool, 0L), path)
      .take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods).take(NpcQuest.BigDeal, 0L).finish(NpcQuest.BigDeal)
      .take(NpcQuest.BartiRoof, 0L).finish(NpcQuest.BartiRoof).take(NpcQuest.BartiDelivery, 0L).finish(NpcQuest.BartiDelivery)
  }

  /** «Яма» взята — на шаге своего пути. */
  private def taken(path: Path): NpcQuests = BartiPitQuest.take(ready(path), 0L)

  private def dao(q: NpcQuests, h: Hero = hero()) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- d.writeLoreData(userId, LoreData(firstSteps = true, firstStepsDone = true).asJson)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def house(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent, o: Option[TestQuestOutcomeDao] = None) =
    BartiHouseState(d, inv, c, Some(TestItemRepository.make), None, None, o)

  /** Строка «как и вы» у первого, кто закончил задание. */
  private val firstStats = "Вы — среди первых, кто закончил «Долговая яма»"

  /** Пояс надзирателя ★ в сумке: пояс 44-го уровня, оранжевый, сюжетный. */
  private def belted(bag: List[Item]): Boolean =
    bag.exists(i => i.name == "Пояс надзирателя" && i.isStoryGear && i.itemType == ItemType.Belt &&
      i.lvl == BartiPitQuest.Level.toLong && i.rarity == ItemRarity.Orange)

  override def spec = suite("«Долговая яма»: Барти в долговой яме совета")(

    test("дом опечатан, пока «Яма» доступна или идёт; до неё и после — экран дома; «Дом Барти» на площади — всё время") {
      def visit(q: NpcQuests) =
        for {
          c      <- content
          d      <- dao(q)
          r      <- TestRenderer.make
          _      <- MarketSquareState(c, Some(d)).enter(testUser, r)
          square <- r.sentScreens.map(_.last)
          r2     <- TestRenderer.make
          _      <- house(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r2)
          door   <- r2.sentScreens.map(_.last)
        } yield (square.choices.map(_.id), door.text)
      for {
        open   <- visit(ready(Path.Shield))
        going  <- visit(taken(Path.Shield))
        closed <- visit(taken(Path.Shield).finish(NpcQuest.BartiPit))
        early  <- visit(ready(Path.Shield).update(NpcQuest.BartiDelivery)(_.copy(done = false)))
      } yield assertTrue(List(open, going, closed, early).forall(_._1.contains("BartiHome"))) &&
              assertTrue(open._2.contains("Имущество под залогом") && going._2.contains("Юнона") &&
                !open._2.contains("Дом Барти за синей дверью") && !going._2.contains("Дом Барти за синей дверью")) &&
              assertTrue(closed._2.contains("Дом Барти за синей дверью") && early._2.contains("Дом Барти за синей дверью"))
    },

    test("с 35-го уровня героя: раньше дом не опечатан — экран дома, задания нет; уже взятая «Яма» идёт на любом уровне") {
      def visit(q: NpcQuests, lvl: Long) =
        for {
          c      <- content
          d      <- dao(q, hero().copy(lvl = lvl))
          r      <- TestRenderer.make
          _      <- MarketSquareState(c, Some(d)).enter(testUser, r)
          square <- r.sentScreens.map(_.last)
          r2     <- TestRenderer.make
          _      <- house(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r2)
          door   <- r2.sentScreens.map(_.last)
          after  <- questOf(d)
        } yield (square.choices.map(_.id), door, after)
      for {
        low   <- visit(ready(Path.Shield), BartiPitQuest.MinHeroLvl - 1L)
        exact <- visit(ready(Path.Shield), BartiPitQuest.MinHeroLvl)
        going <- visit(taken(Path.Alone), 20L)
      } yield assertTrue(low._1.contains("BartiHome") && low._2.text.contains("Дом Барти за синей дверью") &&
                !low._2.text.contains("Имущество под залогом") && !low._3.isTaken(NpcQuest.BartiPit)) &&
              assertTrue(exact._1.contains("BartiHome") && exact._2.text.contains("Имущество под залогом") &&
                exact._3.isTaken(NpcQuest.BartiPit)) &&
              assertTrue(going._1.contains("BartiHome") && BartiPitQuest.toNight(going._3) &&
                going._2.choices.map(_.id) == List("PitFight", "BartiLeave"))
    },

    test("впервые: дверь опечатана, записка Барти — задание взято на шаге своего пути: Тени — Братство, Щиту — к Юноне, одиночке — ночью") {
      def first(path: Path) =
        for {
          c <- content
          d <- dao(ready(path))
          r <- TestRenderer.make
          _ <- house(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
          s <- r.sentScreens.map(_.last)
          q <- questOf(d)
        } yield (s, q)
      for {
        shadow <- first(Path.Shadow)
        shield <- first(Path.Shield)
        alone  <- first(Path.Alone)
      } yield assertTrue(List(shadow, shield, alone).forall { case (s, _) =>
                s.text.contains("Имущество под залогом") && s.text.contains(s"долга: ${BartiPitQuest.debt} серебра") &&
                  s.text.contains("Не всё. Часть") && s.text.contains("📜 Новое задание: «Долговая яма»")
              }) &&
              assertTrue(BartiPitQuest.toOffer(shadow._2) && shadow._1.text.contains("Болтуны нам нужны") &&
                shadow._1.choices.map(_.id) == List("PitBrotherhood", "PitPay", "BartiLeave")) &&
              assertTrue(BartiPitQuest.toJunona(shield._2) && shield._1.text.contains("бумаги совета — Юнона") &&
                shield._1.choices.map(_.id) == List("BartiLeave")) &&
              assertTrue(BartiPitQuest.toNight(alone._2) && alone._1.text.contains("ночью её стерегут пятеро") &&
                alone._1.text.contains(Difficulty.render(BartiPitQuest.Level)) &&
                alone._1.choices.map(_.id) == List("PitFight", "BartiLeave"))
    },

    test("Тень: пусть платит Братство — Барти его человек; или заплатить самому: не хватает — сказано, хватает — долг снят; пояс ★") {
      def act(silver: Long, action: String) =
        for {
          c    <- content
          d    <- dao(taken(Path.Shadow), hero(silver))
          inv   = TestInventoryRepository.withItems(Nil)
          o    <- TestQuestOutcomeDao.make
          r    <- TestRenderer.make
          _    <- house(d, inv, c, Some(o)).action(testUser, tap(action), r)
          said <- texts(r)
          q    <- questOf(d)
          h    <- heroOf(d)
          mine <- o.of(h.id, NpcQuest.BartiPit)
        } yield (said, q, h, inv.snapshot, mine)
      val rich = BartiPitQuest.debt + 5000L
      for {
        bro  <- act(100L, "PitBrotherhood")
        poor <- act(100L, "PitPay")
        paid <- act(rich, "PitPay")
      } yield assertTrue(bro._1.contains("Человек в сером заходит в яму один") && bro._1.contains("Случайно приобрёл") &&
                bro._1.contains("📜 Квест «Долговая яма» завершён.") && bro._1.contains(firstStats) &&
                BartiPitQuest.way(bro._2).contains(Way.Brotherhood) && bro._5.contains(Way.Brotherhood.outcome) &&
                bro._3.silver == 100L && belted(bro._4)) &&
              assertTrue(poor._1.contains(s"нужно ${BartiPitQuest.debt} 🪙") && BartiPitQuest.toOffer(poor._2) && !belted(poor._4) &&
                poor._5.isEmpty) &&
              assertTrue(paid._1.contains("уходят в кассу Торгового дома") && BartiPitQuest.way(paid._2).contains(Way.Self) &&
                paid._5.contains(Way.Self.outcome) && paid._3.silver == 5000L && paid._3.exp > 0L && belted(paid._4))
    },

    test("Щит: у Юноны — заплатить самому или пусть она спишет долг; выбор помним; пояс ★") {
      def junona(silver: Long, action: String) =
        for {
          c      <- content
          d      <- dao(taken(Path.Shield), hero(silver))
          inv     = TestInventoryRepository.withItems(Nil)
          o      <- TestQuestOutcomeDao.make
          sch    <- TestScheduler.make
          home    = HomeState(d, sch, c, inv, TestItemRepository.make, None, None, Some(o))
          r      <- TestRenderer.make
          _      <- home.action(testUser, tap("HomeOffice"), r)
          office <- r.sentScreens.map(_.last)
          _      <- home.action(testUser, tap("HomeJunonaPit"), r)
          ask    <- r.sentScreens.map(_.last)
          _      <- home.action(testUser, tap(action), r)
          said   <- texts(r)
          q      <- questOf(d)
          h      <- heroOf(d)
          mine   <- o.of(h.id, NpcQuest.BartiPit)
        } yield (office, ask, said, q, h, inv.snapshot, mine)
      for {
        write <- junona(100L, "HomeJunonaPitWrite")
        pay   <- junona(BartiPitQuest.debt + 1000L, "HomeJunonaPitPay")
      } yield assertTrue(write._1.choices.exists(_.id == "HomeJunonaPit") && write._2.text.contains("как дохлую мышь") &&
                write._2.choices.map(_.id) == List("HomeJunonaPitPay", "HomeJunonaPitWrite", "HomeOffice")) &&
              assertTrue(write._3.contains("пишет на полях три слова") && BartiPitQuest.way(write._4).contains(Way.Junona) &&
                write._7.contains(Way.Junona.outcome) && write._3.contains(firstStats) &&
                write._5.silver == 100L && belted(write._6)) &&
              assertTrue(pay._3.contains("уходят в кассу Торгового дома") && BartiPitQuest.way(pay._4).contains(Way.Self) &&
                pay._7.contains(Way.Self.outcome) && pay._5.silver == 1000L && belted(pay._6))
    },

    test("одиночка: ночью в яму — Засов и четверо, без бегства; отбились — Барти на свободе, печать сорвана, пояс ★") {
      for {
        c     <- content
        d     <- dao(taken(Path.Alone))
        inv    = TestInventoryRepository.withItems(Nil)
        o     <- TestQuestOutcomeDao.make
        r     <- TestRenderer.make
        res   <- house(d, inv, c, Some(o)).action(testUser, tap("PitFight"), r)
        start <- texts(r)
        b     <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        _     <- d.writeSceneData(userId, StoryFight.mark(BartiHouseState.PitWonKey))
        r2    <- TestRenderer.make
        _     <- house(d, inv, c, Some(o)).enter(testUser, r2)
        freed <- texts(r2)
        q     <- questOf(d)
        h     <- heroOf(d)
        mine  <- o.of(h.id, NpcQuest.BartiPit)
      } yield assertTrue(res == StateType.Battle && start.contains("Ключей много, кружка одна")) &&
              assertTrue(b.exists(x => x.story.contains(BartiPitQuest.FightStory) && x.cantFlee && x.monsterName.contains("Засов") &&
                x.monsterRace == Race.Human.entryName && x.monsterRarity == Rarity.Mythical.entryName &&
                x.monsterLvl == BartiPitQuest.Level.toLong && x.group.others.size == 4 &&
                x.group.others.forall(_.rarity == Rarity.Rare.entryName) && x.group.allies.isEmpty)) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.BartiHouse)) && BattleState.RaidStories.contains(BartiPitQuest.FightStory)) &&
              assertTrue(freed.contains("Этот мне должен") && freed.contains("Печать с синей двери Барти срывает сам") &&
                freed.contains("📜 Квест «Долговая яма» завершён.") && freed.contains(firstStats) &&
                BartiPitQuest.way(q).contains(Way.Breakout) && mine.contains(Way.Breakout.outcome) && belted(inv.snapshot))
    },

    test("смерть в яме — проснуться на Торговой площади, шаг тот же; журнал — шаг своего пути") {
      for {
        c    <- content
        d    <- dao(taken(Path.Alone))
        fight = SoloPveBattle.fromGroup(BartiPitQuest.foes("Засов"), hero(), List.fill(5)(0L)).copy(story = Some(BartiPitQuest.FightStory))
        _    <- d.writeActiveBattle(userId, fight.asJson)
        r    <- TestRenderer.make
        _    <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        wake <- d.readSceneData(userId)
        q    <- questOf(d)
        line  = (p: Path) => QuestJournal.render(c, hero(), LoreData.empty, taken(p), DailyQuests.empty, None, 0L)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("barti.pit.death"))) &&
                wake.exists(_.hcursor.get[StateType]("wakeTo").toOption.contains(StateType.MarketSquare)) && BartiPitQuest.toNight(q)) &&
              assertTrue(line(Path.Shadow).contains("«Долговая яма» — решить, кто вытащит Барти") &&
                line(Path.Shield).contains("спросить Юнону Кинтслев") &&
                line(Path.Alone).contains("ночью вытащить Барти из долговой ямы за ратушей"))
    }
  )
}
