package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.{SceneContent, Screen}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.parcel.Parcels
import pangea.service.state.StoryCollectorQuest.Deal
import pangea.service.state.StoryFurnaceQuest.Fork
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.bank.TradeHouseState
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.home.HomeState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryCollectorQuest, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Акт II, глава 10 «Сборщик»: Сборщик приходит в Кинэт за героем. Тень — Э.
  * приходит ночью и встаёт в строй против людей Сборщика; Щит — Юнона
  * выводит через архив (приказ подписал Рубус), бой в гильдейском зале с
  * Горном и искателями; одиночка — проверка Сборщика у Барти, Барти в строю.
  * Дальше у всех — дегтярный склад за 250 000 у Торгового дома, Сойка по
  * главе 4 и глава Братства: место, покой или ни то, ни другое — выбор в «как
  * и вы». В сумке остаётся купчая, Рубеж II висит до обновления. */
object StoryCollectorSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, state = StateType.GlobalMap).copy(lvl = 46L, silver = 100L)

  /** Главы 1–9 пройдены (путь `path`, Сойка в главе 4 — `sojka`), «Сборщик» на
    * шаге `n`. */
  private def collector(path: Path, n: Int, sojka: SojkaFate = SojkaFate.Broke): NpcQuests = {
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
      .take(NpcQuest.StoryCollector, 0L)
    StoryCollectorQuest.step(done, n)
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

  private val ledger: Item = QuestItemKind.item(QuestItemKind.StoryLedger).copy(id = 986L)
  private val deed: Item   = QuestItemKind.item(QuestItemKind.StoryDeed).copy(id = 987L)

  private def scene(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent, o: Option[TestQuestOutcomeDao] = None) =
    StoryCollectorState(d, inv, c, o)

  private def tradeHouse(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    for {
      userRepo <- TestUserRepository.withUser(testUser)
      bankRepo  = TestBankRepository.of(0)
      parcels   = Parcels(TestParcelDao.empty, bankRepo, c)
    } yield TradeHouseState(d, userRepo, bankRepo, parcels, inv, TestDonations.off, c, None, Some(TestItemRepository.make))

  /** Карточка вещи из сумки — в городе — и нажатие кнопки `go` на ней. */
  private def card(q: NpcQuests, item: Item, go: String) =
    for {
      c     <- content
      d     <- dao(q)
      _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
      r     <- TestRenderer.make
      st     = InventoryState(d, TestInventoryRepository.withItems(List(ledger, deed).filter(_.id == item.id)),
                 TestItemRepository.make, c)
      _     <- st.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${item.id}"}""")), r)
      shown <- r.sentScreens.map(_.last)
      to    <- st.action(testUser, tap(go), r)
    } yield (shown, to)

  private def label(s: Screen, id: String) = s.choices.find(_.id == id).map(_.label)

  override def spec = suite("Акт II, глава 10 «Сборщик»")(

    test("карточка книги печи: Тени — ждать Э., Щиту — к Юноне, потом в гильдейский зал, одиночке — открыть Барти; на шаге склада — без кнопки") {
      for {
        shadow  <- card(collector(Path.Shadow, 1), ledger, "StoryCollectorGo")
        shadow2 <- card(collector(Path.Shadow, 2), ledger, "StoryCollectorGo")
        shield1 <- card(collector(Path.Shield, 1), ledger, "StoryCollectorGo")
        shield2 <- card(collector(Path.Shield, 2), ledger, "StoryCollectorGo")
        shield3 <- card(collector(Path.Shield, 3), ledger, "StoryCollectorGo")
        alone   <- card(collector(Path.Alone, 1), ledger, "StoryCollectorGo")
        alone2  <- card(collector(Path.Alone, 2), ledger, "StoryCollectorGo")
      } yield assertTrue(shadow._1.text.contains("«Ночью не спи. Приду сам»") &&
                shadow._1.text.contains(Difficulty.render(StoryCollectorQuest.Level)) &&
                label(shadow._1, "StoryCollectorGo").contains("🌙 Ждать Э.") && shadow._2 == StateType.StoryCollector) &&
              assertTrue(shadow2._1.text.contains("купить его раньше Братства") && label(shadow2._1, "StoryCollectorGo").isEmpty &&
                shadow2._2 != StateType.StoryCollector) &&
              assertTrue(shield1._1.text.contains("Юнона прислала мальчишку") && label(shield1._1, "StoryCollectorGo").isEmpty &&
                shield1._2 != StateType.StoryCollector) &&
              assertTrue(shield2._1.text.contains("Пропуск Юноны при мне") &&
                label(shield2._1, "StoryCollectorGo").contains("🔨 В гильдейский зал") && shield2._2 == StateType.StoryCollector) &&
              assertTrue(shield3._1.text.contains("Совет засаду не оплатит — значит, плачу я") &&
                label(shield3._1, "StoryCollectorGo").isEmpty) &&
              assertTrue(alone._1.text.contains("Барти стучит ночью") && label(alone._1, "StoryCollectorGo").contains("🏠 Открыть Барти") &&
                alone._2 == StateType.StoryCollector) &&
              assertTrue(alone2._1.text.contains("сбил цену «с трёхсот»") && label(alone2._1, "StoryCollectorGo").isEmpty)
    },

    test("Щит: Юнона в архиве — приказ подписал советник Рубус, у него второй ключ от её ящика; пропуск — и к Горну") {
      for {
        c      <- content
        d      <- dao(collector(Path.Shield, 1))
        sch    <- TestScheduler.make
        home    = HomeState(d, sch, c, TestInventoryRepository.withItems(List(ledger)), TestItemRepository.make)
        r      <- TestRenderer.make
        _      <- home.action(testUser, tap("HomeOffice"), r)
        office <- r.sentScreens.map(_.last)
        _      <- home.action(testUser, tap("HomeJunonaRubus"), r)
        said   <- texts(r)
        q      <- questOf(d)
        other  <- dao(collector(Path.Shadow, 1))
        home2   = HomeState(other, sch, c, TestInventoryRepository.withItems(List(ledger)), TestItemRepository.make)
        r2     <- TestRenderer.make
        _      <- home2.action(testUser, tap("HomeOffice"), r2)
        office2 <- r2.sentScreens.map(_.last)
      } yield assertTrue(label(office, "HomeJunonaRubus").contains("📌 Что решил совет")) &&
              assertTrue(said.contains("Приказ подписал советник Рубус") && said.contains("Второй ключ от моего ящика у него") &&
                said.contains("Пропуск под моей печатью. Настоящей")) &&
              assertTrue(StoryCollectorQuest.toFight(q) && !StoryCollectorQuest.toJunona(q)) &&
              assertTrue(!office2.choices.exists(_.id == "HomeJunonaRubus"))
    },

    test("бой своего пути: Тень — Безмен и четверо, Э. в строю; Щит — пристав Крюк, Горн, Сван и Хильда; одиночка — Щуп, Барти; без бегства") {
      def fight(q: NpcQuests) =
        for {
          c     <- content
          d     <- dao(q)
          r     <- TestRenderer.make
          st     = scene(d, TestInventoryRepository.withItems(List(ledger)), c)
          _     <- st.enter(testUser, r)
          shown <- r.sentScreens.map(_.last)
          res   <- st.action(testUser, tap("CollectorFight"), r)
          start <- r.sentScreens.map(_.last.text)
          b     <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
          loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        } yield (shown, res, start, b, loot)
      def allies(b: Option[SoloPveBattle]) =
        b.toList.flatMap(_.group.allies).map(a => (a.kind, a.home, a.lvl, a.undead.map(_.name))).sortBy(_._4)
      val lvl = StoryCollectorQuest.Level.toLong
      for {
        shadow <- fight(collector(Path.Shadow, 1))
        shield <- fight(collector(Path.Shield, 2))
        alone  <- fight(collector(Path.Alone, 1))
      } yield assertTrue(shadow._1.text.contains("Второй раз так не будет") && shadow._1.text.contains("подписал его сам Сборщик") &&
                shield._1.text.contains("Пристав Крюк читает приказ вслух") && shield._1.text.contains("«Рубус»") &&
                alone._1.text.contains("Ну не отказываться же от задатка?")) &&
              assertTrue(List(shadow, shield, alone).forall { case (shown, res, _, b, loot) =>
                shown.text.contains(Difficulty.render(StoryCollectorQuest.Level)) &&
                  shown.choices.map(_.id) == List("CollectorFight", "CollectorLeave") && res == StateType.Battle &&
                  b.exists(x => x.cantFlee && x.monsterRace == Race.Human.entryName && x.monsterRarity == Rarity.Mythical.entryName &&
                    x.monsterLvl == lvl && x.group.others.size == 4 && x.group.others.forall(_.rarity == Rarity.Rare.entryName)) &&
                  loot.exists(_.returnState.contains(StateType.StoryCollector))
              }) &&
              assertTrue(shadow._4.exists(x => x.story.contains(StoryCollectorQuest.NightStory) && x.monsterName.contains("Безмен")) &&
                shadow._3.contains("Э. встаёт рядом со мной")) &&
              assertTrue(allies(shadow._4) == List((AllyKind.BrotherhoodAgent, 0, lvl, Some("Э.")))) &&
              assertTrue(shield._4.exists(x => x.story.contains(StoryCollectorQuest.HallStory) && x.monsterName.contains("Пристав Крюк")) &&
                shield._3.contains("«Своих Гильдия не отдаёт»")) &&
              assertTrue(allies(shield._4) == List((AllyKind.GuildMaster, 0, lvl, Some("Горн")),
                (AllyKind.GuildSeeker, 0, lvl, Some("Сван")), (AllyKind.GuildSeeker, 0, lvl, Some("Хильда")))) &&
              assertTrue(alone._4.exists(x => x.story.contains(StoryCollectorQuest.CheckStory) && x.monsterName.contains("Щуп")) &&
                alone._3.contains("Барти берёт сковороду")) &&
              assertTrue(allies(alone._4) == List((AllyKind.Informant, 0, lvl, Some("Барти")))) &&
              assertTrue(StoryCollectorQuest.FightStories.forall(BattleState.RaidStories.contains))
    },

    test("после боя — к Рахадиму: склад за 250 000; не хватает — так и сказано, хватает — купчая в сумке, глава на шаге склада") {
      def run(path: Path, n: Int, silver: Long) =
        for {
          c      <- content
          d      <- dao(collector(path, n))
          _      <- d.updateSilver(userId, silver)
          _      <- d.writeSceneData(userId, StoryFight.mark(StoryCollectorState.WonKey))
          inv     = TestInventoryRepository.withItems(List(ledger))
          r      <- TestRenderer.make
          _      <- scene(d, inv, c).enter(testUser, r)
          won    <- r.sentScreens.map(_.last.text)
          q1     <- questOf(d)
          th     <- tradeHouse(d, inv, c)
          r2     <- TestRenderer.make
          _      <- th.enter(testUser, r2)
          menu   <- r2.sentScreens.map(_.last)
          _      <- th.action(testUser, tap("StoryWarehouseBuy"), r2)
          said   <- texts(r2)
          q2     <- questOf(d)
          h      <- heroOf(d)
        } yield (won, q1, menu, said, q2, h, inv.snapshot)
      for {
        shadow <- run(Path.Shadow, 1, 300000L)
        shield <- run(Path.Shield, 2, 250000L)
        alone  <- run(Path.Alone, 1, 100L)
      } yield assertTrue(shadow._1.contains("Безмен лежит на пороге") && shield._1.contains("Пристава Крюка выносят на улицу") &&
                alone._1.contains("Я сбил до двухсот пятидесяти")) &&
              assertTrue(List(shadow, shield, alone).forall { case (_, q1, menu, _, _, _, _) =>
                StoryCollectorQuest.toBuy(q1) && label(menu, "StoryWarehouseBuy").contains("🏚 Купить дегтярный склад (250000 🪙)")
              }) &&
              assertTrue(shadow._4.contains("Со всем, что в нём пахнет") && shadow._4.contains("Опоздало на день") &&
                shield._4.contains("Юнона прислала их утром")) &&
              assertTrue(List((shadow, 50000L), (shield, 0L)).forall { case ((_, _, _, _, q2, h, bag), left) =>
                StoryCollectorQuest.toWarehouse(q2) && h.silver == left &&
                  bag.exists(_.questItem.contains(QuestItemKind.StoryDeed)) && bag.exists(_.questItem.contains(QuestItemKind.StoryLedger))
              }) &&
              assertTrue(alone._4.contains("Склад стоит 250000 🪙") && StoryCollectorQuest.toBuy(alone._5) && alone._6.silver == 100L &&
                !alone._7.exists(_.questItem.contains(QuestItemKind.StoryDeed)))
    },

    test("склад: Сойка по главе 4, старик с палкой и его договор — по пути; карточка купчей ведёт сюда") {
      def at(q: NpcQuests) =
        for {
          c <- content
          d <- dao(q)
          r <- TestRenderer.make
          _ <- scene(d, TestInventoryRepository.withItems(List(ledger, deed)), c).enter(testUser, r)
          s <- r.sentScreens.map(_.last)
        } yield s
      for {
        shadow   <- at(collector(Path.Shadow, 3, sojka = SojkaFate.Forgave))
        shield   <- at(collector(Path.Shield, 4))
        alone    <- at(collector(Path.Alone, 3))
        deedCard <- card(collector(Path.Shield, 4), deed, "StoryWarehouseGo")
      } yield assertTrue(List(shadow, shield, alone).forall(s =>
                s.text.contains("Дегтярный склад пахнет так же") && s.text.contains("на ящике у двери уже сидит старик") &&
                  s.choices.map(_.id) == List("CollectorPlace", "CollectorPeace", "CollectorRefuse", "CollectorLeave"))) &&
              assertTrue(shadow.text.contains("быть должной я не люблю") && shadow.text.contains("выше, чем у Э.")) &&
              assertTrue(shield.text.contains("Я тебя продала") && shield.text.contains("повыше, чем у Рубуса в совете")) &&
              assertTrue(alone.text.contains("Вы ведь всё делаете сами") && !alone.text.contains("Рубус")) &&
              assertTrue(deedCard._1.text.contains("Совет о нём не знает, Юнона знает") &&
                label(deedCard._1, "StoryWarehouseGo").contains("🏚 На склад") && deedCard._2 == StateType.StoryCollector)
    },

    test("договор закрывает главу: книга печи уходит, купчая остаётся, выбор — в главу и в «как и вы», Рубеж II висит") {
      def decide(path: Path, n: Int, deal: Deal) =
        for {
          c     <- content
          d     <- dao(collector(path, n))
          inv    = TestInventoryRepository.withItems(List(ledger, deed))
          o     <- TestQuestOutcomeDao.make
          r     <- TestRenderer.make
          _     <- scene(d, inv, c, Some(o)).action(testUser, tap(s"Collector${deal.key.capitalize}"), r)
          said  <- texts(r)
          after <- questOf(d)
          h     <- heroOf(d)
          mine  <- o.of(h.id, NpcQuest.StoryCollector)
          soon   = InventoryState.questText(c, deed, after, 46L)
        } yield (said, after, h, inv.snapshot, mine, soon)
      val cases = List(
        (Path.Shadow, 3, Deal.Place), (Path.Shield, 4, Deal.Place), (Path.Alone, 3, Deal.Place),
        (Path.Shield, 4, Deal.Peace), (Path.Alone, 3, Deal.Refuse))
      for {
        runs <- ZIO.foreach(cases) { case (p, n, deal) => decide(p, n, deal).map(deal -> _) }
        told  = runs.map(_._2._1)
      } yield assertTrue(runs.forall { case (deal, (said, q, h, bag, mine, soon)) =>
                said.contains("📜 Квест «Сборщик» завершён.") && said.contains("📜 Новое задание: «Чем кормить Клеймо»") &&
                  q.isDone(NpcQuest.StoryCollector) && StoryCollectorQuest.deal(q).contains(deal) && q.isTaken(NpcQuest.StoryFeed) &&
                  mine.contains(deal.outcome) && h.silver == 100L + StoryCollectorQuest.silver && h.exp > 0L &&
                  !bag.exists(_.questItem.contains(QuestItemKind.StoryLedger)) && bag.exists(_.questItem.contains(QuestItemKind.StoryDeed)) &&
                  soon.contains("Рубеж II откроется с обновлением 🔜")
              }) &&
              assertTrue(told.headOption.exists(s => s.contains("«Разумно, — говорит старик") && s.contains("«Про Э. я тоже не забуду»") &&
                s.contains("Э. оставил под дверью"))) &&
              assertTrue(told.lift(1).exists(s => s.contains("«Про советника Рубуса — тоже»") && s.contains("Гильдия скинулась"))) &&
              assertTrue(told.lift(2).exists(s => s.contains("«Разумно") && !s.contains("Про Э.") && !s.contains("Про советника") &&
                s.contains("Барти отдал часть задатка"))) &&
              assertTrue(told.lift(3).exists(_.contains("Забывать мы умеем"))) &&
              assertTrue(told.lift(4).exists(_.contains("Ящика, на котором он сидел, на складе раньше не было")))
    },

    test("смерть в бою главы — проснуться, шаг тот же; журнал — по пути; после главы — «Чем кормить Клеймо» висит") {
      def die(q: NpcQuests, story: String) =
        for {
          c    <- content
          d    <- dao(q)
          fight = SoloPveBattle.fromGroup(StoryCollectorQuest.foes("Безмен"), hero, List.fill(5)(0L)).copy(story = Some(story))
          _    <- d.writeActiveBattle(userId, fight.asJson)
          r    <- TestRenderer.make
          _    <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
          wake <- d.readSceneData(userId)
          q2   <- questOf(d)
        } yield (wake.flatMap(_.hcursor.get[List[String]]("wakeLines").toOption),
                 wake.flatMap(_.hcursor.get[StateType]("wakeTo").toOption), StoryCollectorQuest.toFight(q2))
      for {
        c      <- content
        night  <- die(collector(Path.Shadow, 1), StoryCollectorQuest.NightStory)
        hall   <- die(collector(Path.Shield, 2), StoryCollectorQuest.HallStory)
        check  <- die(collector(Path.Alone, 1), StoryCollectorQuest.CheckStory)
        line    = (q: NpcQuests) => QuestJournal.render(c, hero, LoreData.empty, q, DailyQuests.empty, None, 0L)
        closed  = collector(Path.Alone, 3).finish(NpcQuest.StoryCollector).take(NpcQuest.StoryFeed, 0L)
      } yield assertTrue(night == ((Some(List("story.storyCollector.night.deathShadow")), Some(StateType.GlobalMap), true))) &&
              assertTrue(hall == ((Some(List("story.storyCollector.night.deathShield")), Some(StateType.GlobalMap), true))) &&
              assertTrue(check == ((Some(List("story.storyCollector.night.deathAlone")), Some(StateType.MarketSquare), true))) &&
              assertTrue(line(collector(Path.Shadow, 1)).contains("дождаться ночью Э.") &&
                line(collector(Path.Shield, 1)).contains("узнать у Юноны Кинтслев, что решил совет") &&
                line(collector(Path.Shield, 4)).contains("встретить на складе тех, кто придёт") &&
                line(collector(Path.Alone, 2)).contains("купить дегтярный склад у Торгового дома — 250 000 🪙")) &&
              assertTrue(line(closed).contains("📌 «Чем кормить Клеймо»") && line(closed).contains("Рубеж II откроется с обновлением 🔜"))
    }
  )
}
