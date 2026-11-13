package pangea.service.state.states.village

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{Convoy, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{Difficulty, NpcQuests}
import pangea.model.schedule.TaskKind
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.{CaravanAmbush, CaravanTrip, VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.service.state.states.DeathState
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.{NpcQuestLog, UserAction, Villages}
import pangea.test._
import zio.test._
import zio.{Task, ZIO, durationInt}

/** Караван деревни — третья беда, с 25-го уровня: проводить обоз в соседний
  * город. Четыре перехода по 3–6 минут, у каждого свой текст; в конце
  * перехода — засада с шансом 50% (засад не было — на последнем наверняка):
  * банда, гоблины Чернолома или ищейки Сноходцев, за героя — двое
  * караванщиков. Дошли — к старосте за платой. Пал в засаде или бросил
  * караван в пути — контракт сорван. */
object VillageCaravanSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private val legDone: UserAction = UserAction("", Some(VillageCaravanState.LegAction))

  private val Level      = 27
  private val QuestLevel = 28
  private val Silver     = 2100L

  private def hero: Hero = TestFixtures.hero(userId, dungeonLevel = Level).copy(lvl = 30L, silver = 100L)

  // Деревня гномов, первое название в списке; город — третий в списке (Каэлис).
  private val scene       = VillageScene(Race.Gnome, 0, VillageQuest.CaravanEscort, Level, QuestLevel, Silver, city = 2)
  private val City        = "Каэлис"
  private def contract(step: Int = VillageContract.Taken): VillageContract = VillageContract.of(scene).copy(step = step)
  private def quests(step: Int = VillageContract.Taken): NpcQuests = NpcQuests.empty.copy(village = Some(contract(step)))
  private def paper: Item = QuestItemKind.item(QuestItemKind.VillageContract).copy(id = 901L)

  private val content = ZIO.attempt(SceneContent.load())

  private def dao(q: Option[NpcQuests] = Some(quests()), trip: Option[CaravanTrip] = None) =
    for {
      d <- TestHeroDao.withHero(userId, hero)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
      _ <- ZIO.foreachDiscard(trip)(t => d.writeSceneData(userId, t.asJson))
    } yield d

  /** Сцена каравана с записывающим планировщиком. */
  private def caravan(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent): Task[(VillageCaravanState, TestScheduler)] =
    TestScheduler.make.map(s => (VillageCaravanState(d, inv, s, c), s))

  private def tripOf(d: TestHeroDao): Task[Option[CaravanTrip]] =
    d.readSceneData(userId).map(_.flatMap(_.as[CaravanTrip].toOption))

  private def battleOf(d: TestHeroDao): Task[Option[SoloPveBattle]] =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)

  /** Переход, который уже кончился: часы ушли на десять минут вперёд, а конец
    * перехода — на первой из них. */
  private def ended(leg: Int, ambushes: Int): CaravanTrip = CaravanTrip(leg, ambushes, 60L * 1000L)

  override def spec = suite("Караван деревни")(

    test("пул: с 25-го уровня староста может попросить и о караване; контракт ведёт к каравану, а не к логову") {
      for {
        c <- content
      } yield assertTrue(VillageQuest.CaravanEscort.minLevel == 25 && VillageQuest.CaravanEscort.key == "caravan") &&
              assertTrue(VillageQuest.openAt(24) == List(VillageQuest.BanditLair, VillageQuest.SpiderNests) &&
                VillageQuest.openAt(25) == List(VillageQuest.BanditLair, VillageQuest.SpiderNests, VillageQuest.CaravanEscort)) &&
              assertTrue(Villages.goTo(quests()) == Some(StateType.VillageCaravan -> "caravan") &&
                Villages.goTo(quests(VillageContract.Done)).isEmpty && Villages.toCaravan(quests())) &&
              assertTrue(Villages.cardLine(c, quests()).contains("отдаст тот, кто примет его в городе")) &&
              assertTrue(Villages.cardLine(c, quests()).contains(s"до города $City") &&
                Villages.cardLine(c, quests()).contains(s"Плата — $Silver серебра")) &&
              assertTrue(c.choice("x", "village.card.caravanLabel").label == "🐎 Идти к каравану")
    },

    test("у ворот: обоз и караванщики, сложность, «из засады не сбежать»; кнопки — выступить, персонаж, уйти, отказаться") {
      for {
        c     <- content
        d     <- dao()
        r     <- TestRenderer.make
        st    <- caravan(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _     <- st.enter(testUser, r)
        gates <- r.sentScreens.map(_.last)
      } yield assertTrue(gates.text.contains("Обоз ждёт у ворот") && gates.text.contains(s"До города $City — четыре перехода") &&
                gates.text.contains("Сложность: " + Difficulty.render(QuestLevel)) && gates.text.contains("Из засады не сбежать")) &&
              assertTrue(gates.choices.map(_.id) ==
                List("VillageCaravanGo", "OpenCharacter", "VillageCaravanLeave", "VillageCaravanAbandon"))
    },

    test("«Выступить»: первый переход — его текст, конец через 3–6 минут — в сцене и в планировщике; в пути — персонаж, приложение, бросить") {
      for {
        c     <- content
        d     <- dao()
        r     <- TestRenderer.make
        pair  <- caravan(d, TestInventoryRepository.withItems(List(paper)), c)
        (st, sched) = pair
        res   <- st.action(testUser, tap("VillageCaravanGo"), r)
        road  <- r.sentScreens.map(_.last)
        trip  <- tripOf(d)
        tasks <- sched.scheduled
      } yield assertTrue(res == StateType.VillageCaravan && road.text.contains("Обоз выходит за ворота")) &&
              assertTrue(trip.exists(t => t.leg == 1 && t.ambushes == 0 &&
                t.nextAt >= VillageRates.CaravanLegMinMs && t.nextAt <= VillageRates.CaravanLegMaxMs)) &&
              assertTrue(tasks.map(t => (t.kind, t.expectedState, t.action, t.fireAt)) ==
                trip.toList.map(tr => ((TaskKind.VillageCaravan: TaskKind), (StateType.VillageCaravan: StateType),
                  VillageCaravanState.LegAction, tr.nextAt))) &&
              assertTrue(road.choices.map(_.id) == List("OpenCharacter", "OpenApp", "VillageCaravanAbandon"))
    },

    test("переход кончился тихо: строка тишины и следующий переход — его текст; раньше срока — ничего не происходит") {
      for {
        c     <- content
        d     <- dao(trip = Some(ended(1, 0)))
        r     <- TestRenderer.make
        st    <- caravan(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        early <- st.action(testUser, legDone, r) *> tripOf(d)
        _     <- TestClock.adjust(10.minutes)
        _     <- TestRandom.feedInts(99)
        _     <- st.action(testUser, legDone, r)
        road  <- r.sentScreens.map(_.last)
        trip  <- tripOf(d)
        b     <- battleOf(d)
      } yield assertTrue(early.contains(ended(1, 0))) &&
              assertTrue(road.text.contains("Переход проходит спокойно") && road.text.contains("Тракт ныряет в изгородь") &&
                road.text.contains("ещё три перехода")) &&
              assertTrue(trip.exists(t => t.leg == 2 && t.ambushes == 0 && t.nextAt > 10L * 60L * 1000L) && b.isEmpty)
    },

    test("засада: семеро уровня задания, двое караванщиков в строю героя, без бегства; добыча вернёт к следующему переходу") {
      for {
        c     <- content
        d     <- dao(trip = Some(ended(2, 0)))
        r     <- TestRenderer.make
        st    <- caravan(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _     <- TestClock.adjust(10.minutes)
        // засада (0 < 50) — ищейки Сноходцев (третьи в списке)
        _     <- TestRandom.feedInts(0, 2)
        res   <- st.action(testUser, legDone, r)
        shown <- r.sentScreens.map(_.last)
        b     <- battleOf(d)
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        foes   = b.toList.flatMap(x => x.monsterRace :: (x.group.others ++ x.group.queue).map(_.race))
        guards = b.toList.flatMap(_.group.allies.filter(_.kind == AllyKind.CaravanGuard))
        rare   = MonsterGenerator.generateOfRaceAndRarity(QuestLevel, Race.Gnome, Rarity.Rare).fightStats
      } yield assertTrue(res == StateType.Battle && shown.text.contains("ищейки Сноходцев")) &&
              assertTrue(b.exists(x => x.story.contains(Villages.CaravanStory) && x.lootLevel.contains(QuestLevel.toLong) && x.cantFlee)) &&
              assertTrue(foes.size == VillageRates.CaravanAmbushFront + VillageRates.CaravanAmbushBack &&
                foes.forall(_ == Race.Khajiit.entryName)) &&
              assertTrue(guards.map(_.name) == List("Старший караванщик", "Караванщик") &&
                guards.forall(g => g.stats == rare && g.lvl == QuestLevel.toLong && g.home == 0)) &&
              assertTrue(loot.exists(l => l.returnState.contains(StateType.VillageCaravan) &&
                l.eventData.flatMap(_.as[CaravanTrip].toOption).contains(CaravanTrip(3, 1, 0L))))
    },

    test("последний переход, а засад не было — засада наверняка; была — тихо, город — и плата сразу, с рассказом получателя") {
      for {
        c      <- content
        d1     <- dao(trip = Some(ended(4, 0)))
        r      <- TestRenderer.make
        st1    <- caravan(d1, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _      <- TestClock.adjust(10.minutes)
        _      <- TestRandom.feedInts(99, 0)
        sure   <- st1.action(testUser, legDone, r)
        b      <- battleOf(d1)
        d2     <- dao(trip = Some(ended(4, 1)))
        inv     = TestInventoryRepository.withItems(List(paper))
        pair   <- caravan(d2, inv, c)
        (st2, sched) = pair
        _      <- TestRandom.feedInts(99)
        calm   <- st2.action(testUser, legDone, r)
        city   <- r.sentScreens.map(_.last)
        q      <- questOf(d2)
        sd     <- d2.readSceneData(userId)
        off    <- sched.cancelled
        after  <- d2.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(sure == StateType.Battle && b.isDefined) &&
              assertTrue(calm == StateType.VillageCaravan && city.text.contains(s"Обоз въезжает в город $City") &&
                city.text.contains("купчиха") && city.text.contains(s"Получено $Silver серебра") && city.text.contains("Дошли все пять повозок") &&
                city.text.contains("Контракт деревни выполнен") && !city.text.contains("с карточки") &&
                city.choices.map(_.id) == List("VillageCaravanOut")) &&
              // платят сразу: серебро и опыт у героя, договор закрыт, береста ушла из сумки
              assertTrue(after.silver == hero.silver + Silver && after.exp > hero.exp && q.village.isEmpty &&
                inv.snapshot.isEmpty && sd.contains(Json.Null) && off.contains(userId -> TaskKind.VillageCaravan))
    },

    test("вернулись с добычи после засады — следующий переход с таймером; после засады на последнем — город") {
      for {
        c     <- content
        d1    <- dao(trip = Some(CaravanTrip(3, 1, 0L)))
        r     <- TestRenderer.make
        pair  <- caravan(d1, TestInventoryRepository.withItems(List(paper)), c)
        (st1, sched) = pair
        _     <- st1.enter(testUser, r)
        road  <- r.sentScreens.map(_.last)
        trip  <- tripOf(d1)
        tasks <- sched.scheduled
        d2    <- dao(trip = Some(CaravanTrip(VillageRates.CaravanLegs + 1, 2, 0L)))
        st2   <- caravan(d2, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _     <- st2.enter(testUser, r)
        city  <- r.sentScreens.map(_.last)
        q     <- questOf(d2)
        _     <- st2.enter(testUser, r)
        again <- r.sentScreens.map(_.last)
        after <- d2.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(road.text.contains("Изгородь расступается") && trip.exists(t => t.leg == 3 && t.nextAt > 0L) &&
                tasks.map(_.kind) == List(TaskKind.VillageCaravan)) &&
              assertTrue(city.text.contains(s"Обоз въезжает в город $City") && q.village.isEmpty) &&
              // второй вход платы не даёт: у ворот пусто
              assertTrue(again.text == "У ворот пусто: обоза здесь больше нет." && after.silver == hero.silver + Silver)
    },

    test("из меню — в пути: какой переход идёт, таймер заново, а кончившийся переход — сразу") {
      for {
        c     <- content
        _     <- TestClock.adjust(10.minutes)
        d     <- dao(trip = Some(ended(2, 1)))
        r     <- TestRenderer.make
        pair  <- caravan(d, TestInventoryRepository.withItems(List(paper)), c)
        (st, sched) = pair
        _     <- st.enter(testUser, r)
        road  <- r.sentScreens.map(_.last)
        tasks <- sched.scheduled
      } yield assertTrue(road.text.contains("Караван в пути: переход 2 из 4")) &&
              assertTrue(tasks.map(t => (t.kind, t.fireAt)) == List(TaskKind.VillageCaravan -> 10L * 60L * 1000L))
    },

    test("бросить караван в пути — с подтверждением: контракт сорван, береста из сумки, таймер снят") {
      for {
        c       <- content
        d       <- dao(trip = Some(CaravanTrip(2, 1, 5L * 60L * 1000L)))
        inv      = TestInventoryRepository.withItems(List(paper))
        r       <- TestRenderer.make
        pair    <- caravan(d, inv, c)
        (st, sched) = pair
        ask     <- st.action(testUser, tap("VillageCaravanAbandon"), r)
        confirm <- r.sentScreens.map(_.last)
        q0      <- questOf(d)
        done    <- st.action(testUser, tap("VillageCaravanAbandonYes"), r)
        bye     <- r.sentScreens.map(_.last)
        q       <- questOf(d)
        off     <- sched.cancelled
      } yield assertTrue(ask == StateType.VillageCaravan && confirm.text.contains("Бросить караван на полпути") &&
                confirm.choices.map(_.id) == List("VillageCaravanAbandonYes", "VillageCaravanBack") && q0.village.isDefined) &&
              assertTrue(done == StateType.Dungeon && bye.text.contains("Контракт деревни сорван") && q.village.isEmpty &&
                inv.snapshot.isEmpty && off.contains(userId -> TaskKind.VillageCaravan))
    },

    test("у ворот «Уйти» — обоз подождёт, контракт при герое; «Отказаться» — как у логова") {
      for {
        c     <- content
        d     <- dao()
        inv    = TestInventoryRepository.withItems(List(paper))
        r     <- TestRenderer.make
        st    <- caravan(d, inv, c).map(_._1)
        left  <- st.action(testUser, tap("VillageCaravanLeave"), r)
        kept   = inv.snapshot
        q1    <- questOf(d)
        _     <- st.action(testUser, tap("VillageCaravanAbandon"), r)
        ask   <- r.sentScreens.map(_.last)
        done  <- st.action(testUser, tap("VillageCaravanAbandonYes"), r)
        q2    <- questOf(d)
      } yield assertTrue(left == StateType.Dungeon && q1.village.isDefined && kept.nonEmpty) &&
              assertTrue(ask.text.contains("Отказаться от контракта деревни") && done == StateType.Dungeon &&
                q2.village.isEmpty && inv.snapshot.isEmpty)
    },

    test("пал в засаде: караван разграблен — контракт сорван, береста из сумки; проснуться в лабиринте со строкой засады") {
      for {
        c   <- content
        d   <- dao()
        inv  = TestInventoryRepository.withItems(List(paper))
        r   <- TestRenderer.make
        _   <- d.writeActiveBattle(userId, SoloPveBattle.fromGroup(Villages.caravanAmbush(contract(), CaravanAmbush.Bandits, Rng(1L)),
                 hero, Nil).copy(story = Some(Villages.CaravanStory)).asJson)
        _   <- DeathState(d, inv, c).enter(testUser, r)
        sd  <- d.readSceneData(userId)
        q   <- questOf(d)
      } yield assertTrue(sd.flatMap(_.hcursor.get[List[String]]("wakeLines").toOption).contains(List("village.caravan.death"))) &&
              assertTrue(sd.exists(_.hcursor.downField("wakeTo").failed)) &&
              assertTrue(q.village.isEmpty && inv.snapshot.isEmpty) &&
              assertTrue(c.text("village.caravan.death").contains("Караван разграблен, контракт деревни сорван") &&
                BattleState.RaidStories.contains(Villages.CaravanStory))
    },

    test("засады: банда сборная из смертных рас, Чернолом — гоблины, ищейки — каджиты; все уровня задания, четверо и трое") {
      val c   = contract()
      def of(a: CaravanAmbush, seed: Long) = Villages.caravanAmbush(c, a, Rng(seed))
      val gangs = (1L to 50L).toList.map(of(CaravanAmbush.Bandits, _))
      val size  = VillageRates.CaravanAmbushFront + VillageRates.CaravanAmbushBack
      def rarities(ms: List[pangea.model.monster.Monster]) = ms.map(_.rarity)
      assertTrue(CaravanAmbush.all.forall(a => (1L to 5L).forall(s => of(a, s).size == size && of(a, s).forall(_.lvl == QuestLevel.toLong)))) &&
      assertTrue(gangs.flatten.forall(m => Race.mortals.contains(m.race)) && gangs.flatten.map(_.race).distinct.size > 3) &&
      assertTrue(of(CaravanAmbush.Chernolom, 1L).forall(_.race == Race.Goblin) && of(CaravanAmbush.Hounds, 1L).forall(_.race == Race.Khajiit)) &&
      assertTrue(rarities(of(CaravanAmbush.Hounds, 3L).take(4)).forall(r => VillageRates.CaravanAmbushFrontRarities.contains(r)) &&
        rarities(of(CaravanAmbush.Hounds, 3L).drop(4)).forall(r => VillageRates.CaravanAmbushBackRarities.contains(r)))
    },

    test("караванщики: редкие расы деревни уровня задания, имя на кнопке — целиком; после боя в отряд не встают") {
      val g = Villages.caravanGuard(contract(), "Старший караванщик")
      assertTrue(g.kind == AllyKind.CaravanGuard && g.name == "Старший караванщик" && g.callName == "Старший караванщик") &&
      assertTrue(g.stats == MonsterGenerator.generateOfRaceAndRarity(QuestLevel, Race.Gnome, Rarity.Rare).fightStats &&
        g.hp == g.stats.hp && g.home == 0) &&
      assertTrue(BattleState.goneKey(g) == "battle.ally.caravanGuardDown")
    },

    test("выход: пять повозок на местах 1–5, прочность — как у редкого моба расы деревни; у ворот — подсказка; в пути — сколько цело") {
      for {
        c     <- content
        d     <- dao()
        r     <- TestRenderer.make
        st    <- caravan(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _     <- st.enter(testUser, r)
        gates <- r.sentScreens.map(_.last)
        _     <- st.action(testUser, tap("VillageCaravanGo"), r)
        road  <- r.sentScreens.map(_.last)
        trip  <- tripOf(d)
        rare   = MonsterGenerator.generateOfRaceAndRarity(QuestLevel, Race.Gnome, Rarity.Rare).fightStats
      } yield assertTrue(gates.text.contains("повозки встают на первые пять мест")) &&
              assertTrue(trip.flatMap(_.wagons).exists(ws => ws.map(_.position) == List(1, 2, 3, 4, 5) &&
                ws.forall(w => w.hp == rare.hp && w.armor == rare.armor && w.maxHp == rare.hp && w.burn.isEmpty))) &&
              assertTrue(road.text.contains("Повозок в обозе: 5 из 5"))
    },

    test("засада с обозом: при разбойниках — награбленное, Чернолом бьёт повозки и жжёт вдвое чаще, ищейки их не трогают; караванщики — на свободные повозки") {
      val fresh = Villages.caravanWagons(contract())
      def fight(kind: Int) =
        for {
          c  <- content
          d  <- dao(trip = Some(ended(2, 0).copy(wagons = Some(fresh))))
          r  <- TestRenderer.make
          st <- caravan(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
          _  <- TestRandom.feedInts(0, kind)
          _  <- st.action(testUser, legDone, r)
          b  <- battleOf(d)
        } yield b
      for {
        _      <- TestClock.adjust(10.minutes)
        bandit <- fight(0)
        goblin <- fight(1)
        hound  <- fight(2)
        guards  = goblin.toList.flatMap(_.group.allies.filter(_.kind == AllyKind.CaravanGuard).map(_.position))
      } yield assertTrue(bandit.flatMap(_.group.convoy).contains(Convoy(fresh, hitsWagons = true, ignitePct = 25L, plunders = true))) &&
              assertTrue(goblin.flatMap(_.group.convoy).contains(Convoy(fresh, hitsWagons = true, ignitePct = 50L))) &&
              assertTrue(hound.flatMap(_.group.convoy).exists(cv => !cv.hitsWagons && !cv.plunders && cv.wagons == fresh)) &&
              // герой на первом месте — караванщики встают на повозки 2 и 3
              assertTrue(guards == List(2, 3))
    },

    test("вернулись из засады: побитые повозки едут дальше побитыми; не осталось ни одной — контракт сорван") {
      val fresh  = Villages.caravanWagons(contract())
      val beaten = fresh.map(w => if (w.position == 2) w.copy(hp = 1L, armor = 0L) else w)
      val gone   = fresh.map(_.copy(hp = 0L, armor = 0L))
      for {
        c     <- content
        d1    <- dao(trip = Some(CaravanTrip(3, 1, 0L, Some(beaten))))
        r     <- TestRenderer.make
        st1   <- caravan(d1, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _     <- st1.enter(testUser, r)
        trip  <- tripOf(d1)
        d2    <- dao(trip = Some(CaravanTrip(3, 1, 0L, Some(gone))))
        inv    = TestInventoryRepository.withItems(List(paper))
        pair  <- caravan(d2, inv, c)
        (st2, sched) = pair
        _     <- st2.enter(testUser, r)
        lost  <- r.sentScreens.map(_.last)
        q     <- questOf(d2)
        off   <- sched.cancelled
      } yield assertTrue(trip.exists(t => t.leg == 3 && t.nextAt > 0L && t.wagons.contains(beaten))) &&
              assertTrue(lost.text.contains("Последняя повозка догорает") && lost.choices.map(_.id) == List("VillageCaravanOut")) &&
              assertTrue(q.village.isEmpty && inv.snapshot.isEmpty && off.contains(userId -> TaskKind.VillageCaravan))
    },

    test("плата — за дошедшее: три повозки из пяти — три пятых платы, и купчиха говорит, сколько дошло") {
      val fresh = Villages.caravanWagons(contract())
      val three = fresh.map(w => if (w.position >= 4) w.copy(hp = 0L, armor = 0L) else w)
      for {
        c     <- content
        d     <- dao(trip = Some(CaravanTrip(VillageRates.CaravanLegs + 1, 2, 0L, Some(three))))
        r     <- TestRenderer.make
        st    <- caravan(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _     <- st.enter(testUser, r)
        city  <- r.sentScreens.map(_.last)
        after <- d.getHeroByUserId(userId).map(_.get)
        due    = Silver * 3L / 5L
      } yield assertTrue(city.text.contains("Из 5 повозок до города дошло 3") && city.text.contains(s"Получено $due серебра")) &&
              assertTrue(after.silver == hero.silver + due)
    },

    test("сцена в пути — своими ключами: чужое в scene_data за караван не читается, своё едет туда и обратно") {
      val t = CaravanTrip(3, 2, 12345L)
      assertTrue(t.asJson.as[CaravanTrip].contains(t)) &&
      assertTrue(Json.obj("flowersLeft" -> 3.asJson, "nextFlowerAt" -> 5L.asJson).as[CaravanTrip].isLeft &&
        Json.Null.as[CaravanTrip].isLeft) &&
      assertTrue(Json.obj("caravanLeg" -> 2.asJson).as[CaravanTrip].contains(CaravanTrip(2, 0, 0L))) &&
      assertTrue(!CaravanTrip(4, 0, 0L).arrived && CaravanTrip(5, 0, 0L).arrived && CaravanTrip.start == CaravanTrip(1, 0, 0L))
    },

    test("тексты: четыре перехода, тишина, засады каждого вида, караванщики и всё вокруг") {
      for {
        c <- content
        ambushes = CaravanAmbush.all.map(a => c.text("village.caravan.ambush." + a.key))
      } yield assertTrue(c.list("village.caravan.legs").size == VillageRates.CaravanLegs &&
                c.list("village.caravan.legs").forall(_.contains("{city}"))) &&
              assertTrue(c.list("village.caravan.quiet").nonEmpty && c.list("village.caravan.guards").size == 2) &&
              assertTrue(ambushes.size == 3 && ambushes.forall(_.nonEmpty)) &&
              assertTrue(List("intro", "noEscape", "onRoad", "abandon.text", "abandon.done", "arrival", "gone", "left",
                "death").forall(k => c.text(s"village.caravan.$k").nonEmpty)) &&
              assertTrue(c.choice("x", "village.caravan.goLabel").label == "🐎 Выступить" &&
                c.choice("x", "village.caravan.abandonLabel").label == "Бросить караван") &&
              assertTrue(c.text("battle.ally.caravanGuardDown").contains("{name}"))
    }
  )
}
