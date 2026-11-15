package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.home.{HomeData, House}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.barti.BartiHouseState
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.guild.MasterHornState
import pangea.service.state.states.home.HomeState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryLine, StoryMorningQuest, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Акт II, глава 6 «Наутро»: начинается дома, наутро после кладбища, — у
  * каждого пути Рубежа I своя история. Тень вывозит из города писаря Горна
  * (Горн сам говорит, где тот сидит; в «Трёх костях» — откуп или бой). Щит
  * стережёт Брея в подвале совета — Юнона просит, Горн с искателями встают в
  * строй; Брей заговорил — Юнона отдаёт опись. Одиночка идёт к Барти — тот
  * находит себя в книге ячейки, а потом дерётся со сковородой. У всех в конце
  * опись урожая; глава 7 висит до обновления. */
object StoryMorningSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(silver: Long = 100L): Hero =
    TestFixtures.hero(userId, state = StateType.GlobalMap).copy(lvl = 26L, silver = silver, name = Some("Лис"))

  /** Акт I пройден, на Рубеже I выбран путь `path`. */
  private def afterTool(path: Path): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    val face = StoryPowderQuest.finish(base, WardenFate.Killed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = SojkaFate.Broke.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
    StoryToolQuest.finish(face.take(NpcQuest.StoryTool, 0L), path)
  }

  /** Глава 6 на шаге `n`. */
  private def morning(path: Path, n: Int): NpcQuests =
    StoryMorningQuest.step(afterTool(path).take(NpcQuest.StoryMorning, 0L), n)

  private def lore(branch: Option[String]): LoreData =
    LoreData(firstSteps = true, firstStepsDone = true, cluePasses = 5, prologueBranch = branch)

  private def dao(h: Hero, q: NpcQuests, branch: Option[String] = Some("Human")) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- d.writeLoreData(userId, lore(branch).asJson)
      _ <- d.writeHomeData(userId, HomeData(tier = House.Cottage.tier).asJson)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def battleOf(d: TestHeroDao) = d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
  private def lootOf(d: TestHeroDao)   = d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))

  private def item(kind: QuestItemKind, id: Long): Item = QuestItemKind.item(kind).copy(id = id)
  private val order = item(QuestItemKind.StoryShadowOrder, 961L)
  private val writ  = item(QuestItemKind.StoryCouncilWrit, 962L)
  private val book  = item(QuestItemKind.StoryCellBook, 963L)

  private def cardOf(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    InventoryState(d, inv, TestItemRepository.make, c)

  private def openCard(state: InventoryState, it: Item, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${it.id}"}""")), r)

  private def homeOf(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    TestScheduler.make.map(sch => HomeState(d, sch, c, inv, TestItemRepository.make))

  override def spec = suite("Акт II, глава 6 «Наутро»")(

    test("дома наутро: у каждого пути своя сцена, глава взята; у Тени — записка того, кто пережил пролог, и поручение в сумке; второй раз — ничего") {
      def wake(path: Path, branch: Option[String], bag: List[Item]) =
        for {
          d     <- dao(hero(), afterTool(path), branch)
          inv    = TestInventoryRepository.withItems(bag)
          c     <- content
          r     <- TestRenderer.make
          st    <- homeOf(d, inv, c)
          _     <- st.enter(testUser, r)
          said  <- texts(r)
          q     <- questOf(d)
          r2    <- TestRenderer.make
          _     <- st.enter(testUser, r2)
          again <- texts(r2)
        } yield (said, q, inv.snapshot, again)
      for {
        elorin <- wake(Path.Shadow, Some("Human"), Nil)
        elias  <- wake(Path.Shadow, Some("Elf"), Nil)
        both   <- wake(Path.Shadow, None, Nil)
        shield <- wake(Path.Shield, None, List(writ))
        alone  <- wake(Path.Alone, None, List(book))
      } yield assertTrue(elorin._1.contains("Испытательный срок начинается сегодня") && elorin._1.contains("Расходы — из твоего задатка") &&
                elias._1.contains("С меня пиво") && both._1.contains("И пиво с меня. — тоже Э.") &&
                elorin._1.contains("Сжечь всегда успею")) &&
              assertTrue(elorin._3.exists(_.questItem.contains(QuestItemKind.StoryShadowOrder))) &&
              assertTrue(shield._1.contains("Не про дома") && shield._1.contains("зайти к Юноне Кинтслев в канцелярию совета") &&
                alone._1.contains("Болтает. Платить пивом. Не убирать — полезен")) &&
              assertTrue(List(elorin, elias, both, shield, alone).forall { case (said, q, _, again) =>
                said.contains("📜 Новое задание: «Наутро»") && q.onStep(NpcQuest.StoryMorning, 1) &&
                  StoryLine.active(q).contains(NpcQuest.StoryMorning) && !again.contains("Новое задание")
              }) &&
              assertTrue(shield._3.count(_.questItem.contains(QuestItemKind.StoryCouncilWrit)) == 1 &&
                alone._3.count(_.questItem.contains(QuestItemKind.StoryCellBook)) == 1)
    },

    test("Тень: Горн ищет писаря и сам говорит, где тот сидит; поручение — в «Три кости», из города; откуп — опись, глава закрыта, «Новенький» висит") {
      for {
        c      <- content
        d      <- dao(hero(silver = 30000L), morning(Path.Shadow, 1))
        inv     = TestInventoryRepository.withItems(List(order))
        r      <- TestRenderer.make
        horn    = MasterHornState(d, inv, c)
        _      <- horn.enter(testUser, r)
        menu   <- r.sentScreens.map(_.last)
        _      <- horn.action(testUser, tap("HornClerk"), r)
        said   <- texts(r)
        q1     <- questOf(d)
        _      <- d.writeReturnState(userId, Some(StateType.GlobalMap))
        card    = cardOf(d, inv, c)
        _      <- openCard(card, order, r)
        shown  <- r.sentScreens.map(_.last)
        go     <- card.action(testUser, tap("StoryDenGo"), r)
        den     = StoryDenState(d, inv, TestItemRepository.make, c)
        _      <- den.enter(testUser, r)
        room   <- r.sentScreens.map(_.last)
        _      <- den.action(testUser, tap("DenPay"), r)
        end    <- texts(r)
        last   <- r.sentScreens.map(_.last)
        q      <- questOf(d)
        h      <- heroOf(d)
      } yield assertTrue(menu.choices.exists(_.id == "HornClerk") && said.contains("Ты его не видел?") &&
                said.contains("Обычно он сидит в «Трёх костях»") && StoryMorningQuest.toDen(q1)) &&
              assertTrue(shown.text.contains("Ян Пятак сидит в «Трёх костях»") &&
                shown.text.contains(Difficulty.render(StoryMorningQuest.Level)) &&
                shown.choices.exists(_.id == "StoryDenGo") && go == StateType.StoryDen) &&
              assertTrue(room.text.contains(s"Писарь мне должен ${StoryMorningQuest.debt} серебра") && room.text.contains("Бенно") &&
                room.choices.map(_.id) == List("DenPay", "DenFight", "DenLeave") &&
                room.choices.headOption.exists(_.label.contains(StoryMorningQuest.debt.toString))) &&
              assertTrue(end.contains("Приятно иметь дело") && end.contains("Лодка уходит в туман") &&
                end.contains("«Лис — Кинэт. Не пугать раньше времени»") && end.contains("Отдавать его Э. я пока не тороплюсь") &&
                end.contains("📜 Квест «Наутро» завершён.") && end.contains("📜 Новое задание: «Новенький»")) &&
              assertTrue(q.isDone(NpcQuest.StoryMorning) && q.isTaken(NpcQuest.StoryNewcomer) &&
                StoryLine.active(q).contains(NpcQuest.StoryNewcomer)) &&
              assertTrue(h.silver == 30000L - StoryMorningQuest.debt + StoryMorningQuest.silver &&
                StoryMorningQuest.debt == 26000L && h.exp > 0L) &&
              assertTrue(!inv.snapshot.exists(_.questItem.contains(QuestItemKind.StoryShadowOrder)) &&
                inv.snapshot.exists(_.questItem.contains(QuestItemKind.StoryHarvest)) && last.choices.map(_.id) == List("DenLeave"))
    },

    test("Тень: серебра мало — отказ, писарь сидит; бой — хозяин мифический и четверо вышибал, люди и орки, без бегства; победа — писарь свободен") {
      for {
        c      <- content
        d      <- dao(hero(silver = 0L), morning(Path.Shadow, 2))
        inv     = TestInventoryRepository.withItems(List(order))
        r      <- TestRenderer.make
        den     = StoryDenState(d, inv, TestItemRepository.make, c)
        _      <- den.action(testUser, tap("DenPay"), r)
        poor   <- r.sentScreens.map(_.last)
        q0     <- questOf(d)
        res    <- den.action(testUser, tap("DenFight"), r)
        b      <- battleOf(d)
        loot   <- lootOf(d)
        _      <- d.writeSceneData(userId, StoryFight.mark(StoryDenState.WonKey))
        _      <- den.enter(testUser, r)
        said   <- texts(r)
        q      <- questOf(d)
      } yield assertTrue(poor.text.contains("Столько серебра у меня нет") && poor.choices.exists(_.id == "DenFight") &&
                StoryMorningQuest.toDen(q0)) &&
              assertTrue(res == StateType.Battle && b.exists(x => x.story.contains(StoryMorningQuest.DenStory) && x.cantFlee &&
                x.monsterName.contains("Бенно") && x.monsterRarity == Rarity.Mythical.entryName &&
                x.monsterLvl == StoryMorningQuest.Level.toLong && x.group.others.size == 4 &&
                x.group.others.forall(_.rarity == Rarity.Rare.entryName) &&
                x.group.others.count(_.race == Race.Orc.entryName) == 2)) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.StoryDen)) &&
                BattleState.RaidStories.contains(StoryMorningQuest.DenStory)) &&
              assertTrue(said.contains("Бенно сидит под столом") && said.contains("Лодка уходит в туман") &&
                q.isDone(NpcQuest.StoryMorning))
    },

    test("Щит: Юнона просит стеречь Брея, Горн даёт людей и идёт сам; в подвале — главарь и четверо, Горн и двое искателей в строю; Брей заговорил — у Юноны опись, глава закрыта") {
      for {
        c       <- content
        d       <- dao(hero(), morning(Path.Shield, 1), None)
        inv      = TestInventoryRepository.withItems(List(writ))
        r       <- TestRenderer.make
        home    <- homeOf(d, inv, c)
        _       <- home.action(testUser, tap("HomeOffice"), r)
        office  <- r.sentScreens.map(_.last)
        _       <- home.action(testUser, tap("HomeJunonaBrey"), r)
        asked   <- texts(r)
        q2      <- questOf(d)
        horn     = MasterHornState(d, inv, c)
        _       <- horn.enter(testUser, r)
        hmenu   <- r.sentScreens.map(_.last)
        _       <- horn.action(testUser, tap("HornCellar"), r)
        q3      <- questOf(d)
        _       <- d.writeReturnState(userId, Some(StateType.GlobalMap))
        go      <- cardOf(d, inv, c).action(testUser, tap("StoryCellarGo"), r)
        cellar   = StoryCellarState(d, c)
        _       <- cellar.enter(testUser, r)
        watch   <- r.sentScreens.map(_.last)
        res     <- cellar.action(testUser, tap("CellarFight"), r)
        b       <- battleOf(d)
        loot    <- lootOf(d)
        _       <- d.writeSceneData(userId, StoryFight.mark(StoryCellarState.WonKey))
        _       <- cellar.enter(testUser, r)
        talk    <- r.sentScreens.map(_.last)
        q4      <- questOf(d)
        _       <- home.action(testUser, tap("HomeOffice"), r)
        office2 <- r.sentScreens.map(_.last)
        _       <- home.action(testUser, tap("HomeJunonaTestimony"), r)
        said    <- texts(r)
        q       <- questOf(d)
        h       <- heroOf(d)
        allies   = b.toList.flatMap(_.group.allies)
      } yield assertTrue(office.choices.exists(_.id == "HomeJunonaBrey") && asked.contains("стража у меня через одного продаётся") &&
                asked.contains("Подмастерья держим отдельно") && asked.contains("допей чай") && StoryMorningQuest.toHornMen(q2)) &&
              assertTrue(hmenu.choices.exists(_.id == "HornCellar") && StoryMorningQuest.toCellar(q3) && go == StateType.StoryCellar) &&
              assertTrue(watch.text.contains("Вот и не скучно") && watch.choices.map(_.id) == List("CellarFight", "CellarLeave")) &&
              assertTrue(res == StateType.Battle && b.exists(x => x.story.contains(StoryMorningQuest.CellarStory) && x.cantFlee &&
                x.monsterName.contains("Сухарь") && x.monsterRarity == Rarity.Mythical.entryName && x.group.others.size == 4 &&
                x.group.others.forall(_.rarity == Rarity.Rare.entryName))) &&
              assertTrue(allies.count(a => a.kind == AllyKind.GuildMaster && a.home == 0 && a.undead.exists(_.name == "Горн")) == 1 &&
                allies.count(a => a.kind == AllyKind.GuildSeeker && a.home == 0) == 2 &&
                loot.exists(_.returnState.contains(StateType.StoryCellar))) &&
              assertTrue(talk.text.contains("Колено мне это завтра припомнит") && talk.text.contains("Манжеты у него в пыли") &&
                StoryMorningQuest.toTestimony(q4)) &&
              assertTrue(office2.choices.exists(_.id == "HomeJunonaTestimony") && said.contains("Брей говорит — Брей живёт") &&
                said.contains("Я бы закрыл его учёт сам") && said.contains("«Лис — Кинэт. Не пугать раньше времени»") &&
                said.contains("до первого снега, а не после") && said.contains("📜 Квест «Наутро» завершён.")) &&
              assertTrue(q.isDone(NpcQuest.StoryMorning) && q.isTaken(NpcQuest.StoryNewcomer) &&
                h.silver == 100L + StoryMorningQuest.silver &&
                inv.snapshot.exists(_.questItem.contains(QuestItemKind.StoryHarvest)) &&
                inv.snapshot.exists(_.questItem.contains(QuestItemKind.StoryCouncilWrit)))
    },

    test("Щит: голос Брея — того, кто пережил пролог; без грамоты при себе Юнона и Горн о деле молчат") {
      for {
        c      <- content
        d      <- dao(hero(), morning(Path.Shield, 3), Some("Elf"))
        r      <- TestRenderer.make
        _      <- d.writeSceneData(userId, StoryFight.mark(StoryCellarState.WonKey))
        _      <- StoryCellarState(d, c).enter(testUser, r)
        elias  <- r.sentScreens.map(_.last)
        e      <- dao(hero(), morning(Path.Shield, 2), None)
        _      <- MasterHornState(e, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        bare   <- r.sentScreens.map(_.last)
        f      <- dao(hero(), morning(Path.Shield, 1), None)
        home   <- homeOf(f, TestInventoryRepository.withItems(Nil), c)
        _      <- home.action(testUser, tap("HomeOffice"), r)
        office <- r.sentScreens.map(_.last)
      } yield assertTrue(elias.text.contains("Чёрт с ними") && !elias.text.contains("Манжеты")) &&
              assertTrue(!bare.choices.exists(_.id == "HornCellar") && !office.choices.exists(_.id == "HomeJunonaBrey"))
    },

    test("Сам по себе: книга — к Барти, из города; Барти находит себя в книге; знакомы ли — по «Ходовому товару»; стук — счетовод и четверо, Барти со сковородой в строю; отбились — опись") {
      def visit(met: Boolean) =
        for {
          c      <- content
          q0      = morning(Path.Alone, 1)
          d      <- dao(hero(), if (met) q0.take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods) else q0)
          inv     = TestInventoryRepository.withItems(List(book))
          r      <- TestRenderer.make
          _      <- d.writeReturnState(userId, Some(StateType.GlobalMap))
          card    = cardOf(d, inv, c)
          _      <- openCard(card, book, r)
          shown  <- r.sentScreens.map(_.last)
          go     <- card.action(testUser, tap("StoryBartiGo"), r)
          house   = BartiHouseState(d, inv, c, Some(TestItemRepository.make))
          r2     <- TestRenderer.make
          _      <- house.enter(testUser, r2)
          said   <- texts(r2)
          door   <- r2.sentScreens.map(_.last)
          q2     <- questOf(d)
          res    <- house.action(testUser, tap("BartiStoryFight"), r2)
          b      <- battleOf(d)
          _      <- d.writeSceneData(userId, StoryFight.mark(BartiHouseState.WonKey))
          r3     <- TestRenderer.make
          _      <- house.enter(testUser, r3)
          end    <- texts(r3)
          q      <- questOf(d)
        } yield (shown, go, said, door, q2, res, b, end, q, inv.snapshot)
      for {
        met      <- visit(met = true)
        stranger <- visit(met = false)
      } yield assertTrue(met._1.text.contains("Барти: «Болтает. Платить пивом»") && met._1.choices.exists(_.id == "StoryBartiGo") &&
                met._2 == StateType.BartiHouse) &&
              assertTrue(met._3.contains("А, это ты! Заходи") && !met._3.contains("Мы ведь знакомы?") &&
                stranger._3.contains("Мы ведь знакомы? Нет?") && met._3.contains("Я им лучших сплетен на тысячу серебра натаскал")) &&
              assertTrue(met._4.text.contains("Счетовод ячейки пришёл за своей книгой") &&
                met._4.choices.map(_.id) == List("BartiStoryFight", "BartiLeave") && StoryMorningQuest.toBartiFight(met._5)) &&
              assertTrue(met._6 == StateType.Battle && met._7.exists(x => x.story.contains(StoryMorningQuest.BartiStory) && x.cantFlee &&
                x.monsterName.contains("Счетовод") && x.monsterRarity == Rarity.Mythical.entryName && x.group.others.size == 4) &&
                met._7.toList.flatMap(_.group.allies)
                  .count(a => a.kind == AllyKind.Informant && a.home == 0 && a.undead.exists(_.name == "Барти")) == 1) &&
              assertTrue(met._8.contains("Я одного достал") && met._8.contains("Сойка, — читает Барти вслух") &&
                met._8.contains("📜 Квест «Наутро» завершён.") && met._9.isDone(NpcQuest.StoryMorning) &&
                met._10.exists(_.questItem.contains(QuestItemKind.StoryHarvest)) &&
                met._10.exists(_.questItem.contains(QuestItemKind.StoryCellBook)))
    },

    test("без хранилища вещей у Барти — по-старому: «Наутро» не трогает дом") {
      for {
        c <- content
        d <- dao(hero(), morning(Path.Alone, 1))
        r <- TestRenderer.make
        _ <- BartiHouseState(d, TestInventoryRepository.withItems(List(book)), c).enter(testUser, r)
        q <- questOf(d)
        shown <- r.sentScreens.map(_.last)
      } yield assertTrue(StoryMorningQuest.toBarti(q) && !shown.text.contains("Счетовод"))
    },

    test("смерть в «Трёх костях», в подвале совета и у Барти — проснуться у порта, в городе и на Торговой площади; шаг тот же") {
      def die(path: Path, n: Int, story: String) =
        for {
          d    <- dao(hero(), morning(path, n))
          fight = SoloPveBattle.fromGroup(StoryMorningQuest.gang("Сухарь"), hero(), List.fill(5)(0L)).copy(story = Some(story))
          _    <- d.writeActiveBattle(userId, fight.asJson)
          c    <- content
          r    <- TestRenderer.make
          _    <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
          wake <- d.readSceneData(userId)
          q    <- questOf(d)
        } yield (wake.flatMap(_.hcursor.get[List[String]]("wakeLines").toOption),
                 wake.flatMap(_.hcursor.get[StateType]("wakeTo").toOption), q.onStep(NpcQuest.StoryMorning, n))
      for {
        c      <- content
        den    <- die(Path.Shadow, 2, StoryMorningQuest.DenStory)
        cellar <- die(Path.Shield, 3, StoryMorningQuest.CellarStory)
        barti  <- die(Path.Alone, 2, StoryMorningQuest.BartiStory)
      } yield assertTrue(den == ((Some(List("story.storyMorning.den.death")), Some(StateType.HarborQuarter), true))) &&
              assertTrue(cellar == ((Some(List("story.storyMorning.cellar.death")), Some(StateType.GlobalMap), true))) &&
              assertTrue(barti == ((Some(List("story.storyMorning.barti.death")), Some(StateType.MarketSquare), true))) &&
              assertTrue(c.text("story.storyMorning.den.death").contains("Поручение при вас") &&
                c.text("story.storyMorning.cellar.death").contains("Грамота при вас"))
    },

    test("журнал: глава — сюжетным заданием, шаг своего пути; после неё — «Новенький», на карточке описи — что делать в главе 7") {
      for {
        c <- content
      } yield {
        def journal(q: NpcQuests) = QuestJournal.render(c, hero(), lore(None), q, DailyQuests.empty, None, 0L)
        val done    = morning(Path.Alone, 2).finish(NpcQuest.StoryMorning).take(NpcQuest.StoryNewcomer, 0L)
        val harvest = InventoryState.questText(c, item(QuestItemKind.StoryHarvest, 964L), done, 26L)
        assertTrue(journal(morning(Path.Shadow, 2)).contains("📌 «Наутро»") &&
          journal(morning(Path.Shadow, 2)).contains("вызволить писаря из «Трёх костей»")) &&
        assertTrue(journal(morning(Path.Shield, 3)).contains("стеречь ночью Брея в подвале совета")) &&
        assertTrue(journal(morning(Path.Alone, 1)).contains("зайти к Барти, в дом с синей дверью")) &&
        assertTrue(journal(done).contains("📌 «Новенький»") && journal(done).contains("найти на главной площади того, кто упал из портала")) &&
        assertTrue(harvest.contains("Урожай к первому снегу") && harvest.contains("Сходить посмотреть, кто успеет к нему первым"))
      }
    },

    test("гости в строю — уровня задания, из дома не идут; выбыл — своя строка лога") {
      for {
        c <- content
      } yield {
        val guests = StoryMorningQuest.cellarGuests(c) :+ StoryMorningQuest.bartiGuest(c)
        assertTrue(guests.map(_.kind) == List(AllyKind.GuildMaster, AllyKind.GuildSeeker, AllyKind.GuildSeeker, AllyKind.Informant) &&
          guests.forall(g => g.home == 0 && g.lvl == StoryMorningQuest.Level.toLong && c.text(BattleState.goneKey(g)).contains("{name}")))
      }
    }
  )
}
