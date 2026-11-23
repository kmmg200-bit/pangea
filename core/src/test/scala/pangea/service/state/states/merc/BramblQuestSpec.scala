package pangea.service.state.states.merc

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.battle.{Element, SoloPveBattle}
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, MaterialKind, QuestItemKind}
import pangea.model.monster.MiniBoss
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.squad.{AllyKind, AllyRates, Squad}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.events.SilverVeinState
import pangea.service.state.states.tavern.MercenariesState
import pangea.service.state.states.{ConstructionState, DeathState}
import pangea.service.state.{BramblQuest, NpcQuestLog, QuestJournal, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Замок для предков» — личное задание Брамбла: разговор за столом до найма,
  * слепок с гробницы — отцу-замочнику на Стройку, замок из материалов героя,
  * спуск в Зал предков через серебряную жилу — каменных стражей там двое, и
  * герой с Брамблом видят это до боя; замок на гробнице — потолок Брамбла +8. */
object BramblQuestSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))
  private def pick(key: String, kind: AllyKind): UserAction = tap(key, "kind" -> kind.entryName)

  private def hero(lvl: Long = 12L): Hero =
    TestFixtures.hero(userId, state = StateType.Tavern, dungeonLevel = 5).copy(lvl = lvl)

  private def cast: Item = QuestItemKind.item(QuestItemKind.BramblCast).copy(id = 811L)
  private def lock: Item = QuestItemKind.item(QuestItemKind.FamilyLock).copy(id = 812L)
  private def material(kind: MaterialKind, id: Long): Item = MaterialGenerator.item(kind).copy(id = id)

  /** Ровно то, что Оттар берёт за замок, — россыпью. */
  private def lockPrice: List[Item] =
    List(material(MaterialKind.EverburningIron, 821L), material(MaterialKind.MagicStone, 823L))

  private def toFather: NpcQuests = NpcQuests.empty.take(NpcQuest.BramblLock, 0L)
  private def toHall: NpcQuests   = toFather.update(NpcQuest.BramblLock)(_.copy(step = 2))
  private def done: NpcQuests     = toHall.finish(NpcQuest.BramblLock)

  private def dao(h: Hero, q: Option[NpcQuests] = None) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def tavern(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    MercenariesState(d, inv, c, itemRepo = Some(TestItemRepository.make))

  private def site(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    TestScheduler.make.map(sch => ConstructionState(d, sch, c, Some(inv), None, Some(TestItemRepository.make)))

  private def hall(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    BramblHallState(d, inv, c)

  private def battleOf(d: TestHeroDao) =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  override def spec = suite("Замок для предков")(

    test("у стола: «Поговорить» у Брамбла — латы с гробницы предка, сложность сразу; пока идёт «Пропавшая дружина» — разговора нет") {
      for {
        c     <- content
        d     <- dao(hero())
        r     <- TestRenderer.make
        st     = tavern(d, TestInventoryRepository.withItems(Nil), c)
        _     <- st.action(testUser, pick("MercCard", AllyKind.Gnome), r)
        card  <- r.sentScreens.map(_.last)
        _     <- st.action(testUser, pick("MercTalk", AllyKind.Gnome), r)
        talk  <- r.sentScreens.map(_.last)
        d2    <- dao(hero(), Some(NpcQuests.empty.take(NpcQuest.LostSquad, 0L)))
        _     <- tavern(d2, TestInventoryRepository.withItems(Nil), c).action(testUser, pick("MercCard", AllyKind.Gnome), r)
        squad <- r.sentScreens.map(_.last)
      } yield assertTrue(card.choices.map(_.id) == List("MercTalk", "MercHire", "MercList")) &&
              assertTrue(talk.text.contains("Я снял латы с каменного предка") &&
                talk.text.contains(Difficulty.render(11))) &&
              assertTrue(talk.choices.map(_.id) == List("MercTalkYes", "MercCard")) &&
              assertTrue(!squad.choices.exists(_.id == "MercTalk"))
    },

    test("«Помочь»: слепок в сумке, задание взято; Брамбл говорит о нём, разговора больше нет") {
      for {
        c    <- content
        d    <- dao(hero())
        inv   = TestInventoryRepository.accepting
        r    <- TestRenderer.make
        _    <- tavern(d, inv, c).action(testUser, pick("MercTalkYes", AllyKind.Gnome), r)
        last <- r.sentScreens.map(_.last)
        q    <- questOf(d)
      } yield assertTrue(BramblQuest.toFather(q) && inv.snapshot.exists(_.questItem.contains(QuestItemKind.BramblCast))) &&
              assertTrue(last.text.contains("сам поймёт, чья рука") && last.text.contains("«Слепок с гробницы предка»")) &&
              assertTrue(last.text.contains("Слепок у тебя?") && !last.choices.exists(_.id == "MercTalk"))
    },

    test("Стройка: мастерская Медноусов — только со слепком; материалов мало — нечем платить; хватает — замок в сумке, слепок у Оттара") {
      for {
        c     <- content
        none  <- dao(hero())
        r0    <- TestRenderer.make
        s0    <- site(none, TestInventoryRepository.withItems(Nil), c)
        _     <- s0.enter(testUser, r0)
        plain <- r0.sentScreens.map(_.last)
        d     <- dao(hero(), Some(toFather))
        poor   = TestInventoryRepository.withItems(List(cast, material(MaterialKind.EverburningIron, 821L)))
        r1    <- TestRenderer.make
        s1    <- site(d, poor, c)
        _     <- s1.enter(testUser, r1)
        menu  <- r1.sentScreens.map(_.last)
        _     <- s1.action(testUser, tap("BramblForge"), r1)
        ask   <- r1.sentScreens.map(_.last)
        _     <- s1.action(testUser, tap("BramblForgeGive"), r1)
        q1    <- questOf(d)
        rich   = TestInventoryRepository.withItems(cast :: lockPrice)
        r2    <- TestRenderer.make
        s2    <- site(d, rich, c)
        _     <- s2.action(testUser, tap("BramblForge"), r2)
        pay   <- r2.sentScreens.map(_.last)
        _     <- s2.action(testUser, tap("BramblForgeGive"), r2)
        said  <- texts(r2)
        q2    <- questOf(d)
      } yield assertTrue(!plain.choices.exists(_.id == "BramblForge") && menu.choices.exists(_.id == "BramblForge")) &&
              assertTrue(ask.text.contains("Брамбл. Значит, жив.") && ask.text.contains("Кусок вечно огненного железа")) &&
              // железо есть, камня нет — платить нечем, ничего не списано
              assertTrue(!ask.choices.exists(_.id == "BramblForgeGive") && BramblQuest.toFather(q1) && poor.snapshot.size == 2) &&
              assertTrue(pay.choices.exists(_.id == "BramblForgeGive")) &&
              assertTrue(said.contains("замок вешает тот, кто снял латы") && said.contains("«Замок Медноусов»")) &&
              assertTrue(BramblQuest.toHall(q2) && rich.snapshot.map(_.questItem) == List(Some(QuestItemKind.FamilyLock)))
    },

    test("серебряная жила: замок на руках — кирка проваливается в Зал предков; дружина ждёт — сперва к ней") {
      def harvest(q: NpcQuests) =
        for {
          c   <- content
          d   <- dao(hero(), Some(q))
          sch <- TestScheduler.make
          r   <- TestRenderer.make
          _   <- TestRandom.feedInts(15, 90, 90)
          _   <- TestRandom.feedBooleans(true)
          out <- SilverVeinState(d, sch, c).action(testUser, tap("Harvest"), r)
        } yield out
      for {
        down  <- harvest(toHall)
        squad <- harvest(toHall.take(NpcQuest.LostSquad, 0L))
        plain <- harvest(toFather)
      } yield assertTrue(down == StateType.BramblHall && squad == StateType.SquadMine && plain == StateType.Dungeon)
    },

    test("Зал предков: двое каменных стражей — герой и Брамбл видят их разом; бой с обоими, без бегства, Брамбл гостем со своим холодом") {
      for {
        c     <- content
        d     <- dao(hero(), Some(toHall))
        r     <- TestRenderer.make
        st     = hall(d, TestInventoryRepository.withItems(List(lock)), c)
        _     <- st.enter(testUser, r)
        intro <- r.sentScreens.map(_.last)
        out   <- st.action(testUser, tap("BramblFight"), r)
        b     <- battleOf(d)
        lvl    = MiniBoss.StoneElemental.bossLvl(BramblQuest.Level.toLong)
        gnomes = b.toList.flatMap(_.group.allies.filter(_.kind == AllyKind.Gnome))
      } yield assertTrue(intro.text.contains("Вы замечаете их одновременно с Брамблом") && intro.text.contains("«Двое") &&
                intro.text.contains(Difficulty.render(11))) &&
              assertTrue(intro.choices.map(_.id) == List("BramblFight", "OpenCharacter", "BramblLeave")) &&
              assertTrue(out == StateType.Battle) &&
              assertTrue(b.exists(x => x.bossKind.contains(MiniBoss.StoneElemental.entryName) && x.monsterLvl == lvl &&
                x.group.others.size == 1 && x.group.others.forall(_.boss.contains(MiniBoss.StoneElemental.entryName)))) &&
              assertTrue(b.exists(x => x.story.contains(BramblQuest.HallStory) && x.cantFlee && x.lootLevel.contains(11L))) &&
              assertTrue(gnomes.size == 1 && gnomes.forall(g => g.home == 0 && g.strike.isEmpty && g.element.contains(Element.Cold)))
    },

    test("оба пали: Брамбл вешает замок — задание закрыто, замок ушёл, Брамбл при герое растёт до 15-го; потом в зале тихо") {
      val hired = hero().copy(squad = Squad().hire(AllyKind.Gnome, 12L, 0L))
      for {
        c    <- content
        d    <- dao(hired, Some(toHall))
        _    <- d.writeSceneData(userId, BramblHallState.Progress(won = true).asJson)
        inv   = TestInventoryRepository.withItems(List(lock))
        r    <- TestRenderer.make
        st    = hall(d, inv, c)
        _    <- st.enter(testUser, r)
        said <- texts(r)
        q    <- questOf(d)
        h    <- d.getHeroByUserId(userId).map(_.get)
        _    <- st.enter(testUser, r)
        gone <- r.sentScreens.map(_.last)
      } yield assertTrue(said.contains("Дужка сходится с сухим щелчком") && said.contains("Получено") &&
                said.contains("Брамбл Медноус теперь растёт вместе с вами до 15-го уровня") &&
                said.contains("📜 Квест «Замок для предков» завершён.")) &&
              assertTrue(q.isDone(NpcQuest.BramblLock) && inv.snapshot.isEmpty) &&
              assertTrue(h.squad.allies.exists(a => a.kind == AllyKind.Gnome && a.grown == AllyRates.QuestGrowth)) &&
              assertTrue(gone.text.contains("на гробнице Медноусов висит замок") && gone.choices.map(_.id) == List("BramblOut"))
    },

    test("замок висит: нанятый Брамбл растёт до 15-го, без задания — до 7-го") {
      for {
        c     <- content
        d     <- dao(hero(lvl = 20L), Some(done))
        r     <- TestRenderer.make
        // 20-й уровень — четыре бутылки шнапса, как всегда (цена не меняется)
        inv    = TestInventoryRepository.withItems(
                   List.tabulate(4)(i => pangea.model.item.BrewKind.item(pangea.model.item.BrewKind.Schnapps).copy(id = 950L + i)))
        _     <- tavern(d, inv, c).action(testUser, pick("MercHire", AllyKind.Gnome), r)
        grown <- d.getHeroByUserId(userId).map(_.get.squad.allies.find(_.kind == AllyKind.Gnome))
      } yield assertTrue(grown.exists(a => a.grown == 8L && a.lvlAt(20L) == 15L)) &&
              assertTrue(AllyKind.Gnome.effectiveLvl(20L) == 7L)
    },

    test("смерть в Зале предков: проснуться в лабиринте, замок при герое — шаг тот же") {
      for {
        c    <- content
        d    <- dao(hero(), Some(toHall))
        _    <- d.writeActiveBattle(userId, BramblQuest.hallBattle(hero()).asJson)
        r    <- TestRenderer.make
        _    <- DeathState(d, TestInventoryRepository.withItems(List(lock)), c).enter(testUser, r)
        wake <- d.readSceneData(userId)
        q    <- questOf(d)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("bramblLock.hall.death")))) &&
              assertTrue(BramblQuest.toHall(q)) &&
              assertTrue(c.text("bramblLock.hall.death").contains("Замок при вас"))
    },

    test("журнал: что делать на каждом шаге") {
      for {
        c <- content
        fatherLine = QuestJournal.render(c, hero(), LoreData.empty, toFather, DailyQuests.empty, None, 0L)
        hallLine   = QuestJournal.render(c, hero(), LoreData.empty, toHall, DailyQuests.empty, None, 0L)
      } yield assertTrue(fatherLine.contains("«Замок для предков» — отнести слепок с гробницы отцу Брамбла")) &&
              assertTrue(hallLine.contains("«Замок для предков» — спуститься в Зал предков через серебряную жилу"))
    }
  )
}
