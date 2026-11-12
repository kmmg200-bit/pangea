package pangea.service.state.states.merc

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.squad.{AllyKind, AllyRates, Squad}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.tavern.MercenariesState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{FaelinQuest, NpcQuestLog, QuestJournal, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Отданный лук» — личное задание Фаэлина: разговор за столом до найма,
  * срубленный лук — вернуть изгороди, к старым корням (из лабиринта); у корней
  * лесорубы гильдии Чернолом по гильдейскому приказу — бой разом с пятнадцатью
  * гоблинами (десять простых, четверо мифических, десятник именной); отбились —
  * изгородь отдаёт Фаэлину новый, готовый лук взамен срубленного, и он растёт
  * на восемь. */
object FaelinQuestSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))
  private def pick(key: String, kind: AllyKind): UserAction = tap(key, "kind" -> kind.entryName)

  private def hero(lvl: Long = 15L): Hero =
    TestFixtures.hero(userId, state = StateType.Tavern, dungeonLevel = 5).copy(lvl = lvl)

  private def bow: Item = QuestItemKind.item(QuestItemKind.CutBow).copy(id = 841L)

  private def toRoots: NpcQuests = NpcQuests.empty.take(NpcQuest.FaelinBow, 0L)

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

  private def roots(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) = FaelinRootsState(d, inv, c)

  private def battleOf(d: TestHeroDao) =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  override def spec = suite("Отданный лук")(

    test("у стола: «Поговорить» у Фаэлина — срубленный лук, сложность сразу") {
      for {
        c    <- content
        d    <- dao(hero())
        r    <- TestRenderer.make
        st    = tavern(d, TestInventoryRepository.withItems(Nil), c)
        _    <- st.action(testUser, pick("MercCard", AllyKind.Elf), r)
        card <- r.sentScreens.map(_.last)
        _    <- st.action(testUser, pick("MercTalk", AllyKind.Elf), r)
        talk <- r.sentScreens.map(_.last)
      } yield assertTrue(card.choices.map(_.id) == List("MercTalk", "MercHire", "MercList")) &&
              assertTrue(talk.text.contains("Мне было тридцать, и я хотел лук. Я срезал ветвь сам.") &&
                talk.text.contains("Сложность: " + Difficulty.render(14))) &&
              assertTrue(talk.choices.map(_.id) == List("MercTalkYes", "MercCard"))
    },

    test("«Помочь»: срубленный лук в сумке, задание взято; Фаэлин говорит о нём") {
      for {
        c    <- content
        d    <- dao(hero())
        inv   = TestInventoryRepository.accepting
        r    <- TestRenderer.make
        _    <- tavern(d, inv, c).action(testUser, pick("MercTalkYes", AllyKind.Elf), r)
        last <- r.sentScreens.map(_.last)
        q    <- questOf(d)
      } yield assertTrue(FaelinQuest.toRoots(q) && inv.snapshot.exists(_.questItem.contains(QuestItemKind.CutBow))) &&
              assertTrue(last.text.contains("это мой долг") && last.text.contains("«Срубленный лук Фаэлина»")) &&
              assertTrue(last.text.contains("Мой у тебя") && !last.choices.exists(_.id == "MercTalk"))
    },

    test("карточка лука: сложность и куда нести; «Вернуть лук изгороди» — только из лабиринта") {
      def card(from: StateType) =
        for {
          d     <- dao(hero(), Some(toRoots))
          _     <- d.writeReturnState(userId, Some(from))
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(List(bow)), TestItemRepository.make, c)
          _     <- state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${bow.id}"}""")), r)
          shown <- r.sentScreens.map(_.last)
          go    <- state.action(testUser, tap("FaelinRootsGo"), r)
          said  <- texts(r)
        } yield (shown, go, said)
      for {
        deep <- card(StateType.Dungeon)
        city <- card(StateType.GlobalMap)
      } yield assertTrue(deep._1.text.contains("Сложность: " + Difficulty.render(14)) && deep._1.text.contains("старые корни у живой стены")) &&
              assertTrue(deep._1.choices.exists(_.id == "FaelinRootsGo") && deep._2 == StateType.FaelinRoots) &&
              assertTrue(city._2 == StateType.Inventory && city._3.contains("К старым корням изгороди идут из лабиринта"))
    },

    test("корни: лесорубы Чернолома с гильдейским приказом; бой разом с пятнадцатью — десять простых, четверо мифических, десятник; Фаэлин гостем") {
      for {
        c     <- content
        d     <- dao(hero(), Some(toRoots))
        r     <- TestRenderer.make
        st     = roots(d, TestInventoryRepository.withItems(List(bow)), c)
        _     <- st.enter(testUser, r)
        intro <- r.sentScreens.map(_.last)
        out   <- st.action(testUser, tap("FaelinFight"), r)
        b     <- battleOf(d)
        foes   = b.toList.flatMap(x => (x.monsterRace, x.monsterRarity, x.customName, x.monsterLvl) ::
                   x.group.others.map(s => (s.race, s.rarity, s.customName, s.lvl)))
        elves  = b.toList.flatMap(_.group.allies.filter(_.kind == AllyKind.Elf))
      } yield assertTrue(intro.text.contains("Гильдия Чернолом") && intro.text.contains("Приказ гильдейский") &&
                intro.text.contains("а значит, и изгородь") && intro.text.contains("Сложность: " + Difficulty.render(14))) &&
              assertTrue(intro.choices.map(_.id) == List("FaelinFight", "OpenCharacter", "FaelinLeave")) &&
              assertTrue(out == StateType.Battle) &&
              // все пятнадцать — в строю разом, очереди нет
              assertTrue(foes.size == 15 && b.exists(_.group.queue.isEmpty) &&
                foes.forall(f => f._1 == Race.Goblin.entryName && f._4 == 14L)) &&
              assertTrue(foes.count(_._2 == Rarity.Common.entryName) == 10 && foes.count(_._2 == Rarity.Mythical.entryName) == 4) &&
              assertTrue(foes.count(f => f._2 == Rarity.Legendary.entryName && f._3.contains(FaelinQuest.LeaderName)) == 1) &&
              assertTrue(b.exists(x => x.story.contains(FaelinQuest.RootsStory) && x.cantFlee && x.lootLevel.contains(14L))) &&
              assertTrue(elves.size == 1 && elves.forall(_.home == 0))
    },

    test("отбились: изгородь забирает срубленный лук и отдаёт Фаэлину новый, готовый; он рад; задание закрыто, при герое растёт до 18-го; потом тихо") {
      val hired = hero().copy(squad = Squad().hire(AllyKind.Elf, 15L, 0L))
      for {
        c    <- content
        d    <- dao(hired, Some(toRoots))
        _    <- d.writeSceneData(userId, FaelinRootsState.Progress(won = true).asJson)
        inv   = TestInventoryRepository.withItems(List(bow))
        r    <- TestRenderer.make
        st    = roots(d, inv, c)
        _    <- st.enter(testUser, r)
        said <- texts(r)
        q    <- questOf(d)
        h    <- d.getHeroByUserId(userId).map(_.get)
        _    <- st.enter(testUser, r)
        gone <- r.sentScreens.map(_.last)
      } yield assertTrue(said.contains("Последний лесоруб Чернолома падает") && said.contains("а готовый лук. Отданный.")) &&
              assertTrue(said.contains("И Фаэлин смеётся") && said.contains("Она слушает не только песню")) &&
              assertTrue(said.contains("Фаэлин Тихострел теперь растёт вместе с вами до 18-го уровня") && said.contains("Получено") &&
                said.contains("📜 Квест «Отданный лук» завершён.")) &&
              assertTrue(q.isDone(NpcQuest.FaelinBow) && inv.snapshot.isEmpty) &&
              assertTrue(h.squad.allies.exists(a => a.kind == AllyKind.Elf && a.grown == AllyRates.QuestGrowth)) &&
              assertTrue(gone.text.contains("нет ни одной срезанной") && gone.choices.map(_.id) == List("FaelinOut"))
    },

    test("смерть у корней: проснуться в лабиринте, срубленный лук при герое — шаг тот же") {
      for {
        c    <- content
        d    <- dao(hero(), Some(toRoots))
        _    <- d.writeActiveBattle(userId, FaelinQuest.rootsBattle(hero(), FaelinQuest.foes, Nil).asJson)
        r    <- TestRenderer.make
        _    <- DeathState(d, TestInventoryRepository.withItems(List(bow)), c).enter(testUser, r)
        wake <- d.readSceneData(userId)
        q    <- questOf(d)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("faelinBow.roots.death")))) &&
              assertTrue(FaelinQuest.toRoots(q)) &&
              assertTrue(c.text("faelinBow.roots.death").contains("Срубленный лук при вас"))
    },

    test("журнал: что делать") {
      for {
        c <- content
        line = QuestJournal.render(c, hero(), LoreData.empty, toRoots, DailyQuests.empty, None, 0L)
      } yield assertTrue(line.contains("«Отданный лук» — вернуть срубленный лук Фаэлина изгороди, к старым корням"))
    }
  )
}
