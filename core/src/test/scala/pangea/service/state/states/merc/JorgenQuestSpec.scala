package pangea.service.state.states.merc

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, Element, SoloPveBattle}
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.MiniBoss
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.squad.{Ally, AllyKind, AllyRates, Squad}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.guild.MasterHornState
import pangea.service.state.states.tavern.MercenariesState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{JorgenQuest, MercQuests, NpcQuestLog, QuestJournal, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Жетоны из пепла» — личное задание Йоргена: разговор за столом до найма,
  * список отряда ведёт к логову огненных (их двое, Йорген бьёт холодом),
  * жетоны — Горну; выполнено — потолок Йоргена у героя растёт на восемь. */
object JorgenQuestSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))
  private def pick(key: String, kind: AllyKind): UserAction = tap(key, "kind" -> kind.entryName)

  private def hero(lvl: Long = 10L, dungeonLevel: Int = 7): Hero =
    TestFixtures.hero(userId, state = StateType.Tavern, dungeonLevel = dungeonLevel).copy(lvl = lvl, doubloons = 50L)

  private def roster: Item = QuestItemKind.item(QuestItemKind.JorgenRoster).copy(id = 801L)
  private def tokens: Item = QuestItemKind.item(QuestItemKind.JorgenTokens).copy(id = 802L)

  private def toLair: NpcQuests = NpcQuests.empty.take(NpcQuest.JorgenDebt, 0L)
  private def toHorn: NpcQuests = toLair.update(NpcQuest.JorgenDebt)(_.copy(step = 2))
  private def done: NpcQuests   = toHorn.finish(NpcQuest.JorgenDebt)

  private def dao(h: Hero, q: Option[NpcQuests] = None) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def tavern(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent, items: Boolean = true) =
    MercenariesState(d, inv, c, itemRepo = Option.when(items)(TestItemRepository.make))

  private def lair(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    JorgenLairState(d, inv, TestItemRepository.make, c)

  private def battleOf(d: TestHeroDao) =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  override def spec = suite("Жетоны из пепла")(

    test("у стола: «Поговорить» у Йоргена — их было двое, сложность сразу; без хранилища вещей разговора нет") {
      for {
        c     <- content
        d     <- dao(hero())
        r     <- TestRenderer.make
        st     = tavern(d, TestInventoryRepository.withItems(Nil), c)
        _     <- st.action(testUser, pick("MercCard", AllyKind.Human), r)
        card  <- r.sentScreens.map(_.last)
        _     <- st.action(testUser, pick("MercTalk", AllyKind.Human), r)
        talk  <- r.sentScreens.map(_.last)
        _     <- tavern(d, TestInventoryRepository.withItems(Nil), c, items = false).action(testUser, pick("MercCard", AllyKind.Human), r)
        bare  <- r.sentScreens.map(_.last)
      } yield assertTrue(card.choices.map(_.id) == List("MercTalk", "MercHire", "MercList")) &&
              assertTrue(talk.text.contains("Их было двое") && talk.text.contains(Difficulty.render(9))) &&
              assertTrue(talk.choices.map(_.id) == List("MercTalkYes", "MercCard")) &&
              assertTrue(!bare.choices.exists(_.id == "MercTalk"))
    },

    test("«Помочь»: список отряда в сумке, задание взято; Йорген говорит о логове, ответ — над карточкой, разговора больше нет") {
      for {
        c    <- content
        d    <- dao(hero())
        inv   = TestInventoryRepository.accepting
        r    <- TestRenderer.make
        _    <- tavern(d, inv, c).action(testUser, pick("MercTalkYes", AllyKind.Human), r)
        last <- r.sentScreens.map(_.last)
        q    <- questOf(d)
      } yield assertTrue(JorgenQuest.toLair(q) && inv.snapshot.exists(_.questItem.contains(QuestItemKind.JorgenRoster))) &&
              assertTrue(last.text.contains("долг мой, а не твой") && last.text.contains("«Список отряда Йоргена»")) &&
              assertTrue(last.text.contains("логово ждёт") && !last.text.contains("Йорген Кремень. Имя не моё")) &&
              assertTrue(!last.choices.exists(_.id == "MercTalk") && last.choices.exists(_.id == "MercHire"))
    },

    test("карточка списка: сложность и где логово; «Идти к логову» — только из лабиринта и с 7-го поворота") {
      def card(from: StateType, lvl: Int) =
        for {
          d     <- dao(hero(dungeonLevel = lvl), Some(toLair))
          _     <- d.writeReturnState(userId, Some(from))
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(List(roster)), TestItemRepository.make, c)
          _     <- state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${roster.id}"}""")), r)
          shown <- r.sentScreens.map(_.last)
          go    <- state.action(testUser, tap("JorgenLairGo"), r)
          said  <- texts(r)
        } yield (shown, go, said)
      for {
        deep    <- card(StateType.Dungeon, 7)
        city    <- card(StateType.GlobalMap, 7)
        shallow <- card(StateType.Dungeon, 6)
      } yield assertTrue(deep._1.text.contains(Difficulty.render(9)) && deep._1.text.contains("не ближе 7-го поворота")) &&
              assertTrue(deep._1.choices.exists(_.id == "JorgenLairGo") && deep._2 == StateType.JorgenLair) &&
              assertTrue(city._2 == StateType.Inventory && city._3.contains("идут из лабиринта")) &&
              assertTrue(shallow._2 == StateType.Inventory && shallow._3.contains("с 7-го поворота"))
    },

    test("логово: Йорген — «их двое» и меч холода; бой с обоими огненными, оба минибоссы, без бегства, Йорген гостем с холодом") {
      for {
        c     <- content
        d     <- dao(hero(), Some(toLair))
        r     <- TestRenderer.make
        st     = lair(d, TestInventoryRepository.withItems(Nil), c)
        _     <- st.enter(testUser, r)
        intro <- r.sentScreens.map(_.last)
        out   <- st.action(testUser, tap("JorgenFight"), r)
        b     <- battleOf(d)
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        h      = hero()
        lvl    = MiniBoss.FireElemental.bossLvl(
                   MiniBoss.scalingLvl(h.lvl.max(JorgenQuest.Level.toLong), h.dungeonLevel.toLong))
        guests = b.toList.flatMap(_.group.allies.filter(_.kind == AllyKind.Human))
      } yield assertTrue(intro.text.contains("их двое") && intro.text.contains("сегодня беру холод") &&
                intro.text.contains(Difficulty.render(9))) &&
              assertTrue(intro.choices.map(_.id) == List("JorgenFight", "OpenCharacter", "JorgenLeave")) &&
              assertTrue(out == StateType.Battle) &&
              assertTrue(b.exists(x => x.bossKind.contains(MiniBoss.FireElemental.entryName) && x.monsterLvl == lvl &&
                x.group.others.size == 1 && x.group.others.forall(s => s.boss.contains(MiniBoss.FireElemental.entryName) && s.lvl == lvl))) &&
              assertTrue(b.exists(x => x.story.contains(JorgenQuest.LairStory) && x.cantFlee && x.lootLevel.contains(9L))) &&
              // Йорген не нанят — он гость: места в отряде у него нет, бьёт холодом
              assertTrue(guests.size == 1 && guests.forall(g => g.home == 0 && g.strike.contains(Element.Cold) &&
                g.element.contains(Element.Cold))) &&
              assertTrue(loot.exists(l => l.returnState.contains(StateType.JorgenLair) &&
                l.eventData.flatMap(_.as[JorgenLairState.Progress].toOption).contains(JorgenLairState.Progress(won = true))))
    },

    test("нанятый Йорген идёт в логово сам — в своём строю, с мечом холода; гостя рядом нет") {
      val hired = hero().copy(squad = Squad().hire(AllyKind.Human, 10L, 0L))
      for {
        c <- content
        d <- dao(hired, Some(toLair))
        r <- TestRenderer.make
        _ <- lair(d, TestInventoryRepository.withItems(Nil), c).action(testUser, tap("JorgenFight"), r)
        b <- battleOf(d)
        jorgens = b.toList.flatMap(_.group.allies.filter(_.kind == AllyKind.Human))
      } yield assertTrue(jorgens.size == 1 && jorgens.forall(j => j.home > 0 && j.element.contains(Element.Cold)))
    },

    test("оба пали: в пепле жетоны — в сумку, дальше к Горну; потом в логове тихо") {
      for {
        c    <- content
        d    <- dao(hero(), Some(toLair))
        _    <- d.writeSceneData(userId, JorgenLairState.Progress(won = true).asJson)
        inv   = TestInventoryRepository.accepting
        r    <- TestRenderer.make
        st    = lair(d, inv, c)
        _    <- st.enter(testUser, r)
        said <- texts(r)
        q    <- questOf(d)
        _    <- st.enter(testUser, r)
        gone <- r.sentScreens.map(_.last)
      } yield assertTrue(said.contains("Жетоны находятся один за другим") && said.contains("«Жетоны из пепла»")) &&
              assertTrue(JorgenQuest.toHorn(q) && inv.snapshot.exists(_.questItem.contains(QuestItemKind.JorgenTokens))) &&
              assertTrue(gone.text.contains("Огня больше нет") && gone.choices.map(_.id) == List("JorgenOut"))
    },

    test("Горн: «Показать жетоны» — павшие в учёте, задание закрыто, список и жетоны ушли; Йорген при герое растёт сразу") {
      val hired = hero().copy(squad = Squad().hire(AllyKind.Human, 10L, 0L))
      for {
        c     <- content
        none  <- dao(hired, Some(toHorn))
        r0    <- TestRenderer.make
        _     <- MasterHornState(none, TestInventoryRepository.withItems(Nil), c).enter(testUser, r0)
        empty <- r0.sentScreens.map(_.last)
        d     <- dao(hired, Some(toHorn))
        inv    = TestInventoryRepository.withItems(List(roster, tokens))
        r     <- TestRenderer.make
        st     = MasterHornState(d, inv, c)
        _     <- st.enter(testUser, r)
        menu  <- r.sentScreens.map(_.last)
        _     <- st.action(testUser, tap("HornTokens"), r)
        said  <- texts(r)
        q     <- questOf(d)
        h     <- d.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(!empty.choices.exists(_.id == "HornTokens") && menu.choices.exists(_.id == "HornTokens")) &&
              assertTrue(said.contains("Гильдия платит по жетону") && said.contains("Получено") &&
                said.contains("Йорген Кремень теперь растёт вместе с вами до 13-го уровня") &&
                said.contains("📜 Квест «Жетоны из пепла» завершён.")) &&
              assertTrue(q.isDone(NpcQuest.JorgenDebt) && inv.snapshot.forall(_.questItem.isEmpty)) &&
              assertTrue(h.squad.allies.exists(a => a.kind == AllyKind.Human && a.grown == AllyRates.QuestGrowth))
    },

    test("долг закрыт: цена та же — другая реплика; нанятый Йорген растёт до 13-го, без задания — до 5-го") {
      for {
        c      <- content
        d      <- dao(hero(lvl = 20L), Some(done))
        r      <- TestRenderer.make
        st      = tavern(d, TestInventoryRepository.withItems(Nil), c)
        _      <- st.action(testUser, pick("MercCard", AllyKind.Human), r)
        card   <- r.sentScreens.map(_.last)
        _      <- st.action(testUser, pick("MercHire", AllyKind.Human), r)
        grown  <- d.getHeroByUserId(userId).map(_.get.squad.allies.find(_.kind == AllyKind.Human))
        d2     <- dao(hero(lvl = 20L))
        _      <- tavern(d2, TestInventoryRepository.withItems(Nil), c).action(testUser, pick("MercHire", AllyKind.Human), r)
        plain  <- d2.getHeroByUserId(userId).map(_.get.squad.allies.find(_.kind == AllyKind.Human))
      } yield assertTrue(card.text.contains("Цена та же, только дублоны теперь мои") && card.text.contains("10 дублонов")) &&
              assertTrue(!card.choices.exists(_.id == "MercTalk")) &&
              assertTrue(grown.exists(a => a.grown == 8L && a.lvlAt(20L) == 13L &&
                a.statsAt(20L) == AllyKind.Human.stats(13L, 8L) && a.statsAt(20L) != AllyKind.Human.stats(20L))) &&
              assertTrue(plain.exists(a => a.grown == 0L && a.lvlAt(20L) == 5L))
    },

    test("рост: потолок +8 — в отряде, в бою и в энергии; старая запись союзника — без роста") {
      val squad   = Squad().hire(AllyKind.Human, 20L, 0L)
      val reserve = squad.copy(allies = Nil, reserve = squad.allies)
      val inBattle = BattleAlly.of(squad.grow(AllyKind.Human, 8L).allies.head, 20L)
      val oldAlly = Json.obj("kind" -> "Human".asJson, "position" -> 2.asJson, "hp" -> 10.asJson,
        "armor" -> 10.asJson, "energy" -> 10.asJson).as[Ally].toOption
      assertTrue(AllyKind.Human.effectiveLvl(20L, 8L) == 13L && AllyKind.Human.effectiveLvl(20L) == 5L &&
                 AllyKind.Human.effectiveLvl(10L, 8L) == 10L) &&
      assertTrue(squad.hire(AllyKind.Murloc, 20L, 0L, grown = 8L).allies.find(_.kind == AllyKind.Murloc).exists(_.lvlAt(20L) == 18L)) &&
      assertTrue(reserve.grow(AllyKind.Human, 8L).reserve.forall(_.grown == 8L)) &&
      assertTrue(inBattle.grown == 8L && inBattle.lvl == 13L && inBattle.toAlly.grown == 8L) &&
      assertTrue(AllyKind.Human.energyRegen(inBattle.lvl, inBattle.grown) == AllyRates.EnergyRegenPerLvl * 13L) &&
      assertTrue(oldAlly.exists(_.grown == 0L)) &&
      assertTrue(MercQuests.grown(done, AllyKind.Human) == 8L && MercQuests.grown(toHorn, AllyKind.Human) == 0L &&
                 MercQuests.grown(done, AllyKind.Gnome) == 0L)
    },

    test("смерть в логове: проснуться в лабиринте, список при герое — задание на том же шаге") {
      for {
        c    <- content
        d    <- dao(hero(), Some(toLair))
        _    <- d.writeActiveBattle(userId, JorgenQuest.lairBattle(hero()).asJson)
        r    <- TestRenderer.make
        _    <- DeathState(d, TestInventoryRepository.withItems(List(roster)), c).enter(testUser, r)
        wake <- d.readSceneData(userId)
        q    <- questOf(d)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("jorgen.lair.death")))) &&
              assertTrue(JorgenQuest.toLair(q)) &&
              assertTrue(c.text("jorgen.lair.death").contains("список отряда при вас"))
    },

    test("журнал: что делать на каждом шаге") {
      for {
        c <- content
        lairLine = QuestJournal.render(c, hero(), LoreData.empty, toLair, DailyQuests.empty, None, 0L)
        hornLine = QuestJournal.render(c, hero(), LoreData.empty, toHorn, DailyQuests.empty, None, 0L)
      } yield assertTrue(lairLine.contains("«Жетоны из пепла» — найти логово огненных, где пал отряд Йоргена")) &&
              assertTrue(hornLine.contains("«Жетоны из пепла» — отнести жетоны павших Мастеру Горну в гильдию"))
    }
  )
}
