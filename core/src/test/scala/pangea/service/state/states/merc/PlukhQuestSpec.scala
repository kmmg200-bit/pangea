package pangea.service.state.states.merc

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.skill.MonsterSkill
import pangea.model.squad.{Ally, AllyKind, AllyRates, Squad}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.gustavo.GustavoState
import pangea.service.state.states.tavern.MercenariesState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, PlukhQuest, QuestJournal, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Чужой багор» — личное задание Плюха: разговор за столом до найма, печать
  * Сноходцев — к Густаво (он когда-то освободил мурлоков из их плена и ушёл с
  * ними), зов перевёрнут — ловчие-каджиты выходят на героя в лабиринте: глава
  * именной, четверо мифических, у всех кинжалы молнии (100 урона и −100
  * энергии тому, в кого попал, — герою и союзнику). Отбились — печать
  * тускнеет, Плюх берёт кинжалы себе и растёт на восемь. */
object PlukhQuestSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))
  private def pick(key: String, kind: AllyKind): UserAction = tap(key, "kind" -> kind.entryName)

  private def hero(lvl: Long = 15L): Hero =
    TestFixtures.hero(userId, state = StateType.Tavern, dungeonLevel = 5).copy(lvl = lvl)

  private def seal: Item = QuestItemKind.item(QuestItemKind.SnowwalkerSeal).copy(id = 831L)

  private def toGustavo: NpcQuests = NpcQuests.empty.take(NpcQuest.PlukhGaff, 0L)
  private def toHunt: NpcQuests    = PlukhQuest.turned(toGustavo)

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

  private def hunt(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) = PlukhHuntState(d, inv, c)

  private def battleOf(d: TestHeroDao) =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  /** Ловчий-каджит в паре: энергии хватает только на кинжал (базовые приёмы на
    * 14-м уровне дороже), и бой — тот самый, с ловчими. */
  private def dagger(h: Hero): SoloPveBattle = {
    val mob = MonsterGenerator.generateOfRaceAndRarity(PlukhQuest.Level, Race.Khajiit, Rarity.Mythical)
    SoloPveBattle.from(mob, h).copy(monsterCurrentEnergy = 50L, story = Some(PlukhQuest.HuntStory), noKin = true)
  }

  /** Ход героя «Ждать» — моб ходит своим ходом. */
  private def waitTurn(h: Hero, battle: SoloPveBattle) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- d.writeActiveBattle(userId, battle.asJson)
      r <- TestRenderer.make
      c <- content
      _ <- TestRandom.setSeed(7L)
      _ <- BattleState(d, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Wait"), r)
      t <- texts(r)
    } yield t

  private def fighter(squad: Squad = Squad()): Hero =
    TestFixtures.hero(userId, dungeonLevel = 14).copy(lvl = 14L, squad = squad,
      fightStats = FightStats(atk = 10, hp = 1000000L, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 500))

  override def spec = suite("Чужой багор")(

    test("у стола: «Поговорить» у Плюха — багор не одолженный, сложность сразу") {
      for {
        c    <- content
        d    <- dao(hero())
        r    <- TestRenderer.make
        st    = tavern(d, TestInventoryRepository.withItems(Nil), c)
        _    <- st.action(testUser, pick("MercCard", AllyKind.Murloc), r)
        card <- r.sentScreens.map(_.last)
        _    <- st.action(testUser, pick("MercTalk", AllyKind.Murloc), r)
        talk <- r.sentScreens.map(_.last)
      } yield assertTrue(card.choices.map(_.id) == List("MercTalk", "MercHire", "MercList")) &&
              assertTrue(talk.text.contains("Плюх не одалживал") && talk.text.contains(Difficulty.render(14))) &&
              assertTrue(talk.choices.map(_.id) == List("MercTalkYes", "MercCard"))
    },

    test("«Помочь»: печать в сумке, задание взято; Плюх говорит о ней") {
      for {
        c    <- content
        d    <- dao(hero())
        inv   = TestInventoryRepository.accepting
        r    <- TestRenderer.make
        _    <- tavern(d, inv, c).action(testUser, pick("MercTalkYes", AllyKind.Murloc), r)
        last <- r.sentScreens.map(_.last)
        q    <- questOf(d)
      } yield assertTrue(PlukhQuest.toGustavo(q) && inv.snapshot.exists(_.questItem.contains(QuestItemKind.SnowwalkerSeal))) &&
              assertTrue(last.text.contains("Ты сходи.") && last.text.contains("«Печать Сноходцев»")) &&
              assertTrue(last.text.contains("Печать у тебя? Неси алхимику") && !last.choices.exists(_.id == "MercTalk"))
    },

    test("Густаво: «Показать печать» — только с печатью; он освободил мурлоков и ушёл с ними, зов перевёрнут") {
      for {
        c     <- content
        none  <- dao(hero(), Some(toGustavo))
        r0    <- TestRenderer.make
        _     <- GustavoState(none, TestInventoryRepository.withItems(Nil), c).enter(testUser, r0)
        bare  <- r0.sentScreens.map(_.last)
        d     <- dao(hero(), Some(toGustavo))
        r     <- TestRenderer.make
        st     = GustavoState(d, TestInventoryRepository.withItems(List(seal)), c)
        _     <- st.enter(testUser, r)
        menu  <- r.sentScreens.map(_.last)
        _     <- st.action(testUser, tap("GusSeal"), r)
        said  <- texts(r)
        q     <- questOf(d)
      } yield assertTrue(!bare.choices.exists(_.id == "GusSeal") && menu.choices.exists(_.id == "GusSeal")) &&
              assertTrue(said.contains("выпустил всех из плена и ушёл вместе с ними") && said.contains("перевернуть")) &&
              assertTrue(PlukhQuest.toHunt(q))
    },

    test("карточка печати: что делать на шаге; «Позвать ловчих» — после Густаво и только из лабиринта") {
      def card(q: NpcQuests, from: StateType) =
        for {
          d     <- dao(hero(), Some(q))
          _     <- d.writeReturnState(userId, Some(from))
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(List(seal)), TestItemRepository.make, c)
          _     <- state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${seal.id}"}""")), r)
          shown <- r.sentScreens.map(_.last)
          go    <- state.action(testUser, tap("PlukhHuntGo"), r)
          said  <- texts(r)
        } yield (shown, go, said)
      for {
        first <- card(toGustavo, StateType.Dungeon)
        ready <- card(toHunt, StateType.Dungeon)
        city  <- card(toHunt, StateType.GlobalMap)
      } yield assertTrue(first._1.text.contains("Показать печать Густаво") && !first._1.choices.exists(_.id == "PlukhHuntGo")) &&
              assertTrue(first._1.text.contains(Difficulty.render(14)) && first._2 != StateType.PlukhHunt) &&
              assertTrue(ready._1.text.contains("Густаво перевернул зов") && ready._1.choices.exists(_.id == "PlukhHuntGo")) &&
              assertTrue(ready._2 == StateType.PlukhHunt) &&
              assertTrue(city._2 == StateType.Inventory && city._3.contains("Ловчих зовут из лабиринта"))
    },

    test("ловчие: каджиты — глава именной, четверо мифических; Плюх рад, но гильдия сильнее; бой без бегства, Плюх гостем") {
      for {
        c     <- content
        d     <- dao(hero(), Some(toHunt))
        r     <- TestRenderer.make
        st     = hunt(d, TestInventoryRepository.withItems(List(seal)), c)
        _     <- st.enter(testUser, r)
        intro <- r.sentScreens.map(_.last)
        out   <- st.action(testUser, tap("PlukhFight"), r)
        b     <- battleOf(d)
        foes   = b.toList.flatMap(x => (x.monsterRace, x.monsterRarity, x.customName, x.monsterLvl) ::
                   x.group.others.map(s => (s.race, s.rarity, s.customName, s.lvl)))
        plukh  = b.toList.flatMap(_.group.allies.filter(_.kind == AllyKind.Murloc))
      } yield assertTrue(intro.text.contains("каджиты в серых плащах") && intro.text.contains("Плюх рад") &&
                intro.text.contains("Сноходцы куда сильнее своих ловчих") && intro.text.contains(Difficulty.render(14))) &&
              assertTrue(intro.choices.map(_.id) == List("PlukhFight", "OpenCharacter", "PlukhLeave")) &&
              assertTrue(out == StateType.Battle && foes.size == 5 && foes.forall(f => f._1 == Race.Khajiit.entryName && f._4 == 14L)) &&
              assertTrue(foes.count(_._2 == Rarity.Mythical.entryName) == 4) &&
              assertTrue(foes.count(f => f._2 == Rarity.Legendary.entryName && f._3.contains(PlukhQuest.LeaderName)) == 1) &&
              assertTrue(b.exists(x => x.story.contains(PlukhQuest.HuntStory) && x.cantFlee && x.lootLevel.contains(14L))) &&
              assertTrue(plukh.size == 1 && plukh.forall(_.home == 0))
    },

    test("ловчие пали: печать тускнеет, Плюх берёт кинжалы себе, задание закрыто, при герое он растёт до 18-го; потом тихо") {
      val hired = hero().copy(squad = Squad().hire(AllyKind.Murloc, 15L, 0L))
      for {
        c    <- content
        d    <- dao(hired, Some(toHunt))
        _    <- d.writeSceneData(userId, PlukhHuntState.Progress(won = true).asJson)
        inv   = TestInventoryRepository.withItems(List(seal))
        r    <- TestRenderer.make
        st    = hunt(d, inv, c)
        _    <- st.enter(testUser, r)
        said <- texts(r)
        q    <- questOf(d)
        h    <- d.getHeroByUserId(userId).map(_.get)
        _    <- st.enter(testUser, r)
        gone <- r.sentScreens.map(_.last)
      } yield assertTrue(said.contains("печать в вашей руке тускнеет") && said.contains("Плюх их сбережёт")) &&
              assertTrue(said.contains("Плюх теперь растёт вместе с вами до 18-го уровня") && said.contains("Получено") &&
                said.contains("📜 Квест «Чужой багор» завершён.")) &&
              assertTrue(q.isDone(NpcQuest.PlukhGaff) && inv.snapshot.isEmpty) &&
              assertTrue(h.squad.allies.exists(a => a.kind == AllyKind.Murloc && a.grown == AllyRates.QuestGrowth)) &&
              assertTrue(gone.text.contains("Печать погасла") && gone.choices.map(_.id) == List("PlukhOut"))
    },

    test("кинжал молнии: 20 энергии на любом уровне, только у каджитов и только в бою с ловчими; по герою — 100 урона и −100 энергии") {
      val knife   = MonsterSkill.LightningDagger
      val h       = fighter().copy(fightStats = fighter().fightStats.copy(armor = 1000L, energy = 60L))
      val battle  = dagger(h)
      val cast    = knife.cast(battle, h, 0L)
      assertTrue(knife.cost(1L) == 20L && knife.cost(150L) == 20L) &&
      assertTrue(knife.availableTo(Race.Khajiit) && !knife.availableTo(Race.Human)) &&
      assertTrue(knife.applicable(battle) && !knife.applicable(battle.copy(story = None))) &&
      // броня принимает удар первой; энергии было 60 — сгорело всё
      assertTrue(cast.heroArmor == 900L && cast.heroHp == h.fightStats.hp && cast.heroEnergy.contains(0L)) &&
      assertTrue(cast.line.contains("«Кинжал молнии» — 100 урона, −60 энергии ⚡")) &&
      assertTrue(knife.againstOther.exists(o => o.fixed.contains(100L) && o.energyBurn == 100L))
    },

    test("кинжал молнии в бою: ловчий в паре — по герою; перед ловчим союзник — по союзнику, и у него сгорает энергия") {
      val front = Squad(heroPos = 2, allies = List(Ally(AllyKind.Human, 1, 100000L, 100000L, 500L)))
      for {
        onHero <- waitTurn(fighter(), dagger(fighter()))
        onAlly <- waitTurn(fighter(front), dagger(fighter(front)))
      } yield assertTrue(onHero.contains("«Кинжал молнии» — 100 урона, −100 энергии ⚡")) &&
              assertTrue(onAlly.contains("«Кинжал молнии» → Йорген Кремень") && onAlly.contains("−100 энергии ⚡"))
    },

    test("смерть в бою с ловчими: проснуться в лабиринте, печать при герое — шаг тот же") {
      for {
        c    <- content
        d    <- dao(hero(), Some(toHunt))
        _    <- d.writeActiveBattle(userId, PlukhQuest.huntBattle(hero(), PlukhQuest.hunters, Nil).asJson)
        r    <- TestRenderer.make
        _    <- DeathState(d, TestInventoryRepository.withItems(List(seal)), c).enter(testUser, r)
        wake <- d.readSceneData(userId)
        q    <- questOf(d)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("plukhGaff.hunt.death")))) &&
              assertTrue(PlukhQuest.toHunt(q)) &&
              assertTrue(c.text("plukhGaff.hunt.death").contains("Печать при вас"))
    },

    test("журнал: что делать на каждом шаге") {
      for {
        c <- content
        gusLine  = QuestJournal.render(c, hero(), LoreData.empty, toGustavo, DailyQuests.empty, None, 0L)
        huntLine = QuestJournal.render(c, hero(), LoreData.empty, toHunt, DailyQuests.empty, None, 0L)
      } yield assertTrue(gusLine.contains("«Чужой багор» — показать печать Сноходцев Густаво")) &&
              assertTrue(huntLine.contains("«Чужой багор» — позвать ловчих Сноходцев с карточки печати"))
    }
  )
}
