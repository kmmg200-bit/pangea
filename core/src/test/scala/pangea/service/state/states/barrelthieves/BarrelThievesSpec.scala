package pangea.service.state.states.barrelthieves

import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.barrel.{Barrel, BarrelThieves, StolenGoods}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Achievement, Hero, LoreData}
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Item, ItemSet, ItemType, QuestItemKind, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.states.{DeathState, InventoryState, LootState, UnassumingBarrelState}
import pangea.service.state.{BarrelThievesQuest, NpcQuestLog, QuestJournal, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

import java.time.Instant

/** «Вскрытая бочка»: жребий на кражу при открытии бочки (с 3-го уровня, 5%, не
  * чаще раза в сутки), вскрытая бочка с уликой, Ришелье, след на склад у
  * причала, бой с бандой, возврат в бочку с перешитыми в «Пройдоху» вещами —
  * и повторы с другой бандой и наградой поменьше. */
object BarrelThievesSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val day: Long = BarrelThievesQuest.RollEveryMs

  private def hero(lvl: Long = 10L, race: Race = Race.Human): Hero =
    TestFixtures.hero(userId).copy(lvl = lvl, race = race, silver = 0L)

  private def gear(id: Long, set: Option[ItemSet], kind: ItemType = ItemType.Weapon): Item =
    Item(id, s"Вещь $id", 5L, ItemRarity.Blue, kind, attack = 5, accuracy = 0, energy = 0, armor = 0,
      defence = 0, evasion = 0, set = set)

  private val stoneSword = gear(11L, Some(ItemSet.StoneGuard))
  private val plainHelm  = gear(12L, None, ItemType.Helmet)
  private val goblinBoot = gear(13L, Some(ItemSet.Trickster), ItemType.Boots)

  private def clueItem: Item = QuestItemKind.item(QuestItemKind.BarrelClue).copy(id = 800L)

  /** Бочка, которую уже вскрыли: на дне улика, у воров — вещи и серебро. */
  private def robbedBarrel(items: List[Item], silver: Long, withClue: Boolean = true): TestBarrelRepository =
    new TestBarrelRepository(if (withClue) List(clueItem) else Nil, 0L,
      Some(BarrelThieves(rolledAt = 1L, stolen = Some(StolenGoods(items, silver)))))

  private def quest(step: Int, gang: Int = 0): NpcQuests =
    BarrelThievesQuest.started(NpcQuests.empty, gang, 0L).update(NpcQuest.BarrelThieves)(_.copy(step = step))

  private def dao(h: Hero, q: Option[NpcQuests] = None) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def barrelScene(d: TestHeroDao, inv: TestInventoryRepository, b: TestBarrelRepository, c: SceneContent) =
    UnassumingBarrelState(d, inv, b, c, Some(TestItemRepository.make))

  /** Сутки с начала времён: первый жребий уже можно бросать. */
  private val afterDay = TestClock.setTime(Instant.ofEpochMilli(day + 1000L))

  override def spec = suite("Вскрытая бочка")(

    test("когда бросать жребий: с 3-го уровня, задание не идёт, бочка цела и в ней есть вещь, сутки прошли") {
      val full   = Barrel(1L, hero().id, Items(List(plainHelm)), 0L)
      val money  = Barrel(1L, hero().id, Items(Nil), 5000L)
      val onlyQ  = Barrel(1L, hero().id, Items(List(clueItem)), 0L)
      val recent = full.copy(thieves = Some(BarrelThieves(rolledAt = day)))
      val now    = day + 10L
      assertTrue(BarrelThievesQuest.canRoll(hero(), NpcQuests.empty, full, now)) &&
      assertTrue(!BarrelThievesQuest.canRoll(hero(2L), NpcQuests.empty, full, now)) &&
      assertTrue(BarrelThievesQuest.canRoll(hero(3L), NpcQuests.empty, full, now)) &&
      assertTrue(!BarrelThievesQuest.canRoll(hero(), quest(1), full, now)) &&
      // одно серебро без вещей и одна улика — не повод
      assertTrue(!BarrelThievesQuest.canRoll(hero(), NpcQuests.empty, money, now) &&
                 !BarrelThievesQuest.canRoll(hero(), NpcQuests.empty, onlyQ, now)) &&
      assertTrue(!BarrelThievesQuest.canRoll(hero(), NpcQuests.empty, recent, now) &&
                 BarrelThievesQuest.canRoll(hero(), NpcQuests.empty, recent, 2L * day)) &&
      // задание выполнено — можно снова
      assertTrue(BarrelThievesQuest.canRoll(hero(), quest(2).finish(NpcQuest.BarrelThieves), full, now)) &&
      // впервые — первая банда, дальше — любая, кроме первой
      assertTrue(BarrelThievesQuest.pickGang(first = true, 8, 5) == 0) &&
      assertTrue((0 to 30).map(BarrelThievesQuest.pickGang(first = false, 8, _)).forall(g => g >= 1 && g < 8)) &&
      assertTrue(BarrelThievesQuest.pickGang(first = false, 1, 5) == 0)
    },

    test("открыл бочку и выпало 5%: вещи и серебро унесли, на дне улика; сложность по уровню героя и подсказка про город") {
      for {
        _      <- afterDay
        d      <- dao(hero(lvl = 12L))
        b       = new TestBarrelRepository(List(stoneSword, plainHelm), 3000L)
        r      <- TestRenderer.make
        c      <- content
        _      <- TestRandom.feedInts(4, 0)
        _      <- barrelScene(d, TestInventoryRepository.withItems(Nil), b, c).enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
        q      <- questOf(d)
      } yield assertTrue(b.itemsSnapshot.map(_.questItem) == List(Some(QuestItemKind.BarrelClue)) && b.silverSnapshot == 0L) &&
              assertTrue(b.thievesSnapshot.flatMap(_.stolen).contains(StolenGoods(List(stoneSword, plainHelm), 3000L))) &&
              assertTrue(b.thievesSnapshot.exists(_.rolledAt == day + 1000L)) &&
              assertTrue(BarrelThievesQuest.askAround(q) && BarrelThievesQuest.gangIndex(q) == 0) &&
              assertTrue(screen.text.contains("Крышка сорвана") && screen.text.contains(Difficulty.render(12))) &&
              assertTrue(screen.text.contains("не спрашивает, с кого оно снято") && !screen.text.contains("Торговой площади")) &&
              assertTrue(screen.choices.map(_.id).contains("BarrelTakeClue") && !screen.choices.exists(_.id == "DepositItemsMenu"))
    },

    test("не выпало — бочка как была; жребий снова — только через сутки") {
      for {
        _      <- afterDay
        d      <- dao(hero())
        b       = TestBarrelRepository.withItems(List(plainHelm))
        r      <- TestRenderer.make
        c      <- content
        st      = barrelScene(d, TestInventoryRepository.withItems(Nil), b, c)
        // 50 — мимо; второе открытие в тот же день жребия не тянет — 4 останется в очереди
        _      <- TestRandom.feedInts(50, 4, 0)
        _      <- st.enter(testUser, r)
        _      <- st.enter(testUser, r)
        same   <- r.sentScreens.map(_.last)
        _      <- TestClock.setTime(Instant.ofEpochMilli(2L * day + 2000L))
        _      <- st.enter(testUser, r)
        q      <- questOf(d)
      } yield assertTrue(same.choices.exists(_.id == "DepositItemsMenu")) &&
              // назавтра жребий взял ту самую четвёрку — и бочку вскрыли
              assertTrue(BarrelThievesQuest.active(q) && b.itemsSnapshot.forall(_.isQuestItem))
    },

    test("вскрытая бочка: положить нельзя ни вещь, ни серебро; улику — в сумку") {
      for {
        d     <- dao(hero(), Some(quest(1)))
        b      = robbedBarrel(List(plainHelm), 100L)
        inv    = TestInventoryRepository.withItems(List(gear(20L, None)))
        r     <- TestRenderer.make
        c     <- content
        st     = barrelScene(d, inv, b, c)
        _     <- st.action(testUser, tap("DepositItemsMenu"), r)
        _     <- st.action(testUser, UserAction("", Some(s"""{"action":"${UnassumingBarrelState.DepositItemPrefix}20"}""")), r)
        _     <- st.action(testUser, tap("DepositSilverMenu"), r)
        guard <- r.sentScreens.map(_.last)
        _     <- st.action(testUser, tap("BarrelTakeClue"), r)
        said  <- texts(r)
        after <- r.sentScreens.map(_.last)
      } yield assertTrue(guard.text.contains("Крышка так и висит") && !guard.choices.exists(_.id == "DepositItemsMenu")) &&
              assertTrue(inv.snapshot.exists(_.id == 20L)) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.BarrelClue)) && b.itemsSnapshot.isEmpty) &&
              assertTrue(said.contains("«Засаленный чертёж»") && !after.choices.exists(_.id == "BarrelTakeClue"))
    },

    test("Ришелье: с уликой в сумке — называет банду и склад у причала, задание «по следу»; без улики — кнопки нет") {
      for {
        c     <- content
        d1    <- dao(hero(), Some(quest(1)))
        r     <- TestRenderer.make
        none  <- MerchantState(d1, TestInventoryRepository.withItems(Nil), TestItemRepository.make, c).enter(testUser, r) *>
                   r.sentScreens.map(_.last)
        d2    <- dao(hero(), Some(quest(1)))
        st     = MerchantState(d2, TestInventoryRepository.withItems(List(clueItem)), TestItemRepository.make, c)
        _     <- st.enter(testUser, r)
        menu  <- r.sentScreens.map(_.last)
        _     <- st.action(testUser, tap("RichClue"), r)
        said  <- texts(r)
        q     <- questOf(d2)
      } yield assertTrue(!none.choices.exists(_.id == "RichClue") && menu.choices.exists(_.id == "RichClue")) &&
              assertTrue(said.contains("«Кривые шила»") && said.contains("старом складе у причала")) &&
              assertTrue(said.contains("перешивают под себя") && BarrelThievesQuest.onTrail(q))
    },

    test("карточка улики: сложность по уровню героя; после Ришелье — «Идти по следу», только из города") {
      for {
        c     <- content
        ask    = InventoryState.cardText(c, clueItem, hero(lvl = 23L), quest(1))
        d     <- dao(hero(), Some(quest(2, gang = 3)))
        inv    = TestInventoryRepository.withItems(List(clueItem))
        st     = InventoryState(d, inv, TestItemRepository.make, c)
        r     <- TestRenderer.make
        _     <- d.writeReturnState(userId, Some(StateType.Dungeon))
        _     <- st.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}800"}""")), r)
        card  <- r.sentScreens.map(_.last)
        away  <- st.action(testUser, tap("BarrelTrail"), r)
        said  <- texts(r)
        _     <- d.writeReturnState(userId, Some(StateType.HarborQuarter))
        went  <- st.action(testUser, tap("BarrelTrail"), r)
      } yield assertTrue(ask.contains(Difficulty.render(23)) && ask.contains("Уж он-то узнает, чья это работа")) &&
              assertTrue(card.choices.exists(_.id == "BarrelTrail") && card.text.contains("«Липкие пальцы»")) &&
              assertTrue(away == StateType.Inventory && said.contains("только из города") && went == StateType.BarrelThieves)
    },

    test("склад у причала: банда и сложность, «Напасть» — четверо гоблинов-рабов уровня героя, с отрядом; гоблину — свои слова") {
      val merc = hero().squad.hire(AllyKind.Human, 1L, 0L)
      for {
        c      <- content
        d      <- dao(hero(lvl = 17L).copy(squad = merc), Some(quest(2)))
        r      <- TestRenderer.make
        st      = BarrelThievesState(d, c)
        _      <- st.enter(testUser, r)
        lair   <- r.sentScreens.map(_.last)
        out    <- st.action(testUser, tap("ThievesFight"), r)
        b      <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot   <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        dk     <- dao(hero(race = Race.Goblin), Some(quest(2)))
        _      <- BarrelThievesState(dk, c).enter(testUser, r)
        kin    <- r.sentScreens.map(_.last)
      } yield assertTrue(lair.text.contains("«Кривые шила»") && lair.text.contains(Difficulty.render(17))) &&
              assertTrue(lair.choices.map(_.id) == List("ThievesFight", "ThievesLeave")) &&
              assertTrue(out == StateType.Battle) &&
              assertTrue(b.exists(x => x.story.contains(BarrelThievesQuest.FightStory) && x.cantFlee)) &&
              assertTrue(b.exists(x => 1 + x.group.others.size == 4 && x.monsterLvl == 17L &&
                x.monsterRace == Race.Goblin.entryName && x.monsterRarity == Rarity.Common.entryName &&
                x.group.others.forall(s => s.lvl == 17L && s.rarity == Rarity.Common.entryName))) &&
              assertTrue(b.exists(_.group.allies.exists(_.kind == AllyKind.Human))) &&
              assertTrue(loot.exists(l => l.returnState.contains(StateType.BarrelReturn) &&
                l.eventData.flatMap(_.as[BarrelThievesState.Won].toOption).exists(_.won))) &&
              assertTrue(kin.text.contains("Свой у своих не берёт") && !lair.text.contains("Свой у своих"))
    },

    test("настоящий бой с бандой: добыча как за обычный бой, «Продолжить» — к возврату украденного") {
      val weak = Monster(0L, 10L, Race.Goblin, Rarity.Common,
        FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      val strong = hero().copy(fightStats = FightStats(atk = 500, hp = 2000, armor = 0, defence = 0,
        evasion = 9999, accuracy = 9999, energy = 0))
      for {
        d    <- dao(strong, Some(quest(2)))
        _    <- d.writeActiveBattle(userId, SoloPveBattle.fromGroup(List(weak), strong, Nil, squad = false)
                  .copy(story = Some(BarrelThievesQuest.FightStory)).asJson)
        _    <- d.writeSceneData(userId, LootData(Nil, Nil, returnState = Some(StateType.BarrelReturn),
                  eventData = Some(BarrelThievesState.WonMark.asJson)).asJson)
        inv   = TestInventoryRepository.accepting
        r    <- TestRenderer.make
        c    <- content
        won  <- BattleState(d, inv, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
        h    <- d.getHeroByUserId(userId).map(_.get)
        j    <- TestJournal.make
        back <- LootState(d, inv, TestItemRepository.make, j, c).action(testUser, tap("Continue"), r)
        mark <- d.readSceneData(userId).map(_.flatMap(_.as[BarrelThievesState.Won].toOption))
      } yield assertTrue(won == StateType.Loot && h.exp > 0L && back == StateType.BarrelReturn && mark.exists(_.won)) &&
              assertTrue(BattleState.RaidStories.contains(BarrelThievesQuest.FightStory))
    },

    test("возврат: всё снова в бочке, вещи наборов — «Пройдоха», прочие как были; улика уходит; 4000 и «Своё не отдам»; дальше — сама бочка") {
      for {
        d     <- dao(hero(), Some(quest(2)))
        _     <- d.writeSceneData(userId, BarrelThievesState.WonMark.asJson)
        b      = robbedBarrel(List(stoneSword, plainHelm, goblinBoot), 2500L, withClue = false)
        inv    = TestInventoryRepository.withItems(List(clueItem))
        r     <- TestRenderer.make
        c     <- content
        st     = BarrelReturnState(d, inv, b, c)
        _     <- st.enter(testUser, r)
        said  <- texts(r)
        q     <- questOf(d)
        h     <- d.getHeroByUserId(userId).map(_.get)
        back   = b.itemsSnapshot
      } yield assertTrue(st.autoAdvance.contains(StateType.UnassumingBarrel)) &&
              assertTrue(back.map(_.id) == List(11L, 12L, 13L) && b.silverSnapshot == 2500L && !b.thievesSnapshot.exists(_.stolen.isDefined)) &&
              // меч из «Каменного стража» перешит в «Пройдоху» — с теми же характеристиками
              assertTrue(back.find(_.id == 11L).exists(i => i.set.contains(ItemSet.Trickster) && i.name.contains("Пройдохи") &&
                i.attack == stoneSword.attack && i.lvl == stoneSword.lvl && i.rarity == stoneSword.rarity)) &&
              assertTrue(back.find(_.id == 12L).contains(plainHelm) && back.find(_.id == 13L).contains(goblinBoot)) &&
              assertTrue(!inv.snapshot.exists(_.questItem.contains(QuestItemKind.BarrelClue))) &&
              assertTrue(q.isDone(NpcQuest.BarrelThieves) && h.silver == BarrelThievesQuest.FirstSilver) &&
              assertTrue(h.hasAchievement(Achievement.OwnBack) && Achievement.agiBonus(h) == 1L) &&
              assertTrue(said.contains("набора «Пройдоха»") && said.contains("🪙 2500") && said.contains("+4000")) &&
              assertTrue(said.contains("«Своё не отдам»") && said.contains("📜 Квест «Вскрытая бочка» завершён."))
    },

    test("повтор: после возврата бочку вскрывают снова — другая банда, за возврат 500 и без второго достижения") {
      val veteran = hero().withAchievement(Achievement.OwnBack)
      for {
        _     <- afterDay
        d     <- dao(veteran, Some(quest(2).finish(NpcQuest.BarrelThieves)))
        b      = TestBarrelRepository.withItems(List(plainHelm))
        r     <- TestRenderer.make
        c     <- content
        _     <- TestRandom.feedInts(1, 5)
        _     <- barrelScene(d, TestInventoryRepository.withItems(Nil), b, c).enter(testUser, r)
        q1    <- questOf(d)
        _     <- NpcQuestLog.save(d, userId, BarrelThievesQuest.told(q1))
        _     <- d.writeSceneData(userId, BarrelThievesState.WonMark.asJson)
        _     <- BarrelReturnState(d, TestInventoryRepository.withItems(Nil), b, c).enter(testUser, r)
        said  <- texts(r)
        h     <- d.getHeroByUserId(userId).map(_.get)
        q2    <- questOf(d)
      } yield assertTrue(BarrelThievesQuest.gangIndex(q1) >= 1 && BarrelThievesQuest.askAround(q1)) &&
              assertTrue(h.silver == BarrelThievesQuest.RepeatSilver && h.achievements.count(_ == Achievement.OwnBack.entryName) == 1) &&
              assertTrue(q2.isDone(NpcQuest.BarrelThieves) && b.itemsSnapshot == List(plainHelm)) &&
              assertTrue(said.contains("+500") && !said.contains("🏆"))
    },

    test("смерть на складе: проснуться в порту, по следу — снова; без отметки о победе возврата нет") {
      for {
        c    <- content
        d    <- dao(hero(), Some(quest(2)))
        _    <- d.writeActiveBattle(userId, SoloPveBattle.fromGroup(BarrelThievesQuest.gang(10L), hero(), Nil)
                  .copy(story = Some(BarrelThievesQuest.FightStory)).asJson)
        r    <- TestRenderer.make
        _    <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        wake <- d.readSceneData(userId)
        q    <- questOf(d)
        d2   <- dao(hero(), Some(quest(2)))
        b     = robbedBarrel(List(plainHelm), 0L)
        _    <- BarrelReturnState(d2, TestInventoryRepository.withItems(Nil), b, c).enter(testUser, r)
        q2   <- questOf(d2)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("barrelThieves.death")))) &&
              assertTrue(wake.exists(_.hcursor.get[StateType]("wakeTo").toOption.contains(StateType.HarborQuarter))) &&
              assertTrue(BarrelThievesQuest.onTrail(q) && BarrelThievesQuest.onTrail(q2) && b.thievesSnapshot.exists(_.stolen.isDefined))
    },

    test("журнал: что делать на каждом шаге; старая бочка без воров и пустая запись воров читаются") {
      for {
        c <- content
        ask   = QuestJournal.render(c, hero(), LoreData.empty, quest(1), DailyQuests.empty, None, 0L)
        trail = QuestJournal.render(c, hero(), LoreData.empty, quest(2), DailyQuests.empty, None, 0L)
        old   = parse("{}").toOption.flatMap(_.as[BarrelThieves].toOption)
        full  = BarrelThieves(5L, Some(StolenGoods(List(plainHelm), 10L)))
      } yield assertTrue(ask.contains("«Вскрытая бочка» — узнать, кто в Кинэте мог сбыть краденое")) &&
              assertTrue(trail.contains("идти по следу банды на старый склад у причала")) &&
              assertTrue(old.contains(BarrelThieves()) && full.asJson.as[BarrelThieves].toOption.contains(full)) &&
              assertTrue(c.list(BarrelThievesQuest.GangsKey).headOption.contains("Кривые шила"))
    }
  )
}
