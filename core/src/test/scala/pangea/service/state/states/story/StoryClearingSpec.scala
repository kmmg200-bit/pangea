package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryNewcomerQuest.Fate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.home.HomeState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{HedgeSongQuest, NpcQuestLog, QuestJournal, StoryClearingQuest, StoryLine, StoryPowderQuest,
  StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Акт II, глава 8 «Просека»: гильдия Чернолом рубит в изгороди прямую дорогу,
  * где топор вязнет — рвёт корни порохом, Тихие поют изгороди закрыться. Тень
  * пришла «объяснить договор» и может отдать бригаде долг Братства сама; Щит
  * сперва показывает бирку Юноне, а на просеку идёт с Горном; одиночка входит
  * в лагерь с ящиком шнапса Барти, и рядом встаёт Элиар. Бой — вся бригада, на
  * поле бочки с порохом. У десятника — накладная на лес в Загон, глава 9
  * висит до обновления. */
object StoryClearingSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, state = StateType.Dungeon).copy(lvl = 36L, silver = 100L)

  /** Акт I пройден (путь `path`), главы 6 и 7 закрыты, «Просека» на шаге `n`. */
  private def clearing(path: Path, n: Int): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    val face = StoryPowderQuest.finish(base, WardenFate.Killed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = SojkaFate.Broke.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
    val done = StoryToolQuest.finish(face.take(NpcQuest.StoryTool, 0L), path)
      .take(NpcQuest.StoryMorning, 0L).finish(NpcQuest.StoryMorning)
      .take(NpcQuest.StoryNewcomer, 0L).finish(NpcQuest.StoryNewcomer)
      .update(NpcQuest.StoryNewcomer)(_.copy(counter = Fate.Hedge.n))
      .take(NpcQuest.StoryClearing, 0L)
    StoryClearingQuest.step(done, n)
  }

  /** У каждого пути — свой шаг просеки: у Щита второй, у прочих первый. */
  private def atClearing(path: Path): NpcQuests = clearing(path, if (path == Path.Shield) 2 else 1)

  private def dao(q: NpcQuests, h: Hero = hero) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- d.writeLoreData(userId, LoreData(firstSteps = true, firstStepsDone = true).asJson)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private val pass: Item = QuestItemKind.item(QuestItemKind.StoryPass).copy(id = 982L)

  private def scene(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    StoryClearingState(d, inv, TestItemRepository.make, c)

  private def cardOf(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    InventoryState(d, inv, TestItemRepository.make, c)

  /** Просека и бой: что на экране, куда ушёл ход, бой и роутинг добычи. */
  private def fightOn(path: Path) =
    for {
      c     <- content
      d     <- dao(atClearing(path))
      r     <- TestRenderer.make
      st     = scene(d, TestInventoryRepository.withItems(List(pass)), c)
      _     <- st.enter(testUser, r)
      shown <- r.sentScreens.map(_.last)
      res   <- st.action(testUser, tap("ClearingFight"), r)
      start <- r.sentScreens.map(_.last.text)
      b     <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
    } yield (shown, res, start, b, loot)

  /** Вся чужая сторона боя: тот, кто в полях, строй и очередь. */
  private def brigade(b: SoloPveBattle): List[(String, Option[String])] =
    (b.monsterRarity, b.customName) :: (b.group.others ++ b.group.queue).map(m => (m.rarity, m.customName))

  override def spec = suite("Акт II, глава 8 «Просека»")(

    test("карточка пропуска: сложность и дело своего пути; из лабиринта — на просеку, из города — нельзя; Щиту — сперва к Юноне") {
      def card(path: Path, n: Int, from: StateType) =
        for {
          c     <- content
          d     <- dao(clearing(path, n))
          _     <- d.writeReturnState(userId, Some(from))
          r     <- TestRenderer.make
          inv    = TestInventoryRepository.withItems(List(pass))
          st     = cardOf(d, inv, c)
          _     <- st.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${pass.id}"}""")), r)
          shown <- r.sentScreens.map(_.last)
          go    <- st.action(testUser, tap("StoryClearingGo"), r)
          said  <- texts(r)
        } yield (shown, go, said)
      for {
        shadow  <- card(Path.Shadow, 1, StateType.Dungeon)
        city    <- card(Path.Shadow, 1, StateType.GlobalMap)
        shield1 <- card(Path.Shield, 1, StateType.Dungeon)
        shield2 <- card(Path.Shield, 2, StateType.Dungeon)
        alone   <- card(Path.Alone, 1, StateType.Dungeon)
      } yield assertTrue(shadow._1.text.contains(Difficulty.render(StoryClearingQuest.Level)) &&
                shadow._1.text.contains("«Объясни им договор»") && shadow._1.choices.exists(_.id == "StoryClearingGo") &&
                shadow._2 == StateType.StoryClearing) &&
              assertTrue(city._2 != StateType.StoryClearing && city._3.contains("туда идут из лабиринта")) &&
              assertTrue(shield1._1.text.contains("Юнона должна знать") && !shield1._1.choices.exists(_.id == "StoryClearingGo")) &&
              assertTrue(shield2._1.text.contains("Горн ждёт у изгороди") && shield2._2 == StateType.StoryClearing) &&
              assertTrue(alone._1.text.contains("шнапс ящиками") && alone._2 == StateType.StoryClearing)
    },

    test("Щит: Юнона узнаёт на бирке свою печать и пишет встречный приказ — дальше на просеку с Горном") {
      for {
        c      <- content
        d      <- dao(clearing(Path.Shield, 1))
        inv     = TestInventoryRepository.withItems(List(pass))
        sch    <- TestScheduler.make
        home    = HomeState(d, sch, c, inv, TestItemRepository.make)
        r      <- TestRenderer.make
        _      <- home.action(testUser, tap("HomeOffice"), r)
        office <- r.sentScreens.map(_.last)
        _      <- home.action(testUser, tap("HomeJunonaClearing"), r)
        said   <- texts(r)
        q      <- questOf(d)
        // Чужому пути кнопки у Юноны нет.
        d2     <- dao(clearing(Path.Alone, 1))
        r2     <- TestRenderer.make
        _      <- HomeState(d2, sch, c, inv, TestItemRepository.make).action(testUser, tap("HomeOffice"), r2)
        other  <- r2.sentScreens.map(_.last)
      } yield assertTrue(office.choices.exists(_.id == "HomeJunonaClearing")) &&
              assertTrue(said.contains("«Подпись не моя, — говорит она. — Печать — моя»") &&
                said.contains("Рубку в изгороди прекратить до решения совета") && said.contains("налог на голубятни")) &&
              assertTrue(StoryClearingQuest.atClearing(q) && !StoryClearingQuest.toJunona(q)) &&
              assertTrue(!other.choices.exists(_.id == "HomeJunonaClearing"))
    },

    test("просека: у каждого пути своё дело; бой — вся бригада, бочки с порохом, у Щита Горн, у одиночки Элиар, у Тени никого") {
      for {
        shadow <- fightOn(Path.Shadow)
        shield <- fightOn(Path.Shield)
        alone  <- fightOn(Path.Alone)
      } yield assertTrue(List(shadow, shield, alone).forall { case (shown, res, _, b, loot) =>
                shown.text.contains("Просеку слышно раньше, чем видно") &&
                  shown.text.contains(Difficulty.render(StoryClearingQuest.Level)) && res == StateType.Battle &&
                  b.exists { x =>
                    val all = brigade(x)
                    x.cantFlee && x.story.contains(StoryClearingQuest.FightStory) && x.kegs == StoryClearingQuest.Kegs &&
                      x.monsterRace == Race.Goblin.entryName && x.monsterLvl == StoryClearingQuest.Level.toLong &&
                      all.size == 15 && all.count(_._1 == Rarity.Common.entryName) == 10 &&
                      all.count(_._1 == Rarity.Mythical.entryName) == 4 &&
                      all.count(m => m._1 == Rarity.Legendary.entryName && m._2.contains("Треск Колун")) == 1
                  } &&
                  loot.exists(_.returnState.contains(StateType.StoryClearing))
              }) &&
              assertTrue(shadow._1.choices.map(_.id) == List("ClearingPay", "ClearingFight", "ClearingLeave") &&
                shadow._1.text.contains("Третью неделю рубим в долг") && shadow._1.text.contains(s"Долг — ${StoryClearingQuest.wages} 🪙") &&
                shadow._1.choices.exists(_.label.contains(StoryClearingQuest.wages.toString))) &&
              assertTrue(List(shield, alone).forall(_._1.choices.map(_.id) == List("ClearingFight", "ClearingLeave"))) &&
              assertTrue(shield._1.text.contains("Гильдия этой изгородью кормится триста лет") &&
                alone._1.text.contains("шнапс Барти здесь знают") && alone._1.text.contains("Под певцов")) &&
              assertTrue(shadow._3.contains("Объяснять будешь руками") && shield._3.contains("Колени потом") &&
                alone._3.contains("Я встаю рядом с Элиаром")) &&
              assertTrue(shadow._4.exists(_.group.allies.isEmpty)) &&
              assertTrue(shield._4.toList.flatMap(_.group.allies).map(a => (a.kind, a.home, a.lvl, a.undead.map(_.name))) ==
                List((AllyKind.GuildMaster, 0, StoryClearingQuest.Level.toLong, Some("Горн")))) &&
              assertTrue(alone._4.toList.flatMap(_.group.allies).map(a => (a.kind, a.home, a.lvl, a.undead.map(_.name))) ==
                List((AllyKind.HedgeElf, 0, StoryClearingQuest.Level.toLong, Some(HedgeSongQuest.ElfName)))) &&
              assertTrue(BattleState.RaidStories.contains(StoryClearingQuest.FightStory))
    },

    test("Тень отдаёт долг Братства: не хватает — так и сказано; хватает — без драки, накладная вместо расписки, глава закрыта") {
      def pay(silver: Long) =
        for {
          c    <- content
          d    <- dao(atClearing(Path.Shadow), hero.copy(silver = silver))
          inv   = TestInventoryRepository.withItems(List(pass))
          r    <- TestRenderer.make
          _    <- scene(d, inv, c).action(testUser, tap("ClearingPay"), r)
          said <- texts(r)
          q    <- questOf(d)
          h    <- heroOf(d)
          b    <- d.readActiveBattle(userId)
        } yield (said, q, h, inv.snapshot, b)
      val rich = StoryClearingQuest.wages + 5000L
      for {
        poor <- pay(100L)
        paid <- pay(rich)
      } yield assertTrue(poor._1.contains(s"Колуну нужно ${StoryClearingQuest.wages} 🪙") && StoryClearingQuest.atClearing(poor._2) &&
                poor._3.silver == 100L) &&
              assertTrue(paid._1.contains("Субботу, считай, отработали") && paid._1.contains("Расписку Колун не пишет") &&
                paid._1.contains("накладная — сорок третья") && paid._1.contains("Э. платит за улаженное дело") &&
                paid._1.contains("📜 Квест «Просека» завершён.") && paid._1.contains("📜 Новое задание: «Печь»")) &&
              assertTrue(paid._2.isDone(NpcQuest.StoryClearing) && paid._2.isTaken(NpcQuest.StoryFurnace) &&
                StoryLine.active(paid._2).contains(NpcQuest.StoryFurnace) && paid._5.isEmpty &&
                paid._3.silver == rich - StoryClearingQuest.wages + StoryClearingQuest.silver && paid._3.exp > 0L) &&
              assertTrue(paid._4.exists(_.questItem.contains(QuestItemKind.StoryWaybill)) &&
                !paid._4.exists(_.questItem.contains(QuestItemKind.StoryPass)))
    },

    test("отбились — у каждого пути своя развязка; накладная на лес в сумке, пропуск уходит, серебро; «Печь» висит") {
      def won(path: Path) =
        for {
          c    <- content
          d    <- dao(atClearing(path))
          _    <- d.writeSceneData(userId, StoryFight.mark(StoryClearingState.WonKey))
          inv   = TestInventoryRepository.withItems(List(pass))
          r    <- TestRenderer.make
          _    <- scene(d, inv, c).enter(testUser, r)
          said <- texts(r)
          q    <- questOf(d)
          h    <- heroOf(d)
        } yield (said, q, h, inv.snapshot)
      for {
        shadow <- won(Path.Shadow)
        shield <- won(Path.Shield)
        alone  <- won(Path.Alone)
      } yield assertTrue(List(shadow, shield, alone).forall { case (said, q, h, bag) =>
                said.contains("Доставить: Загон, печной двор") && said.contains("📜 Квест «Просека» завершён.") &&
                  said.contains("📜 Новое задание: «Печь»") &&
                  q.isDone(NpcQuest.StoryClearing) && q.isTaken(NpcQuest.StoryFurnace) &&
                  h.silver == 100L + StoryClearingQuest.silver && h.exp > 0L &&
                  bag.exists(_.questItem.contains(QuestItemKind.StoryWaybill)) && !bag.exists(_.questItem.contains(QuestItemKind.StoryPass))
              }) &&
              assertTrue(shadow._1.contains("зарубок на нём теперь две") && shadow._1.contains("Договор Колун суёт мне сам")) &&
              assertTrue(shield._1.contains("Приказ Юноны прибиваем к самому толстому пню") &&
                shield._1.contains("Во внутреннем кармане у Колуна") && shield._1.contains("Гильдия платит за остановленную рубку")) &&
              assertTrue(alone._1.contains("До утра простоит") && alone._1.contains("Во внутреннем кармане у Колуна") &&
                alone._1.contains("из сундука десятника"))
    },

    test("смерть на просеке — проснуться в лабиринте, шаг тот же; журнал — по пути; Фаэлин при герое — свои слова; накладная — «Печь» с обновлением") {
      for {
        c      <- content
        d      <- dao(atClearing(Path.Alone))
        fight   = SoloPveBattle.fromGroup(StoryClearingQuest.foes("Треск Колун"), hero, List.fill(15)(0L))
                    .copy(story = Some(StoryClearingQuest.FightStory))
        _      <- d.writeActiveBattle(userId, fight.asJson)
        r      <- TestRenderer.make
        _      <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        wake   <- d.readSceneData(userId)
        q      <- questOf(d)
        // Фаэлин в отряде — брат Элиара: у него свои слова.
        withF   = hero.copy(squad = Squad(heroPos = 2, allies = List(Ally(AllyKind.Elf, 1, 1000L, 100L, 0L))))
        d2     <- dao(atClearing(Path.Shadow), withF)
        r2     <- TestRenderer.make
        _      <- scene(d2, TestInventoryRepository.withItems(List(pass)), c).enter(testUser, r2)
        faelin <- r2.sentScreens.map(_.last.text)
        r3     <- TestRenderer.make
        _      <- scene(d, TestInventoryRepository.withItems(List(pass)), c).enter(testUser, r3)
        plain  <- r3.sentScreens.map(_.last.text)
        line    = (p: Path, n: Int) => QuestJournal.render(c, hero, LoreData.empty, clearing(p, n), DailyQuests.empty, None, 0L)
        after   = atClearing(Path.Alone).finish(NpcQuest.StoryClearing).take(NpcQuest.StoryFurnace, 0L)
        journal = QuestJournal.render(c, hero, LoreData.empty, after, DailyQuests.empty, None, 0L)
        waybill = InventoryState.questText(c, QuestItemKind.item(QuestItemKind.StoryWaybill).copy(id = 983L), after, 36L)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("story.storyClearing.clearing.death"))) &&
                wake.exists(_.hcursor.downField("wakeTo").focus.isEmpty) && StoryClearingQuest.atClearing(q)) &&
              assertTrue(faelin.contains("так поёт его брат") && !plain.contains("так поёт его брат")) &&
              assertTrue(line(Path.Shadow, 1).contains("объяснить бригаде Чернолома договор") &&
                line(Path.Shield, 1).contains("показать бирку Чернолома Юноне Кинтслев") &&
                line(Path.Shield, 2).contains("остановить рубку вместе с Горном") &&
                line(Path.Alone, 1).contains("войти с ящиком шнапса Барти в лагерь Чернолома")) &&
              assertTrue(journal.contains("📌 «Печь»") && journal.contains("глава откроется с обновлением")) &&
              assertTrue(waybill.contains("Накладная на лес") && waybill.contains("станет ясно с обновлением 🔜"))
    }
  )
}
