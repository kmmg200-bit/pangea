package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.Rarity
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.skill.MonsterSkill
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryNewcomerQuest.Fate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryClearingQuest, StoryLine, StoryNewcomerQuest, StoryPowderQuest,
  StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Акт II, глава 7 «Новенький»: на главную площадь падает Мило — новый
  * отмеченный, — и за ним приходят раньше героя: у Тени стража совета, у Щита и
  * одиночки — ловцы Братства с сетями. После боя — куда его девать: Братству,
  * совету или через изгородь подальше. Исход идёт в «как и вы», в сумке —
  * пропуск на просеку, глава 8 «Просека» — следом. */
object StoryNewcomerSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, state = StateType.CityCenter).copy(lvl = 31L, silver = 100L)

  /** Акт I пройден (путь `path`), «Наутро» закрыто, «Новенький» на шаге `n`. */
  private def newcomer(path: Path, n: Int): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    val face = StoryPowderQuest.finish(base, WardenFate.Killed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = SojkaFate.Broke.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
    val done = StoryToolQuest.finish(face.take(NpcQuest.StoryTool, 0L), path)
      .take(NpcQuest.StoryMorning, 0L).finish(NpcQuest.StoryMorning).take(NpcQuest.StoryNewcomer, 0L)
    StoryNewcomerQuest.step(done, n)
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

  private val harvest: Item = QuestItemKind.item(QuestItemKind.StoryHarvest).copy(id = 981L)

  private def square(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent, o: Option[TestQuestOutcomeDao] = None) =
    StorySquareState(d, inv, TestItemRepository.make, c, o)

  /** Площадь и бой: что на экране и кто пришёл за Мило. */
  private def fightOn(path: Path) =
    for {
      c    <- content
      d    <- dao(newcomer(path, 1))
      r    <- TestRenderer.make
      st    = square(d, TestInventoryRepository.withItems(List(harvest)), c)
      _    <- st.enter(testUser, r)
      shown <- r.sentScreens.map(_.last)
      res  <- st.action(testUser, tap("SquareFight"), r)
      b    <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      loot <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
    } yield (shown, res, b, loot)

  override def spec = suite("Акт II, глава 7 «Новенький»")(

    test("карточка описи: сложность и что делать; с карточки из города — на главную площадь") {
      for {
        c     <- content
        d     <- dao(newcomer(Path.Shield, 1))
        _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
        r     <- TestRenderer.make
        card   = InventoryState(d, TestInventoryRepository.withItems(List(harvest)), TestItemRepository.make, c)
        _     <- card.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${harvest.id}"}""")), r)
        shown <- r.sentScreens.map(_.last)
        go    <- card.action(testUser, tap("StorySquareGo"), r)
      } yield assertTrue(shown.text.contains(Difficulty.render(StoryNewcomerQuest.Level)) &&
                shown.text.contains("Сходить посмотреть, кто успеет к нему первым") &&
                shown.choices.exists(_.id == "StorySquareGo") && go == StateType.StorySquare)
    },

    test("площадь: Мило в фонтане; у Тени — сержант стражи совета и четверо, без сетей; у Щита и одиночки — Грач и четверо, с сетями") {
      for {
        shadow <- fightOn(Path.Shadow)
        shield <- fightOn(Path.Shield)
        alone  <- fightOn(Path.Alone)
      } yield assertTrue(List(shadow, shield, alone).forall { case (shown, res, b, loot) =>
                shown.text.contains("сидит по пояс в фонтане, в одном сапоге") &&
                  shown.choices.map(_.id) == List("SquareFight", "SquareLeave") && res == StateType.Battle &&
                  b.exists(x => x.cantFlee && x.monsterRarity == Rarity.Mythical.entryName &&
                    x.monsterLvl == StoryNewcomerQuest.Level.toLong && x.group.others.size == 4 &&
                    x.group.others.forall(_.rarity == Rarity.Rare.entryName)) &&
                  loot.exists(_.returnState.contains(StateType.StorySquare))
              }) &&
              assertTrue(shadow._1.text.contains("стража совета") && shadow._3.exists(x =>
                x.monsterName.contains("Сержант Корт") && x.story.contains(StoryNewcomerQuest.GuardStory) &&
                !MonsterSkill.CatcherNet.applicable(x))) &&
              assertTrue(shield._1.text.contains("Юнона просила") && alone._1.text.contains("постою за фонтаном")) &&
              assertTrue(List(shield, alone).forall(t => t._3.exists(x => x.monsterName.contains("Грач") &&
                x.story.contains(StoryNewcomerQuest.CatchersStory) && MonsterSkill.CatcherNet.applicable(x)))) &&
              assertTrue(StoryNewcomerQuest.FightStories.forall(BattleState.RaidStories.contains))
    },

    test("отбились — Мило и вопрос, куда его; у Щита подходит Горн, у одиночки — Барти, у Тени — приказ Э.") {
      def won(path: Path) =
        for {
          c <- content
          d <- dao(newcomer(path, 1))
          _ <- d.writeSceneData(userId, StoryFight.mark(StorySquareState.WonKey))
          r <- TestRenderer.make
          _ <- square(d, TestInventoryRepository.withItems(List(harvest)), c).enter(testUser, r)
          s <- r.sentScreens.map(_.last)
          q <- questOf(d)
        } yield (s, q)
      for {
        shadow <- won(Path.Shadow)
        shield <- won(Path.Shield)
        alone  <- won(Path.Alone)
      } yield assertTrue(List(shadow, shield, alone).forall { case (s, q) =>
                s.text.contains("Меня зовут Мило") && s.text.contains("Куда его?") && StoryNewcomerQuest.toFate(q) &&
                  s.choices.map(_.id) == List("SquareBrotherhood", "SquareCouncil", "SquareHedge", "SquareLeave")
              }) &&
              assertTrue(shadow._1.text.contains("Приказ Э. простой") && shield._1.text.contains("Своих Гильдия не отдаёт") &&
                alone._1.text.contains("Продаём? Шучу"))
    },

    test("куда девать Мило — глава закрыта: исход в подсчёт, опись уходит, пропуск на просеку в сумке, серебро; «Просека» висит") {
      def decide(path: Path, action: String) =
        for {
          c    <- content
          d    <- dao(newcomer(path, 2))
          inv   = TestInventoryRepository.withItems(List(harvest))
          o    <- TestQuestOutcomeDao.make
          r    <- TestRenderer.make
          _    <- square(d, inv, c, Some(o)).action(testUser, tap(action), r)
          said <- texts(r)
          q    <- questOf(d)
          h    <- heroOf(d)
          mine <- o.of(h.id, NpcQuest.StoryNewcomer)
        } yield (said, q, h, inv.snapshot, mine)
      for {
        shadowBro <- decide(Path.Shadow, "SquareBrotherhood")
        shieldBro <- decide(Path.Shield, "SquareBrotherhood")
        shadowCou <- decide(Path.Shadow, "SquareCouncil")
        aloneHed  <- decide(Path.Alone, "SquareHedge")
        c         <- content
      } yield assertTrue(shadowBro._1.contains("«Чисто»") && shadowBro._1.contains("За поясом у сержанта Корта")) &&
              assertTrue(shieldBro._1.contains("Кошель тяжёлый") && shieldBro._1.contains("За поясом у Грача")) &&
              assertTrue(shadowCou._1.contains("упал в фонтан, вышел мокрым") && shadowCou._1.contains("Э. узнает об этом к вечеру")) &&
              assertTrue(aloneHed._1.contains("где выйдет Мило, не знаю даже я")) &&
              assertTrue(List((shadowBro, Fate.Brotherhood), (shieldBro, Fate.Brotherhood), (shadowCou, Fate.Council),
                (aloneHed, Fate.Hedge)).forall { case ((said, q, h, bag, mine), fate) =>
                said.contains("📜 Квест «Новенький» завершён.") && said.contains("Вы — среди первых, кто закончил «Новенький»") &&
                  said.contains("📜 Новое задание: «Просека»") &&
                  q.isDone(NpcQuest.StoryNewcomer) && StoryNewcomerQuest.fate(q).contains(fate) &&
                  q.isTaken(NpcQuest.StoryClearing) && StoryLine.active(q).contains(NpcQuest.StoryClearing) &&
                  mine.contains(fate.outcome) && h.silver == 100L + StoryNewcomerQuest.silver && h.exp > 0L &&
                  bag.exists(_.questItem.contains(QuestItemKind.StoryPass)) && !bag.exists(_.questItem.contains(QuestItemKind.StoryHarvest))
              }) &&
              assertTrue(c.text("questStats.storyNewcomer.hedge").contains("через изгородь"))
    },

    test("смерть на площади — проснуться там же, шаг тот же; журнал и карточка пропуска — что делать в «Просеке»") {
      def die(path: Path, story: String) =
        for {
          c    <- content
          d    <- dao(newcomer(path, 1))
          fight = SoloPveBattle.fromGroup(StoryNewcomerQuest.foes("Грач"), hero, List.fill(5)(0L)).copy(story = Some(story))
          _    <- d.writeActiveBattle(userId, fight.asJson)
          r    <- TestRenderer.make
          _    <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
          wake <- d.readSceneData(userId)
          q    <- questOf(d)
        } yield (wake.flatMap(_.hcursor.get[List[String]]("wakeLines").toOption),
                 wake.flatMap(_.hcursor.get[StateType]("wakeTo").toOption), StoryNewcomerQuest.toSquare(q))
      for {
        c      <- content
        guard  <- die(Path.Shadow, StoryNewcomerQuest.GuardStory)
        catchr <- die(Path.Alone, StoryNewcomerQuest.CatchersStory)
        after   = newcomer(Path.Alone, 2).finish(NpcQuest.StoryNewcomer).take(NpcQuest.StoryClearing, 0L)
        journal = QuestJournal.render(c, hero, LoreData.empty, after, DailyQuests.empty, None, 0L)
        pass    = InventoryState.questText(c, QuestItemKind.item(QuestItemKind.StoryPass).copy(id = 982L), after, 31L)
      } yield assertTrue(List(guard, catchr).forall(_ == ((Some(List("story.storyNewcomer.square.death")), Some(StateType.CityCenter), true)))) &&
              assertTrue(journal.contains("📌 «Просека»") && journal.contains("войти с ящиком шнапса Барти в лагерь Чернолома")) &&
              assertTrue(pass.contains("Пропуск на просеку") && pass.contains("шнапс ящиками") &&
                pass.contains(Difficulty.render(StoryClearingQuest.Level)))
    }
  )
}
