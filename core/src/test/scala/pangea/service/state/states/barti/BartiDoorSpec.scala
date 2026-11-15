package pangea.service.state.states.barti

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{Element, SoloPveBattle}
import pangea.model.hero.{Hero, LoreData}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{DailyQuests, Difficulty, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.story.StoryFight
import pangea.service.state.states.{DeathState, MarketSquareState}
import pangea.service.state.{BartiDoorQuest, NpcQuestLog, QuestJournal, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Синяя дверь» — побочное задание Барти, только у пути «Сам по себе»: Барти
  * похвастался, с кем водится, и ночью Братство присылает жечь его дом демонов.
  * Дом — кнопкой с Торговой площади, пока задание доступно или идёт. Бой с
  * поджигателями, Барти со сковородой — в строю. */
object BartiDoorSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, state = StateType.MarketSquare).copy(lvl = 33L, silver = 100L)

  /** Акт I пройден (путь `path`), «Наутро» и «Большое дело» закрыты. */
  private def ready(path: Path): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    val face = StoryPowderQuest.finish(base, WardenFate.Killed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = SojkaFate.Broke.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
    StoryToolQuest.finish(face.take(NpcQuest.StoryTool, 0L), path)
      .take(NpcQuest.StoryMorning, 0L).finish(NpcQuest.StoryMorning)
      .take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods).take(NpcQuest.BigDeal, 0L).finish(NpcQuest.BigDeal)
  }

  private def night: NpcQuests = ready(Path.Alone).take(NpcQuest.BartiDoor, 0L)

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

  private def house(d: TestHeroDao, c: SceneContent) =
    BartiHouseState(d, TestInventoryRepository.withItems(Nil), c, Some(TestItemRepository.make))

  override def spec = suite("«Синяя дверь»: поджигатели у дома Барти")(

    test("Торговая площадь: «Дом Барти» — только у одиночки, после «Наутро» и «Большого дела», пока задание идёт") {
      def buttons(q: NpcQuests) =
        for {
          c <- content
          d <- dao(q)
          r <- TestRenderer.make
          _ <- MarketSquareState(c, Some(d)).enter(testUser, r)
          s <- r.sentScreens.map(_.last)
        } yield s.choices.map(_.id)
      for {
        alone  <- buttons(ready(Path.Alone))
        shield <- buttons(ready(Path.Shield))
        shadow <- buttons(ready(Path.Shadow))
        early  <- buttons(ready(Path.Alone).update(NpcQuest.StoryMorning)(_.copy(done = false)))
        going  <- buttons(night)
        closed <- buttons(night.finish(NpcQuest.BartiDoor))
      } yield assertTrue(alone.contains("BartiHome") && going.contains("BartiHome")) &&
              assertTrue(!shield.contains("BartiHome") && !shadow.contains("BartiHome") && !early.contains("BartiHome") &&
                !closed.contains("BartiHome"))
    },

    test("впервые: метка на двери, Барти признаётся — задание взято, ночь у двери") {
      for {
        c <- content
        d <- dao(ready(Path.Alone))
        r <- TestRenderer.make
        _ <- house(d, c).enter(testUser, r)
        s <- r.sentScreens.map(_.last)
        q <- questOf(d)
      } yield assertTrue(s.text.contains("круг, перечёркнутый клинком") && s.text.contains("И жене его") &&
                s.text.contains("📜 Новое задание: «Синяя дверь»") && s.text.contains("пальцы у них дымятся") &&
                s.text.contains(Difficulty.render(BartiDoorQuest.Level))) &&
              assertTrue(s.choices.map(_.id) == List("BartiDoorFight", "BartiLeave") && BartiDoorQuest.toNight(q))
    },

    test("бой: Головня и четверо — демоны, их удары жгут огнём; Барти со сковородой в строю; без бегства") {
      for {
        c    <- content
        d    <- dao(night)
        r    <- TestRenderer.make
        res  <- house(d, c).action(testUser, tap("BartiDoorFight"), r)
        b    <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(res == StateType.Battle && b.exists(x => x.story.contains(BartiDoorQuest.FightStory) && x.cantFlee &&
                x.monsterName.contains("Головня") && x.monsterRace == Race.Demon.entryName &&
                x.monsterRarity == Rarity.Mythical.entryName && x.monsterLvl == BartiDoorQuest.Level.toLong &&
                x.group.others.size == 4 && x.group.others.forall(m => m.race == Race.Demon.entryName && m.rarity == Rarity.Rare.entryName))) &&
              assertTrue(Race.Demon.weaponElement.contains(Element.Fire)) &&
              assertTrue(b.toList.flatMap(_.group.allies).count(a => a.kind == AllyKind.Informant && a.home == 0 &&
                a.lvl == BartiDoorQuest.Level.toLong && a.undead.exists(_.name == "Барти")) == 1) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.BartiHouse)) && BattleState.RaidStories.contains(BartiDoorQuest.FightStory))
    },

    test("отбились: дом в дырах, кресло спасено, теперь это убежище; серебро, задание закрыто") {
      for {
        c   <- content
        d   <- dao(night)
        _   <- d.writeSceneData(userId, StoryFight.mark(BartiHouseState.DoorWonKey))
        r   <- TestRenderer.make
        _   <- house(d, c).enter(testUser, r)
        end <- texts(r)
        q   <- questOf(d)
        h   <- heroOf(d)
      } yield assertTrue(end.contains("кресло, в котором нельзя сидеть") && end.contains("Заходи без стука") &&
                end.contains("📜 Квест «Синяя дверь» завершён.")) &&
              assertTrue(q.isDone(NpcQuest.BartiDoor) && h.silver == 100L + BartiDoorQuest.silver && h.exp > 0L)
    },

    test("смерть у двери — проснуться на Торговой площади, шаг тот же; журнал — что делать") {
      for {
        c     <- content
        d     <- dao(night)
        fight  = SoloPveBattle.fromGroup(BartiDoorQuest.foes("Головня"), hero, List.fill(5)(0L)).copy(story = Some(BartiDoorQuest.FightStory))
        _     <- d.writeActiveBattle(userId, fight.asJson)
        r     <- TestRenderer.make
        _     <- DeathState(d, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        wake  <- d.readSceneData(userId)
        q     <- questOf(d)
        line   = QuestJournal.render(c, hero, LoreData.empty, night, DailyQuests.empty, None, 0L)
      } yield assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("barti.door.death"))) &&
                wake.exists(_.hcursor.get[StateType]("wakeTo").toOption.contains(StateType.MarketSquare)) &&
                BartiDoorQuest.toNight(q)) &&
              assertTrue(line.contains("«Синяя дверь» — встретить ночью тех, кто придёт жечь дом Барти"))
    }
  )
}
