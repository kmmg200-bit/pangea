package pangea.service.state.states.barti

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
import pangea.service.state.BartiRoofQuest.Deal
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.home.HomeState
import pangea.service.state.states.story.StoryFight
import pangea.service.state.states.{DeathState, InventoryState, MarketSquareState}
import pangea.service.state.{BartiRoofQuest, NpcQuestLog, QuestJournal, StoryPowderQuest, StoryToolQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Крыша» — побочное задание Барти в Акте II: ночная стража берёт с его лавки
  * плату «за охрану». Лавка — кнопкой с Торговой площади, пока задание
  * доступно или идёт. Щит идёт к Юноне за поддержкой, Тени Братство предлагает
  * решить вопрос само (без боя, но Барти теперь должен Братству), одиночка —
  * сразу ночью в лавку. Ночью — бой с капитаном и четырьмя, Барти под
  * прилавком. */
object BartiRoofSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, state = StateType.MarketSquare).copy(lvl = 28L, silver = 100L)

  /** Акт I пройден (путь `path`), «Большое дело» закрыто. */
  private def ready(path: Path): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    val face = StoryPowderQuest.finish(base, WardenFate.Killed).take(NpcQuest.StoryBrand, 0L).finish(NpcQuest.StoryBrand)
      .take(NpcQuest.StoryList, 0L).finish(NpcQuest.StoryList).update(NpcQuest.StoryList)(_.copy(counter = SojkaFate.Broke.n))
      .take(NpcQuest.StoryFace, 0L).finish(NpcQuest.StoryFace)
    StoryToolQuest.finish(face.take(NpcQuest.StoryTool, 0L), path)
      .take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods).take(NpcQuest.BigDeal, 0L).finish(NpcQuest.BigDeal)
  }

  /** «Крыша» на шаге `n`. */
  private def roof(path: Path, n: Int): NpcQuests = BartiRoofQuest.step(ready(path).take(NpcQuest.BartiRoof, 0L), n)

  private def dao(q: NpcQuests) =
    for {
      d <- TestHeroDao.withHero(userId, hero)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- d.writeLoreData(userId, LoreData(firstSteps = true, firstStepsDone = true).asJson)
      _ <- d.writeHomeData(userId, HomeData(tier = House.Cottage.tier).asJson)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private val receipt: Item = QuestItemKind.item(QuestItemKind.BartiReceipt).copy(id = 971L)

  private def shop(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    BartiShopState(d, inv, TestItemRepository.make, c)

  override def spec = suite("«Крыша»: лавка Барти и ночная стража")(

    test("Торговая площадь: «Лавка Барти» — когда Рубеж I пройден и «Большое дело» закрыто, пока задание идёт; после — уже ради «Большой поставки»") {
      def buttons(q: NpcQuests) =
        for {
          c <- content
          d <- dao(q)
          r <- TestRenderer.make
          _ <- MarketSquareState(c, Some(d)).enter(testUser, r)
          s <- r.sentScreens.map(_.last)
        } yield s.choices.map(_.id)
      for {
        early  <- buttons(ready(Path.Alone).update(NpcQuest.BigDeal)(_.copy(done = false)))
        open   <- buttons(ready(Path.Alone))
        going  <- buttons(roof(Path.Alone, 3))
        // «Крыша» закрыта — лавка остаётся ради «Большой поставки», а закрыта и
        // она — кнопки нет.
        next   <- buttons(roof(Path.Alone, 3).finish(NpcQuest.BartiRoof))
        closed <- buttons(roof(Path.Alone, 3).finish(NpcQuest.BartiRoof).take(NpcQuest.BartiDelivery, 0L)
                    .finish(NpcQuest.BartiDelivery))
        c      <- content
        r      <- TestRenderer.make
        _      <- MarketSquareState(c).enter(testUser, r)
        old    <- r.sentScreens.map(_.last)
      } yield assertTrue(!early.contains("BartiShop") && open.contains("BartiShop") && going.contains("BartiShop") &&
                next.contains("BartiShop") && !closed.contains("BartiShop") && !old.choices.exists(_.id == "BartiShop"))
    },

    test("лавка впервые: квитанция в сумке, задание взято на шаге своего пути — Щиту к Юноне, Тени — Братство, одиночке — сразу ночь") {
      def begin(path: Path) =
        for {
          c    <- content
          d    <- dao(ready(path))
          inv   = TestInventoryRepository.accepting
          r    <- TestRenderer.make
          _    <- shop(d, inv, c).enter(testUser, r)
          s    <- r.sentScreens.map(_.last)
          q    <- questOf(d)
        } yield (s, q, inv.snapshot)
      for {
        shield <- begin(Path.Shield)
        shadow <- begin(Path.Shadow)
        alone  <- begin(Path.Alone)
      } yield assertTrue(List(shield, shadow, alone).forall { case (s, q, bag) =>
                s.text.contains("Над дверью вывеска: «Всё для искателей»") && s.text.contains("📜 Новое задание: «Крыша»") &&
                  q.isTaken(NpcQuest.BartiRoof) && bag.exists(_.questItem.contains(QuestItemKind.BartiReceipt))
              }) &&
              assertTrue(BartiRoofQuest.toJunona(shield._2) && shield._1.text.contains("ходят к Юноне") &&
                shield._1.choices.map(_.id) == List("RoofLeave")) &&
              assertTrue(BartiRoofQuest.toOffer(shadow._2) && shadow._1.text.contains("Э. велел передать") &&
                shadow._1.choices.map(_.id) == List("RoofBrotherhood", "RoofSelf", "RoofLeave")) &&
              assertTrue(BartiRoofQuest.toNight(alone._2) && alone._1.text.contains("Днём, наверное, стесняется") &&
                alone._1.text.contains(Difficulty.render(BartiRoofQuest.Level)) &&
                alone._1.choices.map(_.id) == List("RoofFight", "RoofLeave"))
    },

    test("Щит: Юнона с квитанцией — её люди будут за углом; с карточки — ночью в лавку; бой: капитан и четверо, Барти не в строю; отбились — Воска уводят") {
      for {
        c      <- content
        d      <- dao(roof(Path.Shield, 1))
        inv     = TestInventoryRepository.withItems(List(receipt))
        r      <- TestRenderer.make
        sch    <- TestScheduler.make
        home    = HomeState(d, sch, c, inv, TestItemRepository.make)
        _      <- home.action(testUser, tap("HomeOffice"), r)
        office <- r.sentScreens.map(_.last)
        _      <- home.action(testUser, tap("HomeJunonaRoof"), r)
        said   <- texts(r)
        q3     <- questOf(d)
        _      <- d.writeReturnState(userId, Some(StateType.GlobalMap))
        card    = InventoryState(d, inv, TestItemRepository.make, c)
        _      <- card.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${receipt.id}"}""")), r)
        shown  <- r.sentScreens.map(_.last)
        go     <- card.action(testUser, tap("BartiShopGo"), r)
        st      = shop(d, inv, c)
        res    <- st.action(testUser, tap("RoofFight"), r)
        b      <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot   <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        _      <- d.writeSceneData(userId, StoryFight.mark(BartiShopState.WonKey))
        r2     <- TestRenderer.make
        _      <- st.enter(testUser, r2)
        end    <- texts(r2)
        q      <- questOf(d)
        h      <- heroOf(d)
      } yield assertTrue(office.choices.exists(_.id == "HomeJunonaRoof") && said.contains("Мои будут за углом") &&
                BartiRoofQuest.toNight(q3)) &&
              assertTrue(shown.text.contains("Капитан Воск приходит за деньгами ночью") &&
                shown.choices.exists(c => c.id == "BartiShopGo" && c.label.contains("Ночью")) && go == StateType.BartiShop) &&
              assertTrue(res == StateType.Battle && b.exists(x => x.story.contains(BartiRoofQuest.FightStory) && x.cantFlee &&
                x.monsterName.contains("Капитан Воск") && x.monsterRarity == Rarity.Mythical.entryName &&
                x.monsterLvl == BartiRoofQuest.Level.toLong && x.group.others.size == 4 &&
                x.group.others.forall(m => m.rarity == Rarity.Rare.entryName && m.race == Race.Human.entryName) &&
                !x.group.allies.exists(_.kind == AllyKind.Informant))) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.BartiShop)) && BattleState.RaidStories.contains(BartiRoofQuest.FightStory)) &&
              assertTrue(end.contains("держится за разбитый нос") && end.contains("Дядя тоже будет рад тебя видеть") &&
                end.contains("Есть одна поставка. Большая") && end.contains("📜 Квест «Крыша» завершён.")) &&
              assertTrue(q.isDone(NpcQuest.BartiRoof) && BartiRoofQuest.deal(q).isEmpty && h.silver == 100L + BartiRoofQuest.silver &&
                h.exp > 0L && !inv.snapshot.exists(_.questItem.contains(QuestItemKind.BartiReceipt)))
    },

    test("Тень: «Пусть решает Братство» — без боя, Воска переводят в порт, Барти теперь должен Братству; «Разберусь сам» — ночь, бой, задание закрыто") {
      for {
        c      <- content
        d      <- dao(roof(Path.Shadow, 2))
        inv     = TestInventoryRepository.withItems(List(receipt))
        r      <- TestRenderer.make
        _      <- shop(d, inv, c).action(testUser, tap("RoofBrotherhood"), r)
        let    <- texts(r)
        q1     <- questOf(d)
        bt     <- d.readActiveBattle(userId)
        e      <- dao(roof(Path.Shadow, 2))
        inv2    = TestInventoryRepository.withItems(List(receipt))
        r2     <- TestRenderer.make
        st      = shop(e, inv2, c)
        _      <- st.action(testUser, tap("RoofSelf"), r2)
        night  <- r2.sentScreens.map(_.last)
        q2     <- questOf(e)
        _      <- e.writeSceneData(userId, StoryFight.mark(BartiShopState.WonKey))
        _      <- st.enter(testUser, r2)
        won    <- texts(r2)
        q3     <- questOf(e)
      } yield assertTrue(let.contains("перевели в порт") && let.contains("Я ему не говорю") && let.contains("📜 Квест «Крыша» завершён.") &&
                q1.isDone(NpcQuest.BartiRoof) && BartiRoofQuest.deal(q1).contains(Deal.Brotherhood) && bt.isEmpty &&
                !inv.snapshot.exists(_.questItem.contains(QuestItemKind.BartiReceipt))) &&
              assertTrue(night.text.contains("Сам так сам. Э. запомнит") && night.choices.map(_.id) == List("RoofFight", "RoofLeave") &&
                BartiRoofQuest.toNight(q2)) &&
              assertTrue(won.contains("Э. просил передать: неплохо") && BartiRoofQuest.deal(q3).contains(Deal.Self))
    },

    test("одиночка: отбились — Барти отдаёт долю; смерть в лавке — проснуться на Торговой площади, шаг тот же") {
      for {
        c     <- content
        d     <- dao(roof(Path.Alone, 3))
        r     <- TestRenderer.make
        _     <- d.writeSceneData(userId, StoryFight.mark(BartiShopState.WonKey))
        _     <- shop(d, TestInventoryRepository.withItems(List(receipt)), c).enter(testUser, r)
        won   <- texts(r)
        e     <- dao(roof(Path.Alone, 3))
        fight  = SoloPveBattle.fromGroup(BartiRoofQuest.foes("Капитан Воск"), hero, List.fill(5)(0L))
                   .copy(story = Some(BartiRoofQuest.FightStory))
        _     <- e.writeActiveBattle(userId, fight.asJson)
        _     <- DeathState(e, TestInventoryRepository.withItems(Nil), c).enter(testUser, r)
        wake  <- e.readSceneData(userId)
        q     <- questOf(e)
      } yield assertTrue(won.contains("Вот твоя доля") && won.contains("📜 Квест «Крыша» завершён.")) &&
              assertTrue(wake.exists(_.hcursor.get[List[String]]("wakeLines").toOption.contains(List("barti.roof.death"))) &&
                wake.exists(_.hcursor.get[StateType]("wakeTo").toOption.contains(StateType.MarketSquare)) &&
                BartiRoofQuest.toNight(q) && c.text("barti.roof.death").contains("Стража вернётся следующей ночью"))
    },

    test("журнал: «Крыша» среди заданий горожан, шаг своего пути; карточка квитанции — что делать сейчас") {
      for {
        c <- content
      } yield {
        def journal(q: NpcQuests) = QuestJournal.render(c, hero, LoreData.empty, q, DailyQuests.empty, None, 0L)
        val card = InventoryState.questText(c, receipt, roof(Path.Shield, 1), 28L)
        assertTrue(journal(roof(Path.Shield, 1)).contains("«Крыша» — показать квитанцию стражи Юноне Кинтслев") &&
          journal(roof(Path.Shadow, 2)).contains("пускать ли Братство к капитану") &&
          journal(roof(Path.Alone, 3)).contains("встретить ночью стражу в лавке Барти")) &&
        assertTrue(card.contains("Подпись капитана — Воск") && card.contains(Difficulty.render(BartiRoofQuest.Level)) &&
          card.contains("с бумагами в Кинэте ходят к Юноне"))
      }
    }
  )
}
