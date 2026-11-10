package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.{Hero, LoreData}
import pangea.model.home.HomeData
import pangea.model.item.{Bundling, Item, MaterialKind, QuestItemKind}
import pangea.model.quest.{DailyQuests, FirstSteps, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.states.InventoryState
import pangea.service.state.states.guild.MentorKazimirState
import pangea.service.state.states.home.HomeState
import pangea.service.state.states.temple.TempleAzatState
import pangea.service.state.{NpcQuestLog, QuestJournal, StoryBrandQuest, StoryPowderQuest, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Основная линия, глава 3 «Что горит на груди»: набросок узора ведёт к
  * Казимиру, жрецу и Юноне — каждый сперва называет цену разговора; встреча с
  * Сойкой — с карточки наброска, даром; страница списка с новым именем героя —
  * зацепка главы 4. */
object StoryBrandSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 13L): Hero = TestFixtures.hero(userId).copy(lvl = lvl, name = Some("Ворон"))

  /** Главы 1 и 2 закрыты; Смотрителя — по выбору. */
  private def afterPowder(fate: WardenFate = WardenFate.Killed): NpcQuests = {
    val base = FirstSteps.CityQuests.foldLeft(NpcQuests.empty)((q, x) => q.take(x, 0L).finish(x))
      .take(NpcQuest.StoryMark, 0L).finish(NpcQuest.StoryMark)
      .take(NpcQuest.StoryPowder, 0L)
    StoryPowderQuest.finish(base, fate)
  }

  /** Глава 3 на шаге `step`. */
  private def brand(step: Int, fate: WardenFate = WardenFate.Killed): NpcQuests =
    StoryBrandQuest.step(afterPowder(fate).take(NpcQuest.StoryBrand, 0L), step)

  private val ready: LoreData = LoreData(firstSteps = true, firstStepsDone = true, cluePasses = 5)

  private def dao(h: Hero, q: NpcQuests) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, q)
      _ <- d.writeLoreData(userId, ready.asJson)
      _ <- d.writeHomeData(userId, HomeData(tier = 1).asJson)
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                                  = ZIO.attempt(SceneContent.load())

  private def sketchItem: Item = QuestItemKind.item(QuestItemKind.StorySketch).copy(id = 902L)

  private def material(kind: MaterialKind, id: Long): Item = MaterialGenerator.item(kind).copy(id = id)

  /** Плата Казимиру: по два куска вечно огненного железа и магического камня. */
  private val kazimirPay: List[Item] = List(
    material(MaterialKind.EverburningIron, 911L), material(MaterialKind.EverburningIron, 912L),
    material(MaterialKind.MagicStone, 913L), material(MaterialKind.MagicStone, 914L))

  private val bones: List[Item] = List(material(MaterialKind.CursedBones, 921L), material(MaterialKind.CursedBones, 922L))
  private val hides: List[Item] = List(material(MaterialKind.WhiteWolfHide, 931L), material(MaterialKind.WhiteWolfHide, 932L))

  /** Текст карточки наброска при таком ходе главы. */
  private def cardText(q: NpcQuests) =
    for {
      d     <- dao(hero(), q)
      _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
      r     <- TestRenderer.make
      c     <- content
      state  = InventoryState(d, TestInventoryRepository.withItems(List(sketchItem)), TestItemRepository.make, c)
      _     <- openCard(state, sketchItem, r)
      shown <- r.sentScreens.map(_.last)
    } yield shown.text

  private def openCard(state: InventoryState, item: Item, r: TestRenderer) =
    state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${item.id}"}""")), r)

  override def spec = suite("Основная линия: «Что горит на груди»")(

    test("карточка наброска: сразу после главы 2 — к Казимиру, на любом уровне; «на встречу» — только на четвёртом шаге") {
      def card(lvl: Long, q: NpcQuests) =
        for {
          d     <- dao(hero(lvl), q)
          _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
          r     <- TestRenderer.make
          c     <- content
          state  = InventoryState(d, TestInventoryRepository.withItems(List(sketchItem)), TestItemRepository.make, c)
          _     <- openCard(state, sketchItem, r)
          shown <- r.sentScreens.map(_.last)
        } yield shown
      for {
        low     <- card(1L, afterPowder())
        ready13 <- card(13L, afterPowder())
        third   <- card(13L, brand(3))
        fourth  <- card(13L, brand(4))
      } yield assertTrue(low.text.contains("Покажу лист ему") && ready13.text.contains("Покажу лист ему") && !low.text.contains("окрепну")) &&
              assertTrue(List(low, ready13, third).forall(!_.choices.exists(_.id == "StoryMeetGo"))) &&
              assertTrue(fourth.text.contains("Сойка") && fourth.choices.exists(_.id == "StoryMeetGo"))
    },

    test("Казимир: кнопка — с наброском при герое, на любом уровне; сперва цена — глава берётся, без платы «Отдать» нет") {
      def menu(lvl: Long, items: List[Item]) =
        for {
          d     <- dao(hero(lvl), afterPowder())
          r     <- TestRenderer.make
          c     <- content
          state  = MentorKazimirState(d, TestInventoryRepository.withItems(items), c)
          _     <- state.enter(testUser, r)
          shown <- r.sentScreens.map(_.last)
        } yield (d, state, r, shown)
      for {
        young   <- menu(12L, List(sketchItem))
        empty   <- menu(13L, Nil)
        open    <- menu(13L, List(sketchItem))
        (d, st, r, shown) = open
        _       <- st.action(testUser, tap("KazStory"), r)
        ask     <- r.sentScreens.map(_.last)
        q       <- questOf(d)
      } yield assertTrue(young._4.choices.exists(_.id == "KazStory") && !empty._4.choices.exists(_.id == "KazStory")) &&
              assertTrue(shown.choices.exists(_.id == "KazStory")) &&
              assertTrue(ask.text.contains("задаром Казимир читает одни вывески") && ask.text.contains("два куска вечно огненного железа") &&
                ask.text.contains("📜 Новое задание: «Что горит на груди»")) &&
              assertTrue(!ask.choices.exists(_.id == "KazStoryPay") && ask.choices.exists(_.id == "KazimirMenu")) &&
              assertTrue(StoryBrandQuest.toKazimir(q) && StoryBrandQuest.priced(q))
    },

    test("Казимир: плата при герое — «Отдать»: железо и камни уходят ровно по два, он читает набросок и шлёт к жрецу") {
      val inv = TestInventoryRepository.withItems((sketchItem :: kazimirPay) :+ material(MaterialKind.EverburningIron, 915L))
      for {
        d     <- dao(hero(), afterPowder())
        r     <- TestRenderer.make
        c     <- content
        state  = MentorKazimirState(d, inv, c)
        _     <- state.action(testUser, tap("KazStory"), r)
        ask   <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("KazStoryPay"), r)
        said  <- texts(r)
        q     <- questOf(d)
      } yield assertTrue(ask.choices.exists(_.id == "KazStoryPay")) &&
              assertTrue(said.contains("Вот теперь потолкуем") && said.contains("Это не руна")) &&
              assertTrue(StoryBrandQuest.toPriest(q) && !StoryBrandQuest.priced(q)) &&
              assertTrue(inv.snapshot.map(_.id).toSet == Set(sketchItem.id, 915L))
    },

    test("плата — россыпью и ровно сколько просят: стопка не в счёт") {
      val stack = Bundling.pack(List.fill(Bundling.Size)(MaterialGenerator.item(MaterialKind.MagicStone))).map(_.copy(id = 916L)).toList
      val iron  = kazimirPay.filter(_.material.contains(MaterialKind.EverburningIron))
      val paid  = StoryBrandQuest.payment(kazimirPay :+ material(MaterialKind.EverburningIron, 915L), StoryBrandQuest.KazimirPrice)
      val ids   = paid.map(_.map(_.id).toSet)
      assertTrue(ids.contains(Set(911L, 912L, 913L, 914L))) &&
      assertTrue(stack.nonEmpty && StoryBrandQuest.payment(iron ++ stack, StoryBrandQuest.KazimirPrice).isEmpty) &&
      assertTrue(StoryBrandQuest.payment(bones.take(1), StoryBrandQuest.PriestPrice).isEmpty)
    },

    test("жрец: кнопка на шаге жреца с наброском; сперва цена — две проклятые кости; отдал — об отмеченных, дальше к Юноне") {
      def visit(items: List[Item]) =
        for {
          d     <- dao(hero(), brand(2))
          inv    = TestInventoryRepository.withItems(sketchItem :: items)
          r     <- TestRenderer.make
          c     <- content
          state  = TempleAzatState(d, inv, TestItemRepository.make, c)
          _     <- state.action(testUser, tap("Priest"), r)
          menu  <- r.sentScreens.map(_.last)
          _     <- state.action(testUser, tap("PriestStory"), r)
          ask   <- r.sentScreens.map(_.last)
          _     <- state.action(testUser, tap("PriestStoryPay"), r)
          said  <- texts(r)
          q     <- questOf(d)
        } yield (menu, ask, said, q, inv.snapshot)
      for {
        poor <- visit(Nil)
        rich <- visit(bones)
      } yield assertTrue(poor._1.choices.exists(_.id == "PriestStory")) &&
              assertTrue(poor._2.text.contains("Храм не торгует знанием") && !poor._2.choices.exists(_.id == "PriestStoryPay")) &&
              assertTrue(StoryBrandQuest.toPriest(poor._4) && StoryBrandQuest.priced(poor._4) && !poor._3.contains("Отмеченным не место")) &&
              assertTrue(rich._2.choices.exists(_.id == "PriestStoryPay")) &&
              assertTrue(rich._3.contains("Кости затихают") && rich._3.contains("Отмеченным не место среди мёртвых") && rich._3.contains("Юнону")) &&
              assertTrue(StoryBrandQuest.toJunona(rich._4) && rich._5.map(_.id) == List(sketchItem.id))
    },

    test("Юнона: кнопка в канцелярии на её шаге; сперва пошлина — две шкуры Белого волка; отдал — про Смотрителя по тому, что с ним сделали, и Сойка") {
      def ask(fate: WardenFate, items: List[Item]) =
        for {
          d     <- dao(hero(), brand(3, fate))
          sch   <- TestScheduler.make
          inv    = TestInventoryRepository.withItems(sketchItem :: items)
          r     <- TestRenderer.make
          c     <- content
          state  = HomeState(d, sch, c, inv, TestItemRepository.make)
          _     <- state.action(testUser, tap("HomeOffice"), r)
          menu  <- r.sentScreens.map(_.last)
          _     <- state.action(testUser, tap("HomeJunonaStory"), r)
          price <- r.sentScreens.map(_.last)
          _     <- state.action(testUser, tap("HomeJunonaPay"), r)
          said  <- texts(r)
          q     <- questOf(d)
        } yield (menu, price, said, q, inv.snapshot)
      for {
        poor     <- ask(WardenFate.Killed, Nil)
        killed   <- ask(WardenFate.Killed, hides)
        released <- ask(WardenFate.Released, hides)
        handed   <- ask(WardenFate.Handed, hides)
      } yield assertTrue(poor._1.choices.exists(_.id == "HomeJunonaStory")) &&
              assertTrue(poor._2.text.contains("две шкуры Белого волка") && !poor._2.choices.exists(_.id == "HomeJunonaPay")) &&
              assertTrue(StoryBrandQuest.toJunona(poor._4) && StoryBrandQuest.priced(poor._4) && !poor._3.contains("Зовут Сойкой")) &&
              assertTrue(killed._2.choices.exists(_.id == "HomeJunonaPay")) &&
              assertTrue(killed._3.contains("перерезанным горлом") && released._3.contains("ушёл первым кораблём") &&
                handed._3.contains("Спасибо за Смотрителя")) &&
              assertTrue(List(killed, released, handed).forall { case (_, _, said, q, bag) =>
                said.contains("Пошлина принята") && said.contains("Зовут Сойкой") && StoryBrandQuest.toMeeting(q) &&
                  bag.map(_.id) == List(sketchItem.id) })
    },

    test("карточка и журнал: собеседник назвал цену — строка с ценой; на новом шаге цена ещё не названа") {
      val priced = StoryBrandQuest.namePrice(brand(2))
      val moved  = StoryBrandQuest.step(priced, 3)
      for {
        c       <- content
        plain   <- cardText(brand(2))
        withPay <- cardText(priced)
        jPlain   = QuestJournal.render(c, hero(), ready, brand(2), DailyQuests.empty, None, 0L)
        jPriced  = QuestJournal.render(c, hero(), ready, priced, DailyQuests.empty, None, 0L)
      } yield assertTrue(plain.contains("Спросить жреца в храме Азата") && !plain.contains("проклятые кости")) &&
              assertTrue(withPay.contains("просит две проклятые кости")) &&
              assertTrue(jPlain.contains("спросить жреца в храме Азата") && jPriced.contains("принести жрецу в храме Азата две проклятые кости")) &&
              assertTrue(!StoryBrandQuest.priced(moved))
    },

    test("встреча с Сойкой: с карточки, из города; «Взять страницу» — набросок уходит, страница с новым именем героя в сумке, глава закрыта") {
      for {
        d     <- dao(hero(), brand(4))
        _     <- d.writeReturnState(userId, Some(StateType.GlobalMap))
        inv    = TestInventoryRepository.withItems(List(sketchItem))
        r     <- TestRenderer.make
        c     <- content
        card   = InventoryState(d, inv, TestItemRepository.make, c)
        _     <- openCard(card, sketchItem, r)
        go    <- card.action(testUser, tap("StoryMeetGo"), r)
        state  = StoryMeetingState(d, inv, TestItemRepository.make, c)
        _     <- state.enter(testUser, r)
        meet  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("MeetTake"), r)
        said  <- texts(r)
        q     <- questOf(d)
        out   <- state.action(testUser, tap("MeetLeave"), r)
      } yield assertTrue(go == StateType.StoryMeeting) &&
              assertTrue(meet.text.contains("Клеймо к Клейму тянет") && meet.choices.map(_.id) == List("MeetTake")) &&
              assertTrue(said.contains("моё новое имя: «Ворон»") && said.contains("Кинэт. Ждать") &&
                said.contains("Квест «Что горит на груди» завершён")) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.StoryPage)) &&
                !inv.snapshot.exists(_.questItem.contains(QuestItemKind.StorySketch))) &&
              assertTrue(q.isDone(NpcQuest.StoryBrand) && out == StateType.Tavern)
    },

    test("журнал: глава 3 — сюжетным заданием со своим шагом") {
      for {
        c   <- content
        text = QuestJournal.render(c, hero(), ready, brand(3), DailyQuests.empty, None, 0L)
      } yield assertTrue(text.contains("📌 «Что горит на груди»") && text.contains("спросить Юнону Кинтслев"))
    }
  )
}
