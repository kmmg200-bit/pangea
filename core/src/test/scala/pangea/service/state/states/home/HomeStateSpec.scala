package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.home.{HomeData, HomeRates, House}
import pangea.model.quest.{FirstSteps, NpcQuests}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.stats.{FightStats, ParamsBuff, StatBoost, StatBoosts}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.model.item.{ItemType, Rarity}
import pangea.service.state.states.{GlobalMapState, LootState}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestJournal, TestRenderer, TestScheduler}
import zio.test._
import zio.{Task, ZIO, durationInt}

/** «🏠 Дом»: Юнона Кинтслев и её дома, зачёт старого дома в цену нового, кровать
  * как комната в таверне и «выспался» — больше опыта. */
object HomeStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))
  private def offer(h: House)              = tap("HomeOffer", "h" -> h.key)
  private def buy(h: House, pay: String)   = tap("HomeBuy", "h" -> h.key, "pay" -> pay)

  private def hero(silver: Long = 0L, doubloons: Long = 0L) =
    TestFixtures.hero(userId, StateType.Home).copy(silver = silver, doubloons = doubloons)

  private def make(h: pangea.model.hero.Hero, home: HomeData = HomeData.empty) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- ZIO.when(home != HomeData.empty)(dao.writeHomeData(userId, home.asJson))
      sch <- TestScheduler.make
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (HomeState(dao, sch, c, TestInventoryRepository.accepting, TestItemRepository.make), dao, sch, r)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def heroOf(dao: TestHeroDao) = dao.getHeroByUserId(userId).map(_.get)
  private def last(r: TestRenderer)    = r.sentScreens.map(_.last)
  private def texts(r: TestRenderer)   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private def lootOf(dao: TestHeroDao): Task[Option[LootState.LootData]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[LootState.LootData].toOption))

  override def spec = suite("Дом")(

    test("в городе — кнопка «🏠 Дом» в ряду с Таверной и Гильдией") {
      for {
        dao  <- TestHeroDao.withHero(userId, hero())
        r    <- TestRenderer.make
        c    <- ZIO.attempt(SceneContent.load())
        _    <- GlobalMapState(dao, c).enter(testUser, r)
        city <- last(r)
        row   = (id: String) => city.choices.find(_.id == id).flatMap(_.row)
        out  <- GlobalMapState(dao, c).action(testUser, tap("Home"), r)
      } yield assertTrue(city.choices.exists(ch => ch.id == "Home" && ch.label == "🏠 Дом")) &&
              assertTrue(row("Home") == row("Tavern") && row("Home") == row("Guild")) &&
              assertTrue(out == StateType.Home)
    },

    test("первый раз — листок у ратуши и знакомство с Юноной; дальше — короткое приветствие") {
      for {
        t <- make(hero())
        (state, dao, _, r) = t
        _     <- state.enter(testUser, r)
        first <- last(r)
        met   <- homeOf(dao)
        _     <- state.enter(testUser, r)
        again <- last(r)
      } yield assertTrue(first.text.contains("Юнону Кинтслев") && first.text.contains("ты из искателей") &&
                first.text.contains("Покупка, обустройство, продажа и расширение имущества")) &&
              // она видела, как герой свалился на сторожку, — и подмигивает
              assertTrue(first.text.contains("Она подмигивает")) &&
              assertTrue(met.metJunona && !met.owns) &&
              assertTrue(again.text.contains("Живой — уже хорошо") && !again.text.contains("ратуши")) &&
              // три дома — без цен, четвёртой — «Обустройство»; и «Уйти»
              assertTrue(first.choices.map(_.label) == List(
                "🏠 Домик в две комнаты", "🏠 Добротный дом", "🏠 Ключ от острова", "🛠 Обустройство", "Уйти"))
    },

    test("«Обустройство» без дома: реплика Юноны и «Назад» в канцелярию") {
      for {
        t <- make(hero(), HomeData(metJunona = true))
        (state, _, _, r) = t
        _      <- state.action(testUser, tap("HomeFurnish"), r)
        page   <- last(r)
        _      <- state.action(testUser, tap("HomeOffice"), r)
        office <- last(r)
      } yield assertTrue(page.text.contains("Сперва дом, искатель, потом обстановка.")) &&
              assertTrue(page.choices.map(c => c.id -> c.label) == List("HomeOffice" -> "↩ Назад")) &&
              assertTrue(office.choices.exists(_.id == "HomeFurnish"))
    },

    test("цены с зачётом старого дома: 700 за второй с домиком, 1900 и 1200 за остров; ниже — не продают") {
      import House._
      assertTrue(forSale(None) == List(Cottage, Townhouse, Island)) &&
      assertTrue(forSale(Some(Cottage)) == List(Townhouse, Island) && forSale(Some(Island)).isEmpty) &&
      assertTrue(doubloonPrice(Townhouse, Some(Cottage)) == 700L) &&
      assertTrue(doubloonPrice(Island, Some(Cottage)) == 1900L && doubloonPrice(Island, Some(Townhouse)) == 1200L) &&
      assertTrue(doubloonPrice(Island, None) == 2000L && doubloonPrice(Cottage, None) == 100L) &&
      // за серебро — только домик и только без дома
      assertTrue(silverPrice(Cottage, None).contains(HomeRates.CottageSilver) && silverPrice(Townhouse, None).isEmpty)
    },

    test("покупка в два шага; не хватает — сколько, ничего не списано; купил — дом записан и «логово» в «Встать на ноги»") {
      for {
        t <- make(hero(silver = 15000L), HomeData(metJunona = true))
        (state, dao, _, r) = t
        _      <- state.action(testUser, offer(House.Cottage), r)
        pitch  <- last(r)
        _      <- state.action(testUser, buy(House.Cottage, HomeState.PaySilver), r)
        poor   <- texts(r)
        h1     <- heroOf(dao)
        // добрал серебра — купил
        _      <- dao.updateSilver(userId, 21000L)
        _      <- state.action(testUser, buy(House.Cottage, HomeState.PaySilver), r)
        h2     <- heroOf(dao)
        home   <- homeOf(dao)
        inside <- last(r)
      } yield assertTrue(pitch.text.contains("рунический алтарь") &&
                pitch.text.contains("Подписываем? «Домик в две комнаты» за 20 000 🪙 или 100 🟡.")) &&
              // нажал на дом — цены кнопками и «Назад»
              assertTrue(pitch.choices.map(c => c.id -> c.label) == List(
                "HomeBuy" -> "✍ Подписать за 20 000 🪙", "HomeBuy" -> "✍ Подписать за 100 🟡", "HomeOffice" -> "↩ Назад")) &&
              assertTrue(pitch.choices.filter(_.id == "HomeBuy").map(_.data.get("pay")) ==
                List(Some(HomeState.PaySilver), Some(HomeState.PayDoubloons))) &&
              assertTrue(poor.contains("Не хватает 5 000 🪙") && h1.silver == 15000L) &&
              assertTrue(h2.silver == 1000L && home.house.contains(House.Cottage)) &&
              assertTrue(FirstSteps.progress(h2, NpcQuests.empty, home).house) &&
              // сразу — в дом: кровать, «К Юноне», «Уйти»; библиотеки в домике нет
              assertTrue(inside.text.contains("Ваш домик") && inside.choices.map(_.id) == List("HomeBed", "HomeArmory", "HomeClean", "HomeAltar", "HomeStowAll", "HomeSlots", "HomeOffice", "LeaveHome"))
    },

    test("второй дом с домиком — особая реплика о зачёте и 700 дублонов; старый уходит, библиотека появляется") {
      for {
        t <- make(hero(doubloons = 750L), HomeData(tier = 1, metJunona = true))
        (state, dao, _, r) = t
        _      <- state.action(testUser, tap("HomeOffice"), r)
        office <- last(r)
        _      <- state.action(testUser, offer(House.Townhouse), r)
        pitch  <- last(r)
        _      <- state.action(testUser, buy(House.Townhouse, HomeState.PayDoubloons), r)
        h      <- heroOf(dao)
        home   <- homeOf(dao)
        inside <- last(r)
        // а дом ниже уже не продают — кнопка из старого экрана ничего не купит
        _      <- state.action(testUser, buy(House.Cottage, HomeState.PayDoubloons), r)
        same   <- homeOf(dao)
      } yield assertTrue(office.text.contains("Как дом?") && office.choices.map(_.label) == List(
                "🏠 Добротный дом", "🏠 Ключ от острова", "🛠 Обустройство", "🏠 Домой")) &&
              assertTrue(pitch.choices.map(_.label) == List("✍ Подписать за 700 🟡", "↩ Назад")) &&
              assertTrue(pitch.text.contains("сдать обратно в управление совета") && pitch.text.contains("Выйдет 700") &&
                pitch.text.contains("всё личное тоже") && pitch.text.contains("место, чтобы расшириться")) &&
              assertTrue(pitch.text.contains("Твой «Домик в две комнаты» уходит в управление совета.")) &&
              assertTrue(h.doubloons == 50L && home.house.contains(House.Townhouse)) &&
              assertTrue(inside.choices.map(_.id) == List("HomeBed", "HomeArmory", "HomeLibrary", "HomeAltar", "HomeHerbs", "HomeAzat", "HomeStowAll", "HomeSlots", "HomeOffice", "LeaveHome")) &&
              assertTrue(same.house.contains(House.Townhouse))
    },

    test("остров: рассказ Юноны о ключе-портале; с домом — 1200; купив его, больше купить нечего") {
      for {
        t <- make(hero(doubloons = 1200L), HomeData(tier = 2, metJunona = true))
        (state, dao, _, r) = t
        _      <- state.action(testUser, offer(House.Island), r)
        pitch  <- last(r)
        _      <- state.action(testUser, buy(House.Island, HomeState.PayDoubloons), r)
        h      <- heroOf(dao)
        said   <- texts(r)
        _      <- state.action(testUser, tap("HomeOffice"), r)
        top    <- last(r)
      } yield assertTrue(pitch.text.contains("ключ-портал") && pitch.text.contains("тринадцать с половиной сундуков")) &&
              assertTrue(pitch.text.contains("совет зачтёт его целиком: 1 200")) &&
              assertTrue(h.doubloons == 0L && said.contains("плывут облака")) &&
              assertTrue(top.text.contains("Выше острова") && top.text.contains("среди людей бывают такие ненасытные")) &&
              assertTrue(top.choices.map(_.id) == List("HomeFurnish", "HomeMain"))
    },

    test("кровать как комната в таверне: проспал — травмы прошли, силы вернулись, и «выспался» +10% опыта") {
      val tired = hero().copy(
        fightStats  = FightStats(atk = 10, hp = 1L, armor = 0L, defence = 0, evasion = 0, accuracy = 10, energy = 0L),
        traumaUntil = Some(999999999L), traumaNames = List("Ушиб"))
      for {
        t <- make(tired, HomeData(tier = 1, metJunona = true))
        (state, dao, sch, r) = t
        _      <- state.action(testUser, tap("HomeBed"), r)
        bed    <- last(r)
        _      <- state.action(testUser, tap("HomeSleep"), r)
        asleep <- last(r)
        timer  <- sch.scheduled
        // во сне старые кнопки не работают
        _      <- state.action(testUser, tap("HomeOffice"), r)
        still  <- last(r)
        _      <- TestClock.adjust(2.hours)
        _      <- state.action(testUser, tap(HomeState.WakeKey), r)
        h      <- heroOf(dao)
        said   <- texts(r)
        cancel <- sch.cancelled
      } yield assertTrue(bed.text.contains("продавленным матрасом") && bed.text.contains("Сон — 2 ч")) &&
              assertTrue(asleep.text.startsWith("Вы спите") && asleep.choices.map(_.id) == List("OpenApp", HomeState.WakeKey)) &&
              assertTrue(timer.exists(s => s.kind == TaskKind.HomeSleep && s.expectedState == StateType.Home &&
                s.fireAt == 2L * 3600000L && s.action == HomeState.WakeAction)) &&
              assertTrue(still.text.startsWith("Вы спите")) &&
              assertTrue(h.traumaNames.isEmpty && h.traumaUntil.isEmpty) &&
              assertTrue(h.fightStats.hp == h.effectiveMaxHp(2L * 3600000L) && h.fightStats.armor == h.effectiveMaxArmor(2L * 3600000L)) &&
              assertTrue(h.rested(2L * 3600000L) && h.rested(6L * 3600000L - 1L) && !h.rested(6L * 3600000L)) &&
              assertTrue(said.contains("+10% опыта") && cancel.contains((userId, TaskKind.HomeSleep)))
    },

    test("встать раньше — спросить; «Да» — ни лечения, ни «выспался»") {
      val tired = hero().copy(traumaUntil = Some(999999999L), traumaNames = List("Ушиб"))
      for {
        t <- make(tired, HomeData(tier = 3, metJunona = true))
        (state, dao, _, r) = t
        _     <- state.action(testUser, tap("HomeSleep"), r)
        _     <- TestClock.adjust(20.minutes)
        _     <- state.action(testUser, tap(HomeState.WakeKey), r)
        ask   <- last(r)
        _     <- state.action(testUser, tap("HomeWakeYes"), r)
        h     <- heroOf(dao)
        house <- last(r)
      } yield assertTrue(ask.text.contains("До пробуждения осталось 40 мин") && ask.choices.map(_.id) == List("HomeWakeYes", "HomeWakeNo")) &&
              assertTrue(h.traumaNames == List("Ушиб") && !h.rested(20L * 60000L)) &&
              assertTrue(house.text.contains("на острове"))
    },

    test("«выспался» — +10% к любому опыту (вверх), а с остановленным опытом — ничего") {
      val rested = hero().copy(statBoosts = StatBoosts(List(StatBoost(HomeRates.RestedBoost, ParamsBuff.zero, 1000L))))
      assertTrue(rested.expFrom(100L, 0L) == 110L && rested.expFrom(5L, 0L) == 6L && rested.expFrom(0L, 0L) == 0L) &&
      assertTrue(rested.expFrom(100L, 1000L) == 100L) &&   // срок вышел
      assertTrue(rested.copy(expFrozen = true).expFrom(100L, 0L) == 0L) &&
      assertTrue(rested.getInfo(0L).contains("Выспались дома: +10% опыта") && !hero().getInfo(0L).contains("Выспались"))
    },

    test("библиотека — своя у каждого дома: маленькая в добротном, большая личная на острове, в домике нет") {
      for {
        t2 <- make(hero(), HomeData(tier = 2, metJunona = true))
        (s2, _, _, r2) = t2
        _  <- s2.action(testUser, tap("HomeLibrary"), r2)
        l2 <- last(r2)
        t3 <- make(hero(), HomeData(tier = 3, metJunona = true))
        (s3, _, _, r3) = t3
        _  <- s3.action(testUser, tap("HomeLibrary"), r3)
        l3 <- last(r3)
        t1 <- make(hero(), HomeData(tier = 1, metJunona = true))
        (s1, _, _, r1) = t1
        _  <- s1.action(testUser, tap("HomeLibrary"), r1)
        l1 <- last(r1)
      } yield assertTrue(l2.text.startsWith("Маленькая библиотека")) &&
              assertTrue(l3.text.startsWith("Большая личная библиотека")) &&
              assertTrue(l1.text.contains("Ваш домик"))
    },

    test("алтарь Азата: дом с ним приходит с полной энергией; пополнение +50 за 10 000 серебра, не выше 100") {
      for {
        // купил добротный дом — алтарь полон
        t <- make(hero(doubloons = 700L), HomeData(tier = 1, metJunona = true))
        (state, dao, _, r) = t
        _      <- state.action(testUser, buy(House.Townhouse, HomeState.PayDoubloons), r)
        bought <- homeOf(dao)
        // энергия просела — пополняем
        _      <- dao.writeHomeData(userId, bought.copy(charges = 70).asJson)
        _      <- dao.updateSilver(userId, 25000L)
        _      <- state.action(testUser, tap("HomeAzat"), r)
        screen <- last(r)
        _      <- state.action(testUser, tap("HomeAzatUp"), r)
        topped <- homeOf(dao)
        h1     <- heroOf(dao)
        // полный — второй раз не берёт денег
        _      <- state.action(testUser, tap("HomeAzatUp"), r)
        h2     <- heroOf(dao)
        said   <- texts(r)
        // денег нет — «не хватает»
        _      <- dao.writeHomeData(userId, topped.copy(charges = 10).asJson)
        _      <- dao.updateSilver(userId, 4000L)
        _      <- state.action(testUser, tap("HomeAzatUp"), r)
        poor   <- texts(r)
        low    <- homeOf(dao)
      } yield assertTrue(bought.charges == House.Townhouse.altarCharges && bought.altarMax == 100) &&
              assertTrue(screen.text.contains("растворяются прямо на камне") && screen.text.contains("питает весь дом") &&
                screen.text.contains("Энергия дома: 70/100")) &&
              assertTrue(screen.choices.map(_.label).headOption.contains("✨ Пополнить +50 — 10 000 🪙")) &&
              assertTrue(topped.charges == 100 && h1.silver == 15000L) &&
              assertTrue(h2.silver == 15000L && said.contains("Алтарь и так полон")) &&
              assertTrue(poor.contains("Не хватает 6 000 🪙") && low.charges == 10)
    },

    test("алтарь Азата: в доме — 100, на острове — 300; переезд на остров доливает до 300") {
      for {
        t <- make(hero(doubloons = 1200L), HomeData(tier = 2, metJunona = true, charges = 40))
        (state, dao, _, r) = t
        _      <- state.action(testUser, buy(House.Island, HomeState.PayDoubloons), r)
        moved  <- homeOf(dao)
        // на острове потолок — 300: пополнение упирается в него, а не в 100
        _      <- dao.writeHomeData(userId, moved.copy(charges = 280).asJson)
        _      <- dao.updateSilver(userId, 10000L)
        _      <- state.action(testUser, tap("HomeAzatUp"), r)
        topped <- homeOf(dao)
        screen <- last(r)
      } yield assertTrue(House.values.map(_.altarCharges) == List(0, 100, 300)) &&
              assertTrue(moved.tier == House.Island.tier && moved.charges == 300) &&
              assertTrue(topped.charges == 300 && screen.text.contains("Энергия дома: 300/300"))
    },

    test("места под обстановку: 2 / 4 / 10; в домике одно завалено хламом, в домах выше хлама нет") {
      val cottage = HomeData(tier = 1, metJunona = true)
      for {
        t <- make(hero(), cottage)
        (state, _, _, r) = t
        _     <- state.action(testUser, tap("HomeSlots"), r)
        slots <- last(r)
      } yield assertTrue(House.values.map(_.slots) == List(2, 4, 10)) &&
              assertTrue(cottage.hasClutter && cottage.slotsUsed == 1 && cottage.slotsFree == 1) &&
              assertTrue(cottage.copy(clutterCleared = true).slotsFree == 2) &&
              // переехал — хлам остался в домике
              assertTrue(!cottage.copy(tier = 2).hasClutter && cottage.copy(tier = 2).slotsFree == 4) &&
              assertTrue(slots.text.contains("занято мест: 1 из 2") && slots.text.contains("1. 🗑 Захламление") &&
                slots.text.contains("2. Свободно")) &&
              assertTrue(slots.choices.map(_.id) == List("HomeClean", "HomeFurnish", "HomeMain"))
    },

    test("«Прибраться» — как сон: дом не отпускает; прервался — наубиранное копится; доделал — место свободно") {
      for {
        t <- make(hero(), HomeData(tier = 1, metJunona = true))
        (state, dao, sch, r) = t
        _       <- state.action(testUser, tap("HomeClean"), r)
        working <- last(r)
        timer   <- sch.scheduled.map(_.last)
        // пока убирается — другие кнопки не уводят
        _       <- state.action(testUser, tap("HomeBed"), r)
        locked  <- last(r)
        _       <- TestClock.adjust(1.hour)
        _       <- state.action(testUser, tap(HomeState.CleanStopKey), r)
        paused  <- homeOf(dao)
        said    <- texts(r)
        noFind  <- lootOf(dao)
        // взялся снова — таймер на оставшиеся три часа; он и доделывает
        _       <- state.action(testUser, tap("HomeClean"), r)
        timer2  <- sch.scheduled.map(_.last)
        _       <- TestClock.adjust(3.hours)
        out     <- state.action(testUser, UserAction("", Some(HomeState.CleanStopAction)), r)
        done    <- homeOf(dao)
        end     <- texts(r)
        find    <- lootOf(dao)
      } yield assertTrue(working.text.contains("Вы разбираете хлам") && working.text.contains("4 ч 0 мин") &&
                working.choices.map(_.id) == List("OpenApp", HomeState.CleanStopKey)) &&
              assertTrue(timer.kind == TaskKind.HomeClean && timer.fireAt == 4L * 3600000L &&
                timer.action == HomeState.CleanStopAction) &&
              assertTrue(locked.text.contains("Вы разбираете хлам")) &&
              assertTrue(paused.cleanedMs == 3600000L && paused.hasClutter && said.contains("Осталось разобрать: 3 ч")) &&
              assertTrue(timer2.fireAt == 3600000L + 3L * 3600000L) &&
              assertTrue(!done.hasClutter && done.slotsFree == 2 && end.contains("Хлам разобран")) &&
              // разобрал сам — находка: пурпурная вещь 4-го уровня на экране добычи, потом домой
              assertTrue(noFind.isEmpty && out == StateType.Loot) &&
              assertTrue(find.exists(l => l.items.size == 1 && l.items.forall(i => i.lvl == 4L &&
                i.rarity == Rarity.Violet && ItemType.equippable.contains(i.itemType)) &&
                l.returnState.contains(StateType.Home) && l.header.exists(_.contains("Прежний хозяин припрятал"))))
    },

    test("находка из хлама — на экране добычи со своим текстом") {
      for {
        t <- make(hero(), HomeData(tier = 1, metJunona = true, cleanedMs = HomeRates.ClutterCleanMs - 1000L))
        (state, dao, _, r) = t
        _       <- state.action(testUser, tap("HomeClean"), r)
        _       <- TestClock.adjust(1.second)
        _       <- state.action(testUser, UserAction("", Some(HomeState.CleanStopAction)), r)
        journal <- TestJournal.make
        c       <- ZIO.attempt(SceneContent.load())
        _       <- LootState(dao, TestInventoryRepository.accepting, TestItemRepository.make, journal, c).enter(testUser, r)
        screen  <- last(r)
      } yield assertTrue(screen.text.startsWith("На самом дне, под истлевшими мешками") && screen.text.contains("[Ур.4]")) &&
              assertTrue(screen.choices.exists(_.id == "Take") && screen.choices.exists(_.id == "Leave"))
    },

    test("Юнона: личный посыльный — с домика, за 100 дублонов, одно место; покупка разовая — второго не купить") {
      for {
        t <- make(hero(doubloons = 500L), HomeData(tier = 1, metJunona = true, clutterCleared = true))
        (state, dao, _, r) = t
        _     <- state.action(testUser, tap("HomeFurnItem", "f" -> "courier"), r)
        card  <- last(r)
        _     <- state.action(testUser, tap("HomeFurnBuy", "f" -> "courier"), r)
        once  <- homeOf(dao)
        _     <- state.action(testUser, tap("HomeFurnBuy", "f" -> "courier"), r)
        twice <- homeOf(dao)
        _     <- state.action(testUser, tap("HomeFurnish"), r)
        list  <- last(r)
        _     <- state.action(testUser, tap("HomeFurnItem", "f" -> "courier"), r)
        again <- last(r)
        h     <- heroOf(dao)
        said  <- texts(r)
      } yield assertTrue(card.text.contains("Личный посыльный") && card.text.contains("Займёт одно место в доме")) &&
              assertTrue(card.choices.exists(c => c.id == "HomeFurnBuy" && c.label.contains("100"))) &&
              assertTrue(once.hasCourier && twice.furniture == List("courier") && h.doubloons == 400L) &&
              assertTrue(said.contains("живёт личный посыльный") && said.contains("Посыльный у тебя уже есть")) &&
              // посыльный уже есть — в «Обустройстве» его больше нет, стойка — на месте
              assertTrue(!list.choices.exists(_.data.get("f").contains("courier")) &&
                list.choices.exists(_.data.get("f").contains("rack"))) &&
              // кнопка со старого экрана — карточка без «Купить», и сказано почему
              assertTrue(!again.choices.exists(_.id == "HomeFurnBuy") && again.text.contains("Посыльный у тебя уже есть"))
    },

    test("Юнона: вывезти хлам — за 5 000 серебра; в домике сундуков не продают, только стойку, верстак и посыльного") {
      for {
        t <- make(hero(silver = 6000L), HomeData(tier = 1, metJunona = true))
        (state, dao, _, r) = t
        _      <- state.action(testUser, tap("HomeFurnish"), r)
        page   <- last(r)
        _      <- state.action(testUser, tap("HomeClutterPay"), r)
        home   <- homeOf(dao)
        h      <- heroOf(dao)
        again  <- last(r)
        find   <- lootOf(dao)
      } yield assertTrue(page.text.contains("Мест в доме занято: 1 из 2")) &&
              assertTrue(page.choices.map(_.id) == List("HomeClutterPay", "HomeFurnItem", "HomeFurnItem", "HomeFurnItem", "HomeOffice") &&
                page.choices.head.label == "🧹 Вывезти хлам — 5 000 🪙" &&
                page.choices(1).label == "🧥 Магическая стойка — 100 🟡" &&
                page.choices(2).label == "🛠 Верстак ремесленника — 100 🟡" &&
                page.choices(3).label == "🏃 Личный посыльный — 100 🟡") &&
              assertTrue(!home.hasClutter && h.silver == 1000L) &&
              // вывез за серебро — находки нет
              assertTrue(find.isEmpty) &&
              assertTrue(again.choices.map(_.id) == List("HomeFurnItem", "HomeFurnItem", "HomeFurnItem", "HomeOffice"))
    },

    test("сундук аптекаря — со второго дома, за 100 дублонов, по месту на сундук; мест нет — не продаёт") {
      for {
        t <- make(hero(doubloons = 300L), HomeData(tier = 2, metJunona = true))
        (state, dao, _, r) = t
        _      <- state.action(testUser, tap("HomeFurnish"), r)
        page   <- last(r)
        _      <- state.action(testUser, tap("HomeFurnItem", "f" -> "apothecary"), r)
        card   <- last(r)
        _      <- state.action(testUser, tap("HomeFurnBuy", "f" -> "apothecary"), r)
        home   <- homeOf(dao)
        h      <- heroOf(dao)
        // все четыре места заняты — пятый не встанет
        _      <- dao.writeHomeData(userId, home.copy(furniture = List.fill(4)("apothecary")).asJson)
        _      <- state.action(testUser, tap("HomeFurnBuy", "f" -> "apothecary"), r)
        full   <- homeOf(dao)
        h2     <- heroOf(dao)
        said   <- texts(r)
        // сундук ювелира — только на острове
        _      <- state.action(testUser, tap("HomeFurnBuy", "f" -> "jeweler"), r)
        h3     <- heroOf(dao)
      } yield assertTrue(page.choices.map(_.id) == List("HomeFurnItem", "HomeFurnItem", "HomeFurnItem", "HomeFurnItem", "HomeOffice") &&
                page.choices(3).label == "📦 Сундук аптекаря — 100 🟡") &&
              assertTrue(card.text.contains("на 100 больше, отваров — на 50") && card.text.contains("Свободных мест в доме: 4.")) &&
              assertTrue(home.furniture == List("apothecary") && h.doubloons == 200L) &&
              assertTrue(home.herbShelves == 150 && home.brewShelves == 75) &&
              assertTrue(full.furniture.size == 4 && h2.doubloons == 200L && said.contains("Ставить некуда")) &&
              assertTrue(h3.doubloons == 200L)
    },

    test("остров: полки верстака 150 / 75 по умолчанию; роскошный сундук ювелира — +100 камней за каждый") {
      val island = HomeData(tier = 3, metJunona = true)
      for {
        t <- make(hero(doubloons = 100L), island)
        (state, dao, _, r) = t
        _    <- state.action(testUser, tap("HomeFurnBuy", "f" -> "jeweler"), r)
        home <- homeOf(dao)
      } yield assertTrue(island.herbShelves == 150 && island.brewShelves == 75 && island.gemShelves == 50) &&
              assertTrue(island.copy(furniture = List("apothecary", "jeweler", "jeweler")).herbShelves == 250 &&
                island.copy(furniture = List("jeweler", "jeweler")).gemShelves == 250) &&
              assertTrue(home.furniture == List("jeweler") && home.gemShelves == 150 && home.slotsFree == 9)
    },

    test("сон и «выспался» — по дому: 2 / 1,5 / 1 ч и 4 / 8 / 12 ч") {
      assertTrue(House.values.map(h => (h.sleepMs / 60000L, h.restedMs / 3600000L)) == List((120L, 4L), (90L, 8L), (60L, 12L))) &&
      assertTrue(HomeState.duration(House.Townhouse.sleepMs) == "1 ч 30 мин" && HomeState.money(20000L) == "20 000")
    }
  )
}
