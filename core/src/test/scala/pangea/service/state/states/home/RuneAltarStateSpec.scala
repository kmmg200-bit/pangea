package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.{ChoiceColor, SceneContent}
import pangea.model.home.HomeData
import pangea.model.item.{Item, ItemDetails, ItemType, PassiveKind, Rarity}
import pangea.model.rune.{Rune, RuneData, RuneStone, RuneStoneSize}
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Рунический алтарь: камни ложатся одной кнопкой и становятся счётом по рунам
  * (большая — пять, малая — одна); углубить понимание, как у Казимира; забрать
  * пятёрками — большими, остаток — малыми. */
object RuneAltarStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private val cunning = Rune.Active(Skill.CunningStrike)
  private val healer  = Rune.Passive(PassiveKind.Healer)

  private def stone(id: Long, rune: Rune, size: RuneStoneSize): Item = RuneStone.item(rune, size).copy(id = id)
  private def sword(id: Long): Item =
    Item(id, s"Меч $id", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0,
      evasion = 0, details = ItemDetails.Weapon(Skill.CunningStrike))

  /** Кольцо с пассивкой «Целитель»: пассивная руна. */
  private def ring(id: Long, rarity: Rarity): Item =
    Item(id, s"Кольцо $id", 1L, rarity, ItemType.Ring, attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0,
      evasion = 0, details = ItemDetails.Passive(PassiveKind.Healer))

  private def hero(lvl: Long = 1L, runes: RuneData = RuneData.empty) =
    TestFixtures.hero(userId, StateType.RuneAltar).copy(lvl = lvl, runes = runes)

  private def make(h: pangea.model.hero.Hero, items: List[Item], home: HomeData) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeHomeData(userId, home.asJson)
      inv  = TestInventoryRepository.withItems(items)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (RuneAltarState(dao, inv, TestItemRepository.make, c), dao, inv, r)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def last(r: TestRenderer)  = r.sentScreens.map(_.last)
  /** Число, написанное игроком в ответ на вопрос алтаря. */
  private def typed(text: String): UserAction = UserAction(text, None)
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private val cottage = HomeData(tier = 1, metJunona = true)

  override def spec = suite("Рунический алтарь")(

    test("внизу — одни значки, а что они значат, сказано текстом перед счётом узоров") {
      for {
        t      <- make(hero(), Nil, cottage)
        (state, _, _, r) = t
        _      <- state.enter(testUser, r)
        screen <- last(r)
        labels  = screen.choices.filter(c => Set("AltarPutAll", "AltarGive", "AltarSettings").contains(c.id)).map(_.label)
        legend  = screen.text.indexOf("⬇ — положить")
      } yield assertTrue(labels == List("⬇", "🔥", "⚙")) &&
              assertTrue(legend > 0 && legend < screen.text.indexOf("Алтарь пуст") &&
                         screen.text.contains("🔥 — сжечь вещи с рунами") && screen.text.contains("⚙ — настройки"))
    },

    test("«Сдать вещи»: вещи с рунами отмеченных редкостей — узорами по редкости; камни и неотмеченные остаются") {
      // по умолчанию отмечены серые и белые — та же настройка, что у Казимира
      val items = List(sword(201L), sword(202L).copy(rarity = Rarity.White), sword(203L).copy(rarity = Rarity.Blue),
        ring(204L, Rarity.Gray), stone(205L, cunning, RuneStoneSize.Small))
      for {
        t <- make(hero(), items, cottage.copy(runes = Map(cunning.key -> 3L)))
        (state, dao, inv, r) = t
        _      <- state.enter(testUser, r)
        menu   <- last(r)
        _      <- state.action(testUser, tap("AltarGive"), r)
        ask    <- last(r)
        before <- homeOf(dao)
        _      <- state.action(testUser, tap("AltarGiveYes"), r)
        home   <- homeOf(dao)
        said   <- texts(r)
      } yield assertTrue(menu.choices.map(_.id).containsSlice(List("AltarPutAll", "AltarGive", "AltarSettings", "LeaveAltar"))) &&
              // сперва — вопрос: сколько уйдёт, и ничего ещё не тронуто
              assertTrue(ask.text.contains("3 шт.") && ask.choices.map(_.id) == List("AltarGiveYes", "AltarMenu")) &&
              assertTrue(before.runeCount(cunning.key) == 3L && before.runeCount(healer.key) == 0L) &&
              // два серо-белых меча — по узору «Хитрому удару», серое кольцо — узор «Целителю»
              assertTrue(home.runeCount(cunning.key) == 3L + 2L && home.runeCount(healer.key) == 1L) &&
              // синий меч не отмечен, камень — для «Положить все руны»
              assertTrue(inv.snapshot.map(_.id).sorted == List(203L, 205L)) &&
              assertTrue(said.contains("Вещи рассыпаются узорами по алтарю (3 шт.)"))
    },

    test("«Что сдавать» — та же настройка, что у Казимира: включили синие и оранжевые — 2 и 5 узоров") {
      for {
        t <- make(hero(), List(sword(301L).copy(rarity = Rarity.Blue), ring(302L, Rarity.Orange)), cottage)
        (state, dao, inv, r) = t
        _      <- state.action(testUser, tap("AltarSettings"), r)
        screen <- last(r)
        _      <- state.action(testUser, tap("AltarBurnRarity", "g" -> "Blue"), r)
        _      <- state.action(testUser, tap("AltarBurnRarity", "g" -> "Orange"), r)
        h      <- dao.getHeroByUserId(userId).map(_.get)
        _      <- state.action(testUser, tap("AltarGiveYes"), r)
        home   <- homeOf(dao)
      } yield assertTrue(screen.text.contains("та же настройка, что у Казимира") && screen.choices.size == 8 &&
              screen.choices.exists(_.id == "AltarBurnSets")) &&
              assertTrue(h.runes.burns(Rarity.Blue) && h.runes.burns(Rarity.Orange) && h.runes.burns(Rarity.Gray)) &&
              assertTrue(home.runeCount(cunning.key) == 2L && home.runeCount(healer.key) == 5L && inv.snapshot.isEmpty)
    },

    test("«Сдать вещи»: подходящих нет — так и говорит, ничего не трогает") {
      val items = List(sword(401L).copy(rarity = Rarity.Blue), stone(402L, cunning, RuneStoneSize.Big))
      for {
        t <- make(hero(), items, cottage)
        (state, dao, inv, r) = t
        _    <- state.action(testUser, tap("AltarGive"), r)
        _    <- state.action(testUser, tap("AltarGiveYes"), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield assertTrue(said.contains("В сумке нет вещей с рунами")) &&
              assertTrue(home.runes.isEmpty && inv.snapshot.map(_.id) == List(401L, 402L))
    },

    test("«Положить все руны»: камни уходят из сумки, на алтаре — счёт по рунам; вещи с рунами остаются") {
      val items = List(stone(101L, cunning, RuneStoneSize.Big), stone(102L, cunning, RuneStoneSize.Small),
        stone(103L, cunning, RuneStoneSize.Small), stone(104L, healer, RuneStoneSize.Big), sword(105L))
      for {
        t <- make(hero(), items, cottage)
        (state, dao, inv, r) = t
        _    <- state.enter(testUser, r)
        menu <- last(r)
        _    <- state.action(testUser, tap("AltarPutAll"), r)
        home <- homeOf(dao)
        said <- texts(r)
        list <- last(r)
        // второй раз класть нечего
        _    <- state.action(testUser, tap("AltarPutAll"), r)
        none <- texts(r)
      } yield assertTrue(menu.text.contains("Алтарь пуст")) &&
              assertTrue(home.runes == Map(cunning.key -> 7L, healer.key -> 5L)) &&
              assertTrue(inv.snapshot.map(_.id) == List(105L)) &&      // меч с руной не взят
              assertTrue(said.contains("Алтарь вбирает узоры") && said.contains("• Хитрый удар: +7")) &&
              assertTrue(list.text.contains("Узоров на алтаре: 12")) &&
              // руны — по алфавиту
              assertTrue(list.choices.filter(_.id == "AltarRune").map(_.label) == List("Хитрый удар — 7", "Целитель — 5")) &&
              assertTrue(none.contains("В сумке нет рунных камней"))
    },

    test("экран руны и «углубить» — как у Казимира: до потолка по уровню, остаток ждёт на алтаре") {
      // потолок на 1-м уровне — 30; понимание уже 25, на алтаре 12
      val h = hero(lvl = 1L, runes = RuneData.empty.deepen(cunning, 25L, Rune.cap(1L))._1)
      for {
        t <- make(h, Nil, cottage.withRune(cunning.key, 12L))
        (state, dao, _, r) = t
        _      <- state.action(testUser, tap("AltarRune", "k" -> cunning.key, "rp" -> "0"), r)
        screen <- last(r)
        _      <- state.action(testUser, tap("AltarDeepen", "k" -> cunning.key, "rp" -> "0"), r)
        asked  <- last(r)
        _      <- state.action(testUser, typed("100"), r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
        home   <- homeOf(dao)
        said   <- texts(r)
        // у потолка — узоры остаются, понимание не растёт
        _      <- state.action(testUser, tap("AltarDeepen", "k" -> cunning.key, "rp" -> "0"), r)
        _      <- state.action(testUser, typed("3"), r)
        atCap  <- texts(r)
        home2  <- homeOf(dao)
      } yield assertTrue(screen.text.contains("На алтаре: 12. В сумку выйдет больших рун — 2, малых — 2.")) &&
              assertTrue(screen.text.contains("Понимание: 25/30")) &&
              assertTrue(screen.choices.map(_.id) == List("AltarDeepen", "AltarDeepenAll", "AltarTake", "AltarMenu")) &&
              // сперва спрашивает, сколько; больше, чем лежит, — берёт сколько есть
              assertTrue(asked.text.contains("Сколько узоров") && asked.text.contains("Напишите число")) &&
              assertTrue(after.runes.understandingOf(cunning) == 30L && home.runeCount(cunning.key) == 7L) &&
              assertTrue(said.contains("стало глубже: 30 (+5). На алтаре осталось: 7.")) &&
              assertTrue(atCap.contains("насколько позволяет опыт (30)") && home2.runeCount(cunning.key) == 7L)
    },

    test("забрать сколько сказали: 6 из 13 — одна большая и одна малая, 7 ждут на алтаре; не число — переспросит") {
      for {
        t <- make(hero(), Nil, cottage.withRune(cunning.key, 13L))
        (state, dao, inv, r) = t
        _     <- state.action(testUser, tap("AltarTake", "k" -> cunning.key, "rp" -> "0"), r)
        _     <- state.action(testUser, typed("много"), r)
        still <- homeOf(dao)
        again <- last(r)
        _     <- state.action(testUser, typed("6"), r)
        home  <- homeOf(dao)
        got    = inv.snapshot.flatMap(_.runeStone)
        said  <- texts(r)
      } yield assertTrue(said.contains("Нужно число больше нуля") && still.runeCount(cunning.key) == 13L) &&
              assertTrue(again.text.contains("Сколько узоров")) &&
              assertTrue(got.count(_.size == RuneStoneSize.Big) == 1 && got.count(_.size == RuneStoneSize.Small) == 1) &&
              assertTrue(home.runeCount(cunning.key) == 7L)
    },

    test("забрать: пятёрки — большими, остаток — малыми; места мало — берёт, что влезло, остальное на алтаре") {
      for {
        t <- make(hero(), Nil, cottage.withRune(cunning.key, 13L))
        (state, dao, inv, r) = t
        _    <- state.action(testUser, tap("AltarTake", "k" -> cunning.key, "rp" -> "0"), r)
        _    <- state.action(testUser, typed("13"), r)
        home <- homeOf(dao)
        got   = inv.snapshot.flatMap(_.runeStone)
        said <- texts(r)
        // сумка почти полна: под большие — одно место
        t2 <- make(hero(), (1L to 19L).map(sword).toList, cottage.withRune(cunning.key, 13L))
        (state2, dao2, inv2, r2) = t2
        _     <- state2.action(testUser, tap("AltarTake", "k" -> cunning.key, "rp" -> "0"), r2)
        _     <- state2.action(testUser, typed("13"), r2)
        home2 <- homeOf(dao2)
        got2   = inv2.snapshot.flatMap(_.runeStone)
        said2 <- texts(r2)
      } yield assertTrue(got.count(_.size == RuneStoneSize.Big) == 2 && got.count(_.size == RuneStoneSize.Small) == 3) &&
              assertTrue(got.forall(_.runeKey == cunning.key) && inv.snapshot.forall(_.id > 0L)) &&
              assertTrue(home.runeCount(cunning.key) == 0L && !home.runes.contains(cunning.key)) &&
              assertTrue(said.contains("В сумку легли: больших рун — 2, малых — 3.")) &&
              assertTrue(got2.count(_.size == RuneStoneSize.Big) == 1 && got2.count(_.size == RuneStoneSize.Small) == 3) &&
              assertTrue(home2.runeCount(cunning.key) == 5L && said2.contains("осталось на алтаре: 5"))
    },

    test("экран руны: что она делает и сила понимания; руна не в деле — без пометки") {
      val h = hero(lvl = 10L, runes = RuneData.empty.deepen(cunning, 250L, Rune.cap(10L))._1)
      for {
        t <- make(h, Nil, cottage.withRune(cunning.key, 3L))
        (state, _, _, r) = t
        _      <- state.action(testUser, tap("AltarRune", "k" -> cunning.key, "rp" -> "0"), r)
        screen <- last(r)
      } yield assertTrue(screen.text.startsWith("Хитрый удар\n" + cunning.describeFor(h) + "\n\n")) &&
              assertTrue(screen.text.contains("Понимание: 250/300, сила " + Rune.strengthText(250L) + ".")) &&
              assertTrue(!screen.text.contains("✅"))
    },

    test("«Углубить полностью»: без вопроса — сколько примет понимание до потолка, остаток ждёт; у потолка — так и говорит") {
      // потолок на 1-м уровне — 30; понимание уже 25, на алтаре 12
      val h = hero(lvl = 1L, runes = RuneData.empty.deepen(cunning, 25L, Rune.cap(1L))._1)
      for {
        t <- make(h, Nil, cottage.withRune(cunning.key, 12L))
        (state, dao, _, r) = t
        _      <- state.action(testUser, tap("AltarDeepenAll", "k" -> cunning.key, "rp" -> "0"), r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
        home   <- homeOf(dao)
        said   <- texts(r)
        back   <- last(r)
        _      <- state.action(testUser, tap("AltarDeepenAll", "k" -> cunning.key, "rp" -> "0"), r)
        atCap  <- texts(r)
        home2  <- homeOf(dao)
      } yield assertTrue(!said.contains("Напишите число")) &&
              assertTrue(after.runes.understandingOf(cunning) == 30L && home.runeCount(cunning.key) == 7L) &&
              assertTrue(said.contains("стало глубже: 30 (+5). На алтаре осталось: 7.")) &&
              // остаток есть — снова экран руны
              assertTrue(back.choices.exists(_.id == "AltarDeepenAll")) &&
              assertTrue(atCap.contains("насколько позволяет опыт (30)") && home2.runeCount(cunning.key) == 7L)
    },

    test("«Углубить полностью»: узоров меньше, чем места до потолка, — уходят все, и алтарь больше ничего не ждёт") {
      // потолок на 2-м уровне — 60; понимания нет, на алтаре 12
      for {
        t <- make(hero(lvl = 2L), Nil, cottage.withRune(cunning.key, 12L))
        (state, dao, _, r) = t
        // сперва нажали «Углубить» (алтарь ждёт число), потом передумали — «полностью»
        _     <- state.action(testUser, tap("AltarDeepen", "k" -> cunning.key, "rp" -> "0"), r)
        _     <- state.action(testUser, tap("AltarDeepenAll", "k" -> cunning.key, "rp" -> "0"), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        home  <- homeOf(dao)
        menu  <- last(r)
        // число после этого — уже не ответ алтарю
        _     <- state.action(testUser, typed("5"), r)
        again <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(after.runes.understandingOf(cunning) == 12L && !home.runes.contains(cunning.key)) &&
              assertTrue(menu.text.contains("Алтарь пуст")) &&
              assertTrue(again.runes.understandingOf(cunning) == 12L)
    },

    test("руны в деле — с ✅ и зелёные: выжженная на теле и на надетой вещи; на экране руны — где она") {
      val other = Rune.Active(Skill.values.filterNot(_ == Skill.CunningStrike).head)
      // меч с «Хитрым ударом» надет, «Целитель» выжжен на теле
      val h = hero(runes = RuneData(passive = List(PassiveKind.Healer.entryName)))
        .copy(equipment = TestFixtures.emptyEquipment.copy(weapon = sword(1L)))
      val altar = cottage.withRune(cunning.key, 7L).withRune(healer.key, 5L).withRune(other.key, 3L)
      for {
        t <- make(h, Nil, altar)
        (state, _, _, r) = t
        _      <- state.enter(testUser, r)
        menu   <- last(r)
        _      <- state.action(testUser, tap("AltarRune", "k" -> cunning.key, "rp" -> "0"), r)
        onItem <- last(r)
        _      <- state.action(testUser, tap("AltarRune", "k" -> healer.key, "rp" -> "0"), r)
        onBody <- last(r)
        runes   = menu.choices.filter(_.id == "AltarRune")
        byKey   = runes.map(c => c.data("k") -> c).toMap
      } yield assertTrue(byKey(cunning.key).label == "✅ Хитрый удар — 7" && byKey(healer.key).label == "✅ Целитель — 5") &&
              assertTrue(byKey(other.key).label == s"${other.label} — 3") &&
              assertTrue(byKey(cunning.key).color == ChoiceColor.Positive && byKey(healer.key).color == ChoiceColor.Positive &&
                         byKey(other.key).color == ChoiceColor.Primary) &&
              assertTrue(menu.text.contains("✅ — руна у вас в деле")) &&
              assertTrue(onItem.text.startsWith("✅ Хитрый удар (на надетой вещи)\n")) &&
              assertTrue(onBody.text.startsWith("✅ Целитель (выжжена на теле)\n"))
    },

    test("ни одной руны в деле на странице — строки про ✅ над счётом нет") {
      for {
        t <- make(hero(), Nil, cottage.withRune(cunning.key, 3L))
        (state, _, _, r) = t
        _    <- state.enter(testUser, r)
        menu <- last(r)
      } yield assertTrue(!menu.text.contains("✅") && menu.choices.filter(_.id == "AltarRune").map(_.label) == List("Хитрый удар — 3"))
    },

    test("«Назад» с экрана руны — на ту страницу списка, с которой её выбрали") {
      for {
        t <- make(hero(), Nil, cottage.withRune(cunning.key, 3L))
        (state, _, _, r) = t
        _      <- state.action(testUser, tap("AltarRune", "k" -> cunning.key, "rp" -> "2"), r)
        screen <- last(r)
      } yield assertTrue(screen.choices.find(_.id == "AltarMenu").exists(_.data == Map("p" -> "2"))) &&
              assertTrue(screen.choices.filter(_.id != "AltarMenu").forall(_.data.get("rp").contains("2")))
    }
  )
}
