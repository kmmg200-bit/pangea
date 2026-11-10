package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.LoreData
import pangea.model.home.{HomeData, HomeRates}
import pangea.model.item.{BrewKind, Item, MaterialKind}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{HerbLore, UserAction}
import pangea.test.{TestArtifactRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Верстак травника: травы на полки (из сумки и Живой сумки, до 50), котелок
  * варит, как магия Живой сумки, и тратит энергию дома, сваренное — на полку
  * отваров (до 25). */
object HerbWorkbenchSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def herb(id: Long, kind: MaterialKind): Item = MaterialGenerator.item(kind).copy(id = id)
  private def bottle(id: Long, kind: BrewKind): Item  = BrewKind.item(kind).copy(id = id)
  /** Ромашка, календула, крапива — костоправный отвар, две бутылки. */
  private def boneSet(from: Long): List[Item] =
    List(herb(from, MaterialKind.Chamomile), herb(from + 1, MaterialKind.Calendula), herb(from + 2, MaterialKind.Nettle))

  private val townhouse = HomeData(tier = 2, metJunona = true, charges = 100)

  /** Что игрок написал у верстака — без кнопки. */
  private def say(text: String): UserAction = UserAction(text, None)

  /** Отвары, которые герой уже варил. */
  private def learned(dao: TestHeroDao, kinds: BrewKind*): Task[Unit] =
    HerbLore.writeLore(dao, userId, LoreData(brewed = kinds.map(_.entryName).toList))

  private def recipe(kind: BrewKind): UserAction = tap("WbRecipe", "k" -> kind.entryName, "p" -> "0")

  private def make(home: HomeData, items: List[Item] = Nil, living: List[Item] = Nil) =
    for {
      dao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.HerbWorkbench))
      _   <- dao.writeHomeData(userId, home.asJson)
      inv  = TestInventoryRepository.withItems(items)
      art  = TestArtifactRepository.of(bag = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1, items = living))
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (HerbWorkbenchState(dao, inv, TestItemRepository.make, c, Some(art)), dao, inv, art, r)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def last(r: TestRenderer)  = r.sentScreens.map(_.last)
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Верстак травника")(

    test("«Изученные»: только то, что уже варили, и только из трав; по алфавиту; пусто — так и говорит") {
      val notHerbal = BrewKind.values.find(k => !k.recipe.forall(_.isHerb)).get
      for {
        t <- make(townhouse)
        (state, dao, _, _, r) = t
        _     <- state.action(testUser, tap("WbKnown"), r)
        empty <- last(r)
        _     <- learned(dao, BrewKind.BoneSetter, BrewKind.LivingWater, notHerbal)
        _     <- state.action(testUser, tap("WbMenu"), r)
        menu  <- last(r)
        _     <- state.action(testUser, tap("WbKnown"), r)
        list  <- last(r)
      } yield assertTrue(empty.text.contains("Изученных отваров пока нет")) &&
              assertTrue(menu.choices.exists(_.id == "WbKnown")) &&
              assertTrue(list.choices.filter(_.id == "WbRecipe").map(_.label) ==
                List(BrewKind.LivingWater.label, BrewKind.BoneSetter.label))
    },

    test("выбрал отвар — ждёт числа; число — варит с полок: травы и энергия уходят, отвары встают на полку") {
      val home = townhouse.copy(herbs = boneSet(1L) ++ boneSet(4L) :+ herb(7L, MaterialKind.Sage))
      for {
        t <- make(home)
        (state, dao, _, _, r) = t
        _     <- learned(dao, BrewKind.BoneSetter)
        _     <- state.action(testUser, recipe(BrewKind.BoneSetter), r)
        card  <- last(r)
        _     <- state.action(testUser, say("3"), r)
        after <- homeOf(dao)
        said  <- texts(r)
        again <- last(r)
      } yield assertTrue(card.text.contains(BrewKind.BoneSetter.label) &&
                card.text.contains("Рецепт: Ромашка, Календула, Крапива.") &&
                card.text.contains("Из одной варки — 2 шт.") && card.text.contains("хватит на 4 шт.") &&
                card.text.contains("Напишите число")) &&
              // три отвара — две варки: из одной выходит по два
              assertTrue(after.brews.flatMap(_.brew) == List.fill(4)(BrewKind.BoneSetter)) &&
              assertTrue(after.herbs.flatMap(_.material) == List(MaterialKind.Sage) && after.charges == 98) &&
              assertTrue(said.contains("Готово: «Костоправный отвар» ×4. Варок: 2, энергии ушло: 2.") &&
                said.contains("поэтому вышло 4, а не 3")) &&
              // карточка снова ждёт числа — можно сварить ещё
              assertTrue(again.text.contains("Напишите число"))
    },

    test("не хватает — говорит, чего и сколько; ничего не трогает") {
      val home = townhouse.copy(herbs = boneSet(1L), charges = 1)
      for {
        t <- make(home)
        (state, dao, _, _, r) = t
        _     <- learned(dao, BrewKind.BoneSetter)
        _     <- state.action(testUser, recipe(BrewKind.BoneSetter), r)
        _     <- state.action(testUser, say("6"), r)
        after <- homeOf(dao)
        said  <- texts(r)
      } yield assertTrue(after == home) &&
              assertTrue(said.contains("Для 6 шт. не хватает:\n" +
                "• Календула — ещё 2 (есть 1 из 3)\n" +
                "• Крапива — ещё 2 (есть 1 из 3)\n" +
                "• Ромашка — ещё 2 (есть 1 из 3)\n" +
                "• Энергия дома — ещё 2 (есть 1 из 3)"))
    },

    test("не число — просит число; любая другая кнопка снимает ожидание") {
      val home = townhouse.copy(herbs = boneSet(1L))
      for {
        t <- make(home)
        (state, dao, _, _, r) = t
        _     <- learned(dao, BrewKind.BoneSetter)
        _     <- state.action(testUser, recipe(BrewKind.BoneSetter), r)
        _     <- state.action(testUser, say("много"), r)
        asked <- texts(r)
        _     <- state.action(testUser, tap("WbMenu"), r)
        _     <- state.action(testUser, say("2"), r)
        after <- homeOf(dao)
        shown <- last(r)
      } yield assertTrue(asked.contains("Напишите число — сколько отваров приготовить.")) &&
              // число после ухода с карточки ничего не варит — это просто верстак
              assertTrue(after == home && shown.text.contains("Верстак травника"))
    },

    test("перегонка: основа — с полки отваров, с варки — одна бутылка; основы мало — так и говорит") {
      val home = townhouse.copy(herbs = List(herb(1L, MaterialKind.Wormwood), herb(2L, MaterialKind.Sage),
        herb(3L, MaterialKind.Wormwood), herb(4L, MaterialKind.Sage)), brews = List(bottle(10L, BrewKind.Schnapps)))
      for {
        t <- make(home)
        (state, dao, _, _, r) = t
        _     <- learned(dao, BrewKind.Moonshine)
        _     <- state.action(testUser, recipe(BrewKind.Moonshine), r)
        card  <- last(r)
        _     <- state.action(testUser, say("2"), r)
        short <- texts(r)
        _     <- state.action(testUser, say("1"), r)
        after <- homeOf(dao)
      } yield assertTrue(card.text.contains("Основа: «Шнапс из красавки» с полки отваров.") &&
                card.text.contains("Из одной варки — 1 шт.") && card.text.contains("хватит на 1 шт.")) &&
              assertTrue(short.contains("• «Шнапс из красавки» — ещё 1 (есть 1 из 2)")) &&
              assertTrue(after.brews.flatMap(_.brew) == List(BrewKind.Moonshine)) &&
              assertTrue(after.herbs.flatMap(_.material) == List(MaterialKind.Wormwood, MaterialKind.Sage))
    },

    test("неизученный отвар кнопкой не выбрать — верстак показывает список изученных") {
      for {
        t <- make(townhouse)
        (state, dao, _, _, r) = t
        _    <- learned(dao, BrewKind.BoneSetter)
        _    <- state.action(testUser, recipe(BrewKind.LivingWater), r)
        list <- last(r)
        _    <- state.action(testUser, say("2"), r)
        menu <- last(r)
      } yield assertTrue(list.text.contains("Изученные отвары")) &&
              assertTrue(menu.text.contains("Верстак травника"))
    },

    test("верстак — со второго дома: в домике его нет, и герой возвращается в дом") {
      for {
        t <- make(HomeData(tier = 1, metJunona = true))
        (state, _, _, _, r) = t
        out <- state.action(testUser, tap("WbMenu"), r)
      } yield assertTrue(out == StateType.Home)
    },

    test("«Сложить травы»: из сумки и из Живой сумки, только травы и не больше, чем держат полки") {
      val almostFull = townhouse.copy(herbs = (1L to 48L).map(i => herb(1000L + i, MaterialKind.Sage)).toList)
      for {
        t <- make(almostFull, items = List(herb(201L, MaterialKind.Nettle), bottle(202L, BrewKind.BoneSetter)),
                  living = List(herb(301L, MaterialKind.Chamomile), herb(302L, MaterialKind.Calendula)))
        (state, dao, inv, art, r) = t
        _    <- state.action(testUser, tap("WbPutHerbs"), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield assertTrue(home.herbs.size == HomeRates.HerbShelves) &&
              // из сумки ушла крапива, склянка осталась; из Живой сумки — ромашка, календула не влезла
              assertTrue(inv.snapshot.map(_.id) == List(202L)) &&
              assertTrue(art.snapshot.bag.items.data.map(_.id) == List(302L)) &&
              assertTrue(said.contains("На полки легло трав: 2.") && said.contains("Не поместилось: 1"))
    },

    test("трава с полки — в котелок (до девяти) или в сумку") {
      val home = townhouse.copy(herbs = boneSet(10L))
      for {
        t <- make(home)
        (state, dao, inv, _, r) = t
        _      <- state.action(testUser, tap("WbHerb", "id" -> "10", "p" -> "0"), r)
        screen <- last(r)
        _      <- state.action(testUser, tap("WbHerbPot", "id" -> "10", "p" -> "0"), r)
        _      <- state.action(testUser, tap("WbHerbBag", "id" -> "11", "p" -> "0"), r)
        after  <- homeOf(dao)
        // котелок полон — не кладётся
        t2 <- make(townhouse.copy(herbs = boneSet(20L), cauldron = (1L to 9L).map(i => herb(900L + i, MaterialKind.Sage)).toList))
        (state2, dao2, _, _, r2) = t2
        _      <- state2.action(testUser, tap("WbHerbPot", "id" -> "20", "p" -> "0"), r2)
        full   <- homeOf(dao2)
        said2  <- texts(r2)
      } yield assertTrue(screen.text.contains("на полках 1") && screen.choices.map(_.id) == List("WbHerbPot", "WbHerbBag", "WbMenu")) &&
              assertTrue(after.cauldron.map(_.id) == List(10L) && after.herbs.map(_.id) == List(12L)) &&
              assertTrue(inv.snapshot.map(_.id) == List(11L)) &&
              assertTrue(full.cauldron.size == 9 && full.herbs.size == 3 && said2.contains("В котелке нет места"))
    },

    test("котелок варит: тратит энергию дома, сваренное встаёт на полку отваров; «Зельевар» считает сваренное") {
      for {
        t <- make(townhouse.copy(cauldron = boneSet(30L)))
        (state, dao, _, _, r) = t
        _    <- state.action(testUser, tap("WbCook"), r)
        home <- homeOf(dao)
        said <- texts(r)
        lore <- pangea.service.state.HerbLore.readLore(dao, userId)
      } yield assertTrue(home.cauldron.isEmpty && home.charges == 99) &&
              assertTrue(home.brews.flatMap(_.brew) == List(BrewKind.BoneSetter, BrewKind.BoneSetter)) &&
              assertTrue(home.brews.forall(_.id > 0L)) &&
              assertTrue(said.contains("Сварено: Костоправный отвар ×2 — готовое встало на полку отваров. Варок: 1, энергии ушло: 1.")) &&
              assertTrue(lore.brewed.contains(BrewKind.BoneSetter.entryName))
    },

    test("без энергии дома котелок не варит; не сходится рецепт — так и говорит") {
      for {
        t <- make(townhouse.copy(charges = 0, cauldron = boneSet(40L)))
        (state, dao, _, _, r) = t
        _     <- state.action(testUser, tap("WbCook"), r)
        dry   <- homeOf(dao)
        said  <- texts(r)
        t2 <- make(townhouse.copy(cauldron = List(herb(50L, MaterialKind.Sage))))
        (state2, dao2, _, _, r2) = t2
        _     <- state2.action(testUser, tap("WbCook"), r2)
        still <- homeOf(dao2)
        said2 <- texts(r2)
      } yield assertTrue(dry.cauldron.size == 3 && said.contains("Энергии дома нет")) &&
              assertTrue(still.charges == 100 && still.cauldron.size == 1 && said2.contains("Ничего не сварилось"))
    },

    test("«Вынуть всё»: травы — на полки, отвары — на полку отваров") {
      for {
        t <- make(townhouse.copy(cauldron = List(herb(60L, MaterialKind.Sage), bottle(61L, BrewKind.Schnapps))))
        (state, dao, _, _, r) = t
        _    <- state.action(testUser, tap("WbEmptyPot"), r)
        home <- homeOf(dao)
      } yield assertTrue(home.cauldron.isEmpty && home.herbs.map(_.id) == List(60L) && home.brews.map(_.id) == List(61L))
    },

    test("полка отваров: сложить из сумки и Живой сумки (до 25), бутылку — в сумку или в котелок основой") {
      for {
        t <- make(townhouse, items = List(bottle(70L, BrewKind.Schnapps), herb(71L, MaterialKind.Sage)),
                  living = List(bottle(72L, BrewKind.BoneSetter)))
        (state, dao, inv, art, r) = t
        _      <- state.action(testUser, tap("WbPutBrews"), r)
        put    <- homeOf(dao)
        _      <- state.action(testUser, tap("WbBrew", "id" -> "70", "p" -> "0"), r)
        screen <- last(r)
        _      <- state.action(testUser, tap("WbBrewPot", "id" -> "70", "p" -> "0"), r)
        _      <- state.action(testUser, tap("WbBrewBag", "id" -> "72", "p" -> "0"), r)
        after  <- homeOf(dao)
      } yield assertTrue(put.brews.map(_.id).sorted == List(70L, 72L)) &&
              assertTrue(art.snapshot.bag.items.data.isEmpty) &&
              assertTrue(screen.choices.map(_.id) == List("WbBrewBag", "WbBrewPot", "WbBrews")) &&
              assertTrue(after.cauldron.map(_.id) == List(70L) && after.brews.isEmpty) &&
              assertTrue(inv.snapshot.map(_.id).sorted == List(71L, 72L))
    },

    test("полка отваров: написал число — столько бутылок в сумку, с конца стопки; больше, чем стоит, — сколько есть") {
      val home = townhouse.copy(brews = (80L to 84L).map(bottle(_, BrewKind.BoneSetter)).toList :+ bottle(85L, BrewKind.Schnapps))
      for {
        t <- make(home)
        (state, dao, inv, _, r) = t
        _      <- state.action(testUser, tap("WbBrew", "id" -> "84", "p" -> "0"), r)
        screen <- last(r)
        _      <- state.action(testUser, say("3"), r)
        after1 <- homeOf(dao)
        bag1    = inv.snapshot.map(_.id)
        said1  <- texts(r)
        again  <- last(r)
        _      <- state.action(testUser, say("10"), r)
        after2 <- homeOf(dao)
        list   <- last(r)
      } yield assertTrue(screen.text.contains("на полке 5") && screen.text.contains("напишите число")) &&
              // по одной кнопки на месте
              assertTrue(screen.choices.map(_.id) == List("WbBrewBag", "WbBrewPot", "WbBrews")) &&
              assertTrue(bag1 == List(82L, 83L, 84L) && after1.brews.map(_.id) == List(80L, 81L, 85L)) &&
              // остались ещё — снова экран стопки, и он снова ждёт числа
              assertTrue(said1.contains("«Костоправный отвар» ×3 — в сумку.") &&
                again.text.contains("на полке 2") && again.text.contains("напишите число")) &&
              // просили больше, чем стояло, — забрали сколько было; стопки нет — сама полка
              assertTrue(after2.brews.map(_.id) == List(85L) && inv.snapshot.size == 5) &&
              assertTrue(list.text.contains("Полка отваров"))
    },

    test("забрать числом: в сумке тесно — сколько влезет; не число — просит число; кнопка снимает ожидание") {
      val sage = (1L to 19L).map(i => herb(500L + i, MaterialKind.Sage)).toList // сумка на 20 мест — одно свободно
      val home = townhouse.copy(brews = List(bottle(90L, BrewKind.BoneSetter), bottle(91L, BrewKind.BoneSetter), bottle(92L, BrewKind.BoneSetter)))
      for {
        t <- make(home, items = sage)
        (state, dao, inv, _, r) = t
        _     <- state.action(testUser, tap("WbBrew", "id" -> "92", "p" -> "0"), r)
        _     <- state.action(testUser, say("много"), r)
        asked <- texts(r)
        _     <- state.action(testUser, say("3"), r)
        short <- homeOf(dao)
        said  <- texts(r)
        _     <- state.action(testUser, say("2"), r)
        full  <- homeOf(dao)
        said2 <- texts(r)
        _     <- state.action(testUser, tap("WbBrews", "p" -> "0"), r)
        _     <- state.action(testUser, say("2"), r)
        after <- homeOf(dao)
        menu  <- last(r)
      } yield assertTrue(asked.contains("Напишите число — сколько бутылок забрать в сумку.")) &&
              assertTrue(short.brews.map(_.id) == List(90L, 91L) && inv.snapshot.map(_.id).last == 92L) &&
              assertTrue(said.contains("«Костоправный отвар» ×1 — в сумку.\nВ сумке не хватило места ещё на 2 шт.")) &&
              // сумка полна — ничего не трогаем
              assertTrue(full == short && said2.contains("В сумке нет места.")) &&
              // число после ухода со стопки — просто верстак
              assertTrue(after == short && inv.snapshot.size == 20 && menu.text.contains("Верстак травника"))
    },

    test("бутылка одна — числа не ждёт: подсказки нет, а написанное — просто верстак") {
      val home = townhouse.copy(brews = List(bottle(95L, BrewKind.LivingWater)))
      for {
        t <- make(home)
        (state, dao, inv, _, r) = t
        _      <- state.action(testUser, tap("WbBrew", "id" -> "95", "p" -> "0"), r)
        screen <- last(r)
        _      <- state.action(testUser, say("1"), r)
        after  <- homeOf(dao)
        menu   <- last(r)
      } yield assertTrue(screen.text.contains("на полке 1") && !screen.text.contains("напишите число")) &&
              assertTrue(after == home && inv.snapshot.isEmpty && menu.text.contains("Верстак травника"))
    },

    test("ожидание числа из записи до «забрать числом» читается: варить — как было, забирать — не ждёт") {
      val old = """{"recipe":"BoneSetter","page":2}"""
      assertTrue(io.circe.jawn.decode[HerbWorkbenchState.WbScene](old) ==
                   Right(HerbWorkbenchState.WbScene(recipe = Some("BoneSetter"), page = 2, take = None))) &&
        assertTrue(io.circe.jawn.decode[HerbWorkbenchState.WbScene]("{}") == Right(HerbWorkbenchState.WbScene()))
    },

    test("травы с полок: написал число — столько трав в сумку, с конца стопки; больше, чем лежит, — сколько есть") {
      val home = townhouse.copy(herbs = (110L to 114L).map(herb(_, MaterialKind.Sage)).toList :+ herb(115L, MaterialKind.Nettle))
      for {
        t <- make(home)
        (state, dao, inv, _, r) = t
        _      <- state.action(testUser, tap("WbHerb", "id" -> "114", "p" -> "0"), r)
        screen <- last(r)
        _      <- state.action(testUser, say("3"), r)
        after1 <- homeOf(dao)
        bag1    = inv.snapshot.map(_.id)
        said1  <- texts(r)
        again  <- last(r)
        _      <- state.action(testUser, say("10"), r)
        after2 <- homeOf(dao)
        menu   <- last(r)
      } yield assertTrue(screen.text.contains("Шалфей: на полках 5.") && screen.text.contains("напишите число")) &&
              // по одной кнопки на месте
              assertTrue(screen.choices.map(_.id) == List("WbHerbPot", "WbHerbBag", "WbMenu")) &&
              assertTrue(bag1 == List(112L, 113L, 114L) && after1.herbs.map(_.id) == List(110L, 111L, 115L)) &&
              // остались ещё — снова экран стопки, и он снова ждёт числа
              assertTrue(said1.contains("«Шалфей» ×3 — в сумку.") &&
                again.text.contains("на полках 2") && again.text.contains("напишите число")) &&
              // просили больше, чем лежало, — забрали сколько было; стопки нет — сам верстак
              assertTrue(after2.herbs.map(_.id) == List(115L) && inv.snapshot.size == 5) &&
              assertTrue(menu.text.contains("Верстак травника"))
    },

    test("травы числом: в сумке тесно — сколько влезет; не число — просит число; кнопка снимает ожидание; одна — числа не ждёт") {
      val fill = (1L to 19L).map(i => bottle(600L + i, BrewKind.BoneSetter)).toList // сумка на 20 мест — одно свободно
      val home = townhouse.copy(herbs = List(herb(120L, MaterialKind.Sage), herb(121L, MaterialKind.Sage),
        herb(122L, MaterialKind.Sage), herb(123L, MaterialKind.Nettle)))
      for {
        t <- make(home, items = fill)
        (state, dao, inv, _, r) = t
        _     <- state.action(testUser, tap("WbHerb", "id" -> "122", "p" -> "0"), r)
        _     <- state.action(testUser, say("пару"), r)
        asked <- texts(r)
        _     <- state.action(testUser, say("3"), r)
        short <- homeOf(dao)
        said  <- texts(r)
        _     <- state.action(testUser, say("2"), r)
        full  <- homeOf(dao)
        said2 <- texts(r)
        _     <- state.action(testUser, tap("WbMenu", "p" -> "0"), r)
        _     <- state.action(testUser, say("2"), r)
        after <- homeOf(dao)
        _     <- state.action(testUser, tap("WbHerb", "id" -> "123", "p" -> "0"), r)
        one   <- last(r)
        _     <- state.action(testUser, say("1"), r)
        still <- homeOf(dao)
      } yield assertTrue(asked.contains("Напишите число — сколько трав забрать в сумку.")) &&
              assertTrue(short.herbs.map(_.id) == List(120L, 121L, 123L) && inv.snapshot.map(_.id).last == 122L) &&
              assertTrue(said.contains("«Шалфей» ×1 — в сумку.\nВ сумке не хватило места ещё на 2 шт.")) &&
              // сумка полна — ничего не трогаем
              assertTrue(full == short && said2.contains("В сумке нет места.")) &&
              // число после ухода со стопки — просто верстак
              assertTrue(after == short && inv.snapshot.size == 20) &&
              // трава одна — подсказки нет, и число её не берёт
              assertTrue(one.text.contains("Крапива: на полках 1.") && !one.text.contains("напишите число") && still == short)
    },

    test("запись времён «отвары числом» (с take, без herb) читается: трава числа не ждёт") {
      val brewsOnly = """{"recipe":null,"page":1,"take":"BoneSetter"}"""
      assertTrue(io.circe.jawn.decode[HerbWorkbenchState.WbScene](brewsOnly) ==
                   Right(HerbWorkbenchState.WbScene(recipe = None, page = 1, take = Some("BoneSetter"), herb = None)))
    }
  )
}
