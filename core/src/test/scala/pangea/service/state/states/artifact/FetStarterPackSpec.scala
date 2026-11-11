package pangea.service.state.states.artifact

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.GameEvent
import pangea.model.hero.{AzatState, Hero}
import pangea.model.item.{BrewKind, Bundling, Grip, Item, ItemType, Rarity}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.sender.vk.VkRenderer
import pangea.service.state.UserAction
import pangea.test._
import zio.ZIO
import zio.test._

/** Набор успешного искателя у Фета: 600 дублонов, один на игрока. Стопки живой
  * воды, шнапса и костоправного, фиолетовые 5 уровня — двуручное, одноручное,
  * лук и щит, 25 000 серебра, +7 мест в сумке и благословение Азата на 5 дней.
  * Покупка — в журнал; купившему кнопки больше нет. */
object FetStarterPackSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val day      = 24L * 60L * 60L * 1000L

  private def tap(key: String): UserAction = UserAction("", Some(Map("action" -> key).asJson.noSpaces))

  private def gear(id: Long): Item =
    Item(id, s"Меч $id", lvl = 1L, Rarity.Gray, ItemType.Weapon,
      attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private case class Fix(state: FetShopState, dao: TestHeroDao, inv: TestInventoryRepository,
                         journal: TestJournal, renderer: TestRenderer)

  private def shop(doubloons: Long = 700L, silver: Long = 1000L, bag: List[Item] = Nil, withJournal: Boolean = true) =
    for {
      dao      <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(doubloons = doubloons, silver = silver))
      inv       = TestInventoryRepository.withItems(bag)
      journal  <- TestJournal.make
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield Fix(FetShopState(dao, TestArtifactRepository.empty, inv, content, Some(TestItemRepository.make),
                  Option.when(withJournal)(journal)), dao, inv, journal, renderer)

  private def heroOf(dao: TestHeroDao): ZIO[Any, Throwable, Hero] =
    dao.getHeroByUserId(userId).someOrFail(new Throwable("no hero"))

  private def azatOf(dao: TestHeroDao): ZIO[Any, Throwable, AzatState] =
    dao.readAzatData(userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))

  private def buy(f: Fix) =
    f.state.action(testUser, tap("FetPack"), f.renderer) *>
      f.state.action(testUser, tap("FetPackBuy"), f.renderer) *>
      f.state.action(testUser, tap("FetPackYes"), f.renderer)

  override def spec = suite("Набор успешного искателя")(

    test("на прилавке — первым рядом, и рядов не больше, чем держит ВК") {
      for {
        f    <- shop()
        _    <- f.state.action(testUser, tap("FetShop"), f.renderer)
        last <- f.renderer.sentScreens.map(_.last)
        rows  = last.choices.flatMap(_.row).distinct
      } yield assertTrue(last.choices.headOption.exists(c => c.id == "FetPack" && c.label == "🎁 Набор успешного искателя" &&
                         c.row.contains(0))) &&
              assertTrue(last.choices.count(_.row.contains(0)) == 1) &&
              assertTrue(rows.size <= VkRenderer.MaxRows)
    },

    test("карточка: речь Фета и что в наборе; «Купить за 🟡 600» и «Назад»; подтверждение с ценой") {
      for {
        f    <- shop()
        _    <- f.state.action(testUser, tap("FetPack"), f.renderer)
        card <- f.renderer.sentScreens.map(_.last)
        _    <- f.state.action(testUser, tap("FetPackBuy"), f.renderer)
        ask  <- f.renderer.sentScreens.map(_.last)
      } yield assertTrue(card.text.startsWith("«Лучше этого набора для начинающего искателя ты не найдёшь")) &&
              assertTrue(card.text.contains("• 🪙 25 000 серебра") && card.text.contains("Набор продаётся один раз.")) &&
              assertTrue(card.choices.map(_.id) == List("FetPackBuy", "FetShop") &&
                         card.choices.headOption.exists(_.label == "Купить за 🟡 600")) &&
              assertTrue(ask.text == "Взять «Набор успешного искателя» за 🟡 600?" &&
                         ask.choices.map(_.id) == List("FetPackYes", "FetShop"))
    },

    test("покупка: −600 дублонов, +25 000 серебра, +7 мест и семь вещей даже в полную сумку, благословение на 5 дней") {
      // Номера мечей — не те, что раздаст тестовое хранилище вещей (с единицы).
      val full = (1001L to 1020L).toList.map(gear)
      for {
        f     <- shop(doubloons = 700L, silver = 1000L, bag = full)
        _     <- buy(f)
        hero  <- heroOf(f.dao)
        azat  <- azatOf(f.dao)
        inv   <- f.inv.get(hero.id).mapError(e => new Throwable(e.toString))
        said  <- f.renderer.sentScreens.map(_.map(_.text))
        last  <- f.renderer.sentScreens.map(_.last)
        log   <- f.journal.count(userId, GameEvent.FetStarterPack, 0L)
        got    = inv.items.data.filterNot(i => full.exists(_.id == i.id))
        stacks = got.filter(_.isBundle)
        arms   = got.filterNot(_.isBundle)
        inside = stacks.map(s => Bundling.contents(s).flatMap(_.brew))
        ids    = got.map(_.id) ++ stacks.flatMap(Bundling.contents).map(_.id)
      } yield assertTrue(hero.doubloons == 100L && hero.silver == 26000L) &&
              assertTrue(inv.maxItems == 27L && inv.items.data.size == 27 && got.size == 7) &&
              // три стопки по пять: живая вода, шнапс, костоправный
              assertTrue(inside.map(_.toSet) ==
                           List[Set[BrewKind]](Set(BrewKind.LivingWater), Set(BrewKind.Schnapps), Set(BrewKind.BoneSetter)) &&
                         inside.forall(_.size == Bundling.Size)) &&
              // фиолетовые 5 уровня: двуручное, одноручное, лук — и щит
              assertTrue(arms.forall(a => a.rarity == Rarity.Purple && a.lvl == 5L)) &&
              assertTrue(arms.flatMap(_.grip) == List(Grip.TwoHanded, Grip.OneHanded, Grip.Bow) &&
                         arms.map(_.itemType) == List(ItemType.Weapon, ItemType.Weapon, ItemType.Weapon, ItemType.Shield)) &&
              // у каждой вещи и каждой склянки — свой номер
              assertTrue(ids.distinct.size == ids.size && ids.forall(_ > 0L)) &&
              assertTrue(azat.blessingUntil.contains(5L * day)) &&
              assertTrue(log == 1L) &&
              assertTrue(said.contains(content.text("fet.pack.done")) && !last.choices.exists(_.id == "FetPack"))
    },

    test("благословение уже идёт — продлевается на 5 дней") {
      for {
        f    <- shop()
        _    <- f.dao.writeAzatData(userId, AzatState(blessingUntil = Some(2L * day), restsGrantedAt = Some(0L)).asJson)
        _    <- buy(f)
        azat <- azatOf(f.dao)
      } yield assertTrue(azat.blessingUntil.contains(7L * day))
    },

    test("купил — кнопки больше нет, а нажатия ничего не покупают: прилавок как есть") {
      for {
        f     <- shop(doubloons = 2000L)
        _     <- buy(f)
        _     <- f.state.action(testUser, tap("FetShop"), f.renderer)
        shop1 <- f.renderer.sentScreens.map(_.last)
        _     <- f.state.action(testUser, tap("FetPack"), f.renderer)
        card  <- f.renderer.sentScreens.map(_.last)
        _     <- f.state.action(testUser, tap("FetPackYes"), f.renderer)
        again <- f.renderer.sentScreens.map(_.last)
        hero  <- heroOf(f.dao)
        log   <- f.journal.count(userId, GameEvent.FetStarterPack, 0L)
      } yield assertTrue(!shop1.choices.exists(_.id == "FetPack") && shop1.choices.flatMap(_.row).min == 0) &&
              assertTrue(card.text == content.text("fet.counter") && again.text == content.text("fet.counter")) &&
              assertTrue(hero.doubloons == 1400L && log == 1L)
    },

    test("не хватает дублонов — «Фет разводит руками», ничего не потрачено и не выдано") {
      for {
        f    <- shop(doubloons = 599L)
        _    <- f.state.action(testUser, tap("FetPackYes"), f.renderer)
        last <- f.renderer.sentScreens.map(_.last)
        hero <- heroOf(f.dao)
        log  <- f.journal.count(userId, GameEvent.FetStarterPack, 0L)
      } yield assertTrue(last.text == content.format("fet.notEnough", "price" -> "600")) &&
              assertTrue(hero.doubloons == 599L && hero.silver == 1000L && f.inv.snapshot.isEmpty && log == 0L)
    },

    test("без журнала набор не продаётся: не уследить, что он уже куплен") {
      for {
        f    <- shop(withJournal = false)
        _    <- f.state.action(testUser, tap("FetShop"), f.renderer)
        last <- f.renderer.sentScreens.map(_.last)
      } yield assertTrue(!last.choices.exists(_.id == "FetPack"))
    },

    test("вещей в наборе столько же, сколько он даёт мест: всё влезает в любую сумку") {
      assertTrue(FetShopState.PackBrews.size + FetShopState.PackGear.size == FetShopState.PackSlots.toInt)
    }
  )

  private lazy val content: SceneContent = SceneContent.load()
}
