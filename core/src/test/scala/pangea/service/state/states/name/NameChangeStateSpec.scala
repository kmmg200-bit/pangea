package pangea.service.state.states.name

import io.circe.syntax.EncoderOps
import pangea.engine.{SceneContent, Screen}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.InventoryState
import pangea.service.state.states.artifact.FetShopState
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Смена имени: купон у Фета — своей сценой, «Сменить имя» в сумке (только из
  * города), новое имя текстом — и купон сгорает. */
object NameChangeStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val otherId  = UserId(2L)

  private def tap(key: String): UserAction = UserAction("", Some(Map("action" -> key).asJson.noSpaces))
  private def typed(t: String): UserAction = UserAction(t, None)

  private def coupon(id: Long): Item = QuestItemKind.item(QuestItemKind.NameCoupon).copy(id = id)

  private def named(name: Option[String]): Hero = TestFixtures.hero(userId).copy(name = name)

  /** Сцена смены имени; второй герой — «Тарен». */
  private def scene(bag: List[Item] = List(coupon(900L)), name: Option[String] = Some("Старое Имя")) =
    for {
      dao <- TestHeroDao.withHero(userId, named(name))
      _   <- dao.insertHero(TestFixtures.hero(otherId).copy(id = HeroId(2L), name = Some("Тарен")))
      inv  = TestInventoryRepository.withItems(bag)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (NameChangeState(dao, inv, c), dao, inv, r)

  private def last(r: TestRenderer): Task[Screen] = r.sentScreens.map(_.last)

  private def nameOf(dao: TestHeroDao): Task[Option[String]] = dao.getHeroByUserId(userId).map(_.flatMap(_.name))

  private def coupons(inv: TestInventoryRepository): Int = inv.snapshot.count(_.questItem.contains(QuestItemKind.NameCoupon))

  override def spec = suite("Смена имени")(

    test("у Фета купон на имя — своя кнопка и своя сцена: 50 дублонов, в сумку; рядом с купоном расы") {
      for {
        dao     <- TestHeroDao.withHero(userId, named(None).copy(doubloons = 60L))
        inv      = TestInventoryRepository.withItems(Nil)
        r       <- TestRenderer.make
        c       <- ZIO.attempt(SceneContent.load())
        shop     = FetShopState(dao, TestArtifactRepository.empty, inv, c, Some(TestItemRepository.make))
        _       <- shop.action(testUser, tap("FetShop"), r)
        counter <- last(r)
        _       <- shop.action(testUser, tap("FetNameCoupon"), r)
        card    <- last(r)
        _       <- shop.action(testUser, tap("FetNameCouponBuy"), r)
        ask     <- last(r)
        _       <- shop.action(testUser, tap("FetNameCouponYes"), r)
        hero    <- dao.getHeroByUserId(userId)
        bought   = coupons(inv)
        // второй раз — денег уже не хватает
        _       <- shop.action(testUser, tap("FetNameCouponYes"), r)
        hero2   <- dao.getHeroByUserId(userId)
        said    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        race     = counter.choices.find(_.id == "FetCoupon")
        name     = counter.choices.find(_.id == "FetNameCoupon")
      } yield assertTrue(name.exists(_.label == "🎟 Купон на смену имени") && race.isDefined) &&
              // в одном ряду: у ВК не больше десяти рядов
              assertTrue(race.flatMap(_.row) == name.flatMap(_.row)) &&
              assertTrue(counter.choices.flatMap(_.row).max < 10) &&
              assertTrue(card.text.contains("Купон на смену имени") && card.choices.exists(_.id == "FetNameCouponBuy")) &&
              assertTrue(ask.text.contains("🟡 50") && ask.choices.exists(_.id == "FetNameCouponYes")) &&
              assertTrue(hero.exists(_.doubloons == 10L) && bought == 1) &&
              assertTrue(hero2.exists(_.doubloons == 10L) && coupons(inv) == 1 && said.contains("нужно 🟡 50"))
    },

    test("купон в сумке: «Сменить имя»; не из города — отказ, из города — к выбору имени") {
      for {
        dao   <- TestHeroDao.withHero(userId, named(None))
        inv    = TestInventoryRepository.withItems(List(coupon(900L)))
        r     <- TestRenderer.make
        c     <- ZIO.attempt(SceneContent.load())
        state  = InventoryState(dao, inv, TestItemRepository.make, c)
        _     <- state.action(testUser, tap(s"${InventoryState.ItemActionPrefix}900"), r)
        card  <- last(r)
        _     <- dao.writeReturnState(userId, Some(StateType.Dungeon))
        far   <- state.action(testUser, tap("NameChange"), r)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        _     <- dao.writeReturnState(userId, Some(StateType.GlobalMap))
        near  <- state.action(testUser, tap("NameChange"), r)
      } yield assertTrue(card.choices.exists(ch => ch.id == "NameChange" && ch.label == "✍ Сменить имя")) &&
              assertTrue(card.text.contains("Купон на смену имени")) &&
              assertTrue(far == StateType.Inventory && said.contains("только в городе")) &&
              assertTrue(near == StateType.NameChange)
    },

    test("новое имя текстом: отказы по правилам и за занятое, подтверждение — имя героя, купон сгорел, назад в сумку") {
      for {
        t       <- scene()
        (state, dao, inv, r) = t
        _       <- state.enter(testUser, r)
        ask     <- last(r)
        digits  <- state.action(testUser, typed("Воин2000"), r) *> last(r)
        taken   <- state.action(testUser, typed("ТАРЕН"), r) *> last(r)
        _       <- state.action(testUser, typed("Ли Вэй"), r)
        confirm <- last(r)
        before  <- nameOf(dao)
        out     <- state.action(testUser, tap("NameChangeYes"), r)
        done    <- last(r)
        after   <- nameOf(dao)
      } yield assertTrue(ask.text.contains("Сейчас вас зовут Старое Имя") && ask.text.contains("Напишите новое имя")) &&
              assertTrue(ask.choices.map(_.id) == List("NameChangeCancel")) &&
              assertTrue(digits.text.contains("только русские или латинские буквы")) &&
              assertTrue(taken.text.contains("«ТАРЕН» уже занято")) &&
              assertTrue(confirm.text.contains("Новое имя — Ли Вэй") &&
                confirm.choices.map(_.id) == List("NameChangeYes", "NameChangeOther", "NameChangeCancel")) &&
              // пока не подтвердил — имя прежнее
              assertTrue(before.contains("Старое Имя")) &&
              assertTrue(out == StateType.Inventory && after.contains("Ли Вэй") && done.text.contains("теперь вас зовут Ли Вэй")) &&
              assertTrue(coupons(inv) == 0)
    },

    test("имя заняли, пока игрок подтверждал: спрашиваем снова, купон цел") {
      for {
        t     <- scene()
        (state, dao, inv, r) = t
        _     <- state.enter(testUser, r)
        _     <- state.action(testUser, typed("Ли Вэй"), r)
        _     <- dao.updateName(otherId, "ли вэй")
        out   <- state.action(testUser, tap("NameChangeYes"), r)
        again <- last(r)
        name  <- nameOf(dao)
      } yield assertTrue(out == StateType.NameChange && again.text.contains("уже занято") && again.text.contains("Напишите новое имя")) &&
              assertTrue(name.contains("Старое Имя") && coupons(inv) == 1)
    },

    test("купона в сумке уже нет — имя не меняется; «Отмена» — назад в сумку, купон цел") {
      for {
        t      <- scene(bag = Nil)
        (state, dao, _, r) = t
        _      <- state.enter(testUser, r)
        _      <- state.action(testUser, typed("Ли Вэй"), r)
        gone   <- state.action(testUser, tap("NameChangeYes"), r)
        said   <- last(r)
        name   <- nameOf(dao)
        u      <- scene()
        (state2, _, inv2, r2) = u
        _      <- state2.enter(testUser, r2)
        cancel <- state2.action(testUser, tap("NameChangeCancel"), r2)
      } yield assertTrue(gone == StateType.Inventory && said.text.contains("больше нет") && name.contains("Старое Имя")) &&
              assertTrue(cancel == StateType.Inventory && coupons(inv2) == 1)
    },

    test("без имени (прошёл пролог до имён) — о нынешнем имени не говорим") {
      for {
        t   <- scene(name = None)
        (state, _, _, r) = t
        _   <- state.enter(testUser, r)
        ask <- last(r)
      } yield assertTrue(!ask.text.contains("Сейчас вас зовут") && ask.text.contains("Напишите новое имя"))
    }
  )
}
