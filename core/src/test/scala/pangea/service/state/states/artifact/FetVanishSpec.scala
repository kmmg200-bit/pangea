package pangea.service.state.states.artifact

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.hero.HeroStatsState
import pangea.test.{TestArtifactRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Зелье исчезновения Фета: прячет из рейтингов на неделю за склянку, склянок
  * — сколько угодно разом (числом в чате), туман развеивается даром. */
object FetVanishSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val week     = FetShopState.VanishMs
  private val day      = 24L * 60L * 60L * 1000L

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((Map("action" -> key) ++ data).asJson.noSpaces))

  private def say(text: String): UserAction = UserAction(text, None)

  // С хранилищем вещей — чтобы у прилавка были и лотерея, и купоны: так видно
  // всю клавиатуру целиком.
  private def shop(doubloons: Long) =
    for {
      dao      <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(doubloons = doubloons))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
      state     = FetShopState(dao, TestArtifactRepository.empty, TestInventoryRepository.accepting, content,
                    Some(TestItemRepository.make))
    } yield (state, dao, renderer)

  private def doubloons(dao: TestHeroDao) = dao.getHeroByUserId(userId).map(_.map(_.doubloons).getOrElse(-1L))
  private def texts(r: TestRenderer)      = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private def last(r: TestRenderer)       = r.sentScreens.map(_.last)

  override def spec = suite("Зелье исчезновения")(

    test("у прилавка — «🧪 Зелье исчезновения»; зелья по два в ряд, рядов не больше десяти") {
      for {
        t <- shop(0L)
        (state, _, r) = t
        _      <- state.action(testUser, tap("FetShop"), r)
        screen <- last(r)
        potionRows = screen.choices.filter(c => c.id == "FetPotion" || c.id == "FetVanish").flatMap(_.row)
      } yield assertTrue(screen.choices.exists(c => c.id == "FetVanish" && c.label == "🧪 Зелье исчезновения")) &&
              assertTrue(screen.text.contains("четыре тёмные склянки")) &&
              assertTrue(potionRows.size == 4 && potionRows.distinct.size == 2) &&
              assertTrue(screen.choices.flatMap(_.row).distinct.size <= 10)
    },

    test("карточка: рассказ и «Купить и выпить за 🟡 150»; тумана нет — нет и «Развеять туман»") {
      for {
        t <- shop(0L)
        (state, _, r) = t
        _    <- state.action(testUser, tap("FetVanish"), r)
        card <- last(r)
      } yield assertTrue(card.text.contains("Любимое зелье королей")) &&
              assertTrue(card.text.contains("из всех рейтингов на 7 дней, каждая следующая добавляет ещё 7")) &&
              assertTrue(card.choices.exists(c => c.id == "FetVanishBuy" && c.label == "Купить и выпить за 🟡 150")) &&
              assertTrue(!card.choices.exists(_.id == "FetVanishLift") && !card.text.contains("Туман ещё держится"))
    },

    test("одна склянка: −150 дублонов, рейтинги не видят героя 7 дней") {
      for {
        t <- shop(1000L)
        (state, dao, r) = t
        _       <- state.action(testUser, tap("FetVanish"), r)
        _       <- state.action(testUser, tap("FetVanishBuy"), r)
        confirm <- last(r)
        _       <- state.action(testUser, tap("FetVanishYes", "n" -> "1"), r)
        left    <- doubloons(dao)
        until   <- dao.readTopHiddenUntil(userId)
        said    <- texts(r)
      } yield assertTrue(confirm.text == "Выпить «🧪 Зелье исчезновения» за 🟡 150? Туман продержится 7д 0ч.") &&
              assertTrue(left == 850L && until.contains(week)) &&
              assertTrue(said.contains("🌫 Склянок выпито: 1. Имя ваше подёргивается туманом — рейтинги не увидят вас 7д 0ч."))
    },

    test("партия числом: «3» на карточке — вопрос про ×3 за 450, «Договорились» — три недели") {
      for {
        t <- shop(1000L)
        (state, dao, r) = t
        _       <- state.action(testUser, tap("FetVanish"), r)
        _       <- state.action(testUser, say("3"), r)
        confirm <- last(r)
        _       <- state.action(testUser, tap("FetVanishYes", "n" -> "3"), r)
        left    <- doubloons(dao)
        until   <- dao.readTopHiddenUntil(userId)
      } yield assertTrue(confirm.text == "Выпить «🧪 Зелье исчезновения» ×3 за 🟡 450? Туман продержится 21д 0ч.") &&
              assertTrue(confirm.choices.exists(c => c.id == "FetVanishYes" && c.data.get("n").contains("3"))) &&
              assertTrue(left == 550L && until.contains(3L * week))
    },

    test("склянка, пока туман держится, прибавляет неделю к остатку; кончившийся не в счёт") {
      for {
        t <- shop(1000L)
        (state, dao, r) = t
        _     <- dao.updateTopHiddenUntil(userId, Some(2L * day))
        _     <- state.action(testUser, tap("FetVanish"), r)
        card  <- last(r)
        _     <- state.action(testUser, tap("FetVanishYes", "n" -> "1"), r)
        until <- dao.readTopHiddenUntil(userId)
      } yield assertTrue(card.text.contains("🌫 Туман ещё держится: рейтинги не видят вас ещё 2д 0ч.")) &&
              assertTrue(card.choices.exists(_.id == "FetVanishLift")) &&
              assertTrue(until.contains(2L * day + week)) &&
              assertTrue(FetShopState.vanishUntil(Some(50L), 100L, 1) == 100L + week) &&
              assertTrue(FetShopState.vanishUntil(None, 100L, 2) == 100L + 2L * week)
    },

    test("дублонов на партию не хватает — Фет так и говорит, ничего не списано даже кнопкой") {
      for {
        t <- shop(400L)
        (state, dao, r) = t
        _     <- state.action(testUser, tap("FetVanish"), r)
        _     <- state.action(testUser, say("3"), r)
        said  <- last(r)
        _     <- state.action(testUser, tap("FetVanishYes", "n" -> "3"), r)
        left  <- doubloons(dao)
        until <- dao.readTopHiddenUntil(userId)
      } yield assertTrue(said.text.contains("нужно 🟡 450")) &&
              assertTrue(left == 400L && until.isEmpty)
    },

    test("не число или ноль — Фет переспрашивает; число у прилавка — просто прилавок") {
      for {
        t <- shop(1000L)
        (state, dao, r) = t
        _       <- state.action(testUser, tap("FetShop"), r)
        _       <- state.action(testUser, say("3"), r)
        counter <- last(r)
        _       <- state.action(testUser, tap("FetVanish"), r)
        _       <- state.action(testUser, say("0"), r)
        zero    <- last(r)
        _       <- state.action(testUser, say("много"), r)
        word    <- last(r)
        left    <- doubloons(dao)
      } yield assertTrue(counter.text.contains("На прилавке у Фета") && !counter.choices.exists(_.id == "FetVanishYes")) &&
              assertTrue(zero.text.contains("Сколько склянок?") && word.text.contains("Сколько склянок?")) &&
              assertTrue(left == 1000L)
    },

    test("«Развеять туман» — на карточке, пока он держится: даром, рейтинги снова видят") {
      for {
        t <- shop(1000L)
        (state, dao, r) = t
        _     <- dao.updateTopHiddenUntil(userId, Some(week))
        _     <- state.action(testUser, tap("FetVanish"), r)
        card  <- last(r)
        _     <- state.action(testUser, tap("FetVanishLift"), r)
        ask   <- last(r)
        _     <- state.action(testUser, tap("FetVanishLiftYes"), r)
        until <- dao.readTopHiddenUntil(userId)
        left  <- doubloons(dao)
        said  <- texts(r)
      } yield assertTrue(card.choices.exists(c => c.id == "FetVanishLift" && c.label == "🌤 Развеять туман")) &&
              assertTrue(ask.text.contains("Туман продержался бы ещё 7д 0ч. Развеять?")) &&
              assertTrue(until.isEmpty && left == 1000L) &&
              assertTrue(said.contains("🌤 Туман рассеялся: рейтинги снова вас видят."))
    },

    test("в «Персонаже» — срок исчезновения, пока туман держится; кончился — строки нет") {
      for {
        t <- shop(0L)
        (_, dao, r) = t
        content <- ZIO.attempt(SceneContent.load())
        stats    = HeroStatsState(dao, content)
        _       <- dao.updateTopHiddenUntil(userId, Some(week))
        _       <- stats.enter(testUser, r)
        hidden  <- last(r)
        _       <- dao.updateTopHiddenUntil(userId, None)
        _       <- stats.enter(testUser, r)
        shown   <- last(r)
      } yield assertTrue(hidden.text.contains("🌫 Исчезновение: ещё 7д 0ч")) &&
              assertTrue(!shown.text.contains("Исчезновение")) &&
              assertTrue(!TestFixtures.hero(userId).getInfo(10L, vanishUntil = Some(5L)).contains("Исчезновение"))
    }
  )
}
