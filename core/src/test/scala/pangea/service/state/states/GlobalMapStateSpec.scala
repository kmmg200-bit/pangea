package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

object GlobalMapStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState(hero: pangea.model.hero.Hero) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (GlobalMapState(heroDao, content), heroDao, renderer)

  private val baseHero    = TestFixtures.hero(userId)
  private val fullHp      = baseHero.baseStats.vit * 24L  // 240
  private val richHero    = baseHero.copy(silver = 500L, doubloons = 37L, fightStats = baseHero.fightStats.copy(hp = fullHp))

  override def spec = suite("GlobalMapState")(

    test("enter → показывает HP, серебро и дублоны, кнопки Tavern и ReturnToDungeon") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("Кинэт")) &&
              // кошелёк целиком: серебро и под ним дублоны
              assertTrue(screens.head.text.endsWith("🪙 Серебро: 500\n🟡 Дублоны: 37")) &&
              assertTrue(screens.head.choices.map(_.id).contains("Tavern")) &&
              assertTrue(screens.head.choices.map(_.id).contains("ReturnToDungeon")) &&
              // «📱 Приложение» — в одном ряду с «Персонажем», сразу за ним
              assertTrue(screens.head.choices.filter(_.row.contains(2)).map(_.id) ==
                List("OpenCharacter", "OpenApp", "ReturnToDungeon")) &&
              assertTrue(pangea.service.state.AppButton.on(screens.head.choices))
    },

    test("ReturnToDungeon → переходит в Dungeon") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("ReturnToDungeon"), renderer)
      } yield assertTrue(result == StateType.Dungeon)
    },

    test("Tavern → переходит в Tavern") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Tavern"), renderer)
      } yield assertTrue(result == StateType.Tavern)
    },

    test("enter → показывает кнопки Guild и MarketSquare") {
      for {
        triple              <- makeState(baseHero)
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(screens.head.choices.map(_.id).contains("Guild")) &&
              assertTrue(screens.head.choices.map(_.id).contains("MarketSquare"))
    },

    test("Guild → переходит в Guild") {
      for {
        triple              <- makeState(baseHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Guild"), renderer)
      } yield assertTrue(result == StateType.Guild)
    },

    test("MarketSquare → переходит в MarketSquare") {
      for {
        triple              <- makeState(baseHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("MarketSquare"), renderer)
      } yield assertTrue(result == StateType.MarketSquare)
    }
  )
}
