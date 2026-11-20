package pangea.service.state.states.arena

import pangea.engine.SceneContent
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.TestRenderer
import zio.ZIO
import zio.test._

/** Арена закрыта: экран-заглушка — что закрыто, и назад в Центр города. */
object ArenaClosedSpec extends ZIOSpecDefault {

  private val testUser = User(UserId(1L), VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  override def spec = suite("Арена закрыта")(

    test("вход: «закрыто на перестройку» и одна кнопка — назад") {
      for {
        content <- ZIO.attempt(SceneContent.load())
        r       <- TestRenderer.make
        _       <- ArenaState(content).enter(testUser, r)
        shown   <- r.sentScreens.map(_.last)
      } yield assertTrue(shown.text == "⚔ Арена закрыта на перестройку. Бои между искателями вернутся — в новом виде.") &&
              assertTrue(shown.choices.map(_.id) == List("LeaveArena") && shown.choices.map(_.label) == List("↩ Назад"))
    },

    test("кнопка старой арены или текст — снова заглушка; «Назад» — в Центр города") {
      for {
        content <- ZIO.attempt(SceneContent.load())
        state    = ArenaState(content)
        r       <- TestRenderer.make
        old     <- state.action(testUser, tap("ArenaSolo"), r)
        typed   <- state.action(testUser, UserAction("1234", None), r)
        shown   <- r.sentScreens
        left    <- state.action(testUser, tap("LeaveArena"), r)
      } yield assertTrue(old == StateType.Arena && typed == StateType.Arena) &&
              assertTrue(shown.size == 2 && shown.forall(_.text.startsWith("⚔ Арена закрыта"))) &&
              assertTrue(left == StateType.CityCenter)
    }
  )
}
