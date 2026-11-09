package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer, TestScheduler}
import zio.{Ref, Task, ZIO}
import zio.test._
import zio.test.TestClock

object RestStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val anyAction: UserAction = UserAction("", Some("""{"action":"Wakeup"}"""))

  private def makeState(hero: pangea.model.hero.Hero) =
    for {
      heroDao   <- TestHeroDao.withHero(userId, hero)
      renderer  <- TestRenderer.make
      scheduler <- TestScheduler.make
      content   <- ZIO.attempt(SceneContent.load())
    } yield (RestState(heroDao, scheduler, content), heroDao, renderer)

  /** ВК, который не принимает первые `fails` сообщений — как обрыв в 20:30. */
  private final class FlakyRenderer(fails: Ref[Int], inner: TestRenderer) extends Renderer {
    def show(user: User, screen: Screen): Task[Unit] =
      fails.getAndUpdate(n => (n - 1).max(0)).flatMap(n =>
        if (n > 0) ZIO.fail(new RuntimeException("VK API error 10: Internal server error"))
        else inner.show(user, screen))
  }

  override def spec = suite("RestState")(

    test("enter → показывает экран отдыха, записывает restStartedAt") {
      for {
        triple              <- makeState(TestFixtures.hero(userId))
        (state, heroDao, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
        sceneData           <- heroDao.readSceneData(userId)
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.choices.map(_.id) == List("OpenApp")) &&
              assertTrue(sceneData.isDefined) &&
              assertTrue(sceneData.flatMap(_.hcursor.get[Long]("restStartedAt").toOption).isDefined)
    },

    test("action до 30с → показывает 'ещё X', остаётся в Rest") {
      for {
        triple              <- makeState(TestFixtures.hero(userId))
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        result              <- state.action(testUser, anyAction, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Rest) &&
              assertTrue(screens.exists(s => s.text.contains("с") || s.text.contains("м")))
    },

    test("action после 30с → восстанавливает HP, переходит в Dungeon") {
      for {
        triple              <- makeState(TestFixtures.hero(userId))
        (state, heroDao, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        _                   <- TestClock.adjust(java.time.Duration.ofSeconds(31))
        result              <- state.action(testUser, anyAction, renderer)
        updatedHero         <- heroDao.getHeroByUserId(userId)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(updatedHero.exists(_.fightStats.hp == updatedHero.get.baseStats.vit * 24L)) &&
              assertTrue(screens.exists(_.text.contains("отдохнули")))
    },

    test("action после 30с → восстанавливает armor до maxArmor") {
      import pangea.model.item.{Item, ItemType, Rarity}
      val helmet = Item(1L, "Шлем", 1L, Rarity.Gray, ItemType.Helmet,
        attack=0, accuracy=0, energy=0, armor=30, defence=0, evasion=0)
      val heroWithArmor = TestFixtures.hero(userId).copy(
        equipment  = TestFixtures.emptyEquipment.copy(helmet = helmet),
        fightStats = TestFixtures.hero(userId).fightStats.copy(armor = 0L)  // броня истрачена в бою
      )
      for {
        triple              <- makeState(heroWithArmor)
        (state, heroDao, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        _                   <- TestClock.adjust(java.time.Duration.ofSeconds(31))
        _                   <- state.action(testUser, anyAction, renderer)
        updatedHero         <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updatedHero.exists(_.fightStats.armor == 30L)) &&  // восстановлена до allArmor
              assertTrue(updatedHero.exists(_.fightStats.armor == updatedHero.get.maxArmor))
    },

    test("post-death recovery: если DeathState записал restDurationMs=2мин, enter читает его и action ждёт полные 2 мин") {
      val deathRestMs = 2L * 60L * 1000L  // level 1 death recovery
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        renderer <- TestRenderer.make
        scheduler <- TestScheduler.make
        content  <- ZIO.attempt(SceneContent.load())
        // simulate what DeathState writes before transitioning to Rest
        _        <- heroDao.writeSceneData(userId, Json.obj("restDurationMs" -> deathRestMs.asJson))
        state     = RestState(heroDao, scheduler, content)
        _        <- state.enter(testUser, renderer)
        // 30 seconds not enough for post-death recovery
        _        <- TestClock.adjust(java.time.Duration.ofSeconds(31))
        resultEarly <- state.action(testUser, anyAction, renderer)
        // advance past the full 2 minutes
        _        <- TestClock.adjust(java.time.Duration.ofSeconds(91))
        resultDone  <- state.action(testUser, anyAction, renderer)
      } yield assertTrue(resultEarly == StateType.Rest) &&
              assertTrue(resultDone  == StateType.Dungeon)
    },

    test("enter → планирует push-пробуждение Revive на now+duration, expectedState=Rest") {
      for {
        heroDao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        renderer  <- TestRenderer.make
        scheduler <- TestScheduler.make
        content   <- ZIO.attempt(SceneContent.load())
        state      = RestState(heroDao, scheduler, content)
        _         <- state.enter(testUser, renderer)
        scheduled <- scheduler.scheduled
      } yield assertTrue(scheduled.size == 1) &&
              assertTrue(scheduled.head.kind == TaskKind.Revive) &&
              assertTrue(scheduled.head.expectedState == StateType.Rest)
    },

    test("пробуждение снимает отложенный Revive") {
      for {
        heroDao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        renderer  <- TestRenderer.make
        scheduler <- TestScheduler.make
        content   <- ZIO.attempt(SceneContent.load())
        state      = RestState(heroDao, scheduler, content)
        _         <- state.enter(testUser, renderer)
        _         <- TestClock.adjust(java.time.Duration.ofSeconds(31))
        result    <- state.action(testUser, anyAction, renderer)
        cancelled <- scheduler.cancelled
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(cancelled.contains(userId -> TaskKind.Revive))
    },
    test("отдыха в scene_data нет (сцену стёрли) — первое же сообщение будит, а не «Ещё 30с»") {
      for {
        triple              <- makeState(TestFixtures.hero(userId))
        (state, heroDao, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        _                   <- heroDao.writeSceneData(userId, Json.Null)
        result              <- state.action(testUser, anyAction, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.last.text.contains("отдохнули")) &&
              assertTrue(!screens.exists(_.text.contains("до восстановления")))
    },

    test("ВК не принял «вы пришли в себя»: отдых и Revive на месте, повтор будит туда же, куда сказано") {
      for {
        heroDao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        renderer  <- TestRenderer.make
        scheduler <- TestScheduler.make
        content   <- ZIO.attempt(SceneContent.load())
        // сюжетная смерть: проснуться в городе и услышать, чем кончилось
        _         <- heroDao.writeSceneData(userId, Json.obj(
                       "restDurationMs" -> 60000L.asJson,
                       "postDeath"      -> true.asJson,
                       "wakeTo"         -> (StateType.GlobalMap: StateType).asJson,
                       "wakeLines"      -> List("murlocVillage.raid.death").asJson))
        state      = RestState(heroDao, scheduler, content)
        _         <- state.enter(testUser, renderer)
        _         <- TestClock.adjust(java.time.Duration.ofSeconds(61))
        fails     <- Ref.make(1)
        broken    <- state.action(testUser, anyAction, new FlakyRenderer(fails, renderer)).exit
        scene     <- heroDao.readSceneData(userId)
        kept       = scene.flatMap(_.hcursor.get[Long]("restStartedAt").toOption).isDefined &&
                       scene.flatMap(_.hcursor.get[StateType]("wakeTo").toOption).contains(StateType.GlobalMap)
        cancelled <- scheduler.cancelled
        retry     <- state.action(testUser, anyAction, renderer)
        after     <- heroDao.readSceneData(userId)
        cancelled2 <- scheduler.cancelled
        screens   <- renderer.sentScreens
      } yield assertTrue(broken.isFailure && kept) &&
              // задача пробуждения не сняла сама себя — поллер её повторит
              assertTrue(!cancelled.contains(userId -> TaskKind.Revive)) &&
              assertTrue(retry == StateType.GlobalMap && after.contains(Json.Null)) &&
              assertTrue(cancelled2.contains(userId -> TaskKind.Revive)) &&
              assertTrue(screens.exists(_.text == content.text("murlocVillage.raid.death")))
    }
  )
}
