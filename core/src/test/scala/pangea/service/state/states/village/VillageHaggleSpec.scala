package pangea.service.state.states.village

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.{VillageQuest, VillageRates, VillageScene}
import pangea.service.state.{NpcQuestLog, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Торг у старосты — у всех заданий деревни: «Поторговаться» один раз; с
  * шансом 50% староста прибавляет к серебру 15%, иначе стоит на своём. Дублоны
  * торг не трогает. */
object VillageHaggleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val Silver = 1000L

  // Деревня гномов, гнездо сетеплётов, староста уже рассказал о беде.
  private val plea = VillageScene(Race.Gnome, 0, VillageQuest.SpiderNests, 20, 21, Silver, city = 2,
    step = VillageScene.Headman, doubloons = 3L)

  private def hero: Hero = TestFixtures.hero(userId, dungeonLevel = 20)

  private val content = ZIO.attempt(SceneContent.load())

  private def village(scene: VillageScene): Task[(VillageState, TestHeroDao)] =
    for {
      d <- TestHeroDao.withHero(userId, hero)
      _ <- d.writeSceneData(userId, scene.asJson)
      c <- content
    } yield (VillageState(d, TestInventoryRepository.accepting, TestItemRepository.make, c), d)

  private def sceneOf(d: TestHeroDao): Task[Option[VillageScene]] =
    d.readSceneData(userId).map(_.flatMap(_.as[VillageScene].toOption))

  override def spec = suite("Торг у старосты")(

    test("у старосты — «Поторговаться», рядом со «Взяться» и «Отказаться»; в просьбах больше нет «торговаться не буду»") {
      for {
        c     <- content
        pair  <- village(plea)
        (st, _) = pair
        r     <- TestRenderer.make
        _     <- st.enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
      } yield assertTrue(VillageRates.HagglePct == 50 && VillageRates.HaggleBonusPct == 15L) &&
              assertTrue(shown.choices.map(_.id) == List("VillageAccept", "VillageDecline", "VillageHaggle") &&
                shown.choices.lastOption.exists(_.label == "💰 Поторговаться")) &&
              assertTrue(VillageQuest.values.forall(q => !c.text(s"village.plea.${q.key}.text").contains("торговаться не буду")))
    },

    test("сторговались (бросок): серебро +15%, дублоны прежние; торговаться больше нельзя; взялся — за новую плату") {
      for {
        pair  <- village(plea)
        (st, d) = pair
        r     <- TestRenderer.make
        _     <- TestRandom.feedInts(0)
        res   <- st.action(testUser, tap("VillageHaggle"), r)
        shown <- r.sentScreens.map(_.last)
        s     <- sceneOf(d)
        _     <- st.action(testUser, tap("VillageAccept"), r)
        q     <- NpcQuestLog.load(d, userId)
      } yield assertTrue(res == StateType.Village && VillageRates.haggled(Silver) == 1150L) &&
              assertTrue(shown.text.contains("Староста долго молчит, потом нехотя кивает. «Ладно, 1150 серебра и 3 дублона.") &&
                shown.text.contains("Плата — 1150 серебра и 3 дублона.") &&
                shown.choices.map(_.id) == List("VillageAccept", "VillageDecline")) &&
              assertTrue(s.exists(x => x.haggled && x.silver == 1150L && x.doubloons == 3L)) &&
              assertTrue(q.village.exists(v => v.silver == 1150L && v.doubloons == 3L))
    },

    test("не сторговались: «Цена честная», плата прежняя; торговаться больше нельзя") {
      for {
        pair  <- village(plea)
        (st, d) = pair
        r     <- TestRenderer.make
        _     <- TestRandom.feedInts(99)
        _     <- st.action(testUser, tap("VillageHaggle"), r)
        shown <- r.sentScreens.map(_.last)
        s     <- sceneOf(d)
      } yield assertTrue(shown.text.contains("«Цена честная, — повторяет староста") &&
                shown.text.contains(s"Плата — $Silver серебра и 3 дублона.") &&
                shown.choices.map(_.id) == List("VillageAccept", "VillageDecline")) &&
              assertTrue(s.exists(x => x.haggled && x.silver == Silver))
    },

    test("второй раз торговаться нельзя: та же просьба, плата прежняя; вернулся — без кнопки торга") {
      for {
        pair  <- village(plea.copy(haggled = true, silver = 1150L))
        (st, d) = pair
        r     <- TestRenderer.make
        _     <- st.action(testUser, tap("VillageHaggle"), r)
        again <- r.sentScreens.map(_.last)
        s     <- sceneOf(d)
        _     <- st.enter(testUser, r)
        back  <- r.sentScreens.map(_.last)
      } yield assertTrue(again.text.contains("Месяц назад из изгороди вышли сетеплёты") &&
                again.text.contains("Плата — 1150 серебра и 3 дублона.") &&
                again.choices.map(_.id) == List("VillageAccept", "VillageDecline")) &&
              assertTrue(s.exists(x => x.haggled && x.silver == 1150L)) &&
              assertTrue(back.choices.map(_.id) == List("VillageAccept", "VillageDecline"))
    },

    test("у входа в деревню «Поторговаться» — старая кнопка: показать, что сейчас") {
      for {
        pair  <- village(plea.copy(step = VillageScene.Arrival))
        (st, d) = pair
        r     <- TestRenderer.make
        _     <- st.action(testUser, tap("VillageHaggle"), r)
        shown <- r.sentScreens.map(_.last)
        s     <- sceneOf(d)
      } yield assertTrue(shown.choices.map(_.id).contains("VillageHeadman") && s.exists(x => !x.haggled && x.silver == Silver))
    },

    test("сцена, начатая до торга, — без поля: не торговались") {
      val old = plea.asJson.mapObject(_.remove("villageHaggled"))
      assertTrue(old.as[VillageScene].exists(x => !x.haggled && x.silver == Silver)) &&
      assertTrue(plea.copy(haggled = true).asJson.as[VillageScene].exists(_.haggled))
    }
  )
}
