package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{CityExit, LostSquadQuest, NpcQuestLog, UserAction}
import pangea.test._
import zio.ZIO
import zio.test._

/** Стройка: меню работ и прораб Гуннар Отвес — его кнопка, его экран и то, что
  * посреди смены к нему не отлучиться. */
object ConstructionStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** «Пропавшая дружина» на шаге `step`; `gone` — дружина брошена. */
  private def quest(step: Int, gone: Boolean = false): NpcQuests =
    NpcQuests.empty.take(NpcQuest.LostSquad, 0L).update(NpcQuest.LostSquad)(_.copy(step = step, bonus = gone))

  private def make(quests: Option[NpcQuests] = None) =
    for {
      d   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = 100L))
      _   <- ZIO.foreachDiscard(quests)(NpcQuestLog.save(d, userId, _))
      sch <- TestScheduler.make
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (ConstructionState(d, sch, c, Some(TestInventoryRepository.withItems(Nil))), d, r)

  override def spec = suite("Стройка")(

    test("в меню — прораб: о нём сказано в описании, его кнопка — под работами, над «Назад»") {
      for {
        t <- make()
        (site, _, r) = t
        _    <- site.enter(testUser, r)
        menu <- r.sentScreens.map(_.last)
      } yield assertTrue(menu.text.contains("прораб Гуннар Отвес")) &&
              assertTrue(menu.choices.map(_.id) ==
                List("CarryStone", "BreakWall", "BuildWall", "Foreman", "LeaveConstruction", CityExit.route._1)) &&
              assertTrue(menu.choices.find(_.id == "Foreman").exists(_.label == "👷 Прораб Гуннар Отвес"))
    },

    test("Гуннар без задания: кто он и про работу, о дружине ни слова; «Назад» — снова меню") {
      for {
        t <- make()
        (site, _, r) = t
        to     <- site.action(testUser, tap("Foreman"), r)
        gunnar <- r.sentScreens.map(_.last)
        back   <- site.action(testUser, tap("BackFromForeman"), r)
        menu   <- r.sentScreens.map(_.last)
      } yield assertTrue(to == StateType.Construction && back == StateType.Construction) &&
              assertTrue(gunnar.text.contains("коренастый седой гном") && gunnar.text.contains("Работа есть всегда")) &&
              assertTrue(!gunnar.text.contains("Двенадцать фонарей")) &&
              assertTrue(gunnar.choices.map(_.id) == List("BackFromForeman")) &&
              assertTrue(menu.choices.exists(_.id == "Foreman"))
    },

    test("Гуннар, пока дружина под землёй: ждёт своих из штольни под Серебряной жилой; ящика нет — нет и кнопки") {
      for {
        t <- make(Some(quest(1)))
        (site, _, r) = t
        _      <- site.action(testUser, tap("Foreman"), r)
        gunnar <- r.sentScreens.map(_.last)
      } yield assertTrue(gunnar.text.contains("коренастый седой гном") && gunnar.text.contains("пустые носилки")) &&
              assertTrue(gunnar.text.contains("под Серебряной жилой")) &&
              assertTrue(gunnar.choices.map(_.id) == List("BackFromForeman"))
    },

    test("посреди смены старые кнопки прораба и ящика показывают экран работы: смена цела, ящик на руках") {
      for {
        t <- make(Some(quest(4, gone = true)))
        (site, d, r) = t
        _     <- site.action(testUser, tap("CarryStone"), r)
        talk  <- site.action(testUser, tap("Foreman"), r)
        s1    <- r.sentScreens.map(_.last)
        crate <- site.action(testUser, tap("SquadCrate"), r)
        s2    <- r.sentScreens.map(_.last)
        q     <- NpcQuestLog.load(d, userId)
        hero  <- d.getHeroByUserId(userId)
        scene <- d.readSceneData(userId)
      } yield assertTrue(talk == StateType.Construction && crate == StateType.Construction) &&
              assertTrue(s1.text.contains("заняты работой") && s2.text == s1.text) &&
              assertTrue(LostSquadQuest.crateOnHands(q) && hero.exists(_.silver == 100L)) &&
              assertTrue(scene.exists(_.hcursor.get[String]("constructionKind").toOption.contains("CarryStone")))
    }
  )
}
