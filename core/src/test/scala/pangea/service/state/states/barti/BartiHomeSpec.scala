package pangea.service.state.states.barti

import pangea.engine.SceneContent
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.MarketSquareState
import pangea.service.state.{BartiHome, NpcQuestLog}
import pangea.test._
import zio.ZIO
import zio.test._

/** Дом Барти — его место: после «Большого дела» кнопка на Торговой площади —
  * насовсем, а в доме — экран с его заданиями (кнопка на каждое). */
object BartiHomeSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private val content = ZIO.attempt(SceneContent.load())

  /** «Ходовой товар» закрыт; «Большое дело» — по `done`. */
  private def deal(done: Boolean): NpcQuests = {
    val goods = NpcQuests.empty.take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods).take(NpcQuest.BigDeal, 0L)
    if (done) goods.finish(NpcQuest.BigDeal) else goods
  }

  override def spec = suite("Дом Барти — его место")(

    test("кнопка «Дом Барти» на площади: после «Большого дела» — есть, до него — нет; Барти дома, пока «Яма» не опечатала дверь") {
      val hero = TestFixtures.hero(userId)
      assertTrue(BartiHome.onSquare(deal(done = true)) && !BartiHome.onSquare(deal(done = false))) &&
        assertTrue(BartiHome.atHome(deal(done = true), hero, story = false) &&
          !BartiHome.atHome(deal(done = false), hero, story = false)) &&
        assertTrue(BartiHome.errands(deal(done = true), story = false).isEmpty &&
          BartiHome.errands(deal(done = true), story = true) == List(BartiHome.Errand.Morning))
    },

    test("после «Большого дела»: на площади «Дом Барти», в доме — Барти у себя; заданий нет — мешок наудачу и «Уйти» на площадь") {
      for {
        c      <- content
        d      <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = StateType.MarketSquare))
        _      <- NpcQuestLog.save(d, userId, deal(done = true))
        r      <- TestRenderer.make
        _      <- MarketSquareState(c, Some(d)).enter(testUser, r)
        square <- r.sentScreens.map(_.last)
        go     <- MarketSquareState(c, Some(d)).action(testUser, pangea.service.state.UserAction("", Some("""{"action":"BartiHome"}""")), r)
        house   = BartiHouseState(d, TestInventoryRepository.withItems(Nil), c, Some(TestItemRepository.make))
        r2     <- TestRenderer.make
        _      <- house.enter(testUser, r2)
        home   <- r2.sentScreens.map(_.last)
        out    <- house.action(testUser, pangea.service.state.UserAction("", Some("""{"action":"BartiLeave"}""")), r2)
      } yield assertTrue(square.choices.exists(_.id == "BartiHome") && !square.choices.exists(_.id == "BartiShop") &&
                go == StateType.BartiHouse) &&
              assertTrue(home.text.contains("Дом Барти за синей дверью") && home.text.contains("Ну, почти всегда") &&
                home.choices.map(_.id) == List("BartiLottery", "BartiLeave") && out == StateType.MarketSquare)
    }
  )
}
