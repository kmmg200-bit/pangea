package pangea.service.chat

import pangea.engine.{Renderer, SceneContent}
import pangea.model.hero.{Achievement, Hero}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.{PlayerLock, State, StateHandler, UserAction}
import pangea.test._
import zio.{Task, ZIO}
import zio.test._

/** «Дать Торопыга» в беседе ответом на сообщение игрока: «Админ 1» выдаёт
  * достижение тому, на чьё сообщение ответил; от прочих команда молча
  * пропускается. */
object ChatGrantSpec extends ZIOSpecDefault {

  private val adminId  = UserId(1L)
  private val playerId = UserId(2L)
  private val admin    = User(adminId, VkId("41"), TelegramId("tg1"))
  private val player   = User(playerId, VkId("42"), TelegramId("tg2"))
  private val chat     = 2000000001L

  /** Панель, где «Админ 1» — только `adminId`. */
  private val panel: AdminPanel = new AdminPanel {
    def intercept(user: User, hero: Hero, action: UserAction, renderer: Renderer): Task[Boolean] = ZIO.succeed(false)
    def homeWatchers(except: UserId): Task[List[VkId]] = ZIO.succeed(Nil)
    def canGrant(userId: UserId): Task[Boolean] = ZIO.succeed(userId == adminId)
  }

  private def handler(h: Hero) =
    for {
      heroDao   <- TestHeroDao.withHero(playerId, h)
      heroRepo  <- TestHeroRepository.withHero(playerId, h)
      userRepo  <- TestUserRepository.withUsers(admin, player)
      api       <- TestApi.make
      content   <- ZIO.attempt(SceneContent.load())
      lock      <- PlayerLock.make
      payouts    = Payouts(TestPayoutDao.empty, heroDao, content)
      parcels    = Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
      transfers  = Transfers(TestInventoryRepository.accepting, userRepo, parcels, new TestPlayers, content,
                     heroDao, TestBankRepository.empty, payouts, lock)
    } yield (new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers,
               Map.empty[StateType, State], panel, content, lock), heroDao, api)

  private val hero = TestFixtures.hero(playerId).copy(name = Some("Ярик"))

  override def spec = suite("«Дать» в беседе")(

    test("команду узнают в любом написании; выдаётся словом только «Торопыга»") {
      assertTrue(ChatCommand.grantCommand("Дать Торопыга").contains(Achievement.Hasty)) &&
      assertTrue(ChatCommand.grantCommand("  дать   торопыга! ").contains(Achievement.Hasty)) &&
      assertTrue(ChatCommand.grantCommand("Дать Мерзавец").isEmpty && ChatCommand.grantCommand("Дать").isEmpty) &&
      assertTrue(ChatCommand.grantCommand("Передать Торопыга").isEmpty && ChatCommand.grantCommand("торопыга").isEmpty)
    },

    test("«Торопыга» — +1 к силе, ловкости, телосложению и интеллекту") {
      val got = hero.withAchievement(Achievement.Hasty)
      val b   = hero.nativeBaseStats
      val a   = got.nativeBaseStats
      assertTrue(a.str == b.str + 1L && a.agi == b.agi + 1L && a.vit == b.vit + 1L && a.int == b.int + 1L) &&
      assertTrue(Achievement.Hasty.bonusLine == "+1 ко всем характеристикам")
    },

    test("«Админ 1» даёт: игроку — строка и приписка, в беседу — подтверждение; второй раз — «уже есть»") {
      for {
        t <- handler(hero)
        (state, dao, api) = t
        _      <- state.grantFromChat(admin.vkId, player.vkId, Achievement.Hasty, ChatCommand.eventKey(chat, 0L, Some(1L)), chat)
        after  <- dao.getHeroByUserId(playerId).map(_.get)
        dm     <- api.sentMessages.map(_.map(_._1))
        _      <- state.grantFromChat(admin.vkId, player.vkId, Achievement.Hasty, ChatCommand.eventKey(chat, 0L, Some(2L)), chat)
        said   <- api.chatMessages
        again  <- dao.getHeroByUserId(playerId).map(_.get)
      } yield assertTrue(after.hasAchievement(Achievement.Hasty)) &&
              assertTrue(dm.size == 1 && dm.head.contains("«Торопыга»") && dm.head.contains("Вы так торопили админа")) &&
              assertTrue(said == List("🏆 Ярик получает достижение «Торопыга». +1 ко всем характеристикам.",
                                      "Ярик: достижение «Торопыга» уже есть.")) &&
              assertTrue(again.achievements.count(_ == Achievement.Hasty.entryName) == 1)
    },

    test("не «Админ 1» — молча мимо; повтор того же сообщения тоже") {
      for {
        t <- handler(hero)
        (state, dao, api) = t
        _      <- state.grantFromChat(player.vkId, player.vkId, Achievement.Hasty, ChatCommand.eventKey(chat, 0L, Some(1L)), chat)
        _      <- state.grantFromChat(VkId("777"), player.vkId, Achievement.Hasty, ChatCommand.eventKey(chat, 0L, Some(2L)), chat)
        after  <- dao.getHeroByUserId(playerId).map(_.get)
        said   <- api.chatMessages
        dm     <- api.sentMessages
      } yield assertTrue(!after.hasAchievement(Achievement.Hasty) && said.isEmpty && dm.isEmpty)
    }
  )
}
