package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Строка удара моба говорит, сколько он реально снял: стихия его оружия
  * (огонь демона бьёт по HP на 10% сильнее) учтена, и видно, чем он бьёт. */
object MobElementHitSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** Герой без брони и защиты, который не уворачивается. */
  private val hero: Hero = TestFixtures.hero(userId).copy(
    lvl        = 10L,
    fightStats = FightStats(atk = 1, hp = 10000, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    baseStats  = BaseStats(agi = 1, vit = 10, str = 1, int = 1))

  private def foe(race: Race): SoloPveBattle =
    SoloPveBattle.from(Monster(0L, 10L, race, Rarity.Common,
      FightStats(atk = 1000, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 10000, energy = 0)), hero)
      .copy(noKin = true)

  /** Ход героя и что из него вышло: сколько HP он потерял и что увидел. */
  private def turn(b: SoloPveBattle, action: String, rolls: Task[Unit]): Task[(Long, String)] =
    for {
      dao   <- TestHeroDao.withHero(userId, hero)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      _     <- rolls
      _     <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap(action), r)
      after <- dao.getHeroByUserId(userId).map(_.get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (hero.fightStats.hp - after.fightStats.hp, log)

  /** Герой мажет, моб попадает; разбросы ровные. */
  private val strike = TestRandom.feedInts(1, 99, 99) *> TestRandom.feedLongs(100L, 100L)

  private val Hit = """наносит (\d+) урона""".r

  private def shown(log: String): Option[Long] = Hit.findFirstMatchIn(log).map(_.group(1).toLong)

  override def spec = suite("Удар моба со стихией")(

    test("демон бьёт огнём: в строке — то, что реально снято с HP (с прибавкой огня), и приписка «огнём 🔥»") {
      for {
        demon  <- turn(foe(Race.Demon), "Attack", strike)
        goblin <- turn(foe(Race.Goblin), "Attack", strike)
      } yield assertTrue(demon._2.contains("урона огнём 🔥") && shown(demon._2).contains(demon._1)) &&
              assertTrue(!goblin._2.contains("огнём") && shown(goblin._2).contains(goblin._1))
    },

    test("удар в спину при бегстве — тем же расчётом: огонь демона в нём есть, и в строке то же число") {
      for {
        // удар в спину достаёт героя; разброс ровный
        fled <- turn(foe(Race.Demon), "ConfirmFlee", TestRandom.feedInts(99, 99) *> TestRandom.feedLongs(100L))
      } yield assertTrue(fled._2.contains("урона огнём 🔥") && shown(fled._2).contains(fled._1) && fled._1 > 0L)
    }
  )
}
