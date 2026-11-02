package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.ItemSet
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Легендарный моб смертной расы роняет ровно одну вещь малого набора своей
  * расы; прочие — ни одной. */
object RaceSetDropSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val hero: Hero = TestFixtures.hero(userId, dungeonLevel = 7).copy(
    fightStats = FightStats(atk = 100000L, hp = 500000L, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  private def mob(race: Race, rarity: Rarity): Monster =
    Monster(0L, 1L, race, rarity, FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  /** Убить одного моба с одного удара — и что легло в добычу. */
  private def lootOf(m: Monster, story: Option[String] = None): Task[(StateType, LootData)] =
    for {
      dao <- TestHeroDao.withHero(userId, hero)
      _   <- dao.writeActiveBattle(userId, SoloPveBattle.from(m, hero, squad = false).copy(noKin = true, story = story).asJson)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
      _   <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
      res <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
      loot <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).getOrElse(LootData(Nil, Nil)))
    } yield (res, loot)

  override def spec = suite("Вещи малых расовых наборов в добыче")(

    test("легендарный орк роняет ровно одну вещь «Орды» уровня добычи") {
      for {
        t <- lootOf(mob(Race.Orc, Rarity.Legendary))
        (res, loot) = t
        sets = loot.items.filter(_.set.isDefined)
      } yield assertTrue(res == StateType.Loot) &&
              assertTrue(sets.size == 1 && sets.forall(_.set.contains(ItemSet.Horde))) &&
              assertTrue(sets.forall(_.lvl == hero.dungeonLevel.toLong))
    },

    test("не легендарный — ни одной; легендарный без своего набора (элементаль) — тоже") {
      for {
        common <- lootOf(mob(Race.Orc, Rarity.Mythical))
        elem   <- lootOf(mob(Race.Elemental, Rarity.Legendary))
      } yield assertTrue(!common._2.items.exists(_.set.isDefined)) &&
              assertTrue(!elem._2.items.exists(_.set.isDefined))
    }
  )
}
