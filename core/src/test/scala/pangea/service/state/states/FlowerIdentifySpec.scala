package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.{Hero, Knowledge, LoreData}
import pangea.model.item.{Item, MaterialKind}
import pangea.model.stats.BaseStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{HerbLore, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Опознать цветок из сумки: странный — на знание простых трав, необычный — на
  * знание редких. Не знаешь — цветок рассыпается, а бросок на догадку тот же,
  * что при сборе; знаешь — он становится случайной травой своего ранга. */
object FlowerIdentifySpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def selectItem(id: Long): UserAction =
    UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}$id"}"""))

  private def hero(int: Long = 40L): Hero =
    TestFixtures.hero(userId).copy(baseStats = BaseStats(str = 5L, vit = 5L, agi = 5L, int = int))

  private def flower(kind: MaterialKind, id: Long): Item = MaterialGenerator.item(kind).copy(id = id)

  private val rank1 = LoreData.empty.learn(Knowledge.FlowersRank1, alone = false)
  private val both  = rank1.learn(Knowledge.FlowersRank2, alone = false)

  private def make(lore: LoreData, items: List[Item], h: Hero = hero()) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeLoreData(userId, lore.asJson)
      inv  = TestInventoryRepository.withItems(items)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (InventoryState(dao, inv, TestItemRepository.make, c), dao, inv, r)

  private def loreOf(dao: TestHeroDao): Task[LoreData] = HerbLore.readLore(dao, userId)
  private def texts(r: TestRenderer)                  = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Опознание цветка")(

    test("на карточке странного и необычного цветка — «Опознать»; у травы её нет") {
      for {
        t <- make(LoreData.empty, List(flower(MaterialKind.StrangeFlower, 1L), flower(MaterialKind.UnusualFlower, 2L),
               flower(MaterialKind.Nettle, 3L)))
        (state, _, _, r) = t
        _       <- state.action(testUser, selectItem(1L), r)
        strange <- r.sentScreens.map(_.last)
        _       <- state.action(testUser, selectItem(2L), r)
        unusual <- r.sentScreens.map(_.last)
        _       <- state.action(testUser, selectItem(3L), r)
        nettle  <- r.sentScreens.map(_.last)
      } yield assertTrue(strange.choices.exists(c => c.id == "IdentifyFlower" && c.label == "🔍 Опознать")) &&
              assertTrue(unusual.choices.exists(_.id == "IdentifyFlower") && unusual.text.contains("Необычный цветок")) &&
              assertTrue(!nettle.choices.exists(_.id == "IdentifyFlower"))
    },

    test("странный цветок без знаний: рассыпается, бросок на простые травы (интеллект ÷ 4) — неудача, потом удача") {
      for {
        t <- make(LoreData.empty, List(flower(MaterialKind.StrangeFlower, 1L), flower(MaterialKind.StrangeFlower, 2L)))
        (state, dao, inv, r) = t
        // 40 ÷ 4 = 10: бросок 11 — мимо
        _     <- TestRandom.feedInts(11)
        _     <- state.action(testUser, selectItem(1L), r)
        _     <- state.action(testUser, tap("IdentifyFlower"), r)
        miss  <- loreOf(dao)
        bag1   = inv.snapshot.map(_.id)
        said1 <- texts(r)
        // бросок 10 — в цель; из стопки тратится один цветок, второй остаётся
        _     <- TestRandom.feedInts(10)
        _     <- state.action(testUser, selectItem(bag1.headOption.getOrElse(0L)), r)
        _     <- state.action(testUser, tap("IdentifyFlower"), r)
        hit   <- loreOf(dao)
        said2 <- texts(r)
      } yield assertTrue(bag1.size == 1 && !miss.knows(Knowledge.FlowersRank1)) &&
              assertTrue(said1.contains("рассыпается в труху") && said1.contains("Понять так ничего и не удалось")) &&
              assertTrue(inv.snapshot.isEmpty && hit.knows(Knowledge.FlowersRank1) && hit.learnedAlone(Knowledge.FlowersRank1)) &&
              assertTrue(said2.contains("стал лучше разбираться"))
    },

    test("знаешь простые травы — странный цветок становится случайной травой 1 ранга, на месте") {
      for {
        t <- make(rank1, List(flower(MaterialKind.StrangeFlower, 7L)))
        (state, _, inv, r) = t
        _    <- TestRandom.feedInts(2)
        _    <- state.action(testUser, selectItem(7L), r)
        _    <- state.action(testUser, tap("IdentifyFlower"), r)
        card <- r.sentScreens.map(_.last)
        said <- texts(r)
        herb  = MaterialKind.herbsOfRank(1)(2)
      } yield assertTrue(inv.snapshot.map(i => i.id -> i.material) == List(7L -> Some(herb))) &&
              assertTrue(said.contains(s"и теперь узнаёте его: это ${herb.displayName}")) &&
              assertTrue(card.text.contains(herb.displayName) && !card.choices.exists(_.id == "IdentifyFlower"))
    },

    test("необычный цветок — всегда на знание редких трав (интеллект ÷ 8), даже без простых; зная их — трава 2 ранга") {
      for {
        t1 <- make(LoreData.empty, List(flower(MaterialKind.UnusualFlower, 1L)))
        (s1, d1, inv1, r1) = t1
        // 40 ÷ 8 = 5: бросок 5 — в цель, и это знание о редких
        _    <- TestRandom.feedInts(5)
        _    <- s1.action(testUser, selectItem(1L), r1)
        _    <- s1.action(testUser, tap("IdentifyFlower"), r1)
        l1   <- loreOf(d1)
        said <- texts(r1)
        t2 <- make(both, List(flower(MaterialKind.UnusualFlower, 9L)))
        (s2, _, inv2, r2) = t2
        _    <- TestRandom.feedInts(0)
        _    <- s2.action(testUser, selectItem(9L), r2)
        _    <- s2.action(testUser, tap("IdentifyFlower"), r2)
      } yield assertTrue(inv1.snapshot.isEmpty && l1.knows(Knowledge.FlowersRank2) && !l1.knows(Knowledge.FlowersRank1)) &&
              assertTrue(said.contains("узнаю и редкие цветы")) &&
              assertTrue(inv2.snapshot.map(_.material) == List(Some(MaterialKind.herbsOfRank(2)(0))))
    },

    test("сорванная редкая трава без знаний — «Необычный цветок», простая — «Странный»; догадка у каждого своя") {
      assertTrue(HerbLore.recognised(LoreData.empty, MaterialKind.herbsOfRank(2)(0)) == MaterialKind.UnusualFlower) &&
        assertTrue(HerbLore.recognised(rank1, MaterialKind.herbsOfRank(2)(0)) == MaterialKind.UnusualFlower) &&
        assertTrue(HerbLore.recognised(LoreData.empty, MaterialKind.herbsOfRank(1)(0)) == MaterialKind.StrangeFlower) &&
        assertTrue(HerbLore.recognised(both, MaterialKind.herbsOfRank(2)(0)) == MaterialKind.herbsOfRank(2)(0)) &&
        assertTrue(HerbLore.insightTarget(LoreData.empty, MaterialKind.StrangeFlower).contains(Knowledge.FlowersRank1)) &&
        assertTrue(HerbLore.insightTarget(LoreData.empty, MaterialKind.UnusualFlower).contains(Knowledge.FlowersRank2)) &&
        assertTrue(HerbLore.insightTarget(rank1, MaterialKind.StrangeFlower).isEmpty) &&
        assertTrue(HerbLore.insightTarget(both, MaterialKind.UnusualFlower).isEmpty) &&
        assertTrue(HerbLore.price(MaterialKind.UnusualFlower) == HerbLore.StrangeFlowerPrice && MaterialKind.UnusualFlower.isHerb)
    }
  )
}
