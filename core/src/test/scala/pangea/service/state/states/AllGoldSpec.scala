package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.hero.{Achievement, AchievementRates, Equipment, Hero}
import pangea.model.item.{Grip, Item, ItemType, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** «Весь в золоте»: всё надетое легендарное — одиннадцать мест на теле и обе
  * руки (двуручное закрывает обе). Проверяется при надевании, как набор. */
object AllGoldSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def selectItem(itemId: Long): UserAction =
    UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}$itemId"}"""))

  private def piece(id: Long, t: ItemType, rarity: Rarity = Rarity.Orange, grip: Option[Grip] = None): Item =
    Item(id, "Предмет", 1L, rarity, t,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = grip)

  /** Одиннадцать мест на теле — этой редкости; руки пусты. */
  private def body(rarity: Rarity = Rarity.Orange): Equipment =
    TestFixtures.emptyEquipment.copy(
      helmet = piece(801L, ItemType.Helmet, rarity), shoulderPads = piece(802L, ItemType.ShoulderPads, rarity),
      chestPlate = piece(803L, ItemType.ChestPlate, rarity), bracelets = piece(804L, ItemType.Bracelets, rarity),
      gloves = piece(805L, ItemType.Gloves, rarity), pants = piece(806L, ItemType.Pants, rarity),
      boots = piece(807L, ItemType.Boots, rarity), amulet = piece(808L, ItemType.Amulet, rarity),
      firstRing = piece(809L, ItemType.Ring, rarity), secondRing = piece(810L, ItemType.Ring, rarity),
      belt = piece(811L, ItemType.Belt, rarity))

  private val oneHand = Some(Grip.OneHanded)
  private val twoHand = Some(Grip.TwoHanded)

  /** Надеть вещь из сумки: что сказали и каким стал герой. */
  private def equipFromBag(eq: Equipment, item: Item, already: List[String] = Nil) =
    for {
      dao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(equipment = eq, achievements = already))
      inv    = TestInventoryRepository.withItems(List(item))
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      state  = InventoryState(dao, inv, TestItemRepository.make, c, None)
      _     <- state.action(testUser, selectItem(item.id), r)
      _     <- state.action(testUser, tap("Equip"), r)
      said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      after <- dao.getHeroByUserId(userId).map(_.get)
    } yield (said, after)

  private val Line = "Вы получили достижение «Весь в золоте». +2% к атаке, защите, уклонению, точности и броне."

  override def spec = suite("«Весь в золоте»")(

    test("двуручное легендарное к легендарному телу — достижение") {
      for {
        got <- equipFromBag(body(), piece(50L, ItemType.Weapon, grip = twoHand))
      } yield assertTrue(got._1.contains(Line)) && assertTrue(got._2.hasAchievement(Achievement.AllGold))
    },

    test("одноручное: нужна и вторая рука — пустая левая не в счёт") {
      val withLeft = body().copy(leftHand = piece(60L, ItemType.Shield))
      for {
        bare <- equipFromBag(body(), piece(50L, ItemType.Weapon, grip = oneHand))
        full <- equipFromBag(withLeft, piece(50L, ItemType.Weapon, grip = oneHand))
      } yield assertTrue(!bare._2.hasAchievement(Achievement.AllGold)) &&
              assertTrue(full._1.contains(Line) && full._2.hasAchievement(Achievement.AllGold))
    },

    test("одна не легендарная вещь — достижения нет; доп. слот не нужен") {
      val oneEpic = body().copy(belt = piece(811L, ItemType.Belt, Rarity.Purple))
      for {
        epic <- equipFromBag(oneEpic, piece(50L, ItemType.Weapon, grip = twoHand))
      } yield assertTrue(!epic._2.hasAchievement(Achievement.AllGold)) &&
              assertTrue(Achievement.allGold(body().copy(weapon = piece(50L, ItemType.Weapon, grip = twoHand))))
    },

    test("выдаётся один раз") {
      for {
        again <- equipFromBag(body(), piece(50L, ItemType.Weapon, grip = twoHand),
                   already = List(Achievement.AllGold.entryName))
      } yield assertTrue(!again._1.contains("Вы получили достижение")) &&
              assertTrue(again._2.achievements == List(Achievement.AllGold.entryName))
    },

    test("+2% к атаке, защите, уклонению, точности и потолку брони — и без золота на себе") {
      val bare = TestFixtures.hero(userId).copy(
        fightStats = FightStats(atk = 1000, hp = 100, armor = 0, defence = 1000,
                                evasion = 1000, accuracy = 1000, energy = 0),
        equipment  = TestFixtures.emptyEquipment.copy(chestPlate =
                       Item(70L, "Кираса", 1L, Rarity.Blue, ItemType.ChestPlate,
                         attack = 0, accuracy = 0, energy = 0, armor = 1000, defence = 0, evasion = 0)))
      val gold: Hero = bare.withAchievement(Achievement.AllGold)
      val now        = gold.effectiveFightStats(0L)
      assertTrue(AchievementRates.AllGoldStatPct == 2L) &&
      assertTrue(now.atk == 1020L && now.defence == 1020L && now.evasion == 1020L && now.accuracy == 1020L) &&
      assertTrue(bare.maxArmor == 1000L && gold.maxArmor == 1020L)
    }
  )
}
