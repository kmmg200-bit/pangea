package pangea.service.state

import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.{CubeCraft, MaterialGenerator}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemSet, ItemType, MaterialKind, Rarity}
import pangea.model.quest.{DailyKind, DailyTask}
import pangea.model.rune.RuneData
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.states.{InventoryState, LootState}
import pangea.service.storage.ItemStore.Place
import pangea.test._
import zio.ZIO
import zio.test._

/** Сюжетная экипировка (`Item.isStoryGear`): ★ вместо кружка редкости; её не
  * передают, не продают хламом, не отдают «всё разом», не кладут в бочку, не
  * выбрасывают и не теряют — не влезла в сумку, уходит посылкой, на почте не
  * сгорает. В кубе — только смена набора. Старые вещи без поля — обычные. */
object StoryGearSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val content  = ZIO.attempt(SceneContent.load())
  private val day      = 24L * 60L * 60L * 1000L

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId).copy(lvl = 20L)

  /** Сюжетный шлем — с кружком редкости в имени, как у сгенерированных вещей. */
  private def storyHelm(id: Long): Item =
    Item(id, "🟣 Шлем старшины", 20L, Rarity.Violet, ItemType.Helmet,
      attack = 0, accuracy = 3, energy = 0, armor = 40, defence = 5, evasion = 0).asStoryGear

  private def plainHelm(id: Long): Item = storyHelm(id).copy(name = "🟣 Шлем часового", story = None)

  private def storyAxe(id: Long, rarity: Rarity): Item =
    Item(id, "Секира рубежа", 25L, rarity, ItemType.Weapon,
      attack = 90, accuracy = 5, energy = 0, armor = 0, defence = 0, evasion = 0).asStoryGear

  /** Передача — как в MailShelfSpec: из её проверок здесь нужны только «что можно отдать». */
  private def transfers =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      userRepo <- TestUserRepository.withUsers(testUser)
      c        <- content
      lock     <- PlayerLock.make
      inv       = TestInventoryRepository.withItems(Nil)
      bank      = TestBankRepository.empty
      parcels   = Parcels(TestParcelDao.empty, bank, c)
      payouts   = Payouts(TestPayoutDao.empty, heroDao, c)
    } yield Transfers(inv, userRepo, parcels, new TestPlayers, c, heroDao, bank, payouts, lock)

  override def spec = suite("Сюжетная экипировка")(

    test("старая вещь без поля «story» (и без набора и хвата) читается обычной, сюжетная — сюжетной") {
      val plain  = plainHelm(7L)
      val stored = plain.asJson.asObject.map(_.remove("story").remove("set").remove("grip").asJson.noSpaces).getOrElse("")
      assertTrue(!stored.contains("story")) &&
      assertTrue(decode[Item](stored).exists(i => !i.isStoryGear && i.name == plain.name && i.armor == 40L)) &&
      assertTrue(decode[Item](storyHelm(8L).asJson.noSpaces).exists(_.isStoryGear))
    },

    test("в списке — ★ вместо кружка редкости, уровень на месте; на карточке — строка о том, что с ней можно") {
      val h = hero
      assertTrue(storyHelm(1L).displayTitle == "★ [Ур.20] Шлем старшины") &&
      assertTrue(plainHelm(2L).displayTitle == "🟣 [Ур.20] Шлем часового") &&
      assertTrue(InventoryState.itemText(storyHelm(1L), h.equipment, Some(h)).contains(Item.StoryGearNote)) &&
      assertTrue(!InventoryState.itemText(plainHelm(2L), h.equipment, Some(h)).contains(Item.StoryGearNote))
    },

    test("новые клинок старого мурлока и шлем «Двенадцать фонарей» — сюжетные, без кружка редкости в имени") {
      val blade  = MurlocQuest.blade(hero, Rng(1L))
      val helmet = LostSquadQuest.helmet(hero, Rng(2L))
      assertTrue(blade.isStoryGear && blade.displayTitle == s"★ [Ур.${blade.lvl}] ${blade.name}") &&
      assertTrue(helmet.isStoryGear && helmet.name == LostSquadQuest.HelmetName &&
        helmet.displayTitle == s"★ [Ур.${helmet.lvl}] ${LostSquadQuest.HelmetName}")
    },

    test("не передаётся, не уходит хламом, в огонь «всё разом», мурлокам и по поручению; бочка — нет, банк — да") {
      val story   = storyAxe(1L, Rarity.Green)
      val plain   = story.copy(id = 2L, story = None)
      val junkAll = MerchantState.JunkSaleSettings(rarities = Rarity.values.toSet)
      val burnAll = RuneData(burnRarities = Rarity.values.map(_.entryName).toSet)
      val guards  = DailyTask(DailyKind.GuardWeapons, day = 0L, goal = 3L, taken = true)
      for {
        t <- transfers
      } yield assertTrue(!t.sendable(story) && t.sendable(plain)) &&
              assertTrue(t.refusal(List(story), "Секира").contains("transfer.noStoryGear")) &&
              assertTrue(!MerchantState.isJunk(story, junkAll) && MerchantState.isJunk(plain, junkAll)) &&
              assertTrue(!burnAll.burnsGear(story) && burnAll.burnsGear(plain)) &&
              assertTrue(MurlocQuest.gear(List(story, plain)) == List(plain)) &&
              assertTrue(guards.toGive(List(story, plain)) == List(plain)) &&
              assertTrue(!Place.Barrel.accepts(story) && Place.Barrel.accepts(plain) && Place.Vault.accepts(story))
    },

    test("в кубе — только смена набора: ★ и имя остаются; перековки мифрилом нет") {
      val helm    = storyHelm(1L)
      val iron    = MaterialGenerator.item(MaterialKind.EverburningIron).copy(id = 2L)
      val infused = CubeCraft.craft(List(helm, iron), charges = 5, Rng(3L))
      val legend  = storyAxe(3L, Rarity.Orange)
      val mithril = MaterialGenerator.mithril.copy(id = 4L)
      val reforge = CubeCraft.craft(List(legend, mithril), charges = 5, Rng(4L))
      assertTrue(infused.chargesUsed == 1) &&
      assertTrue(infused.items.exists(i => i.isStoryGear && i.name == helm.name && i.set.contains(ItemSet.WildFlame))) &&
      assertTrue(reforge.chargesUsed == 0 && reforge.items.contains(legend))
    },

    test("на почте сюжетное не сгорает и срока почты не начинает") {
      for {
        c       <- content
        dao      = TestParcelDao.empty
        parcels  = Parcels(dao, TestBankRepository.empty, c)
        h        = hero
        line    <- StoryGearPost.send(parcels, h, "с добычи", List(storyHelm(1L)), 0L)
        month   <- dao.burn(h.id, 30 * day - Parcels.ShelfMs)
        sent    <- parcels.send(h.id, "Пётр", plainHelm(2L), 30 * day)
        _       <- parcels.arrive(h.id, List(sent.id), 30 * day)
        listed  <- parcels.waiting(h.id)
        later   <- dao.burn(h.id, 37 * day - Parcels.ShelfMs)
      } yield assertTrue(line.exists(_.contains("Сюжетное на почте не сгорает")) && month.isEmpty) &&
              assertTrue(Parcels.burnsAt(listed).contains(30 * day + Parcels.ShelfMs)) &&
              assertTrue(later.map(_.item.id) == List(2L) && dao.snapshot.map(_.item.id) == List(1L))
    },

    test("добыча: не влезла в сумку — посылкой; «Оставить» сюжетную не бросает") {
      def loot(full: Boolean) =
        for {
          c       <- content
          dao      = TestParcelDao.empty
          heroDao <- TestHeroDao.withHero(userId, hero)
          _       <- heroDao.writeSceneData(userId, LootData(items = List(storyHelm(-1L)), silvers = Nil).asJson)
          r       <- TestRenderer.make
          j       <- TestJournal.make
          inv      = if (full) TestInventoryRepository.full else TestInventoryRepository.accepting
          state    = LootState(heroDao, inv, TestItemRepository.make, j, c, None, Some(Parcels(dao, TestBankRepository.empty, c)))
        } yield (state, r, inv, dao)
      for {
        full  <- loot(full = true)
        (st1, r1, _, dao1) = full
        _     <- st1.action(testUser, tap("Take"), r1)
        said1 <- r1.sentScreens.map(_.map(_.text).mkString("\n"))
        room  <- loot(full = false)
        (st2, r2, inv2, dao2) = room
        _     <- st2.action(testUser, tap("Leave"), r2)
        said2 <- r2.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(dao1.snapshot.exists(_.item.isStoryGear) && said1.contains("Сюжетное на почте не сгорает")) &&
              assertTrue(inv2.snapshot.exists(_.isStoryGear) && dao2.snapshot.isEmpty && said2.contains("Сюжетное не бросают"))
    },

    test("карточка в сумке: «Выбросить» у сюжетной нет, а старая кнопка её не выбросит") {
      val helm = storyHelm(5L)
      for {
        c       <- content
        heroDao <- TestHeroDao.withHero(userId, hero)
        inv      = TestInventoryRepository.withItems(List(helm))
        state    = InventoryState(heroDao, inv, TestItemRepository.make, c)
        r       <- TestRenderer.make
        _       <- state.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${helm.id}"}""")), r)
        card    <- r.sentScreens.map(_.last)
        _       <- state.action(testUser, tap("Drop"), r)
        said    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(!card.choices.exists(_.id == "Drop") && card.choices.exists(_.id == "Equip")) &&
              assertTrue(inv.snapshot.exists(_.id == helm.id) && said.contains("Сюжетную вещь не выбросить"))
    }
  )
}
