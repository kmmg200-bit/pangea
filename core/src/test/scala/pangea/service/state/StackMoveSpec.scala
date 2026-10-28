package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.engine.{Choice, SceneContent, Screen}
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.{AzatState, CubeStatus}
import pangea.model.item.{BrewKind, Item, ItemDetails, ItemType, Rarity, TrophyKind}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.artifact.ArtifactState
import pangea.service.state.states.bank.BankVaultState
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.states.temple.CubeState
import pangea.service.state.states.{InventoryState, UnassumingBarrelState}
import pangea.test._
import zio.ZIO
import zio.test._

/** Стопка целиком: взял или положил одну — рядом с ней «⏬ все N», и всё это
  * одним сообщением; куб и артефакты выгребаются одной кнопкой; Ришелье берёт
  * стопку разом; из сумки её можно выбросить, но сперва спросят. */
object StackMoveSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def press(c: Choice): UserAction = tap(c.id, c.data.toSeq: _*)

  private def water(id: Long): Item = BrewKind.item(BrewKind.LivingWater).copy(id = id)
  private def waters(ids: Long*): List[Item] = ids.toList.map(water)

  private def sword(id: Long): Item =
    Item(id, s"Меч $id", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  /** Сумка (на двадцать мест), в которой свободно ровно `free`. */
  private def filler(free: Int): List[Item] = (1 to (20 - free)).toList.map(i => sword(100L + i))

  private def trophy(id: Long): Item =
    Item(id, "Голова (Человек)", 5L, Rarity.Gray, ItemType.Trophy,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Trophy("Human", TrophyKind.Head))

  /** Кнопка экрана по началу id. */
  private def button(screen: Screen, prefix: String): Option[Choice] = screen.choices.find(_.id.startsWith(prefix))

  private def content = ZIO.attempt(SceneContent.load())

  override def spec = suite("Стопка целиком")(

    test("хранилище: забрал одну — одним сообщением, и в ряду стопки «⏬ все 3»; нажал — остальное в сумку") {
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        inv   = TestInventoryRepository.withItems(Nil)
        bank  = TestBankRepository.of(1, waters(1L, 2L, 3L, 4L))
        r    <- TestRenderer.make
        c    <- content
        state = BankVaultState(dao, inv, bank, c)
        _    <- state.action(testUser, tap("VaultWithdrawItems"), r)
        _    <- r.reset
        _    <- state.action(testUser, tap("VaultTake_4"), r)
        one  <- r.sentScreens
        all   = one.lastOption.flatMap(button(_, BankVaultState.WithdrawAllPrefix))
        stack = one.lastOption.flatMap(button(_, BankVaultState.WithdrawItemPrefix))
        _    <- state.action(testUser, press(all.get), r)
        said <- r.sentScreens.map(_.last.text)
      } yield assertTrue(one.size == 1 && one.head.text.contains("Живая вода забран из хранилища.")) &&
              assertTrue(all.exists(_.label == "⏬ все 3") && all.flatMap(_.row) == stack.flatMap(_.row)) &&
              assertTrue(bank.itemsSnapshot.isEmpty && inv.snapshot.size == 4) &&
              assertTrue(said.contains("Забрано из хранилища: Живая вода ×3."))
    },

    test("хранилище: «все» забирает, сколько влезет, — об остальном так и сказано") {
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        inv   = TestInventoryRepository.withItems(filler(2))
        bank  = TestBankRepository.of(1, waters(1L, 2L, 3L, 4L))
        r    <- TestRenderer.make
        c    <- content
        state = BankVaultState(dao, inv, bank, c)
        _    <- state.action(testUser, tap("VaultTake_4"), r)
        all  <- r.sentScreens.map(s => button(s.last, BankVaultState.WithdrawAllPrefix))
        _    <- state.action(testUser, press(all.get), r)
        said <- r.sentScreens.map(_.last.text)
      } yield assertTrue(said.contains("Забрано из хранилища: Живая вода. Не поместилось: 2.")) &&
              assertTrue(bank.itemsSnapshot.size == 2 && inv.snapshot.size == 20)
    },

    test("бочка: положил одну — «⏬ все 2»; места на одну — одна и легла, о второй сказано") {
      for {
        dao    <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        inv     = TestInventoryRepository.withItems(waters(1L, 2L, 3L))
        barrel  = TestBarrelRepository.of((1L to 8L).toList.map(i => sword(50L + i)), 0L)
        r      <- TestRenderer.make
        c      <- content
        state   = UnassumingBarrelState(dao, inv, barrel, c)
        _      <- state.action(testUser, tap("DepositItem_3"), r)
        one    <- r.sentScreens
        all     = one.lastOption.flatMap(button(_, UnassumingBarrelState.DepositAllPrefix))
        _      <- state.action(testUser, press(all.get), r)
        said   <- r.sentScreens.map(_.last.text)
      } yield assertTrue(one.size == 1 && one.head.text.contains("Живая вода убран в бочку.")) &&
              assertTrue(all.exists(_.label == "⏬ все 2")) &&
              assertTrue(said.contains("Убрано в бочку: Живая вода. Не поместилось: 1.")) &&
              assertTrue(barrel.itemsSnapshot.size == 10 && inv.snapshot.map(_.id) == List(1L))
    },

    test("бочка: из стопки, которой нет рядом с только что взятой, кнопки «все» нет") {
      for {
        dao    <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        inv     = TestInventoryRepository.withItems(Nil)
        barrel  = TestBarrelRepository.of(waters(1L, 2L, 3L) :+ sword(9L), 0L)
        r      <- TestRenderer.make
        c      <- content
        state   = UnassumingBarrelState(dao, inv, barrel, c)
        _      <- state.action(testUser, tap("WithdrawItemsMenu"), r)
        list   <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("WithdrawItem_9"), r)
        after  <- r.sentScreens.map(_.last)
      } yield assertTrue(button(list, UnassumingBarrelState.WithdrawAllPrefix).isEmpty) &&
              // взяли меч — у отваров «все» так и не появилось
              assertTrue(button(after, UnassumingBarrelState.WithdrawAllPrefix).isEmpty) &&
              assertTrue(after.text.contains("Меч 9 забран из бочки."))
    },

    test("куб: «⏬ Забрать всё» выгребает его в сумку одним сообщением") {
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        _    <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = 5,
                  cubeItems = waters(1L, 2L) :+ sword(3L)).asJson)
        inv   = TestInventoryRepository.withItems(Nil)
        r    <- TestRenderer.make
        c    <- content
        state = CubeState(dao, inv, TestItemRepository.make, c)
        _    <- state.enter(testUser, r)
        menu <- r.sentScreens.map(_.last)
        _    <- r.reset
        _    <- state.action(testUser, tap("CubeTakeAll"), r)
        done <- r.sentScreens
        azat <- dao.readAzatData(userId).map(_.flatMap(_.as[AzatState].toOption).get)
      } yield assertTrue(menu.choices.exists(_.id == "CubeTakeAll")) &&
              assertTrue(done.size == 1 && done.head.text.contains("Забрано из куба: Живая вода ×2, Меч 3.")) &&
              // куб пуст — и кнопки больше нет
              assertTrue(!done.head.choices.exists(_.id == "CubeTakeAll")) &&
              assertTrue(azat.cubeItems.isEmpty && inv.snapshot.size == 3)
    },

    test("живая сумка: «⏬ Забрать всё» — сколько влезет, остальное ждёт внутри") {
      val repo = TestArtifactRepository.of(bag =
        TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1, items = waters(1L, 2L, 3L)))
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        inv   = TestInventoryRepository.withItems(filler(2))
        r    <- TestRenderer.make
        c    <- content
        state = ArtifactState(ArtifactKind.LivingBag, dao, inv, TestItemRepository.make, repo, c)
        _    <- state.action(testUser, tap("ArtifactTakeAll"), r)
        said <- r.sentScreens.map(_.last.text)
      } yield assertTrue(said.contains("Снова у вас в сумке: Живая вода ×2. Не поместилось: 1.")) &&
              assertTrue(repo.snapshot.of(ArtifactKind.LivingBag).items.data.size == 1 && inv.snapshot.size == 20)
    },

    test("живая сумка: положил одну — «⏬ все 2» кладёт остальное") {
      val repo = TestArtifactRepository.of(bag = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1))
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        inv   = TestInventoryRepository.withItems(waters(1L, 2L, 3L))
        r    <- TestRenderer.make
        c    <- content
        state = ArtifactState(ArtifactKind.LivingBag, dao, inv, TestItemRepository.make, repo, c)
        _    <- state.action(testUser, tap("ArtPut_3"), r)
        all  <- r.sentScreens.map(s => button(s.last, ArtifactState.PutAllPrefix))
        _    <- state.action(testUser, press(all.get), r)
        said <- r.sentScreens.map(_.last.text)
      } yield assertTrue(all.exists(_.label == "⏬ все 2")) &&
              assertTrue(said.contains("Внутри: Живая вода ×2. Свободно мест: 12")) &&
              assertTrue(inv.snapshot.isEmpty && repo.snapshot.of(ArtifactKind.LivingBag).items.data.size == 3)
    },

    test("Ришелье: у стопки на карточке «Продать все 3 — цена»; продал — серебро за все, одним сообщением") {
      val price = ((5L + 5) * 1.2 * Rarity.Gray.factorR).toLong.max(1L)
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(lvl = 10L, silver = 0L))
        inv   = TestInventoryRepository.withItems(List(trophy(1L), trophy(2L), trophy(3L)))
        r    <- TestRenderer.make
        c    <- content
        state = MerchantState(dao, inv, TestItemRepository.make, c)
        _    <- state.action(testUser, tap("Sell"), r)
        _    <- state.action(testUser, tap(s"${MerchantState.SellItemPrefix}3"), r)
        card <- r.sentScreens.map(_.last)
        all   = button(card, "ConfirmSellAll")
        _    <- r.reset
        _    <- state.action(testUser, press(all.get), r)
        done <- r.sentScreens
        hero <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(all.exists(_.label == s"🪙 Продать все 3 — ${3 * price}")) &&
              assertTrue(hero.silver == 3 * price && inv.snapshot.isEmpty) &&
              assertTrue(done.size == 1 && done.head.text.contains(s"Вы продали Голова (Человек) ×3 за ${3 * price} серебра."))
    },

    test("Ришелье: продал одну — строка о продаже над списком, а не отдельным сообщением") {
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(lvl = 10L, silver = 0L))
        inv   = TestInventoryRepository.withItems(List(trophy(1L), trophy(2L)))
        r    <- TestRenderer.make
        c    <- content
        state = MerchantState(dao, inv, TestItemRepository.make, c)
        _    <- state.action(testUser, tap("Sell"), r)
        _    <- state.action(testUser, tap(s"${MerchantState.SellItemPrefix}2"), r)
        _    <- r.reset
        _    <- state.action(testUser, tap("ConfirmSellItem"), r)
        done <- r.sentScreens
      } yield assertTrue(done.size == 1 && done.head.text.contains("Вы продали Голова (Человек) за")) &&
              assertTrue(inv.snapshot.map(_.id) == List(1L))
    },

    test("сумка: «🗑 Выбросить все 3» — сперва спросят; согласился — стопки нет, и сказано это над списком") {
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        inv   = TestInventoryRepository.withItems(waters(1L, 2L, 3L) :+ sword(9L))
        r    <- TestRenderer.make
        c    <- content
        state = InventoryState(dao, inv, TestItemRepository.make, c)
        _    <- state.action(testUser, tap(s"${InventoryState.ItemActionPrefix}3"), r)
        card <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("DropAll"), r)
        ask  <- r.sentScreens.map(_.last)
        yes   = button(ask, "DropAllYes")
        kept  = inv.snapshot.size
        _    <- state.action(testUser, press(yes.get), r)
        said <- r.sentScreens.map(_.last.text)
      } yield assertTrue(button(card, "DropAll").exists(_.label == "🗑 Выбросить все 3")) &&
              assertTrue(ask.text.contains("Выбросить Живая вода ×3? Вернуть их будет нельзя.") && kept == 4) &&
              assertTrue(inv.snapshot.map(_.id) == List(9L)) &&
              assertTrue(said.contains("Выброшено: Живая вода ×3."))
    },

    test("сумка: выбросил одну — та же карточка с меньшим счётчиком и строкой о выброшенном, одним сообщением") {
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        inv   = TestInventoryRepository.withItems(waters(1L, 2L, 3L))
        r    <- TestRenderer.make
        c    <- content
        state = InventoryState(dao, inv, TestItemRepository.make, c)
        _    <- state.action(testUser, tap(s"${InventoryState.ItemActionPrefix}3"), r)
        _    <- r.reset
        _    <- state.action(testUser, tap("Drop"), r)
        done <- r.sentScreens
      } yield assertTrue(done.size == 1 && done.head.text.contains("Живая вода выброшен.")) &&
              assertTrue(done.head.text.contains("В сумке таких: 2 шт.")) &&
              assertTrue(button(done.head, "DropAll").exists(_.label == "🗑 Выбросить все 2"))
    }
  )
}
