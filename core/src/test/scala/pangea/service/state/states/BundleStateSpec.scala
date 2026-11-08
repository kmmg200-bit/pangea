package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.engine.{Choice, SceneContent, Screen}
import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.hero.{AzatState, CubeStatus}
import pangea.model.item.{Bundling, GemKind, Item, ItemDetails, ItemType, MaterialKind, Rarity, TrophyKind}
import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
import pangea.model.skill.Skill
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.states.temple.CubeState
import pangea.test._
import zio.ZIO
import zio.test._

/** Стопка в сумке: «📦 В стопку» у пяти одинаковых, карточка стопки и
  * «Распаковать»; куб и Ришелье стопку не берут. */
object BundleStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def open(id: Long): UserAction = tap(s"${InventoryState.ItemActionPrefix}$id")

  private def button(screen: Screen, id: String): Option[Choice] = screen.choices.find(_.id == id)

  private def mithril(id: Long): Item = MaterialGenerator.item(MaterialKind.Mithril).copy(id = id)
  private def dust(id: Long): Item    = MaterialGenerator.item(MaterialKind.RubyDust).copy(id = id)
  private def ruby(id: Long): Item    = GemGenerator.item(GemKind.Ruby, 1).copy(id = id)

  private def trophy(id: Long): Item =
    Item(id, "Голова (Человек)", 5L, Rarity.Gray, ItemType.Trophy, attack = 0, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0, details = ItemDetails.Trophy("Human", TrophyKind.Head))

  private def smallRune(id: Long): Item =
    RuneStone.item(Rune.Active(Skill.CunningStrike), RuneStoneSize.Small).copy(id = id)

  /** Сумка на двадцать мест: `n` мечей, чтобы занять места. */
  private def swords(n: Int): List[Item] =
    (1 to n).toList.map(i => Item(200L + i, s"Меч $i", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0,
      energy = 0, armor = 0, defence = 0, evasion = 0))

  private def range(from: Long, to: Long)(make: Long => Item): List[Item] = (from to to).toList.map(make)

  private def content = ZIO.attempt(SceneContent.load())

  /** Сумка с вещами, экран сумки и рендерер. Номера вещам тестовая база даёт с 1 —
    * у вещей в тестах номера от сотни, чтобы не путаться. */
  private def inventory(items: List[Item]) =
    for {
      dao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
      inv  = TestInventoryRepository.withItems(items)
      r   <- TestRenderer.make
      c   <- content
    } yield (InventoryState(dao, inv, TestItemRepository.make, c), inv, r)

  override def spec = suite("Стопка в сумке")(

    test("пять одинаковых — на карточке «📦 В стопку»; четыре — кнопки нет") {
      for {
        f <- inventory(range(101L, 105L)(mithril) ++ range(111L, 114L)(ruby))
        (state, _, r) = f
        _    <- state.action(testUser, open(101L), r)
        five <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, open(111L), r)
        four <- r.sentScreens.map(_.last)
      } yield assertTrue(button(five, "Pack").exists(c => c.label == "📦 В стопку" && c.row.contains(1))) &&
              assertTrue(button(four, "Pack").isEmpty)
    },

    test("трофеи и малые руны в стопку не собираются, даже если их пять") {
      for {
        f <- inventory(range(101L, 105L)(trophy) ++ range(111L, 115L)(smallRune))
        (state, _, r) = f
        _      <- state.action(testUser, open(101L), r)
        heads  <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, open(111L), r)
        runes  <- r.sentScreens.map(_.last)
      } yield assertTrue(button(heads, "Pack").isEmpty && button(runes, "Pack").isEmpty)
    },

    test("«В стопку»: пять последних — в одну вещь, показанная остаётся, и карточка та же") {
      for {
        f <- inventory(range(101L, 107L)(mithril))
        (state, inv, r) = f
        _    <- state.action(testUser, open(101L), r)
        _    <- r.reset
        _    <- state.action(testUser, tap("Pack"), r)
        done <- r.sentScreens
        bag   = inv.snapshot
        made  = bag.find(_.isBundle)
      } yield assertTrue(bag.size == 3 && bag.filterNot(_.isBundle).map(_.id) == List(101L, 102L)) &&
              assertTrue(made.exists(b => Bundling.contents(b) == range(103L, 107L)(mithril) && b.id > 0L)) &&
              assertTrue(done.size == 1 && done.head.text.contains("📦 Собрано в стопку: Мифрил ×5.")) &&
              // на карточке мифрила — двое оставшихся, на стопку их уже не хватает
              assertTrue(done.head.text.contains("В сумке таких: 2 шт.") && button(done.head, "Pack").isEmpty)
    },

    test("собрали последние пять — карточки нет, список со стопкой одной строкой") {
      for {
        f <- inventory(range(101L, 105L)(mithril))
        (state, inv, r) = f
        _    <- state.action(testUser, open(101L), r)
        _    <- state.action(testUser, tap("Pack"), r)
        list <- r.sentScreens.map(_.last)
      } yield assertTrue(inv.snapshot.size == 1 && inv.snapshot.forall(_.isBundle)) &&
              assertTrue(list.choices.exists(_.label == "📦 Мифрил"))
    },

    test("карточка стопки: что внутри и «Распаковать»; распаковка возвращает те же вещи") {
      val bundle = Bundling.pack(range(101L, 105L)(ruby)).get.copy(id = 50L)
      for {
        f <- inventory(List(bundle))
        (state, inv, r) = f
        _    <- state.action(testUser, open(50L), r)
        card <- r.sentScreens.map(_.last)
        _    <- r.reset
        _    <- state.action(testUser, tap("Unpack"), r)
        done <- r.sentScreens
      } yield assertTrue(card.text.contains(bundle.displayTitle) && card.text.contains("Стопка: 5 шт. на одном месте.")) &&
              assertTrue(card.text.contains(ruby(1L).statsLines.head)) &&
              assertTrue(button(card, "Unpack").exists(_.label == "📦 Распаковать") && button(card, "Pack").isEmpty) &&
              assertTrue(button(card, "Equip").isEmpty) &&
              assertTrue(inv.snapshot == range(101L, 105L)(ruby)) &&
              assertTrue(done.size == 1 && done.head.text.contains(s"📦 Стопка распакована: ${ruby(1L).displayTitle} ×5 — в сумке."))
    },

    test("распаковать некуда — стопка на месте, сказано, сколько мест освободить") {
      val bundle = Bundling.pack(range(101L, 105L)(mithril)).get.copy(id = 50L)
      for {
        f <- inventory(bundle :: swords(17))
        (state, inv, r) = f
        _    <- state.action(testUser, open(50L), r)
        _    <- state.action(testUser, tap("Unpack"), r)
        said <- r.sentScreens.map(_.last.text)
      } yield assertTrue(said.contains("📦 Чтобы распаковать стопку, освободите в сумке мест: 2.")) &&
              assertTrue(inv.snapshot.size == 18 && inv.snapshot.exists(_.id == 50L))
    },

    test("пыль: в полной сумке стопку не собрать — ей нужно место") {
      for {
        f <- inventory(range(101L, 105L)(dust) ++ swords(20))
        (state, inv, r) = f
        _    <- state.action(testUser, open(101L), r)
        card <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("Pack"), r)
        said <- r.sentScreens.map(_.last.text)
      } yield assertTrue(button(card, "Pack").isDefined) &&
              assertTrue(said.contains("📦 Стопке нужно свободное место в сумке")) &&
              assertTrue(inv.snapshot.size == 25 && !inv.snapshot.exists(_.isBundle))
    },

    test("пыль: распаковка упирается в предел горстей — стопка лежит как есть") {
      val bundle = Bundling.pack(range(101L, 105L)(dust)).get.copy(id = 50L)
      for {
        f <- inventory(bundle :: range(301L, 396L)(dust))
        (state, inv, r) = f
        _    <- state.action(testUser, open(50L), r)
        _    <- state.action(testUser, tap("Unpack"), r)
        said <- r.sentScreens.map(_.last.text)
      } yield assertTrue(said.contains(s"больше ${Item.HoardLimit} горстей в сумке не держат")) &&
              assertTrue(inv.snapshot.count(_.isBundle) == 1 && inv.snapshot.size == 97)
    },

    test("куб стопку не предлагает, а нажатую по старой кнопке — не берёт") {
      val bundle = Bundling.pack(range(101L, 105L)(ruby)).get.copy(id = 50L)
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        _    <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = 5).asJson)
        inv   = TestInventoryRepository.withItems(List(bundle, mithril(7L)))
        r    <- TestRenderer.make
        c    <- content
        state = CubeState(dao, inv, TestItemRepository.make, c)
        _    <- state.action(testUser, tap("CubeDeposit"), r)
        list <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap(CubeState.DepositPrefix + "50"), r)
        said <- r.sentScreens.map(_.last.text)
        azat <- dao.readAzatData(userId).map(_.flatMap(_.as[AzatState].toOption).get)
      } yield assertTrue(list.choices.exists(_.id == CubeState.DepositPrefix + "7")) &&
              assertTrue(!list.choices.exists(_.id == CubeState.DepositPrefix + "50")) &&
              assertTrue(said.contains("📦 Стопку в куб не положить — сначала распакуйте её.")) &&
              assertTrue(azat.cubeItems.isEmpty && inv.snapshot.exists(_.id == 50L))
    },

    test("Ришелье стопку не покупает: её нет ни в списке, ни в «хламе»") {
      val bundle = Bundling.pack(range(101L, 105L)(mithril)).get.copy(id = 50L)
      for {
        dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(lvl = 10L, silver = 0L))
        inv   = TestInventoryRepository.withItems(List(bundle, trophy(7L)))
        r    <- TestRenderer.make
        c    <- content
        state = MerchantState(dao, inv, TestItemRepository.make, c)
        _    <- state.action(testUser, tap("Sell"), r)
        list <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("SellJunk"), r)
      } yield assertTrue(list.choices.exists(_.id == s"${MerchantState.SellItemPrefix}7")) &&
              assertTrue(!list.choices.exists(_.id == s"${MerchantState.SellItemPrefix}50")) &&
              assertTrue(inv.snapshot.exists(_.id == 50L))
    }
  )
}
