package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.{Equipment, Hero}
import pangea.model.home.{Furniture, HomeData}
import pangea.model.item.{Item, ItemDetails, ItemType, PassiveKind, QuestItemKind, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.InventoryState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.test._
import zio.{Task, ZIO}

/** Магическая стойка: целый комплект на стойке, обмен — кнопкой дома или ключом
  * из сумки; в доме стоек не больше 1 / 2 / 4, к каждой — свой ключ. */
object RackSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def gear(id: Long, t: ItemType, atk: Long = 0L, armor: Long = 0L, lvl: Long = 1L,
                   details: ItemDetails = ItemDetails.Plain): Item =
    Item(id, s"Вещь $id", lvl, Rarity.Blue, t, attack = atk, accuracy = 0, energy = 0, armor = armor, defence = 0,
      evasion = 0, details = details)

  private val sword  = gear(10L, ItemType.Weapon, atk = 7L)
  private val helmet = gear(11L, ItemType.Helmet, armor = 30L)

  /** Герой 10 уровня в мече; атака уже с мечом (50 своих + 7). */
  private def hero(eq: Equipment = TestFixtures.emptyEquipment.copy(weapon = sword)): Hero =
    TestFixtures.hero(userId, StateType.Home).copy(lvl = 10L, doubloons = 300L, equipment = eq,
      fightStats = FightStats(atk = 57L, hp = 100L, armor = 40L, defence = 0L, evasion = 0L, accuracy = 0L, energy = 10L))

  private def make(h: Hero, home: HomeData, items: List[Item] = Nil) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeHomeData(userId, home.asJson)
      inv  = TestInventoryRepository.withItems(items)
      sch <- TestScheduler.make
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (HomeState(dao, sch, c, inv, TestItemRepository.make), dao, inv, r, c)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def heroOf(dao: TestHeroDao) = dao.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)   = r.sentScreens.map(_.map(_.text).mkString("\n"))

  /** Дом с одной пустой стойкой. */
  private val withRack = HomeData(tier = 2, metJunona = true, furniture = List("rack"), racks = List(Equipment.empty))

  override def spec = suite("Магическая стойка")(

    test("обмен со стойкой туда-обратно не чинит броню: запас держит долю от максимума") {
      val heavy = gear(13L, ItemType.Helmet, armor = 1000L)
      val light = gear(14L, ItemType.Helmet, armor = 100L)
      val worn  = hero(TestFixtures.emptyEquipment.copy(weapon = sword, helmet = heavy))
      val beaten = worn.copy(fightStats = worn.fightStats.copy(armor = 100L))
      for {
        t  <- make(beaten, withRack.copy(racks = List(TestFixtures.emptyEquipment.copy(helmet = light))))
        (state, dao, _, r, _) = t
        _  <- state.action(testUser, tap("HomeRackSwap", "n" -> "1"), r)
        h1 <- heroOf(dao)
        _  <- state.action(testUser, tap("HomeRackSwap", "n" -> "1"), r)
        h2 <- heroOf(dao)
      } yield assertTrue(h1.equipment.helmet.id == light.id && h1.fightStats.armor == 10L && h1.fightStats.atk == 50L) &&
              // прежде: 100 − 900 обрезалось до нуля, и обратная смена давала 900 из 1000
              assertTrue(h2.equipment.helmet.id == heavy.id && h2.fightStats.armor == 100L && h2.fightStats.atk == 57L)
    },

    test("Юнона: стойка за 100 дублонов, с ключом №1; в домик — не больше одной") {
      for {
        t <- make(hero(), HomeData(tier = 1, metJunona = true, clutterCleared = true))
        (state, dao, inv, r, _) = t
        _     <- state.action(testUser, tap("HomeFurnItem", "f" -> "rack"), r)
        card  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("HomeFurnBuy", "f" -> "rack"), r)
        home  <- homeOf(dao)
        h     <- heroOf(dao)
        said  <- texts(r)
        _     <- state.action(testUser, tap("HomeFurnBuy", "f" -> "rack"), r)
        twice <- homeOf(dao)
        h2    <- heroOf(dao)
        more  <- texts(r)
      } yield assertTrue(card.text.contains("Магическая стойка держит целый комплект") && card.text.contains("не больше 1 (уже стоит 0)")) &&
              assertTrue(home.furniture == List("rack") && home.racks == List(Equipment.empty) && h.doubloons == 200L) &&
              assertTrue(inv.snapshot.flatMap(_.questItem) == List(QuestItemKind.RackKey1)) &&
              assertTrue(said.contains("магическая стойка №1") && said.contains("ключ стойки №1")) &&
              // вторая в домик не встанет, хоть место и есть
              assertTrue(twice.racks.size == 1 && h2.doubloons == 200L && more.contains("Здесь их помещается 1")) &&
              assertTrue(List(1, 2, 4) == List(1, 2, 3).flatMap(t => HomeData(tier = t).house.map(_.racks)))
    },

    test("дома: «Поменяться» — меч на стойку, герой пуст; ещё раз — меч обратно; атака пересчитана") {
      for {
        t <- make(hero(), withRack)
        (state, dao, _, r, _) = t
        _     <- state.action(testUser, tap("HomeSlots"), r)
        slots <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("HomeRack", "n" -> "1"), r)
        empty <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("HomeRackSwap", "n" -> "1"), r)
        h1    <- heroOf(dao)
        home1 <- homeOf(dao)
        shown <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("HomeRackSwap", "n" -> "1"), r)
        h2    <- heroOf(dao)
        home2 <- homeOf(dao)
      } yield assertTrue(slots.text.contains("1. 🧥 Магическая стойка №1") && slots.choices.exists(_.id == "HomeRack")) &&
              assertTrue(empty.text.contains("Стойка пуста.") &&
                empty.choices.map(_.id) == List("HomeRackSwap", "HomeRackPut", "HomeRackOff", "HomeRackTake", "HomeSlots")) &&
              assertTrue(h1.equipment.weapon.itemType == ItemType.NoItem && h1.fightStats.atk == 50L) &&
              assertTrue(home1.racks.head.weapon.id == sword.id && shown.text.contains("Правая рука: [Ур.1] Вещь 10")) &&
              assertTrue(h2.equipment.weapon.id == sword.id && h2.fightStats.atk == 57L && home2.racks == List(Equipment.empty))
    },

    test("ключ из сумки: «Повернуть ключ» — магия завихрилась и сменила комплект") {
      val key = QuestItemKind.item(QuestItemKind.RackKey1).copy(id = 77L)
      for {
        t <- make(hero(), withRack, items = List(key))
        (_, dao, inv, r, c) = t
        bag    = InventoryState(dao, inv, TestItemRepository.make, c)
        _     <- bag.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}77"}""")), r)
        card  <- r.sentScreens.map(_.last)
        _     <- bag.action(testUser, tap("RackKeyUse"), r)
        h     <- heroOf(dao)
        home  <- homeOf(dao)
        said  <- texts(r)
      } yield assertTrue(card.text.contains("Ключ стойки №1") && card.choices.exists(_.id == "RackKeyUse")) &&
              assertTrue(h.equipment.weapon.itemType == ItemType.NoItem && home.racks.head.weapon.id == sword.id) &&
              assertTrue(said.contains("Магия в ключе завихрилась и сменила весь надетый комплект"))
    },

    test("нельзя: не по уровню, нечем меняться, сумка не вместит своё без «Тайника»") {
      val heavy  = withRack.copy(racks = List(TestFixtures.emptyEquipment.copy(helmet = gear(20L, ItemType.Helmet, lvl = 15L))))
      val stash  = gear(21L, ItemType.Boots, details = ItemDetails.Passive(PassiveKind.Stash))
      val crowd  = (1L to 15L).map(i => gear(100L + i, ItemType.Gloves)).toList
      for {
        t1 <- make(hero(), heavy)
        (s1, dao1, _, r1, _) = t1
        _    <- s1.action(testUser, tap("HomeRackSwap", "n" -> "1"), r1)
        h1   <- heroOf(dao1)
        why1 <- texts(r1)
        t2 <- make(hero(TestFixtures.emptyEquipment), withRack)
        (s2, _, _, r2, _) = t2
        _    <- s2.action(testUser, tap("HomeRackSwap", "n" -> "1"), r2)
        why2 <- texts(r2)
        // в сапогах «Тайник» (+10 мест), в сумке 15 вещей при 20 местах: без сапог — только 10
        t3 <- make(hero(TestFixtures.emptyEquipment.copy(boots = stash)), withRack, items = crowd)
        (s3, dao3, _, r3, _) = t3
        _    <- s3.action(testUser, tap("HomeRackSwap", "n" -> "1"), r3)
        h3   <- heroOf(dao3)
        why3 <- texts(r3)
      } yield assertTrue(h1.equipment.weapon.id == sword.id && why1.contains("не по уровню: Вещь 20 (15)")) &&
              assertTrue(why2.contains("Меняться нечем")) &&
              assertTrue(h3.equipment.boots.id == stash.id && why3.contains("сумка не вместит"))
    },

    test("надеть на стойку из сумки: вещь — на стойку, прежняя — в сумку; не по уровню — нельзя; на герое ничего не меняется") {
      val helmet2 = gear(13L, ItemType.Helmet, armor = 5L)
      val high    = gear(14L, ItemType.Helmet, lvl = 15L)
      for {
        t <- make(hero(TestFixtures.emptyEquipment), withRack, items = List(helmet, helmet2, high))
        (state, dao, inv, r, _) = t
        _     <- state.action(testUser, tap("HomeRack", "n" -> "1"), r)
        card  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("HomeRackPut", "n" -> "1"), r)
        list  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("HomeRackPick", "n" -> "1", "id" -> "11", "p" -> "0"), r)
        first <- homeOf(dao)
        bag1   = inv.snapshot.map(_.id).sorted
        _     <- state.action(testUser, tap("HomeRackPick", "n" -> "1", "id" -> "13", "p" -> "0"), r)
        swap  <- homeOf(dao)
        bag2   = inv.snapshot.map(_.id).sorted
        _     <- state.action(testUser, tap("HomeRackPick", "n" -> "1", "id" -> "14", "p" -> "0"), r)
        stay  <- homeOf(dao)
        said  <- texts(r)
        h     <- heroOf(dao)
      } yield assertTrue(card.choices.map(_.id) == List("HomeRackSwap", "HomeRackPut", "HomeRackOff", "HomeRackTake", "HomeSlots")) &&
              assertTrue(list.text.contains("Что надеть на стойку №1?") && list.choices.count(_.id == "HomeRackPick") == 3) &&
              assertTrue(first.racks.head.helmet.id == 11L && bag1 == List(13L, 14L)) &&
              // второй шлем — вместо первого, первый вернулся в сумку
              assertTrue(swap.racks.head.helmet.id == 13L && bag2 == List(11L, 14L) &&
                said.contains("На стойку №1 надето: [Ур.1] Вещь 13. Снято в сумку: Вещь 11.")) &&
              assertTrue(stay.racks.head.helmet.id == 13L && said.contains("Недостаточный уровень! Требуется 15")) &&
              assertTrue(h.equipment == TestFixtures.emptyEquipment && h.fightStats.atk == 57L)
    },

    test("на стойке два кольца — спрашивает, какое снять; выбрал второе — оно в сумке") {
      val r1 = gear(31L, ItemType.Ring)
      val r2 = gear(32L, ItemType.Ring)
      val r3 = gear(33L, ItemType.Ring)
      val rings = withRack.copy(racks = List(TestFixtures.emptyEquipment.copy(firstRing = r1, secondRing = r2)))
      for {
        t <- make(hero(TestFixtures.emptyEquipment), rings, items = List(r3))
        (state, dao, inv, r, _) = t
        _     <- state.action(testUser, tap("HomeRackPick", "n" -> "1", "id" -> "33", "p" -> "0"), r)
        ask   <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("HomeRackPick", "n" -> "1", "id" -> "33", "p" -> "0", "slot" -> "2"), r)
        home  <- homeOf(dao)
      } yield assertTrue(ask.choices.filter(_.id == "HomeRackPick").flatMap(_.data.get("slot")) == List("1", "2")) &&
              assertTrue(home.racks.head.firstRing.id == 31L && home.racks.head.secondRing.id == 33L) &&
              assertTrue(inv.snapshot.map(_.id) == List(32L))
    },

    test("«Снять вещь»: по кнопке на занятое место — одна вещь в сумку, остальное висит") {
      val hung = withRack.copy(racks = List(TestFixtures.emptyEquipment.copy(helmet = helmet, weapon = sword)))
      for {
        t <- make(hero(TestFixtures.emptyEquipment), hung)
        (state, dao, inv, r, _) = t
        _    <- state.action(testUser, tap("HomeRackOff", "n" -> "1"), r)
        list <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("HomeRackDrop", "n" -> "1", "s" -> "0"), r)
        home <- homeOf(dao)
      } yield assertTrue(list.choices.filter(_.id == "HomeRackDrop").map(_.label) ==
                List("Шлем: [Ур.1] Вещь 11", "Правая рука: [Ур.1] Вещь 10")) &&
              assertTrue(home.racks.head.helmet.itemType == ItemType.NoItem && home.racks.head.weapon.id == sword.id) &&
              assertTrue(inv.snapshot.map(_.id) == List(11L))
    },

    test("«Снять всё в сумку»: со стойки — в сумку, стойка пуста") {
      val hung = withRack.copy(racks = List(TestFixtures.emptyEquipment.copy(helmet = helmet, weapon = sword)))
      for {
        t <- make(hero(TestFixtures.emptyEquipment), hung)
        (state, dao, inv, r, _) = t
        _    <- state.action(testUser, tap("HomeRackTake", "n" -> "1"), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield assertTrue(home.racks == List(Equipment.empty) && inv.snapshot.map(_.id).sorted == List(10L, 11L)) &&
              assertTrue(said.contains("Со стойки в сумку: 2 шт.") && Furniture.Rack.doubloons == 100L)
    }
  )
}
