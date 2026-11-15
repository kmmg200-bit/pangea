package pangea.service.state.states.barti

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.admin.AdminStats
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.model.GameEvent
import pangea.model.hero.{Hero, Knowledge, LoreData}
import pangea.model.item.{Item, ItemType, MaterialKind, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.state.BartiLottery.Prize
import pangea.service.state.{BartiLottery, NpcQuestLog, UserAction}
import pangea.test._
import zio.ZIO
import zio.test._

/** Серебряная лотерея Барти: мешок конфиската наудачу за 5000 серебра — десять
  * находок, больше всего хлама, цветов и ингредиентов, изредка — кошелёк с
  * дублонами; до десяти мешков в сутки, дальше — «конфиската на сегодня нет». */
object BartiLotterySpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(Map("action" -> key).asJson.noSpaces))

  /** «Ходовой товар» и «Большое дело» закрыты: Барти дома. */
  private val settled: NpcQuests =
    NpcQuests.empty.take(NpcQuest.HotGoods, 0L).finish(NpcQuest.HotGoods).take(NpcQuest.BigDeal, 0L).finish(NpcQuest.BigDeal)

  private def gear(id: Long): Item =
    Item(id, s"Меч $id", lvl = 1L, Rarity.Gray, ItemType.Weapon,
      attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private case class Fix(state: BartiHouseState, dao: TestHeroDao, inv: TestInventoryRepository, journal: TestJournal,
                         bank: TestBankRepository, renderer: TestRenderer)

  private def house(silver: Long = 6000L, bag: List[Item] = Nil, vault: Long = 0L, quests: NpcQuests = settled,
                    lvl: Long = 20L, items: Boolean = true) =
    for {
      dao      <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = StateType.BartiHouse).copy(lvl = lvl, silver = silver))
      _        <- NpcQuestLog.save(dao, userId, quests)
      inv       = TestInventoryRepository.withItems(bag)
      journal  <- TestJournal.make
      bank      = TestBankRepository.of(cells = 1, silver = vault)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield Fix(BartiHouseState(dao, inv, content, Option.when(items)(TestItemRepository.make), None, Some(bank), None,
                  Some(journal), Some(TestArtifactRepository.empty)), dao, inv, journal, bank, renderer)

  private def heroOf(dao: TestHeroDao): ZIO[Any, Throwable, Hero] =
    dao.getHeroByUserId(userId).someOrFail(new Throwable("no hero"))

  private def sacks(n: Int, lvl: Long, lore: LoreData): List[BartiLottery.Find] =
    (1 to n).foldLeft((List.empty[BartiLottery.Find], Rng(7L))) { case ((acc, r), _) =>
      val (sack, next) = BartiLottery.sack(lvl, lore, r)
      (acc ++ sack, next)
    }._1

  /** Первое зерно, мешок с которого — у героя 20-го уровня без знаний о травах,
    * как в `house`, — подходит под `p`. Зерно отдаётся мешку через TestRandom. */
  private def seedWhere(p: List[BartiLottery.Find] => Boolean): Long =
    Iterator.from(1).map(_.toLong).find(seed => p(BartiLottery.sack(20L, LoreData.empty, Rng(seed))._1)).getOrElse(0L)

  private def purses(finds: List[BartiLottery.Find]): Int = finds.count(_.isInstanceOf[BartiLottery.Find.Purse])

  private def weight(p: Prize => Boolean): Int = BartiLottery.Table.collect { case (prize, w) if p(prize) => w }.sum

  override def spec = suite("Серебряная лотерея Барти")(

    test("таблица: в сумме тысяча; хлам, цветы и ингредиенты — 75%; легендарная вещь — 0,4%; кошельки 5, 25, 50 и 75 дублонов — 2%") {
      val junkGear    = Set[Rarity](Rarity.Gray, Rarity.White)
      val ingredients = (MaterialKind.dusts.toList ++ BartiLottery.BossIngredients ++
        List(MaterialKind.RatPelt, MaterialKind.RatTail, MaterialKind.PlagueWorms, MaterialKind.SpiderSilk,
          MaterialKind.VenomGland, MaterialKind.RedSpiderGland)).toSet
      val cheap = weight {
        case Prize.Gear(r)         => junkGear.contains(r)
        case Prize.Herb(_)         => true
        case Prize.Material(kinds) => kinds.forall(ingredients.contains)
        case _                     => false
      }
      val purseWeights = BartiLottery.Table.collect { case (Prize.Doubloons(d), w) => d -> w }
      assertTrue(BartiLottery.TotalWeight == 1000L && cheap == 751) &&
        assertTrue(purseWeights == List(5L -> 12, 25L -> 5, 50L -> 2, 75L -> 1)) &&
        assertTrue(weight(_ == Prize.Gear(Rarity.Orange)) == 4 && BartiLottery.Price == 5000L &&
          BartiLottery.Size == 10 && BartiLottery.DailyLimit == 10L) &&
        assertTrue(BartiLottery.BossIngredients.distinct.size == 8)
    },

    test("мешок — десять находок; вещи уровня героя (−5…+1); бросков — как в таблице") {
      val finds = sacks(2000, 30L, LoreData.empty)
      val all   = BartiLottery.items(finds)
      val worn  = all.filter(i => ItemType.settable.contains(i.itemType) && i.itemType != ItemType.Flask)
      val herbs = all.count(_.material.exists(_.isHerb))
      val gray  = worn.count(_.rarity == Rarity.Gray)
      val share = (n: Int) => n.toDouble / finds.size
      assertTrue(finds.size == 20000 && BartiLottery.sack(30L, LoreData.empty, Rng(1L))._1.size == BartiLottery.Size) &&
        assertTrue(worn.forall(i => i.lvl >= 25L && i.lvl <= 31L)) &&
        assertTrue(share(herbs) > 0.265 && share(herbs) < 0.305 && share(gray) > 0.095 && share(gray) < 0.125) &&
        assertTrue(share(purses(finds)) > 0.015 && share(purses(finds)) < 0.025) &&
        assertTrue(worn.exists(_.rarity == Rarity.Orange) && share(worn.count(_.rarity == Rarity.Orange)) < 0.007)
    },

    test("цветы — какими их видит герой: без знаний — странный и необычный цветок, со знанием — сама трава") {
      val blind = BartiLottery.items(sacks(300, 30L, LoreData.empty)).flatMap(_.material).filter(_.isHerb)
      val wise  = BartiLottery.items(sacks(300, 30L,
        LoreData(knowledge = List(Knowledge.FlowersRank1.entryName, Knowledge.FlowersRank2.entryName))))
        .flatMap(_.material).filter(_.isHerb)
      assertTrue(blind.nonEmpty && blind.forall(MaterialKind.isUnknownFlower) &&
        blind.contains(MaterialKind.StrangeFlower) && blind.contains(MaterialKind.UnusualFlower)) &&
        assertTrue(wise.nonEmpty && !wise.exists(MaterialKind.isUnknownFlower) && wise.exists(_.herbRank == 2))
    },

    test("экран дома: первым рядом — «🎲 Мешок наудачу», задания — ниже; без хранилища вещей лотереи нет") {
      for {
        f     <- house(quests = settled.take(NpcQuest.BartiRoof, 0L))
        _     <- f.state.enter(testUser, f.renderer)
        home  <- f.renderer.sentScreens.map(_.last)
        g     <- house(items = false)
        _     <- g.state.enter(testUser, g.renderer)
        bare  <- g.renderer.sentScreens.map(_.last)
      } yield assertTrue(home.choices.map(c => c.id -> c.row) ==
                List("BartiLottery" -> Some(0), "BartiShop" -> Some(1), "BartiLeave" -> Some(2))) &&
              assertTrue(home.choices.headOption.exists(_.label == "🎲 Мешок наудачу")) &&
              assertTrue(!bare.choices.exists(_.id == "BartiLottery"))
    },

    test("рассказ Барти: своя лотерея, что внутри — не знает и не подсматривает; мешок — 5000, десять вещей; на сегодня 10 из 10") {
      for {
        f    <- house()
        _    <- f.state.action(testUser, tap("BartiLottery"), f.renderer)
        last <- f.renderer.sentScreens.map(_.last)
      } yield assertTrue(last.text.contains("Моя собственная лотерея!") && last.text.contains("даже не подсматриваю") &&
                last.text.contains("Там может быть что угодно") && last.text.contains("Мешок — 5000 🪙, в нём 10 вещей.") &&
                last.text.contains("Мешков у Барти на сегодня: 10 из 10.")) &&
              assertTrue(last.choices.map(_.id) == List("BartiSackBuy", "BartiBack") &&
                last.choices.headOption.exists(_.label == "🎲 Купить мешок — 5000 🪙"))
    },

    test("купить мешок: −5000 серебра, десять вещей в сумке, мешок — в журнал; на сегодня 9 из 10; «Назад» — к экрану дома") {
      for {
        f      <- house(silver = 6000L)
        _      <- TestRandom.feedLongs(seedWhere(purses(_) == 0))
        _      <- f.state.action(testUser, tap("BartiSackBuy"), f.renderer)
        last   <- f.renderer.sentScreens.map(_.last)
        hero   <- heroOf(f.dao)
        logged <- f.journal.logged
        back   <- f.state.action(testUser, tap("BartiBack"), f.renderer)
        home   <- f.renderer.sentScreens.map(_.last)
      } yield assertTrue(hero.silver == 1000L && f.inv.snapshot.size == BartiLottery.Size &&
                f.inv.snapshot.forall(_.id > 0L)) &&
              assertTrue(logged.count(_.eventType == GameEvent.BartiLottery) == 1 &&
                logged.headOption.exists(_.payload.hcursor.get[Long]("price").toOption.contains(5000L))) &&
              assertTrue(last.text.contains("Барти развязывает бечёвку и вытряхивает мешок на стол:") &&
                last.text.split("\n").count(_.startsWith("• ")) == BartiLottery.Size &&
                last.text.contains("Мешков у Барти на сегодня: 9 из 10.") &&
                last.choices.map(_.id) == List("BartiSackBuy", "BartiBack")) &&
              assertTrue(back == StateType.BartiHouse && home.text.contains("Дом Барти за синей дверью"))
    },

    test("серебра с собой мало — Барти добирает из ячейки Торгового дома") {
      for {
        f    <- house(silver = 2000L, vault = 3000L)
        _    <- TestRandom.feedLongs(seedWhere(purses(_) == 0))
        _    <- f.state.action(testUser, tap("BartiSackBuy"), f.renderer)
        hero <- heroOf(f.dao)
      } yield assertTrue(hero.silver == 0L && f.bank.silverSnapshot == 0L && f.inv.snapshot.size == BartiLottery.Size)
    },

    test("кошелёк с дублонами: дублоны — сразу в кошель героя, места в сумке он не занимает; в списке — строкой, в журнале — суммой") {
      val seed  = seedWhere(purses(_) > 0)
      val finds = BartiLottery.sack(20L, LoreData.empty, Rng(seed))._1
      val coins = BartiLottery.doubloons(finds)
      for {
        f      <- house(silver = 6000L)
        before <- heroOf(f.dao)
        _      <- TestRandom.feedLongs(seed)
        _      <- f.state.action(testUser, tap("BartiSackBuy"), f.renderer)
        last   <- f.renderer.sentScreens.map(_.last)
        hero   <- heroOf(f.dao)
        logged <- f.journal.logged
      } yield assertTrue(coins > 0L && hero.doubloons == before.doubloons + coins && hero.silver == 1000L) &&
              assertTrue(f.inv.snapshot.size == BartiLottery.Size - purses(finds)) &&
              assertTrue(finds.collect { case BartiLottery.Find.Purse(d) => d }.forall(d =>
                last.text.contains(s"• 👛 Кошелёк с дублонами: 🟡 $d"))) &&
              assertTrue(last.text.split("\n").count(_.startsWith("• ")) == BartiLottery.Size) &&
              assertTrue(logged.headOption.exists(_.payload.hcursor.get[Long]("doubloons").toOption.contains(coins)))
    },

    test("десять мешков за сутки: одиннадцатый — «на сегодня мешков конфиската у меня нет», серебро цело") {
      for {
        f    <- house(silver = 60000L)
        _    <- ZIO.foreachDiscard(1 to 10)(_ => f.journal.append(GameEvent(userId, GameEvent.BartiLottery, Json.obj())))
        _    <- f.state.action(testUser, tap("BartiSackBuy"), f.renderer)
        last <- f.renderer.sentScreens.map(_.last)
        hero <- heroOf(f.dao)
      } yield assertTrue(last.text.contains("На сегодня мешков конфиската у меня нет") && hero.silver == 60000L &&
                f.inv.snapshot.isEmpty && last.choices.map(_.id) == List("BartiBack"))
    },

    test("в сумке меньше десяти мест или серебра мало даже с ячейкой — отказ, ничего не тратим") {
      for {
        f     <- house(bag = (1L to 11L).map(gear).toList)
        _     <- f.state.action(testUser, tap("BartiSackBuy"), f.renderer)
        room  <- f.renderer.sentScreens.map(_.last)
        roomH <- heroOf(f.dao)
        g     <- house(silver = 4999L)
        _     <- g.state.action(testUser, tap("BartiSackBuy"), g.renderer)
        poor  <- g.renderer.sentScreens.map(_.last)
        poorH <- heroOf(g.dao)
        logF  <- f.journal.logged
        logG  <- g.journal.logged
      } yield assertTrue(room.text.contains("В мешке 10 вещей, а у тебя свободно мест: 9") && roomH.silver == 6000L &&
                f.inv.snapshot.size == 11) &&
              assertTrue(poor.text.contains("5000 серебром, делец") && poorH.silver == 4999L && g.inv.snapshot.isEmpty) &&
              assertTrue(logF.isEmpty && logG.isEmpty)
    },

    test("пока Барти в «Долговой яме», лотереи нет: дом опечатан, старая кнопка мешок не продаёт") {
      val pit = settled.take(NpcQuest.BartiRoof, 0L).finish(NpcQuest.BartiRoof).take(NpcQuest.BartiDelivery, 0L)
        .finish(NpcQuest.BartiDelivery).take(NpcQuest.BartiPit, 0L)
      for {
        f    <- house(quests = pit, lvl = 40L)
        _    <- f.state.action(testUser, tap("BartiSackBuy"), f.renderer)
        last <- f.renderer.sentScreens.map(_.last)
        hero <- heroOf(f.dao)
      } yield assertTrue(!last.choices.exists(_.id == "BartiLottery") && !last.text.contains("Дом Барти за синей дверью") &&
                hero.silver == 6000L && f.inv.snapshot.isEmpty)
    },

    test("админ-панель: сколько мешков купили у Барти — всего и за неделю") {
      val text = AdminPanel.statsText(AdminStats.empty.copy(bartiLottery = 14L, bartiLottery7d = 5L)).replace("\r", "")
      assertTrue(text.contains("🎲 Мешки наудачу у Барти\n  куплено всего: 14\n  за неделю: 5"))
    }
  )
}
