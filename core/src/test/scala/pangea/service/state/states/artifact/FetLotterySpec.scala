package pangea.service.state.states.artifact

import io.circe.syntax.EncoderOps
import pangea.dao.admin.AdminStats
import pangea.engine.SceneContent
import pangea.model.GameEvent
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity}
import pangea.model.rune.{RuneStone, RuneStoneSize}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.sender.vk.VkRenderer
import pangea.service.state.UserAction
import pangea.test._
import zio.ZIO
import zio.test._

/** Беспроигрышная лотерея Фета: три большие руны случайных узоров за 35
  * дублонов; места в сумке — на все три разом; каждая игра — в журнал, по нему
  * админ-панель считает лотереи. */
object FetLotterySpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(Map("action" -> key).asJson.noSpaces))

  private def gear(id: Long): Item =
    Item(id, s"Меч $id", lvl = 1L, Rarity.Gray, ItemType.Weapon,
      attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private case class Fix(state: FetShopState, dao: TestHeroDao, inv: TestInventoryRepository,
                         journal: TestJournal, renderer: TestRenderer)

  private def shop(doubloons: Long = 100L, bag: List[Item] = Nil) =
    for {
      dao      <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(doubloons = doubloons))
      inv       = TestInventoryRepository.withItems(bag)
      journal  <- TestJournal.make
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield Fix(FetShopState(dao, TestArtifactRepository.empty, inv, content, Some(TestItemRepository.make), Some(journal)),
                dao, inv, journal, renderer)

  private def heroOf(dao: TestHeroDao): ZIO[Any, Throwable, Hero] =
    dao.getHeroByUserId(userId).someOrFail(new Throwable("no hero"))

  private def bigRunes(items: List[Item]): List[Item] = items.filter(_.details match {
    case ItemDetails.RuneStone(_, RuneStoneSize.Big) => true
    case _                                           => false
  })

  override def spec = suite("Лотерея Фета")(

    test("на прилавке — своя кнопка, и рядов не больше, чем держит ВК") {
      for {
        f    <- shop()
        _    <- f.state.action(testUser, tap("FetShop"), f.renderer)
        last <- f.renderer.sentScreens.map(_.last)
        rows  = last.choices.flatMap(_.row).distinct
      } yield assertTrue(last.choices.exists(c => c.id == "FetRunes" && c.label == "🎲 Беспроигрышная лотерея")) &&
              assertTrue(rows.size <= VkRenderer.MaxRows)
    },

    test("карточка: беспроигрышная лотерея, три большие руны за 🟡 35, нужно три места") {
      for {
        f    <- shop()
        _    <- f.state.action(testUser, tap("FetRunes"), f.renderer)
        last <- f.renderer.sentScreens.map(_.last)
      } yield assertTrue(last.text.contains("Беспроигрышная лотерея")) &&
              assertTrue(last.text.contains("3 большие руны за 🟡 35")) &&
              assertTrue(last.text.contains("В сумке нужно 3 свободных места")) &&
              assertTrue(last.choices.map(_.id) == List("FetRunesBuy", "FetShop")) &&
              assertTrue(last.choices.headOption.exists(_.label == "Тянуть за 🟡 35"))
    },

    test("сыграл: −35 дублонов, три большие руны в сумке — каждая со своим номером, игра в журнале") {
      for {
        f      <- shop(doubloons = 100L, bag = List(gear(500L)))
        _      <- f.state.action(testUser, tap("FetRunesBuy"), f.renderer)
        ask    <- f.renderer.sentScreens.map(_.last)
        _      <- f.state.action(testUser, tap("FetRunesYes"), f.renderer)
        hero   <- heroOf(f.dao)
        runes   = bigRunes(f.inv.snapshot)
        events <- f.journal.logged
        texts  <- f.renderer.sentScreens.map(_.map(_.text))
      } yield assertTrue(ask.inline && ask.choices.map(_.id) == List("FetRunesYes", "FetShop")) &&
              assertTrue(hero.doubloons == 65L) &&
              assertTrue(runes.size == 3 && runes.map(_.id).distinct.size == 3 && runes.forall(_.id > 0L)) &&
              assertTrue(f.inv.snapshot.size == 4) &&
              assertTrue(events.size == 1 && events.forall(_.eventType == GameEvent.FetRuneLottery)) &&
              assertTrue(events.headOption.exists(e => e.payload.hcursor.get[Long]("price").contains(35L) &&
                e.payload.hcursor.get[List[String]]("runes").exists(_.size == 3))) &&
              assertTrue(texts.exists(t => t.contains("Я же говорил — беспроигрышная") &&
                runes.forall(r => t.contains(r.displayTitle))))
    },

    test("дублонов мало — отказ ещё на подтверждении и при старой кнопке, ничего не списано и не записано") {
      for {
        f      <- shop(doubloons = 34L)
        _      <- f.state.action(testUser, tap("FetRunesBuy"), f.renderer)
        ask    <- f.renderer.sentScreens.map(_.last)
        _      <- f.state.action(testUser, tap("FetRunesYes"), f.renderer)
        last   <- f.renderer.sentScreens.map(_.last)
        hero   <- heroOf(f.dao)
        events <- f.journal.logged
      } yield assertTrue(ask.text.contains("нужно 🟡 35") && last.text.contains("нужно 🟡 35")) &&
              assertTrue(hero.doubloons == 34L && f.inv.snapshot.isEmpty && events.isEmpty)
    },

    test("в сумке два места — руны не влезут: отказ, дублоны целы") {
      val bag = (1L to 18L).toList.map(gear)
      for {
        f      <- shop(doubloons = 100L, bag = bag)
        _      <- f.state.action(testUser, tap("FetRunesBuy"), f.renderer)
        ask    <- f.renderer.sentScreens.map(_.last)
        _      <- f.state.action(testUser, tap("FetRunesYes"), f.renderer)
        last   <- f.renderer.sentScreens.map(_.last)
        hero   <- heroOf(f.dao)
        events <- f.journal.logged
      } yield assertTrue(ask.text.contains("Нужно свободных мест: 3, сейчас: 2")) &&
              assertTrue(last.text.contains("Нужно свободных мест: 3, сейчас: 2")) &&
              assertTrue(hero.doubloons == 100L && f.inv.snapshot.size == 18 && events.isEmpty)
    },

    test("ровно три места — хватает") {
      val bag = (1L to 17L).toList.map(gear)
      for {
        f    <- shop(doubloons = 35L, bag = bag)
        _    <- f.state.action(testUser, tap("FetRunesYes"), f.renderer)
        hero <- heroOf(f.dao)
      } yield assertTrue(hero.doubloons == 0L && bigRunes(f.inv.snapshot).size == 3)
    },

    test("узор каждой руны — случайный из всех, повторы бывают") {
      for {
        _     <- TestRandom.feedInts(0, 0, 5)
        runes <- FetShopState.drawRunes
      } yield assertTrue(runes == List(RuneStone.all(0), RuneStone.all(0), RuneStone.all(5)))
    },

    test("в админке — сколько раз сыграли всего и за неделю") {
      // Переводы строк — как в исходнике: в рабочей копии Windows это «\r\n»,
      // поэтому сравниваем без «\r».
      val text = AdminPanel.statsText(AdminStats.empty.copy(fetLottery = 12L, fetLottery7d = 3L)).replace("\r", "")
      assertTrue(text.contains("🎲 Лотерея рун у Фета\n  сыграли всего: 12\n  за неделю: 3"))
    }
  )
}
