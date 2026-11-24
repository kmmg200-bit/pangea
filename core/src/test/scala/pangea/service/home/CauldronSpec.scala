package pangea.service.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.LoreData
import pangea.model.home.{HomeData, HomeRates}
import pangea.model.item.{BrewKind, Item, MaterialKind}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.HerbLore
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Котелок без экрана — общий у бота и Mini App: то, чего бот не делает сам
  * (перенос по номеру с любой полки и одной вещью обратно), и что огромное
  * число отваров не варится в долг. Остальное — варку, «Вынуть всё»,
  * изученные — держит [[pangea.service.state.states.home.HerbWorkbenchSpec]]. */
object CauldronSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("1"), TelegramId("-1"))
  private val hero   = TestFixtures.hero(userId, state = StateType.MarketSquare)

  private def herb(id: Long, kind: MaterialKind): Item = MaterialGenerator.item(kind).copy(id = id)
  private def bottle(id: Long, kind: BrewKind): Item  = BrewKind.item(kind).copy(id = id)

  private val townhouse = HomeData(tier = 2, metJunona = true, charges = 100)

  private def make(home: HomeData, bag: List[Item] = Nil) =
    for {
      dao <- TestHeroDao.withHero(userId, hero)
      _   <- dao.writeHomeData(userId, home.asJson)
      inv  = TestInventoryRepository.withItems(bag)
      c   <- ZIO.attempt(SceneContent.load())
    } yield (new Cauldron(dao, inv, TestItemRepository.make, c), dao, inv, c)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

  def spec = suite("Котелок: общий у бота и приложения")(
    test("по номеру — с той полки, где лежит: трава с полок трав, склянка с полки отваров; полный котелок не берёт") {
      val home = townhouse.copy(herbs = List(herb(1L, MaterialKind.Chamomile)), brews = List(bottle(2L, BrewKind.BoneSetter)))
      val full = townhouse.copy(herbs = List(herb(1L, MaterialKind.Chamomile)),
                   cauldron = List.tabulate(HomeRates.CauldronSlots)(i => herb(10L + i, MaterialKind.Nettle)))
      for {
        t1            <- make(home)
        (pot, dao, _, c) = t1
        a             <- pot.putById(userId, 1L)
        b             <- pot.putById(userId, 2L)
        gone          <- pot.putById(userId, 99L)
        after         <- homeOf(dao)
        t2            <- make(full)
        (pot2, dao2, _, _) = t2
        refused       <- pot2.putById(userId, 1L)
        kept          <- homeOf(dao2)
      } yield assertTrue(
        a.isEmpty && b.isEmpty,
        after.cauldron.map(_.id) == List(1L, 2L),
        after.herbs.isEmpty && after.brews.isEmpty,
        gone.contains(c.text("stack.gone")),
        refused.contains(c.format("home.workbench.potFull", "max" -> HomeRates.CauldronSlots.toString)),
        kept.herbs.map(_.id) == List(1L) && kept.cauldron.size == HomeRates.CauldronSlots
      )
    },
    test("обратно одной вещью — на её полку; полка полна — вещь остаётся в котелке") {
      val home = townhouse.copy(cauldron = List(herb(1L, MaterialKind.Chamomile), bottle(2L, BrewKind.BoneSetter)))
      for {
        t1              <- make(home)
        (pot, dao, inv, _) = t1
        a               <- pot.back(hero, 1L)
        b               <- pot.back(hero, 2L)
        after           <- homeOf(dao)
        crowded          = townhouse.copy(cauldron = List(herb(1L, MaterialKind.Chamomile)),
                             herbs = List.tabulate(townhouse.herbShelves)(i => herb(100L + i, MaterialKind.Nettle)))
        t2              <- make(crowded)
        (pot2, dao2, _, c) = t2
        refused         <- pot2.back(hero, 1L)
        kept            <- homeOf(dao2)
      } yield assertTrue(
        a.isEmpty && b.isEmpty,
        after.cauldron.isEmpty,
        after.herbs.map(_.id) == List(1L),
        after.brews.map(_.id) == List(2L),
        inv.snapshot.isEmpty,
        refused.contains(c.text("miniapp.storages.herbs.full")),
        kept.cauldron.map(_.id) == List(1L)
      )
    },
    test("огромное число отваров: варок не меньше нуля — не хватает энергии, дом как был") {
      // Раньше (asked + per - 1) / per на Int.MaxValue переполнялось: варок
      // выходило меньше нуля, ничего не варилось, а энергия дома прибывала.
      val herbs = List(herb(1L, MaterialKind.Chamomile), herb(2L, MaterialKind.Calendula), herb(3L, MaterialKind.Nettle))
      val home  = townhouse.copy(herbs = herbs)
      for {
        t             <- make(home)
        (pot, dao, _, c) = t
        _             <- HerbLore.writeLore(dao, userId, LoreData(brewed = List(BrewKind.BoneSetter.entryName)))
        r             <- TestRenderer.make
        made          <- pot.brew(user, hero, BrewKind.BoneSetter, Int.MaxValue, r)
        after         <- homeOf(dao)
        said          <- r.sentScreens.map(_.map(_.text))
      } yield assertTrue(
        made.isEmpty,
        after.charges == 100,
        after.herbs.map(_.id) == List(1L, 2L, 3L),
        after.brews.isEmpty,
        said.exists(_.contains(c.text("home.workbench.lackEnergy")))
      )
    }
  )
}
