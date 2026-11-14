package pangea.service.miniapp

import pangea.generator.item.GemGenerator
import pangea.model.item.GemKind
import pangea.model.state.StateType
import pangea.service.trade.Trades
import pangea.test.TestTrades._
import zio.test._

/** Обмен в Mini App — данными: предложения глазами каждого, что можно
  * положить (стопками, с номерами вещей) и почему вне города можно только
  * отозвать. Правила — у Trades (см. TradesSpec). */
object MiniAppTradeSpec extends ZIOSpecDefault {

  private def skull(id: Long) = GemGenerator.item(GemKind.Skull, 1).copy(id = id)

  override def spec = suite("Обмен в Mini App")(

    test("предложение глазами обоих: что отдаю, что получаю, чей ход, моё или мне, сколько ждать") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", Trades.Draft(List(sword.id), 100L, 0L, Some("Хочу щит")), 1000L)
                    .map(_.toOption.get)
        b      <- f.hero(bob)
        _      <- f.trades.answer(bob, b, offer.id, Trades.Draft(List(shield.id), 0L, 0L, None), 2000L)
        open   <- f.trades.open(alice.userId)
        names   = open.map(_.id -> "Борис").toMap
        mine    = MiniAppView.trade(a, open, names, Nil, f.content, _.name, f.trades.describe, 5000L)
        theirs  = MiniAppView.trade(b, open, open.map(_.id -> "Алиса").toMap, Nil, f.content, _.name, f.trades.describe, 5000L)
      } yield assertTrue(
        mine.offers.map(o => (o.partner, o.yourTurn, o.proposed)) == List(("Борис", true, true)),
        mine.offers.headOption.exists(o => o.mine.items.map(_.ids) == List(List(sword.id)) && o.mine.silver == 100L &&
          o.mine.note.contains("Хочу щит") && o.theirs.summary == shield.displayTitle &&
          o.leftMs == 2000L + Trades.LifetimeMs - 5000L),
        theirs.offers.map(o => (o.partner, o.yourTurn, o.proposed)) == List(("Алиса", false, false)),
        mine.feePct == 10L && mine.minMoney == 50L && mine.maxNote == Trades.MaxNote
      )
    },

    test("что можно положить — стопками, у стопки номера всех её вещей; вне города — почему только отозвать") {
      for {
        f     <- fx()
        a     <- f.hero(alice)
        stock  = List(axe, skull(31L), skull(32L))
        here   = MiniAppView.trade(a, Nil, Map.empty, stock, f.content, _.name, f.trades.describe, 0L)
        far    = MiniAppView.trade(a.copy(state = StateType.Dungeon), Nil, Map.empty, stock, f.content, _.name, f.trades.describe, 0L)
      } yield assertTrue(
        here.stock.map(s => (s.item.title, s.ids)) == List((axe.displayTitle, List(axe.id)), (skull(31L).displayTitle, List(31L, 32L))),
        here.stock.lastOption.exists(_.item.count == 2),
        here.away.isEmpty, far.away.contains(f.content.text("trade.notInCity"))
      )
    },

    test("подсказки имён: все герои с именем по алфавиту, без самого игрока и без безымянных") {
      for {
        f     <- fx()
        _     <- f.heroes.insertHero(pangea.test.TestFixtures.hero(pangea.model.user.UserId(3L))
                   .copy(id = pangea.model.hero.HeroId(3L), name = None))
        _     <- f.heroes.insertHero(pangea.test.TestFixtures.hero(pangea.model.user.UserId(4L))
                   .copy(id = pangea.model.hero.HeroId(4L), name = Some("Ардаэль"), lvl = 12L))
        all   <- f.heroes.namedHeroes
        view   = MiniAppView.heroNames(alice.userId, all)
      } yield assertTrue(view.heroes.map(_.name) == List("Ардаэль", "Борис"), view.heroes.headOption.exists(_.lvl == 12L))
    },

    test("номера вещей из запроса: через запятую, мусор отбрасывается") {
      assertTrue(
        MiniApp.TradeAct.items(Some("11, 12,x,,15")) == List(11L, 12L, 15L),
        MiniApp.TradeAct.items(None).isEmpty
      )
    }
  )
}
