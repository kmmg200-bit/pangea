package pangea.service.state.states.bank

import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import pangea.engine.Screen
import pangea.model.state.StateType
import pangea.model.trade.TradeStatus
import pangea.model.user.User
import pangea.service.state.UserAction
import pangea.service.trade.Trades
import pangea.test.TestTrades._
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Обмен» в Торговом доме: список, карточка, своя сторона текстом и кнопками,
  * принять и отозвать — всё через общий сервис обмена. */
object TradeStateSpec extends ZIOSpecDefault {

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((Map("action" -> key) ++ data).asJson.noSpaces))
  private def typed(text: String): UserAction = UserAction(text, None)

  private def screen(f: Fx) = TradeState(f.heroes, f.trades, f.transfers, f.users, f.players, f.content)
  private def last(r: TestRenderer): Task[Screen] = r.sentScreens.map(_.last)
  private def ids(s: Screen): List[String] = s.choices.map(_.id)
  private def labelOf(s: Screen, id: String): Option[String] = s.choices.find(_.id == id).map(_.label)

  /** Алиса собирает и отправляет предложение экраном: меч, 100 серебра, записка. */
  private def propose(s: TradeState, r: TestRenderer) =
    ZIO.foreachDiscard(List(tap("TradeNew"), typed("Борис"), tap("TradeAdd"), tap(s"${TradeState.AddPrefix}${sword.id}"),
      tap("TradeDraft"), tap("TradeSilver"), typed("100"), tap("TradeNote"), typed("Хочу щит"), tap("TradeSend")))(
      s.action(alice, _, r))

  private def act(s: TradeState, user: User, r: TestRenderer, uas: UserAction*) = ZIO.foreachDiscard(uas)(s.action(user, _, r))

  override def spec = suite("«Обмен» в Торговом доме")(

    test("в Торговом доме — кнопка «Обмен», и ведёт она на экран обмена") {
      for {
        f     <- fx()
        r     <- TestRenderer.make
        house  = TradeHouseState(f.heroes, f.users, TestBankRepository.empty, pangea.service.parcel.Parcels(TestParcelDao.empty,
                   TestBankRepository.empty, f.content), TestInventoryRepository.accepting, TestDonations.off, f.content)
        _     <- house.enter(alice, r)
        menu  <- last(r)
        next  <- house.action(alice, tap("TradeMenu"), r)
      } yield assertTrue(labelOf(menu, "TradeMenu").contains(f.content.text("bank.tradeHouse.tradeLabel")),
        next == StateType.Trade, StateType.cityStates.contains(StateType.Trade))
    },

    test("предложить: имя текстом, вещь из списка, серебро и записка — и отправить; на карточке ждём партнёра") {
      for {
        f     <- fx()
        s      = screen(f)
        r     <- TestRenderer.make
        _     <- s.enter(alice, r)
        list  <- last(r)
        _     <- propose(s, r)
        card  <- last(r)
        all   <- r.sentScreens
        offer  = f.dao.snapshot.headOption
      } yield assertTrue(
        list.text.contains(f.content.text("trade.menu.empty")), ids(list).contains("TradeNew"),
        offer.exists(o => o.fromSide.items == List(sword) && o.fromSide.silver == 100L && o.fromSide.note.contains("Хочу щит")),
        f.bags.of(aHero) == List(axe, ring, fang),
        card.text.startsWith(f.content.text("trade.done.sent")), card.text.contains(f.content.text("trade.card.theirTurn")),
        labelOf(card, "TradeCancel").contains(f.content.text("trade.card.cancel")), !ids(card).contains("TradeAccept"),
        all.exists(_.text.contains(f.content.format("trade.pick.added", "name" -> sword.displayTitle)))
      )
    },

    test("имя: нет такого героя или своё — отказ и снова вопрос; сумма не числом — переспросить") {
      for {
        f      <- fx()
        s       = screen(f)
        r      <- TestRenderer.make
        _      <- s.enter(alice, r)
        _      <- act(s, alice, r, tap("TradeNew"), typed("Некто"))
        nobody <- last(r)
        _      <- act(s, alice, r, typed("алиса"))
        self   <- last(r)
        _      <- act(s, alice, r, typed("Борис"), tap("TradeSilver"), typed("сто"))
        word   <- last(r)
      } yield assertTrue(
        nobody.text.startsWith(f.content.format("trade.noPartner", "name" -> "Некто")),
        nobody.text.endsWith(f.content.text("trade.ask.name")),
        self.text.startsWith(f.content.text("trade.self")),
        word.text.startsWith(f.content.text("trade.ask.notNumber"))
      )
    },

    test("партнёру: в списке — «ваш ход», на карточке — принять, изменить, отклонить; ответил своим — ход ушёл") {
      for {
        f     <- fx()
        a     <- f.hero(alice)
        offer <- f.trades.propose(alice, a, "Борис", Trades.Draft(List(sword.id), 100L, 0L, None), 1L).map(_.toOption.get)
        s      = screen(f)
        r     <- TestRenderer.make
        _     <- s.enter(bob, r)
        list  <- last(r)
        _     <- s.action(bob, tap("TradeOpen", TradeState.IdKey -> offer.id.toString), r)
        card  <- last(r)
        _     <- act(s, bob, r, tap("TradeEdit", TradeState.IdKey -> offer.id.toString), tap("TradeAdd"),
                   tap(s"${TradeState.AddPrefix}${shield.id}"))
        lastIn <- last(r)
        _     <- act(s, bob, r, tap("TradeSend"))
        after <- last(r)
        now    = f.dao.snapshot.headOption
      } yield assertTrue(
        list.choices.exists(c => c.id == "TradeOpen" && c.label == f.content.format("trade.menu.openYours", "n" -> "1", "name" -> "Иван Иванов")),
        // На само предложение — согласиться, а не подтвердить: подтверждает тот, кто предложил.
        ids(card).contains("TradeAgree"), !ids(card).contains("TradeAccept"), ids(card).contains("TradeEdit"),
        labelOf(card, "TradeCancel").contains(f.content.text("trade.card.decline")),
        // Положил последнее, что было, — сказано, что легло, а не «положить нечего».
        lastIn.text.startsWith(f.content.format("trade.pick.added", "name" -> shield.displayTitle)),
        !lastIn.text.contains(f.content.text("trade.pick.none")), ids(lastIn).contains("TradeSend"),
        after.text.startsWith(f.content.text("trade.done.answered")),
        now.exists(o => o.turn == alice.userId && o.toSide.items == List(shield)), f.bags.of(bHero).isEmpty
      )
    },

    test("согласиться и подтвердить: оба раза переспросить, что отдаёте и что получаете; в списке — что досталось") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", Trades.Draft(List(sword.id), 0L, 0L, None), 1L).map(_.toOption.get)
        b      <- f.hero(bob)
        _      <- f.trades.answer(bob, b, offer.id, Trades.Draft(List(shield.id), 0L, 0L, None), 2L)
        s       = screen(f)
        ra     <- TestRenderer.make
        rb     <- TestRenderer.make
        id      = TradeState.IdKey -> offer.id.toString
        _      <- act(s, alice, ra, tap("TradeOpen", id), tap("TradeAgree", id))
        askA   <- last(ra)
        _      <- s.action(alice, tap("TradeAgreeYes", id), ra)
        agreed <- last(ra)
        _      <- s.enter(alice, ra)
        listA  <- last(ra)
        _      <- s.action(alice, tap("TradeOpen", id), ra)
        cardA  <- last(ra)
        _      <- act(s, bob, rb, tap("TradeOpen", id))
        cardB  <- last(rb)
        _      <- s.action(bob, tap("TradeAccept", id), rb)
        askB   <- last(rb)
        _      <- s.action(bob, tap("TradeAcceptYes", id), rb)
        done   <- last(rb)
      } yield assertTrue(
        askA.text == f.content.format("trade.agreeAsk", "mine" -> sword.displayTitle, "theirs" -> shield.displayTitle),
        agreed.text.startsWith(f.content.text("trade.done.agreed")),
        listA.text.contains(f.content.text("trade.menu.waitConfirm")),
        cardA.text.contains(f.content.text("trade.card.waitConfirm")), !ids(cardA).contains("TradeAgree"),
        cardB.text.contains(f.content.text("trade.card.confirmTurn")),
        labelOf(cardB, "TradeAccept").contains(f.content.text("trade.card.accept")), !ids(cardB).contains("TradeAgree"),
        askB.text == f.content.format("trade.confirm", "mine" -> shield.displayTitle, "theirs" -> sword.displayTitle),
        done.text.startsWith(f.content.format("trade.done.accepted", "got" -> sword.displayTitle)),
        f.dao.snapshot.headOption.exists(_.status == TradeStatus.Accepted),
        f.sentTo(aHero) == List("Щит"), f.sentTo(bHero) == List("Меч")
      )
    },

    test("отозвать с карточки: всё вернулось, в списке предложения нет") {
      for {
        f     <- fx()
        s      = screen(f)
        r     <- TestRenderer.make
        _     <- s.enter(alice, r)
        _     <- propose(s, r)
        id     = f.dao.snapshot.headOption.map(_.id.toString).getOrElse("")
        _     <- s.action(alice, tap("TradeCancel", TradeState.IdKey -> id), r)
        list  <- last(r)
      } yield assertTrue(
        list.text.startsWith(f.content.text("trade.done.cancelled")), list.text.contains(f.content.text("trade.menu.empty")),
        f.bags.of(aHero).toSet == Set(sword, axe, ring, fang)
      )
    },

    test("сцена другого экрана или старой версии читается пустой") {
      assertTrue(
        parse("{}").flatMap(_.as[TradeState.TradeScene]) == Right(TradeState.TradeScene.empty),
        parse("""{"query":"меч","count":1}""").flatMap(_.as[TradeState.TradeScene]) == Right(TradeState.TradeScene.empty)
      )
    }
  ) @@ TestAspect.sequential
}
