package pangea.service.trade

import io.circe.parser.decode
import pangea.model.item.Item
import pangea.model.state.StateType
import pangea.model.trade.{TradeSide, TradeStatus}
import pangea.test.TestTrades._
import zio.test._
import zio.{Task, ZIO, durationInt}

/** Обмен предложением: своё — в заклад, ответ — своим, принимает тот, чей ход;
  * отозвать и отклонить — когда угодно, и тогда всё возвращается целиком. */
object TradesSpec extends ZIOSpecDefault {

  private val now = 1000000L

  private def draft(items: List[Item] = Nil, silver: Long = 0L, doubloons: Long = 0L, note: Option[String] = None) =
    Trades.Draft(items.map(_.id), silver, doubloons, note)

  /** Посылки получателю и письма идут отдельным волокном — ждём по живым часам. */
  private def eventually[A](get: => A)(ok: A => Boolean): Task[A] =
    Live.live((ZIO.sleep(10.millis) *> ZIO.succeed(get)).repeatUntil(ok)
      .timeoutFail(new Throwable("не дождались"))(5.seconds))

  private def offerOf(e: Either[String, pangea.model.trade.TradeOffer]) = e.toOption.getOrElse(throw new Exception(e.toString))

  override def spec = suite("Обмен между игроками")(

    test("предложение: своё — в заклад, серебро с комиссией; ход и письмо — партнёру; срок — трое суток") {
      for {
        f     <- fx()
        a     <- f.hero(alice)
        res   <- f.trades.propose(alice, a, "борис", draft(List(sword), silver = 100L, note = Some("Хочу щит")), now)
        offer  = offerOf(res)
        a2    <- f.hero(alice)
      } yield assertTrue(
        offer.from == alice.userId, offer.to == bob.userId, offer.turn == bob.userId,
        offer.status == TradeStatus.Open, offer.expiresAt == now + Trades.LifetimeMs,
        offer.fromSide.items == List(sword), offer.fromSide.silver == 100L, offer.fromSide.silverFee == 10L,
        offer.fromSide.note.contains("Хочу щит"),
        f.bags.of(aHero) == List(axe, ring, fang), a2.silver == 890L,
        f.letters(bob) == List(f.content.format("trade.offered", "name" -> "Иван Иванов"))
      )
    },

    test("отказы — и ничего не тронуто: не в городе, нет героя, сам с собой, пусто, трофей, чужая вещь, мало денег, длинная записка") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        far    <- fx(StateType.Dungeon).flatMap(g => g.hero(alice).flatMap(h => g.trades.propose(alice, h, "Борис", draft(List(sword)), now)))
        nobody <- f.trades.propose(alice, a, "Некто", draft(List(sword)), now)
        self   <- f.trades.propose(alice, a, "Алиса", draft(List(sword)), now)
        empty  <- f.trades.propose(alice, a, "Борис", draft(), now)
        trophy <- f.trades.propose(alice, a, "Борис", draft(List(fang)), now)
        alien  <- f.trades.propose(alice, a, "Борис", draft(List(shield)), now)
        little <- f.trades.propose(alice, a, "Борис", draft(silver = 10L), now)
        poor   <- f.trades.propose(alice, a, "Борис", draft(silver = 5000L), now)
        long   <- f.trades.propose(alice, a, "Борис", draft(note = Some("а" * (Trades.MaxNote + 1))), now)
        a2     <- f.hero(alice)
        c       = f.content
      } yield assertTrue(
        far == Left(c.text("trade.notInCity")),
        nobody == Left(c.format("trade.noPartner", "name" -> "Некто")),
        self == Left(c.text("trade.self")),
        empty == Left(c.text("trade.empty")),
        trophy == Left(c.format("trade.notTradable", "name" -> "Клык волка")),
        alien == Left(c.text("trade.itemGone")),
        little == Left(c.format("trade.tooLittle", "min" -> "🪙 50")),
        poor == Left(c.format("trade.notEnoughSilver", "need" -> "🪙 5500", "fee" -> "🪙 500", "have" -> "🪙 1000")),
        long == Left(c.format("trade.noteTooLong", "max" -> Trades.MaxNote.toString)),
        f.bags.of(aHero) == List(sword, axe, ring, fang), a2.silver == 1000L, f.dao.snapshot.isEmpty
      )
    },

    test("открытых предложений — не больше пяти") {
      for {
        f     <- fx()
        a     <- f.hero(alice)
        five  <- ZIO.foreach(1 to Trades.MaxOpen)(_ => f.trades.propose(alice, a, "Борис", draft(note = Some("?")), now))
        sixth <- f.trades.propose(alice, a, "Борис", draft(note = Some("?")), now)
      } yield assertTrue(five.forall(_.isRight), sixth == Left(f.content.format("trade.tooMany", "max" -> "5")))
    },

    test("ответ: своё — в заклад, ход и письмо — тому, кто предложил; не в свой ход — ни ответить, ни принять") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword), silver = 100L), now).map(offerOf)
        b      <- f.hero(bob)
        reply  <- f.trades.answer(bob, b, offer.id, draft(List(shield), doubloons = 50L), now + 5L).map(offerOf)
        b2     <- f.hero(bob)
        again  <- f.trades.answer(bob, b2, offer.id, draft(), now + 6L)
        early  <- f.trades.confirm(bob, b2, offer.id, now + 6L)
        agree  <- f.trades.agree(bob, b2, offer.id, now + 6L)
      } yield assertTrue(
        reply.turn == alice.userId, reply.toSide.items == List(shield), reply.toSide.doubloonsFee == 5L,
        reply.expiresAt == now + 5L + Trades.LifetimeMs,
        f.bags.of(bHero).isEmpty, b2.doubloons == 45L,
        f.letters(alice) == List(f.content.format("trade.answered", "name" -> "Иван Иванов")),
        again == Left(f.content.text("trade.notYourTurn")), early == Left(f.content.text("trade.notYourTurn")),
        agree == Left(f.content.text("trade.notYourTurn"))
      )
    },

    test("ответил своим — соглашается тот, кто предложил, подтверждает ответивший: каждому — заклад другого, комиссия сгорела") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword), silver = 100L), now).map(offerOf)
        b      <- f.hero(bob)
        _      <- f.trades.answer(bob, b, offer.id, draft(List(shield), doubloons = 50L), now)
        a2     <- f.hero(alice)
        early  <- f.trades.confirm(alice, a2, offer.id, now + 1L)
        agreed <- f.trades.agree(alice, a2, offer.id, now + 1L).map(offerOf)
        b2     <- f.hero(bob)
        twice  <- f.trades.agree(bob, b2, offer.id, now + 2L)
        done   <- f.trades.confirm(bob, b2, offer.id, now + 2L).map(offerOf)
        got     = f.content.format("trade.accepted", "name" -> "Иван Иванов", "got" -> (shield.displayTitle + ", 🟡 50"))
        mail   <- eventually(f.letters(alice))(_.exists(_.startsWith(got)))
        late   <- f.trades.cancel(alice, offer.id, now + 3L)
        a3     <- f.hero(alice)
        b3     <- f.hero(bob)
      } yield assertTrue(
        early == Left(f.content.text("trade.notAgreed")),
        agreed.turn == bob.userId, agreed.agreedBy.contains(alice.userId), agreed.expiresAt == now + 1L + Trades.LifetimeMs,
        f.letters(bob).contains(f.content.format("trade.agreed", "name" -> "Иван Иванов")),
        twice == Left(f.content.text("trade.agreedAlready")),
        done.status == TradeStatus.Accepted,
        f.sentTo(bHero) == List("Меч"), f.sentTo(aHero) == List("Щит"),
        f.payouts.snapshot.get(bHero).contains((100L, 0L)), f.payouts.snapshot.get(aHero).contains((0L, 50L)),
        a3.silver == 890L, b3.doubloons == 45L, mail.nonEmpty,
        late == Left(f.content.text("trade.closed"))
      )
    },

    test("просто согласиться на предложение — ничего не уходит, пока тот, кто предложил, не подтвердит") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword), note = Some("Хочу щит")), now).map(offerOf)
        b      <- f.hero(bob)
        grab   <- f.trades.confirm(bob, b, offer.id, now + 1L)
        agreed <- f.trades.agree(bob, b, offer.id, now + 1L).map(offerOf)
        still  <- f.trades.confirm(bob, b, offer.id, now + 2L)
        sent0   = f.sentTo(bHero)
        a2     <- f.hero(alice)
        done   <- f.trades.confirm(alice, a2, offer.id, now + 3L).map(offerOf)
      } yield assertTrue(
        grab == Left(f.content.text("trade.notAgreed")),
        agreed.turn == alice.userId, agreed.agreedBy.contains(bob.userId), agreed.status == TradeStatus.Open,
        still == Left(f.content.text("trade.notYourTurn")), sent0.isEmpty,
        done.status == TradeStatus.Accepted, f.sentTo(bHero) == List("Меч")
      )
    },

    test("правка своей стороны снимает согласие: подтверждать нечего, пока партнёр не согласится снова") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword, axe)), now).map(offerOf)
        b      <- f.hero(bob)
        _      <- f.trades.agree(bob, b, offer.id, now + 1L)
        a2     <- f.hero(alice)
        edited <- f.trades.answer(alice, a2, offer.id, draft(List(sword)), now + 2L).map(offerOf)
        b2     <- f.hero(bob)
        early  <- f.trades.confirm(bob, b2, offer.id, now + 3L)
        again  <- f.trades.agree(bob, b2, offer.id, now + 3L).map(offerOf)
      } yield assertTrue(
        edited.agreedBy.isEmpty, edited.turn == bob.userId,
        early == Left(f.content.text("trade.notAgreed")),
        again.agreedBy.contains(bob.userId), again.turn == alice.userId
      )
    },

    test("правка своей стороны: убранное — назад в сумку, добавленное — в заклад, деньги — разницей") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword, axe), silver = 100L), now).map(offerOf)
        b      <- f.hero(bob)
        _      <- f.trades.answer(bob, b, offer.id, draft(note = Some("Без топора, но с кольцом")), now)
        a2     <- f.hero(alice)
        edited <- f.trades.answer(alice, a2, offer.id, draft(List(sword, ring), silver = 50L), now).map(offerOf)
        a3     <- f.hero(alice)
      } yield assertTrue(
        edited.fromSide.items == List(sword, ring), edited.fromSide.silver == 50L, edited.fromSide.silverFee == 5L,
        f.bags.of(aHero) == List(fang, axe), a3.silver == 890L,
        f.payouts.snapshot.get(aHero).contains((55L, 0L)), edited.turn == bob.userId
      )
    },

    test("запись опоздала — положенное возвращается, сказано заново открыть") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword)), now).map(offerOf)
        b      <- f.hero(bob)
        _       = f.dao.loseNextUpdate()
        stale  <- f.trades.answer(bob, b, offer.id, draft(List(shield), doubloons = 50L), now)
        b2     <- f.hero(bob)
      } yield assertTrue(
        stale == Left(f.content.text("trade.stale")),
        f.bags.of(bHero) == List(shield), b2.doubloons == 100L,
        f.dao.snapshot.headOption.exists(o => o.toSide.isEmpty && o.turn == bob.userId)
      )
    },

    test("отозвать можно в любой момент — и не в свой ход, и вне города: своё — в сумку, деньги целиком, с комиссией") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword), silver = 100L, doubloons = 50L), now).map(offerOf)
        _      <- f.heroes.updateState(alice.userId, StateType.Dungeon)
        done   <- f.trades.cancel(alice, offer.id, now + 1L).map(offerOf)
        a2     <- f.hero(alice)
      } yield assertTrue(
        done.status == TradeStatus.Cancelled,
        f.bags.of(aHero).toSet == Set(sword, axe, ring, fang),
        f.payouts.snapshot.get(aHero).contains((110L, 0L)), a2.doubloons == 200L,
        // Борис ничего не клал — и «возвращается» ему не пишем.
        f.letters(bob).lastOption.contains(f.content.format("trade.cancelled", "name" -> "Иван Иванов"))
      )
    },

    test("отклонить: заклад предложившего — ему посылкой и деньгами, ему письмо") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        offer  <- f.trades.propose(alice, a, "Борис", draft(List(sword), silver = 100L), now).map(offerOf)
        done   <- f.trades.cancel(bob, offer.id, now + 1L).map(offerOf)
        mail   <- eventually(f.letters(alice))(_.nonEmpty)
      } yield assertTrue(
        done.status == TradeStatus.Declined,
        f.sentTo(aHero) == List("Меч"), f.payouts.snapshot.get(aHero).contains((110L, 0L)),
        mail.headOption.exists(_.startsWith(
          s"${f.content.format("trade.declined", "name" -> "Иван Иванов")} ${f.content.text("trade.returns")}"))
      )
    },

    test("срок вышел — закрыто, заклады хозяевам, обоим письмо; свежее не тронуто") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        old    <- f.trades.propose(alice, a, "Борис", draft(List(sword)), now).map(offerOf)
        b      <- f.hero(bob)
        _      <- f.trades.answer(bob, b, old.id, draft(List(shield)), now)
        a2     <- f.hero(alice)
        fresh  <- f.trades.propose(alice, a2, "Борис", draft(List(axe)), now + Trades.LifetimeMs).map(offerOf)
        _      <- f.trades.expire(now + Trades.LifetimeMs + 1L)
        toA    <- eventually(f.letters(alice))(_.nonEmpty)
        toB    <- eventually(f.letters(bob))(_.exists(_.startsWith(f.content.format("trade.expired", "name" -> "Иван Иванов"))))
        all    <- ZIO.succeed(f.dao.snapshot)
      } yield assertTrue(
        all.find(_.id == old.id).exists(_.status == TradeStatus.Expired),
        all.find(_.id == fresh.id).exists(_.status == TradeStatus.Open),
        f.sentTo(aHero) == List("Меч"), f.sentTo(bHero) == List("Щит"),
        toA.exists(_.startsWith(s"${f.content.format("trade.expired", "name" -> "Иван Иванов")} ${f.content.text("trade.returns")}")),
        toB.exists(_.startsWith(s"${f.content.format("trade.expired", "name" -> "Иван Иванов")} ${f.content.text("trade.returns")}"))
      )
    },

    test("срок вышел, а партнёр ничего не клал: ему — без «возвращается», предложившему — с ним") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        _      <- f.trades.propose(alice, a, "Борис", draft(List(sword)), now)
        _      <- f.trades.expire(now + Trades.LifetimeMs + 1L)
        expired = f.content.format("trade.expired", "name" -> "Иван Иванов")
        toA    <- eventually(f.letters(alice))(_.nonEmpty)
        toB    <- eventually(f.letters(bob))(_.contains(expired))
      } yield assertTrue(
        toA.exists(_.startsWith(s"$expired ${f.content.text("trade.returns")}")),
        toB.contains(expired), !toB.exists(_.contains(f.content.text("trade.returns")))
      )
    },

    test("/restart: своё предложение отозвано, чужое отклонено; партнёру — его заклад и письмо, заклад стёртого пропал") {
      for {
        f      <- fx()
        a      <- f.hero(alice)
        mine   <- f.trades.propose(alice, a, "Борис", draft(List(sword), silver = 100L), now).map(offerOf)
        b      <- f.hero(bob)
        _      <- f.trades.answer(bob, b, mine.id, draft(List(shield)), now)
        b2     <- f.hero(bob)
        theirs <- f.trades.propose(bob, b2, "Алиса", draft(silver = 200L), now).map(offerOf)
        _      <- f.trades.forfeit(alice.userId, now + 1L)
        wiped   = s"${f.content.format("trade.wiped", "name" -> "Иван Иванов")} ${f.content.text("trade.returns")}"
        toB    <- eventually(f.letters(bob))(_.count(_.startsWith(wiped)) == 2)
        // срок потом выходит — а возвращать уже нечего: оба закрыты
        _      <- f.trades.expire(now + Trades.LifetimeMs + 1L)
        a2     <- f.hero(alice)
        all    <- ZIO.succeed(f.dao.snapshot)
      } yield assertTrue(
        all.find(_.id == mine.id).exists(_.status == TradeStatus.Cancelled),
        all.find(_.id == theirs.id).exists(_.status == TradeStatus.Declined),
        // Борису — щит посылкой и 200 серебра с комиссией
        f.sentTo(bHero) == List("Щит"), f.payouts.snapshot.get(bHero).contains((220L, 0L)),
        toB.count(_.startsWith(wiped)) == 2,
        // Алисе — ничего: ни меча, ни 100 серебра с комиссией, ни письма о закрытии
        f.sentTo(aHero).isEmpty, f.payouts.snapshot.get(aHero).isEmpty,
        f.bags.of(aHero) == List(axe, ring, fang), a2.silver == 890L,
        !f.letters(alice).exists(_.contains(f.content.format("trade.wiped", "name" -> "Иван Иванов")))
      )
    },

    test("/restart без открытых обменов ничего не трогает и никому не пишет") {
      for {
        f     <- fx()
        a     <- f.hero(alice)
        offer <- f.trades.propose(alice, a, "Борис", draft(List(sword)), now).map(offerOf)
        _     <- f.trades.cancel(alice, offer.id, now + 1L)
        sent   = f.players.sentLetters.size
        _     <- f.trades.forfeit(alice.userId, now + 2L)
        _     <- f.trades.forfeit(bob.userId, now + 2L)
      } yield assertTrue(f.players.sentLetters.size == sent, f.dao.snapshot.forall(_.status == TradeStatus.Cancelled))
    },

    test("сторона из старой записи без новых полей читается: недостающее — нулём и пусто") {
      assertTrue(decode[TradeSide]("""{"items":[],"silver":100}""") == Right(TradeSide(Nil, 100L, 0L, 0L, 0L, None)))
    }
  ) @@ TestAspect.sequential
}
