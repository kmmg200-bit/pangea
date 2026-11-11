package pangea.service.state.states.hero

import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.squad.{Ally, AllyKind, AllyRates, Squad, UndeadForm}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.{Duration, ZIO}
import zio.test._

/** «Отряд» в меню персонажа: строй по позициям, карточка союзника,
  * перестановка (в том числе на место героя) и увольнение. */
object SquadStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  /** Союзник в кнопках адресуется местом в строю: поднятых с алтаря бывает
    * несколько, и вид их не различает. */
  private def pick(key: String, at: Int, extra: (String, String)*): UserAction = {
    val fields = (("action" -> key) +: ("pos" -> at.toString) +: extra).map { case (k, v) => s""""$k":"$v"""" }
    UserAction("", Some(fields.mkString("{", ",", "}")))
  }

  private val lvl = 10L
  private val baseHero: Hero = TestFixtures.hero(userId, state = StateType.HeroStats).copy(lvl = lvl,
    squad = Squad.empty.hire(AllyKind.Human, lvl, 0L).hire(AllyKind.Gnome, lvl, 0L))

  private def makeState(hero: Hero) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (SquadState(heroDao, content), heroDao, renderer)

  override def spec = suite("SquadState")(

    test("крыса держится сутки и убегает со своим прощанием, а в карточке у неё зубы вместо стихии") {
      val form   = UndeadForm("Чумная крыса", lvl,
        pangea.model.stats.FightStats(atk = 40, hp = 400, armor = 200, defence = 5,
                                      evasion = 20, accuracy = 80, energy = 50))
      val withRat = baseHero.copy(squad = Squad.empty.summonRat(form, lvl, 0L))
      def open(after: Long) =
        for {
          t <- makeState(withRat)
          (state, dao, r) = t
          _     <- TestClock.adjust(Duration.fromMillis(after))
          _     <- state.enter(testUser, r)
          // карточка союзника: её показывают по месту в строю
          _     <- state.action(testUser, pick("SquadAlly", 2), r)
          said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
          squad <- dao.getHeroByUserId(userId).map(_.get.squad)
        } yield (said, squad)
      for {
        early <- open(AllyRates.RatMs - 1L)
        late  <- open(AllyRates.RatMs)
      } yield assertTrue(early._2.allies.size == 1 && early._2.hasRat) &&
              // в карточке на месте стихии — зубы, статы её собственные
              assertTrue(early._1.contains("Чумная крыса") && early._1.contains("зубы")) &&
              assertTrue(early._1.contains("400/400")) &&
              assertTrue(late._1.contains("утекает в ближайшую щель")) &&
              assertTrue(late._2.allies.isEmpty && late._2.offDuty.isEmpty && late._2.away.isEmpty)
    },

    test("поднятого хватает на сутки: потом кости рассыпаются, и отряд его не ждёт") {
      val form  = UndeadForm("Гоблин немощный раб", 16L, AllyKind.Human.stats(3L))
      val risen = baseHero.copy(squad = Squad.empty.raise(form, lvl, 0L))
      def open(after: Long) =
        for {
          t <- makeState(risen)
          (state, dao, r) = t
          _     <- TestClock.adjust(Duration.fromMillis(after))
          _     <- state.enter(testUser, r)
          said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
          squad <- dao.getHeroByUserId(userId).map(_.get.squad)
        } yield (said, squad)
      for {
        early <- open(AllyRates.UndeadMs - 1L)
        late  <- open(AllyRates.UndeadMs)
      } yield assertTrue(early._2.allies.size == 1 && !early._1.contains("истощилась")) &&
              assertTrue(late._1.contains(
                "💀 Тёмная энергия что держала кости Гоблин немощный раб вместе истощилась и они развалились.")) &&
              assertTrue(late._2.allies.isEmpty && late._2.offDuty.isEmpty && late._2.away.isEmpty)
    },

    test("кнопка «Отряд» в профиле есть только с союзниками") {
      for {
        withT <- (for {
                   dao      <- TestHeroDao.withHero(userId, baseHero)
                   renderer <- TestRenderer.make
                   content  <- ZIO.attempt(SceneContent.load())
                   _        <- HeroStatsState(dao, content).enter(testUser, renderer)
                   scr      <- renderer.sentScreens
                 } yield scr.last.choices.exists(_.id == "OpenSquad"))
        without <- (for {
                   dao      <- TestHeroDao.withHero(userId, baseHero.copy(squad = Squad.empty))
                   renderer <- TestRenderer.make
                   content  <- ZIO.attempt(SceneContent.load())
                   _        <- HeroStatsState(dao, content).enter(testUser, renderer)
                   scr      <- renderer.sentScreens
                 } yield scr.last.choices.exists(_.id == "OpenSquad"))
      } yield assertTrue(withT && !without)
    },

    test("enter → строй по позициям: герой, союзники, пустое место; кнопки — герой и союзники по порядку, «Резерв» и «Назад»") {
      for {
        t <- makeState(baseHero)
        (state, _, renderer) = t
        _   <- state.enter(testUser, renderer)
        scr <- renderer.sentScreens.map(_.last)
      } yield assertTrue(scr.text.contains("1. Вы")) &&
              assertTrue(scr.text.contains("2. Йорген Кремень — ❤ 625/625  🧥 750/750")) &&
              assertTrue(scr.text.contains("3. Брамбл Медноус — ❤ 560/560  🧥 1225/1225")) &&
              assertTrue(scr.text.contains("4. — пусто —")) &&
              assertTrue(scr.choices.map(_.label) ==
                List("👤 Вы", "Йорген Кремень", "Брамбл Медноус", "Резерв (0/10)", "↩ Назад")) &&
              // «Резерв» — в последнем ряду, рядом с «Назад»
              assertTrue(scr.choices.takeRight(2).map(_.row).distinct.size == 1) &&
              assertTrue(scr.choices.forall(_.label.length <= pangea.engine.Choice.MaxLabelLength))
    },

    test("карточка: состояние и позиция, кнопки «На N (кто там)», «Выгнать», «К отряду»") {
      for {
        t <- makeState(baseHero)
        (state, _, renderer) = t
        _   <- state.action(testUser, pick("SquadAlly", 2), renderer)
        scr <- renderer.sentScreens.map(_.last)
        moves = scr.choices.filter(_.id == "SquadMove").map(c => c.label -> c.data("to"))
      } yield assertTrue(scr.text.startsWith("Йорген Кремень\nЧеловек, стихия 🔥 — позиция 2\n")) &&
              assertTrue(scr.text.contains(" ❤ 625/625  🧥 Броня 750/750  ⚡ Энергия 500/500\n ⚔ Атк 100  🛡 Защ 200\n 🎯 Точн 500  👁 Укл 500")) &&
              assertTrue(moves.take(3) == List("На 1 (вы)" -> "1", "На 3 (Брамбл Медноус)" -> "3", "На 4 (пусто)" -> "4")) &&
              // мест в строю одиннадцать: кнопка на каждое, кроме своего
              assertTrue(moves.size == pangea.model.squad.AllyRates.Positions - 1) &&
              assertTrue(scr.choices.flatMap(_.row).groupBy(identity).forall(_._2.size <= 5)) &&
              assertTrue(scr.choices.exists(_.id == "SquadDismiss") && scr.choices.exists(_.id == "SquadList")) &&
              assertTrue(scr.choices.forall(_.label.length <= pangea.engine.Choice.MaxLabelLength))
    },

    test("перестановка на место героя: герой встаёт на прежнее; на союзника — меняются; сохраняется") {
      for {
        t <- makeState(baseHero)
        (state, dao, renderer) = t
        r1 <- state.action(testUser, pick("SquadMove", 2, "to" -> "1"), renderer)
        h1 <- dao.getHeroByUserId(userId).map(_.get)
        _  <- state.action(testUser, pick("SquadMove", 3, "to" -> "1"), renderer)
        h2 <- dao.getHeroByUserId(userId).map(_.get)
        scr <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(r1 == StateType.Squad) &&
              assertTrue(h1.squad.heroPos == 2 && h1.squad.allyAt(1).exists(_.kind == AllyKind.Human)) &&
              assertTrue(h2.squad.allyAt(1).exists(_.kind == AllyKind.Gnome) && h2.squad.allyAt(3).exists(_.kind == AllyKind.Human)) &&
              assertTrue(scr.contains("Йорген Кремень теперь на позиции 1."))
    },

    test("герой выбирает место сам: карточка с кнопкой на каждое; на пустое — встаёт, на союзника — меняются") {
      for {
        t <- makeState(baseHero)
        (state, dao, renderer) = t
        _    <- state.action(testUser, tap("SquadHero"), renderer)
        card <- renderer.sentScreens.map(_.last)
        moves = card.choices.filter(_.id == "SquadHeroMove").map(c => c.label -> c.data("to"))
        _    <- state.action(testUser, UserAction("", Some("""{"action":"SquadHeroMove","to":"10"}""")), renderer)
        h1   <- dao.getHeroByUserId(userId).map(_.get)
        _    <- state.action(testUser, UserAction("", Some("""{"action":"SquadHeroMove","to":"3"}""")), renderer)
        h2   <- dao.getHeroByUserId(userId).map(_.get)
        said <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
        _    <- state.enter(testUser, renderer)
        list <- renderer.sentScreens.map(_.last)
      } yield assertTrue(card.text.startsWith("Вы — позиция 1.")) &&
              assertTrue(moves.take(3) == List("На 2 (Йорген Кремень)" -> "2", "На 3 (Брамбл Медноус)" -> "3", "На 4 (пусто)" -> "4")) &&
              assertTrue(moves.size == AllyRates.Positions - 1 && card.choices.exists(_.id == "SquadList")) &&
              assertTrue(card.choices.forall(_.label.length <= pangea.engine.Choice.MaxLabelLength)) &&
              // на пустое 10-е: союзники стоят где стояли
              assertTrue(h1.squad.heroPos == 10 && h1.squad.allyAt(2).exists(_.kind == AllyKind.Human) &&
                         h1.squad.allyAt(3).exists(_.kind == AllyKind.Gnome)) &&
              // на место Брамбла: он встаёт на прежнее место героя
              assertTrue(h2.squad.heroPos == 3 && h2.squad.allyAt(10).exists(_.kind == AllyKind.Gnome)) &&
              assertTrue(said.contains("Вы теперь на позиции 10.") && said.contains("Вы теперь на позиции 3.")) &&
              // в строю кнопки по порядку мест: Йорген на 2, герой на 3, Брамбл на 10
              assertTrue(list.choices.map(_.label) == List("Йорген Кремень", "👤 Вы", "Брамбл Медноус", "Резерв (0/10)", "↩ Назад"))
    },

    test("увольнение — с подтверждением; «Нет» оставляет, «Да» убирает и сохраняет") {
      for {
        t <- makeState(baseHero)
        (state, dao, renderer) = t
        _   <- state.action(testUser, pick("SquadDismiss", 2), renderer)
        ask <- renderer.sentScreens.map(_.last)
        _   <- state.action(testUser, pick("SquadAlly", 2), renderer)
        kept <- dao.getHeroByUserId(userId).map(_.get)
        _   <- state.action(testUser, pick("SquadDismissDo", 2), renderer)
        gone <- dao.getHeroByUserId(userId).map(_.get)
        back <- state.action(testUser, tap("BackFromSquad"), renderer)
      } yield assertTrue(ask.text == "Выгнать Йорген Кремень из отряда? Нанять снова можно будет в таверне.") &&
              assertTrue(ask.choices.map(_.id) == List("SquadDismissDo", "SquadAlly")) &&
              assertTrue(kept.squad.has(AllyKind.Human)) &&
              assertTrue(!gone.squad.has(AllyKind.Human) && gone.squad.has(AllyKind.Gnome)) &&
              assertTrue(back == StateType.HeroStats)
    },

    test("резерв: пустой — с объяснением; из карточки союзник уходит туда и стоит в списке, «Отряд» в профиле не пропадает") {
      for {
        t <- makeState(baseHero)
        (state, dao, renderer) = t
        _     <- state.action(testUser, tap("SquadReserve"), renderer)
        empty <- renderer.sentScreens.map(_.last)
        _     <- state.action(testUser, pick("SquadAlly", 2), renderer)
        card  <- renderer.sentScreens.map(_.last)
        _     <- state.action(testUser, pick("SquadToReserve", 2), renderer)
        _     <- state.action(testUser, pick("SquadToReserve", 3), renderer)
        said  <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
        hero  <- dao.getHeroByUserId(userId).map(_.get)
        _     <- state.action(testUser, tap("SquadReserve"), renderer)
        list  <- renderer.sentScreens.map(_.last)
        // в строю никого, а в резерве двое — «Отряд» в профиле на месте
        stats <- (for {
                   r2      <- TestRenderer.make
                   content <- ZIO.attempt(SceneContent.load())
                   _       <- HeroStatsState(dao, content).enter(testUser, r2)
                   scr     <- r2.sentScreens
                 } yield scr.last.choices.exists(_.id == "OpenSquad"))
      } yield assertTrue(empty.text.startsWith("Резерв пуст.") && empty.choices.map(_.id) == List("SquadList")) &&
              // «В резерв» — в одном ряду с «Выгнать»
              assertTrue(card.choices.filter(_.row == card.choices.find(_.id == "SquadDismiss").flatMap(_.row))
                .map(_.id) == List("SquadToReserve", "SquadDismiss")) &&
              assertTrue(said.contains("Йорген Кремень уходит в резерв.") && said.contains("Брамбл Медноус уходит в резерв.")) &&
              assertTrue(hero.squad.allies.isEmpty && hero.squad.reserve.map(_.kind) == List(AllyKind.Human, AllyKind.Gnome)) &&
              assertTrue(list.text.startsWith("Резерв — 2 из 10.") && list.text.contains("1. Йорген Кремень — ❤ 625/625")) &&
              assertTrue(list.choices.map(_.id) == List("SquadReserveAlly", "SquadReserveAlly", "SquadList")) &&
              assertTrue(list.choices.headOption.exists(c => c.data == Map("r" -> "0", "who" -> "Йорген Кремень"))) &&
              assertTrue(stats)
    },

    test("из резерва — в строй: на свободное место; мест нет — спрашивают, кого отправить взамен, и меняют") {
      val orc     = UndeadForm("Поднятый орк", 3L, AllyKind.Human.stats(3L))
      val waiting = baseHero.copy(squad = baseHero.squad.copy(reserve = List(
        Ally(AllyKind.Undead, 0, 100L, 0L, 0L, hiredUntil = Long.MaxValue, undead = Some(orc)))))
      // тот же резерв, а строй полон: Йорген, Брамбл и восемь поднятых
      val crowded = waiting.copy(squad = (4 to AllyRates.Positions).foldLeft(waiting.squad)((s, p) =>
        s.copy(allies = s.allies :+ Ally(AllyKind.Undead, p, 1L, 0L, 0L, hiredUntil = Long.MaxValue,
          undead = Some(orc.copy(name = s"Поднятый $p"))))))
      def card(at: Int) = pick("SquadSwapIn", at, "r" -> "0", "who" -> "Поднятый орк")
      for {
        t1 <- makeState(waiting)
        (s1, d1, r1) = t1
        _    <- s1.action(testUser, UserAction("", Some("""{"action":"SquadDeploy","r":"0","who":"Поднятый орк"}""")), r1)
        free <- d1.getHeroByUserId(userId).map(_.get)
        said <- r1.sentScreens.map(_.map(_.text).mkString("\n"))
        t2 <- makeState(crowded)
        (s2, d2, r2) = t2
        _    <- s2.action(testUser, UserAction("", Some("""{"action":"SquadDeploy","r":"0","who":"Поднятый орк"}""")), r2)
        ask  <- r2.sentScreens.map(_.last)
        _    <- s2.action(testUser, card(2), r2)
        swap <- d2.getHeroByUserId(userId).map(_.get)
        told <- r2.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(free.squad.allyAt(4).exists(_.name == "Поднятый орк") && free.squad.reserve.isEmpty) &&
              assertTrue(said.contains("Поднятый орк встаёт в строй на позицию 4.")) &&
              assertTrue(ask.text.startsWith("В строю нет мест.") && ask.choices.count(_.id == "SquadSwapIn") == 10) &&
              assertTrue(ask.choices.lastOption.exists(_.id == "SquadReserveAlly")) &&
              // Йорген уходит в резерв, орк встаёт на его место
              assertTrue(swap.squad.allyAt(2).exists(_.name == "Поднятый орк") &&
                         swap.squad.reserve.map(_.kind) == List(AllyKind.Human)) &&
              assertTrue(told.contains("Йорген Кремень уходит в резерв, а Поднятый орк встаёт на позицию 2."))
    },

    test("не наёмник — в строю и в резерве: «покинет вас навсегда», а не «нанять в таверне»") {
      val form  = UndeadForm("Гоблин немощный раб", 16L, AllyKind.Human.stats(3L))
      val risen = baseHero.copy(squad = baseHero.squad.raise(form, lvl, 0L))
      val pos   = risen.squad.inOrder.find(_.kind == AllyKind.Undead).map(_.position).getOrElse(0)
      val gone  = "Выгнать Гоблин немощный раб из отряда? Гоблин немощный раб покинет вас навсегда: в таверне таких не нанимают."
      for {
        t <- makeState(risen)
        (state, _, renderer) = t
        _     <- state.action(testUser, pick("SquadDismiss", pos), renderer)
        line  <- renderer.sentScreens.map(_.last)
        t2 <- makeState(risen.copy(squad = risen.squad.toReserve(pos)))
        (state2, _, renderer2) = t2
        _     <- state2.action(testUser,
                   UserAction("", Some(s"""{"action":"SquadReserveDismiss","r":"0","who":"Гоблин немощный раб"}""")), renderer2)
        inRes <- renderer2.sentScreens.map(_.last)
      } yield assertTrue(pos > 0 && line.text == gone && inRes.text == gone) &&
              assertTrue(!line.text.contains("таверне можно") && !line.text.contains("Нанять снова"))
    },

    test("выгнать из резерва — с подтверждением; сдвинулся список — кнопка ни о чём") {
      val reserved = baseHero.copy(squad = baseHero.squad.toReserve(2))
      def act(key: String, who: String) =
        UserAction("", Some(s"""{"action":"$key","r":"0","who":"$who"}"""))
      for {
        t <- makeState(reserved)
        (state, dao, renderer) = t
        _     <- state.action(testUser, act("SquadReserveDismiss", "Йорген Кремень"), renderer)
        ask   <- renderer.sentScreens.map(_.last)
        // кнопка с чужим именем на этом месте — ничего не трогаем
        _     <- state.action(testUser, act("SquadReserveDismissDo", "Брамбл Медноус"), renderer)
        kept  <- dao.getHeroByUserId(userId).map(_.get)
        _     <- state.action(testUser, act("SquadReserveDismissDo", "Йорген Кремень"), renderer)
        gone  <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(ask.text.contains("Выгнать Йорген Кремень из отряда?")) &&
              assertTrue(ask.choices.map(_.id) == List("SquadReserveDismissDo", "SquadReserveAlly")) &&
              assertTrue(kept.squad.reserve.map(_.kind) == List(AllyKind.Human)) &&
              assertTrue(gone.squad.reserve.isEmpty && !gone.squad.has(AllyKind.Human) && gone.squad.has(AllyKind.Gnome))
    }
  )
}
