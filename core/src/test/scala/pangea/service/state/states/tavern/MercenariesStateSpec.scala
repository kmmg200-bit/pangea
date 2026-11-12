package pangea.service.state.states.tavern

import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.item.{BrewKind, Item}
import pangea.model.squad.{AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Наёмники в таверне: за столом только свободные; Йорген берёт серебром,
  * Плюх — Живой водой, Брамбл — шнапсом; вернувшийся из отлучки встречает
  * репликой и снова нанимается. */
object MercenariesStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def pick(key: String, kind: AllyKind): UserAction =
    UserAction("", Some(s"""{"action":"$key","kind":"${kind.entryName}"}"""))

  private def brew(kind: BrewKind, id: Long): Item = BrewKind.item(kind).copy(id = id)

  private val baseHero: Hero = TestFixtures.hero(userId, state = StateType.Tavern).copy(lvl = 10L, silver = 0L)

  private def makeState(hero: Hero, items: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (MercenariesState(heroDao, invRepo, content), heroDao, invRepo, renderer)

  override def spec = suite("MercenariesState")(

    test("enter → четверо за столом и «Назад»; карточка — история и реплика с ценой") {
      for {
        t <- makeState(baseHero)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, pick("MercCard", AllyKind.Murloc), renderer)
        screens <- renderer.sentScreens
        list     = screens.head
        card     = screens.last
      } yield assertTrue(list.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Human", "Murloc", "Gnome", "Elf")) &&
              assertTrue(list.choices.exists(_.id == "BackFromMercs")) &&
              assertTrue(card.text.contains("«Плюх. Плюх — береговик")) &&
              assertTrue(card.text.contains("флаконов: 2.")) &&
              assertTrue(card.choices.exists(c => c.id == "MercHire" && c.data.get("kind").contains("Murloc")))
    },

    test("Йорген: всегда 10 дублонов — мало → отказ, хватает → нанят, дублоны списаны") {
      for {
        poor <- makeState(baseHero.copy(doubloons = 9L))
        (ps, pdao, _, pr) = poor
        _     <- ps.action(testUser, pick("MercHire", AllyKind.Human), pr)
        pHero <- pdao.getHeroByUserId(userId).map(_.get)
        pScr  <- pr.sentScreens.map(_.map(_.text).mkString("\n"))
        rich <- makeState(baseHero.copy(doubloons = 12L, silver = 100000L))
        (rs, rdao, _, rr) = rich
        _     <- rs.action(testUser, pick("MercHire", AllyKind.Human), rr)
        rHero <- rdao.getHeroByUserId(userId).map(_.get)
        rScr  <- rr.sentScreens
      } yield assertTrue(pScr.contains("Не хватает дублонов: нужно 10, у вас 9")) &&
              assertTrue(pHero.squad.isEmpty && pHero.doubloons == 9L) &&
              // серебро при найме Йоргена не трогается вовсе
              assertTrue(rHero.doubloons == 2L && rHero.silver == 100000L) &&
              assertTrue(rHero.squad.allyAt(2).exists(_.kind == AllyKind.Human)) &&
              assertTrue(rScr.exists(_.text.contains("Йорген Кремень встаёт из-за стола"))) &&
              // за столом его больше нет
              assertTrue(rScr.last.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Murloc", "Gnome", "Elf"))
    },

    test("Плюх берёт Живой водой (уровень 10 → 2 флакона): одного мало, два — уходят из сумки") {
      for {
        one <- makeState(baseHero, List(brew(BrewKind.LivingWater, 1L), brew(BrewKind.Schnapps, 2L)))
        (os, odao, _, or) = one
        _     <- os.action(testUser, pick("MercHire", AllyKind.Murloc), or)
        oHero <- odao.getHeroByUserId(userId).map(_.get)
        oScr  <- or.sentScreens.map(_.map(_.text).mkString("\n"))
        two <- makeState(baseHero, List(brew(BrewKind.LivingWater, 1L), brew(BrewKind.LivingWater, 2L), brew(BrewKind.Schnapps, 3L)))
        (ts, tdao, tinv, tr) = two
        _     <- ts.action(testUser, pick("MercHire", AllyKind.Murloc), tr)
        tHero <- tdao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(oScr.contains("нужно 2 — «Живая вода», у вас 1")) &&
              assertTrue(oHero.squad.isEmpty) &&
              assertTrue(tHero.squad.has(AllyKind.Murloc)) &&
              assertTrue(tinv.snapshot.map(_.id) == List(3L))
    },

    test("Фаэлин: «Ядовитая смазка» по уровню (10 → 2) и всегда 5 дублонов; не хватает чего-то — не берёт ничего") {
      val salves = List(brew(BrewKind.VenomSalve, 1L), brew(BrewKind.VenomSalve, 2L))
      for {
        a <- makeState(baseHero.copy(doubloons = 4L), salves)
        (as, adao, ainv, ar) = a
        _     <- as.action(testUser, pick("MercCard", AllyKind.Elf), ar)
        card  <- ar.sentScreens.map(_.last.text)
        _     <- as.action(testUser, pick("MercHire", AllyKind.Elf), ar)
        aHero <- adao.getHeroByUserId(userId).map(_.get)
        aScr  <- ar.sentScreens.map(_.map(_.text).mkString("\n"))
        b <- makeState(baseHero.copy(doubloons = 9L), salves.take(1))
        (bs, bdao, binv, br) = b
        _     <- bs.action(testUser, pick("MercHire", AllyKind.Elf), br)
        bHero <- bdao.getHeroByUserId(userId).map(_.get)
        bScr  <- br.sentScreens.map(_.map(_.text).mkString("\n"))
        c <- makeState(baseHero.copy(doubloons = 9L), salves :+ brew(BrewKind.Schnapps, 3L))
        (cs, cdao, cinv, cr) = c
        _     <- cs.action(testUser, pick("MercHire", AllyKind.Elf), cr)
        cHero <- cdao.getHeroByUserId(userId).map(_.get)
        cScr  <- cr.sentScreens
      } yield assertTrue(card.contains("«Фаэлин Тихострел.") && card.contains("флаконов: 2,") && card.contains("и 5 дублонов")) &&
              // смазки хватает, дублонов нет — ничего не списано
              assertTrue(aScr.contains("Не хватает дублонов: нужно 5, у вас 4")) &&
              assertTrue(aHero.squad.isEmpty && aHero.doubloons == 4L && ainv.snapshot.size == 2) &&
              // дублоны есть, смазки мало — тоже ничего
              assertTrue(bScr.contains("нужно 2 — «Ядовитая смазка», у вас 1")) &&
              assertTrue(bHero.squad.isEmpty && bHero.doubloons == 9L && binv.snapshot.size == 1) &&
              // хватает всего — нанят: две смазки ушли, шнапс остался, дублонов на пять меньше
              assertTrue(cHero.squad.has(AllyKind.Elf) && cHero.doubloons == 4L && cinv.snapshot.map(_.id) == List(3L)) &&
              assertTrue(cScr.exists(_.text.contains("Фаэлин Тихострел встаёт из-за стола")))
    },

    test("Брамбл берёт шнапсом; на первом уровне — одну бутылку") {
      val lowHero = baseHero.copy(lvl = 1L)
      for {
        t <- makeState(lowHero, List(brew(BrewKind.Schnapps, 1L)))
        (state, dao, inv, renderer) = t
        _    <- state.action(testUser, pick("MercCard", AllyKind.Gnome), renderer)
        card <- renderer.sentScreens.map(_.last.text)
        _    <- state.action(testUser, pick("MercHire", AllyKind.Gnome), renderer)
        hero <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(card.contains("бутылок: 1.")) &&
              assertTrue(hero.squad.has(AllyKind.Gnome) && inv.snapshot.isEmpty)
    },

    test("наёмник отработал 12 часов: у стола уходит с репликой, его нет; через 12 часов снова здесь — без реплики о свитке") {
      for {
        t <- makeState(baseHero.copy(doubloons = 10L))
        (state, dao, _, renderer) = t
        _     <- state.action(testUser, pick("MercHire", AllyKind.Human), renderer)
        _     <- TestClock.adjust(zio.Duration.fromMillis(12L * 60L * 60L * 1000L))
        _     <- state.enter(testUser, renderer)
        scr   <- renderer.sentScreens
        hero  <- dao.getHeroByUserId(userId).map(_.get)
        _     <- TestClock.adjust(zio.Duration.fromMillis(12L * 60L * 60L * 1000L))
        _     <- state.enter(testUser, renderer)
        all   <- renderer.sentScreens
      } yield assertTrue(scr.exists(_.text.contains("Йорген Кремень отработал свой день"))) &&
              assertTrue(!hero.squad.has(AllyKind.Human) && hero.squad.offDuty.contains("Human")) &&
              assertTrue(scr.last.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Murloc", "Gnome", "Elf")) &&
              assertTrue(all.last.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Human", "Murloc", "Gnome", "Elf")) &&
              assertTrue(!all.exists(_.text.contains("снова в таверне")))
    },

    test("в отлучке — за столом нет; вернулся — реплика о свитке один раз и снова нанимается") {
      val away = baseHero.copy(squad = Squad.empty.sentAway(AllyKind.Human, 0L))
      for {
        gone <- makeState(away)
        (gs, _, _, gr) = gone
        _      <- gs.enter(testUser, gr)
        gList  <- gr.sentScreens.map(_.head)
        _      <- TestClock.adjust(zio.Duration.fromMillis(24L * 60L * 60L * 1000L))
        back <- makeState(away)
        (bs, bdao, _, br) = back
        _      <- bs.enter(testUser, br)
        bScr   <- br.sentScreens
        bHero  <- bdao.getHeroByUserId(userId).map(_.get)
        _      <- bs.enter(testUser, br)
        again  <- br.sentScreens
      } yield assertTrue(gList.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Murloc", "Gnome", "Elf")) &&
              assertTrue(bScr.head.text.contains("Йорген Кремень снова в таверне.")) &&
              assertTrue(bScr.head.text.contains("свиток вынес")) &&
              assertTrue(bScr.last.choices.filter(_.id == "MercCard").flatMap(_.data.get("kind")) == List("Human", "Murloc", "Gnome", "Elf")) &&
              assertTrue(bHero.squad.away.isEmpty) &&
              assertTrue(again.count(_.text.contains("снова в таверне")) == 1)
    },

    test("у стола сказано, кто в отлучке и когда вернётся: по свитку и после найма; кто в отряде — не в отлучке") {
      val hour  = 60L * 60L * 1000L
      // Йорген ушёл по свитку (сутки), Плюх отработал найм и отдыхает (12 часов),
      // Брамбл и Фаэлин в отряде.
      val squad = Squad.empty.sentAway(AllyKind.Human, 0L).hire(AllyKind.Gnome, 10L, 0L).hire(AllyKind.Elf, 10L, 0L)
                    .copy(offDuty = Map("Murloc" -> 12L * hour))
      for {
        t <- makeState(baseHero.copy(squad = squad))
        (state, _, _, renderer) = t
        // Прошло 6 часов 40 минут: Йоргену ещё 17ч 20мин, Плюху — 5ч 20мин.
        _    <- TestClock.adjust(zio.Duration.fromMillis(6L * hour + 40L * 60000L))
        _    <- state.enter(testUser, renderer)
        list <- renderer.sentScreens.map(_.last)
      } yield assertTrue(list.text.contains("За дальним столом сегодня пусто.")) &&
              assertTrue(list.text.contains("🕓 Йорген Кремень вернётся через 17ч 20мин.")) &&
              assertTrue(list.text.contains("🕓 Плюх вернётся через 5ч 20мин.")) &&
              assertTrue(!list.text.contains("Брамбл") && !list.text.contains("Фаэлин")) &&
              assertTrue(list.choices.count(_.id == "MercCard") == 0)
    },

    test("отдыхающего Йоргена можно поднять: 5000 серебра и его 10 дублонов; следующий раз — 10 000") {
      val hour  = 60L * 60L * 1000L
      val tired = Squad.empty.copy(offDuty = Map("Human" -> 12L * hour))
      for {
        t <- makeState(baseHero.copy(squad = tired, silver = 20000L, doubloons = 20L))
        (state, dao, _, renderer) = t
        _     <- state.enter(testUser, renderer)
        list  <- renderer.sentScreens.map(_.last)
        _     <- state.action(testUser, pick("MercRush", AllyKind.Human), renderer)
        offer <- renderer.sentScreens.map(_.last)
        _     <- state.action(testUser, pick("MercRushHire", AllyKind.Human), renderer)
        hero  <- dao.getHeroByUserId(userId).map(_.get)
        // отработал и снова отдыхает — второй подъём уже дороже
        _     <- dao.updateSquad(userId, hero.squad.copy(allies = Nil, offDuty = Map("Human" -> 12L * hour)))
        _     <- state.action(testUser, pick("MercRush", AllyKind.Human), renderer)
        again <- renderer.sentScreens.map(_.last)
      } yield assertTrue(list.choices.exists(c => c.id == "MercRush" && c.data.get("kind").contains("Human"))) &&
              assertTrue(offer.text.contains("5000 серебра сверху") && offer.text.contains("10 дублонов")) &&
              assertTrue(hero.squad.has(AllyKind.Human) && hero.silver == 15000L && hero.doubloons == 10L) &&
              assertTrue(hero.squad.overtimeOf(AllyKind.Human) == 1) &&
              assertTrue(again.choices.exists(_.label.contains("10000")))
    },

    test("не хватает своего серебра — добирается из ячейки в Торговом доме") {
      val hour  = 60L * 60L * 1000L
      val tired = Squad.empty.copy(offDuty = Map("Human" -> 12L * hour))
      val bank  = pangea.test.TestBankRepository.of(1, silver = 4000L)
      for {
        heroDao  <- TestHeroDao.withHero(userId, baseHero.copy(squad = tired, silver = 2000L, doubloons = 10L))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = MercenariesState(heroDao, TestInventoryRepository.withItems(Nil), content, Some(bank))
        _        <- state.action(testUser, pick("MercRushHire", AllyKind.Human), renderer)
        hero     <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(hero.squad.has(AllyKind.Human) && hero.silver == 0L) &&
              assertTrue(bank.silverSnapshot == 1000L)
    },

    test("дали отдохнуть — обычный найм, и переработки забыты") {
      val worked = Squad.empty.copy(overtime = Map("Human" -> 3))
      for {
        t <- makeState(baseHero.copy(squad = worked, doubloons = 10L))
        (state, dao, _, renderer) = t
        _    <- state.action(testUser, pick("MercHire", AllyKind.Human), renderer)
        hero <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(hero.squad.has(AllyKind.Human) && hero.squad.overtimeOf(AllyKind.Human) == 0)
    },

    test("одиннадцатый подъём за серебро — «Переработки наше всё», +100 к HP") {
      val hour  = 60L * 60L * 1000L
      val tired = Squad.empty.copy(offDuty = Map("Human" -> 12L * hour), overtimeTotal = 10)
      for {
        t <- makeState(baseHero.copy(squad = tired, silver = 100000L, doubloons = 10L))
        (state, dao, _, renderer) = t
        before = baseHero.effectiveMaxHp(0L)
        _    <- state.action(testUser, pick("MercRushHire", AllyKind.Human), renderer)
        hero <- dao.getHeroByUserId(userId).map(_.get)
        said <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(hero.hasAchievement(pangea.model.hero.Achievement.Overtime)) &&
              assertTrue(said.contains("Переработки наше всё")) &&
              assertTrue(hero.effectiveMaxHp(0L) == before + 100L)
    },

    test("никто не в отлучке — строк о возвращении нет") {
      for {
        t <- makeState(baseHero)
        (state, _, _, renderer) = t
        _    <- state.enter(testUser, renderer)
        list <- renderer.sentScreens.map(_.last)
      } yield assertTrue(!list.text.contains("вернётся через"))
    },

    test("строй полон — нанятый встаёт в резерв и об этом сказано; полон и резерв — нанять нельзя") {
      val orc  = pangea.model.squad.UndeadForm("Поднятый орк", 3L, AllyKind.Human.stats(3L))
      val line = (1 to pangea.model.squad.AllyRates.Positions - 1).foldLeft(Squad.empty)((s, _) => s.raise(orc, 10L, 0L))
      val full = (1 to pangea.model.squad.AllyRates.ReserveSize).foldLeft(line)((s, _) => s.raise(orc, 10L, 0L))
      for {
        t1 <- makeState(baseHero.copy(doubloons = 12L, squad = line))
        (s1, d1, _, r1) = t1
        _    <- s1.action(testUser, pick("MercHire", AllyKind.Human), r1)
        h1   <- d1.getHeroByUserId(userId).map(_.get)
        said <- r1.sentScreens.map(_.map(_.text).mkString("\n"))
        t2 <- makeState(baseHero.copy(doubloons = 12L, squad = full))
        (s2, d2, _, r2) = t2
        _    <- s2.action(testUser, pick("MercHire", AllyKind.Human), r2)
        h2   <- d2.getHeroByUserId(userId).map(_.get)
        no   <- r2.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(h1.squad.allies == line.allies && h1.squad.reserve.map(_.kind) == List(AllyKind.Human)) &&
              assertTrue(h1.doubloons == 2L && said.contains("В строю мест нет — Йорген Кремень ждёт в резерве.")) &&
              assertTrue(h2.squad == full && h2.doubloons == 12L) &&
              assertTrue(no.contains("Ни в строю, ни в резерве нет свободного места."))
    }
  )
}
