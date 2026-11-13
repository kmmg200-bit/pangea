package pangea.model.squad

import io.circe.syntax.EncoderOps
import zio.test._

/** Отряд: места в строю на героя и до десяти союзников, найм на первую
  * свободную, перестановки (с союзником — меняются, с героем — герой на
  * прежнюю), отлучка по свитку на сутки и поднятые с алтаря. */
object SquadSpec extends ZIOSpecDefault {

  private val lvl = 10L
  private val now = 1_000L

  override def spec = suite("Squad")(

    test("статы союзника — ставка на уровень героя, но не выше своего потолка") {
      val h = AllyKind.Human.stats(lvl)    // потолок 5
      val g = AllyKind.Gnome.stats(lvl)    // потолок 7
      val m = AllyKind.Murloc.stats(lvl)   // потолок 10 — растёт до конца
      assertTrue(AllyKind.Human.maxLvl == 5L && AllyKind.Gnome.maxLvl == 7L && AllyKind.Murloc.maxLvl == 10L) &&
      assertTrue(h.hp == 625L && h.armor == 750L && h.atk == 100L && h.energy == 500L) &&
      assertTrue(h.accuracy == 500L && h.defence == 200L && h.evasion == 500L) &&
      assertTrue(g.hp == 560L && g.armor == 1225L && g.defence == 350L && g.evasion == 420L) &&
      assertTrue(m.hp == 1000L && m.armor == 1000L && m.atk == 300L) &&
      // выше потолка сильнее не становятся, ниже — растут как раньше
      assertTrue(AllyKind.Human.stats(50L) == h && AllyKind.Human.stats(3L).hp == 375L) &&
      assertTrue(AllyKind.Human.energyRegen(lvl) == 35L && AllyKind.Murloc.energyRegen(lvl) == 70L) &&
      assertTrue(AllyKind.brewsFor(1L) == 1L && AllyKind.brewsFor(4L) == 1L && AllyKind.brewsFor(5L) == 1L &&
                 AllyKind.brewsFor(10L) == 2L && AllyKind.brewsFor(23L) == 4L) &&
      assertTrue(AllyKind.Human.element.contains(pangea.model.battle.Element.Fire) &&
                 AllyKind.Murloc.element.contains(pangea.model.battle.Element.Lightning) &&
                 AllyKind.Gnome.element.contains(pangea.model.battle.Element.Cold)) &&
      // крыса бьёт зубами: стихии у неё нет, статы и имя — из своей формы
      assertTrue(AllyKind.Rat.element.isEmpty && !AllyKind.hireable.contains(AllyKind.Rat))
    },

    test("найм: на первую свободную позицию, здоровым; повторно — нет") {
      val s1 = Squad.empty.hire(AllyKind.Human, lvl, 0L)
      val s2 = s1.hire(AllyKind.Murloc, lvl, 0L)
      val s3 = s2.hire(AllyKind.Gnome, lvl, 0L)
      val s4 = s3.hire(AllyKind.Human, lvl, 0L)
      assertTrue(s1.allyAt(2).exists(a => a.kind == AllyKind.Human && a.hp == 625L && a.armor == 750L && a.energy == 500L)) &&
      assertTrue(s2.allyAt(3).exists(_.kind == AllyKind.Murloc) && s3.allyAt(4).exists(_.kind == AllyKind.Gnome)) &&
      assertTrue(s4 == s3) &&
      assertTrue(s3.inOrder.map(_.position) == List(2, 3, 4))
    },

    test("в строю десять мест для союзников: одиннадцатому в строю места нет — он встаёт в резерв") {
      val full = (1 to AllyRates.Positions - 1).foldLeft(Squad.empty) { (s, i) =>
        s.raise(UndeadForm(s"Поднятый $i", 1L, AllyKind.Human.stats(1L)), lvl, now)
      }
      val extra = full.raise(UndeadForm("Лишний", 1L, AllyKind.Human.stats(1L)), lvl, now)
      assertTrue(full.allies.size == 10 && full.full && full.freePosition.isEmpty) &&
      // строй как был, а одиннадцатый — в резерве
      assertTrue(extra.allies == full.allies && extra.reserve.map(_.name) == List("Лишний")) &&
      // заменой место в строю освобождают по-прежнему
      assertTrue(full.replaceAt(3, UndeadForm("Лишний", 1L, AllyKind.Human.stats(1L)), lvl, now)
                   .allyAt(3).exists(_.name == "Лишний"))
    },

    test("поднятый живёт своими статами и убирается по месту") {
      val form = UndeadForm("Гоблин немощный раб", 16L, AllyKind.Human.stats(3L))
      val s    = Squad.empty.hire(AllyKind.Human, lvl, 1000L).raise(form, lvl, 1000L)
      val up   = s.allyAt(3).get
      assertTrue(up.name == "Гоблин немощный раб" && up.statsAt(lvl) == AllyKind.Human.stats(3L)) &&
      assertTrue(up.lvlAt(lvl) == 16L) &&
      // «отработал найм» — это не про него: он рассыпается, а не уходит
      assertTrue(!up.expired(Long.MaxValue)) &&
      // наёмник рядом с ним свой срок всё так же отрабатывает
      assertTrue(s.expire(1000L + AllyRates.HireMs)._2.map(_.kind) == List(AllyKind.Human)) &&
      assertTrue(s.dismissAt(3).allies.map(_.kind) == List(AllyKind.Human)) &&
      assertTrue(s.dismissAll(Set(2, 3)).allies.isEmpty)
    },

    test("поднятого держит тёмная сила ровно сутки, и ждать его потом неоткуда") {
      val form = UndeadForm("Гоблин немощный раб", 16L, AllyKind.Human.stats(3L))
      val s    = Squad.empty.raise(form, lvl, 1000L)
      val up   = s.allyAt(2).get
      val (before, still) = s.expire(1000L + AllyRates.UndeadMs - 1L)
      val (after,  dust)  = s.expire(1000L + AllyRates.UndeadMs)
      assertTrue(up.hiredUntil == 1000L + AllyRates.UndeadMs && AllyRates.UndeadMs == 24L * 60L * 60L * 1000L) &&
      assertTrue(still.isEmpty && before == s) &&
      assertTrue(dust.map(_.name) == List("Гоблин немощный раб") && after.allies.isEmpty) &&
      // в отлучку он не садится: возвращаться ему неоткуда
      assertTrue(after.offDuty.isEmpty && after.away.isEmpty)
    },

    test("поднятый до того, как завёлся срок, получает его с этой минуты, а не рассыпается") {
      val form  = UndeadForm("Старый скелет", 5L, AllyKind.Human.stats(3L))
      val old   = Squad.empty.raise(form, lvl, 1000L).updateAt(2)(_.copy(hiredUntil = 0L))
      val dated = old.settleUndead(5000L)
      assertTrue(old.expire(Long.MaxValue - 1L)._2.isEmpty) &&
      assertTrue(dated.allyAt(2).exists(_.hiredUntil == 5000L + AllyRates.UndeadMs)) &&
      // уже посчитанному срок второй раз не переставляют
      assertTrue(dated.settleUndead(9000L) == dated)
    },

    test("герой на позиции 3: наём обходит его место") {
      val s = Squad(heroPos = 3).hire(AllyKind.Human, lvl, 0L).hire(AllyKind.Murloc, lvl, 0L).hire(AllyKind.Gnome, lvl, 0L)
      assertTrue(s.inOrder.map(a => a.kind -> a.position) == List(AllyKind.Human -> 1, AllyKind.Murloc -> 2, AllyKind.Gnome -> 4))
    },

    test("перестановка: на пустое — просто; на союзника — меняются; на героя — герой встаёт на прежнюю") {
      val s = Squad.empty.hire(AllyKind.Human, lvl, 0L).hire(AllyKind.Murloc, lvl, 0L) // герой 1, Йорген 2, Плюх 3
      val toFree = s.move(AllyKind.Human, 4)
      val swap   = s.move(AllyKind.Human, 3)
      val toHero = s.move(AllyKind.Murloc, 1)
      assertTrue(toFree.allyAt(4).exists(_.kind == AllyKind.Human) && toFree.allyAt(2).isEmpty) &&
      assertTrue(swap.allyAt(3).exists(_.kind == AllyKind.Human) && swap.allyAt(2).exists(_.kind == AllyKind.Murloc)) &&
      assertTrue(toHero.heroPos == 3 && toHero.allyAt(1).exists(_.kind == AllyKind.Murloc)) &&
      assertTrue(s.move(AllyKind.Human, 2) == s && s.move(AllyKind.Human, 99) == s && s.move(AllyKind.Gnome, 4) == s)
    },

    test("герой шагает на позицию союзника — тот встаёт на его прежнюю") {
      val s = Squad.empty.hire(AllyKind.Human, lvl, 0L)
      val m = s.moveHero(2)
      assertTrue(m.heroPos == 2 && m.allyAt(1).exists(_.kind == AllyKind.Human)) &&
      assertTrue(s.moveHero(1) == s && s.moveHero(99) == s)
    },

    test("свиток: из отряда вон, сутки в отлучке, потом возвращается — и встречается один раз") {
      val now = 1_000_000L
      val s   = Squad.empty.hire(AllyKind.Human, lvl, 0L).sentAway(AllyKind.Human, now)
      assertTrue(!s.has(AllyKind.Human) && s.isAway(AllyKind.Human, now) && s.isAway(AllyKind.Human, now + AllyRates.AwayMs - 1L)) &&
      assertTrue(!s.isAway(AllyKind.Human, now + AllyRates.AwayMs)) &&
      assertTrue(s.returned(now).isEmpty && s.returned(now + AllyRates.AwayMs) == List(AllyKind.Human)) &&
      assertTrue(s.welcomeBack(AllyKind.Human).returned(now + AllyRates.AwayMs).isEmpty)
    },

    test("отдых восстанавливает всех; текущее не выше потолков") {
      val s = Squad.empty.hire(AllyKind.Human, lvl, 0L).update(AllyKind.Human)(_.copy(hp = 1L, armor = 0L, energy = 5L))
      val r = s.restored(lvl)
      val c = Ally(AllyKind.Human, 2, 99999L, -5L, 99999L).clamped(lvl)
      assertTrue(r.allyAt(2).exists(a => a.hp == 625L && a.armor == 750L && a.energy == 500L)) &&
      assertTrue(c.hp == 625L && c.armor == 0L && c.energy == 500L)
    },

    test("найм на 12 часов: отработавший уходит из отряда и садится за стол через 12 часов; реплики о свитке ему не положено") {
      val s = Squad.empty.hire(AllyKind.Human, lvl, 1000L)
      val (same, none) = s.expire(1000L + AllyRates.HireMs - 1L)
      val (gone, who)  = s.expire(1000L + AllyRates.HireMs)
      val back = 1000L + AllyRates.HireMs + AllyRates.OffDutyMs
      assertTrue(s.allyAt(2).exists(_.hiredUntil == 1000L + AllyRates.HireMs)) &&
      assertTrue(same == s && none.isEmpty) &&
      assertTrue(who.map(_.kind) == List(AllyKind.Human) && !gone.has(AllyKind.Human)) &&
      assertTrue(gone.isAway(AllyKind.Human, back - 1L) && !gone.isAway(AllyKind.Human, back)) &&
      assertTrue(gone.returned(back).isEmpty && gone.welcomeBack(AllyKind.Human).offDuty.isEmpty) &&
      assertTrue(AllyRates.HireMs == 12L * 60L * 60L * 1000L && AllyRates.OffDutyMs == AllyRates.HireMs)
    },

    test("ушёл союзник — его место просто пустеет: остальные и герой стоят где стояли") {
      val three = Squad(heroPos = 2, allies = List(Ally(AllyKind.Human, 1, 1L, 1L, 1L, 9L), Ally(AllyKind.Gnome, 4, 1L, 1L, 1L, 9L)))
      val alone = three.dismiss(AllyKind.Human).dismiss(AllyKind.Gnome)
      val one   = three.dismiss(AllyKind.Human)
      val (expired, gone) = three.copy(allies = three.allies.map(_.copy(hiredUntil = 0L))).expire(1L)
      val away  = three.sentAway(AllyKind.Human, 0L)
      val byPos = three.dismissAll(Set(1))
      assertTrue(alone.heroPos == 2 && alone.isEmpty) &&
      assertTrue(one.heroPos == 2 && one.allyAt(1).isEmpty && one.allyAt(4).exists(_.kind == AllyKind.Gnome)) &&
      assertTrue(gone.size == 2 && expired.heroPos == 2 && expired.isEmpty) &&
      assertTrue(away.heroPos == 2 && away.allyAt(4).exists(_.kind == AllyKind.Gnome)) &&
      assertTrue(byPos.heroPos == 2 && byPos.allyAt(4).exists(_.kind == AllyKind.Gnome))
    },

    test("в бою строй съезжает к первому месту, промежутки остаются: наёмник на 6, герой на 10 — сдвиг на 5") {
      val gap = Squad(heroPos = 10, allies = List(Ally(AllyKind.Human, 6, 1L, 1L, 1L, 9L)))
      assertTrue(gap.frontShift == 5) &&
      assertTrue(Squad(heroPos = 4).frontShift == 3) &&
      assertTrue(Squad(heroPos = 3, allies = List(Ally(AllyKind.Human, 1, 1L, 1L, 1L, 9L))).frontShift == 0)
    },

    test("двое на одном месте при чтении расходятся: лишний — на первое свободное, остальные где стояли") {
      // герой и Медоус оба на 1 — так оставлял сюжетный бой без отряда
      val merged = Squad(heroPos = 1, allies = List(Ally(AllyKind.Human, 1, 1L, 1L, 1L, 9L), Ally(AllyKind.Gnome, 3, 1L, 1L, 1L, 9L)))
      val healed = merged.asJson.as[Squad].toOption
      // два союзника на одном месте
      val twins  = Squad(heroPos = 4, allies = List(Ally(AllyKind.Human, 2, 1L, 1L, 1L, 9L), Ally(AllyKind.Gnome, 2, 1L, 1L, 1L, 9L)))
      val fine   = Squad(heroPos = 2, allies = List(Ally(AllyKind.Human, 1, 1L, 1L, 1L, 9L)))
      assertTrue(healed.exists(s => s.heroPos == 1 && s.allyAt(2).exists(_.kind == AllyKind.Human) &&
                                    s.allyAt(3).exists(_.kind == AllyKind.Gnome))) &&
      assertTrue(twins.repaired.allyAt(2).exists(_.kind == AllyKind.Human) && twins.repaired.allyAt(1).exists(_.kind == AllyKind.Gnome)) &&
      assertTrue(fine.repaired == fine)
    },

    test("Тёмный волк в отряде один: новый встаёт на место прежнего, и свободного места для этого не нужно") {
      val old   = UndeadForm(UndeadForm.DarkWolfName, 5L, AllyKind.Human.stats(5L))
      val fresh = UndeadForm(UndeadForm.DarkWolfName, 9L, AllyKind.Human.stats(9L))
      val orc   = UndeadForm("Поднятый орк", 3L, AllyKind.Human.stats(3L))
      // наёмник на месте 2, прежний волк — на 3
      val s1    = Squad.empty.hire(AllyKind.Human, lvl, 0L).raise(old, lvl, now)
      val s2    = s1.raise(fresh, lvl, now + 1L)
      // полный отряд: новый волк всё равно встаёт — на место прежнего
      val full  = (1 to AllyRates.Positions - 3).foldLeft(s1)((s, _) => s.raise(orc, lvl, now))
      val s3    = full.raise(fresh, lvl, now)
      // рядовой поднятый волку не соперник
      val s4    = s1.raise(orc, lvl, now)
      assertTrue(s2.darkWolves.map(w => w.position -> w.lvlAt(lvl)) == List(3 -> 9L)) &&
      assertTrue(s2.allies.size == 2 && s2.allyAt(2).exists(_.kind == AllyKind.Human)) &&
      // срок — с той минуты, когда поднялся новый
      assertTrue(s2.allyAt(3).exists(_.hiredUntil == now + 1L + AllyRates.UndeadMs)) &&
      assertTrue(full.full && s3.darkWolves.map(_.lvlAt(lvl)) == List(9L) && s3.allies.size == full.allies.size) &&
      assertTrue(s4.darkWolves.size == 1 && s4.allies.size == 3)
    },

    test("новый волк одолевает всех прежних; восставший волк не ждёт у шатра, а ждавший встаёт на место волка, не названного") {
      def wolf(l: Long) = UndeadForm(UndeadForm.DarkWolfName, l, AllyKind.Human.stats(1L))
      val orc = UndeadForm("Поднятый орк", 3L, AllyKind.Human.stats(3L))
      // двое волков — с тех пор, когда предела не было
      val legacy = Squad.empty.copy(allies = List(
        Ally(AllyKind.Undead, 2, 1L, 0L, 0L, now + 5L, Some(wolf(4L))),
        Ally(AllyKind.Undead, 5, 1L, 0L, 0L, now + 5L, Some(wolf(6L)))))
      val beaten = legacy.raise(wolf(8L), lvl, now)
      // полный отряд, волк на месте 2
      val full     = (1 to AllyRates.Positions - 2).foldLeft(Squad.empty.raise(wolf(4L), lvl, now))((s, _) => s.raise(orc, lvl, now))
      val admitted = full.admitRisen(wolf(8L), lvl, now)
      // волк ждал у шатра, а свой встал, пока он ждал: названный (на месте 5) остаётся
      val took     = full.copy(pendingRisen = Some(wolf(8L))).takeRisenPlace(5, lvl, now)
      assertTrue(beaten.darkWolves.map(w => w.position -> w.lvlAt(lvl)) == List(2 -> 8L) && beaten.allies.size == 1) &&
      assertTrue(full.full && admitted.pendingRisen.isEmpty) &&
      assertTrue(admitted.darkWolves.map(w => w.position -> w.lvlAt(lvl)) == List(2 -> 8L)) &&
      assertTrue(admitted.allies.size == full.allies.size) &&
      assertTrue(took.pendingRisen.isEmpty && took.darkWolves.map(w => w.position -> w.lvlAt(lvl)) == List(2 -> 8L)) &&
      assertTrue(took.allyAt(5).exists(_.name == "Поднятый орк") && took.allies.size == full.allies.size)
    },

    test("отряд переживает сериализацию, а пустая запись читается как пустой отряд") {
      val s    = Squad(heroPos = 2, allies = List(Ally(AllyKind.Gnome, 1, 10L, 20L, 30L)), away = Map("Human" -> 5L))
      val back = s.asJson.as[Squad].toOption
      val old  = io.circe.Json.obj().as[Squad].toOption
      assertTrue(back.contains(s)) && assertTrue(old.contains(Squad.empty))
    },

    test("резерв переживает сериализацию, а запись до резерва читается с пустым") {
      val s = Squad(allies = List(Ally(AllyKind.Gnome, 2, 10L, 20L, 30L)),
        reserve = List(Ally(AllyKind.Murloc, 0, 5L, 6L, 7L, hiredUntil = 99L)))
      val back = s.asJson.as[Squad].toOption
      // так лежит отряд в базе до резерва
      val old = io.circe.parser.parse(
        """{"heroPos":1,"allies":[{"kind":"Gnome","position":2,"hp":10,"armor":20,"energy":30,"hiredUntil":5}],
          |"away":{},"offDuty":{},"overtime":{},"overtimeTotal":0}""".stripMargin).flatMap(_.as[Squad]).toOption
      assertTrue(back.contains(s)) &&
      assertTrue(old.exists(o => o.reserve.isEmpty && o.allies.map(_.kind) == List(AllyKind.Gnome)))
    },

    test("резерв: лишний поднятый и наёмник встают туда, когда в строю нет мест; нет мест и там — никуда") {
      val orc  = UndeadForm("Поднятый орк", 3L, AllyKind.Human.stats(3L))
      // строй полон: герой на 1, поднятые на 2–11
      val full = (1 to AllyRates.Positions - 1).foldLeft(Squad.empty)((s, _) => s.raise(orc, lvl, now))
      val (r1, p1) = full.raised(orc, lvl, now)
      val (h1, q1) = full.hired(AllyKind.Murloc, lvl, now)
      // полон и резерв
      val packed = (1 to AllyRates.ReserveSize).foldLeft(full)((s, _) => s.raise(orc, lvl, now))
      val (r2, p2) = packed.raised(orc, lvl, now)
      val (h2, q2) = packed.hired(AllyKind.Murloc, lvl, now)
      assertTrue(AllyRates.ReserveSize == 10) &&
      assertTrue(full.full && p1 == Placed.Reserve && r1.reserve.map(_.name) == List("Поднятый орк")) &&
      assertTrue(r1.reserve.forall(_.position == 0) && r1.allies == full.allies) &&
      assertTrue(q1 == Placed.Reserve && h1.reserve.exists(_.kind == AllyKind.Murloc)) &&
      // наёмник в резерве тоже «есть»: второго такого не нанять
      assertTrue(h1.has(AllyKind.Murloc) && h1.hired(AllyKind.Murloc, lvl, now)._2 == Placed.Nowhere) &&
      assertTrue(packed.reserveFull && !packed.canTake && p2 == Placed.Nowhere && r2 == packed) &&
      assertTrue(q2 == Placed.Nowhere && h2 == packed) &&
      // восставший из боя: есть место в резерве — встаёт туда, нет — ждёт у шатра
      assertTrue(full.admitRisen(orc, lvl, now).reserve.size == 1 && full.admitRisen(orc, lvl, now).pendingRisen.isEmpty) &&
      assertTrue(packed.admitRisen(orc, lvl, now).pendingRisen.contains(orc))
    },

    test("в резерв можно убрать весь строй — десять союзников; полный резерв больше не берёт") {
      def risen(name: String) = UndeadForm(name, 1L, AllyKind.Human.stats(1L))
      val full   = (1 to AllyRates.Positions - 1).foldLeft(Squad.empty)((s, i) => s.raise(risen(s"Поднятый $i"), lvl, now))
      val hidden = full.inOrder.map(_.position).foldLeft(full)((s, p) => s.toReserve(p))
      // строй пуст, и в него снова встаёт новый — а в резерв его уже не убрать
      val more   = hidden.raise(risen("Ещё один"), lvl, now)
      val pos    = more.inOrder.headOption.map(_.position).getOrElse(0)
      assertTrue(AllyRates.ReserveSize == AllyRates.Positions - 1) &&
      assertTrue(full.allies.size == 10 && hidden.allies.isEmpty && hidden.reserve.size == 10 && hidden.reserveFull) &&
      assertTrue(hidden.reserve.map(_.name).toSet == full.allies.map(_.name).toSet) &&
      assertTrue(more.allies.map(_.name) == List("Ещё один") && more.toReserve(pos) == more)
    },

    test("резерв руками: туда и обратно, обмен со строем, выгнать; крысу туда не берут") {
      val orc     = UndeadForm("Поднятый орк", 3L, AllyKind.Human.stats(3L))
      val rat     = UndeadForm("Крыса", 1L, AllyKind.Human.stats(1L))
      // Йорген на 2, орк на 3
      val s0      = Squad.empty.hire(AllyKind.Human, lvl, 0L).raise(orc, lvl, now)
      val s1      = s0.toReserve(3)
      val back    = s1.deploy(0)
      // строй полон: выставить можно только обменом
      val full    = (1 to AllyRates.Positions - 2).foldLeft(s1)((s, _) => s.raise(orc.copy(name = "Другой"), lvl, now))
      val swapped = full.swapIn(0, 2)
      val withRat = Squad.empty.summonRat(rat, lvl, now, near = 2)
      assertTrue(s1.allyAt(3).isEmpty && s1.reserve.map(_.name) == List("Поднятый орк")) &&
      assertTrue(s1.reserve.forall(_.position == 0)) &&
      assertTrue(back.allyAt(3).exists(_.name == "Поднятый орк") && back.reserve.isEmpty) &&
      assertTrue(full.full && full.deploy(0) == full) &&
      // орк из резерва — на место Йоргена, Йорген — в резерв
      assertTrue(swapped.allyAt(2).exists(_.name == "Поднятый орк") && swapped.reserve.map(_.kind) == List(AllyKind.Human)) &&
      assertTrue(swapped.reserve.forall(_.position == 0) && swapped.allies.size == full.allies.size) &&
      assertTrue(s1.dismissReserve(0).reserve.isEmpty && s1.dismissReserve(5) == s1) &&
      assertTrue(withRat.toReserve(2) == withRat && withRat.allyAt(2).exists(a => !withRat.reservable(a)))
    },

    test("в резерве идут сроки и лечит отдых; Тёмный волк один на строй и резерв") {
      def wolf(l: Long) = UndeadForm(UndeadForm.DarkWolfName, l, AllyKind.Human.stats(1L))
      val orc  = UndeadForm("Поднятый орк", 3L, AllyKind.Human.stats(3L))
      // Плюх на 2, орк на 3 — оба в резерв
      val s1   = Squad.empty.hire(AllyKind.Murloc, lvl, now).raise(orc, lvl, now).toReserve(2).toReserve(3)
      val hurt = s1.copy(reserve = s1.reserve.map(_.copy(hp = 1L)))
      // через 12 часов наёмник отработал своё, поднятому держаться ещё сутки
      val (after, leaving) = s1.expire(now + AllyRates.HireMs)
      // волк ждёт в резерве — новый одолевает его и встаёт в строй
      val newWolf = Squad.empty.raise(wolf(4L), lvl, now).toReserve(2).raise(wolf(9L), lvl, now)
      assertTrue(s1.allies.isEmpty && s1.reserve.size == 2 && s1.nonEmpty && s1.has(AllyKind.Murloc)) &&
      assertTrue(hurt.restored(lvl).reserve.forall(a => a.hp == a.statsAt(lvl).hp)) &&
      assertTrue(leaving.map(_.kind) == List(AllyKind.Murloc) && after.reserve.map(_.name) == List("Поднятый орк")) &&
      assertTrue(after.offDuty.contains(AllyKind.Murloc.entryName)) &&
      assertTrue(newWolf.darkWolves.map(w => w.position -> w.lvlAt(lvl)) == List(2 -> 9L) && newWolf.reserve.isEmpty)
    },

    test("герой пал — резерв уходит весь: наёмник сядет за стол через сутки, без реплики о свитке, поднятый — насовсем; строй цел") {
      val orc  = UndeadForm("Поднятый орк", 3L, AllyKind.Human.stats(3L))
      // Йорген на 2 остаётся в строю, Плюх (3) и орк (4) — в резерве
      val line = Squad.empty.hire(AllyKind.Human, lvl, now)
      val s    = line.hire(AllyKind.Murloc, lvl, now).raise(orc, lvl, now).toReserve(3).toReserve(4)
      val (left, gone) = s.reserveLeaves(now)
      val back = now + AllyRates.AwayMs
      assertTrue(s.reserve.size == 2 && gone == s.reserve && gone.map(_.name) == List("Плюх", "Поднятый орк")) &&
      assertTrue(left.reserve.isEmpty && left.allies == s.allies && left.heroPos == s.heroPos) &&
      assertTrue(!left.has(AllyKind.Murloc) && left.isAway(AllyKind.Murloc, back - 1L) && !left.isAway(AllyKind.Murloc, back)) &&
      // поднятый за стол не садится, а свитка не было — встречать некого
      assertTrue(left.offDuty.keySet == Set(AllyKind.Murloc.entryName) && left.away.isEmpty && left.returned(back).isEmpty) &&
      // пуст резерв — отряд как был
      assertTrue(line.reserveLeaves(now) == (line, Nil))
    }
  )
}
