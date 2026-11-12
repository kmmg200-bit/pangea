package pangea.model.squad

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.stats.FightStats

/** Своя форма союзника: имя, уровень и потолки статов прямо на нём, а не от
 *  уровня героя. Так живут те, кого не наняли, а добыли: поднятый с алтаря
 *  (имя остаётся от того, кем он был при жизни, см. [[AllyKind.Undead]]) и
 *  крыса, выскочившая из-под ног на набор «Крыса» ([[AllyKind.Rat]]) — она тот
 *  же моб, что водится в канализации. Имя типа осталось от первого из них. */
final case class UndeadForm(name: String, lvl: Long, stats: FightStats) {

  /** Уникальный поднятый — Тёмный волк с алтаря: тот самый Белый волк, а не
    * рядовой моб, поэтому умений наёмников (фляга, починка, удары) у него нет —
    * только своя атака. Узнаём по имени: оно же лежит и в уже поднятых. */
  def unique: Boolean = name == UndeadForm.DarkWolfName
}

object UndeadForm {

  /** Имя того, что встаёт с алтаря по клыку Белого волка. */
  val DarkWolfName: String = "Тёмный волк"

  implicit val encoder: Encoder[UndeadForm] = (u: UndeadForm) =>
    Json.obj("name" -> u.name.asJson, "lvl" -> u.lvl.asJson, "stats" -> u.stats.asJson)

  implicit val decoder: Decoder[UndeadForm] = (c: HCursor) =>
    for {
      name  <- c.get[String]("name")
      lvl   <- c.getOrElse[Long]("lvl")(1L)
      stats <- c.get[FightStats]("stats")
    } yield UndeadForm(name, lvl, stats)
}

/** Союзник в отряде: кто, на какой позиции, что с ним сейчас и до какого
 *  момента он при герое (`hiredUntil`, epoch ms). Потолки считаются от уровня
 *  героя ([[AllyKind.stats]]), здесь — только текущее. У добытого союзника —
 *  поднятого с алтаря и крысы — вместо этого своя форма ([[UndeadForm]]): его
 *  статы от уровня героя не зависят, а срок — не найм, а то, насколько хватит
 *  тёмной силы ([[AllyRates.UndeadMs]]) или терпения крысы ([[AllyRates.RatMs]]). */
final case class Ally(
  kind:       AllyKind,
  position:   Int,
  hp:         Long,
  armor:      Long,
  energy:     Long,
  hiredUntil: Long               = 0L,
  undead:     Option[UndeadForm] = None,
  /** На сколько уровней вырос его потолок: герой выполнил его личное задание
    * (см. `MercQuests`, [[AllyRates.QuestGrowth]]). Ставится при найме — и тому,
    * кто уже в отряде, когда задание закрыто. В старых записях поля нет — 0. */
  grown:      Long               = 0L
) {
  def name: String = undead.map(_.name).getOrElse(kind.name)

  /** Тёмный волк с алтаря или из боя с «Некромантом» — в отряде он один. */
  def isDarkWolf: Boolean = undead.exists(_.unique)

  /** Потолки статов: у наёмника — по уровню героя, у поднятого — свои. */
  def statsAt(lvl: Long): FightStats = undead.map(_.stats).getOrElse(kind.stats(lvl, grown))

  /** Уровень, по которому он дерётся. */
  def lvlAt(heroLvl: Long): Long = undead.map(_.lvl).getOrElse(kind.effectiveLvl(heroLvl, grown))

  /** Найм истёк — отработал свой день. Добытый союзник не уходит так: у него
    * свой срок, см. [[crumbled]]. */
  def expired(nowMs: Long): Boolean = undead.isEmpty && hiredUntil <= nowMs

  /** Срок добытого союзника вышел: у поднятого кончилась тёмная сила в костях,
    * у крысы — терпение. Поднятый без срока — из тех, кого подняли до того, как
    * срок вообще завели: такому его ставит
    * [[Squad.settleUndead]], а не рассыпает на месте. */
  def crumbled(nowMs: Long): Boolean = undead.isDefined && hiredUntil > 0L && hiredUntil <= nowMs

  /** Уходит из отряда прямо сейчас — по любой из двух причин. */
  def leaving(nowMs: Long): Boolean = expired(nowMs) || crumbled(nowMs)

  /** Полностью здоров на этом уровне героя. */
  def restored(lvl: Long): Ally = {
    val s = statsAt(lvl)
    copy(hp = s.hp, armor = s.armor, energy = s.energy)
  }

  /** Текущее не выше потолков (герой мог понизиться — не бывает, но и выше не держим). */
  def clamped(lvl: Long): Ally = {
    val s = statsAt(lvl)
    copy(hp = hp.min(s.hp).max(0L), armor = armor.min(s.armor).max(0L), energy = energy.min(s.energy).max(0L))
  }
}

object Ally {
  implicit val encoder: Encoder[Ally] = deriveEncoder
  implicit val decoder: Decoder[Ally] = (c: HCursor) =>
    for {
      kind     <- c.get[AllyKind]("kind")
      position <- c.get[Int]("position")
      hp       <- c.getOrElse[Long]("hp")(0L)
      armor    <- c.getOrElse[Long]("armor")(0L)
      energy   <- c.getOrElse[Long]("energy")(0L)
      until    <- c.getOrElse[Long]("hiredUntil")(0L)
      undead   <- c.getOrElse[Option[UndeadForm]]("undead")(None)
      grown    <- c.getOrElse[Long]("grown")(0L)
    } yield Ally(kind, position, hp, armor, energy, until, undead, grown)
}

/** Отряд героя: места в строю (см. [[AllyRates.Positions]]) делят герой
 *  (`heroPos`) и союзники — до десяти; расставляет их игрок, и встать можно на
 *  любое место. В бой строй выходит, съехав к первому месту (см. [[frontShift]]):
 *  союзник на месте N стоит против врага на месте N и достаёт соседние места.
 *  Ушёл союзник — его место просто пустеет. `away` — кто ушёл по свитку и
 *  когда вернётся в таверну (ключ — вид); `offDuty` — кто отработал свой найм
 *  или ушёл из резерва, когда герой пал (см. [[reserveLeaves]]), и когда снова
 *  сядет за стол. Хранится в `heroes.squad_data`. */
final case class Squad(
  heroPos: Int              = 1,
  allies:  List[Ally]       = Nil,
  away:    Map[String, Long] = Map.empty,
  offDuty: Map[String, Long] = Map.empty,
  /** Восставший, которому не нашлось места в строю («Некромант», порог 12): он
    * ждёт у шатра, пока герой не решит, кем за него пожертвовать. Экран выбора
    * — в «Отряде», как у алтаря в пещере. */
  pendingRisen: Option[UndeadForm] = None,
  /** Сколько раз наёмника подняли из-за стола за серебро, не дав отдохнуть
    * (ключ — вид). Каждый следующий раз дороже; дали отдохнуть — счёт с нуля. */
  overtime: Map[String, Int] = Map.empty,
  /** Сколько всего раз герой поднимал наёмников за серебро — для «Переработки
    * наше всё». */
  overtimeTotal: Int = 0,
  /** Резерв: кто при герое, но не в строю, — до [[AllyRates.ReserveSize]].
    * Сюда встают лишние поднятые и наёмники, когда в строю нет мест, и те, кого
    * герой отправил сам. В бой резерв не идёт, а сроки найма и тёмной силы у
    * него идут как в строю. Места в строю у них нет (`position` — 0). */
  reserve: List[Ally] = Nil
) {
  /** Пуст ли отряд целиком — и строй, и резерв. */
  def isEmpty: Boolean  = allies.isEmpty && reserve.isEmpty
  def nonEmpty: Boolean = !isEmpty

  /** Есть ли такой при герое — в строю или в резерве: Брамбл и Плюх помогают и
    * из резерва, а нанять второго такого же нельзя. */
  def has(kind: AllyKind): Boolean = (allies ++ reserve).exists(_.kind == kind)

  /** В резерве больше нет мест. */
  def reserveFull: Boolean = reserve.sizeIs >= AllyRates.ReserveSize

  /** Найдётся ли новенькому место — в строю или в резерве. */
  def canTake: Boolean = freePosition.isDefined || !reserveFull

  def allyAt(pos: Int): Option[Ally] = allies.find(_.position == pos)

  /** В отлучке — по свитку или после отработанного найма. */
  def isAway(kind: AllyKind, nowMs: Long): Boolean =
    away.get(kind.entryName).exists(_ > nowMs) || offDuty.get(kind.entryName).exists(_ > nowMs)

  /** Когда он вернётся в таверну, если сейчас в отлучке: по свитку или после
    * отработанного найма — что позже. */
  def backAt(kind: AllyKind, nowMs: Long): Option[Long] =
    List(away.get(kind.entryName), offDuty.get(kind.entryName)).flatten.filter(_ > nowMs).maxOption

  /** Кто вернулся из отлучки по свитку к этому моменту — им нужна реплика. */
  def returned(nowMs: Long): List[AllyKind] =
    away.collect { case (k, until) if until <= nowMs => AllyKind.withNameOption(k) }.flatten.toList

  /** Сколько раз его уже поднимали без отдыха. */
  def overtimeOf(kind: AllyKind): Int = overtime.getOrElse(kind.entryName, 0)

  /** Поднят из-за стола за серебро, не отдохнув: в отряд — и счёт переработок растёт. */
  def rushed(kind: AllyKind, lvl: Long, nowMs: Long, grown: Long = 0L): Squad =
    welcomeBack(kind).hire(kind, lvl, nowMs, grown)
      .copy(overtime = overtime.updated(kind.entryName, overtimeOf(kind) + 1), overtimeTotal = overtimeTotal + 1)

  /** Отдохнул как положено — переработки забыты. */
  def rested(kind: AllyKind): Squad = copy(overtime = overtime - kind.entryName)

  /** Стереть записи об отлучке (реплика показана / срок вышел). */
  def welcomeBack(kind: AllyKind): Squad = copy(away = away - kind.entryName, offDuty = offDuty - kind.entryName)

  /** Первая свободная позиция, если есть. */
  def freePosition: Option[Int] =
    (1 to AllyRates.Positions).find(p => p != heroPos && allyAt(p).isEmpty)

  /** Нанять на [[AllyRates.HireMs]], полностью здоровым (см. [[hired]]). */
  def hire(kind: AllyKind, lvl: Long, nowMs: Long, grown: Long = 0L): Squad = hired(kind, lvl, nowMs, grown)._1

  /** Нанять: на первую свободную позицию строя, а мест нет — в резерв. Такой
    * уже есть (в строю или в резерве) или мест нет нигде — отряд как был
    * (`Placed.Nowhere`). `grown` — насколько вырос его потолок у этого героя
    * (см. [[Ally.grown]]). */
  def hired(kind: AllyKind, lvl: Long, nowMs: Long, grown: Long = 0L): (Squad, Placed) =
    if (has(kind)) (this, Placed.Nowhere)
    else place(Ally(kind, 0, 0L, 0L, 0L, hiredUntil = nowMs + AllyRates.HireMs, grown = grown).restored(lvl))

  /** Личное задание наёмника выполнено, а он уже при герое — в строю или в
    * резерве: потолок растёт и у него, с этой минуты. Сил у него столько же,
    * сколько было: вырос предел, а не раны. */
  def grow(kind: AllyKind, grown: Long): Squad = {
    def up(a: Ally): Ally = if (a.kind == kind && a.undead.isEmpty) a.copy(grown = grown) else a
    copy(allies = allies.map(up), reserve = reserve.map(up))
  }

  /** Новенький — на первое свободное место строя, мест нет — в резерв. */
  private def place(a: Ally): (Squad, Placed) =
    freePosition match {
      case Some(p)              => (copy(allies = allies :+ a.copy(position = p)), Placed.Line)
      case None if !reserveFull => (copy(reserve = reserve :+ a.copy(position = 0)), Placed.Reserve)
      case None                 => (this, Placed.Nowhere)
    }

  /** Кто покидает отряд к этому моменту — из строя и из резерва: наёмник
    * отработал свой найм и сядет за стол снова через [[AllyRates.OffDutyMs]], а
    * поднятый рассыпался — его ждать неоткуда. Возвращает отряд и ушедших
    * целиком: наверху по ним решают, что сказать игроку. */
  def expire(nowMs: Long): (Squad, List[Ally]) = {
    val gone = (allies ++ reserve).filter(_.leaving(nowMs))
    if (gone.isEmpty) (this, Nil)
    else {
      val hired = gone.filter(_.undead.isEmpty).map(_.kind)
      (copy(allies = allies.filterNot(_.leaving(nowMs)), reserve = reserve.filterNot(_.leaving(nowMs)),
            offDuty = offDuty ++ hired.map(k => k.entryName -> (nowMs + AllyRates.OffDutyMs))), gone)
    }
  }

  /** Герой пал в бою, куда брали отряд, — резерв уходит от него весь, разом.
    * Наёмник сядет за стол таверны снова только через сутки
    * ([[AllyRates.AwayMs]], как унесённый свитком), но реплики о свитке ему не
    * положено: свитка не было, поэтому он в `offDuty`, а не в `away`. Поднятый
    * рассыпается — ждать его неоткуда. Строй не трогаем. Возвращает отряд и
    * ушедших. */
  def reserveLeaves(nowMs: Long): (Squad, List[Ally]) =
    if (reserve.isEmpty) (this, Nil)
    else {
      val hired = reserve.filter(_.undead.isEmpty).map(_.kind)
      (copy(reserve = Nil, offDuty = offDuty ++ hired.map(k => k.entryName -> (nowMs + AllyRates.AwayMs))), reserve)
    }

  /** Поднятые до того, как у них завёлся срок, получают его с этой минуты:
    * иначе они рассыпались бы все разом при первом же заходе героя. */
  def settleUndead(nowMs: Long): Squad = {
    def undated(a: Ally): Boolean = a.undead.isDefined && a.hiredUntil <= 0L
    def dated(a: Ally): Ally = if (undated(a)) a.copy(hiredUntil = nowMs + AllyRates.UndeadMs) else a
    if (!(allies ++ reserve).exists(undated)) this
    else copy(allies = allies.map(dated), reserve = reserve.map(dated))
  }

  /** Убрать союзника этого вида — из строя и из резерва. */
  def dismiss(kind: AllyKind): Squad =
    copy(allies = allies.filterNot(_.kind == kind), reserve = reserve.filterNot(_.kind == kind))

  /** Убрать союзника с этой позиции. Позиция — единственный надёжный ключ:
    * поднятых с алтаря в отряде может быть несколько, и вид их не различает. */
  def dismissAt(pos: Int): Squad = dismissAll(Set(pos))

  /** Убрать сразу всех с этих позиций: их места пустеют, остальные стоят где
    * стояли. */
  def dismissAll(positions: Set[Int]): Squad =
    if (positions.isEmpty) this
    else copy(allies = allies.filterNot(a => positions.contains(a.position)))

  def updateAt(pos: Int)(f: Ally => Ally): Squad =
    copy(allies = allies.map(a => if (a.position == pos) f(a) else a))

  /** Поднятый встаёт в отряд (см. [[raised]]). */
  def raise(form: UndeadForm, lvl: Long, nowMs: Long, lasts: Long = 1L): Squad =
    raised(form, lvl, nowMs, lasts)._1

  /** Поднятый встаёт на свободное место строя, а мест нет — в резерв; нет и
    * там — отряд как был (`Placed.Nowhere`), заменой тогда заведует
    * [[replaceAt]]. Держится [[AllyRates.UndeadMs]] с этой минуты, а с
    * «Некромантом» (порог 6) вдвое дольше: `lasts` — этот множитель.
    *
    * Тёмный волк в отряде один — в строю и в резерве вместе ([[wolfRival]]):
    * если он уже есть, новый сцепляется с ним и побеждает. Прежний стоял в
    * строю — новый встаёт на его место; был в резерве — прежний рассыпается, а
    * новый встаёт как любой поднятый. Свободного места для этого не нужно. */
  def raised(form: UndeadForm, lvl: Long, nowMs: Long, lasts: Long = 1L): (Squad, Placed) =
    wolfRival(form) match {
      case Some(old) if old.position > 0 => (replaceAt(old.position, form, lvl, nowMs, lasts), Placed.Line)
      case Some(_)                       => withoutWolves.raised(form, lvl, nowMs, lasts)
      case None                          => place(risen(form, 0, lvl, nowMs, lasts))
    }

  /** Поднятый занимает место того, кто на нём стоял. Встаёт Тёмный волк — с
    * ним уходят и прежние волки, из строя и из резерва: второму не бывать. */
  def replaceAt(pos: Int, form: UndeadForm, lvl: Long, nowMs: Long, lasts: Long = 1L): Squad = {
    val beaten = if (form.unique) withoutWolves else this
    beaten.copy(allies = beaten.allies.filterNot(_.position == pos) :+ risen(form, pos, lvl, nowMs, lasts))
  }

  private def withoutWolves: Squad =
    copy(allies = allies.filterNot(_.isDarkWolf), reserve = reserve.filterNot(_.isDarkWolf))

  /** Тёмные волки отряда: сперва строй по местам, потом резерв. Больше одного
    * их не бывает: новый сцепляется с прежним и побеждает (см. [[raised]]).
    * Несколько бывает лишь у тех, кто поднял их до этого правила, — новый
    * одолеет всех разом. */
  def darkWolves: List[Ally] = inOrder.filter(_.isDarkWolf) ++ reserve.filter(_.isDarkWolf)

  /** С кем сцепится этот поднятый: Тёмный волк — с прежним волком отряда
    * (первым по строю, а нет в строю — из резерва), прочим делить нечего. */
  def wolfRival(form: UndeadForm): Option[Ally] =
    if (form.unique) darkWolves.headOption else None

  /** Крыса из-под ног встаёт на свободное место и держится [[AllyRates.RatMs]].
    * Мест нет — отряд как был: лишней крысе в строю стоять негде. */
  def summonRat(form: UndeadForm, lvl: Long, nowMs: Long): Squad =
    freePosition.fold(this)(p => copy(allies = allies :+
      Ally(AllyKind.Rat, p, 0L, 0L, 0L,
        hiredUntil = nowMs + AllyRates.RatMs, undead = Some(form)).restored(lvl)))

  /** Есть ли при герое хоть одна крыса: по ней порог 12 набора «Крыса» решает,
    * выдавать ли одну на входе в бой. */
  def hasRat: Boolean = allies.exists(_.kind == AllyKind.Rat)

  /** Восставший в бою уходит с героем на двое суток ([[AllyRates.RisenLasts]]):
    * в строй, а мест нет — в резерв (см. [[raised]]). Нет мест и в резерве —
    * ждёт у шатра ([[pendingRisen]]), и в «Отряде» герой решит, кем за него
    * пожертвовать. Ждущего второй раз не перебиваем: первый дождётся ответа. */
  def admitRisen(form: UndeadForm, lvl: Long, nowMs: Long): Squad =
    raised(form, lvl, nowMs, AllyRates.RisenLasts) match {
      case (_, Placed.Nowhere) => if (pendingRisen.isDefined) this else copy(pendingRisen = Some(form))
      case (placed, _)         => placed
    }

  /** Ответ на вопрос «кем жертвуем»: ждущий занимает место этого союзника.
    * Ждёт Тёмный волк, а свой уже стоит в строю (встал, пока этот ждал), —
    * названный союзник остаётся: новый вцепляется в прежнего волка и встаёт
    * на его место. */
  def takeRisenPlace(pos: Int, lvl: Long, nowMs: Long): Squad =
    pendingRisen.fold(this) { form =>
      val at = wolfRival(form).map(_.position).filter(_ > 0).getOrElse(pos)
      replaceAt(at, form, lvl, nowMs, AllyRates.RisenLasts).copy(pendingRisen = None)
    }

  /** Ждущего отпускают: тело оседает прахом. */
  def dismissRisen: Squad = copy(pendingRisen = None)

  private def risen(form: UndeadForm, pos: Int, lvl: Long, nowMs: Long, lasts: Long): Ally =
    Ally(AllyKind.Undead, pos, 0L, 0L, 0L,
      hiredUntil = nowMs + AllyRates.UndeadMs * lasts.max(1L), undead = Some(form)).restored(lvl)

  /** Все места заняты — новому нужно потеснить кого-то из своих. */
  def full: Boolean = freePosition.isEmpty

  /** Союзник ушёл по свитку: из отряда — вон, вернётся через сутки. */
  def sentAway(kind: AllyKind, nowMs: Long): Squad =
    dismiss(kind).copy(away = away.updated(kind.entryName, nowMs + AllyRates.AwayMs))

  /** Наёмник ушёл от героя сам, без свитка (Фаэлин остался у изгороди с
    * братом): из отряда — вон, за стол таверны сядет через сутки
    * ([[AllyRates.AwayMs]]). Свитка не было — он в `offDuty`, а не в `away`, и
    * реплики о свитке ему не положено (как резерву в [[reserveLeaves]]). */
  def stayedBehind(kind: AllyKind, nowMs: Long): Squad =
    dismiss(kind).copy(offDuty = offDuty.updated(kind.entryName, nowMs + AllyRates.AwayMs))

  /** На сколько мест строй съезжает к первому в бою: пустые места перед первым
    * бойцом — героем или союзником — уходят, промежутки между бойцами остаются.
    * Наёмник на 6-м и герой на 10-м встают в бою на 1-е и 5-е, герой один — на
    * 1-е. В самом отряде места от этого не меняются. */
  def frontShift: Int = (heroPos :: allies.map(_.position)).min - 1

  /** Переставить союзника с позиции `from` на позицию `pos`: занята другим —
    * меняются местами, занята героем — герой встаёт на его прежнюю. */
  def moveAt(from: Int, pos: Int): Squad =
    allies.find(_.position == from) match {
      case None => this
      case Some(_) if pos < 1 || pos > AllyRates.Positions || pos == from => this
      case Some(_) =>
        if (pos == heroPos)
          copy(heroPos = from, allies = allies.map(x => if (x.position == from) x.copy(position = pos) else x))
        else
          copy(allies = allies.map { x =>
            if (x.position == from) x.copy(position = pos)
            else if (x.position == pos) x.copy(position = from)
            else x
          })
    }

  /** То же по виду наёмника — им пользуются экраны, где союзник один такой. */
  def move(kind: AllyKind, pos: Int): Squad =
    allies.find(_.kind == kind).fold(this)(a => moveAt(a.position, pos))

  /** Герой встаёт на позицию `pos`: союзник оттуда — на его прежнюю. */
  def moveHero(pos: Int): Squad =
    if (pos < 1 || pos > AllyRates.Positions || pos == heroPos) this
    else copy(heroPos = pos, allies = allies.map(x => if (x.position == pos) x.copy(position = heroPos) else x))

  /** Двое на одном месте — герой и союзник или два союзника — так быть не
    * должно: такой строй оставлял сюжетный бой без отряда. Лишний встаёт на
    * первое свободное место, остальные стоят где стояли. */
  def repaired: Squad =
    if (allyAt(heroPos).isEmpty && allies.map(_.position).distinct.size == allies.size) this
    else {
      val (placed, _) = allies.foldLeft((Vector.empty[Ally], Set(heroPos))) { case ((acc, taken), a) =>
        if (!taken.contains(a.position)) (acc :+ a, taken + a.position)
        else {
          // Свободное — не занятое ни героем, ни уже вставшими, ни теми, кто
          // ещё встанет на своё.
          val busy = taken ++ allies.map(_.position)
          (1 to AllyRates.Positions).find(p => !busy.contains(p)) match {
            case Some(p) => (acc :+ a.copy(position = p), taken + p)
            case None    => (acc :+ a, taken)
          }
        }
      }
      copy(allies = placed.toList)
    }

  def update(kind: AllyKind)(f: Ally => Ally): Squad =
    copy(allies = allies.map(a => if (a.kind == kind) f(a) else a))

  /** Все живы и здоровы — после отдыха: и строй, и резерв. */
  def restored(lvl: Long): Squad = copy(allies = allies.map(_.restored(lvl)), reserve = reserve.map(_.restored(lvl)))

  /** По позициям. */
  def inOrder: List[Ally] = allies.sortBy(_.position)

  // ── Резерв ──────────────────────────────────────────────────────────────────

  /** Может ли он уйти в резерв: крыса там не усидит — лишняя и так убегает. */
  def reservable(a: Ally): Boolean = a.kind != AllyKind.Rat

  /** Союзник с места `pos` уходит в резерв, его место в строю пустеет. Крысу
    * туда не берут, резерв полон — отряд как был. */
  def toReserve(pos: Int): Squad =
    allyAt(pos) match {
      case Some(a) if reservable(a) && !reserveFull =>
        copy(allies = allies.filterNot(_.position == pos), reserve = reserve :+ a.copy(position = 0))
      case _ => this
    }

  /** Из резерва (`i` — номер в нём) — в строй, на первое свободное место. Мест
    * нет — отряд как был: тогда меняют местами ([[swapIn]]). */
  def deploy(i: Int): Squad =
    (reserve.lift(i), freePosition) match {
      case (Some(a), Some(p)) => copy(allies = allies :+ a.copy(position = p), reserve = reserve.patch(i, Nil, 1))
      case _                  => this
    }

  /** Из резерва — на место `pos` в строю, а стоявший там — в резерв, на его
    * место в списке. Крысу в резерв не отправить — тогда отряд как был. */
  def swapIn(i: Int, pos: Int): Squad =
    (reserve.lift(i), allyAt(pos)) match {
      case (Some(r), Some(a)) if reservable(a) =>
        copy(allies = allies.map(x => if (x.position == pos) r.copy(position = pos) else x),
             reserve = reserve.updated(i, a.copy(position = 0)))
      case _ => this
    }

  /** Выгнать из резерва. */
  def dismissReserve(i: Int): Squad =
    if (reserve.isDefinedAt(i)) copy(reserve = reserve.patch(i, Nil, 1)) else this
}

/** Куда встал новенький — поднятый или нанятый: в строй, в резерв или никуда,
  * если мест нет нигде. */
sealed trait Placed

object Placed {
  case object Line    extends Placed
  case object Reserve extends Placed
  case object Nowhere extends Placed
}

object Squad {
  val empty: Squad = Squad()

  implicit val encoder: Encoder[Squad] = deriveEncoder
  // Резерва в записях до него нет — читается как пустой.
  implicit val decoder: Decoder[Squad] = (c: HCursor) =>
    for {
      heroPos <- c.getOrElse[Int]("heroPos")(1)
      allies  <- c.getOrElse[List[Ally]]("allies")(Nil)
      away    <- c.getOrElse[Map[String, Long]]("away")(Map.empty)
      offDuty <- c.getOrElse[Map[String, Long]]("offDuty")(Map.empty)
      pending <- c.getOrElse[Option[UndeadForm]]("pendingRisen")(None)
      overtime <- c.getOrElse[Map[String, Int]]("overtime")(Map.empty)
      total    <- c.getOrElse[Int]("overtimeTotal")(0)
      reserve  <- c.getOrElse[List[Ally]]("reserve")(Nil)
    } yield Squad(heroPos, allies, away, offDuty, pending, overtime, total, reserve).repaired

  implicit val meta: Meta[Squad] = new Meta(pgDecoderGet, pgEncoderPut)
}
