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
  undead:     Option[UndeadForm] = None
) {
  def name: String = undead.map(_.name).getOrElse(kind.name)

  /** Тёмный волк с алтаря или из боя с «Некромантом» — в отряде он один. */
  def isDarkWolf: Boolean = undead.exists(_.unique)

  /** Потолки статов: у наёмника — по уровню героя, у поднятого — свои. */
  def statsAt(lvl: Long): FightStats = undead.map(_.stats).getOrElse(kind.stats(lvl))

  /** Уровень, по которому он дерётся. */
  def lvlAt(heroLvl: Long): Long = undead.map(_.lvl).getOrElse(kind.effectiveLvl(heroLvl))

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
    } yield Ally(kind, position, hp, armor, energy, until, undead)
}

/** Отряд героя: места в строю (см. [[AllyRates.Positions]]) делят герой
 *  (`heroPos`) и союзники — до десяти; расставляет их игрок, и встать можно на
 *  любое место. В бой строй выходит, съехав к первому месту (см. [[frontShift]]):
 *  союзник на месте N стоит против врага на месте N и достаёт соседние места.
 *  Ушёл союзник — его место просто пустеет. `away` — кто ушёл по свитку и
 *  когда вернётся в таверну (ключ — вид); `offDuty` — кто отработал свой найм и
 *  когда снова сядет за стол. Хранится в `heroes.squad_data`. */
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
  overtimeTotal: Int = 0
) {
  def isEmpty: Boolean  = allies.isEmpty
  def nonEmpty: Boolean = allies.nonEmpty

  def has(kind: AllyKind): Boolean = allies.exists(_.kind == kind)

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
  def rushed(kind: AllyKind, lvl: Long, nowMs: Long): Squad =
    welcomeBack(kind).hire(kind, lvl, nowMs)
      .copy(overtime = overtime.updated(kind.entryName, overtimeOf(kind) + 1), overtimeTotal = overtimeTotal + 1)

  /** Отдохнул как положено — переработки забыты. */
  def rested(kind: AllyKind): Squad = copy(overtime = overtime - kind.entryName)

  /** Стереть записи об отлучке (реплика показана / срок вышел). */
  def welcomeBack(kind: AllyKind): Squad = copy(away = away - kind.entryName, offDuty = offDuty - kind.entryName)

  /** Первая свободная позиция, если есть. */
  def freePosition: Option[Int] =
    (1 to AllyRates.Positions).find(p => p != heroPos && allyAt(p).isEmpty)

  /** Нанять на [[AllyRates.HireMs]]: на первую свободную позицию, полностью здоровым. */
  def hire(kind: AllyKind, lvl: Long, nowMs: Long): Squad =
    if (has(kind)) this
    else freePosition.fold(this)(p =>
      copy(allies = allies :+ Ally(kind, p, 0L, 0L, 0L, hiredUntil = nowMs + AllyRates.HireMs).restored(lvl)))

  /** Кто покидает отряд к этому моменту: наёмник отработал свой найм и сядет
    * за стол снова через [[AllyRates.OffDutyMs]], а поднятый рассыпался — его
    * ждать неоткуда. Возвращает отряд и ушедших целиком: наверху по ним
    * решают, что сказать игроку. */
  def expire(nowMs: Long): (Squad, List[Ally]) = {
    val gone = allies.filter(_.leaving(nowMs))
    if (gone.isEmpty) (this, Nil)
    else {
      val hired = gone.filter(_.undead.isEmpty).map(_.kind)
      (copy(allies = allies.filterNot(_.leaving(nowMs)),
            offDuty = offDuty ++ hired.map(k => k.entryName -> (nowMs + AllyRates.OffDutyMs))), gone)
    }
  }

  /** Поднятые до того, как у них завёлся срок, получают его с этой минуты:
    * иначе они рассыпались бы все разом при первом же заходе героя. */
  def settleUndead(nowMs: Long): Squad =
    if (!allies.exists(a => a.undead.isDefined && a.hiredUntil <= 0L)) this
    else copy(allies = allies.map { a =>
      if (a.undead.isDefined && a.hiredUntil <= 0L) a.copy(hiredUntil = nowMs + AllyRates.UndeadMs) else a
    })

  def dismiss(kind: AllyKind): Squad = copy(allies = allies.filterNot(_.kind == kind))

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

  /** Поднятый встаёт на свободное место — и держится [[AllyRates.UndeadMs]] с
    * этой минуты, а с «Некромантом» (порог 6) вдвое дольше: `lasts` — этот
    * множитель. Мест нет — отряд как был; заменой заведует [[replaceAt]].
    *
    * Тёмный волк в отряде один: если он уже есть, новый сцепляется с ним и
    * побеждает — встаёт на место прежнего ([[wolfRival]]). Свободного места
    * для этого не нужно. */
  def raise(form: UndeadForm, lvl: Long, nowMs: Long, lasts: Long = 1L): Squad =
    wolfRival(form) match {
      case Some(old) => replaceAt(old.position, form, lvl, nowMs, lasts)
      case None      => freePosition.fold(this)(p => copy(allies = allies :+ risen(form, p, lvl, nowMs, lasts)))
    }

  /** Поднятый занимает место того, кто на нём стоял. Встаёт Тёмный волк — с
    * ним уходят и прежние волки: второму в отряде не бывать. */
  def replaceAt(pos: Int, form: UndeadForm, lvl: Long, nowMs: Long, lasts: Long = 1L): Squad =
    copy(allies = allies.filterNot(a => a.position == pos || (form.unique && a.isDarkWolf)) :+
      risen(form, pos, lvl, nowMs, lasts))

  /** Тёмные волки отряда, по местам. Больше одного их не бывает: новый
    * сцепляется с прежним и побеждает (см. [[raise]]). Несколько бывает лишь у
    * тех, кто поднял их до этого правила, — новый одолеет всех разом. */
  def darkWolves: List[Ally] = inOrder.filter(_.isDarkWolf)

  /** С кем сцепится этот поднятый: Тёмный волк — с прежним волком отряда
    * (первым по строю), прочим делить нечего. */
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

  /** Восставший в бою уходит с героем на [[AllyRates.RisenMs]]. Мест нет — он
    * ждёт у шатра ([[pendingRisen]]), и в «Отряде» герой решит, кем за него
    * пожертвовать. Ждущего второй раз не перебиваем: первый дождётся ответа.
    * Тёмному волку при прежнем волке ждать незачем: он встаёт на место
    * прежнего (см. [[raise]]). */
  def admitRisen(form: UndeadForm, lvl: Long, nowMs: Long): Squad =
    if (freePosition.isDefined || wolfRival(form).isDefined) raise(form, lvl, nowMs, AllyRates.RisenLasts)
    else if (pendingRisen.isDefined) this
    else copy(pendingRisen = Some(form))

  /** Ответ на вопрос «кем жертвуем»: ждущий занимает место этого союзника.
    * Ждёт Тёмный волк, а свой в отряде уже есть (он встал, пока этот ждал), —
    * названный союзник остаётся: новый вцепляется в прежнего волка и встаёт
    * на его место. */
  def takeRisenPlace(pos: Int, lvl: Long, nowMs: Long): Squad =
    pendingRisen.fold(this) { form =>
      val at = wolfRival(form).fold(pos)(_.position)
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

  /** Все живы и здоровы — после отдыха. */
  def restored(lvl: Long): Squad = copy(allies = allies.map(_.restored(lvl)))

  /** По позициям. */
  def inOrder: List[Ally] = allies.sortBy(_.position)
}

object Squad {
  val empty: Squad = Squad()

  implicit val encoder: Encoder[Squad] = deriveEncoder
  implicit val decoder: Decoder[Squad] = (c: HCursor) =>
    for {
      heroPos <- c.getOrElse[Int]("heroPos")(1)
      allies  <- c.getOrElse[List[Ally]]("allies")(Nil)
      away    <- c.getOrElse[Map[String, Long]]("away")(Map.empty)
      offDuty <- c.getOrElse[Map[String, Long]]("offDuty")(Map.empty)
      pending <- c.getOrElse[Option[UndeadForm]]("pendingRisen")(None)
      overtime <- c.getOrElse[Map[String, Int]]("overtime")(Map.empty)
      total    <- c.getOrElse[Int]("overtimeTotal")(0)
    } yield Squad(heroPos, allies, away, offDuty, pending, overtime, total).repaired

  implicit val meta: Meta[Squad] = new Meta(pgDecoderGet, pgEncoderPut)
}
