package pangea.model.battle

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.skill.{MonsterEnergy, Skill}
import pangea.model.stats.FightStats

/** Состояние активного навыка в рамках конкретного боя. Ключ — `itemId` предмета,
 *  на котором висит навык: разные предметы со «одним и тем же» Skill имеют
 *  независимые `cooldown` и `uses` (на будущее — например, два предмета с
 *  «Малым исцелением» катаются параллельно). */
case class SkillSlotState(
  itemId:   Long,
  skill:    Skill,
  cooldown: Int = 0,
  uses:     Int = 0,
  // Тетива «Мощного выстрела» натянута: сколько раз герой тянул дальше (см.
  // Skill.Effect.Charge). None — не целится. На арене едет со слотами стороны.
  aim:      Option[Int] = None
)

object SkillSlotState {
  implicit val encoder: Encoder[SkillSlotState] = deriveEncoder
  implicit val decoder: Decoder[SkillSlotState] = deriveDecoder
}

case class SoloPveBattle(
  monsterLvl:          Long,
  monsterRace:         String,
  monsterRarity:       String,
  monsterStats:        FightStats,
  monsterCurrentHp:    Long,
  monsterCurrentArmor: Long,
  heroBattleState:     HeroBattleState = HeroBattleState.empty,
  // Бонусное действие раунда потрачено: глоток или бросок с пояса, Блок или
  // Контрудар — что-то одно за раунд (если «Быстрые руки» не вернут его).
  consumableUsedThisRound: Boolean = false,
  monsterMarked:       Boolean = false,
  /** Активный моб поднят некромантом: раса своя, природа могильная. */
  monsterUndead:       Boolean = false,
  skillSlots:          List[SkillSlotState] = Nil,
  effects:             BattleEffects = BattleEffects.empty, // тикающие статус-эффекты (яд/реген)
  toughnessUsed:       Boolean = false, // пассивка «Крепкость» срабатывает один раз за бой
  // Вид элементаля, если это бой с минибоссом (имя варианта Elemental). У
  // обычных мобов пусто — по нему бой и отличает босса от рядового врага.
  bossKind:       Option[String] = None,
  // Какую способность элементаль применит следующей (он ходит строго по кругу),
  // и сколько зарядов он уже собрал рядом с собой: у огненного это сферы огня,
  // у каменного — валуны. Механика у них одна, поэтому счётчик общий.
  bossTurn:       Int = 0,
  bossCharges:    Int = 0,
  // Сколько раз минибосс уже поднимался после обнуления HP (Гнилой Джо).
  bossRevives:    Int = 0,
  // Применил ли минибосс своё первое бьющее умение: у Белого волка оно бьёт
  // вдвое, дальше двойной урон — только по шансу.
  bossFirstSkillSpent: Boolean = false,
  // Текущая энергия моба. У рядовых мобов не расходуется (их скиллы бесплатны),
  // а элементаль тратит её на способности и восстанавливает по столько-то за раунд.
  monsterCurrentEnergy: Long = 0L,
  // Групповая часть боя: мобы под номерами 2+, павшие, счётчик раундов, Таран.
  // Обычный бой 1 на 1 — группа из одного, здесь пусто (см. GroupState).
  group: GroupState = GroupState.empty,
  // Сюжетный бой (ключ сценария): добыча и опыт за него решаются сюжетом, а не
  // таблицами; смерть в нём — тоже (см. DeathState).
  story: Option[String] = None,
  // Имя моба от сюжета («Коллектор») вместо имени по расе и редкости.
  customName: Option[String] = None,
  // Божественное оружие бьёт раз в раунд — и своего счётчика ни с кем не делит.
  divineUsedThisRound: Boolean = false,
  // Приговорённый: сколько раундов он терпит, прежде чем удрать. 0 — противник
  // никуда не денется и дерётся до конца, как все.
  escapesAfter:   Int     = 0,
  // Звать некого: ни подкрепления со стороны, ни сородичей на клич. Так дерутся
  // обитатели пещеры — она посчитана до последнего, и лишним взяться неоткуда.
  noKin:          Boolean = false,
  // Бой на арене: какая это строка боя и кто напротив. Пока оно есть, в ответ
  // на ход никто не бьёт — соперник ответит своим ходом, а итог решает арена.
  arena:          Option[ArenaRef] = None,
  // Уровень тех, кого минибосс зовёт себе на помощь: у Крысиного короля это
  // уровень задания, по которому катается вся канализация. 0 — звать некого.
  minionLvl:      Long = 0L,
  // Сколько HP минибосс уже потерял в этом раунде — от кого и от чего угодно:
  // по нему считается предел (см. BossRoundCap), броня — в `bossArmorTaken`.
  // Обнуляется с началом раунда героя. До общего предела здесь копился урон по
  // Крысиному королю — такой счёт читается как HP и живёт до конца раунда.
  bossTaken:      Long = 0L,
  // Западня: бежать отсюда некуда. Так дерутся те, кто сам выбрал место и
  // время, — городская банда, подкараулившая героя в лабиринте.
  noFlee:         Boolean = false,
  // По какому уровню считаются добыча и опыт за павших. Задание с доски
  // обещает противников своего уровня — разбойников, крыс канализации, — и
  // награда за них идёт по нему, а не по этажу лабиринта героя. None — по
  // этажу, как в самом лабиринте.
  lootLevel:      Option[Long] = None,
  // Святая вода разлита по полю: до конца боя жжёт мёртвых, если её не гасят
  // миазмы (см. Aura.HolyWater).
  holyWater:      Boolean = false,
  // Щит героя поднят (Блок): на сколько процентов до его следующего хода
  // режутся удары и умения врагов по нему — по редкости щита (см. Skill.Block,
  // Equipment.blockPct). 0 — щит опущен.
  heroBlockPct:   Long = 0L,
  // На арене: соперник — «моб» этого зеркала — поднял щит своим ходом, и удары
  // героя по нему режутся на столько процентов (см. ArenaBattle). 0 — опущен.
  monsterBlockPct: Long = 0L,
  // Кинжал героя наизготовку (Контрудар): первый удар врага, от которого он
  // увернётся до своего следующего хода, получит ответ наверняка.
  heroCounterReady: Boolean = false,
  // На арене: соперник держит парирующий кинжал наготове — промах героя, и он
  // ответит ударом этой силы (см. ArenaBattle.counterPower). 0 — не держит.
  monsterCounterPower: Long = 0L,
  // На арене: кто это из бойцов соперника стоит в полях — сам он
  // (`SoloPveBattle.ArenaHeroUnit`) или его союзник (см. MonsterSlot.arenaUnit).
  monsterArenaUnit: Option[Int] = None,
  // Раунд героя ещё идёт: прошлое его действие раунд не закончило (пояс, Блок,
  // Контрудар). Щит и стойку прошлого раунда снимает только первое действие
  // нового (см. BattleState.resolveLoaded).
  midRound: Boolean = false,
  // Умения, взятые в этом раунде бонусным действием (Блок, Контрудар): в конце
  // раунда их перезарядка не тикает — как у умения, которым раунд кончился.
  bonusSlots: List[Long] = Nil,
  // Стена трофеев дома героя: прибавка к его урону по расам (entryName → %).
  // Перечитывается из дома каждый ход (см. BattleState.resolveLoaded).
  trophyWall: Map[String, Int] = Map.empty,
  // Кто из отряда бил того, кто в полях, последним (см. MonsterSlot.lastAllyHit).
  monsterLastAllyHit: Option[String] = None,
  // Герой уже бил в этом бою: удар из засады («Тихая роща», порог 6) — только
  // первый.
  heroStruck:     Boolean = false,
  // Сколько раундов боя прошло — по ним «Первопроходец» (порог 6) набирает
  // атаку и защиту. Растёт в каждом tickBuffs: он идёт ровно раз за раунд, и в
  // бою с минибоссом тоже, где счётчик группы стоит.
  roundsFought:   Int     = 0,
  // «Второе дыхание» («Первопроходец», порог 12) уже спасло героя — раз за бой.
  secondWindUsed: Boolean = false,
  // Приговорённый (см. `escapesAfter`): его имя — по нему он узнаётся в строю,
  // где бы ни стоял. Уходит только он, призванные им сородичи остаются.
  preyName:       Option[String] = None,
  // Приговорённый ушёл, пока стоял в паре, а бой на этом кончился победой над
  // его сородичами: в полях всё ещё он, но в павшие он не идёт.
  preyGone:       Boolean = false,
  // На арене: прибавка к уклонению соперника от его бафов — «Перекат», зелье
  // уворота (см. ArenaBattle). Он здесь «моб», и своих бафов у него в этом
  // зеркале нет. Держится только за самим соперником, не за его союзниками.
  monsterDodgeBonus: Long = 0L,
  // Комбо стихий (Огонь+Воздух, Молния+Холод) за этот ход героя уже было: второе
  // не срабатывает — стихии ложатся поодиночке. Сбрасывается в начале каждого
  // хода героя (см. BattleState.resolveLoaded).
  comboUsed: Boolean = false,
  // Сколько брони минибосс уже потерял в этом раунде — пара к `bossTaken`, по
  // ней считается предел брони (см. BossRoundCap).
  bossArmorTaken: Long = 0L,
  // Отряд героя взяли в этот бой — пусть даже строй был пуст (см. `from`).
  // Пав в таком бою, герой теряет и резерв (см. DeathState). В бой один на
  // один — девица, охота Марисы, хозяин бродячего ларца — отряд не брали: он
  // оставался дома, и резерв цел. Арена тоже его не берёт — на песке копии.
  // В записях до этого поля его нет: такой бой читается как бой без отряда —
  // начат по старым правилам, и резерв из него не уходит.
  squadTaken: Boolean = false,
  // Порода того, кто в полях (сетеплёты у деревни): ездит со слотом, как имя.
  monsterBreed: Option[pangea.model.monster.Breed] = None,
  // Цепная молния «Грозы» (порог 12) в этом раунде уже перескакивала: она — не
  // чаще раза за раунд. В записях до этого поля его нет — читается как false.
  chainUsedThisRound: Boolean = false,
  // Приговорённый (`preyName`) ушёл из боя живым — откуда бы ни ушёл: из пары
  // или из строя. Победа переносит это в данные сцены (см. `markPreyLeft`): вор
  // с артефактом деревни ушёл — погоня продолжается. В записях до этого поля
  // его нет — читается как false.
  preyLeft: Boolean = false,
  // Сколько бочек с порохом ещё стоит на поле (см. PowderKeg): в конце раунда
  // одна может рвануть. Ставит их сюжет; в прочих боях и в записях до этого
  // поля — ноль.
  kegs: Int = 0,
  // Кто тянет узор из героя (см. PatternPull): имя того, кто это умеет, —
  // Сборщика на Рубеже II. Ставит сюжет; в прочих боях и в записях до этого
  // поля — никто.
  puller: Option[String] = None,
  // Тело, в котором герой дерётся этот бой (см. BattleBody): в прологе — Лео,
  // ассасин 25-го уровня. Бой читает героя через него и пишет в него же, а
  // настоящий герой этого боя не касается. В прочих боях и в записях до этого
  // поля — своё тело героя.
  body: Option[BattleBody] = None
) {

  /** Множитель Стены трофеев к урону героя по тому, кто сейчас в полях. */
  def trophyMult: Double = 1.0 + trophyWall.getOrElse(monsterRace, 0) / 100.0

  // ── Группа ────────────────────────────────────────────────────────────────

  def isGroup: Boolean = group.isGroup

  /** Активный моб (тот, что в паре с героем) как слот — для перекладывания.
    * Круг минибосса и счёт его предела за раунд уходят вместе с ним. */
  def activeSlot: MonsterSlot = MonsterSlot(
    lvl = monsterLvl, race = monsterRace, rarity = monsterRarity, stats = monsterStats,
    currentHp = monsterCurrentHp, currentArmor = monsterCurrentArmor, marked = monsterMarked,
    currentEnergy = monsterCurrentEnergy, effects = effects.monsterPart, boss = bossKind,
    customName = customName, undead = monsterUndead, arenaUnit = monsterArenaUnit,
    lastAllyHit = monsterLastAllyHit,
    bossTurn = bossTurn, bossCharges = bossCharges, bossRevives = bossRevives,
    bossFirstSkillSpent = bossFirstSkillSpent, bossTaken = bossTaken, bossArmorTaken = bossArmorTaken,
    breed = monsterBreed)

  /** Поставить слот в пару: его состояние и эффекты — в поля активного моба,
    * геройская половина эффектов (и разовые флаги героя вроде «Крепкости»)
    * остаётся как была. Минибосс ездит вместе со своим слотом: пока в полях
    * стоит призванная им крыса, бой про босса ничего не знает — и правила босса
    * к ней не относятся. Счётчики его круга (`bossTurn`, `bossCharges` и
    * прочие) и счёт предела за раунд едут вместе с ним: у того, кто в полях,
    * они на бою, у того, кто в строю, — на его слоте. Поэтому минибоссов в бою
    * может быть и двое — у каждого свой круг. */
  def withActive(slot: MonsterSlot): SoloPveBattle = copy(
    monsterLvl = slot.lvl, monsterRace = slot.race, monsterRarity = slot.rarity, monsterStats = slot.stats,
    monsterCurrentHp = slot.currentHp, monsterCurrentArmor = slot.currentArmor, monsterMarked = slot.marked,
    monsterCurrentEnergy = slot.currentEnergy, effects = effects.withMonsterPart(slot.effects),
    bossKind = slot.boss, customName = slot.customName, monsterUndead = slot.undead,
    monsterArenaUnit = slot.arenaUnit, monsterLastAllyHit = slot.lastAllyHit,
    bossTurn = slot.bossTurn, bossCharges = slot.bossCharges, bossRevives = slot.bossRevives,
    bossFirstSkillSpent = slot.bossFirstSkillSpent, bossTaken = slot.bossTaken, bossArmorTaken = slot.bossArmorTaken,
    monsterBreed = slot.breed)

  /** Кто на чужой стороне держит ауру и сколько у него энергии: живой минибосс
    * со своей аурой, где бы он ни стоял — в паре или в строю, первым или за
    * спинами своих (см. [[Aura]]). Нет такого — None, и поле дышит свободно. */
  def auraHolder: Option[(Aura, Long)] =
    if (activeHoldsAura) boss.flatMap(_.aura).map(_ -> monsterCurrentEnergy)
    else group.others.lift(auraSlotIdx).flatMap(slot =>
      slot.boss.flatMap(pangea.model.monster.MiniBoss.byName).flatMap(_.aura).map(_ -> slot.currentEnergy))

  /** Списать энергию с того, кто держит ауру, — где бы он ни стоял. Ищем его
    * тем же правилом, что и [[auraHolder]]: платит тот, кто держит. */
  def spendAuraEnergy(spent: Long): SoloPveBattle =
    if (activeHoldsAura) copy(monsterCurrentEnergy = (monsterCurrentEnergy - spent).max(0L))
    else group.others.lift(auraSlotIdx).fold(this)(slot =>
      copy(group = group.copy(others = group.others.updated(auraSlotIdx,
        slot.copy(currentEnergy = (slot.currentEnergy - spent).max(0L))))))

  /** Держит ли ауру тот, кто в полях боя: живой минибосс со своей аурой.
    * Павший её не держит, даже пока лежит в полях до конца раунда. */
  private def activeHoldsAura: Boolean = monsterCurrentHp > 0L && boss.exists(_.aura.isDefined)

  /** Индекс в строю живого минибосса со своей аурой; −1 — такого там нет.
    * Смотрим весь строй: Некромант часто стоит за спинами своих мертвецов —
    * герой за отрядом, перемешивание, — и раньше, когда смотрели только первого
    * живого, его миазмы тогда молча гасли. */
  private def auraSlotIdx: Int =
    group.others.indexWhere(slot =>
      slot.alive && slot.boss.flatMap(pangea.model.monster.MiniBoss.byName).exists(_.aura.isDefined))

  /** Стоит ли Некромант за спинами своих: между ним и героем есть хоть один его
    * мертвец. Тогда мистический шаг ему ни к чему — он стоит, где стоял, даже
    * если позади него поднялись новые. */
  def necromancerCovered: Boolean = coveredAt(group.activePos)

  /** Куда Некроманту прятаться: за спины своих — на ближайшее к герою свободное
    * место, перед которым стоит хоть один его мертвец, но не ближе к герою, чем
    * он стоит сейчас: к герою шаг его не ведёт. Занятые места не годятся —
    * вставать вторым на чужое место нельзя, а поменяться значило бы выставить
    * своего мертвеца на место, где его никто не достанет. Он и так за спинами
    * своих, своих не осталось или встать за ними некуда — None, и тогда шаг
    * становится пропуском, бесплатным. */
  def necromancerHideaway: Option[Int] = {
    val mine = math.abs(group.activePos - group.heroPos)
    if (necromancerCovered) None
    else group.freePlaces
      .filter(p => math.abs(p - group.heroPos) >= mine && coveredAt(p))
      .sortBy(p => (math.abs(p - group.heroPos), -p)).headOption
  }

  /** Заслоняет ли место `pos` от героя мертвец строя: стоит по ту же руку от
    * героя, что и место, и ближе к нему — или прямо напротив героя. Если герой
    * зашёл вглубь строя, мертвец по другую руку от него здесь никого не
    * заслоняет. */
  private def coveredAt(pos: Int): Boolean = {
    val d = pos - group.heroPos
    group.entries.exists { case (p, slot) =>
      val u = p - group.heroPos
      slot.alive && Nature.of(slot.race, slot.undead) == Nature.Undead &&
        math.abs(u) < math.abs(d) && (u == 0 || math.signum(u) == math.signum(d))
    }
  }

  /** Активный уходит на свободное место `pos`. Пара при этом пустеет: напротив
    * героя никого не остаётся, и достать ушедшего можно только подойдя.
    *
    * Этим Некромант и прячется — за спинами своих; строй его
    * оттуда не тянет ([[MiniBoss.holdsPosition]]), а колдовать он продолжает:
    * круг способностей идёт у того, кто стоит в полях боя, где бы он ни стоял. */
  def stepActiveTo(pos: Int): SoloPveBattle = copy(group = group.copy(activePos = pos))

  /** «Отбрасывающий выстрел»: тот, кто в полях, отлетает по своему ряду от
    * героя на `dist` мест — пока места свободны: живого он не перелетает и
    * встаёт перед ним. Стоял напротив героя — летит туда, где свободнее (при
    * равных — дальше по строю). Минибоссов и сооружения не сдвинуть, лететь
    * некуда — None. Возвращает бой и новое место. */
  def pushActive(dist: Int): Option[(SoloPveBattle, Int)] =
    if (bossKind.isDefined || Race.immovable(Race.withName(monsterRace))) None
    else {
      val from = group.activePos
      def flight(step: Int): Int =
        (1 to dist).map(from + step * _)
          .takeWhile(p => p >= 1 && p <= Formation.MonsterPlaces && !group.hasMonster(p))
          .lastOption.getOrElse(from)
      val side = from - group.heroPos
      val to =
        if (side > 0) flight(1)
        else if (side < 0) flight(-1)
        else {
          val (right, left) = (flight(1), flight(-1))
          if (math.abs(left - from) > math.abs(right - from)) left else right
        }
      Option.when(to != from)((stepActiveTo(to), to))
    }

  /** Живые бойцы соперника на арене — с местами: тот, кто в полях, и строй. */
  def arenaUnits: List[(Int, MonsterSlot)] =
    ((group.activePos, activeSlot) :: group.entries).filter { case (_, s) => s.alive && s.arenaUnit.isDefined }

  /** Боец соперника на арене с номером `id`: его место и он сам. Павшего здесь
    * уже нет. */
  def arenaUnit(id: Int): Option[(Int, MonsterSlot)] =
    arenaUnits.find { case (_, s) => s.arenaUnit.contains(id) }

  /** Поднят ли щит героя (Блок). */
  def heroShieldUp: Boolean = heroBlockPct > 0L

  /** Поднят ли щит у соперника на арене. */
  def monsterShieldUp: Boolean = monsterBlockPct > 0L

  /** Поднят ли щит у того, кто в полях. На арене щит держит только сам
    * соперник, а не его союзник, вставший в поля на удар. */
  def foeShieldUp: Boolean = monsterShieldUp && monsterArenaUnit.forall(_ == SoloPveBattle.ArenaHeroUnit)

  /** На сколько процентов щит того, кто в полях, режет удар героя (0 — не режет). */
  def foeBlockPct: Long = if (foeShieldUp) monsterBlockPct else 0L

  /** Сила контрудара того, кто в полях: на арене кинжал держит только сам
    * соперник. */
  def foeCounterPower: Long =
    if (monsterArenaUnit.forall(_ == SoloPveBattle.ArenaHeroUnit)) monsterCounterPower else 0L

  /** Прибавка к уклонению того, кто в полях: на арене она есть только у самого
    * соперника. */
  def foeDodgeBonus: Long =
    if (monsterArenaUnit.forall(_ == SoloPveBattle.ArenaHeroUnit)) monsterDodgeBonus else 0L

  /** Пара пуста: активный моб стоит не напротив героя. */
  def unpaired: Boolean = !group.paired

  /** Развернуть в поля моба с места `pos` (для удара по нему): прежний активный
    * сворачивается в слот на своё место. Мобы не двигаются, герой тоже — это
    * подмена «кто в полях», и повторный `engage` на прежнее место всё
    * возвращает. Пустое или уже активное место — бой не меняется. */
  def engage(pos: Int): SoloPveBattle = {
    val idx = group.idxOf(pos)
    if (idx < 0 || pos == group.activePos) this
    else withActive(group.others(idx)).copy(group = group.copy(
      others    = group.others.updated(idx, activeSlot),
      places    = group.places.updated(idx, group.activePos),
      activePos = pos))
  }

  /** Герой шагает на место `pos` (Таран): моб оттуда встаёт с ним в пару,
    * прежний активный остаётся на своём месте. Пустое, чужое или своё место —
    * бой не меняется. */
  def moveHeroTo(pos: Int): SoloPveBattle =
    if (pos == group.heroPos || !group.hasMonster(pos)) this
    else {
      val e = engage(pos)
      e.copy(group = e.group.copy(heroPos = pos))
    }

  /** Запомнить, по кому герой бил: когда напротив пусто, экран боя считает
    * его шансы против этой цели. */
  def rememberTarget(pos: Int): SoloPveBattle =
    copy(group = group.copy(lastTarget = monsterAt(pos).orElse(group.lastTarget)))

  /** Ближайший к герою из индексов `others` (при равном расстоянии — правее). */
  private def nearestOf(idxs: List[Int]): Int =
    idxs.minBy { i =>
      val d = group.places(i) - group.heroPos
      (math.abs(d), if (d > 0) 0 else 1)
    }

  /** Сооружения с места не сходят: ни к герою, ни в перемешивании. */
  private def movable(slot: MonsterSlot): Boolean =
    !Race.immovable(Race.withName(slot.race)) &&
      !slot.boss.flatMap(pangea.model.monster.MiniBoss.byName).exists(_.holdsPosition)

  /** Шагает ли моб сам: подвижный и не пригвождён. Пригвождённый — не
    * сооружение: сквозь него не проходят, и место в строю за ним он держит. */
  private def walks(slot: MonsterSlot): Boolean = movable(slot) && !slot.effects.monsterRooted

  /** Активный моб пал, а в группе есть ещё: записать его в павшие, а в поля
    * боя развернуть ближайшего — НА ЕГО СОБСТВЕННОМ месте. Никого к герою не
    * тянем: место перед ним пустеет, и занять его враги должны сами, шаг за
    * шагом ([[closeIn]]). Пока оно пусто, герой бьёт соседей, а они его —
    * сбоку.
    *
    * Герой с места не сходит: у отряда позиции свои. Мобов больше нет — None,
    * это победа. Отложенный Таран остаётся: герой шагнёт туда, куда таранил,
    * даже если того, кого таранил, там уже нет (см. `BattleState.endRound`). */
  def promoteNext: Option[SoloPveBattle] = nextInPair(fell = true)

  /** Активный ушёл с поля живым — приговорённый удрал: замена та же, что после
    * гибели, только в павшие ушедший не пишется — добычи с него нет. */
  def promoteAfterEscape: Option[SoloPveBattle] = nextInPair(fell = false)

  private def nextInPair(fell: Boolean): Option[SoloPveBattle] =
    if (group.others.isEmpty && group.queue.isEmpty) None
    else if (group.others.isEmpty) admitQueued._1.nextInPair(fell)   // строй пуст, но за ним ждут — входят сами
    else {
      val idx = nearestOf(group.places.indices.toList)
      Some(withActive(group.others(idx)).copy(group = group.copy(
        others      = group.others.patch(idx, Nil, 1),
        places      = group.places.patch(idx, Nil, 1),
        slain       = if (fell) group.slain :+ slainActive else group.slain,
        activePos   = group.places(idx))))
    }

  /** Кто сам дошёл до места напротив героя, тот и встаёт с ним в пару: это не
    * подтягивание, а итог собственного шага ([[closeIn]] доводит мобов до
    * свободного места перед героем). Пара цела или перед героем пусто — None.
    *
    * Издалека никого не тянем: павший в паре не подставляет замену, и место
    * перед героем враги занимают сами. */
  def engageArrived: Option[SoloPveBattle] =
    Option.when(!group.paired && group.occupied(group.heroPos))(engage(group.heroPos))

  /** Напротив героя пусто, а свободный моб есть — он шагает к герою: сам
    * активный, если его никто не держит, иначе ближайший свободный из строя
    * (активный возвращается на своё место). Пара цела или шагать некому — None.
    *
    * Осталось для начальной расстановки ([[settle]]): в бою, который только что
    * собрали, моб встаёт напротив героя сразу, без лишнего раунда ходьбы. */
  def pullFree: Option[SoloPveBattle] =
    if (group.paired) None
    // Спрятавшийся сам решает, где стоять: ни его к герою не тянут, ни вместо
    // него в пару никого не ставят (см. MiniBoss.holdsPosition).
    else if (!movable(activeSlot)) None
    else if (group.freeAt(group.activePos) && movable(activeSlot))
      Some(copy(group = group.copy(activePos = group.heroPos)))
    else {
      val free = group.places.indices.toList
        .filter(i => group.freeAt(group.places(i)) && movable(group.others(i)))
      if (free.isEmpty) None
      else {
        val idx = nearestOf(free)
        Some(withActive(group.others(idx)).copy(group = group.copy(
          others    = group.others.updated(idx, activeSlot),
          places    = group.places.updated(idx, group.activePos),
          activePos = group.heroPos)))
      }
    }

  /** Собрать пару в начале боя: на месте героя стоит моб — он и активный;
    * иначе к герою шагает свободный, если есть. */
  def settle: SoloPveBattle =
    if (group.paired) this
    else if (group.occupied(group.heroPos)) engage(group.heroPos)
    else pullFree.getOrElse(this)

  /** Моб вне пары `others(idx)` пал — в павшие, строй смыкается. */
  def sideFallen(idx: Int): SoloPveBattle = copy(group = group.withoutSlot(idx))

  /** Активный моб как запись о павшем — для добычи после боя. */
  def slainActive: SlainMonster =
    SlainMonster(monsterLvl, monsterRace, monsterRarity, monsterMarked, monsterName, monsterUndead, bossKind, monsterBreed)

  /** Куда встанет пришедший: за спины своих, на первое место позади последнего
    * из них. Хвост схемы занят (в караване там башни) — на самое дальнее
    * свободное место, какое есть. Мест нет совсем — None, такой ждёт в очереди.
    *
    * Место героя тут ни при чём: у чужой стороны своя нумерация, и то, что герой
    * зашёл глубоко, подкрепление вглубь не отодвигает (см. `GroupState.size`). */
  def reinforcementPlace: Option[Int] = {
    // Строй — это те, кто ходит: башни стоят поодаль, и вставать за них незачем,
    // своих они не ведут.
    val walkers = ((group.activePos, activeSlot) :: group.entries).collect {
      case (pos, slot) if movable(slot) => pos
    }
    val behind = (0 :: walkers).max + 1
    (behind to Formation.MonsterPlaces).find(p => !group.hasMonster(p))
      .orElse(group.freePlaces.lastOption)
  }

  /** Подкрепление встаёт за строем; не нашлось места — ждёт в очереди. */
  def withReinforcement(slot: MonsterSlot): SoloPveBattle =
    reinforcementPlace.fold(copy(group = group.copy(queue = group.queue :+ slot)))(pos =>
      copy(group = group.copy(others = group.others :+ slot, places = group.places :+ pos)))

  /** Есть ли место в строю ещё для одного. */
  def hasRoom: Boolean = reinforcementPlace.isDefined

  /** Пришедший встаёт в строй, если есть место, иначе — в очередь за ним. */
  def admit(slot: MonsterSlot): SoloPveBattle =
    if (hasRoom) withReinforcement(slot) else copy(group = group.copy(queue = group.queue :+ slot))

  /** Конец раунда: мобы вне досягаемости героя подтягиваются к нему — по месту
    * за раунд, орки по два (см. [[RaceTactics]]), если места свободны, — ближние
    * первыми, чтобы следом двинулись и дальние. Досягаемость у каждого своя:
    * эльф-стрелок останавливается, как только достаёт героя. Не двигаются: те, кто уже достаёт героя, кто занят союзником
    * напротив, и моб в полях (за него — `pullFree`). На место героя никто не
    * шагает: к пустой паре ведёт `pullFree`. В дыму герой не виден — все стоят.
    * Герой лежит — к нему тоже никто не идёт: мобы идут к отряду, а отряд к
    * ним ([[closeInOnSquad]], [[squadAdvance]]).
    * Возвращает бой и кто куда шагнул. */
  def closeIn: (SoloPveBattle, List[(MonsterSlot, Int)]) =
    if (effects.heroInSmoke || group.heroDown) (this, Nil)
    else {
      closeInSlots
    }

  /** Шаг того, кто стоит в полях боя: он такой же моб и к герою идёт сам — по
    * месту за раунд, орк по два (см. [[RaceTactics]]). Раньше его подставлял
    * строй (см. [[promoteNext]]). Пара цела, идти некуда, держит позицию или
    * занят союзником — None. */
  def stepActiveTowardsHero: Option[SoloPveBattle] =
    if (group.paired || !walks(activeSlot) || group.allyAt(group.activePos).exists(_.alive)) None
    else {
      val race  = Race.withName(monsterRace)
      val reach = RaceTactics.reachOf(race, boss)
      // Кто уже достаёт героя, тот и бьёт оттуда: вставать напротив ему незачем,
      // и место павшего он не занимает. Встал напротив союзника — занят им.
      // Напротив непокрытая повозка — моб занят ею, как союзником.
      @annotation.tailrec
      def walk(at: Int, left: Int): Int =
        if (left <= 0 || math.abs(at - group.heroPos) <= reach || group.wagonBait(at).isDefined ||
            (at != group.activePos && group.allyAt(at).exists(_.alive))) at
        else copy(group = group.copy(activePos = at)).stepTowardsHero(at) match {
          case None       => at
          case Some(next) => walk(next, left - 1)
        }
      val to = walk(group.activePos, RaceTactics.speed(race))
      Option.when(to != group.activePos)(copy(group = group.copy(activePos = to)))
    }

  /** Та же ходьба, но только для слотов строя: активный шагает отдельно. */
  private def closeInSlots: (SoloPveBattle, List[(MonsterSlot, Int)]) =
    {
      val order = group.places.indices.toList.sortBy(i => math.abs(group.places(i) - group.heroPos))
      order.foldLeft((this, List.empty[(MonsterSlot, Int)])) { case ((b, moved), idx) =>
        val slot  = b.group.others(idx)
        val race  = Race.withName(slot.race)
        val reach = RaceTactics.reachOf(race, slot.boss.flatMap(pangea.model.monster.MiniBoss.byName))
        // Шаг за шагом, сколько позволяет скорость. Сооружение стоит там, где
        // стоит: подтягивать его к герою нечем. Те, кто уже достаёт героя, тоже
        // стоят: место павшего в паре никто не занимает по принуждению (см.
        // promoteNext). Встал напротив союзника — занят им.
        @annotation.tailrec
        def walk(bb: SoloPveBattle, left: Int): SoloPveBattle = {
          val pos  = bb.group.places(idx)
          val far  = math.abs(pos - bb.group.heroPos) > reach
          val busy = bb.group.allyAt(pos).exists(_.alive) || bb.group.wagonBait(pos).isDefined
          if (left <= 0 || !far || busy || !walks(slot)) bb
          else bb.stepTowardsHero(pos) match {
            case None       => bb
            case Some(next) => walk(bb.copy(group = bb.group.copy(places = bb.group.places.updated(idx, next))), left - 1)
          }
        }
        val after = walk(b, RaceTactics.speed(race))
        if (after.group.places(idx) == b.group.places(idx)) (b, moved)
        else (after, moved :+ (slot -> after.group.places(idx)))
      }
    }

  /** Герой лежит — лежачего мобы не добивают, и им остаётся отряд. Идут они к
    * нему так же, как шли бы к герою ([[closeIn]]): кто не достаёт ни одного
    * живого союзника, шагает к ближайшему — по месту за раунд, орки по два, — и
    * встаёт, как только достанет: эльф-стрелок за три места, прочие — с
    * соседнего (см. [[RaceTactics]]). Ближние к своей цели идут первыми, чтобы
    * следом двинулись и дальние. Сооружения, пригвождённые и кто держит
    * позицию (Некромант) стоят — к ним идёт отряд ([[squadAdvance]]). В дыму не
    * видно никого — все стоят.
    *
    * Раньше, пока герой лежал, не шагал никто, и моб, до которого союзнику не
    * дотянуться, стоял так до конца времён: бой шёл пустыми раундами.
    * Возвращает бой и кто куда шагнул. */
  def closeInOnSquad: (SoloPveBattle, List[(MonsterSlot, Int)]) =
    if (!group.heroDown || effects.heroInSmoke) (this, Nil)
    else {
      def reachOf(slot: MonsterSlot): Int =
        RaceTactics.reachOf(Race.withName(slot.race), slot.boss.flatMap(pangea.model.monster.MiniBoss.byName))
      val walkers = ((group.activePos, activeSlot) :: group.entries).filter { case (pos, slot) =>
        slot.alive && walks(slot) && allyWithin(pos, reachOf(slot)).isEmpty && group.wagonBait(pos).isEmpty
      }
      val order = walkers.sortBy { case (pos, _) => allyNear(pos).fold(Int.MaxValue)(g => math.abs(g - pos)) }
      // Место моба за чужие шаги не меняется: на занятое место никто не встаёт.
      order.foldLeft((this, List.empty[(MonsterSlot, Int)])) { case ((b, moved), (from, slot)) =>
        b.allyNear(from).fold((b, moved)) { goal =>
          val reach = reachOf(slot)
          @annotation.tailrec
          def walk(at: Int, left: Int): Int =
            if (left <= 0 || b.allyWithin(at, reach).isDefined) at
            else b.stepToward(at, goal) match {
              case None       => at
              case Some(next) => walk(next, left - 1)
            }
          val to = walk(from, RaceTactics.speed(Race.withName(slot.race)))
          if (to == from) (b, moved) else (b.relocate(from, to), moved :+ (slot -> to))
        }
      }
    }

  /** Ближайший к месту `pos` живой союзник (при равном расстоянии — тот, что
    * ближе к началу строя): к нему и идёт моб, пока герой лежит. Нет — None. */
  private def allyNear(pos: Int): Option[Int] =
    group.allies.collect { case a if a.alive => a.position }.sortBy(p => (math.abs(p - pos), p)).headOption

  /** Кого из союзников достаёт моб с места `pos`, бьющий на `reach` мест:
    * стоящего напротив, а нет его — ближайшего в досягаемости (при равном
    * расстоянии — ближе к началу строя). Никого — None. */
  def allyWithin(pos: Int, reach: Int): Option[BattleAlly] =
    group.allies.filter(a => a.alive && math.abs(a.position - pos) <= reach)
      .sortBy(a => (math.abs(a.position - pos), a.position)).headOption

  /** Моб с места `from` встаёт на место `to`: активный — в полях, прочие — в строю. */
  private def relocate(from: Int, to: Int): SoloPveBattle =
    if (from == group.activePos) copy(group = group.copy(activePos = to))
    else copy(group = group.copy(places = group.places.updated(group.idxOf(from), to)))

  /** Кого бьёт союзник: моба напротив, затем того, что в паре с героем, затем
    * соседей — ближних раньше дальних, — в его досягаемости ([[BattleAlly.reach]]):
    * у всех это соседи ([[GroupState.Reach]]), у лучника и у поднятого эльфа —
    * дальше. Бить некого — None. */
  def allyTarget(a: BattleAlly): Option[Int] = {
    val reach = a.reach
    (List(a.position, group.heroPos) ++ (1 to reach).flatMap(d => List(a.position - d, a.position + d))).distinct
      .filter(p => math.abs(p - a.position) <= reach)
      .find(p => monsterAt(p).exists(_.alive))
  }

  /** Отряд идёт на врага сам — и пока герой лежит, и при живом герое: кому бить
    * некого (см. [[allyTarget]]), шагает к ближайшему живому мобу по своему ряду
    * — на позицию за раунд, пока тот не окажется в досягаемости. Героя — лежит
    * он или стоит — обходит; на место другого союзника не встаёт — подождёт.
    * Своей стороной ([[Formation.HeroPlaces]]) не ограничен: идёт хоть до хвоста
    * чужого строя ([[Formation.MonsterPlaces]]) — иначе спрятавшегося там не
    * достать. Ближние к своей цели идут первыми, чтобы следом двинулись и
    * дальние. Сошедший с места запоминает, где стоял (`BattleAlly.stood`): после
    * боя он встаёт туда же — в отряде мест столько, сколько у своей стороны.
    *
    * Иначе тот, кто сам к отряду не идёт, — Некромант за спинами своих, — стоял
    * бы вне досягаемости до конца боя, а союзник с края строя — без дела, пока
    * герой не переставит его сам. Когда шагать, решает раунд: в конце, после
    * шагов мобов. Возвращает бой и кто куда шагнул. */
  def squadAdvance: (SoloPveBattle, List[(BattleAlly, Int)]) = {
    // Пригвождённый (арена) с места не сходит, даже когда бить ему некого.
    // Опутанный паутиной — тоже.
    // Караванщик на своей повозке её не бросает: стоит и ждёт, кто к ней сунется.
    val idle  = group.allies.filter(a => a.alive && a.rooted <= 0 && a.webbed <= 0 && a.frozen <= 0 && allyTarget(a).isEmpty &&
                  !(a.kind == pangea.model.squad.AllyKind.CaravanGuard && group.convoy.exists(_.wagonAt(a.position).isDefined)))
    val order = idle.sortBy(a => mobNear(a.position).fold(Int.MaxValue)(p => math.abs(p - a.position)))
    // Позиция союзника за чужие шаги не меняется: на занятую никто не встаёт.
    order.foldLeft((this, List.empty[(BattleAlly, Int)])) { case ((b, moved), a) =>
      b.mobNear(a.position).flatMap(b.allyStepToward(a.position, _)).fold((b, moved)) { next =>
        (b.copy(group = b.group.updateAlly(a.position)(x =>
           x.copy(position = next, stood = x.stood.orElse(Some(x.position))))),
         moved :+ (a -> next))
      }
    }
  }

  /** Ближайшее к позиции `pos` место, где стоит живой моб (при равном
    * расстоянии — ближе к началу строя). Мобов нет — None. */
  private def mobNear(pos: Int): Option[Int] =
    ((group.activePos, activeSlot) :: group.entries).collect { case (p, s) if s.alive => p }
      .sortBy(p => (math.abs(p - pos), p)).headOption

  /** Куда шагнёт союзник с позиции `pos` к мобу на месте `goal`: на соседнюю
    * позицию в его сторону, а лежит там герой — через него. Там союзник или
    * край схемы боя — None. */
  private def allyStepToward(pos: Int, goal: Int): Option[Int] = {
    val dir  = if (goal > pos) 1 else -1
    val near = pos + dir
    val next = if (near == group.heroPos) near + dir else near
    Option.when(next >= 1 && next <= Formation.MonsterPlaces && group.allyAt(next).isEmpty)(next)
  }

  /** Куда шагнёт моб с места `pos` в сторону героя.
    *
    * Обычно на соседнее место, если оно свободно. Стоит там сооружение —
    * проходит насквозь и встаёт за ним, на первое свободное: башню не обойти
    * стороной, но она и не перегораживает улицу. Иначе те, кому в караване
    * выпало место позади башен, топтались бы там до конца боя, пока герой
    * разбирает охрану в одиночку.
    *
    * Живого моба так не перешагнуть: за ним очередь, и она движется сама. */
  def stepTowardsHero(pos: Int): Option[Int] = stepToward(pos, group.heroPos)

  /** Куда шагнёт моб с места `pos` в сторону места `goal` — по тем же правилам,
    * что и к герою ([[stepTowardsHero]]): пока герой лежит, мобы так идут к
    * отряду ([[closeInOnSquad]]). */
  def stepToward(pos: Int, goal: Int): Option[Int] = {
    val step = if (pos > goal) -1 else 1
    @annotation.tailrec
    def walk(at: Int): Option[Int] = {
      val next = at + step
      // На место перед героем встать можно — и тогда пара собирается сама
      // (см. engageArrived). Раньше оттуда никого не ждали: замену к герою
      // подставлял строй.
      if (next < 1) None
      else if (!group.hasMonster(next)) Some(next)
      else if (monsterAt(next).exists(s => !movable(s))) walk(next)
      else None
    }
    walk(pos)
  }

  /** Герой лежит — тому, кто стоял напротив него, больше нечего делать:
    * лежачего мобы не добивают, а бить со своего места ему некого. Он отвязывается
    * и встаёт напротив ближайшего живого союзника, у которого ещё никого нет:
    * союзники до него дотягивались и раньше ([[GroupState.allyAt]] и `allyTarget`), а он их — нет.
    *
    * Возвращает бой и место, куда перешёл, если перешёл. Сооружение с места
    * не сходит, и без свободного места напротив союзника всё остаётся как есть. */
  def leaveDownedHero: (SoloPveBattle, Option[Int]) =
    if (!group.heroDown || group.activePos != group.heroPos || monsterCurrentHp <= 0L || !walks(activeSlot))
      (this, None)
    else
      group.allies
        .filter(a => a.alive && !group.hasMonster(a.position))
        .sortBy(a => (math.abs(a.position - group.heroPos), a.position))
        .headOption
        .fold((this, Option.empty[Int]))(a =>
          (copy(group = group.copy(activePos = a.position)), Some(a.position)))

  /** Из очереди в строй — сколько влезет. Возвращает бой и вошедших. */
  def admitQueued: (SoloPveBattle, List[MonsterSlot]) =
    group.queue.foldLeft((copy(group = group.copy(queue = Nil)), List.empty[MonsterSlot])) { case ((b, in), s) =>
      if (b.hasRoom) (b.withReinforcement(s), in :+ s) else (b.copy(group = b.group.copy(queue = b.group.queue :+ s)), in)
    }

  /** Кого перемешивание поднимает с места, по возрастанию мест: все, кроме
    * сооружений, тех, кто держит позицию сам, и пригвождённых. */
  def movableInOrder: List[MonsterSlot] = monstersInOrder.filter(walks)

  /** Перемешать строй: подвижные мобы тянут жребий и встают заново — сперва на
    * все места, где напротив кто-то есть (герой, пока он на ногах, и живые
    * союзники — от героя и дальше), а кому таких мест не досталось, те
    * занимают первые свободные. Пустот после этого в строю не остаётся: кто
    * стоял в хвосте, может оказаться в первом ряду, и наоборот.
    *
    * Сооружения в жребий не идут и мест своих не отдают: их не сдвинуть
    * (см. [[movable]]), и раньше одна башня в караване отменяла перемешивание
    * целиком. Герой с места не сходит.
    *
    * `order` — жребий: новый порядок по [[movableInOrder]]. Не перестановка
    * или двигать некого — бой не меняется. */
  def reshuffle(order: List[Int]): SoloPveBattle = {
    val movers = movableInOrder
    if (order.sorted != movers.indices.toList || movers.isEmpty) this
    else {
      val fixed  = ((group.activePos, activeSlot) :: group.entries).filterNot { case (_, s) => walks(s) }
      val locked = fixed.map(_._1).toSet
      // Места, где напротив кто-то стоит, — туда перемешивание ставит первыми:
      // от места героя и дальше по союзникам. Лежачий герой целью не считается.
      val contact = ((if (group.heroDown) Nil else List(group.heroPos)) ++
                      group.allies.filter(_.alive).map(_.position))
                      .distinct.filterNot(locked.contains)
                      .sortBy(p => (math.abs(p - group.heroPos), p))
      val spare = (1 to Formation.MonsterPlaces).toList.filterNot(p => locked(p) || contact.contains(p))
      val spots = (contact ++ spare).take(movers.size).zip(order.map(movers))
      val all   = (fixed ++ spots).sortBy(_._1)
      // В полях боя — тот, кто встал напротив героя; напротив пусто (герой лежит
      // или мест с ним не нашлось) — кто остался на прежнем месте полей, а в
      // крайнем случае ближайший к герою.
      val at = all.collectFirst { case (p, _) if p == group.heroPos => p }
        .orElse(all.collectFirst { case (p, _) if p == group.activePos => p })
        .getOrElse(all.minBy { case (p, _) => (math.abs(p - group.heroPos), p) }._1)
      val (active, rest) = all.partition(_._1 == at)
      withActive(active.head._2).copy(group = group.copy(
        activePos = at, others = rest.map(_._2), places = rest.map(_._1)))
    }
  }

  /** Все живые мобы, кроме сооружений: ими бой и держится — когда таких не
    * осталось, стрелкам некого прикрывать (см. `BattleState`). */
  def livingGuards: List[MonsterSlot] =
    (activeSlot :: group.others).filter(s => s.alive && !Race.immovable(Race.withName(s.race)))

  /** Все мобы по возрастанию мест (пустые места пропущены); активный — на своём месте. */
  def monstersInOrder: List[MonsterSlot] =
    ((group.activePos, activeSlot) :: group.entries).sortBy(_._1).map(_._2)

  /** Весь строй по местам, с пустотами: место → кто на нём. */
  def placesInOrder: List[(Int, Option[MonsterSlot])] = {
    val byPos = ((group.activePos, activeSlot) :: group.entries).toMap
    (1 to group.size).toList.map(pos => pos -> byPos.get(pos))
  }

  /** Моб на месте `pos`, если там кто-то стоит. */
  def monsterAt(pos: Int): Option[MonsterSlot] =
    if (pos == group.activePos) Some(activeSlot) else group.entries.find(_._1 == pos).map(_._2)

  /** Моб напротив героя, если пара не пуста. */
  def pairedMonster: Option[MonsterSlot] = if (group.paired) Some(activeSlot) else None

  /** Раса, которой приходит подкрепление: первого моба этого боя. */
  def reinforcementRace: String = group.originRace.getOrElse(monsterRace)

  /** Элементаль этого боя, если сражаемся с минибоссом. */
  def boss: Option[pangea.model.monster.MiniBoss] =
    bossKind.flatMap(pangea.model.monster.MiniBoss.byName)

  // ── Предел минибосса за раунд ─────────────────────────────────────────────

  /** Сколько ещё HP и брони (именно в таком порядке) может потерять в этом
    * раунде минибосс с такими максимумами (см.
    * [[pangea.model.monster.BossRoundCap]]). Счёт свой у каждого минибосса и
    * ездит вместе с ним (см. [[withActive]]): тот, кто стоит в строю, получает
    * удар во временной паре — со своим счётом. */
  def bossRoom(maxHp: Long, maxArmor: Long): (Long, Long) = {
    import pangea.model.monster.BossRoundCap
    ((BossRoundCap.of(maxHp, BossRoundCap.HpPct) - bossTaken).max(0L),
     (BossRoundCap.of(maxArmor, BossRoundCap.ArmorPct) - bossArmorTaken).max(0L))
  }

  /** Урон тому, кто в полях: `armorDmg` по броне, `hpDmg` по HP. Минибосс за
    * раунд теряет не больше своего предела (см. BossRoundCap): что сверх — до
    * него не доходит, а дошедшее идёт в счёт раунда. Прочим — как есть.
    *
    * Все пути урона по тому, кто в полях, идут через этот метод, а не через
    * `copy`, — чтобы новый источник урона не обошёл предел, забыв про него. */
  def hurtActive(armorDmg: Long, hpDmg: Long): SoloPveBattle.Hurt = {
    val wantArmor = armorDmg.max(0L).min(monsterCurrentArmor)
    val wantHp    = hpDmg.max(0L)
    val isBoss    = boss.isDefined
    val (hpRoom, armorRoom) =
      if (isBoss) bossRoom(monsterStats.hp, monsterStats.armor) else (Long.MaxValue, Long.MaxValue)
    val armor = wantArmor.min(armorRoom)
    val hp    = wantHp.min(hpRoom)
    val newHp = (monsterCurrentHp - hp).max(0L)
    val hurt  = copy(monsterCurrentArmor = monsterCurrentArmor - armor, monsterCurrentHp = newHp)
    // В счёт идёт то, что босс реально потерял: удар сверх остатка HP его не
    // увеличивает — Гнилой Джо встаёт и держит предел дальше.
    val tallied =
      if (!isBoss) hurt
      else hurt.copy(bossTaken = bossTaken + (monsterCurrentHp - newHp), bossArmorTaken = bossArmorTaken + armor)
    // Предел встал на пути впервые за раунд: этот удар до него дошёл, а не
    // упёрся в уже исчерпанный. О павшем говорить незачем.
    val held = newHp > 0L && ((hp < wantHp && hpRoom > 0L) || (armor < wantArmor && armorRoom > 0L))
    SoloPveBattle.Hurt(tallied, armor, hp, wantArmor + wantHp, held)
  }

  /** Урон мобу строя под индексом `idx` — по тем же правилам, что и тому, кто в
    * полях (см. [[hurtActive]]): он встаёт во временную пару, получает удар и
    * возвращается на место — со своим счётом предела (он едет в слоте).
    * Павшего из строя не убирает — это дело вызывающего. */
  def hurtSlot(idx: Int, armorDmg: Long, hpDmg: Long): (SoloPveBattle, SoloPveBattle.Hurt) = {
    val hurt = withActive(group.others(idx)).hurtActive(armorDmg, hpDmg)
    (copy(group = group.copy(others = group.others.updated(idx, hurt.battle.activeSlot))), hurt)
  }

  /** Новый раунд героя: предел минибосса считается заново — и у того, кто в
    * полях, и у тех, кто в строю. */
  def freshRound: SoloPveBattle =
    copy(bossTaken = 0L, bossArmorTaken = 0L, group = group.copy(others = group.others.map(_.freshRound)))

  /** Бой, сохранённый, пока круг минибосса жил только на бою: в полях не
    * минибосс, а круг и счёт предела лежат на бою — значит, они того минибосса,
    * что стоит в строю. Переносим их на его слот, где они живут теперь (см.
    * [[withActive]]). Слот уже со своим кругом или минибосса в строю нет — как
    * есть. */
  def cycleToBossSlot: SoloPveBattle = {
    val cycle = activeSlot
    if (bossKind.isDefined || !cycle.hasBossCycle) this
    else group.others.indexWhere(_.boss.isDefined) match {
      case -1 => this
      case i if group.others(i).hasBossCycle => this
      case i =>
        val boss = group.others(i).copy(bossTurn = bossTurn, bossCharges = bossCharges, bossRevives = bossRevives,
          bossFirstSkillSpent = bossFirstSkillSpent, bossTaken = bossTaken, bossArmorTaken = bossArmorTaken)
        copy(group = group.copy(others = group.others.updated(i, boss)),
          bossTurn = 0, bossCharges = 0, bossRevives = 0, bossFirstSkillSpent = false, bossTaken = 0L, bossArmorTaken = 0L)
    }
  }

  /** Урон от процентов (доля максимума HP: раны, взрыв Огня с Воздухом, ауры)
    * по тому, кто в полях: минибосс получает вдвое меньше (см.
    * [[pangea.model.monster.BossPercentDamage]]), прочие — как есть. */
  def pctDamage(damage: Long): Long =
    if (boss.isDefined) pangea.model.monster.BossPercentDamage(damage) else damage

  /** Обновляет эффекты боя, уважая иммунитеты расы моба: элементалю нельзя
   *  навесить яд или кровотечение — ему нечему течь и нечего травить. Все точки,
   *  накладывающие эффекты на моба, идут через этот метод, а не через `copy`,
   *  чтобы иммунитет нельзя было обойти, забыв про него в новом источнике. */
  def withEffects(e: BattleEffects): SoloPveBattle = {
    val race    = Race.withName(monsterRace)
    val noDots  = if (Race.immuneToDots(race)) e.copy(monsterPoison = None, monsterBleed = None)
                  else if (Race.immuneToBleed(race)) e.copy(monsterBleed = None)
                  else e
    // Огненного элементаля вдобавок нельзя поджечь — он и так пламя. Дракона —
    // тоже: огонь он носит в себе.
    val noBurn  = if (boss.exists(_.immuneToBurn) || Race.immuneToBurn(race)) noDots.copy(monsterBurn = None)
                  else noDots
    copy(effects = noBurn)
  }
  def toMonster: Monster =
    Monster(0L, monsterLvl, Race.withName(monsterRace), Rarity.withName(monsterRarity), monsterStats,
      monsterMarked, undead = monsterUndead)

  /** Имя моба для лога и экрана боя. У минибосса оно именное и приходит от вида
   *  («Огненный Элементаль»); у обычных мобов — из таблицы раса × редкость.
   *  Держим его ЗДЕСЬ, а не в [[Monster]]: тот читается из таблицы `monsters`
   *  целиком (`select *`), и лишнее поле сломало бы чтение. */
  /** Сбежать нельзя: западня (`noFlee`) или сюжетный бой — из него не уходят. */
  def cantFlee: Boolean = noFlee || story.isDefined

  def monsterName: String = customName.orElse(boss.map(_.monsterName)).getOrElse(toMonster.name)

  def rarity: Rarity = Rarity.withName(monsterRarity)

  /** Тик в конце хода игрока. `skipSlots` — itemId слотов, кулдауны которых не
   *  должны декрементиться (например, слот, скил из которого только что
   *  использован — его cd должен начать отсчитываться со СЛЕДУЮЩЕГО хода). */
  def tickBuffs(skipSlots: Set[Long] = Set.empty): SoloPveBattle = copy(
    heroBattleState         = heroBattleState.tick,
    roundsFought            = roundsFought + 1,
    consumableUsedThisRound = false,
    // Божественное оружие живёт своим счётчиком: «быстрые руки» его не ускоряют,
    // а расходник в руках её не блокирует.
    divineUsedThisRound      = false,
    chainUsedThisRound       = false,
    skillSlots              = skillSlots.map(s =>
      if (skipSlots.contains(s.itemId) || bonusSlots.contains(s.itemId)) s
      else s.copy(cooldown = (s.cooldown - 1).max(0))
    ),
    bonusSlots              = Nil,
    // Временные стихийные эффекты тикают вместе с бафами: буст Воздуха и %-дебаф
    // защиты цели (комбо Молния+Холод) живут ограниченное число ходов.
    effects = effects.copy(
      airBoostTurns        = (effects.airBoostTurns - 1).max(0),
      // Буст Воздуха на самом мобе тикает вместе с остальным временным.
      mobAirBoostTurns     = (effects.mobAirBoostTurns - 1).max(0),
      monsterSkillBlockedTurns = (effects.monsterSkillBlockedTurns - 1).max(0),
      // Оцепенение элементаля от холода тикает вместе с прочими временными эффектами.
      chilledTurns         = (effects.chilledTurns - 1).max(0),
      // Дебафы каменного элементаля живут теми же ходами.
      heroStunnedTurns     = (effects.heroStunnedTurns - 1).max(0),
      heroGroundedTurns    = (effects.heroGroundedTurns - 1).max(0),
      monsterWeakenedTurns = (effects.monsterWeakenedTurns - 1).max(0),
      // Уклонение волка после прыжка живёт своими четырьмя ходами.
      mobInstinctTurns     = (effects.mobInstinctTurns - 1).max(0),
      // Смазка на оружии героя и дым сходят за свои раунды.
      heroPoisonCoatTurns  = (effects.heroPoisonCoatTurns - 1).max(0),
      heroBleedCoatTurns   = (effects.heroBleedCoatTurns - 1).max(0),
      heroSmokeTurns       = (effects.heroSmokeTurns - 1).max(0),
      heroTrueStrikeTurns  = (effects.heroTrueStrikeTurns - 1).max(0),
      monsterRootedTurns   = (effects.monsterRootedTurns - 1).max(0),
      // Разогнанная ветром туча Громового собирается обратно за свои раунды.
      mobDispersedTurns    = effects.mobDispersedTurns.map(_ - 1).filter(_ > 0),
      // Узор, что тянет Сборщик, возвращается к герою за свои раунды.
      heroDrainTurns       = effects.heroDrainTurns.map(_ - 1).filter(_ > 0),
      monsterDefenceDebuff = effects.monsterDefenceDebuff.flatMap(_.ticked)
    )
  )

  def slotByItem(itemId: Long): Option[SkillSlotState] = skillSlots.find(_.itemId == itemId)

  def updateSlot(itemId: Long)(f: SkillSlotState => SkillSlotState): SoloPveBattle =
    copy(skillSlots = skillSlots.map(s => if (s.itemId == itemId) f(s) else s))

  /** Слот «Мощного выстрела», если тетива сейчас натянута. */
  def aimSlot: Option[SkillSlotState] = skillSlots.find(_.aim.isDefined)

  /** Стартовый запас энергии обычного моба: `pctRoll`% (5–30) от его потолка,
   *  умноженные на редкость. Копить с нуля мобу некогда — короткий бой кончится
   *  раньше, чем он покажет хоть одно умение. Минибоссов это не касается: они
   *  входят в бой с полным запасом. */
  def withStartEnergy(pctRoll: Long): SoloPveBattle =
    if (bossKind.isDefined) this
    else copy(monsterCurrentEnergy = MonsterEnergy.startEnergy(monsterLvl, rarity, pctRoll))
}

object SoloPveBattle {
  /** Итог удара по тому, кто в полях (см. [[SoloPveBattle.hurtActive]]): бой
    * после него, сколько дошло по броне и по HP, сколько просили (`asked`) и
    * встал ли на пути предел минибосса впервые за раунд (`held` — пора сказать
    * об этом строкой). */
  final case class Hurt(battle: SoloPveBattle, armor: Long, hp: Long, asked: Long, held: Boolean) {
    /** Сколько дошло всего. */
    def dealt: Long = armor + hp

    /** Предел съел удар целиком: просили, а не дошло ничего. */
    def absorbed: Boolean = dealt == 0L && asked > 0L
  }

  /** Сборка боя из моба и героя: статы/hp/броня берутся с моба, слоты активных
   *  навыков отдаёт сам герой (`hero.activeSkillSlots`). Единая точка входа в
   *  бой для всех событий. `squad = false` — сюжетный бой, в который отряд не
   *  берут: герой один, на месте 1. */
  def from(monster: Monster, hero: Hero, squad: Boolean = true): SoloPveBattle = SoloPveBattle(
    monsterLvl          = monster.lvl,
    monsterRace         = monster.race.entryName,
    monsterRarity       = monster.rarity.entryName,
    monsterStats        = monster.fightStats,
    monsterCurrentHp    = monster.fightStats.hp,
    monsterCurrentArmor = monster.fightStats.armor,
    monsterMarked       = monster.marked,
    monsterUndead       = monster.undead,
    customName          = monster.customName,
    monsterBreed        = monster.breed,
    skillSlots          = hero.activeSkillSlots,
    // Зеркальный настой выпит до боя — копии входят в бой вместе с героем.
    effects             = BattleEffects(heroMirrors = hero.weaponDust.mirrors),
    monsterCurrentEnergy = monster.fightStats.energy,
    squadTaken          = squad,
    // Отряд съезжает к месту 1, промежутки между бойцами остаются (герой один —
    // на месте 1, см. Squad.frontShift); в отряде каждый записан под своим
    // местом (`home`) — по нему его там и найдут после боя. Моб обычной встречи
    // всегда появляется на месте 1, где бы ни стоял герой.
    group =
      if (squad) {
        val shift = hero.squad.frontShift
        GroupState(heroPos = hero.squad.heroPos - shift, activePos = 1,
          allies = hero.squad.inOrder.map(a => BattleAlly.of(a, hero.lvl,
            hero.sets.undeadAllyBoostPct, hero.sets.undeadEntersFresh).copy(position = a.position - shift)))
      }
      else GroupState()
  )

  /** Бой против группы: мобы встают по местам 1, 2, … подряд; в паре с героем
    * — тот, что на его месте, а если там пусто — свободный (см. `settle`).
    * Раса первого запоминается — подкрепление приходит такой же.
    *
    * Мест у врагов ровно [[Formation.MonsterPlaces]] — кому не хватило, ждут в
    * очереди и входят, как только место освободится. `towers` — сколько
    * сооружений идёт в конце списка: им отданы последние места схемы, они с них
    * не сходят и в очередь не попадают никогда (караван без своих стрелков —
    * уже не караван). */
  def fromGroup(monsters0: List[Monster], hero: Hero, startEnergies: List[Long],
                squad: Boolean = true, towers: Int = 0,
                headBoss: Option[pangea.model.monster.MiniBoss] = None): SoloPveBattle = {
    // Двое именных одной расы под одним именем в бой не выходят.
    val monsters = Monster.distinctNames(monsters0)
    val energies = startEnergies.padTo(monsters.size, 0L)
    val slots = monsters.zip(energies).map { case (m, e) =>
      MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp,
        m.fightStats.armor, m.marked, e, BattleEffects.empty, customName = m.customName,
        undead = m.undead, breed = m.breed)
    }
    // Хотя бы один боец в строю нужен: с него начинается бой, и сооружению эта
    // роль не подходит — оно не ходит и не встаёт в пару.
    val towerCount         = towers.max(0).min(slots.size - 1)
    val (fighters, towered) = slots.splitAt(slots.size - towerCount)
    val towerPlaces         = ((Formation.MonsterPlaces - towerCount + 1) to Formation.MonsterPlaces).toList
    // Место первого занято им самим, последние — башнями: остальным остаётся то,
    // что между.
    val (standing, waiting) = fighters.tail.splitAt(Formation.MonsterPlaces - towerCount - 1)
    // `headBoss` — первый в списке минибосс (Некромант со своими мертвецами).
    // Отметка — на него самого и до `settle`: герой стоит не первым — в пару
    // встаёт тот, кто напротив, а босс уходит в строй, и отметка уезжает с ним.
    // Повешенная на бой потом, она досталась бы тому, кто в полях, — мертвецу,
    // а настоящий Некромант стоял бы в строю простым мобом.
    val head = from(monsters.head, hero, squad).copy(monsterCurrentEnergy = energies.head,
      bossKind = headBoss.map(_.entryName))
    head.copy(group = head.group.copy(
      others     = standing ++ towered,
      places     = (2 to standing.size + 1).toList ++ towerPlaces,
      queue      = waiting,
      originRace = Some(monsters.head.race.entryName))).settle
  }

  implicit val encoder: Encoder[SoloPveBattle] = deriveEncoder

  implicit val decoder: Decoder[SoloPveBattle] = (c: HCursor) =>
    for {
      monsterLvl          <- c.get[Long]("monsterLvl")
      monsterRace         <- c.get[String]("monsterRace")
      monsterRarity       <- c.get[String]("monsterRarity")
      monsterStats        <- c.get[FightStats]("monsterStats")
      monsterCurrentHp    <- c.get[Long]("monsterCurrentHp")
      monsterCurrentArmor <- c.get[Long]("monsterCurrentArmor")
      heroBattleState     <- c.getOrElse[HeroBattleState]("heroBattleState")(HeroBattleState.empty)
      consumableUsed      <- c.getOrElse[Boolean]("consumableUsedThisRound")(false)
      monsterMarked       <- c.getOrElse[Boolean]("monsterMarked")(false)
      monsterUndead       <- c.getOrElse[Boolean]("monsterUndead")(false)
      skillSlots          <- c.getOrElse[List[SkillSlotState]]("skillSlots")(Nil)
      effects             <- c.getOrElse[BattleEffects]("effects")(BattleEffects.empty)
      toughnessUsed       <- c.getOrElse[Boolean]("toughnessUsed")(false)
      bossKind       <- c.getOrElse[Option[String]]("bossKind")(None)
      bossTurn       <- c.getOrElse[Int]("bossTurn")(0)
      charges             <- c.getOrElse[Int]("bossCharges")(0)
      revives             <- c.getOrElse[Int]("bossRevives")(0)
      firstSkill          <- c.getOrElse[Boolean]("bossFirstSkillSpent")(false)
      monsterEnergy       <- c.getOrElse[Long]("monsterCurrentEnergy")(0L)
      group               <- c.getOrElse[GroupState]("group")(GroupState.empty)
      story               <- c.getOrElse[Option[String]]("story")(None)
      customName          <- c.getOrElse[Option[String]]("customName")(None)
      divineUsed          <- c.getOrElse[Boolean]("divineUsedThisRound")(false)
      escapesAfter        <- c.getOrElse[Int]("escapesAfter")(0)
      noKin               <- c.getOrElse[Boolean]("noKin")(false)
      arena               <- c.getOrElse[Option[ArenaRef]]("arena")(None)
      minionLvl           <- c.getOrElse[Long]("minionLvl")(0L)
      bossTaken           <- c.getOrElse[Long]("bossTaken")(0L)
      noFlee              <- c.getOrElse[Boolean]("noFlee")(false)
      lootLevel           <- c.getOrElse[Option[Long]]("lootLevel")(None)
      holyWater           <- c.getOrElse[Boolean]("holyWater")(false)
      heroBlockPct        <- c.getOrElse[Long]("heroBlockPct")(0L)
      monsterBlockPct     <- c.getOrElse[Long]("monsterBlockPct")(0L)
      heroCounterReady    <- c.getOrElse[Boolean]("heroCounterReady")(false)
      monsterCounterPower <- c.getOrElse[Long]("monsterCounterPower")(0L)
      monsterArenaUnit    <- c.getOrElse[Option[Int]]("monsterArenaUnit")(None)
      midRound            <- c.getOrElse[Boolean]("midRound")(false)
      bonusSlots          <- c.getOrElse[List[Long]]("bonusSlots")(Nil)
      trophyWall          <- c.getOrElse[Map[String, Int]]("trophyWall")(Map.empty)
      monsterLastAllyHit  <- c.getOrElse[Option[String]]("monsterLastAllyHit")(None)
      heroStruck          <- c.getOrElse[Boolean]("heroStruck")(false)
      roundsFought        <- c.getOrElse[Int]("roundsFought")(0)
      secondWindUsed      <- c.getOrElse[Boolean]("secondWindUsed")(false)
      preyName            <- c.getOrElse[Option[String]]("preyName")(None)
      preyGone            <- c.getOrElse[Boolean]("preyGone")(false)
      monsterDodgeBonus   <- c.getOrElse[Long]("monsterDodgeBonus")(0L)
      comboUsed           <- c.getOrElse[Boolean]("comboUsed")(false)
      bossArmorTaken      <- c.getOrElse[Long]("bossArmorTaken")(0L)
      // Бой, начатый до этого поля, — без отряда: резерв из него не уходит.
      squadTaken          <- c.getOrElse[Boolean]("squadTaken")(false)
      monsterBreed        <- pangea.model.monster.Breed.read(c, "monsterBreed")
      chainUsed           <- c.getOrElse[Boolean]("chainUsedThisRound")(false)
      preyLeft            <- c.getOrElse[Boolean]("preyLeft")(false)
      // Бой, сохранённый до бочек с порохом, — без них.
      kegs                <- c.getOrElse[Int]("kegs")(0)
      // Бой, сохранённый до Рубежа II, — узор в нём никто не тянет.
      puller              <- c.getOrElse[Option[String]]("puller")(None)
      // Бой, сохранённый до пролога с боем, — в своём теле.
      body                <- c.getOrElse[Option[BattleBody]]("body")(None)
    } yield SoloPveBattle(monsterLvl, monsterRace, monsterRarity, monsterStats,
                         monsterCurrentHp, monsterCurrentArmor, heroBattleState, consumableUsed, monsterMarked,
                         monsterUndead,
                         skillSlots, effects, toughnessUsed, bossKind, bossTurn, charges, revives, firstSkill, monsterEnergy, group,
                         story, customName, divineUsed, escapesAfter, noKin, arena, minionLvl, bossTaken,
                         noFlee, lootLevel, holyWater, heroBlockPct, monsterBlockPct,
                         heroCounterReady, monsterCounterPower, monsterArenaUnit, midRound, bonusSlots, trophyWall,
                         monsterLastAllyHit, heroStruck, roundsFought, secondWindUsed, preyName, preyGone,
                         monsterDodgeBonus, comboUsed, bossArmorTaken, squadTaken, monsterBreed,
                         chainUsed, preyLeft, kegs, puller, body).cycleToBossSlot
      // ↑ Бой, сохранённый до того, как круг минибосса переехал на слот: круг
      // того, кто стоит в строю, забираем с боя на его слот.

  /** Номер самого соперника среди «мобов» зеркала арены; его союзники — от
    * единицы (см. `ArenaSide.allies`). */
  val ArenaHeroUnit: Int = 0

  /** Отметка в данных события сцены (`LootData.eventData`), что приговорённый
    * ушёл из боя живым (см. `preyLeft`): её кладёт победа, читает сцена, куда
    * вернёт добыча. */
  val PreyLeftKey: String = "preyLeft"

  def markPreyLeft(eventData: Json): Json = eventData.deepMerge(Json.obj(PreyLeftKey -> Json.True))

  def preyLeftIn(sceneData: Json): Boolean = sceneData.hcursor.get[Boolean](PreyLeftKey).toOption.contains(true)
}
