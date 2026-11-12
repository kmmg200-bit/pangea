package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.model.battle.{BattleAlly, Element, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.{Ally, AllyKind, AllyRates}
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** Личные задания наёмников: у каждого, кто сидит за дальним столом таверны,
  * своё — его старый долг или его снаряжение, и в нём — его история. Берётся
  * разговором за столом, до найма («Поговорить» на карточке, см.
  * MercenariesState). Выполнено — потолок наёмника у этого героя растёт на
  * [[AllyRates.QuestGrowth]]: при каждом найме ([[grown]]) и сразу — у того,
  * кто уже при герое ([[grow]]). Задания есть у Йоргена ([[JorgenQuest]]) и
  * Брамбла ([[BramblQuest]]). */
object MercQuests {

  /** Чьё это задание — если у наёмника оно есть. */
  def quest(kind: AllyKind): Option[NpcQuest] = kind match {
    case AllyKind.Human => Some(NpcQuest.JorgenDebt)
    case AllyKind.Gnome => Some(NpcQuest.BramblLock)
    case _              => None
  }

  /** Уровень его задания: сложность — в разговоре и на карточке предмета. */
  def level(kind: AllyKind): Option[Int] = kind match {
    case AllyKind.Human => Some(JorgenQuest.Level)
    case AllyKind.Gnome => Some(BramblQuest.Level)
    case _              => None
  }

  /** Ключ его текстов в scenes.yaml. */
  def textBase(kind: AllyKind): Option[String] = kind match {
    case AllyKind.Human => Some("jorgen")
    case AllyKind.Gnome => Some("bramblLock")
    case _              => None
  }

  /** Что наёмник отдаёт герою, когда тот согласился: с этим предметом задание и
    * ведётся (см. правило «задание ведёт предмет»). */
  def startItem(kind: AllyKind): Option[QuestItemKind] = kind match {
    case AllyKind.Human => Some(QuestItemKind.JorgenRoster)
    case AllyKind.Gnome => Some(QuestItemKind.BramblCast)
    case _              => None
  }

  /** Взять его задание: предмет — в сумку, задание — на первый шаг. Нечего
    * брать или уже взято — false. */
  def take(kind: AllyKind, heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
           userId: UserId, hero: Hero, nowMs: Long): Task[Boolean] = kind match {
    case AllyKind.Human => JorgenQuest.take(heroDao, inventory, items, userId, hero, nowMs)
    case AllyKind.Gnome => BramblQuest.take(heroDao, inventory, items, userId, hero, nowMs)
    case _              => ZIO.succeed(false)
  }

  /** О задании можно заговорить: оно у него есть, ещё не взято и не выполнено. */
  def talkOpen(q: NpcQuests, kind: AllyKind): Boolean = quest(kind).exists(n => !q.isTaken(n) && !q.isDone(n))

  /** Задание выполнено. */
  def done(q: NpcQuests, kind: AllyKind): Boolean = quest(kind).exists(q.isDone)

  /** Насколько вырос его потолок у этого героя. */
  def grown(q: NpcQuests, kind: AllyKind): Long = if (done(q, kind)) AllyRates.QuestGrowth else 0L

  /** Задание закрыто, а наёмник уже при герое — в строю или в резерве: он
    * растёт с этой минуты, не дожидаясь следующего найма. */
  def grow(heroDao: HeroDao, userId: UserId, hero: Hero, kind: AllyKind): Task[Unit] =
    ZIO.when(hero.squad.has(kind))(heroDao.updateSquad(userId, hero.squad.grow(kind, AllyRates.QuestGrowth))).unit

  // ── Бой своего задания ──────────────────────────────────────────────────────

  /** Наёмник в бою своего задания — всегда рядом. Он в строю — дерётся сам;
    * `strike` — чем бьёт в этом бою (None — своей стихией). Нет его в строю (не
    * нанят или стоит в резерве) — идёт гостем на этот бой, на ближайшее к герою
    * свободное место, и в отряд после боя не встаёт (см.
    * `BattleState.squadAfterBattle`). Мест в строю нет — бой без него. */
  def withMerc(battle: SoloPveBattle, hero: Hero, kind: AllyKind, strike: Option[Element] = None,
               grown: Long = 0L): SoloPveBattle = {
    val allies = battle.group.allies
    if (allies.exists(_.kind == kind))
      battle.copy(group = battle.group.copy(allies =
        allies.map(a => if (a.kind == kind && strike.isDefined) a.copy(strike = strike) else a)))
    else battle.group.nearestFreeAllyPlace.fold(battle)(pos =>
      battle.copy(group = battle.group.copy(allies = allies :+ guest(hero, kind, strike, grown).copy(position = pos))))
  }

  /** Наёмник-гость: на полных силах, по своему уровню у этого героя. Места в
    * отряде у него нет (`home` — 0). */
  def guest(hero: Hero, kind: AllyKind, strike: Option[Element] = None, grown: Long = 0L): BattleAlly =
    BattleAlly.of(Ally(kind, 0, 0L, 0L, 0L, grown = grown).restored(hero.lvl), hero.lvl).copy(strike = strike)

  /** Минибоссы своего задания: все враги боя — одного вида, оба со своим кругом
    * (см. `MonsterSlot.bossTurn`), на полной энергии; кто бы ни встал с героем в
    * пару, правила у него свои. */
  def bossFight(boss: MiniBoss, count: Int, questLevel: Int, hero: Hero): SoloPveBattle = {
    val lvl  = boss.bossLvl(questLevel.toLong)
    val foes = List.fill(count)(Monster(0L, lvl, Race.Elemental, Rarity.Legendary, boss.stats(lvl)))
    val base = SoloPveBattle.fromGroup(foes, hero, foes.map(_.fightStats.energy))
    base.copy(bossKind = Some(boss.entryName),
      group = base.group.copy(others = base.group.others.map(_.copy(boss = Some(boss.entryName)))))
  }
}
