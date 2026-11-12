package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.model.battle.{Element, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.MiniBoss
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** «Жетоны из пепла» — личное задание Йоргена Кремня (см. [[MercQuests]]).
  * Берётся разговором за столом наёмников: его отряд пал в логове огненных
  * элементалей — их там было двое, — и без жетонов павших гильдия пишет их
  * пропавшими, а семьи не получают своей доли. Список отряда ведёт к логову
  * (из лабиринта, не ближе поворота логов элементалей); там — бой с обоими
  * огненными, Йорген рядом и бьёт холодом; в пепле — жетоны; их — Мастеру
  * Горну. Сам ход — [[NpcQuest.JorgenDebt]] в `npc_quests`; сцены — таверна
  * (MercenariesState), логово (JorgenLairState) и Горн (MasterHornState). */
object JorgenQuest {

  /** Сложность задания — 💀🦴🦴🦴🦴. По ней же уровень элементалей. */
  val Level: Int = 9

  /** Ключ сюжетного боя в логове (см. `SoloPveBattle.story`): без «Сбежать», а
    * награда как за обычный бой — с каждого минибосса своя. */
  val LairStory: String = "jorgenLair"

  /** Огненных в логове двое — потому отряд и пал. */
  val Elementals: Int = 2

  /** Логово — там же, где все логова элементалей: не ближе их поворота (см.
    * `StateType.EventMinLevel`). */
  val LairDungeonLevel: Int = StateType.EventMinLevel.getOrElse(StateType.ElementalLair, 1)

  def deepEnough(dungeonLevel: Int): Boolean = dungeonLevel >= LairDungeonLevel

  // ── Ход задания ─────────────────────────────────────────────────────────────

  /** Задания у героя ещё не было: о нём можно заговорить. */
  def fresh(q: NpcQuests): Boolean = !q.isTaken(NpcQuest.JorgenDebt) && !q.isDone(NpcQuest.JorgenDebt)

  /** Список отряда на руках — идти к логову. */
  def toLair(q: NpcQuests): Boolean = q.onStep(NpcQuest.JorgenDebt, 1)

  /** Жетоны на руках — нести Горну. */
  def toHorn(q: NpcQuests): Boolean = q.onStep(NpcQuest.JorgenDebt, 2)

  /** Герой согласился помочь: список отряда — в сумку, задание взято. Уже было —
    * ничего (false). */
  def take(heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
           userId: UserId, hero: Hero, nowMs: Long): Task[Boolean] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (!fresh(q)) ZIO.succeed(false)
      else QuestSupport.giveItem(inventory, items, hero, QuestItemKind.JorgenRoster) *>
        NpcQuestLog.save(heroDao, userId, q.take(NpcQuest.JorgenDebt, nowMs)).as(true)
    }

  /** Оба огненных пали: жетоны из пепла — в сумку, дальше — к Горну. Не тот
    * шаг — ничего (false). */
  def tokensFound(heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
                  userId: UserId, hero: Hero): Task[Boolean] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (!toLair(q)) ZIO.succeed(false)
      else QuestSupport.giveItem(inventory, items, hero, QuestItemKind.JorgenTokens) *>
        NpcQuestLog.save(heroDao, userId, q.update(NpcQuest.JorgenDebt)(_.copy(step = 2))).as(true)
    }

  // ── Бой в логове ────────────────────────────────────────────────────────────

  /** Бой в логове: оба огненных — минибоссы по уровню задания, у каждого свой
    * круг (см. `MonsterSlot.bossTurn`), с полной энергией; отряд героя и Йорген
    * с мечом холода — огонь огнём не бьют. Он в строю — берёт этот меч сам,
    * нет — идёт гостем (см. `MercQuests.withMerc`). Бой сюжетный — без «Сбежать»
    * и подкреплений, — награда по уровню задания. */
  def lairBattle(hero: Hero): SoloPveBattle = {
    val fight = MercQuests.bossFight(MiniBoss.FireElemental, Elementals, Level, hero)
    MercQuests.withMerc(fight.copy(story = Some(LairStory), lootLevel = Some(Level.toLong)), hero,
      AllyKind.Human, strike = Some(Element.Cold))
  }
}
