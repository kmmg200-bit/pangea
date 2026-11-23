package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{BattleAlly, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.{AllyKind, UndeadForm}
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** «Тот, кто уговаривает деревья» — всё, что нужно сюжету в разных сценах:
  * собрать волну орков-лесорубов, поставить Элиара в строй, выдать ветвь,
  * узнать, на каком шаге задание. Сам ход — [[NpcQuest.HedgeSong]] в
  * `npc_quests`; сцены — встреча и волны в лабиринте
  * ([[pangea.service.state.states.hedgesong.HedgeSongState]]) и Лавка Фета
  * (развязка). Задание берётся только победой над третьей волной. У героя-эльфа
  * Элиар говорит иначе: `key` подставляет вариант `<ключ>Kin`. */
object HedgeSongQuest {

  /** Сложность задания — 7 ур. По ней же уровень орков и Элиара. */
  val Level: Int = 7

  /** Ключ сюжетного боя (см. `SoloPveBattle.story`). Как и бой у завала,
    * награждается как обычный бой: опыт и добыча с каждого павшего — по уровню
    * задания (`lootLevel`). */
  val WaveStory: String = "hedgeSong"

  /** Волн три: каждая — свой бой, как у одурманенного каравана. */
  val Waves: Int = 3

  /** Первая волна — воины и живучие, вторая — живучие и командиры отряда, по
    * четыре; в третьей — именной вожак и три командира. */
  val WaveSize: Int                     = 4
  val FirstWaveRarities: List[Rarity]   = List(Rarity.Uncommon, Rarity.Rare)
  val SecondWaveRarities: List[Rarity]  = List(Rarity.Rare, Rarity.Mythical)
  val LastWaveCaptains: Int             = 3

  /** Вожак лесорубов: его костёр третью зиму горит ветвями изгороди. */
  val LeaderName: String = "Бокк Последний у Костра"

  /** Эльф, что уговаривает изгородь: по силе — именной эльф уровня задания. */
  val ElfName: String = "Элиар Тихая Ветвь"

  /** Герой той же крови — Элиар говорит с ним иначе. */
  def isKin(hero: Hero): Boolean = hero.race == Race.Elf

  /** Фаэлин Тихострел — младший брат Элиара. Он при герое (в строю или в
    * резерве) — у него в задании свои слова (`hedgeSong.faelin.*`), а уйти
    * посреди песни он с героем не уйдёт: останется с братом. */
  def faelin(hero: Hero): Boolean = hero.squad.has(AllyKind.Elf)

  /** Ключ текста с поправкой на кровь героя: эльфу — `<key>Kin`. */
  def key(hero: Hero, base: String): String = if (isKin(hero)) base + "Kin" else base

  // ── Ход задания ─────────────────────────────────────────────────────────────

  /** Задания у героя ещё не было: эльф может встретиться. */
  def fresh(q: NpcQuests): Boolean = !q.isTaken(NpcQuest.HedgeSong) && !q.isDone(NpcQuest.HedgeSong)

  /** Ветвь на руках — нести Фету. */
  def toFet(q: NpcQuests): Boolean = q.onStep(NpcQuest.HedgeSong, 1)

  /** Изгородь отдала ветвь, когда Фаэлин стоял рядом (`counter` = 1): в
    * таверне он говорит об этом как тот, кто там был. В старых записях
    * `counter` — 0: Фаэлина тогда в таверне ещё не было. */
  def faelinWasThere(q: NpcQuests): Boolean = q.of(NpcQuest.HedgeSong).counter > 0L

  /** Третья волна отбита: изгородь отдала ветвь — она в сумку, задание взято
    * сразу на шаге «нести Фету»; был ли при этом Фаэлин — в `counter`. Уже
    * было — ничего. */
  def branchGiven(heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
                  userId: UserId, hero: Hero, nowMs: Long): Task[Boolean] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (!fresh(q)) ZIO.succeed(false)
      else QuestSupport.giveItem(inventory, items, hero, QuestItemKind.HedgeBranch) *>
        NpcQuestLog.save(heroDao, userId, q.take(NpcQuest.HedgeSong, nowMs)
          .update(NpcQuest.HedgeSong)(_.copy(counter = if (faelin(hero)) 1L else 0L))).as(true)
    }

  // ── Волны ───────────────────────────────────────────────────────────────────

  /** Орки волны `n` (с единицы) уровня задания. В третьей — три командира и
    * последним вожак. */
  def wave(n: Int, rng: Rng): (List[Monster], Rng) = {
    def roll(count: Int, pool: List[Rarity], r: Rng): (List[Monster], Rng) =
      (1 to count).foldLeft((List.empty[Monster], r)) { case ((acc, rr), _) =>
        val (rarity, next) = rr.pick(pool)
        (acc :+ MonsterGenerator.generateOfRaceAndRarity(Level, Race.Orc, rarity), next)
      }
    n match {
      case 1 => roll(WaveSize, FirstWaveRarities, rng)
      case 2 => roll(WaveSize, SecondWaveRarities, rng)
      case _ =>
        val captains = List.fill(LastWaveCaptains)(MonsterGenerator.generateOfRaceAndRarity(Level, Race.Orc, Rarity.Mythical))
        val leader   = MonsterGenerator.generateOfRaceAndRarity(Level, Race.Orc, Rarity.Legendary, Some(LeaderName))
        (captains :+ leader, rng)
    }
  }

  /** Элиар в строю: именной эльф уровня задания, на полных силах. Умения у
    * него свои, союзничьи ([[pangea.model.squad.AllySkill]]); место ему
    * раздаёт бой. */
  def elf: BattleAlly = elfAt(Level)

  /** Тот же Элиар уровня `lvl` — так он встаёт в строй и в других заданиях
    * (на просеке Чернолома — уровнем главы). */
  def elfAt(lvl: Int): BattleAlly = {
    val stats = MonsterGenerator.generateOfRaceAndRarity(lvl, Race.Elf, Rarity.Legendary).fightStats
    BattleAlly(AllyKind.HedgeElf, position = 0, hp = stats.hp, armor = stats.armor, energy = stats.energy,
      stats = stats, lvl = lvl.toLong, undead = Some(UndeadForm(ElfName, lvl.toLong, stats)), home = 0)
  }

  /** Поставить Элиара в строй — на ближайшее к герою свободное место. Отряд
    * занял всё — он поёт из-за спин и в бой не встаёт. */
  def withElf(battle: SoloPveBattle): SoloPveBattle =
    battle.group.nearestFreeAllyPlace.fold(battle)(pos =>
      battle.copy(group = battle.group.copy(allies = battle.group.allies :+ elf.copy(position = pos))))
}
