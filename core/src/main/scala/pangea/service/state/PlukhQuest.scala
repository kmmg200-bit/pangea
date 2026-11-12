package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.skill.MonsterSkill
import pangea.model.squad.AllyKind
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** «Чужой багор» — личное задание Плюха (см. [[MercQuests]]). Берётся
  * разговором за столом наёмников: свой грозовой багор Плюх «одолжил» у
  * Сноходцев — те ловили им мурлоков, бьют молнией в воду, и мурлок всплывает, —
  * а теперь их печать на древке зовёт багор домой, и он трещит всё тише.
  * Срезанную печать — к Густаво: он когда-то варил для Сноходцев, пока не
  * узнал, на ком они пробуют составы, — освободил мурлоков из плена и ушёл
  * вместе с ними. Он знает их знаки и переворачивает зов — пусть зовёт не багор
  * домой, а ловчих к герою. С карточки печати, из лабиринта, — позвать их: бой
  * с ловчими-каджитами (глава именной, остальные мифические), у всех — кинжалы
  * молнии ([[MonsterSkill.LightningDagger]]); Плюх рядом. Кончились ловчие —
  * печать тускнеет, кинжалы Плюх берёт себе, багор его по праву, задание
  * закрыто, Плюх растёт. Сам ход — [[NpcQuest.PlukhGaff]] в
  * `npc_quests`; сцены — таверна (MercenariesState), лавка Густаво
  * (GustavoState) и ловчие (PlukhHuntState). */
object PlukhQuest {

  /** Сложность задания — ⚰️🦴🦴🦴🦴. По ней же уровень ловчих. */
  val Level: Int = 14

  /** Ключ сюжетного боя с ловчими: без «Сбежать», а награда как за обычный бой.
    * В этом бою каджиты мечут кинжалы молнии (см. MonsterSkill.LightningDagger). */
  val HuntStory: String = MonsterSkill.LightningDagger.Story

  /** Ловчие — каджиты: четверо мифических, и ведёт их глава — именной. */
  val Hunters: Int          = 4
  val HunterRarity: Rarity  = Rarity.Mythical

  /** Глава ловчих Сноходцев — его знак Плюх и срезал. */
  val LeaderName: String = "Ко'Ширр Ловчий"

  // ── Ход задания ─────────────────────────────────────────────────────────────

  /** Задания у героя ещё не было: о нём можно заговорить. */
  def fresh(q: NpcQuests): Boolean = !q.isTaken(NpcQuest.PlukhGaff) && !q.isDone(NpcQuest.PlukhGaff)

  /** Печать на руках — показать Густаво. */
  def toGustavo(q: NpcQuests): Boolean = q.onStep(NpcQuest.PlukhGaff, 1)

  /** Густаво перевернул зов — позвать ловчих, из лабиринта. */
  def toHunt(q: NpcQuests): Boolean = q.onStep(NpcQuest.PlukhGaff, 2)

  /** Герой согласился помочь: печать — в сумку, задание взято. Уже было —
    * ничего (false). */
  def take(heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
           userId: UserId, hero: Hero, nowMs: Long): Task[Boolean] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (!fresh(q)) ZIO.succeed(false)
      else QuestSupport.giveItem(inventory, items, hero, QuestItemKind.SnowwalkerSeal) *>
        NpcQuestLog.save(heroDao, userId, q.take(NpcQuest.PlukhGaff, nowMs)).as(true)
    }

  /** Густаво перевернул зов печати: дальше — к ловчим. Печать остаётся у героя. */
  def turned(q: NpcQuests): NpcQuests = q.update(NpcQuest.PlukhGaff)(_.copy(step = 2))

  // ── Бой с ловчими ───────────────────────────────────────────────────────────

  /** Ловчие уровня задания: четверо мифических каджитов и последним — их глава,
    * именной легендарный. */
  def hunters: List[Monster] =
    List.fill(Hunters)(MonsterGenerator.generateOfRaceAndRarity(Level, Race.Khajiit, HunterRarity)) :+
      MonsterGenerator.generateOfRaceAndRarity(Level, Race.Khajiit, Rarity.Legendary, Some(LeaderName))

  /** Бой с ловчими: отряд героя и Плюх со своим багром — он в строю или
    * гостем (см. `MercQuests.withMerc`). Бой сюжетный — без «Сбежать» и
    * подкреплений, — награда по уровню задания. */
  def huntBattle(hero: Hero, foes: List[Monster], energies: List[Long]): SoloPveBattle =
    MercQuests.withMerc(SoloPveBattle.fromGroup(foes, hero, energies)
      .copy(story = Some(HuntStory), lootLevel = Some(Level.toLong)), hero, AllyKind.Murloc)
}
