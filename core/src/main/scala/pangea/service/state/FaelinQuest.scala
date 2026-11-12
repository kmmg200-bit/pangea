package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** «Отданный лук» — личное задание Фаэлина Тихострела (см. [[MercQuests]]).
  * Берётся разговором за столом наёмников: лук у Фаэлина срубленный, а не
  * отданный — изгородь не отдала ветвь тому, кто так и не выучился петь, и он
  * срезал её сам. За это он и ушёл из рода; теперь лук трескается. Срубленное
  * возвращают туда, где резали: с карточки лука, из лабиринта, — к старым
  * корням изгороди. Там лесорубы гильдии Чернолом валят изгородь по
  * гильдейскому приказу — бой разом с пятнадцатью гоблинами, Фаэлин рядом;
  * отбились — изгородь
  * забирает срубленный лук и отдаёт ему новый, готовый — отданный: она слушает
  * не только песню. Задание закрыто, Фаэлин растёт. Сам ход — [[NpcQuest.FaelinBow]] в
  * `npc_quests`; сцены — таверна (MercenariesState) и старые корни
  * (FaelinRootsState). */
object FaelinQuest {

  /** Сложность задания — ⚰️🦴🦴🦴🦴. По ней же уровень врагов у корней. */
  val Level: Int = 14

  /** Ключ сюжетного боя у корней: без «Сбежать», а награда как за обычный бой. */
  val RootsStory: String = "faelinRoots"

  /** У корней — лесорубы гильдии Чернолом, пятнадцать разом (мест у чужой
    * стороны ровно столько — встают все, без очереди): десять простых — по силе
    * как воры из «Вскрытой бочки», — четверо мифических и последним десятник,
    * именной легендарный. */
  val Loggers: Int          = 10
  val LoggerRarity: Rarity  = BarrelThievesQuest.GangRarity
  val Foremen: Int          = 4
  val ForemanRarity: Rarity = Rarity.Mythical
  val LeaderName: String    = "Скрежет Пила"

  // ── Ход задания ─────────────────────────────────────────────────────────────

  /** Задания у героя ещё не было: о нём можно заговорить. */
  def fresh(q: NpcQuests): Boolean = !q.isTaken(NpcQuest.FaelinBow) && !q.isDone(NpcQuest.FaelinBow)

  /** Срубленный лук на руках — вернуть изгороди. */
  def toRoots(q: NpcQuests): Boolean = q.onStep(NpcQuest.FaelinBow, 1)

  /** Герой согласился помочь: срубленный лук — в сумку, задание взято. Уже было
    * — ничего (false). */
  def take(heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
           userId: UserId, hero: Hero, nowMs: Long): Task[Boolean] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (!fresh(q)) ZIO.succeed(false)
      else QuestSupport.giveItem(inventory, items, hero, QuestItemKind.CutBow) *>
        NpcQuestLog.save(heroDao, userId, q.take(NpcQuest.FaelinBow, nowMs)).as(true)
    }

  // ── Бой у корней ────────────────────────────────────────────────────────────

  /** Лесорубы Чернолома уровня задания: впереди десять простых, за ними
    * четверо мифических, последним — десятник. */
  def foes: List[Monster] = {
    def goblin(rarity: Rarity, name: Option[String] = None): Monster =
      MonsterGenerator.generateOfRaceAndRarity(Level, Race.Goblin, rarity, name)
    List.fill(Loggers)(goblin(LoggerRarity)) ++ List.fill(Foremen)(goblin(ForemanRarity)) :+
      goblin(Rarity.Legendary, Some(LeaderName))
  }

  /** Бой у корней: отряд героя и Фаэлин со своим луком — он в строю или гостем
    * (см. `MercQuests.withMerc`). Бой сюжетный — без «Сбежать» и подкреплений,
    * — награда по уровню задания. */
  def rootsBattle(hero: Hero, enemies: List[Monster], energies: List[Long]): SoloPveBattle =
    MercQuests.withMerc(SoloPveBattle.fromGroup(enemies, hero, energies)
      .copy(story = Some(RootsStory), lootLevel = Some(Level.toLong)), hero, AllyKind.Elf)
}
