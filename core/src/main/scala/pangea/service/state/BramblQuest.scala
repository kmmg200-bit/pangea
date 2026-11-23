package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{MaterialKind, QuestItemKind}
import pangea.model.monster.MiniBoss
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.StoryBrandQuest.Price
import zio.{Task, ZIO}

/** «Замок для предков» — личное задание Брамбла Медноуса (см. [[MercQuests]]).
  * Берётся разговором за столом наёмников: доспех с нижнего горизонта Брамбл
  * снял с каменного предка в Зале предков, когда неделю копал наверх, а взятое
  * у предков по закону рода возвращают работой рода — замком. Род у него
  * замочники, сам он ремеслу не выучился. Слепок с гробницы — его отцу, Оттару
  * Медноусу, на Стройку: тот куёт замок из материалов героя. С замком — вниз,
  * в Зал предков: туда ведёт старая штольня под серебряной жилой (как в
  * «Пропавшей дружине»). У гробницы — двое каменных стражей; Брамбл рядом.
  * Отбились — замок на гробнице, задание закрыто, Брамбл растёт. Сам ход —
  * [[NpcQuest.BramblLock]] в `npc_quests`; сцены — таверна (MercenariesState),
  * Стройка (ConstructionState), серебряная жила (SilverVeinState) и Зал
  * предков (BramblHallState). */
object BramblQuest {

  /** Сложность задания — 11 ур. По ней же уровень каменных стражей. */
  val Level: Int = 11

  /** Ключ сюжетного боя в Зале предков: без «Сбежать», а награда как за
    * обычный бой — с каждого минибосса своя. */
  val HallStory: String = "bramblHall"

  /** Каменных стражей у гробницы двое. */
  val Guardians: Int = 2

  /** Что Оттар Медноус берёт за замок для предков: кусок железа, что не
    * остывает, — на тело замка, и магический камень — на сувальды. */
  val LockPrice: List[Price] = List(Price(MaterialKind.EverburningIron, 1), Price(MaterialKind.MagicStone, 1))

  // ── Ход задания ─────────────────────────────────────────────────────────────

  /** Задания у героя ещё не было: о нём можно заговорить. */
  def fresh(q: NpcQuests): Boolean = !q.isTaken(NpcQuest.BramblLock) && !q.isDone(NpcQuest.BramblLock)

  /** Слепок на руках — нести отцу Брамбла на Стройку. */
  def toFather(q: NpcQuests): Boolean = q.onStep(NpcQuest.BramblLock, 1)

  /** Замок на руках — вниз, в Зал предков. */
  def toHall(q: NpcQuests): Boolean = q.onStep(NpcQuest.BramblLock, 2)

  /** Под серебряной жилой откроется штольня к Залу предков: замок на руках. */
  def hallOpens(q: NpcQuests): Boolean = toHall(q)

  /** Герой согласился помочь: слепок — в сумку, задание взято. Уже было —
    * ничего (false). */
  def take(heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
           userId: UserId, hero: Hero, nowMs: Long): Task[Boolean] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (!fresh(q)) ZIO.succeed(false)
      else QuestSupport.giveItem(inventory, items, hero, QuestItemKind.BramblCast) *>
        NpcQuestLog.save(heroDao, userId, q.take(NpcQuest.BramblLock, nowMs)).as(true)
    }

  /** Оттар куёт замок: материалы уходят из сумки (россыпью, ровно сколько он
    * просит — см. `StoryBrandQuest.pay`), слепок он оставляет себе, замок — в
    * сумку, дальше — в Зал предков. Не тот шаг или материалов не хватает —
    * ничего не трогает (false). */
  def forge(heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
            userId: UserId, hero: Hero): Task[Boolean] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (!toFather(q)) ZIO.succeed(false)
      else StoryBrandQuest.pay(inventory, hero, LockPrice).flatMap {
        case false => ZIO.succeed(false)
        case true =>
          QuestSupport.removeItems(inventory, hero, Set(QuestItemKind.BramblCast)) *>
            QuestSupport.giveItem(inventory, items, hero, QuestItemKind.FamilyLock) *>
            NpcQuestLog.save(heroDao, userId, q.update(NpcQuest.BramblLock)(_.copy(step = 2))).as(true)
      }
    }

  // ── Бой в Зале предков ──────────────────────────────────────────────────────

  /** Бой у гробницы: оба каменных стража — минибоссы по уровню задания, у
    * каждого свой круг; отряд героя и Брамбл со своей секирой холода — он в
    * строю или гостем (см. `MercQuests.withMerc`). Бой сюжетный — без «Сбежать»
    * и подкреплений, — награда по уровню задания. */
  def hallBattle(hero: Hero): SoloPveBattle = {
    val fight = MercQuests.bossFight(MiniBoss.StoneElemental, Guardians, Level, hero)
    MercQuests.withMerc(fight.copy(story = Some(HallStory), lootLevel = Some(Level.toLong)), hero, AllyKind.Gnome)
  }
}
