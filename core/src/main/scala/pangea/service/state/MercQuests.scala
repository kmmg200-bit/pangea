package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.{AllyKind, AllyRates}
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** Личные задания наёмников: у каждого, кто сидит за дальним столом таверны,
  * своё — его старый долг или его снаряжение, и в нём — его история. Берётся
  * разговором за столом, до найма («Поговорить» на карточке, см.
  * MercenariesState). Выполнено — потолок наёмника у этого героя растёт на
  * [[AllyRates.QuestGrowth]]: при каждом найме ([[grown]]) и сразу — у того,
  * кто уже при герое ([[grow]]). Пока задание есть у Йоргена ([[JorgenQuest]]). */
object MercQuests {

  /** Чьё это задание — если у наёмника оно есть. */
  def quest(kind: AllyKind): Option[NpcQuest] = kind match {
    case AllyKind.Human => Some(NpcQuest.JorgenDebt)
    case _              => None
  }

  /** Уровень его задания: сложность — в разговоре и на карточке предмета. */
  def level(kind: AllyKind): Option[Int] = kind match {
    case AllyKind.Human => Some(JorgenQuest.Level)
    case _              => None
  }

  /** Ключ его текстов в scenes.yaml. */
  def textBase(kind: AllyKind): Option[String] = kind match {
    case AllyKind.Human => Some("jorgen")
    case _              => None
  }

  /** Что наёмник отдаёт герою, когда тот согласился: с этим предметом задание и
    * ведётся (см. правило «задание ведёт предмет»). */
  def startItem(kind: AllyKind): Option[QuestItemKind] = kind match {
    case AllyKind.Human => Some(QuestItemKind.JorgenRoster)
    case _              => None
  }

  /** Взять его задание: предмет — в сумку, задание — на первый шаг. Нечего
    * брать или уже взято — false. */
  def take(kind: AllyKind, heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
           userId: UserId, hero: Hero, nowMs: Long): Task[Boolean] = kind match {
    case AllyKind.Human => JorgenQuest.take(heroDao, inventory, items, userId, hero, nowMs)
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
}
