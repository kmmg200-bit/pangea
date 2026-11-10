package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{Item, ItemSet, QuestItemKind}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** Общее для сюжетных заданий: сюжетные предметы в сумке (положить, если ещё
  * нет; забрать) и выдача достижений. Сами задания — [[MarisaQuest]],
  * [[MurlocQuest]]. */
object QuestSupport {

  /** Есть ли у героя такой сюжетный предмет. */
  def hasItem(items: List[Item], kind: QuestItemKind): Boolean = items.exists(_.questItem.contains(kind))

  /** Положить сюжетный предмет, если его ещё нет. Возвращает, положили ли. */
  def giveItem(inventoryRepo: InventoryRepository, itemRepo: ItemRepository, hero: Hero, kind: QuestItemKind): Task[Boolean] =
    for {
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      given <- if (hasItem(inv.items.data, kind)) ZIO.succeed(false)
               else itemRepo.persist(hero.id, QuestItemKind.item(kind))
                      .flatMap(it => inventoryRepo.addItem(hero.id, it).mapError(e => new Throwable(e.toString)))
                      .as(true)
    } yield given

  /** Забрать из сумки все сюжетные предметы этих видов. */
  def removeItems(inventoryRepo: InventoryRepository, hero: Hero, kinds: Set[QuestItemKind]): Task[Unit] =
    for {
      inv <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      ids  = inv.items.data.filter(_.questItem.exists(kinds.contains)).map(_.id).toSet
      _   <- ZIO.when(ids.nonEmpty)(inventoryRepo.removeItems(ids, hero.id).mapError(e => new Throwable(e.toString)))
    } yield ()

  /** Набор собран целиком — все двенадцать вещей надеты разом: за каждый такой
    * набор своё достижение, один раз и навсегда (см. [[Achievement.forSet]]).
    * Зовётся после всякого надевания: только надевание и может собрать набор. */
  def grantFullSets(heroDao: HeroDao, content: SceneContent, user: User, hero: Hero, renderer: Renderer): Task[Hero] =
    ZIO.foldLeft(ItemSet.values.toList.filter(s => hero.sets.pieces(s) >= Achievement.FullSetPieces)
                   .flatMap(Achievement.forSet))(hero) {
      (h, a) => grant(heroDao, content, user, h, a, renderer)
    }

  /** Вехи — убитые, уровень героя (см. [[Achievement.milestonesDue]]): порог
    * пройден, а достижения ещё нет — выдать. Зовётся после каждого хода
    * игрока: уровень и убитые приходят из многих мест, и проверять по самому
    * герою надёжнее, чем помнить о каждом. */
  def grantMilestones(heroDao: HeroDao, content: SceneContent, user: User, hero: Hero, renderer: Renderer): Task[Hero] =
    ZIO.foldLeft(Achievement.milestonesDue(hero))(hero)((h, a) => grant(heroDao, content, user, h, a, renderer))

  /** Достижение — один раз; строка о нём показывается сразу. */
  def grant(heroDao: HeroDao, content: SceneContent, user: User, hero: Hero, a: Achievement, renderer: Renderer): Task[Hero] =
    if (hero.hasAchievement(a)) ZIO.succeed(hero)
    else {
      val updated = hero.withAchievement(a)
      val line    = content.format("marisa.achievement", "title" -> a.title, "bonus" -> a.bonusLine)
      val note    = Notes.get(a).map(key => "\n\n" + content.text(key)).getOrElse("")
      heroDao.updateAchievements(user.userId, updated.achievements) *>
        renderer.show(user, Screen(line + note, Nil)).as(updated)
    }

  /** Достижения, у которых под строкой о получении — своя приписка. */
  private val Notes: Map[Achievement, String] = Map(Achievement.Hasty -> "chat.grant.hastyNote")
}
