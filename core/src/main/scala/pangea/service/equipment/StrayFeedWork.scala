package pangea.service.equipment

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.hero.HeroId
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.quest.NpcQuests
import pangea.model.user.UserId
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{NpcQuestLog, QuestGoods, StrayCasketQuest}
import zio.{Task, ZIO}

/** Кормление бродячего ларца камнями с карточки — те же правила и слова, что
  * в боте. */
final class StrayFeedWork(heroDao: HeroDao, inventoryRepo: InventoryRepository, artifacts: ArtifactRepository,
                          content: SceneContent) {

  /** Кнопки кормления по качеству камней при герое — `grade` и подпись. */
  def choices(goods: List[QuestGoods.Good]): List[(Int, String)] = {
    val labels = content.list("strayCasket.feed.grades")
    StrayCasketQuest.grades(goods).flatMap { case (grade, n) =>
      labels.lift(grade - 1).map(l => grade -> l.replace("{n}", n.toString))
    }
  }

  /** Скормить ларцу `casketId` камень качества `grade`. Итог — что сказать;
    * `None` — не ларец, не голоден, нет камня или качество устарело. */
  def feed(userId: UserId, heroId: HeroId, casketItem: Item, grade: Int): Task[Option[String]] =
    if (!casketItem.questItem.contains(QuestItemKind.StrayCasket)) ZIO.none
    else
      for {
        quests <- NpcQuestLog.load(heroDao, userId)
        out    <- if (!StrayCasketQuest.hungry(quests)) ZIO.none
                  else
                    for {
                      goods <- QuestGoods.all(inventoryRepo, Some(artifacts), heroId)
                      res   <- StrayCasketQuest.pick(goods, grade) match {
                                 case None => ZIO.none
                                 case Some(chosen) =>
                                   StrayCasketQuest.feed(heroDao, inventoryRepo, Some(artifacts), userId, heroId, chosen)
                                     .map(_.map(noteAfter(_, chosen)))
                               }
                    } yield res
      } yield out

  private def noteAfter(next: NpcQuests, good: QuestGoods.Good): String = {
    val n      = StrayCasketQuest.fed(next)
    val eaten  = content.format("strayCasket.feed.eaten", "gem" -> good.item.name,
                   "fed" -> n.toString, "goal" -> StrayCasketQuest.GemsGoal.toString)
    val memory = StrayCasketQuest.memoryAfter(n).flatMap(i => content.list("strayCasket.memories").lift(i - 1))
    val sated  = Option.when(StrayCasketQuest.leads(next))(content.text("strayCasket.sated"))
    (List(eaten) ++ memory.toList ++ sated.toList).mkString("\n\n")
  }
}