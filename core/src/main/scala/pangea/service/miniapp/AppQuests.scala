package pangea.service.miniapp

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.miniapp.MiniAppView.{QuestActView, QuestAskView}
import pangea.service.state.{NpcQuestLog, QuestCard, SceneDoor}
import zio.{Ref, Task, ZIO}

/** Кнопка задания с карточки сюжетного предмета — из приложения: те же кнопки,
  * условия и слова, что в боте ([[QuestCard]]). Где герой — по его сцене, а
  * если в чате открыт «Персонаж» — по тому, откуда его открыли. Само задание
  * идёт в чате: в его сцену ведёт дверь бота ([[SceneDoor]]), а приложению —
  * что продолжение там. Лок игрока держит вызывающий. */
final class AppQuests(heroDao: HeroDao, inventories: InventoryRepository, card: QuestCard, content: SceneContent,
                      door: SceneDoor) {
  import AppQuests.Outcome

  /** Нажали кнопку `act` на карточке предмета `itemId` из сумки. Кнопка не та,
    * что сейчас на карточке, или предмета уже нет — она устарела. */
  def run(user: User, hero: Hero, itemId: Long, act: String): Task[Outcome] =
    for {
      inv    <- inventories.get(hero.id).mapError(e => new Throwable(e.toString))
      quests <- NpcQuestLog.load(heroDao, user.userId)
      back   <- heroDao.readReturnState(user.userId)
      stale   = Outcome.said(content.text("miniapp.quest.stale"))
      out    <- inv.items.data.find(i => i.id == itemId && QuestCard.offers(content, i, quests, act)) match {
                  case None => ZIO.succeed(stale)
                  case Some(_) =>
                    card.go(act, hero, quests, QuestCard.place(hero.state, back)) match {
                      case QuestCard.Go.Stale            => ZIO.succeed(stale)
                      case QuestCard.Go.Refused(text, _) => ZIO.succeed(Outcome.said(text))
                      case QuestCard.Go.Ask(screen) =>
                        ZIO.succeed(Outcome(None, chat = false, Some(QuestAskView(screen.text,
                          screen.choices.map(c => QuestActView(c.id, MiniAppView.bare(c.label)))))))
                      case QuestCard.Go.Here(run) =>
                        for {
                          lines <- Ref.make(Vector.empty[String])
                          _     <- run(user, new MiniApp.Collect(lines))
                          said  <- lines.get
                        } yield Outcome(Option.when(said.nonEmpty)(said.mkString("\n\n")), chat = false, None)
                      case QuestCard.Go.Enter(scene, prep) =>
                        prep(user, door.chat) *>
                          door.enter(user, hero.state, scene).as(Outcome(Some(content.text("miniapp.quest.toChat")),
                            chat = true, None))
                    }
                }
    } yield out
}

object AppQuests {

  /** Что вышло: что сказать (`note`), ушло ли задание дальше в чат (`chat`),
    * что спросить прежде (`ask`). */
  final case class Outcome(note: Option[String], chat: Boolean, ask: Option[QuestAskView])

  object Outcome {
    def said(text: String): Outcome = Outcome(Some(text), chat = false, None)
  }
}
