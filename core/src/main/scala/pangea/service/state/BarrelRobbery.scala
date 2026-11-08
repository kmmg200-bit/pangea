package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.Difficulty
import pangea.model.user.User
import pangea.repository.barrel.BarrelRepository
import pangea.repository.item.ItemRepository
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Жребий на кражу из бочки («Вскрытая бочка», [[BarrelThievesQuest]]): бот
  * бросает его, когда герой открывает бочку, Mini App — когда из приложения
  * впервые за сутки берутся за бочку. Чаще раза в сутки не бросают. Улика —
  * новая вещь, поэтому нужен `items`. */
final class BarrelRobbery(heroDao: HeroDao, barrelRepo: BarrelRepository, items: ItemRepository, content: SceneContent) {

  /** Бросить жребий. Выпало — гоблины уносят всё, на дне остаётся улика,
    * задание на первом шаге. Возвращает, вскрыли ли бочку сейчас. */
  def roll(user: User, hero: Hero): Task[Boolean] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      quests <- NpcQuestLog.load(heroDao, user.userId)
      barrel <- barrelRepo.get(hero.id).mapError(asThrowable)
      robbed <- if (!BarrelThievesQuest.canRoll(hero, quests, barrel, now)) ZIO.succeed(false)
                else for {
                  roll <- Random.nextIntBounded(100)
                  hit   = roll < BarrelThievesQuest.ChancePct
                  _    <- if (!hit) barrelRepo.markRolled(hero.id, now).mapError(asThrowable)
                          else for {
                            pick <- Random.nextIntBounded(Int.MaxValue)
                            gang  = BarrelThievesQuest.pickGang(BarrelThievesQuest.firstTime(hero),
                                      content.list(BarrelThievesQuest.GangsKey).size, pick)
                            clue <- items.persist(hero.id, QuestItemKind.item(QuestItemKind.BarrelClue))
                            _    <- barrelRepo.steal(hero.id, clue, now).mapError(asThrowable)
                            _    <- NpcQuestLog.save(heroDao, user.userId, BarrelThievesQuest.started(quests, gang, now))
                          } yield ()
                } yield hit
    } yield robbed

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object BarrelRobbery {

  /** Вскрытая бочка: что случилось (`robbedNow` — только что), сложность по
    * уровню героя и куда идти. */
  def text(content: SceneContent, hero: Hero, robbedNow: Boolean): String =
    content.text(if (robbedNow) "barrelThieves.robbed" else "barrelThieves.stillRobbed") + "\n\n" +
      content.format("marisa.difficulty", "difficulty" -> Difficulty.render(hero.lvl.toInt)) + "\n\n" +
      content.text("barrelThieves.askAround")
}
