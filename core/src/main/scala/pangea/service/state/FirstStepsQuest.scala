package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.{Hero, LoreData}
import pangea.model.home.HomeData
import pangea.model.quest.{FirstSteps, NpcQuests, QuestOutcome}
import pangea.model.user.User
import zio.{Task, ZIO}

/** Как задание «Встать на ноги» выглядит на экране (см. [[FirstSteps]]): пункты
  * с отметками — ✅ сделано, ⬜ ещё нет; всё сделано — подсказка вернуться
  * домой. Засчитывается задание дома: при входе, когда все пункты выполнены, —
  * сцена с мыслями героя, своя для каждого дома, и «Задание выполнено». */
object FirstStepsQuest {

  /** Пункты задания, по строке на каждый. */
  def objectives(content: SceneContent, hero: Hero, quests: NpcQuests, home: HomeData = HomeData.empty): String = {
    val p = FirstSteps.progress(hero, quests, home)
    def mark(ok: Boolean): String = if (ok) "✅" else "⬜"
    List(
      s"${mark(p.strong)} ${content.format("firstSteps.strong", "lvl" -> FirstSteps.TargetLvl.toString)}",
      s"${mark(p.house)} ${content.text("firstSteps.house")}",
      s"${mark(p.city)} ${content.format("firstSteps.city",
        "done" -> p.cityDone.toString, "total" -> FirstSteps.CityQuests.size.toString)}"
    ).++(Option.when(p.done)(content.text("firstSteps.goHome"))).mkString("\n")
  }

  /** Вход в свой дом: все пункты выполнены, а задание ещё не засчитано — сцена
    * по дому (домик, добротный дом, остров), «Задание выполнено» и — под ним —
    * в каком доме закончили задание другие герои (исход — дом, см.
    * [[QuestOutcome]]); задание засчитано, счёт исследований до первой зацепки
    * основной линии — с нуля. Иначе — ничего. */
  def completeAtHome(heroDao: HeroDao, content: SceneContent, user: User, home: HomeData, renderer: Renderer,
                     outcomes: Option[QuestOutcomeDao] = None): Task[Unit] =
    for {
      hero   <- heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
                  .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
      lore   <- heroDao.readLoreData(user.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- ZIO.foreachDiscard(home.house.filter(_ => !lore.firstStepsDone && FirstSteps.progress(hero, quests, home).done)) { h =>
                  for {
                    _     <- heroDao.writeLoreData(user.userId, lore.copy(firstStepsDone = true, cluePasses = 0).asJson)
                    stats <- ZIO.foreach(QuestOutcome.find(FirstSteps.Key, h.key))(o =>
                               QuestOutcomeLog.recordAndCount(outcomes, content, hero, o)).map(_.flatten)
                    _     <- renderer.show(user, Screen(
                               content.text(s"firstSteps.done.${h.key}") + "\n\n" +
                                 QuestOutcomeLog.withStats(content.text("firstSteps.doneLine"), stats), Nil))
                  } yield ()
                }
    } yield ()

  /** Сообщение о том, что задание получено. */
  def taken(content: SceneContent, hero: Hero, quests: NpcQuests): String =
    content.format("firstSteps.taken",
      "title"      -> content.text("firstSteps.title"),
      "objectives" -> objectives(content, hero, quests))
}
