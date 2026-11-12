package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.{Achievement, Hero}
import pangea.model.quest.{NpcQuest, QuestOutcome}
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.schedule.Scheduler
import pangea.service.state.{AppButton, BramblQuest, CityExit, LostSquadQuest, NpcQuestLog, QuestOutcomeLog, QuestSupport, State,
  StoryBrandQuest, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Городская стройка. На входе — меню из трёх работ разной длительности; после
  * выбора игрок «занят» (как [[events.SilverVeinState]]): scene_data хранит вид
  * работы и старт, поллер по таймеру (`Construction`) выдаёт серебро и
  * возвращает в GlobalMap. Кнопка «Уйти» — досрочное прерывание с
  * подтверждением.
  *
  * Формула серебра: `lvl(героя) × часы`, домноженное на `100 ± d` процентов, где
  * `d ∈ [10, 20]` (знак и величина роллятся отдельно).
  *
  * Работами заправляет прораб Гуннар Отвес — своя кнопка в меню: кто он и что
  * говорит по ходу «Пропавшей дружины»; ящик дружины несут ему.
  *
  * За складами — мастерская Оттара Медноуса, отца Брамбла: пока слепок с
  * гробницы предка у героя («Замок для предков», см. BramblQuest), в меню
  * кнопка к нему — он куёт замок из материалов героя.
  */
case class ConstructionState(
  heroDao: HeroDao,
  scheduler: Scheduler,
  content: SceneContent,
  // Сумка — чтобы забрать ящик «Пропавшей дружины», когда его отдают Гуннару.
  inventoryRepo: Option[InventoryRepository] = None,
  // Ящик отдан — исход «Пропавшей дружины» (см. QuestOutcome).
  outcomes: Option[QuestOutcomeDao] = None,
  // Хранилище вещей — выковать замок Медноусов («Замок для предков»): без
  // него замок не создать, и кнопки к мастерской нет.
  itemRepo: Option[ItemRepository] = None
) extends State {
  import ConstructionState._

  private val branch = new Branch(
    routes = Map(
      "CarryStone" -> Target.Run { (user, _, renderer) =>
        startJob(user, renderer, Job.CarryStone)
      },
      "BreakWall" -> Target.Run { (user, _, renderer) =>
        startJob(user, renderer, Job.BreakWall)
      },
      "BuildWall" -> Target.Run { (user, _, renderer) =>
        startJob(user, renderer, Job.BuildWall)
      },
      "LeaveWork" -> Target.Run { (user, _, renderer) =>
        leaveWork(user, renderer)
      },
      "ConfirmLeaveWork" -> Target.Run { (user, _, renderer) =>
        confirmLeave(user, renderer)
      },
      "CancelLeaveWork" -> Target.Run { (user, _, renderer) =>
        enter(user, renderer).as(StateType.Construction)
      },
      "Finish" -> Target.Run((user, _, renderer) => finish(user, renderer)),
      "Foreman" -> Target.Run((user, _, renderer) => offShift(user, renderer)(showForeman(user, renderer))),
      "SquadCrate" -> Target.Run((user, _, renderer) => offShift(user, renderer)(handOverCrate(user, renderer))),
      "BackFromForeman" -> Target.Run { (user, _, renderer) =>
        enter(user, renderer).as(StateType.Construction)
      },
      "BramblForge"     -> Target.Run((user, _, renderer) => offShift(user, renderer)(showForge(user, renderer))),
      "BramblForgeGive" -> Target.Run((user, _, renderer) => offShift(user, renderer)(forgeLock(user, renderer))),
      "LeaveConstruction" -> Target.Goto(StateType.MarketSquare),
      CityExit.route
    ),
    fallback = Target.Run { (user, _, renderer) =>
      enter(user, renderer).as(StateType.Construction)
    }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.Loot

  // Вход: активная работа в scene_data → экран работы, иначе — меню выбора.
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    activeJob(user).flatMap {
      case Some(_) => showWork(user, renderer)
      case None    => showMenu(user, renderer)
    }

  override def action(
      user: User,
      ua: UserAction,
      renderer: Renderer
  ): Task[StateType] = branch.act(user, ua, renderer)

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      // Слепок с гробницы у героя — к отцу Брамбла, в мастерскую за складами.
      val forge = Option.when(forgeOpen(quests))(content.choice("BramblForge", "bramblLock.forge.label"))
      renderer.show(
        user,
        Screen(
          content.text("construction.menu.text"),
          List(
            content.choice(
              "CarryStone",
              "construction.carryStoneLabel",
              "duration" -> formatHours(Job.CarryStone.hours)
            ),
            content.choice(
              "BreakWall",
              "construction.breakWallLabel",
              "duration" -> formatHours(Job.BreakWall.hours)
            ),
            content.choice(
              "BuildWall",
              "construction.buildWallLabel",
              "duration" -> formatHours(Job.BuildWall.hours)
            ),
            content.choice("Foreman", "construction.foremanLabel")
          ) ++ forge.toList ++ List(
            content.choice("LeaveConstruction", "construction.back"),
            CityExit.button(content)
          )
        )
      )
    }

  /** Мастерская Медноусов открыта: слепок на руках, и замок есть из чего создать. */
  private def forgeOpen(quests: pangea.model.quest.NpcQuests): Boolean =
    itemRepo.isDefined && inventoryRepo.isDefined && BramblQuest.toFather(quests)

  /** Оттар Медноус узнаёт руку сына на слепке и называет цену замка; материалы
    * при герое — «Отдать материалы». Не тот шаг — меню. */
  private def showForge(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      hero   <- getHero(user)
      res    <- (inventoryRepo, forgeOpen(quests)) match {
                  case (Some(inv), true) =>
                    // Цену Оттар называет сам; материалов при герое не хватает —
                    // кнопки «Отдать» нет, как у собеседников главы 3.
                    StoryBrandQuest.canPay(inv, hero, BramblQuest.LockPrice).flatMap { can =>
                      renderer.show(user, StoryBrandQuest.askScreen(content, content.text("bramblLock.forge.text"),
                        "BramblForgeGive", "bramblLock.forge.giveLabel", can,
                        content.choice("BackFromForeman", "construction.back")))
                    }.as(StateType.Construction)
                  case _ => enter(user, renderer).as(StateType.Construction)
                }
    } yield res

  /** Отдать материалы: Оттар куёт замок — он в сумку, слепок остаётся у него.
    * Не хватает чего-то — ничего не списано, экран мастерской снова. */
  private def forgeLock(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      hero   <- getHero(user)
      res    <- (inventoryRepo, itemRepo) match {
                  case (Some(inv), Some(items)) if forgeOpen(quests) =>
                    BramblQuest.forge(heroDao, inv, items, user.userId, hero).flatMap {
                      case false => showForge(user, renderer)
                      case true =>
                        renderer.show(user, Screen(content.text("bramblLock.forge.done") + "\n\n" +
                          content.format("marisa.questItemAdded", "item" -> pangea.model.item.QuestItemKind.FamilyLock.displayName),
                          Nil)) *> enter(user, renderer).as(StateType.Construction)
                    }
                  case _ => enter(user, renderer).as(StateType.Construction)
                }
    } yield res

  /** Прораб Гуннар Отвес: кто он и что говорит — по ходу «Пропавшей дружины»
    * (см. [[LostSquadQuest.foremanLine]]). Ящик дружины на руках — он его примет. */
  private def showForeman(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      val line = LostSquadQuest.foremanLine(quests).getOrElse("construction.foreman.idle")
      renderer.show(user, Screen(
        content.text("construction.foreman.about") + "\n\n" + content.text(line),
        Option.when(LostSquadQuest.crateOnHands(quests))(content.choice("SquadCrate", "lostSquad.foreman.crateLabel")).toList :+
          content.choice("BackFromForeman", "construction.back")
      )).as(StateType.Construction)
    }

  /** Пока идёт смена, к прорабу не отлучиться: старая кнопка покажет экран
    * работы. Ящик, отданный посреди смены, стёр бы её из scene_data — и серебро
    * за неё вместе с ней. */
  private def offShift(user: User, renderer: Renderer)(run: => Task[StateType]): Task[StateType] =
    activeJob(user).flatMap {
      case Some(_) => showWork(user, renderer).as(StateType.Construction)
      case None    => run
    }

  /** Ящик «Пропавшей дружины» — Гуннару: он узнаёт свою руду и всё понимает.
    * Серебро — сразу в руки, кусок железа — экраном добычи; задание закрыто,
    * «Расхититель штолен». */
  private def handOverCrate(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!LostSquadQuest.crateOnHands(quests)) enter(user, renderer).as(StateType.Construction)
                else for {
                  _    <- heroDao.updateSilver(user.userId, hero.silver + LostSquadQuest.CrateSilver)
                  _    <- renderer.show(user, Screen(content.format("lostSquad.foreman.crate",
                            "silver" -> LostSquadQuest.CrateSilver.toString), Nil))
                  line <- inventoryRepo.fold(
                            NpcQuestLog.modify(heroDao, user.userId)(_.finish(NpcQuest.LostSquad)).as(content.text("lostSquad.questDone")))(
                            repo => LostSquadQuest.finish(heroDao, repo, content, user.userId, hero))
                  stats <- QuestOutcomeLog.recordAndCount(outcomes, content, hero, QuestOutcome.SquadLooted)
                  _    <- renderer.show(user, Screen(QuestOutcomeLog.withStats(line, stats), Nil))
                  _    <- QuestSupport.grant(heroDao, content, user, hero, Achievement.MineLooter, renderer)
                  _    <- heroDao.writeSceneData(user.userId, LootData(items = List(MaterialGenerator.item(LostSquadQuest.CrateBonus)),
                            silvers = Nil, returnState = Some(StateType.Construction),
                            header = Some(content.text("lostSquad.foreman.ironHeader"))).asJson)
                } yield StateType.Loot
    } yield res

  // Локальная склонения для подписи кнопки: «1 час», «4 часа», «8 часов».
  private def formatHours(h: Int): String = {
    val word = h % 10 match {
      case 1 if h % 100 != 11 => "час"
      case 2 | 3 | 4 if !(h % 100 >= 12 && h % 100 <= 14) => "часа"
      case _                                              => "часов"
    }
    s"$h $word"
  }

  private def showWork(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(
      user,
      // Работа идёт сама — рядом с «Уйти» кнопка приложения, «В город» ниже.
      Screen(
        content.text("construction.work.text"),
        AppButton.inRow(content, content.screen("construction.work").choices) :+ CityExit.button(content, Some(1))
      )
    )

  private def startJob(
      user: User,
      renderer: Renderer,
      job: Job
  ): Task[StateType] =
    activeJob(user).flatMap {
      // Защита от перезапуска: если работа уже идёт, просто возвращаем её экран.
      case Some(_) => showWork(user, renderer).as(StateType.Construction)
      case None =>
        for {
          now <- nowMs
          _ <- heroDao.writeSceneData(
            user.userId,
            Json.obj(
              KindKey      -> job.entryName.asJson,
              StartedAtKey -> now.asJson
            )
          )
          _ <- scheduler.schedule(
            user.userId,
            now + job.durationMs,
            TaskKind.Construction,
            StateType.Construction,
            FinishAction
          )
          _ <- showWork(user, renderer)
        } yield StateType.Construction
    }

  // Ручной выход: до конца — confirm с остатком времени; после — сам завершаем.
  private def leaveWork(user: User, renderer: Renderer): Task[StateType] =
    for {
      now <- nowMs
      job <- activeJob(user)
      result <- job match {
        case None => enter(user, renderer).as(StateType.Construction)
        case Some((j, start)) if now - start >= j.durationMs =>
          finish(user, renderer)
        case Some((j, start)) =>
          val remaining = formatRemaining(j.durationMs - (now - start))
          renderer
            .show(
              user,
              Screen(
                content.format(
                  "construction.confirmLeave.text",
                  "remaining" -> remaining
                ),
                content.screen("construction.confirmLeave").choices
              )
            )
            .as(StateType.Construction)
      }
    } yield result

  // Досрочный выход: серебра нет, снимаем задачу, чистим scene_data.
  private def confirmLeave(user: User, renderer: Renderer): Task[StateType] =
    scheduler.cancel(user.userId, TaskKind.Construction) *>
      heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer
        .show(user, Screen(content.text("construction.left"), Nil))
        .as(StateType.MarketSquare)

  // Завершение работы — выдача серебра и финальный текст. Возврат в GlobalMap.
  private def finish(user: User, renderer: Renderer): Task[StateType] =
    activeJob(user).flatMap {
      case None => ZIO.succeed(StateType.MarketSquare)
      case Some((job, _)) =>
        for {
          now  <- nowMs
          hero <- getHero(user)
          base = hero.lvl.toLong * job.hours.toLong
          delta <- Random.nextIntBetween(MinSpreadPct, MaxSpreadPct + 1)
          sign  <- Random.nextBoolean.map(if (_) 1 else -1)
          reward = ((base * (100L + sign * delta.toLong)) / 100L).max(1L)
          // Опыт: уровень героя × множитель работы (0.5 / 1 / 2 за 1 / 4 / 8 ч), вверх.
          expGained = hero.expFrom(math.ceil(hero.lvl.toDouble * job.expMult).toLong, now)
          leveled   = hero.gainExp(expGained)
          _ <- heroDao.updateSilver(user.userId, hero.silver + reward)
          _ <- heroDao.updateExpAndLevel(
            user.userId,
            leveled.exp,
            leveled.lvl,
            leveled.upgradePoints
          )
          _ <- ZIO.when(job.doubloons > 0)(
            heroDao.updateDoubloons(
              user.userId,
              hero.doubloons + job.doubloons.toLong
            )
          )
          _ <- scheduler.cancel(user.userId, TaskKind.Construction)
          _ <- heroDao.writeSceneData(user.userId, Json.Null)
          doubloonSuffix =
            if (job.doubloons > 0)
              " " + content.text("construction.doneDoubloons")
            else ""
          _ <- renderer.show(
            user,
            Screen(
              content.format(
                "construction.done",
                "silver" -> reward.toString,
                "exp"    -> expGained.toString
              ) + doubloonSuffix,
              Nil
            )
          )
          _ <- ZIO.when(leveled.lvl > hero.lvl)(
            renderer.show(
              user,
              Screen(
                content.format(
                  "construction.levelUp",
                  "level" -> leveled.lvl.toString
                ),
                Nil
              )
            )
          )
        } yield StateType.MarketSquare
    }

  private def activeJob(user: User): Task[Option[(Job, Long)]] =
    heroDao.readSceneData(user.userId).map { data =>
      for {
        json  <- data
        kind  <- json.hcursor.get[String](KindKey).toOption
        job   <- Job.fromName(kind)
        start <- json.hcursor.get[Long](StartedAtKey).toOption
      } yield (job, start)
    }

  private def nowMs: Task[Long] =
    ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao
      .getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def formatRemaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val h    = secs / 3600
    val m    = (secs % 3600) / 60
    if (h > 0) s"${h}ч ${m}мин"
    else if (m > 0) s"${m}мин"
    else s"${secs}с"
  }
}

object ConstructionState {
  import enumeratum._

  sealed abstract class Job(val hours: Int, val doubloons: Int, val expMult: Double)
      extends EnumEntry {
    val durationMs: Long = hours.toLong * 3600L * 1000L
  }
  object Job extends Enum[Job] {
    val values = findValues
    case object CarryStone extends Job(1, doubloons = 0, expMult = 0.5)
    case object BreakWall  extends Job(4, doubloons = 0, expMult = 1.0)
    case object BuildWall  extends Job(8, doubloons = 1, expMult = 2.0)
    def fromName(name: String): Option[Job] = withNameOption(name)
  }

  val MinSpreadPct: Int = 10
  val MaxSpreadPct: Int = 20

  private val KindKey      = "constructionKind"
  private val StartedAtKey = "constructionStartedAt"
  private val FinishAction = """{"action":"Finish"}"""
}
