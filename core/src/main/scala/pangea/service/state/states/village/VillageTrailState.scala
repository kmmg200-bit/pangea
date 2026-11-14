package pangea.service.state.states.village

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.Monster
import pangea.model.quest.Difficulty
import pangea.model.schedule.TaskKind
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.model.village.{ArtifactTrail, VillageContract, VillageQuest, VillageRates}
import pangea.repository.inventory.InventoryRepository
import pangea.service.schedule.Scheduler
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{AppButton, CharacterMenu, NpcQuestLog, State, UserAction, Villages}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Погоня за ворами артефакта деревни — куда ведёт карточка «Контракта
  * деревни», из лабиринта, когда староста просил вернуть украденную святыню
  * (см. [[Villages]], `VillageQuest.StolenArtifact`).
  *
  * У ворот деревни — след воров, уходящий в изгородь, и видно, насколько опасно
  * дело. «Идти по следу» — погоня: от [[VillageRates.TrailLegsMin]] до
  * [[VillageRates.TrailLegsMax]] переходов (бросок в начале), каждый по 3–6
  * минут — «афк», как караван: переход идёт сам (задача планировщика
  * `TaskKind.VillageTrail`). Сколько их, игроку не говорим (так решил
  * пользователь): первый уводит в изгородь, средние идут по следу, последний —
  * к стану, и что воры близко, видно только по тексту.
  *
  * У стана — «Напасть»: сюжетный бой без «Сбежать» с бандой и вором, у которого
  * артефакт; отряд с героем, награда как за обычный бой уровня задания. Добыча
  * вернёт сюда с отметкой победы: артефакт у героя, дальше — к старосте за
  * платой, как после логова.
  *
  * Сойти со следа (в пути или у стана, с подтверждением) или пасть в стане —
  * след остыл: контракт при герое, но погоню начинать заново, от ворот.
  * «Отказаться от контракта» — как у логова. */
case class VillageTrailState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  scheduler:     Scheduler,
  content:       SceneContent
) extends State {

  import VillageTrailState._

  private val branch = new Branch(
    routes = Map(
      "VillageTrailGo"         -> Target.Run { (u, _, r) => atTrail(u, r)(c => depart(u, c, r)) },
      LegKey                   -> Target.Run { (u, _, r) => atTrail(u, r)(c => legDone(u, c, r)) },
      "VillageTrailFight"      -> Target.Run { (u, _, r) => atTrail(u, r)(c => fight(u, c, r)) },
      "VillageTrailLeave"      -> Target.Run { (u, _, r) => leave(u, r) },
      "VillageTrailQuit"       -> Target.Run { (u, _, r) => atTrail(u, r)(_ => askQuit(u, r)) },
      "VillageTrailQuitYes"    -> Target.Run { (u, _, r) => atTrail(u, r)(_ => quit(u, r)) },
      "VillageTrailAbandon"    -> Target.Run { (u, _, r) => atTrail(u, r)(c => askAbandon(u, c, r)) },
      "VillageTrailAbandonYes" -> Target.Run { (u, _, r) => atTrail(u, r)(_ => abandon(u, r)) },
      "VillageTrailBack"       -> Target.Run { (u, _, r) => enter(u, r).as(StateType.VillageTrail) },
      "VillageTrailOut"        -> Target.Run { (u, _, r) => out(u, r) },
      "OpenCharacter"          -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.VillageTrail) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.VillageTrail) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.Battle, StateType.HeroStats, StateType.VillageTrail)

  /** Вход: контракта на артефакт нет — здесь пусто; воры перебиты (добыча
    * положила отметку) — артефакт у героя; погоня не начата — у ворот; все
    * переходы пройдены — у стана; переход идёт — по следу (кончился, пока герой
    * был в меню, — кончится сразу). */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now      <- nowMs
      contract <- standing(user)
      data     <- heroDao.readSceneData(user.userId)
      trail     = data.flatMap(_.as[ArtifactTrail].toOption)
      _        <- contract match {
                    case None =>
                      clear(user) *> renderer.show(user, Screen(text("gone"), List(outChoice)))
                    case Some(c) if data.exists(won) => victory(user, c, renderer)
                    case Some(c) => trail match {
                      case None                => renderer.show(user, gatesScreen(c))
                      case Some(t) if t.atCamp => renderer.show(user, campScreen(c))
                      case Some(t) =>
                        scheduler.schedule(user.userId, t.nextAt.max(now), TaskKind.VillageTrail,
                          StateType.VillageTrail, LegAction) *>
                          renderer.show(user, trailScreen(text("onTrail")))
                    }
                  }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Контракт и сцена ────────────────────────────────────────────────────────

  /** Контракт на артефакт, пока он у воров. */
  private def standing(user: User): Task[Option[VillageContract]] =
    NpcQuestLog.load(heroDao, user.userId).map(q => q.village.filter(_ => Villages.toTrail(q)))

  /** Кнопка погони — пока артефакт у воров; иначе показать, что сейчас. */
  private def atTrail(user: User, renderer: Renderer)(f: VillageContract => Task[StateType]): Task[StateType] =
    standing(user).flatMap {
      case Some(c) => f(c)
      case None    => enter(user, renderer).as(StateType.VillageTrail)
    }

  private def readTrail(user: User): Task[Option[ArtifactTrail]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[ArtifactTrail].toOption))

  private def writeTrail(user: User, trail: ArtifactTrail): Task[Unit] =
    heroDao.writeSceneData(user.userId, trail.asJson)

  // ── Тексты и экраны ─────────────────────────────────────────────────────────

  /** Текст погони: `village.artifact.<key>`. */
  private def key(k: String): String = s"village.artifact.$k"

  private def text(k: String): String = content.text(key(k))

  /** У ворот: след воров, сложность и что сойти со следа — начинать заново. */
  private def gatesScreen(c: VillageContract): Screen =
    Screen(
      Villages.joined(content.format(key("intro"), Villages.args(content, c): _*), difficulty(c.level),
        text("trailHint")),
      List(
        content.choice("VillageTrailGo", key("goLabel")).copy(row = Some(0)),
        content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(1)),
        content.choice("VillageTrailLeave", "village.lair.leaveLabel").copy(row = Some(1)),
        content.choice("VillageTrailAbandon", "village.lair.abandonLabel").copy(row = Some(2))))

  /** По следу: что видно на нём. Переход кончится сам — рядом с «Персонажем»
    * кнопка приложения. Сколько ещё идти, не говорим. */
  private def trailScreen(body: String): Screen =
    Screen(body, List(
      content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(0)),
      AppButton.choice(content, row = Some(0)),
      content.choice("VillageTrailQuit", key("quitLabel")).copy(row = Some(1))))

  /** У стана: кто там, сложность и что из этого боя не сбежать. */
  private def campScreen(c: VillageContract): Screen =
    Screen(
      Villages.joined(content.format(key("camp"), Villages.args(content, c): _*), difficulty(c.level),
        content.text("village.lair.noEscape")),
      List(
        content.choice("VillageTrailFight", key("fightLabel")).copy(row = Some(0)),
        content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(1)),
        content.choice("VillageTrailQuit", key("quitLabel")).copy(row = Some(1)),
        content.choice("VillageTrailAbandon", "village.lair.abandonLabel").copy(row = Some(2))))

  /** Что видно на этом переходе: первый уводит от ворот в изгородь, последний
    * выводит к стану, средние — по очереди из `village.artifact.legs.middle`. */
  private def legText(c: VillageContract, t: ArtifactTrail): String =
    if (t.leg <= 1) content.format(key("legs.first"), Villages.args(content, c): _*)
    else if (t.lastLeg) content.format(key("legs.last"), Villages.args(content, c): _*)
    else {
      val middle = content.list(key("legs.middle"))
      middle.lift(Math.floorMod(t.leg - 2, middle.size.max(1))).fold("")(line => format(line, c))
    }

  /** Подставить в строку из списка то же, что и в прочие тексты деревни. */
  private def format(line: String, c: VillageContract): String =
    Villages.args(content, c).foldLeft(line) { case (s, (k, v)) => s.replace(s"{$k}", v) }

  private def difficulty(level: Int): String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(level.max(1)))

  private def outChoice = content.choice("VillageTrailOut", "village.toDungeonLabel")

  // ── Погоня ──────────────────────────────────────────────────────────────────

  /** «Идти по следу»: сколько переходов до стана — бросок, и первый из них. Уже
    * в пути — показать, что сейчас. */
  private def depart(user: User, c: VillageContract, renderer: Renderer): Task[StateType] =
    readTrail(user).flatMap {
      case Some(_) => enter(user, renderer).as(StateType.VillageTrail)
      case None =>
        for {
          now  <- nowMs
          legs <- Random.nextIntBetween(VillageRates.TrailLegsMin, VillageRates.TrailLegsMax + 1)
          res  <- startLeg(user, c, ArtifactTrail.start(legs, 0L), now, renderer)
        } yield res
    }

  /** Начать переход `t.leg`: что видно на следу — и таймер до конца перехода.
    * Момент — в сцене, чтобы пережить уход в меню. */
  private def startLeg(user: User, c: VillageContract, t: ArtifactTrail, now: Long, renderer: Renderer): Task[StateType] =
    for {
      delta <- Random.nextLongBetween(VillageRates.TrailLegMinMs, VillageRates.TrailLegMaxMs + 1L)
      trail  = t.copy(nextAt = now + delta)
      _     <- writeTrail(user, trail)
      _     <- scheduler.schedule(user.userId, trail.nextAt, TaskKind.VillageTrail, StateType.VillageTrail, LegAction)
      _     <- renderer.show(user, trailScreen(legText(c, trail)))
    } yield StateType.VillageTrail

  /** Переход кончился: следующий — или, если он был последним, стан воров.
    * Переход ещё идёт (действие пришло раньше срока) — показать, что сейчас. */
  private def legDone(user: User, c: VillageContract, renderer: Renderer): Task[StateType] =
    for {
      now   <- nowMs
      trail <- readTrail(user)
      res   <- trail match {
                 case Some(t) if !t.atCamp && t.nextAt > 0L && t.nextAt <= now =>
                   val next = t.copy(leg = t.leg + 1, nextAt = 0L)
                   if (next.atCamp) writeTrail(user, next) *> renderer.show(user, campScreen(c)).as(StateType.VillageTrail)
                   else startLeg(user, c, next, now, renderer)
                 case _ => enter(user, renderer).as(StateType.VillageTrail)
               }
    } yield res

  // ── Стан воров ──────────────────────────────────────────────────────────────

  /** Бой в стане: банда и вор с артефактом — уровня задания. Добыча вернёт сюда
    * с отметкой победы. Не у стана (старая кнопка) — показать, что сейчас. */
  private def fight(user: User, c: VillageContract, renderer: Renderer): Task[StateType] =
    readTrail(user).flatMap {
      case Some(t) if t.atCamp =>
        for {
          hero     <- getHero(user)
          seed     <- Random.nextLong
          foes      = Villages.thiefGang(c, Rng(seed))
          energies <- ZIO.foreach(foes)(startEnergy)
          battle    = SoloPveBattle.fromGroup(foes, hero, energies)
                        .copy(story = Some(Villages.ArtifactStory), lootLevel = Some(c.level.max(1).toLong))
          routing   = LootData(Nil, Nil, returnState = Some(StateType.VillageTrail), eventData = Some(wonMark))
          _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
          _        <- heroDao.writeSceneData(user.userId, routing.asJson)
          _        <- renderer.show(user, Screen(content.format(key("fight"), Villages.args(content, c): _*), Nil))
        } yield StateType.Battle
      case _ => enter(user, renderer).as(StateType.VillageTrail)
    }

  /** Воры перебиты: артефакт у героя — к старосте за платой. */
  private def victory(user: User, c: VillageContract, renderer: Renderer): Task[Unit] =
    for {
      _ <- clear(user)
      _ <- Villages.lairCleared(heroDao, user.userId, VillageQuest.StolenArtifact)
      _ <- renderer.show(user, Screen(
             Villages.joined(content.format(key("victory"), Villages.args(content, c): _*),
               content.text("village.lair.returnHint")),
             List(outChoice)))
    } yield ()

  // ── Уход и отказ ────────────────────────────────────────────────────────────

  /** «Уйти» — только у ворот, пока погоня не начата: след подождёт, контракт при
    * герое. В пути и у стана — только сойти со следа. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    readTrail(user).flatMap {
      case Some(_) => enter(user, renderer).as(StateType.VillageTrail)
      case None    => clear(user) *> renderer.show(user, Screen(text("left"), Nil)).as(StateType.Dungeon)
    }

  /** Сойти со следа — с подтверждением: погоню потом начинать заново. */
  private def askQuit(user: User, renderer: Renderer): Task[StateType] =
    readTrail(user).flatMap {
      case None => enter(user, renderer).as(StateType.VillageTrail)
      case Some(_) =>
        renderer.show(user, Screen(text("quit.text"), List(
          content.choice("VillageTrailQuitYes", key("quit.yesLabel")).copy(row = Some(0)),
          content.choice("VillageTrailBack", "village.lair.abandon.noLabel").copy(row = Some(0))))).as(StateType.VillageTrail)
    }

  /** Сошёл со следа: погоня кончилась, контракт при герое. */
  private def quit(user: User, renderer: Renderer): Task[StateType] =
    readTrail(user).flatMap {
      case None => enter(user, renderer).as(StateType.VillageTrail)
      case Some(_) =>
        scheduler.cancel(user.userId, TaskKind.VillageTrail) *> clear(user) *>
          renderer.show(user, Screen(text("quit.done"), Nil)).as(StateType.Dungeon)
    }

  private def askAbandon(user: User, c: VillageContract, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.format("village.lair.abandon.text", Villages.args(content, c): _*), List(
      content.choice("VillageTrailAbandonYes", "village.lair.abandon.yesLabel").copy(row = Some(0)),
      content.choice("VillageTrailBack", "village.lair.abandon.noLabel").copy(row = Some(0))))).as(StateType.VillageTrail)

  /** Отказался: контракт и договор уходят, погоня кончается — деревни снова
    * встречаются. */
  private def abandon(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      _    <- scheduler.cancel(user.userId, TaskKind.VillageTrail)
      _    <- Villages.abandon(heroDao, inventoryRepo, user.userId, hero)
      _    <- clear(user)
      _    <- renderer.show(user, Screen(content.text("village.lair.abandon.done"), Nil))
    } yield StateType.Dungeon

  /** «В лабиринт» — после победы или с пустого следа. Посреди погони эта кнопка
    * старая: показать, что сейчас. */
  private def out(user: User, renderer: Renderer): Task[StateType] =
    readTrail(user).flatMap {
      case Some(_) => enter(user, renderer).as(StateType.VillageTrail)
      case None    => clear(user).as(StateType.Dungeon)
    }

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  private def startEnergy(m: Monster): Task[Long] =
    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct))

  private def clear(user: User): Task[Unit] = heroDao.writeSceneData(user.userId, Json.Null)

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object VillageTrailState {

  /** Конец перехода по следу — действие задачи планировщика. */
  val LegKey: String    = "VillageTrailLeg"
  val LegAction: String = s"""{"action":"$LegKey"}"""

  /** Отметка в scene_data, что воры в стане перебиты: её пишет добыча, читает
    * вход. Ключ свой: в scene_data лежат и чужие структуры. */
  val WonKey: String = "villageArtifactWon"

  val wonMark: Json = Json.fromJsonObject(JsonObject(WonKey -> true.asJson))

  def won(sceneData: Json): Boolean = sceneData.hcursor.get[Boolean](WonKey).toOption.contains(true)
}
