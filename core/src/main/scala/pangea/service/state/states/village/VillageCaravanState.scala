package pangea.service.state.states.village

import io.circe.Json
import io.circe.syntax.EncoderOps
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
import pangea.model.village.{CaravanAmbush, CaravanTrip, VillageContract, VillageRates}
import pangea.repository.inventory.InventoryRepository
import pangea.service.schedule.Scheduler
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{AppButton, CharacterMenu, NpcQuestLog, State, UserAction, Villages}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Караван деревни — куда ведёт карточка «Контракта деревни», из лабиринта,
  * когда староста просил проводить обоз в соседний город (см. [[Villages]],
  * `VillageQuest.CaravanEscort`).
  *
  * У ворот — обоз и двое караванщиков; видно, насколько опасна дорога.
  * «Выступить» — в путь. Путь — [[VillageRates.CaravanLegs]] перехода, каждый
  * по 3–6 минут — «афк», как поляна цветов: переход идёт сам (задача
  * планировщика `TaskKind.VillageCaravan`), у каждого свой текст — где идёт
  * караван и как близок город. В конце перехода — засада с шансом
  * [[VillageRates.CaravanAmbushPct]]%; засад не было — на последнем она будет
  * наверняка. Засада — сюжетный бой без «Сбежать», за героя бьются
  * караванщики, награда как за обычный бой уровня задания. Добыча возвращает
  * сюда, к следующему переходу. «Персонаж» — обычное меню: по возвращении
  * караван в пути, а кончившийся за это время переход кончается сразу.
  *
  * В обозе пять повозок — на первых местах ряда героя, «под» строем (см.
  * pangea.model.battle.Wagon): кто стоит на повозке, тот принимает удары за неё
  * и сбивает с неё огонь. Побитые едут дальше побитыми, огонь после боя
  * тушат. Не осталось ни одной — везти нечего, контракт сорван.
  *
  * Дошли — платит тот, кто принял обоз в городе, за то, что дошло: серебро и
  * опыт сразу, с его рассказом, и контракт выполнен — к старосте возвращаться не
  * нужно (так решил пользователь). Пал в засаде — караван разграблен, контракт
  * сорван (см. DeathState). «Бросить караван» в пути — тоже сорван, с
  * подтверждением. У ворот «Уйти» контракт не трогает: обоз подождёт;
  * «Отказаться от контракта» — как у логова. */
case class VillageCaravanState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  scheduler:     Scheduler,
  content:       SceneContent
) extends State {

  import VillageCaravanState._

  private val branch = new Branch(
    routes = Map(
      "VillageCaravanGo"         -> Target.Run { (u, _, r) => atCaravan(u, r)(c => depart(u, c, r)) },
      LegKey                     -> Target.Run { (u, _, r) => atCaravan(u, r)(c => legDone(u, c, r)) },
      "VillageCaravanLeave"      -> Target.Run { (u, _, r) => leave(u, r) },
      "VillageCaravanAbandon"    -> Target.Run { (u, _, r) => atCaravan(u, r)(c => askAbandon(u, c, r)) },
      "VillageCaravanAbandonYes" -> Target.Run { (u, _, r) => atCaravan(u, r)(_ => abandon(u, r)) },
      "VillageCaravanBack"       -> Target.Run { (u, _, r) => enter(u, r).as(StateType.VillageCaravan) },
      "VillageCaravanOut"        -> Target.Run { (u, _, _) => clear(u).as(StateType.Dungeon) },
      "OpenCharacter"            -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.VillageCaravan) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.VillageCaravan) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.Battle, StateType.HeroStats, StateType.VillageCaravan)

  /** Вход: контракта на караван нет — у ворот пусто; у ворот — обоз ждёт; в
    * пути — переход идёт (кончился, пока герой был в меню, — кончится сразу);
    * вернулись с добычи — следующий переход или город. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now      <- nowMs
      contract <- standing(user)
      trip     <- readTrip(user)
      _        <- contract match {
                    case None =>
                      clear(user) *> renderer.show(user, Screen(content.text("village.caravan.gone"), List(outChoice)))
                    case Some(c) => trip match {
                      case None                       => renderer.show(user, gatesScreen(c))
                      // Из засады вернулись без единой повозки — везти нечего.
                      case Some(t) if t.plundered     => lose(user, renderer).unit
                      case Some(t) if t.arrived       => arrive(user, c, t, renderer).unit
                      case Some(t) if t.nextAt <= 0L  => startLeg(user, c, t, now, renderer).unit
                      case Some(t) =>
                        scheduler.schedule(user.userId, t.nextAt.max(now), TaskKind.VillageCaravan,
                          StateType.VillageCaravan, LegAction) *>
                          renderer.show(user, roadScreen(c, t, legStart = false))
                    }
                  }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Контракт и сцена ────────────────────────────────────────────────────────

  /** Контракт на караван, пока он не дошёл. */
  private def standing(user: User): Task[Option[VillageContract]] =
    NpcQuestLog.load(heroDao, user.userId).map(q => q.village.filter(_ => Villages.toCaravan(q)))

  /** Кнопка каравана — пока он не дошёл; иначе показать, что сейчас. */
  private def atCaravan(user: User, renderer: Renderer)(f: VillageContract => Task[StateType]): Task[StateType] =
    standing(user).flatMap {
      case Some(c) => f(c)
      case None    => enter(user, renderer).as(StateType.VillageCaravan)
    }

  private def readTrip(user: User): Task[Option[CaravanTrip]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[CaravanTrip].toOption))

  private def writeTrip(user: User, trip: CaravanTrip): Task[Unit] =
    heroDao.writeSceneData(user.userId, trip.asJson)

  // ── Экраны ──────────────────────────────────────────────────────────────────

  /** У ворот: обоз, караванщики, сложность и что ждёт в пути. */
  private def gatesScreen(c: VillageContract): Screen =
    Screen(
      Villages.joined(content.format("village.caravan.intro", Villages.args(content, c): _*), difficulty(c.level),
        content.text("village.caravan.noEscape") + "\n" + content.text("village.caravan.wagonsHint")),
      List(
        content.choice("VillageCaravanGo", "village.caravan.goLabel").copy(row = Some(0)),
        content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(1)),
        content.choice("VillageCaravanLeave", "village.lair.leaveLabel").copy(row = Some(1)),
        content.choice("VillageCaravanAbandon", "village.lair.abandonLabel").copy(row = Some(2))))

  /** В пути: в начале перехода — где идёт караван (`legStart`), иначе — какой
    * переход идёт. Переход кончится сам — рядом с «Персонажем» кнопка
    * приложения. */
  private def roadScreen(c: VillageContract, t: CaravanTrip, legStart: Boolean): Screen = {
    val text =
      if (legStart) legText(c, t.leg)
      else content.format("village.caravan.onRoad", "leg" -> t.leg.toString, "legs" -> VillageRates.CaravanLegs.toString)
    // Сколько повозок ещё в обозе — путь без повозок этого не показывает.
    val carts = t.intact.fold("")(n => "\n" + content.format("village.caravan.wagonsLeft",
                  "n" -> n.toString, "total" -> VillageRates.CaravanWagons.toString))
    Screen(text + carts, List(
      content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(0)),
      AppButton.choice(content, row = Some(0)),
      content.choice("VillageCaravanAbandon", "village.caravan.abandonLabel").copy(row = Some(1))))
  }

  /** Где идёт караван на этом переходе и как близок город: `village.caravan.legs` по порядку. */
  private def legText(c: VillageContract, leg: Int): String = {
    val legs = content.list("village.caravan.legs")
    legs.lift((leg - 1).max(0).min(legs.size - 1)).fold("")(line => format(line, c))
  }

  /** Переход прошёл тихо: строки `village.caravan.quiet` по очереди. */
  private def quietLine(leg: Int): String = {
    val lines = content.list("village.caravan.quiet")
    lines.lift(Math.floorMod(leg - 1, lines.size.max(1))).getOrElse("")
  }

  /** Подставить в строку из списка то же, что и в прочие тексты деревни. */
  private def format(line: String, c: VillageContract): String =
    Villages.args(content, c).foldLeft(line) { case (s, (k, v)) => s.replace(s"{$k}", v) }

  private def difficulty(level: Int): String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(level.max(1)))

  private def outChoice = content.choice("VillageCaravanOut", "village.toDungeonLabel")

  // ── Путь ────────────────────────────────────────────────────────────────────

  /** «Выступить»: первый переход. Уже в пути — показать, что сейчас. */
  private def depart(user: User, c: VillageContract, renderer: Renderer): Task[StateType] =
    readTrip(user).flatMap {
      case Some(_) => enter(user, renderer).as(StateType.VillageCaravan)
      case None    => nowMs.flatMap(now => startLeg(user, c, CaravanTrip.start.copy(wagons = Some(Villages.caravanWagons(c))),
                        now, renderer))
    }

  /** Начать переход `t.leg`: где идёт караван — и таймер до конца перехода.
    * Момент — в сцене, чтобы пережить уход в меню. `note` — что было в конце
    * прошлого перехода. */
  private def startLeg(user: User, c: VillageContract, t: CaravanTrip, now: Long, renderer: Renderer,
                       note: String = ""): Task[StateType] =
    for {
      delta <- Random.nextLongBetween(VillageRates.CaravanLegMinMs, VillageRates.CaravanLegMaxMs + 1L)
      trip   = t.copy(nextAt = now + delta)
      _     <- writeTrip(user, trip)
      _     <- scheduler.schedule(user.userId, trip.nextAt, TaskKind.VillageCaravan, StateType.VillageCaravan, LegAction)
      _     <- renderer.show(user, roadScreen(c, trip, legStart = true).noted(note))
    } yield StateType.VillageCaravan

  /** Переход кончился: засада или тихо. Засад ещё не было, а переход последний —
    * засада будет. Переход ещё идёт (действие пришло раньше срока) — показать,
    * что сейчас. */
  private def legDone(user: User, c: VillageContract, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      trip <- readTrip(user)
      res  <- trip match {
                case Some(t) if !t.arrived && t.nextAt > 0L && t.nextAt <= now =>
                  Random.nextIntBetween(0, 100).flatMap { roll =>
                    val sure = t.leg >= VillageRates.CaravanLegs && t.ambushes == 0
                    if (sure || roll < VillageRates.CaravanAmbushPct) ambush(user, c, t, renderer)
                    else quiet(user, c, t, now, renderer)
                  }
                case _ => enter(user, renderer).as(StateType.VillageCaravan)
              }
    } yield res

  /** Тихий переход: строка об этом — и следующий переход или город. */
  private def quiet(user: User, c: VillageContract, t: CaravanTrip, now: Long, renderer: Renderer): Task[StateType] = {
    val next = t.copy(leg = t.leg + 1, nextAt = 0L)
    if (next.arrived) arrive(user, c, next, renderer, quietLine(t.leg))
    else startLeg(user, c, next, now, renderer, quietLine(t.leg))
  }

  /** Засада: кто — по жребию; сюжетный бой с ними уровня задания, караванщики —
    * в строю героя. Добыча вернёт сюда — к следующему переходу. */
  private def ambush(user: User, c: VillageContract, t: CaravanTrip, renderer: Renderer): Task[StateType] =
    for {
      hero     <- getHero(user)
      i        <- Random.nextIntBounded(CaravanAmbush.all.size)
      kind      = CaravanAmbush.all.lift(i).getOrElse(CaravanAmbush.Bandits)
      seed     <- Random.nextLong
      foes      = Villages.caravanAmbush(c, kind, Rng(seed))
      energies <- ZIO.foreach(foes)(startEnergy)
      plain     = SoloPveBattle.fromGroup(foes, hero, energies)
      // Повозки — «под» строем героя, со своими повадками засады; караванщики —
      // на свободные повозки.
      convoyed  = plain.copy(group = plain.group.copy(convoy = t.wagons.map(Villages.convoy(_, kind))))
      battle    = Villages.withGuards(convoyed, c, content.list("village.caravan.guards"))
                    .copy(story = Some(Villages.CaravanStory), lootLevel = Some(c.level.max(1).toLong))
      next      = t.copy(leg = t.leg + 1, ambushes = t.ambushes + 1, nextAt = 0L)
      routing   = LootData(Nil, Nil, returnState = Some(StateType.VillageCaravan), eventData = Some(next.asJson))
      _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _        <- heroDao.writeSceneData(user.userId, routing.asJson)
      _        <- renderer.show(user, Screen(
                    content.format(s"village.caravan.ambush.${kind.key}", Villages.args(content, c): _*), Nil))
    } yield StateType.Battle

  /** Караван в городе: платит тот, кто принял обоз, — за то, что дошло:
    * каждая потерянная повозка — её доля долой (путь без повозок — целиком),
    * и у серебра, и у дублонов. Плата и опыт сразу, с его рассказом; контракт
    * выполнен. Договор
    * стирается первым (см. Villages.pay): повторный конец перехода второй платы
    * не даст. */
  private def arrive(user: User, c: VillageContract, t: CaravanTrip, renderer: Renderer,
                     note: String = ""): Task[StateType] =
    if (t.plundered) lose(user, renderer)
    else {
      val total  = VillageRates.CaravanWagons
      val intact = t.intact.getOrElse(total).min(total)
      // Дублоны — тоже долей, с округлением вниз.
      val due    = c.copy(silver = c.silver * intact / total, doubloons = c.doubloons * intact / total)
      for {
        now  <- nowMs
        hero <- getHero(user)
        _    <- scheduler.cancel(user.userId, TaskKind.VillageCaravan)
        _    <- clear(user)
        paid <- Villages.pay(heroDao, inventoryRepo, user.userId, hero, due, now)
        (after, exp) = paid
        a     = Villages.args(content, due)
        lvlUp = if (after.lvl > hero.lvl) "\n" + content.format("npcQuests.common.levelUp", "level" -> after.lvl.toString) else ""
        count = if (intact >= total) content.text("village.caravan.delivered.all")
                else content.format("village.caravan.delivered.some", "n" -> intact.toString, "total" -> total.toString)
        _    <- renderer.show(user, Screen(
                  Villages.joined(note, content.format("village.caravan.arrival", a: _*), count,
                    content.format("village.pay.reward", ("exp" -> exp.toString) +: a: _*) + lvlUp,
                    content.text("village.questDone")),
                  List(outChoice)))
      } yield StateType.VillageCaravan
    }

  /** Повозок не осталось: везти нечего — контракт сорван, как если бы караван
    * бросили. */
  private def lose(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      _    <- scheduler.cancel(user.userId, TaskKind.VillageCaravan)
      _    <- Villages.abandon(heroDao, inventoryRepo, user.userId, hero)
      _    <- clear(user)
      _    <- renderer.show(user, Screen(content.text("village.caravan.lost"), List(outChoice)))
    } yield StateType.VillageCaravan

  // ── Уход и отказ ────────────────────────────────────────────────────────────

  /** «Уйти» — только у ворот: обоз подождёт, контракт при герое. В пути — только
    * бросить караван. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    readTrip(user).flatMap {
      case Some(_) => enter(user, renderer).as(StateType.VillageCaravan)
      case None    => clear(user) *> renderer.show(user, Screen(content.text("village.caravan.left"), Nil)).as(StateType.Dungeon)
    }

  /** У ворот — отказаться от контракта, как у логова; в пути — бросить караван. */
  private def askAbandon(user: User, c: VillageContract, renderer: Renderer): Task[StateType] =
    readTrip(user).flatMap { trip =>
      val (textKey, yesKey) =
        if (trip.isDefined) ("village.caravan.abandon.text", "village.caravan.abandon.yesLabel")
        else ("village.lair.abandon.text", "village.lair.abandon.yesLabel")
      renderer.show(user, Screen(content.format(textKey, Villages.args(content, c): _*), List(
        content.choice("VillageCaravanAbandonYes", yesKey).copy(row = Some(0)),
        content.choice("VillageCaravanBack", "village.lair.abandon.noLabel").copy(row = Some(0)))))
    }.as(StateType.VillageCaravan)

  /** Отказался или бросил: контракт сорван — береста уходит из сумки, договор —
    * из журнала, и деревни снова встречаются. */
  private def abandon(user: User, renderer: Renderer): Task[StateType] =
    for {
      trip <- readTrip(user)
      hero <- getHero(user)
      _    <- scheduler.cancel(user.userId, TaskKind.VillageCaravan)
      _    <- Villages.abandon(heroDao, inventoryRepo, user.userId, hero)
      _    <- clear(user)
      _    <- renderer.show(user, Screen(content.text(
                if (trip.isDefined) "village.caravan.abandon.done" else "village.lair.abandon.done"), Nil))
    } yield StateType.Dungeon

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

object VillageCaravanState {

  /** Конец перехода — действие задачи планировщика. */
  val LegKey: String    = "VillageCaravanLeg"
  val LegAction: String = s"""{"action":"$LegKey"}"""
}
