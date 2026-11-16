package pangea.service.state.states.registration

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, BattleBody, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{BeltSlot, FlaskKind, GemKind, ItemDetails, Rarity => ItemRarity}
import pangea.model.monster.Rarity
import pangea.model.skill.Skill
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{PrologueFight, UserAction}
import pangea.service.state.states.battle.BattleState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestJournal, TestPlayers, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Бой перед прологом: «Начать» ставит героя в прежнее тело Лео — ассасина
  * 25-го уровня с четырьмя умениями и легендарным поясом, — рядом Элиас и три
  * шестёрки гильдии, против — десять человек дворянской стражи. Проиграть
  * нельзя ни одной тактикой, Элиас не падает, а настоящий герой после боя тот
  * же, что до него; дальше — пролог как был. */
object PrologueFightSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def act(id: String, data: Map[String, String] = Map.empty): UserAction =
    UserAction("", Some((Map("action" -> id) ++ data).asJson.noSpaces))

  /** Герой только что зарегистрирован: первый уровень, ничего на нём нет. */
  private val fresh: Hero = TestFixtures.hero(userId, StateType.Registration)

  private final case class World(reg: RegistrationState, battle: BattleState, dao: TestHeroDao,
                                 renderer: TestRenderer, content: SceneContent)

  private def world: Task[World] =
    for {
      content  <- ZIO.attempt(SceneContent.load())
      dao      <- TestHeroDao.withHero(userId, fresh)
      renderer <- TestRenderer.make
      journal  <- TestJournal.make
      inv       = TestInventoryRepository.accepting
    } yield World(RegistrationState(new TestPlayers, dao, inv, TestItemRepository.make, journal, content),
                  BattleState(dao, inv, TestItemRepository.make, content), dao, renderer, content)

  private def battleOf(dao: TestHeroDao): Task[Option[SoloPveBattle]] =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def heroOf(dao: TestHeroDao): Task[Hero] = dao.getHeroByUserId(userId).map(_.get)

  /** Как жмёт кнопки игрок: только «Атака», только «Ждать» — или с умом:
    * фляга целителя, когда худо, готовые умения, остальное — «Атака». */
  private sealed trait Tactic
  private object Tactic {
    case object Attack extends Tactic
    case object Wait   extends Tactic
    case object Smart  extends Tactic
    case object Naive  extends Tactic
  }

  private val SkillOrder: List[Skill] = List(Skill.BladeWhirl, Skill.CunningStrike, Skill.QuickStrike)

  private def pick(tactic: Tactic, b: SoloPveBattle, real: Hero): UserAction = tactic match {
    case Tactic.Wait   => act("Wait")
    case Tactic.Attack => act("Attack")
    case Tactic.Naive =>
      val worn  = b.body.get.on(real)
      val belt  = worn.equipment.belt.details match {
        case ItemDetails.Belt(slots) => slots
        case _                       => Nil
      }
      val heal  = belt.indexWhere {
        case f: BeltSlot.Flask => f.kind == FlaskKind.Healer && f.charges > 0
        case _                 => false
      }
      if (!b.consumableUsedThisRound && heal >= 0 && worn.fightStats.hp * 100L < worn.effectiveMaxHp(0L) * 35L)
        act("BeltUse", Map("slot" -> heal.toString))
      else act("Attack")
    case Tactic.Smart =>
      val worn   = b.body.get.on(real)
      val maxHp  = worn.effectiveMaxHp(0L)
      val belt   = worn.equipment.belt.details match {
        case ItemDetails.Belt(slots) => slots
        case _                       => Nil
      }
      val healer = belt.indexWhere {
        case f: BeltSlot.Flask => f.kind == FlaskKind.Healer && f.charges > 0
        case _                 => false
      }
      val ready  = b.skillSlots.filter(s => s.cooldown <= 0 && worn.fightStats.energy >= s.skill.energyCost(worn))
      val armorLow = worn.fightStats.armor * 2L < worn.effectiveMaxArmor(0L)
      val skill  = (SkillOrder ++ Option.when(armorLow)(Skill.Reinforcement).toList)
                     .flatMap(k => ready.find(_.skill == k)).headOption
      if (!b.consumableUsedThisRound && healer >= 0 && worn.fightStats.hp * 100L < maxHp * 45L)
        act("BeltUse", Map("slot" -> healer.toString))
      else skill.fold(act("Attack"))(s => act(s"Skill_${s.itemId}"))
  }

  /** Нажать; спросил бой цель — выбрать первую из предложенных. Ход не
    * случился (бить некого: напротив пусто, а соседи не по руке) — «Ждать»,
    * как сделал бы игрок. */
  private def press(w: World, ua: UserAction): Task[StateType] =
    for {
      before <- w.dao.readActiveBattle(userId)
      _      <- w.renderer.reset
      state  <- w.battle.action(testUser, ua, w.renderer)
      shown  <- w.renderer.sentScreens
      pressed = BattleState.parseAction(ua)
      asked   = shown.lastOption.toList.flatMap(_.choices)
                  .find(c => c.data.contains("target") && pressed.contains(c.id))
      next   <- asked.fold[Task[StateType]](ZIO.succeed(state))(c => w.battle.action(testUser, act(c.id, c.data), w.renderer))
      after  <- w.dao.readActiveBattle(userId)
      moved  <- if (next == StateType.Battle && after == before) w.battle.action(testUser, act("Wait"), w.renderer)
                else ZIO.succeed(next)
    } yield moved

  /** Итог боя одной тактикой. */
  private final case class Run(state: StateType, turns: Int, minHpPct: Long, eliasStood: Boolean,
                               deaths: Int)

  private val TurnLimit = 150

  private def play(w: World, tactic: Tactic): Task[Run] =
    ZIO.iterate(Run(StateType.Battle, 0, 100L, eliasStood = true, deaths = 0))(r =>
      r.state == StateType.Battle && r.turns < TurnLimit) { r =>
      for {
        b     <- battleOf(w.dao).someOrFail(new Throwable("боя нет"))
        real  <- heroOf(w.dao)
        st    <- press(w, pick(tactic, b, real))
        after <- battleOf(w.dao)
        pct    = after.flatMap(_.body).map(body =>
                   body.fightStats.hp * 100L / body.on(real).effectiveMaxHp(0L).max(1L)).getOrElse(r.minHpPct)
        elias  = after.forall(_.group.allies.exists(a => a.kind == AllyKind.BrotherhoodAgent && a.alive))
      } yield Run(st, r.turns + 1, r.minHpPct.min(pct), r.eliasStood && elias,
                  r.deaths + (if (st == StateType.Death) 1 else 0))
    }

  private def started(seed: Long): Task[World] =
    for {
      _  <- TestRandom.setSeed(seed)
      w  <- world
      st <- w.reg.action(testUser, act("PrologueFight"), w.renderer)
      _  <- ZIO.when(st != StateType.Battle)(ZIO.fail(new Throwable(s"не бой: $st")))
      _  <- w.battle.enter(testUser, w.renderer)
    } yield w

  override def spec = suite("Бой перед прологом")(

    test("«Начать» ведёт в бой: Лео 25-го уровня, четыре умения, огненные клинки и легендарные фляги") {
      for {
        w       <- world
        _       <- w.reg.action(testUser, UserAction("привет", None), w.renderer)
        welcome <- w.renderer.sentScreens.map(_.last)
        _       <- w.renderer.reset
        st      <- w.reg.action(testUser, act("PrologueFight"), w.renderer)
        lead    <- w.renderer.sentScreens.map(_.map(_.text).mkString("\n"))
        b       <- battleOf(w.dao).map(_.get)
        real    <- heroOf(w.dao)
        body     = b.body.get
        worn     = body.on(real)
        belt     = body.equipment.belt.details match {
                     case ItemDetails.Belt(slots) => slots
                     case _                       => Nil
                   }
        rubies   = (body.equipment.weapon.sockets ++ body.equipment.leftHand.sockets).flatten
      } yield assertTrue(welcome.choices.map(_.id) == List("PrologueFight") && st == StateType.Battle) &&
              assertTrue(lead.contains("дома Ардевилей") && lead.contains("Элиас")) &&
              assertTrue(body.lvl == 25L && body.sparePct == PrologueFight.SparePct && b.story.contains(PrologueFight.Story)) &&
              assertTrue(b.skillSlots.map(_.skill).toSet ==
                Set[Skill](Skill.QuickStrike, Skill.CunningStrike, Skill.BladeWhirl, Skill.Reinforcement)) &&
              assertTrue(belt.collect { case f: BeltSlot.Flask => (f.kind, f.rarity, f.charges) } ==
                List(FlaskKind.Healer, FlaskKind.Smith, FlaskKind.Vigor, FlaskKind.Fire).map(k => (k, ItemRarity.Orange, 10))) &&
              assertTrue(rubies.nonEmpty && rubies.forall(_.kind == GemKind.Ruby) && worn.gems.weaponElements.nonEmpty) &&
              // Полное тело: HP, броня и энергия — до потолков, около легендарного человека 25-го.
              assertTrue(body.fightStats.hp == worn.effectiveMaxHp(0L) && body.fightStats.hp >= 3000L &&
                body.fightStats.armor == worn.effectiveMaxArmor(0L) && body.fightStats.armor >= 2500L &&
                body.fightStats.energy == worn.maxEnergy(0L)) &&
              // Сам герой — каким был: первый уровень, ничего не надето.
              assertTrue(real == fresh)
    },

    test("строй: Элиас у плеча Лео, равный ему и со страховкой, три шестёрки-«рабы»; против — десять стражников, все с разными именами") {
      for {
        w      <- world
        _      <- w.reg.action(testUser, act("PrologueFight"), w.renderer)
        b      <- battleOf(w.dao).map(_.get)
        scene  <- w.dao.readSceneData(userId)
        allies  = b.group.allies
        elias   = allies.find(_.kind == AllyKind.BrotherhoodAgent).get
        lackeys = allies.filter(_.kind == AllyKind.GuildLackey)
        foes    = (b.activeSlot :: b.group.others) ++ b.group.queue
        legend  = pangea.generator.monster.MonsterGenerator.generateOfRaceAndRarity(25, pangea.model.monster.Race.Human,
                    Rarity.Legendary).fightStats
      } yield assertTrue(elias.name == "Элиас" && elias.position == b.group.heroPos + 1 &&
                elias.stats == legend && elias.spare == legend.hp * PrologueFight.SparePct / 100L) &&
              assertTrue(lackeys.size == 3 && lackeys.forall(a => a.name == "Шестёрка гильдии" && a.spare == 0L &&
                a.stats.hp < elias.stats.hp / 3L)) &&
              assertTrue(foes.size == 10 && foes.map(_.name).distinct.size == 10 &&
                b.activeSlot.name == "Капитан Ортвин" && b.activeSlot.rarity == PrologueFight.CaptainRarity.entryName &&
                b.group.heroPos == 1 && b.group.activePos == 1 && lackeys.map(_.position).sorted == List(3, 4, 5) &&
                foes.forall(f => f.lvl == 25L && f.race == pangea.model.monster.Race.Human.entryName)) &&
              assertTrue(scene.flatMap(_.hcursor.downField("returnState").as[String].toOption).contains("Registration"))
    },

    test("ни одна тактика не проигрывает: бой кончается, Лео не ниже страховки, Элиас стоит, настоящий герой не тронут") {
      val fights = List.range(1L, 7L).flatMap(seed => List(Tactic.Smart, Tactic.Naive, Tactic.Attack).map(seed -> _)) ++
                     List.range(1L, 4L).map(_ -> Tactic.Wait)
      for {
        runs <- ZIO.foreach(fights) { case (seed, tactic) =>
                  for {
                    w     <- started(seed * 7919L)
                    run   <- play(w, tactic)
                    real  <- heroOf(w.dao)
                    scene <- w.dao.readSceneData(userId)
                    left  <- battleOf(w.dao)
                  } yield (tactic, run, real, scene, left)
                }
      } yield assertTrue(runs.forall { case (_, r, _, _, left) => r.state == StateType.Registration && left.isEmpty }) &&
              assertTrue(runs.forall { case (_, r, _, _, _) => r.deaths == 0 && r.minHpPct >= PrologueFight.SparePct }) &&
              assertTrue(runs.forall { case (_, r, _, _, _) => r.eliasStood }) &&
              assertTrue(runs.forall { case (_, _, real, _, _) => real == fresh }) &&
              assertTrue(runs.forall { case (_, _, _, scene, _) => PrologueFight.won(scene) })
    },

    test("после победы — строка после боя и первый пассаж пролога одним сообщением, дальше всё как было") {
      for {
        w      <- started(42L)
        run    <- play(w, Tactic.Smart)
        _      <- w.renderer.reset
        _      <- w.reg.enter(testUser, w.renderer)
        shown  <- w.renderer.sentScreens
        scene  <- w.dao.readSceneData(userId)
        _      <- w.reg.action(testUser, act("P2_Dagger"), w.renderer)
        dagger <- w.renderer.sentScreens.map(_.last)
      } yield assertTrue(run.state == StateType.Registration) &&
              assertTrue(shown.size == 1 && shown.head.text.contains("Элорин будет доволен") &&
                shown.head.text.contains("Всякая жизнь заканчивается смертью") &&
                shown.head.choices.map(_.id) == List("P2_Dagger")) &&
              assertTrue(!PrologueFight.won(scene)) &&
              assertTrue(dagger.text.contains("Прости, брат"))
    },

    test("старая кнопка «Начать» ведёт в пролог сразу, а вход в регистрацию без победы ничего не показывает") {
      for {
        w     <- world
        _     <- w.reg.enter(testUser, w.renderer)
        quiet <- w.renderer.sentScreens
        _     <- w.reg.action(testUser, act("P1_Intro"), w.renderer)
        intro <- w.renderer.sentScreens.map(_.last)
        b     <- battleOf(w.dao)
      } yield assertTrue(quiet.isEmpty && b.isEmpty) &&
              assertTrue(intro.text.contains("Всякая жизнь заканчивается смертью"))
    },

    test("страховка союзника: удар, рана, взрыв — не ниже своей доли; под ней — не ниже того, что есть; без неё — до нуля") {
      val a = BattleAlly(AllyKind.BrotherhoodAgent, 2, hp = 1000L, armor = 0L, energy = 0L,
                stats = pangea.model.stats.FightStats(atk = 1, hp = 1000, armor = 0, defence = 0, evasion = 0,
                  accuracy = 0, energy = 0), lvl = 25L, spare = 150L)
      assertTrue(a.lose(300L) == 700L && a.lose(5000L) == 150L && a.copy(hp = 100L).lose(50L) == 100L) &&
      // Под двумя страховками потеря вдвое мягче: 1000 − 800 — не 200, а 250.
      assertTrue(a.lose(800L) == 250L && a.copy(hp = 250L).lose(100L) == 200L) &&
      assertTrue(a.copy(spare = 0L).lose(5000L) == 0L && a.lose(-5L) == 1000L)
    },

    test("бой и союзник, записанные до пролога с боем, читаются: тела нет, страховки нет") {
      for {
        w <- world
        _ <- w.reg.action(testUser, act("PrologueFight"), w.renderer)
        b <- battleOf(w.dao).map(_.get)
      } yield {
        val stored = b.asJson
        val old    = stored.mapObject(_.remove("body")).hcursor.downField("group").downField("allies")
                       .withFocus(_.mapArray(_.map(_.mapObject(_.remove("spare"))))).top
        val read   = old.flatMap(_.as[SoloPveBattle].toOption)
        assertTrue(read.exists(r => r.body.isEmpty && r.group.allies.forall(_.spare == 0L))) &&
        assertTrue(b.body.flatMap(body => body.asJson.as[BattleBody].toOption).contains(b.body.get)) &&
        assertTrue(Json.obj().hcursor.get[Option[BattleBody]]("body").contains(None))
      }
    }
  )
}
