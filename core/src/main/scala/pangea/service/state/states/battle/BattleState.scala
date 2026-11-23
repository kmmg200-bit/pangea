package pangea.service.state.states.battle

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.loot.LootGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{Aura, AuraRates, BattleAlly, BattleEffects, Bleed, Buff, Burn, Convoy, Element, Formation, GroupState, HealCurse, HealedWounds, MonsterSlot, Nature, Poison, PatternPull, PowderKeg, RaceTactics, Regen, RedSpider, SkillJam, SlainMonster, SoloPveBattle, SkillSlotState, SpiderWeb, TimedDefenceDebuff, Wagon, Ward}
import pangea.model.cave.CaveGenerator
import pangea.model.item.ItemSet
import pangea.model.squad.AllySkill
import pangea.model.hero.{Achievement, AzatState, CubeStatus, Hero, HeroSets, WeaponCoat, WeaponDust}
import pangea.model.item.QuestItemKind
import pangea.model.quest.{BoardKind, NpcQuest}
import pangea.model.item.{BeltSlot, BrewEffect, BrewKind, BrewRates, FlaskEffect, FlaskRates, GripRates, Item, ItemDetails, ItemType, PassiveKind, PotionKind, RefillSource, DivineKind, DivineRates, TrapNet}
import pangea.model.home.HomeData
import pangea.model.monster.{BossPercentDamage, Breed, MiniBoss, NamedTrophy, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.stats.{ParamsBuff, StatBoost}
import pangea.model.trauma.{Trauma, TraumaRoll}
import pangea.model.schedule.TaskKind
import pangea.model.skill.{MonsterEnergy, MonsterSkill, Skill}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.states.LootState
import pangea.service.state.MurlocQuest
import pangea.service.state.states.gustavo.GustavoState
import pangea.repository.bank.BankRepository
import pangea.repository.barrel.BarrelRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.battle.BattleState.Victim
import pangea.service.schedule.Scheduler
import pangea.service.state.{AzatData, BarrelThievesQuest, BeltContents, BigDealQuest, BoardProgress, BramblQuest, HedgeSongQuest, HotGoodsQuest,
  FaelinQuest, JorgenQuest, LostSquadQuest, PlukhQuest,
  MarisaQuest, NpcQuestLog, State, StoryFaceQuest, StoryListQuest, StoryMarkQuest, StoryPowderQuest, StoryToolQuest, UserAction, KillLogData,
  Villages}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

/** Бой строится как «прочитать всё в начале хода → чисто посчитать ход →
  * записать всё один раз в конце». Боевые функции ([[attackTurn]]/[[skillTurn]]/
  * [[beltTurn]]/[[fleeTurn]] и внутренние [[playerStrike]]/
  * [[monsterPhase]]/[[dealSkillDamage]]) НЕ трогают `heroDao`/`renderer` — они
  * только считают и накапливают лог сообщений ([[BattleState.TurnResult]]).
  * Единственная точка I/O — [[resolve]]/[[commit]]: она персистит итог хода
  * ровно один раз и показывает склеенный лог. Так исключён целый класс багов
  * «одна из веток забыла сохранить стат» (см. Кровавую жатву). */
case class BattleState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  // Таймер раундов без героя (он обнулён, отряд дерётся дальше).
  scheduler:     Scheduler = Scheduler.none,
  // Хранилище в Торговом доме и неприметная бочка: уникальный трофей не падает
  // второй раз, даже если первый лежит там.
  bank:          Option[BankRepository] = None,
  barrel:        Option[BarrelRepository] = None
) extends State {

  import BattleState.{AllyBlow, MobStrike, Outcome, TurnResult, VictoryOutcome}

  /** Чистое вычисление одного хода: снимок состояния → результат хода. */
  private type Turn = (Hero, SoloPveBattle, Long) => Task[TurnResult]

  private val branch = new Branch(
    routes = Map(
      "Attack"      -> Target.Run((u, ua, r) => attackRoute(u, BattleState.parseTarget(ua), r)),
      "Wait"        -> Target.Run((u, _, r) => resolve(u, r)(waitTurn)),
      // Тетива натянута, а бить некого — стрела уходит мимо.
      "LooseAim"    -> Target.Run((u, _, r) => resolve(u, r)(looseTurn)),
      "Move"        -> Target.Run((u, _, r) => moveRoute(u, r)),
      "MoveTo"      -> Target.Run((u, ua, r) => resolve(u, r)(moveTurn(BattleState.parseTarget(ua)))),
      "SquadTick"   -> Target.Run((u, _, r) => squadTick(u, r)),
      // Пояс ход не тратит: он присылает список расходников, а применяет уже
      // кнопка внутри того сообщения.
      "Belt"        -> Target.Run((u, _,  r) => showBelt(u, r)),
      "BeltUse"     -> Target.Run((u, ua, r) => beltUse(u, ua, r)),
      "UseDivine"    -> Target.Run((u, _, r) => resolve(u, r)(divineTurn)),
      "ThrowNet"     -> Target.Run((u, ua, r) => netRoute(u, BattleState.parseTarget(ua), r)),
      "BeltPick"     -> Target.Run((u, ua, r) => beltPick(u, ua, r)),
      "ThrowMix"     -> Target.Run((u, _, r) => resolve(u, r)(mixTurn)),
      "Miasma"      -> Target.Run((u, _, r) => toggleMiasma(u, r)),
      "Flee"        -> Target.Run((u, _, r) => flee(u, r)),
      "ConfirmFlee" -> Target.Run((u, _, r) => resolve(u, r)(fleeTurn)),
      "CancelFlee"  -> Target.Run((u, _, r) => showScreen(u, r).as(StateType.Battle)),
      "CancelTarget" -> Target.Run((u, _, r) => showScreen(u, r).as(StateType.Battle))
    ),
    fallback = Target.Run { (user, ua, renderer) =>
      // Кнопки способностей именуются Skill_<itemId> и роутятся динамически: id
      // указывает на конкретный предмет, поэтому два предмета с «одним» Skill
      // имеют независимые cd/uses. В группе к умению может прилагаться цель —
      // номер моба в строю (см. skillRoute).
      BattleState.parseSkillAction(ua) match {
        case Some(itemId) => skillRoute(user, itemId, BattleState.parseTarget(ua), renderer)
        case None         => showScreen(user, renderer).as(StateType.Battle)
      }
    }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    getBattle(user).flatMap { battle =>
      if (battle.group.heroDown) showDown(user, battle, renderer).unit
      else startingAllies(user, battle, renderer) *> showScreen(user, renderer)
    }

  /** Кто выходит к герою в начале боя: крыса («Крыса», порог 12) и мурлоки-
    * собратья («Мать Глубин», порог 12). «Начало боя» — пока не кончился первый
    * раунд: уход за снаряжением и возврат на том же раунде вторых не приводят, а
    * павших в бою они уже не заменяют. Раунды считает сам бой: счётчик группы в
    * бою с минибоссом стоит. */
  private def startingAllies(user: User, battle: SoloPveBattle, renderer: Renderer): Task[Unit] =
    if (battle.roundsFought > 0) ZIO.unit
    else getHero(user).flatMap { hero =>
      val (ratted, ratLines) = startingRat(hero, battle)
      val (joined, kinLines) = startingKin(hero, ratted)
      val lines              = ratLines ++ kinLines
      ZIO.when(lines.nonEmpty)(
        heroDao.writeActiveBattle(user.userId, joined.asJson) *>
          renderer.show(user, Screen(lines.mkString("\n"), Nil))).unit
    }

  /** «Крыса», порог 12: если в начале боя при герое нет ни одной крысы, одна
    * приходит сразу, без броска. */
  private def startingRat(hero: Hero, battle: SoloPveBattle): (SoloPveBattle, Vector[String]) =
    if (!hero.sets.startsWithRat || battle.group.hasRatAlly) (battle, Vector.empty)
    else callRat(hero, battle).fold((battle, Vector.empty[String])) { case (joined, line) => (joined, Vector(line)) }

  /** «Мать Глубин», порог 12: из воды выходят мурлоки-собратья — один, а против
    * трёх и больше врагов (в строю и в очереди за ним) двое. Уже вышедшие второй
    * раз не выходят. Встают на ближайшие к герою свободные места — сколько влезет. */
  private def startingKin(hero: Hero, battle: SoloPveBattle): (SoloPveBattle, Vector[String]) = {
    val foes = battle.group.aliveCount + battle.group.queue.size
    val out  = battle.group.allies.exists(_.kind == pangea.model.squad.AllyKind.MurlocKin)
    (1 to (if (out) 0 else hero.sets.kinCount(foes))).foldLeft((battle, Vector.empty[String])) {
      case ((b, lines), _) =>
        b.group.nearestFreeAllyPlace.fold((b, lines)) { pos =>
          val kin = BattleState.kinAlly(hero, pos)
          (b.copy(group = b.group.copy(allies = b.group.allies :+ kin)),
           lines :+ content.format("battle.kinCall", "name" -> kin.name, "n" -> pos.toString))
        }
    }
  }

  override def action(
      user: User,
      ua: UserAction,
      renderer: Renderer
  ): Task[StateType] =
    heroDao.readActiveBattle(user.userId).flatMap {
      case None       => recover(user, renderer)
      case Some(json) =>
        ZIO.fromEither(json.as[SoloPveBattle]).flatMap { battle =>
          // Герой обнулён — ходит только таймер отряда; кнопки лишь показывают, как дела.
          if (battle.group.heroDown && !BattleState.parseAction(ua).contains("SquadTick")) showDown(user, battle, renderer)
          else branch.act(user, ua, renderer)
        }
    }

  /** Герой числится в бою, а боя нет. Так бывает, когда победа уже записана
    * (бой очищен, опыт начислен, добыча в scene_data), а переход к экрану
    * добычи не состоялся — оборвалась отправка сообщений, перезапуск между
    * записью и переходом, — либо бой сняли снаружи. Раньше это была ошибка
    * «пропишите /home» на каждое нажатие. Теперь: добыча ждёт — ведём к ней,
    * иначе — тихо в лабиринт. */
  private def recover(user: User, renderer: Renderer): Task[StateType] =
    heroDao.readSceneData(user.userId).flatMap { scene =>
      val won = scene.flatMap(_.as[LootState.LootData].toOption).exists(_.won)
      if (won) ZIO.succeed(StateType.Loot)
      else heroDao.writeSceneData(user.userId, Json.Null) *>
        renderer.show(user, Screen(content.text("battle.interrupted"), Nil)).as(StateType.Dungeon)
    }

  // ── Оболочка I/O: читаем всё → считаем ход → пишем всё ──────────────────────

  /** Прочитать снимок (герой + бой + время) один раз, посчитать ход чистой
    * функцией `turn`, затем один раз закоммитить результат. */
  private def resolve(user: User, renderer: Renderer)(turn: Turn): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      state  <- resolveLoaded(user, renderer, hero, battle, now)(turn)
    } yield state

  private def resolveLoaded(user: User, renderer: Renderer, hero: Hero, battle: SoloPveBattle,
                            now: Long)(turn: Turn): Task[StateType] =
    for {
      // Стена трофеев — какая она сейчас дома: прибавка к урону по расам.
      wall   <- trophyWall(user)
      walled  = battle.copy(trophyWall = wall)
      // Щит, поднятый в прошлом раунде, своё отстоял, и кинжал наизготовку тоже:
      // их снимает первое действие нового раунда. Посреди раунда — после пояса,
      // Блока или Контрудара — они ещё держатся (см. Skill.Block, Counterstrike).
      // С ними же заново считается и предел минибосса за раунд (BossRoundCap).
      // Комбо стихий — не больше одного за ход героя: счёт с нуля каждый ход.
      fresh   = walled.copy(comboUsed = false)
      struck <- turn(hero,
                  if (fresh.midRound) fresh else fresh.freshRound.copy(heroBlockPct = 0L, heroCounterReady = false), now)
      // «Упырь» (порог 12): пир — только за тех, кого уложил САМ герой, и тут
      // же, в бою. Считаем до хода союзников: их добычу упырь не ест.
      feasted = ghoulFeast(battle, struck, now)
      // «Преисподняя» (порог 12): добитые героем возвращают ему энергию.
      pushed  = underworldPush(battle, feasted, now)
      // «Некромант» (порог 12): добитый героем может встать и пойти за ним.
      result <- necroRise(battle, pushed)
      // «Первопроходец» (порог 12): смертельный удар пары — второе дыхание.
      breathed = secondWind(result, side = false)
      // «Каменный страж» (порог 12) смотрит на ход целиком: важно не то, какой
      // именно источник добил героя до полоски, а что за этот ход он её перешёл.
      guarded = stoneGuardRescue(hero, breathed, now)
      // Гнилой Джо поднимается после обнуления HP — победу над ним ещё рано
      // засчитывать. Ловим здесь, где сходятся ВСЕ пути урона: удар, навык,
      // шипы, яд. Иначе каждый из них пришлось бы проверять отдельно.
      risen   = joeRises(guarded)
      // Группа: павший в паре уступает место следующему, мобы вне пары ходят
      // (сбоку бьёт только сосед), в конце раунда — подкрепление, перемешивание
      // каждый шестой раунд и отложенный Таран. Ход, который не был ходом
      // (навык не готов и т.п.), группу не двигает.
      promoted = promoteAfterKill(risen)
      // Герой сам добил активного, а бой идёт — его раны тикают и в этом
      // раунде (см. dueWounds).
      ached    = dueWounds(promoted, now)
      // Союзники бьют после ответа активного моба; добили активного — снова
      // продвигаем следующего к герою.
      allied  <- allyPhase(ached)
      // Добил союзник — Джо встаёт так же, как от удара героя.
      promoted2 = promoteAfterKill(joeRises(allied))
      sided   <- sideMobsPhase(promoted2, now)
      // Добил удар сбоку — второе дыхание и тут, если его ещё не было.
      sidedUp  = secondWind(sided, side = true)
      // Активный вне пары мог истечь ранами на своём ходу — тогда следующий
      // (а Джо — встаёт).
      promoted3 = promoteAfterKill(joeRises(sidedUp))
      // Аура держит всё поле: нежить крепнет, живые чахнут. Добитых ею разбираем
      // тем же порядком, что павших от ран, — и героя тоже: обнулила его — это
      // смерть в этот же ход, и «Первопроходец» спасает и от неё (см. auraFelled).
      // Раны союзников тикают там же, где и прочие: после всех ходов раунда.
      allyTicked = tickAllyEffects(promoted3)
      prefs   <- BattleState.prefs(heroDao, user.userId)
      fogged   = auraTick(allyTicked, prefs.miasma)
      promoted4 = promoteAfterKill(joeRises(fogged))
      // Бочки с порохом на поле (просека Чернолома): одна может рвануть —
      // павших от неё разбираем тем же порядком, что павших от ауры.
      blasted  <- kegBlast(promoted4, now)
      promoted5 = promoteAfterKill(joeRises(blasted))
      // Сборщик тянет узор из героя (Рубеж II): раз в несколько раундов —
      // атака героя ниже, сам он лечится. Лежачего героя это не касается.
      pulledOut = patternPull(promoted5)
      // Охрана вся полегла — стрелкам некого прикрывать: они бросают башни и
      // уходят, бой на этом кончается победой.
      abandoned = towersAlone(pulledOut)
      ended   <- endRound(battle, abandoned).map(preyFlees)
      // «Крыса» (порог 6): раз в раунд у героя из-под ног может выскочить своя.
      ratted  <- ratCall(hero, ended)
      // Напротив героя пусто, а свободный моб появился — он шагает к герою.
      pulled  = pullFreeStep(ratted)
      // Мобы своё прошли — теперь отряд: кому некого бить, идёт к врагу.
      advanced = squadStep(pulled)
      // Паутина держит до самого конца раунда — после всех шагов она слабеет;
      // проклятие Некроманта — тоже, после всего лечения раунда.
      unwebbed = webTick(advanced)
      // Подсказка про поглощённый удар идёт последней строкой раунда — уже после
      // всего, что в нём случилось.
      hinted  = plainSteelHint(hero, unwebbed)
      // Раунд не кончился — герой ещё в нём (см. SoloPveBattle.midRound).
      marked   = hinted.copy(battle = hinted.battle.copy(midRound = hinted.outcome == Outcome.Continue && !hinted.endsRound))
      state  <- commit(user, marked, now, renderer, hero)
    } yield state

  /** Крыса из-под ног героя встаёт в строй на его стороне — если там есть
    * свободное место. Она тот же моб, что водится в канализации: обычная на
    * пороге 6, чумная на пороге 12 ([[HeroSets.summonsPlagueRats]]), уровнем в
    * самого героя. `home = 0` помечает её как пришедшую не из отряда — после боя
    * по этому признаку она в отряд и попадает (см. [[squadAfterBattle]]).
    *
    * Возвращает бой и строку для лога, либо None, если встать некуда. */
  private def callRat(hero: Hero, battle: SoloPveBattle): Option[(SoloPveBattle, String)] =
    battle.group.nearestFreeAllyPlace.map { pos =>
      val ally = BattleState.ratAlly(hero, pos)
      (battle.copy(group = battle.group.copy(allies = battle.group.allies :+ ally)),
       content.format("battle.ratCall", "name" -> ally.name, "n" -> pos.toString))
    }

  /** «Крыса», порог 6: раз в раунд из-под ног героя может выскочить крыса. Пока
    * герой лежит, звать её некому; бросок без набора не тратится. */
  private def ratCall(hero: Hero, res: TurnResult): Task[TurnResult] =
    chanceRoll(
      res.outcome == Outcome.Continue && res.endsRound && !res.battle.group.heroDown &&
        hero.sets.ratSummonChancePct > 0,
      hero.sets.ratSummonChancePct
    ).map {
      case false => res
      case true  => callRat(hero, res.battle).fold(res) { case (joined, line) =>
        res.copy(battle = joined, log = res.log :+ line)
      }
    }

  /** Башни стоят, пока есть кого прикрывать. Не осталось живой охраны — бой
    * кончен: стрелки уходят, сами башни в добычу и опыт не идут. */
  private def towersAlone(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue) res
    else if (res.battle.livingGuards.nonEmpty) res
    else if (!(res.battle.activeSlot :: res.battle.group.others).exists(s =>
               s.alive && BattleState.isTowerSlot(s))) res
    else res.copy(outcome = Outcome.Victory, log = res.log :+ content.text("battle.towersAbandoned"))

  /** Напоминание, что голое железо против этого врага почти бесполезно: без
    * такой строки игрок видит только маленькие числа и решает, что игра его
    * обманывает. Висит, пока бой идёт и оружие без стихий; на победе и смерти
    * не нужна. Сейчас единственный, кто так держит сталь, — каменный элементаль,
    * поэтому и текст про камень. */
  private def plainSteelHint(hero: Hero, res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue) res
    else if (!res.battle.boss.exists(_.plainDamageTakenMult < 1.0)) res
    else if (hero.gems.weaponElements.nonEmpty) res
    else res.copy(log = res.log :+ content.text("battle.elemental.stoneAbsorbs"))

  /** «Отказывается умирать»: пока у минибосса остались подъёмы, обнуление HP не
    * заканчивает бой — он встаёт с частью здоровья и слабеет в атаке. Пламя всё
    * меняет: начиная со второго падения горящий Джо упокаивается насовсем.
    *
    * Зовётся после каждой фазы хода, где Джо могли добить (удар, союзники,
    * раны, аура, бочки). Упокоенному огнём подъёмы тут же списываются все —
    * иначе каждая следующая фаза снова находила бы горящего Джо с нулём HP и
    * повторяла строку «Огонь смог упокоить…».
    *
    * Возвращает исход как есть, если это не он, подъёмы кончились или победы в
    * этом ходу вовсе не было. */
  private def joeRises(res: TurnResult): TurnResult = {
    val battle = res.battle
    val revives = battle.boss.map(_.revives).getOrElse(Nil)
    if (res.outcome != Outcome.Victory || revives.isEmpty) res
    else {
      val deathNo = battle.bossRevives + 1 // какое это по счёту падение
      val burning = battle.effects.monsterBurn.isDefined
      if (deathNo > revives.size) res // подъёмы кончились — смерть окончательная
      else if (burning && deathNo >= MiniBoss.RottenJoe.FireEndsFromDeath)
        res.copy(battle = battle.copy(bossRevives = revives.size), log = res.log :+ content.text("battle.joe.burnedOut"))
      else {
        val maxHp  = battle.monsterStats.hp
        val newHp  = (maxHp * revives(deathNo - 1) / 100L).max(1L)
        val risen  = battle.copy(
          monsterCurrentHp = newHp,
          bossRevives      = deathNo,
          effects = battle.effects.copy(
            monsterWeakenedTurns = MiniBoss.RottenJoe.ReviveAtkCutTurns,
            monsterWeakenedPct   = MiniBoss.RottenJoe.ReviveAtkCutPct))
        val key = if (deathNo == 1) "battle.joe.risesFirst" else "battle.joe.risesAgain"
        res.copy(
          battle  = risen,
          outcome = Outcome.Continue,
          log     = (res.log :+ content.text(key)) :+ content.format("battle.joe.riseWeakness",
                      "pct" -> MiniBoss.RottenJoe.ReviveAtkCutPct.toString,
                      "turns" -> MiniBoss.RottenJoe.ReviveAtkCutTurns.toString))
      }
    }
  }

  /** Приговорённый дотерпел свои раунды: уходит он один — живым и без добычи.
    * Призванные им сородичи остаются, и бой идёт дальше. Ушёл из пары — к
    * герою встаёт следующий; ушёл последним, а сородичей уже перебили, — это
    * победа над ними, с приговорённого ничего нет. Ушёл, не призвав никого, —
    * бой кончается, как раньше (см. dungeonCommit): срок остаётся, по нему и
    * видно. Пал раньше срока — уходить некому, срок снимается. */
  private def preyFlees(res: TurnResult): TurnResult = {
    val b = res.battle
    if (res.outcome != Outcome.Continue || b.escapesAfter <= 0 || b.group.round < b.escapesAfter || b.preyName.isEmpty) res
    else {
      val calm  = b.copy(escapesAfter = 0)
      // Вор с артефактом деревни уходит по-своему: не шатаясь, а со свёртком.
      val (kinKey, lastKey) =
        if (b.story.contains(Villages.ArtifactStory)) ("battle.thiefEscapedKin", "battle.thiefEscaped")
        else ("battle.preyEscapedKin", "battle.preyEscaped")
      val left  = (name: String) => res.log :+ content.format(kinKey, "monster" -> name)
      if (b.monsterCurrentHp > 0L && b.customName == b.preyName)
        calm.promoteAfterEscape match {
          case Some(next)                     => res.copy(battle = next.copy(preyLeft = true), log = left(b.monsterName))
          case None if b.group.slain.nonEmpty =>
            res.copy(battle = calm.copy(preyGone = true, preyLeft = true), outcome = Outcome.Victory,
                     log = res.log :+ content.format(lastKey, "monster" -> b.monsterName))
          case None                           => res
        }
      else b.group.others.indexWhere(s => s.alive && s.customName == b.preyName) match {
        case -1  => res.copy(battle = calm)
        case idx =>
          val prey = b.group.others(idx)
          res.copy(battle = calm.copy(preyLeft = true, group = calm.group.copy(
                     others = calm.group.others.patch(idx, Nil, 1), places = calm.group.places.patch(idx, Nil, 1))),
                   log = left(prey.name))
      }
    }
  }

  /** «Каменный страж» (порог 12): ход, за который HP героя провалилось ниже
    * порога, поднимает ему броню. Срабатывает именно НА ПЕРЕСЕЧЕНИИ — пока герой
    * сидит под порогом, каждый следующий удар брони уже не даёт. */
  private def stoneGuardRescue(before: Hero, res: TurnResult, nowMs: Long): TurnResult = {
    val hero = res.hero
    if (!hero.sets.rescuesOnLowHp || hero.fightStats.hp <= 0) res
    else {
      val limit = hero.effectiveMaxHp(nowMs) * hero.sets.lowHpThresholdPct / 100L
      if (before.fightStats.hp < limit || hero.fightStats.hp >= limit) res
      else {
        val maxArmor = hero.effectiveMaxArmor(nowMs)
        val restore  = (maxArmor * hero.sets.rescueArmorPct / 100L).max(1L)
        val newArmor = (hero.fightStats.armor + restore).min(maxArmor)
        val gained   = newArmor - hero.fightStats.armor
        if (gained <= 0L) res
        else res.copy(
          hero = hero.copy(fightStats = hero.fightStats.copy(armor = newArmor)),
          log  = res.log :+ content.format("battle.stoneGuardRescue", "armor" -> gained.toString)
        )
      }
    }
  }

  /** «Первопроходец» (порог 12, только человеку): раз за бой смерть оставляет
    * герою 1 HP и снимает с него яд, кровь и пламя. Ловим здесь, как подъём
    * Джо: важно не что добило — удар, умение, шипы, горение, — а что герой пал.
    * Лежачего не поднимает: он пал раньше. Строка идёт
    * туда же, где удар: от удара сбоку — в сводку сбоку (`side`). */
  private def secondWind(res: TurnResult, side: Boolean): TurnResult =
    if (res.outcome != Outcome.Death || res.hero.fightStats.hp > 0L || !res.hero.sets.secondWind ||
        res.battle.secondWindUsed || res.battle.group.heroDown) res
    else {
      val line = content.text("battle.secondWind")
      res.copy(
        hero    = res.hero.copy(fightStats = res.hero.fightStats.copy(hp = 1L)),
        battle  = res.battle.copy(
          secondWindUsed = true,
          effects        = res.battle.effects.copy(heroPoison = None, heroBleed = None, heroBurn = None)),
        outcome = Outcome.Continue,
        log     = if (side) res.log else res.log :+ line,
        sideLog = if (side) res.sideLog :+ line else res.sideLog)
    }

  /** Единственная точка записи и показа за ход. Персист итоговых
    * снаряжения+статов героя выполняется ВСЕГДА и вне ветвления по исходу —
    * поэтому ни одна ветка не может «забыть» сохранить изменённый стат.
    *
    * ЖЁСТКИЙ ИНВАРИАНТ: сначала ВСЕ записи хода (неразрывным блоком), только
    * потом показы. Отправка сообщений — сетевой вызов к ВК: он может упасть,
    * а фибру обработки может прервать разрыв HTTP-соединения (ВК не дождался
    * ответа). Если бы запись шла после показа, ход «отображался, но не
    * случался»: игрок видел урон, а в БД оставалось прежнее состояние боя —
    * так терялось добивание навыком (моб оживал с прежним HP). */
  private def commit(
      user: User,
      raw: TurnResult,
      nowMs: Long,
      renderer: Renderer,
      before: Hero
  ): Task[StateType] =
    dungeonCommit(user, raw, nowMs, renderer, before)

  /** Остальное, что ход поменял в герое, — и что тоже пишется только здесь:
    * бафы (отвар с пояса, лопнувший пузырь), травмы (снятые костоправным),
    * быстрые отдыхи от бодрящего сбора и, пока бой идёт, пыль со смазкой на
    * оружии — покрытие живёт до конца боя, и о нём должен знать не только
    * текущий ход. Пишем лишь то, что изменилось против героя до хода. */
  private def persistExtras(user: User, res: TurnResult, before: Hero, nowMs: Long): Task[Unit] =
    ZIO.when(res.hero.statBoosts != before.statBoosts)(
      heroDao.updateStatBoosts(user.userId, res.hero.statBoosts)) *>
      ZIO.when(res.hero.traumaNames != before.traumaNames || res.hero.traumaUntil != before.traumaUntil)(
        heroDao.updateTrauma(user.userId, res.hero.traumaUntil, res.hero.traumaNames)) *>
      ZIO.when(res.instantRests > 0)(
        AzatData.load(heroDao, user.userId, nowMs).flatMap(azat =>
          heroDao.writeAzatData(user.userId,
            azat.copy(instantRests = azat.instantRests + res.instantRests).asJson))) *>
      ZIO.when(res.outcome == Outcome.Continue && res.hero.weaponDust != before.weaponDust)(
        heroDao.updateWeaponDust(user.userId, res.hero.weaponDust)).unit

  /** Бой кончился: пыль на оружии осыпается — покрытие на один бой, чем бы он
    * ни кончился, — а опустевшие ячейки пояса добирают из сумки то же, чем
    * были полны. Здесь только запись; что легло на пояс, говорит
    * [[showRefilled]] — показы идут после всех записей хода. */
  private def afterBattle(user: User, hero: Hero): Task[List[String]] =
    ZIO.when(!hero.weaponDust.isEmpty)(heroDao.updateWeaponDust(user.userId, WeaponDust.empty)) *>
      giantGone(user, hero) *>
      refillBelt(user, hero)

  /** Бой кончился — великанье зелье сходит: баф снят, а здоровье, налитое
    * сверх обычного предела, возвращается к нему. */
  private def giantGone(user: User, hero: Hero): Task[Unit] =
    ZIO.when(hero.statBoosts.boosts.exists(_.name == BrewRates.GiantBoost)) {
      val calm = hero.copy(statBoosts = hero.statBoosts.without(BrewRates.GiantBoost))
      ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS)).flatMap { now =>
        val max = calm.effectiveMaxHp(now)
        heroDao.updateStatBoosts(user.userId, calm.statBoosts) *>
          ZIO.when(hero.fightStats.hp > max)(
            heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = max)))
      }
    }.unit

  private def showRefilled(user: User, names: List[String], renderer: Renderer): Task[Unit] =
    ZIO.when(names.nonEmpty)(renderer.show(user, Screen(content.format("battle.belt.refilled",
      "names" -> BeltContents.counted(names)), Nil))).unit

  private def dungeonCommit(
      user: User,
      raw: TurnResult,
      nowMs: Long,
      renderer: Renderer,
      before: Hero
  ): Task[StateType] = {
    // Пузырьковый нектар: смерть, которой не будет. Считаем до persistHero,
    // чтобы спасённый герой сохранился вместе с остальным исходом.
    val res = bubbleRescue(raw, nowMs)
    val persistHero =
      heroDao.updateEquipmentAndFightStats(user.userId, res.hero.equipment, res.hero.fightStats) *>
        persistExtras(user, res, before, nowMs)
    // Отряд: что с союзниками стало и кто ушёл по свитку — по концу боя.
    // Крыса, выскочившая из-под ног (`home = 0`), после боя остаётся при герое —
    // даже если до этого отряда у него не было вовсе.
    val persistSquad =
      ZIO.when(res.hero.squad.nonEmpty || res.battle.group.alliesGone.nonEmpty ||
               res.battle.group.allies.exists(_.home <= 0))(
        heroDao.updateSquad(user.userId, squadAfterBattle(res.hero, res.battle, nowMs)))
    val msg     = res.log.mkString("\n")
    val showLog = ZIO.when(msg.nonEmpty)(renderer.show(user, Screen(msg, Nil)))
    // Групповая сводка: что делали мобы вне пары и кто где стоит. Строй
    // показываем, только пока бой идёт, — после него он не нужен. А вот что
    // случилось сбоку, говорим при любом исходе: иначе союзник, которого
    // унесло свитком тем же ходом, каким кончился бой, пропадал бы молча.
    val standing  = if (res.outcome == Outcome.Continue && res.battle.group.hasFormation) groupLines(res.battle) else Vector.empty
    val groupMsg  = (res.sideLog ++ standing).mkString("\n")
    val showGroup = ZIO.when(groupMsg.nonEmpty)(renderer.show(user, Screen(groupMsg, Nil)))
    res.outcome match {
      // Приговорённый дотерпел свои раунды и ушёл: добычи с него нет, но и
      // герой цел — событие просто кончилось.
      case Outcome.Continue if res.battle.escapesAfter > 0 && res.battle.group.round >= res.battle.escapesAfter =>
        (persistHero *> persistSquad *> afterBattle(user, res.hero) <* heroDao.clearActiveBattle(user.userId))
          .uninterruptible.flatMap { refilled =>
            showLog *> showGroup *> showRefilled(user, refilled, renderer) *>
              renderer.show(user, Screen(content.format("battle.preyEscaped", "monster" -> res.battle.monsterName), Nil)) *>
              ZIO.when(res.battle.group.heroDown)(
                renderer.show(user, Screen(content.text("battle.squad.heroUpEscaped"), Nil)))
          }
            .as(StateType.Dungeon)
      case Outcome.Continue =>
        (persistHero *> heroDao.writeActiveBattle(user.userId, res.battle.asJson)).uninterruptible *>
          showLog *> showGroup *>
          renderer.show(user, buildBattleScreen(res.hero, res.battle, res.hero.effectiveMaxHp(nowMs), nowMs,
            BattleState.mixInOffhand(res.hero)))
            .as(StateType.Battle)
      case Outcome.Victory =>
        for {
          written <- (persistHero *> persistSquad *> afterBattle(user, res.hero) <*>
                       applyVictory(user, res.hero, res.battle)).uninterruptible
          (refilled, outcome) = written
          _       <- showLog
          // Что случилось сбоку в этот же ход — до итогов победы: иначе
          // союзник, которого унесло свитком, пропал бы молча.
          _       <- showGroup
          _       <- showRefilled(user, refilled, renderer)
          _       <- showVictory(user, outcome, renderer)
        } yield StateType.Loot
      case Outcome.Death if res.battle.group.allies.exists(_.alive) && !res.battle.group.heroDown =>
        // Отряд ещё на ногах: герой падает, но не умирает — бой идёт без него,
        // раунд за раундом по таймеру, а смерть отложена до исхода. Сколько
        // раундов показать, а с какого доиграть сразу — см. squadTick.
        // Упавший получает травму сразу; проиграет отряд — смерть второй уже
        // не даст (см. DeathState).
        val fell = res.battle.copy(group = res.battle.group.copy(heroDown = true, downRounds = 0))
        val (down, turned) = fell.leaveDownedHero
        (persistHero *> heroDao.writeActiveBattle(user.userId, down.asJson) *>
          giveTrauma(res.hero, nowMs, "battle.squad.fallTrauma", "battle.squad.fallTraumaMax")).uninterruptible
          .flatMap { case (_, traumaLine) =>
            showLog *> showGroup *>
              scheduler.schedule(user.userId, nowMs + BattleState.SquadTickMs, TaskKind.SquadFight, StateType.Battle, BattleState.SquadTickAction) *>
              renderer.show(user, Screen(content.format("battle.squad.heroDown",
                "rounds" -> BattleState.ShownDownRounds.toString) + turnsAwayLine(down, turned) + "\n" + traumaLine, Nil)) *>
              showDown(user, down, renderer)
          }
      case Outcome.Death =>
        (persistHero *> persistSquad *> afterBattle(user, res.hero)).uninterruptible.flatMap { refilled =>
          showLog *> showGroup *> showRefilled(user, refilled, renderer) *>
            renderer.show(user, Screen(content.text("battle.death"), Nil)) *>
            failQuestBehind(user, renderer).as(StateType.Death)
        }
      case Outcome.Fled =>
        // Сюжетный бой идёт не в лабиринте — бежать из него в город.
        val to = if (res.battle.story.isDefined) StateType.GlobalMap else StateType.Dungeon
        // Сбежал (или ничья) из налёта на мурлоков, из боя у завала или от
        // орков у поющего эльфа — развязки «после боя» не будет: роутинг
        // добычи стираем, иначе его подхватила бы следующая победа и засчитала
        // несыгранный бой.
        val dropRouting = res.battle.story.exists(BattleState.RaidStories.contains)
        // Лежачий сам не бежит: это отряд доигрывал бой и не взял верх — ничья,
        // все отступают и выносят его (см. autoResolve).
        val fledKey = if (res.battle.group.heroDown) "battle.squad.retreat" else "battle.fled"
        (persistHero *> persistSquad *> afterBattle(user, res.hero) <* heroDao.clearActiveBattle(user.userId) <*
           ZIO.when(dropRouting)(heroDao.writeSceneData(user.userId, Json.Null)))
          .uninterruptible.flatMap { refilled =>
            showLog *> showGroup *> showRefilled(user, refilled, renderer) *>
              renderer.show(user, Screen(content.text(fledKey), Nil)) *>
              failQuestBehind(user, renderer).as(to)
          }
    }
  }

  /** За каким заданием герой пришёл в этот бой: видно по роутингу добычи — он
    * говорит, куда бой вернётся, а значит и откуда герой здесь. Караван и
    * пещеры ходят своими состояниями, воры — экраном поживы. Пещеру, канализацию
    * и пещеру некроманта различает сама сцена. */
  private def questBehind(user: User): Task[Option[BoardKind]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[LootState.LootData].toOption).flatMap { loot =>
      loot.returnState.flatMap {
        case StateType.ThievesSpoils => Some(BoardKind.Thieves)
        case StateType.Caravan       => Some(BoardKind.CaravanRout)
        case StateType.MonsterCave   =>
          loot.eventData.flatMap(_.as[pangea.model.cave.CaveScene].toOption)
            .map(pangea.service.state.states.events.cave.MonsterCaveState.questOf)
            .orElse(Some(BoardKind.CaveClear))
        case _ => None
      }
    })

  /** Герой отступил от квестовой цели — сбежал или пал. Цель на этом потеряна:
    * объявление пропадает с доски. Сделанное задание (`done`) это не трогает —
    * охрану он уже перебил, а плату берут у доски. */
  private def failQuestBehind(user: User, renderer: Renderer): Task[Unit] =
    questBehind(user).flatMap {
      case None       => ZIO.unit
      case Some(kind) =>
        BoardProgress.markFailed(heroDao, user.userId, kind).flatMap { failed =>
          // Цель потеряна — возвращаться к ней некуда: роутинг добычи стираем,
          // иначе он остался бы висеть и повёл бы героя к пустой сцене.
          ZIO.when(failed)(
            heroDao.writeSceneData(user.userId, Json.Null) *>
              renderer.show(user, Screen(content.text("questBoard.failed"), Nil))).unit
        }
    }

  /** Continue без изменения состояния — только сообщение (неготовый скилл, пустая
    * фляга и т.п.): ход не тратится, экран перерисовывается. */
  private def cont(hero: Hero, battle: SoloPveBattle, msg: String): Task[TurnResult] =
    ZIO.succeed(TurnResult(hero, battle, Vector(msg), Outcome.Continue, endsRound = false))

  // ── Ход игрока: базовая атака ───────────────────────────────────────────────

  /** Кнопка «Атака». Целей несколько (моб в паре и соседи) — сперва
    * спрашиваем, в кого бить, как у умений; одна — бьём сразу. */
  private def attackRoute(user: User, target: Option[Int], renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      state  <-
        if (target.isEmpty && heroTargets(hero, battle).size > 1)
          renderer.show(user, attackTargetScreen(hero, battle)).as(StateType.Battle)
        else resolveLoaded(user, renderer, hero, battle, now)(attackTurn(target))
    } yield state

  /** «Ловчая сеть»: в кого бросить — врагов в досягаемости несколько, спросить,
    * как у атаки; один — бросить сразу (см. [[netTurn]]). */
  private def netRoute(user: User, target: Option[Int], renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      asks    = target.isEmpty && hero.equipment.additionalWeapon.net.isDefined && !battle.divineUsedThisRound &&
                  battle.aimSlot.isEmpty && BattleState.netTargets(battle).size > 1
      state  <-
        if (asks)
          renderer.show(user, targetChoices(battle, "ThrowNet", TrapNet.Name, support = false,
            BattleState.netTargets(battle))).as(StateType.Battle)
        else resolveLoaded(user, renderer, hero, battle, now)(netTurn(target))
    } yield state

  /** Бросок ловчей сети из доп. слота (см. [[TrapNet]]): враг не дальше
    * `TrapNet.Reach` мест раунд не сходит с места. Раз за раунд, как и
    * божественное оружие того же слота, и раунд бросок не завершает — свой
    * удар герой делает следом. Последний бросок рвёт сеть: слот пустеет. */
  private def netTurn(target: Option[Int]): Turn = (hero, battle, _) =>
    hero.equipment.additionalWeapon.net match {
      case None                                  => cont(hero, battle, content.text("battle.net.none"))
      case Some(_) if battle.aimSlot.isDefined   => cont(hero, battle, content.text("battle.powerShot.handsBusy"))
      case Some(_) if battle.divineUsedThisRound => cont(hero, battle, content.text("battle.net.alreadyUsed"))
      case Some(net) =>
        val targets = BattleState.netTargets(battle)
        target.orElse(targets.headOption) match {
          case None                                => cont(hero, battle, content.text("battle.net.noTarget"))
          case Some(pos) if !targets.contains(pos) => cont(hero, battle, content.text("battle.group.targetGone"))
          case Some(pos) =>
            val name      = battle.monsterAt(pos).fold("")(_.targetName)
            val spent     = net.spent
            val torn      = spent.charges <= 0
            val slot      = if (torn) Item.NoItem else hero.equipment.additionalWeapon.copy(details = spent)
            val heroAfter = hero.copy(equipment = hero.equipment.copy(additionalWeapon = slot))
            val log       = Vector(content.format("battle.net.thrown", "monster" -> name)) ++
                            Option.when(torn)(content.text("battle.net.torn")).toVector
            ZIO.succeed(TurnResult(heroAfter, BattleState.netAt(battle, pos).copy(divineUsedThisRound = true), log,
              Outcome.Continue, endsRound = false))
        }
    }

  /** Базовая атака по месту `target` (без цели — по единственной досягаемой):
    * по мобу в паре — с его ответом; по соседу — через подмену в полях, после
    * чего отвечает моб в паре, а если пара пуста — ход тихо кончается и мобы
    * ходят сами. Бить некого — ход не тратится. */
  private def attackTurn(target: Option[Int])(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] = {
    val targets = heroTargets(hero, battle)
    target.orElse(targets.headOption) match {
      case None                                 => cont(hero, battle, content.text("battle.group.noTarget"))
      case Some(pos) if !targets.contains(pos)  => cont(hero, battle, content.text("battle.group.targetGone"))
      case Some(pos) => battle.aimSlot match {
        // Тетива натянута: «Атака» спускает «Мощный выстрел» — дальше как у
        // любого умения: урон со всеми прибавками и базовая атака следом.
        case Some(slot) => skillHit(hero, battle, slot, nowMs, pos)
        case None if pos == battle.group.heroPos => playerStrike(hero, battle.rememberTarget(pos), nowMs, Vector.empty, Set.empty)
        case None => strikeSide(hero, battle, pos, nowMs, Vector.empty, Set.empty).flatMap(closeTurn(_, nowMs, Set.empty))
      }
    }
  }

  /** «Выстрелить», когда тетива натянута, а бить некого: стрела уходит мимо,
    * рука свободна. Ход на этом кончается, умение — на перезарядку. Враг уже
    * в досягаемости (старая кнопка) — это обычная «Атака»: выстрел по нему. */
  private def looseTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    battle.aimSlot match {
      case None => cont(hero, battle, content.text("battle.skillUnavailable"))
      case Some(_) if heroTargets(hero, battle).nonEmpty => attackTurn(None)(hero, battle, nowMs)
      case Some(slot) =>
        val missed = battle.updateSlot(slot.itemId)(s => s.copy(cooldown = s.skill.cooldown, uses = s.uses + 1, aim = None))
        closeTurn(TurnResult(hero, missed, Vector(content.text("battle.powerShot.missed")), Outcome.Continue),
          nowMs, Set(slot.itemId))
    }

  /** «Ждать»: бить некого — герой пропускает удар, раунд идёт своим чередом. */
  private def waitTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    closeTurn(TurnResult(hero, battle, Vector(content.text("battle.group.waited")), Outcome.Continue), nowMs, Set.empty)

  /** «Переместиться»: с кем из союзников поменяться местами. */
  private def moveRoute(user: User, renderer: Renderer): Task[StateType] =
    getBattle(user).flatMap { battle =>
      // Шаг ровно один — влево или вправо, как у мобов. Соседнее место может
      // быть пустым: тогда герой просто переходит на него.
      val choices = BattleState.stepTargets(battle).zipWithIndex.map { case (pos, i) =>
        val label = battle.group.allyAt(pos).filter(_.alive) match {
          case Some(a) => content.format("battle.group.moveTo", "n" -> pos.toString, "name" -> a.name)
          case None    => content.format("battle.group.moveToFree", "n" -> pos.toString)
        }
        pangea.engine.Choice("MoveTo", pangea.engine.Choice.fit(label),
          data = Map("target" -> pos.toString), row = Some(i / BattleState.AlliesPerRow))
      }
      val cancel = pangea.engine.Choice("CancelTarget", content.text("battle.group.cancelTarget"),
        color = pangea.engine.ChoiceColor.Negative,
        row = Some((choices.size + BattleState.AlliesPerRow - 1) / BattleState.AlliesPerRow))
      renderer.show(user, Screen(content.text("battle.group.moveWhom"), choices :+ cancel)).as(StateType.Battle)
    }

  /** Герой делает шаг на соседнее место. Стоял там союзник — они меняются
    * местами. Сам союзник сходит с места, только когда бить ему некого: в
    * конце раунда он идёт к врагу (см. `SoloPveBattle.squadAdvance`).
    * Стоял напротив моб — герой теперь с ним в паре, и тот отвечает; ход на
    * этом кончается. */
  private def moveTurn(target: Option[Int])(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    target.filter(BattleState.stepTargets(battle).contains) match {
      case None => cont(hero, battle, content.text("battle.group.targetGone"))
      case Some(pos) =>
        val from    = battle.group.heroPos
        val ally    = battle.group.allyAt(pos).filter(_.alive)
        val swapped = ally.fold(battle)(a =>
          battle.copy(group = battle.group.updateAlly(a.position)(_.copy(position = from))))
        val moved   = if (swapped.group.hasMonster(pos)) swapped.moveHeroTo(pos)
                      else swapped.copy(group = swapped.group.copy(heroPos = pos))
        val line    = ally match {
          case Some(a) => content.format("battle.group.moved", "name" -> a.name, "n" -> pos.toString)
          case None    => content.format("battle.group.stepped", "n" -> pos.toString)
        }
        closeTurn(TurnResult(hero, moved, Vector(line), Outcome.Continue), nowMs, Set.empty)
    }

  /** Такт боя без героя (он обнулён). Первые [[BattleState.ShownDownRounds]]
    * раунда идут по таймеру и с логом (см. [[squadRound]]); на следующем такте
    * отряд доводит бой до конца сам, и игрок видит только сводку (см.
    * [[autoResolve]]). Мобов не осталось — герой приходит в себя с
    * [[BattleState.DownReviveHp]] HP и забирает победу; не осталось союзников —
    * смерть, отложенная до этого момента. */
  private def squadTick(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      state  <-
        if (!battle.group.heroDown) showScreen(user, renderer).as(StateType.Battle)
        else if (battle.group.downRounds >= BattleState.ShownDownRounds) autoResolve(user, hero, battle, now, renderer)
        else squadRound(hero, battle, now).flatMap { res =>
          if (squadOver(res)) settleSquad(user, hero, res, now, renderer)
          else {
            val msg      = (res.log ++ res.sideLog).mkString("\n")
            val standing = groupLines(res.battle).mkString("\n")
            heroDao.writeActiveBattle(user.userId, res.battle.asJson) *>
              ZIO.when(msg.nonEmpty)(renderer.show(user, Screen(msg, Nil))) *>
              renderer.show(user, Screen(standing, Nil)) *>
              scheduler.schedule(user.userId, now + BattleState.SquadTickMs, TaskKind.SquadFight, StateType.Battle, BattleState.SquadTickAction) *>
              showDown(user, res.battle, renderer)
          }
        }
    } yield state

  /** Раунд без героя: союзники бьют, мобы отвечают им — ударом и умениями,
    * минибоссы идут по своему кругу; раны тикают, аура держит поле, Гнилой Джо
    * встаёт. Конец раунда — как обычно, но героя никто не трогает, а кому некого
    * бить, идёт к врагу: мобы к отряду, отряд к мобам. Только считает — ничего не
    * пишет и не показывает. */
  private def squadRound(hero: Hero, battle: SoloPveBattle, now: Long): Task[TurnResult] =
    for {
      // Место напротив союзника могло освободиться и позже — смотрим каждый раунд.
      // Раунд новый — и предел минибосса за раунд считается заново.
      moved    <- ZIO.succeed(battle.freshRound.leaveDownedHero)
      (walked, turnedTo) = moved
      allied   <- allyPhase(TurnResult(hero, walked,
                    Vector(turnsAwayLine(walked, turnedTo)).filter(_.nonEmpty), Outcome.Continue))
      // Гнилой Джо встаёт, кто бы его ни добил: союзник — тоже.
      promoted  = promoteAfterKill(joeRises(allied))
      // Раунд идёт и без героя: временное на поле сходит за свои раунды, как
      // в его ход (tickBuffs). Иначе пригвождённый (📌) так и стоял бы
      // пригвождённым до конца боя, а дым не рассеивался бы вовсе.
      ticked    = promoted.copy(battle = promoted.battle.tickBuffs())
      sided    <- sideMobsPhase(ticked, now)
      // Истёк ранами на своём ходу — тоже смерть, и от неё Джо тоже встаёт.
      promoted2 = promoteAfterKill(joeRises(sided))
      // Дальше — как в обычном раунде: раны на союзниках тикают, аура держит
      // поле. Свою ауру лежачий не держит — держать её нечем; чужая (миазмы
      // Некроманта) работает как всегда. Самого лежачего она не трогает, так
      // что и смерти от неё ему нет: его исход решит отряд (см. auraFelled).
      allyTicked = tickAllyEffects(promoted2)
      fogged     = promoteAfterKill(joeRises(auraTick(allyTicked, on = false)))
      // Бочкам с порохом всё равно, стоит герой или лежит: рвутся и теперь,
      // только лежачего не задевают (см. kegTargets).
      blasted   <- kegBlast(fogged, now)
      kegged     = promoteAfterKill(joeRises(blasted))
      // Охрану доломали союзники, пока герой лежал, — башням некого
      // прикрывать, и бой кончается так же, как если бы её добил он сам.
      abandoned = towersAlone(kegged)
      ended    <- endRound(battle, abandoned).map(preyFlees)
      // Конец раунда: кому некого бить, идёт к врагу — мобы к отряду, отряд
      // к мобам. Каждый раунд, даже если в этом никто никого не задел: иначе
      // такой раунд ничем бы не кончался, и следующий тоже — бой стоял бы.
      res       = if (ended.outcome != Outcome.Continue) ended
                  else ended.copy(battle = BattleState.webTicked(ended.battle.closeInOnSquad._1.squadAdvance._1))
    } yield res.copy(battle = res.battle.copy(group = res.battle.group.copy(downRounds = res.battle.group.downRounds + 1)))

  /** Кончился ли бой без героя: мобов не осталось, союзников не осталось или
    * приговорённый дотерпел свои раунды и ушёл. */
  private def squadOver(res: TurnResult): Boolean =
    res.outcome == Outcome.Victory || res.battle.group.allies.forall(!_.alive) ||
      (res.battle.escapesAfter > 0 && res.battle.group.round >= res.battle.escapesAfter)

  /** Исход боя без героя (см. [[squadOver]]): победа, смерть или ушедший враг. */
  private def settleSquad(user: User, before: Hero, res: TurnResult, now: Long, renderer: Renderer): Task[StateType] =
    res.outcome match {
      case Outcome.Victory =>
        val revived = res.hero.copy(fightStats = res.hero.fightStats.copy(hp = BattleState.DownReviveHp))
        commit(user, res.copy(hero = revived, log = res.log :+ content.text("battle.squad.heroUp")), now, renderer, before)
      case _ if res.battle.group.allies.forall(!_.alive) =>
        commit(user, res.copy(outcome = Outcome.Death, log = res.log :+ content.text("battle.squad.allGone")), now, renderer, before)
      // Приговорённый дотерпел свои раунды и ушёл — и пока герой лежал тоже:
      // добычи нет, а герой приходит в себя, как после победы отряда.
      case _ =>
        val revived = res.hero.copy(fightStats = res.hero.fightStats.copy(hp = BattleState.DownReviveHp))
        commit(user, res.copy(hero = revived), now, renderer, before)
    }

  /** Отряд доводит бой до конца сам: раунд за раундом, без таймера и без лога,
    * пока бой не кончится (см. [[squadOver]]), но не дольше
    * [[BattleState.AutoResolveRounds]] раундов. Игроку — одна сводка: скольких
    * добил каждый и кто кого выбил. Награда, смерть и ушедший враг — как после
    * обычного раунда без героя. Не кончился и за этот срок — ничья: обе стороны
    * отступают, добычи нет, а отряд выносит героя, и тот приходит в себя, как
    * после победы. Выбывшие так и остаются выбывшими. */
  private def autoResolve(user: User, before: Hero, battle: SoloPveBattle, now: Long, renderer: Renderer): Task[StateType] = {
    def play(res: TurnResult, left: Int): Task[TurnResult] =
      if (squadOver(res) || left <= 0) ZIO.succeed(res)
      else squadRound(res.hero, res.battle, now).flatMap(play(_, left - 1))
    play(TurnResult(before, battle, Vector.empty, Outcome.Continue), BattleState.AutoResolveRounds).flatMap { played =>
      val res = played.copy(log = squadSummary(played.battle), sideLog = Vector.empty)
      if (squadOver(res)) settleSquad(user, before, res, now, renderer)
      else {
        // Ничья — побег всем отрядом: исход тот же, что у бегства (см. dungeonCommit).
        val revived = res.hero.copy(fightStats = res.hero.fightStats.copy(hp = BattleState.DownReviveHp))
        commit(user, res.copy(hero = revived, outcome = Outcome.Fled), now, renderer, before)
      }
    }
  }

  /** Сводка доигранного боя: по строке на каждого, кто дрался в нём рядом с
    * героем, по местам в отряде. Сколько врагов добил, а выбывший — ещё и кто
    * его выбил. */
  private def squadSummary(battle: SoloPveBattle): Vector[String] = {
    // Выбывшие — все, кого унесло из строя, и лежащий в строю без HP.
    val (standing, down) = battle.group.allies.partition(_.alive)
    val fought = standing.map(_ -> false) ++ (down ++ battle.group.alliesGone).map(_ -> true)
    // Пришедшие уже в бою (крыса из-под ног, восставший) места в отряде не
    // имеют (`home = 0`) — они в конце.
    val lines = fought.sortBy { case (a, _) => (if (a.home > 0) a.home else Int.MaxValue, a.position) }.map {
      case (a, false) => content.format("battle.squad.summary.standing", "name" -> a.name, "kills" -> a.kills.toString)
      case (a, true) =>
        a.lastHitBy match {
          case Some(by) => content.format("battle.squad.summary.fallenBy",
                             "name" -> a.name, "kills" -> a.kills.toString, "monster" -> by)
          case None     => content.format("battle.squad.summary.fallen", "name" -> a.name, "kills" -> a.kills.toString)
        }
    }
    content.text("battle.squad.summary.title") +: lines.toVector
  }

  /** Экран героя, лежащего без сил: только строй. Кнопок нет и не нужно —
    * жать лежачему нечего, бой идёт своей чередой по таймеру. Клавиатуру
    * убираем явно: иначе так и висели бы кнопки боя, которые всё равно ничего
    * не делают. Показанные раунды кончились — следующим тактом отряд доведёт
    * бой до конца, о чём и говорим. */
  private def showDown(user: User, battle: SoloPveBattle, renderer: Renderer): Task[StateType] = {
    val key = if (battle.group.downRounds >= BattleState.ShownDownRounds) "battle.squad.downScreenLast" else "battle.squad.downScreen"
    renderer.show(user, Screen(
      content.text(key) + "\n\n" + groupLines(battle).mkString("\n"),
      Nil, hideKeyboard = true)).as(StateType.Battle)
  }

  /** Строка о том, что моб ушёл от лежащего к союзнику; пустая, если не ушёл. */
  private def turnsAwayLine(battle: SoloPveBattle, to: Option[Int]): String =
    to.flatMap(pos => battle.group.allyAt(pos)).fold("")(ally =>
      "\n" + content.format("battle.squad.turnsAway",
        "monster" -> battle.monsterName, "name" -> ally.name))

  /** Удар героя по мобу на месте `pos`, стоящему не напротив него: моб
    * разворачивается в поля, получает удар (без ответа) и сворачивается
    * обратно; добитый сосед — в павшие, добитый активный вне пары — Victory,
    * дальше промоут. Что было в полях, туда и возвращается. */
  private def strikeSide(
      hero: Hero, battle: SoloPveBattle, pos: Int, nowMs: Long, log: Vector[String], skip: Set[Long]
  ): Task[TurnResult] = {
    val home = battle.group.activePos
    playerStrike(hero, battle.engage(pos), nowMs, log, skip, retaliate = false)
      .map(res => settleAfterHit(res, pos, home, nowMs, skip))
  }

  /** После удара по месту `pos` (см. [[strikeSide]]): запомнить цель, вернуть
    * прежнего активного в поля, добитого — в павшие или в победу. */
  private def settleAfterHit(res: TurnResult, pos: Int, home: Int, nowMs: Long, skip: Set[Long]): TurnResult = {
    val struck = res.battle.rememberTarget(pos)
    val dead   = struck.monsterCurrentHp <= 0L
    if (pos == home) {
      if (dead) victoryByHero(res.hero, struck, res.log, nowMs, skip) else res.copy(battle = struck)
    } else {
      val back = struck.engage(home)
      if (dead)
        res.copy(battle = back.sideFallen(back.group.idxOf(pos)),
          log = res.log :+ content.format("battle.group.sideSlain", "monster" -> struck.monsterName))
      else res.copy(battle = back)
    }
  }

  /** Завершить ход героя: в паре отвечает моб, без пары ход кончается тихо.
    * Ход, который уже кончился (победа, смерть, «ничего не случилось»), не трогаем. */
  private def closeTurn(res: TurnResult, nowMs: Long, skip: Set[Long]): Task[TurnResult] =
    if (res.outcome != Outcome.Continue || !res.endsRound) ZIO.succeed(res)
    else if (res.hero.fightStats.hp <= 0) ZIO.succeed(res.copy(outcome = Outcome.Death))
    else respond(res.hero, res.battle, nowMs, res.log, skip)

  /** Ответ на ход героя: моб в паре бьёт и кастует ([[monsterPhase]]); пары
    * нет — только геройская сторона конца раунда ([[quietPhase]]). */
  private def respond(hero: Hero, battle: SoloPveBattle, nowMs: Long, log: Vector[String],
                      skip: Set[Long], vampHeal: Long = 0L): Task[TurnResult] =
    if (battle.group.paired) monsterPhase(hero, battle, nowMs, log, skip, vampHeal)
    else quietPhase(hero, battle, nowMs, log, skip, vampHeal)

  /** Конец раунда без моба в паре: тик бафов и кулдаунов, раны и реген героя,
    * энергия. Мобы ходят потом сами — как мобы вне пары. */
  private def quietPhase(hero: Hero, battle: SoloPveBattle, nowMs: Long, log: Vector[String],
                         skip: Set[Long], vampHeal: Long = 0L): Task[TurnResult] = {
    // Минибосс, стоящий вне пары (Некромант за спинами своих), круга не теряет:
    // колдует он тем же порядком, где бы ни стоял. Тому, кто ушёл из пары своим
    // же шагом, этот ход не достаётся — он уже сходил в фазе пары.
    val casted =
      if (battle.boss.exists(_.holdsPosition) && battle.monsterCurrentHp > 0L)
        bossTurnCast(hero, battle, nowMs, Victim.Hero)
      else ZIO.succeed((battle, hero, ""))
    for {
      c <- casted
      (afterCast0, castHero0, castLine) = c
      // Удар проклятой энергией бьёт и отсюда: свалил союзника — тот уходит из
      // строя, как от удара; по герою — может оставить травму, как всякий приём.
      (afterCast, goneLines) = sweepCastFallen(battle, afterCast0)
      trauma <- hitTrauma(hero, castHero0, nowMs)
      (castHero, traumaLine) = trauma
    } yield {
      val ticked = afterCast.tickBuffs(skip)
      val (tickedHero, tickedBattle, heroEffectLine) = tickHeroEffects(castHero, ticked, nowMs)
      val alive = tickedHero.fightStats.hp > 0
      val fed   = if (alive) endOfRoundHero(tickedHero, tickedBattle, nowMs) else tickedHero
      val lines = (log ++ Vector(castLine).filter(_.nonEmpty) ++ goneLines ++ traumaLine.toVector ++
                    Vector(heroEffectLine).filter(_.nonEmpty)) ++ vampLine(vampHeal)
      TurnResult(fed, tickedBattle, lines, if (alive) Outcome.Continue else Outcome.Death)
    }
  }

  /** Кого из стоявших на ногах до умения оно добило — тех убрать из строя (см.
    * [[sweepFallenAlly]]). Бьёт умение не всегда того, кто стоит напротив:
    * прыжок волка и удар проклятой энергией Некроманта цель ищут сами. */
  private def sweepCastFallen(before: SoloPveBattle, after: SoloPveBattle): (SoloPveBattle, Vector[String]) =
    before.group.allies.filter(_.alive).foldLeft((after, Vector.empty[String])) { case ((b, lines), a) =>
      val (swept, line) = sweepFallenAlly(b, Some(a))
      (swept, lines ++ line.toVector)
    }

  /** Конец раунда: кому из отряда некого бить, шагает к ближайшему врагу
    * (`SoloPveBattle.squadAdvance`). Идут после мобов: те уже подтянулись к
    * герою, и отряд идёт к тем, кто остался вне досягаемости.
    *
    * Только при живом герое: пока он лежит, раунды идут в `squadTick`, и отряд
    * шагает там. */
  /** Конец раунда: паутина на герое и на союзниках держит на раунд меньше —
    * после всех шагов раунда, чтобы она держала и их (см. SpiderWeb). Так же и
    * проклятие Некроманта (см. HealCurse): после всего лечения раунда. Ход,
    * который раунда не кончил, их не трогает. */
  private def webTick(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound) res
    else res.copy(battle = BattleState.webTicked(res.battle))

  private def squadStep(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle.group.heroDown) res
    else res.copy(battle = res.battle.squadAdvance._1)

  /** Кто сам дошёл до места напротив героя, тот и встаёт с ним в пару (конец
    * раунда). Издалека никого не тянем: павший в паре замены не подставляет, и
    * место перед героем враги занимают сами, шаг за шагом. */
  private def pullFreeStep(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle.group.heroDown) res
    else {
      // Сперва тот, кто в полях боя, делает свой шаг, потом смотрим, не дошёл ли
      // кто-нибудь до места напротив героя.
      val stepped = res.battle.stepActiveTowardsHero.getOrElse(res.battle)
      val paired  = stepped.engageArrived.getOrElse(stepped)
      if (paired.group.paired && !res.battle.group.paired)
        res.copy(battle = paired,
          sideLog = res.sideLog :+ content.format("battle.group.steps", "monster" -> paired.monsterName))
      else res.copy(battle = paired)
    }

  /** Базовая атака после умения, когда пары нет: по ближайшей досягаемой
    * цели, а если бить некого — ход кончается тихо. */
  private def followUp(hero: Hero, battle: SoloPveBattle, nowMs: Long, log: Vector[String], skip: Set[Long]): Task[TurnResult] =
    heroTargets(hero, battle).sortBy(p => math.abs(p - battle.group.heroPos)).headOption match {
      case None      => quietPhase(hero, battle, nowMs, log, skip)
      case Some(pos) => strikeSide(hero, battle, pos, nowMs, log, skip).flatMap(closeTurn(_, nowMs, skip))
    }

  /** Базовая атака игрока. `log` — уже накопленные строки хода (например, строка
    * применённого скилла, после которого идёт базовая атака). `skipSlots`
    * пробрасывается в `monsterPhase → tickBuffs`, чтобы только что использованный
    * слот не тикался в этом же ходу (см. SoloPveBattle.tickBuffs).
    */
  /** `isRepeat` — это переигранный промах от «Охотника» (порог 6). Такой заход
    * не повторяется ещё раз — не чаще раза за раунд. */
  private def playerStrike(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      log: Vector[String],
      skip: Set[Long],
      isRepeat: Boolean = false,
      // false — только удар, без ответа моба: вызывающий сам решит, кто отвечает
      // (удар по соседу возвращает героя в пару, и отвечает моб оттуда).
      retaliate: Boolean = true,
      // true — удар без промаха: броска попадания нет (Контрудар).
      sureHit: Boolean = false
  ): Task[TurnResult] =
    for {
      buffedEff <- ZIO.succeed(effWithAir(hero, battle, nowMs))
      // «Тихая роща» (порог 6): первый удар боя — из засады, без промаха и сильнее.
      ambush     = hero.sets.ambushes && !battle.heroStruck
      hitRoll <- if (sureHit || ambush) ZIO.succeed(101) else Random.nextIntBetween(1, 101)
      mobDodge   = mobDodgeChance(buffedEff.accuracy, battle)
      // Попадание считается как hitRoll(1..100) > dodge, т.е. фактический шанс =
      // 100 - floor(dodge) — тот же, что на экране боя (см. buildBattleScreen).
      result <-
        if (hitRoll > mobDodge) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            // Без оружия — когда пусты обе руки.
            noWeapon = hero.equipment.unarmed
            weaponMod: Double = if (noWeapon) 0.5 else 1.0
            // Из лука вблизи — на пятую часть слабее (см. rangedCloseMod).
            closeMod = BattleState.rangedCloseMod(hero, battle)
            // «Разбойник» (+5%) множит урон обычной атаки — умения он не трогает. Усиление стихии сюда НЕ
            // входит: оно сдвигает грани урона по броне/HP в splitElementalDamage.
            // Пыль, севшая неудачно (всполох магии или четвёртая горсть), режет
            // весь урон героя на четверть — ровно этот бой (см. WeaponDust).
            // «Мерзавец» бьёт на 5% сильнее — и получает на 5% больше (см. mobStrike).
            // Стена трофеев прибавляет по проценту за каждого именного той же расы.
            // По горящему врагу «Преисподняя» (порог 6) бьёт сильнее.
            damage =
              (((hero.effectiveBaseStats(nowMs).str * 3L + buffedEff.atk) * spread / 100L) *
                weaponMod * closeMod * hero.passives.finalDamageMult * hero.weaponDust.damageMult *
                Achievement.damagePct(hero) / 100.0 * battle.trophyMult * burningMult(hero, battle) *
                (if (ambush) hero.sets.ambushDamagePct / 100.0 else 1.0)).toLong
                .max(1L)
            // Стихии оружия модифицируют раздельно урон по броне и по HP
            // (см. splitElementalDamage). Без стихий поведение прежнее.
            // Сопротивление минибосса стихии оружия — до разбивки по броне/HP.
            resisted = (damage * bossResistance(hero, battle)).toLong.max(1L)
            // Защита моба режет удар ДО разбивки по броне/HP, иначе поехали бы
            // грани стихий (см. splitElementalDamage).
            guarded = (resisted * (1.0 - monsterDefenceCut(hero, battle, buffedEff, nowMs))).toLong.max(1L)
            (armorRaw, hpRaw) = splitElementalDamage(battle.monsterCurrentArmor, guarded, hero)
            // Предел минибосса за раунд: больше 40% HP и 40% брони он не
            // теряет, сколько бы герой ни вложил в удар (см. BossRoundCap).
            hurt     = battle.hurtActive(armorRaw, hpRaw)
            armorDmg = hurt.armor
            hpDmg    = hurt.hp
            // В лог идёт то, что РЕАЛЬНО ушло в цель: сырое число сбивало с толку
            // там, где сопротивление режет удар в разы (каменного элементаля голая
            // сталь берёт на 20%, и «357 урона» превращались в 71 снятой брони).
            attackLine = content.format(
              "battle.hit",
              "damage"  -> (armorDmg + hpDmg).toString,
              "monster" -> battle.monsterName
            ) + capLine(hurt).map("\n" + _).getOrElse("")
            newHp    = hurt.battle.monsterCurrentHp
            // Баф «ядовитые атаки»: на любом попадании снимается. Моб травится, только
            // если удар прошёл в HP (hpDmg > 0) и моб жив: повторное попадание стакает
            // текущий яд (+OnHit), иначе накладывает свежий.
            poisonsNow = battle.effects.heroPoisonousAttacks && hpDmg > 0 && newHp > 0
            effectsAfterPotion =
              if (!battle.effects.heroPoisonousAttacks) battle.effects
              else battle.effects.copy(
                heroPoisonousAttacks = false,
                monsterPoison =
                  if (poisonsNow) Some(battle.effects.monsterPoison.map(_.stacked).getOrElse(Poison.onHit))
                  else battle.effects.monsterPoison
              )
            // Изумруд в оружии: при уроне по HP пассивно накладывает/стакает яд.
            gemPoisonPct = hero.gems.weaponPoisonPct
            gemPoisons   = gemPoisonPct > 0 && hpDmg > 0 && newHp > 0
            effectsAfterGem =
              if (!gemPoisons) effectsAfterPotion
              else effectsAfterPotion.copy(
                monsterPoison = Some(effectsAfterPotion.monsterPoison
                  .map(p => Poison(p.pct + gemPoisonPct))
                  .getOrElse(Poison(gemPoisonPct))))
            // Фляги яда и крови (на раунды) и смазки из отваров (на весь бой): пока
            // оружие смазано, удар по HP травит (как отравленное оружие) либо пускает
            // кровь — стакается с тем, что висит.
            coatPoisons = (battle.effects.heroPoisonCoat || hero.weaponDust.poisonCoated ||
                            hero.weaponDust.redVenomCoated) && hpDmg > 0 && newHp > 0
            coatBleeds  = (battle.effects.heroBleedCoat || hero.weaponDust.bleedCoated) && hpDmg > 0 && newHp > 0
            effectsCoated =
              if (!coatPoisons) effectsAfterGem
              else effectsAfterGem.copy(
                monsterPoison = Some(effectsAfterGem.monsterPoison.map(_.stacked).getOrElse(Poison.onHit)))
            effects =
              if (!coatBleeds) effectsCoated
              else effectsCoated.copy(
                monsterBleed = Some(effectsCoated.monsterBleed.map(_.stackedWith(FlaskRates.CoatBleedPct))
                                 .getOrElse(Bleed(FlaskRates.CoatBleedPct))))
            // Яд красного паука на клинке: удар по HP с шансом сбивает врагу
            // ближайшее умение — тем же блоком, что и комбо Молния+Холод. Бросок
            // без смазки не тратится.
            venomBlocks <- chanceRoll(hero.weaponDust.redVenomCoated && hpDmg > 0 && newHp > 0,
                             BrewRates.RedVenomBlockPct)
            effectsVenom =
              if (!venomBlocks) effects
              else effects.copy(monsterSkillBlockedTurns =
                effects.monsterSkillBlockedTurns.max(BattleState.VenomSkillBlockTurns))
            // «Упырь» (порог 6): шанс, что удар по HP пустит цели кровь. Бросок
            // не тратится, если набора нет, — детерминизм боевых тестов.
            setBleeds <- chanceRoll(
                           hero.sets.bleedOnHitChancePct > 0 && hpDmg > 0 && newHp > 0,
                           hero.sets.bleedOnHitChancePct)
            effectsBled =
              if (!setBleeds) effectsVenom
              else effectsVenom.copy(monsterBleed = Some(
                effectsVenom.monsterBleed.map(_.stackedWith(hero.sets.bleedPct)).getOrElse(Bleed(hero.sets.bleedPct))))
            // «Крыса» (порог 4): шанс, что удар по HP ещё и отравит. Бросок без
            // набора не тратится — детерминизм боевых тестов.
            setPoisons <- chanceRoll(
                            hero.sets.poisonOnHitChancePct > 0 && hpDmg > 0 && newHp > 0,
                            hero.sets.poisonOnHitChancePct)
            effectsRat =
              if (!setPoisons) effectsBled
              else effectsBled.copy(monsterPoison = Some(
                effectsBled.monsterPoison.map(p => Poison(p.pct + hero.sets.poisonPct))
                  .getOrElse(Poison(hero.sets.poisonPct))))
            // «Тихая роща» (порог 9): удар по HP с шансом ослабляет врага на два
            // раунда. Висящее ослабление посильнее не перебивается.
            weakens <- chanceRoll(hero.sets.weakenOnHitChancePct > 0 && hpDmg > 0 && newHp > 0,
                         hero.sets.weakenOnHitChancePct)
            effectsWeak =
              if (!weakens) effectsRat
              else effectsRat.copy(
                monsterWeakenedTurns = effectsRat.monsterWeakenedTurns.max(ItemSet.QuietGrove.WeakenTurns),
                monsterWeakenedPct   = effectsRat.monsterWeakenedPct.max(ItemSet.QuietGrove.WeakenPct))
            // Череп в оружии, «Упырь» (порог 4) и «Орда» (порог 12, ниже половины
            // HP): вампиризм с урона по HP. Кровь пьётся только с раны — удар,
            // целиком поглощённый бронёй, не лечит.
            maxHp      = hero.effectiveMaxHp(nowMs)
            vamp       = if (hero.gems.vampirismPct > 0 && hpDmg > 0) (hpDmg * hero.gems.vampirismPct / 100L).max(0L) else 0L
            stealPct   = hero.sets.lifestealPct + hero.sets.woundLifestealPct(hero.fightStats.hp, maxHp)
            setSteal   = if (stealPct > 0 && hpDmg > 0) (hpDmg * stealPct / 100L).max(0L) else 0L
            // Вампирская фляга: удар по HP лечит и тратит один из её ударов.
            flaskBite  = battle.effects.heroVampiricHits > 0 && hpDmg > 0
            flaskSteal = if (flaskBite) (hpDmg * FlaskRates.VampiricPct / 100L).max(0L) else 0L
            // Проклятого Некромантом выпитая кровь не лечит (см. HealCurse);
            // удар вампирской фляги при этом всё равно тратится.
            stolen     = if (battle.effects.heroCursed) 0L else vamp + setSteal + flaskSteal
            healedHero = if (stolen > 0) hero.copy(fightStats = hero.fightStats.copy(hp = (hero.fightStats.hp + stolen).min(maxHp))) else hero
            vampGained = healedHero.fightStats.hp - hero.fightStats.hp
            effectsFed = if (!flaskBite) effectsWeak else effectsWeak.copy(heroVampiricHits = effectsWeak.heroVampiricHits - 1)
            hitBattle = hurt.battle.copy(heroStruck = true).withEffects(effectsFed)
            // Проки стихий оружия (30% каждый) — только если моб жив после удара.
            elemResult <- if (newHp > 0) resolveElementProcs(hero, hitBattle, hurt.hp, skill = false, hurt.dealt)
                          else ZIO.succeed((hitBattle, Vector.empty[String], Set.empty[Element]))
            (procced, elemLog, fired) = elemResult
            // «Стужа» (порог 6): прок холода возвращает герою часть брони.
            (icedHero, iceLog) = frostArmor(healedHero, fired, nowMs)
            // «Пройдоха» (порог 6): грязный приём — удар по HP с шансом выжигает
            // врагу энергию, как Молния. Бросок без набора не тратится.
            tricked <- chanceRoll(hero.sets.energyBurnOnHitChancePct > 0 && hpDmg > 0 && newHp > 0,
                         hero.sets.energyBurnOnHitChancePct)
            (updated, trickLog) =
              if (!tricked) (procced, Vector.empty[String])
              else {
                val (burnt, lost) = BattleState.burnEnergy(procced, hero.sets.energyBurnPct)
                (burnt, Vector(content.format("battle.tricksterTrick", "monster" -> battle.monsterName,
                  "energy" -> lost.toString)))
              }
            // Шипы огненного элементаля: пока цела броня, он возвращает часть
            // урона и поджигает бьющего. Скованный холодом молчит.
            thornsResult = bossThorns(icedHero, updated, battle.monsterCurrentArmor, resisted, nowMs)
            (thornedHero, thornedBattle, thornsLine) = thornsResult
            log1 = (log ++ Option.when(ambush)(content.text("battle.groveAmbush")).toVector) :+ attackLine
            log2 =
              (if (poisonsNow || gemPoisons || coatPoisons || setPoisons)
                log1 :+ content.format("battle.poisonApplied", "monster" -> battle.monsterName)
              else log1) ++ Option.when(weakens)(content.format("battle.groveWeaken", "monster" -> battle.monsterName,
                "pct" -> ItemSet.QuietGrove.WeakenPct.toString, "turns" -> ItemSet.QuietGrove.WeakenTurns.toString)).toVector ++
                Option.when(venomBlocks)(content.format("battle.redVenomBlocks", "monster" -> battle.monsterName)).toVector
            // Сам вампиризм о себе не говорит: его число уезжает в конец раунда и
            // там складывается с чужой кровью в одну строку (см. vampLine). Если
            // раунд кончился раньше хода моба, строку дописываем здесь же.
            log4 = (log2 ++ elemLog ++ iceLog ++ trickLog) ++ Vector(thornsLine).filter(_.nonEmpty)
            fed  = log4 ++ vampLine(vampGained)
            r <-
              if (!retaliate) ZIO.succeed(TurnResult(thornedHero, thornedBattle, fed, Outcome.Continue))
              else if (thornedBattle.monsterCurrentHp <= 0) ZIO.succeed(victoryByHero(thornedHero, thornedBattle, fed, nowMs, skip))
              else if (thornedHero.fightStats.hp <= 0) ZIO.succeed(TurnResult(thornedHero, thornedBattle, fed, Outcome.Death))
              else respond(thornedHero, thornedBattle, nowMs, log4, skip, vampGained)
          } yield r
        } else {
          val attackLine = content.text("battle.miss")
          val missLog    = log :+ attackLine
          // «Охотник» (порог 6): промах можно переиграть — но не больше одного
          // повтора за раунд, поэтому повторный заход идёт с canRepeat = false.
          // Бросок не тратится без набора: детерминизм боевых тестов.
          chanceRoll(!isRepeat && hero.sets.repeatOnMissChancePct > 0, hero.sets.repeatOnMissChancePct)
            .flatMap { repeats =>
              if (repeats)
                playerStrike(hero, battle, nowMs, missLog :+ content.text("battle.hunterRepeat"), skip,
                  isRepeat = true, retaliate = retaliate)
              else if (!retaliate) ZIO.succeed(TurnResult(hero, battle, missLog, Outcome.Continue))
              else respond(hero, battle, nowMs, missLog, skip)
            }
        }
    } yield result

  // ── Стихии оружия ───────────────────────────────────────────────────────────

  /** Боевые статы героя с учётом бафов боя, прибавок наборов, которые зависят
    * от хода боя (раунды, раны), временного буста Воздуха (прок Воздуха: +10%
    * уклонения/точности на `airBoostTurns` ходов) и дебафов каменного
    * элементаля. Постоянный +5% от воздуха в оружии уже учтён в
    * `hero.effectiveFightStats`.
    *
    * Единая точка: и уклонение, и снижение урона, и шанс попасть по мобу считают
    * статы отсюда, поэтому дебаф достаточно применить здесь один раз. */
  private def effWithAir(hero: Hero, battle: SoloPveBattle, nowMs: Long): FightStats = {
    val buffed = battle.heroBattleState.applyTo(hero.effectiveFightStats(nowMs))
    // «Первопроходец» (порог 6) с каждым раундом прибавляет атаку и защиту,
    // «Орда» (пороги 9 и 12) — атаку с каждой раной.
    val roundPct = hero.sets.roundBonusPct(battle.roundsFought)
    val atkPct   = roundPct + hero.sets.woundAttackPct(hero.fightStats.hp, hero.effectiveMaxHp(nowMs))
    val fs0 =
      if (atkPct == 0L) buffed
      else buffed.copy(
        atk     = buffed.atk + buffed.atk * atkPct / 100L,
        defence = buffed.defence + buffed.defence * roundPct / 100L
      )
    val fs =
      if (battle.effects.airBoostTurns <= 0) fs0
      else {
        val p = Element.Air.ProcBonusPct
        fs0.copy(
          evasion  = fs0.evasion + fs0.evasion * p / 100L,
          accuracy = fs0.accuracy + fs0.accuracy * p / 100L
        )
      }
    // Каменный элементаль: россыпь валунов сбивает защиту и уклонение, вязкая
    // земля — точность и уклонение. Уклонению может достаться от обоих сразу.
    val stunned  = if (battle.effects.heroStunned) MiniBoss.StoneElemental.BurstDebuffPct else 0L
    val grounded = if (battle.effects.heroGrounded) MiniBoss.StoneElemental.GroundCutPct else 0L
    val debuffed =
      if (stunned == 0L && grounded == 0L) fs
      else fs.copy(
        defence  = fs.defence * (100L - stunned) / 100L,
        evasion  = fs.evasion * (100L - stunned - grounded).max(0L) / 100L,
        accuracy = fs.accuracy * (100L - grounded) / 100L
      )
    // Сборщик тянет узор (см. PatternPull): атака героя ниже, пока держится.
    if (!battle.effects.heroDrained) debuffed
    else debuffed.copy(atk = PatternPull.drainedAtk(debuffed.atk))
  }

  /** «Упырь» (порог 12): каждый убитый враг — пир, и герой восстанавливает часть
    * HP и брони ТУТ ЖЕ, в бою, а не по его исходу. Считаем по числу павших за
    * ход: в группе их может лечь несколько, и за каждого положена своя доля.
    * Проценты считаются от эффективных потолков, восстановленное не может
    * опустить текущее значение (если оно почему-то выше потолка). */
  private def ghoulFeast(before: SoloPveBattle, res: TurnResult, nowMs: Long): TurnResult =
    ghoulFeastFor(heroKills(before, res), res, nowMs)

  /** Пир за `kills` добитых героем — см. [[ghoulFeast]]; в ход мобов их считает
    * [[killsInMobTurn]]. */
  private def ghoulFeastFor(kills: Int, res: TurnResult, nowMs: Long): TurnResult = {
    if (kills <= 0 || !res.hero.sets.feastsOnKill) res
    else {
      val hero     = res.hero
      val maxHp    = hero.effectiveMaxHp(nowMs)
      val maxArmor = hero.effectiveMaxArmor(nowMs)
      // Проклятого пир кормит только бронёй: раны не затягиваются (см. HealCurse).
      val newHp    =
        if (res.battle.effects.heroCursed) hero.fightStats.hp
        else (hero.fightStats.hp + maxHp * hero.sets.feastHpPct * kills / 100L).min(maxHp).max(hero.fightStats.hp)
      val newArmor = (hero.fightStats.armor + maxArmor * hero.sets.feastArmorPct * kills / 100L).min(maxArmor).max(hero.fightStats.armor)
      val hpGained    = newHp - hero.fightStats.hp
      val armorGained = newArmor - hero.fightStats.armor
      if (hpGained <= 0 && armorGained <= 0) res
      else res.copy(
        hero = hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor)),
        log  = res.log :+ content.format("battle.ghoulFeast",
                 "hp" -> hpGained.toString, "armor" -> armorGained.toString))
    }
  }

  /** Сколько врагов добил герой своим ходом: павшие в строю попадают в `slain`,
    * а тот, кто остался в полях с нулём HP (последний в бою), в этот список не
    * идёт — его считаем отдельно. */
  private def heroKills(before: SoloPveBattle, res: TurnResult): Int = {
    val inLine = res.battle.group.slain.size - before.group.slain.size
    val active = if (res.battle.monsterCurrentHp <= 0L && before.monsterCurrentHp > 0L) 1 else 0
    (inLine + active).max(0)
  }

  /** Кого добил герой в ход одного моба: тот истёк ранами — кровью, ядом,
    * пламенем от ударов героя, — напоролся на шипы или попал под контрудар.
    * Павшие за этот ход, кроме засчитанных союзнику: он бил последним (см.
    * [[creditKill]]) — тот же счёт, что у итогов боя ([[BattleState.killedByHero]]).
    * Лежащий или павший герой ничего не добивает. */
  private def heroKillsInMobTurn(before: SoloPveBattle, res: TurnResult): Int =
    if (res.battle.group.heroDown || res.hero.fightStats.hp <= 0L) 0
    else (heroKills(before, res) - (allyKills(res.battle) - allyKills(before))).max(0)

  private def allyKills(b: SoloPveBattle): Int = (b.group.allies ++ b.group.alliesGone).map(_.kills).sum

  /** Добитые героем в ход моба — тоже его добитые: пир «Упыря», напор
    * «Преисподней», «Некромант» — тут же, как за удар героя, и до хода
    * следующего моба. `before` — бой до хода этого моба, `hero` и `after` —
    * после. Возвращает героя, бой и строки об этом. */
  private def killsInMobTurn(before: SoloPveBattle, hero: Hero, after: SoloPveBattle,
                             nowMs: Long): Task[(Hero, SoloPveBattle, Vector[String])] = {
    val res  = TurnResult(hero, after, Vector.empty, Outcome.Continue)
    val mine = heroKillsInMobTurn(before, res)
    if (mine <= 0) ZIO.succeed((hero, after, Vector.empty))
    else {
      val fed = underworldPushFor(mine, ghoulFeastFor(mine, res, nowMs), nowMs)
      // Встать за героем может только его добитый: все павшие за этот ход — его.
      (if (mine == heroKills(before, res)) necroRise(before, fed) else ZIO.succeed(fed))
        .map(r => (r.hero, r.battle, r.log))
    }
  }

  /** «Преисподняя» (порог 12, только демону): каждый добитый героем враг
    * возвращает долю максимума энергии — тут же, в бою. Считаем так же, как
    * пир упыря, до хода союзников. */
  private def underworldPush(before: SoloPveBattle, res: TurnResult, nowMs: Long): TurnResult =
    underworldPushFor(heroKills(before, res), res, nowMs)

  /** Напор за `kills` добитых героем — см. [[underworldPush]]. */
  private def underworldPushFor(kills: Int, res: TurnResult, nowMs: Long): TurnResult = {
    val pct   = res.hero.sets.killEnergyPct
    if (kills <= 0 || pct <= 0L) res
    else {
      val hero   = res.hero
      val maxEn  = hero.maxEnergy(nowMs)
      val newEn  = (hero.fightStats.energy + maxEn * pct * kills / 100L).min(maxEn).max(hero.fightStats.energy)
      val gained = newEn - hero.fightStats.energy
      if (gained <= 0L) res
      else res.copy(
        hero = hero.copy(fightStats = hero.fightStats.copy(energy = newEn)),
        log  = res.log :+ content.format("battle.underworldPush", "energy" -> gained.toString))
    }
  }

  /** Сколько ходов держится попутный ветер, поднятый Воздухом героя (с оружия
    * или из фляги): «Пустынь» (порог 6) прибавляет к нему ход. */
  private def heroAirTurns(hero: Hero): Int = Element.Air.ProcTurns + hero.sets.airTurnsBonus

  /** «Преисподняя» (порог 6): по горящему врагу удар и умение сильнее. */
  private def burningMult(hero: Hero, battle: SoloPveBattle): Double =
    if (battle.effects.monsterBurn.isDefined) 1.0 + hero.sets.burningDamageBonusPct / 100.0 else 1.0

  /** «Некромант» (порог 12): кого герой добил сам, тот может встать и пойти за
    * ним. Считаем так же, как пир упыря, — по разнице павших до и после хода, и
    * до хода союзников: их добычу некромант не поднимает.
    *
    * Восставший выходит в строй сразу, с [[ItemSet.Necromancer.RiseHpPct]]
    * процентами HP и без брони (её сбили, пока он был жив), а после боя уходит с
    * героем на двое суток. Статы — как у поднятого с алтаря, по тому же трофею, и
    * имя своё: названный врагом остаётся названным.
    *
    * Не поднимаются те, в кого поднимать нечего: элементали — чистая стихия,
    * сооружения — железо, Гнилой Джо и так нежить, а Крысиный король — не тело,
    * а сборище крыс. Белый волк встаёт как по клыку. */
  private def necroRise(before: SoloPveBattle, res: TurnResult): Task[TurnResult] =
    if (res.hero.sets.riseChancePct <= 0L) ZIO.succeed(res)
    else {
      val fresh  = res.battle.group.slain.drop(before.group.slain.size)
      val active = Option.when(res.battle.monsterCurrentHp <= 0L && before.monsterCurrentHp > 0L)(
        before.slainActive -> before.boss)
      val fallen = fresh.map(_ -> Option.empty[MiniBoss]) ++ active.toList
      ZIO.foldLeft(fallen)(res) { case (acc, (slain, boss)) =>
        BattleState.riseForm(slain, boss) match {
          case None       => ZIO.succeed(acc)
          case Some(form) =>
            chanceRoll(true, acc.hero.sets.riseChancePct).map {
              case false => acc
              case true  =>
                // Тёмный волк в строю один: поднятый второй вцепляется в
                // прежнего и побеждает — встаёт на его место, а прежний уходит
                // в павшие (после боя его нет и в отряде, см. squadAfterBattle).
                val rival = if (form.unique) acc.battle.group.darkWolf else None
                rival.map(_.position).orElse(acc.battle.group.nearestFreeAllyPlace).fold(acc) { pos =>
                  val hp = (form.stats.hp * ItemSet.Necromancer.RiseHpPct / 100L).max(1L)
                  val ally = BattleAlly(pangea.model.squad.AllyKind.Undead, pos,
                    hp = hp, armor = 0L, energy = form.stats.energy,
                    stats = form.stats, lvl = form.lvl, undead = Some(form), home = 0)
                  val group = rival.fold(acc.battle.group)(old =>
                    acc.battle.group.updateAlly(old.position)(_.copy(hp = 0L, lastHitBy = Some(form.name)))
                      .withoutAlly(old.position))
                  val line = if (rival.isDefined) "battle.necroWolfFight" else "battle.necroRise"
                  acc.copy(
                    battle = acc.battle.copy(group = group.copy(allies = group.allies :+ ally)),
                    log = acc.log :+ content.format(line, "name" -> form.name, "n" -> pos.toString))
                }
            }
        }
      }
    }

  /** Насколько урон героя по элементалю ослаблен или усилен его стихией: своя
    * стихия почти не вредит (огонь по огненному — 20%), противоположная бьёт
    * сильнее (холод — 150%). Оружие сразу с двумя стихиями перемножает их
    * множители. Для обычных мобов всегда 1.0. */
  private def bossResistance(hero: Hero, battle: SoloPveBattle): Double =
    battle.boss.fold(1.0) { e =>
      // Оружие без единой стихии — «голая сталь»: каменного она почти не берёт.
      if (hero.gems.weaponElements.isEmpty) e.plainDamageTakenMult
      else hero.gems.weaponElements.foldLeft(1.0)((m, el) => m * e.damageTakenMult(el))
    }

  /** Как удар моба ложится на героя. Обычно — привычным порядком (сперва броня,
    * остаток в HP). У каменного элементаля грани РАЗДЕЛЬНЫ: пока броня цела, 90%
    * урона снимает её и одновременно 30% уходит в HP, так что броня от него не
    * спасает; чего броня не покрыла — уходит в HP, и у героя без брони удар
    * целиком приходится на здоровье.
    *
    * Моб, посыпавший оружие порошком, бьёт стихией: её грани двигают доли урона
    * по броне и HP так же, как стихии оружия двигают урон героя по мобу.
    * Возвращает новые hp и armor героя и сколько удар реально снял (броня плюс
    * HP, после стихий и расколов) — это число и идёт в лог. */
  private def bossHit(battle: SoloPveBattle, hero: Hero, damage0: Long, nowMs: Long): (Long, Long, Long) = {
    // «Некромант» (порог 10): мёртвая плоть занимается охотнее — огненный удар
    // по ней весомее. Прочие стихии мертвецу всё равно.
    val damage =
      if (powderElement(battle).contains(Element.Fire)) (damage0 * hero.sets.fireTakenMult).toLong.max(1L)
      else damage0
    // «Орда» (порог 6): что дошло до HP, режет упрямство — если удар большой.
    def toHpAfter(loss: Long): Long = hero.sets.stubbornLoss(loss, hero.effectiveMaxHp(nowMs))
    battle.boss.flatMap(_.heroHitSplit) match {
      // Минибосс с расколом: пара чисел — ДОЛИ удара, каменный бьёт и в броню,
      // и в HP мимо неё одновременно, это его особенность.
      case Some((armorPart, hpPart)) =>
        val curArmor = hero.fightStats.armor.max(0L)
        val absorbed = (damage * armorPart).toLong.min(curArmor)
        val toHp     = toHpAfter((damage - absorbed).max((damage * hpPart).toLong))
        ((hero.fightStats.hp - toHp).max(0L), curArmor - hero.sets.armorSpent(absorbed), absorbed + toHp)
      case None =>
        powderSplit(battle) match {
          case None => MonsterSkill.physicalHit(battle, hero, damage, nowMs)
          // Стихия порошка: пара чисел — МНОЖИТЕЛИ, как у стихии в оружии героя.
          // Броня поглощает удар (со своим множителем), в HP идёт только то, что
          // за неё вылилось (со своим). Подставлять множители в раскол минибосса
          // нельзя: огонь с 1.1 по HP гнал 110% удара сквозь броню.
          case Some((armorMult, hpMult)) =>
            val curArmor  = hero.fightStats.armor.max(0L)
            val rawArmor  = math.min(curArmor, damage)
            val rawHp     = damage - rawArmor
            val armorDmg  = (rawArmor * armorMult).toLong.min(curArmor).max(0L)
            val hpDmg     = toHpAfter(((rawHp * hpMult).toLong + lightningPierce(battle, armorDmg)).max(0L))
            ((hero.fightStats.hp - hpDmg).max(0L), curArmor - hero.sets.armorSpent(armorDmg), armorDmg + hpDmg)
        }
    }
  }

  /** Шипы огненного элементаля. Пока у него ЦЕЛА БРОНЯ (проверяется её запас до
    * удара), обычная атака героя возвращается частью урона и поджигает бьющего.
    * Скованный холодом элементаль шипами не отвечает.
    *
    * Возвращает героя после отдачи, бой с наложенным горением и строку лога
    * (пустую, если шипов не было). */
  private def bossThorns(
      hero: Hero,
      battle: SoloPveBattle,
      armorBeforeHit: Long,
      damageDealt: Long,
      nowMs: Long
  ): (Hero, SoloPveBattle, String) =
    battle.boss match {
      // Шипы — особенность огненного: о каменного руки не обжигают.
      case Some(MiniBoss.FireElemental) if armorBeforeHit > 0 && !battle.effects.chilled =>
        val thorns = bossDamageTaken(hero, battle, (damageDealt * MiniBoss.FireElemental.ThornsPct / 100L).max(1L))
        val (hurt, _) = hurtHero(hero, thorns, nowMs)
        // Пламя шипов по герою под холодом: лёд тает, огонь не занимается.
        val thawed = battle.effects.coldOnHero
        val burned =
          if (thawed) battle.copy(effects = battle.effects.copy(heroColdDefenceCut = 0))
          else if (battle.effects.warded(Ward.Fire)) battle
          else battle.copy(effects = battle.effects.copy(heroBurn = Some(
            battle.effects.heroBurn.map(_.reignited).getOrElse(Burn(MiniBoss.FireElemental.ThornsBurnPct)))))
        (hurt, burned, content.format("battle.elemental.thorns", "damage" -> thorns.toString) +
          (if (thawed) "\n" + content.text("battle.elements.thawHero") else ""))
      case _ => (hero, battle, "")
    }

  /** Ход элементаля-минибосса: он применяет способности строго ПО КРУГУ, а не
    * случайно, как рядовые мобы. Не хватило энергии или способность сейчас
    * бесполезна (щит на целом элементале) — раунд пропускается, но очередь всё
    * равно сдвигается: иначе он навсегда застрял бы на дорогом умении.
    *
    * Возвращает то же, что обычный каст: обновлённый бой, героя и строку лога. */
  private def bossTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] =
    battle.boss match {
      case Some(MiniBoss.StoneElemental) => stoneTurnCast(hero, battle, nowMs, victim)
      case Some(MiniBoss.ThunderElemental) => thunderTurnCast(hero, battle, nowMs, victim)
      case Some(MiniBoss.IceElemental)   => iceTurnCast(hero, battle, nowMs, victim)
      case Some(MiniBoss.RottenJoe)      => joeTurnCast(hero, battle, nowMs, victim)
      case Some(MiniBoss.WhiteWolf)      => wolfTurnCast(hero, battle, nowMs, victim)
      case Some(MiniBoss.RatKing)        => ratKingTurnCast(hero, battle)
      case Some(MiniBoss.Necromancer)    => necromancerTurnCast(hero, battle, nowMs)
      case _                             => fireTurnCast(hero, battle, nowMs, victim)
    }

  /** Сколько сейчас HP у того, по кому бьёт приём, — чтобы понять, дошло ли до
    * тела (кровь и травмы вешают только тогда). */
  private def victimHp(v: Victim, hero: Hero, battle: SoloPveBattle): Long = v match {
    case Victim.Hero      => hero.fightStats.hp
    case Victim.Ally(pos) => battle.group.allyAt(pos).map(_.hp).getOrElse(0L)
  }

  /** Сколько тому, по кому бьёт приём, не хватает до полного здоровья. */
  private def victimMissingHp(v: Victim, hero: Hero, battle: SoloPveBattle, nowMs: Long): Long = v match {
    case Victim.Hero      => (hero.effectiveMaxHp(nowMs) - hero.fightStats.hp).max(0L)
    case Victim.Ally(pos) => battle.group.allyAt(pos).map(a => (a.stats.hp - a.hp).max(0L)).getOrElse(0L)
  }

  /** Кого минибосс в полях боя достаёт приёмом, бьющим на `reach` мест, — как
    * обычным ударом, только дальше: союзника напротив; нет его — героя (лежачего
    * не трогают); нет и его — ближайшего союзника, не дальше `reach` мест. Цель
    * и её место. Так ищут цель прыжок Белого волка и удар проклятой энергией
    * Некроманта. */
  private def reachTarget(b: SoloPveBattle, reach: Int): Option[(Victim, Int)] = {
    val g   = b.group
    val pos = g.activePos
    g.allyAt(pos).filter(_.alive).map(a => (Victim.Ally(a.position): Victim) -> a.position)
      .orElse(Option.when(!g.heroDown && math.abs(pos - g.heroPos) <= reach)((Victim.Hero: Victim) -> g.heroPos))
      .orElse(b.allyWithin(pos, reach).map(a => (Victim.Ally(a.position): Victim) -> a.position))
  }

  /** Урон по цели приёма мимо брони — весь в HP: так бьёт проклятая энергия
    * Некроманта. Герою — с поправкой на поднятый щит, наборы и «Упрямство», как
    * всякий приём минибосса (см. [[hurtVictim]]), только броня удара не
    * принимает; союзнику — по его защите и тоже мимо брони. Возвращает героя,
    * бой и сколько урона прошло (для строки лога). */
  private def hurtVictimHp(v: Victim, hero: Hero, battle: SoloPveBattle, damage: Long, nowMs: Long): (Hero, SoloPveBattle, Long) =
    v match {
      case Victim.Hero =>
        val dealt = bossDamageTaken(hero, battle, BattleState.shieldCut(battle.heroBlockPct, damage))
        val toHp  = hero.sets.stubbornLoss(dealt, hero.effectiveMaxHp(nowMs))
        (hero.copy(fightStats = hero.fightStats.copy(hp = (hero.fightStats.hp - toHp).max(0L))), battle, toHp)
      case Victim.Ally(pos) =>
        battle.group.allyAt(pos).filter(_.alive) match {
          case None => (hero, battle, 0L)
          case Some(a) =>
            val reduced = (damage * (1.0 - allyReduction(a, battle.monsterStats.atk))).toLong.max(1L)
            val hurt    = a.copy(hp = (a.hp - reduced).max(0L), lastHitBy = Some(battle.monsterName))
            (hero, battle.copy(group = battle.group.updateAlly(pos)(_ => hurt)), reduced)
        }
    }

  /** Круг Некроманта: удар проклятой энергией → мистический шаг → созидание.
    *
    * Удар проклятой энергией бьёт того, кого он достаёт на три места (цель —
    * как у обычного удара, см. [[reachTarget]]), всей атакой сразу в HP, мимо
    * брони, и проклинает: до конца следующего раунда раны цели не затягиваются
    * (см. HealCurse). Достать некого — пропуск, бесплатный. Шаг уводит его за
    * спины своих — на ближайшее свободное место, перед которым стоит его
    * мертвец, и никогда не ближе к герою. Он и так за спинами своих — стоит, где
    * стоял, молча; прятаться не за кем или некуда — пропуск со словами. Шаг на
    * месте ничего не стоит. Созидание поднимает нового мертвеца той же расы, что
    * пещера, третьего или четвёртого ранга, уровнем в саму пещеру.
    *
    * Не хватило энергии — способность не применяется, но очередь сдвигается. */
  private def necromancerTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long
  ): Task[(SoloPveBattle, Hero, String)] = {
    val necro  = MiniBoss.Necromancer
    val lvl    = battle.monsterLvl
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, necro)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Удар проклятой энергией: вся атака сразу в HP — и проклятие, раны
      // не затягиваются до конца следующего раунда. В дыму из-за спин своих он
      // никого не видит — так же, как не видят мобы вне пары.
      case 0 =>
        val cost = necro.CurseCostPerLvl * lvl
        val prey =
          if (battle.effects.heroInSmoke && !battle.group.paired) None
          else reachTarget(battle, necro.CurseReach).map(_._1)
        prey match {
          case Some(victim) if energy >= cost =>
            val raw = (monsterAttack(battle) * necro.CurseDamageFactor).toLong.max(1L)
            val (hurt, struck, dealt) = hurtVictimHp(victim, hero, spend(cost, next), raw, nowMs)
            val (cursed, line) = victim match {
              case Victim.Hero =>
                (struck.copy(effects = struck.effects.copy(heroCursedTurns = Some(HealCurse.Turns))),
                 content.format("battle.necro.curse", "damage" -> dealt.toString))
              case Victim.Ally(pos) =>
                (struck.copy(group = struck.group.updateAlly(pos)(_.copy(cursed = HealCurse.Turns))),
                 content.format("battle.necro.curseAlly",
                   "name" -> struck.group.allyAt(pos).fold("")(_.name), "damage" -> dealt.toString))
            }
            ZIO.succeed((cursed, hurt, line))
          case _ => ZIO.succeed((next, hero, ""))
        }

      // 2) Мистический шаг: за спины своих. Кто и так за ними, тот не уходит.
      case 1 =>
        val cost = necro.StepCostPerLvl * lvl
        if (battle.necromancerCovered) ZIO.succeed((next, hero, ""))
        else if (battle.effects.monsterRooted) ZIO.succeed((next, hero, content.text("battle.necro.pinned")))
        else battle.necromancerHideaway match {
          case None       => ZIO.succeed((next, hero, content.text("battle.necro.noHide")))
          case Some(_) if energy < cost => ZIO.succeed((next, hero, ""))
          case Some(pos)  =>
            val moved = spend(cost, next).stepActiveTo(pos)
            ZIO.succeed((moved, hero, content.format("battle.necro.step", "n" -> pos.toString)))
        }

      // 3) Созидание: поднимает нового мертвеца.
      case _ =>
        val cost = necro.CreateCostPerLvl * lvl
        // Уровнем в саму пещеру (лабиринт или задание), как и его охрана, а не
        // в свой: у босса уровень — четверть геройского, и поднятый выходил
        // пустышкой. В бою, записанном без уровня пещеры, — по старшему из строя.
        val mobLvl =
          if (battle.minionLvl > 0L) battle.minionLvl
          else battle.group.others.map(_.lvl).maxOption.getOrElse(lvl)
        if (energy < cost || !battle.hasRoom) ZIO.succeed((next, hero, ""))
        else
          for {
            pick   <- Random.nextIntBounded(necro.CreateRarities.size)
            rarity  = necro.CreateRarities(pick)
            // Поднимает он смертных — тех, кто в этой пещере жил. Боссовая раса
            // сюда не годится: мобов такой расы генератор не знает.
            race    = battle.group.originRace.flatMap(Race.withNameOption)
                        .filter(Race.mortals.contains)
                        .orElse(battle.group.others.flatMap(s => Race.withNameOption(s.race))
                                  .find(Race.mortals.contains))
                        .getOrElse(Race.Orc)
            pct    <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
            m       = MonsterGenerator.generateOfRaceAndRarity(mobLvl.toInt, race, rarity)
            slot    = MonsterSlot(m.lvl, race.entryName, rarity.entryName, m.fightStats,
                        m.fightStats.hp, m.fightStats.armor, m.marked,
                        MonsterEnergy.startEnergy(m.lvl, rarity, pct), BattleEffects.empty, undead = true)
            joined  = spend(cost, next).admit(slot)
          } yield (joined, hero, content.format("battle.necro.create", "name" -> slot.name))
    }
  }

  /** Круг Крысиного короля: призыв → пропуск → объединение. */
  private def ratKingTurnCast(
      hero: Hero,
      battle: SoloPveBattle
  ): Task[(SoloPveBattle, Hero, String)] = {
    val king   = MiniBoss.RatKing
    val lvl    = battle.monsterLvl
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, king)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Призыв крыс: из труб лезут ещё две-три, уровнем в саму канализацию.
      case 0 =>
        val cost = king.SummonCostPerLvl * lvl
        if (energy < cost || !battle.hasRoom) ZIO.succeed((next, hero, ""))
        else
          for {
            rolled <- Random.nextIntBetween(king.SummonMin, king.SummonMax + 1)
            // Больше, чем влезет в строй, он не зовёт: очередь за спинами
            // копилась бы весь бой и вываливалась разом.
            n      = rolled.min(battle.group.freePlaces.size).max(1)
            seeds <- ZIO.foreach(List.fill(n)(()))(_ => Random.nextLong)
            pcts  <- ZIO.foreach(seeds)(_ =>
                       Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L))
            ratLvl = battle.minionLvl.max(1L).toInt
            slots  = seeds.zip(pcts).map { case (seed, pct) =>
                       val (rarity, _) = CaveGenerator.rollQuestRarity(Rng(seed))
                       val m = MonsterGenerator.generateOfRaceAndRarity(ratLvl, Race.Animal, rarity)
                       MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats,
                         m.fightStats.hp, m.fightStats.armor, m.marked,
                         MonsterEnergy.startEnergy(m.lvl, m.rarity, pct), BattleEffects.empty)
                     }
            joined = slots.foldLeft(spend(cost, next))((b, s) => b.admit(s))
          } yield (joined, hero, content.text("battle.ratKing.summon"))

      // 2) Пропуск хода.
      case 1 => ZIO.succeed((next, hero, ""))

      // 3) Объединение: он поглощает своего — слабейшего из тех, кто рядом.
      case _ =>
        val cost = king.MergeCostPerLvl * lvl
        val prey = battle.group.others.zipWithIndex.filter(_._1.alive).sortBy(_._1.currentHp).headOption
        prey match {
          case Some((slot, idx)) if energy >= cost =>
            val grown = spend(cost, next).copy(
              monsterStats = battle.monsterStats.copy(
                hp    = battle.monsterStats.hp + slot.stats.hp,
                armor = battle.monsterStats.armor + slot.stats.armor,
                atk   = battle.monsterStats.atk + king.MergeAtkPerLvl * lvl),
              monsterCurrentHp    = battle.monsterCurrentHp + slot.stats.hp,
              monsterCurrentArmor = battle.monsterCurrentArmor + slot.stats.armor,
              // Съеденная в павшие не идёт: её не убивали, и добычи за неё нет.
              group = battle.group.devour(idx))
            ZIO.succeed((grown, hero, content.text("battle.ratKing.merge")))
          case _ => ZIO.succeed((next, hero, ""))
        }
    }
  }

  /** Строка о том, что удар упёрся в предел минибосса за раунд (см.
    * `BossRoundCap`, `SoloPveBattle.hurtActive`) — раз за раунд на HP и раз на
    * броню, а не на каждый срезанный удар. У Крысиного короля от кома
    * отваливается крыса, прочие просто держатся. */
  private def capLine(hurt: SoloPveBattle.Hurt): Option[String] =
    Option.when(hurt.held)(capText(hurt.battle))

  /** Сама строка о пределе — для того, кто стоит в полях боя `b`. */
  private def capText(b: SoloPveBattle): String =
    if (b.boss.contains(MiniBoss.RatKing)) content.text("battle.ratKing.capped")
    else content.format("battle.bossCapped", "monster" -> b.monsterName)

  /** Круг огненного: всплеск → сфера → щит → пропуск. */
  private def fireTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] = {
    val lvl      = battle.monsterLvl
    val atk      = battle.monsterStats.atk
    val energy   = battle.monsterCurrentEnergy
    val next     = nextTurn(battle, MiniBoss.FireElemental)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Огненный всплеск: урон огнём + поджог героя на 1%.
      case 0 =>
        val cost = MiniBoss.FireElemental.SplashCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val raw        = (atk * MiniBoss.FireElemental.SplashDamageFactor).toLong.max(1L)
          val (h, b1, l) = hurtVictim(victim, hero, spend(cost, next), raw, nowMs)
          // Пламя не складывается, а разгорается заново — и на герое, и на том,
          // кто стоит перед элементалем вместо него.
          val (burned, thawed) = reigniteVictim(victim, b1, MiniBoss.FireElemental.SplashBurnPct)
          ZIO.succeed((burned, h,
            content.format("battle.elemental.fireSplash", "damage" -> l.toString) +
              (if (thawed) "\n" + thawLine(victim, b1) else "")))
        }

      // 2) Сфера огня: копится до трёх, третья сразу срывается в смерч.
      case 1 =>
        val cost = MiniBoss.FireElemental.OrbCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val orbs = battle.bossCharges + 1
          if (orbs < MiniBoss.FireElemental.OrbsToBurst)
            ZIO.succeed((spend(cost, next).copy(bossCharges = orbs), hero,
              content.text("battle.elemental.orbCreated")))
          else chargeBurst(hero, spend(cost, next).copy(bossCharges = 0), atk, nowMs, MiniBoss.FireElemental, victim)
        }

      // 3) Огненный щит: чинит броню и HP, но только если есть что чинить.
      case 2 =>
        val cost     = MiniBoss.FireElemental.ShieldCostPerLvl * lvl
        val maxHp    = battle.monsterStats.hp
        val maxArmor = battle.monsterStats.armor
        val hurt     = battle.monsterCurrentHp < maxHp || battle.monsterCurrentArmor < maxArmor
        if (energy < cost || !hurt) ZIO.succeed((next, hero, ""))
        else {
          val newHp    = (battle.monsterCurrentHp + maxHp * MiniBoss.FireElemental.ShieldHpPct / 100L).min(maxHp)
          val newArmor = (battle.monsterCurrentArmor + maxArmor * MiniBoss.FireElemental.ShieldArmorPct / 100L).min(maxArmor)
          val healed   = newHp - battle.monsterCurrentHp
          val repaired = newArmor - battle.monsterCurrentArmor
          ZIO.succeed((
            spend(cost, next).copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor),
            hero,
            content.format("battle.elemental.shield",
              "hp" -> healed.toString, "armor" -> repaired.toString)))
        }

      // 4) Пропуск хода.
      case _ => ZIO.succeed((next, hero, ""))
    }
  }

  /** Залп накопленных зарядов: ×2 атаки моба плюс 5% от максимумов HP и брони
    * героя. Если урон дошёл до HP, с шансом 20% герой получает травму — ту же,
    * что при смерти (см. [[pangea.model.trauma.TraumaRoll]]). Считается одинаково
    * у смерча огненного и россыпи каменного; расходятся они лишь текстом и
    * последействием — камни вдобавок сбивают героя с ног. */
  private def chargeBurst(
      hero: Hero,
      battle: SoloPveBattle,
      atk: Long,
      nowMs: Long,
      kind: MiniBoss,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] = {
    val stone    = kind == MiniBoss.StoneElemental
    val statPct  = if (stone) MiniBoss.StoneElemental.BurstHeroStatPct else MiniBoss.FireElemental.BurstHeroStatPct
    val traumaAt = if (stone) MiniBoss.StoneElemental.BurstTraumaChancePct else MiniBoss.FireElemental.BurstTraumaChancePct
    val textKey  = if (stone) "battle.elemental.boulderBurst" else "battle.elemental.orbBurst"
    // Залп считается от запаса того, в кого летит: у союзника свои потолки.
    val (maxHp, maxArmor) = victim match {
      case Victim.Hero      => (hero.effectiveMaxHp(nowMs), hero.effectiveMaxArmor(nowMs))
      case Victim.Ally(pos) => battle.group.allyAt(pos).fold((0L, 0L))(a => (a.stats.hp, a.stats.armor))
    }
    val raw      = atk * 2L + (maxHp * statPct / 100L) + (maxArmor * statPct / 100L)
    val hpBefore = victimHp(victim, hero, battle)
    val (hit, struck, damage) = hurtVictim(victim, hero, battle, raw, nowMs)
    val toHp     = (hpBefore - victimHp(victim, hit, struck)).max(0L)
    // «Подгорный мастер» (порог 12): пока после залпа на герое осталась броня, с
    // ног его не сбить и травмы залп не оставляет.
    val steady   = victim == Victim.Hero && hit.sets.steadfast(hit.fightStats.armor)
    // Каменная россыпь сбивает с ног: защита и уклонение героя срезаны на 3 хода.
    // Союзника сбивать нечем — у него таких счётчиков нет, ему достаётся только удар.
    val after =
      if (!stone || victim != Victim.Hero || steady) struck
      else struck.copy(effects = struck.effects.copy(heroStunnedTurns = MiniBoss.StoneElemental.BurstDebuffTurns))
    val burstLine = content.format(textKey, "damage" -> damage.toString)
    val line =
      if (!stone || victim != Victim.Hero) burstLine
      else if (steady) burstLine + "\n" + content.text("battle.steadfast")
      else burstLine + "\n" + content.format("battle.elemental.boulderStun",
             "pct" -> MiniBoss.StoneElemental.BurstDebuffPct.toString,
             "turns" -> MiniBoss.StoneElemental.BurstDebuffTurns.toString)
    // Травма — геройская беда: союзник её не получает.
    if (toHp <= 0 || victim != Victim.Hero || hit.fightStats.hp <= 0 || steady) ZIO.succeed((after, hit, line))
    else
      for {
        roll   <- Random.nextIntBetween(1, 101)
        result <- if (roll > traumaAt) ZIO.succeed((after, hit, line))
                  else giveTrauma(hit, nowMs).map { case (h, traumaLine) => (after, h, line + "\n" + traumaLine) }
      } yield result
  }

  /** Сдвиг очереди способностей: круг у каждого вида своей длины. */
  private def nextTurn(battle: SoloPveBattle, kind: MiniBoss): SoloPveBattle =
    battle.copy(bossTurn = (battle.bossTurn + 1) % kind.abilities)

  /** Атака моба с учётом временного ослабления: подожжённый камень бьёт слабее,
    * поднявшийся из мёртвых Джо — тоже. Процент приходит вместе со сроком. */
  private def monsterAttack(battle: SoloPveBattle): Long =
    if (battle.effects.monsterWeakened)
      (battle.monsterStats.atk * (100L - battle.effects.monsterWeakenedPct).max(0L) / 100L).max(1L)
    else battle.monsterStats.atk

  /** Потолок брони моба с учётом того, сколько его срезали поджоги. Текущая броня
    * может остаться ВЫШЕ потолка — её поджог не трогает, но чинить выше уже нельзя. */
  private def monsterArmorCap(battle: SoloPveBattle): Long =
    (battle.monsterStats.armor - battle.effects.monsterMaxArmorCut).max(0L)

  /** Круг Гнилого Джо: смрад → широкий удар → гнилое восстановление → пропуск. */
  private def joeTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] = {
    val joe    = MiniBoss.RottenJoe
    val lvl    = battle.monsterLvl
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, joe)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Ядовитый смрад: травит героя. Повторный смрад стакает яд, как и у мобов.
      case 0 =>
        val cost = joe.StenchCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          ZIO.succeed((poisonVictim(victim, spend(cost, next), joe.StenchPoisonPct),
            hero, content.text("battle.joe.stench")))
        }

      // 2) Широкий удар: три четверти атаки, и с шансом 5% — травма, если дошло до HP.
      case 1 =>
        val cost = joe.SweepCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val raw      = (monsterAttack(battle) * joe.SweepDamageFactor).toLong.max(1L)
          val hpBefore = victimHp(victim, hero, battle)
          val (hurt, spent, dealt) = hurtVictim(victim, hero, spend(cost, next), raw, nowMs)
          val line     = content.format("battle.joe.sweep", "damage" -> dealt.toString)
          // Травму оставляет только удар по герою: союзник ран не копит. Гном в
          // «Подгорном мастере» (порог 12), пока на нём броня, её тоже не получает.
          if (victim != Victim.Hero || victimHp(victim, hurt, spent) >= hpBefore || hurt.fightStats.hp <= 0 ||
              hurt.sets.steadfast(hurt.fightStats.armor))
            ZIO.succeed((spent, hurt, line))
          else
            for {
              roll   <- Random.nextIntBetween(1, 101)
              result <- if (roll > joe.SweepTraumaChancePct) ZIO.succeed((spent, hurt, line))
                        else giveTrauma(hurt, nowMs).map { case (h, tl) => (spent, h, line + "\n" + tl) }
            } yield result
        }

      // 3) Гнилое восстановление: черви латают своё жилище.
      case 2 =>
        val cost  = joe.RegrowCostPerLvl * lvl
        val maxHp = battle.monsterStats.hp
        if (energy < cost || battle.monsterCurrentHp >= maxHp) ZIO.succeed((next, hero, ""))
        else {
          val newHp  = (battle.monsterCurrentHp + maxHp * joe.RegrowHpPct / 100L).min(maxHp)
          val healed = newHp - battle.monsterCurrentHp
          ZIO.succeed((spend(cost, next).copy(monsterCurrentHp = newHp), hero,
            content.format("battle.joe.regrow", "hp" -> healed.toString)))
        }

      // 4) Пропуск хода.
      case _ => ZIO.succeed((next, hero, ""))
    }
  }

  /** Круг Белого волка: пасть → когти → резкий прыжок → пропуск → смыкание
    * пасти. Первое бьющее умение боя наносит двойной урон, дальше двойной — с
    * шансом 5% (см. MiniBoss.WhiteWolf.CritChancePct). Бьёт он холодом, как и
    * обычной атакой: урон ложится через `bossHit` с гранями стихии. Пасть и
    * когти оставляют кровотечение, если удар дошёл до HP. Прыжок цель ищет сам
    * — дальше обычного удара (см. MiniBoss.WhiteWolf.LeapReach), и волк делает
    * его, даже когда с места никого не достаёт (MiniBoss.leapTurn). Не хватило
    * энергии — умение пропускается, а круг сдвигается, как у остальных боссов. */
  private def wolfTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] = {
    val wolf   = MiniBoss.WhiteWolf
    val lvl    = battle.monsterLvl
    val atk    = monsterAttack(battle)
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, wolf)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    /** Множитель урона умения: первое бьющее — всегда вдвое (и запоминается),
      * дальше бросок на крит. Возвращает множитель, был ли крит и бой. */
    def critMult(b: SoloPveBattle): Task[(Long, Boolean, SoloPveBattle)] =
      if (!b.bossFirstSkillSpent) ZIO.succeed((wolf.CritMult, true, b.copy(bossFirstSkillSpent = true)))
      else Random.nextIntBetween(1, 101).map(roll =>
        if (roll <= wolf.CritChancePct) (wolf.CritMult, true, b) else (1L, false, b))

    /** Удар умением по `prey`: крит, урон холодом по броне и HP, кровотечение
      * при уроне по HP. `line` получает урон, который дошёл. */
    def bite(b: SoloPveBattle, prey: Victim, raw: Long, bleedPct: Int, line: Long => String): Task[(SoloPveBattle, Hero, String)] =
      critMult(b).map { case (mult, critted, b1) =>
        val hpBefore = victimHp(prey, hero, b1)
        val (hurt, bitten, damage) = prey match {
          case Victim.Hero =>
            // По герою волк бьёт холодом: грани стихии считает `bossHit`.
            val dealt                     = bossDamageTaken(hero, battle, (raw * mult).max(1L))
            val (newHp, newArmor, landed) = bossHit(battle, hero, dealt, nowMs)
            (hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor)), b1, landed)
          case ally => hurtVictim(ally, hero, b1, (raw * mult).max(1L), nowMs)
        }
        val bleeds = bleedPct > 0 && victimHp(prey, hurt, bitten) < hpBefore
        val b2     = if (!bleeds) bitten else bleedVictim(prey, bitten, bleedPct)
        val text = List(
          Some(line(damage)),
          Option.when(critted)(content.text("battle.wolf.crit")),
          Option.when(bleeds)(content.format("battle.wolf.bleeds", "pct" -> bleedPct.toString))
        ).flatten.mkString("\n")
        (b2, hurt, text)
      }

    /** Волк встаёт напротив цели на месте `at`; занято другим мобом — на
      * ближайшее к ней свободное место по пути от него. */
    def leapTo(b: SoloPveBattle, at: Int): SoloPveBattle = {
      val g    = b.group
      val from = g.activePos
      if (at == from) b
      else {
        val back = if (at > from) -1 else 1
        Iterator.iterate(at)(_ + back).takeWhile(_ != from)
          .find(p => p >= 1 && p <= Formation.MonsterPlaces && !g.hasMonster(p))
          .fold(b)(p => b.copy(group = g.copy(activePos = p)))
      }
    }

    /** Никого в досягаемости прыжка — волк прыгает к ближайшему врагу (герою на
      * ногах или союзнику) на `LeapReach` мест, без удара. */
    def leapCloser(b: SoloPveBattle): SoloPveBattle = {
      val g     = b.group
      val from  = g.activePos
      val goals = (if (g.heroDown) Nil else List(g.heroPos)) ++ g.allies.filter(_.alive).map(_.position)
      goals.sortBy(p => (math.abs(p - from), p)).headOption.fold(b) { goal =>
        @annotation.tailrec
        def walk(at: Int, left: Int): Int =
          if (left <= 0 || at == goal) at
          else b.copy(group = g.copy(activePos = at)).stepToward(at, goal) match {
            case None       => at
            case Some(step) => walk(step, left - 1)
          }
        b.copy(group = g.copy(activePos = walk(from, wolf.LeapReach)))
      }
    }

    /** Строка прыжка: по кому пришёлся и сколько, и что уклонение выросло. */
    def leapLine(prey: Victim, b: SoloPveBattle, damage: Long): String = {
      val dodge = List("pct" -> wolf.LeapDodgePct.toString, "turns" -> wolf.LeapDodgeTurns.toString)
      prey match {
        case Victim.Ally(pos) =>
          content.format("battle.wolf.leapAlly",
            (("name" -> b.group.allyAt(pos).fold("")(_.name)) :: ("damage" -> damage.toString) :: dodge): _*)
        case Victim.Hero =>
          content.format("battle.wolf.leap", (("damage" -> damage.toString) :: dodge): _*)
      }
    }

    battle.bossTurn match {
      // 1) Яростная пасть: половина атаки, кровь при уроне по HP.
      case 0 =>
        val cost = wolf.FangsCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else bite(spend(cost, next), victim, (atk * wolf.FangsDamageFactor).toLong.max(1L), wolf.FangsBleedPct,
               d => content.format("battle.wolf.fangs", "damage" -> d.toString))

      // 2) Яростные когти: то же, но слабее.
      case 1 =>
        val cost = wolf.ClawsCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else bite(spend(cost, next), victim, (atk * wolf.ClawsDamageFactor).toLong.max(1L), wolf.ClawsBleedPct,
               d => content.format("battle.wolf.claws", "damage" -> d.toString))

      // 3) Резкий прыжок: к цели не дальше трёх мест — встать напротив неё и
      //    укусить на четверть атаки; никого так близко нет — прыжок на три
      //    места к ближайшему врагу, без удара. После прыжка уклонение волка
      //    выше на 4 раунда. Пригвождённый не прыгает: кусает с места того, кого
      //    достаёт обычным ударом, а не достаёт никого — шаг пропадает.
      case 2 =>
        val cost   = wolf.LeapCostPerLvl * lvl
        val pinned = battle.effects.monsterRooted
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val dodgy = spend(cost, next).copy(effects = next.effects.copy(mobInstinctTurns = wolf.LeapDodgeTurns))
          reachTarget(battle, if (pinned) GroupState.Reach else wolf.LeapReach) match {
            case Some((prey, at)) =>
              val landed = if (pinned) dodgy else leapTo(dodgy, at)
              bite(landed, prey, (atk * wolf.LeapDamageFactor).toLong.max(1L), 0, d => leapLine(prey, landed, d))
            case None if pinned => ZIO.succeed((next, hero, ""))
            case None =>
              ZIO.succeed((leapCloser(dodgy), hero, content.format("battle.wolf.leapCloser",
                "pct" -> wolf.LeapDodgePct.toString, "turns" -> wolf.LeapDodgeTurns.toString)))
          }
        }

      // 4) Пропуск хода.
      case 3 => ZIO.succeed((next, hero, ""))

      // 5) Смыкание пасти: четверть недостающего герою HP плюс пятая часть атаки. Бесплатно.
      case _ =>
        val missing = victimMissingHp(victim, hero, battle, nowMs)
        val raw     = missing * wolf.JawsMissingHpPct / 100L + (atk * wolf.JawsDamageFactor).toLong
        bite(next, victim, raw.max(1L), 0, d => content.format("battle.wolf.jaws", "damage" -> d.toString))
    }
  }

  /** Круг каменного: всплеск → валун → восстановление → вязкая земля → пропуск. */
  private def stoneTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] = {
    val lvl    = battle.monsterLvl
    val atk    = monsterAttack(battle)
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, MiniBoss.StoneElemental)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Каменный всплеск: осколок в упор, половина атаки.
      case 0 =>
        val cost = MiniBoss.StoneElemental.SplashCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val raw        = (atk * MiniBoss.StoneElemental.SplashDamageFactor).toLong.max(1L)
          val (h, b1, l) = hurtVictim(victim, hero, spend(cost, next), raw, nowMs)
          ZIO.succeed((b1, h,
            content.format("battle.elemental.stoneSplash", "damage" -> l.toString)))
        }

      // 2) Каменный валун: копится до трёх, третий сразу уходит в россыпь.
      case 1 =>
        val cost = MiniBoss.StoneElemental.BoulderCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val boulders = battle.bossCharges + 1
          if (boulders < MiniBoss.StoneElemental.BouldersToBurst)
            ZIO.succeed((spend(cost, next).copy(bossCharges = boulders), hero,
              content.text("battle.elemental.boulderCreated")))
          else chargeBurst(hero, spend(cost, next).copy(bossCharges = 0), atk, nowMs, MiniBoss.StoneElemental, victim)
        }

      // 3) Восстановление камня: чинит броню и HP, но только если есть что чинить.
      case 2 =>
        val cost     = MiniBoss.StoneElemental.RestoreCostPerLvl * lvl
        val maxHp    = battle.monsterStats.hp
        val cap      = monsterArmorCap(battle)
        val hurt     = battle.monsterCurrentHp < maxHp || battle.monsterCurrentArmor < cap
        if (energy < cost || !hurt) ZIO.succeed((next, hero, ""))
        else {
          val newHp    = (battle.monsterCurrentHp + maxHp * MiniBoss.StoneElemental.RestoreHpPct / 100L).min(maxHp)
          val newArmor = (battle.monsterCurrentArmor + battle.monsterStats.armor * MiniBoss.StoneElemental.RestoreArmorPct / 100L)
                           .min(cap.max(battle.monsterCurrentArmor))
          val healed   = newHp - battle.monsterCurrentHp
          val repaired = newArmor - battle.monsterCurrentArmor
          ZIO.succeed((
            spend(cost, next).copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor),
            hero,
            content.format("battle.elemental.stoneRestore",
              "hp" -> healed.toString, "armor" -> repaired.toString)))
        }

      // 4) Вязкая земля: режет герою точность и уклонение.
      case 3 =>
        val cost = MiniBoss.StoneElemental.GroundCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else
          ZIO.succeed((
            spend(cost, next).copy(effects = next.effects.copy(heroGroundedTurns = MiniBoss.StoneElemental.GroundTurns)),
            hero,
            content.format("battle.elemental.stickyGround",
              "pct" -> MiniBoss.StoneElemental.GroundCutPct.toString,
              "turns" -> MiniBoss.StoneElemental.GroundTurns.toString)))

      // 5) Пропуск хода.
      case _ => ZIO.succeed((next, hero, ""))
    }
  }

  /** Круг Громового: разряд → грозовой заряд → цепная молния → ионный покров →
    * пропуск. Бьёт он молнией (см. MiniBoss.attackElement): по герою — гранями
    * стихии через [[bossHit]], по союзнику — по его защите теми же гранями (см.
    * [[zapVictim]]). Цель — та, что пришла: напротив, а нет — герой или
    * ближайший союзник в трёх местах (он дальник, см. RaceTactics.reachOf). Не
    * хватило энергии — шаг пропускается, а круг сдвигается. */
  private def thunderTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] = {
    val thunder = MiniBoss.ThunderElemental
    val lvl     = battle.monsterLvl
    val atk     = monsterAttack(battle)
    val energy  = battle.monsterCurrentEnergy
    val next    = nextTurn(battle, thunder)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Разряд: половина атаки и выжженная энергия цели.
      case 0 =>
        val cost = thunder.BoltCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val (h1, b1, dealt) = zapVictim(victim, hero, spend(cost, next),
                                  (atk * thunder.BoltDamageFactor).toLong.max(1L), nowMs)
          val (h2, b2, burnt) = burnVictimEnergy(victim, h1, b1, thunder.BoltEnergyBurnPct, nowMs)
          val head = victim match {
            case Victim.Hero      => content.format("battle.thunder.bolt", "damage" -> dealt.toString)
            case Victim.Ally(pos) => content.format("battle.thunder.boltAlly",
                                       "name" -> b2.group.allyAt(pos).fold("")(_.name), "damage" -> dealt.toString)
          }
          val burnLine = Option.when(burnt > 0L)(victim match {
            case Victim.Hero      => content.format("battle.mobLightningBurn", "energy" -> burnt.toString)
            case Victim.Ally(pos) => content.format("battle.ally.lightningBurn",
                                       "name" -> b2.group.allyAt(pos).fold("")(_.name), "energy" -> burnt.toString)
          })
          ZIO.succeed((b2, h2, (head +: burnLine.toVector).mkString("\n")))
        }

      // 2) Грозовой заряд: копится до трёх, третий сразу срывается в раскат.
      case 1 =>
        val cost = thunder.ChargeCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val charges = battle.bossCharges + 1
          if (charges < thunder.ChargesToBurst)
            ZIO.succeed((spend(cost, next).copy(bossCharges = charges), hero,
              content.format("battle.thunder.charge", "n" -> charges.toString, "of" -> thunder.ChargesToBurst.toString)))
          else thunderClap(hero, spend(cost, next).copy(bossCharges = 0), atk, nowMs, victim)
        }

      // 3) Цепная молния: по цели и по тем, кто стоит с ней рядом.
      case 2 =>
        val cost = thunder.ChainCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else ZIO.succeed(chainLightning(hero, spend(cost, next), atk, nowMs, victim))

      // 4) Ионный покров: чинит броню и HP, но только если есть что чинить.
      case 3 =>
        val cost     = thunder.CoverCostPerLvl * lvl
        val maxHp    = battle.monsterStats.hp
        val maxArmor = battle.monsterStats.armor
        val hurt     = battle.monsterCurrentHp < maxHp || battle.monsterCurrentArmor < maxArmor
        if (energy < cost || !hurt) ZIO.succeed((next, hero, ""))
        else {
          val newHp    = (battle.monsterCurrentHp + maxHp * thunder.CoverHpPct / 100L).min(maxHp)
          val newArmor = (battle.monsterCurrentArmor + maxArmor * thunder.CoverArmorPct / 100L).min(maxArmor)
          ZIO.succeed((
            spend(cost, next).copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor),
            hero,
            content.format("battle.thunder.cover",
              "hp" -> (newHp - battle.monsterCurrentHp).toString,
              "armor" -> (newArmor - battle.monsterCurrentArmor).toString)))
        }

      // 5) Пропуск хода.
      case _ => ZIO.succeed((next, hero, ""))
    }
  }

  /** Удар Громового по цели — молнией. Герою — гранями стихии через
    * [[bossHit]], и поднятый щит режет его, как всякий приём минибосса (см.
    * [[hurtVictim]]); союзнику — по его защите и теми же гранями (см.
    * [[allyHitSplit]]). Возвращает героя, бой и сколько урона прошло. */
  private def zapVictim(v: Victim, hero: Hero, battle: SoloPveBattle, damage: Long, nowMs: Long): (Hero, SoloPveBattle, Long) =
    v match {
      case Victim.Hero =>
        val dealt                     = bossDamageTaken(hero, battle, BattleState.shieldCut(battle.heroBlockPct, damage))
        val (newHp, newArmor, landed) = bossHit(battle, hero, dealt, nowMs)
        (hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor)), battle, landed)
      case Victim.Ally(pos) =>
        battle.group.allyAt(pos).filter(_.alive) match {
          case None => (hero, battle, 0L)
          case Some(a) =>
            val reduced           = (damage * (1.0 - allyReduction(a, battle.monsterStats.atk))).toLong.max(1L)
            val (armorDmg, hpDmg) = allyHitSplit(battle, a, reduced)
            val hurt = a.copy(armor = a.armor - armorDmg, hp = (a.hp - hpDmg).max(0L),
                         lastHitBy = Some(battle.monsterName))
            (hero, battle.copy(group = battle.group.updateAlly(pos)(_ => hurt)), armorDmg + hpDmg)
        }
    }

  /** Выжечь цели `pct`% потолка её энергии (разряд Громового): герою — из его
    * запаса, союзнику — из своего. Возвращает героя, бой и сколько сгорело. */
  private def burnVictimEnergy(v: Victim, hero: Hero, battle: SoloPveBattle, pct: Long, nowMs: Long): (Hero, SoloPveBattle, Long) =
    v match {
      case Victim.Hero =>
        val burnt = BattleState.energyBurnOf(hero.maxEnergy(nowMs), hero.fightStats.energy, pct)
        (hero.copy(fightStats = hero.fightStats.copy(energy = hero.fightStats.energy - burnt)), battle, burnt)
      case Victim.Ally(pos) =>
        battle.group.allyAt(pos).filter(_.alive) match {
          case None => (hero, battle, 0L)
          case Some(a) =>
            val burnt = BattleState.energyBurnOf(a.stats.energy, a.energy, pct)
            (hero, battle.copy(group = battle.group.updateAlly(pos)(_.copy(energy = a.energy - burnt))), burnt)
        }
    }

  /** Громовой раскат: накопленные заряды разом — две атаки плюс 5% максимумов HP
    * и брони цели, молнией. Дошёл до HP героя — с шансом 20% травма, как от
    * смерча огненного («Подгорный мастер» с бронёй её не получает). И
    * оглушение: следующее умение цели срывается — у героя и у союзника (тот же
    * срыв, что от яда красного сетеплёта, см. RedSpider). */
  private def thunderClap(
      hero: Hero,
      battle: SoloPveBattle,
      atk: Long,
      nowMs: Long,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] = {
    val thunder = MiniBoss.ThunderElemental
    // Раскат считается от запаса того, в кого летит: у союзника свои потолки.
    val (maxHp, maxArmor) = victim match {
      case Victim.Hero      => (hero.effectiveMaxHp(nowMs), hero.effectiveMaxArmor(nowMs))
      case Victim.Ally(pos) => battle.group.allyAt(pos).fold((0L, 0L))(a => (a.stats.hp, a.stats.armor))
    }
    val raw      = atk * 2L + (maxHp * thunder.BurstHeroStatPct / 100L) + (maxArmor * thunder.BurstHeroStatPct / 100L)
    val hpBefore = victimHp(victim, hero, battle)
    val (hit, struck, damage) = zapVictim(victim, hero, battle, raw, nowMs)
    val toHp     = (hpBefore - victimHp(victim, hit, struck)).max(0L)
    val (stunned, lines) = victim match {
      case Victim.Hero =>
        (struck.copy(effects = struck.effects.withSkillJam(SkillJam.Thunder)),
          Vector(content.format("battle.thunder.clap", "damage" -> damage.toString),
                 content.text("battle.thunder.stunned")))
      case Victim.Ally(pos) =>
        val name = struck.group.allyAt(pos).fold("")(_.name)
        (struck.copy(group = struck.group.updateAlly(pos)(_.copy(skillJammed = true, skillJam = Some(SkillJam.Thunder)))),
          Vector(content.format("battle.thunder.clapAlly", "name" -> name, "damage" -> damage.toString),
                 content.format("battle.thunder.stunnedAlly", "name" -> name)))
    }
    val line   = lines.mkString("\n")
    // Травма — геройская беда: союзник её не получает.
    val steady = victim == Victim.Hero && hit.sets.steadfast(hit.fightStats.armor)
    if (toHp <= 0 || victim != Victim.Hero || hit.fightStats.hp <= 0 || steady) ZIO.succeed((stunned, hit, line))
    else
      for {
        roll   <- Random.nextIntBetween(1, 101)
        result <- if (roll > thunder.BurstTraumaChancePct) ZIO.succeed((stunned, hit, line))
                  else giveTrauma(hit, nowMs).map { case (h, traumaLine) => (stunned, h, line + "\n" + traumaLine) }
      } yield result
  }

  /** Цепная молния: по цели — 0,4 атаки, и по 0,2 — каждому, кто стоит с ней
    * рядом в её ряду (на соседних местах — герой или союзники). Лежачего героя
    * молния не трогает. */
  private def chainLightning(
      hero: Hero,
      battle: SoloPveBattle,
      atk: Long,
      nowMs: Long,
      victim: Victim
  ): (SoloPveBattle, Hero, String) = {
    val thunder = MiniBoss.ThunderElemental
    val g       = battle.group
    val at      = victim match {
      case Victim.Hero      => g.heroPos
      case Victim.Ally(pos) => pos
    }
    val neighbours = List(at - 1, at + 1).flatMap { p =>
      if (p == g.heroPos && !g.heroDown) Some(Victim.Hero: Victim)
      else g.allyAt(p).filter(_.alive).map(a => Victim.Ally(a.position): Victim)
    }
    val (h1, b1, dealt) = zapVictim(victim, hero, battle, (atk * thunder.ChainDamageFactor).toLong.max(1L), nowMs)
    val head = victim match {
      case Victim.Hero      => content.format("battle.thunder.chain", "damage" -> dealt.toString)
      case Victim.Ally(pos) => content.format("battle.thunder.chainAlly",
                                 "name" -> b1.group.allyAt(pos).fold("")(_.name), "damage" -> dealt.toString)
    }
    val (bOut, hOut, lines) = neighbours.foldLeft((b1, h1, Vector(head))) { case ((b, h, acc), n) =>
      val (h2, b2, d) = zapVictim(n, h, b, (atk * thunder.ChainNeighbourFactor).toLong.max(1L), nowMs)
      val line = n match {
        case Victim.Hero      => content.format("battle.thunder.chainJumpYou", "damage" -> d.toString)
        case Victim.Ally(pos) => content.format("battle.thunder.chainJump",
                                   "name" -> b2.group.allyAt(pos).fold("")(_.name), "damage" -> d.toString)
      }
      (b2, h2, acc :+ line)
    }
    (bOut, hOut, lines.mkString("\n"))
  }

  /** Круг Ледяного: ледяной шип → ледяная глыба → метель → ледяной панцирь →
    * пропуск. Бьёт он холодом (см. MiniBoss.attackElement): по герою — гранями
    * стихии через [[bossHit]] (по броне на 10% сильнее), по союзнику — по его
    * защите теми же гранями (см. [[zapVictim]]). Цель — та, что пришла:
    * напротив, а нет — герой или ближайший союзник в трёх местах (он дальник).
    * Не хватило энергии — шаг пропускается, а круг сдвигается. */
  private def iceTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] = {
    val ice    = MiniBoss.IceElemental
    val lvl    = battle.monsterLvl
    val atk    = monsterAttack(battle)
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, ice)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Ледяной шип: половина атаки — и защита цели наверняка ниже.
      case 0 =>
        val cost = ice.SpikeCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val (h1, b1, dealt) = zapVictim(victim, hero, spend(cost, next),
                                  (atk * ice.SpikeDamageFactor).toLong.max(1L), nowMs)
          val head = victim match {
            case Victim.Hero      => content.format("battle.ice.spike", "damage" -> dealt.toString)
            case Victim.Ally(pos) => content.format("battle.ice.spikeAlly",
                                       "name" -> b1.group.allyAt(pos).fold("")(_.name), "damage" -> dealt.toString)
          }
          val (b2, biteLine) = coldBite(victim, b1)
          ZIO.succeed((b2, h1, (head +: biteLine.toVector).mkString("\n")))
        }

      // 2) Ледяная глыба: копится до трёх, третья обрушивается ледопадом.
      case 1 =>
        val cost = ice.BlockCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val blocks = battle.bossCharges + 1
          if (blocks < ice.BlocksToFall)
            ZIO.succeed((spend(cost, next).copy(bossCharges = blocks), hero,
              content.format("battle.ice.block", "n" -> blocks.toString, "of" -> ice.BlocksToFall.toString)))
          else iceFall(hero, spend(cost, next).copy(bossCharges = 0), atk, nowMs, victim)
        }

      // 3) Метель: всех, кого он достаёт, — и задетых отбросить от себя.
      case 2 =>
        val cost = ice.BlizzardCostPerLvl * lvl
        if (energy < cost || blizzardTargets(battle).isEmpty) ZIO.succeed((next, hero, ""))
        else ZIO.succeed(blizzard(hero, spend(cost, next), atk, nowMs))

      // 4) Ледяной панцирь: броня обратно — не выше потолка, что оставил огонь, —
      //    и HP, как у прочих элементалей; только если есть что чинить. Броню
      //    выше потолка (потолок упал, пока она была цела) он не срезает.
      case 3 =>
        val cost  = ice.ShellCostPerLvl * lvl
        val maxHp = battle.monsterStats.hp
        val cap   = monsterArmorCap(battle)
        val hurt  = battle.monsterCurrentHp < maxHp || battle.monsterCurrentArmor < cap
        if (energy < cost || !hurt) ZIO.succeed((next, hero, ""))
        else {
          val newHp    = (battle.monsterCurrentHp + maxHp * ice.ShellHpPct / 100L).min(maxHp)
          val newArmor = (battle.monsterCurrentArmor + battle.monsterStats.armor * ice.ShellArmorPct / 100L)
                           .min(cap.max(battle.monsterCurrentArmor))
          ZIO.succeed((spend(cost, next).copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor), hero,
            content.format("battle.ice.shell",
              "hp" -> (newHp - battle.monsterCurrentHp).toString,
              "armor" -> (newArmor - battle.monsterCurrentArmor).toString)))
        }

      // 5) Пропуск хода.
      case _ => ZIO.succeed((next, hero, ""))
    }
  }

  /** Холод ледяного шипа — наверняка, как прок холода в ударе моба: защита цели
    * ниже на [[Element.Cold.DefenceReductionCut]] п.п. и копится. Горящему холод
    * гасит пламя и сам не ложится; героя от него бережёт защитный отвар. */
  private def coldBite(v: Victim, b: SoloPveBattle): (SoloPveBattle, Option[String]) =
    v match {
      case Victim.Hero =>
        if (b.effects.heroBurn.isDefined)
          (b.copy(effects = b.effects.copy(heroBurn = None)), Some(content.text("battle.elements.quenchHero")))
        else if (b.effects.warded(Ward.Cold)) (b, Some(content.text("battle.ward.cold")))
        else
          (b.copy(effects = b.effects.copy(
             heroColdDefenceCut = b.effects.heroColdDefenceCut + Element.Cold.DefenceReductionCut)),
           Some(content.format("battle.mobColdBite", "cut" -> Element.Cold.DefenceReductionCut.toString)))
      case Victim.Ally(pos) =>
        b.group.allyAt(pos).filter(_.alive) match {
          case None => (b, None)
          case Some(a) if a.burn.isDefined =>
            (b.copy(group = b.group.updateAlly(pos)(_.copy(burn = None))),
             Some(content.format("battle.ally.quench", "name" -> a.name)))
          case Some(a) =>
            (b.copy(group = b.group.updateAlly(pos)(x => x.copy(coldCut = x.coldCut + Element.Cold.DefenceReductionCut))),
             Some(content.format("battle.ally.coldBite", "name" -> a.name,
               "cut" -> Element.Cold.DefenceReductionCut.toString)))
        }
    }

  /** Ледопад: накопленные глыбы разом — две атаки плюс 5% максимумов HP и брони
    * цели, холодом. Дошёл до HP героя — с шансом 20% травма, как от смерча
    * огненного. И цель вмерзает: не сходит с места остаток этого раунда и два
    * следующих — ни шагом, ни Тараном, ни прыжком. «Подгорный мастер», пока на
    * нём броня, не вмерзает и травмы не получает: с ног его не сбить. */
  private def iceFall(
      hero: Hero,
      battle: SoloPveBattle,
      atk: Long,
      nowMs: Long,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] = {
    val ice = MiniBoss.IceElemental
    // Ледопад считается от запаса того, на кого рушится: у союзника свои потолки.
    val (maxHp, maxArmor) = victim match {
      case Victim.Hero      => (hero.effectiveMaxHp(nowMs), hero.effectiveMaxArmor(nowMs))
      case Victim.Ally(pos) => battle.group.allyAt(pos).fold((0L, 0L))(a => (a.stats.hp, a.stats.armor))
    }
    val raw      = atk * 2L + (maxHp * ice.FallHeroStatPct / 100L) + (maxArmor * ice.FallHeroStatPct / 100L)
    val hpBefore = victimHp(victim, hero, battle)
    val (hit, struck, damage) = zapVictim(victim, hero, battle, raw, nowMs)
    val toHp     = (hpBefore - victimHp(victim, hit, struck)).max(0L)
    val steady   = victim == Victim.Hero && hit.sets.steadfast(hit.fightStats.armor)
    // Лёг посреди раунда: держит остаток этого и ещё FrozenTurns раундов.
    val holds    = ice.FrozenTurns + 1
    val (frozen, lines) = victim match {
      case Victim.Hero =>
        val head = content.format("battle.ice.fall", "damage" -> damage.toString)
        if (steady) (struck, Vector(head, content.text("battle.steadfast")))
        else
          (struck.copy(effects = struck.effects.copy(
             heroFrozenTurns = Some(struck.effects.heroFrozenTurns.getOrElse(0).max(holds)))),
            Vector(head, content.format("battle.ice.frozen", "turns" -> ice.FrozenTurns.toString)))
      case Victim.Ally(pos) =>
        val name = struck.group.allyAt(pos).fold("")(_.name)
        (struck.copy(group = struck.group.updateAlly(pos)(a => a.copy(frozen = a.frozen.max(holds)))),
          Vector(content.format("battle.ice.fallAlly", "name" -> name, "damage" -> damage.toString),
                 content.format("battle.ice.frozenAlly", "name" -> name, "turns" -> ice.FrozenTurns.toString)))
    }
    val line = lines.mkString("\n")
    // Травма — геройская беда: союзник её не получает.
    if (toHp <= 0 || victim != Victim.Hero || hit.fightStats.hp <= 0 || steady) ZIO.succeed((frozen, hit, line))
    else
      for {
        roll   <- Random.nextIntBetween(1, 101)
        result <- if (roll > ice.FallTraumaChancePct) ZIO.succeed((frozen, hit, line))
                  else giveTrauma(hit, nowMs).map { case (h, traumaLine) => (frozen, h, line + "\n" + traumaLine) }
      } yield result
  }

  /** Кого накрывает метель Ледяного: героя на ногах и живых союзников не дальше
    * его досягаемости (трёх мест) от того места, где он стоит. */
  private def blizzardTargets(b: SoloPveBattle): List[Victim] = {
    val g     = b.group
    val reach = MiniBoss.IceElemental.Reach
    val hero  = Option.when(!g.heroDown && math.abs(g.heroPos - g.activePos) <= reach)(Victim.Hero: Victim)
    hero.toList ++ g.allies.filter(a => a.alive && math.abs(a.position - g.activePos) <= reach)
      .sortBy(_.position).map(a => Victim.Ally(a.position): Victim)
  }

  /** Метель: по четверти атаки холодом каждому, кого Ледяной достаёт (см.
    * [[blizzardTargets]]), — и всех задетых отбрасывает от него на место. Не
    * сдвигается тот, кто вмёрз или опутан, «Подгорный мастер» с бронёй (с ног
    * не сбить), и кому некуда: занятое место держит, за край строя не
    * отбросить. Отбрасывает сперва дальних — ближним есть куда встать. Стоящего
    * прямо напротив — к началу строя, а там занято — к концу. */
  private def blizzard(hero: Hero, battle: SoloPveBattle, atk: Long, nowMs: Long): (SoloPveBattle, Hero, String) = {
    val ice     = MiniBoss.IceElemental
    val at      = battle.group.activePos
    val targets = blizzardTargets(battle)
    val raw     = (atk * ice.BlizzardDamageFactor).toLong.max(1L)
    // 1) удар — каждому
    val (struckB, struckH, hitLines) = targets.foldLeft((battle, hero, Vector.empty[String])) { case ((b, h, acc), v) =>
      val (h2, b2, d) = zapVictim(v, h, b, raw, nowMs)
      val line = v match {
        case Victim.Hero      => content.format("battle.ice.blizzardYou", "damage" -> d.toString)
        case Victim.Ally(pos) => content.format("battle.ice.blizzardAlly",
                                   "name" -> b2.group.allyAt(pos).fold("")(_.name), "damage" -> d.toString)
      }
      (b2, h2, acc :+ line)
    }
    // 2) отбросить — от дальних к ближним
    def placeOf(v: Victim, b: SoloPveBattle): Int = v match {
      case Victim.Hero      => b.group.heroPos
      case Victim.Ally(pos) => pos
    }
    def taken(b: SoloPveBattle, p: Int): Boolean = p == b.group.heroPos || b.group.allyAt(p).isDefined
    def landing(b: SoloPveBattle, from: Int): Option[Int] = {
      val ways = if (from < at) List(from - ice.BlizzardPush) else if (from > at) List(from + ice.BlizzardPush)
                 else List(from - ice.BlizzardPush, from + ice.BlizzardPush)
      ways.find(p => p >= 1 && p <= Formation.WalkPlaces && !taken(b, p))
    }
    val order = targets.sortBy(v => -math.abs(placeOf(v, battle) - at))
    val (pushedB, pushLines) = order.foldLeft((struckB, Vector.empty[String])) { case ((b, acc), v) =>
      v match {
        case Victim.Hero =>
          val standing = struckH.fightStats.hp > 0
          if (!standing || b.effects.heroRooted) (b, acc)
          else if (struckH.sets.steadfast(struckH.fightStats.armor)) (b, acc :+ content.text("battle.steadfast"))
          else landing(b, b.group.heroPos).fold((b, acc)) { p =>
            val moved = if (b.group.hasMonster(p)) b.moveHeroTo(p) else b.copy(group = b.group.copy(heroPos = p))
            (moved, acc :+ content.format("battle.ice.pushedYou", "n" -> p.toString))
          }
        case Victim.Ally(pos) =>
          b.group.allyAt(pos).filter(a => a.alive && a.rooted <= 0 && a.webbed <= 0 && a.frozen <= 0) match {
            case None => (b, acc)
            case Some(a) =>
              landing(b, pos).fold((b, acc)) { p =>
                (b.copy(group = b.group.updateAlly(pos)(_.copy(position = p))),
                  acc :+ content.format("battle.ice.pushedAlly", "name" -> a.name, "n" -> p.toString))
              }
          }
      }
    }
    (pushedB, struckH, ((content.text("battle.ice.blizzard") +: hitLines) ++ pushLines).mkString("\n"))
  }

  /** Травма от смерча — та же механика, что и при смерти: прогресс по тирам
    * (лёгкие → средние → тяжёлые), уже полученные исключаются. Пишется сразу,
    * потому что `commit` сохраняет только экипировку и боевые статы. */
  private def giveTrauma(hero: Hero, nowMs: Long, key: String = "battle.elemental.orbTrauma",
                         maxKey: String = "battle.elemental.orbTraumaMax"): Task[(Hero, String)] = {
    val existing = if (hero.traumaActive(nowMs)) hero.traumaNames else Nil
    val pool     = TraumaRoll.pool(existing)
    val until    = hero.traumaUntilWithNew(nowMs)
    if (pool.isEmpty) ZIO.succeed((hero, content.text(maxKey)))
    else
      for {
        idx   <- Random.nextIntBetween(0, pool.length)
        trauma = pool(idx)
        names  = existing :+ trauma.name
        _     <- heroDao.updateTrauma(hero.userId, Some(until), names)
      } yield (
        hero.copy(traumaUntil = Some(until), traumaNames = names),
        content.format(key, "traumaName" -> trauma.name)
      )
  }

  /** Удар моба по HP может оставить травму. Мелкие удары (до 5% потолка HP) не в
    * счёт; ощутимый — 1%; сокрушительный (больше половины потолка) — 50%.
    * Считается по каждому удару и приёму отдельно; яд, кровь и огонь — не удары.
    * Мёртвому травму не добавляем (её даст смерть), и не дублируем ту, что приём
    * выдал сам (смерч, широкий удар Джо). Бросок не тратится, если удар был
    * мелким или ушёл в броню. */
  private def hitTrauma(before: Hero, after: Hero, nowMs: Long): Task[(Hero, Option[String])] = {
    val lost = before.fightStats.hp - after.fightStats.hp
    // Потолок — эффективный максимум, но не ниже того, что было: HP выше потолка
    // (сошёл баф, сменилась вещь) не должно делать каждый удар сокрушительным.
    val ceiling = after.effectiveMaxHp(nowMs).max(before.fightStats.hp)
    // «Подгорный мастер» (порог 12): пока на герое после удара есть броня, удар
    // травмы не оставляет — бросок не тратится.
    if (lost * 100L <= ceiling * BattleState.HitTraumaMinPct || after.fightStats.hp <= 0L ||
        after.traumaNames != before.traumaNames || after.sets.steadfast(after.fightStats.armor))
      ZIO.succeed((after, None))
    else {
      val crushing = lost * 100L > ceiling * BattleState.HitTraumaCrushPct
      val chance   = if (crushing) BattleState.HitTraumaCrushChancePct else BattleState.HitTraumaChancePct
      val key      = if (crushing) "battle.hitTraumaCrush" else "battle.hitTrauma"
      chanceRoll(active = true, chance).flatMap { hit =>
        if (hit) giveTrauma(after, nowMs, key).map { case (h, l) => (h, Some(l)) }
        else ZIO.succeed((after, None))
      }
    }
  }

  /** ЕДИНАЯ точка активного исцеления героя — всё, чем он лечит себя сам:
    * любая фляга, зелье из пояса, активное умение. Механика висит на самом
    * действии, а не на предмете, поэтому новая фляга или новое умение попадают
    * под неё сами, без правок здесь.
    *
    * Пока герой горит, исцеление слабее на 50% + текущий процент горения: при
    * 50% горения от него не остаётся ничего. Само горение при этом НЕ гаснет —
    * герой продолжает гореть на свои проценты (в отличие от мобов, которых
    * лечение из горения выводит). Тикающая регенерация сюда не входит: она не
    * действие игрока, а эффект, который идёт своим чередом.
    *
    * Возвращает новое HP, сколько реально вылечено и строку-пояснение, если
    * пламя часть лечения съело (без неё слабое зелье читалось бы как поломка). */
  private def activeHeal(
      hero: Hero,
      battle0: SoloPveBattle,
      raw: Long,
      nowMs: Long
  ): (Long, Long, SoloPveBattle, Option[String]) = {
    // Кровотечение лечение снимает целиком — то же правило, что и у моба
    // (см. Bleed): рана затянута. Яд оно ослабляет, а пламя тратит собой:
    // горение до 50% сгорает целиком, сверх того остаётся гореть излишек
    // (см. HealedWounds). Из-под миазм тьмы лечение не выводит.
    //
    // Проклятого Некромантом лечение не лечит: фляга или умение тратятся и с
    // ранами делают всё то же, только HP не прибавляют (см. HealCurse).
    val fx     = battle0.effects
    val cursed = fx.heroCursed
    val healed = HealedWounds.of(raw, fx.heroPoison, fx.heroBurn)
    val newHp  = if (cursed) hero.fightStats.hp else (hero.fightStats.hp + healed.amount).min(hero.effectiveMaxHp(nowMs))
    val battle = battle0.copy(effects = fx.copy(heroBleed = None, heroPoison = healed.poison,
      heroBurn = healed.burn))
    val curseLine = Option.when(cursed)(content.text("battle.necro.healCursed"))
    val burnLine = healed.burnCutPct.map(cut => healed.burn match {
      case None    => content.format("battle.burnEatsHealOut", "pct" -> cut.toString)
      case Some(b) => content.format("battle.burnEatsHeal", "pct" -> cut.toString, "left" -> b.pct.toString)
    })
    val poisonLine = fx.heroPoison.map(_ => healed.poison match {
      case None    => content.text("battle.healCuresPoison")
      case Some(p) => content.format("battle.healEasesPoison", "left" -> p.pct.toString)
    })
    val lines = List(curseLine, burnLine, poisonLine).flatten
    (newHp, newHp - hero.fightStats.hp, battle, Option.when(lines.nonEmpty)(lines.mkString("\n")))
  }

  /** Строки о том, что лечение героя сделало с ранами союзника: пламя съело
    * часть лечения, яд ослаб или вышел. Кровь уходит молча — как и у героя. */
  private def allyWoundLines(a: BattleAlly, w: HealedWounds): Vector[String] =
    Vector(
      w.burnCutPct.map(cut => w.burn match {
        case None    => content.format("battle.group.allyBurnEatsHealOut", "name" -> a.name, "pct" -> cut.toString)
        case Some(b) => content.format("battle.group.allyBurnEatsHeal",
                          "name" -> a.name, "pct" -> cut.toString, "left" -> b.pct.toString)
      }),
      a.poison.map(_ => w.poison match {
        case None    => content.format("battle.group.allyHealCuresPoison", "name" -> a.name)
        case Some(p) => content.format("battle.group.allyHealEasesPoison", "name" -> a.name, "left" -> p.pct.toString)
      })
    ).flatten

  /** Насколько защита моба срежет удар героя — одинаково для базовой атаки и для
    * умений, которые режутся защитой. Порядок: пробитие героя сначала съедает саму
    * защиту (уже с учётом %-дебафа комбо), остаток идёт в общую формулу против
    * Мощи героя, и только потом Холод и «Дикое пламя» срезают итоговые проценты. */
  private def monsterDefenceCut(
      hero: Hero,
      battle: SoloPveBattle,
      buffed: FightStats,
      nowMs: Long
  ): Double = {
    val base   = hero.effectiveBaseStats(nowMs)
    val power  = BattleState.power(base.str, buffed.atk)
    val pierce = BattleState.pierce(base.int, base.agi, power, buffed.pierce)
    val red0   = BattleState.defenceReduction(effectiveMonsterDefence(battle), pierce, power)
    // «Дикое пламя» (порог 12) добавляет к срезу столько п.п., на сколько цель горит.
    val burnCut =
      if (hero.sets.skillsAlwaysIgnite) battle.effects.monsterBurn.map(_.pct).getOrElse(0) else 0
    (red0 - (battle.effects.monsterColdDefenceCut + burnCut) / 100.0).max(0.0)
  }

  /** Стихия, которой бьёт моб. У минибосса она своя (Белый волк — холодом), у
    * прочих — расовая: каджит всегда с ветром, демон с огнём, гном с холодом,
    * человек и гоблин с молнией (см. [[Race.weaponElement]]). */
  private def powderElement(battle: SoloPveBattle): Option[Element] =
    battle.boss.flatMap(_.attackElement)
      .orElse(Race.withNameOption(battle.monsterRace).flatMap(_.weaponElement))

  /** Чем бьёт моб — приписка к строке его удара («… огнём 🔥»): стихия
    * меняет, сколько уходит в броню и в HP, и игрок должен это видеть. Без
    * стихии — пусто. */
  private def strikeBy(battle: SoloPveBattle): String =
    powderElement(battle).fold("")(e => content.text(s"battle.strikeBy.${e.entryName}"))

  /** Удар, оставивший в ране яд (отравленное оружие, стрелы Фаэлина), помечен
    * прямо в своей строке: «, отравлен 🟢» — у союзника и у врага. Отдельной
    * строки о яде нет. */
  private def poisonMark(poisoned: Boolean): String =
    if (poisoned) content.text("battle.poisonedStrike") else ""

  /** То же, когда отравлен герой: «, вы отравлены 🟢». */
  private def heroPoisonMark(poisoned: Boolean): String =
    if (poisoned) content.text("battle.poisonedYou") else ""

  /** Удар опутал паутиной: «, опутан 🕸» — у союзника. */
  private def webMark(webbed: Boolean): String =
    if (webbed) content.text("battle.webbedStrike") else ""

  /** То же, когда опутан герой: «, вы опутаны 🕸». */
  private def heroWebMark(webbed: Boolean): String =
    if (webbed) content.text("battle.webbedYou") else ""

  /** Почему герой не сходит с места: опутан паутиной — своя строка; вмёрз в лёд
    * (ледопад Ледяного) — своя; в сети ловцов (бой с ловцами, см.
    * BattleState.netFight) — своя; иначе общая «пригвождён». */
  private def heldKey(battle: SoloPveBattle, pinned: String): String =
    if (battle.effects.heroWebbed) "battle.webHolds"
    else if (battle.effects.heroFrozen) "battle.iceHolds"
    else if (BattleState.netFight(battle) && battle.effects.heroRootedTurns > 0) "battle.netHolds"
    else pinned

  /** Прок воздуха в ударе моба: ему точность и уклонение на несколько ходов. */
  private def airBoostLine(monster: String): String =
    content.format("battle.mobAirBoost", "monster" -> monster,
      "pct" -> Element.Air.ProcBonusPct.toString, "turns" -> Element.Air.ProcTurns.toString)

  /** Доли урона по броне и HP от стихии удара моба. У каменного элементаля
    * свой раскол, он важнее — там это природа удара, а не стихия. */
  private def powderSplit(battle: SoloPveBattle): Option[(Double, Double)] =
    powderElement(battle).map(e => (e.armorMult, e.hpMult))

  /** Сколько из урона по броне молния минибосса пробивает дальше, в HP: та же
    * доля, что у Молнии в оружии героя ([[Element.Lightning.ArmorToHpFrac]]).
    * Так бьёт Громовой элементаль; у людей и гоблинов молния на оружии броню
    * не пробивает. */
  private def lightningPierce(battle: SoloPveBattle, armorDmg: Long): Long =
    if (battle.boss.flatMap(_.attackElement).contains(Element.Lightning))
      (armorDmg * Element.Lightning.ArmorToHpFrac).toLong
    else 0L

  /** «Каменный страж» (порог 4): стихийный урон по герою слабее на 20%.
    * Стихийным считается ВЕСЬ урон элементаля — он не бьёт «обычной атакой», он
    * и есть огонь: и удар, и всплеск, и смерч, и шипы, и горение. У прочих мобов
    * стихийного урона нет, поэтому им порог ничего не режет.
    *
    * Огонь по герою-нежити («Некромант», порог 10) наоборот весомее — весь урон
    * огненного элементаля и есть огонь. */
  private def bossDamageTaken(hero: Hero, battle: SoloPveBattle, damage: Long): Long =
    if (battle.boss.isEmpty || damage <= 0L) damage
    else {
      val fire = if (battle.boss.contains(MiniBoss.FireElemental)) hero.sets.fireTakenMult else 1.0
      (damage * hero.sets.elementalDamageTakenMult * fire).toLong.max(1L)
    }

  /** Урон по цели приёма. Герою — привычным путём ([[hurtHero]]) с поправкой на
    * его наборы; союзнику — по его защите и броне, как при обычном ударе моба.
    * Возвращает героя, бой и сколько урона прошло (для строки лога). */
  private def hurtVictim(v: Victim, hero: Hero, battle: SoloPveBattle, damage: Long, nowMs: Long): (Hero, SoloPveBattle, Long) =
    v match {
      case Victim.Hero =>
        // Приём босса — тоже умение: поднятый щит режет и его.
        val (hurt, dealt) = hurtHero(hero, bossDamageTaken(hero, battle, BattleState.shieldCut(battle.heroBlockPct, damage)), nowMs)
        (hurt, battle, dealt)
      case Victim.Ally(pos) =>
        battle.group.allyAt(pos).filter(_.alive) match {
          case None => (hero, battle, 0L)
          case Some(a) =>
            val reduced  = (damage * (1.0 - allyReduction(a, battle.monsterStats.atk))).toLong.max(1L)
            val toArmor  = reduced.min(a.armor)
            val toHp     = reduced - toArmor
            val hurt     = a.copy(armor = a.armor - toArmor, hp = (a.hp - toHp).max(0L), lastHitBy = Some(battle.monsterName))
            (hero, battle.copy(group = battle.group.updateAlly(pos)(_ => hurt)), reduced)
        }
    }

  /** Повесить рану на цель приёма: герою — в эффекты боя, союзнику — на него
    * самого. Яд копится, как и у героя: повторный приём складывает проценты. */
  private def poisonVictim(v: Victim, battle: SoloPveBattle, pct: Int): SoloPveBattle =
    v match {
      // Змеиная сыворотка: яд на героя не ложится до конца боя.
      case Victim.Hero if battle.effects.warded(Ward.Poison) => battle
      case Victim.Hero =>
        val grown = battle.effects.heroPoison.map(p => Poison(p.pct + pct)).getOrElse(Poison(pct))
        battle.copy(effects = battle.effects.copy(heroPoison = Some(grown)))
      case Victim.Ally(pos) =>
        battle.copy(group = battle.group.updateAlly(pos)(a =>
          a.copy(poison = Some(a.poison.map(p => Poison(p.pct + pct)).getOrElse(Poison(pct))))))
    }

  private def bleedVictim(v: Victim, battle: SoloPveBattle, pct: Int): SoloPveBattle =
    v match {
      // Запирающий отвар: раны героя не кровят до конца боя.
      case Victim.Hero if battle.effects.warded(Ward.Bleed) => battle
      case Victim.Hero =>
        val grown = battle.effects.heroBleed.map(b => Bleed(b.pct + pct)).getOrElse(Bleed(pct))
        battle.copy(effects = battle.effects.copy(heroBleed = Some(grown)))
      case Victim.Ally(pos) =>
        battle.copy(group = battle.group.updateAlly(pos)(a =>
          a.copy(bleed = Some(a.bleed.map(b => Bleed(b.pct + pct)).getOrElse(Bleed(pct))))))
    }

  /** Поджечь цель: уже горящее пламя разгорается заново, непогашенное — зажигается.
    * Герой под холодом не загорается: лёд тает (второе в ответе — так и вышло). */
  private def reigniteVictim(v: Victim, battle: SoloPveBattle, pct: Int): (SoloPveBattle, Boolean) =
    v match {
      case Victim.Hero if battle.effects.coldOnHero =>
        (battle.copy(effects = battle.effects.copy(heroColdDefenceCut = 0)), true)
      // Несгораемый настой: пламя на героя не ложится до конца боя.
      case Victim.Hero if battle.effects.warded(Ward.Fire) => (battle, false)
      case Victim.Hero =>
        val fire = battle.effects.heroBurn.map(_.reignited).getOrElse(Burn(pct))
        (battle.copy(effects = battle.effects.copy(heroBurn = Some(fire))), false)
      // Огонь по замёрзшему союзнику растапливает лёд и не занимается — как и на герое.
      case Victim.Ally(pos) if battle.group.allyAt(pos).exists(_.cold) =>
        (battle.copy(group = battle.group.updateAlly(pos)(_.copy(coldCut = 0))), true)
      case Victim.Ally(pos) =>
        (battle.copy(group = battle.group.updateAlly(pos)(a =>
          a.copy(burn = Some(a.burn.map(_.reignited).getOrElse(Burn(pct)))))), false)
    }

  /** Строка о том, что пламя растопило лёд на цели приёма. */
  private def thawLine(v: Victim, battle: SoloPveBattle): String = v match {
    case Victim.Hero      => content.text("battle.elements.thawHero")
    case Victim.Ally(pos) => battle.group.allyAt(pos).fold("")(a => content.format("battle.ally.thaw", "name" -> a.name))
  }

  /** Насколько защита союзника режет удар моба. Холод на нём её обгрызает — как
    * у героя (см. [[heroDamageReduction]]). */
  private def allyReduction(a: BattleAlly, monsterAtk: Long): Double =
    (BattleState.damageReduction(a.stats.defence, 0L, monsterAtk, 0L) - a.coldCut / 100.0).max(0.0)

  /** Куда уходит удар моба по союзнику — как по герою ([[bossHit]]): у
    * минибосса с расколом (каменный) доли в броню и мимо неё в HP, у прочих —
    * сперва броня, остаток в HP, с гранями стихии удара. Урон по броне и по HP. */
  private def allyHitSplit(tmp: SoloPveBattle, a: BattleAlly, damage: Long): (Long, Long) =
    tmp.boss.flatMap(_.heroHitSplit) match {
      case Some((armorPart, hpPart)) =>
        val absorbed = (damage * armorPart).toLong.min(a.armor)
        (absorbed, (damage - absorbed).max((damage * hpPart).toLong))
      case None =>
        val (armorMult, hpMult) = powderSplit(tmp).getOrElse((1.0, 1.0))
        // Опутанного паутиной броня принимает не весь удар: часть идёт прямо в HP.
        val rawArmor = math.min(a.armor, (damage * RedSpider.armorShare(a.webbed > 0, tmp.monsterBreed)).toLong)
        val armorDmg = (rawArmor * armorMult).toLong.min(a.armor).max(0L)
        (armorDmg, (((damage - rawArmor) * hpMult).toLong + lightningPierce(tmp, armorDmg)).max(0L))
    }

  /** Тик ран союзников в конце раунда — те же три, что у героя и у мобов: яд
    * слабеет, кровь держится, пламя растёт, и бьют они мимо брони. Поднятого с
    * алтаря ни яд, ни кровь не берут, зато горит он жарче живого. Павшего от
    * ран уносит свиток, а поднятый рассыпается там же, где стоял. */
  private def tickAllyEffects(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound) res
    else {
      val (ticked, lines) = res.battle.group.allies.foldLeft((List.empty[BattleAlly], Vector.empty[String])) {
        case ((acc, log), a) if !a.alive || (a.poison.isEmpty && a.bleed.isEmpty && a.burn.isEmpty) =>
          (acc :+ a, log)
        case ((acc, log), a0) =>
          var ally  = a0
          var out   = log
          val maxHp = a0.stats.hp
          if (a0.isUndead && (a0.poison.isDefined || a0.bleed.isDefined)) {
            ally = ally.copy(poison = None, bleed = None)
            out  = out :+ content.format("battle.ally.undeadImmune", "name" -> a0.name)
          }
          ally.poison.foreach { p =>
            val dmg = p.damageOn(maxHp)
            ally = ally.copy(hp = (ally.hp - dmg).max(0L), poison = p.decayed)
            out   = out :+ allyDotTick(Poison.Icon, Poison.Label, dmg, a0.name)
          }
          ally.bleed.foreach { b =>
            val dmg = b.damageOn(maxHp)
            ally = ally.copy(hp = (ally.hp - dmg).max(0L))
            out   = out :+ allyDotTick(Bleed.Icon, Bleed.Label, dmg, a0.name)
          }
          ally.burn.foreach { bn =>
            val fire  = if (a0.isUndead) ItemSet.Necromancer.UndeadFireTakenPct else 0L
            val dmg   = bn.damageOn(maxHp) * (100L + fire) / 100L
            val steps = if (a0.isUndead) ItemSet.Necromancer.UndeadBurnGrowthMult else 1L
            ally = ally.copy(hp = (ally.hp - dmg).max(0L),
                             burn = Some((1L to steps).foldLeft(bn)((b, _) => b.grown)))
            out   = out :+ allyDotTick(Burn.Icon, Burn.Label, dmg, a0.name)
          }
          (acc :+ ally, out)
      }
      val fallen  = ticked.filter(a => !a.alive && res.battle.group.allyAt(a.position).exists(_.alive))
      val updated = res.battle.copy(group = res.battle.group.copy(allies = ticked))
      val swept   = fallen.foldLeft(updated)((b, a) => b.copy(group = b.group.withoutAlly(a.position)))
      val gone    = fallen.map(a => content.format(BattleState.goneKey(a), "name" -> a.name))
      val (burnt, wagonLines) = wagonFires(swept)
      res.copy(battle = burnt, log = res.log ++ lines ++ gone ++ wagonLines)
    }

  /** Пламя на повозках обоза — в конце раунда, после ран союзников: кто стоит
    * на горящей повозке, тот сбивает огонь, и она в этот раунд не горит; на
    * непокрытой огонь жжёт HP (мимо брони) и разгорается. Сгоревшая — из обоза
    * вон. Обоза нет — ничего. */
  private def wagonFires(battle: SoloPveBattle): (SoloPveBattle, Vector[String]) =
    battle.group.convoy.fold((battle, Vector.empty[String])) { convoy =>
      val g = battle.group
      val (wagons, lines) = convoy.wagons.foldLeft((List.empty[Wagon], Vector.empty[String])) { case ((acc, log), w) =>
        w.burn match {
          case Some(bn) if w.alive =>
            douser(g, w.position) match {
              case Some(line) => (acc :+ w.cooled, log :+ line)
              case None =>
                val dmg   = bn.damageOn(w.maxHp).max(1L)
                val hot   = w.copy(hp = (w.hp - dmg).max(0L), burn = Some(bn.grown))
                val burns = content.format("battle.group.wagonBurns", "n" -> w.position.toString, "damage" -> dmg.toString)
                if (hot.alive) (acc :+ hot, log :+ burns)
                else (acc :+ hot.cooled, log :+ burns :+ content.format("battle.group.wagonBurnt", "n" -> w.position.toString))
            }
          case _ => (acc :+ w, log)
        }
      }
      (battle.copy(group = g.copy(convoy = Some(convoy.copy(wagons = wagons)))), lines)
    }

  /** Кто сбивает огонь с повозки на месте `pos`: герой на ногах, стоящий на ней,
    * или живой союзник — строка об этом; никого — None. */
  private def douser(g: GroupState, pos: Int): Option[String] =
    if (pos == g.heroPos && !g.heroDown) Some(content.format("battle.group.wagonDousedHero", "n" -> pos.toString))
    else g.allyAt(pos).filter(_.alive).map(a => content.format("battle.group.wagonDoused", "name" -> a.name, "n" -> pos.toString))

  /** Строка тика раны на союзнике — та же, что для мобов, только имя его. */
  private def allyDotTick(icon: String, label: String, damage: Long, name: String): String =
    content.format("battle.ally.dotTick", "icon" -> icon, "dot" -> label,
      "damage" -> damage.toString, "name" -> name)

  /** Урон герою от способности минибосса: сперва броня, остаток в HP. Сам по
    * себе НИЧЕГО не поджигает — пламя вешает тот, кто им владеет. Возвращает
    * героя и сколько урона реально прошло (для строки лога): броня плюс HP после
    * «Упрямства», без обрезки нулём. Расход брони режет
    * порог 6 «Каменного стража» — прикрывает она при этом всё так же. */
  private def hurtHero(hero: Hero, damage: Long, nowMs: Long): (Hero, Long) = {
    val toArmor = damage.min(hero.fightStats.armor)
    // «Орда» (порог 6): большой удар упрямство держит — режется то, что дошло до HP.
    val toHp    = hero.sets.stubbornLoss(damage - toArmor, hero.effectiveMaxHp(nowMs))
    (hero.copy(fightStats = hero.fightStats.copy(
       armor = hero.fightStats.armor - hero.sets.armorSpent(toArmor),
       hp    = (hero.fightStats.hp - toHp).max(0L))),
     toArmor + toHp)
  }

  /** Разбивает `damage` на урон по броне и по HP с учётом стихийных модификаторов
    * оружия. Здесь же учитывается усиление стихии (+2%/грейд): оно входит в
    * `armorDamageMult`/`hpDamageMult` процентными пунктами, а не множит общий
    * урон — см. [[pangea.model.hero.HeroGems.elementalBoost]]. Без стихий
    * (мультипликаторы = 1, доля молнии = 0) поведение прежнее: броня поглощает
    * `min(armor, damage)`, остальное — в HP. */
  private def splitElementalDamage(curArmor: Long, damage: Long, hero: Hero): (Long, Long) = {
    val g = hero.gems
    // «Дикое пламя» (порог 4): обе грани урона огнём выше на свои п.п. Бонус
    // работает только если Огонь реально есть в оружии — набор усиливает стихию,
    // а не выдаёт её.
    val fireBonus    = if (g.hasElement(Element.Fire)) hero.sets.fireDamageBonus else 0.0
    val rawArmorPart = math.min(curArmor, damage)
    val rawHpPart    = damage - rawArmorPart
    val armorDmg     = math.min(curArmor, (rawArmorPart * (g.armorDamageMult + fireBonus)).toLong).max(0L)
    val hpFromHp     = (rawHpPart * (g.hpDamageMult + fireBonus)).toLong
    // Молния: часть урона по броне дополнительно бьёт по HP; «Гроза» (порог 4)
    // прошивает броню вдвое сильнее.
    val hpFromArmor  = (armorDmg * g.lightningArmorToHpFrac * hero.sets.lightningPierceMult).toLong
    (armorDmg, (hpFromHp + hpFromArmor).max(0L))
  }

  /** Роллит проки стихий оружия (по 30% на каждую стихию в оружии) и применяет их:
    * сперва комбо (Огонь+Воздух, Молния+Холод), затем оставшиеся одиночные эффекты.
    * Возвращает обновлённый бой и строки лога. Урон комбо снимается сразу (может
    * добить моба — исход перепроверяется вызывающим по `monsterCurrentHp`). */
  private def resolveElementProcs(hero: Hero, battle: SoloPveBattle, hpDealt: Long, skill: Boolean,
                                  chainFrom: Long = 0L): Task[(SoloPveBattle, Vector[String], Set[Element])] = {
    val elems = hero.gems.weaponElements
    if (elems.isEmpty) ZIO.succeed((battle, Vector.empty, Set.empty))
    else
      // Роллим в фиксированном порядке Element.values для детерминизма тестов.
      ZIO.foreach(Element.values.filter(elems.contains).toList) { e =>
        // «Дикое пламя» (порог 10) поднимает шанс поджечь, «Подгорный мастер»
        // (порог 9) — сковать холодом, «Пустынь» (порог 6) — шанс Воздуха.
        val chance =
          if (e == Element.Fire) Element.ProcChancePct + hero.sets.igniteChanceBonusPct
          else if (e == Element.Cold) Element.ProcChancePct + hero.sets.coldChanceBonusPct
          else if (e == Element.Air) Element.ProcChancePct + hero.sets.airChanceBonusPct
          // «Гроза» (порог 10) — шанс прока молнии.
          else if (e == Element.Lightning) Element.ProcChancePct + hero.sets.lightningChanceBonusPct
          else Element.ProcChancePct
        Random.nextIntBetween(1, 101).map(roll => e -> (roll <= chance))
      }.map { rolls =>
        val fired              = rolls.collect { case (e, true) => e }.toSet
        // Молния выжигает с обычной атаки половину урона по HP, с умения — 20%
        // (см. Element); соседям цепная молния — по тому же правилу.
        val burnOf: Long => Long =
          if (skill) (hp: Long) => Element.skillLightningBurn(hp) else (hp: Long) => Element.lightningBurn(hp)
        val (procced, log)     = applyProcs(battle, fired, heroAirTurns(hero), heroProcMods(hero, burnOf(hpDealt)))
        val (chained, chainLog) = chainLightning(hero, procced, fired, chainFrom, burnOf)
        (chained, log ++ chainLog, fired)
      }
  }

  private def heroProcMods(hero: Hero, burn: Long): BattleState.ProcMods =
    BattleState.ProcMods(burn, hero.sets.burnHurts, hero.sets.coldCutPct, hero.sets.coldRoots)

  /** «Гроза» (порог 12): прок молнии перескакивает на врагов рядом с целью — им
    * по `ChainPct`% урона удара `struck` (сперва броня, остаток в HP; Громовому
    * молния почти не вредит), и энергию им молния выжигает по тем же правилам.
    * Не чаще раза за раунд. `struck` = 0 — перескока нет (отголосок по строю). */
  private def chainLightning(hero: Hero, battle: SoloPveBattle, fired: Set[Element],
                             struck: Long, burnOf: Long => Long): (SoloPveBattle, Vector[String]) =
    if (!fired(Element.Lightning) || !hero.sets.chainLightning || battle.chainUsedThisRound || struck <= 0L)
      (battle, Vector.empty)
    else {
      val pos = battle.group.activePos
      val (swept, lines) = strikeOthers(battle, List(pos - 1, pos + 1)) { tmp =>
        val resist   = tmp.boss.map(_.damageTakenMult(Element.Lightning)).getOrElse(1.0)
        val amount   = (struck * ItemSet.Storm.ChainPct / 100L * resist).toLong.max(1L)
        val hpBefore = tmp.monsterCurrentHp
        val (cut, line) = plainHit(tmp, amount)(d =>
          content.format("battle.storm.chain", "monster" -> tmp.monsterName, "damage" -> d.toString))
        if (cut.monsterCurrentHp <= 0L) (cut, line)
        else {
          val (zapped, zapLog) = applyProcs(cut, Set(Element.Lightning), heroAirTurns(hero),
            heroProcMods(hero, burnOf(hpBefore - cut.monsterCurrentHp)))
          (zapped, (line +: zapLog).mkString("\n"))
        }
      }
      if (lines.isEmpty) (battle, lines) else (swept.copy(chainUsedThisRound = true), lines)
    }

  /** «Стужа» (порог 6): каждый прок холода возвращает герою часть брони — не
    * выше потолка. Без набора или без холода герой как был. */
  private def frostArmor(hero: Hero, fired: Set[Element], nowMs: Long): (Hero, Vector[String]) =
    if (!fired(Element.Cold) || hero.sets.coldArmorRestorePct <= 0L) (hero, Vector.empty)
    else {
      val max    = hero.effectiveMaxArmor(nowMs)
      val gained = (max * hero.sets.coldArmorRestorePct / 100L).max(1L).min((max - hero.fightStats.armor).max(0L))
      if (gained <= 0L) (hero, Vector.empty)
      else (hero.copy(fightStats = hero.fightStats.copy(armor = hero.fightStats.armor + gained)),
            Vector(content.format("battle.frost.armor", "armor" -> gained.toString)))
    }

  /** Строка прока холода героя: обычная — как всегда; со «Стужей» — с её срезом
    * защиты и обездвиживанием. */
  private def coldLine(mods: BattleState.ProcMods): String =
    if (mods.coldRoots) content.format("battle.frost.coldRoots", "cut" -> mods.coldCut.toString)
    else if (mods.coldCut != Element.Cold.DefenceReductionCut) content.format("battle.frost.cold", "cut" -> mods.coldCut.toString)
    else Element.Cold.procText

  /** Применяет сработавшие проки стихий `fired` к мобу: сперва комбо, затем
    * одиночные. Чистая функция — ею же пользуется стихийная фляга, у которой прок
    * гарантирован. Иммунитеты цели соблюдает сама (итог — через `withEffects`):
    * кого огонь не берёт, того прок Огня не поджигает. */
  private def applyProcs(battle: SoloPveBattle, fired: Set[Element], airTurns: Int,
                         mods: BattleState.ProcMods): (SoloPveBattle, Vector[String]) = {
    val maxHp      = battle.monsterStats.hp
    val maxArmor   = MonsterSkill.monsterMaxArmor(battle)
    // Огонь и холод гасят друг друга. Сработали оба в одном ударе — ни пламя, ни
    // мороз на цель не ложатся, а что на ней было, то и остаётся. И в комбо ни
    // тот, ни другой уже не идёт: ни Огонь+Воздух, ни Молния+Холод — Воздух и
    // Молния тогда срабатывают поодиночке. Поодиночке: холод по горящему гасит
    // огонь и сам не ложится, огонь по замёрзшему растапливает лёд и сам не
    // занимается.
    val steam      = fired(Element.Fire) && fired(Element.Cold)
    // Комбо — не больше одного за ход героя (см. SoloPveBattle.comboUsed): было
    // уже — стихии ложатся поодиночке. Обоим комбо разом не бывать: одному нужен
    // Огонь, другому Холод, а вместе они гасят друг друга.
    val combos     = !battle.comboUsed && !steam
    val fireAir    = combos && fired(Element.Fire) && fired(Element.Air)
    val lightCold  = combos && fired(Element.Lightning) && fired(Element.Cold)

    // Кого огонь не берёт (огненный элементаль — он сам пламя).
    val fireproof  = battle.boss.exists(_.immuneToBurn)

    var b   = battle
    var log = Vector.empty[String]
    if (steam) log = log :+ content.text("battle.elements.steam")

    // Огонь по цели: под холодом — лёд тает и пламя не занимается.
    def thaw(): Unit = {
      b = b.copy(effects = b.effects.copy(monsterColdDefenceCut = 0, chilledTurns = 0))
      log = log :+ content.format("battle.elements.thaw", "monster" -> b.monsterName)
    }

    // Комбо Огонь+Воздух: мгновенный урон 30% макс.HP + текущий % горения, горение → 2%.
    // Кого огонь не берёт — огненного элементаля, он сам пламя (immuneToBurn), —
    // того комбо не ранит, а раздувает: лечит на часть того урона.
    if (fireAir) {
      val burnPct = b.effects.monsterBurn.map(_.pct).getOrElse(0)
      val dmg     = (maxHp.toDouble * (30 + burnPct) / 100.0).toLong.max(0L)
      if (fireproof) {
        val heal = (dmg * BattleState.ComboFireAirFeedsFirePct / 100L).min((maxHp - b.monsterCurrentHp).max(0L))
        b = b.copy(monsterCurrentHp = b.monsterCurrentHp + heal)
        log = log :+ content.format("battle.comboFireAirAbsorbed", "monster" -> b.monsterName, "heal" -> heal.toString)
      } else {
        // Взрыв снимает 30% HP и больше — это урон от процентов, и минибосс
        // получает вдвое меньше (BossPercentDamage), а теряет от него не
        // больше остатка своего предела за раунд (BossRoundCap). Огненный
        // выше лечится от полного взрыва: лечение — не урон.
        val hurt = b.hurtActive(0L, b.pctDamage(dmg))
        b = hurt.battle
        log = log :+ (
          if (hurt.absorbed) content.format("battle.comboFireAirHeld", "monster" -> b.monsterName)
          else content.format("battle.comboFireAir", "monster" -> b.monsterName, "damage" -> hurt.hp.toString))
        log = log ++ capLine(hurt)
      }
      // Горение после взрыва — по тем же правилам огня и холода; кого огонь не
      // берёт, тот и не загорается.
      if (!steam) {
        if (b.effects.coldOnMonster) thaw()
        else if (!fireproof) b = b.copy(effects = b.effects.copy(monsterBurn = Some(Burn(Burn.Initial))))
      }
    }

    // Комбо Молния+Холод: -10% макс.брони (не переходит в HP) и застрявшее
    // умение — ближайший каст моб пропускает целиком. Одиночные проки обеих
    // стихий при этом ОСТАЮТСЯ: комбо идёт им в довесок, а не вместо них.
    if (lightCold) {
      val armorCut = (maxArmor.toDouble * BattleState.ComboArmorCutPct / 100.0).toLong.max(0L)
      // Броню минибосса трещина срезает не больше остатка предела за раунд.
      val hurt     = b.hurtActive(armorCut, 0L)
      b = hurt.battle.copy(
        effects = hurt.battle.effects.copy(monsterSkillBlockedTurns = BattleState.ComboSkillBlockTurns)
      )
      log = (log :+ content.format("battle.comboLightningCold", "monster" -> b.monsterName,
        "armor" -> hurt.armor.toString)) ++ capLine(hurt)
    }

    // Одиночные проки стихий — идут своим чередом, в том числе после комбо.
    // Холод по горящему: пламя гаснет, мороз не ложится.
    if (fired(Element.Cold) && !steam && b.effects.monsterBurn.isDefined) {
      b = b.copy(effects = b.effects.copy(monsterBurn = None))
      log = log :+ content.format("battle.elements.quench", "monster" -> b.monsterName)
    }
    // Ледяного мороз не сковывает, а наращивает на нём лёд: броня прибавляется
    // (не выше потолка, что ему оставил огонь), а защита не тает.
    else if (fired(Element.Cold) && !steam && b.boss.contains(MiniBoss.IceElemental)) {
      val cap    = monsterArmorCap(b)
      val gained = (b.monsterStats.armor * MiniBoss.IceElemental.ColdArmorGainPct / 100L).max(1L)
                     .min((cap - b.monsterCurrentArmor).max(0L))
      b = b.copy(monsterCurrentArmor = b.monsterCurrentArmor + gained)
      log = log :+ content.format("battle.ice.coldGrows", "monster" -> b.monsterName, "armor" -> gained.toString)
    }
    else if (fired(Element.Cold) && !steam) {
      b = b.copy(effects = b.effects.copy(
        monsterColdDefenceCut = b.effects.monsterColdDefenceCut + mods.coldCut))
      // «Стужа» (порог 12): каждый прок холода обездвиживает цель на раунд.
      if (mods.coldRoots) b = rooted(b, ItemSet.Frost.RootRounds)
      log = log :+ coldLine(mods)
      // Огненного элементаля прок Холода вдобавок сковывает: шипы молчат,
      // его атаки перестают поджигать, точность срезана, и одна собранная
      // сфера гаснет. Каменному холод ничего сверх обычного не делает.
      if (b.boss.contains(MiniBoss.FireElemental)) {
        val orbsLeft = (b.bossCharges - 1).max(0)
        if (b.bossCharges > 0)
          log = log :+ content.format("battle.elemental.orbDestroyed", "orbs" -> orbsLeft.toString)
        b = b.copy(
          bossCharges = orbsLeft,
          effects          = b.effects.copy(chilledTurns = MiniBoss.FireElemental.ChilledTurns))
      }
    }
    if (fired(Element.Fire) && !fireAir && !steam && b.effects.coldOnMonster) thaw()
    // Огненного элементаля не поджечь — он и сам пламя.
    else if (fired(Element.Fire) && !fireAir && !steam && fireproof)
      log = log :+ content.text("battle.burnImmune")
    else if (fired(Element.Fire) && !fireAir && !steam) {
      val burn = b.effects.monsterBurn.map(_.reignited).getOrElse(Burn.onIgnite)
      b = b.copy(effects = b.effects.copy(monsterBurn = Some(burn)))
      log = log :+ Element.Fire.procText
      // Каменный элементаль от пламени плывёт: бьёт слабее, теряет валун и
      // часть ПОТОЛКА брони (текущая при этом остаётся какая была).
      if (b.boss.contains(MiniBoss.StoneElemental)) {
        val left = (b.bossCharges - 1).max(0)
        if (b.bossCharges > 0)
          log = log :+ content.format("battle.elemental.boulderMelted", "boulders" -> left.toString)
        b = b.copy(
          bossCharges = left,
          effects = b.effects.copy(
            monsterWeakenedTurns = MiniBoss.StoneElemental.BurnedTurns,
            monsterWeakenedPct   = MiniBoss.StoneElemental.BurnedDamageCutPct,
            monsterMaxArmorCut   = b.effects.monsterMaxArmorCut + MiniBoss.StoneElemental.BurnedMaxArmorCut))
        log = log :+ content.format("battle.elemental.stoneMelts",
                "pct" -> MiniBoss.StoneElemental.BurnedDamageCutPct.toString,
                "turns" -> MiniBoss.StoneElemental.BurnedTurns.toString,
                "armor" -> MiniBoss.StoneElemental.BurnedMaxArmorCut.toString)
      }
      // Ледяной от пламени тает: гаснет одна накопленная глыба, и потолок его
      // брони ниже (текущая остаётся какая была — как у каменного).
      if (b.boss.contains(MiniBoss.IceElemental)) {
        val ice  = MiniBoss.IceElemental
        val left = (b.bossCharges - 1).max(0)
        val cut  = (b.monsterStats.armor * ice.MeltMaxArmorCutPct / 100L).max(1L)
        b = b.copy(
          bossCharges = left,
          effects     = b.effects.copy(monsterMaxArmorCut = b.effects.monsterMaxArmorCut + cut))
        log = log :+ content.format("battle.ice.melts", "blocks" -> left.toString, "armor" -> cut.toString)
      }
    }
    // Попутный ветер: срок — `airTurns` (у героя с «Пустынью» дольше, см. heroAirTurns).
    if (fired(Element.Air) && !fireAir) {
      b = b.copy(effects = b.effects.copy(airBoostTurns = airTurns))
      log = log :+ Element.Air.procText.replace("{turns}", airTurns.toString)
      // Громовому ветер вдобавок разгоняет тучу: гаснет один накопленный заряд,
      // и на несколько раундов он бьёт менее точно — так же, как холод сковывает
      // огненного.
      if (b.boss.contains(MiniBoss.ThunderElemental)) {
        val thunder = MiniBoss.ThunderElemental
        b = b.copy(
          bossCharges = (b.bossCharges - 1).max(0),
          effects     = b.effects.copy(mobDispersedTurns = Some(thunder.DispersedTurns)))
        log = log :+ content.format("battle.thunder.dispersed",
          "pct" -> thunder.DispersedAccuracyCutPct.toString, "turns" -> thunder.DispersedTurns.toString,
          "charges" -> b.bossCharges.toString)
      }
    }
    if (fired(Element.Lightning) && b.boss.contains(MiniBoss.ThunderElemental)) {
      // Громовой молнию не теряет, а впитывает: энергии у него прибавляется.
      val cap    = b.monsterStats.energy
      val gained = ((cap * MiniBoss.ThunderElemental.LightningChargePct / 100L).max(1L))
                     .min((cap - b.monsterCurrentEnergy).max(0L))
      b = b.copy(monsterCurrentEnergy = b.monsterCurrentEnergy + gained)
      log = log :+ content.format("battle.thunder.absorbs", "monster" -> b.monsterName, "energy" -> gained.toString)
    }
    else if (fired(Element.Lightning)) {
      // Выжиг энергии: половина урона удара по HP (см. Element.LightningBurnOfHpPct),
      // но не больше, чем у цели есть. Умение, на которое моб копил,
      // откладывается. Строка — всегда, как и прежде: прок был, даже если
      // выжигать оказалось нечего.
      val (burnt, lost) = BattleState.burnEnergyBy(b, mods.burn)
      b = burnt
      // «Гроза» (порог 6): выжженная энергия бьёт и по HP — столько же урона;
      // минибосс теряет не больше остатка своего предела за раунд.
      if (mods.burnHurts && lost > 0L && b.monsterCurrentHp > 0L) {
        val hurt = b.hurtActive(0L, lost)
        b = hurt.battle
        log = log :+ (
          if (hurt.absorbed) capText(b)
          else content.format("battle.storm.burnHurts", "monster" -> b.monsterName, "energy" -> lost.toString,
                 "damage" -> hurt.hp.toString) + capLine(hurt).map("\n" + _).getOrElse(""))
      }
      else
        log = log :+ content.format("battle.lightningBurn", "monster" -> b.monsterName, "energy" -> lost.toString)
    }
    if (fireAir || lightCold) b = b.copy(comboUsed = true)
    (b.withEffects(b.effects), log)
  }

  /** Умения, которые моб может применить прямо сейчас: доступные его расе,
    * полезные в этой ситуации и оплатимые текущей энергией. */
  private def affordableSkills(battle: SoloPveBattle): Seq[MonsterSkill] = {
    val race = Race.withName(battle.monsterRace)
    MonsterSkill.values.filter { s =>
      s.availableTo(race) && s.applicable(battle) && s.cost(battle.monsterLvl) <= battle.monsterCurrentEnergy
    }
  }

  /** Итоговая защита моба с учётом временного %-дебафа (комбо Молния+Холод). */
  private def effectiveMonsterDefence(battle: SoloPveBattle): Long =
    battle.effects.monsterDefenceDebuff match {
      case Some(d) => (battle.monsterStats.defence * (100 - d.pct) / 100).max(0L)
      case None    => battle.monsterStats.defence
    }

  /** Урон яда или кровотечения по мобу с поправкой на его природу: зверь из
    * плоти и крови (Белый волк) истекает на пятую часть сильнее. Горение сюда
    * не входит. */
  private def monsterDotDamage(battle: SoloPveBattle, base: Long): Long =
    battle.boss.map(_.dotDamageTakenMult).filter(_ != 1.0).fold(base)(m => (base * m).toLong)

  /** Строка тика DoT в конце раунда. Одна на все три — яд, кровотечение и
    * пламя: меняется только значок с именем эффекта. Больше их нигде в логе не
    * поминают, чтобы одно и то же число не попадалось игроку трижды за раунд;
    * сколько снимется за ход, видно в шапке рядом с HP. */
  private def dotTick(icon: String, label: String, damage: Long, monster: String): String =
    content.format("battle.dotTick", "icon" -> icon, "dot" -> label,
      "damage" -> damage.toString, "monster" -> monster)

  /** То же в сторону ГЕРОЯ: имя моба тут ни при чём. */
  private def heroDotTick(icon: String, label: String, damage: Long): String =
    content.format("battle.heroDotTick", "icon" -> icon, "dot" -> label, "damage" -> damage.toString)

  /** Вампиризм за раунд — одной строкой, как реген, и не важно, откуда он пришёл:
    * череп в оружии, «Упырь» (порог 4), вампирская фляга или чужая кровь (порог
    * 10). Пусто, если пить было нечего. */
  private def vampLine(healed: Long): Vector[String] =
    if (healed > 0L) Vector(content.format("battle.vampirism", "healed" -> healed.toString))
    else Vector.empty

  // ── Ход монстра ─────────────────────────────────────────────────────────────

  /** Ход моба после атаки игрока. `log` — строки, накопленные в фазе игрока;
    * между сегментами игрока и монстра вставляется пустая строка-разделитель
    * (в склейке `mkString("\n")` даёт двойной перенос).
    */
  /** Что случилось с героем от обычной атаки моба: новые HP и броня, сам урон,
    * побочные строки (Непробиваемый, Крепкость, шипы, поджог, яд, проки
    * порошка) и бой после удара (шипы, порошок). Строку «наносит N урона»
    * собирает вызывающий — у активного моба и у бьющего сбоку она разная. */
  private def mobStrike(hero: Hero, battle: SoloPveBattle, buffedEff: FightStats, nowMs: Long): Task[MobStrike] =
    // Зеркальный настой: пока копии целы, удар достаётся им, а не герою.
    if (battle.effects.heroMirrors > 0) {
      val left = battle.effects.heroMirrors - 1
      ZIO.succeed(MobStrike(
        newHp      = hero.fightStats.hp,
        newArmor   = hero.fightStats.armor,
        damage     = 0L,
        extraLines = List(content.format("battle.mirrorTook",
                       "monster" -> battle.monsterName, "left" -> left.toString)),
        battle     = battle.copy(effects = battle.effects.copy(heroMirrors = left)),
        mirrored   = true))
    } else
      // Отвести обычный удар целиком: механика готова, но ставку пока никто не
      // даёт (см. HeroSets.blockAttackChancePct) — броска при нуле нет.
      chanceRoll(hero.sets.blockAttackChancePct > 0L, hero.sets.blockAttackChancePct).flatMap {
        case true =>
          ZIO.succeed(MobStrike(
            newHp      = hero.fightStats.hp,
            newArmor   = hero.fightStats.armor,
            damage     = 0L,
            extraLines = List(content.format("battle.hunterBlock", "monster" -> battle.monsterName)),
            battle     = battle,
            mirrored   = true))
        case false => plainMobStrike(hero, battle, buffedEff, nowMs)
      }

  private def plainMobStrike(hero: Hero, battle: SoloPveBattle, buffedEff: FightStats, nowMs: Long): Task[MobStrike] = {
    val monster = battle.toMonster
    for {
            spread <- Random.nextLongBetween(80L, 121L)
            rawDamage = (monsterAttack(battle) * spread / 100L * Achievement.damagePct(hero) / 100L).max(1L)
            reduction = BattleState.heroDamageReduction(hero, battle, buffedEff, monster.fightStats.atk, nowMs)
            // Поднятый щит принимает три четверти того, что прошло сквозь защиту.
            baseReduced = BattleState.shieldCut(battle.heroBlockPct, (rawDamage * (1.0 - reduction)).toLong.max(1L))
            // «Непробиваемый»: 20% шанс срезать полученный урон обычной атаки вдвое.
            impenTriggered <- chanceRoll(hero.passives.hasImpenetrable, PassiveKind.Impenetrable.TriggerPct)
            impenDamage =
              if (impenTriggered) (baseReduced * (100L - hero.passives.impenetrableReductionPct) / 100L).max(1L)
              else baseReduced
            // «Мать Глубин» (порог 6): с шансом удар соскальзывает по чешуе.
            slipped <- chanceRoll(hero.sets.slipChancePct > 0L, hero.sets.slipChancePct)
            slipDamage = if (slipped) (impenDamage * (100L - hero.sets.slipCutPct) / 100L).max(1L) else impenDamage
            // Удар элементаля — это стихия, а не сталь: «Каменный страж» его режет.
            reducedDamage = bossDamageTaken(hero, battle, slipDamage)
            (newHp, newArmor, landed) = bossHit(battle, hero, reducedDamage, nowMs)
            // «Крепкость»: при обнулении брони 25% шанс одноразово восстановить 10% макс.брони.
            armorEmptied = hero.fightStats.armor > 0 && newArmor <= 0
            toughTriggered <- chanceRoll(
              armorEmptied && !battle.toughnessUsed && hero.passives.hasToughness,
              PassiveKind.Toughness.TriggerPct
            )
            restoredArmor =
              if (toughTriggered) (hero.effectiveMaxArmor(nowMs) * hero.passives.toughnessRestorePct / 100L).max(1L) else 0L
            // «Шипастый»: вернуть 5% полученного урона врагу (по HP моба, мимо брони).
            thorns = if (hero.passives.hasSpiky) (reducedDamage * hero.passives.thornsPct / 100L).max(1L) else 0L
            // Поджигает героя обычной атакой только тот, у кого огонь в природе
            // (огненный элементаль), и только пока не скован холодом. У камня и
            // гнили шанс нулевой, поэтому бросок у них не тратится.
            // Огненный порошок даёт мобу тот же шанс поджечь, что и обычный прок Огня.
            igniteChance = battle.boss.map(_.heroIgniteChancePct)
                             .getOrElse(if (powderElement(battle).contains(Element.Fire)) Element.ProcChancePct else 0L)
            ignites <- chanceRoll(
              igniteChance > 0L && !battle.effects.chilled,
              (igniteChance - hero.sets.igniteResistPct).max(0L)
            )
            burnPct = battle.boss.map(_.heroIgniteBurnPct).getOrElse(Burn.Initial)
            // Прок стихии порошка — тот же 30%-й бросок, что и у стихий оружия.
            // Огонь уже отыгран поджогом выше, здесь остаются холод, воздух и
            // молния: она выжигает герою энергию, как прок Молнии в оружии — мобу.
            procElement = powderElement(battle).filter(e => e == Element.Cold || e == Element.Air || e == Element.Lightning)
            mobProc <- chanceRoll(procElement.isDefined, Element.ProcChancePct)
            // Молния выжигает половину урона, что удар снял с HP героя.
            energyBurnt = if (mobProc && procElement.contains(Element.Lightning))
                            Element.lightningBurn(hero.fightStats.hp - newHp).min(hero.fightStats.energy.max(0L))
                          else 0L
            // Отравленное оружие: удар, дошедший до HP, травит. Мурлок и эльф
            // ходят с ним всегда (Race.weaponPoison), но травят через раз
            // (Poison.WeaponChancePct); Крысиному королю яд поставлен флагом при
            // встрече, и он травит каждым таким ударом. Бросок — только если удар
            // дошёл до HP.
            reachedHp = newHp < hero.fightStats.hp
            bladeHits <- chanceRoll(reachedHp && !battle.effects.monsterPoisonsOnHit &&
                           Race.withNameOption(battle.monsterRace).exists(_.weaponPoison), Poison.WeaponChancePct)
            // Красный сетеплёт травит с шансом (см. RedSpider).
            redPoison <- chanceRoll(reachedHp && RedSpider.is(battle.monsterBreed), RedSpider.PoisonPct)
            poisons = reachedHp && (battle.effects.monsterPoisonsOnHit || bladeHits || redPoison)
            // Паутина взрослого и красного сетеплёта: удар с шансом опутывает (см. SpiderWeb).
            webs <- chanceRoll(SpiderWeb.weaves(battle.monsterBreed), SpiderWeb.chancePct(battle.monsterLvl,
                      hero.effectiveBaseStats(nowMs).agi, buffedEff.evasion, buffedEff.defence))
            // Огонь и холод гасят друг друга и на герое: огонь по замёрзшему
            // растапливает лёд и не занимается, холод по горящему гасит пламя и
            // не ложится.
            thawed   = ignites && battle.effects.coldOnHero
            quenched = mobProc && procElement.contains(Element.Cold) &&
                         (battle.effects.heroBurn.isDefined || (ignites && !thawed))
            // Защитные отвары: что выпито, то до конца боя на героя не ложится —
            // и в лог идёт «не взяло», а не «подожгли».
            fireWarded   = ignites && !thawed && battle.effects.warded(Ward.Fire)
            poisonWarded = poisons && battle.effects.warded(Ward.Poison)
            // Паучья сыворотка: бьёт сетеплёт — ни его яд, ни паутина не берут.
            spiderWard   = battle.effects.warded(Ward.Spider) && Breed.isSpider(battle.monsterBreed)
            spiderPoisonWarded = poisons && !poisonWarded && spiderWard
            webWarded    = webs && spiderWard
            webbed       = webs && !webWarded
            // Отравил красный — с шансом следующее умение героя сорвётся.
            jams <- chanceRoll(redPoison && !poisonWarded && !spiderWard, RedSpider.JamPct)
            coldWarded   = mobProc && procElement.contains(Element.Cold) && !quenched &&
                             battle.effects.warded(Ward.Cold)
            effectsAfterHit = {
              val burned =
                if (!ignites) battle.effects
                else if (thawed) battle.effects.copy(heroColdDefenceCut = 0)
                else if (fireWarded) battle.effects
                else battle.effects.copy(heroBurn = Some(
                  battle.effects.heroBurn.map(_.reignited).getOrElse(Burn(burnPct))))
              val poisonedE =
                if (!poisons || poisonWarded || spiderPoisonWarded) burned
                else burned.copy(heroPoison = Some(
                  burned.heroPoison.map(p => Poison(p.pct + Poison.OnHit)).getOrElse(Poison.onHit)))
              // Холод обгрызает защиту героя (накопительно, не затухает), воздух
              // поднимает мобу точность и уклонение на несколько ходов.
              (if (mobProc) procElement else None) match {
                case Some(Element.Cold) if quenched => poisonedE.copy(heroBurn = None)
                case Some(Element.Cold) if coldWarded => poisonedE
                case Some(Element.Cold) =>
                  poisonedE.copy(heroColdDefenceCut =
                    poisonedE.heroColdDefenceCut + Element.Cold.DefenceReductionCut)
                case Some(Element.Air) =>
                  poisonedE.copy(mobAirBoostTurns = Element.Air.ProcTurns)
                case _ => poisonedE
              }
            }
            // Шипы ранят минибосса не больше остатка его предела за раунд (см.
            // BossRoundCap); целиком поглощённые молчат.
            thorned = battle.hurtActive(0L, thorns)
            battleAfterAtk = thorned.battle.copy(
              toughnessUsed    = battle.toughnessUsed || toughTriggered,
              effects          = {
                val webbedFx = if (webbed) effectsAfterHit.copy(heroWebTurns = Some(SpiderWeb.Turns)) else effectsAfterHit
                if (jams) webbedFx.withSkillJam(SkillJam.Spider) else webbedFx
              }
            )
            lines = List(
              Option.when(impenTriggered)(content.format("battle.impenetrable",
                "pct" -> hero.passives.impenetrableReductionPct.toString)),
              Option.when(slipped)(content.text("battle.deepSlip")),
              Option.when(toughTriggered)(content.format("battle.toughness", "armor" -> restoredArmor.toString)),
              Option.when(thorned.hp > 0)(content.format("battle.thorns", "damage" -> thorned.hp.toString, "monster" -> battle.monsterName)),
              capLine(thorned),
              // Из минибоссов поджигает только огненный элементаль; остальные —
              // мобы с огнём на оружии (демоны), у них строка без элементаля.
              Option.when(ignites && !thawed && !fireWarded)(content.text(
                if (battle.boss.isDefined) "battle.elemental.ignites" else "battle.mobIgnites")),
              Option.when(fireWarded)(content.text("battle.ward.fire")),
              Option.when(thawed)(content.text("battle.elements.thawHero")),
              // Яд от оружия — пометкой в строке самого удара (см. poisonMark).
              Option.when(poisonWarded)(content.text("battle.ward.poison")),
              Option.when(spiderPoisonWarded)(content.text("battle.ward.spiderPoison")),
              Option.when(webWarded)(content.text("battle.ward.spiderWeb")),
              Option.when(mobProc && procElement.contains(Element.Cold) && !quenched && !coldWarded)(content.format("battle.mobColdBite",
                "cut" -> Element.Cold.DefenceReductionCut.toString)),
              Option.when(coldWarded)(content.text("battle.ward.cold")),
              Option.when(quenched)(content.text("battle.elements.quenchHero")),
              Option.when(mobProc && procElement.contains(Element.Air))(airBoostLine(battle.monsterName)),
              Option.when(energyBurnt > 0L)(content.format("battle.mobLightningBurn", "energy" -> energyBurnt.toString))
            ).flatten
          } yield MobStrike(newHp, newArmor + restoredArmor, landed, lines, battleAfterAtk,
                    poisoned = poisons && !poisonWarded && !spiderPoisonWarded, webbed = webbed, energyBurnt = energyBurnt)
  }

  /** Умение моба поверх обычной атаки: самое дорогое по карману, оплачивается
    * энергией. «Охотник» может погасить первую вредную способность. Возвращает
    * бой моба, героя и строку лога (пустую, если кастовать нечего). */
  private def mobSkillCast(hero: Hero, battle: SoloPveBattle, nowMs: Long, victim: Victim): Task[(SoloPveBattle, Hero, String)] =
    for {
      affordable <- ZIO.succeed(affordableSkills(battle).toVector)
      out <-
        if (affordable.nonEmpty)
          for {
            best <- ZIO.succeed {
                      val top = affordable.map(_.cost(battle.monsterLvl)).max
                      affordable.filter(_.cost(battle.monsterLvl) == top)
                    }
            idx <- Random.nextIntBetween(0, best.size)
            ms   = best(idx)
            paid = battle.copy(monsterCurrentEnergy = (battle.monsterCurrentEnergy - ms.cost(battle.monsterLvl)).max(0L))
            cast = ms.cast(paid, hero, nowMs)
            onAlly = (victim, ms.againstOther) match {
                       case (Victim.Ally(pos), Some(hit)) => Some((pos, hit))
                       case _                             => None
                     }
            hurts = cast.heroHp < hero.fightStats.hp || cast.heroArmor < hero.fightStats.armor
            // «Охотник» (порог 10): приём, который должен был навредить, с шансом
            // срывается целиком — и так каждый раз, а не единожды за бой.
            blocked <- chanceRoll(hurts && hero.sets.blockSkillChancePct > 0L, hero.sets.blockSkillChancePct)
          } yield onAlly match {
            // Перед мобом стоит не герой: бьющее умение достаётся союзнику, а
            // лечение и починка доспеха (у них нет доли урона) идут себе, как
            // шли.
            case Some((pos, hit)) => strikeAllyWithSkill(hero, paid, pos, hit, ms, nowMs)
            case None =>
              if (blocked) (cast.battle, hero, content.text("battle.hunterBlockSkill"))
              else (cast.battle, hero.copy(fightStats = hero.fightStats.copy(hp = cast.heroHp, armor = cast.heroArmor,
                     // Кинжал молнии жжёт энергию героя; прочие умения её не трогают.
                     energy = cast.heroEnergy.getOrElse(hero.fightStats.energy))), cast.line)
          }
        else ZIO.succeed((battle, hero, ""))
    } yield out

  /** Умение моба по союзнику: та же доля его атаки (или ровно `fixed` урона —
    * у кинжала молнии), его же броня и защита (у дробящего — мимо брони), яд,
    * если умение его оставляет, и сгоревшая энергия, если умение её жжёт. */
  private def strikeAllyWithSkill(
      hero: Hero, battle: SoloPveBattle, pos: Int, hit: MonsterSkill.OtherHit, skill: MonsterSkill,
      nowMs: Long
  ): (SoloPveBattle, Hero, String) =
    battle.group.allyAt(pos).filter(_.alive) match {
      case None => (battle, hero, "")
      // Сеть ловцов: урона нет — союзник остаток раунда и весь следующий не
      // сходит с места (снимается в конце раунда, см. webTicked).
      case Some(a) if hit.roots > 0 && hit.fixed.isEmpty && hit.factor <= 0.0 =>
        (battle.copy(group = battle.group.updateAlly(pos)(x => x.copy(rooted = x.rooted.max(hit.roots)))), hero,
          content.format("battle.ally.mobSkillNet", "monster" -> battle.monsterName, "name" -> a.name, "skill" -> skill.label))
      case Some(a) =>
        val raw = hit.fixed.getOrElse((battle.monsterStats.atk * hit.factor).toLong).max(1L)
        val (struck, dealt) =
          if (hit.ignoresArmor)
            (battle.copy(group = battle.group.updateAlly(pos)(x =>
              x.copy(hp = (x.hp - raw).max(0L), lastHitBy = Some(battle.monsterName)))), raw)
          else {
            val (_, b, d) = hurtVictim(Victim.Ally(pos), hero, battle, raw, nowMs)
            (b, d)
          }
        val poisoned = if (hit.poisonPct > 0) poisonVictim(Victim.Ally(pos), struck, hit.poisonPct) else struck
        // Кинжал молнии: у того, в кого попал, сгорает энергия — сколько было, не больше.
        val left     = poisoned.group.allyAt(pos).map(_.energy).getOrElse(0L)
        val burnt    = hit.energyBurn.min(left).max(0L)
        val drained  = if (burnt <= 0L) poisoned
                       else poisoned.copy(group = poisoned.group.updateAlly(pos)(x => x.copy(energy = x.energy - burnt)))
        val key      = if (hit.poisonPct > 0) "battle.ally.mobSkillPoison"
                       else if (hit.energyBurn > 0L) "battle.ally.mobSkillBurn"
                       else "battle.ally.mobSkill"
        (drained, hero, content.format(key, "monster" -> battle.monsterName, "name" -> a.name,
          "skill" -> skill.label, "damage" -> dealt.toString, "burn" -> burnt.toString))
    }

  private def monsterPhase(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      log: Vector[String],
      skip: Set[Long],
      vampHeal: Long
  ): Task[TurnResult] =
    for {
      ticked <- ZIO.succeed(battle.tickBuffs(skip))
      buffedEff = effWithAir(hero, ticked, nowMs)
      hitRoll <- Random.nextIntBetween(1, 101)
      dodge = playerDodgeChance(hero, ticked, nowMs)

      // 1) Обычная атака моба — обновляем hp/armor героя и (для Шипастого/Крепкости) бой.
      atkResult <-
        if (hitRoll > dodge)
          mobStrike(hero, ticked, buffedEff, nowMs).map { st =>
            val line  = content.format("battle.mobHit", "damage" -> st.damage.toString, "monster" -> ticked.monsterName,
                          "elem" -> (strikeBy(ticked) + heroPoisonMark(st.poisoned) + heroWebMark(st.webbed)))
            val lines = if (st.mirrored) st.extraLines else line :: st.extraLines
            (st.newHp, st.newArmor, (hero.fightStats.energy - st.energyBurnt).max(0L), lines.mkString("\n"), st.battle)
          }
        else {
          val missLine = content.format("battle.mobMiss", "monster" -> ticked.monsterName)
          // Контрудар: герой увернулся — и тут же бьёт в ответ, наверняка. Раз за раунд.
          if (ticked.heroCounterReady)
            playerStrike(hero, ticked.copy(heroCounterReady = false), nowMs, Vector.empty, skip,
              retaliate = false, sureHit = true).map { res =>
              (res.hero.fightStats.hp, res.hero.fightStats.armor, res.hero.fightStats.energy,
                (Vector(missLine, content.text("battle.counter")) ++ res.log).mkString("\n"), res.battle)
            }
          else ZIO.succeed((hero.fightStats.hp, hero.fightStats.armor, hero.fightStats.energy, missLine, ticked))
        }
      (hpAfterAtk, armorAfterAtk, energyAfterAtk, mobLine0, battleAfterAtk) = atkResult
      // Удар по HP может оставить травму — сразу за строкой удара.
      struckTrauma <- hitTrauma(hero, hero.copy(fightStats = hero.fightStats.copy(
                        hp = hpAfterAtk, armor = armorAfterAtk, energy = energyAfterAtk)), nowMs)
      (heroAfterAtk, strikeTraumaLine) = struckTrauma
      mobLine = strikeTraumaLine.fold(mobLine0)(l => mobLine0 + "\n" + l)

      // 2) Умение моба оплачивается ЭНЕРГИЕЙ, а не броском кубика: моб берёт самое
      // дорогое из того, что сейчас по карману, применимо и доступно его расе, и
      // списывает цену. Не хватило — бьёт только обычной атакой и копит дальше.
      // Умение идёт ПОВЕРХ обычной атаки и может быть как уроном (CrushingStrike —
      // мимо брони и damageReduction), так и хилом/починкой моба.
      castResult <-
        // Моб не кастует, если герой мёртв или моб добит Шипастым (battleAfterAtk).
        if (heroAfterAtk.fightStats.hp <= 0 || battleAfterAtk.monsterCurrentHp <= 0)
          ZIO.succeed((battleAfterAtk, heroAfterAtk, ""))
        // Скован комбо Молния+Холод: умение застряло, и круг минибосса тоже стоит.
        else if (battleAfterAtk.effects.monsterSkillBlockedTurns > 0)
          ZIO.succeed((battleAfterAtk, heroAfterAtk,
            content.format("battle.skillBlocked", "monster" -> battleAfterAtk.monsterName)))
        // Минибосс не выбирает умение: он идёт строго по своему кругу.
        else if (battleAfterAtk.boss.isDefined)
          bossTurnCast(heroAfterAtk, battleAfterAtk, nowMs, Victim.Hero)
        else mobSkillCast(heroAfterAtk, battleAfterAtk, nowMs, Victim.Hero)
      (castBattle, finalHero0, castLine1) = castResult
      // Приём, что бьёт не одного (цепная молния Громового), мог свалить и
      // союзника рядом с героем — тот уходит из строя, как от удара.
      (finalBattle, castGone) = sweepCastFallen(battleAfterAtk, castBattle)
      castLine0 = (castLine1 +: castGone).filter(_.nonEmpty).mkString("\n")
      // Приём по HP — тоже может оставить травму.
      castTrauma <- hitTrauma(heroAfterAtk, finalHero0, nowMs)
      (finalHero, castTraumaLine) = castTrauma
      castLine = castTraumaLine.fold(castLine0)(l => (if (castLine0.isEmpty) "" else castLine0 + "\n") + l)

      // 3) Конец раунда: тик статус-эффектов. Стадии героя и монстра — раздельные,
      // каждая со своей строкой. Применяются только пока герой жив.
      heroAlive = finalHero.fightStats.hp > 0
      (tickedHero, battleAfterHero, heroEffectLine) =
        if (heroAlive) tickHeroEffects(finalHero, finalBattle, nowMs)
        else (finalHero, finalBattle, "")
      // Энергия копится в конце раунда — из неё оплачивается следующее умение.
      // У минибосса свой реген от BossLvL, у обычного моба — от уровня и редкости;
      // «Ужасающий» героя его урезает (см. [[mobEnergyGain]]).
      battleWithEnergy = {
        val maxEn = battleAfterHero.monsterStats.energy
        val gain  = mobEnergyGain(tickedHero, battleAfterHero)
        battleAfterHero.copy(monsterCurrentEnergy = (battleAfterHero.monsterCurrentEnergy + gain).min(maxEn))
      }
      (tickedBattle, monsterEffectLine, bleedDealt) =
        if (heroAlive) tickMonsterEffects(battleWithEnergy, ticked.monsterName, tickedHero.sets)
        else (battleWithEnergy, "", 0L)
      // «Упырь» (порог 10): кровь ЧУЖОЙ стороны идёт в лечение и делится ровно
      // между вампирами СВОЕЙ. Сегодня вампир один — сам герой, поэтому ему
      // достаётся всё (см. ItemSet.Ghoul.bleedHealShare).
      // Жив ли герой ПОСЛЕ своих тиков: горение, яд и кровь в конце раунда
      // могут добить его, и тогда ни реген, ни чужая кровь не поднимают его с
      // нуля. Раньше тут смотрели жизнь до тиков — и герой на 0 HP получал
      // свои (+N) обратно, бой шёл дальше, а пламя «снимало» тысячи впустую.
      heroStands         = tickedHero.fightStats.hp > 0
      vampiresOnHeroSide = if (tickedHero.sets.healsFromBleed) 1 else 0
      bleedShare         = ItemSet.Ghoul.bleedHealShare(bleedDealt, vampiresOnHeroSide)
      // Проклятого Некромантом чужая кровь не лечит (см. HealCurse).
      heroFedByBleed =
        if (heroStands && bleedShare > 0L && !tickedBattle.effects.heroCursed)
          tickedHero.copy(fightStats = tickedHero.fightStats.copy(
            hp = (tickedHero.fightStats.hp + bleedShare).min(tickedHero.effectiveMaxHp(nowMs))))
        else tickedHero
      // Выпито за раунд: вампиризм с удара плюс чужая кровь. Считаем по
      // РЕАЛЬНОМУ приросту HP — у полного героя лечить уже нечего, и в логе тогда
      // не появится числа, которого он не получил.
      bleedGained = heroFedByBleed.fightStats.hp - tickedHero.fightStats.hp
      drunk       = vampHeal + bleedGained

      // Реген энергии в конце хода: +(Интеллект + 0.5·Ловкость), не меньше 1 и не
      // выше максимума. «Сосредоточенность» множит реген на 1.1. Пока герой жив.
      finalHeroWithEnergy = if (heroStands) endOfRoundHero(heroFedByBleed, tickedBattle, nowMs) else heroFedByBleed

      // Сегмент монстра: пустой разделитель, строка атаки, затем (в исходном
      // порядке) каст моба, тики DoT на мобе, тики и реген героя, и последним —
      // вампиризм. Только непустые.
      monsterLog = {
        val base       = (log :+ "") :+ mobLine
        val withCast   = if (castLine.nonEmpty) base :+ castLine else base
        val withPoison = if (monsterEffectLine.nonEmpty) withCast :+ monsterEffectLine else withCast
        val withHero   = if (heroEffectLine.nonEmpty) withPoison :+ heroEffectLine else withPoison
        withHero ++ vampLine(drunk)
      }

      outcome =
        if (finalHeroWithEnergy.fightStats.hp <= 0) Outcome.Death
        else if (tickedBattle.monsterCurrentHp <= 0) Outcome.Victory
        else Outcome.Continue
    } yield TurnResult(finalHeroWithEnergy, tickedBattle, monsterLog, outcome)

  /** Реген энергии героя в конце раунда: +(Интеллект + 0.5·Ловкость), не меньше
    * 1 и не выше максимума. «Сосредоточенность» (пассивка) и черепа в оружии
    * множат реген, а «Охотник» (порог 4) удваивает вклад именно ловкости. Одна
    * точка на оба конца раунда: после хода моба и на добивании, когда до хода
    * моба не дошло. */
  /** Конец раунда для героя: энергия ([[regainEnergy]]) и реген пассивок
    * «Целебный»/«Самовосстанавливающийся» с черепами в снаряжении — то самое
    * `(+N)` с экрана боя ([[Hero.withCombatRegen]]).
    *
    * Одна точка на все ходы. Раньше реген HP висел на базовой атаке героя, и
    * раунд без удара — «Ждать», шаг к союзнику, умение, после которого бить
    * некого, — проходил без регена вовсе. */
  /** Конец раунда героя: энергия и боевой реген. Проклятого Некромантом реген
    * HP не лечит — броню чинит, энергию копит, как всегда (см. HealCurse). */
  private def endOfRoundHero(hero: Hero, battle: SoloPveBattle, nowMs: Long): Hero = {
    val regen = regainEnergy(hero, nowMs).withCombatRegen(nowMs)
    if (battle.effects.heroCursed) regen.copy(fightStats = regen.fightStats.copy(hp = hero.fightStats.hp))
    else regen
  }

  private def regainEnergy(hero: Hero, nowMs: Long): Hero = {
    val b       = hero.effectiveBaseStats(nowMs)
    val agiPart = 0.5 * b.agi * hero.sets.agiEnergyRegenMult
    val regen   = ((b.int + agiPart) * hero.passives.energyRegenMult * hero.gems.energyRegenMult).toLong.max(1L)
    val maxEn   = hero.maxEnergy(nowMs)
    hero.copy(fightStats = hero.fightStats.copy(energy = (hero.fightStats.energy + regen).min(maxEn)))
  }

  /** Победа ударом или умением героя: раунд кончился без хода моба, но энергия
    * за него всё равно восстанавливается, а бафы, кулдауны и «фляга уже пита»
    * отсчитывают раунд, как после хода моба (`tickBuffs`, `skip` — слот только
    * что применённого умения). Без этого в группе следующий моб встаёт в пару,
    * а фляга и умения считают, что раунд не кончился. В бою 1 на 1 тик
    * безобиден — бой всё равно кончился.
    *
    * Раны героя здесь не тикают: кончится ли бой, решит промоут. Пойдёт бой
    * дальше — они тикают там (см. [[dueWounds]]). */
  private def victoryByHero(hero: Hero, battle: SoloPveBattle, log: Vector[String], nowMs: Long, skip: Set[Long]): TurnResult =
    TurnResult(endOfRoundHero(hero, battle, nowMs), battle.tickBuffs(skip), log, Outcome.Victory, woundsDue = true)

  /** Раны героя в раунде, который он сам кончил, добив активного (см.
    * [[victoryByHero]]): до конца хода, где они тикают, — ответа моба в паре
    * или тихого конца раунда — дело не дошло. Бой этим кончился — им тикать
    * незачем, победа уже за героем. А если бой идёт — в группе к герою шагнёт
    * следующий, Гнилой Джо встал, — это обычный раунд, и яд, кровь, пламя и
    * реген на герое тикают, как в любом другом.
    * Раньше герой, добивавший по мобу за раунд, носил яд, не теряя от него ни
    * HP, а сам яд не слабел. Тик добил героя — это смерть, как от удара: с
    * «Первопроходцем» и «Каменным стражем». */
  private def dueWounds(res: TurnResult, nowMs: Long): TurnResult =
    if (!res.woundsDue || res.outcome != Outcome.Continue) res
    else {
      val (hero, battle, line) = tickHeroEffects(res.hero, res.battle, nowMs)
      val ticked = res.copy(hero = hero, battle = battle, woundsDue = false,
        log     = res.log ++ Vector(line).filter(_.nonEmpty),
        outcome = if (hero.fightStats.hp > 0L) Outcome.Continue else Outcome.Death)
      stoneGuardRescue(res.hero, secondWind(ticked, side = false), nowMs)
    }

  /** Сколько снимет с героя один тик горения. По нему же экран боя пишет
    * прогноз у HP героя (🔥-N): что обещано, то тик и снимет.
    *
    * Наценка огня по нежити («Некромант», порог 10) — ровно одна. Против
    * огненного элементаля её уже кладёт [[bossDamageTaken]]: весь его урон и
    * есть огонь. Прочее горение (огненный порошок) берёт её здесь. */
  private def heroBurnDamage(hero: Hero, battle: SoloPveBattle, burn: Burn, nowMs: Long): Long = {
    val dealt = bossDamageTaken(hero, battle, burn.damageOn(hero.effectiveMaxHp(nowMs)))
    if (battle.boss.contains(MiniBoss.FireElemental)) dealt
    else (dealt * hero.sets.fireTakenMult).toLong
  }

  /** Сколько снимет с героя один тик яда — с поправкой «Крысы» (порог 10).
    * Его же показывает прогноз у HP героя (🟢-N). */
  private def heroPoisonDamage(hero: Hero, poison: Poison, nowMs: Long): Long =
    (poison.damageOn(hero.effectiveMaxHp(nowMs)) * hero.sets.heroDotTakenMult).toLong

  /** Сколько снимет с героя один тик кровотечения — с поправкой «Крысы»
    * (порог 10). Его же показывает прогноз у HP героя (🔴-N). */
  private def heroBleedDamage(hero: Hero, bleed: Bleed, nowMs: Long): Long =
    (bleed.damageOn(hero.effectiveMaxHp(nowMs)) * hero.sets.heroDotTakenMult).toLong

  /** Тик эффектов ГЕРОЯ в конце раунда: реген лечит на `pct`% макс.HP и слабеет.
    * Возвращает обновлённых героя и бой (с ослабленным регеном) плюс строку
    * эффекта (пустая, если регена нет). */
  private def tickHeroEffects(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long
  ): (Hero, SoloPveBattle, String) = {
    // «Некромант» (порог 10): герой сам мёртв — травить и обескровливать в нём
    // нечего. Яд и кровь гаснут на нём сразу, одной строкой на оба.
    val undead  = hero.sets.heroIsUndead
    val hadRot  = undead && (battle.effects.heroPoison.isDefined || battle.effects.heroBleed.isDefined)
    val battle0 = if (!hadRot) battle
                  else battle.copy(effects = battle.effects.copy(heroPoison = None, heroBleed = None))
    val rotLine = if (hadRot) content.text("battle.undeadImmune") else ""
    // Горение на герое (огненный элементаль) тикает до регена: оно усиливается
    // каждый раунд, как и горение на мобе. Мёртвая плоть занимается охотнее:
    // и урон огня по ней выше, и само пламя растёт быстрее.
    val (burnedHero, burnedBattle, burnLine) = battle0.effects.heroBurn match {
      case Some(burn) =>
        val dmg   = heroBurnDamage(hero, battle0, burn, nowMs)
        val grown = (1L to hero.sets.heroBurnGrowthMult.max(1L)).foldLeft(burn)((b, _) => b.grown)
        (hero.copy(fightStats = hero.fightStats.copy(hp = (hero.fightStats.hp - dmg).max(0L))),
         battle0.copy(effects = battle0.effects.copy(heroBurn = Some(grown))),
         heroDotTick(Burn.Icon, Burn.Label, dmg))
      case None => (hero, battle0, "")
    }
    // Яд на герое: снимает % макс.HP мимо брони и слабеет, как яд на мобе.
    // Гниль Джо тикает той же строкой, что любой другой яд: своё у минибосса —
    // само отравление («Ядовитые зловония…»), а не его последствия.
    // «Крыса» (порог 10): зараза герою не новость — яд и кровь снимают с него
    // вдвое меньше.
    val (poisonedHero, poisonedBattle, poisonLine) = burnedBattle.effects.heroPoison match {
      case Some(poison) =>
        val dmg = heroPoisonDamage(burnedHero, poison, nowMs)
        (burnedHero.copy(fightStats = burnedHero.fightStats.copy(
           hp = (burnedHero.fightStats.hp - dmg).max(0L))),
         burnedBattle.copy(effects = burnedBattle.effects.copy(heroPoison = poison.decayed)),
         heroDotTick(Poison.Icon, Poison.Label, dmg))
      case None => (burnedHero, burnedBattle, "")
    }
    // Кровотечение на герое (пасть и когти волка): % макс.HP мимо брони. Само
    // собой оно не затухает — снимает его только лечение (см. activeHeal); с
    // «Крысой» (порог 10) рана затягивается, как слабеет яд.
    val (bledHero, bledBattle, bleedLine) = poisonedBattle.effects.heroBleed match {
      case Some(bleed) =>
        val dmg   = heroBleedDamage(poisonedHero, bleed, nowMs)
        val decay = hero.sets.heroBleedDecayPerRound
        val left  = if (decay <= 0) Some(bleed) else Bleed.of(bleed.pct - decay)
        (poisonedHero.copy(fightStats = poisonedHero.fightStats.copy(
           hp = (poisonedHero.fightStats.hp - dmg).max(0L))),
         poisonedBattle.copy(effects = poisonedBattle.effects.copy(heroBleed = left)),
         heroDotTick(Bleed.Icon, Bleed.Label, dmg))
      case None => (poisonedHero, poisonedBattle, "")
    }
    val (finalHero, finalBattle, regenLine) = bledBattle.effects.heroRegen match {
      // Проклятого Некромантом реген не лечит, но срок его идёт (см. HealCurse);
      // строки «+0» не пишем — значок проклятия у HP и так виден.
      case Some(regen) if bledBattle.effects.heroCursed =>
        (bledHero, bledBattle.copy(effects = bledBattle.effects.copy(heroRegen = regen.decayed)), "")
      case Some(regen) =>
        val maxHp  = bledHero.effectiveMaxHp(nowMs)
        // Регенерация горением НЕ режется: это не действие игрока, а эффект,
        // который тикает сам по себе (см. activeHeal).
        val heal   = regen.healOn(maxHp)
        val newHp  = (bledHero.fightStats.hp + heal).min(maxHp)
        val healed = newHp - bledHero.fightStats.hp
        (
          bledHero.copy(fightStats = bledHero.fightStats.copy(hp = newHp)),
          bledBattle.copy(effects = bledBattle.effects.copy(heroRegen = regen.decayed)),
          content.format("battle.regenTick", "healed" -> healed.toString)
        )
      case None => (bledHero, bledBattle, "")
    }
    // Строки бывают непустыми одновременно (горим, травимся и регенерируем) — склеиваем.
    (finalHero, finalBattle,
      List(rotLine, burnLine, poisonLine, bleedLine, regenLine).filter(_.nonEmpty).mkString("\n"))
  }

  /** Тик DoT-эффектов МОНСТРА в конце раунда (каждый снимает `pct`% макс.HP мимо
    * брони). Порядок: яд → кровотечение → горение. Яд слабеет ([[Poison.decayed]]),
    * кровотечение НЕ затухает, горение усиливается ([[Burn.grown]]). Возвращает
    * обновлённый бой и склеенную строку эффектов (пустую, если DoT нет).
    *
    * Минибосса раны ранят вдвое слабее — это урон от процентов (см.
    * `BossPercentDamage`) — и не больше остатка его предела за раунд (см.
    * `BossRoundCap`): целиком поглощённый тик молчит, а дошедший до предела
    * договаривает об этом строкой. */
  private def tickMonsterEffects(
      battle: SoloPveBattle,
      monsterName: String,
      sets: HeroSets
  ): (SoloPveBattle, String, Long) = {
    val maxHp = battle.monsterStats.hp
    var eff   = battle.effects
    var cur   = battle // HP и счёт предела минибосса — через hurtActive
    var lines = Vector.empty[String]
    var held  = Vector.empty[String]
    var bled  = 0L // урон кровотечения за этот тик — «Упырь» (порог 10) лечит им героя

    // Рана снимает HP — долю максимума, поэтому с минибосса вдвое меньше;
    // возвращает, сколько сняла на деле.
    def tick(icon: String, label: String, dmg: Long): Long = {
      val hurt = cur.hurtActive(0L, battle.pctDamage(dmg))
      cur = hurt.battle
      if (!hurt.absorbed) lines = lines :+ dotTick(icon, label, hurt.hp, monsterName)
      held = held ++ capLine(hurt)
      hurt.hp
    }

    // Поднятый мёртв: травить и обескровливать в нём нечего, и зараза с него
    // сходит сама — ровно как с героя-нежити. Зато сухая плоть занимается
    // охотнее: огонь по ней весомее и растёт быстрее.
    val undead = battle.monsterUndead
    if (undead && (eff.monsterPoison.isDefined || eff.monsterBleed.isDefined)) {
      eff   = eff.copy(monsterPoison = None, monsterBleed = None)
      lines = lines :+ content.format("battle.monsterUndeadImmune", "monster" -> monsterName)
    }

    eff.monsterPoison.foreach { p =>
      tick(Poison.Icon, Poison.Label, monsterDotDamage(battle, p.damageOn(maxHp)))
      eff = eff.copy(monsterPoison = p.decayed)
    }
    eff.monsterBleed.foreach { b =>
      // Кровотечение не затухает — сила остаётся прежней.
      bled += tick(Bleed.Icon, Bleed.Label, monsterDotDamage(battle, b.damageOn(maxHp)))
    }
    eff.monsterBurn.foreach { bn =>
      val fire = if (undead) ItemSet.Necromancer.UndeadFireTakenPct else 0L
      tick(Burn.Icon, Burn.Label, bn.damageOn(maxHp) * (100L + fire) / 100L)
      // «Дикое пламя» (порог 6) ускоряет рост: шаг применяется несколько раз,
      // сохраняя саму кривую роста (+2 п.п. до порога, дальше +1). На поднятом
      // пламя растёт вдвое охотнее — тем же порядком.
      val steps = sets.burnGrowthMult.max(1L) * (if (undead) ItemSet.Necromancer.UndeadBurnGrowthMult else 1L)
      eff = eff.copy(monsterBurn = Some((1L to steps).foldLeft(bn)((b, _) => b.grown)))
    }
    val after = cur.copy(effects = eff)
    // Истёк ранами — в счёт тому из отряда, кто бил его последним: пламя,
    // яд и кровь от его ударов и добили.
    val credited =
      if (battle.monsterCurrentHp > 0L && after.monsterCurrentHp <= 0L) creditKill(after, after.monsterLastAllyHit)
      else after
    (credited, (lines ++ held).mkString("\n"), bled)
  }

  /** Добитый засчитывается союзнику по имени `by` — стоит он ещё или уже выбыл
    * (по нему сводка доигранного боя, см. `BattleAlly.kills`). */
  private def creditKill(battle: SoloPveBattle, by: Option[String]): SoloPveBattle =
    by.fold(battle) { name =>
      val g = battle.group
      g.allies.find(_.name == name) match {
        case Some(a) => battle.copy(group = g.updateAlly(a.position)(x => x.copy(kills = x.kills + 1)))
        case None =>
          val i = g.alliesGone.lastIndexWhere(_.name == name)
          if (i < 0) battle
          else battle.copy(group = g.copy(alliesGone =
                 g.alliesGone.updated(i, g.alliesGone(i).copy(kills = g.alliesGone(i).kills + 1))))
      }
    }

  // ── Ход игрока: активный навык ──────────────────────────────────────────────

  /** Кнопка умения. В группе умение с уроном достаёт и соседа героя (моб под
    * номером 2), поэтому сначала спрашиваем, в кого бить; лечение и броня — себе
    * или союзнику рядом, а рядом никого — себе, без вопросов. Неготовому умению
    * экран цели не нужен — обычный ход сам скажет, почему нельзя. */
  private def skillRoute(user: User, itemId: Long, target: Option[Int], renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      state  <-
        if (target.isEmpty && needsTarget(hero, battle, itemId))
          renderer.show(user, targetScreen(hero, battle, itemId)).as(StateType.Battle)
        else resolveLoaded(user, renderer, hero, battle, now)(
          skillTurn(itemId, target
            .orElse(battle.slotByItem(itemId).filterNot(s => BattleState.supports(s.skill.effect))
              .flatMap(s => skillAims(hero, battle, s.skill).headOption))
            .getOrElse(battle.group.heroPos)))
    } yield state

  /** Спрашивать ли цель: умение готово, и выбрать есть из чего — бьёт, а рядом
    * больше одного врага (у Тарана — врага или свободного места), или лечит, а
    * рядом стоит союзник. */
  private def needsTarget(hero: Hero, battle: SoloPveBattle, itemId: Long): Boolean =
    battle.slotByItem(itemId).exists { slot =>
      val ready = slot.cooldown <= 0 && hero.fightStats.energy >= slot.skill.energyCost(hero)
      // «Мощный выстрел» по кнопке только натягивают: цель выберет «Атака».
      ready && !BattleState.charges(slot.skill.effect) && battle.aimSlot.isEmpty && (
        (BattleState.dealsDamage(slot.skill.effect) && skillAims(hero, battle, slot.skill).size > 1) ||
        (BattleState.supports(slot.skill.effect) && supportAllies(battle).nonEmpty) ||
        (BattleState.rolls(slot.skill.effect) && skillAims(hero, battle, slot.skill).size > 1))
    }

  /** Экран выбора цели для базовой атаки: те же кнопки, что у умения с уроном. */
  private def attackTargetScreen(hero: Hero, battle: SoloPveBattle): Screen =
    targetChoices(battle, "Attack", content.text("battle.group.attackLabel"), support = false,
      heroTargets(hero, battle))

  /** Экран выбора цели: моб напротив и соседи по местам, с их здоровьем и
    * бронёй в процентах. Моб на кнопке — «Раса-ранг», именной — по имени (см.
    * `MonsterSlot.targetName`), союзник — по имени без фамилии (см.
    * `BattleAlly.callName`). */
  private def targetScreen(hero: Hero, battle: SoloPveBattle, itemId: Long): Screen = {
    val slot       = battle.slotByItem(itemId)
    val skillLabel = slot.map(_.skill.label).getOrElse("")
    val support    = slot.exists(s => BattleState.supports(s.skill.effect))
    slot.map(_.skill.effect) match {
      case Some(Skill.Effect.Roll(pct, maxPlaces, _)) => rollScreen(battle, itemId, skillLabel, pct, maxPlaces)
      case _ =>
        targetChoices(battle, s"Skill_$itemId", skillLabel, support,
          slot.fold(battle.group.attackTargets)(s => skillTargets(hero, battle, s.skill)),
          if (slot.exists(_.skill == Skill.Ram)) BattleState.ramSpots(battle) else Nil)
    }
  }

  /** Куда перекатиться: свободные места рядом, и у каждого — сколько
    * уклонения он даст. */
  private def rollScreen(battle: SoloPveBattle, itemId: Long, label: String, pct: Int, maxPlaces: Int): Screen = {
    val choices = BattleState.rollSpots(battle, maxPlaces).zipWithIndex.map { case (pos, i) =>
      val gain = pct.toLong * math.abs(pos - battle.group.heroPos)
      pangea.engine.Choice(s"Skill_$itemId",
        pangea.engine.Choice.fit(content.format("battle.roll.spot", "n" -> pos.toString, "pct" -> gain.toString)),
        data = Map("target" -> pos.toString), row = Some(i / BattleState.AlliesPerRow))
    }
    val cancel = pangea.engine.Choice("CancelTarget", content.text("battle.group.cancelTarget"),
      color = pangea.engine.ChoiceColor.Negative,
      row = Some((choices.size + BattleState.AlliesPerRow - 1) / BattleState.AlliesPerRow))
    Screen(content.format("battle.group.chooseSpot", "skill" -> label), choices :+ cancel)
  }

  /** По кому герой может ударить оружием: мечом — соседей, из лука — дальше
    * (см. [[BattleState.heroReach]]). */
  private def heroTargets(hero: Hero, battle: SoloPveBattle): List[Int] =
    battle.group.targetsWithin(BattleState.heroReach(hero))

  /** По кому достаёт умение (см. [[BattleState.skillReach]]). */
  private def skillTargets(hero: Hero, battle: SoloPveBattle, skill: Skill): List[Int] =
    battle.group.targetsWithin(BattleState.skillReach(hero, skill))

  /** Куда можно применить умение: по врагам в досягаемости, а Таран — ещё и на
    * свободное место рядом (см. [[BattleState.ramSpots]]); Перекат — только на
    * свободные места своего ряда (см. [[BattleState.rollSpots]]). */
  private def skillAims(hero: Hero, battle: SoloPveBattle, skill: Skill): List[Int] =
    if (skill == Skill.Ram) (skillTargets(hero, battle, skill) ++ BattleState.ramSpots(battle)).sorted
    else skill.effect match {
      case Skill.Effect.Roll(_, maxPlaces, _) => BattleState.rollSpots(battle, maxPlaces)
      case _                                  => skillTargets(hero, battle, skill)
    }

  /** До каких союзников достают лечение и броня: живые соседи героя по ряду
    * (см. [[BattleState.SupportReach]]) — так же, как удар по врагам. */
  private def supportAllies(battle: SoloPveBattle): List[BattleAlly] =
    battle.group.alliesWithin(BattleState.SupportReach)

  /** Кнопки целей под действие `choiceId`: лечение и броня — себе или
    * союзнику рядом (см. [[supportAllies]]); урон — `enemies`, моб в паре и все,
    * до кого достаёт оружие (см. [[heroTargets]]), а вперемешку с ними по
    * местам — свободные `spots`, куда можно рвануть Тараном. */
  private def targetChoices(battle: SoloPveBattle, choiceId: String, label: String, support: Boolean,
                            enemies: List[Int], spots: List[Int] = Nil): Screen = {
    val targets =
      if (support) {
        val me = pangea.engine.Choice(choiceId, content.text("battle.group.targetSelf"),
          data = Map("target" -> battle.group.heroPos.toString), row = Some(0))
        me :: supportAllies(battle).zipWithIndex.map { case (a, i) =>
          pangea.engine.Choice(choiceId,
            targetLabel("battle.group.targetAlly", a.position, a.callName, a.hpPct, a.armorPct),
            data = Map("target" -> a.position.toString), row = Some(1 + i / BattleState.AlliesPerRow))
        }
      } else
        (enemies ++ spots).sorted.zipWithIndex.flatMap { case (pos, i) =>
          val text = battle.monsterAt(pos) match {
            case Some(m) => Some(targetLabel("battle.group.targetLabel", pos, m.targetName, m.hpPct, m.armorPct))
            case None    => Option.when(spots.contains(pos))(content.format("battle.group.moveToFree", "n" -> pos.toString))
          }
          text.map(t => pangea.engine.Choice(
            id    = choiceId,
            label = pangea.engine.Choice.fit(t),
            data  = Map("target" -> pos.toString),
            row   = Some(i)))
        }
    val cancel = pangea.engine.Choice("CancelTarget", content.text("battle.group.cancelTarget"),
      color = pangea.engine.ChoiceColor.Negative, row = Some(targets.size))
    val title  = if (spots.isEmpty) "battle.group.chooseTarget" else "battle.group.chooseSpot"
    Screen(content.format(title, "skill" -> label), targets :+ cancel)
  }

  /** Подпись кнопки цели: место, имя, здоровье и броня. В предел ВК не
    * влезает — короче становится имя, а место и проценты остаются целыми. */
  private def targetLabel(key: String, pos: Int, name: String, hp: Long, armor: Long): String = {
    def label(n: String) = content.format(key, "n" -> pos.toString, "name" -> n,
      "hp" -> hp.toString, "armor" -> armor.toString)
    val over = label(name).length - pangea.engine.Choice.MaxLabelLength
    if (over <= 0) label(name)
    else pangea.engine.Choice.fit(label(name.take((name.length - over - 1).max(1)) + "…"))
  }

  /** Применение активного навыка. Проверки (слот существует / готов / хватает
    * энергии) дают Continue-сообщение без траты хода. Умение ВСЕГДА срабатывает:
    * списываем энергию (в героя, персист — в commit), применяем эффект с ±20%
    * разбросом, ставим cd и инкрементируем uses. `target` — место цели в строю:
    * место героя — моб напротив, соседнее — сосед. */
  private def skillTurn(itemId: Long, target: Int)(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    battle.slotByItem(itemId) match {
      case None =>
        cont(hero, battle, content.text("battle.skillUnavailable"))
      // Тетива натянута: кнопка «Мощного выстрела» — это «Тянуть дальше», а
      // другие умения рука, держащая тетиву, не возьмёт.
      case Some(slot) if slot.aim.isDefined =>
        holdAim(hero, battle, slot, nowMs)
      case Some(_) if battle.aimSlot.isDefined =>
        cont(hero, battle, content.text("battle.powerShot.handsBusy"))
      case Some(slot) if slot.cooldown > 0 =>
        cont(hero, battle, content.text("battle.skillOnCooldown"))
      case Some(slot) if hero.fightStats.energy < slot.skill.energyCost(hero) =>
        cont(
          hero,
          battle,
          content.format(
            "battle.notEnoughEnergy",
            "cost"   -> slot.skill.energyCost(hero).toString,
            "energy" -> hero.fightStats.energy.toString
          )
        )
      // Таран туда, где врага нет: рывок на свободное место без удара.
      // Пригвождённому рваться некуда — ход не тратится.
      case Some(slot) if slot.skill == Skill.Ram && !battle.group.hasMonster(target) =>
        if (battle.effects.heroRooted) cont(hero, battle, content.text(heldKey(battle, "battle.group.ramPinnedSpot")))
        else if (BattleState.ramSpots(battle).contains(target))
          (if (battle.effects.skillJammed) jammed(hero, battle, slot, nowMs) else ramDash(paid(hero, slot), battle, slot, target, nowMs))
        else if (skillTargets(hero, battle, slot.skill).isEmpty) cont(hero, battle, content.text("battle.group.noTarget"))
        else cont(hero, battle, content.text("battle.group.targetGone"))
      case Some(slot) if BattleState.needsEnemy(slot.skill.effect) && skillTargets(hero, battle, slot.skill).isEmpty =>
        cont(hero, battle, content.text("battle.group.noTarget"))
      // Лечение и броня — себе или союзнику рядом; до дальнего не дотянуться.
      case Some(slot) if BattleState.supports(slot.skill.effect) && target != battle.group.heroPos &&
                         !supportAllies(battle).exists(_.position == target) =>
        cont(hero, battle, content.text("battle.group.allyOutOfReach"))
      // Блок и Контрудар тратят бонусное действие раунда — то же, что пояс.
      case Some(slot) if BattleState.bonusAction(slot.skill.effect) && battle.consumableUsedThisRound =>
        cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
      // Яд красного сетеплёта: умение срывается — энергия ушла, умения нет, ход
      // потрачен. Перекат на чужое место — сперва отказ, как всегда.
      case Some(slot) if battle.effects.skillJammed &&
                         !(BattleState.rolls(slot.skill.effect) && !skillAims(hero, battle, slot.skill).contains(target)) =>
        jammed(hero, battle, slot, nowMs)
      case Some(slot) if BattleState.charges(slot.skill.effect) =>
        drawAim(paid(hero, slot), battle, slot, nowMs)
      // Перекат — только на свободное место рядом, пригвождённому — никуда;
      // отказ не стоит ни хода, ни энергии.
      case Some(slot) if BattleState.rolls(slot.skill.effect) && !skillAims(hero, battle, slot.skill).contains(target) =>
        cont(hero, battle, content.text(
          if (battle.effects.heroRooted) heldKey(battle, "battle.roll.pinned")
          else if (skillAims(hero, battle, slot.skill).isEmpty) "battle.roll.noRoom"
          else "battle.group.targetGone"))
      case Some(slot) if BattleState.rolls(slot.skill.effect) =>
        rollTurn(paid(hero, slot), battle, slot, target, nowMs)
      case Some(slot) =>
        skillHit(paid(hero, slot), battle, slot, nowMs, target)
    }

  private def skillJamLine(kind: Option[String]): String =
    if (SkillJam.isThunder(kind)) content.text("battle.thunder.skillJammed")
    else content.text("battle.skillJammed")

  private def allySkillJamLine(name: String, kind: Option[String]): String =
    if (SkillJam.isThunder(kind)) content.format("battle.thunder.skillJammedAlly", "name" -> name)
    else content.format("battle.ally.skillJammed", "name" -> name)

  /** Сорвалось следующее умение (яд красного сетеплёта или громовой раскат):
    * энергия ушла, умения нет, перезарядка не началась. Потрачено то, что взяло
    * бы само умение: обычное — ход (враги отвечают), Блок и Контрудар —
    * бонусное действие раунда, ход остаётся. Срыв один: дальше умения
    * работают. */
  private def jammed(hero: Hero, battle: SoloPveBattle, slot: SkillSlotState, nowMs: Long): Task[TurnResult] = {
    val cleared = battle.copy(effects = battle.effects.withoutSkillJam)
    val line    = Vector(skillJamLine(battle.effects.skillJamKind))
    if (BattleState.bonusAction(slot.skill.effect))
      ZIO.succeed(TurnResult(paid(hero, slot), cleared.copy(consumableUsedThisRound = true), line, Outcome.Continue,
        endsRound = false))
    else closeTurn(TurnResult(paid(hero, slot), cleared, line, Outcome.Continue), nowMs, Set.empty)
  }

  /** «Перекат»: герой встаёт на свободное место своего ряда и за каждое
    * пройденное место получает прибавку к уклонению на `rounds` раундов —
    * этот и следующие. Встал напротив врага — тот с ним в паре. Дальше —
    * базовая атака с нового места, как после прочих умений: в паре — по паре,
    * без пары — по ближайшему в досягаемости, а бить некого — ход кончается
    * тихо. */
  private def rollTurn(hero: Hero, battle: SoloPveBattle, slot: SkillSlotState, target: Int, nowMs: Long): Task[TurnResult] =
    slot.skill.effect match {
      case Skill.Effect.Roll(pct, _, rounds) =>
        val gain   = pct.toLong * math.abs(target - battle.group.heroPos)
        val bumped = battle.updateSlot(slot.itemId)(s => s.copy(cooldown = s.skill.cooldown, uses = s.uses + 1))
        val moved  =
          if (bumped.group.hasMonster(target)) bumped.moveHeroTo(target)
          else bumped.copy(group = bumped.group.copy(heroPos = target))
        // Бафы тикают в конце раунда, после ответа врагов: запас (rounds − 1)
        // застаёт ответ и в этом раунде, и в следующих.
        val dodged = moved.copy(heroBattleState = moved.heroBattleState.add(
          Buff(0L, 0L, 0L, dodgePct = gain, defencePct = 0L, turnsLeft = Some(rounds - 1))))
        val line   = slot.skill.hitTemplate.replaceFirst("\\{\\}", target.toString).replaceFirst("\\{\\}", gain.toString)
        val skip   = Set(slot.itemId)
        if (dodged.group.paired) playerStrike(hero, dodged, nowMs, Vector(line), skip)
        else followUp(hero, dodged, nowMs, Vector(line), skip)
      case _ => cont(hero, battle, content.text("battle.skillUnavailable"))
    }

  /** Герой, заплативший энергией за умение. */
  private def paid(hero: Hero, slot: SkillSlotState): Hero =
    hero.copy(fightStats = hero.fightStats.copy(energy = (hero.fightStats.energy - slot.skill.energyCost(hero)).max(0L)))

  /** «Мощный выстрел»: тетива натянута — урона и атаки нет, ход кончается,
    * враги отвечают. Энергия уже списана; перезарядка — после выстрела. */
  private def drawAim(hero: Hero, battle: SoloPveBattle, slot: SkillSlotState, nowMs: Long): Task[TurnResult] = {
    val drawn = battle.updateSlot(slot.itemId)(_.copy(aim = Some(0)))
    closeTurn(TurnResult(hero, drawn, Vector(content.text("battle.powerShot.draw")), Outcome.Continue),
      nowMs, Set(slot.itemId))
  }

  /** «Тянуть дальше»: выстрел сильнее ещё на долю умения, но и этот ход — без
    * урона. Дальше предела не тянется — остаётся спустить стрелу. Энергии
    * продление не просит. */
  private def holdAim(hero: Hero, battle: SoloPveBattle, slot: SkillSlotState, nowMs: Long): Task[TurnResult] =
    (slot.skill.effect, slot.aim) match {
      case _ if slot.cooldown > 0 => cont(hero, battle, content.text("battle.skillOnCooldown"))
      case (Skill.Effect.Charge(pct, max), Some(holds)) if holds < max =>
        val held = battle.updateSlot(slot.itemId)(_.copy(aim = Some(holds + 1)))
        val line = content.format("battle.powerShot.hold", "mult" -> BattleState.drawMult(pct, holds + 1))
        closeTurn(TurnResult(hero, held, Vector(line), Outcome.Continue), nowMs, Set(slot.itemId))
      case _ => cont(hero, battle, content.text("battle.powerShot.atLimit"))
    }

  /** Таран на свободное место: рывок без удара. Герой встаёт туда сразу —
    * союзник с того места уступает его и встаёт на прежнее место героя, — и ход
    * на этом кончается: ни урона, ни базовой атаки. Энергия уже списана, а
    * умение уходит на перезарядку, как после удара. */
  private def ramDash(hero: Hero, battle: SoloPveBattle, slot: SkillSlotState, target: Int, nowMs: Long): Task[TurnResult] = {
    val ally    = battle.group.allyAt(target).filter(_.alive)
    val bumped  = battle.updateSlot(slot.itemId)(s => s.copy(cooldown = s.skill.cooldown, uses = s.uses + 1))
    val moved   = BattleState.stepTo(bumped, target)
    val line    = ally match {
      case Some(a) => content.format("battle.group.ramDashSwap", "n" -> target.toString, "name" -> a.name)
      case None    => content.format("battle.group.ramDash", "n" -> target.toString)
    }
    closeTurn(TurnResult(hero, moved, Vector(line), Outcome.Continue), nowMs, Set(slot.itemId))
  }

  /** Применяет эффект навыка (матч по Skill.Effect). После урона/лечения идёт
    * базовая атака игрока (кроме случая, когда моб убит самим скиллом). Кулдаун
    * использованного слота не тикается в этом же ходу (`skip`). Изменения статов
    * героя (энергия/HP Кровавой жатвы/лечение) едут в `hero` результата и
    * персистятся в commit — здесь никаких записей в БД.
    *
    * Удар по соседу (место в радиусе, не своё) идёт через временный шаг героя к
    * нему: сосед оказывается напротив, получает урон и проки, и герой шагает
    * обратно — базовая атака и ответ мобов идут по-прежнему. Убитый умением
    * сосед уходит в павшие сразу, Таран по живому соседу помечает место, на
    * которое герой шагнёт в конце раунда. Умения, задевающие остальных в строю
    * (дуга Размашистого — соседей цели, Вихрь — соседей цели, к которой герой
    * шагнул, Веерный порез — всех в досягаемости героя, см. [[strikeOthers]]),
    * могут добить того, кто стоял в
    * полях до подмены; тогда возвращать некого, цель остаётся в полях, а пары у
    * героя больше нет. «Отбросить» меняет строй само (см. [[knockBack]]): в
    * полях после него стоит тот, кто вышел на место отброшенного. */
  private def skillHit(
      hero: Hero,
      battle0: SoloPveBattle,
      slot: SkillSlotState,
      nowMs: Long,
      target: Int
  ): Task[TurnResult] = {
    // «Отскок»: сперва прыжок, потом выстрел — уже с нового места. Пригвождённый
    // не прыгает — стреляет с места.
    val leapTo =
      if (slot.skill.effect != Skill.Effect.Leap || battle0.effects.heroRooted) None
      else battle0.group.leapSpot(target, BattleState.displacement(hero), BattleState.skillReach(hero, slot.skill))
    // «Вихрь клинка»: сперва шаг напротив цели — союзник с того места встаёт на
    // прежнее место героя, как при Таране, — потом вихрь уже с нового места:
    // цель в паре, её соседи в досягаемости. Пригвождённый не шагает — кружит
    // с места.
    val whirlTo     = if (slot.skill.effect != Skill.Effect.Whirl) None else BattleState.whirlSpot(battle0, target)
    val whirlAlly   = whirlTo.flatMap(battle0.group.allyAt(_).filter(_.alive))
    val battle      = leapTo.map(p => battle0.copy(group = battle0.group.copy(heroPos = p)))
                        .orElse(whirlTo.map(BattleState.stepTo(battle0, _)))
                        .getOrElse(battle0)
    val home        = battle.group.activePos
    val paired      = battle.group.paired
    val damageSkill = BattleState.dealsDamage(slot.skill.effect)
    // Цель не в полях (сосед в досягаемости, а из лука — и дальше) — разворачиваем
    // её туда на время.
    val aimSide = damageSkill && target != home &&
                    battle.group.inReachOf(target, BattleState.skillReach(hero, slot.skill))
    val aimed   = if (aimSide) battle.engage(target) else battle
    val skip    = Set(slot.itemId)
    // Таран по мобу не из пары помечает место, куда герой шагнёт в конце раунда, —
    // и тогда, когда удар его добил: шаг на опустевшее место тоже шаг.
    def ramMark(b: SoloPveBattle): SoloPveBattle =
      if (slot.skill == Skill.Ram && target != b.group.heroPos) b.copy(group = b.group.copy(pendingMove = Some(target))) else b
    // Что делать после урона. В паре: победа или базовая атака по паре (по
    // соседу — сперва вернуть пару в поля). Пары нет: базовая атака идёт по
    // той же цели, а у лечения — по единственному соседу, если он есть; ответа
    // пары нет — ход кончается тихо. `at` — где цель стоит сейчас:
    // «Отбрасывающий выстрел» уносит её с того места, куда целились.
    def nextAt(at: Int): (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult] = (h, b0, lines) => {
      val b    = if (damageSkill) b0.rememberTarget(at) else b0
      val dead = damageSkill && b.monsterCurrentHp <= 0
      // Отброшенного базовая атака достаёт в этом же раунде, как бы далеко он ни
      // улетел: выстрел и удар — одно движение. Дальше — по обычной дальности.
      if (!aimSide) {
        if (dead) ZIO.succeed(victoryByHero(h, ramMark(b), lines, nowMs, skip))
        else if (paired) playerStrike(h, b, nowMs, lines, skip)
        else if (damageSkill) playerStrike(h, ramMark(b), nowMs, lines, skip)
        else followUp(h, b, nowMs, lines, skip)
      } else {
        val back = b.engage(home)
        // Прежний активный пал от отголоска умения (дуга, вихрь, порез): его
        // место пусто, `engage` ничего не вернул — в полях так и стоит цель, и
        // пары у героя нет.
        val homeGone = back.group.activePos != home
        if (dead) {
          if (homeGone) ZIO.succeed(victoryByHero(h, ramMark(back), lines, nowMs, skip))
          else {
            val fallen = ramMark(back.sideFallen(back.group.idxOf(at)))
            val slain  = lines :+ content.format("battle.group.sideSlain", "monster" -> b.monsterName)
            if (paired) playerStrike(h, fallen, nowMs, slain, skip) else followUp(h, fallen, nowMs, slain, skip)
          }
        } else {
          val rammed = ramMark(back)
          if (paired && !homeGone) playerStrike(h, rammed, nowMs, lines, skip)
          else strikeSide(h, rammed, at, nowMs, lines, skip).flatMap(closeTurn(_, nowMs, skip))
        }
      }
    }
    val next = nextAt(target)
    // Лечение и броня, наведённые на живого союзника, достаются ему, а не герою.
    object OnAlly {
      def unapply(effect: Skill.Effect): Option[BattleAlly] = effect match {
        case Skill.Effect.Heal | Skill.Effect.RepairArmor | Skill.Effect.GuardRepair(_, _) if target != aimed.group.heroPos =>
          aimed.group.allyAt(target).filter(_.alive)
        case _ => None
      }
    }
    for {
      spread <- Random.nextLongBetween(80L, 121L)
      // Понимание руны множит число умения — и с вещи, и с клейма (см. Hero.runeMult).
      // Умение оружия из лука вблизи — на пятую часть слабее, как и удар; удары
      // телом и щитом лук не трогает.
      closeMod = if (Skill.armorSkills.contains(slot.skill)) 1.0 else BattleState.rangedCloseMod(hero, aimed)
      raw = (slot.skill.baseValue(hero, nowMs) * spread / 100.0 * hero.weaponDust.damageMult * hero.runeMult(slot.skill) *
               closeMod).toLong.max(1L)
      // Спущенная стрела «Мощного выстрела» тетиву отпускает.
      bumped = aimed.updateSlot(slot.itemId)(s => s.copy(cooldown = s.skill.cooldown, uses = s.uses + 1, aim = None))
      // Строка умения: «Умение» → цель: урон. Цель — тот, кого бьют (`aimed`).
      tmpl = slot.skill.hitTemplate.replace("{target}", aimed.monsterName)
      // Урон, срезанный защитой моба (для эффектов, которые «упираются» в защиту).
      // Дебаф Холода снижает итоговое %-снижение цели на monsterColdDefenceCut п.п.;
      // временный дебаф защиты (комбо) режет саму защиту (effectiveMonsterDefence).
      reduced = (raw * (1.0 - monsterDefenceCut(hero, aimed, effWithAir(hero, aimed, nowMs), nowMs))).toLong.max(1L)
      result <- slot.skill.effect match {
        case Skill.Effect.Damage(reducedByDefence) =>
          val value = if (reducedByDefence) reduced else raw
          dealSkillDamage(hero, bumped, value, dealt => tmpl.replace("{}", dealt.toString), next)

        case Skill.Effect.Sweep(pct) =>
          // По цели — как удар, срезанный защитой; её соседям по строю — доля
          // от того, что по ней прошло. У выстрела вместо дуги — осколки стрелы.
          val splashKey =
            if (Skill.rangedSkills.contains(slot.skill)) "battle.group.splitSplash" else "battle.group.sweepSplash"
          dealSkillDamage(hero, bumped, reduced, dealt => tmpl.replace("{}", dealt.toString), next,
            splash = (b, dealt) => strikeOthersProcs(hero, b, around(b.group.activePos)) { tmp =>
              plainHit(tmp, (dealt * pct / 100L).max(1L))(d =>
                content.format(splashKey, "monster" -> tmp.monsterName, "damage" -> d.toString))
            })

        case Skill.Effect.Whirl =>
          // Шаг к цели уже сделан (см. начало). По цели — как удар, срезанный
          // защитой; каждому другому в досягаемости героя — а это теперь её
          // соседи — тот же бросок, срезанный уже его защитой.
          val step = whirlTo match {
            case Some(p) =>
              whirlAlly.fold(content.format("battle.group.whirlStep", "n" -> p.toString))(a =>
                content.format("battle.group.whirlStepSwap", "n" -> p.toString, "name" -> a.name)) + "\n"
            case None if battle0.effects.heroRooted && target != battle0.group.heroPos =>
              content.text(heldKey(battle0, "battle.group.whirlPinned")) + "\n"
            case None => ""
          }
          dealSkillDamage(hero, bumped, reduced, dealt => step + tmpl.replace("{}", dealt.toString), next,
            splash = (b, _) => strikeOthersProcs(hero, b, around(b.group.heroPos)) { tmp =>
              val amount = (raw * (1.0 - monsterDefenceCut(hero, tmp, effWithAir(hero, tmp, nowMs), nowMs))).toLong.max(1L)
              plainHit(tmp, amount)(d =>
                content.format("battle.group.whirlHit", "monster" -> tmp.monsterName, "damage" -> d.toString))
            })

        case Skill.Effect.FanBleed(pct) =>
          // Цели — урон и кровь, как у «Кровотечения»; каждому другому в
          // досягаемости героя — тот же урон и та же кровь (иммунитеты — в withEffects).
          def bleed(b: SoloPveBattle): SoloPveBattle =
            b.withEffects(b.effects.copy(monsterBleed = Some(b.effects.monsterBleed.map(_.stackedWith(pct)).getOrElse(Bleed(pct)))))
          dealSkillDamage(hero, bleed(bumped), raw, dealt => tmpl.replace("{}", dealt.toString), next,
            splash = (b, _) => strikeOthersProcs(hero, b, around(b.group.heroPos)) { tmp =>
              val (cut, line) = plainHit(tmp, raw)(d =>
                content.format("battle.group.fanCut", "monster" -> tmp.monsterName, "damage" -> d.toString, "pct" -> pct.toString))
              (bleed(cut), line)
            })

        case Skill.Effect.WarCry(maxPct, turns) =>
          // Урона нет: все мобы в досягаемости героя — и в полях, и в строю —
          // теряют pct% защиты на `turns` ходов и пропускают ближайшее умение.
          val pct = raw.min(maxPct.toLong).max(1L).toInt
          def cowed(b: SoloPveBattle): SoloPveBattle = b.withEffects(b.effects.copy(
            monsterDefenceDebuff     = Some(TimedDefenceDebuff(pct, turns)),
            monsterSkillBlockedTurns = BattleState.WarCrySkillBlockTurns))
          val active  = if (bumped.group.attackTargets.contains(bumped.group.activePos)) cowed(bumped) else bumped
          val (shouted, _) = strikeOthers(active, around(active.group.heroPos))(tmp => (cowed(tmp), ""))
          next(hero, shouted, Vector(tmpl.replace("{}", pct.toString)))

        case Skill.Effect.Knockback =>
          // Урон без защиты; выживший отлетает в конец строя, а последний
          // выходит на его место — базовая атака и ответ идут уже по нему.
          // Сооружение с места не сдвинуть; минибосса — можно.
          val line: Long => String = dealt => tmpl.replace("{}", dealt.toString)
          val thrown: (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult] = (h, b, lines) =>
            if (b.monsterCurrentHp <= 0) next(h, b, lines)
            else if (BattleState.isTower(b))
              next(h, b, lines :+ content.format("battle.group.unshovable", "monster" -> b.monsterName))
            else knockBack(b) match {
              case None                    => next(h, b, lines)
              case Some((moved, newcomer)) =>
                next(h, moved, lines :+ content.format("battle.group.knockedBack", "monster" -> b.monsterName, "next" -> newcomer))
            }
          dealSkillDamage(hero, bumped, raw, line, thrown)

        case Skill.Effect.Pin(turns) =>
          // Урон, срезанный защитой, — и цель не сходит с места.
          dealSkillDamage(hero, rooted(bumped, turns), reduced, dealt => tmpl.replace("{}", dealt.toString), next)

        // Перекат урона не наносит и ведётся своим ходом (см. skillTurn).
        case Skill.Effect.Roll(_, _, _) =>
          rollTurn(hero, battle0, slot, target, nowMs)

        case Skill.Effect.Charge(pct, _) =>
          // «Атака» спустила натянутую тетиву: урон, срезанный защитой, и
          // каждое «Тянуть дальше» прибавляет к нему `pct`%. Первый {} — во
          // сколько раз сильнее, второй — урон.
          val holds = slot.aim.getOrElse(0)
          val drawn = (reduced * (100L + pct.toLong * holds) / 100L).max(1L)
          val mult  = java.util.regex.Matcher.quoteReplacement(BattleState.drawMult(pct, holds))
          dealSkillDamage(hero, bumped, drawn,
            dealt => tmpl.replaceFirst("\\{\\}", mult).replaceFirst("\\{\\}", dealt.toString), next)

        case Skill.Effect.Volley(turns) =>
          // По цели — как удар, срезанный защитой; каждому другому в пределах
          // дальности оружия — тот же бросок, срезанный уже его защитой. Все
          // задетые раунд не шагают.
          val reach = BattleState.skillReach(hero, slot.skill)
          dealSkillDamage(hero, rooted(bumped, turns), reduced, dealt => tmpl.replace("{}", dealt.toString), next,
            splash = (b, _) => strikeOthersProcs(hero, b, (b.group.heroPos - reach) to (b.group.heroPos + reach)) { tmp =>
              val amount = (raw * (1.0 - monsterDefenceCut(hero, tmp, effWithAir(hero, tmp, nowMs), nowMs))).toLong.max(1L)
              val (cut, line) = plainHit(tmp, amount)(d =>
                content.format("battle.group.barrageHit", "monster" -> tmp.monsterName, "damage" -> d.toString))
              (rooted(cut, turns), line)
            })

        case Skill.Effect.Leap =>
          // Прыжок уже сделан (см. начало): выстрел — как удар, срезанный защитой.
          val jump = leapTo.fold(content.text(
              if (battle0.effects.heroRooted) heldKey(battle0, "battle.group.leapPinned") else "battle.group.leapNoRoom"))(p =>
              content.format("battle.group.leap", "n" -> p.toString)) + "\n"
          dealSkillDamage(hero, bumped, reduced, dealt => jump + tmpl.replace("{}", dealt.toString), next)

        case Skill.Effect.Push =>
          // Урон, срезанный защитой; выживший отлетает от героя, и пара рвётся.
          // Минибосса и сооружение не сдвинуть.
          val pushed: (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult] = (h, b, lines) =>
            if (b.monsterCurrentHp <= 0) next(h, b, lines)
            else b.pushActive(BattleState.displacement(hero)) match {
              case Some((moved, to)) =>
                nextAt(to)(h, moved, lines :+ content.format("battle.group.pushed", "monster" -> b.monsterName, "n" -> to.toString))
              case None if b.bossKind.isDefined || Race.immovable(Race.withName(b.monsterRace)) =>
                next(h, b, lines :+ content.format("battle.group.unpushable", "monster" -> b.monsterName))
              case None => next(h, b, lines)
            }
          dealSkillDamage(hero, bumped, reduced, dealt => tmpl.replace("{}", dealt.toString), pushed)

        case Skill.Effect.BleedDamage(pct) =>
          // Урон сразу + наложение (стак) КРОВОТЕЧЕНИЯ на моба (отдельно от яда).
          val stacked  = bumped.effects.monsterBleed.map(_.stackedWith(pct)).getOrElse(Bleed(pct))
          val bled     = bumped.withEffects(bumped.effects.copy(monsterBleed = Some(stacked)))
          // Отдельное сообщение «истекает кровью» убрано — прок уже виден по
          // компактному индикатору (🔴 -N ❤), приписанному к строке атаки.
          dealSkillDamage(hero, bled, raw, dealt => tmpl.replace("{}", dealt.toString), next)

        case Skill.Effect.WeakSpotStrike =>
          for {
            roll <- Random.nextIntBetween(1, 101)
            // «Охотник» (порог 12) прибавляется к своему шансу умения, а не
            // множит урон второй раз: четверного удара больше нет.
            chance = (2L * hero.effectiveBaseStats(nowMs).int - aimed.monsterLvl + hero.sets.skillCritBonusPct)
                       .max(0L).min(95L)
            doubled = roll <= chance
            value   = if (doubled) raw * 2L else raw
            line: (Long => String) = (dealt: Long) =>
              tmpl.replace("{}", dealt.toString) +
                (if (doubled) "\n" + content.text("battle.weakSpotDouble") else "")
            r <- dealSkillDamage(hero, bumped, value, line, next, critRolled = true)
          } yield r

        case Skill.Effect.BloodHarvest =>
          // Урон (доля тек.HP уже в baseValue, см. Skill.BloodHarvestHpToDamagePct)
          // ценой 10% текущего HP.
          // Раненый герой едет дальше в цепочке и персистится в commit ЕДИНООБРАЗНО
          // во всех ветках победы — HP-цена не теряется при добивании базовой атакой.
          val hpLost      = (hero.fightStats.hp * Skill.BloodHarvestHpCostPct / 100L).max(0L)
          val woundedHero = hero.copy(fightStats = hero.fightStats.copy(hp = (hero.fightStats.hp - hpLost).max(1L)))
          // Первый {} — урон по цели (пишем фактический), второй — цена крови героя.
          val line: Long => String = dealt =>
            tmpl.replaceFirst("\\{\\}", dealt.toString).replaceFirst("\\{\\}", hpLost.toString)
          dealSkillDamage(woundedHero, bumped, raw, line, next)

        // Лечение и броня по союзнику: то же число, но в его потолки; «Заслон»
        // союзнику даёт только броню — бафа защиты у него нет. С ранами
        // лечение обходится так же, как с ранами героя (см. activeHeal): кровь
        // снимает, яд ослабляет, пламя тратит собой.
        case OnAlly(ally) =>
          val heals = slot.skill.effect == Skill.Effect.Heal
          val full  = if (heals) (raw * hero.passives.healMult).toLong.max(1L) else raw
          val (updated, gained, wounds) =
            if (heals) {
              // Проклятого Некромантом лечение не лечит: умение тратится и с
              // ранами делает то же, только HP не прибавляет (см. HealCurse).
              val w  = HealedWounds.of(full, ally.poison, ally.burn)
              val nh = if (ally.healBlocked) ally.hp else (ally.hp + w.amount).min(ally.stats.hp)
              val cursedLine =
                if (ally.healBlocked) Vector(content.format("battle.necro.allyHealCursed", "name" -> ally.name))
                else Vector.empty[String]
              (ally.copy(hp = nh, poison = w.poison, bleed = None, burn = w.burn),
               nh - ally.hp, cursedLine ++ allyWoundLines(ally, w))
            } else {
              val na = (ally.armor + full).min(ally.stats.armor)
              (ally.copy(armor = na), na - ally.armor, Vector.empty[String])
            }
          val line = content.format("battle.group.supportAlly", "name" -> ally.name,
            "skill" -> slot.skill.label, "amount" -> gained.toString, "what" -> (if (heals) "HP" else "брони"))
          next(hero, bumped.copy(group = bumped.group.updateAlly(ally.position)(_ => updated)), line +: wounds)

        case Skill.Effect.Heal =>
          // «Целитель» множит активное лечение на 1.1, дальше — общий расчёт.
          val full = (raw * hero.passives.healMult).toLong.max(1L)
          val (newHp, healed, healedBattle, cutLine) = activeHeal(hero, bumped, full, nowMs)
          val healedHero = hero.copy(fightStats = hero.fightStats.copy(hp = newHp))
          val lines      = Vector(tmpl.replace("{}", healed.toString)) ++ cutLine
          next(healedHero, healedBattle, lines)

        case Skill.Effect.RepairArmor =>
          val (newStats, gained) = repairArmor(hero, raw, nowMs)
          next(hero.copy(fightStats = newStats), bumped, Vector(tmpl.replace("{}", gained.toString)))

        case Skill.Effect.GuardRepair(defencePct, turns) =>
          // Восстановление брони + временный %-баф итоговой защиты.
          val (newStats, gained) = repairArmor(hero, raw, nowMs)
          val guarded = bumped.copy(heroBattleState = bumped.heroBattleState.add(
            Buff(0L, 0L, 0L, dodgePct = 0L, defencePct = defencePct, turnsLeft = Some(turns))))
          next(hero.copy(fightStats = newStats), guarded, Vector(tmpl.replace("{}", gained.toString)))

        // Щит поднят (сколько снимет — по редкости лучшего в руках) или кинжал
        // наизготовку — до конца раунда. Это бонусное действие, как глоток с
        // пояса: ход не кончается, можно ударить или взять умение, а «Быстрые
        // руки» могут его и не потратить. Перезарядка в конце этого раунда не
        // тикает (bonusSlots).
        case Skill.Effect.Block | Skill.Effect.Counter =>
          quickHandsRoll(hero).map { case (consumed, qhLog) =>
            val pct    = hero.equipment.blockPct
            val raised =
              if (slot.skill.effect == Skill.Effect.Block) bumped.copy(heroBlockPct = pct)
              else bumped.copy(heroCounterReady = true)
            TurnResult(hero,
              raised.copy(consumableUsedThisRound = consumed, bonusSlots = raised.bonusSlots :+ slot.itemId),
              Vector(tmpl.replace("{}", pct.toString)) ++ qhLog, Outcome.Continue, endsRound = false)
          }
      }
    } yield result
  }

  /** Наносит `value` урона мобу в паре (сначала броня, затем HP) и отдаёт ход
    * дальше в `next`: там решается, победа это, базовая атака или возврат
    * временной пары (см. [[skillHit]]). Никаких записей в БД: итог (в т.ч.
    * изменённые статы героя) уезжает в TurnResult и персистится в commit. */
  /** `line` — шаблон строки умения: получает урон, который РЕАЛЬНО прошёл по
    * цели (после сопротивления минибосса и граней стихий), а не заявленный. */
  private def dealSkillDamage(
      hero: Hero,
      battle: SoloPveBattle,
      value: Long,
      line: Long => String,
      next: (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult],
      // Что тот же удар делает с остальным строем (Размашистый, Вихрь, Веерный
      // порез): получает бой после удара по цели и урон, который по ней прошёл
      // бы без предела минибосса (см. BossRoundCap) — после сопротивления и
      // граней стихий.
      splash: (SoloPveBattle, Long) => Task[(SoloPveBattle, Vector[String])] = (b, _) => ZIO.succeed((b, Vector.empty)),
      // Крит по этому умению уже разыгран снаружи («Удар в слабое место» катает
      // свой шанс сам) — второй раз не катаем.
      critRolled: Boolean = false
  ): Task[TurnResult] =
    // «Охотник» (порог 12): любое атакующее умение с шансом бьёт вдвое.
    chanceRoll(!critRolled && hero.sets.skillCritBonusPct > 0L, hero.sets.skillCritBonusPct).flatMap { crit =>
      dealSkillDamageCritted(hero, battle, value, line, next, splash, crit)
    }

  private def dealSkillDamageCritted(
      hero: Hero,
      battle: SoloPveBattle,
      value: Long,
      line: Long => String,
      next: (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult],
      splash: (SoloPveBattle, Long) => Task[(SoloPveBattle, Vector[String])],
      crit: Boolean
  ): Task[TurnResult] = {
    // Стена трофеев: прибавка к урону умения по расе цели. Здесь, а не в числе
    // умения, — то же число лечит и чинит броню, а их стена не трогает. Так же и
    // горящая цель («Преисподняя», порог 6).
    val walled = (value.max(1L) * battle.trophyMult * burningMult(hero, battle)).toLong.max(1L)
    val raw = if (crit) walled * 2L else walled
    // Урон навыка тоже несёт стихию оружия — раздельный урон по броне/HP с тем же
    // усилением. Проки стихий роллятся отдельным броском (как и на обычной атаке).
    val resisted          = (raw * bossResistance(hero, battle)).toLong.max(1L)
    val (armorRaw, hpRaw) = splitElementalDamage(battle.monsterCurrentArmor, resisted, hero)
    // Предел минибосса за раунд — общий на всё, что по нему прошло: удар,
    // умения, отряд, раны (см. BossRoundCap).
    val hurt              = battle.hurtActive(armorRaw, hpRaw)
    val critLine          = if (crit) "\n" + content.text("battle.skillCrit") else ""
    val skillLine         = line(hurt.dealt) + critLine + capLine(hurt).map("\n" + _).getOrElse("")
    val newHp    = hurt.battle.monsterCurrentHp
    // «Упырь» (порог 12): любое умение, нанёсшее урон, всегда пускает цели кровь.
    val bledEffects0 =
      if (!hero.sets.skillsAlwaysBleed || newHp <= 0) battle.effects
      else battle.effects.copy(monsterBleed = Some(
        battle.effects.monsterBleed.map(_.stackedWith(hero.sets.bleedPct)).getOrElse(Bleed(hero.sets.bleedPct))))
    // «Дикое пламя» (порог 12): умение, нанёсшее урон, всегда поджигает — уже
    // горящая цель разгорается сильнее, как от обычного прока Огня. Но только
    // если в оружии есть Огонь: без рубина поджигать нечем.
    val bledEffects =
      if (!hero.sets.skillsAlwaysIgnite || !hero.gems.hasElement(Element.Fire) || newHp <= 0) bledEffects0
      else bledEffects0.copy(monsterBurn = Some(
        bledEffects0.monsterBurn.map(_.reignited).getOrElse(Burn.onIgnite)))
    val effects  = bledEffects
    val hit      = hurt.battle.withEffects(effects)
    // Отголосок по строю — от силы самого удара: предел минибосса держит только
    // его самого и соседей не бережёт.
    val struck   = armorRaw + hpRaw
    if (newHp <= 0)
      splash(hit, struck).flatMap { case (swept, splashLog) =>
        next(hero, swept, Vector(skillLine) ++ splashLog)
      }
    else
      resolveElementProcs(hero, hit, hurt.hp, skill = true, hurt.dealt).flatMap {
        case (afterProcs, elemLog, fired) =>
          ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS)).flatMap { now =>
            // «Стужа» (порог 6): прок холода возвращает герою часть брони.
            val (iced, iceLog) = frostArmor(hero, fired, now)
            splash(afterProcs, struck).flatMap { case (swept, splashLog) =>
              next(iced, swept, Vector(skillLine) ++ elemLog ++ iceLog ++ splashLog)
            }
          }
      }
  }

  /** Места в досягаемости от `pos`: оно само и соседние (см. `GroupState.Reach`). */
  private def around(pos: Int): Range = (pos - GroupState.Reach) to (pos + GroupState.Reach)

  /** Умение задевает и остальных в строю: каждый живой моб на местах `positions`
    * (кроме того, что в полях) разворачивается во временную пару, `hit` бьёт его
    * там — урон и эффекты ложатся с иммунитетами через `withEffects` — и он
    * сворачивается обратно; добитый уходит в павшие, а Таран в него сгорает
    * (см. `GroupState.withoutSlot`). Пустая строка от `hit` в лог не идёт.
    * Бросков нет — чистая функция. */
  private def strikeOthers(battle: SoloPveBattle, positions: Seq[Int])(
      hit: SoloPveBattle => (SoloPveBattle, String)
  ): (SoloPveBattle, Vector[String]) = {
    val near = positions.toList.distinct.sorted
      .filter(pos => pos != battle.group.activePos && battle.monsterAt(pos).exists(_.alive))
    near.foldLeft((battle, Vector.empty[String])) { case ((b, log), pos) =>
      val idx         = b.group.idxOf(pos)
      val (tmp, line) = hit(b.withActive(b.group.others(idx)))
      // Задетым мог оказаться минибосс: его счёт предела вернётся в строй вместе
      // со слотом.
      val placed      = b.copy(group = b.group.copy(others = b.group.others.updated(idx, tmp.activeSlot)))
      val lines       = if (line.isEmpty) log else log :+ line
      if (tmp.monsterCurrentHp > 0L) (placed, lines)
      else (placed.sideFallen(idx), lines :+ content.format("battle.group.sideSlain", "monster" -> tmp.monsterName))
    }
  }

  /** То же, что [[strikeOthers]], но задетого ещё и обжигают стихии оружия:
    * выжил после удара — по нему бросаются проки, как по цели умения (по 30% на
    * стихию, с бонусами наборов; комбо — не больше одного за ход героя, см.
    * `SoloPveBattle.comboUsed`). Добить его может и комбо — тогда он уходит в
    * павшие. Геройская половина эффектов (бодрость от Воздуха), заряды босса и
    * отметка о комбо возвращаются в бой — они не на слоте моба. */
  private def strikeOthersProcs(hero: Hero, battle: SoloPveBattle, positions: Seq[Int])(
      hit: SoloPveBattle => (SoloPveBattle, String)
  ): Task[(SoloPveBattle, Vector[String])] = {
    val near = positions.toList.distinct.sorted
      .filter(pos => pos != battle.group.activePos && battle.monsterAt(pos).exists(_.alive))
    ZIO.foldLeft(near)((battle, Vector.empty[String])) { case ((b, log), pos) =>
      val idx = b.group.idxOf(pos)
      b.group.others.lift(idx) match {
        case None => ZIO.succeed((b, log))
        case Some(slot) =>
          val (struck, line) = hit(b.withActive(slot))
          // Отголосок — урон умения: молния выжигает задетому 20% того, что
          // по нему прошло в HP. Цепная молния с отголоска не перескакивает, и
          // броню «Стужа» с него не возвращает — это дело удара по самой цели.
          val procs =
            if (struck.monsterCurrentHp > 0L)
              resolveElementProcs(hero, struck, slot.currentHp - struck.monsterCurrentHp, skill = true)
                .map { case (t, l, _) => (t, l) }
            else ZIO.succeed((struck, Vector.empty[String]))
          procs.map { case (tmp, procLog) =>
            // Задетым мог оказаться минибосс, стоящий в строю: его счёт предела
            // и заряды (холод гасит сферу) возвращаются в строй вместе со слотом.
            val placed = b.copy(
              group       = b.group.copy(others = b.group.others.updated(idx, tmp.activeSlot)),
              effects     = tmp.effects.withMonsterPart(b.effects),
              comboUsed   = tmp.comboUsed)
            val lines = (if (line.isEmpty) log else log :+ line) ++ procLog
            if (tmp.monsterCurrentHp > 0L) (placed, lines)
            else (placed.sideFallen(idx), lines :+ content.format("battle.group.sideSlain", "monster" -> tmp.monsterName))
          }
      }
    }
  }

  /** Урон-«отголосок» умения по мобу во временной паре: броня, потом HP — без
    * защиты и стихий (это отголосок удара, а не удар). Проки стихий по задетому
    * бросает [[strikeOthersProcs]]. Минибосс в строю теряет от него не больше
    * остатка своего предела за раунд. Возвращает бой и строку — `line` получает
    * урон, который дошёл на деле, — с припиской о пределе, если он встал; съел
    * предел отголосок целиком — вместо «0 урона» строка о самом пределе. */
  private def plainHit(tmp: SoloPveBattle, amount: Long)(line: Long => String): (SoloPveBattle, String) = {
    val armorDmg = math.min(tmp.monsterCurrentArmor, amount)
    val hurt     = tmp.hurtActive(armorDmg, amount - armorDmg)
    (hurt.battle,
     if (hurt.absorbed) capText(hurt.battle) else line(hurt.dealt) + capLine(hurt).map("\n" + _).getOrElse(""))
  }

  /** Моб в полях не сходит с места `turns` раундов. Счётчик ставится с запасом
    * в один тик: он тикает в начале хода моба, а шаги — в конце раунда (см.
    * `BattleEffects.monsterRootedTurns`). Новое не укорачивает уже наложенное. */
  private def rooted(b: SoloPveBattle, turns: Int): SoloPveBattle =
    b.withEffects(b.effects.copy(monsterRootedTurns = b.effects.monsterRootedTurns.max(turns + 1)))

  /** «Отбросить»: активный моб отлетает на последнее занятое место строя, а тот,
    * кто там стоял, выходит на его место — и в поля. Сооружение в хвосте (башня
    * каравана) с места не сходит: его место пропускаем, и цель меняется с
    * последним из тех, кто не сооружение. Позади одни сооружения, или активный
    * и так последний (или один) — None. Возвращает бой и имя вышедшего. */
  private def knockBack(battle: SoloPveBattle): Option[(SoloPveBattle, String)] = {
    val g     = battle.group
    val swaps = g.places.zip(g.others).collect { case (p, s) if !BattleState.isTowerSlot(s) => p }
    val last  = (g.activePos :: swaps).max
    if (last == g.activePos) None
    else {
      val idx   = g.idxOf(last)
      val from  = g.activePos
      val moved = battle.copy(group = g.copy(places = g.places.updated(idx, from), activePos = last))
      Some((moved.engage(from), g.others(idx).name))
    }
  }

  /** Восстановление брони героя на `value` (кап — эффективный максимум). Возвращает
    * новые статы и фактически восстановленную величину. */
  private def repairArmor(hero: Hero, value: Long, nowMs: Long): (FightStats, Long) = {
    val maxArmor = hero.effectiveMaxArmor(nowMs)
    val newArmor = (hero.fightStats.armor + value).min(maxArmor)
    (hero.fightStats.copy(armor = newArmor), newArmor - hero.fightStats.armor)
  }

  // ── Фляга и пояс (расходники, ход монстра не провоцируют) ────────────────────

  /** Бросок «сработала ли пассивка» с шансом `pct`% — но ТОЛЬКО если она активна
    * (`active`). Если пассивки нет, случайное число не тратится (важно для
    * детерминированных боевых тестов): сразу `false`. */
  private def chanceRoll(active: Boolean, pct: Long): Task[Boolean] =
    if (!active) ZIO.succeed(false)
    else Random.nextIntBetween(1, 101).map(_ <= pct)

  /** Пассивка «Быстрые руки»: с 25% шансом расходник не тратит раунд (можно применить
    * ещё раз). Возвращает флаг «раунд потрачен» (в `consumableUsedThisRound`) и
    * приписку в лог, если прокнуло. */
  private def quickHandsRoll(hero: Hero): Task[(Boolean, Vector[String])] =
    chanceRoll(hero.passives.hasQuickHands, hero.passives.quickHandsChancePct).map { repeated =>
      if (repeated) (false, Vector(content.text("battle.quickHands")))
      else (true, Vector.empty)
    }

  /** Эффект выпитой фляги. Герой приходит уже с потраченным зарядом в ячейке
    * пояса, а бой — с отметкой о расходнике. */
  private def flaskEffect(
      hero: Hero,
      battle: SoloPveBattle,
      effect: FlaskEffect,
      flaskName: String,
      nowMs: Long,
      qhLog: Vector[String]
  ): TurnResult =
    effect match {
        case FlaskEffect.HealPercent(pct) =>
          val maxHp    = hero.effectiveMaxHp(nowMs)
          // «Целитель» множит лечение фляги на 1.1, дальше — общий расчёт.
          val fullHeal = (maxHp * pct / 100L * hero.passives.healMult).toLong.max(1L)
          val (newHp, healed, healedBattle, cutLine) = activeHeal(hero, battle, fullHeal, nowMs)
          // Глоток фляги дополнительно восстанавливает 5% максимума Энергии.
          val maxEn      = hero.maxEnergy(nowMs)
          val newEnergy  = (hero.fightStats.energy + (maxEn * 5 / 100L).max(1L)).min(maxEn)
          val energyBack = newEnergy - hero.fightStats.energy
          val newStats   = hero.fightStats.copy(hp = newHp, energy = newEnergy)
          val msg = content.format(
            "battle.flaskUsed",
            "flask"  -> flaskName,
            "healed" -> healed.toString,
            "hp"     -> newHp.toString,
            "max"    -> maxHp.toString,
            "energy" -> energyBack.toString
          )
          val lines = (Vector(msg) ++ cutLine) ++ qhLog
          // Бой берём из расчёта лечения — в нём уже потраченное горение.
          TurnResult(hero.copy(fightStats = newStats),
            healedBattle, lines, Outcome.Continue, endsRound = false)
        case FlaskEffect.AddBuff(buff, rounds) =>
          val timedBuff = buff.copy(turnsLeft = Some(rounds))
          val newBattle = battle.copy(heroBattleState = battle.heroBattleState.add(timedBuff))
          TurnResult(hero, newBattle, Vector(content.text("battle.flaskBuff")) ++ qhLog,
            Outcome.Continue, endsRound = false)

        // Фляга кузнеца: чинит броню до потолка.
        case FlaskEffect.ArmorPercent(pct) =>
          val maxArmor = hero.effectiveMaxArmor(nowMs)
          val newArmor = (hero.fightStats.armor + (maxArmor * pct / 100L).max(1L)).min(maxArmor).max(hero.fightStats.armor)
          val msg = content.format("battle.flaskArmor", "flask" -> flaskName,
            "armor" -> (newArmor - hero.fightStats.armor).toString, "cur" -> newArmor.toString, "max" -> maxArmor.toString)
          TurnResult(hero.copy(fightStats = hero.fightStats.copy(armor = newArmor)),
            battle, Vector(msg) ++ qhLog, Outcome.Continue, endsRound = false)

        // Фляга бодрости: энергия до потолка.
        case FlaskEffect.EnergyPercent(pct) =>
          val maxEn = hero.maxEnergy(nowMs)
          val newEn = (hero.fightStats.energy + (maxEn * pct / 100L).max(1L)).min(maxEn).max(hero.fightStats.energy)
          val msg = content.format("battle.flaskEnergy", "flask" -> flaskName,
            "energy" -> (newEn - hero.fightStats.energy).toString, "cur" -> newEn.toString, "max" -> maxEn.toString)
          TurnResult(hero.copy(fightStats = hero.fightStats.copy(energy = newEn)),
            battle, Vector(msg) ++ qhLog, Outcome.Continue, endsRound = false)

        // Стихийная фляга: урона нет, прок стихии — гарантирован. Иммунитеты
        // цели в силе: огненного элементаля пламя не берёт, элементалю и
        // нежити прок ложится как обычный (см. withEffects).
        case FlaskEffect.Splash(element) =>
          // Урона фляга не наносит — молния выжигает долю потолка энергии, как
          // прежде (см. Element.LightningEnergyBurnPct).
          val flaskBurn          = (battle.monsterStats.energy * Element.LightningEnergyBurnPct / 100L).max(1L)
          val (procced, procLog) = applyProcs(battle, Set(element), heroAirTurns(hero), heroProcMods(hero, flaskBurn))
          val guarded = procced.withEffects(procced.effects)
          val head = content.format("battle.flaskSplash", "flask" -> flaskName, "monster" -> battle.monsterName)
          val lines =
            if (element == Element.Fire && guarded.boss.exists(_.immuneToBurn))
              Vector(head, content.text("battle.burnImmune"))
            else Vector(head) ++ procLog
          TurnResult(hero, guarded, lines ++ qhLog, Outcome.Continue, endsRound = false)

        // Фляга очищения: всё вредное с героя долой — раны, дебафы, срез защиты,
        // проклятие Некроманта.
        case FlaskEffect.Cleanse =>
          val cleaned = battle.effects.copy(
            heroBurn = None, heroPoison = None, heroBleed = None,
            heroStunnedTurns = 0, heroGroundedTurns = 0, heroColdDefenceCut = 0, heroWebTurns = None,
            heroSkillJammed = None, heroSkillJam = None, heroCursedTurns = None, heroFrozenTurns = None, heroDrainTurns = None)
          TurnResult(hero, battle.copy(effects = cleaned),
            Vector(content.text("battle.flaskCleanse")) ++ qhLog, Outcome.Continue, endsRound = false)

        // Дымная фляга: мобы вне пары на несколько раундов теряют из виду и
        // героя, и друг друга. Запас в один тик — см. BattleEffects.heroSmokeTurns.
        case FlaskEffect.Smoke(rounds) =>
          TurnResult(hero,
            battle.copy(effects = battle.effects.copy(heroSmokeTurns = rounds + 1)),
            Vector(content.format("battle.flaskSmoke", "rounds" -> rounds.toString)) ++ qhLog,
            Outcome.Continue, endsRound = false)

        // Вампирская фляга: следующие удары по HP лечат (см. playerStrike).
        case FlaskEffect.Vampiric(hits, pct) =>
          TurnResult(hero,
            battle.copy(effects = battle.effects.copy(heroVampiricHits = hits)),
            Vector(content.format("battle.flaskVampiric", "hits" -> hits.toString, "pct" -> pct.toString)) ++ qhLog,
            Outcome.Continue, endsRound = false)

        // Фляги яда и крови: оружие смазано на несколько раундов (см. playerStrike).
        case FlaskEffect.PoisonCoat(rounds) =>
          TurnResult(hero,
            battle.copy(effects = battle.effects.copy(heroPoisonCoatTurns = rounds)),
            Vector(content.format("battle.flaskPoisonCoat", "rounds" -> rounds.toString)) ++ qhLog,
            Outcome.Continue, endsRound = false)
        case FlaskEffect.BleedCoat(rounds) =>
          TurnResult(hero,
            battle.copy(effects = battle.effects.copy(heroBleedCoatTurns = rounds)),
            Vector(content.format("battle.flaskBleedCoat", "rounds" -> rounds.toString)) ++ qhLog,
            Outcome.Continue, endsRound = false)
    }

  /** Пояс в бою: сумка с расходниками. Кнопка «Пояс» ход не тратит — она
    * присылает отдельное сообщение со списком ячеек, и кнопки живут внутри
    * него (`inline`), поэтому боевая клавиатура и экран боя остаются как есть.
    *
    * Опустевшие ячейки стоят в списке с пометкой «пусто», но кнопки у них нет:
    * брать из них нечего. */
  private def showBelt(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      // Пояс показывают и после хода, которым бой кончился: боя тогда уже нет.
      aiming <- heroDao.readActiveBattle(user.userId)
                  .map(_.flatMap(_.as[SoloPveBattle].toOption).exists(_.aimSlot.isDefined))
      belt    = hero.equipment.belt
      // Номер у строки и у кнопки — настоящий номер ячейки: иначе высыпанная
      // горсть в первой ячейке сдвинула бы все остальные.
      slots  = BattleState.beltCells(hero)
      cells  = BattleState.beltUsable(hero)
      cap    = ItemDetails.Belt.capacityFor(belt.rarity)
      screen =
        if (belt.itemType != ItemType.Belt) Screen(content.text("battle.belt.none"), Nil, inline = true)
        // Тетива натянута — до пояса рука не дотянется.
        else if (aiming) Screen(content.text("battle.powerShot.beltBusy"), Nil, inline = true)
        else if (slots.isEmpty) Screen(content.text("battle.belt.empty"), Nil, inline = true)
        else {
          // Стопка — сколько в ней из скольких: ячейка держит столько, сколько у
          // пояса ячеек.
          val stack = ItemDetails.Belt.stackFor(belt.rarity)
          val lines = slots.map { case (slot, idx) =>
            if (slot.uses > 0)
              content.format("battle.belt.line", "n" -> (idx + 1).toString,
                "name" -> slot.name, "uses" -> (slot match {
                  case s: BeltSlot.Stack => s"${s.count}/$stack"
                  case other             => other.uses.toString
                }))
            else content.format("battle.belt.spentLine", "n" -> (idx + 1).toString, "name" -> slot.name)
          }
          val buttons = cells.zipWithIndex.map { case ((slot, idx), i) =>
            pangea.engine.Choice("BeltUse", pangea.engine.Choice.fit(
              content.format("battle.belt.useBtn", "name" -> slot.name)),
              data = Map("slot" -> idx.toString), row = Some(i))
          }
          val head = if (cells.isEmpty) "battle.belt.headSpent" else "battle.belt.head"
          Screen(content.format(head, "used" -> slots.size.toString,
            "cap" -> cap.toString) + "\n" + lines.mkString("\n"), buttons, inline = true)
        }
      _ <- renderer.show(user, screen)
    } yield StateType.Battle

  /** Применить расходник из ячейки пояса. Фляга пьётся, пыль ложится на
    * оружие, отвар уходит внутрь, роза раскрывается; ход при этом не кончается
    * — как и раньше у расходников.
    *
    * Отвару, которому нужно знать, на что его тратят (какую травму снять, какую
    * флягу заправить), сперва задаём вопрос — ход на вопрос не уходит. */
  private def beltUse(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    BattleState.parseSlot(ua) match {
      case None      => showBelt(user, renderer)
      case Some(idx) =>
        (getHero(user) <*> ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))).flatMap { case (hero, now) =>
          BattleState.beltAt(hero, idx) match {
            case Some(br: BeltSlot.Brew) if BattleState.needsPick(br.kind) =>
              heroDao.readActiveBattle(user.userId).map(_.flatMap(_.as[SoloPveBattle].toOption)).flatMap {
                // Боя уже нет — кнопка из старого сообщения.
                case None         => showBelt(user, renderer)
                case Some(battle) =>
                  brewRefusal(hero, battle, br.kind, now) match {
                    case Some(why) => renderer.show(user, Screen(why, Nil, inline = true)).as(StateType.Battle)
                    case None      => offerBeltPick(user, hero, battle, idx, br.kind, now, renderer)
                  }
              }
            case _ =>
              resolve(user, renderer)(beltTurn(idx)) <* showBelt(user, renderer)
          }
        }
    }

  /** Выбор назван: тратим ячейку на него. Кнопка несёт и номер ячейки, и сам
    * выбор, поэтому между вопросом и ответом ничего не хранится. */
  private def beltPick(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    (BattleState.parseSlot(ua), BattleState.parsePick(ua)) match {
      case (Some(idx), Some(pick)) =>
        resolve(user, renderer)(beltTurn(idx, Some(pick))) <* showBelt(user, renderer)
      case _ => showBelt(user, renderer)
    }

  /** Вопрос к отвару: костоправный спрашивает, какую травму снять, заправка
    * (живая вода) — какую флягу заправить, мерцающий настой — на какое место
    * встать. Кнопки живут внутри отдельного сообщения, как и сам пояс, —
    * боевая сцена не меняется. */
  private def offerBeltPick(user: User, hero: Hero, battle: SoloPveBattle, idx: Int, kind: BrewKind,
                            nowMs: Long, renderer: Renderer): Task[StateType] = {
    val (head, options, perRow) = kind.effect match {
      case BrewEffect.CureTrauma =>
        (content.text("battle.belt.whichTrauma"),
          hero.activeTraumas(nowMs).filter(_.severity != Trauma.Heavy).distinct.map(t => (t.name, t.name)),
          BattleState.PicksPerRow)
      // Фляга — по номеру ячейки: две одинаковые фляги различаются только им.
      // Подпись длинная, поэтому по одной в ряд.
      case BrewEffect.RefillFlask(sources) =>
        (content.text("battle.belt.whichFlask"),
          refillableFlasks(hero, sources).map { case (i, f) =>
            (content.format("battle.belt.flaskPick", "n" -> (i + 1).toString, "name" -> f.name,
              "charges" -> f.charges.toString, "max" -> f.maxCharges.toString), i.toString)
          },
          1)
      // Места — свободные своей стороны, по номеру: не больше, чем держит
      // встроенная клавиатура ВК, ближайшие к герою (см. shiftChoices).
      case BrewEffect.Shift =>
        (content.text("battle.belt.wherePlace"),
          BattleState.shiftChoices(battle).map(p => (content.format("battle.belt.placePick", "n" -> p.toString), p.toString)),
          BattleState.PlacesPerRow)
      case _ => (content.text("battle.belt.gone"), Nil, 1)
    }
    val buttons = options.zipWithIndex.map { case ((label, pick), i) =>
      pangea.engine.Choice("BeltPick", pangea.engine.Choice.fit(label),
        data = Map("slot" -> idx.toString, "pick" -> pick),
        row = Some(i / perRow))
    }
    renderer.show(user, Screen(head, buttons, inline = true)).as(StateType.Battle)
  }

  /** Ход расходником из ячейки `idx`. `pick` — ответ на вопрос отвара, если он
    * его задавал (см. [[offerBeltPick]]). */
  private def beltTurn(idx: Int, pick: Option[String] = None)(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    hero.equipment.belt.details match {
      // Кнопка из списка, открытого до того, как натянули тетиву.
      case _: ItemDetails.Belt if battle.aimSlot.isDefined =>
        cont(hero, battle, content.text("battle.powerShot.beltBusy"))
      case b: ItemDetails.Belt if battle.consumableUsedThisRound =>
        val _ = b
        cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
      case b: ItemDetails.Belt =>
        b.at(idx) match {
          case None => cont(hero, battle, content.text("battle.belt.gone"))
          // Кнопка из старого сообщения: у пустой ячейки кнопки нет.
          case Some(f: BeltSlot.Flask) if f.empty =>
            cont(hero, battle, content.text("battle.belt.gone"))
          case Some(f: BeltSlot.Flask) =>
            quickHandsRoll(hero).map { case (consumed, qhLog) =>
              // Выпитая до дна фляга остаётся в ячейке пустой: её дольют
              // Густаво, ручей или отвар, а вампирская напьётся крови сама.
              val drunk = withBelt(hero, b.updated(idx, f.spent))
              flaskEffect(drunk, battle.copy(consumableUsedThisRound = consumed), f.kind.effect,
                f.name, nowMs, qhLog)
            }
          case Some(d: BeltSlot.Dust) if d.spent =>
            cont(hero, battle, content.text("battle.belt.gone"))
          case Some(d: BeltSlot.Dust) =>
            quickHandsRoll(hero).map { case (consumed, qhLog) =>
              val sprinkled = sprinkleInBattle(hero, d.kind)
              val (dusted, line) = sprinkled
              // Ячейка остаётся пустой, но помнит пыль: после боя доберёт такую же.
              val next  = b.updated(idx, d.taken)
              TurnResult(withBelt(dusted, next), battle.copy(consumableUsedThisRound = consumed),
                Vector(line) ++ qhLog, Outcome.Continue, endsRound = false)
            }
          case Some(br: BeltSlot.Brew) if br.kind == BrewKind.MushroomMix =>
            cont(hero, battle, content.text("battle.mixOffhandOnly"))
          case Some(br: BeltSlot.Brew) if br.spent =>
            cont(hero, battle, content.text("battle.belt.gone"))
          // Кнопка из старого сообщения: ответ на вопрос отвара устарел (травму
          // уже сняли, рода нет). Склянку на это не тратим — пусть спросит снова.
          case Some(br: BeltSlot.Brew) if !pickValid(hero, br.kind, pick, nowMs) =>
            cont(hero, battle, content.text("battle.belt.askAgain"))
          case Some(br: BeltSlot.Brew) =>
            brewRefusal(hero, battle, br.kind, nowMs) match {
              // Лечить нечего, мазать нечем — склянка цела, и раунд тоже.
              case Some(why) => cont(hero, battle, why)
              case None =>
                quickHandsRoll(hero).map { case (consumed, qhLog) =>
                  // Выпитая ячейка остаётся пустой, но помнит отвар: после боя
                  // доберёт такой же из сумки.
                  val drunk = withBelt(hero, b.updated(idx, br.taken))
                  brewEffect(drunk, battle.copy(consumableUsedThisRound = consumed), br.kind, nowMs, qhLog, pick)
                }
            }
          case Some(r: BeltSlot.Rose) if r.withered =>
            cont(hero, battle, content.text("battle.belt.gone"))
          case Some(r: BeltSlot.Rose) =>
            quickHandsRoll(hero).map { case (consumed, qhLog) =>
              // Раскрытие бьёт по всему полю, как божественное оружие, — только
              // силу цветок берёт от уровня хозяина, своего у него нет. На поясе
              // он расходник, как и всё прочее: один за раунд.
              val kind             = r.kind
              val blow             = DivineRates.DamagePerLvl * hero.lvl
              val (swept, _, foes) = sweep(battle, blow, kind.element, kind.poisons)
              val spent            = r.spent
              val line = content.format(s"battle.rose.${kind.entryName}",
                "damage" -> blow.toString, "foes" -> foes.toString)
              val log = Vector(line) ++
                (if (spent.withered) Vector(content.format("battle.rose.withered", "name" -> kind.itemName))
                 else Vector.empty) ++ qhLog
              val outcome = if (swept.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
              val hero1   = withBelt(hero, b.updated(idx, spent))
              val after   = if (outcome == Outcome.Victory && swept.promoteNext.isEmpty) endOfRoundHero(hero1, swept, nowMs) else hero1
              TurnResult(after, swept.copy(consumableUsedThisRound = consumed), log, outcome, endsRound = false)
            }
          case Some(h: BeltSlot.HolyWater) if h.spent =>
            cont(hero, battle, content.text("battle.belt.gone"))
          // Вода уже на поле — вторая склянка ничего не прибавит, её не тратим.
          case Some(_: BeltSlot.HolyWater) if battle.holyWater =>
            cont(hero, battle, content.text("battle.holyWater.already"))
          case Some(h: BeltSlot.HolyWater) =>
            quickHandsRoll(hero).map { case (consumed, qhLog) =>
              // Святая вода держится до конца боя. Ячейка пустеет, но помнит
              // склянку: после боя доберёт такую же из сумки.
              TurnResult(withBelt(hero, b.updated(idx, h.taken)),
                battle.copy(holyWater = true, consumableUsedThisRound = consumed),
                Vector(content.text("battle.holyWater.poured")) ++ qhLog, Outcome.Continue, endsRound = false)
            }
        }
      case _ => cont(hero, battle, content.text("battle.belt.none"))
    }

  /** Годится ли ответ на вопрос отвара: травма ещё на герое и не тяжёлая;
    * фляга в названной ячейке всё ещё своего толка и неполная. Отвару, который
    * ничего не спрашивает, годится любой. */
  private def pickValid(hero: Hero, kind: BrewKind, pick: Option[String], nowMs: Long): Boolean =
    kind.effect match {
      case BrewEffect.CureTrauma =>
        pick.flatMap(Trauma.byName)
          .exists(t => t.severity != Trauma.Heavy && hero.activeTraumas(nowMs).contains(t))
      case BrewEffect.RefillFlask(sources) =>
        pick.flatMap(_.toIntOption).exists(i => refillableFlasks(hero, sources).exists(_._1 == i))
      // Место могли занять, пока игрок думал, — тогда герой встанет рядом;
      // годится любое место своей стороны.
      case BrewEffect.Shift =>
        pick.flatMap(_.toIntOption).exists(p => p >= 1 && p <= Formation.WalkPlaces)
      case _ => true
    }

  /** Почему этим отваром сейчас не воспользоваться: лечить нечего, мазать
    * нечем, заправлять некуда. Ячейка при этом остаётся полной, а раунд —
    * нетронутым. */
  private def brewRefusal(hero: Hero, battle: SoloPveBattle, kind: BrewKind, nowMs: Long): Option[String] =
    kind.effect match {
      case BrewEffect.CureTrauma =>
        val active = hero.activeTraumas(nowMs)
        if (active.isEmpty) Some(content.text("battle.belt.noTrauma"))
        else if (active.forall(_.severity == Trauma.Heavy)) Some(content.text("battle.belt.onlyHeavy"))
        else None
      case BrewEffect.Coat(_, _) if hero.equipment.unarmed =>
        Some(content.text("battle.belt.noWeapon"))
      case BrewEffect.RefillFlask(sources) if refillableFlasks(hero, sources).isEmpty =>
        Some(content.text("battle.belt.noFlask"))
      // Мерцать некуда: бой без строя (такой босс ходит один на один),
      // герой пригвождён или все места отряда заняты.
      case BrewEffect.Shift if battle.boss.exists(!_.fightsInGroup) =>
        Some(content.text("battle.belt.noFormation"))
      case BrewEffect.Shift if battle.effects.heroRooted =>
        Some(content.text(heldKey(battle, "battle.belt.shimmerPinned")))
      case BrewEffect.Shift if battle.group.freeHeroPlaces.isEmpty =>
        Some(content.text("battle.belt.noPlace"))
      // Защита того же толка уже держит до конца боя — второй склянке делать нечего.
      case BrewEffect.Ward(w) if battle.effects.warded(w) =>
        Some(content.text("battle.belt.wardAlready"))
      // Великанье зелье не складывается само с собой.
      case BrewEffect.Giant if hero.statBoosts.hasActive(BrewRates.GiantBoost, nowMs) =>
        Some(content.text("battle.belt.giantAlready"))
      case _ => None
    }

  /** Какие фляги на поясе может заправить отвар: своего толка и не полные — с
    * номерами ячеек. */
  private def refillableFlasks(hero: Hero, sources: Set[RefillSource]): List[(Int, BeltSlot.Flask)] =
    hero.equipment.belt.details match {
      case b: ItemDetails.Belt =>
        b.slots.zipWithIndex.collect {
          case (f: BeltSlot.Flask, i)
            if sources.contains(f.kind.effect.refillSource) && f.charges < f.maxCharges => (i, f)
        }
      case _ => Nil
    }

  /** Отвар из ячейки пояса. Герой приходит уже с пустой ячейкой, а бой — с
    * отметкой о расходнике. Делает то же, что сделал бы глоток с карточки, —
    * только посреди боя. Чистый расчёт, как и всякий боевой ход: бафы, травмы и
    * быстрые отдыхи едут в результате хода, а пишет их commit (см.
    * [[persistExtras]]). */
  private def brewEffect(
      hero: Hero,
      battle: SoloPveBattle,
      kind: BrewKind,
      nowMs: Long,
      qhLog: Vector[String],
      pick: Option[String]
  ): TurnResult = {
    def done(h: Hero, b: SoloPveBattle, lines: Vector[String]): TurnResult = {
      val outcome = if (b.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
      val after   = if (outcome == Outcome.Victory && b.promoteNext.isEmpty) endOfRoundHero(h, b, nowMs) else h
      TurnResult(after, b, lines ++ qhLog, outcome, endsRound = false)
    }
    def boosted(name: String, buff: ParamsBuff, until: Long, line: String): TurnResult =
      done(hero.copy(statBoosts = hero.statBoosts.add(StatBoost(name, buff, until), nowMs)), battle, Vector(line))
    kind.effect match {
      // Боевой отвар: ровно то, что делала склянка того же толка.
      case BrewEffect.Potion(potion) =>
        val (stats, next, msg) = applyPotion(hero, battle, potion, kind.label, nowMs)
        done(hero.copy(fightStats = stats), next, Vector(msg))

      // Грибная смесь: лопается под ногами, достаётся всем, кто стоит в поле.
      case BrewEffect.Bomb =>
        val blow             = BrewRates.MushroomDamagePerLvl * hero.lvl
        val (swept, _, foes) = sweep(battle, blow, None, poisons = true)
        done(hero, swept, Vector(content.format("battle.mixThrown",
          "damage" -> blow.toString, "foes" -> foes.toString)))

      // Смазка на клинок: держится до конца боя, как и наложенная до него.
      case BrewEffect.Coat(coat, _) =>
        val key = coat match {
          case WeaponCoat.Poison   => "battle.belt.coatPoison"
          case WeaponCoat.Bleed    => "battle.belt.coatBleed"
          case WeaponCoat.RedVenom => "battle.belt.coatRedVenom"
        }
        done(hero.copy(weaponDust = hero.weaponDust.copy(coat = Some(coat))), battle,
          Vector(content.format(key, "weapon" -> hero.equipment.mainWeapon.name)))

      // Костоправный отвар: названную травму снимает прямо в сече.
      case BrewEffect.CureTrauma =>
        pick.flatMap(Trauma.byName)
          .filter(t => t.severity != Trauma.Heavy && hero.activeTraumas(nowMs).contains(t)) match {
          case None    => done(hero, battle, Vector(content.text("battle.belt.noTrauma")))
          case Some(t) =>
            val names = BattleState.withoutFirst(hero.traumaNames, t.name)
            val until = if (names.isEmpty) None else hero.traumaUntil
            done(hero.copy(traumaUntil = until, traumaNames = names), battle,
              Vector(content.format("battle.belt.cured", "trauma" -> t.name)))
        }

      // Заправка: отвар уходит в ту флягу своего толка, которую назвал игрок
      // (см. offerBeltPick); не назвал — в первую такую.
      case BrewEffect.RefillFlask(sources) =>
        val chosen = pick.flatMap(_.toIntOption)
        (refillableFlasks(hero, sources).find { case (i, _) => chosen.forall(_ == i) },
         hero.equipment.belt.details) match {
          case (Some((i, f)), b: ItemDetails.Belt) =>
            done(withBelt(hero, b.updated(i, f.refilled)), battle,
              Vector(content.format("battle.belt.flaskRefilled", "flask" -> f.name)))
          case _ => done(hero, battle, Vector(content.text("battle.belt.noFlask")))
        }

      // Бодрящий сбор: быстрые отдыхи ложатся в запас, как и выпитый у костра.
      case BrewEffect.InstantRest =>
        done(hero, battle, Vector(content.format("battle.belt.rests",
          "count" -> BrewRates.InstantRests.toString))).copy(instantRests = BrewRates.InstantRests)

      // Часовой баф: складывается с зельями Густаво, как и выпитый до боя.
      case BrewEffect.Boost(name, buff, label) =>
        boosted(name, buff, nowMs + Achievement.brewMs(hero, BrewRates.BoostDurationMs),
          content.format("battle.belt.boosted", "brew" -> kind.label,
            "pct" -> BrewRates.BoostPct.toString, "stat" -> label))

      // Мерцающий настой: место запоминается, шаг — в конце раунда (endRound).
      case BrewEffect.Shift =>
        pick.flatMap(_.toIntOption) match {
          case None    => done(hero, battle, Vector(content.text("battle.belt.askAgain")))
          case Some(p) =>
            done(hero, battle.copy(group = battle.group.copy(pendingShift = Some(p))),
              Vector(content.format("battle.belt.shimmer", "brew" -> kind.label, "n" -> p.toString)))
        }

      // Великанье зелье: предел здоровья выше до конца боя, прибавка налита сразу.
      case BrewEffect.Giant =>
        val before = hero.effectiveMaxHp(nowMs)
        val grown  = hero.copy(statBoosts = hero.statBoosts.add(
                       StatBoost(BrewRates.GiantBoost, ParamsBuff.zero, nowMs + BrewRates.GiantMs), nowMs))
        val max    = grown.effectiveMaxHp(nowMs)
        val hp     = (grown.fightStats.hp + (max - before).max(0L)).min(max)
        done(grown.copy(fightStats = grown.fightStats.copy(hp = hp)), battle,
          Vector(content.format("battle.belt.giant", "brew" -> kind.label,
            "pct" -> BrewRates.GiantHpPct.toString, "hp" -> hp.toString, "max" -> max.toString)))

      // Ровная регенерация: лечит каждый раунд, не слабея, и сходит разом.
      case BrewEffect.SteadyRegen(pct, rounds) =>
        done(hero, battle.copy(effects = battle.effects.copy(heroRegen = Some(Regen(pct, Some(rounds))))),
          Vector(content.format("battle.belt.moonRegen", "brew" -> kind.label,
            "pct" -> pct.toString, "rounds" -> rounds.toString)))

      // Защитный отвар: снимает своё и держит до конца боя.
      case BrewEffect.Ward(w) =>
        done(hero, battle.copy(effects = battle.effects.withWard(w)),
          Vector(content.format(s"battle.belt.ward.${w.entryName}", "brew" -> kind.label)))

      // Зеркальный настой: копии встают тут же, в этом бою.
      case BrewEffect.Mirror(copies) =>
        done(hero, battle.copy(effects = battle.effects.copy(heroMirrors = copies)),
          Vector(content.format("battle.belt.mirrors", "brew" -> kind.label, "count" -> copies.toString)))

      // Волчий зов: запах держится час и сгорит на ближайшей встрече.
      case BrewEffect.WolfCall =>
        boosted(BrewRates.WolfCallBoost, ParamsBuff.zero, nowMs + Achievement.brewMs(hero, BrewRates.WolfCallMs),
          content.text("battle.belt.wolfCall"))

      // Гнилой зов — узелок: смрад ведёт к Гнилому Джо, ближайшая встреча — он.
      case BrewEffect.RotCall =>
        boosted(BrewRates.RotCallBoost, ParamsBuff.zero, nowMs + Achievement.brewMs(hero, BrewRates.RotCallMs),
          content.text("battle.belt.rotCall"))

      // Пузырь: срока у плёнки нет — она ждёт смерти, сколько бы её ни ждать.
      case BrewEffect.Bubble =>
        boosted(BrewRates.BubbleBoost, ParamsBuff.zero, Long.MaxValue, content.text("battle.belt.bubble"))

      // В ячейку эти не ложатся вовсе (см. BrewKind.beltable): шнапс с самогоном
      // носят Трактирщику, а приговор пьют до боя — с кем встретиться, решают
      // не посреди сечи. На Inert же не сидит ни один отвар.
      case BrewEffect.Sellable(_) | BrewEffect.Sentence | BrewEffect.Inert =>
        done(hero, battle, Vector(content.text("battle.belt.notInBattle")))
    }
  }

  /** Эффект боевого отвара — того, что пришёл на место склянки. Мгновенные
    * (лечение, броня, энергия) меняют статы героя; временные (атака, защита,
    * уворот) кладут баф на [[BrewRates.PotionBuffRounds]] раундов; яд и реген
    * ставят свой статус-эффект. Возвращает новые статы, обновлённый бой и
    * строку эффекта — записывает всё commit. */
  private def applyPotion(
      hero: Hero,
      battle: SoloPveBattle,
      potion: PotionKind,
      brew: String,
      nowMs: Long
  ): (FightStats, SoloPveBattle, String) = {
    val buffed = battle.heroBattleState.applyTo(hero.effectiveFightStats(nowMs))
    val rounds = BrewRates.PotionBuffRounds
    potion match {
      case PotionKind.Healing =>
        val maxHp = hero.effectiveMaxHp(nowMs)
        // «Целитель» множит лечение отвара на 1.1, дальше — общий расчёт.
        val full  = (maxHp * BrewRates.PotionHealPct / 100L * hero.passives.healMult).toLong.max(1L)
        val (newHp, healed, healedBattle, cutLine) = activeHeal(hero, battle, full, nowMs)
        val line  = content.format("battle.beltHeal", "brew" -> brew,
                      "healed" -> healed.toString, "hp" -> newHp.toString, "max" -> maxHp.toString)
        (hero.fightStats.copy(hp = newHp), healedBattle, (line +: cutLine.toVector).mkString("\n"))

      case PotionKind.Metal =>
        val maxArmor = hero.effectiveMaxArmor(nowMs)
        val restore  = (maxArmor * BrewRates.PotionArmorPct / 100L).max(1L)
        val newArmor = (hero.fightStats.armor + restore).min(maxArmor).max(hero.fightStats.armor)
        (hero.fightStats.copy(armor = newArmor), battle,
          content.format("battle.beltMetal", "brew" -> brew,
            "armor" -> (newArmor - hero.fightStats.armor).toString,
            "cur" -> newArmor.toString, "max" -> maxArmor.toString))

      case PotionKind.Poison =>
        (hero.fightStats, battle.copy(effects = battle.effects.copy(heroPoisonousAttacks = true)),
          content.format("battle.beltPoison", "brew" -> brew))

      case PotionKind.Regeneration =>
        (hero.fightStats, battle.copy(effects = battle.effects.copy(heroRegen = Some(Regen.onDrink))),
          content.format("battle.beltRegen", "brew" -> brew, "pct" -> BrewRates.PotionRegenPct.toString))

      case PotionKind.Evasion =>
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(0L, 0L, 0L, dodgePct = BrewRates.PotionDodgePct.toLong, defencePct = 0L, turnsLeft = Some(rounds)))),
          content.format("battle.beltEvasion", "brew" -> brew,
            "pct" -> BrewRates.PotionDodgePct.toString, "rounds" -> rounds.toString))

      case PotionKind.Energy =>
        val maxEn     = hero.maxEnergy(nowMs)
        val restore   = (maxEn * BrewRates.PotionEnergyPct / 100L).max(1L)
        val newEnergy = (hero.fightStats.energy + restore).min(maxEn).max(hero.fightStats.energy)
        (hero.fightStats.copy(energy = newEnergy), battle,
          content.format("battle.beltEnergy", "brew" -> brew,
            "energy" -> (newEnergy - hero.fightStats.energy).toString,
            "cur" -> newEnergy.toString, "max" -> maxEn.toString))

      case PotionKind.Attack =>
        val bonus = (buffed.atk * BrewRates.PotionAtkPct / 100L).max(1L)
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(atk = bonus, 0L, 0L, dodgePct = 0L, defencePct = 0L, turnsLeft = Some(rounds)))),
          content.format("battle.beltAttack", "brew" -> brew,
            "atk" -> bonus.toString, "rounds" -> rounds.toString))

      case PotionKind.Defence =>
        val bonus = (buffed.defence * BrewRates.PotionDefencePct / 100L).max(1L)
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(0L, 0L, defence = bonus, dodgePct = 0L, defencePct = 0L, turnsLeft = Some(rounds)))),
          content.format("battle.beltDefence", "brew" -> brew,
            "defence" -> bonus.toString, "rounds" -> rounds.toString))
    }
  }

  /** Горсть пыли на оружие прямо в бою: правила те же, что у наковальни —
    * слой ложится, противоположная стихия даёт всполох, четвёртая горсть
    * сбивает покрытие (см. [[WeaponDust.sprinkle]]). */
  private def sprinkleInBattle(hero: Hero, dust: pangea.model.item.MaterialKind): (Hero, String) = {
    val outcome = WeaponDust.sprinkle(dust, hero.weaponDust, hero.equipment.handGems)
    val line = outcome match {
      case _: WeaponDust.Outcome.Applied  => content.format("battle.belt.dustApplied", "dust" -> dust.displayName)
      case _: WeaponDust.Outcome.Clash    => content.format("battle.belt.dustClash", "dust" -> dust.displayName)
      case _: WeaponDust.Outcome.Overload => content.format("battle.belt.dustOverload", "dust" -> dust.displayName)
    }
    (hero.copy(weaponDust = outcome.next), line)
  }

  /** Опустевшие ячейки пояса добирают из сумки то же, чем были полны: горсть
    * пыли, склянку отвара, такой же цветок. Чего не нашлось, остаётся пустым —
    * игрок доложит сам. */
  private def refillBelt(user: User, hero: Hero): Task[List[String]] = {
    val stack = ItemDetails.Belt.stackFor(hero.equipment.belt.rarity)
    hero.equipment.belt.details match {
      case b: ItemDetails.Belt if b.slots.exists(BattleState.refillable(_, stack)) =>
        for {
          inv <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
          // Идём по ячейкам, забирая из сумки то, что взяли: две неполные
          // ячейки одного вида не должны разобрать одну и ту же щепоть дважды.
          // Стопка добирается до полной, роза — одна, взамен осыпавшейся.
          taken = b.slots.foldLeft((List.empty[BeltSlot], List.empty[Item], inv.items.data)) {
                    case ((slots, used, bag), s: BeltSlot.Stack) if s.count < stack =>
                      val more = bag.filter(i => BeltSlot.of(i).exists(s.sameKind)).take(stack - s.count)
                      (slots :+ s.withCount(s.count + more.size), used ++ more, bag.filterNot(i => more.exists(_.id == i.id)))
                    case ((slots, used, bag), r: BeltSlot.Rose) if r.withered =>
                      bag.find(_.rose.exists(_.kind == r.kind)) match {
                        case Some(item) => (slots :+ r.refilled, used :+ item, bag.filterNot(_.id == item.id))
                        case None       => (slots :+ r, used, bag)
                      }
                    case ((slots, used, bag), other) => (slots :+ other, used, bag)
                  }
          (slots, used, _) = taken
          _ <- ZIO.when(used.nonEmpty)(
                 inventoryRepo.removeItems(used.map(_.id).toSet, hero.id)
                   .mapError(e => new Throwable(e.toString)) *>
                   heroDao.updateEquipment(user.userId, hero.equipment.copy(
                     belt = hero.equipment.belt.copy(details = b.copy(slots = slots)))))
        } yield used.map(_.name)
      case _ => ZIO.succeed(Nil)
    }
  }

  /** Пояс героя с новым содержимым. */
  private def withBelt(hero: Hero, belt: ItemDetails.Belt): Hero =
    hero.copy(equipment = hero.equipment.copy(
      belt = hero.equipment.belt.copy(details = belt)))

  /** Экран подтверждения бегства (чистая навигация, состояние не меняется). */
  /** Кнопка миазм: отпустить тьму или свернуть её. Выбор держится до
    * следующего нажатия, а не до конца боя (см. BattlePrefs). Ход он не тратит —
    * это не действие, а переключатель. */
  private def toggleMiasma(user: User, renderer: Renderer): Task[StateType] =
    for {
      prefs <- BattleState.prefs(heroDao, user.userId)
      next   = prefs.withMiasma(!prefs.miasma)
      _     <- heroDao.writeBattlePrefs(user.userId, next.asJson)
      _     <- renderer.show(user, Screen(
                 content.text(if (next.miasma) "battle.miasmaOn" else "battle.miasmaOff"), Nil))
      _     <- showScreen(user, renderer)
    } yield StateType.Battle

  private def flee(user: User, renderer: Renderer): Task[StateType] =
    getBattle(user).flatMap { battle =>
      // Западня или сюжетный бой: уйти отсюда некуда.
      if (battle.cantFlee) renderer.show(user, Screen(noFleeText(battle), Nil)).as(StateType.Battle)
      else renderer.show(user, content.screen("battle.fleeConfirm")).as(StateType.Battle)
    }

  /** Почему не сбежать: из сюжетного боя не уходят, из западни — некуда. */
  private def noFleeText(battle: SoloPveBattle): String =
    if (battle.story.isDefined) content.text("battle.noFleeStory")
    else content.format("battle.noFlee", "monster" -> battle.monsterName)

  /** Подтверждённое бегство: моб делает один удар (без каста/тиков/регена, как и
    * прежде). Умер герой — Death; иначе — Fled (бой очистит commit). */
  private def fleeTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    // Старая кнопка подтверждения из сюжетного боя или западни — не бегство.
    if (battle.cantFlee) cont(hero, battle, noFleeText(battle))
    else for {
      buffedEff <- ZIO.succeed(effWithAir(hero, battle, nowMs))
      monster   = battle.toMonster
      // Мобы вне пары могут не выпустить: по 5% за каждого. Окружили — бой
      // продолжается, как будто бегства и не было. Гоблина в «Пройдохе»
      // (порог 12) окружают реже.
      surroundPct = GroupState.SurroundPctPerFreeMob * battle.group.others.size *
                      (100L - hero.sets.surroundCutPct) / 100L
      surrounded <- chanceRoll(surroundPct > 0L, surroundPct)
      hitRoll <- Random.nextIntBetween(1, 101)
      // «Сверкающий» дают +25% к уклонению именно при бегстве.
      result <-
        if (surrounded)
          ZIO.succeed(TurnResult(hero, battle,
            Vector(content.format("battle.group.surround", "race" -> monster.race.genitivePlural)), Outcome.Continue))
        // Напротив никого — бить в спину некому.
        else if (battle.unpaired)
          ZIO.succeed(TurnResult(hero, battle, Vector.empty, Outcome.Fled))
        else if (hitRoll > playerDodgeChance(hero, battle, nowMs, fleeing = true)) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            rawDamage = (monster.fightStats.atk * spread / 100L).max(1L)
            reduction = BattleState.heroDamageReduction(hero, battle, buffedEff, monster.fightStats.atk, nowMs)
            baseReduced = (rawDamage * (1.0 - reduction)).toLong.max(1L)
            // «Непробиваемый»: 20% шанс срезать урон обычной атаки вдвое.
            impenTriggered <- chanceRoll(hero.passives.hasImpenetrable, PassiveKind.Impenetrable.TriggerPct)
            impenDamage =
              if (impenTriggered) (baseReduced * (100L - hero.passives.impenetrableReductionPct) / 100L).max(1L)
              else baseReduced
            // «Мать Глубин» (порог 6): чешуя скользкая и для удара в спину.
            slipped <- chanceRoll(hero.sets.slipChancePct > 0L, hero.sets.slipChancePct)
            reducedDamage = if (slipped) (impenDamage * (100L - hero.sets.slipCutPct) / 100L).max(1L) else impenDamage
            // Удар в спину при бегстве разбирается тем же расчётом, что и обычная
            // атака в бою: та же баффовая броня, тот же порог 6 «Каменного стража».
            parting     = bossDamageTaken(hero, battle, reducedDamage)
            (newHp, newArmor, landed) = bossHit(battle, hero, parting, nowMs)
            hero1       = hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor))
            partTrauma <- hitTrauma(hero, hero1, nowMs)
            (hero2, traumaLine) = partTrauma
            mobLine     = content.format("battle.mobHit", "damage" -> landed.toString, "monster" -> battle.monsterName,
                            "elem" -> strikeBy(battle))
            lines       = Vector(mobLine) ++ Option.when(slipped)(content.text("battle.deepSlip")).toVector ++
                            traumaLine.toVector
          } yield
            if (newHp > 0) TurnResult(hero2, battle, lines, Outcome.Fled)
            // «Первопроходец» (порог 12): устоял после удара в спину — и всё-таки ушёл.
            else secondWind(TurnResult(hero2, battle, lines, Outcome.Death), side = false) match {
              case up if up.outcome == Outcome.Continue => up.copy(outcome = Outcome.Fled)
              case dead                                 => dead
            }
        } else
          ZIO.succeed(TurnResult(hero, battle, Vector.empty, Outcome.Fled))
    } yield result

  // ── Группа ──────────────────────────────────────────────────────────────────

  /** Активный моб пал, а в группе есть ещё: в паре встаёт следующий по номеру,
    * бой продолжается. Павший ждёт выдачи добычи в `slain`. Новичок в этом
    * раунде уже не бьёт — он только что шагнул вперёд. */
  private def promoteAfterKill(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Victory) res
    else res.battle.promoteNext match {
      case None       => res
      case Some(next) =>
        // Замену к герою не подставляем — просто говорим, что место опустело.
        val line = content.format("battle.group.fell", "monster" -> res.battle.monsterName)
        res.copy(battle = next, outcome = Outcome.Continue, log = res.log :+ line)
    }

  /** Ход ауры в конце раунда (см. [[Aura]]). Аура держит ВСЁ поле боя и
    * достаётся каждому, кто на нём стоит, — своим и чужим одинаково, по природе
    * существа ([[Nature]]). Герой держит её силой: доля энергии уходит за раунд
    * и она же идёт в силу ауры, так что к концу схватки аура слабеет сама.
    * Энергии меньше порога — держать нечем, аура оседает.
    *
    * В логе аура говорит о себе ОДНОЙ строкой на всё поле: перечислять, кому
    * сколько досталось, бессмысленно — в групповом бою это двадцать строк. Своё
    * герой видит отдельно: его-то лечение или урон ему и важны. Павший от ауры
    * союзник тоже сказан — это событие, а не сдача.
    *
    * Герой-некромант на пороге 10 сам числится нежитью, поэтому своей же аурой
    * лечится.
    *
    * Святая вода и миазмы гасят друг друга: пока на поле держится тьма — чья
    * угодно, героя или Некроманта, — не работает ни она, ни вода. Свернул герой
    * миазмы, выдохся или пал Некромант — вода начинает жечь мёртвых. */
  private def auraTick(res: TurnResult, on: Boolean): TurnResult =
    if (res.battle.holyWater && miasmaHeld(res, on)) holyClash(res)
    else {
      val byHero = res.hero.sets.auras.foldLeft(res)((acc, aura) => heroAura(acc, aura, on))
      // Чужая аура: её держит моб, и платит он своей энергией. Правила те же — и
      // она так же не разбирает, кто на поле свой.
      val byMob = byHero.battle.auraHolder.fold(byHero) { case (aura, _) => mobAura(byHero, aura) }
      auraFelled(if (byMob.battle.holyWater) holyTick(byMob) else byMob)
    }

  /** Кого аура добила, того и разбираем — так же, как павших от удара или ран.
    *
    * Сперва героя. Стоящий на нуле — это смерть, и она важнее победы, даже если
    * та же аура свалила и того, кто в полях боя (как в [[monsterPhase]] и
    * [[sideMobsPhase]]). «Первопроходец» спасает и от неё — как после хода.
    * Дальше смерть разбирает commit: при живом отряде герой ляжет, и бой пойдёт
    * без него. Раньше исход оставался «бой идёт»: герой стоял с нулём HP и
    * следующим ходом ещё бил — добил активного один на один — победа на нуле
    * HP, не добил — смерть ходом позже, а второго дыхания не было вовсе.
    * Лежачего аура не трогает (см. [[applyAura]]) — его исход решит отряд.
    *
    * Потом того, кто в полях боя: аура добила его, а герой стоит (или его
    * поднял «Первопроходец»), — это победа над ним, и дальше её разбирают тем
    * же порядком: Джо встаёт, в группе место перед героем пустеет. Раньше
    * мёртвый так и стоял в паре с нулём HP: бил героя, а в павших не числился —
    * Некроманту было нечего поднимать. */
  private def auraFelled(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound) res
    else {
      val heroFell = res.hero.fightStats.hp <= 0L && !res.battle.group.heroDown
      val stood    = if (heroFell) secondWind(res.copy(outcome = Outcome.Death), side = false) else res
      if (stood.outcome != Outcome.Continue || stood.battle.monsterCurrentHp > 0L) stood
      else stood.copy(outcome = Outcome.Victory)
    }

  // ── Узор Сборщика ───────────────────────────────────────────────────────────

  /** «Тянет узор» (см. [[PatternPull]]): раз в несколько раундов, в конце
    * раунда, тот, кто это умеет (`SoloPveBattle.puller`), тянет Клеймо героя —
    * только героя, стоящего на ногах. Атака героя падает на свои раунды, а сам
    * он восстанавливает долю своего максимума HP — где бы ни стоял: в полях или
    * в строю. Его нет на поле, раунд не кончился или бой решён — ничего. */
  private[battle] def patternPull(res: TurnResult): TurnResult = {
    val b = res.battle
    b.puller match {
      case Some(name) if res.outcome == Outcome.Continue && res.endsRound && PatternPull.due(b.roundsFought) &&
                         !b.group.heroDown && res.hero.fightStats.hp > 0L =>
        val atActive = b.monsterCurrentHp > 0L && b.customName.contains(name)
        val slotIdx  = b.group.others.indexWhere(s => s.alive && s.customName.contains(name))
        val healed: Option[(SoloPveBattle, Long)] =
          if (atActive) {
            val hp = (b.monsterCurrentHp + PatternPull.healOn(b.monsterStats.hp)).min(b.monsterStats.hp)
            Some((b.copy(monsterCurrentHp = hp), hp - b.monsterCurrentHp))
          }
          else b.group.others.lift(slotIdx).map { s =>
            val hp = (s.currentHp + PatternPull.healOn(s.stats.hp)).min(s.stats.hp)
            (b.copy(group = b.group.copy(others = b.group.others.updated(slotIdx, s.copy(currentHp = hp)))), hp - s.currentHp)
          }
        healed match {
          case None => res
          case Some((after, gained)) =>
            val drained = after.copy(effects = after.effects.copy(heroDrainTurns = Some(PatternPull.Turns)))
            res.copy(battle = drained, log = res.log :+ content.format("battle.patternPull", "name" -> name,
              "pct" -> PatternPull.AttackCutPct.toString, "turns" -> PatternPull.Turns.toString, "hp" -> gained.toString))
        }
      case _ => res
    }
  }

  // ── Бочки с порохом ─────────────────────────────────────────────────────────

  /** Бочки с порохом на поле (см. [[PowderKeg]]): в конце раунда одна может
    * рвануть. Кого задеть, решает бросок: взрыв не разбирает сторон — герой,
    * отряд и мобы вперемешку. Пламя на героя ложится с поправкой на
    * «Каменного стража» (порог 10), как от удара. Бочек нет, раунд не
    * кончился или бой уже решён — поле молчит. */
  private def kegBlast(res: TurnResult, now: Long): Task[TurnResult] =
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle.kegs <= 0) ZIO.succeed(res)
    else
      for {
        blows   <- chanceRoll(active = true, PowderKeg.ChancePct)
        order   <- if (blows) Random.shuffle(kegTargets(res)) else ZIO.succeed(List.empty[BattleState.KegTarget])
        hit      = order.take(PowderKeg.Victims)
        catches <- chanceRoll(hit.contains(BattleState.KegTarget.Hero), 100L - res.hero.sets.igniteResistPct)
      } yield if (hit.isEmpty) res else kegHits(res, hit, catches, now)

  /** Кто стоит на поле и кого может задеть взрыв. Лежачего героя бочка не
    * трогает — как и аура: его исход решит отряд. */
  private def kegTargets(res: TurnResult): List[BattleState.KegTarget] = {
    val b = res.battle
    Option.when(!b.group.heroDown && res.hero.fightStats.hp > 0L)(BattleState.KegTarget.Hero).toList ++
      b.group.allies.filter(_.alive).map(a => BattleState.KegTarget.Ally(a.position)) ++
      Option.when(b.monsterCurrentHp > 0L)(BattleState.KegTarget.Active).toList ++
      b.group.others.zipWithIndex.collect { case (slot, idx) if slot.alive => BattleState.KegTarget.Slot(idx) }
  }

  /** Взрыв бочки по тем, кого задело (`hit`): каждому — доля его максимума HP
    * мимо брони (минибоссу — вдвое меньше и не больше остатка его предела за
    * раунд, см. `BossRoundCap`) и пламя: горящий разгорается, замёрзший
    * оттаивает, а кого огонь не берёт — не загорается. `heroCatches` — легло ли
    * пламя на героя. Строка одна на весь взрыв; павшие союзники уходят своими
    * строками, павшие из строя — в павшие. Героя и того, кто в полях,
    * разбирает [[auraFelled]] — тем же порядком, что павших от ауры. Бочкой на
    * поле меньше. */
  private[battle] def kegHits(res: TurnResult, hit: List[BattleState.KegTarget], heroCatches: Boolean,
                              now: Long): TurnResult = {
    import BattleState.KegTarget
    def fire(lit: Boolean): String = if (lit) content.text("battle.kegs.fire") else ""
    val dead = content.text("battle.kegs.dead")
    def part(name: String, hp: Long, mark: String): String =
      content.format("battle.kegs.hit", "name" -> name, "hp" -> hp.toString, "mark" -> mark)
    val start = (res.hero, res.battle, Vector.empty[String], Vector.empty[BattleAlly], List.empty[Int])
    val (hero, battle, parts, gone, fallen) = hit.foldLeft(start) {
      case ((h, b, ps, gs, fs), KegTarget.Hero) =>
        val dmg   = PowderKeg.damageOn(h.effectiveMaxHp(now)).min(h.fightStats.hp)
        val hurt  = h.copy(fightStats = h.fightStats.copy(hp = h.fightStats.hp - dmg))
        // Пламя ложится только на того, кто устоял: павшему гореть незачем.
        val lit   = if (heroCatches && hurt.fightStats.hp > 0L) reigniteVictim(Victim.Hero, b, Burn.Initial)._1 else b
        val burns = lit.effects.heroBurn.isDefined && lit.effects.heroBurn != b.effects.heroBurn
        (hurt, lit, ps :+ content.format("battle.kegs.hitHero", "hp" -> dmg.toString, "mark" -> fire(burns)), gs, fs)
      case ((h, b, ps, gs, fs), KegTarget.Ally(pos)) =>
        b.group.allyAt(pos).filter(_.alive) match {
          case None    => (h, b, ps, gs, fs)
          case Some(a) =>
            val dmg  = PowderKeg.damageOn(a.stats.hp).min(a.hp)
            val hurt = b.copy(group = b.group.updateAlly(pos)(x => x.copy(hp = x.hp - dmg)))
            if (a.hp - dmg <= 0L)
              (h, hurt.copy(group = hurt.group.withoutAlly(pos)), ps :+ part(a.name, dmg, dead), gs :+ a, fs)
            else {
              val lit   = reigniteVictim(Victim.Ally(pos), hurt, Burn.Initial)._1
              val burn  = lit.group.allyAt(pos).flatMap(_.burn)
              (h, lit, ps :+ part(a.name, dmg, fire(burn.isDefined && burn != a.burn)), gs, fs)
            }
        }
      case ((h, b, ps, gs, fs), KegTarget.Active) =>
        if (b.monsterCurrentHp <= 0L) (h, b, ps, gs, fs)
        else {
          val hurt         = b.hurtActive(0L, b.pctDamage(PowderKeg.damageOn(b.monsterStats.hp)))
          val lost         = b.monsterCurrentHp - hurt.battle.monsterCurrentHp
          val (lit, burns) = if (hurt.battle.monsterCurrentHp <= 0L) (hurt.battle, false) else kegIgnite(hurt.battle)
          (h, lit, ps :+ part(b.monsterName, lost, if (lit.monsterCurrentHp <= 0L) dead else fire(burns)), gs, fs)
        }
      case ((h, b, ps, gs, fs), KegTarget.Slot(idx)) =>
        b.group.others.lift(idx).filter(_.alive) match {
          case None       => (h, b, ps, gs, fs)
          case Some(slot) =>
            val raw     = PowderKeg.damageOn(slot.stats.hp)
            val boss    = slot.boss.flatMap(MiniBoss.byName).isDefined
            val (hb, _) = b.hurtSlot(idx, 0L, if (boss) BossPercentDamage(raw) else raw)
            val after   = hb.group.others.lift(idx).getOrElse(slot)
            val lost    = slot.currentHp - after.currentHp
            if (!after.alive) (h, hb, ps :+ part(slot.name, lost, dead), gs, idx :: fs)
            else {
              // Пламя — тем же правилом, что тому, кто в полях: слот на миг
              // встаёт в пару и возвращается в строй со своими эффектами.
              val (lit, burns) = kegIgnite(hb.withActive(after))
              val back = hb.copy(group = hb.group.copy(others = hb.group.others.updated(idx, lit.activeSlot)))
              (h, back, ps :+ part(slot.name, lost, fire(burns)), gs, fs)
            }
        }
    }
    // Павших из строя убираем с конца, чтобы индексы не плыли.
    val swept = fallen.sorted.reverse.foldLeft(battle)((b, idx) => b.sideFallen(idx))
    val left  = (swept.kegs - 1).max(0)
    val line  =
      if (left > 0) content.format("battle.kegs.blast", "hits" -> parts.mkString(", "), "left" -> left.toString)
      else content.format("battle.kegs.blastLast", "hits" -> parts.mkString(", "))
    val goneLines = gone.map(a => content.format(BattleState.goneKey(a), "name" -> a.name))
    auraFelled(res.copy(hero = hero, battle = swept.copy(kegs = left), log = (res.log :+ line) ++ goneLines))
  }

  /** Пламя от взрыва на того, кто в полях: под холодом лёд тает и огонь не
    * занимается, горящий разгорается сильнее, а кого огонь не берёт (огненный
    * элементаль, дракон), тот и не загорается (см. `withEffects`). Второе —
    * занялось ли пламя. */
  private def kegIgnite(b: SoloPveBattle): (SoloPveBattle, Boolean) =
    if (b.effects.coldOnMonster) (b.copy(effects = b.effects.copy(monsterColdDefenceCut = 0, chilledTurns = 0)), false)
    else {
      val lit = b.withEffects(b.effects.copy(monsterBurn = Some(b.effects.monsterBurn.map(_.reignited).getOrElse(Burn.onIgnite))))
      (lit, lit.effects.monsterBurn.isDefined)
    }

  /** Держатся ли на поле миазмы в этом раунде: герой их не свернул и ему есть
    * чем платить — или их держит моб, у которого хватает энергии. Осевшие
    * миазмы святую воду не гасят. */
  private def miasmaHeld(res: TurnResult, on: Boolean): Boolean = {
    val miasma = Aura.Miasma
    val byHero = on && res.hero.sets.auras.contains(miasma) && res.hero.fightStats.energy >= miasma.minEnergy
    val byMob  = res.battle.auraHolder.exists { case (aura, energy) => aura == miasma && energy >= aura.minEnergy }
    byHero || byMob
  }

  /** Святая вода и миазмы встретились: обе молчат, и строка об этом одна. */
  private def holyClash(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound) res
    else res.copy(log = res.log :+ content.text("battle.holyWater.clash"))

  /** Святая вода на поле: каждый мёртвый — свой, чужой, сам герой-нежить —
    * теряет долю максимума HP. Живых и камня она не касается, а нет на поле
    * мёртвых — молчит, чтобы не сорить строкой каждый раунд. */
  private def holyTick(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound || !undeadOnField(res)) res
    else applyAura(res, Aura.HolyWater, 0L,
      Vector(content.format("battle.aura.holyWater.holds", "pct" -> AuraRates.HolyWaterPct.toString)))

  /** Есть ли на поле хоть один мёртвый: герой, союзник или моб. */
  private def undeadOnField(res: TurnResult): Boolean = {
    val b = res.battle
    res.hero.sets.heroIsUndead ||
      b.group.allies.exists(a => a.alive && Nature.of(a.kind.race) == Nature.Undead) ||
      (b.monsterCurrentHp > 0L && Nature.of(b.monsterRace, b.monsterUndead) == Nature.Undead) ||
      b.group.others.exists(s => s.alive && Nature.of(s.race, s.undead) == Nature.Undead)
  }

  /** Аура, которую держит моб (Некромант): энергия его, правила общие. Энергии
    * не хватает — аура оседает, и строка об этом одна на всё поле. */
  private def mobAura(res: TurnResult, aura: Aura): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound) res
    else res.battle.auraHolder match {
      case None => res
      case Some((_, energy)) if energy < aura.minEnergy =>
        res.copy(log = res.log :+ content.text(s"battle.aura.${aura.key}.mobFades"))
      case Some((_, energy)) =>
        val spent  = aura.cost(energy)
        val paid   = res.copy(battle = res.battle.spendAuraEnergy(spent))
        val lines  = Vector(content.format(s"battle.aura.${aura.key}.mobHolds", "energy" -> spent.toString))
        applyAura(paid, aura, spent, lines)
    }

  private def heroAura(res: TurnResult, aura: Aura, on: Boolean): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound || !on) res
    else if (res.hero.fightStats.energy < aura.minEnergy)
      res.copy(log = res.log :+ content.text(s"battle.aura.${aura.key}.fades"))
    else {
      val spent = aura.cost(res.hero.fightStats.energy)
      val paid  = res.copy(hero = res.hero.copy(fightStats = res.hero.fightStats.copy(
        energy = (res.hero.fightStats.energy - spent).max(0L))))
      applyAura(paid, aura, spent,
        Vector(content.format(s"battle.aura.${aura.key}.holds", "energy" -> spent.toString)))
    }

  /** Сама аура: каждому на поле по его природе — герою, союзникам и чужой
    * стороне. Энергию держатель к этому моменту уже заплатил, `head` — строка о
    * самой ауре. Своё герой видит отдельной строкой, остальные — молча: в
    * групповом бою это были бы двадцать строк. */
  private def applyAura(res: TurnResult, aura: Aura, spent: Long, head: Vector[String]): TurnResult = {
    def power(maxHp: Long): Long = aura.power(maxHp, spent)

    // ── Герой ─────────────────────────────────────────────────────────────────
    val heroNature = if (res.hero.sets.heroIsUndead) Nature.Undead else Nature.of(res.hero.race)
    // Лежачего аура не трогает — ни лечит, ни жжёт: встанет он, когда отряд
    // отобьётся, а до тех пор его здоровье — ноль.
    val heroSign   = if (res.battle.group.heroDown) 0 else aura.sign(heroNature)
    val maxHp      = res.hero.effectiveMaxHp(0L)
    val heroHp     = heroSign match {
      // Проклятого Некромантом аура не лечит (см. HealCurse).
      case s if s > 0 && res.battle.effects.heroCursed => res.hero.fightStats.hp
      // Лечение не опускает: текущее HP выше потолка аура не срезает.
      case s if s > 0 => (res.hero.fightStats.hp + power(maxHp)).min(maxHp).max(res.hero.fightStats.hp)
      case s if s < 0 => (res.hero.fightStats.hp - power(maxHp)).max(0L)
      case _          => res.hero.fightStats.hp
    }
    val heroDelta = heroHp - res.hero.fightStats.hp
    val hero1     = res.hero.copy(fightStats = res.hero.fightStats.copy(hp = heroHp))

    // ── Союзники: та же мера, по их природе ───────────────────────────────────
    val allies = res.battle.group.allies.map { a =>
      val sign = if (!a.alive) 0 else aura.sign(Nature.of(a.kind.race))
      sign match {
        case s if s > 0 && a.healBlocked => a
        case s if s > 0 => a.copy(hp = (a.hp + power(a.stats.hp)).min(a.stats.hp).max(a.hp))
        case s if s < 0 => a.copy(hp = (a.hp - power(a.stats.hp)).max(0L))
        case _          => a
      }
    }
    val fellAllies  = allies.filter(a => !a.alive && res.battle.group.allyAt(a.position).exists(_.alive))
    val withAllies  = res.battle.copy(group = res.battle.group.copy(allies = allies))
    val sweptAllies = fellAllies.foldLeft(withAllies)((b, a) => b.copy(group = b.group.withoutAlly(a.position)))

    // ── Чужая сторона ─────────────────────────────────────────────────────────
    val battle2 = auraOnMonsters(sweptAllies, aura, spent)
    val lines = head ++
      Option.when(heroDelta > 0L)(
        content.format(s"battle.aura.${aura.key}.feeds", "hp" -> heroDelta.toString)).toVector ++
      Option.when(heroDelta < 0L)(
        content.format(s"battle.aura.${aura.key}.burns", "hp" -> (-heroDelta).toString)).toVector ++
      fellAllies.map(a => content.format("battle.ally.auraFell", "name" -> a.name))
    res.copy(hero = hero1, battle = battle2, log = res.log ++ lines)
  }

  /** Аура по чужой стороне: каждому по его природе, павшие уходят в добычу.
    * Строк не даёт — о себе аура сказала одной на всё поле. Минибосса её доля
    * от максимума HP жжёт вдвое слабее (см. `BossPercentDamage`; вложенное
    * героем — как есть), и не больше остатка его предела за раунд (см.
    * `BossRoundCap`) — где бы он ни стоял. `spent` — сколько энергии в неё
    * вложено. */
  private def auraOnMonsters(battle: SoloPveBattle, aura: Aura, spent: Long): SoloPveBattle = {
    def healed(cur: Long, maxHp: Long): Long = (cur + aura.power(maxHp, spent)).min(maxHp).max(cur)
    def harm(maxHp: Long, boss: Boolean): Long =
      (if (boss) BossPercentDamage(aura.pctPart(maxHp)) else aura.pctPart(maxHp)) + spent
    // Активный: его HP живут в полях боя.
    val afterActive = {
      val sign = if (battle.monsterCurrentHp <= 0L) 0 else aura.sign(Nature.of(battle.monsterRace, battle.monsterUndead))
      if (sign > 0) battle.copy(monsterCurrentHp = healed(battle.monsterCurrentHp, battle.monsterStats.hp))
      else if (sign < 0) battle.hurtActive(0L, harm(battle.monsterStats.hp, battle.boss.isDefined)).battle
      else battle
    }
    // Слоты строя: павших убираем с конца, чтобы индексы не плыли.
    val (touched, fallen) = afterActive.group.others.indices.foldLeft((afterActive, List.empty[Int])) {
      case ((b, gone), idx) =>
        val slot = b.group.others(idx)
        val sign = if (!slot.alive) 0 else aura.sign(Nature.of(slot.race, slot.undead))
        val next =
          if (sign > 0) b.copy(group = b.group.copy(others = b.group.others.updated(idx,
                          slot.copy(currentHp = healed(slot.currentHp, slot.stats.hp)))))
          else if (sign < 0) b.hurtSlot(idx, 0L, harm(slot.stats.hp, slot.boss.flatMap(MiniBoss.byName).isDefined))._1
          else b
        (next, if (slot.alive && !next.group.others(idx).alive) idx :: gone else gone)
    }
    fallen.sorted.reverse.foldLeft(touched)((b, idx) => b.sideFallen(idx))
  }

  // ── Союзники ────────────────────────────────────────────────────────────────

  /** Союзники ходят после ответа активного моба, по позициям. Каждый бьёт врага
    * напротив, а если там пусто — ближайшего в досягаемости (сперва моба в паре
    * с героем), стихией своего оружия; сверху — случайное умение из тех, на что
    * хватает энергии и что к месту. Добитый уходит в павшие — и это засчитывается
    * герою. Энергия копится в конце хода. */
  private def allyPhase(res: TurnResult): Task[TurnResult] =
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle.group.allies.isEmpty) ZIO.succeed(res)
    else
      ZIO.foldLeft(res.battle.group.allies.sortBy(_.position))((res.battle, Vector.empty[String])) {
        case ((b, log), a0) =>
          // Каждый ходит раз — свой, найденный по месту: за фазу союзники мест
          // не меняют (шаг к врагу — после неё, см. squadAdvance). По виду
          // искать нельзя: крыс, поднятых, караванщиков бывает по нескольку, и
          // первый из них ходил за всех, а прочие стояли.
          b.group.allyAt(a0.position).filter(x => x.alive && x.kind == a0.kind) match {
            case None    => ZIO.succeed((b, log))
            case Some(a) => allyTurn(a, b).map { case (b2, lines) => (b2, log ++ lines) }
          }
      }.map { case (b, log) =>
        val outcome = if (b.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
        res.copy(battle = b, outcome = outcome, log = res.log ++ log)
      }

  private def allyTurn(a: BattleAlly, battle: SoloPveBattle): Task[(SoloPveBattle, Vector[String])] =
    for {
      // 1) обычная атака по цели
      struck <- battle.allyTarget(a) match {
        case None      => ZIO.succeed(AllyBlow(battle, None, 0L, slew = false))
        case Some(pos) => allyHits(a, battle, pos, a.stats.atk, crushing = false)
      }
      b1 = struck.battle
      // 2) случайное умение из тех, что по карману и к месту — у него самого, по
      // месту: таких же по виду в строю может быть несколько
      a1 = b1.group.allyAt(a.position).getOrElse(a)
      target2 = b1.allyTarget(a1)
      // Крыса умений не знает — только кусает (см. AllyKind.usesSkills), а
      // Тёмный волк — уникальный, не наёмник (см. UndeadForm.unique).
      // Удар попадает в выбор только вместе с целью — без неё его и нет.
      options = if (!a1.usesSkills) Nil else AllySkill.values.toList.flatMap {
        case AllySkill.QuickStrike     => target2.map(BattleState.AllyCast.Strike(_, crushing = false))
        case AllySkill.CrushingStrike  => target2.map(BattleState.AllyCast.Strike(_, crushing = true))
        // Под проклятием Некроманта фляга его не вылечит — он её и не тратит.
        case AllySkill.HealingFlask    => Option.when(a1.hp < a1.stats.hp && !a1.healBlocked)(BattleState.AllyCast.Flask)
        case AllySkill.EmergencyRepair => Option.when(a1.armor < a1.stats.armor)(BattleState.AllyCast.Repair)
      }.filter(_.skill.cost(a1.lvl) <= a1.energy)
      casted <-
        if (options.isEmpty) ZIO.succeed((AllyBlow(b1, None, 0L, slew = false), Vector.empty[String]))
        else Random.nextIntBetween(0, options.size).flatMap { i =>
          val cast = options(i)
          val paid = b1.copy(group = b1.group.updateAlly(a1.position)(x => x.copy(energy = x.energy - cast.skill.cost(a1.lvl))))
          // Яд красного сетеплёта: умение срывается — энергия ушла, умения нет.
          if (a1.skillJammed) {
            val jamKind = a1.skillJam.orElse(Some(SkillJam.Spider))
            ZIO.succeed((AllyBlow(paid.copy(group = paid.group.updateAlly(a1.position)(_.copy(skillJammed = false, skillJam = None))),
              None, 0L, slew = false), Vector(allySkillJamLine(a1.name, jamKind))))
          }
          else cast match {
            case BattleState.AllyCast.Strike(target, crushing) =>
              val factor = if (crushing) AllySkill.CrushingStrikeFactor else AllySkill.QuickStrikeFactor
              allyHits(a1, paid, target, (a1.stats.atk * factor).toLong.max(1L),
                crushing = crushing, sure = true).map(_ -> Vector.empty[String])
            case BattleState.AllyCast.Flask =>
              val heal = (a1.stats.hp * AllySkill.HealPct / 100L).max(1L)
              val newHp = (a1.hp + heal).min(a1.stats.hp)
              // Кровь и пламя фляга снимает целиком, яд ослабляет — как у мобов.
              ZIO.succeed((AllyBlow(paid.copy(group = paid.group.updateAlly(a1.position)(
                  x => x.copy(hp = newHp, poison = x.poison.flatMap(_.weakenedByHeal), bleed = None, burn = None))),
                  None, 0L, slew = false),
                Vector(content.format("battle.ally.healingFlask", "name" -> a1.name, "hp" -> (newHp - a1.hp).toString))))
            case BattleState.AllyCast.Repair =>
              val fix = (a1.stats.armor * AllySkill.RepairPct / 100L).max(1L)
              val newArmor = (a1.armor + fix).min(a1.stats.armor)
              ZIO.succeed((AllyBlow(paid.copy(group = paid.group.updateAlly(a1.position)(_.copy(armor = newArmor))), None, 0L, slew = false),
                Vector(content.format("battle.ally.emergencyRepair", "name" -> a1.name, "armor" -> (newArmor - a1.armor).toString))))
          }
        }
      (blow2, aidLog) = casted
      b2 = blow2.battle
      // 3) энергия копится
      b3 = b2.copy(group = b2.group.updateAlly(a.position)(x =>
             x.copy(energy = (x.energy + x.kind.energyRegen(x.lvl, x.grown)).min(x.stats.energy))))
      // Лог — одной строкой на цель, с суммарным уроном удара и умения; проки и
      // раны считаются, но экран не засоряют. Промах — только если не попало ничем;
      // попало, но минибосс держит предел за раунд, — так и говорим.
      blows   = List(struck, blow2).flatMap(b => b.target.map(name => (name, b.damage, b.slew)))
      byName  = blows.groupMapReduce(_._1)(_._2)(_ + _)
      heldOn  = List(struck, blow2).filter(_.held).flatMap(_.target).toSet
      // Яд от удара — пометкой в той же строке, как у мобов (см. poisonMark).
      venomOn = List(struck, blow2).filter(_.poisoned).flatMap(_.target).toSet
      order   = blows.map(_._1).distinct
      hitLog  = order.map { m =>
        val dmg = byName(m)
        if (dmg > 0L) content.format("battle.ally.hit", "name" -> a.name, "monster" -> m, "damage" -> dmg.toString,
                        "poison" -> poisonMark(venomOn(m)))
        else if (heldOn(m)) content.format("battle.ally.held", "name" -> a.name, "monster" -> m)
        else content.format("battle.ally.miss", "name" -> a.name, "monster" -> m)
      }.toVector
      slewLog = blows.collect { case (m, _, true) => content.format("battle.ally.slew", "name" -> a.name, "monster" -> m) }.toVector
      // Добитые — ему в счёт, и стоявший в полях тоже (строки «добивает» у того
      // нет: его смерть — победа или промоут): по ним сводка доигранного боя.
      finished = List(struck, blow2).count(_.killed)
      b4 = if (finished == 0) b3
           else b3.copy(group = b3.group.updateAlly(a.position)(x => x.copy(kills = x.kills + finished)))
    } yield (b4, hitLog ++ aidLog ++ slewLog)

  /** Удар союзника по мобу на месте `pos`: бросок на попадание против уклонения
    * моба, разброс ±20%, защита моба (кроме дробящего — тот идёт прямо в HP),
    * сопротивление минибосса стихии, грани стихии по броне и HP, прок стихии
    * 30%. Моб вне пары правится через временную пару. Добитый — в павшие. */
  private def allyHits(
      a: BattleAlly, battle: SoloPveBattle, pos: Int, raw: Long, crushing: Boolean, sure: Boolean = false
  ): Task[AllyBlow] = {
    val active = pos == battle.group.activePos
    val idx    = battle.group.idxOf(pos)
    val tmp    = if (active) battle else battle.withActive(battle.group.others(idx))
    val elem   = a.element
    for {
      // Умения (`sure`) не промахиваются — как и у героя; бросок не тратится.
      hitRoll <- if (sure) ZIO.succeed(101) else Random.nextIntBetween(1, 101)
      dodge    = mobDodgeChance(a.stats.accuracy, tmp)
      out <-
        if (hitRoll <= dodge) ZIO.succeed(AllyBlow(battle, Some(tmp.monsterName), 0L, slew = false))
        else for {
          spread <- Random.nextLongBetween(80L, 121L)
          // Лучник вблизи бьёт слабее, как лук героя (см. allyCloseMod).
          base    = (raw * spread / 100L * BattleState.allyCloseMod(a, pos)).toLong.max(1L)
          // Крыса бьёт зубами, без стихии: грани урона у неё обычные, а боссовое
          // сопротивление — как у голой стали (см. AllyKind.element).
          resist  = tmp.boss.map(b => elem.fold(b.plainDamageTakenMult)(b.damageTakenMult)).getOrElse(1.0)
          dmg     = (base * resist).toLong.max(1L)
          (armorRaw, hpRaw) = if (crushing) (0L, dmg) else {
            val cut       = (dmg * (1.0 - BattleState.defenceReduction(effectiveMonsterDefence(tmp), 0L, a.stats.atk))).toLong.max(1L)
            val rawArmor  = math.min(tmp.monsterCurrentArmor, cut)
            val rawHp     = cut - rawArmor
            val armorMult = elem.map(_.armorMult).getOrElse(1.0)
            val hpMult    = elem.map(_.hpMult).getOrElse(1.0)
            val aDmg      = math.min(tmp.monsterCurrentArmor, (rawArmor * armorMult).toLong).max(0L)
            val lightning = if (elem.contains(Element.Lightning)) (aDmg * Element.Lightning.ArmorToHpFrac).toLong else 0L
            (aDmg, ((rawHp * hpMult).toLong + lightning).max(0L))
          }
          // Предел минибосса за раунд общий для всех, кто его бьёт: и для героя,
          // и для отряда (см. BossRoundCap).
          hurt     = tmp.hurtActive(armorRaw, hpRaw)
          newHp    = hurt.battle.monsterCurrentHp
          // Кто бил последним, запоминаем: истечёт моб ранами — убит им.
          hit      = hurt.battle.copy(monsterLastAllyHit = Some(a.name))
          dealt    = hurt.dealt
          // Прок стихии считается молча — строки его в лог союзника не идут.
          procRoll <- if (newHp > 0L) Random.nextIntBetween(1, 101) else ZIO.succeed(100)
          procced  = if (newHp > 0L && procRoll <= Element.ProcChancePct)
                       applyProcs(hit, elem.toSet, Element.Air.ProcTurns, BattleState.ProcMods(Element.lightningBurn(hurt.hp)))._1
                     else hit
          // Яд на наконечниках (Фаэлин): удар, дошедший до HP, травит через раз.
          // Бросок — только у того, кто травит, и только если есть кого.
          venomRoll <- if (a.kind.poisonPct > 0L && hurt.hp > 0L && newHp > 0L) Random.nextIntBetween(1, 101)
                       else ZIO.succeed(101)
          venoms    = venomRoll <= a.kind.poisonPct
          venomed   = if (!venoms) procced
                      else procced.copy(effects = procced.effects.copy(monsterPoison =
                        Some(procced.effects.monsterPoison.map(_.stacked).getOrElse(Poison.onHit))))
          // Иммунитеты цели — в withEffects: нежить яд не берёт, и пометки тогда нет.
          guarded  = venomed.withEffects(venomed.effects)
          poisoned = venoms && guarded.effects.monsterPoison.isDefined
          // Этим ударом добил — был жив до него, а после него нет.
          killed   = tmp.monsterCurrentHp > 0L && guarded.monsterCurrentHp <= 0L
          // Удар дошёл, но предел минибосса съел его целиком: это не промах.
          held     = hurt.absorbed
          // обратно: активный — как есть, слот — в строй; добитый слот — в павшие
          result =
            if (active) AllyBlow(guarded, Some(tmp.monsterName), dealt, slew = false, killed = killed, held = held,
                          poisoned = poisoned)
            else {
              // Счёт предела минибосса — на его слоте: возвращается в строй вместе с ним.
              val restored = battle.copy(group = battle.group.copy(others = battle.group.others.updated(idx, guarded.activeSlot)))
              if (guarded.monsterCurrentHp <= 0L) AllyBlow(restored.sideFallen(idx), Some(tmp.monsterName), dealt, slew = true, killed = killed)
              else AllyBlow(restored, Some(tmp.monsterName), dealt, slew = false, held = held, poisoned = poisoned)
            }
        } yield result
    } yield out
  }

  /** Моб бьёт повозку напротив (засада на караван): увернуться ей нечем — удар
    * доходит всегда, с тем же разбросом, что по союзнику; сперва броня, остаток —
    * в HP. С шансом засады удар поджигает (см. Convoy.ignitePct). Разбитая
    * повозка из обоза выбывает. */
  private def mobStrikesWagon(w: Wagon, tmp: SoloPveBattle): Task[(SoloPveBattle, Vector[String])] =
    tmp.group.convoy.fold[Task[(SoloPveBattle, Vector[String])]](ZIO.succeed((tmp, Vector.empty[String]))) { convoy =>
      for {
        spread <- Random.nextLongBetween(80L, 121L)
        damage  = (monsterAttack(tmp) * spread / 100L).max(1L)
        struck  = w.struck(damage)
        fire   <- chanceRoll(struck.alive && convoy.ignitePct > 0L, convoy.ignitePct)
        after   = if (fire) struck.ignited else struck
        line    = content.format("battle.group.wagonHit", "monster" -> tmp.monsterName, "n" -> w.position.toString,
                    "damage" -> damage.toString, "fire" -> (if (fire) content.text("battle.group.wagonFire") else ""))
        broken  = Option.when(!after.alive)(content.format("battle.group.wagonBroken", "n" -> w.position.toString))
      } yield (tmp.copy(group = tmp.group.copy(convoy = Some(convoy.updated(after)))), Vector(line) ++ broken.toVector)
    }

  /** Моб бьёт союзника: бросок против уклонения союзника, урон с разбросом,
    * защита союзника (холод её обгрызает), броня — потом HP, у каменного —
    * раскол. Сверху — всё то же, что достаётся от удара герою: отравленное
    * оружие травит (мурлок, эльф, Крысиный король), огонь поджигает, холод
    * сковывает, воздух подхватывает самого моба; огонь и холод гасят друг
    * друга. Обнулённый союзник уходит по свитку. */
  private def mobStrikesAlly(a: BattleAlly, tmp: SoloPveBattle): Task[(SoloPveBattle, Vector[String])] =
    for {
      hitRoll <- Random.nextIntBetween(1, 101)
      dodge    = BattleState.dodgeChance(0L, a.stats.evasion, a.stats.defence, tmp.monsterStats.accuracy)
      out <-
        if (hitRoll <= dodge) ZIO.succeed((tmp, Vector(content.format("battle.ally.mobMiss", "monster" -> tmp.monsterName, "name" -> a.name))))
        else for {
          spread <- Random.nextLongBetween(80L, 121L)
          raw      = (monsterAttack(tmp) * spread / 100L).max(1L)
          reduced  = (raw * (1.0 - allyReduction(a, tmp.monsterStats.atk))).toLong.max(1L)
          (armorDmg, hpDmg) = allyHitSplit(tmp, a, reduced)
          // Те же броски, что при ударе по герою (см. plainMobStrike).
          igniteChance = tmp.boss.map(_.heroIgniteChancePct)
                           .getOrElse(if (powderElement(tmp).contains(Element.Fire)) Element.ProcChancePct else 0L)
          ignites <- chanceRoll(igniteChance > 0L && !tmp.effects.chilled, igniteChance)
          burnPct      = tmp.boss.map(_.heroIgniteBurnPct).getOrElse(Burn.Initial)
          procElement  = powderElement(tmp).filter(e => e == Element.Cold || e == Element.Air || e == Element.Lightning)
          mobProc <- chanceRoll(procElement.isDefined, Element.ProcChancePct)
          // Молния выжигает энергию и союзнику — так же, как герою.
          zapped   = if (mobProc && procElement.contains(Element.Lightning))
                       Element.lightningBurn(hpDmg).min(a.energy.max(0L))
                     else 0L
          bladeHits <- chanceRoll(hpDmg > 0L && !tmp.effects.monsterPoisonsOnHit &&
                         Race.withNameOption(tmp.monsterRace).exists(_.weaponPoison), Poison.WeaponChancePct)
          redPoison <- chanceRoll(hpDmg > 0L && RedSpider.is(tmp.monsterBreed), RedSpider.PoisonPct)
          poisons  = hpDmg > 0L && (tmp.effects.monsterPoisonsOnHit || bladeHits || redPoison)
          jams <- chanceRoll(redPoison, RedSpider.JamPct)
          // Паутина — и по союзнику: у него ловкости нет, вместо неё его уровень (см. SpiderWeb).
          webs <- chanceRoll(SpiderWeb.weaves(tmp.monsterBreed),
                    SpiderWeb.chancePct(tmp.monsterLvl, a.lvl, a.stats.evasion, a.stats.defence))
          thawed   = ignites && a.cold
          bites    = mobProc && procElement.contains(Element.Cold)
          quenched = bites && (a.burn.isDefined || (ignites && !thawed))
          airy     = mobProc && procElement.contains(Element.Air)
          struck   = a.copy(armor = a.armor - armorDmg, hp = (a.hp - hpDmg).max(0L), lastHitBy = Some(tmp.monsterName),
                       energy = (a.energy - zapped).max(0L))
          burned   =
            if (!ignites) struck
            else if (thawed) struck.copy(coldCut = 0)
            else struck.copy(burn = Some(struck.burn.map(_.reignited).getOrElse(Burn(burnPct))))
          poisoned = if (poisons) burned.copy(poison = Some(burned.poison.map(_.stacked).getOrElse(Poison.onHit))) else burned
          hurt0    =
            if (quenched) poisoned.copy(burn = None)
            else if (bites) poisoned.copy(coldCut = poisoned.coldCut + Element.Cold.DefenceReductionCut)
            else poisoned
          hurt     = {
            val webbedA = if (webs) hurt0.copy(webbed = SpiderWeb.Turns) else hurt0
            if (jams) webbedA.copy(skillJammed = true, skillJam = Some(SkillJam.Spider)) else webbedA
          }
          after    = if (airy) tmp.copy(effects = tmp.effects.copy(mobAirBoostTurns = Element.Air.ProcTurns)) else tmp
          line     = content.format("battle.ally.mobHit", "monster" -> tmp.monsterName, "name" -> a.name,
                       "damage" -> (armorDmg + hpDmg).toString, "poison" -> (poisonMark(poisons) + webMark(webs)))
          extras   = List(
            Option.when(ignites && !thawed)(content.format("battle.ally.ignites", "name" -> a.name)),
            Option.when(thawed)(content.format("battle.ally.thaw", "name" -> a.name)),
            Option.when(bites && !quenched)(content.format("battle.ally.coldBite", "name" -> a.name,
              "cut" -> Element.Cold.DefenceReductionCut.toString)),
            Option.when(quenched)(content.format("battle.ally.quench", "name" -> a.name)),
            Option.when(airy)(airBoostLine(tmp.monsterName)),
            Option.when(zapped > 0L)(content.format("battle.ally.lightningBurn", "name" -> a.name,
              "energy" -> zapped.toString))
          ).flatten
          result   =
            if (hurt.alive) (after.copy(group = after.group.updateAlly(a.position)(_ => hurt)), line +: extras.toVector)
            // В павшие — уже битым: с нулём HP и тем, кто его выбил (сводка
            // доигранного боя читает это оттуда).
            else (after.copy(group = after.group.updateAlly(a.position)(_ => hurt).withoutAlly(a.position)),
                  // Наёмника уносит свиток, а поднятого с алтаря выносить
                  // нечем: он рассыпается там же, где стоял (см. goneKey).
                  Vector(line, content.format(BattleState.goneKey(a), "name" -> a.name)))
        } yield result
    } yield out

  /** Союзник, которого умение моба или минибосса обнулило, уходит из строя так
    * же, как от удара (см. [[mobStrikesAlly]]): наёмника уносит свиток,
    * поднятый рассыпается. Раньше такой оставался лежать в строю с нулём HP, а
    * после боя возвращался в отряд и вставал с первым же отдыхом. */
  private def sweepFallenAlly(battle: SoloPveBattle, ally: Option[BattleAlly]): (SoloPveBattle, Option[String]) =
    ally.flatMap(a => battle.group.allyAt(a.position)).filter(!_.alive) match {
      case Some(a) =>
        (battle.copy(group = battle.group.withoutAlly(a.position)),
          Some(content.format(BattleState.goneKey(a), "name" -> a.name)))
      case None => (battle, None)
    }

  /** Отряд после боя: состояние союзников из боя, выбывшие — кто в отлучку,
    * кто насовсем. Наёмника уносит свиток на сутки, а поднятый с алтаря
    * рассыпается прахом — возвращаться ему неоткуда. Крыса, выскочившая в бою
    * из-под ног (её признак — `home = 0`, в отряде её ещё нет), остаётся при
    * герое на [[pangea.model.squad.AllyRates.RatMs]]. */
  private def squadAfterBattle(hero: Hero, battle: SoloPveBattle, nowMs: Long): pangea.model.squad.Squad = {
    val (joined, fromSquad) = battle.group.allies.partition(_.home <= 0)
    // Павших убираем первыми, по местам в отряде (`home`): их места пустеют,
    // остальные стоят где стояли.
    val (raised, hired) = battle.group.alliesGone.partition(_.undead.isDefined)
    val left = hired.foldLeft(hero.squad.dismissAll(raised.map(_.home).toSet))((s, a) => s.sentAway(a.kind, nowMs))
    // В бою строй съезжал к месту 1 (Squad.frontShift) — в отряд все
    // возвращаются на столько же мест дальше: выбранная расстановка не сбивается.
    // Позиции могли поменяться за бой (Таран, «Переместиться») — вместе с героем;
    // за край строя при этом не выходим. Кто кончил бой в хвосте чужого строя
    // (ходят до Formation.WalkPlaces), сперва встаёт на свободное место отряда
    // (см. BattleState.homePlaces).
    val placed    = BattleState.homePlaces(
                      fromSquad.map(a => a.home -> a.toAlly.position).toMap + (BattleState.HeroKey -> battle.group.heroPos))
    val heroAt    = placed.getOrElse(BattleState.HeroKey, battle.group.heroPos)
    val battleEnd = placed.values.max
    val back      = hero.squad.frontShift.min(pangea.model.squad.AllyRates.Positions - battleEnd).max(0)
    val byHome    = fromSquad.map(a => a.home -> a).toMap
    // Отряда в бою не было — сюжетный бой один на один (девушка, охота Марисы):
    // герой там всегда на месте 1, и о строе это ничего не говорит. Строй не
    // трогаем — иначе герой вставал на 1, где уже стоит союзник.
    val fought  = fromSquad.nonEmpty || battle.group.alliesGone.nonEmpty
    val synced0 =
      if (!fought) left
      else left.copy(
        heroPos = heroAt + back,
        allies  = left.allies.map { old =>
          byHome.get(old.position).fold(old) { a =>
            val after = a.toAlly
            after.copy(position = placed.getOrElse(a.home, after.position) + back, hiredUntil = old.hiredUntil)
          }
        })
    // Места боя → места отряда. Строй отряда в бой входил сдвинутым к первому
    // месту (frontShift) — обратно на столько же, как у всех из отряда (`back`);
    // отряд в бою не бился — сдвиг тот, с каким бой начался.
    val toSquad = if (fought) back else hero.squad.frontShift
    // Выжившие, пришедшие не из отряда, — в отряд: крыса на сутки — туда, где
    // стояла в бою, — восставший на двое. Крысе мест не нашлось — разбежится,
    // восставший будет ждать ответа, кем за него пожертвовать (см.
    // Squad.admitRisen).
    val synced = joined.filter(_.alive).foldLeft(synced0) { (s, a) =>
      a.undead.fold(s) { form =>
        a.kind match {
          // Дружинники «Пропавшей дружины» шли только на этот бой — после него
          // они уходят наверх со своими, в отряд не встают.
          case pangea.model.squad.AllyKind.SquadGnome => s
          // Мурлок-собрат выходил из воды только на этот бой — туда и уходит.
          case pangea.model.squad.AllyKind.MurlocKin  => s
          // Бывший пленник с заставы и так стоит в отряде, восставшим он не станет.
          case pangea.model.squad.AllyKind.FreedCaptive => s
          // Элиар пел изгороди только этот бой — с ней и остаётся.
          case pangea.model.squad.AllyKind.HedgeElf   => s
          // Капитаны каравана бились за собрата только этот бой — и уходят к своим.
          case pangea.model.squad.AllyKind.CaravanKhajiit => s
          // Караванщики деревни бились за свой обоз — с ним и остаются.
          case pangea.model.squad.AllyKind.CaravanGuard => s
          // Горн с искателями стерегли подвал совета, Барти дрался у себя дома, —
          // после боя они остаются там же.
          case pangea.model.squad.AllyKind.GuildMaster => s
          case pangea.model.squad.AllyKind.GuildSeeker => s
          case pangea.model.squad.AllyKind.Informant   => s
          // Асессор совета шёл на штурм Загона — после него возвращается к бумагам.
          case pangea.model.squad.AllyKind.CouncilEnvoy => s
          // Э. встал рядом на одну ночь — и уходит по своим делам.
          case pangea.model.squad.AllyKind.BrotherhoodAgent => s
          // Сойка спрыгнула с крыши на одну драку — и уходит по крышам.
          case pangea.model.squad.AllyKind.Sojka => s
          case pangea.model.squad.AllyKind.Rat        => s.summonRat(form, a.lvl, nowMs, near = a.position + toSquad)
          case _                                      => s.admitRisen(form, a.lvl, nowMs)
        }
      }
    }
    synced
  }

  /** Ход мобов вне пары. Сосед героя (номер 2) достаёт его сбоку: обычная
    * атака, а поверх — умение, как у моба в паре; лечащее умение он отдаёт
    * раненому соседу, если сам цел. Мобы дальше по строю героя не достают —
    * копят энергию и лечат соседей. Раны на них (яд, кровь, огонь) тикают как у
    * всех; кого добило — уходит в павшие. */
  private def sideMobsPhase(res: TurnResult, nowMs: Long): Task[TurnResult] =
    if (res.outcome != Outcome.Continue || !(res.battle.isGroup || res.battle.unpaired || res.battle.group.heroDown) ||
        !res.endsRound) ZIO.succeed(res)
    else {
      val smokeLine = if (res.battle.effects.heroInSmoke) Vector(content.text("battle.flaskSmokeHolds")) else Vector.empty[String]
      // Ходят те, кто стоял в строю к началу фазы, — ищем их по местам: павший
      // смыкает ряды, призванный минибоссом встаёт в строй, и индексы плывут.
      // Пришедшие в этом раунде не бьют — они только что встали.
      // Кого добил в ход моба сам герой (раны, шипы, контрудар) — его добитые:
      // пир, напор, «Некромант» тут же, до хода следующего (см. killsInMobTurn).
      ZIO.foldLeft(res.battle.group.places)((res.hero, res.battle, Vector.empty[String])) {
        case ((h, b, log), place) =>
          val idx = b.group.idxOf(place)
          // Герой пал прямо сейчас — мобы замирают; лежит с прошлого раунда — бьют отряд.
          if ((h.fightStats.hp <= 0 && !b.group.heroDown) || idx < 0) ZIO.succeed((h, b, log))
          else freeMobTurn(h, b, idx, nowMs).flatMap { case (h2, b2, lines) =>
            killsInMobTurn(b, h2, b2, nowMs).map { case (h3, b3, fed) => (h3, b3, log ++ lines ++ fed) }
          }
      }.flatMap { case (h, b, log) =>
        // Активный вне пары ходит последним — как моб вне пары; при лежащем
        // герое так же ходит и тот, что в паре: отвечать ему некому.
        if (b.group.paired && !b.group.heroDown) ZIO.succeed((h, b, log))
        else if (h.fightStats.hp <= 0 && !b.group.heroDown) ZIO.succeed((h, b, log))
        else unpairedActiveTurn(h, b, nowMs).flatMap { case (h2, b2, lines) =>
          killsInMobTurn(b, h2, b2, nowMs).map { case (h3, b3, fed) => (h3, b3, log ++ lines ++ fed) }
        }
      }.map { case (h, b, log) =>
        val outcome =
          if (h.fightStats.hp <= 0 && !b.group.heroDown) Outcome.Death
          else if ((b.unpaired || b.group.heroDown) && b.monsterCurrentHp <= 0) Outcome.Victory   // истёк ранами — дальше промоут
          else Outcome.Continue
        res.copy(hero = h, battle = b, outcome = outcome, sideLog = res.sideLog ++ smokeLine ++ log)
      }
    }

  /** Один моб вне пары, `idx` — его индекс в `others`; достаёт героя, если его
    * место — соседнее. Минибосс ходит иначе — см. [[bossSlotTurn]]. */
  private def freeMobTurn(hero: Hero, battle: SoloPveBattle, idx: Int, nowMs: Long): Task[(Hero, SoloPveBattle, Vector[String])] = {
    val slot = battle.group.others(idx)
    val pos  = battle.group.posOf(idx)
    if (slot.boss.isDefined) bossSlotTurn(hero, battle, pos, nowMs)
    else {
      // Временный бой: этот моб — «активный», геройская половина эффектов та же;
      // его временные эффекты тикают здесь (общий tickBuffs крутит только пару).
      val tmp0 = battle.withActive(slot)
      val tmp  = tmp0.copy(effects = tickSlotEffects(tmp0.effects))
      freeTurnOf(hero, battle, tmp, pos, nowMs, realActive = false).map { case (h2, b2, ticked, log3) =>
        // Обратно в строй — или в павшие, если раны добили. Союзники правились во
        // временном бою (удар по стоящему напротив) — забираем их оттуда.
        val afterSlot = ticked.activeSlot
        val merged = b2.copy(effects = ticked.effects.withMonsterPart(b2.effects),
                             group = b2.group.copy(allies = ticked.group.allies, alliesGone = ticked.group.alliesGone,
                               convoy = ticked.group.convoy))
        val placed = merged.copy(group = merged.group.copy(others = merged.group.others.updated(idx, afterSlot)))
        if (afterSlot.alive) (h2, placed, log3)
        else (h2, placed.sideFallen(idx), log3 :+ content.format("battle.group.sideFell", "monster" -> afterSlot.name))
      }
    }
  }

  /** Минибосс, стоящий в строю, а не в полях боя. Его круг едет с ним в слоте
    * (см. `SoloPveBattle.withActive`), а приёмы меняют сам строй — зов крыс,
    * созидание, шаг за спины своих. Поэтому на свой ход он встаёт в поля
    * (как под удар героя, см.
    * [[strikeSide]]), ходит как тот, кто там стоит, и возвращается в строй.
    * Раньше его ход шёл во временном бою, и всё это пропадало: король звал крыс
    * впустую, а Некромант не колдовал вовсе. Истёк ранами — в павшие. */
  private def bossSlotTurn(hero: Hero, battle: SoloPveBattle, pos: Int, nowMs: Long): Task[(Hero, SoloPveBattle, Vector[String])] = {
    val home    = battle.group.activePos
    val engaged = battle.engage(pos)
    // Его временные эффекты тикают здесь, как у всякого моба строя.
    val tmp     = engaged.copy(effects = tickSlotEffects(engaged.effects))
    freeTurnOf(hero, tmp, tmp, pos, nowMs, realActive = false).map { case (h2, b2, ticked, log3) =>
      val merged = mergeFieldsTurn(b2, ticked)
      val boss   = merged.activeSlot
      val at     = merged.group.activePos
      val back   = merged.engage(home)
      if (boss.alive) (h2, back, log3)
      else (h2, back.sideFallen(back.group.idxOf(at)), log3 :+ content.format("battle.group.sideFell", "monster" -> boss.name))
    }
  }

  /** Активный моб, стоящий не напротив героя: ходит как моб вне пары, прямо в
    * полях (его временные эффекты уже оттикали в `tickBuffs`). Истёк ранами —
    * остаётся в полях с нулём HP, промоут дальше по цепочке. */
  private def unpairedActiveTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[(Hero, SoloPveBattle, Vector[String])] =
    freeTurnOf(hero, battle, battle, battle.group.activePos, nowMs, realActive = true).map { case (h2, b2, ticked, log3) =>
      (h2, mergeFieldsTurn(b2, ticked), log3)
    }

  /** Свести ход того, кто стоит в полях боя (см. [[freeTurnOf]]). Лечение соседа
    * правит `battle`-копию, и вылеченного надо забрать оттуда. Но ход минибосса
    * мог изменить и сам строй — зов крыс, поглощение своего; тогда главный он:
    * иначе места остались бы от нового строя, а слоты — от старого, и бой падал
    * бы на первом же обращении к месту. */
  private def mergeFieldsTurn(b2: SoloPveBattle, ticked: SoloPveBattle): SoloPveBattle = {
    val group =
      if (b2.group.others.sizeCompare(ticked.group.others) == 0)
        ticked.group.copy(others = b2.group.others)
      else ticked.group
    ticked.copy(group = group)
  }

  /** Ход моба вне пары, развёрнутого в поля `tmp` на месте `pos`: удар, умение,
    * энергия, тик ран. Бьёт он союзника напротив; нет его — героя, если
    * достаёт; не достаёт — ближайшего союзника в досягаемости: союзника — по тем
    * же правилам, что и героя, сбоку, а эльф — за три места. `realActive` —
    * ходит ли тот, кто стоит в полях по-настоящему, а не встал туда на свой ход.
    * Возвращает героя, `battle` с возможным лечением соседа и `tmp` после хода;
    * собрать это обратно — дело вызывающего. */
  private def freeTurnOf(
      hero: Hero, battle: SoloPveBattle, tmp: SoloPveBattle, pos: Int, nowMs: Long, realActive: Boolean
  ): Task[(Hero, SoloPveBattle, SoloPveBattle, Vector[String])] = {
    // В дыму (дымная фляга) мобы вне пары не видят ни героя, ни друг друга:
    // ни удара сбоку, ни лечения соседу — только раны тикают и энергия копится.
    val smoke       = battle.effects.heroInSmoke
    // Стрелок на башне достаёт любое место и бьёт того, кто к нему ближе.
    // В дыму он не стреляет вовсе: цели не видно (см. дымную флягу).
    val tower       = BattleState.isTower(tmp)
    val reach       = RaceTactics.reachOf(Race.withName(tmp.monsterRace), tmp.boss)
    // Напротив стоит союзник — моб занят им и до героя не тянется; у башни
    // «напротив» нет, поэтому ей ищем ближайшую цель по всему строю.
    val opposite    =
      if (smoke) None
      else if (tower) BattleState.towerTarget(battle, pos)
      else battle.group.allyAt(pos).filter(_.alive)
    // Засада на караван: напротив непокрытая повозка — моб бьёт её (ему нужен
    // товар), а не тянется к герою и союзникам. Ищейки Сноходцев повозок не
    // трогают (см. GroupState.wagonBait).
    val wagon       = if (smoke || tower || opposite.isDefined) None else battle.group.wagonBait(pos)
    // Лежащего героя мобы не добивают — его смерть решится исходом боя. Эльф —
    // стрелок: достаёт героя на три места (см. RaceTactics), прочие — соседа.
    val reachesHero = !smoke && opposite.isEmpty && wagon.isEmpty && !battle.group.heroDown &&
                        (tower || battle.group.inReachOf(pos, reach))
    // Ни напротив никого, ни героя не достать — бьёт союзника, которого достаёт.
    val facing      =
      if (smoke || tower || opposite.isDefined || reachesHero || wagon.isDefined) opposite
      else battle.allyWithin(pos, reach)
    for {
      // 1) удар: по союзнику, которого бьёт, либо сбоку по герою, если достаёт
      struck <-
        (facing, wagon) match {
          case (Some(ally), _) => mobStrikesAlly(ally, tmp).map { case (t2, lines) => (hero, t2, lines) }
          case (None, Some(w)) => mobStrikesWagon(w, tmp).map { case (t2, lines) => (hero, t2, lines) }
          case (None, None) =>
            if (!reachesHero) ZIO.succeed((hero, tmp, Vector.empty[String]))
            else for {
              hitRoll <- Random.nextIntBetween(1, 101)
              dodge    = playerDodgeChance(hero, tmp, nowMs)
              afar     = math.abs(pos - battle.group.heroPos) > GroupState.Reach
              out <-
                if (hitRoll > dodge)
                  mobStrike(hero, tmp, effWithAir(hero, tmp, nowMs), nowMs).map { st =>
                    // Сосед бьёт сбоку, а кто дальше (эльф, Громовой элементаль) — издалека.
                    val line = content.format(if (afar) "battle.group.rangedHit" else "battle.group.sideHit",
                                 "monster" -> tmp.monsterName, "damage" -> st.damage.toString,
                                 "elem" -> (strikeBy(tmp) + heroPoisonMark(st.poisoned)))
                    val head = if (st.mirrored) Vector.empty[String] else Vector(line)
                    (hero.copy(fightStats = hero.fightStats.copy(hp = st.newHp, armor = st.newArmor,
                       energy = (hero.fightStats.energy - st.energyBurnt).max(0L))), st.battle,
                      head ++ st.extraLines)
                  }
                else {
                  val miss = content.format(if (afar) "battle.group.rangedMiss" else "battle.group.sideMiss",
                               "monster" -> tmp.monsterName)
                  // Контрудар и по соседу: увернулся от удара сбоку — бьёт в ответ, наверняка.
                  if (tmp.heroCounterReady)
                    playerStrike(hero, tmp.copy(heroCounterReady = false), nowMs, Vector.empty, Set.empty,
                      retaliate = false, sureHit = true)
                      .map(res => (res.hero, res.battle, Vector(miss, content.text("battle.counter")) ++ res.log))
                  else ZIO.succeed((hero, tmp, Vector(miss)))
                }
            } yield out
        }
      (h0, t1, log0) = struck
      // Контрудар один на раунд: отработал по этому мобу — снят и с самого боя.
      countered = tmp.heroCounterReady && !t1.heroCounterReady
      sideTrauma <- hitTrauma(hero, h0, nowMs)
      (h1, sideTraumaLine) = sideTrauma
      log1 = log0 ++ sideTraumaLine.toVector
      // 2) умение: раненому соседу — лечение, себе — что по карману, герою — урон
      //    (минибосс вне пары идёт по своему кругу). Герой пал прямо сейчас —
      //    мобы замирают; лежит с прошлого раунда — колдуют по отряду, как и бьют
      //    его (см. sideMobsPhase). Раньше у лежачего ноль HP глушил умения всем.
      casted <-
        if (smoke || (h1.fightStats.hp <= 0 && !battle.group.heroDown) || t1.monsterCurrentHp <= 0 ||
            t1.effects.monsterSkillBlockedTurns > 0)
          ZIO.succeed((h1, t1, battle, Vector.empty[String]))
        else allyAid(h1, t1, battle, pos, nowMs).flatMap {
          case Some((t2, b2, line)) => ZIO.succeed((h1, t2, b2, Vector(line)))
          case None =>
            // Кто держит позицию (Некромант), колдует в ход героя (`quietPhase`),
            // если стоит в полях боя: здесь ему ход не достаётся, иначе за раунд
            // вышло бы два. Герой лежит — его хода нет; стоит Некромант в строю —
            // тот ход не его. Тогда он колдует здесь.
            val holdsOwn = t1.boss.exists(_.holdsPosition) && realActive && !battle.group.heroDown
            // Минибосс, который дерётся в строю, идёт по своему кругу и не
            // доставая героя: зов крыс и объединение — про строй, а не про
            // героя, и ждать свободного места напротив ему незачем. Прикрыли его
            // союзником — круг всё равно идёт.
            val inFormation = t1.boss.exists(_.fightsInGroup)
            // Бьёт он того же, кого бил с руки: союзника, а нет его — героя.
            val prey = facing.fold[Victim](Victim.Hero)(a => Victim.Ally(a.position))
            // Прыжок волка — шаг, который он делает и не доставая никого с места:
            // цель он ищет сам, дальше обычного удара, а нет её — прыгает ближе.
            val leaps = t1.boss.exists(_.leapTurn.contains(t1.bossTurn))
            if (!holdsOwn && (reachesHero || inFormation || facing.isDefined || leaps))
              (if (t1.boss.isDefined) bossTurnCast(h1, t1, nowMs, prey) else mobSkillCast(h1, t1, nowMs, prey)).map {
                case (t2, h2, line) => (h2, t2, battle, if (line.isEmpty) Vector.empty else Vector(line))
              }
            else selfAid(h1, t1, nowMs).map { case (t2, line) => (h1, t2, battle, if (line.isEmpty) Vector.empty else Vector(line)) }
        }
      (h2raw, t2cast, b2, log2raw) = casted
      // Союзника добило умение — уходит тем же путём, что и от удара. Бьёт оно
      // не всегда того, кто стоит напротив, — поэтому смотрим всех, кто стоял на
      // ногах до умения (см. sweepCastFallen).
      (t2, goneLines) = sweepCastFallen(t1, t2cast)
      castSideTrauma <- hitTrauma(h1, h2raw, nowMs)
      (h2, castSideLine) = castSideTrauma
      log2 = log2raw ++ goneLines ++ castSideLine.toVector
      // 3) конец хода этого моба: энергия и тик ран
      regen      = mobEnergyGain(h2, t2)
      withEnergy = t2.copy(monsterCurrentEnergy = (t2.monsterCurrentEnergy + regen).min(t2.monsterStats.energy))
      (ticked, dotLine, _) = tickMonsterEffects(withEnergy, withEnergy.monsterName, h2.sets)
      log3 = log1 ++ log2 ++ (if (dotLine.isEmpty) Vector.empty else Vector(dotLine))
    } yield (h2, if (countered) b2.copy(heroCounterReady = false) else b2, ticked, log3)
  }

  /** Временные эффекты на мобе вне пары тикают сами: общий `tickBuffs` крутит
    * только активную пару. */
  private def tickSlotEffects(e: BattleEffects): BattleEffects = e.copy(
    mobAirBoostTurns         = (e.mobAirBoostTurns - 1).max(0),
    monsterSkillBlockedTurns = (e.monsterSkillBlockedTurns - 1).max(0),
    monsterWeakenedTurns     = (e.monsterWeakenedTurns - 1).max(0),
    monsterRootedTurns       = (e.monsterRootedTurns - 1).max(0),
    mobDispersedTurns        = e.mobDispersedTurns.map(_ - 1).filter(_ > 0),
    monsterDefenceDebuff     = e.monsterDefenceDebuff.flatMap(_.ticked)
  )

  /** Сколько энергии враг копит за раунд: минибосс — от своего уровня, моб — от
    * уровня и редкости. «Ужасающий» героя урезает реген, но хоть единицу враг
    * накопит — если копил вообще. */
  private def mobEnergyGain(hero: Hero, b: SoloPveBattle): Long = {
    val base = b.boss.map(_.energyRegen(b.monsterLvl)).getOrElse(MonsterEnergy.regen(b.monsterLvl, b.rarity))
    base.min((base * hero.passives.enemyEnergyRegenMult).toLong.max(1L))
  }

  /** Помощь соседу: если у моба по карману лечение или починка, сам он цел, а
    * сосед по строю (место ±1) ранен — умение уходит соседу, цена списывается
    * с лекаря. Сосед на месте героя — активный моб. */
  private def allyAid(
      hero: Hero, caster: SoloPveBattle, battle: SoloPveBattle, myPos: Int, nowMs: Long
  ): Task[Option[(SoloPveBattle, SoloPveBattle, String)]] = {
    val race       = Race.withName(caster.monsterRace)
    val affordable = MonsterSkill.values.filter(sk =>
      sk.availableTo(race) && sk.cost(caster.monsterLvl) <= caster.monsterCurrentEnergy)
    // Кандидаты: соседи по местам; тот, что в полях боя, — активный.
    val neighbours: List[Either[Unit, Int]] =
      List(myPos - 1, myPos + 1).flatMap { p =>
        if (p == battle.group.activePos) List(Left(()))
        else if (battle.group.occupied(p)) List(Right(battle.group.idxOf(p)))
        else Nil
      }
    def target(n: Either[Unit, Int]): SoloPveBattle = n match {
      case Left(_)  => battle
      case Right(i) => battle.withActive(battle.group.others(i))
    }
    val heals   = affordable.contains(MonsterSkill.HealingFlask) && !MonsterSkill.HealingFlask.applicable(caster)
    val repairs = affordable.contains(MonsterSkill.EmergencyRepair) && !MonsterSkill.EmergencyRepair.applicable(caster)
    val pick: Option[(MonsterSkill, Either[Unit, Int])] =
      (if (heals) neighbours.find(n => MonsterSkill.HealingFlask.applicable(target(n))).map(MonsterSkill.HealingFlask -> _) else None)
        .orElse(if (repairs) neighbours.find(n => MonsterSkill.EmergencyRepair.applicable(target(n))).map(MonsterSkill.EmergencyRepair -> _) else None)
    ZIO.succeed(pick.map { case (skill, n) =>
      val tgt    = target(n)
      val cast   = skill.cast(tgt, hero, nowMs)
      val paid   = caster.copy(monsterCurrentEnergy = (caster.monsterCurrentEnergy - skill.cost(caster.monsterLvl)).max(0L))
      val (line, healed) =
        if (skill == MonsterSkill.HealingFlask)
          (content.format("battle.group.healedAlly", "monster" -> caster.monsterName, "target" -> tgt.monsterName,
            "hp" -> (cast.battle.monsterCurrentHp - tgt.monsterCurrentHp).toString), cast.battle)
        else
          (content.format("battle.group.repairedAlly", "monster" -> caster.monsterName, "target" -> tgt.monsterName,
            "armor" -> (cast.battle.monsterCurrentArmor - tgt.monsterCurrentArmor).toString), cast.battle)
      val updated = n match {
        case Left(_)  => healed.copy(group = battle.group)                          // активный
        case Right(i) => battle.copy(group = battle.group.copy(others = battle.group.others.updated(i, healed.activeSlot)))
      }
      (paid, updated, line)
    })
  }

  /** Матка сетеплётов рождает новорождённых: каждые
    * [[pangea.model.village.VillageRates.MotherBroodEvery]] раунда — по
    * [[pangea.model.village.VillageRates.MotherBroodCount]], уровнем в саму матку,
    * пока в строю есть место (в очередь они не встают). Где бы она ни стояла —
    * в паре или в строю; пала — не рождает. Ни опыта, ни добычи за них нет. */
  private def motherBrood(b: SoloPveBattle): Task[(SoloPveBattle, Vector[String])] = {
    val mother = pangea.model.monster.Breed.SpiderMother
    val rates  = pangea.model.village.VillageRates
    val lvl    = Option.when(b.monsterBreed.contains(mother) && b.monsterCurrentHp > 0L)(b.monsterLvl)
                   .orElse(b.group.others.collectFirst { case s if s.alive && s.breed.contains(mother) => s.lvl })
    val n      = rates.MotherBroodCount.min(b.group.freePlaces.size)
    lvl match {
      case Some(l) if b.group.round % rates.MotherBroodEvery == 0 && n > 0 =>
        ZIO.foreach(List.fill(n)(()))(_ => Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L))
          .map { pcts =>
            val slots = pcts.map { pct =>
              val m = Villages.newborn(l.toInt)
              MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp, m.fightStats.armor,
                m.marked, MonsterEnergy.startEnergy(m.lvl, m.rarity, pct), BattleEffects.empty,
                customName = m.customName, breed = m.breed)
            }
            (slots.foldLeft(b)(_ admit _), Vector(content.format("battle.spiderBrood", "n" -> n.toString)))
          }
      case _ => ZIO.succeed((b, Vector.empty[String]))
    }
  }

  /** Моб, который никого не достаёт, тратит энергию только на себя. */
  private def selfAid(hero: Hero, caster: SoloPveBattle, nowMs: Long): Task[(SoloPveBattle, String)] = {
    val race = Race.withName(caster.monsterRace)
    val own  = List(MonsterSkill.HealingFlask, MonsterSkill.EmergencyRepair).filter(sk =>
      sk.availableTo(race) && sk.applicable(caster) && sk.cost(caster.monsterLvl) <= caster.monsterCurrentEnergy)
    ZIO.succeed(own.headOption match {
      case None     => (caster, "")
      case Some(sk) =>
        val paid = caster.copy(monsterCurrentEnergy = (caster.monsterCurrentEnergy - sk.cost(caster.monsterLvl)).max(0L))
        val cast = sk.cast(paid, hero, nowMs)
        (cast.battle, cast.line)
    })
  }

  /** Конец раунда группы. Ход, который ходом не был (`turn` вернул бой без
    * изменений), сюда не доходит. Порядок: подкрепление → перемешивание каждый
    * четвёртый раунд → отложенный Таран, чтобы перемешивание его не съело →
    * мерцающий настой. */
  private def endRound(before: SoloPveBattle, res: TurnResult): Task[TurnResult] =
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle == before ||
        res.battle.boss.exists(!_.fightsInGroup)) ZIO.succeed(res)
    else {
      val b0 = res.battle.copy(group = res.battle.group.copy(round = res.battle.group.round + 1))
      for {
        // подкрепление: сородич первого моба, редкость как обычно (сюжетный бой — нет)
        comes <- chanceRoll(b0.group.aliveCount < GroupState.MaxMonsters && b0.story.isEmpty && !b0.noKin,
                   GroupState.ReinforcementChancePct)
        withMore <-
          if (!comes) ZIO.succeed((b0, Vector.empty[String]))
          else for {
            seed  <- Random.nextLong
            race   = Race.withName(b0.reinforcementRace)
            (m, _) = MonsterGenerator.generateOfRace(res.hero.dungeonLevel, race, Rng(seed))
            pct   <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
            slot   = MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp,
                       m.fightStats.armor, m.marked, MonsterEnergy.startEnergy(m.lvl, m.rarity, pct), BattleEffects.empty)
            more   = b0.withReinforcement(slot)
            joined = more.copy(group = more.group.copy(originRace = Some(b0.reinforcementRace)))
          } yield (joined, Vector(content.text("battle.group.reinforcement")))
        (b1a, log1a) = withMore
        // призыв: в конце первого раунда легендарные и мифические зовут сородичей
        // (сюжетный бой — нет); кому не хватило места — в очередь за строем
        summoned <-
          if (b1a.group.round != 1 || b1a.story.isDefined || b1a.noKin) ZIO.succeed((b1a, Vector.empty[String]))
          else ZIO.foldLeft(b1a.monstersInOrder.filter(m => BattleState.summons(Rarity.withName(m.rarity))))((b1a, Vector.empty[String])) {
            case ((b, log), caller) => summonKin(b, caller, res.hero.dungeonLevel).map { case (b2, line) => (b2, log :+ line) }
          }
        (b1b, log1b) = summoned
        // очередь: освободилось место — из-за спин выходят следующие
        (b1e, entered) = b1b.admitQueued
        // Матка сетеплётов: каждые три раунда — новорождённые на места, что
        // остались после очереди.
        hatched <- motherBrood(b1e)
        (b1q, log1h) = hatched
        // дальние подтягиваются к герою на одно свободное место — чтобы в
        // следующем раунде достать его сбоку; в сводку это не идёт, перестановка
        // и так видна по строю
        (b1, _) = b1q.closeIn
        log1 = log1a ++ log1b ++ entered.map(m => content.format("battle.group.fromQueue", "monster" -> m.name)).toVector ++ log1h
        // перемешивание: каждый шестой раунд, если есть кого мешать. Жребий
        // тянут только подвижные — сооружения стоят где стояли. Группу берём у
        // УЖЕ перемешанного боя — иначе новый активный встанет поверх старого
        // строя, один моб пропадёт, а другой задвоится. Строй не изменился —
        // молчим: сообщать о перемешивании, которого не было, незачем.
        shuffled <-
          if (!b1.isGroup || b1.group.round % GroupState.ShufflePeriod != 0) ZIO.succeed((b1, Vector.empty[String]))
          else Random.shuffle(b1.movableInOrder.indices.toList).map { order =>
            val mixed = b1.reshuffle(order)
            val b     = mixed.copy(group = mixed.group.copy(pendingMove = None))
            val line  =
              if (b.placesInOrder == b1.placesInOrder) Vector.empty[String]
              else if (b.group.paired) Vector(content.format("battle.group.shuffle", "monster" -> b.monsterName))
              else Vector(content.text("battle.group.shuffleNoPair"))
            (b, line)
          }
        (b2, log2) = shuffled
        // Таран: герой шагает на место, куда таранил; мобы остаются где стояли.
        // Того, кого таранили, там может уже не быть — добил ударом или он пал
        // позже: место опустело, но шаг всё равно делается, если ничего не
        // мешает. Мешает пригвождённость: в хвост чужого строя (до
        // Formation.WalkPlaces) герой ходит, как и союзники.
        rammed = b2.group.pendingMove match {
          case Some(pos) if pos == b2.group.heroPos =>
            (b2.copy(group = b2.group.copy(pendingMove = None)), Vector.empty[String])
          // Пригвождённый (сеть ловцов Братства) Тараном бьёт, но с места не
          // сходит — и должен знать почему.
          case Some(_) if b2.effects.heroRooted =>
            (b2.copy(group = b2.group.copy(pendingMove = None)), Vector(content.text(heldKey(b2, "battle.group.ramPinned"))))
          case Some(pos) =>
            val moved =
              if (b2.group.hasMonster(pos)) b2.moveHeroTo(pos)
              else b2.copy(group = b2.group.copy(heroPos = pos))
            // Таран бьёт только мобов; союзник, стоявший на том месте, уступает
            // его герою и встаёт на его прежнее.
            val b = moved.copy(group = moved.group.copy(allies = moved.group.allies.map(a =>
              if (a.position == pos) a.copy(position = b2.group.heroPos) else a)))
            val line =
              if (b.group.hasMonster(pos)) content.format("battle.group.ram", "pos" -> pos.toString, "monster" -> b.monsterName)
              else content.format("battle.group.ramEmpty", "pos" -> pos.toString)
            (b.copy(group = b.group.copy(pendingMove = None)), Vector(line))
          case None => (b2, Vector.empty[String])
        }
        (b3, log3) = rammed
        // Мерцающий настой: герой встаёт на выбранное место своей стороны — уже
        // после Тарана, раз оба случились в одном раунде. Напротив стоит моб —
        // герой с ним в паре, как после «Переместиться».
        shifted = b3.group.pendingShift match {
          case None => (b3, Vector.empty[String])
          case Some(want) =>
            val b  = b3.copy(group = b3.group.copy(pendingShift = None))
            val to = b.group.shiftSpot(want)
            if (b.group.heroDown) (b, Vector.empty[String])
            else if (b.effects.heroRooted) (b, Vector(content.text(heldKey(b, "battle.group.shimmerPinned"))))
            else if (to == b.group.heroPos)
              (b, if (to == want) Vector.empty[String]
                  else Vector(content.format("battle.group.shimmerStay", "want" -> want.toString)))
            else
              (if (b.group.hasMonster(to)) b.moveHeroTo(to) else b.copy(group = b.group.copy(heroPos = to)), Vector(
                if (to == want) content.format("battle.group.shimmer", "n" -> to.toString)
                else content.format("battle.group.shimmerAside", "want" -> want.toString, "n" -> to.toString)))
        }
        (b4, log4) = shifted
      } yield res.copy(battle = b4, sideLog = res.sideLog ++ log1 ++ log2 ++ log3 ++ log4)
    }

  /** Призыв сородичей: легендарный зовёт 2–3 третьего–четвёртого ранга,
    * мифический — 1–3 первого–третьего. Той же расы, уровня этажа, со
    * стартовой энергией как у подкрепления. Кому не хватило места — в очередь. */
  private def summonKin(battle: SoloPveBattle, caller: MonsterSlot, dungeonLevel: Int): Task[(SoloPveBattle, String)] = {
    val (nMin, nMax, ranks) = Rarity.withName(caller.rarity) match {
      case Rarity.Legendary => (GroupState.LegendarySummonMin, GroupState.LegendarySummonMax, List(Rarity.Rare, Rarity.Mythical))
      case _                => (GroupState.MythicalSummonMin, GroupState.MythicalSummonMax, List(Rarity.Common, Rarity.Uncommon, Rarity.Rare))
    }
    val race = Race.withName(caller.race)
    for {
      n   <- Random.nextIntBetween(nMin, nMax + 1)
      kin <- ZIO.foreach((1 to n).toList) { _ =>
               for {
                 i   <- Random.nextIntBetween(0, ranks.size)
                 pct <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                 m    = MonsterGenerator.generateOfRaceAndRarity(dungeonLevel, race, ranks(i))
               } yield MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp,
                        m.fightStats.armor, m.marked, MonsterEnergy.startEnergy(m.lvl, m.rarity, pct), BattleEffects.empty)
             }
    } yield {
      val joined = kin.foldLeft(battle)(_ admit _)
      val waiting = joined.group.queue.size - battle.group.queue.size
      val line = content.format("battle.group.summon", "monster" -> caller.name, "kin" -> kin.map(_.name).mkString(", "))
      (joined, if (waiting > 0) line + " " + content.format("battle.group.summonQueued", "n" -> waiting.toString) else line)
    }
  }

  /** Строки группового экрана: кто с кем в паре, у кого сколько осталось. С
    * отрядом строй двухрядный: слева герой и союзники по позициям, справа мобы. */
  /** Значки ран союзника для строки строя: пусто, если на нём ничего нет.
    * Пригвождённый — с булавкой, как моб; вмёрзший в лёд Ледяного — со льдом. */
  private def allyDots(a: BattleAlly): String = {
    val marks = List(a.poison.map(_ => Poison.Icon), a.bleed.map(_ => Bleed.Icon), a.burn.map(_ => Burn.Icon),
                     Option.when(a.cold)(Element.Cold.emoji),
                     Option.when(a.rooted > 0)(content.text("battle.group.pinnedMark").trim),
                     Option.when(a.frozen > 0)(MiniBoss.IceElemental.FrozenIcon),
                     Option.when(a.webbed > 0)(SpiderWeb.Icon),
                     Option.when(a.healBlocked)(HealCurse.Icon)).flatten
    if (marks.isEmpty) "" else " " + marks.mkString
  }

  private def groupLines(battle: SoloPveBattle): Vector[String] = {
    val queued = if (battle.group.queue.isEmpty) Vector.empty
                 else Vector(content.format("battle.group.queueLine", "n" -> battle.group.queue.size.toString))
    // Бочки с порохом (см. PowderKeg): сколько их ещё на поле.
    val kegs   = Option.when(battle.kegs > 0)(content.format("battle.kegs.onField", "n" -> battle.kegs.toString)).toVector
    formationLines(battle) ++ queued ++ kegs
  }

  /** Имя моба в строю — с булавкой, пока он пригвождён. */
  private def mobLabel(m: MonsterSlot): String =
    if (m.effects.monsterRooted) m.name + content.text("battle.group.pinnedMark") else m.name

  private def formationLines(battle: SoloPveBattle): Vector[String] =
    if (battle.group.allies.isEmpty && battle.group.convoy.isEmpty && battle.group.paired)
      battle.placesInOrder.map {
        case (pos, Some(m)) =>
          val key = if (pos == battle.group.heroPos) "battle.group.lineHero" else "battle.group.lineFree"
          content.format(key, "n" -> pos.toString, "monster" -> mobLabel(m), "hp" -> m.hpPct.toString, "armor" -> m.armorPct.toString)
        case (pos, None) =>
          content.format("battle.group.lineEmpty", "n" -> pos.toString)
      }.toVector
    else
      (1 to battle.group.rows).toVector.map { pos =>
        val unit =
          if (pos == battle.group.heroPos) Some(content.text("battle.group.sideHero"))
          else battle.group.allyAt(pos).map(a => content.format("battle.group.sideAlly",
                 "name" -> a.name, "hp" -> a.hpPct.toString, "armor" -> a.armorPct.toString,
                 "dots" -> allyDots(a)))
        // Повозка обоза — «под» тем, кто на ней стоит; никого — она сама на месте.
        val cart = battle.group.convoy.flatMap(_.wagonAt(pos))
        def fire(w: Wagon): String = if (w.burn.isDefined) " " + Burn.Icon else ""
        val left = (unit, cart) match {
          case (Some(u), Some(w)) => u + content.format("battle.group.underWagon", "hp" -> w.hpPct.toString, "fire" -> fire(w))
          case (Some(u), None)    => u
          case (None, Some(w))    => content.format("battle.group.sideWagon", "hp" -> w.hpPct.toString,
                                       "armor" -> w.armorPct.toString, "fire" -> fire(w))
          case (None, None)       => content.text("battle.group.sideNone")
        }
        val right = battle.monsterAt(pos).map(m => content.format("battle.group.sideMonster",
            "monster" -> mobLabel(m), "hp" -> m.hpPct.toString, "armor" -> m.armorPct.toString))
          .getOrElse(content.text("battle.group.sideNone"))
        content.format("battle.group.lineRow", "n" -> pos.toString, "left" -> left, "right" -> right)
      }

  // ── Победа ──────────────────────────────────────────────────────────────────

  /** Награды за победу: РОЛЛ + ВСЕ ЗАПИСИ в БД (опыт, лут в scene_data, очистка
    * боя, куб Азата, путь вглубь). Ничего не показывает — сообщения собирает
    * [[showVictory]] по возвращённому [[VictoryOutcome]]. Разделение сделано,
    * чтобы победа была ЗАФИКСИРОВАНА до первого сетевого вызова (см. commit). */
  private def applyVictory(
      user: User,
      hero: Hero,
      battle: SoloPveBattle
  ): Task[VictoryOutcome] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      azat <- loadAzat(user)
      // Недельное благословение Азата: +10% опыта/серебра, +10% редкости, +5% доп. дроп.
      blessed = azat.blessingActive(now)
      // Все павшие этого боя в порядке гибели; в бою 1 на 1 — один моб.
      // Башни в счёт не идут: с них нечего взять и не за что дать опыт.
      // Приговорённый ушёл последним (`preyGone`) — в полях ещё он, но он не пал.
      fallen0 = if (battle.preyGone) battle.group.slain else battle.group.slain :+ battle.slainActive
      // Редкость последнего павшего — в обычном бою это и есть моб в полях.
      lastRarity = fallen0.lastOption.map(m => Rarity.withName(m.rarity)).getOrElse(battle.rarity)
      fallen  = {
        // Новорождённых сетеплётов матка рожает прямо в бою: за них, как и за
        // башни, — ни опыта, ни добычи, и в счёт павших они не идут.
        val spoils = fallen0.filterNot(m => Race.withNameOption(m.race).exists(Race.immovable) ||
                       m.breed.contains(pangea.model.monster.Breed.NewbornSpider))
        // Бой из одних башен всё же должен чем-то кончиться — тогда берём как есть.
        if (spoils.nonEmpty) spoils else fallen0
      }
      // Сюжетный бой: ни опыта, ни добычи по таблицам — только то, что положил
      // сюжет. Налёт на деревню мурлоков — исключение: награда как за обычный
      // бой по сложности задания (её он называет сам, lootLevel). Задание
      // с доски называет свой уровень само (см. SoloPveBattle.lootLevel):
      // разбойники третьего уровня и падают как третьего.
      // Подъём «Пропавшей дружины» сквозь гоблинов — такой же: награда как за
      // обычный бой, по уровню героя.
      raid       = battle.story.exists(BattleState.RaidStories.contains)
      storyFight = battle.story.isDefined && !raid
      killLevel  = battle.lootLevel.getOrElse(if (raid) hero.lvl else hero.dungeonLevel.toLong)
      // Что срезают со зверей этого боя: в гнезде сетеплётов — паучье.
      beastSpoils = BattleState.beastSpoils(battle.story)
      // Минибосс боя — тот, кто пал минибоссом, когда бы он ни пал: Некроманта
      // со свитой бьют первым, Крысиный король падает раньше своих крыс. Смотреть
      // только на последнего добитого нельзя — тогда бой считался бы рядовым.
      bossIdx    = fallen.indexWhere(_.boss.flatMap(MiniBoss.byName).isDefined)
      bossAt     = fallen.lift(bossIdx)
      fallenBoss = bossAt.flatMap(_.boss).flatMap(MiniBoss.byName)
      // Все минибоссы боя — с местом среди павших: в логове Йоргена огненных двое.
      fallenBosses = fallen.flatMap(m => m.boss.flatMap(MiniBoss.byName).map(_ -> m))
      // У минибосса своя награда за уровень босса, а не по этажу и редкости — у
      // каждого, сколько бы их ни пало. Группа — опыт одной суммой за всех.
      baseExp = if (storyFight) 0L
                else if (fallenBosses.nonEmpty) fallenBosses.map { case (b, rec) => b.expReward(rec.lvl) }.sum.max(1L)
                else fallen.map(m => (killLevel * Rarity.withName(m.rarity).factor).toLong.max(1L)).sum.max(1L)
      // Опыт остановлен зельем Фета — за бой ничего (expFrom).
      expGained = hero.expFrom(
                    if (storyFight) 0L else if (blessed) (baseExp * (100L + BattleState.BlessingBonusPct) / 100L).max(1L) else baseExp,
                    now)
      leveled = hero.gainExp(expGained)
      // лут катаем чистым ядром; начисление (инвентарь/серебро) — в LootState
      seed <- Random.nextLong
      // У минибосса дроп свой и всегда есть; обычная таблица лута не катается.
      // Группа — своя добыча с каждого павшего, по порядку гибели.
      (perMonster, rngAfter) = fallenBoss.zip(bossAt) match {
        case _ if storyFight => (List(battle.monsterName -> List.empty[LootGenerator.LootDrop]), Rng(seed))
        case Some((e, rec)) =>
          // Этаж встречи нужен клыку волка: трофей считается по нему.
          val (d, r) = LootGenerator.rollMiniBoss(e, rec.lvl, hero.lvl, Rng(seed),
            floorLvl = hero.dungeonLevel.toLong)
          // Прочие павшие — те, кого босс позвал, и его свита — роняют своё.
          // Съеденных королём в павших нет — их не убивали (см. GroupState.devour).
          // Порядок тот же, что у `fallen`: босс стоит среди павших на своём месте.
          fallen.zipWithIndex.foldLeft((List.empty[(String, List[LootGenerator.LootDrop])], r)) {
            case ((acc, rr), (m, i)) if i == bossIdx => (acc :+ (m.name -> d), rr)
            case ((acc, rr), (m, _)) =>
              m.boss.flatMap(MiniBoss.byName) match {
                // Второй минибосс боя (в логове Йоргена огненных двое) — тоже со
                // своим дропом, а не по таблице.
                case Some(other) =>
                  val (dd, r3) = LootGenerator.rollMiniBoss(other, m.lvl, hero.lvl, rr,
                    floorLvl = hero.dungeonLevel.toLong)
                  (acc :+ (m.name -> dd), r3)
                case None =>
                  val (dd, r3) = LootGenerator.roll(
                    Rarity.withName(m.rarity), Race.withName(m.race), killLevel, rr,
                    gearChanceBonusPct = hero.gems.gearDropBonusPct,
                    rarityBumpPct = if (blessed) BattleState.BlessingBonusPct else 0L,
                    beastSpoils = beastSpoils)
                  (acc :+ (m.name -> (dd ++ BattleState.breedDrops(m.breed))), r3)
              }
          }
        case None =>
          fallen.foldLeft((List.empty[(String, List[LootGenerator.LootDrop])], Rng(seed))) { case ((acc, rng), m) =>
            val (d, r) = LootGenerator.roll(
              Rarity.withName(m.rarity),
              Race.withName(m.race),
              killLevel,
              rng,
              gearChanceBonusPct = hero.gems.gearDropBonusPct,
              rarityBumpPct = if (blessed) BattleState.BlessingBonusPct else 0L,
              beastSpoils = beastSpoils
            )
            (acc :+ (m.name -> (d ++ BattleState.breedDrops(m.breed))), r)
          }
      }
      // «Таксидермист»/«Ювелир» дают отдельные доп. дропы поверх основного лута —
      // с каждого павшего.
      (withExtras, rngAfter2) = fallen.zip(perMonster).foldLeft((List.empty[(String, List[LootGenerator.LootDrop])], rngAfter)) {
        case ((acc, rng), (m, (name, d))) =>
          val (extra, r) = LootGenerator.rollPassiveDrops(
            hero.passives.taxidermistChancePct,
            hero.passives.jewelerChancePct,
            Rarity.withName(m.rarity),
            Race.withName(m.race),
            killLevel,
            rng
          )
          // Награбленное с обозов — на разбойничьей заставе и с разбойников в
          // засаде на караван, с каждого павшего.
          val (spoils, r2) = BattleState.plunder(battle.story, battle.group.convoy, r)
          (acc :+ (name -> (d ++ extra ++ spoils)), r2)
      }
      // Благословение: 5% шанс дополнительной экипировки после боя.
      (blessingGear, rngAfter3) =
        if (!blessed) (Option.empty[pangea.model.item.Item], rngAfter2)
        // С минибосса лишняя вещь — из его набора: она с того, кого убили.
        else fallenBoss.fold(
          LootGenerator.rollBlessingExtraGear(BattleState.BlessingExtraDropPct, lastRarity, killLevel, rngAfter2))(b =>
          LootGenerator.rollBlessingBossGear(BattleState.BlessingExtraDropPct, b, hero.lvl, rngAfter2))
      // Каждый легендарный моб смертной расы роняет ровно одну вещь малого
      // набора своей расы. Сюжетный бой — нет: там добычу кладёт сюжет.
      (raceGear, rngAfter4) =
        if (storyFight) (List.empty[Option[pangea.model.item.Item]], rngAfter3)
        else BattleState.raceSetDrops(fallen, killLevel, rngAfter3)
      // «Пройдоха» (порог 12): с каждого добитого САМИМ героем — шанс лишней
      // вещи. Чьи именно павшие, по ним не видно, — но союзники ведут свой счёт.
      (pocketed, rngAfter5) =
        if (storyFight) (List.empty[pangea.model.item.Item], rngAfter4)
        else BattleState.pocketGear(hero.sets.extraGearChancePct, BattleState.killedByHero(fallen.size, battle),
               lastRarity, killLevel, rngAfter4)
      // Налёт на деревню мурлоков: тот же клинок старого мурлока, что старейшина
      // дарит за помощь, — те же статы, уровень героя.
      raidBlade = Option.when(battle.story.contains(MurlocQuest.RaidStory))(MurlocQuest.blade(hero, rngAfter5))
      // Именной оставляет свой уникальный трофей — если у героя такого ещё
      // нет. Сюжетный бой — нет: там добычу кладёт сюжет.
      owned <- if (storyFight) ZIO.succeed(Set.empty[String]) else ownedNamedTrophies(user, hero, fallen)
      named  = if (storyFight) Nil else BattleState.namedTrophies(fallen, killLevel, owned)
      lootByMonster = withExtras.zipWithIndex.map { case ((name, drops), i) =>
        LootState.MonsterLoot(
          monsterName = name,
          items       = drops.flatMap(_.itemOpt) ++ named.lift(i).flatten ++ raceGear.lift(i).flatten,
          silvers     = drops.collect { case LootGenerator.LootDrop.Silver(a, _) =>
                          a * BattleState.silverScalePct(blessed, hero.sets.battleSilverBonusPct) / 100L },
          doubloons   = drops.collect { case LootGenerator.LootDrop.Doubloons(a) => a }.sum
        )
      }
      // Коллектор из «Письма Марисе» оставляет ровно 500 серебра и 10 дублонов.
      storyLoot = battle.story.filter(_ == MarisaQuest.CollectorStory).map(_ =>
        LootState.MonsterLoot(battle.monsterName, Nil, List(MarisaQuest.CollectorSilver), MarisaQuest.CollectorDoubloons))
      first = storyLoot.getOrElse(lootByMonster.head)
      // routing события (returnState/eventData) кладётся в scene_data ДО боя
      // (напр. цепочка «мобы с сокровищем»); переносим его в добычу, чтобы экран
      // добычи знал, куда вернуться. Для обычного боя scene_data пуст → None.
      prev <- heroDao
        .readSceneData(user.userId)
        .map(_.flatMap(_.as[LootState.LootData].toOption))
      // Сотый убитый: старейшина мурлоков ждёт героя после добычи — если та не
      // ведёт в другую сцену (тогда он выйдет на ближайшем осмотре этажа).
      kills = hero.kills + fallen.size.toLong
      elderWaits <- if (kills >= NpcQuest.MurlocElderKill) MurlocQuest.markPending(heroDao, user.userId, now)
                    else ZIO.succeed(false)
      // После добычи с элементаля игрок идёт осматривать ЕГО логово. Гнилого Джо
      // это не касается — осматривать после него нечего. Сюжетный бой с
      // элементалями (логово Йоргена) возвращает в свою сцену.
      lootReturn = if (fallenBoss.exists(_.race == Race.Elemental) && battle.story.isEmpty)
                     Some(StateType.ElementalSearch)
                   else prev.flatMap(_.returnState).orElse(Option.when(elderWaits)(StateType.MurlocElder))
      // Первый экран добычи — первый павший; остальные ждут своей очереди. Имя
      // моба над добычей показываем только в группе.
      lootData = LootState.LootData(
        items = first.items ++ blessingGear.toList ++ pocketed ++ raidBlade.toList,
        silvers = first.silvers,
        doubloons = first.doubloons,
        returnState = lootReturn,
        // Засада на караван: повозки, какими вышли из боя, — назад в его сцену.
        // Ушёл живым тот, за кем пришли (вор с артефактом деревни), — сцена
        // узнает и это (см. SoloPveBattle.preyLeft).
        eventData = prev.flatMap(_.eventData).map(e => battle.group.convoy.fold(e)(Convoy.returned(e, _)))
                      .map(e => if (battle.preyLeft) SoloPveBattle.markPreyLeft(e) else e),
        monsterName = Option.when(fallen.size > 1)(first.monsterName),
        queue = lootByMonster.tail,
        won = true
      )
      // Первый поверженный легендарный моб роняет квестовый «Неактивный куб Азата»
      // (если куба ещё нет и он не был куплен). Хранится флагом в azat_data.
      cubeDropped = fallen.exists(_.rarity == Rarity.Legendary.entryName) && azat.cubeAbsent
      _ <- ZIO.when(cubeDropped)(saveAzat(user, azat.copy(cube = CubeStatus.FoundInactive)))
      _ <- heroDao.clearActiveBattle(user.userId)
      // Задание Густаво: павшие идут в счёт, пока действует его зелье.
      _ <- NpcQuestLog.onVictory(heroDao, user.userId, fallen.size, GustavoState.potionActive(hero, now))
      // Вампирская фляга пьёт кровь павших: с каждого — шанс на глоток.
      flaskRefill <- vampiricRefill(user, fallen.size)
      // Счёт убитых за всю жизнь: пятидесятый оставляет письмо Марисе (сотый
      // зовёт старейшину мурлоков — см. выше).
      _ <- heroDao.updateKills(user.userId, kills)
      // Кого именно положили — в журнал: по нему раса потом придёт за расплатой.
      _ <- KillLogData.add(heroDao, user.userId, fallen.map(_.race))
      letterFound <- if (hero.kills < NpcQuest.MarisaLetterKill && kills >= NpcQuest.MarisaLetterKill)
                       MarisaQuest.giveLetter(heroDao, inventoryRepo, itemRepo, user.userId, hero)
                     else ZIO.succeed(false)
      // Пятисотый убитый (или любой следующий у того, кто перевалил раньше) —
      // фонарь старшины «Двенадцати фонарей».
      lantern <- if (kills >= NpcQuest.LostSquadKill) LostSquadQuest.start(heroDao, inventoryRepo, itemRepo, user.userId, hero)
                 else ZIO.none
      _ <- heroDao.updateExpAndLevel(
        user.userId,
        leveled.exp,
        leveled.lvl,
        leveled.upgradePoints
      )
      _ <- heroDao.writeSceneData(user.userId, lootData.asJson)
      // победа над «Отмеченным тьмой» на текущем этаже открывает путь вглубь
      newMaxDungeon = math.min(150, hero.dungeonLevel + 1)
      unlocksDarkness =
        fallen.exists(_.marked) && newMaxDungeon > hero.maxDungeonLevel
      _ <- ZIO.when(unlocksDarkness)(
        heroDao.updateMaxDungeonLevel(user.userId, newMaxDungeon)
      )
    } yield VictoryOutcome(
      monsterName       = bossAt.filter(_ => fallenBoss.isDefined).map(_.name)
                            .getOrElse(fallen.map(_.name).mkString(", ")),
      expGained         = expGained,
      newLvl            = Option.when(leveled.lvl > hero.lvl)(leveled.lvl),
      unlocksDarkness   = unlocksDarkness,
      cubeDropped       = cubeDropped,
      elementalDefeated = fallenBoss.exists(_ != MiniBoss.WhiteWolf),
      wolfDefeated      = fallenBoss.contains(MiniBoss.WhiteWolf),
      slainCount        = fallen.size,
      letterFound       = letterFound,
      lantern           = lantern,
      flaskRefill       = flaskRefill
    )

  /** Вампирская фляга: за каждого павшего — бросок на +1 заряд (FlaskRates.VampiricRefillPct),
    * но не выше полной. Бросок не тратится, если фляги нет или она полна. Пьёт
    * первая неполная вампирская на поясе, хоть бы и пустая.
    *
    * Героя читаем заново: до нас пояс уже добрал из сумки опустевшие ячейки
    * ([[refillBelt]]), и запись по старому снимку стёрла бы этот добор.
    * Возвращает (сколько долила, зарядов стало, максимум), если долила. */
  private def vampiricRefill(user: User, slain: Int): Task[Option[(Int, Int, Int)]] =
    getHero(user).flatMap { hero =>
      val equip = hero.equipment
      val onBelt = equip.belt.details match {
        case b: ItemDetails.Belt =>
          b.slots.zipWithIndex.collectFirst {
            case (f: BeltSlot.Flask, i) if f.kind.effect.refillSource == RefillSource.Blood && f.charges < f.maxCharges =>
              (f.charges, f.maxCharges,
                (n: Int) => equip.copy(belt = equip.belt.copy(details = b.updated(i, f.copy(charges = n)))))
          }
        case _ => None
      }
      onBelt match {
        case None => ZIO.succeed(None)
        case Some((charges, max, withCharges)) =>
          ZIO.foldLeft((1 to slain).toList)(charges) { (cur, _) =>
            chanceRoll(cur < max, FlaskRates.VampiricRefillPct).map(hit => if (hit) cur + 1 else cur)
          }.flatMap { after =>
            if (after == charges) ZIO.succeed(None)
            else heroDao.updateEquipment(user.userId, withCharges(after))
                   .as(Some((after - charges, after, max)))
          }
      }
    }

  /** Показ итогов победы — вызывается ПОСЛЕ того, как [[applyVictory]] всё
    * записал. Падение любого сообщения уже не откатывает бой. */
  private def showVictory(
      user: User,
      outcome: VictoryOutcome,
      renderer: Renderer
  ): Task[Unit] =
    for {
      _ <- renderer.show(
        user,
        Screen(
          content.format(
            if (outcome.expGained == 0L) "battle.victoryNoExp"
            else if (outcome.slainCount > 1) "battle.group.victory" else "battle.victory",
            "monster" -> outcome.monsterName,
            "exp"     -> outcome.expGained.toString
          ),
          Nil
        )
      )
      _ <- ZIO.when(outcome.letterFound)(
        renderer.show(user, Screen(content.text("marisa.letterFound") + "\n\n" +
          content.format("marisa.questItemAdded", "item" -> QuestItemKind.MarisaLetter.displayName), Nil))
      )
      // Фонарь старшины: Брамбл в отряде узнаёт его сразу, иначе — подсказка
      // про гнома за столом наёмников.
      _ <- ZIO.foreachDiscard(outcome.lantern) { brambl =>
        renderer.show(user, Screen(content.text("lostSquad.lanternFound") + "\n\n" +
          content.format("marisa.questItemAdded", "item" -> QuestItemKind.ForemanLantern.displayName) + "\n\n" +
          content.text(if (brambl) "lostSquad.bramblInSquad" else "lostSquad.tavernHint"), Nil))
      }
      _ <- ZIO.when(outcome.unlocksDarkness)(
        renderer.show(
          user,
          Screen(content.text("battle.darknessConquered"), Nil)
        )
      )
      _ <- ZIO.foreachDiscard(outcome.newLvl)(lvl =>
        renderer.show(user, Screen(s"Вы получили новый уровень $lvl!", Nil))
      )
      _ <- ZIO.when(outcome.cubeDropped)(
        renderer.show(user, Screen(content.text("battle.cubeFound"), Nil))
      )
      // Победа над минибоссом: своя реплика перед экраном добычи.
      _ <- ZIO.when(outcome.elementalDefeated)(
        renderer.show(user, Screen(content.text("battle.elemental.victory"), Nil))
      )
      _ <- ZIO.when(outcome.wolfDefeated)(
        renderer.show(user, Screen(content.text("battle.wolf.victory"), Nil))
      )
      _ <- ZIO.foreachDiscard(outcome.flaskRefill) { case (gained, charges, max) =>
        renderer.show(user, Screen(content.format("battle.flaskBlood",
          "gained" -> gained.toString, "charges" -> charges.toString, "max" -> max.toString), Nil))
      }
    } yield ()

  private def loadAzat(user: User): Task[AzatState] =
    ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      .flatMap(now => AzatData.load(heroDao, user.userId, now))

  private def saveAzat(user: User, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(user.userId, azat.asJson)

  /** Бросок грибной смеси из доп. слота: склянка лопается под ногами врагов,
    * достаётся всем в поле, и споры травят выживших. Расходник раунда. */
  private def mixTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    if (battle.aimSlot.isDefined) cont(hero, battle, content.text("battle.powerShot.handsBusy"))
    else if (battle.consumableUsedThisRound) cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
  else if (!BattleState.mixInOffhand(hero)) cont(hero, battle, content.text("battle.mixNone"))
    else
      quickHandsRoll(hero).flatMap { case (consumed, qhLog) =>
        val blow             = BrewRates.MushroomDamagePerLvl * hero.lvl
        val (swept, _, foes) = sweep(battle, blow, None, poisons = true)
        val outcome = if (swept.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
        val cleared = hero.copy(equipment = hero.equipment.copy(additionalWeapon = Item.NoItem))
        val after   = if (outcome == Outcome.Victory && swept.promoteNext.isEmpty) endOfRoundHero(cleared, swept, nowMs) else cleared
        ZIO.succeed(TurnResult(after, swept.copy(consumableUsedThisRound = consumed),
          Vector(content.format("battle.mixThrown", "damage" -> blow.toString,
            "foes" -> foes.toString)) ++ qhLog,
          outcome, endsRound = false))
      }

  // ── Божественное оружие ─────────────────────────────────────────────────────

  /** Удар божественного оружия: бьёт всех врагов на поле разом — сперва броня,
    * остатком по HP, — стихия ложится на каждого без броска, и сверху идёт грань
    * своего вида: вытянутая жизнь, верный глаз, яд или ветер в спину. Раунд не завершает
    * (герой бьёт дальше), но за раунд удар один, и «Быстрые руки» его не
    * ускоряют. Последний удар рассыпает божественное оружие прямо в руках. */
  private def divineTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    hero.equipment.additionalWeapon.divine match {
      case None                                 => cont(hero, battle, content.text("battle.divine.none"))
      case Some(_) if battle.aimSlot.isDefined  => cont(hero, battle, content.text("battle.powerShot.handsBusy"))
      case Some(_) if battle.divineUsedThisRound => cont(hero, battle, content.text("battle.divine.alreadyUsed"))
      case Some(divine)                          =>
        val item  = hero.equipment.additionalWeapon
        val kind  = divine.kind
        val blow  = DivineRates.DamagePerLvl * item.lvl
        // `dealt` — сколько сняли со всех разом (по нему пьёт череп), `blow` —
        // сколько прилетело каждому.
        val (swept, dealt, foes) = divineSweep(battle, blow, kind)

        // Грань вида: череп пьёт половину отнятого, аметист даёт верный глаз,
        // бриллиант — ветер в спину за каждого задетого. Яд змеиного клинка
        // уже лёг на врагов в самом ударе.
        val maxHp   = hero.effectiveMaxHp(nowMs)
        // Проклятого Некромантом выпитое черепом не лечит (см. HealCurse).
        val drained = if (kind == DivineKind.DarkLordSword && !battle.effects.heroCursed)
                        (dealt * DivineRates.DrainPct / 100L).max(0L) else 0L
        val healed  = ((hero.fightStats.hp + drained).min(maxHp) - hero.fightStats.hp).max(0L)
        val heroFed = if (healed > 0) hero.copy(fightStats = hero.fightStats.copy(hp = hero.fightStats.hp + healed)) else hero

        val eyed =
          if (kind == DivineKind.AmethystGoddess)
            swept.withEffects(swept.effects.copy(heroTrueStrikeTurns = DivineRates.BuffRounds))
          else swept
        val hasted =
          if (kind == DivineKind.WindLordStaff && foes > 0)
            eyed.copy(heroBattleState = eyed.heroBattleState.add(
              Buff(0L, 0L, 0L, dodgePct = DivineRates.HastePctPerFoe * foes, defencePct = 0L,
                   turnsLeft = Some(DivineRates.BuffRounds))))
          else eyed

        // Заряд потрачен; последний рассыпает божественное оружие — слот пустеет.
        val spent      = divine.spent
        val crumbles   = spent.charges <= 0
        val slot       = if (crumbles) Item.NoItem else item.copy(details = spent)
        val heroAfter0 = heroFed.copy(equipment = heroFed.equipment.copy(additionalWeapon = slot))

        // Числа граней — для строк аметиста и бриллианта; остальным они не нужны.
        val line = content.format(s"battle.divine.${kind.entryName}",
          "damage" -> blow.toString, "heal" -> healed.toString, "foes" -> foes.toString,
          "pct" -> DivineRates.TrueStrikePct.toString, "rounds" -> DivineRates.BuffRounds.toString,
          "haste" -> (DivineRates.HastePctPerFoe * foes).toString)
        val log  = Vector(line) ++
          (if (crumbles) Vector(content.format("battle.divine.crumbled", "name" -> kind.itemName)) else Vector.empty)

        val outcome = if (hasted.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
        // Божественное оружие раунд не завершает — энергию за раунд герой возьмёт своим
        // ударом; но если поле ею и закончилось, раунд кончился вместе с боем.
        val heroAfter = if (outcome == Outcome.Victory && hasted.promoteNext.isEmpty) endOfRoundHero(heroAfter0, hasted, nowMs) else heroAfter0
        ZIO.succeed(TurnResult(heroAfter, hasted.copy(divineUsedThisRound = true), log, outcome, endsRound = false))
    }

  /** Проход божественного оружия по всему полю: каждому врагу — удар, который
    * сперва сносит броню и лишь остатком идёт в HP (защита его не режет,
    * сопротивление минибосса в силе), и прок стихии со стопроцентным шансом;
    * змеиный клинок вдобавок травит. Павшие уходят в добычу. Возвращает бой,
    * суммарный нанесённый урон и число задетых врагов. */
  private def divineSweep(battle: SoloPveBattle, damage: Long, kind: DivineKind): (SoloPveBattle, Long, Int) =
    sweep(battle, damage, kind.element, poisons = kind == DivineKind.SnakeGodBlade)

  /** Сам проход: врагов бьёт одинаково, кто бы ни замахнулся — клинок или
    * цветок. Отличаются только стихия удара и то, травит ли он. */
  private def sweep(
    battle:  SoloPveBattle,
    damage:  Long,
    element: Option[Element],
    poisons: Boolean
  ): (SoloPveBattle, Long, Int) = {

    def strike(b: SoloPveBattle): (SoloPveBattle, Long) = {
      val resist = element.map(e => b.boss.map(_.damageTakenMult(e)).getOrElse(1.0))
        .getOrElse(b.boss.map(_.plainDamageTakenMult).getOrElse(1.0))
      val full     = (damage * resist).toLong.max(1L)
      // Сперва броня, остаток — в HP: удар тяжёлый, но не сквозь доспех.
      // Минибосс теряет не больше остатка своего предела за раунд.
      val armorDmg = full.min(b.monsterCurrentArmor)
      val hurt     = b.hurtActive(armorDmg, (full - armorDmg).min(b.monsterCurrentHp))
      val dealt    = hurt.dealt
      val hit      = hurt.battle
      // Стихия и яд ложатся молча: строки проков только засорили бы экран.
      val alive   = hit.monsterCurrentHp > 0L
      val procced = if (!alive) hit
                    else element.map(e => applyProcs(hit, Set(e), Element.Air.ProcTurns,
                           BattleState.ProcMods(Element.lightningBurn(hurt.hp)))._1).getOrElse(hit)
      val venomed =
        if (poisons && alive)
          procced.withEffects(procced.effects.copy(
            monsterPoison = Some(procced.effects.monsterPoison.map(_.stacked).getOrElse(Poison.onHit))))
        else procced
      (venomed, dealt)
    }

    // Мобы вне пары — с конца строя: павший уходит из `others`, и удаление не
    // сдвигает ещё не битых.
    val (sided, sideDealt, sideFoes) =
      battle.group.others.indices.reverse.foldLeft((battle, 0L, 0)) { case ((b, total, foes), idx) =>
        val slot = b.group.others(idx)
        if (!slot.alive) (b, total, foes)
        else {
          val (hit, dealt) = strike(b.withActive(slot))
          // Задетым мог быть минибосс: его счёт предела едет обратно в слоте.
          val back    = b.copy(group = b.group.copy(others = b.group.others.updated(idx, hit.activeSlot)))
          val settled = if (hit.monsterCurrentHp <= 0L) back.sideFallen(idx) else back
          (settled, total + dealt, foes + 1)
        }
      }
    val (swept, activeDealt) = strike(sided)
    (swept, sideDealt + activeDealt, sideFoes + 1)
  }

  // ── Экран боя ───────────────────────────────────────────────────────────────

  /** Экран боя; в группе перед ним — строй, чтобы с порога было видно, кто
    * стоит против героя (после хода строй идёт в сводке раунда). */
  private def showScreen(user: User, renderer: Renderer): Task[Unit] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      prefs  <- BattleState.prefs(heroDao, user.userId)
      _ <- ZIO.when(battle.group.hasFormation)(
             renderer.show(user, Screen(groupLines(battle).mkString("\n"), Nil)))
      _ <- renderer.show(
        user,
        buildBattleScreen(hero, battle, hero.effectiveMaxHp(now), now, BattleState.mixInOffhand(hero), prefs.miasma)
      )
    } yield ()

  private def buildBattleScreen(
      hero: Hero,
      battle0: SoloPveBattle,
      maxHp: Long,
      nowMs: Long,
      mixReady:     Boolean,
      miasmaOn:     Boolean = true
  ): Screen = {
    // Напротив пусто — шансы героя считаем против последней его цели (кто в
    // полях, тот в этот момент дерётся не с ним).
    val battle    = if (battle0.group.paired) battle0 else battle0.group.lastTarget.map(battle0.withActive).getOrElse(battle0)
    val buffedEff = effWithAir(hero, battle, nowMs)
    val playerDodgePct  = playerDodgeChance(hero, battle, nowMs).toInt
    val monsterDodgePct = mobDodgeChance(buffedEff.accuracy, battle).toInt
    val mobHitPct       = 100 - playerDodgePct
    val heroHitPct      = 100 - monsterDodgePct
    val reductionPct = (BattleState.heroDamageReduction(
      hero, battle, buffedEff, battle.monsterStats.atk, nowMs) * 100.0).toInt
    // Защита моба показывается так же, как своя, — процентом снижения урона, и
    // уже с учётом пробития героя: сырое число защиты игроку ни о чём не говорит,
    // ведь один и тот же моб против сильного героя держит хуже.
    val monsterReductionPct = (monsterDefenceCut(hero, battle, buffedEff, nowMs) * 100.0).toInt
    // Энергия моба видна игроку: по ней читается, когда прилетит умение, и по ней
    // же видно работу Молнии. Шанс каста больше не показываем — его нет.
    val mobEnergy    = battle.monsterCurrentEnergy.max(0L)
    val mobMaxEnergy = battle.monsterStats.energy.max(0L)
    val maxEnergy     = hero.maxEnergy(nowMs)
    // Статус-эффекты рядом со статами своей сущности: яд у HP моба (-урон/ход),
    // реген у HP/брони героя (+восстановление/ход). Реген HP = зелье регенерации
    // (затухающее) + пассивка «Целебный» (4% макс.HP); реген брони =
    // «Самовосстанавливающийся» (4% макс.брони). Показываем суммарной припиской.
    val maxArmor = hero.effectiveMaxArmor(nowMs)
    // Сводка DoT у HP моба: яд 🟢 / кровотечение 🔴 / горение 🔥 (урон/ход).
    // С минибосса раны снимают вдвое меньше (BossPercentDamage) — так и пишем.
    val monsterMaxHp = battle.monsterStats.hp
    val monsterPoison = {
      val parts = List(
        battle.effects.monsterPoison.map(p => s"🟢-${battle.pctDamage(monsterDotDamage(battle, p.damageOn(monsterMaxHp)))}"),
        battle.effects.monsterBleed.map(b => s"🔴-${battle.pctDamage(monsterDotDamage(battle, b.damageOn(monsterMaxHp)))}"),
        battle.effects.monsterBurn.map(bn => s"🔥-${battle.pctDamage(bn.damageOn(monsterMaxHp))}")
      ).flatten
      if (parts.isEmpty) "" else " (" + parts.mkString(" ") + ")"
    }
    // Раны на ГЕРОЕ — тем же видом у его HP: яд 🟢 / кровотечение 🔴 / горение
    // 🔥 и сколько снимет ближайший тик, тем же расчётом, что сам тик
    // (tickHeroEffects), — по настоящему бою, а не по подставленной цели.
    // Герой-нежить яд и кровь не берёт: тик гасит их без урона — не пишем.
    val heroFx     = battle0.effects
    val undeadHero = hero.sets.heroIsUndead
    val heroDots = {
      val parts = List(
        heroFx.heroPoison.filterNot(_ => undeadHero).map(p => s"${Poison.Icon}-${heroPoisonDamage(hero, p, nowMs)}"),
        heroFx.heroBleed.filterNot(_ => undeadHero).map(b => s"${Bleed.Icon}-${heroBleedDamage(hero, b, nowMs)}"),
        heroFx.heroBurn.map(bn => s"${Burn.Icon}-${heroBurnDamage(hero, battle0, bn, nowMs)}"),
        Option.when(heroFx.heroWebbed)(SpiderWeb.Icon),
        Option.when(heroFx.heroCursed)(HealCurse.Icon),
        // Вмёрз в лёд (ледопад Ледяного элементаля).
        Option.when(heroFx.heroFrozen)(MiniBoss.IceElemental.FrozenIcon),
        Option.when(heroFx.heroDrained)(PatternPull.Icon)
      ).flatten
      if (parts.isEmpty) "" else " (" + parts.mkString(" ") + ")"
    }
    val beltHpRegen    = battle.effects.heroRegen.map(_.healOn(maxHp)).getOrElse(0L)
    // Реген HP/ход = зелье + «Целебный» и наборы (% макс.HP) + черепа в снаряжении (промилле макс.HP).
    val hpRegenTotal   = beltHpRegen + maxHp * (hero.passives.hpRegenPct + hero.sets.hpRegenPct) / 100L +
                           maxHp * hero.gems.hpRegenPerMille / 1000L
    // Под проклятием Некроманта реген HP не идёт — и обещать его незачем.
    val heroRegen      = if (hpRegenTotal > 0 && !heroFx.heroCursed) s" (+$hpRegenTotal)" else ""
    val armorRegen     = maxArmor * (hero.passives.armorRegenPct + hero.sets.armorRegenPct) / 100L
    val heroArmorRegen = if (armorRegen > 0) s" (+$armorRegen)" else ""
    val text = content.format(
      if (battle0.group.paired) "battle.enter.text" else "battle.enter.noPair",
      "monster"         -> battle.cardMonsterName,
      "monsterRace"     -> battle.toMonster.raceName,
      "monsterHp"       -> battle.monsterCurrentHp.toString,
      "monsterMax"      -> battle.monsterStats.hp.toString,
      "monsterPoison"   -> monsterPoison,
      "heroDots"        -> heroDots,
      "heroRegen"       -> heroRegen,
      "monsterArmor"    -> battle.monsterCurrentArmor.toString,
      "monsterMaxArmor" -> battle.monsterStats.armor.toString,
      "monsterAtk"      -> battle.monsterStats.atk.toString,
      "monsterReduction" -> s"$monsterReductionPct%",
      "mobHit"          -> s"$mobHitPct%",
      "monsterDodge"    -> s"$monsterDodgePct%",
      "heroHit"         -> s"$heroHitPct%",
      "heroDodge"       -> s"$playerDodgePct%",
      "heroHp"          -> hero.fightStats.hp.toString,
      "heroMax"         -> maxHp.toString,
      "heroArmorRegen"  -> heroArmorRegen,
      "heroArmor"       -> hero.fightStats.armor.min(maxArmor).toString,
      "heroMaxArmor"    -> maxArmor.toString,
      "heroAtk"        -> buffedEff.atk.toString,
      "heroReduction"  -> s"$reductionPct%",
      "heroEnergy"     -> hero.fightStats.energy.min(maxEnergy).toString,
      "heroMaxEnergy"  -> maxEnergy.toString,
      "monsterEnergy"    -> mobEnergy.toString,
      "monsterMaxEnergy" -> mobMaxEnergy.toString
    )
    // Тетива «Мощного выстрела» натянута: рука занята, и вместо умений — одна
    // «Тянуть дальше», пока есть куда тянуть. Стрелу спускает «Атака».
    val aiming = battle0.aimSlot
    val aimLine = aiming.fold("") { slot =>
      slot.skill.effect match {
        case Skill.Effect.Charge(pct, max) =>
          val holds = slot.aim.getOrElse(0)
          "\n" + content.format(if (holds < max) "battle.powerShot.status" else "battle.powerShot.statusMax",
            "mult" -> BattleState.drawMult(pct, holds))
        case _ => ""
      }
    }
    // Готовый навык синий, если хватает энергии на каст, и красный — если нет.
    // Блок и Контрудар краснеют и тогда, когда бонусное действие раунда уже
    // потрачено (пояс или второе из них).
    val skillButtons = aiming match {
      case Some(slot) =>
        (slot.skill.effect, slot.aim) match {
          case (Skill.Effect.Charge(pct, max), Some(holds)) if holds < max && slot.cooldown <= 0 =>
            List(pangea.engine.Choice(s"Skill_${slot.itemId}",
              content.format("battle.powerShot.holdLabel", "mult" -> BattleState.drawMult(pct, holds + 1)),
              row = Some(0)))
          case _ => Nil
        }
      case None =>
        battle.skillSlots.collect {
          case slot if slot.cooldown <= 0 =>
            val enoughEnergy = hero.fightStats.energy >= slot.skill.energyCost(hero)
            val bonusSpent   = BattleState.bonusAction(slot.skill.effect) && battle.consumableUsedThisRound
            pangea.engine.Choice(
              id = s"Skill_${slot.itemId}",
              label = slot.skill.label,
              color =
                if (enoughEnergy && !bonusSpent) pangea.engine.ChoiceColor.Primary
                else pangea.engine.ChoiceColor.Negative,
              // Умения щита стоят в ряду действий, справа от «Атаки»; прочие — над ним.
              row = Some(if (Skill.handSkills.contains(slot.skill)) 1 else 0)
            )
        }
    }
    val (handButtons, topButtons) = skillButtons.partition(_.row.contains(1))
    // Ряды кнопок: row 0 — активные навыки; row 1 — Атака; row 2 — Пояс;
    // row 3 — Сбежать. Отдельной кнопки фляги больше нет: фляги живут в поясе,
    // и он присылает их списком. Места под заглушки (доп. слот, стиль атаки,
    // сменить цель) пока скрыты.
    val beltButton = Option.when(hero.equipment.belt.itemType == ItemType.Belt) {
      val slots = BattleState.beltSlots(hero)
      pangea.engine.Choice(
        "Belt",
        s"👝 Пояс (${slots.size})",
        // Пока тетива натянута, до пояса не дотянуться — он красный.
        color =
          if (slots.isEmpty || aiming.isDefined) pangea.engine.ChoiceColor.Negative
          else pangea.engine.ChoiceColor.Primary,
        row = Some(2)
      )
    }
    // В доп. слоте божественное оружие (а не обычная вещь) — своя кнопка над
    // «Сбежать»: удар по всему полю.
    val divineButton = hero.equipment.additionalWeapon.divine.map { _ =>
      pangea.engine.Choice("UseDivine", pangea.engine.Choice.fit(hero.equipment.additionalWeapon.name),
        color = if (battle0.divineUsedThisRound || aiming.isDefined) pangea.engine.ChoiceColor.Negative
                else pangea.engine.ChoiceColor.Positive,
        row = Some(3))
    }
    // Ловчая сеть в доп. слоте — своя кнопка рядом: бросок в двух шагах, с
    // числом оставшихся бросков. Бросить не в кого — кнопка красная.
    val netButton = hero.equipment.additionalWeapon.net.map { n =>
      pangea.engine.Choice("ThrowNet",
        pangea.engine.Choice.fit(content.format("battle.net.label", "n" -> n.charges.toString)),
        color = if (battle0.divineUsedThisRound || aiming.isDefined || BattleState.netTargets(battle0).isEmpty)
                  pangea.engine.ChoiceColor.Negative
                else pangea.engine.ChoiceColor.Positive,
        row = Some(3))
    }
    // Грибная смесь стоит в доп. слоте — своя кнопка; из сумки и пояса не бросают.
    val mixButton = Option.when(mixReady)(
      pangea.engine.Choice("ThrowMix",
        pangea.engine.Choice.fit(content.format("battle.mixLabel", "count" -> "1")),
        color = if (battle0.consumableUsedThisRound || aiming.isDefined) pangea.engine.ChoiceColor.Negative
                else pangea.engine.ChoiceColor.Positive,
        row = Some(2)))
    // Бить некого (напротив пусто, соседей нет) — вместо атаки «Переместиться»
    // (поменяться местами с союзником), а без союзников — «Ждать».
    // Бить некого — можно шагнуть в сторону (если есть куда) или просто
    // выждать: шаг ход тратит, а выжидание копит энергию, и подменять одно
    // другим неправильно.
    val actionButtons =
      if (heroTargets(hero, battle0).nonEmpty) List(pangea.engine.Choice("Attack", "Атака", row = Some(1)))
      else
        List(
          Option.when(BattleState.stepTargets(battle0).nonEmpty)(
            pangea.engine.Choice("Move", content.text("battle.group.moveLabel"), row = Some(1))),
          Some(pangea.engine.Choice("Wait", content.text("battle.group.waitLabel"), row = Some(1))),
          // Тетива натянута, а бить некого: выжидать и шагать можно, не опуская
          // лука, — или спустить стрелу мимо и освободить руку.
          aiming.map(_ => pangea.engine.Choice("LooseAim", content.text("battle.powerShot.looseLabel"), row = Some(1)))
        ).flatten
    // «Некромант» (порог 10): тьму можно отпустить или свернуть. Ход на это не
    // тратится, поэтому кнопка стоит не среди действий, а рядом с ними.
    val miasmaButton = Option.when(hero.sets.spreadsMiasma)(
      pangea.engine.Choice("Miasma",
        content.text(if (miasmaOn) "battle.miasmaLabelOn" else "battle.miasmaLabelOff"),
        color = if (miasmaOn) pangea.engine.ChoiceColor.Positive else pangea.engine.ChoiceColor.Secondary,
        row = Some(3)))
    val mainButtons =
      actionButtons ++
        handButtons ++
        beltButton.toList ++
        mixButton.toList ++
        divineButton.toList ++
        netButton.toList ++
        miasmaButton.toList ++
        // Из сюжетного боя не сбежать — и кнопки нет. У западни она есть: что
        // уйти некуда, герой узнаёт, попробовав.
        Option.unless(battle.story.isDefined)(
          pangea.engine.Choice(
            "Flee",
            "Сбежать",
            color = pangea.engine.ChoiceColor.Negative,
            row = Some(4)
          )
        ).toList
    Screen(text + aimLine, topButtons ++ mainButtons)
  }

  /** Пузырьковый нектар: если герой обнулился, а плёнка на нём ещё цела —
    * смерть не состоится, останется один HP, и нектар на этом кончится.
    * Лопнувшую плёнку запишет commit вместе с прочими бафами (см.
    * [[persistExtras]]). */
  private def bubbleRescue(res: TurnResult, nowMs: Long): TurnResult =
    if (res.outcome != Outcome.Death || !res.hero.statBoosts.hasActive(BrewRates.BubbleBoost, nowMs)) res
    else {
      val saved = res.hero.copy(
        fightStats = res.hero.fightStats.copy(hp = BattleState.BubbleHp),
        statBoosts = res.hero.statBoosts.without(BrewRates.BubbleBoost))
      res.copy(hero = saved, outcome = Outcome.Continue,
        log = res.log :+ content.format("battle.bubbleSaved", "hp" -> BattleState.BubbleHp.toString))
    }

  /** Чьи уникальные трофеи у героя уже есть: в сумке, в бочке, в хранилище, на
    * Стене трофеев. Читаем всё это, только если среди павших есть именной. */
  private def ownedNamedTrophies(user: User, hero: Hero, fallen: List[SlainMonster]): Task[Set[String]] =
    if (!fallen.exists(m => NamedTrophy.raceOf(BattleState.trophyName(m)).isDefined)) ZIO.succeed(Set.empty)
    else for {
      bag    <- inventoryRepo.get(hero.id).fold(_ => List.empty[Item], _.items.data)
      stored <- barrel.fold(ZIO.succeed(List.empty[Item]))(_.get(hero.id).fold(_ => List.empty[Item], _.items.data))
      vault  <- bank.fold(ZIO.succeed(List.empty[Item]))(_.get(hero.id).fold(_ => List.empty[Item], _.items.data))
      home   <- heroDao.readHomeData(user.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
    } yield (bag ++ stored ++ vault).flatMap(_.namedTrophy).map(_.monster).toSet ++ home.wall

  /** Стена трофеев дома: прибавка к урону героя по расам. */
  private def trophyWall(user: User): Task[Map[String, Int]] =
    heroDao.readHomeData(user.userId).map(_.flatMap(_.as[HomeData].toOption).fold(Map.empty[String, Int])(_.wallPct))

  private def getHero(user: User): Task[Hero] =
    heroDao
      .getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def getBattle(user: User): Task[SoloPveBattle] =
    heroDao
      .readActiveBattle(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No active battle for user ${user.userId}"))
      .flatMap(json => ZIO.fromEither(json.as[SoloPveBattle]))

  /** Уклонение игрока от удара моба, в процентах [5;95]: защита игрока в знаменателе,
    * точность моба ×3 (снижена «Сливающимся» на 10%, см. [[HeroPassives.enemyAccuracyMult]] —
    * это правит саму точность атакующего в формуле, а не добавляет уклонение герою напрямую).
    * Поверх базового шанса добавляются бонус боевых бафов (зелье уворота) и плоские
    * бонусы пассивок — «Быстрые ноги» всегда, а «Сверкающий» — только при бегстве
    * (`fleeing`). Потолок 95% абсолютный. */
  private def playerDodgeChance(hero: Hero, battle: SoloPveBattle, nowMs: Long, fleeing: Boolean = false): Double = {
    // Буст Воздуха (+10% уклонения на airBoostTurns ходов) входит в effWithAir.
    val buffed          = effWithAir(hero, battle, nowMs)
    val agi             = hero.effectiveBaseStats(nowMs).agi
    val passiveBonus    = hero.passives.dodgeBonusPct + (if (fleeing) hero.passives.fleeDodgeBonusPct else 0L)
    // Скованный холодом элементаль бьёт менее точно — срез в п.п. от его точности.
    val chillCut        = if (battle.effects.chilled) MiniBoss.FireElemental.ChilledAccuracyCutPct else 0L
    // Горящему Джо тоже не до прицеливания (у кого пламя точность не сбивает —
    // срез нулевой, см. MiniBoss.burnAccuracyCutPct).
    val burnCut         =
      if (battle.effects.monsterBurn.isEmpty) 0L
      else battle.boss.map(_.burnAccuracyCutPct).getOrElse(0L)
    // Прок Воздуха у самого моба поднимает ему точность на те же 10%.
    val airBoost = if (battle.effects.mobAirBoostTurns > 0) Element.Air.ProcBonusPct else 0L
    // Громовой, чью тучу разогнал ветер, тоже бьёт менее точно.
    val disperseCut = if (battle.effects.mobDispersed) MiniBoss.ThunderElemental.DispersedAccuracyCutPct else 0L
    val monsterAccuracy =
      (battle.monsterStats.accuracy * hero.passives.enemyAccuracyMult *
        (100L + airBoost - chillCut - burnCut - disperseCut).max(0L) / 100.0).toLong
    (BattleState.dodgeChance(agi, buffed.evasion, buffed.defence, monsterAccuracy)
      + battle.heroBattleState.dodgeBonus + passiveBonus).min(95.0).max(5.0)
  }

  // Уклонение моба от удара игрока — та же формула: у моба нет ловкости (agi = 0),
  // в знаменателе только точность игрока ×3. Защита сюда БОЛЬШЕ НЕ входит: она
  // отвечает лишь за процентное снижение урона (см. monsterDefenceCut), иначе
  // одна цифра работала бы дважды — и в уклонении, и в защите.
  private def mobDodgeChance(heroAccuracy: Long, battle: SoloPveBattle): Double = {
    // Тот же прок Воздуха добавляет мобу и уклонения; резкий прыжок волка — свои 5%.
    val airPct      = if (battle.effects.mobAirBoostTurns > 0) Element.Air.ProcBonusPct else 0L
    val leapPct     = if (battle.effects.mobLeapDodge) MiniBoss.WhiteWolf.LeapDodgePct else 0L
    val evasion     = battle.monsterStats.evasion * (100L + airPct + leapPct) / 100L
    val dodge       = BattleState.dodgeChance(0L, evasion, 0L, heroAccuracy)
    // Пока держится грань аметистовой божественного оружия, герой бьёт почти наверняка.
    if (battle.effects.heroTrueStrikeTurns > 0) dodge.min((100L - DivineRates.TrueStrikePct).toDouble) else dodge
  }
}

object BattleState {

  /** Грибная смесь в доп. слоте — её можно бросить в бою. */
  def mixInOffhand(hero: Hero): Boolean =
    hero.equipment.additionalWeapon.brew.contains(BrewKind.MushroomMix)

  /** Сюжетные бои, которые награждаются как обычные: опыт и добыча с каждого
    * павшего (налёт на мурлоков, бой у завала, волны орков у поющего эльфа,
    * банда гоблинов, вскрывшая бочку, меченые звери у тайника Сороки, люди
    * Смотрителя на дегтярном складе, чистильщики у Мельничного моста).
    * Сбежали или ничья — роутинг добычи у них стирается: развязки «после боя»
    * не будет. Прочие сюжетные бои — без добычи по таблицам. */
  val RaidStories: Set[String] =
    Set(MurlocQuest.RaidStory, LostSquadQuest.EscortStory, HedgeSongQuest.WaveStory, BarrelThievesQuest.FightStory,
      StoryMarkQuest.FightStory, StoryPowderQuest.FightStory, StoryListQuest.FightStory, StoryFaceQuest.FightStory,
      StoryToolQuest.FightStory, HotGoodsQuest.FightStory, BigDealQuest.AmbushStory, JorgenQuest.LairStory,
      Villages.ArtifactStory, Villages.HuntersStory,
      BramblQuest.HallStory, PlukhQuest.HuntStory, FaelinQuest.RootsStory, Villages.RaidStory, Villages.BanditsStory,
      Villages.SpidersStory, Villages.CaravanStory, pangea.service.state.BartiRoofQuest.FightStory,
      pangea.service.state.BartiDoorQuest.FightStory, pangea.service.state.StoryClearingQuest.FightStory,
      pangea.service.state.BartiDeliveryQuest.FightStory, pangea.service.state.BartiSaltQuest.FightStory,
      pangea.service.state.BartiPitQuest.FightStory, pangea.service.state.StoryFeedQuest.FightStory) ++
      pangea.service.state.StoryCollectorQuest.FightStories ++
      pangea.service.state.StoryFurnaceQuest.FightStories ++
      pangea.service.state.StoryMorningQuest.FightStories ++ pangea.service.state.StoryNewcomerQuest.FightStories

  /** Бой с ловцами Братства (MonsterSkill.CatcherNet): здесь пригвождение героя
    * и союзников — это их сеть. */
  def netFight(battle: SoloPveBattle): Boolean =
    battle.story.contains(MonsterSkill.CatcherNet.Story)

  /** Паутина, проклятие Некроманта и лёд Ледяного элементаля на герое и
    * союзниках — на раунд меньше (см. SpiderWeb, HealCurse,
    * MiniBoss.IceElemental.FrozenTurns). Сеть ловцов — тоже, но только в бою с
    * ловцами ([[netFight]]). */
  def webTicked(battle: SoloPveBattle): SoloPveBattle = {
    val nets    = netFight(battle)
    val effects = battle.effects.webTicked.curseTicked.frozenTicked
    battle.copy(
      effects = if (nets) effects.copy(heroRootedTurns = (effects.heroRootedTurns - 1).max(0)) else effects,
      group   = battle.group.copy(allies = battle.group.allies.map(a =>
        if (a.webbed > 0 || a.cursed > 0 || a.frozen > 0 || (nets && a.rooted > 0))
          a.copy(webbed = (a.webbed - 1).max(0), cursed = (a.cursed - 1).max(0), frozen = (a.frozen - 1).max(0),
            rooted = if (nets) (a.rooted - 1).max(0) else a.rooted)
        else a)))
  }

  /** Что срезают со зверей в бою с этим сюжетом: в гнезде сетеплётов звери —
    * пауки, и с них паучье; везде прочее — крысиное, как было. */
  def beastSpoils(story: Option[String]): List[(Option[pangea.model.item.MaterialKind], Int)] =
    if (story.contains(Villages.SpidersStory)) LootGenerator.SpiderSpoils else LootGenerator.RatSpoils

  /** Награбленное с обозов (см. LootGenerator.BanditSpoils) — сверх таблицы, с
    * одного павшего. Это добыча деревни и её контрактов, в обычных боях
    * лабиринта его нет (решил пользователь): на разбойничьей заставе — с шансом
    * LootGenerator.BanditSpoilsChancePct, в засаде разбойников на караван
    * (`Convoy.plunders`) — вдвое реже, LootGenerator.AmbushSpoilsChancePct. Нет
    * броска — генератор не тронут. */
  def plunder(story: Option[String], convoy: Option[Convoy], rng: Rng): (List[LootGenerator.LootDrop], Rng) =
    if (story.contains(Villages.BanditsStory)) LootGenerator.banditLoot(rng, LootGenerator.BanditSpoilsChancePct)
    else if (convoy.exists(_.plunders)) LootGenerator.banditLoot(rng, LootGenerator.AmbushSpoilsChancePct)
    else (Nil, rng)

  /** Добыча породы сверх таблицы: с красного сетеплёта — его железа, всегда. */
  def breedDrops(breed: Option[pangea.model.monster.Breed]): List[LootGenerator.LootDrop] =
    breed match {
      case Some(pangea.model.monster.Breed.RedSpider) =>
        List(LootGenerator.LootDrop.Gear(pangea.generator.item.MaterialGenerator.item(pangea.model.item.MaterialKind.RedSpiderGland)))
      case _ => Nil
    }

  /** Умение союзника, выбранное на ход. Удар несёт свою цель: без цели его
    * в выборе просто нет, и доставать её потом неоткуда. */
  private[battle] sealed abstract class AllyCast(val skill: AllySkill)
  private[battle] object AllyCast {
    final case class Strike(target: Int, crushing: Boolean)
      extends AllyCast(if (crushing) AllySkill.CrushingStrike else AllySkill.QuickStrike)
    case object Flask  extends AllyCast(AllySkill.HealingFlask)
    case object Repair extends AllyCast(AllySkill.EmergencyRepair)
  }

  /** Сколько союзников помещается в ряд клавиатуры: их бывает до десяти, а
    * рядов у ВК всего десять. */
  val AlliesPerRow: Int = 2

  /** Чем кончается выбывший союзник: наёмника уносит свиток, поднятый
    * рассыпается, дружинника «Пропавшей дружины» оттаскивают свои,
    * мурлока-собрата забирает вода, а поющего эльфа прячет изгородь. */
  def goneKey(a: BattleAlly): String =
    if (a.kind == pangea.model.squad.AllyKind.SquadGnome) "battle.ally.squadGnomeDown"
    else if (a.kind == pangea.model.squad.AllyKind.MurlocKin) "battle.ally.kinDown"
    else if (a.kind == pangea.model.squad.AllyKind.FreedCaptive) "battle.ally.captiveDown"
    else if (a.kind == pangea.model.squad.AllyKind.HedgeElf) "battle.ally.hedgeElfDown"
    else if (a.kind == pangea.model.squad.AllyKind.CaravanKhajiit) "battle.ally.caravanKhajiitDown"
    else if (a.kind == pangea.model.squad.AllyKind.CaravanGuard) "battle.ally.caravanGuardDown"
    else if (a.kind == pangea.model.squad.AllyKind.GuildMaster) "battle.ally.guildMasterDown"
    else if (a.kind == pangea.model.squad.AllyKind.GuildSeeker) "battle.ally.guildSeekerDown"
    else if (a.kind == pangea.model.squad.AllyKind.Informant) "battle.ally.informantDown"
    else if (a.kind == pangea.model.squad.AllyKind.CouncilEnvoy) "battle.ally.councilEnvoyDown"
    else if (a.kind == pangea.model.squad.AllyKind.BrotherhoodAgent) "battle.ally.brotherhoodAgentDown"
    else if (a.kind == pangea.model.squad.AllyKind.Sojka) "battle.ally.sojkaDown"
    else if (a.undead.isDefined) "battle.ally.crumbled"
    else "battle.ally.scroll"

  /** Уникальные трофеи павших — по порядку гибели. У именного — его трофей,
    * если у героя такого нет (`owned` — чьи трофеи у него уже есть) и в этом
    * бою он ещё не выпал; поднятый некромантом второй раз его не роняет. */
  def namedTrophies(fallen: List[SlainMonster], killLevel: Long, owned: Set[String]): List[Option[Item]] =
    fallen.foldLeft((List.empty[Option[Item]], owned)) { case ((acc, seen), m) =>
      val name = trophyName(m)
      val drop = if (m.undead || seen.contains(name)) None else NamedTrophy.item(name, m.race, killLevel)
      (acc :+ drop, seen ++ drop.map(_ => name))
    }._1

  /** Выжечь врагу `pct`% энергии от её ПОТОЛКА: эффект не зависит от того,
    * сколько моб успел накопить. Возвращает бой и сколько реально сгорело. */
  def burnEnergy(b: SoloPveBattle, pct: Long): (SoloPveBattle, Long) = {
    val burned = (b.monsterStats.energy * pct / 100L).max(1L)
    val left   = (b.monsterCurrentEnergy - burned).max(0L)
    (b.copy(monsterCurrentEnergy = left), b.monsterCurrentEnergy - left)
  }

  /** Что прок стихии делает сверх обычного. `burn` — сколько энергии выжигает
    * прок Молнии (половина урона удара по HP, у грозовой фляги — доля потолка);
    * прочее — наборы героя: «Гроза» (порог 6) — выжженное бьёт и по HP,
    * «Стужа» (4 и 12) — холод режет защиту сильнее и обездвиживает. Союзники и
    * удары по всему полю ходят без наборов. */
  final case class ProcMods(
    burn:      Long,
    burnHurts: Boolean = false,
    coldCut:   Int     = Element.Cold.DefenceReductionCut,
    coldRoots: Boolean = false
  )

  /** Выжечь врагу `amount` энергии, но не больше того, что у него есть.
    * Возвращает бой и сколько сгорело на деле. */
  def burnEnergyBy(b: SoloPveBattle, amount: Long): (SoloPveBattle, Long) = {
    val left = (b.monsterCurrentEnergy - amount.max(0L)).max(0L)
    (b.copy(monsterCurrentEnergy = left), b.monsterCurrentEnergy - left)
  }

  /** Сколько энергии выжигает молния герою или союзнику: `pct`% от потолка
    * `max`, но не больше того, что у него есть сейчас (`current`). Так же, как
    * прок Молнии в оружии выжигает её мобу (см. [[burnEnergy]]). */
  def energyBurnOf(max: Long, current: Long, pct: Long): Long =
    (max * pct / 100L).max(1L).min(current.max(0L))

  /** Серебро с боя, в %: благословение Азата и «Пройдоха» (порог 9) — сверху. */
  def silverScalePct(blessed: Boolean, setBonusPct: Long): Long =
    100L + (if (blessed) BlessingBonusPct else 0L) + setBonusPct

  /** Скольких павших добил сам герой: союзники считают своих (`BattleAlly.kills`,
    * и у ушедших по свитку тоже), остальное — его. */
  def killedByHero(fallen: Int, battle: SoloPveBattle): Int =
    (fallen - (battle.group.allies ++ battle.group.alliesGone).map(_.kills).sum).max(0)

  /** «Пройдоха» (порог 12): с каждого из `kills` добитых героем — свой бросок на
    * лишнюю вещь (как у благословения: экипировка по редкости моба). Без набора
    * броска нет — RNG не тратится. */
  def pocketGear(chancePct: Long, kills: Int, tier: Rarity, killLevel: Long, rng: Rng): (List[Item], Rng) =
    if (chancePct <= 0L) (Nil, rng)
    else (1 to kills).foldLeft((List.empty[Item], rng)) { case ((acc, r), _) =>
      val (item, next) = LootGenerator.rollBlessingExtraGear(chancePct, tier, killLevel, r)
      (acc ++ item.toList, next)
    }

  /** Вещи малых расовых наборов с павших: каждый легендарный моб смертной расы
    * роняет ровно одну вещь набора своей расы (см. LootGenerator.raceSetGear),
    * прочие — ничего. Список идёт по павшим, как у именных трофеев. */
  def raceSetDrops(fallen: List[SlainMonster], killLevel: Long, rng: Rng): (List[Option[Item]], Rng) =
    fallen.foldLeft((List.empty[Option[Item]], rng)) { case ((acc, r), m) =>
      Option.when(m.rarity == Rarity.Legendary.entryName)(m.race).flatMap(Race.withNameOption) match {
        case None => (acc :+ None, r)
        case Some(race) =>
          val (item, next) = LootGenerator.raceSetGear(race, killLevel, r)
          (acc :+ item, next)
      }
    }

  /** Под каким именем павший значится среди именных: отмеченный тьмой — тот же
    * именной, метка тьмы к его имени не относится. */
  def trophyName(m: SlainMonster): String =
    if (m.marked) m.name.stripPrefix(s"${pangea.model.monster.Monster.MarkedPrefix} ") else m.name

  /** Кем встанет павший, если «Некромант» (порог 12) его поднимет. None — в нём
    * нечего поднимать: элементаль, сооружение, уже нежить или Крысиный король
    * (он не тело, а сборище крыс). Белый волк встаёт как по клыку, прочие — как
    * по своему трофею, и с тем именем, под которым пали. */
  def riseForm(slain: pangea.model.battle.SlainMonster,
               boss: Option[MiniBoss]): Option[pangea.model.squad.UndeadForm] =
    boss match {
      case Some(MiniBoss.WhiteWolf) =>
        Some(pangea.service.state.states.events.cave.DarkAltar.wolfOf(slain.lvl))
      case Some(_)                  => None
      case None =>
        Race.withNameOption(slain.race).filterNot(r =>
          r == Race.Elemental || r == Race.Construct || r == Race.Undead
        ).flatMap { race =>
          pangea.model.monster.Rarity.withNameOption(slain.rarity).map { rarity =>
            pangea.service.state.states.events.cave.DarkAltar
              .fromTrophy(race, rarity, slain.lvl.max(1L)).copy(name = slain.name)
          }
        }
    }

  /** Переключатели боя героя: по умолчанию миазмы включены. */
  def prefs(heroDao: pangea.dao.hero.HeroDao, userId: pangea.model.user.UserId): Task[pangea.model.battle.BattlePrefs] =
    heroDao.readBattlePrefs(userId).map(
      _.flatMap(_.as[pangea.model.battle.BattlePrefs].toOption).getOrElse(pangea.model.battle.BattlePrefs.default))

  /** Сооружение ли это — башня со стрелком и всё, что ей подобно. */
  def isTower(battle: SoloPveBattle): Boolean =
    Race.withNameOption(battle.monsterRace).exists(Race.immovable)

  def isTowerSlot(slot: pangea.model.battle.MonsterSlot): Boolean =
    Race.withNameOption(slot.race).exists(Race.immovable)

  /** Кого достанет стрелок с башни: он бьёт с любого места, поэтому целью
    * берёт того, кто к нему ближе. Герой ближе или на равном расстоянии — бьёт
    * героя (тогда None, и удар идёт обычным путём); ближе союзник — достаётся
    * союзнику. Лежачего героя в расчёт не берём: его не добивают. */
  def towerTarget(battle: SoloPveBattle, pos: Int): Option[pangea.model.battle.BattleAlly] = {
    val heroDist = if (battle.group.heroDown) Int.MaxValue else math.abs(pos - battle.group.heroPos)
    battle.group.allies.filter(a => a.alive && math.abs(pos - a.position) < heroDist)
      .minByOption(a => math.abs(pos - a.position))
  }

  /** Куда герой может шагнуть: ровно одно место влево или вправо, и только в
    * пределах строя — за его край, туда, где никого нет и не будет, ходить
    * незачем. В хвост чужого строя — можно, как и союзникам: дальше
    * [[Formation.WalkPlaces]] мест нет ни у кого. Место может быть занято
    * союзником (тогда поменяются) или пустым. */
  def stepTargets(battle: SoloPveBattle): List[Int] =
    // Пригвождённый с места не сходит.
    if (battle.effects.heroRooted) Nil
    else {
      val here  = battle.group.heroPos
      val edge  = battle.group.rows.min(Formation.WalkPlaces)
      // Опутанный союзник с места не сойдёт — поменяться с ним нельзя.
      List(here - 1, here + 1).filter(p => p >= 1 && p <= edge &&
        !battle.group.allyAt(p).exists(a => a.alive && (a.webbed > 0 || a.frozen > 0)))
    }

  /** Исход хода — определяет переход и терминальные действия в [[BattleState.commit]]. */
  sealed trait Outcome
  object Outcome {
    case object Continue extends Outcome
    case object Victory  extends Outcome
    case object Death    extends Outcome
    case object Fled     extends Outcome
  }

  /** Уже ЗАПИСАННЫЙ итог победы — всё, что осталось показать игроку.
    * `newLvl` непуст, если ход дал новый уровень. */
  final case class VictoryOutcome(
      // В групповом бою — имена всех павших через запятую.
      monsterName: String,
      expGained: Long,
      newLvl: Option[Long],
      unlocksDarkness: Boolean,
      cubeDropped: Boolean,
      // Победа над минибоссом ведёт не в обычную добычу, а в осмотр логова.
      elementalDefeated: Boolean = false,
      // Белый волк: своя реплика, логова у него нет.
      wolfDefeated: Boolean = false,
      // Сколько мобов легло в этом бою: больше одного — групповая реплика.
      slainCount: Int = 1,
      // Пятидесятый убитый оставил письмо Марисе — отдельное сообщение после победы.
      letterFound: Boolean = false,
      // Пятисотый — фонарь старшины «Двенадцати фонарей»; внутри — в отряде ли Брамбл.
      lantern: Option[Boolean] = None,
      // Вампирская фляга напилась крови: сколько долила, стало, максимум.
      flaskRefill: Option[(Int, Int, Int)] = None
  )

  /** Результат чистого вычисления хода: итоговый герой и бой (для персиста),
    * накопленный лог сообщений (склеивается и показывается один раз) и исход. */
  /** Итог обычной атаки моба по герою — см. `BattleState.mobStrike`. */
  final case class MobStrike(
    newHp:      Long,
    newArmor:   Long,
    damage:     Long,
    extraLines: List[String],
    battle:     SoloPveBattle,
    /** Удар ушёл в призрачную копию: героя он не тронул, и строки об уроне не будет. */
    mirrored:   Boolean = false,
    /** Удар оставил в ране яд — строка удара помечена (см. `poisonMark`). */
    poisoned:   Boolean = false,
    /** Удар опутал героя паутиной — строка удара помечена (см. `heroWebMark`). */
    webbed:     Boolean = false,
    /** Сколько энергии героя выжег прок Молнии в ударе (см. `energyBurnOf`). */
    energyBurnt: Long   = 0L)

  /** Удар союзника: бой после него, кого бил, сколько снял, добил ли соседа. */
  /** Удар союзника: `slew` — добитый стоял в строю и уходит в павшие со своей
    * строкой «добивает»; `killed` — этот удар вообще кого-то добил, в том числе
    * стоявшего в полях боя; `held` — удар дошёл, но минибосс его не почувствовал:
    * предел за раунд уже исчерпан (см. `BossRoundCap`), и в лог он идёт не
    * промахом. */
  final case class AllyBlow(battle: SoloPveBattle, target: Option[String], damage: Long, slew: Boolean,
                            killed: Boolean = false, held: Boolean = false, poisoned: Boolean = false)

  /** Лучник-союзник вблизи — по тому, кто напротив, и по соседям — бьёт на
    * пятую часть слабее, как лук героя ([[GripRates.RangedClosePenaltyPct]]).
    * `pos` — место цели. */
  def allyCloseMod(a: BattleAlly, pos: Int): Double =
    if (a.kind.ranged && math.abs(pos - a.position) <= GroupState.Reach)
      (100.0 - GripRates.RangedClosePenaltyPct) / 100.0
    else 1.0

  /** Кто зовёт сородичей в конце первого раунда. */
  def summons(rarity: Rarity): Boolean = rarity == Rarity.Legendary || rarity == Rarity.Mythical

  /** Раунд боя без героя — каждые полминуты. */
  val SquadTickMs: Long = 30L * 1000L
  val SquadTickAction: String = """{"action":"SquadTick"}"""
  /** С каким HP герой приходит в себя, когда отряд добил всех без него. */
  val DownReviveHp: Long = 1L
  /** Сколько раундов без героя идут по таймеру и с логом. На следующем такте
    * отряд доводит бой до конца сам (см. `autoResolve`). */
  val ShownDownRounds: Int = 3
  /** Сколько раундов отряд доигрывает самое большее. Не кончился бой и за них —
    * ничья: все отступают, герой цел, добычи нет. */
  val AutoResolveRounds: Int = 97

  /** С каким HP остаётся тот, кого вытащил пузырьковый нектар. */
  val BubbleHp: Long = 1L

  /** Кого достаёт приём того, кто ходит: героя или союзника напротив него.
    * Приёмы мобов и минибоссов бьют того, кто перед ними, а не того, кто им
    * интереснее. */
  sealed trait Victim
  object Victim {
    case object Hero                     extends Victim
    final case class Ally(position: Int) extends Victim
  }

  /** Кого задело взрывом бочки с порохом (см. PowderKeg): героя, союзника на
    * месте `pos`, того, кто в полях, или моба строя под индексом `idx`. */
  sealed trait KegTarget
  object KegTarget {
    case object Hero                extends KegTarget
    final case class Ally(pos: Int) extends KegTarget
    case object Active              extends KegTarget
    final case class Slot(idx: Int) extends KegTarget
  }

  final case class TurnResult(
      hero: Hero,
      battle: SoloPveBattle,
      log: Vector[String],
      outcome: Outcome,
      // Что делали мобы вне пары: удары сбоку, лечение врага, подкрепление,
      // перемешивание. Идёт игроку ОТДЕЛЬНЫМ сообщением после лога раунда.
      sideLog: Vector[String] = Vector.empty,
      // Был ли это ход, за которым идёт конец раунда: атака, умение, бегство.
      // Глоток фляги или зелья и «ничего не случилось» (умение не готово, фляга
      // пуста) раунд не завершают — мобы вне пары не ходят, подкрепление не идёт.
      endsRound: Boolean = true,
      // Быстрые отдыхи, которые ход положил в запас (бодрящий сбор с пояса).
      // Запас лежит у Азата, а не в герое, — его допишет commit.
      instantRests: Int = 0,
      // Раны героя за этот раунд ещё не тикали: он сам добил активного, и до
      // конца хода, где они тикают, дело не дошло (см. victoryByHero). Пойдёт
      // бой дальше — они тикают после промоута (см. dueWounds).
      woundsDue: Boolean = false
  )

  /** Чем на поясе героя ещё можно воспользоваться — их число стоит на кнопке
    * «Пояс». Пустые ячейки не считаем: брать из них нечего — ни глотка, ни
    * горсти. */
  def beltSlots(hero: Hero): List[BeltSlot] = beltUsable(hero).map(_._1)

  /** Все ячейки пояса с их настоящими номерами — и полные, и опустевшие. */
  def beltCells(hero: Hero): List[(BeltSlot, Int)] = hero.equipment.belt.details match {
    case b: ItemDetails.Belt => b.slots.zipWithIndex
    case _                   => Nil
  }

  /** Ячейки, из которых есть что взять, с настоящими номерами: по ним кнопка и
    * найдёт содержимое, сколько бы пустых ячеек ни стояло выше. */
  def beltUsable(hero: Hero): List[(BeltSlot, Int)] = beltCells(hero).filter(_._1.uses > 0)

  /** Что лежит в ячейке с этим номером. */
  def beltAt(hero: Hero, idx: Int): Option[BeltSlot] = hero.equipment.belt.details match {
    case b: ItemDetails.Belt => b.at(idx)
    case _                   => None
  }

  /** Спрашивает ли отвар, на что его тратят: костоправный — какую травму
    * снять, заправка (живая вода) — какую флягу заправить, мерцающий настой —
    * на какое место встать. Приговор спрашивал
    * бы род, но в ячейку он не ложится. */
  def needsPick(kind: BrewKind): Boolean = kind.effect match {
    case BrewEffect.CureTrauma | BrewEffect.RefillFlask(_) | BrewEffect.Shift => true
    case _                                                                    => false
  }

  /** Ячейка, которую после боя можно добрать из сумки: неполная стопка (в
    * ячейке их `stack`) или осыпавшаяся роза. */
  def refillable(slot: BeltSlot, stack: Int): Boolean = slot match {
    case s: BeltSlot.Stack => s.count < stack
    case r: BeltSlot.Rose  => r.withered
    case _                 => false
  }

  /** Список без первого такого имени (травмы с одинаковым именем не дубли —
    * их может быть несколько). */
  def withoutFirst(names: List[String], name: String): List[String] = {
    val i = names.indexOf(name)
    if (i < 0) names else names.patch(i, Nil, 1)
  }

  /** Сколько кнопок выбора ставим в ряд, когда отвар спрашивает. */
  val PicksPerRow: Int = 3

  /** Сколько мест в ряд, когда мерцающий настой спрашивает, куда встать. */
  val PlacesPerRow: Int = 4

  /** Ключ героя в [[homePlaces]]: у союзников из отряда ключ — их место в
    * отряде (`home`), а оно с единицы. */
  val HeroKey: Int = 0

  /** Места после боя. В бою ходят до хвоста чужого строя
    * ([[Formation.WalkPlaces]]), а мест у отряда [[Formation.HeroPlaces]]: кто
    * кончил бой на своей стороне, стоит где стоял, а кто дальше — встаёт на
    * свободное место отряда, ближайшее к краю (самый дальний — к самому краю).
    * Ключи — `home` союзников и [[HeroKey]] героя, значения — места в бою. */
  def homePlaces(at: Map[Int, Int]): Map[Int, Int] = {
    val (inside, outside) = at.partition(_._2 <= Formation.HeroPlaces)
    outside.toList.sortBy { case (_, p) => -p }.foldLeft(inside) { case (acc, (key, p)) =>
      val taken = acc.values.toSet
      acc.updated(key, (Formation.HeroPlaces to 1 by -1).find(q => !taken.contains(q)).getOrElse(p.min(Formation.HeroPlaces)))
    }
  }

  /** Сколько кнопок держит встроенная клавиатура ВК — «До 10 кнопок» (dev.vk.com,
    * «Клавиатура»): больше — и ВК не примет сообщение целиком. */
  val InlineButtons: Int = 10

  /** Куда мерцающий настой предлагает встать: свободные места своего ряда до
    * хвоста чужого строя ([[Formation.WalkPlaces]]). Их бывает больше, чем
    * держит встроенная клавиатура, — тогда [[InlineButtons]] ближайших к герою;
    * на кнопках — по номеру. */
  def shiftChoices(battle: SoloPveBattle): List[Int] =
    battle.group.freeHeroPlaces.sortBy(p => (math.abs(p - battle.group.heroPos), p)).take(InlineButtons).sorted

  /** Бонус благословения Азата (в %): к опыту, серебру и редкости добычи. */
  val BlessingBonusPct: Long = AzatState.BlessingBonusPct
  /** Шанс (в %) дополнительной экипировки после боя при благословении. */
  val BlessingExtraDropPct: Long = AzatState.BlessingExtraDropPct

  /** Из payload UserAction достаём itemId, если action имеет вид `Skill_<id>`.
    */
  def parseSkillAction(ua: pangea.service.state.UserAction): Option[Long] = {
    val key = ua.payload
      .flatMap(p =>
        io.circe.jawn
          .decode[Map[String, String]](p)
          .toOption
          .flatMap(_.get("action"))
      )
      .getOrElse("")
    key match {
      case s"Skill_$rest" => rest.toLongOption
      case _              => None
    }
  }

  /** Номер ячейки пояса, если кнопка его несла. */
  def parseSlot(ua: pangea.service.state.UserAction): Option[Int] =
    ua.payload.flatMap(p =>
      io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("slot")).flatMap(_.toIntOption))

  /** Что игрок выбрал на вопрос отвара: имя травмы или номер ячейки фляги. */
  def parsePick(ua: pangea.service.state.UserAction): Option[String] =
    ua.payload.flatMap(p =>
      io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("pick"))).filter(_.nonEmpty)

  /** Номер цели умения в строю (с нуля), если кнопка его несла. */
  def parseTarget(ua: pangea.service.state.UserAction): Option[Int] =
    ua.payload.flatMap(p =>
      io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("target")).flatMap(_.toIntOption))

  def parseAction(ua: pangea.service.state.UserAction): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  /** Дальность лечения и брони на союзника: только соседи героя по ряду, как
    * удар мечом по врагам. Массовое или дальнобойное лечение, если появится, —
    * отдельными умениями со своей дальностью. */
  val SupportReach: Int = 1

  /** Лечит ли умение или чинит броню — такое можно отдать союзнику рядом
    * (см. [[SupportReach]]). */
  def supports(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.Heal | Skill.Effect.RepairArmor | Skill.Effect.GuardRepair(_, _) => true
    case _                                                                             => false
  }

  /** Поднятый щит (см. Skill.Block): снимает `blockPct`% урона — уже срезанного
    * защитой, — но единица проходит всегда. Щит опущен (0) — урон как есть. */
  def shieldCut(blockPct: Long, damage: Long): Long =
    if (blockPct > 0L) (damage * (100L - blockPct) / 100L).max(1L) else damage

  /** На сколько мест в обе стороны достаёт оружие героя: лук и арбалет — на
    * три, прочее — только соседей (см. [[GripRates.RangedReach]]). Стрелковое
    * всегда двуручное, поэтому лежит в правой руке. */
  def heroReach(hero: Hero): Int =
    if (hero.equipment.weapon.ranged) GripRates.RangedReach + hero.sets.rangedReachBonus else GroupState.Reach

  /** Досягаемость умения: умения оружия бьют как оружие героя, а умения
    * нагрудника, удары телом и щитом, — вблизи: «Отбросить» — соседей, Таран —
    * на [[RamReach]] места. */
  def skillReach(hero: Hero, skill: Skill): Int =
    if (skill == Skill.Ram) RamReach
    else if (Skill.armorSkills.contains(skill)) GroupState.Reach
    else heroReach(hero)

  /** На сколько мест в обе стороны рвётся Таран: и к врагу через одного, и на
    * свободное место. */
  val RamReach: Int = 2

  /** Герой встаёт на место `to` своего ряда: живой союзник оттуда уступает
    * его и встаёт на прежнее место героя, а моб напротив, если есть, — с
    * героем в паре (см. `SoloPveBattle.moveHeroTo`). Таран на свободное место
    * и шаг «Вихря клинка» к цели. */
  def stepTo(battle: SoloPveBattle, to: Int): SoloPveBattle = {
    val from    = battle.group.heroPos
    val swapped = battle.group.allyAt(to).filter(_.alive).fold(battle)(a =>
      battle.copy(group = battle.group.updateAlly(a.position)(_.copy(position = from))))
    if (swapped.group.hasMonster(to)) swapped.moveHeroTo(to)
    else swapped.copy(group = swapped.group.copy(heroPos = to))
  }

  /** Куда «Вихрь клинка» шагает перед ударом — напротив цели на месте
    * `target`. None — кружит с места: цель и так напротив, герой пригвождён
    * или до того места не дотянуться (дальше [[Formation.WalkPlaces]] мест нет). */
  def whirlSpot(battle: SoloPveBattle, target: Int): Option[Int] =
    Some(target).filter(t => t != battle.group.heroPos && t >= 1 && t <= Formation.WalkPlaces &&
      battle.group.hasMonster(t) && !battle.effects.heroRooted)

  /** Свободные места, куда герой может рвануть Тараном без удара: в его
    * досягаемости, в пределах [[Formation.WalkPlaces]] и без врага напротив — союзник оттуда
    * уступит место. Только там, где места что-то значат и строй виден: в
    * группе и с отрядом. Пригвождённому — никуда. */
  def ramSpots(battle: SoloPveBattle): List[Int] =
    if (battle.effects.heroRooted || !battle.group.hasFormation) Nil
    else {
      val here = battle.group.heroPos
      (1 to RamReach).toList.flatMap(d => List(here - d, here + d))
        .filter(p => p >= 1 && p <= Formation.WalkPlaces && !battle.group.hasMonster(p)).sorted
    }

  /** Крыса из-под ног героя (набор «Крыса»): обычная на пороге 6, чумная на
    * пороге 12 ([[HeroSets.summonsPlagueRats]]), уровнем в самого героя.
    * `home = 0` помечает её как пришедшую не из отряда — после боя в лабиринте
    * по этому признаку она в отряд и попадает (см. squadAfterBattle). */
  def ratAlly(hero: Hero, pos: Int): BattleAlly = {
    val rarity = if (hero.sets.summonsPlagueRats) Rarity.Rare else Rarity.Uncommon
    val rat    = MonsterGenerator.generateOfRaceAndRarity(hero.lvl.toInt, Race.Animal, rarity)
    val form   = pangea.model.squad.UndeadForm(rat.name, hero.lvl, rat.fightStats)
    BattleAlly(pangea.model.squad.AllyKind.Rat, pos,
      hp = rat.fightStats.hp, armor = rat.fightStats.armor, energy = rat.fightStats.energy,
      stats = rat.fightStats, lvl = hero.lvl, undead = Some(form), home = 0)
  }

  /** Мурлок-собрат «Матери Глубин» (порог 12): по статам — редкий мурлок
    * уровня героя, на стороне героя и только на этот бой (см. AllyKind.MurlocKin). */
  def kinAlly(hero: Hero, pos: Int): BattleAlly = {
    val kind = pangea.model.squad.AllyKind.MurlocKin
    val kin  = MonsterGenerator.generateOfRaceAndRarity(hero.lvl.toInt, Race.Murloc, Rarity.Rare)
    BattleAlly(kind, pos,
      hp = kin.fightStats.hp, armor = kin.fightStats.armor, energy = kin.fightStats.energy,
      stats = kin.fightStats, lvl = hero.lvl,
      undead = Some(pangea.model.squad.UndeadForm(kind.name, hero.lvl, kin.fightStats)), home = 0)
  }

  /** На сколько мест прыгает «Отскок» и отлетает цель «Отбрасывающего
    * выстрела»: на одно меньше дальности оружия, но хоть на одно, — так цель
    * остаётся на выстреле. Дальность станет другой — и это число вместе с ней. */
  def displacement(hero: Hero): Int = (heroReach(hero) - 1).max(1)

  /** Стрелковое вблизи бьёт слабее: по цели напротив героя и по соседям — на
    * пятую часть (см. [[GripRates.RangedClosePenaltyPct]]); у эльфа в «Тихой
    * роще» (порог 12) — вдвое меньше. Цель — тот, кто стоит в полях боя: удар
    * по соседу или дальней цели идёт через подмену. */
  def rangedCloseMod(hero: Hero, battle: SoloPveBattle): Double =
    if (hero.equipment.weapon.ranged && math.abs(battle.group.activePos - battle.group.heroPos) <= GroupState.Reach)
      (100.0 - GripRates.RangedClosePenaltyPct * (100L - hero.sets.closePenaltyCutPct) / 100.0) / 100.0
    else 1.0

  /** Умение берётся бонусным действием раунда (тем же, что пояс), а не ходом:
    * Блок и Контрудар. */
  def bonusAction(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.Block | Skill.Effect.Counter => true
    case _                                         => false
  }

  /** Натягивают ли умение ходами, прежде чем выстрелить (Мощный выстрел). */
  def charges(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.Charge(_, _) => true
    case _                         => false
  }

  /** Перекатывается ли герой умением по своему ряду (Перекат). */
  def rolls(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.Roll(_, _, _) => true
    case _                          => false
  }

  /** Куда герой может перекатиться: места своего ряда не дальше `maxPlaces`,
    * без союзника — на занятое союзником не перекатываются. Строй для этого не
    * нужен: и в бою один на один герой отходит, а враг подходит сам.
    * Пригвождённому — никуда. */
  def rollSpots(battle: SoloPveBattle, maxPlaces: Int): List[Int] =
    if (battle.effects.heroRooted) Nil
    else {
      val here = battle.group.heroPos
      (1 to maxPlaces).toList.flatMap(d => List(here - d, here + d))
        .filter(p => p >= 1 && p <= Formation.WalkPlaces && battle.group.freeAt(p)).sorted
    }

  /** «×2,5» — во сколько раз сильнее выстрел после `holds` продлений по `pct`%. */
  def drawMult(pct: Int, holds: Int): String =
    (BigDecimal(100L + pct.toLong * holds) / 100).bigDecimal.stripTrailingZeros.toPlainString.replace('.', ',')

  /** Бьёт ли умение по врагу — таким в группе нужна цель. */
  def dealsDamage(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.Damage(_) | Skill.Effect.Sweep(_) | Skill.Effect.Whirl | Skill.Effect.FanBleed(_) | Skill.Effect.Knockback |
         Skill.Effect.BleedDamage(_) | Skill.Effect.WeakSpotStrike | Skill.Effect.BloodHarvest |
         Skill.Effect.Pin(_) | Skill.Effect.Volley(_) | Skill.Effect.Leap | Skill.Effect.Push |
         Skill.Effect.Charge(_, _) => true
    case Skill.Effect.Heal | Skill.Effect.RepairArmor | Skill.Effect.GuardRepair(_, _) | Skill.Effect.WarCry(_, _) |
         Skill.Effect.Block | Skill.Effect.Counter | Skill.Effect.Roll(_, _, _) => false
  }

  /** Нужен ли умению хоть один враг в досягаемости: удар — по цели, клич — по
    * тем, кто рядом; без них ход не тратится. */
  def needsEnemy(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.WarCry(_, _) => true
    case other                     => dealsDamage(other)
  }

  /** На сколько тиков Боевой клич запирает умения мобов вокруг: гаснет один
    * каст — ближайший (двойка по той же причине, что и у ComboSkillBlockTurns). */
  val WarCrySkillBlockTurns: Int = 2

  /** Яд красного паука на клинке гасит ближайший каст врага — двойка по той же
    * причине, что и у ComboSkillBlockTurns. */
  val VenomSkillBlockTurns: Int = 2

  /** Куда долетает ловчая сеть: моб в паре и живые враги не дальше
    * `TrapNet.Reach` мест от героя, слева направо — как досягаемость оружия. */
  def netTargets(battle: SoloPveBattle): List[Int] =
    battle.group.targetsWithin(TrapNet.Reach).filter(p => battle.monsterAt(p).exists(_.alive))

  /** Сеть на враге с места `pos`: `TrapNet.HoldRounds` раундов он не сходит с
    * места — счётчик «Пригвоздить» с запасом в один тик (см. `rooted`). Новое не
    * укорачивает уже наложенное. */
  def netAt(battle: SoloPveBattle, pos: Int): SoloPveBattle = {
    val hold = TrapNet.HoldRounds + 1
    if (pos == battle.group.activePos)
      battle.withEffects(battle.effects.copy(monsterRootedTurns = battle.effects.monsterRootedTurns.max(hold)))
    else battle.group.idxOf(pos) match {
      case -1  => battle
      case idx =>
        val slot = battle.group.others(idx)
        battle.copy(group = battle.group.copy(others = battle.group.others.updated(idx,
          slot.copy(effects = slot.effects.copy(monsterRootedTurns = slot.effects.monsterRootedTurns.max(hold))))))
    }
  }

  /** Шанс уклонения защищающегося юнита от удара атакующего, в процентах, зажат
    * в [5, 95]: 100 * (agi + evasion) / (agi + evasion + defence * 1 +
    * attackerAccuracy * 3) Единая логика «попадания по юниту» для обеих
    * сторон: положительные параметры (числитель) — ловкость и уклонение
    * защищающегося; отрицательные (знаменатель) — защита защищающегося (×1) и
    * точность атакующего (×3). Атакующий попадает, если бросок 1..100 больше
    * уклонения. Вес точности был 1.5: его подняли вдвое, а точность вещей и
    * мобов вдвое срезали — попадания те же, а урон умений от точности меньше.
    */
  /** Процентное снижение урона, наносимого защищающемуся юниту: reduction = (P
    * + Iₚ) / (P + Iₚ + Iₑ × 2), затем к итогу ПРИБАВЛЯЕТСЯ bonusPct п.п. —
    * процентный баф «Заслона» бьёт по ИТОГОВОМУ снижению, а не по защите. Всё
    * вместе зажато сверху 0.7. Где `P` — защита защитника, `Iₚ` — его интеллект,
    * `Iₑ` — интеллект атакующего, `bonusPct` — прибавка в п.п. к итогу. Применяется
    * к чистому урону до брони. Соотношение «×2 для интеллекта атакующего» — из ТЗ.
    * Возврат — доля [0; 0.7].
    */
  def damageReduction(
      protection: Long,
      defenderInt: Long,
      attackerInt: Long,
      bonusPct: Long
  ): Double = {
    val numer   = (protection + defenderInt).toDouble
    val denom   = numer + attackerInt * 2.0
    val raw     = if (denom <= 0.0) 0.0 else numer / denom
    val boosted = raw + bonusPct / 100.0
    boosted.max(0.0).min(0.7)
  }

  /** Итоговое снижение урона у героя: его защита против атаки моба, плюс бонус
    * «Заслона», минус то, что обгрызли морозные удары (холод режет снижение в
    * п.п. и не затухает). То же число, что на экране боя в `heroReduction`. */
  def heroDamageReduction(
      hero: Hero,
      battle: SoloPveBattle,
      buffed: FightStats,
      monsterAtk: Long,
      nowMs: Long
  ): Double = {
    val base = damageReduction(
      protection  = buffed.defence,
      defenderInt = hero.effectiveBaseStats(nowMs).int,
      attackerInt = monsterAtk,
      bonusPct    = battle.heroBattleState.reductionBonusPct
    )
    (base - battle.effects.heroColdDefenceCut / 100.0).max(0.0)
  }

  /** Мощь бойца — его ударный потенциал: у героя это сила втрое плюс атака (ровно
    * то, из чего складывается удар), у моба — просто атака. Стоит в знаменателе
    * снижения урона у того, кого он бьёт, поэтому считается одинаково для обеих
    * сторон — так же будет и в бою двух героев. */
  def power(str: Long, atk: Long): Long = str * 3L + atk

  /** Пробитие защиты: интеллект и ловкость с двойным весом, десятая часть Мощи и
    * стат «пробитие» с предметов (пока его никто не даёт). Растёт линейно по
    * уровню — в одном темпе с защитой мобов, поэтому доля срезанной защиты не
    * уезжает от первого уровня к последнему. У мобов пробития нет вовсе: защиту
    * героя они не пробивают. */
  def pierce(int: Long, agi: Long, power: Long, pierceStat: Long): Long =
    2L * int + 2L * agi + power / 10L + pierceStat

  /** Снижение урона защитой: пробитие сначала съедает саму защиту, и уже её
    * остаток идёт в общую формулу. Интеллекта у мобов нет, поэтому в числителе
    * только защита. */
  def defenceReduction(defence: Long, pierce: Long, attackerPower: Long): Double =
    damageReduction(
      protection  = (defence - pierce).max(0L),
      defenderInt = 0L,
      attackerInt = attackerPower,
      bonusPct    = 0L
    )

  /** Травма от удара моба: удар должен снять больше `HitTraumaMinPct` потолка
    * HP, чтобы вообще считаться; тогда шанс `HitTraumaChancePct`; удар сверх
    * `HitTraumaCrushPct` потолка — сокрушительный, шанс `HitTraumaCrushChancePct`. */
  val HitTraumaMinPct: Long         = 5L
  val HitTraumaChancePct: Long      = 1L
  val HitTraumaCrushPct: Long       = 50L
  val HitTraumaCrushChancePct: Long = 50L

  /** Сколько процентов ПОТОЛКА брони срезает комбо Молния+Холод. */
  val ComboArmorCutPct: Long = 10L

  /** Комбо Огонь+Воздух по тому, кого огонь не берёт (огненный элементаль):
    * не урон, а лечение — на этот процент урона, который комбо нанесло бы. */
  val ComboFireAirFeedsFirePct: Long = 50L

  /** На сколько тиков комбо запирает умение моба. Гаснет один каст — тот, что был
    * бы ответом на комбо. Двойка, а не единица, потому что тик буфов идёт в начале
    * хода моба и сразу съедает один заряд. */
  val ComboSkillBlockTurns: Int = 2

  def dodgeChance(
      agi: Long,
      evasion: Long,
      defence: Long,
      attackerAccuracy: Long
  ): Double = {
    val positive = (agi + evasion).toDouble
    val denom    = positive + defence * 1.0 + attackerAccuracy * 3.0
    val raw      = if (denom <= 0.0) 0.0 else 100.0 * positive / denom
    raw.max(5.0).min(95.0)
  }
}
