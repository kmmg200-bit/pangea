package pangea.service.state.states.dungeon

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.monster.{DepthRates, MonsterGenerator}
import pangea.model.battle.SoloPveBattle
import pangea.model.item.{BeltSlot, BrewRates, ItemDetails}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
import pangea.model.hero.Hero
import pangea.model.schedule.TaskKind
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.schedule.Scheduler
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.service.state.{AppButton, CharacterMenu, HotGoodsQuest, InstantRest, KillLogData, MurlocQuest, NpcQuestLog, RareQuests,
  SquadDuty, State, StoryLine, UserAction, Villages}
import pangea.service.state.states.events.RaceRevengeState.RevengeScene
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

case class DungeonState(heroDao: HeroDao, inventoryRepo: pangea.repository.inventory.InventoryRepository, scheduler: Scheduler, content: SceneContent) extends State {

  import DungeonState.{MinTrackMs, MaxTrackMs, TrackAction, TrackingData}

  /** «Отдых»: если благословение оставило быстрые отдыхи — тратим один прямо тут
    * и остаёмся в лабиринте. Ждать у костра, когда в кармане есть мгновенный
    * отдых, смысла нет, и лишний клик по кнопке тоже. Зарядов нет — обычный
    * привал, как и раньше. */
  private def rest(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      used <- InstantRest.use(heroDao, scheduler, content, user, now, renderer)
      res  <- used match {
        case Some(_) => enter(user, renderer).as(StateType.Dungeon)
        case None    => ZIO.succeed(StateType.Rest)
      }
    } yield res

  private val branch = new Branch(
    routes = Map(
      "FindEvent"     -> Target.Run { (user, _, renderer) => findEvent(user, renderer) },
      "GoDarker"      -> Target.Run { (user, _, renderer) => goDarker(user, renderer) },
      "StopTracking"  -> Target.Run { (user, _, renderer) => stopTracking(user, renderer) },
      "DragonAttack"  -> Target.Run { (user, _, renderer) => dragonAttack(user, renderer) },
      "DragonLeave"   -> Target.Run { (user, _, renderer) => dragonLeave(user, renderer) },
      "GoLighter"     -> Target.Run { (user, _, renderer) => goLighter(user, renderer) },
      "GoToCity"       -> Target.Goto(StateType.GlobalMap),
      "OpenCharacter"  -> Target.Run { (user, _, _) => CharacterMenu.open(heroDao, user.userId, StateType.Dungeon) },
      "Rest"           -> Target.Run { (user, _, renderer) => rest(user, renderer) }
    ),
    // Написал текст или нажал старую кнопку (клиент ВК не сменил клавиатуру,
    // и под рукой осталось «Продолжить» с добычи) — экран этажа ещё раз, с его
    // кнопками. Переход в то же состояние ничего не показывает, и без этого
    // игрок жал бы в тишину, пока не догадается прописать /home.
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.Dungeon) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets + StateType.Dungeon + StateType.HeroStats + StateType.MurlocElder + StateType.RottenJoe +
      StateType.BartiMeet ++ RareQuests.Encounters.map(_._2)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now      <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero0    <- getHero(user)
      // Отработавшие свой день наёмники уходят здесь же, на пороге этажа.
      hero     <- SquadDuty.settle(heroDao, content, user, hero0, now, renderer)
      tracking <- readTracking(user)
      // Пока идёт выслеживание тьмы — герой заперт в нём: никаких других
      // действий, только экран ожидания с единственной кнопкой «идти по следу».
      _        <- renderer.show(user, tracking.fold(enterScreen(hero, now))(_ => trackingScreen))
    } yield ()

  /** Экран этажа. Раскладка по рядам:
   *   - row 0: «Исследовать уровень» (зелёная);
   *   - row 1: «Двигаться к тьме»/«…к свету» — зелёные если ход открыт, красные если закрыт;
   *   - row 2: «В портал», «Персонаж»;
   *   - row 3: «Отдых» (зелёная). */
  private def enterScreen(hero: Hero, nowMs: Long): Screen = {
    val text       = content.format("dungeon.enter.text",
      "level"    -> hero.dungeonLevel.toString,
      "hp"       -> hero.fightStats.hp.toString,
      "maxHp"    -> hero.effectiveMaxHp(nowMs).toString,
      "armor"    -> hero.fightStats.armor.toString,
      "maxArmor" -> hero.effectiveMaxArmor(nowMs).toString,
      "energy"   -> hero.fightStats.energy.min(hero.maxEnergy(nowMs)).toString,
      "maxEnergy"-> hero.maxEnergy(nowMs).toString)
    val byId       = content.screen("dungeon.enter").choices.map(c => c.id -> c).toMap
    def mv(id: String, open: Boolean): pangea.engine.Choice =
      byId(id).copy(color = if (open) ChoiceColor.Positive else ChoiceColor.Negative, row = Some(1))
    val choices = List(
      byId("FindEvent").copy(color     = ChoiceColor.Positive, row = Some(0)),
      mv("GoDarker",  hero.canGoDarker),
      mv("GoLighter", hero.canGoLighter),
      byId("GoToCity").copy(row        = Some(2)),
      byId("OpenCharacter").copy(row   = Some(2)),
      byId("Rest").copy(color          = ChoiceColor.Positive, row = Some(3))
    )
    Screen(text, choices)
  }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Нажатие «Исследовать уровень»: показываем экран осмотра и сразу же
   *  выбираем и разыгрываем случайное событие. Старейшина мурлоков, которому
   *  не досталось экрана добычи после двухсотого убитого, выходит здесь. */
  private def findEvent(user: User, renderer: Renderer): Task[StateType] =
    getHero(user).flatMap { hero =>
      // Логово дракона: здесь осмотр — только он, раньше любых встреч.
      if (DungeonState.dragonLair(hero.dungeonLevel)) showDragon(user, renderer)
      else NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
        if (MurlocQuest.elderPending(quests)) ZIO.succeed(StateType.MurlocElder)
        else rollEvent(user, renderer, quests)
      }
    }

  /** Предупреждение стражи о драконе: «Напасть» или «Уйти». */
  private def showDragon(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("dungeon.dragon.text"), List(
      Choice("DragonAttack", content.text("dungeon.dragon.attackBtn"), color = ChoiceColor.Negative, row = Some(0)),
      Choice("DragonLeave", content.text("dungeon.dragon.leaveBtn"), color = ChoiceColor.Positive, row = Some(0))
    ))).as(StateType.Dungeon)

  /** «Напасть»: бой с драконом этого уровня. Сородичей он не зовёт, энергия —
    * вся его, без обычного стартового броска. Кнопка из старого сообщения вне
    * логова — просто экран этажа. */
  private def dragonAttack(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      res  <-
        if (!DungeonState.dragonLair(hero.dungeonLevel)) renderer.show(user, enterScreen(hero, now)).as(StateType.Dungeon)
        else {
          val battle = SoloPveBattle.from(MonsterGenerator.dragon(hero.dungeonLevel), hero).copy(noKin = true)
          heroDao.writeActiveBattle(user.userId, battle.asJson) *>
            heroDao.writeSceneData(user.userId, io.circe.Json.Null).as(StateType.Battle)
        }
    } yield res

  /** «Уйти»: герой отступает на [[DungeonState.DragonRetreatLevel]]-й уровень.
    * Глубже он спуститься может и потом, но в логове снова встретит дракона.
    * Кнопка из старого сообщения вне логова — просто экран этажа. */
  private def dragonLeave(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      _    <-
        if (!DungeonState.dragonLair(hero.dungeonLevel)) renderer.show(user, enterScreen(hero, now))
        else {
          val level = DungeonState.DragonRetreatLevel
          heroDao.updateDungeonLevel(user.userId, level) *>
            renderer.show(user, enterScreen(hero.copy(dungeonLevel = level), now)
              .noted(content.format("dungeon.dragon.left", "level" -> level.toString)))
        }
    } yield StateType.Dungeon

  private def rollEvent(user: User, renderer: Renderer, quests: NpcQuests): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      _      <- renderer.show(user, Screen(content.text("dungeon.findEvent"), Nil,
                  image = DungeonState.explorePhoto(content.list("dungeon.explorePhotos"), now)))
      // Волчий зов: пока от героя пахнет палёным хмелем, навстречу выходит
      // Белый волк — и запах на этом кончается.
      called  = hero.statBoosts.hasActive(BrewRates.WolfCallBoost, now)
      // Гнилой зов: смрад развязанного узелка выводит к Гнилому Джо. Оба зова
      // разом — волк первым, Джо следующим осмотром.
      rotten  = hero.statBoosts.hasActive(BrewRates.RotCallBoost, now)
      // Приговор: имя рода держится час, и первая же встреча — его именное
      // существо, которое кто-то уже почти добил.
      doomed  = sentencedRace(hero, now)
      // Расплата: раса, которой герой проредил ряды, ждёт своей очереди.
      // Таких рас может накопиться несколько — выходят по одной за осмотр.
      killLog0 <- KillLogData.read(heroDao, user.userId)
      // Городская банда отсчитывает осмотры: герой помешал им по объявлению
      // с доски, и однажды они его найдут (см. GangGrudge).
      killLog  = killLog0.stepGang
      _       <- ZIO.when(killLog != killLog0)(KillLogData.write(heroDao, user.userId, killLog))
      gang     = killLog.gangDue
      avengers = killLog.owed(KillLogData.RevengeEvery).headOption
      // «Охотник»/«Скрытность» сдвигают долю боевых событий в пуле, а глубина
      // открывает новые события (StateType.EventMinLevel). Взял контракт
      // деревни — деревни не встречаются, пока он при герое (см. Villages).
      pool    = StateType.eventsAt(hero.dungeonLevel, hero.passives.battleEncounterFactor)
                  .filterNot(e => e == StateType.Village && Villages.taken(quests))
      idx    <- Random.nextIntBounded(pool.size)
      event   = pool(idx)
      // Первая зацепка основной линии — встреча назначена (см. StoryLine):
      // после обязательных встреч и раньше редких заданий и пула.
      story  <- StoryLine.firstClueDue(heroDao, hero, quests)
      // Редкое задание лабиринта — своим броском, после обязательных встреч и
      // до пула (см. RareQuests). Встреча уже назначена — не бросаем.
      forced  = gang.isDefined || avengers.isDefined || doomed.isDefined || called || rotten
      // Барти (см. HotGoodsQuest): куб есть и товар в сумке — он сам находит
      // героя, после зацепки основной линии и раньше редких заданий; задания
      // ещё не было — встреча наугад, своим броском после редких.
      bartiDue <- if (forced || story.isDefined) ZIO.succeed(false) else bartiReady(user, hero, quests)
      rare   <- if (forced || story.isDefined || bartiDue) ZIO.none else RareQuests.roll(quests, hero.dungeonLevel)
      bartiNew <- if (forced || story.isDefined || bartiDue || rare.isDefined) ZIO.succeed(false)
                  else HotGoodsQuest.roll(quests, hero.lvl)
      result <- (gang, avengers, doomed) match {
                  case (Some(g), _, _)    => gangTrap(user, hero, g, killLog, renderer)
                  case (_, Some(race), _) => revenge(user, race, killLog)
                  case (_, _, Some(prey)) => sentencedPrey(user, hero, prey, renderer)
                  case _ if called        => wolfOnCall(user, hero, renderer)
                  case _ if rotten        => joeOnCall(user, hero, renderer)
                  case _ =>
                    story.orElse(rare) match {
                      // Встреча начинается с чистого листа: что бы ни осталось
                      // в scene_data от прошлой такой же встречи, оно не её.
                      case Some(scene) => heroDao.writeSceneData(user.userId, io.circe.Json.Null).as(scene)
                      case None if bartiDue || bartiNew =>
                        heroDao.writeSceneData(user.userId, io.circe.Json.Null).as(StateType.BartiMeet)
                      case None =>
                        event match {
                          case StateType.Battle => startBattle(user, hero)
                          case StateType.Spring => healAtSpring(user, hero, now, renderer)
                          // Деревня — с чистого листа: что бы ни осталось в
                          // scene_data от прошлой, это не она.
                          case StateType.Village =>
                            heroDao.writeSceneData(user.userId, io.circe.Json.Null).as(StateType.Village)
                          case other            => ZIO.succeed(other)
                        }
                    }
                }
    } yield result

  /** «Ходовой товар» взят, куб есть и товар в сумке — Барти пора найти героя.
    * Задание не взято — читать нечего. */
  private def bartiReady(user: User, hero: Hero, quests: NpcQuests): Task[Boolean] =
    if (!quests.isTaken(NpcQuest.HotGoods)) ZIO.succeed(false)
    else HotGoodsQuest.refresh(heroDao, inventoryRepo, user.userId, hero).map(HotGoodsQuest.ready)

  /** Раса пришла за расплатой. Долг отмечаем сразу, на входе в событие: оно
    * случилось, чем бы ни кончилось — боем, уговорами или бегством. Следующая
    * расплата от этой расы — ещё через [[KillLogData.RevengeEvery]] убитых. */
  private def revenge(
    user: User, race: pangea.model.monster.Race, log: pangea.model.hero.KillLog
  ): Task[StateType] =
    for {
      seed  <- Random.nextLong
      named  = MonsterGenerator.legendaryName(race, Rng(seed))._1
      scene <- Random.nextIntBounded(1000).map(RevengeScene(race.entryName, _, named))
      _     <- KillLogData.write(heroDao, user.userId, log.markAvenged(race))
      _     <- heroDao.writeSceneData(user.userId, scene.asJson)
    } yield StateType.RaceRevenge

  /** Банда дождалась: её именной бьёт героя первым, из-за угла. Удар ложится
    * ДО боя — в бой герой входит уже раненым, — и уйти оттуда нельзя: место и
    * время выбирал не он. Счёты на этом сведены, второй раз эта шайка не
    * придёт (придёт следующая, если герой снова помешает). */
  private def gangTrap(
    user: User, hero: Hero, race: Race, log: pangea.model.hero.KillLog, renderer: Renderer
  ): Task[StateType] = for {
    seed  <- Random.nextLong
    res   <- trapBattle(user, hero, race, log, MonsterGenerator.legendaryName(race, Rng(seed))._1, renderer)
  } yield res

  private def trapBattle(
    user: User, hero: Hero, race: Race, log: pangea.model.hero.KillLog,
    gangName: Option[String], renderer: Renderer
  ): Task[StateType] = {
    val named  = MonsterGenerator.generateOfRaceAndRarity(hero.lvl.toInt, race, Rarity.Legendary, gangName)
    // Удар из-за угла: доля атаки именного, но не больше, чем у героя осталось
    // без одного, — засада калечит, а не убивает.
    val raw    = (named.fightStats.atk * DungeonState.TrapDamagePct / 100L).max(1L)
    val toArmor = raw.min(hero.fightStats.armor)
    val toHp    = (raw - toArmor).min((hero.fightStats.hp - 1L).max(0L))
    val hurt    = hero.fightStats.copy(
      armor = hero.fightStats.armor - toArmor,
      hp    = (hero.fightStats.hp - toHp).max(1L))
    val battle  = SoloPveBattle.from(named, hero.copy(fightStats = hurt)).copy(noFlee = true)
    for {
      _ <- heroDao.updateFightStats(user.userId, hurt)
      _ <- KillLogData.write(heroDao, user.userId, log.gangSettled)
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _ <- heroDao.writeSceneData(user.userId, io.circe.Json.Null)
      _ <- renderer.show(user, Screen(
             content.format(s"thieves.ambushScene.${race.entryName}",
               "name" -> named.name, "gang" -> content.text(s"thieves.gang.${race.entryName}"),
               "damage" -> (toArmor + toHp).toString), Nil))
    } yield StateType.Battle
  }

  /** Какой род приговорён, если приговор ещё держится. */
  private def sentencedRace(hero: Hero, nowMs: Long): Option[Race] =
    hero.statBoosts.active(nowMs).map(_.name)
      .find(_.startsWith(BrewRates.SentenceBoost))
      .flatMap(n => Race.withNameOption(n.drop(BrewRates.SentenceBoost.length)))

  /** Приговорённый: именное существо названного рода, до которого кто-то
    * добрался первым. Стоит на четверти сил и драться не хочет — если за
    * [[BrewRates.SentenceRounds]] раунда его не добить, уйдёт. */
  private def sentencedPrey(user: User, hero: Hero, race: Race, renderer: Renderer): Task[StateType] = {
    // Имя приговорённому достаётся из списка его расы — как и всякому именному.
    val monster = MonsterGenerator.generateOfRaceAndRarity(hero.dungeonLevel, race, Rarity.Legendary,
      MonsterGenerator.legendaryName(race, Rng(hero.lvl * 31L + hero.dungeonLevel.toLong))._1)
    val hp      = (monster.fightStats.hp * BrewRates.SentenceHpPct / 100L).max(1L)
    val armor   = monster.fightStats.armor * BrewRates.SentenceHpPct / 100L
    val battle  = SoloPveBattle.from(monster, hero).copy(
      monsterCurrentHp    = hp,
      monsterCurrentArmor = armor,
      escapesAfter        = BrewRates.SentenceRounds,
      preyName            = monster.customName)
    for {
      _ <- heroDao.updateStatBoosts(user.userId,
             hero.statBoosts.without(BrewRates.SentenceBoost + race.entryName))
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _ <- heroDao.writeSceneData(user.userId, io.circe.Json.Null)
      _ <- renderer.show(user, Screen(content.format("dungeon.sentenced",
             "monster" -> monster.name, "rounds" -> BrewRates.SentenceRounds.toString), Nil))
    } yield StateType.Battle
  }

  /** Смрад узелка вывел к Гнилому Джо: та же встреча, что и на раскопках, — с
    * выбором, драться ли. Узелок на этом выдыхается. */
  private def joeOnCall(user: User, hero: Hero, renderer: Renderer): Task[StateType] =
    heroDao.updateStatBoosts(user.userId, hero.statBoosts.without(BrewRates.RotCallBoost)) *>
      heroDao.writeSceneData(user.userId, io.circe.Json.Null) *>
      renderer.show(user, Screen(content.text("dungeon.rotCall"), Nil)).as(StateType.RottenJoe)

  /** На запах вышел Белый волк: тот же зверь, что подкрадывается на поляне, но
    * здесь герой знает, за кем шёл, — и успел приготовиться. */
  private def wolfOnCall(user: User, hero: Hero, renderer: Renderer): Task[StateType] = {
    val wolf    = MiniBoss.WhiteWolf
    val lvl     = wolf.bossLvl(hero.lvl)
    val monster = Monster(0L, lvl, wolf.race, Rarity.Legendary, wolf.stats(lvl))
    val battle  = SoloPveBattle.from(monster, hero).copy(bossKind = Some(wolf.entryName))
    for {
      _ <- heroDao.updateStatBoosts(user.userId, hero.statBoosts.without(BrewRates.WolfCallBoost))
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _ <- heroDao.writeSceneData(user.userId, io.circe.Json.Null)
      _ <- renderer.show(user, Screen(content.text("dungeon.wolfCall"), Nil))
    } yield StateType.Battle
  }

  private def healAtSpring(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[StateType] = {
    val maxHp    = hero.effectiveMaxHp(nowMs)
    val healed   = (maxHp - hero.fightStats.hp).max(0L)
    // Ручеёк дополнительно восстанавливает 10% максимума Энергии.
    val maxEn        = hero.maxEnergy(nowMs)
    val newEnergy    = (hero.fightStats.energy + (maxEn * 10 / 100L).max(1L)).min(maxEn)
    val energyBack   = newEnergy - hero.fightStats.energy
    // Ручей — вода: наполняет только флягу целителя; кузнеца, стихийную и прочие
    // — нет. На поясе — хоть початую, хоть выпитую до дна.
    val beltRefilled = hero.equipment.belt.details match {
      case b: ItemDetails.Belt if b.slots.exists(BeltSlot.refillsAtSpring) =>
        Some(b.copy(slots = b.slots.map {
          case f: BeltSlot.Flask if BeltSlot.refillsAtSpring(f) => f.refilled
          case other                                            => other
        }))
      case _ => None
    }
    val hasFlask = beltRefilled.isDefined
    val refilledEq = hero.equipment.copy(
      belt = beltRefilled.fold(hero.equipment.belt)(b => hero.equipment.belt.copy(details = b)))
    for {
      _ <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = maxHp, energy = newEnergy))
      _ <- ZIO.when(hasFlask)(heroDao.updateEquipment(user.userId, refilledEq))
      _ <- inventoryRepo.refillFlasks(hero.id).orElse(ZIO.unit)
      // Засада — тем же сообщением, что и ручеёк: «…Однако на шум пришёл и кто-то
      // ещё». Экран боя придёт следом — его рисует бой.
      roll   <- Random.nextIntBetween(1, 101)
      ambush  = roll <= 50
      _ <- renderer.show(user, Screen(
             content.format("dungeon.spring",
               "healed"      -> healed.toString,
               "energy"      -> energyBack.toString,
               "flaskFilled" -> (if (hasFlask) content.text("dungeon.springFlaskFilled") else "")) +
               (if (ambush) content.text("dungeon.springAmbush") else ""), Nil))
      result <- if (ambush) startBattle(user, hero) else ZIO.succeed(StateType.Dungeon)
    } yield result
  }

  private def startBattle(user: User, hero: Hero): Task[StateType] =
    for {
      seed         <- Random.nextLong
      (monster, _)  = MonsterGenerator.generate(hero.dungeonLevel, Rng(seed))
      startPct     <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      battle        = SoloPveBattle.from(monster, hero).withStartEnergy(startPct)
      _            <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      // Обычный бой: после добычи возврат в лабиринт — чистим routing в scene_data,
      // чтобы victory не подхватил чужой «куда вернуться» от прошлого события.
      _            <- heroDao.writeSceneData(user.userId, io.circe.Json.Null)
    } yield StateType.Battle

  /** Движение к свету (выше). Путь наверх открыт всегда, кроме первого этажа. */
  private def goLighter(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      _ <- if (!hero.canGoLighter)
             renderer.show(user, enterScreen(hero, now).noted(content.text("dungeon.alreadyAtTop")))
           else {
             val newLevel = math.max(1, hero.dungeonLevel - 1)
             heroDao.updateDungeonLevel(user.userId, newLevel) *>
               renderer.show(user, enterScreen(hero.copy(dungeonLevel = newLevel), now))
           }
    } yield StateType.Dungeon

  /** Движение к тьме (глубже). Если путь уже открыт — просто спускаемся. Иначе
   *  запускается «выслеживание»: таймер 2–5 минут (в scene_data), по истечении
   *  которого герой выходит на Отмеченного тьмой ЦЕЛЕВОГО (следующего) уровня. */
  private def goDarker(user: User, renderer: Renderer): Task[StateType] =
    for {
      now      <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero     <- getHero(user)
      tracking <- readTracking(user)
      result <- (hero.canGoDarker, tracking) match {
        case (true, _)                              => descend(user, hero, now, renderer)
        // логово дракона: выслеживать глубже — значит выйти на него. Начатое
        // раньше выслеживание (таймер сработал) кончается тем же.
        case (false, t) if DungeonState.dragonLair(hero.dungeonLevel) =>
          ZIO.when(t.isDefined)(heroDao.writeSceneData(user.userId, io.circe.Json.Null) *>
            scheduler.cancel(user.userId, TaskKind.DarknessTracking)) *> showDragon(user, renderer)
        // абсолютное дно лабиринта — выслеживать нечего, глубже пути нет
        case (false, _) if hero.dungeonLevel >= 150 => atDeepest(user, hero, now, renderer)
        case (false, None)                          => startTracking(user, now, renderer)
        // выслеживание уже идёт: сюда попадаем по срабатыванию таймера (поллер
        // шлёт синтетический GoDarker в момент дедлайна) — выходим на Отмеченного.
        case (false, Some(_))                       => encounterMarked(user, hero, renderer)
      }
    } yield result

  private def atDeepest(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[StateType] =
    renderer.show(user, enterScreen(hero, nowMs).noted(content.text("dungeon.deepestReached"))).as(StateType.Dungeon)

  /** Шаг к тьме. Перешёл на [[DepthRates.DeepFrom]]-й уровень — над экраном
    * строка о том, что дальше воздух другой: с него мобы крепче (см. DepthRates);
    * так — при каждом переходе туда с уровня выше. */
  private def descend(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[StateType] = {
    val newLevel = math.min(150, hero.dungeonLevel + 1)
    val screen   = enterScreen(hero.copy(dungeonLevel = newLevel), nowMs)
    heroDao.updateDungeonLevel(user.userId, newLevel) *>
      renderer.show(user,
        if (newLevel == DepthRates.DeepFrom && hero.dungeonLevel < DepthRates.DeepFrom) screen.noted(content.text("dungeon.deepAir"))
        else screen)
        .as(StateType.Dungeon)
  }

  /** Старт выслеживания: пишем дедлайн в scene_data и планируем push-задачу —
   *  поллер сам разбудит героя синтетическим GoDarker в момент дедлайна (игрок
   *  ничего не жмёт, длительность ему не сообщается). На экране ожидания остаётся
   *  только «перестать искать». */
  private def startTracking(user: User, nowMs: Long, renderer: Renderer): Task[StateType] =
    for {
      extra   <- Random.nextLongBetween(MinTrackMs, MaxTrackMs)
      deadline = nowMs + extra
      _ <- heroDao.writeSceneData(user.userId, TrackingData(deadline).asJson)
      _ <- scheduler.schedule(user.userId, deadline, TaskKind.DarknessTracking, StateType.Dungeon, TrackAction)
      _ <- renderer.show(user, trackingScreen.noted(content.text("dungeon.trackingStart")))
    } yield StateType.Dungeon

  /** Время выслеживания вышло — генерируем гарантированного Отмеченного тьмой
   *  ЦЕЛЕВОГО уровня (`dungeonLevel + 1`) и уходим в бой. Победа над ним двигает
   *  `maxDungeonLevel` (см. BattleState). scene_data чистим — таймер отработал. */
  private def encounterMarked(user: User, hero: Hero, renderer: Renderer): Task[StateType] =
    for {
      seed          <- Random.nextLong
      targetLevel    = math.min(150, hero.dungeonLevel + 1)
      (monster, _)   = MonsterGenerator.generateMarked(targetLevel, Rng(seed))
      startPct      <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      battle         = SoloPveBattle.from(monster, hero).withStartEnergy(startPct)
      _ <- renderer.show(user, Screen(content.text("dungeon.trackingFound"), Nil))
      _ <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _ <- heroDao.writeSceneData(user.userId, io.circe.Json.Null)
      _ <- scheduler.cancel(user.userId, TaskKind.DarknessTracking)
    } yield StateType.Battle

  /** Прекратить выслеживание без подтверждения: сбрасываем таймер и возвращаем
   *  обычный экран этажа. */
  private def stopTracking(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      _    <- heroDao.writeSceneData(user.userId, io.circe.Json.Null)
      _    <- scheduler.cancel(user.userId, TaskKind.DarknessTracking)
      _    <- renderer.show(user, enterScreen(hero, now).noted(content.text("dungeon.trackingStopped")))
    } yield StateType.Dungeon

  /** Экран ожидания во время выслеживания: кнопка «перестать искать»
   *  (мгновенный выход, без переспроса) и приложение. Продвижение делает
   *  таймер, не игрок — поэтому кнопки «идти по следу» нет. */
  private def trackingScreen: Screen =
    Screen(
      content.text("dungeon.trackingWait"),
      AppButton.inRow(content, List(
        Choice("StopTracking", content.text("dungeon.trackingLeaveBtn"), color = ChoiceColor.Negative)
      ))
    )

  private def readTracking(user: User): Task[Option[TrackingData]] =
    heroDao.readSceneData(user.userId)
      .map(_.flatMap(_.as[TrackingData].toOption))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object DungeonState {

  /** Фото лабиринта к осмотру — случайное из списка. Выбирается по миллисекунде
    * нажатия, а не броском: для картинки это та же случайность, зато броски
    * самих событий не сдвигаются. Список пуст — без картинки. */
  def explorePhoto(photos: List[String], nowMs: Long): Option[String] =
    if (photos.isEmpty) None else photos.lift((nowMs % photos.size).toInt)

  /** Сколько процентов своей атаки именной вкладывает в удар из засады. Герой
    * после него остаётся жив — хотя бы на единице HP. */
  val TrapDamagePct: Long = 150L

  /** Логово дракона — уровни лабиринта с 26-го по 50-й: здесь любой осмотр и
    * выслеживание выводят на дракона. «Уйти» отводит на 25-й. */
  val DragonFrom: Int         = 26
  val DragonTo: Int           = 50
  val DragonRetreatLevel: Int = 25

  def dragonLair(dungeonLevel: Int): Boolean = dungeonLevel >= DragonFrom && dungeonLevel <= DragonTo

  // Диапазон выслеживания прохода вглубь: 2–5 минут. Игроку длительность не
  // сообщается — время определяется по стенным часам (`nowMs + rnd`).
  private val MinTrackMs: Long = 2L * 60L * 1000L
  private val MaxTrackMs: Long = 5L * 60L * 1000L

  // payload синтетического действия, которым поллер будит выслеживание в момент
  // дедлайна (маршрутизируется в goDarker, как и обычное нажатие «к тьме»).
  private val TrackAction: String = """{"action":"GoDarker"}"""

  /** Прогресс выслеживания в scene_data: момент (мс), когда Отмеченный тьмой
   *  будет «выслежен». Пока это лежит в scene_data — герой в режиме ожидания. */
  final case class TrackingData(darknessTrackingUntil: Long)
  object TrackingData {
    implicit val encoder: Encoder[TrackingData] = deriveEncoder[TrackingData]
    implicit val decoder: Decoder[TrackingData] = deriveDecoder[TrackingData]
  }
}
