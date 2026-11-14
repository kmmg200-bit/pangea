package pangea.service.state.states.village

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.{MiniBoss, Monster}
import pangea.model.quest.Difficulty
import pangea.model.schedule.TaskKind
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.model.village.{ArtifactOffer, ArtifactTrail, VillageContract, VillageQuest, VillageRates}
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
  * Вор в стане стоит первым и в бою не задерживается: не свалили его за
  * [[VillageRates.ThiefEscapeRounds]] раунда — уходит живым со свёртком (так
  * уходит и приговорённый, см. `SoloPveBattle.escapesAfter`), а банда дерётся
  * дальше. Победа над ней — и сразу переход по его следу, а за ним он сам,
  * загнанный, с двумя подручными: второй раз бежать ему некуда.
  *
  * Покупатели (вариант Battle Brothers). С шансом [[VillageRates.NecromancerPct]]%
  * воров застают при сделке: у костра с ними Некромант — минибосс, он стоит
  * первым, вор — за ним. После победы с шансом [[VillageRates.MageOfferPct]]%
  * из кустов выходит маг Сноходцев — тот, кто заказал кражу, — и предлагает за
  * артефакт дублоны ([[VillageRates.mageDoubloons]]). Продал — контракт сорван,
  * дублоны у героя; отказал — к старосте. Был Некромант — мага нет: сделка и
  * так сорвана.
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
      "VillageTrailSell"       -> Target.Run { (u, _, r) => atTrail(u, r)(c => sell(u, c, r)) },
      "VillageTrailRefuse"     -> Target.Run { (u, _, r) => atTrail(u, r)(_ => refuse(u, r)) },
      "VillageTrailBack"       -> Target.Run { (u, _, r) => enter(u, r).as(StateType.VillageTrail) },
      "VillageTrailOut"        -> Target.Run { (u, _, r) => out(u, r) },
      "OpenCharacter"          -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.VillageTrail) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.VillageTrail) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Dungeon, StateType.Battle, StateType.HeroStats, StateType.VillageTrail)

  /** Вход: контракта на артефакт нет — здесь пусто; маг Сноходцев ждёт ответа —
    * его предложение; бой в стане выигран (добыча положила отметку) — артефакт у
    * героя, а если вор ушёл из боя — погоня за ним; погоня не начата — у ворот;
    * все переходы пройдены — у стана; переход идёт — по следу (кончился, пока
    * герой был в меню, — кончится сразу). */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now      <- nowMs
      contract <- standing(user)
      data     <- heroDao.readSceneData(user.userId)
      trail     = data.flatMap(_.as[ArtifactTrail].toOption)
      offer     = data.flatMap(_.as[ArtifactOffer].toOption)
      _        <- contract match {
                    case None =>
                      clear(user) *> renderer.show(user, Screen(text("gone"), List(outChoice)))
                    case Some(c) if offer.isDefined => ZIO.foreachDiscard(offer)(o => renderer.show(user, offerScreen(c, o)))
                    case Some(c) if data.exists(won) =>
                      trail match {
                        case Some(t) if !t.escaped && data.exists(SoloPveBattle.preyLeftIn) => thiefGone(user, c, t, now, renderer)
                        case _                                                             => victory(user, c, trail, renderer)
                      }
                    case Some(c) => trail match {
                      case None                => renderer.show(user, gatesScreen(c))
                      case Some(t) if t.atCamp => renderer.show(user, campScreen(c, t))
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

  private def readOffer(user: User): Task[Option[ArtifactOffer]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[ArtifactOffer].toOption))

  /** Погоня идёт или маг ждёт ответа: «Идти по следу», «Уйти» и «В лабиринт»
    * тогда — старые кнопки. */
  private def busy(user: User): Task[Boolean] =
    heroDao.readSceneData(user.userId).map(_.exists(d => d.as[ArtifactTrail].isRight || d.as[ArtifactOffer].isRight))

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

  /** У стана: кто там, сложность, что вор со свёртком в бою не задержится и что
    * из этого боя не сбежать. Застали при сделке — у костра Некромант; загнанный
    * вор — свой текст, и бежать ему уже некуда. */
  private def campScreen(c: VillageContract, t: ArtifactTrail): Screen =
    Screen(
      Villages.joined(
        content.format(key(if (t.escaped) "cornered" else if (t.necromancer) "campNecromancer" else "camp"), args(c, t): _*),
        difficulty(c.level),
        (if (t.escaped) "" else content.format(key("thiefHint"), "rounds" -> VillageRates.ThiefEscapeRounds.toString) + "\n") +
          content.text("village.lair.noEscape")),
      List(
        content.choice("VillageTrailFight", key("fightLabel")).copy(row = Some(0)),
        content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(1)),
        content.choice("VillageTrailQuit", key("quitLabel")).copy(row = Some(1)),
        content.choice("VillageTrailAbandon", "village.lair.abandonLabel").copy(row = Some(2))))

  /** Что видно на этом переходе: первый уводит от ворот в изгородь, последний
    * выводит к стану, средние — по очереди из `village.artifact.legs.middle`;
    * за ушедшим вором — его след. */
  private def legText(c: VillageContract, t: ArtifactTrail): String =
    if (t.escaped) content.format(key("escapedLeg"), args(c, t): _*)
    else if (t.leg <= 1) content.format(key("legs.first"), args(c, t): _*)
    else if (t.lastLeg) content.format(key("legs.last"), args(c, t): _*)
    else {
      val middle = content.list(key("legs.middle"))
      middle.lift(Math.floorMod(t.leg - 2, middle.size.max(1))).fold("")(line => format(line, c, t))
    }

  /** Подстановки погони: то же, что в прочих текстах деревни, и имя вора (не
    * запомнили — просто «Вор»). Вор в текстах — в начале фразы. */
  private def args(c: VillageContract, t: ArtifactTrail): Seq[(String, String)] =
    Villages.args(content, c) :+ ("thief" -> t.thief.getOrElse(text("thiefNameless")))

  /** Подставить в строку из списка то же, что и в прочие тексты погони. */
  private def format(line: String, c: VillageContract, t: ArtifactTrail): String =
    args(c, t).foldLeft(line) { case (s, (k, v)) => s.replace(s"{$k}", v) }

  private def difficulty(level: Int): String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(level.max(1)))

  private def outChoice = content.choice("VillageTrailOut", "village.toDungeonLabel")

  /** Маг Сноходцев: кто он и сколько даёт за артефакт — продать или отказать. */
  private def offerScreen(c: VillageContract, o: ArtifactOffer): Screen = {
    val price = Villages.doubloonsLine(content, o.doubloons)
    Screen(content.format(key("mageOffer"), Villages.args(content, c) :+ ("offer" -> price): _*), List(
      content.choice("VillageTrailSell", key("sellLabel"), "offer" -> price).copy(row = Some(0)),
      content.choice("VillageTrailRefuse", key("refuseLabel")).copy(row = Some(0)),
      content.choice("OpenCharacter", "village.lair.characterLabel").copy(row = Some(1))))
  }

  // ── Погоня ──────────────────────────────────────────────────────────────────

  /** «Идти по следу»: сколько переходов до стана — бросок, и первый из них. Уже
    * в пути или маг ждёт ответа — показать, что сейчас. */
  private def depart(user: User, c: VillageContract, renderer: Renderer): Task[StateType] =
    busy(user).flatMap {
      case true => enter(user, renderer).as(StateType.VillageTrail)
      case false =>
        for {
          now  <- nowMs
          legs <- Random.nextIntBetween(VillageRates.TrailLegsMin, VillageRates.TrailLegsMax + 1)
          res  <- startLeg(user, c, ArtifactTrail.start(legs, 0L), now, renderer)
        } yield res
    }

  /** Начать переход `t.leg`: что видно на следу — и таймер до конца перехода.
    * Момент — в сцене, чтобы пережить уход в меню. `note` — что было перед ним. */
  private def startLeg(user: User, c: VillageContract, t: ArtifactTrail, now: Long, renderer: Renderer,
                       note: String = ""): Task[StateType] =
    for {
      delta <- Random.nextLongBetween(VillageRates.TrailLegMinMs, VillageRates.TrailLegMaxMs + 1L)
      trail  = t.copy(nextAt = now + delta)
      _     <- writeTrail(user, trail)
      _     <- scheduler.schedule(user.userId, trail.nextAt, TaskKind.VillageTrail, StateType.VillageTrail, LegAction)
      _     <- renderer.show(user, trailScreen(legText(c, trail)).noted(note))
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
                   if (next.atCamp)
                     dealRoll(next).flatMap(camp =>
                       writeTrail(user, camp) *> renderer.show(user, campScreen(c, camp)).as(StateType.VillageTrail))
                   else startLeg(user, c, next, now, renderer)
                 case _ => enter(user, renderer).as(StateType.VillageTrail)
               }
    } yield res

  /** Вышли к стану: застанут ли воров при сделке с Некромантом — бросок сейчас,
    * чтобы стан и бой сошлись. Загнанного вора это не касается. */
  private def dealRoll(t: ArtifactTrail): Task[ArtifactTrail] =
    if (t.escaped) ZIO.succeed(t)
    else Random.nextIntBetween(0, 100).map(roll => t.copy(necromancer = roll < VillageRates.NecromancerPct))

  // ── Стан воров ──────────────────────────────────────────────────────────────

  /** Бой в стане: вор с артефактом и банда — уровня задания. Вор стоит первым и
    * уходит, если не свалить его за [[VillageRates.ThiefEscapeRounds]] раунда;
    * застали при сделке — первым стоит Некромант, вор за ним. Загнанному вору —
    * с двумя подручными — бежать некуда. Добыча вернёт сюда с отметкой победы, а
    * кто вор — в погоне: ушёл — за ним и пойдём. Не у стана (старая кнопка) —
    * показать, что сейчас. */
  private def fight(user: User, c: VillageContract, renderer: Renderer): Task[StateType] =
    readTrail(user).flatMap {
      case Some(t) if t.atCamp =>
        for {
          hero     <- getHero(user)
          seed     <- Random.nextLong
          gang      = if (t.escaped) Villages.cornered(c, t.thief, t.thiefRace, Rng(seed)) else Villages.thiefGang(c, Rng(seed))
          thief     = gang.headOption
          dealer    = Option.when(t.necromancer && !t.escaped)(Villages.dealer(c))
          foes      = dealer.toList ++ gang
          // Некромант выходит с полной силой, как в пещере: без энергии первый
          // его круг прошёл бы впустую.
          energies <- ZIO.foreach(foes)(startEnergy).map(es => dealer.fold(es)(d => d.fightStats.energy :: es.drop(1)))
          plain     = dealer.fold(SoloPveBattle.fromGroup(foes, hero, energies))(_ =>
                        withDealer(SoloPveBattle.fromGroup(foes, hero, energies, headBoss = Some(MiniBoss.Necromancer)), c, thief))
                        .copy(story = Some(Villages.ArtifactStory), lootLevel = Some(c.level.max(1).toLong))
          // Уходит только вор с именем: по имени его и узнают в строю.
          battle    = thief.flatMap(_.customName).filterNot(_ => t.escaped).fold(plain)(name =>
                        plain.copy(escapesAfter = VillageRates.ThiefEscapeRounds, preyName = Some(name)))
          marked    = t.copy(thief = thief.flatMap(_.customName), thiefRace = thief.map(_.race))
          routing   = LootData(Nil, Nil, returnState = Some(StateType.VillageTrail),
                        eventData = Some(marked.asJson.deepMerge(wonMark)))
          _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
          _        <- heroDao.writeSceneData(user.userId, routing.asJson)
          _        <- renderer.show(user, Screen(content.format(key(
                        if (t.escaped) "fightCornered" else if (dealer.isDefined) "fightNecromancer" else "fight"),
                        args(c, marked): _*), Nil))
        } yield StateType.Battle
      case _ => enter(user, renderer).as(StateType.VillageTrail)
    }

  /** Некромант в стане — как в пещере: подручных себе поднимает уровнем задания
    * и расы вора (сам он нежить, и мобов его расы не бывает), сородичей со
    * стороны не ждёт. */
  private def withDealer(b: SoloPveBattle, c: VillageContract, thief: Option[Monster]): SoloPveBattle =
    b.copy(minionLvl = c.level.max(1).toLong, noKin = true,
      group = b.group.copy(originRace = thief.map(_.race.entryName).orElse(b.group.originRace)))

  /** Бой выигран, но вор со свёртком ушёл: сразу — переход по его следу, а кто
    * он и был ли в стане Некромант, погоня помнит. */
  private def thiefGone(user: User, c: VillageContract, t: ArtifactTrail, now: Long, renderer: Renderer): Task[Unit] = {
    val next = ArtifactTrail.after(t.thief, t.thiefRace, 0L, t.necromancer)
    startLeg(user, c, next, now, renderer, note = content.format(key("thiefGone"), args(c, next): _*)).unit
  }

  /** Воры перебиты — в стане или загнанный вор: свёрток у героя. С шансом
    * [[VillageRates.MageOfferPct]]% из кустов выходит маг Сноходцев и ждёт
    * ответа; был при сделке Некромант — мага нет. Иначе — к старосте за платой. */
  private def victory(user: User, c: VillageContract, trail: Option[ArtifactTrail], renderer: Renderer): Task[Unit] = {
    val t     = trail.getOrElse(ArtifactTrail(0, 1, 0L))
    val found = content.format(key(if (t.escaped) "victoryCornered" else "victory"), args(c, t): _*)
    for {
      mage <- if (t.necromancer) ZIO.succeed(false) else Random.nextIntBetween(0, 100).map(_ < VillageRates.MageOfferPct)
      _    <- if (mage) {
                val o = ArtifactOffer(VillageRates.mageDoubloons(c.silver, c.doubloons))
                heroDao.writeSceneData(user.userId, o.asJson) *>
                  renderer.show(user, Screen(found, Nil)) *> renderer.show(user, offerScreen(c, o))
              }
              else
                clear(user) *> Villages.lairCleared(heroDao, user.userId, VillageQuest.StolenArtifact) *>
                  renderer.show(user, Screen(Villages.joined(found, content.text("village.lair.returnHint")), List(outChoice)))
    } yield ()
  }

  // ── Маг Сноходцев ───────────────────────────────────────────────────────────

  /** Продал магу: контракт сорван — береста и договор уходят, деревни снова
    * встречаются; дублоны — герою. Предложения нет (старая кнопка) — показать,
    * что сейчас. */
  private def sell(user: User, c: VillageContract, renderer: Renderer): Task[StateType] =
    readOffer(user).flatMap {
      case None => enter(user, renderer).as(StateType.VillageTrail)
      case Some(o) =>
        val price = Villages.doubloonsLine(content, o.doubloons)
        for {
          hero <- getHero(user)
          _    <- Villages.sell(heroDao, inventoryRepo, user.userId, hero, o.doubloons)
          _    <- clear(user)
          _    <- renderer.show(user, Screen(
                    Villages.joined(content.format(key("sold"), Villages.args(content, c): _*),
                      content.format(key("soldReward"), "offer" -> price)),
                    List(outChoice)))
        } yield StateType.VillageTrail
    }

  /** Отказал магу: свёрток у героя — к старосте за платой. */
  private def refuse(user: User, renderer: Renderer): Task[StateType] =
    readOffer(user).flatMap {
      case None => enter(user, renderer).as(StateType.VillageTrail)
      case Some(_) =>
        clear(user) *> Villages.lairCleared(heroDao, user.userId, VillageQuest.StolenArtifact) *>
          renderer.show(user, Screen(Villages.joined(text("refused"), content.text("village.lair.returnHint")),
            List(outChoice))).as(StateType.VillageTrail)
    }

  // ── Уход и отказ ────────────────────────────────────────────────────────────

  /** «Уйти» — только у ворот, пока погоня не начата: след подождёт, контракт при
    * герое. В пути и у стана — только сойти со следа; маг ждёт ответа. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    busy(user).flatMap {
      case true  => enter(user, renderer).as(StateType.VillageTrail)
      case false => clear(user) *> renderer.show(user, Screen(text("left"), Nil)).as(StateType.Dungeon)
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

  /** «В лабиринт» — после победы или с пустого следа. Посреди погони или пока
    * маг ждёт ответа эта кнопка старая: показать, что сейчас. */
  private def out(user: User, renderer: Renderer): Task[StateType] =
    busy(user).flatMap {
      case true  => enter(user, renderer).as(StateType.VillageTrail)
      case false => clear(user).as(StateType.Dungeon)
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
