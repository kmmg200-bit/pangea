package pangea.service.state.states.hedgesong

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.Difficulty
import pangea.model.skill.MonsterEnergy
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.hedgesong.HedgeSongState._
import pangea.service.state.{CharacterMenu, HedgeSongQuest, NpcQuestLog, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Тот, кто уговаривает деревья» в лабиринте. Сюда ведёт редкая встреча с
  * осмотра уровня (RareQuests, см. DungeonState): эльф Элиар поёт изгороди,
  * прося у неё ветвь, а на песню идут орки-лесорубы.
  *
  * «Встать на стражу» — бой с первой волной. Волн три (HedgeSongQuest.wave),
  * каждая — свой бой, как у одурманенного каравана: строй героя, его отряд и
  * Элиар — по силе именной эльф уровня задания. Бой сюжетный (`story =
  * hedgeSong`): без «Сбежать» и без призыва сородичей, а награда как за обычный
  * бой — опыт и добыча с каждого павшего по уровню задания. Добыча каждой
  * волны возвращает сюда (`Progress(cleared)`): между волнами — «К бою»,
  * «Персонаж» и «Уйти». Третья отбита — изгородь отдаёт ветвь, задание взято.
  *
  * «Не сейчас», «Уйти» между волнами, смерть или ничья задание не начинают:
  * эльфа надо встретить снова.
  *
  * Фаэлин Тихострел при герое — младший брат Элиара: на встрече, между
  * волнами и в конце у него свои слова (`hedgeSong.faelin.*`), а на «Уйти» он
  * остаётся у изгороди с братом и сутки не в отряде. */
case class HedgeSongState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "HedgeFight"    -> Target.Run { (u, _, r) => fight(u, r) },
      "HedgeNotNow"   -> Target.Run { (u, _, r) => notNow(u, r) },
      "HedgeLeave"    -> Target.Run { (u, _, r) => leave(u, r) },
      "HedgeOut"      -> Target.Goto(StateType.Dungeon),
      "OpenCharacter" -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.HedgeSong) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.HedgeSong) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.HeroStats, StateType.HedgeSong)

  /** Вход: третья волна отбита — ветвь; между волнами — следующая; задания ещё
    * не было — встреча. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now      <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero     <- getHero(user)
      quests   <- NpcQuestLog.load(heroDao, user.userId)
      progress <- readProgress(user)
      fresh     = HedgeSongQuest.fresh(quests)
      _        <- progress.map(_.cleared) match {
                    case Some(n) if fresh && n >= HedgeSongQuest.Waves => victory(user, hero, now, renderer)
                    case Some(n) if fresh && n > 0                     => renderer.show(user, betweenScreen(n, hero))
                    case _ if fresh                                    => renderer.show(user, meetScreen(hero))
                    case _ => renderer.show(user, Screen(content.text("hedgeSong.gone"), List(outChoice)))
                  }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Экраны ──────────────────────────────────────────────────────────────────

  /** Эльф поёт, на песню идут орки; сложность — сразу. Фаэлин при герое —
    * узнаёт брата по песне. */
  private def meetScreen(hero: Hero): Screen =
    Screen(withFaelin(hero, content.text("hedgeSong." + HedgeSongQuest.key(hero, "meet")), "meet") +
      "\n\n" + difficultyLine, List(
      content.choice("HedgeFight", "hedgeSong.guardLabel").copy(row = Some(0)),
      content.choice("HedgeNotNow", "hedgeSong.notNowLabel").copy(row = Some(0))))

  /** Волна `cleared` отбита, следующая уже идёт. */
  private def betweenScreen(cleared: Int, hero: Hero): Screen =
    Screen(withFaelin(hero, content.text(s"hedgeSong.between$cleared"), s"between$cleared"), List(
      content.choice("HedgeFight", "hedgeSong.nextLabel").copy(row = Some(0)),
      content.choice("OpenCharacter", "hedgeSong.characterLabel").copy(row = Some(1)),
      content.choice("HedgeLeave", "hedgeSong.leaveLabel").copy(row = Some(1))))

  private def difficultyLine: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(HedgeSongQuest.Level))

  /** Текст сцены, а Фаэлин при герое — и его слова отдельным абзацем
    * (`hedgeSong.faelin.<line>`). Слова одни на любую кровь героя. */
  private def withFaelin(hero: Hero, text: String, line: String): String =
    if (HedgeSongQuest.faelin(hero)) text + "\n\n" + content.text(s"hedgeSong.faelin.$line") else text

  private def outChoice = content.choice("HedgeOut", "hedgeSong.toDungeonLabel")

  // ── Волны ───────────────────────────────────────────────────────────────────

  /** Следующая волна: орки уровня задания, отряд героя и Элиар в строю. Добыча
    * вернёт сюда с числом отбитых волн. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero     <- getHero(user)
      quests   <- NpcQuestLog.load(heroDao, user.userId)
      progress <- readProgress(user)
      cleared   = progress.map(_.cleared).getOrElse(0)
      res      <- if (!HedgeSongQuest.fresh(quests) || cleared >= HedgeSongQuest.Waves) enter(user, renderer).as(StateType.HedgeSong)
                  else for {
                    seed     <- Random.nextLong
                    n         = cleared + 1
                    (orcs, _) = HedgeSongQuest.wave(n, Rng(seed))
                    energies <- ZIO.foreach(orcs)(m =>
                                  Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                    .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                    battle    = HedgeSongQuest.withElf(SoloPveBattle.fromGroup(orcs, hero, energies)
                                  .copy(story = Some(HedgeSongQuest.WaveStory), lootLevel = Some(HedgeSongQuest.Level.toLong)))
                    routing   = LootData(Nil, Nil, returnState = Some(StateType.HedgeSong),
                                  eventData = Some(Progress(n).asJson))
                    _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                    _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                    _        <- renderer.show(user, Screen(content.text(s"hedgeSong.wave$n"), Nil))
                  } yield StateType.Battle
    } yield res

  /** Третья волна отбита: песня кончилась, изгородь отдала ветвь. Фаэлин при
    * герое — братья прощаются. */
  private def victory(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[Unit] =
    for {
      _     <- heroDao.writeSceneData(user.userId, Json.Null)
      given <- HedgeSongQuest.branchGiven(heroDao, inventoryRepo, itemRepo, user.userId, hero, nowMs)
      _     <- renderer.show(user, Screen(withFaelin(hero,
                 content.text("hedgeSong." + HedgeSongQuest.key(hero, "thanks")), "thanks"), Nil))
      _     <- ZIO.when(given)(renderer.show(user, Screen(
                 content.format("marisa.questItemAdded", "item" -> QuestItemKind.HedgeBranch.displayName), Nil)))
      _     <- renderer.show(user, Screen(content.text("hedgeSong.branchHint"), List(outChoice)))
    } yield ()

  // ── Отказ и уход ────────────────────────────────────────────────────────────

  /** «Не сейчас»: задание не начато, эльф может встретиться снова. Фаэлин при
    * герое уходит с ним — орки ещё не пришли, — только оглядывается. */
  private def notNow(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!HedgeSongQuest.fresh(quests)) enter(user, renderer).as(StateType.HedgeSong)
                else heroDao.writeSceneData(user.userId, Json.Null) *>
                  renderer.show(user, Screen(withFaelin(hero, content.text("hedgeSong.notNow"), "notNow"), Nil))
                    .as(StateType.Dungeon)
    } yield res

  /** «Уйти» между волнами: песня обрывается, задание не начато. Фаэлин при
    * герое не уходит: остаётся у изгороди за спиной брата — из отряда вон, за
    * стол таверны сядет через сутки (без свитка, см. `Squad.stayedBehind`), — и
    * песня не обрывается. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      stays   = HedgeSongQuest.faelin(hero)
      res    <- if (!HedgeSongQuest.fresh(quests)) enter(user, renderer).as(StateType.HedgeSong)
                else for {
                  _ <- heroDao.writeSceneData(user.userId, Json.Null)
                  _ <- ZIO.when(stays)(heroDao.updateSquad(user.userId, hero.squad.stayedBehind(AllyKind.Elf, now)))
                  _ <- renderer.show(user, Screen(content.text(if (stays) "hedgeSong.faelin.stays" else "hedgeSong.left"), Nil))
                } yield StateType.Dungeon
    } yield res

  private def readProgress(user: User): Task[Option[Progress]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Progress].toOption))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object HedgeSongState {

  /** Сколько волн уже отбито. */
  final case class Progress(cleared: Int)

  object Progress {
    implicit val encoder: Encoder[Progress] = (p: Progress) => Json.obj("hedgeSongCleared" -> p.cleared.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры (сцены «Персонажа» —
    // рядом, под своими ключами), и чужое поле не должно читаться как наше.
    implicit val decoder: Decoder[Progress] = (c: HCursor) => c.get[Int]("hedgeSongCleared").map(Progress(_))
  }
}
