package pangea.service.state.states.merc

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.item.QuestItemKind
import pangea.model.quest.{Difficulty, NpcQuest}
import pangea.model.skill.MonsterEnergy
import pangea.model.squad.{AllyKind, AllyRates}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.merc.FaelinRootsState._
import pangea.service.state.{CharacterMenu, FaelinQuest, MercQuests, NpcQuestDialog, NpcQuestLog, QuestSupport, State, UserAction}
import zio.{Random, Task, ZIO}

/** Старые корни изгороди («Отданный лук», см. [[FaelinQuest]]). Сюда ведёт
  * карточка срубленного лука — из лабиринта.
  *
  * У корней — те, с кем придётся драться; «В бой» — один бой со всеми: строй
  * героя, его отряд и Фаэлин — в строю или гостем (см. `MercQuests.withMerc`).
  * Бой сюжетный: без «Сбежать», а награда как за обычный бой. Добыча
  * возвращает сюда (`Progress`): отбились — изгородь забирает срубленный лук и
  * отдаёт Фаэлину новый, готовый, — задание закрыто, его потолок растёт.
  *
  * «Уйти», смерть или ничья задание не двигают: лук при герое, к корням можно
  * вернуться. */
case class FaelinRootsState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  /** Задание закрывается здесь — опыт как за задание горожанина. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.FaelinBow, "FaelinBow")

  private val branch = new Branch(
    routes = Map(
      "FaelinFight"   -> Target.Run { (u, _, r) => fight(u, r) },
      "FaelinLeave"   -> Target.Run { (u, _, r) => leave(u, r) },
      "FaelinOut"     -> Target.Goto(StateType.Dungeon),
      "OpenCharacter" -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.FaelinRoots) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.FaelinRoots) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.HeroStats, StateType.FaelinRoots)

  /** Вход: отбились — изгородь отдаёт новый лук; срубленный на руках — корни и бой;
    * иначе — у корней тихо. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      quests   <- NpcQuestLog.load(heroDao, user.userId)
      progress <- readProgress(user)
      _        <- if (!FaelinQuest.toRoots(quests)) renderer.show(user, Screen(content.text("faelinBow.roots.gone"), List(outChoice)))
                  else if (progress.exists(_.won)) branchGiven(user, renderer)
                  else renderer.show(user, introScreen)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Экраны ──────────────────────────────────────────────────────────────────

  /** Старые корни и те, кто у них; сложность — сразу. */
  private def introScreen: Screen =
    Screen(content.text("faelinBow.roots.intro") + "\n\n" + difficultyLine, List(
      content.choice("FaelinFight", "faelinBow.roots.fightLabel").copy(row = Some(0)),
      content.choice("OpenCharacter", "faelinBow.roots.characterLabel").copy(row = Some(1)),
      content.choice("FaelinLeave", "faelinBow.roots.leaveLabel").copy(row = Some(1))))

  private def difficultyLine: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(FaelinQuest.Level))

  private def outChoice = content.choice("FaelinOut", "faelinBow.roots.toDungeonLabel")

  // ── Бой ─────────────────────────────────────────────────────────────────────

  /** Бой у корней. Добыча вернёт сюда — с отметкой, что отбились. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!FaelinQuest.toRoots(quests)) enter(user, renderer).as(StateType.FaelinRoots)
                else for {
                  foes     <- ZIO.succeed(FaelinQuest.foes)
                  energies <- ZIO.foreach(foes)(m =>
                                Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                  .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                  battle    = FaelinQuest.rootsBattle(hero, foes, energies)
                  routing   = LootData(Nil, Nil, returnState = Some(StateType.FaelinRoots),
                                eventData = Some(Progress(won = true).asJson))
                  _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _        <- renderer.show(user, Screen(content.text("faelinBow.roots.fight"), Nil))
                } yield StateType.Battle
    } yield res

  /** Отбились: изгородь забирает срубленный лук и отдаёт Фаэлину новый — готовый.
    * Задание закрыто, лук уходит из сумки, опыт — как за задание горожанина,
    * потолок Фаэлина у героя растёт (при герое он — сразу, см. MercQuests). */
  private def branchGiven(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      _    <- heroDao.writeSceneData(user.userId, Json.Null)
      _    <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.CutBow))
      done <- quest.complete(user, hero, identity)
      (_, expLine) = done
      _    <- MercQuests.grow(heroDao, user.userId, hero, AllyKind.Elf)
      grew  = content.format("faelinBow.roots.grown", "name" -> AllyKind.Elf.name,
                "lvl" -> (AllyKind.Elf.maxLvl + AllyRates.QuestGrowth).toString)
      _    <- renderer.show(user, Screen(content.text("faelinBow.roots.victory"), Nil))
      _    <- renderer.show(user, Screen(List(grew, expLine, content.text("faelinBow.questDone")).mkString("\n\n"),
                List(outChoice)))
    } yield ()

  // ── Уход ────────────────────────────────────────────────────────────────────

  /** «Уйти»: лук при герое — к корням можно вернуться. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("faelinBow.roots.left"), Nil)).as(StateType.Dungeon)

  private def readProgress(user: User): Task[Option[Progress]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Progress].toOption))

  private def getHero(user: User) =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object FaelinRootsState {

  /** Отбились ли у корней — пишет добыча, читает вход. */
  final case class Progress(won: Boolean)

  object Progress {
    implicit val encoder: Encoder[Progress] = (p: Progress) => Json.obj("faelinRootsWon" -> p.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры, и чужое поле не должно
    // читаться как наше.
    implicit val decoder: Decoder[Progress] = (c: HCursor) => c.get[Boolean]("faelinRootsWon").map(Progress(_))
  }
}
