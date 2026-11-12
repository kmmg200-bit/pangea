package pangea.service.state.states.merc

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.item.QuestItemKind
import pangea.model.quest.{Difficulty, NpcQuest}
import pangea.model.squad.{AllyKind, AllyRates}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.merc.BramblHallState._
import pangea.service.state.{BramblQuest, CharacterMenu, MercQuests, NpcQuestDialog, NpcQuestLog, QuestSupport, State,
  UserAction}
import zio.{Task, ZIO}

/** Зал предков под серебряной жилой («Замок для предков», см. [[BramblQuest]]).
  * Сюда проваливается кирка, когда замок Медноусов у героя на руках.
  *
  * У гробницы герой и Брамбл замечают сразу: каменных стражей двое. «В бой» —
  * один бой с обоими: строй героя, его отряд и Брамбл — в строю или гостем
  * (см. `MercQuests.withMerc`). Бой сюжетный: без «Сбежать», а награда как за
  * обычный бой. Добыча возвращает сюда (`Progress`): оба пали — Брамбл вешает
  * замок на гробницу, задание закрыто, его потолок растёт.
  *
  * «Уйти», смерть или ничья задание не двигают: замок при герое, к залу ведёт
  * любая серебряная жила. */
case class BramblHallState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  /** Задание закрывается здесь — опыт как за задание горожанина. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.BramblLock, "BramblLock")

  private val branch = new Branch(
    routes = Map(
      "BramblFight"   -> Target.Run { (u, _, r) => fight(u, r) },
      "BramblLeave"   -> Target.Run { (u, _, r) => leave(u, r) },
      "BramblOut"     -> Target.Goto(StateType.Dungeon),
      "OpenCharacter" -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.BramblHall) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.BramblHall) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.HeroStats, StateType.BramblHall)

  /** Вход: оба стража пали — замок на гробницу; замок на руках — зал и стражи;
    * иначе — в зале тихо. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      quests   <- NpcQuestLog.load(heroDao, user.userId)
      progress <- readProgress(user)
      _        <- if (!BramblQuest.toHall(quests)) renderer.show(user, Screen(content.text("bramblLock.hall.gone"), List(outChoice)))
                  else if (progress.exists(_.won)) hangLock(user, renderer)
                  else renderer.show(user, introScreen)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Экраны ──────────────────────────────────────────────────────────────────

  /** Зал предков: у гробницы — двое каменных стражей, герой и Брамбл видят это
    * разом; сложность — сразу. */
  private def introScreen: Screen =
    Screen(content.text("bramblLock.hall.intro") + "\n\n" + difficultyLine, List(
      content.choice("BramblFight", "bramblLock.hall.fightLabel").copy(row = Some(0)),
      content.choice("OpenCharacter", "bramblLock.hall.characterLabel").copy(row = Some(1)),
      content.choice("BramblLeave", "bramblLock.hall.leaveLabel").copy(row = Some(1))))

  private def difficultyLine: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(BramblQuest.Level))

  private def outChoice = content.choice("BramblOut", "bramblLock.hall.toDungeonLabel")

  // ── Бой ─────────────────────────────────────────────────────────────────────

  /** Бой с обоими стражами. Добыча вернёт сюда — с отметкой, что они пали. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!BramblQuest.toHall(quests)) enter(user, renderer).as(StateType.BramblHall)
                else {
                  val battle  = BramblQuest.hallBattle(hero)
                  val routing = LootData(Nil, Nil, returnState = Some(StateType.BramblHall),
                                  eventData = Some(Progress(won = true).asJson))
                  heroDao.writeActiveBattle(user.userId, battle.asJson) *>
                    heroDao.writeSceneData(user.userId, routing.asJson) *>
                    renderer.show(user, Screen(content.text("bramblLock.hall.fight"), Nil)).as(StateType.Battle)
                }
    } yield res

  /** Оба стража пали: Брамбл вешает замок на гробницу — задание закрыто, замок
    * уходит из сумки, опыт — как за задание горожанина, потолок Брамбла у героя
    * растёт (при герое он — сразу, см. MercQuests). */
  private def hangLock(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      _    <- heroDao.writeSceneData(user.userId, Json.Null)
      _    <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.FamilyLock, QuestItemKind.BramblCast))
      done <- quest.complete(user, hero, identity)
      (_, expLine) = done
      _    <- MercQuests.grow(heroDao, user.userId, hero, AllyKind.Gnome)
      grew  = content.format("bramblLock.hall.grown", "name" -> AllyKind.Gnome.name,
                "lvl" -> (AllyKind.Gnome.maxLvl + AllyRates.QuestGrowth).toString)
      _    <- renderer.show(user, Screen(content.text("bramblLock.hall.victory"), Nil))
      _    <- renderer.show(user, Screen(List(grew, expLine, content.text("bramblLock.questDone")).mkString("\n\n"),
                List(outChoice)))
    } yield ()

  // ── Уход ────────────────────────────────────────────────────────────────────

  /** «Уйти»: зал ждёт, замок при герое — вниз ведёт любая серебряная жила. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("bramblLock.hall.left"), Nil)).as(StateType.Dungeon)

  private def readProgress(user: User): Task[Option[Progress]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Progress].toOption))

  private def getHero(user: User) =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object BramblHallState {

  /** Пали ли оба стража — пишет добыча, читает вход. */
  final case class Progress(won: Boolean)

  object Progress {
    implicit val encoder: Encoder[Progress] = (p: Progress) => Json.obj("bramblHallWon" -> p.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры, и чужое поле не должно
    // читаться как наше.
    implicit val decoder: Decoder[Progress] = (c: HCursor) => c.get[Boolean]("bramblHallWon").map(Progress(_))
  }
}
