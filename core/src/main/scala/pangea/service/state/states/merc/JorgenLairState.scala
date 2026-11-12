package pangea.service.state.states.merc

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.item.QuestItemKind
import pangea.model.quest.Difficulty
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.merc.JorgenLairState._
import pangea.service.state.{CharacterMenu, JorgenQuest, NpcQuestLog, State, UserAction}
import zio.{Task, ZIO}

/** Логово огненных, где пал отряд Йоргена («Жетоны из пепла», см.
  * [[JorgenQuest]]). Сюда ведёт карточка списка отряда — из лабиринта.
  *
  * У входа Йорген говорит, что огненных двое, и берёт меч холода. «В бой» —
  * один бой с обоими элементалями: строй героя, его отряд и Йорген — в строю
  * или гостем (см. `JorgenQuest.withJorgen`). Бой сюжетный: без «Сбежать», а
  * награда как за обычный бой. Добыча возвращает сюда (`Progress`): оба пали —
  * в пепле жетоны, дальше — к Горну.
  *
  * «Уйти», смерть или ничья задание не двигают: список при герое, логово ждёт. */
case class JorgenLairState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "JorgenFight"   -> Target.Run { (u, _, r) => fight(u, r) },
      "JorgenLeave"   -> Target.Run { (u, _, r) => leave(u, r) },
      "JorgenOut"     -> Target.Goto(StateType.Dungeon),
      "OpenCharacter" -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.JorgenLair) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.JorgenLair) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.HeroStats, StateType.JorgenLair)

  /** Вход: оба огненных пали — жетоны; список на руках — вход в логово; иначе —
    * в логове тихо. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      quests   <- NpcQuestLog.load(heroDao, user.userId)
      progress <- readProgress(user)
      _        <- if (!JorgenQuest.toLair(quests)) renderer.show(user, Screen(content.text("jorgen.lair.gone"), List(outChoice)))
                  else if (progress.exists(_.won)) victory(user, renderer)
                  else renderer.show(user, introScreen)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Экраны ──────────────────────────────────────────────────────────────────

  /** Вход в логово: Йорген говорит, что огненных двое, и берёт меч холода;
    * сложность — сразу. */
  private def introScreen: Screen =
    Screen(content.text("jorgen.lair.intro") + "\n\n" + difficultyLine, List(
      content.choice("JorgenFight", "jorgen.lair.fightLabel").copy(row = Some(0)),
      content.choice("OpenCharacter", "jorgen.lair.characterLabel").copy(row = Some(1)),
      content.choice("JorgenLeave", "jorgen.lair.leaveLabel").copy(row = Some(1))))

  private def difficultyLine: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(JorgenQuest.Level))

  private def outChoice = content.choice("JorgenOut", "jorgen.lair.toDungeonLabel")

  // ── Бой ─────────────────────────────────────────────────────────────────────

  /** Бой с обоими огненными. Добыча вернёт сюда — с отметкой, что они пали. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!JorgenQuest.toLair(quests)) enter(user, renderer).as(StateType.JorgenLair)
                else {
                  val battle  = JorgenQuest.lairBattle(hero)
                  val routing = LootData(Nil, Nil, returnState = Some(StateType.JorgenLair),
                                  eventData = Some(Progress(won = true).asJson))
                  heroDao.writeActiveBattle(user.userId, battle.asJson) *>
                    heroDao.writeSceneData(user.userId, routing.asJson) *>
                    renderer.show(user, Screen(content.text("jorgen.lair.fight"), Nil)).as(StateType.Battle)
                }
    } yield res

  /** Оба огненных пали: в пепле — жетоны отряда. */
  private def victory(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      _     <- heroDao.writeSceneData(user.userId, Json.Null)
      found <- JorgenQuest.tokensFound(heroDao, inventoryRepo, itemRepo, user.userId, hero)
      _     <- renderer.show(user, Screen(content.text("jorgen.lair.victory"), Nil))
      _     <- ZIO.when(found)(renderer.show(user, Screen(
                 content.format("marisa.questItemAdded", "item" -> QuestItemKind.JorgenTokens.displayName), Nil)))
      _     <- renderer.show(user, Screen(content.text("jorgen.lair.tokensHint"), List(outChoice)))
    } yield ()

  // ── Уход ────────────────────────────────────────────────────────────────────

  /** «Уйти»: логово ждёт, список при герое. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("jorgen.lair.left"), Nil)).as(StateType.Dungeon)

  private def readProgress(user: User): Task[Option[Progress]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Progress].toOption))

  private def getHero(user: User) =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object JorgenLairState {

  /** Пали ли оба огненных — пишет добыча, читает вход. */
  final case class Progress(won: Boolean)

  object Progress {
    implicit val encoder: Encoder[Progress] = (p: Progress) => Json.obj("jorgenLairWon" -> p.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры, и чужое поле не должно
    // читаться как наше.
    implicit val decoder: Decoder[Progress] = (c: HCursor) => c.get[Boolean]("jorgenLairWon").map(Progress(_))
  }
}
