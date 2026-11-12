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
import pangea.service.state.states.merc.PlukhHuntState._
import pangea.service.state.{CharacterMenu, MercQuests, NpcQuestDialog, NpcQuestLog, PlukhQuest, QuestSupport, State, UserAction}
import zio.{Random, Task, ZIO}

/** Ловчие Сноходцев на зов перевёрнутой печати («Чужой багор», см.
  * [[PlukhQuest]]). Сюда ведёт карточка печати — из лабиринта, когда Густаво
  * уже перевернул зов.
  *
  * Печать вспыхивает, и на зов выходят ловчие-каджиты с сетями и кинжалами
  * молнии; Плюх их помнит — и рад драться, хоть и знает, что гильдия куда
  * сильнее ловчих. «В бой» — один бой со всеми: строй героя, его отряд и Плюх
  * — в строю или гостем (см. `MercQuests.withMerc`). Бой сюжетный: без
  * «Сбежать», а награда как за обычный бой. Добыча возвращает сюда
  * (`Progress`): ловчие пали — печать тускнеет, кинжалы Плюх берёт себе,
  * багор его по праву, задание закрыто, его потолок растёт.
  *
  * «Уйти», смерть или ничья задание не двигают: печать при герое, позвать
  * ловчих можно снова. */
case class PlukhHuntState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  /** Задание закрывается здесь — опыт как за задание горожанина. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.PlukhGaff, "PlukhGaff")

  private val branch = new Branch(
    routes = Map(
      "PlukhFight"    -> Target.Run { (u, _, r) => fight(u, r) },
      "PlukhLeave"    -> Target.Run { (u, _, r) => leave(u, r) },
      "PlukhOut"      -> Target.Goto(StateType.Dungeon),
      "OpenCharacter" -> Target.Run { (u, _, _) => CharacterMenu.open(heroDao, u.userId, StateType.PlukhHunt) }
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.PlukhHunt) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.HeroStats, StateType.PlukhHunt)

  /** Вход: ловчие пали — багор Плюха по праву; зов перевёрнут — ловчие идут;
    * иначе — тихо. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      quests   <- NpcQuestLog.load(heroDao, user.userId)
      progress <- readProgress(user)
      _        <- if (!PlukhQuest.toHunt(quests)) renderer.show(user, Screen(content.text("plukhGaff.hunt.gone"), List(outChoice)))
                  else if (progress.exists(_.won)) settle(user, renderer)
                  else renderer.show(user, introScreen)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Экраны ──────────────────────────────────────────────────────────────────

  /** Печать вспыхивает, на зов выходят ловчие; сложность — сразу. */
  private def introScreen: Screen =
    Screen(content.text("plukhGaff.hunt.intro") + "\n\n" + difficultyLine, List(
      content.choice("PlukhFight", "plukhGaff.hunt.fightLabel").copy(row = Some(0)),
      content.choice("OpenCharacter", "plukhGaff.hunt.characterLabel").copy(row = Some(1)),
      content.choice("PlukhLeave", "plukhGaff.hunt.leaveLabel").copy(row = Some(1))))

  private def difficultyLine: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(PlukhQuest.Level))

  private def outChoice = content.choice("PlukhOut", "plukhGaff.hunt.toDungeonLabel")

  // ── Бой ─────────────────────────────────────────────────────────────────────

  /** Бой с ловчими. Добыча вернёт сюда — с отметкой, что они пали. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!PlukhQuest.toHunt(quests)) enter(user, renderer).as(StateType.PlukhHunt)
                else for {
                  foes     <- ZIO.succeed(PlukhQuest.hunters)
                  energies <- ZIO.foreach(foes)(m =>
                                Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                  .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                  battle    = PlukhQuest.huntBattle(hero, foes, energies)
                  routing   = LootData(Nil, Nil, returnState = Some(StateType.PlukhHunt),
                                eventData = Some(Progress(won = true).asJson))
                  _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _        <- renderer.show(user, Screen(content.text("plukhGaff.hunt.fight"), Nil))
                } yield StateType.Battle
    } yield res

  /** Ловчие пали: печать тускнеет — звать багор домой больше некому. Задание
    * закрыто, печать уходит из сумки, опыт — как за задание горожанина, потолок
    * Плюха у героя растёт (при герое он — сразу, см. MercQuests). */
  private def settle(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      _    <- heroDao.writeSceneData(user.userId, Json.Null)
      _    <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.SnowwalkerSeal))
      done <- quest.complete(user, hero, identity)
      (_, expLine) = done
      _    <- MercQuests.grow(heroDao, user.userId, hero, AllyKind.Murloc)
      grew  = content.format("plukhGaff.hunt.grown", "name" -> AllyKind.Murloc.name,
                "lvl" -> (AllyKind.Murloc.maxLvl + AllyRates.QuestGrowth).toString)
      _    <- renderer.show(user, Screen(content.text("plukhGaff.hunt.victory"), Nil))
      _    <- renderer.show(user, Screen(List(grew, expLine, content.text("plukhGaff.questDone")).mkString("\n\n"),
                List(outChoice)))
    } yield ()

  // ── Уход ────────────────────────────────────────────────────────────────────

  /** «Уйти»: печать при герое — позвать ловчих можно снова. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("plukhGaff.hunt.left"), Nil)).as(StateType.Dungeon)

  private def readProgress(user: User): Task[Option[Progress]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Progress].toOption))

  private def getHero(user: User) =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object PlukhHuntState {

  /** Пали ли ловчие — пишет добыча, читает вход. */
  final case class Progress(won: Boolean)

  object Progress {
    implicit val encoder: Encoder[Progress] = (p: Progress) => Json.obj("plukhHuntWon" -> p.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры, и чужое поле не должно
    // читаться как наше.
    implicit val decoder: Decoder[Progress] = (c: HCursor) => c.get[Boolean]("plukhHuntWon").map(Progress(_))
  }
}
