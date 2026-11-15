package pangea.service.state.states.barti

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.quest.{Difficulty, NpcQuest, QuestOutcome}
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.parcel.Parcels
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.story.StoryFight
import pangea.service.state.{BartiSaltQuest, BigDealQuest, NpcQuestDialog, NpcQuestLog, QuestOutcomeLog, State, UserAction}
import zio.{Random, Task, ZIO}

/** «Большое дело», шаг 3: каджитский караван у Сухого колодца на Западной
  * дороге, ночью (см. BigDealQuest). Сюда ведёт карточка адреса Барти, из
  * города. Барти везёт товар героя — и свой запас сверху. Каджиты будто
  * готовят подвох: ножи за спиной, «золото потом». «Поставить ящики» —
  * каджиты отступают в темноту, гаснут костры, и из-за повозок выходит ночная
  * стража: сюжетный бой с отрядом героя, без «Сбежать», награда как за
  * обычный бой. Добыча возвращает сюда с отметкой победы
  * ([[BartiCaravanState.Won]]): каджиты платят дублонами сполна, а Барти,
  * когда отходят от костров, проговаривается, что разболтал о сделке. Пал —
  * задание всё равно закрыто (см. DeathState). «Уйти» — караван подождёт.
  * Герой-каджит: караван встречает собрата теплее, а в засаде двое капитанов
  * каравана сразу встают на его сторону.
  *
  * «Соль» (только одиночка, см. BartiSaltQuest): тот же караван везёт соль в
  * Загон, и место в повозке для героя нашлось. «Ехать с караваном» — на
  * соляной колее дозор Загона: сюжетный бой, капитаны Ра'Тхар и Ма'Кесс в
  * строю; добыча вернёт сюда с отметкой победы — Та'Нахир платит за охрану
  * соли, задание закрыто. */
case class BartiCaravanState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  outcomes:      Option[QuestOutcomeDao] = None,
  // Амулет от каравана: сохранить и, если не влез в сумку, — посылкой.
  itemRepo:      Option[ItemRepository] = None,
  parcels:       Option[Parcels] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "CaravanBoxes" -> Target.Run { (u, _, r) => boxes(u, r) },
      "CaravanLeave" -> Target.Run { (u, _, r) => leave(u, r) },
      "SaltRide"     -> Target.Run { (u, _, r) => ride(u, r) },
      "SaltLeave"    -> Target.Run { (u, _, r) => saltLeave(u, r) },
      "CaravanOut"   -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.BartiCaravan) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.BartiCaravan, StateType.GlobalMap)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      data   <- heroDao.readSceneData(user.userId)
      won     = data.exists(_.as[BartiCaravanState.Won].exists(_.won))
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (won && BigDealQuest.toCaravan(quests)) paid(user, renderer)
                else if (BigDealQuest.toCaravan(quests)) getHero(user).flatMap(h => renderer.show(user, caravanScreen(h)))
                // «Соль»: соляной караван в Загон.
                else if (StoryFight.won(data, BartiCaravanState.SaltWonKey) && BartiSaltQuest.toCaravan(quests)) saltWon(user, renderer)
                else if (BartiSaltQuest.toCaravan(quests)) renderer.show(user, saltScreen)
                else renderer.show(user, Screen(content.text("barti.caravan.gone"), List(out)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def out = content.choice("CaravanOut", "barti.caravan.outLabel")

  /** Свой текст для героя-каджита: `base` или `baseKin`. */
  private def kin(hero: Hero, base: String): String =
    content.text(if (BigDealQuest.isKin(hero)) s"barti.caravan.${base}Kin" else s"barti.caravan.$base").trim

  /** Караван ночью: Барти с тачкой, каджиты с ножами, «золото — потом». */
  private def caravanScreen(hero: Hero): Screen =
    Screen(
      kin(hero, "text") + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(BigDealQuest.Level)),
      List(
        content.choice("CaravanBoxes", "barti.caravan.boxesLabel").copy(row = Some(0)),
        content.choice("CaravanLeave", "barti.caravan.leaveLabel").copy(row = Some(0))))

  /** «Уйти»: сделка ждёт следующей ночи. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("barti.caravan.left"), Nil)).as(StateType.GlobalMap)

  /** «Поставить ящики»: каджиты отступают в темноту — и выходит ночная стража.
    * Бой — капитан и трое уровня задания, отряд с героем; добыча вернёт сюда
    * с отметкой, что стража перебита. */
  private def boxes(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!BigDealQuest.toCaravan(quests)) enter(user, renderer).as(StateType.BartiCaravan)
                else for {
                  guards   <- ZIO.succeed(BigDealQuest.guards(content.text("barti.caravan.captainName")))
                  energies <- ZIO.foreach(guards)(m =>
                                Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                  .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                  plain     = SoloPveBattle.fromGroup(guards, hero, energies).copy(story = Some(BigDealQuest.AmbushStory))
                  battle    = if (BigDealQuest.isKin(hero)) BigDealQuest.withCaptains(plain, content.list("barti.caravan.kinCaptains"))
                              else plain
                  routing   = LootData(Nil, Nil, returnState = Some(StateType.BartiCaravan),
                                eventData = Some(BartiCaravanState.WonMark.asJson))
                  _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _        <- renderer.show(user, Screen(kin(hero, "ambush"), Nil))
                  _        <- renderer.show(user, Screen(content.text("barti.caravan.fightStart"), Nil))
                } yield StateType.Battle
    } yield res

  /** Стража перебита: каджиты возвращаются и платят сполна; отойдя от костров,
    * Барти проговаривается и передаёт амулет от каравана. Задание закрыто;
    * исход — в подсчёт «как и вы». */
  private def paid(user: User, renderer: Renderer): Task[Unit] =
    for {
      _     <- heroDao.writeSceneData(user.userId, Json.Null)
      hero  <- getHero(user)
      note  <- BigDealQuest.pay(heroDao, inventoryRepo, itemRepo, parcels, content, user, hero, BigDealQuest.WonDoubloons,
                 "barti.caravan.reward", "barti.caravan.amulet")
      stats <- if (note.isDefined) QuestOutcomeLog.recordAndCount(outcomes, content, hero, QuestOutcome.BigDealWon)
               else ZIO.none
      _     <- renderer.show(user, Screen(kin(hero, "paid"), Nil))
      _     <- renderer.show(user, Screen(content.text("barti.caravan.slip").trim, Nil))
      _     <- renderer.show(user, Screen(note.fold("")(QuestOutcomeLog.withStats(_, stats)), List(out)))
    } yield ()

  // ── «Соль» ─────────────────────────────────────────────────────────────────

  /** «Уйти»: соляной караван подождёт ещё ночь. */
  private def saltLeave(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("barti.salt.left"), Nil)).as(StateType.GlobalMap)

  /** Соляной караван у колодца: Та'Нахир, мешки с солью и двое капитанов, что
    * поедут рядом с героем. */
  private def saltScreen: Screen =
    Screen(
      content.text("barti.salt.caravan").trim + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(BartiSaltQuest.Level)),
      List(
        content.choice("SaltRide", "barti.salt.rideLabel").copy(row = Some(0)),
        content.choice("SaltLeave", "barti.salt.leaveLabel").copy(row = Some(0))))

  /** «Ехать с караваном»: на соляной колее — дозор Загона, бой; капитаны
    * каравана в строю. Добыча вернёт сюда с отметкой победы. */
  private def ride(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!BartiSaltQuest.toCaravan(quests)) enter(user, renderer).as(StateType.BartiCaravan)
      else for {
        hero <- getHero(user)
        _    <- StoryFight.start(heroDao, user, hero, BartiSaltQuest.foes(content.text("barti.salt.leaderName")),
                  BartiSaltQuest.captains(content), BartiSaltQuest.FightStory, StateType.BartiCaravan,
                  BartiCaravanState.SaltWonKey)
        _    <- renderer.show(user, Screen(content.text("barti.salt.road").trim, Nil))
      } yield StateType.Battle
    }

  /** Дозор побит: Та'Нахир платит за охрану соли и повезёт героя к печи, когда
    * шум уляжется. Задание закрыто. */
  private def saltWon(user: User, renderer: Renderer): Task[Unit] =
    for {
      _    <- heroDao.writeSceneData(user.userId, Json.Null)
      hero <- getHero(user)
      paid  = hero.copy(doubloons = hero.doubloons + BartiSaltQuest.Doubloons)
      _    <- heroDao.updateDoubloons(user.userId, paid.doubloons)
      done <- NpcQuestDialog(heroDao, content, NpcQuest.BartiSalt, "BartiSalt").complete(user, paid, identity)
      (_, expLine) = done
      _    <- renderer.show(user, Screen(List(
                content.text("barti.salt.won").trim,
                content.format("barti.salt.reward", "doubloons" -> BartiSaltQuest.Doubloons.toString, "exp" -> expLine),
                content.text("barti.salt.questDone")).mkString("\n\n"), List(out)))
    } yield ()

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object BartiCaravanState {

  /** Отметка в scene_data: дозор Загона на соляной колее побит («Соль»). */
  val SaltWonKey: String = "bartiSaltWon"

  /** Отметка в scene_data: ночная стража у каравана перебита. */
  final case class Won(won: Boolean = true)

  object Won {
    implicit val encoder: Encoder[Won] = (w: Won) => Json.obj("bartiAmbushWon" -> w.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры.
    implicit val decoder: Decoder[Won] = (c: HCursor) => c.get[Boolean]("bartiAmbushWon").map(Won(_))
  }

  val WonMark: Won = Won(won = true)
}
