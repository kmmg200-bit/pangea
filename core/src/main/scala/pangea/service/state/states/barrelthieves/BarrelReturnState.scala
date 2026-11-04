package pangea.service.state.states.barrelthieves

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.QuestItemKind
import pangea.model.quest.NpcQuest
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.barrel.BarrelRepository
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{BarrelThievesQuest, NpcQuestLog, QuestSupport, StackMove, State, UserAction}
import zio.{Random, Task, ZIO}

/** «Вскрытая бочка», развязка: банда разбита (добыча боя привела сюда с
  * отметкой [[BarrelThievesState.Won]]) — украденное возвращается в бочку.
  * Гоблины успели поработать: вещи из наборов теперь вещи «Пройдохи»
  * (BarrelThievesQuest.rework). Улика уходит, задание закрыто; впервые —
  * 4000 серебра и «Своё не отдам», дальше — 500. Сцена — узел-эффект: экрана
  * у неё нет, и герой сразу оказывается у своей бочки. Пришли без отметки или
  * задание уже закрыто — просто к бочке. */
case class BarrelReturnState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  barrelRepo:    BarrelRepository,
  content:       SceneContent
) extends State {

  override def targetStates: Set[StateType] = Set(StateType.UnassumingBarrel)

  override def autoAdvance: Option[StateType] = Some(StateType.UnassumingBarrel)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      won    <- heroDao.readSceneData(user.userId).map(_.exists(_.as[BarrelThievesState.Won].exists(_.won)))
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- heroDao.writeSceneData(user.userId, Json.Null)
      _      <- ZIO.when(won && BarrelThievesQuest.onTrail(quests))(giveBack(user, renderer))
    } yield ()

  /** Банда разбита: своё — в бочку, перешитое — «Пройдохой», награда. */
  private def giveBack(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero    <- getHero(user)
      seed    <- Random.nextLong
      barrel  <- barrelRepo.get(hero.id).mapError(asThrowable)
      stolen   = barrel.thieves.flatMap(_.stolen).map(_.items).getOrElse(Nil)
      (all, changed) = BarrelThievesQuest.rework(stolen, Rng(seed))
      back    <- barrelRepo.restore(hero.id, _ => all).mapError(asThrowable)
      _       <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.BarrelClue))
      silver   = BarrelThievesQuest.silverFor(hero)
      first    = BarrelThievesQuest.firstTime(hero)
      _       <- heroDao.updateSilver(user.userId, hero.silver + silver)
      _       <- NpcQuestLog.modify(heroDao, user.userId)(_.finish(NpcQuest.BarrelThieves))
      returned = back.map(g => StackMove.names(g.items)).filter(_.nonEmpty)
      lines    = List(
                   Some(content.text("barrelThieves." + BarrelThievesQuest.key(hero, "back"))),
                   returned.map(n => content.format("barrelThieves.returned", "items" -> n)),
                   back.map(_.silver).filter(_ > 0L).map(s => content.format("barrelThieves.returnedSilver", "silver" -> s.toString)),
                   Option.when(changed.nonEmpty)(content.format("barrelThieves.reworked", "items" -> StackMove.names(changed))),
                   Some(content.format("barrelThieves.reward", "silver" -> silver.toString))
                 ).flatten
      _       <- renderer.show(user, Screen(lines.mkString("\n\n"), Nil))
      _       <- ZIO.when(first)(QuestSupport.grant(heroDao, content, user, hero.copy(silver = hero.silver + silver),
                   Achievement.OwnBack, renderer))
      _       <- renderer.show(user, Screen(content.text("barrelThieves.questDone"), Nil))
    } yield ()

  // Узел-эффект: ввода у него нет, дальше — бочка.
  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    ZIO.succeed(StateType.UnassumingBarrel)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}
