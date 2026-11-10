package pangea.service.state.states.barti

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{BigDealQuest, NpcQuestLog, QuestSupport, State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Дом Барти — синяя дверь на Торговой площади (см. BigDealQuest). Сюда ведёт
  * карточка адреса, из города. Впервые — затея с контрабандой: каджитам
  * нужны шнапс и живая вода, платят они золотом; задание взято. Дальше Барти
  * принимает товар хоть по одной склянке («📦 Отдать товар» берёт из сумки
  * всё, что ему ещё нужно) и складывает у себя. Собрано всё — ждать каравана:
  * пока герой ниже уровня задания, Барти говорит «рано, караван не пришёл»
  * (для этого задания так решил пользователь); дорос — караван пришёл, и
  * задание ведёт к нему. «Уйти» — на Торговую площадь. */
case class BartiHouseState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "BartiGive"  -> Target.Run { (u, _, r) => give(u, r) },
      "BartiLeave" -> Target.Goto(StateType.MarketSquare)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.BartiHouse) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      items  <- bag(hero)
      _      <- if (BigDealQuest.canBegin(quests) && QuestSupport.hasItem(items, QuestItemKind.BartiAddress))
                  begin(user, quests, items, renderer)
                else if (BigDealQuest.collecting(quests)) renderer.show(user, stockScreen(None, quests, items))
                else if (BigDealQuest.waiting(quests)) gate(user, hero, quests, None, renderer)
                else if (BigDealQuest.toCaravan(quests)) renderer.show(user, Screen(content.text("barti.house.arrived").trim, List(leave)))
                else renderer.show(user, Screen(content.text("barti.house.closed"), List(leave)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave: Choice =
    content.choice("BartiLeave", "barti.house.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(1))

  /** Впервые: затея с контрабандой — задание взято. */
  private def begin(user: User, quests: NpcQuests, items: List[Item], renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      taken  = quests.take(NpcQuest.BigDeal, now)
      _     <- NpcQuestLog.save(heroDao, user.userId, taken)
      _     <- renderer.show(user, Screen(content.text("barti.house.pitch").trim, Nil))
      _     <- renderer.show(user, stockScreen(Some(content.format("barti.taken",
                 "title" -> content.text("journal.quests.bigDeal.title"),
                 "step"  -> content.list("journal.quests.bigDeal.steps").headOption.getOrElse(""))), taken, items))
    } yield ()

  /** Сколько у Барти уже сложено, и — если в сумке есть, что ему ещё нужно, —
    * «Отдать товар». */
  private def stockScreen(lead: Option[String], quests: NpcQuests, items: List[Item]): Screen = {
    val s    = BigDealQuest.stock(quests)
    val line = content.format("barti.house.stock", "schnapps" -> s.schnapps.toString, "water" -> s.water.toString,
      "goal" -> BigDealQuest.Goal.toString)
    val give = Option.when(BigDealQuest.takeable(items, s).nonEmpty)(
      content.choice("BartiGive", "barti.house.giveLabel").copy(color = ChoiceColor.Positive, row = Some(0)))
    Screen((lead.toList :+ line).mkString("\n\n"), give.toList :+ leave)
  }

  /** «Отдать товар»: Барти берёт из сумки всё, что ему ещё нужно, по одной
    * склянке, — и складывает у себя. Собрал всё — ждать каравана. */
  private def give(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      items  <- bag(hero)
      s       = BigDealQuest.stock(quests)
      take    = BigDealQuest.takeable(items, s)
      _      <- if (!BigDealQuest.collecting(quests)) enter(user, renderer)
                else if (take.isEmpty) renderer.show(user, stockScreen(Some(content.text("barti.house.nothing")), quests, items))
                else for {
                  _     <- ZIO.foreachDiscard(take)(i => inventoryRepo.removeItem(i.id, hero.id).mapError(e => new Throwable(e.toString)))
                  next   = take.flatMap(_.brew).foldLeft(s)((acc, k) => acc.plus(k, 1))
                  stored = BigDealQuest.withStock(quests, next)
                  saved  = if (next.full) BigDealQuest.step(stored, 2) else stored
                  _     <- NpcQuestLog.save(heroDao, user.userId, saved)
                  said   = content.format("barti.house.given", "schnapps" -> next.schnapps.toString, "water" -> next.water.toString)
                  left   = items.filterNot(i => take.exists(_.id == i.id))
                  _     <- if (next.full) gate(user, hero, saved, Some(said), renderer)
                           else renderer.show(user, stockScreen(Some(said), saved, left))
                } yield ()
    } yield StateType.BartiHouse

  /** Товар собран: ниже уровня задания — «рано, караван не пришёл» (так решил
    * пользователь для этого задания); дорос — караван пришёл, задание ведёт к
    * нему. `lead` — что сказать перед этим. */
  private def gate(user: User, hero: Hero, quests: NpcQuests, lead: Option[String], renderer: Renderer): Task[Unit] =
    if (!BigDealQuest.arrives(hero))
      renderer.show(user, Screen((lead.toList :+ content.text("barti.house.early").trim).mkString("\n\n"), List(leave)))
    else NpcQuestLog.save(heroDao, user.userId, BigDealQuest.step(quests, 3)) *>
      renderer.show(user, Screen((lead.toList :+ content.text("barti.house.arrived").trim).mkString("\n\n"), List(leave)))

  private def bag(hero: Hero): Task[List[Item]] =
    inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).map(_.items.data)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
