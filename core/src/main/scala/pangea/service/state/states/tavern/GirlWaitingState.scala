package pangea.service.state.states.tavern

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.TreasureMapGenerator
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.purse.Purse
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Девушка с картой отца ждёт в таверне оплаты ([[GirlDeal]]): в событии
  * серебра на карту не хватило, и она согласилась подождать четыре часа.
  * Здесь — только расчёт: заплатить по прежней цене и забрать карту или
  * вернуться позже. Отсюда — всегда обратно в таверну. */
case class GirlWaitingState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  bank:          Option[BankRepository] = None
) extends State {

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "PayGirl"   -> Target.Run { (u, _, r) => pay(u, r) },
      "GirlLater" -> Target.Goto(StateType.Tavern)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.GirlWaiting) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.GirlWaiting

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      deal <- GirlDeal.waiting(heroDao, user.userId, now)
      // Не дождалась (срок вышел, пока герой шёл через зал) — кнопка одна: назад.
      _    <- deal.fold(GirlDeal.clear(heroDao, user.userId) *> renderer.show(user, Screen(
                content.text("girlWaiting.gone"), List(content.choice("GirlLater", "girlWaiting.later")))))(d =>
                renderer.show(user, screen(d, now)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def screen(deal: GirlDeal, now: Long): Screen =
    Screen(
      content.format("girlWaiting.text", "price" -> deal.price.toString, "left" -> GirlWaitingState.left(deal.until - now)),
      List(
        Choice("PayGirl", content.format("girlWaiting.pay", "price" -> deal.price.toString),
          color = ChoiceColor.Positive, row = Some(0)),
        // «Позже» — обратно в таверну; она ждёт свой срок дальше.
        content.choice("GirlLater", "girlWaiting.later").copy(row = Some(1))
      ))

  /** Заплатить: ушла — так и говорим; серебра снова мало — ждёт дальше, срок
    * тот же; сумка полна — ничего не списываем. Иначе карта — герою, сделка
    * закрыта. */
  private def pay(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      deal <- GirlDeal.waiting(heroDao, user.userId, now)
      res  <- deal match {
                case None =>
                  GirlDeal.clear(heroDao, user.userId) *>
                    renderer.show(user, Screen(content.text("girlWaiting.gone"), Nil)).as(StateType.Tavern)
                case Some(d) =>
                  purse.wallet(hero).flatMap { wallet =>
                    if (!wallet.canAfford(d.price))
                      renderer.show(user, Screen(content.format("girlWaiting.noSilver", "price" -> d.price.toString), Nil)) *>
                        renderer.show(user, screen(d, now)).as(StateType.GirlWaiting)
                    else handOver(user, hero, d, renderer)
                  }
              }
    } yield res

  private def handOver(user: User, hero: Hero, deal: GirlDeal, renderer: Renderer): Task[StateType] =
    for {
      map   <- itemRepo.persist(hero.id, TreasureMapGenerator.create(deal.mapLvl, half = false))
      added <- inventoryRepo.addItem(hero.id, map).as(true).catchAll(_ => ZIO.succeed(false))
      res   <- if (!added)
                 renderer.show(user, Screen(content.text("girlWaiting.noRoom"), Nil)).as(StateType.Tavern)
               else
                 purse.charge(user.userId, hero, deal.price) *>
                   GirlDeal.clear(heroDao, user.userId) *>
                   renderer.show(user, Screen(content.format("girlWaiting.bought", "price" -> deal.price.toString), Nil))
                     .as(StateType.Tavern)
    } yield res

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object GirlWaitingState {

  /** Сколько ей ещё ждать: «3ч 12мин», «3ч», «40мин». */
  def left(ms: Long): String = {
    val mins = (ms / 60000L).max(1L)
    val h    = mins / 60
    val m    = mins % 60
    if (h > 0 && m > 0) s"${h}ч ${m}мин" else if (h > 0) s"${h}ч" else s"${m}мин"
  }
}
