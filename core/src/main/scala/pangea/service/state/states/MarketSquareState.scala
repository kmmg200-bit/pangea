package pangea.service.state.states

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{BartiDoorQuest, BartiRoofQuest, NpcQuestLog, State, UserAction}
import zio.{Task, ZIO}

/** Торговая площадь. Пока «Крыша» доступна или идёт (см. BartiRoofQuest), здесь
  * же — лавка Барти, а пока идёт «Синяя дверь» (BartiDoorQuest) — его дом; без
  * журнала заданий (`heroDao` — в старых тестах) их нет. */
case class MarketSquareState(content: SceneContent, heroDao: Option[HeroDao] = None) extends State {

  private val branch = new Branch(
    routes = Map(
      "StreetMerchants"    -> Target.Goto(StateType.StreetMerchants),
      // Кнопки лавок жили здесь, пока уличные торговцы были подменю площади:
      // старое сообщение с ними пусть по-прежнему ведёт куда обещало.
      "MerchantRichelieu"  -> Target.Goto(StateType.Merchant),
      "Gustavo"            -> Target.Goto(StateType.Gustavo),
      "Construction"       -> Target.Goto(StateType.Construction),
      "TradeHouse"         -> Target.Goto(StateType.TradeHouse),
      "BartiShop"          -> Target.Goto(StateType.BartiShop),
      "BartiHome"          -> Target.Goto(StateType.BartiHouse),
      "BackToMarketSquare" -> Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.MarketSquare) },
      "BackToCity"         -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.MarketSquare) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    ZIO.foreach(heroDao)(dao => NpcQuestLog.load(dao, user.userId)).flatMap { quests =>
      val byId  = content.screen("marketSquare.enter").choices.map(c => c.id -> c).toMap
      val barti = Option.when(quests.exists(BartiRoofQuest.shopOpen))(
        content.choice("BartiShop", "marketSquare.bartiShopLabel").copy(color = ChoiceColor.Positive, row = Some(1)))
      // «Синяя дверь» (только одиночка): дом Барти — пока задание доступно или идёт.
      val door = Option.when(quests.exists(BartiDoorQuest.houseOpen))(
        content.choice("BartiHome", "marketSquare.bartiHomeLabel").copy(color = ChoiceColor.Positive, row = Some(1)))
      val choices = List(
        byId("StreetMerchants").copy(row = Some(0)),
        byId("Construction").copy(row = Some(0)),
        byId("TradeHouse").copy(row = Some(1))
      ) ++ barti.toList ++ door.toList :+ byId("BackToCity").copy(color = ChoiceColor.Negative, row = Some(2))
      renderer.show(user, Screen(content.text("marketSquare.enter.text"), choices))
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
