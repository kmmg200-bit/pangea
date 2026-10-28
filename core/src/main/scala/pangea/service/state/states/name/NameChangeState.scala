package pangea.service.state.states.name

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.item.QuestItemKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{NamePick, State, UserAction}
import zio.{Task, ZIO}

/** Смена имени по купону Фета ([[QuestItemKind.NameCoupon]]): игрок пишет новое
  * имя, подтверждает — имя его, купон сгорает. Сюда приходят из сумки и туда же
  * возвращаются. Имя на подтверждении — в `scene_data` ([[NamePick.PendingKey]]). */
case class NameChangeState(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent) extends State {

  private val names = NamePick(heroDao, content)

  private val branch = new Branch(
    routes = Map(
      "NameChangeYes"    -> Target.Run { (u, _, r) => accept(u, r) },
      "NameChangeOther"  -> Target.Run { (u, _, r) => ask(u, None, r) },
      "NameChangeCancel" -> Target.Goto(StateType.Inventory)
    ),
    fallback = Target.Run { (u, ua, r) =>
      names.typed(ua) match {
        case Some(raw) => propose(u, raw, r)
        case None      => ask(u, None, r)
      }
    }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.obj()) *> ask(user, None, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Спросить новое имя (с отказом над вопросом, если прошлое не подошло). */
  private def ask(user: User, refusal: Option[String], renderer: Renderer): Task[StateType] =
    heroDao.getHeroByUserId(user.userId).flatMap { hero =>
      val now  = hero.flatMap(_.name).map(n => content.format("nameChange.current", "name" -> n) + "\n\n").getOrElse("")
      val text = refusal.map(_ + "\n\n").getOrElse("") + now + content.text("nameChange.ask")
      heroDao.writeSceneData(user.userId, Json.obj()) *>
        renderer.show(user, Screen(text, List(cancel(0)))).as(StateType.NameChange)
    }

  /** Имя пришло текстом: подошло — переспросить, нет — сказать почему. */
  private def propose(user: User, raw: String, renderer: Renderer): Task[StateType] =
    names.check(user, raw).flatMap {
      case Left(refusal) => renderer.show(user, Screen(refusal, List(cancel(0)))).as(StateType.NameChange)
      case Right(name) =>
        heroDao.writeSceneData(user.userId, Json.obj(NamePick.PendingKey -> name.asJson)) *>
          renderer.show(user, Screen(content.format("nameChange.confirm", "name" -> name), List(
            Choice("NameChangeYes", content.text("nameChange.yesLabel"), color = ChoiceColor.Positive, row = Some(0)),
            Choice("NameChangeOther", content.text("nameChange.otherLabel"), row = Some(1)),
            cancel(1)
          ))).as(StateType.NameChange)
    }

  /** «Да»: имя — герою, купон — сгорает. Купона в сумке уже нет (продан,
    * выброшен) — имя не меняется. Пока игрок подтверждал, имя занял другой —
    * спрашиваем снова, купон цел. */
  private def accept(user: User, renderer: Renderer): Task[StateType] =
    for {
      pending <- heroDao.readSceneData(user.userId).map(_.flatMap(_.hcursor.get[String](NamePick.PendingKey).toOption))
      hero    <- heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_)).orElseFail(new Throwable(s"No hero for user ${user.userId}"))
      inv     <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      coupon   = inv.items.data.find(_.questItem.contains(QuestItemKind.NameCoupon))
      next    <- (pending, coupon) match {
                   case (None, _) => ask(user, None, renderer)
                   case (_, None) =>
                     renderer.show(user, Screen(content.text("nameChange.noCoupon"), Nil)).as(StateType.Inventory)
                   case (Some(name), Some(c)) =>
                     names.claim(user, name).flatMap {
                       case Left(refusal) => ask(user, Some(refusal), renderer)
                       case Right(_) =>
                         inventoryRepo.removeItem(c.id, hero.id).mapError(e => new Throwable(e.toString)) *>
                           heroDao.writeSceneData(user.userId, Json.Null) *>
                           renderer.show(user, Screen(content.format("nameChange.done", "name" -> name), Nil))
                             .as(StateType.Inventory)
                     }
                 }
    } yield next

  private def cancel(row: Int): Choice =
    Choice("NameChangeCancel", content.text("nameChange.cancelLabel"), color = ChoiceColor.Negative, row = Some(row))
}
