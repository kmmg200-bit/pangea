package pangea.service.state.states.parcel

import io.circe.jawn
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.parcel.Parcel
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.parcel.Parcels
import pangea.service.state.{CityExit, ItemMenu, State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Почта Торгового дома: сюда попадает переданное, что не влезло в банковскую
 *  ячейку. Вещи отсюда забирают в сумку — по одной или всё, что поместится.
 *  Лежат они не вечно: сколько осталось до сгорания, видно в шапке (см.
 *  [[Parcels]]). */
case class MailState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  parcels:       Parcels,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "MailList"  -> Target.Run { (u, _, r) => showMail(u, r).as(StateType.Mail) },
      "MailAll"   -> Target.Run { (u, _, r) => takeAll(u, r).as(StateType.Mail) },
      "LeaveMail" -> Target.Goto(StateType.TradeHouse),
      CityExit.route
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = showMail(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Экран почты. `note` — что только что забрано (или почему нет): тем же
    * сообщением, над почтой. */
  private def showMail(user: User, renderer: Renderer, note: String = ""): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      items <- parcels.waiting(hero.id)
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      _ <- if (items.isEmpty) renderer.show(user, Screen(content.text("mail.empty"), leaveRow).noted(note))
           else {
             val page = items.take(ItemMenu.DefaultPageSize)
             val buttons = page.zipWithIndex.map { case (p, i) =>
               Choice("MailTake", ItemMenu.truncate(content.format("mail.takeOne",
                 "name" -> p.item.displayTitle, "from" -> p.fromName)),
                 data = Map("id" -> p.id.toString), row = Some(i))
             }
             // Срок у почты общий — через сколько сгорит всё, что на ней лежит.
             // Сюжетная экипировка не сгорает — об этом так и говорим.
             val burnsKey = if (items.exists(_.item.isStoryGear)) "mail.burnsInStory" else "mail.burnsIn"
             val burns = Parcels.burnsAt(items)
               .map(at => "\n" + content.format(burnsKey, "left" -> Parcels.left(at - now)))
               .getOrElse("")
             val text = content.format("mail.header", "free" -> inv.freeSlots.toString) + burns + "\n" +
               page.map(line).mkString("\n")
             renderer.show(user, Screen(text, buttons ++ List(
               Choice("MailAll",   content.text("mail.takeAll"), row = Some(buttons.size)),
               Choice("LeaveMail", content.text("mail.back"), color = ChoiceColor.Negative, row = Some(buttons.size + 1)),
               CityExit.button(content, Some(buttons.size + 1))
             )).noted(note))
           }
    } yield ()

  private def line(p: Parcel): String =
    content.format("mail.line", "name" -> p.item.displayTitle, "from" -> p.fromName)

  /** Забрать одну посылку в сумку. */
  private def take(user: User, parcelId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      items <- parcels.waiting(hero.id)
      _ <- items.find(_.id == parcelId) match {
        case None => showMail(user, renderer)
        case Some(_) if inv.freeSlots <= 0 => showMail(user, renderer, content.text("common.inventoryFull"))
        case Some(parcel) =>
          parcels.claim(hero.id, parcel).flatMap {
            case false => showMail(user, renderer)
            case true =>
              inventoryRepo.addItem(hero.id, parcel.item).mapError(asThrowable) *>
                showMail(user, renderer, content.format("mail.taken", "name" -> parcel.item.displayTitle))
          }
      }
    } yield ()

  /** Забрать всё, что влезет в сумку. Посылок берём столько, сколько в сумке
    * свободных мест, а не первые десять с экрана: экран показывает десять, но
    * кнопка — про всё. */
  private def takeAll(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      fit   <- parcels.waiting(hero.id, inv.freeSlots.max(0L))
      taken <- ZIO.foreach(fit) { parcel =>
                 parcels.claim(hero.id, parcel).flatMap {
                   case false => ZIO.none
                   case true  => inventoryRepo.addItem(hero.id, parcel.item).mapError(asThrowable).as(Some(parcel))
                 }
               }.map(_.flatten)
      _ <- showMail(user, renderer,
             if (taken.isEmpty) content.text("common.inventoryFull")
             else content.format("mail.tookAll", "items" -> tookList(taken)))
    } yield ()

  /** Забранное одной строкой: одинаковые — вместе, «Щит ×3», как при «Забрать
    * всё» из хранилищ. Длинный список сворачиваем в «и ещё N»: забрать можно и
    * сотню вещей, а сообщение ВК не резиновое. */
  private def tookList(taken: List[Parcel]): String = {
    val names           = taken.map(_.item.displayTitle)
    val groups          = names.distinct.map(name => name -> names.count(_ == name))
    val (shown, hidden) = groups.splitAt(MailState.ListedKinds)
    val line            = shown.map { case (name, n) => if (n > 1) s"$name ×$n" else name }.mkString(", ")
    val rest            = hidden.map(_._2).sum
    if (rest > 0) line + content.format("parcel.andMore", "count" -> rest.toString) else line
  }

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("id")))
      .flatMap(_.toLongOption) match {
        case Some(id) => take(user, id, renderer).as(StateType.Mail)
        case None     => showMail(user, renderer).as(StateType.Mail)
      }

  private def leaveRow: List[Choice] =
    List(Choice("LeaveMail", content.text("mail.back"), color = ChoiceColor.Negative, row = Some(0)),
         CityExit.button(content, Some(0)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object MailState {

  /** Сколько разных вещей перечисляем в «Забрано: …»; остальное — «и ещё N». */
  val ListedKinds: Int = 20
}
