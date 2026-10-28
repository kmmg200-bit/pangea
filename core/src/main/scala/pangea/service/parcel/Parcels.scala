package pangea.service.parcel

import pangea.dao.parcel.ParcelDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.item.Item
import pangea.model.parcel.Parcel
import pangea.model.user.{User, UserId}
import pangea.repository.bank.BankRepository
import pangea.util.TimeLeft
import zio.{Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** Передача вещей между игроками. Отправитель отдаёт вещь из города, а
 *  получателю она приходит посылкой и сразу ложится в его банковскую ячейку
 *  (см. [[Transfers]]). Не приняла ячейка (её нет или нет места) — вещь ждёт
 *  на почте в Торговом доме.
 *
 *  Почта хранит вещи [[Parcels.ShelfDays]] дней с того, как на неё легла
 *  первая: что пришло позже, ждёт до того же срока. Срок вышел — всё, что лежит
 *  на почте, сгорает само, без хода хозяина (см. [[burnExpired]]). Пока на
 *  почте что-то есть, срок не обновляется; забрал всё — следующая посылка
 *  начнёт новый.
 *
 *  Посылками это сделано не для красоты: чужую сумку нельзя трогать в тот
 *  момент, когда её хозяин сам в ней роется, — запись бы затёрлась. Всё, что
 *  попадает к получателю в ячейку, кладётся под его собственным локом. */
final case class Parcels(dao: ParcelDao, bank: BankRepository, content: SceneContent) {

  /** Сколько мест можно разобрать за один заход: экран не резиновый. */
  val DeliveryBatch: Long = 10L

  def send(toHeroId: HeroId, fromName: String, item: Item, now: Long): Task[Parcel] =
    dao.add(toHeroId, fromName, item, now)

  def waiting(heroId: HeroId, limit: Long = DeliveryBatch): Task[List[Parcel]] =
    dao.list(heroId, limit)

  def waitingCount(heroId: HeroId): Task[Long] = dao.count(heroId)

  /** Забрать посылку себе: `false` — её уже забрали. */
  def claim(heroId: HeroId, parcel: Parcel): Task[Boolean] = dao.claim(parcel.id, heroId)

  /** Только что отправленное — по местам, под локом получателя. Сперва
    * сгорает просроченная почта: иначе новая вещь унаследовала бы истёкший
    * срок. Что герой успел разложить своим ходом, не трогаем. */
  def arrive(heroId: HeroId, ids: List[Long], now: Long): Task[Parcels.Landing] =
    for {
      burned <- dao.burn(heroId, now - Parcels.ShelfMs)
      fresh  <- dao.inTransit(heroId, ids)
      landed <- land(heroId, fresh, now)
    } yield landed.copy(burned = burned)

  /** На ходу героя — страховка: посылки, которые не разложились сразу, и
    * почта, если в ячейке появилось место. */
  def deliver(user: User, hero: Hero, renderer: Renderer): Task[Unit] =
    for {
      now     <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      listed  <- dao.list(hero.id, DeliveryBatch)
      // Срок смотрим по уже прочитанному: на почте лежат самые старые
      // посылки, и лишний запрос на каждое действие игрока ни к чему.
      burned  <- if (Parcels.burnsAt(listed).exists(_ <= now)) dao.burn(hero.id, now - Parcels.ShelfMs)
                 else ZIO.succeed(Nil)
      parcels <- if (burned.isEmpty) ZIO.succeed(listed) else dao.list(hero.id, DeliveryBatch)
      landed  <- land(hero.id, parcels, now)
      _       <- ZIO.foreachDiscard(report(landed.copy(burned = burned)))(text => renderer.show(user, Screen(text, Nil)))
    } yield ()

  /** Фоном: посылки, которые не разложились сразу (отправлены не позже
    * `before`), — по героям, с хозяевами. */
  def stranded(before: Long): Task[List[(HeroId, UserId, List[Long])]] =
    dao.stranded(before).map(_.groupBy { case (heroId, userId, _) => (heroId, userId) }.toList
      .map { case ((heroId, userId), rows) => (heroId, userId, rows.map(_._3)) }
      .sortBy(_._1.value))

  /** Фоном: просроченные почты сгорают. Возвращает, у кого что сгорело. */
  def burnExpired(now: Long): Task[List[(UserId, List[Parcel])]] =
    dao.expired(now - Parcels.ShelfMs)
      .flatMap(ZIO.foreach(_) { case (heroId, userId) => dao.burn(heroId, now - Parcels.ShelfMs).map(userId -> _) })
      .map(_.filter(_._2.nonEmpty))

  /** Что сказать герою о раскладке: что сгорело, что легло в ячейку, что — на
    * почту. Про срок почты — только если он пошёл с этих вещей. */
  def report(landed: Parcels.Landing): Option[String] = {
    val lines = List(
      Option.when(landed.burned.nonEmpty)(burnedText(landed.burned)),
      Option.when(landed.delivered.nonEmpty)(content.format("parcel.delivered", "items" -> titles(landed.delivered))),
      Option.when(landed.mailed.nonEmpty)(content.format(
        if (landed.started) "parcel.mailed" else "parcel.mailedMore", "items" -> titles(landed.mailed)))
    ).flatten
    Option.when(lines.nonEmpty)(lines.mkString("\n\n"))
  }

  /** Письмо о сгоревшем. */
  def burnedText(burned: List[Parcel]): String =
    content.format("parcel.burned", "items" -> titles(burned))

  /** В ячейку, а что не влезло — на почту. */
  private def land(heroId: HeroId, parcels: List[Parcel], now: Long): Task[Parcels.Landing] =
    for {
      delivered <- ZIO.foreach(parcels)(toVault(heroId, _)).map(_.flatten)
      mailed     = parcels.filter(p => p.mailedAt.isEmpty && !delivered.exists(_.id == p.id))
      started   <- dao.toMail(heroId, mailed.map(_.id), now)
    } yield Parcels.Landing(Nil, delivered, mailed, started)

  /** «Меч (Иван), Щит (Пётр)». Сгореть может и больше, чем умещается в
    * сообщение, — лишнее сворачиваем в «и ещё N». */
  private def titles(parcels: List[Parcel]): String = {
    val names = parcels.sortBy(_.id).map(p => s"${p.item.displayTitle} (${p.fromName})")
    val shown = names.take(DeliveryBatch.toInt).mkString(", ")
    val rest  = names.size - DeliveryBatch.toInt
    if (rest > 0) shown + content.format("parcel.andMore", "count" -> rest.toString) else shown
  }

  /** Одна посылка в ячейку: кладём, а потом забираем её из списка. Если забрать
    * не вышло (кто-то успел раньше), вещь достаём обратно — иначе она удвоится.
    * Ячейка не приняла — посылка просто остаётся где была. */
  private def toVault(heroId: HeroId, parcel: Parcel): Task[Option[Parcel]] =
    bank.deposit(heroId, parcel.item).either.flatMap {
      case Left(_) => ZIO.none
      case Right(_) =>
        dao.claim(parcel.id, heroId).flatMap {
          case true  => ZIO.some(parcel)
          case false => bank.withdraw(heroId, parcel.item.id).ignore.as(None)
        }
    }
}

object Parcels {

  /** Сколько дней почта хранит вещи. В текстах — словами: «7 дней». */
  val ShelfDays: Long = 7L
  val ShelfMs: Long   = ShelfDays * 24L * 60L * 60L * 1000L

  /** Чем кончилась раскладка: что сгорело перед ней (срок почты вышел), что
    * легло в ячейку, что — на почту, и пошёл ли с этих вещей срок почты (до
    * них она была пуста). */
  final case class Landing(burned: List[Parcel], delivered: List[Parcel], mailed: List[Parcel], started: Boolean)

  /** Когда сгорит почта: срок у неё общий — от первой легшей вещи. `None` —
    * на почте пусто (посылки в пути не в счёт). */
  def burnsAt(parcels: List[Parcel]): Option[Long] =
    parcels.flatMap(_.mailedAt).minOption.map(_ + ShelfMs)

  /** Сколько ещё хранит почта: «6д 23ч», «5ч 20мин», «40мин» — как у
    * благословения и наёмников в отлучке (см. [[TimeLeft]]). */
  def left(ms: Long): String = TimeLeft.format(ms)

  val live: ZLayer[ParcelDao with BankRepository with SceneContent, Nothing, Parcels] =
    ZLayer.fromFunction(Parcels(_, _, _))
}
