package pangea.service.parcel

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.dao.parcel.ParcelDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.home.{HomeData, HomeIntake, HomePlace}
import pangea.model.item.Item
import pangea.model.parcel.Parcel
import pangea.model.user.{User, UserId}
import pangea.repository.bank.BankRepository
import pangea.util.TimeLeft
import zio.{Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** Передача вещей между игроками. Отправитель отдаёт вещь из города, а
 *  получателю она приходит посылкой и сразу ложится по местам: что берёт его
 *  дом — в дом (руны — на алтарь, травы и отвары — на полки верстака, камни —
 *  в хранилище, см. [[HomeIntake]]), остальное — в банковскую ячейку (см.
 *  [[Transfers]]). Не приняла ячейка (её нет или нет места) — вещь ждёт на
 *  почте в Торговом доме.
 *
 *  Почта хранит вещи [[Parcels.ShelfDays]] дней с того, как на неё легла
 *  первая: что пришло позже, ждёт до того же срока. Срок вышел — всё, что лежит
 *  на почте, сгорает само, без хода хозяина (см. [[burnExpired]]). Пока на
 *  почте что-то есть, срок не обновляется; забрал всё — следующая посылка
 *  начнёт новый.
 *
 *  Посылками это сделано не для красоты: чужую сумку нельзя трогать в тот
 *  момент, когда её хозяин сам в ней роется, — запись бы затёрлась. Всё, что
 *  попадает к получателю в дом или ячейку, кладётся под его собственным локом. */
final case class Parcels(dao: ParcelDao, bank: BankRepository, content: SceneContent,
                         // Дом получателя: без него (в тестах) посылки в дом не идут.
                         heroDao: Option[HeroDao] = None) {

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
    * срок. Что герой успел разложить своим ходом, не трогаем. `userId` —
    * хозяин героя: по нему читается его дом. */
  def arrive(heroId: HeroId, userId: UserId, ids: List[Long], now: Long): Task[Parcels.Landing] =
    for {
      burned <- dao.burn(heroId, now - Parcels.ShelfMs)
      fresh  <- dao.inTransit(heroId, ids)
      landed <- land(heroId, userId, fresh, now)
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
      landed  <- land(hero.id, user.userId, parcels, now)
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

  /** Что сказать герою о раскладке: что сгорело, что легло в дом, что — в
    * ячейку, что — на почту. Про срок почты — только если он пошёл с этих
    * вещей. */
  def report(landed: Parcels.Landing): Option[String] = {
    val (story, plain) = landed.mailed.partition(_.item.isStoryGear)
    val lines = List(
      Option.when(landed.burned.nonEmpty)(burnedText(landed.burned)),
      Option.when(landed.homed.nonEmpty)(content.format("parcel.home",
        "items" -> titles(landed.homed.map(_._1)), "places" -> places("at", landed.homed.map(_._2)))),
      Option.when(landed.delivered.nonEmpty)(content.format("parcel.delivered", "items" -> titles(landed.delivered))),
      Option.when(plain.nonEmpty)(content.format(
        if (landed.started) "parcel.mailed" else "parcel.mailedMore", "items" -> titles(plain))),
      Option.when(story.nonEmpty)(content.format("parcel.mailedStory", "items" -> titles(story)))
    ).flatten
    Option.when(lines.nonEmpty)(lines.mkString("\n\n"))
  }

  /** Письмо о сгоревшем. */
  def burnedText(burned: List[Parcel]): String =
    content.format("parcel.burned", "items" -> titles(burned))

  /** Куда в доме что легло, словами, по порядку и без повторов: «на полках
    * верстака, на руническом алтаре». `form` — «at» (где лежит) или «to» (куда
    * легло). */
  def places(form: String, where: List[HomePlace]): String =
    HomePlace.ordered.filter(where.contains).map(p => content.text(s"parcel.places.$form.${p.key}")).mkString(", ")

  /** В дом, что он берёт, следом — в ячейку, а что не влезло — на почту. */
  private def land(heroId: HeroId, userId: UserId, parcels: List[Parcel], now: Long): Task[Parcels.Landing] =
    for {
      homed     <- toHome(heroId, userId, parcels)
      rest       = parcels.filterNot(p => homed.exists(_._1.id == p.id))
      delivered <- ZIO.foreach(rest)(toVault(heroId, _)).map(_.flatten)
      mailed     = rest.filter(p => p.mailedAt.isEmpty && !delivered.exists(_.id == p.id))
      // Сюжетная экипировка не сгорает: срока почты она не начинает.
      (story, plain) = mailed.partition(_.item.isStoryGear)
      started   <- dao.toMail(heroId, plain.map(_.id), now)
      _         <- dao.toMail(heroId, story.map(_.id), now)
    } yield Parcels.Landing(Nil, delivered, mailed, started, homed)

  /** Что из посылок берёт дом (см. [[HomeIntake]]): кладём в дом, а потом
    * забираем посылки из списка — как с ячейкой. Какую забрать не вышло (её
    * успели сжечь), ту из дома убираем: иначе вещь удвоится. Дома нет — ничего.
    *
    * Дом читаем, только если среди посылок есть то, что он вообще берёт:
    * `deliver` зовёт сюда на каждом ходу героя, и пустая почта или меч на ней
    * не должны стоить лишнего запроса. */
  private def toHome(heroId: HeroId, userId: UserId, parcels: List[Parcel]): Task[List[(Parcel, HomePlace)]] =
    heroDao.filter(_ => parcels.exists(p => HomeIntake.wanted(p.item)))
      .fold[Task[List[(Parcel, HomePlace)]]](ZIO.succeed(Nil)) { heroes =>
      for {
        home    <- heroes.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
        intake   = HomeIntake.of(home, parcels.map(_.item))
        offered  = parcels.flatMap(p => intake.taken.collectFirst { case (it, place) if it.id == p.item.id => p -> place })
        _       <- ZIO.when(offered.nonEmpty)(heroes.writeHomeData(userId, intake.home.asJson))
        claimed <- ZIO.filter(offered) { case (p, _) => dao.claim(p.id, heroId) }
        _       <- ZIO.when(claimed.size < offered.size)(
                     heroes.writeHomeData(userId, HomeIntake.of(home, claimed.map(_._1.item)).home.asJson))
      } yield claimed
    }

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
    * легло в ячейку, что — на почту, пошёл ли с этих вещей срок почты (до них
    * она была пуста) и что легло в дом — с местом. */
  final case class Landing(burned: List[Parcel], delivered: List[Parcel], mailed: List[Parcel], started: Boolean,
                           homed: List[(Parcel, HomePlace)] = Nil)

  /** Когда сгорит почта: срок у неё общий — от первой легшей вещи. `None` —
    * на почте пусто (посылки в пути не в счёт) или лежит одна сюжетная
    * экипировка: она не сгорает (см. `Item.isStoryGear`). */
  def burnsAt(parcels: List[Parcel]): Option[Long] =
    parcels.filterNot(_.item.isStoryGear).flatMap(_.mailedAt).minOption.map(_ + ShelfMs)

  /** Сколько ещё хранит почта: «6д 23ч», «5ч 20мин», «40мин» — как у
    * благословения и наёмников в отлучке (см. [[TimeLeft]]). */
  def left(ms: Long): String = TimeLeft.format(ms)

  val live: ZLayer[ParcelDao with BankRepository with SceneContent with HeroDao, Nothing, Parcels] =
    ZLayer.fromFunction((dao: ParcelDao, bank: BankRepository, content: SceneContent, heroes: HeroDao) =>
      Parcels(dao, bank, content, Some(heroes)))
}
