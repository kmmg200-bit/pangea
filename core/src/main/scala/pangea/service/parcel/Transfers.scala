package pangea.service.parcel

import pangea.dao.hero.HeroDao
import pangea.engine.{Players, SceneContent}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.item.{Item, ItemStack, ItemType}
import pangea.model.parcel.Parcel
import pangea.model.user.{User, UserId}
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.user.UserRepository
import pangea.service.chat.ChatCommand
import pangea.service.chat.ChatCommand.Money
import pangea.service.payout.Payouts
import pangea.service.purse.Purse
import pangea.service.state.PlayerLock
import zio.{Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** Кому и что передают. Имя получателя берём из ВК один раз и носим с собой:
 *  игроку важно видеть, кому он отдаёт. */
final case class TransferTarget(heroId: HeroId, userId: UserId, name: String)

/** Общая часть передачи вещей: что подходит под запрос, можно ли отдать сразу
 *  и как, собственно, отдать. Этим пользуются и команда из беседы, и экран
 *  выбора в личке — правила у них одни. Деньги передаются здесь же
 *  ([[sendSilver]], [[takeDoubloons]]), но своим путём: в сумке их нет. */
final case class Transfers(
  inventoryRepo: InventoryRepository,
  userRepo:      UserRepository,
  parcels:       Parcels,
  players:       Players,
  content:       SceneContent,
  heroDao:       HeroDao,
  bank:          BankRepository,
  payouts:       Payouts,
  lock:          PlayerLock
) {

  private val purse = Purse(heroDao, Some(bank))

  /** Что вообще можно передать: вещь занимает место, не сюжетная и не трофей.
    * Трофеи — личная добыча: обычные сдают в Гильдии, уникальные вешают на
    * Стену трофеев, а не передают друг другу. */
  def sendable(item: Item): Boolean =
    !item.isQuestItem && !item.weightless && item.itemType != ItemType.Trophy && !item.isNamedTrophy

  /** Под запрос подошли только трофеи — о них стоит сказать отдельно, иначе
    * «ничего не нашлось» сбивает с толку. Ключ текста отказа: про обычные
    * трофеи и про уникальные — разный. */
  def trophyRefusal(items: List[Item], query: String): Option[String] = {
    def found(p: Item => Boolean): Boolean = items.exists(i => p(i) && ChatCommand.matches(query, i.name))
    if (matching(items, query).nonEmpty) None
    else if (found(_.itemType == ItemType.Trophy)) Some("transfer.noTrophies")
    else if (found(_.isNamedTrophy)) Some("transfer.noNamedTrophies")
    else None
  }

  /** Подходящее под запрос. Точное совпадение по названию бьёт частичное:
    * «надколотый череп» отдаст именно череп, а не всё, где встретилось слово.
    * Уровень в запросе не нужен — по названию находятся вещи всех уровней. */
  def matching(items: List[Item], query: String): List[Item] = {
    val fit   = items.filter(i => sendable(i) && ChatCommand.matches(query, i.name))
    val exact = fit.filter(i => ChatCommand.sameName(query, i.name))
    if (exact.nonEmpty) exact else fit
  }

  /** Можно ли отдать без уточнения: когда спутать не с чем. Одна-единственная
    * подходящая вещь уходит сразу, даже если это экипировка. Уточняем, когда
    * подходящих несколько: у экипировки они различаются характеристиками, а у
    * прочего — тем, что это вообще разные вещи. */
  def unambiguous(items: List[Item]): Boolean =
    items match {
      case Nil            => false
      case _ :: Nil       => true
      case first :: _     =>
        !items.exists(i => ItemType.equippable.contains(i.itemType)) &&
          items.forall(i => sameStack(first, i))
    }

  /** Вещи той же «стопки», что и выбранная: по ним считается «2 штуки». */
  def sameStack(a: Item, b: Item): Boolean =
    a.name == b.name && ItemStack.key(a) == ItemStack.key(b)

  /** Попытка отдать без уточнения — с этого начинается любая передача.
    * Экипировку всегда выбирают кнопками, всё остальное с точным названием
    * уходит сразу. */
  def quickSend(
    sender: User,
    hero:   Hero,
    target: TransferTarget,
    query:  String,
    count:  Int,
    now:    Long
  ): Task[Transfers.Outcome] =
    for {
      inv <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      fit  = matching(inv.items.data, query)
      res <- trophyRefusal(inv.items.data, query) match {
               case Some(key)                 => ZIO.succeed(Transfers.Outcome.Forbidden(content.text(key)))
               case None if fit.isEmpty       => ZIO.succeed(Transfers.Outcome.Empty)
               case None if !unambiguous(fit) => ZIO.succeed(Transfers.Outcome.NeedPick)
               case None =>
                 send(sender, hero, target, fit.take(count.max(1)), now).map { shipment =>
                   Transfers.Outcome.Sent(content.format("transfer.sent",
                     "name"  -> shipment.items.head.displayTitle,
                     "count" -> shipment.items.size.toString,
                     "to"    -> target.name), shipment.handOver)
                 }
             }
    } yield res

  /** Отдать вещи: из сумки отправителя — в посылки получателю, плюс строка в
    * общий чат. Возвращает, что ушло, и доставку получателю — её запускают,
    * отпустив лок отправителя (см. [[handOver]]). */
  def send(
    sender: User,
    hero:   Hero,
    target: TransferTarget,
    items:  List[Item],
    now:    Long
  ): Task[Transfers.Shipment] =
    for {
      name <- players.getDisplayName(sender).orElse(ZIO.succeed(content.text("transfer.someone")))
      sent <- ZIO.foreach(items) { item =>
                inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
                  parcels.send(target.heroId, name, item, now)
              }
      _    <- ZIO.when(items.nonEmpty)(announce(sender, name, target, items))
    } yield Transfers.Shipment(items, handOver(name, target, sent))

  /** Доставка получателю — под ЕГО локом: посылки сразу в ячейку, что не
    * влезло — на почту, и письмо, где теперь вещи. Лок отправителя к этому
    * времени отпущен: двое, что одновременно передают друг другу, ждали бы
    * друг друга вечно. Не вышло — посылки подберёт фон ([[mailRound]]) или
    * ход получателя ([[Parcels.deliver]]), что раньше. */
  private def handOver(fromName: String, target: TransferTarget, sent: List[Parcel]): Task[Unit] =
    ZIO.when(sent.nonEmpty) {
      lock.withLock(target.userId) {
        for {
          now      <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
          landed   <- parcels.arrive(target.heroId, sent.map(_.id), now)
          receiver <- userRepo.getUserById(target.userId).option.map(_.flatten)
          _        <- ZIO.foreachDiscard(receiver.zip(letter(fromName, sent, landed))) { case (r, text) =>
                        players.notify(r, text).ignore
                      }
        } yield ()
      }.catchAllCause(cause => ZIO.logErrorCause(s"Посылки не дошли до игрока ${target.userId.value}", cause))
    }.unit

  /** Письмо получателю: где теперь переданное, а если почта перед этим
    * сгорела (срок вышел, а фон ещё не успел), — и об этом. Про срок почты —
    * только когда он пошёл с этих вещей: пока на почте что-то лежит, срок у
    * новых тот же, что у остального. */
  private def letter(fromName: String, sent: List[Parcel], landed: Parcels.Landing): Option[String] = {
    val vault  = landed.delivered.size
    val mail   = landed.mailed.size
    val notice = Option.when(vault + mail > 0) {
      val where =
        if (mail == 0) content.text("transfer.noticeVault")
        else if (vault == 0) content.text("transfer.noticeMail")
        else content.format("transfer.noticeSplit", "vault" -> vault.toString, "mail" -> mail.toString)
      val shelf = if (landed.started) content.text("transfer.noticeShelf") else ""
      content.format("transfer.notice",
        "from"  -> fromName,
        "name"  -> sent.head.item.displayTitle,
        "count" -> (vault + mail).toString) + where + shelf
    }
    val lines = Option.when(landed.burned.nonEmpty)(parcels.burnedText(landed.burned)).toList ++ notice
    Option.when(lines.nonEmpty)(lines.mkString("\n\n"))
  }

  /** Почта фоном, раз в минуту, — хода хозяев не ждём. Посылки, которые не
    * разложились сразу (доставка упала или их отправили до того, как они
    * стали ложиться сразу), — по местам, под локом получателя, и ему письмо.
    * Срок почты вышел — всё, что на ней лежит, сгорает, и хозяину письмо:
    * тут лок не нужен — посылку забирают удалением, и одна и та же не
    * достанется и огню, и хозяину. */
  def mailRound(now: Long): Task[Unit] =
    for {
      stuck  <- parcels.stranded(now - Transfers.StrandedMs)
      _      <- ZIO.foreachDiscard(stuck) { case (heroId, userId, ids) =>
                  lock.withLock(userId)(parcels.arrive(heroId, ids, now))
                    .flatMap(landed => ZIO.foreachDiscard(parcels.report(landed))(write(userId, _)))
                    .catchAllCause(cause => ZIO.logErrorCause(s"Посылки игрока ${userId.value} не разложились", cause))
                }
      burned <- parcels.burnExpired(now)
      _      <- ZIO.foreachDiscard(burned) { case (userId, lost) => write(userId, parcels.burnedText(lost)) }
    } yield ()

  /** Письмо игроку по его номеру. */
  private def write(userId: UserId, text: String): Task[Unit] =
    userRepo.getUserById(userId).option.map(_.flatten)
      .flatMap(ZIO.foreachDiscard(_)(players.notify(_, text).ignore))

  /** Передать серебро. Отправитель платит сразу — сумму и комиссию сверху
    * (см. [[Transfers.fee]]): сперва с рук, нехватку из ячейки, как любые траты
    * (см. [[Purse]]). Получателю сумма ложится в банковскую ячейку, а не
    * приняла она — ждёт его в городе, как выручка с аукциона (см. [[Payouts]]):
    * на руках в лабиринте половина сгорела бы при смерти. Комиссия не достаётся
    * никому. Самого героя получателя здесь не трогаем — его пишут только под
    * его собственным локом. Зачислить не вышло — всё снятое возвращается. */
  def sendSilver(sender: User, hero: Hero, target: TransferTarget, amount: Long): Task[Transfers.Outcome] =
    tooLittle(Money.Silver, amount) match {
      case Some(refusal) => ZIO.succeed(Transfers.Outcome.Forbidden(refusal))
      case None =>
        val fee  = Transfers.fee(amount)
        val cost = amount + fee
        purse.charge(sender.userId, hero, cost).flatMap {
          case None =>
            purse.total(hero).map(have => Transfers.Outcome.Forbidden(
              notEnough("transfer.money.notEnoughSilver", Money.Silver, cost, fee, have)))
          case Some(paid) =>
            val refund = heroDao.updateSilver(sender.userId, paid.silver + cost)
            for {
              intoVault <- bank.depositSilver(target.heroId, amount).either.flatMap {
                             case Right(_) => ZIO.succeed(true)
                             case Left(_)  => payouts.queue(target.heroId, silver = amount).as(false)
                           }.tapError(_ => refund)
              _         <- tellMoney(sender, target, Transfers.moneyLine(Money.Silver, amount),
                             if (intoVault) Transfers.Landed.Vault else Transfers.Landed.City)
            } yield Transfers.Outcome.Sent(sentLine(Money.Silver, amount, target))
        }
    }

  /** Дублоны, шаг первый — под локом отправителя: снять с его рук сумму и
    * комиссию сверху (см. [[Transfers.fee]]). `Some` — отказ: меньше минимума
    * или столько у него нет, и ничего не списано.
    *
    * Дублоны не теряются нигде, поэтому и отдают их откуда угодно, и получатель
    * берёт их сразу в руки ([[giveDoubloons]]) — ждать города им незачем. */
  def takeDoubloons(sender: User, hero: Hero, amount: Long): Task[Option[String]] =
    tooLittle(Money.Doubloons, amount) match {
      case Some(refusal) => ZIO.some(refusal)
      case None =>
        val fee  = Transfers.fee(amount)
        val cost = amount + fee
        if (hero.doubloons < cost)
          ZIO.some(notEnough("transfer.money.notEnoughDoubloons", Money.Doubloons, cost, fee, hero.doubloons))
        else heroDao.updateDoubloons(sender.userId, hero.doubloons - cost).as(None)
    }

  /** Шаг второй — под локом ПОЛУЧАТЕЛЯ: дублоны ему в руки. Героя читаем
    * заново, уже под его локом: иначе зачисление затёрлось бы его же тратой. */
  def giveDoubloons(target: TransferTarget, amount: Long): Task[Unit] =
    addDoubloons(target.userId, amount)

  /** Зачислить не вышло — вернуть отправителю всё снятое, комиссию тоже. Под
    * его локом и по свежему герою: между шагами он мог что-то купить. */
  def returnDoubloons(sender: User, amount: Long): Task[Unit] =
    addDoubloons(sender.userId, amount + Transfers.fee(amount))

  private def addDoubloons(userId: UserId, amount: Long): Task[Unit] =
    heroDao.getHeroByUserId(userId).someOrFail(new Throwable(s"Нет героя у игрока ${userId.value}"))
      .flatMap(h => heroDao.updateDoubloons(userId, h.doubloons + amount))

  /** Дублоны дошли: письмо получателю, строка в беседу. Возвращает ответ
    * отправителю. */
  def doubloonsDelivered(sender: User, target: TransferTarget, amount: Long): Task[String] =
    tellMoney(sender, target, Transfers.moneyLine(Money.Doubloons, amount), Transfers.Landed.Hand)
      .as(sentLine(Money.Doubloons, amount, target))

  /** Меньше минимума не передают (см. [[Transfers.MinMoney]]). */
  private def tooLittle(money: Money, amount: Long): Option[String] =
    Option.when(amount < Transfers.MinMoney)(content.format("transfer.money.tooLittle",
      "min" -> Transfers.moneyLine(money, Transfers.MinMoney)))

  /** Отказ, когда денег не хватает: сколько нужно с комиссией и сколько есть. */
  private def notEnough(key: String, money: Money, cost: Long, fee: Long, have: Long): String =
    content.format(key,
      "need" -> Transfers.moneyLine(money, cost),
      "fee"  -> Transfers.moneyLine(money, fee),
      "have" -> Transfers.moneyLine(money, have))

  /** Ответ отправителю: что ушло и сколько сгорело комиссией. */
  private def sentLine(money: Money, amount: Long, target: TransferTarget): String =
    content.format("transfer.money.sent",
      "money" -> Transfers.moneyLine(money, amount),
      "to"    -> target.name,
      "fee"   -> Transfers.moneyLine(money, Transfers.fee(amount)))

  /** Письмо получателю — где теперь его деньги, — и строка в общий чат. */
  private def tellMoney(sender: User, target: TransferTarget, line: String, landed: Transfers.Landed): Task[Unit] =
    for {
      fromName <- players.getDisplayName(sender).orElse(ZIO.succeed(content.text("transfer.someone")))
      receiver <- userRepo.getUserById(target.userId).option.map(_.flatten)
      key       = landed match {
                    case Transfers.Landed.Vault => "transfer.money.noticeVault"
                    case Transfers.Landed.City  => "transfer.money.noticeCity"
                    case Transfers.Landed.Hand  => "transfer.money.noticeHand"
                  }
      _        <- ZIO.foreachDiscard(receiver)(r => players.notify(r, content.format(key,
                    "from" -> fromName, "money" -> line)).ignore)
      _        <- players.announce(content.format("transfer.money.announce",
                    "from"  -> mention(sender.vkId.value, fromName),
                    "to"    -> receiver.map(r => mention(r.vkId.value, target.name)).getOrElse(target.name),
                    "money" -> line)).ignore
    } yield ()

  /** Строка в общий чат: передача — дело публичное. Письмо получателю уходит
    * с доставкой ([[handOver]]) — когда уже ясно, где вещи. */
  private def announce(sender: User, fromName: String, target: TransferTarget, items: List[Item]): Task[Unit] =
    for {
      receiver <- userRepo.getUserById(target.userId).option.map(_.flatten)
      _        <- players.announce(content.format("transfer.announce",
                    "from"  -> mention(sender.vkId.value, fromName),
                    "to"    -> receiver.map(r => mention(r.vkId.value, target.name)).getOrElse(target.name),
                    "name"  -> items.head.displayTitle,
                    "count" -> items.size.toString)).ignore
    } yield ()

  /** Кликабельное упоминание игрока в ВК. */
  private def mention(vkId: String, name: String): String = s"[id$vkId|$name]"
}

object Transfers {

  /** Чем кончилась попытка передать по одной фразе из беседы. */
  sealed trait Outcome
  object Outcome {
    /** Всё однозначно — вещи уже ушли, отправителю остаётся это сообщение.
      * `handOver` — доставка получателю: её запускают, отпустив лок
      * отправителя. У денег её нет — они дошли сразу. */
    final case class Sent(message: String, handOver: Task[Unit] = ZIO.unit) extends Outcome
    /** В сумке нет ничего похожего. */
    case object Empty extends Outcome
    /** Так не передать: трофеи из рук в руки не ходят, денег не хватает или
      * сумму не назвали. Отправителю остаётся это сообщение. */
    final case class Forbidden(message: String) extends Outcome
    /** Надо уточнить кнопками: экипировка или несколько разных вещей. */
    case object NeedPick extends Outcome
  }

  /** Что ушло и доставка получателю (см. [[Transfers.send]]). */
  final case class Shipment(items: List[Item], handOver: Task[Unit])

  /** Куда легли переданные деньги — об этом пишем получателю. */
  sealed trait Landed
  object Landed {
    /** Серебро — в банковскую ячейку. */
    case object Vault extends Landed
    /** Серебро, которое ячейка не приняла: ждёт его в городе. */
    case object City  extends Landed
    /** Дублоны — сразу в руки. */
    case object Hand  extends Landed
  }

  /** Меньше этого денег не передают — ни серебра, ни дублонов. */
  val MinMoney: Long = 50L

  /** Посылка в пути дольше минуты — значит, сразу разложить её не вышло, и
    * её подберёт фон ([[Transfers.mailRound]]). */
  val StrandedMs: Long = 60L * 1000L

  /** Комиссия с передачи денег, в процентах. Платит отправитель сверху суммы,
    * и не достаётся она никому: получатель получает ровно названное. */
  val FeePct: Long = 10L

  /** Комиссия с этой суммы, с округлением вверх: десятая часть, но не меньше —
    * с 55 это 6, а не 5. */
  def fee(amount: Long): Long = (amount * FeePct + 99L) / 100L

  /** Сумма со значком валюты: 🪙 серебро, 🟡 дублоны — как везде в игре. */
  def moneyLine(money: Money, amount: Long): String = money match {
    case Money.Silver    => s"🪙 $amount"
    case Money.Doubloons => s"🟡 $amount"
  }

  val live: ZLayer[InventoryRepository with UserRepository with Parcels with Players with SceneContent
                     with HeroDao with BankRepository with Payouts with PlayerLock, Nothing, Transfers] =
    ZLayer.fromFunction(Transfers(_, _, _, _, _, _, _, _, _))
}
