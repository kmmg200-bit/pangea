package pangea.service.state

import io.circe.Json
import pangea.metrics.GameMetrics
import pangea.dao.hero.HeroDao
import pangea.engine.{Choice, ChoiceColor, Renderer, Screen}
import pangea.model.hero.Achievement
import pangea.model.home.HomeData
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.vk.Community
import pangea.repository.hero.HeroRepository
import pangea.repository.user.UserRepository
import io.circe.syntax.EncoderOps
import pangea.service.activity.{ActivityContest, GroupEvent}
import pangea.service.admin.{AdminPanel, HomeAlert}
import pangea.engine.SceneContent
import pangea.service.referral.Referrals
import pangea.service.promo.PromoCodes
import pangea.service.ErrorLog
import pangea.service.chat.{ChatCommand, ChatProfile, Links}
import pangea.service.parcel.{Parcels, TransferTarget, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.states.parcel.TransferState
import pangea.service.sender.{Api, ChatTarget}
import pangea.service.sender.vk.{VkPlayers, VkRenderer}
import pangea.service.state.states.StatesMap
import pangea.service.trade.Trades
import zio.{Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

class StateHandler(
  api: Api,
  userRepo: UserRepository,
  heroRepo: HeroRepository,
  heroDao: HeroDao,
  payouts: Payouts,
  parcels: Parcels,
  transfers: Transfers,
  states: Map[StateType, State],
  admin: AdminPanel,
  content: SceneContent,
  lock: PlayerLock,
  // Билеты за активность в группе, код друга и промокоды. Без них (тесты) этих
  // частей просто нет: события группы не считаются, бонус за 10 уровень не
  // платится, промокод уходит сцене обычным текстом.
  activity: Option[ActivityContest] = None,
  referrals: Option[Referrals] = None,
  promo: Option[PromoCodes] = None,
  // Обмены: /restart закрывает открытые. Без них (тесты) /restart обмены не
  // трогает.
  trades: Option[Trades] = None
) extends SceneDoor {

  /** Чат игрока с ботом — для двери из приложения ([[SceneDoor]]). */
  override val chat: Renderer = VkRenderer(api)

  /** Из приложения — в сцену бота: тот же переход, что после хода в чате
    * (экран сцены, запись героя, сквозные сцены). Лок держит приложение. */
  override def enter(user: User, from: StateType, to: StateType): Task[Unit] =
    transitionTo(user, from, to, chat)

  /** Имена игроков в беседе: имя героя, а нет его — из ВК. */
  private val players = VkPlayers(api, heroDao)

  /** Событие группы ВК для билетов за активность: вышел пост, игрок поставил
    * лайк или оставил комментарий. Лок игрока не нужен — героя это не трогает,
    * только таблицы билетов. */
  def groupEvent(event: GroupEvent): Task[Unit] =
    ZIO.foreachDiscard(activity)(contest =>
      ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS)).flatMap(now => contest.handle(event, now)))

  def makeActionVK(
      vkId: VkId,
      eventId: Long,
      action: UserAction
  ): Task[Unit] = {
    val renderer = VkRenderer(api)
    GameMetrics.timed(GameMetrics.turnSeconds) {
      for {
        userOp <- userRepo.getUserByVkId(vkId)
        user <- userOp match {
          case Some(u) => ZIO.succeed(u)
          case None    => userRepo.insertUserByVk(vkId)
        }
        asked <- GameMetrics.now
        _ <- lock.withLock(user.userId) {
          for {
            _     <- GameMetrics.since(GameMetrics.lockWaitSeconds, asked)
            isNew <- userRepo.checkAndRecordEvent(user.userId, eventId)
            _     <- ZIO.when(isNew)(makeActionSafe(user, action, renderer, Some(eventId)))
          } yield ()
        }
      } yield ()
    }
  }

  def makeActionTelegram(
      telegramId: TelegramId,
      action: UserAction
  ): Task[Unit] = {
    val renderer = VkRenderer(api)
    for {
      userOp <- userRepo.getUserByTelegramId(telegramId)
      user <- userOp match {
        case Some(u) => ZIO.succeed(u)
        case None    => userRepo.insertUserByTelegramId(telegramId)
      }
      _ <- lock.withLock(user.userId)(makeActionSafe(user, action, renderer))
    } yield ()
  }

  /** «Передать» из общей беседы: адресата бот берёт из процитированного
    * сообщения, а выбирать вещь отправитель будет у себя в личке — там у
    * каждой кнопки видны характеристики, и одинаковые названия не путаются.
    * «Передать серебро 100» и «Передать дублоны 5» отдают деньги: их в сумке
    * не ищут.
    *
    * Вещи и серебро отдают только из города; получатель может быть где угодно,
    * вещь придёт ему посылкой, серебро — в ячейку или в руки в городе. Дублоны
    * не теряются нигде: их отдают откуда угодно, и в руки они идут сразу.
    *
    * `chat` — беседа, где написали «Передать»: строка о передаче уходит туда
    * же (см. `Transfers.log`). */
  def transferFromChat(senderVk: VkId, targetVk: VkId, query: String, eventId: Long,
                       chat: Option[ChatTarget.Peer] = None): Task[Unit] = {
    val renderer = VkRenderer(api)
    userRepo.getUserByVkId(senderVk).flatMap {
      case None => ZIO.unit  // писал не игрок — молчим
      case Some(sender) =>
        lock.withLock(sender.userId) {
          // Ключ события беседы приходит готовым (см. ChatCommand.eventKey):
          // он отрицательный, чтобы не путаться с номерами из лички.
          userRepo.checkAndRecordEvent(sender.userId, eventId).flatMap { fresh =>
            if (fresh) startTransfer(sender, targetVk, query, renderer, chat) else ZIO.none
          }
        }
          // Хвост передачи — то, что делается уже под локом получателя (дублоны
          // ему в руки, посылки в ячейку или на почту). Два лока разом не
          // держим: двое, что одновременно передают друг другу, ждали бы друг
          // друга вечно.
          .flatMap(ZIO.foreachDiscard(_)(identity))
    }
  }

  /** Личка гильдейского сообщества: оно только для бесед — одна строка, где
    * играть. Ответ — от его имени, туда же, в личку. */
  def pointToGame(peerId: Long): Task[Unit] =
    api.sendToChat(ChatTarget.Peer(peerId, Community.Guilds), content.format("chat.guildsDirect", "game" -> Links.Game))

  /** «Мой профиль» и «Моё снаряжение» из беседы: ответ уходит туда же, в беседу
    * `chat`, откуда спросили, — о себе рассказывают при всех. Писал не игрок
    * или игрок без героя — молчим: беседа не место для сообщений об ошибках. */
  def selfToChat(senderVk: VkId, command: ChatCommand.Self, eventId: Long, chat: ChatTarget.Peer): Task[Unit] =
    userRepo.getUserByVkId(senderVk).flatMap {
      case None => ZIO.unit
      case Some(sender) =>
        lock.withLock(sender.userId) {
          // Ключ события беседы — как и у «Передать», уже отрицательный.
          userRepo.checkAndRecordEvent(sender.userId, eventId).flatMap { fresh =>
            ZIO.when(fresh)(tellAboutSelf(sender, command, chat)).unit
          }
        }
    }

  /** «Туториал» из беседы: ссылка на статью — туда же, в беседу `chat`.
    * Отвечаем любому, кто спросил: игрок он или пока нет, статья ему не
    * помешает. От повтора того же сообщения защищаемся, когда пишет игрок, — как
    * у прочих команд беседы: счётчик событий есть только у игрока. */
  def tutorialToChat(senderVk: VkId, eventId: Long, chat: ChatTarget.Peer): Task[Unit] = {
    val text  = content.format("chat.tutorial", "tutorial" -> Links.Tutorial)
    val reply = api.sendToChat(chat, text)
    userRepo.getUserByVkId(senderVk).flatMap {
      case None => reply
      case Some(sender) =>
        lock.withLock(sender.userId) {
          userRepo.checkAndRecordEvent(sender.userId, eventId).flatMap(fresh => ZIO.when(fresh)(reply).unit)
        }
    }
  }

  /** «Faq» из беседы — та же схема, что у «Туториал» (см. [[tutorialToChat]]). */
  def faqToChat(senderVk: VkId, eventId: Long, chat: ChatTarget.Peer): Task[Unit] = {
    val text  = content.format("chat.faq", "faq" -> Links.Faq)
    val reply = api.sendToChat(chat, text)
    userRepo.getUserByVkId(senderVk).flatMap {
      case None => reply
      case Some(sender) =>
        lock.withLock(sender.userId) {
          userRepo.checkAndRecordEvent(sender.userId, eventId).flatMap(fresh => ZIO.when(fresh)(reply).unit)
        }
    }
  }

  /** «Дать Торопыга» из беседы ответом на сообщение игрока: достижение — тому,
    * на чьё сообщение ответили. Давать вправе только «Админ 1»; от прочих
    * команду молча пропускаем. Ответ — в ту же беседу, игроку в личку — строка
    * о достижении. Локи берём по очереди: сперва написавшего (защита от
    * повтора), потом получателя. */
  def grantFromChat(senderVk: VkId, targetVk: VkId, a: Achievement, eventId: Long, chat: ChatTarget.Peer): Task[Unit] =
    userRepo.getUserByVkId(senderVk).flatMap {
      case None => ZIO.unit
      case Some(sender) =>
        for {
          fresh   <- lock.withLock(sender.userId)(userRepo.checkAndRecordEvent(sender.userId, eventId))
          allowed <- if (fresh) admin.canGrant(sender.userId) else ZIO.succeed(false)
          _       <- ZIO.when(allowed)(grantTo(targetVk, a, chat))
        } yield ()
    }

  private def grantTo(targetVk: VkId, a: Achievement, chat: ChatTarget.Peer): Task[Unit] = {
    userRepo.getUserByVkId(targetVk).flatMap {
      case None => api.sendToChat(chat, content.text("chat.grant.noHero"))
      case Some(target) =>
        for {
          outcome <- lock.withLock(target.userId) {
                       heroDao.getHeroByUserId(target.userId).flatMap {
                         case None                                 => ZIO.succeed(Option.empty[Boolean])
                         case Some(hero) if hero.hasAchievement(a) => ZIO.succeed(Option(false))
                         case Some(hero) =>
                           QuestSupport.grant(heroDao, content, target, hero, a, VkRenderer(api)).as(Option(true))
                       }
                     }
          name    <- players.getDisplayName(target).orElse(ZIO.succeed(StateHandler.TransferSomeone))
          _       <- api.sendToChat(chat, outcome match {
                       case None        => content.text("chat.grant.noHero")
                       case Some(true)  => content.format("chat.grant.done", "name" -> name, "title" -> a.title,
                                             "bonus" -> a.bonusLine)
                       case Some(false) => content.format("chat.grant.already", "name" -> name, "title" -> a.title)
                     })
        } yield ()
    }
  }

  private def tellAboutSelf(sender: User, command: ChatCommand.Self, chat: ChatTarget.Peer): Task[Unit] =
    heroRepo.getHero(sender.userId).flatMap {
      case None => ZIO.unit
      case Some(hero) =>
        for {
          now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
          name <- players.getDisplayName(sender).orElse(ZIO.succeed(StateHandler.TransferSomeone))
          text <- command match {
                    case ChatCommand.Self.Gear => ZIO.succeed(ChatProfile.gear(name, hero))
                    case ChatCommand.Self.Profile =>
                      AzatData.load(heroDao, sender.userId, now).map(azat =>
                        ChatProfile.profile(name, hero, now, azat.blessingUntil, azat.instantRests, content))
                  }
          _    <- api.sendToChat(chat, text)
        } yield ()
    }

  /** Начало передачи — под локом отправителя. Возвращает хвост, если он есть:
    * то, что надо сделать уже под локом получателя, когда этот лок отпущен. */
  private def startTransfer(sender: User, targetVk: VkId, query: String, renderer: Renderer,
                            chat: Option[ChatTarget.Peer]): Task[Option[Task[Unit]]] =
    for {
      heroOp   <- heroRepo.getHero(sender.userId)
      targetOp <- userRepo.getUserByVkId(targetVk)
      targetHero <- targetOp.fold(ZIO.none: Task[Option[pangea.model.hero.Hero]])(u => heroRepo.getHero(u.userId))
      money     = ChatCommand.moneyQuery(query)
      // Дублоны не теряются нигде — их можно отдать хоть из лабиринта.
      anywhere  = money.exists(_._1 == ChatCommand.Money.Doubloons)
      silver    = money.exists(_._1 == ChatCommand.Money.Silver)
      items     = money.isEmpty
      inCity    = heroOp.exists(h => StateType.cityStates.contains(h.state))
      // Вне города серебро и вещи передаёт личный посыльный — серебро только из
      // ячейки, вещи из хранилища Торгового дома, а нет там — с полок дома. В
      // городе он носит вещь, которой нет при герое.
      courier  <- if (heroOp.isDefined && (items || (silver && !inCity))) hasCourier(sender.userId)
                  else ZIO.succeed(false)
      tail <- (heroOp, targetOp, targetHero) match {
        case (Some(hero), Some(target), Some(theirHero)) if target.userId != sender.userId =>
          if (!anywhere && !inCity && !courier)
            api.sendMessage(sender, StateHandler.TransferNotInCity, List.empty, None).as(None)
          else
            for {
              name   <- players.getDisplayName(target).orElse(ZIO.succeed(StateHandler.TransferSomeone))
              to      = TransferTarget(theirHero.id, target.userId, name)
              tail   <- money match {
                          case Some((ChatCommand.Money.Doubloons, Some(amount))) =>
                            startDoubloons(sender, hero, to, amount, chat)
                          case Some((ChatCommand.Money.Silver, Some(amount))) =>
                            (if (inCity) transfers.sendSilver(sender, hero, to, amount, chat)
                             else transfers.sendSilverFromVault(sender, hero, to, amount, chat))
                              .flatMap(reply(sender, _)).as(None)
                          // Деньги, но без суммы: спрашиваем, сколько.
                          case Some((_, None)) =>
                            api.sendMessage(sender, content.text("transfer.money.howMuch"), List.empty, None).as(None)
                          // Вещи: в городе — из того, что при герое, а с
                          // посыльным, если там нет, — из хранилища или с полок
                          // дома; вне города (сюда без посыльного не дойти) —
                          // только посыльный.
                          case None if inCity =>
                            sendItems(sender, hero, target, theirHero, to, query, renderer, chat, courier)
                          case None =>
                            courierItems(sender, hero, to, query, chat)
                        }
            } yield tail
        case (Some(_), _, _) =>
          api.sendMessage(sender, StateHandler.TransferNoTarget, List.empty, None).as(None)
        case _ => ZIO.none
      }
    } yield tail

  /** Вещи вне города — из хранилища или с полок дома, личным посыльным. Экрана выбора нет: он
    * затёр бы сцену, где сейчас герой, — посыльный берёт только понятное без
    * выбора (см. [[Transfers.courierSend]]). Ушли — хвостом доставка получателю:
    * ей нужен его лок. */
  private def courierItems(sender: User, hero: pangea.model.hero.Hero, to: TransferTarget,
                           query: String, chat: Option[ChatTarget.Peer]): Task[Option[Task[Unit]]] =
    for {
      now     <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      outcome <- transfers.courierSend(sender, hero, to, ChatCommand.itemQuery(query), ChatCommand.count(query), now,
                   chat)
      tail    <- outcome match {
                   case sent: Transfers.Outcome.Sent => reply(sender, sent).as(Some(sent.handOver))
                   case other                        => reply(sender, other).as(None)
                 }
    } yield tail

  /** Есть ли дома у игрока личный посыльный (см. `Furniture.Courier`). */
  private def hasCourier(userId: UserId): Task[Boolean] =
    heroDao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).exists(_.hasCourier))

  /** Дублоны: с отправителя снимаем здесь, под его локом, а зачисляем хвостом —
    * под локом получателя. Не вышло зачислить — возвращаем, снова под локом
    * отправителя. «Отправлено» говорим, только когда дублоны дошли. */
  private def startDoubloons(sender: User, hero: pangea.model.hero.Hero, to: TransferTarget,
                             amount: Long, chat: Option[ChatTarget.Peer]): Task[Option[Task[Unit]]] =
    transfers.takeDoubloons(sender, hero, amount).flatMap {
      case Some(refusal) => api.sendMessage(sender, refusal, List.empty, None).as(None)
      case None =>
        ZIO.some(
          lock.withLock(to.userId)(transfers.giveDoubloons(to, amount)).foldCauseZIO(
            cause =>
              ErrorLog.logged(s"Дублоны не дошли до ${to.userId.value}")(cause) *>
                lock.withLock(sender.userId)(transfers.returnDoubloons(sender, amount)) *>
                api.sendMessage(sender, content.text("transfer.money.failed"), List.empty, None).unit,
            _ =>
              transfers.doubloonsDelivered(sender, to, amount, chat)
                .flatMap(api.sendMessage(sender, _, List.empty, None)).unit
          ))
    }

  /** Ответ отправителю на итог передачи. */
  private def reply(sender: User, outcome: Transfers.Outcome): Task[Unit] =
    outcome match {
      case Transfers.Outcome.Sent(message, _)   => api.sendMessage(sender, message, List.empty, None).unit
      case Transfers.Outcome.Forbidden(message) => api.sendMessage(sender, message, List.empty, None).unit
      case Transfers.Outcome.Empty | Transfers.Outcome.NeedPick => ZIO.unit
    }

  /** Вещь в городе — из того, что при герое (сумка, ларец, Живая сумка,
    * шкаф): понятное название уходит сразу; экран нужен только там, где вещь
    * можно спутать, — в первую очередь у экипировки. Нет при герое, а дома
    * есть посыльный (`courier`) — он несёт из хранилища или с полок дома (см.
    * [[Transfers.quickSend]]). Ушла — хвостом возвращаем доставку получателю:
    * ей нужен его лок. */
  private def sendItems(sender: User, hero: pangea.model.hero.Hero, target: User, theirHero: pangea.model.hero.Hero,
                        to: TransferTarget, query: String, renderer: Renderer,
                        chat: Option[ChatTarget.Peer], courier: Boolean): Task[Option[Task[Unit]]] = {
    val item  = ChatCommand.itemQuery(query)
    val count = ChatCommand.count(query)
    for {
      now     <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      outcome <- transfers.quickSend(sender, hero, to, item, count, now, chat, courier)
      tail <- outcome match {
        case Transfers.Outcome.Empty =>
          api.sendMessage(sender, content.format("transfer.nothing", "query" -> item), List.empty, None).as(None)
        case Transfers.Outcome.NeedPick =>
          // Выбирают в личке, а строка о передаче уйдёт в беседу, где её затеяли.
          val scene = TransferState.TransferScene(
            toHeroId = theirHero.id.value,
            toUserId = target.userId.value,
            toName   = to.name,
            query    = item,
            count    = count,
            chat     = chat.map(_.peerId),
            guildChat = chat.map(_.community == Community.Guilds))
          (heroDao.writeSceneData(sender.userId, scene.asJson) *>
            // Куда вернуть после передачи — туда же, откуда позвали.
            heroDao.writeReturnState(sender.userId, Some(hero.state)) *>
            enterState(sender, StateType.Transfer, renderer)).as(None)
        case sent: Transfers.Outcome.Sent => reply(sender, sent).as(Some(sent.handOver))
        case refused                      => reply(sender, refused).as(None)
      }
    } yield tail
  }

  /** Показать экран состояния и записать его герою (без проверки «уже там»). */
  private def enterState(user: User, to: StateType, renderer: Renderer): Task[Unit] =
    for {
      target <- ZIO.fromOption(states.get(to)).orElseFail(new Throwable(s"Not found state '$to'"))
      _      <- target.enter(user, renderer)
      _      <- heroRepo.updateState(user.userId, to)
    } yield ()

  /** Исполняет отложенную задачу (см. `Scheduler`) под локом игрока. Действие
    * применяется только если текущее состояние героя совпадает с
    * `expectedState` — иначе задача потеряла смысл (игрок ушёл в другой стейт)
    * и тихо пропускается. Ошибки `makeAction` пробрасываются наружу, чтобы
    * планировщик мог посчитать попытку.
    */
  def runScheduled(
      userId: UserId,
      expectedState: StateType,
      action: UserAction
  ): Task[StateHandler.RunResult] = {
    val renderer = VkRenderer(api)
    lock.withLock(userId) {
      userRepo.getUserById(userId).flatMap {
        case None => ZIO.succeed(StateHandler.RunResult.Skipped)
        case Some(user) =>
          heroRepo.getHero(userId).flatMap {
            case Some(hero) if hero.state == expectedState =>
              makeAction(user, action, renderer)
                .as(StateHandler.RunResult.Executed)
            case _ =>
              ZIO.succeed(StateHandler.RunResult.Skipped)
          }
      }
    }
  }

  /** `messageId` — номер сообщения игрока в диалоге сообщества: его /home и
    * подтверждение /restart пересылаются «Админ 1». */
  private def makeActionSafe(
      user: User,
      action: UserAction,
      renderer: Renderer,
      messageId: Option[Long] = None
  ): Task[Unit] =
    makeAction(user, action, renderer, messageId)
      // Любая поломка хода — и ошибка, и падение кода (defect): причина целиком
      // в лог, игроку — что делать дальше.
      .catchAllCause(cause =>
        ErrorLog.logged(s"Ход игрока ${user.userId.value} не обработан")(cause) *>
          api.sendMessage(
            user,
            "Произошла ошибка, пропишите /home или обратитесь в техподдержку",
            List.empty,
            None
          )
      )

  private def makeAction(
      user: User,
      action: UserAction,
      renderer: Renderer,
      messageId: Option[Long] = None
  ): Task[Unit] =
    for {
      heroOp <- heroRepo.getHero(user.userId)
      hero <- heroOp match {
        case Some(h) => ZIO.succeed(h)
        case None    => heroRepo.registerNewHero(user.userId)
      }
      // Выручка с аукциона, не влезшая в банковскую ячейку, догоняет героя в
      // городе: в лабиринте такие деньги наполовину сгорели бы при смерти.
      // Дублоны не сгорают нигде — они приходят где угодно (переводы, призы
      // конкурса, награда за код друга).
      _ <- (if (StateType.cityStates.contains(hero.state)) payouts.deliver(user, hero, renderer)
            else payouts.deliverDoubloons(user, hero, renderer)).ignore
      // Друг, пришедший по коду, дорос до 10 уровня — владельцу кода награда.
      _ <- ZIO.foreachDiscard(referrals)(_.payLevelBonus(user, hero)).ignore
      // Посылки ложатся в банковскую ячейку откуда угодно: она не при герое.
      _ <- parcels.deliver(user, hero, renderer).ignore
      hero <- heroRepo.getHero(user.userId).map(_.getOrElse(hero))
      // Админ-панель стоит перед игровым автоматом: пока она открыта, ввод
      // идёт ей, а состояние героя и сцена остаются нетронутыми.
      inPanel <- admin.intercept(user, hero, action, renderer)
      // Промокод — голым сообщением с любого экрана, как /home: ответ приходит
      // письмом, а сцена и её кнопки остаются как были.
      byCode  <- if (inPanel) ZIO.succeed(false)
                 else ZIO.foreach(promo)(_.intercept(user, hero, action, renderer)).map(_.contains(true))
      _ <-
        if (inPanel || byCode) ZIO.unit
        else if (StateHandler.isHomeCommand(action))
          goHome(user, renderer) *>
            toAdmins(user, "/home", HomeAlert.caption(user.vkId, hero.state), messageId)
        else if (StateHandler.isRestartCommand(action))
          offerRestart(user, renderer)
        else if (StateHandler.isRestartConfirm(action))
          restart(user, renderer) *>
            toAdmins(user, "/restart", HomeAlert.wipeCaption(user.vkId, hero.lvl, hero.state), messageId)
        else
          for {
            state <- ZIO
              .fromOption(states.get(hero.state))
              .orElseFail(
                new Throwable(
                  s"Not found state of hero with id ${hero.id} of user ${user.userId}: state - ${hero.state}"
                )
              )

            _ <- GameMetrics.timed(GameMetrics.stateActionSeconds.tagged("state", hero.state.entryName)) {
                   state.action(user, action, renderer)
                     .flatMap(transitionTo(user, hero.state, _, renderer))
                 }
          } yield ()
      // Вехи — убитые, уровень: какой бы сценой герой ни перевалил порог,
      // достижение приходит строкой после хода. Не вышло — ход всё равно
      // состоялся: пишем в лог, а веху выдаст следующий ход.
      _ <- heroDao.getHeroByUserId(user.userId)
             .flatMap(ZIO.foreachDiscard(_)(QuestSupport.grantMilestones(heroDao, content, user, _, renderer)))
             .catchAllCause(ErrorLog.logged(s"Вехи героя ${user.userId.value} не проверены"))
    } yield ()

  /** `/restart`, первый шаг: только предупреждение с inline-кнопкой. Клавиатура
    * текущей сцены при этом остаётся на месте — передумавший игрок просто играет
    * дальше, никакой «отмены» ему не нужно. */
  private def offerRestart(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(
      StateHandler.RestartWarning,
      List(Choice(StateHandler.RestartConfirmId, "💀 Да, стереть всё и начать заново",
        color = ChoiceColor.Negative)),
      inline = true))

  /** `/restart`, второй шаг: герой и всё, что к нему привязано, удаляются одной
    * транзакцией. Запись пользователя остаётся, поэтому тут же прогоняем пустое
    * действие через обычный диспетчер — он не найдёт героя, заведёт нового и
    * покажет первый экран регистрации, как при самом первом входе.
    *
    * Обмены привязаны к игроку, а не к герою, и сами со стиранием не уйдут:
    * их закрываем до него (см. Trades.forfeit) — партнёру его заклад, а
    * заклад игрока пропадает вместе с героем. */
  private def restart(user: User, renderer: Renderer): Task[Unit] =
    for {
      now <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      _ <- ZIO.foreachDiscard(trades)(_.forfeit(user.userId, now))
      _ <- heroDao.deleteHero(user.userId)
      _ <- api.sendMessage(user, StateHandler.RestartDone, List.empty, None)
      _ <- makeAction(user, UserAction("", None), renderer)
    } yield ()

  /** Глобальная команда `/home` (см. ARCHITECTURE.md §10) — аварийный выход в
    * город из ЛЮБОГО состояния, минуя обработчик текущей сцены (на случай если
    * игрок застрял из-за возможного бага). Работает поверх обычного диспетчера:
    * durable-статы, инвентарь и исход боя не трогаются — герой не умирает, не
    * получает и не теряет ничего. Сбрасывается только эфемерный слой (активный
    * бой, `scene_data`), чтобы не тащить за собой зависшую/битую сцену в город. */
  private def goHome(user: User, renderer: Renderer): Task[Unit] =
    for {
      _ <- heroDao.clearActiveBattle(user.userId)
      _ <- heroDao.writeSceneData(user.userId, Json.Null)
      target <- ZIO
        .fromOption(states.get(StateType.GlobalMap))
        .orElseFail(new Throwable("GlobalMap state is not registered in StatesMap"))
      _ <- target.enter(user, renderer)
      _ <- heroRepo.updateState(user.userId, StateType.GlobalMap)
    } yield ()

  /** /home и подтверждённый /restart уходят всем «Админ 1», кроме самого
    * игрока: группа пересылает его сообщение с подписью `caption` — кто и из
    * какой сцены (см. HomeAlert). Сама команда /restart, без подтверждения, не
    * пересылается: админу нужны те, кто и правда стёр героя. Игрок к этому
    * моменту уже получил свой ответ, и сбой пересылки его не касается.
    * `command` — для строки в логе. */
  private def toAdmins(user: User, command: String, caption: String, messageId: Option[Long]): Task[Unit] =
    admin.homeWatchers(user.userId)
      .flatMap(ZIO.foreachDiscard(_)(to =>
        // Один недоступный админ не должен оставить без письма остальных.
        api.forwardToUser(to, caption, messageId)
          .catchAll(err => ZIO.logWarning(s"$command не переслан админу ${to.value}: ${err.getMessage}"))))
      .catchAll(err => ZIO.logWarning(s"$command не переслан админам: ${err.getMessage}"))

  /** Performs a state transition (enter + persist) and follows any
    * `autoAdvance` chain so effect nodes route onward without a player action.
    * The fuel guard stops a misconfigured cycle of auto-advancing states.
    *
    * `enter` идёт ДО записи состояния: он готовит данные экрана (напр.
    * `FoundItemState` генерирует находку и кладёт её в `scene_data`). Если бы
    * состояние писалось первым, падение `enter` оставляло бы героя в новом
    * состоянии без его данных — и следующее нажатие («Забрать») валилось бы на
    * чтении `scene_data`, пока игрок не пропишет /home.
    */
  private def transitionTo(
      user: User,
      from: StateType,
      to: StateType,
      renderer: Renderer,
      fuel: Int = 16
  ): Task[Unit] =
    (for {
      target <- ZIO
        .fromOption(states.get(to))
        .orElseFail(
          new Throwable(s"Not found state '$to' for user ${user.userId}")
        )
      _ <- target.enter(user, renderer)
      _ <- heroRepo.updateState(user.userId, to)
      _ <- target.autoAdvance match {
        case Some(next) if fuel > 0 =>
          transitionTo(user, to, next, renderer, fuel - 1)
        case _ => ZIO.unit
      }
    } yield ()).unless(to == from).unit
}

object StateHandler {

  /** Исход исполнения отложенной задачи. */
  sealed trait RunResult
  object RunResult {

    /** Действие применено (состояние совпало с ожидаемым). */
    case object Executed extends RunResult

    /** Задача неактуальна (нет игрока/героя или состояние сменилось) — закрыть
      * как Done.
      */
    case object Skipped extends RunResult
  }

  /** `/home` — глобальная команда, а не кнопка: срабатывает только на «голый»
    * текст без payload (иначе кнопка с совпадающей по случайности подписью
    * тоже считалась бы командой). Регистр и пробелы по краям не важны. */
  private def isHomeCommand(action: UserAction): Boolean =
    action.payload.isEmpty && action.text.trim.equalsIgnoreCase("/home")

  /** `/restart` — по тем же правилам, что и `/home`: голый текст, без payload. */
  private def isRestartCommand(action: UserAction): Boolean =
    action.payload.isEmpty && action.text.trim.equalsIgnoreCase("/restart")

  /** Подтверждение перезапуска — единственная кнопка, которую диспетчер ловит
    * поверх состояний. Её id больше нигде не используется, так что чужую
    * кнопку с ним не спутать. */
  private def isRestartConfirm(action: UserAction): Boolean =
    action.payload.exists(p =>
      io.circe.jawn.decode[Map[String, String]](p).toOption
        .flatMap(_.get("action")).contains(RestartConfirmId))

  val RestartConfirmId: String = "RestartConfirm"

  val RestartWarning: String =
    "⚠️ Это сотрёт героя без возможности вернуть: уровень, опыт, серебро, дублоны, " +
    "снаряжение, сумку, бочку, куб Азата, репутацию и всё, что вы узнали о мире. " +
    "Игра начнётся с самого начала.\n\nЕсли передумали — просто продолжайте играть."

  val RestartDone: String = "💀 Прошлое стёрто. Начинаем заново."

  /** Ответы на «Передать» из беседы: короткие и в личку, чтобы не шуметь в чате. */
  val TransferNotInCity: String =
    "Передавать вещи и серебро можно только в городе — не из лабиринта, не посреди задания и не из меню «Персонаж». Вернитесь в город и повторите.\n\n" +
    "Серебро из ячейки, вещи из хранилища Торгового дома и с полок дома передаст откуда угодно личный посыльный — его можно нанять у Юноны в «Обустройстве»."

  val TransferNoTarget: String =
    "Не понял, кому передавать. Ответьте на сообщение игрока (или перешлите его) и напишите «Передать …»."

  val TransferSomeone: String = "искатель"

  val live: ZLayer[
    Api with StatesMap with HeroRepository with UserRepository with HeroDao with Payouts with Parcels
      with Transfers with AdminPanel with SceneContent with ActivityContest with Referrals with PromoCodes
      with PlayerLock with Trades,
    Nothing,
    StateHandler
  ] =
    ZLayer.fromZIO(
      for {
        api       <- ZIO.service[Api]
        userRepo  <- ZIO.service[UserRepository]
        heroRepo  <- ZIO.service[HeroRepository]
        heroDao   <- ZIO.service[HeroDao]
        payouts   <- ZIO.service[Payouts]
        parcels   <- ZIO.service[Parcels]
        transfers <- ZIO.service[Transfers]
        statesMap <- ZIO.service[StatesMap]
        admin     <- ZIO.service[AdminPanel]
        content   <- ZIO.service[SceneContent]
        activity  <- ZIO.service[ActivityContest]
        referrals <- ZIO.service[Referrals]
        promo     <- ZIO.service[PromoCodes]
        // Тот же лок, что у доставки посылок (см. Transfers): ход игрока и
        // посылка ему в ячейку не должны идти одновременно.
        lock      <- ZIO.service[PlayerLock]
        trades    <- ZIO.service[Trades]
      } yield new StateHandler(api, userRepo, heroRepo, heroDao, payouts, parcels, transfers, statesMap.states,
                               admin, content, lock, Some(activity), Some(referrals), Some(promo), Some(trades))
    )
}
