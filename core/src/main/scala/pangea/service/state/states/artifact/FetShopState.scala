package pangea.service.state.states.artifact

import io.circe.{Json, jawn}
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Journal, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.ItemGenerator
import pangea.model.GameEvent
import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.hero.{FetPotion, Hero}
import pangea.model.item.{BrewKind, Bundling, Grip, Item, ItemType, MaterialKind, QuestItemKind, Rarity}
import pangea.model.quest.NpcQuest
import pangea.model.rune.{RuneStone, RuneStoneSize}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.{AzatData, CityExit, HedgeSongQuest, NpcQuestDialog, QuestSupport, StackMove, State, StrayCasketQuest, UserAction}
import pangea.util.TimeLeft
import zio.{Random, Task, ZIO}

/** Лавка Фета в Торговом доме: сборные артефакты Азата. Каждый стоит
 *  [[HeroArtifacts.StepPriceDoubloons]] дублонов, и за ту же цену Фет трижды
 *  его улучшит — каждая ступень добавляет мест внутри; Живую сумку тому, кто
 *  принёс «Живую кожу», ларец тому, кто принёс бродячий ларец, и шкаф тому,
 *  кто принёс ветвь живой изгороди, — и в четвёртый раз (см.
 *  [[HeroArtifacts.maxTier]]). С краю прилавка — три
 *  зелья ([[FetPotion]]) по [[FetShopState.PotionPriceDoubloons]] дублонов: их
 *  выпивают сразу, при покупке. Рядом — Зелье исчезновения: склянка за
 *  [[FetShopState.VanishPriceDoubloons]] прячет героя из рейтингов на
 *  [[FetShopState.VanishDays]] дней, склянок можно взять сколько угодно разом, а
 *  туман развеять раньше срока — даром. И два купона — каждый своей сценой: на смену
 *  расы за [[FetShopState.CouponPriceDoubloons]] (см. RaceQuestState) и на смену
 *  имени за [[FetShopState.NameCouponPriceDoubloons]] (см. NameChangeState). Оба
 *  ложатся в сумку, а в ход идут с карточки предмета. И беспроигрышная лотерея:
 *  [[FetShopState.LotteryRunes]] большие руны случайных узоров за
 *  [[FetShopState.LotteryPriceDoubloons]] — каждая игра пишется в журнал, по нему
 *  админ-панель считает, сколько раз в неё сыграли. Первым рядом прилавка —
 *  Набор успешного искателя за [[FetShopState.PackPriceDoubloons]]: он один на
 *  игрока, покупка пишется в журнал, и купившему кнопки больше нет. */
case class FetShopState(
  heroDao:       HeroDao,
  artifacts:     ArtifactRepository,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  // Купоны и руны лотереи — новые предметы в сумке: без хранилища вещей их не
  // продать (у каждой вещи должен быть свой номер из базы).
  itemRepo:      Option[ItemRepository] = None,
  // Журнал событий: в нём учёт сыгранных лотерей для админ-панели.
  journal:       Option[Journal] = None
) extends State {

  /** «Живая кожа»: кусок с Гнилого Джо — из такого Фет доращивает сумку. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.Fet, "Fet")

  /** «Бродячий ларец»: развязка — здесь, у прилавка; от разговора о задании
    * нужны только закрытие и опыт. */
  private val stray = NpcQuestDialog(heroDao, content, NpcQuest.StrayCasket, "Stray")

  /** «Тот, кто уговаривает деревья»: ветвь от Элиара — тоже сюда. */
  private val hedge = NpcQuestDialog(heroDao, content, NpcQuest.HedgeSong, "Hedge")

  private val branch = new Branch(
    routes = Map(
      "FetShop"     -> Target.Run { (u, _, r)  => showShop(u, r).as(StateType.FetShop) },
      quest.questAction   -> Target.Run { (u, _, r) => questTalk(u, r) },
      quest.acceptAction  -> Target.Run { (u, _, r) => quest.accept(u, r) *> showShop(u, r).as(StateType.FetShop) },
      quest.declineAction -> Target.Run { (u, _, r) => showShop(u, r).as(StateType.FetShop) },
      "FetStray"    -> Target.Run { (u, _, r)  => strayTurnIn(u, r).as(StateType.FetShop) },
      "FetBranch"   -> Target.Run { (u, _, r)  => branchTurnIn(u, r).as(StateType.FetShop) },
      "FetGoods"    -> Target.Run { (u, ua, r) => withKind(ua)(showGoods(u, _, r)).as(StateType.FetShop) },
      "FetBuy"      -> Target.Run { (u, ua, r) => withKind(ua)(confirm(u, _, r)).as(StateType.FetShop) },
      "FetBuyYes"   -> Target.Run { (u, ua, r) => withKind(ua)(buy(u, _, r)).as(StateType.FetShop) },
      "FetPotion"     -> Target.Run { (u, ua, r) => withPotion(ua)(showPotion(u, _, r)).as(StateType.FetShop) },
      "FetPotionBuy"  -> Target.Run { (u, ua, r) => withPotion(ua)(confirmPotion(u, _, r)).as(StateType.FetShop) },
      "FetPotionYes"  -> Target.Run { (u, ua, r) => withPotion(ua)(drinkPotion(u, _, r)).as(StateType.FetShop) },
      // Зелье исчезновения: склянок сколько угодно разом — числом в чате.
      "FetVanish"        -> Target.Run { (u, _, r)  => showVanish(u, r).as(StateType.FetShop) },
      "FetVanishBuy"     -> Target.Run { (u, _, r)  => confirmVanish(u, 1, r).as(StateType.FetShop) },
      "FetVanishYes"     -> Target.Run { (u, ua, r) => drinkVanish(u, payloadCount(ua), r).as(StateType.FetShop) },
      "FetVanishLift"    -> Target.Run { (u, _, r)  => confirmLift(u, r).as(StateType.FetShop) },
      "FetVanishLiftYes" -> Target.Run { (u, _, r)  => liftVanish(u, r).as(StateType.FetShop) },
      "Text"             -> Target.Run { (u, ua, r) => onText(u, ua, r).as(StateType.FetShop) },
      "FetCoupon"     -> Target.Run { (u, _, r)  => showCoupon(u, r).as(StateType.FetShop) },
      "FetCouponBuy"  -> Target.Run { (u, _, r)  => confirmCoupon(u, r).as(StateType.FetShop) },
      "FetCouponYes"  -> Target.Run { (u, _, r)  => buyCoupon(u, r).as(StateType.FetShop) },
      "FetNameCoupon"    -> Target.Run { (u, _, r) => showNameCoupon(u, r).as(StateType.FetShop) },
      "FetNameCouponBuy" -> Target.Run { (u, _, r) => confirmNameCoupon(u, r).as(StateType.FetShop) },
      "FetNameCouponYes" -> Target.Run { (u, _, r) => buyNameCoupon(u, r).as(StateType.FetShop) },
      // Набор успешного искателя: купил — кнопки нет, и нажатия ведут к прилавку.
      "FetPack"     -> Target.Run { (u, _, r)  => showPack(u, r).as(StateType.FetShop) },
      "FetPackBuy"  -> Target.Run { (u, _, r)  => confirmPack(u, r).as(StateType.FetShop) },
      "FetPackYes"  -> Target.Run { (u, _, r)  => buyPack(u, r).as(StateType.FetShop) },
      "FetRunes"    -> Target.Run { (u, _, r)  => showLottery(u, r).as(StateType.FetShop) },
      "FetRunesBuy" -> Target.Run { (u, _, r)  => playLottery(u, 1, r).as(StateType.FetShop) },
      // «Купить до лимита» — все игры, что остались на сегодня. Кнопка «×10» из
      // старых сообщений делает то же самое: лимит меньше десяти.
      "FetRunesAll" -> Target.Run { (u, _, r)  => playLottery(u, FetShopState.LotteryDailyLimit.toInt, r).as(StateType.FetShop) },
      "FetRunesTen" -> Target.Run { (u, _, r)  => playLottery(u, FetShopState.LotteryDailyLimit.toInt, r).as(StateType.FetShop) },
      // Кнопка «Договорились» прежнего подтверждения в старых сообщениях — одна игра.
      "FetRunesYes" -> Target.Run { (u, _, r)  => playLottery(u, 1, r).as(StateType.FetShop) },
      "LeaveFetShop" -> Target.Goto(StateType.TradeHouse),
      CityExit.route
    ),
    fallback = Target.Run { (u, _, r) => showShop(u, r).as(StateType.FetShop) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.text("fet.intro"), Nil)) *> showShop(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showShop(user: User, renderer: Renderer): Task[Unit] =
    quest.load(user).zip(packOffered(user)).flatMap { case (quests, pack) =>
      val goods = ArtifactKind.values.toList.zipWithIndex.map { case (k, i) =>
        Choice("FetGoods", content.text(s"artifact.${k.key}.title"), data = Map("kind" -> k.entryName), row = Some(i))
      }
      // Зелья — по два в ряд, Зелье исчезновения последним: рядов у ВК десять,
      // и все они уже заняты.
      val potionButtons = FetPotion.values.toList.map { p =>
        Choice("FetPotion", content.text(s"fet.potions.${p.key}.title"), data = Map("potion" -> p.entryName))
      } :+ Choice("FetVanish", content.text("fet.vanish.title"))
      val potions = potionButtons.zipWithIndex.map { case (c, i) =>
        c.copy(row = Some(ArtifactKind.values.size + i / FetShopState.PotionsPerRow))
      }
      val potionRows = (potionButtons.size + FetShopState.PotionsPerRow - 1) / FetShopState.PotionsPerRow
      // Лотерея — своим рядом под зельями. Рядов теперь до десяти — столько,
      // сколько держит клавиатура ВК: новому товару — в общий ряд.
      val lotteryRow = ArtifactKind.values.size + potionRows
      val lottery    = if (itemRepo.isEmpty) Nil
                       else List(Choice("FetRunes", content.text("fet.runes.title"), row = Some(lotteryRow)))
      // Оба купона — в одном ряду: у ВК не больше десяти рядов на клавиатуре.
      val couponRow = lotteryRow + (if (lottery.isEmpty) 0 else 1)
      val coupons   = if (itemRepo.isEmpty) Nil
                      else List(
                        Choice("FetCoupon", content.text("fet.coupon.title"), row = Some(couponRow)),
                        Choice("FetNameCoupon", content.text("fet.nameCoupon.title"), row = Some(couponRow)))
      val questRow = couponRow + (if (coupons.isEmpty) 0 else 1)
      // Задание Фета, бродячий ларец и ветвь от Элиара — в одном ряду: рядов у
      // ВК мало.
      val strayBtn  = Option.when(StrayCasketQuest.toFet(quests))(
                        content.choice("FetStray", "strayCasket.fet.showLabel"))
      val branchBtn = Option.when(HedgeSongQuest.toFet(quests))(
                        content.choice("FetBranch", "hedgeSong.fet.showLabel"))
      val talk      = (quest.button(quests).toList ++ strayBtn.toList ++ branchBtn.toList)
                        .map(_.copy(row = Some(questRow)))
      val leaveRow = questRow + (if (talk.isEmpty) 0 else 1)
      // У прилавка число в чате уже ничего не значит: карточка Зелья
      // исчезновения закрыта.
      val counter = goods ++ potions ++ lottery ++ coupons ++ talk ++ List(
        Choice("LeaveFetShop", content.text("fet.leave"), color = ChoiceColor.Negative, row = Some(leaveRow)),
        CityExit.button(content, Some(leaveRow)))
      // Набор успешного искателя — первым рядом, пока игрок его не купил:
      // остальные ряды съезжают на один. Купил — кнопки больше нет совсем.
      val buttons =
        if (!pack) counter
        else Choice("FetPack", content.text("fet.pack.title"), color = ChoiceColor.Positive, row = Some(0)) ::
          counter.map(c => c.copy(row = c.row.map(_ + 1)))
      heroDao.writeSceneData(user.userId, Json.Null) *>
        renderer.show(user, Screen(content.text("fet.counter"), buttons))
    }

  // ── «Живая кожа» ──────────────────────────────────────────────

  private def questTalk(user: User, renderer: Renderer): Task[StateType] =
    quest.load(user).flatMap { quests =>
      if (quests.isDone(NpcQuest.Fet)) showShop(user, renderer)
      else if (!quests.isTaken(NpcQuest.Fet)) quest.offer(user, renderer, quest.text("intro"))
      else turnIn(user, renderer)
    }.as(StateType.FetShop)

  /** Сдача: кожа уходит Фету, герой получает золото, опыт и Живую сумку: своя
    * есть — ступень сверху, нет — Фет выдаёт новую (как ларец и шкаф в их
    * заданиях). Потолок сумки у героя теперь [[HeroArtifacts.QuestMaxTier]]. */
  private def turnIn(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      _ <- inv.items.data.find(FetShopState.ghoulSkin) match {
        case None => renderer.show(user, Screen(quest.text("step1Fail"), Nil)) *> showShop(user, renderer)
        case Some(skin) =>
          for {
            _    <- inventoryRepo.removeItem(skin.id, hero.id).mapError(e => new Throwable(e.toString))
            _    <- heroDao.updateDoubloons(user.userId, hero.doubloons + FetShopState.QuestDoubloons)
            done <- quest.complete(user, hero, identity)
            (_, expLine) = done
            before <- artifacts.get(hero.id).mapError(asThrowable).map(_.bag)
            // Новая сумка приходит первой ступенью и с полными зарядами, своя —
            // растёт на ступень (см. ArtifactRepository.upgrade).
            grown <- artifacts.upgrade(hero.id, ArtifactKind.LivingBag, HeroArtifacts.QuestMaxTier).mapError(asThrowable)
            bagLine = quest.format(if (before.owned) "bagGrown" else "bagNew",
                        "tier" -> grown.tier.toString, "maxTier" -> HeroArtifacts.QuestMaxTier.toString,
                        "capacity" -> grown.capacity.toString)
            _    <- renderer.show(user, Screen(quest.text("outro"), Nil))
            _    <- renderer.show(user, Screen(quest.format("reward",
                      "doubloons" -> FetShopState.QuestDoubloons.toString, "exp" -> expLine) + "\n\n" + bagLine, Nil))
            _    <- showShop(user, renderer)
          } yield ()
      }
    } yield ()

  // ── «Бродячий ларец» ──────────────────────────────────────────

  /** Развязка: Фет узнаёт ларец своего друга и оставляет его себе на память, а
    * герою взамен — новый Ларец Азата, если своего нет, или ступень к своему;
    * потолок ларца у героя теперь [[HeroArtifacts.QuestMaxTier]]. Камни, что
    * съел бродячий ларец, Фет сразу кладёт в ларец героя (не влезли — в
    * сумку). Задание закрыто, опыт — как за задание горожанина. */
  private def strayTurnIn(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      quests <- stray.load(user)
      _      <- if (!StrayCasketQuest.toFet(quests)) showShop(user, renderer)
                else for {
                  before <- artifacts.get(hero.id).mapError(asThrowable).map(_.casket)
                  _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StrayCasket))
                  done   <- stray.complete(user, hero, _.copy(strayGems = Nil))
                  (_, expLine) = done
                  grown  <- artifacts.upgrade(hero.id, ArtifactKind.Casket, HeroArtifacts.QuestMaxTier).option
                  placed <- returnGems(hero, StrayCasketQuest.returned(quests))
                  (inCasket, inBag) = placed
                  casketLine = grown.fold("")(g => content.format(
                                 if (before.owned) "strayCasket.fet.casketGrown" else "strayCasket.fet.newCasket",
                                 "title" -> content.text("artifact.casket.title"), "tier" -> g.tier.toString,
                                 "maxTier" -> HeroArtifacts.QuestMaxTier.toString, "capacity" -> g.capacity.toString))
                  gemsLine = List(
                               Option.when(inCasket.nonEmpty)(content.format("strayCasket.fet.gems",
                                 "gems" -> StackMove.names(inCasket))),
                               Option.when(inBag.nonEmpty)(content.format("strayCasket.fet.gemsToBag",
                                 "gems" -> StackMove.names(inBag)))).flatten
                  _      <- renderer.show(user, Screen(content.text("strayCasket.fet.outro"), Nil))
                  _      <- renderer.show(user, Screen((List(casketLine).filter(_.nonEmpty) ++ gemsLine :+ expLine)
                              .mkString("\n\n"), Nil))
                  _      <- renderer.show(user, Screen(content.text("strayCasket.questDone"), Nil))
                  _      <- showShop(user, renderer)
                } yield ()
    } yield ()

  // ── «Тот, кто уговаривает деревья» ────────────────────────────

  /** Развязка: ветвь живой изгороди — Фету; герою — новый Миниатюрный шкаф,
    * если своего нет, или ступень к своему; потолок шкафа у героя теперь
    * [[HeroArtifacts.QuestMaxTier]]. Задание закрыто, опыт — как за задание
    * горожанина. */
  private def branchTurnIn(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      quests <- hedge.load(user)
      _      <- if (!HedgeSongQuest.toFet(quests)) showShop(user, renderer)
                else for {
                  before <- artifacts.get(hero.id).mapError(asThrowable).map(_.wardrobe)
                  _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.HedgeBranch))
                  done   <- hedge.complete(user, hero, identity)
                  (_, expLine) = done
                  grown  <- artifacts.upgrade(hero.id, ArtifactKind.Wardrobe, HeroArtifacts.QuestMaxTier).option
                  line    = grown.fold("")(g => content.format(
                              if (before.owned) "hedgeSong.fet.wardrobeGrown" else "hedgeSong.fet.newWardrobe",
                              "title" -> content.text("artifact.wardrobe.title"), "tier" -> g.tier.toString,
                              "maxTier" -> HeroArtifacts.QuestMaxTier.toString, "capacity" -> g.capacity.toString))
                  _      <- renderer.show(user, Screen(content.text("hedgeSong.fet.outro"), Nil))
                  _      <- renderer.show(user, Screen((List(line).filter(_.nonEmpty) :+ expLine).mkString("\n\n"), Nil))
                  _      <- renderer.show(user, Screen(content.text("hedgeSong.questDone"), Nil))
                  _      <- showShop(user, renderer)
                } yield ()
    } yield ()

  /** Съеденные камни — снова вещами: в ларец героя, а что не влезло — в сумку.
    * Возвращает, что легло в ларец и что в сумку. Место в ларце есть всегда:
    * ступень сверху добавляет пятнадцать мест, а камней не больше десяти, —
    * сумка тут на всякий случай. Без хранилища вещей камни не создать — тогда
    * ничего. */
  private def returnGems(hero: Hero, gems: List[Item]): Task[(List[Item], List[Item])] =
    itemRepo match {
      case None => ZIO.succeed((Nil, Nil))
      case Some(items) =>
        ZIO.foldLeft(gems)((List.empty[Item], List.empty[Item])) { case ((inCasket, inBag), gem) =>
          items.persist(hero.id, gem).flatMap { saved =>
            artifacts.put(hero.id, ArtifactKind.Casket, saved).either.flatMap {
              case Right(_) => ZIO.succeed((inCasket :+ saved, inBag))
              case Left(_)  => inventoryRepo.addItem(hero.id, saved).either.map {
                case Right(_) => (inCasket, inBag :+ saved)
                case Left(_)  => (inCasket, inBag)
              }
            }
          }
        }
    }

  /** Карточка товара: рассказ Фета, цена и что сейчас с артефактом у героя. */
  private def showGoods(user: User, kind: ArtifactKind, renderer: Renderer): Task[Unit] =
    mine(user, kind).zip(topTier(user, kind)).flatMap { case (a, top) =>
      val state =
        if (!a.owned) content.format("fet.stateNone", "slots" -> kind.slotsPerTier.toString)
        else if (a.canUpgrade(top)) content.format("fet.stateOwned",
          "tier" -> a.tier.toString, "maxTier" -> top.toString,
          "capacity" -> a.capacity.toString, "next" -> (a.capacity + kind.slotsPerTier).toString)
        else content.format("fet.stateFull", "capacity" -> a.capacity.toString)
      val buyLabel =
        if (!a.owned) content.format("fet.buyLabel", "price" -> HeroArtifacts.StepPriceDoubloons.toString)
        else content.format("fet.upgradeLabel", "price" -> HeroArtifacts.StepPriceDoubloons.toString)
      val buttons =
        Option.when(a.canUpgrade(top))(Choice("FetBuy", Choice.fit(buyLabel),
          color = ChoiceColor.Positive, data = Map("kind" -> kind.entryName), row = Some(0))).toList :+
          Choice("FetShop", content.text("fet.back"), color = ChoiceColor.Negative, row = Some(1))
      renderer.show(user, Screen(content.text(s"artifact.${kind.key}.pitch") + "\n\n" + state, buttons))
    }

  private def confirm(user: User, kind: ArtifactKind, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      a    <- mine(user, kind)
      top  <- topTier(user, kind)
      price = HeroArtifacts.StepPriceDoubloons
      _ <- if (!a.canUpgrade(top)) showGoods(user, kind, renderer)
           else if (hero.doubloons < price)
             renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
           else
             renderer.show(user, Screen(
               content.format(if (a.owned) "fet.confirmUpgrade" else "fet.confirmBuy",
                 "title" -> content.text(s"artifact.${kind.key}.title"), "price" -> price.toString),
               List(
                 Choice("FetBuyYes", content.text("fet.confirmYes"), color = ChoiceColor.Positive,
                   data = Map("kind" -> kind.entryName), row = Some(0)),
                 Choice("FetShop", content.text("fet.confirmNo"), color = ChoiceColor.Negative, row = Some(0))
               ), inline = true))
    } yield ()

  private def buy(user: User, kind: ArtifactKind, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      a    <- mine(user, kind)
      top  <- topTier(user, kind)
      price = HeroArtifacts.StepPriceDoubloons
      _ <- if (!a.canUpgrade(top)) showGoods(user, kind, renderer)
           else if (hero.doubloons < price)
             renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
           else
             heroDao.updateDoubloons(user.userId, hero.doubloons - price) *>
               artifacts.upgrade(hero.id, kind, top).mapError(asThrowable).flatMap { grown =>
                 renderer.show(user, Screen(content.format(if (a.owned) "fet.upgraded" else "fet.bought",
                   "title" -> content.text(s"artifact.${kind.key}.title"),
                   "tier" -> grown.tier.toString, "maxTier" -> top.toString,
                   "capacity" -> grown.capacity.toString), Nil))
               } *> showGoods(user, kind, renderer)
    } yield ()

  // ── Зелья ─────────────────────────────────────────────────────────────────

  /** Карточка зелья: рассказ Фета и что оно сделает с героем сейчас. Толку нет
    * (опыт и так стоит, сбрасывать нечего) — так и сказано, кнопки покупки нет. */
  private def showPotion(user: User, potion: FetPotion, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      val price  = FetShopState.PotionPriceDoubloons
      val useful = FetPotion.useful(potion, hero)
      val state  =
        if (!useful) content.text(s"fet.potions.${potion.key}.useless")
        else if (potion == FetPotion.Respec)
          content.format("fet.potions.respec.state", "points" -> Hero.spentPoints(hero).toString)
        else ""
      val buttons =
        Option.when(useful)(Choice("FetPotionBuy",
          Choice.fit(content.format("fet.potionBuyLabel", "price" -> price.toString)),
          color = ChoiceColor.Positive, data = Map("potion" -> potion.entryName), row = Some(0))).toList :+
          Choice("FetShop", content.text("fet.back"), color = ChoiceColor.Negative, row = Some(1))
      renderer.show(user, Screen(content.text(s"fet.potions.${potion.key}.pitch") +
        (if (state.isEmpty) "" else "\n\n" + state), buttons))
    }

  private def confirmPotion(user: User, potion: FetPotion, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      val price = FetShopState.PotionPriceDoubloons
      if (!FetPotion.useful(potion, hero)) showPotion(user, potion, renderer)
      else if (hero.doubloons < price)
        renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
      else
        renderer.show(user, Screen(
          content.format("fet.confirmPotion",
            "title" -> content.text(s"fet.potions.${potion.key}.title"), "price" -> price.toString),
          List(
            Choice("FetPotionYes", content.text("fet.confirmYes"), color = ChoiceColor.Positive,
              data = Map("potion" -> potion.entryName), row = Some(0)),
            Choice("FetShop", content.text("fet.confirmNo"), color = ChoiceColor.Negative, row = Some(0))
          ), inline = true))
    }

  /** Выпить зелье: дублоны — Фету, действие — сразу. Толку уже нет (старая
    * кнопка) или дублонов не хватает — ничего не тратим. */
  private def drinkPotion(user: User, potion: FetPotion, renderer: Renderer): Task[Unit] =
    for {
      now  <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      price = FetShopState.PotionPriceDoubloons
      _ <- if (!FetPotion.useful(potion, hero)) showPotion(user, potion, renderer)
           else if (hero.doubloons < price)
             renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
           else
             heroDao.updateDoubloons(user.userId, hero.doubloons - price) *>
               (potion match {
                 case FetPotion.ExpStop  => heroDao.updateExpFrozen(user.userId, frozen = true)
                 case FetPotion.ExpStart => heroDao.updateExpFrozen(user.userId, frozen = false)
                 case FetPotion.Respec   => respec(user, hero, now)
               }) *>
               renderer.show(user, Screen(content.format(s"fet.potions.${potion.key}.done",
                 "points" -> Hero.spentPoints(hero).toString), Nil)) *>
               showShop(user, renderer)
    } yield ()

  /** Вложенные очки — в свободные, характеристики — к старту (см. Hero.respec).
    * Живучесть и интеллект могли упасть: здоровье и энергия выше нового потолка
    * срезаются до него. */
  private def respec(user: User, hero: Hero, now: Long): Task[Unit] = {
    val reset  = Hero.respec(hero)
    val maxHp  = reset.effectiveMaxHp(now)
    val maxEn  = reset.maxEnergy(now)
    val capped = hero.fightStats.copy(hp = hero.fightStats.hp.min(maxHp), energy = hero.fightStats.energy.min(maxEn))
    heroDao.updateBaseStats(user.userId, reset.baseStats) *>
      heroDao.updateExpAndLevel(user.userId, reset.exp, reset.lvl, reset.upgradePoints) *>
      ZIO.when(capped != hero.fightStats)(heroDao.updateFightStats(user.userId, capped)).unit
  }

  // ── Зелье исчезновения ────────────────────────────────────────────────────

  /** Карточка: рассказ Фета, сколько ещё держится туман и «Развеять», пока он
    * держится. Пока карточка открыта, число в чате — сколько склянок взять
    * (см. [[onText]]). */
  private def showVanish(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      until <- heroDao.readTopHiddenUntil(user.userId)
      left   = until.filter(_ > now).map(u => TimeLeft.format(u - now))
      _     <- heroDao.writeSceneData(user.userId, Json.obj(FetShopState.VanishKey -> Json.True))
      text   = content.format("fet.vanish.pitch", "days" -> FetShopState.VanishDays.toString) +
                 left.fold("")(l => "\n\n" + content.format("fet.vanish.state", "left" -> l))
      buttons = List(
                  Some(Choice("FetVanishBuy",
                    Choice.fit(content.format("fet.potionBuyLabel", "price" -> FetShopState.VanishPriceDoubloons.toString)),
                    color = ChoiceColor.Positive, row = Some(0))),
                  left.map(_ => Choice("FetVanishLift", content.text("fet.vanish.liftLabel"), row = Some(0))),
                  Some(Choice("FetShop", content.text("fet.back"), color = ChoiceColor.Negative, row = Some(1)))
                ).flatten
      _     <- renderer.show(user, Screen(text, buttons))
    } yield ()

  /** Сколько склянок взять — числом в чате, пока открыта карточка зелья. В
    * другое время число у прилавка ничего не значит — снова прилавок. */
  private def onText(user: User, ua: UserAction, renderer: Renderer): Task[Unit] =
    heroDao.readSceneData(user.userId).flatMap { scene =>
      val onCard = scene.flatMap(_.hcursor.get[Boolean](FetShopState.VanishKey).toOption).contains(true)
      if (!onCard) showShop(user, renderer)
      else ua.text.trim match {
        // Пустое (картинка, стикер, перезапуск сцены) — снова карточка.
        case "" => showVanish(user, renderer)
        case t  => t.toIntOption.filter(_ >= 1) match {
          case Some(n) => confirmVanish(user, n, renderer)
          case None    => renderer.show(user, Screen(content.text("fet.vanish.badCount"), Nil))
        }
      }
    }

  /** «Выпить столько-то за столько-то? Туман продержится …» — срок с учётом
    * остатка. Дублонов не хватает — Фет так и говорит, клавиатура карточки
    * остаётся: можно написать число поменьше. */
  private def confirmVanish(user: User, n: Int, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      hero  <- getHero(user)
      until <- heroDao.readTopHiddenUntil(user.userId)
      price  = FetShopState.VanishPriceDoubloons * n
      left   = TimeLeft.format(FetShopState.vanishUntil(until, now, n) - now)
      title  = content.text("fet.vanish.title")
      _ <- if (hero.doubloons < price)
             renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), Nil))
           else
             renderer.show(user, Screen(
               if (n == 1) content.format("fet.vanish.confirmOne", "title" -> title, "price" -> price.toString, "left" -> left)
               else content.format("fet.vanish.confirmMany", "title" -> title, "count" -> n.toString,
                      "price" -> price.toString, "left" -> left),
               List(
                 Choice("FetVanishYes", content.text("fet.confirmYes"), color = ChoiceColor.Positive,
                   data = Map("n" -> n.toString), row = Some(0)),
                 Choice("FetVanish", content.text("fet.confirmNo"), color = ChoiceColor.Negative, row = Some(0))
               ), inline = true))
    } yield ()

  /** Выпить `n` склянок: дублоны — Фету, к сроку тумана — по неделе за каждую.
    * Дублонов уже не хватает (потратил, пока думал) — ничего не тратим. */
  private def drinkVanish(user: User, n: Int, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      hero  <- getHero(user)
      until <- heroDao.readTopHiddenUntil(user.userId)
      price  = FetShopState.VanishPriceDoubloons * n
      _ <- if (hero.doubloons < price)
             renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
           else {
             val after = FetShopState.vanishUntil(until, now, n)
             heroDao.updateDoubloons(user.userId, hero.doubloons - price) *>
               heroDao.updateTopHiddenUntil(user.userId, Some(after)) *>
               renderer.show(user, Screen(content.format("fet.vanish.done",
                 "count" -> n.toString, "left" -> TimeLeft.format(after - now)), Nil)) *>
               showShop(user, renderer)
           }
    } yield ()

  /** «Развеять туман?» — бесплатно, но выпитое Фет не вернёт. Тумана уже нет —
    * снова карточка. */
  private def confirmLift(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      until <- heroDao.readTopHiddenUntil(user.userId)
      _ <- until.filter(_ > now) match {
             case None => showVanish(user, renderer)
             case Some(u) =>
               renderer.show(user, Screen(content.format("fet.vanish.liftConfirm", "left" -> TimeLeft.format(u - now)),
                 List(
                   Choice("FetVanishLiftYes", content.text("fet.vanish.liftYes"), color = ChoiceColor.Positive, row = Some(0)),
                   Choice("FetVanish", content.text("fet.confirmNo"), color = ChoiceColor.Negative, row = Some(0))
                 ), inline = true))
           }
    } yield ()

  /** Развеять туман: рейтинги снова видят героя. Денег Фет не берёт. */
  private def liftVanish(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      until <- heroDao.readTopHiddenUntil(user.userId)
      _ <- if (!until.exists(_ > now)) showVanish(user, renderer)
           else heroDao.updateTopHiddenUntil(user.userId, None) *>
             renderer.show(user, Screen(content.text("fet.vanish.lifted"), Nil)) *>
             showShop(user, renderer)
    } yield ()

  /** Сколько склянок обещало подтверждение: его число едет в кнопке. */
  private def payloadCount(ua: UserAction): Int =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption)
      .flatMap(_.get("n")).flatMap(_.toIntOption).filter(_ >= 1).getOrElse(1)

  private def nowMs: Task[Long] =
    ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))

  private def withPotion(ua: UserAction)(f: FetPotion => Task[Unit]): Task[Unit] =
    ua.payload
      .flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("potion")))
      .flatMap(FetPotion.withNameOption) match {
        case Some(potion) => f(potion)
        case None         => ZIO.unit
      }

  // ── Купон на смену расы ───────────────────────────────────────────────────

  private def showCoupon(user: User, renderer: Renderer): Task[Unit] =
    if (itemRepo.isEmpty) showShop(user, renderer)
    else renderer.show(user, Screen(content.text("fet.coupon.pitch"), List(
      Choice("FetCouponBuy", Choice.fit(content.format("fet.buyLabel", "price" -> FetShopState.CouponPriceDoubloons.toString)),
        color = ChoiceColor.Positive, row = Some(0)),
      Choice("FetShop", content.text("fet.back"), color = ChoiceColor.Negative, row = Some(1)))))

  private def confirmCoupon(user: User, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      val price = FetShopState.CouponPriceDoubloons
      if (itemRepo.isEmpty) showShop(user, renderer)
      else if (hero.doubloons < price)
        renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
      else
        renderer.show(user, Screen(content.format("fet.coupon.confirm", "price" -> price.toString), List(
          Choice("FetCouponYes", content.text("fet.confirmYes"), color = ChoiceColor.Positive, row = Some(0)),
          Choice("FetShop", content.text("fet.confirmNo"), color = ChoiceColor.Negative, row = Some(0))
        ), inline = true))
    }

  /** Купить купон: дублоны — Фету, купон — в сумку. Места он не занимает, так
    * что влезает всегда. Дублонов не хватает (старая кнопка) — ничего не тратим. */
  private def buyCoupon(user: User, renderer: Renderer): Task[Unit] =
    itemRepo match {
      case None => showShop(user, renderer)
      case Some(items) =>
        getHero(user).flatMap { hero =>
          val price = FetShopState.CouponPriceDoubloons
          if (hero.doubloons < price)
            renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
          else
            heroDao.updateDoubloons(user.userId, hero.doubloons - price) *>
              items.persist(hero.id, QuestItemKind.item(QuestItemKind.RaceCoupon))
                .flatMap(inventoryRepo.addItem(hero.id, _).mapError(asThrowable)) *>
              renderer.show(user, Screen(content.text("fet.coupon.done"), Nil)) *>
              showShop(user, renderer)
        }
    }

  // ── Купон на смену имени ─────────────────────────────────────────────────

  private def showNameCoupon(user: User, renderer: Renderer): Task[Unit] =
    if (itemRepo.isEmpty) showShop(user, renderer)
    else renderer.show(user, Screen(content.text("fet.nameCoupon.pitch"), List(
      Choice("FetNameCouponBuy", Choice.fit(content.format("fet.buyLabel", "price" -> FetShopState.NameCouponPriceDoubloons.toString)),
        color = ChoiceColor.Positive, row = Some(0)),
      Choice("FetShop", content.text("fet.back"), color = ChoiceColor.Negative, row = Some(1)))))

  private def confirmNameCoupon(user: User, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      val price = FetShopState.NameCouponPriceDoubloons
      if (itemRepo.isEmpty) showShop(user, renderer)
      else if (hero.doubloons < price)
        renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
      else
        renderer.show(user, Screen(content.format("fet.nameCoupon.confirm", "price" -> price.toString), List(
          Choice("FetNameCouponYes", content.text("fet.confirmYes"), color = ChoiceColor.Positive, row = Some(0)),
          Choice("FetShop", content.text("fet.confirmNo"), color = ChoiceColor.Negative, row = Some(0))
        ), inline = true))
    }

  /** Купить купон на смену имени: дублоны — Фету, купон — в сумку. Места он
    * не занимает. Дублонов не хватает (старая кнопка) — ничего не тратим. */
  private def buyNameCoupon(user: User, renderer: Renderer): Task[Unit] =
    itemRepo match {
      case None => showShop(user, renderer)
      case Some(items) =>
        getHero(user).flatMap { hero =>
          val price = FetShopState.NameCouponPriceDoubloons
          if (hero.doubloons < price)
            renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
          else
            heroDao.updateDoubloons(user.userId, hero.doubloons - price) *>
              items.persist(hero.id, QuestItemKind.item(QuestItemKind.NameCoupon))
                .flatMap(inventoryRepo.addItem(hero.id, _).mapError(asThrowable)) *>
              renderer.show(user, Screen(content.text("fet.nameCoupon.done"), Nil)) *>
              showShop(user, renderer)
        }
    }

  // ── Беспроигрышная лотерея ───────────────────────────────────────────────

  private def showLottery(user: User, renderer: Renderer): Task[Unit] =
    if (itemRepo.isEmpty) showShop(user, renderer)
    else playsLeft(user).flatMap { left =>
      val price = FetShopState.LotteryPriceDoubloons
      renderer.show(user, Screen(
        content.format("fet.runes.pitch", "count" -> FetShopState.LotteryRunes.toString, "price" -> price.toString,
          "limit" -> FetShopState.LotteryDailyLimit.toString) + "\n\n" +
          content.format("fet.runes.left", "left" -> left.toString,
            "limit" -> FetShopState.LotteryDailyLimit.toString),
        List(
          Choice("FetRunesBuy", Choice.fit(content.format("fet.runes.buyLabel", "price" -> price.toString)),
            color = ChoiceColor.Positive, row = Some(0)),
          Choice("FetRunesAll", content.text("fet.runes.buyAllLabel"), color = ChoiceColor.Positive, row = Some(0)),
          Choice("FetShop", content.text("fet.back"), color = ChoiceColor.Negative, row = Some(1)))))
    }

  /** Сколько игр у героя осталось на сегодня (сутки — по Москве). Считаем по
    * журналу событий: каждая игра в нём — своя строка. Без журнала лимита нет. */
  private def playsLeft(user: User): Task[Long] =
    for {
      now   <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      today <- journal.fold[Task[Long]](ZIO.succeed(0L))(_.count(user.userId, GameEvent.FetRuneLottery,
                 pangea.service.activity.ActivityContest.dayStart(now)))
    } yield (FetShopState.LotteryDailyLimit - today).max(0L)

  /** Почему сыграть `plays` раз нельзя: дублонов не хватает на все игры или в
    * сумке нет места на все руны разом (большая руна занимает место). `None` —
    * можно. */
  private def lotteryRefusal(hero: Hero, plays: Int): Task[Option[String]] =
    inventoryRepo.get(hero.id).mapError(asThrowable).map { inv =>
      val price = FetShopState.LotteryPriceDoubloons * plays
      val need  = FetShopState.LotteryRunes.toLong * plays
      if (hero.doubloons < price) Some(content.format("fet.notEnough", "price" -> price.toString))
      else if (!inv.hasRoomFor(need))
        Some(content.format("fet.runes.noRoom", "need" -> need.toString, "free" -> inv.freeSlots.toString))
      else None
    }

  /** Сыграть `asked` раз (одна игра или «до лимита») — сразу, без подтверждения.
    * Не больше [[FetShopState.LotteryDailyLimit]] игр за сутки по Москве: осталось
    * меньше, чем просили, — играем сколько осталось; не осталось — отказ. Дублоны
    * — Фету за все игры, руны — в сумку, каждая со своим номером из базы
    * (одинаковые руны без номера делили бы −1, и сумка снимала бы их разом).
    * Узор каждой — случайный из всех, повторы бывают. Каждая игра — своей
    * строкой в журнал: по ним и лимит, и учёт в админ-панели. Дублонов не хватает
    * или места нет — отказ, ничего не тратим. */
  private def playLottery(user: User, asked: Int, renderer: Renderer): Task[Unit] =
    itemRepo match {
      case None => showShop(user, renderer)
      case Some(items) =>
        for {
          hero <- getHero(user)
          left <- playsLeft(user)
          plays = asked.toLong.min(left).toInt
          _    <- if (plays <= 0) renderer.show(user, Screen(content.format("fet.runes.limit",
                    "limit" -> FetShopState.LotteryDailyLimit.toString), backRow))
                  else lotteryRefusal(hero, plays).flatMap {
                    case Some(why) => renderer.show(user, Screen(why, backRow))
                    case None =>
                      for {
                        games <- ZIO.foreach(List.fill(plays)(()))(_ => FetShopState.drawRunes)
                        _     <- heroDao.updateDoubloons(user.userId,
                                   hero.doubloons - FetShopState.LotteryPriceDoubloons * plays)
                        saved <- ZIO.foreach(games)(runes =>
                                   ZIO.foreach(runes)(r => items.persist(hero.id, RuneStone.item(r, RuneStoneSize.Big))))
                        _     <- ZIO.foreachDiscard(saved.flatten)(inventoryRepo.addItem(hero.id, _).mapError(asThrowable))
                        _     <- ZIO.foreachDiscard(saved)(logLottery(user, _))
                        _     <- renderer.show(user, Screen(content.format("fet.runes.done",
                                   "runes" -> FetShopState.runeLines(saved.flatten)), Nil))
                        _     <- showShop(user, renderer)
                      } yield ()
                  }
        } yield ()
    }

  /** Игра — в журнал событий: по нему лимит на сутки и учёт в админ-панели. Не
    * записалось — покупка от этого не рушится, только в лог. */
  private def logLottery(user: User, runes: List[Item]): Task[Unit] =
    journal.fold(ZIO.unit)(_.append(GameEvent(user.userId, GameEvent.FetRuneLottery, Json.obj(
      "price" -> FetShopState.LotteryPriceDoubloons.asJson,
      "runes" -> runes.map(_.name).asJson)))
      .catchAllCause(cause => ZIO.logErrorCause("лотерея Фета не записана в журнал", cause)))

  // ── Набор успешного искателя ─────────────────────────────────────────────

  /** Предлагать ли набор: он один на игрока — купил, и строка о покупке лежит в
    * журнале. Без журнала (или без хранилища вещей) не продаём вовсе: иначе не
    * уследить, что набор уже куплен. */
  private def packOffered(user: User): Task[Boolean] =
    (journal, itemRepo) match {
      case (Some(log), Some(_)) => log.count(user.userId, GameEvent.FetStarterPack, 0L).map(_ == 0L)
      case _                    => ZIO.succeed(false)
    }

  /** Карточка: речь Фета и что в наборе. Набор уже куплен — кнопки к нему
    * нет, а нажатие из старого сообщения ведёт к прилавку. */
  private def showPack(user: User, renderer: Renderer): Task[Unit] =
    packOffered(user).flatMap {
      case false => showShop(user, renderer)
      case true =>
        renderer.show(user, Screen(content.text("fet.pack.pitch"), List(
          Choice("FetPackBuy", Choice.fit(content.format("fet.buyLabel",
            "price" -> FetShopState.PackPriceDoubloons.toString)), color = ChoiceColor.Positive, row = Some(0)),
          Choice("FetShop", content.text("fet.back"), color = ChoiceColor.Negative, row = Some(1)))))
    }

  private def confirmPack(user: User, renderer: Renderer): Task[Unit] =
    packOffered(user).zip(getHero(user)).flatMap {
      case (false, _) => showShop(user, renderer)
      case (true, hero) if hero.doubloons < FetShopState.PackPriceDoubloons =>
        renderer.show(user, Screen(content.format("fet.notEnough",
          "price" -> FetShopState.PackPriceDoubloons.toString), backRow))
      case (true, _) =>
        renderer.show(user, Screen(content.format("fet.pack.confirm",
          "price" -> FetShopState.PackPriceDoubloons.toString), List(
          Choice("FetPackYes", content.text("fet.confirmYes"), color = ChoiceColor.Positive, row = Some(0)),
          Choice("FetShop", content.text("fet.confirmNo"), color = ChoiceColor.Negative, row = Some(0))
        ), inline = true))
    }

  /** Купить набор. Сперва вещи получают номера из базы, следом покупка — в
    * журнал: не записалась — покупки нет, и дважды набор не купить. Потом
    * дублоны — Фету, а герою: +7 мест в сумке, семь вещей в неё (влезут всегда:
    * мест прибавилось столько же), серебро на руки, репутация искателей и
    * благословение Азата — продлится, если уже идёт. Уже куплен — к прилавку,
    * без единого слова. */
  private def buyPack(user: User, renderer: Renderer): Task[Unit] =
    (journal, itemRepo) match {
      case (Some(log), Some(items)) =>
        for {
          offered <- packOffered(user)
          hero    <- getHero(user)
          price    = FetShopState.PackPriceDoubloons
          _ <- if (!offered) showShop(user, renderer)
               else if (hero.doubloons < price)
                 renderer.show(user, Screen(content.format("fet.notEnough", "price" -> price.toString), backRow))
               else
                 for {
                   goods <- packGoods(hero, items)
                   _     <- log.append(GameEvent(user.userId, GameEvent.FetStarterPack, Json.obj(
                              "price" -> price.asJson, "items" -> goods.map(_.displayTitle).asJson)))
                   _     <- heroDao.updateDoubloons(user.userId, hero.doubloons - price)
                   _     <- inventoryRepo.increaseCapacity(hero.id, FetShopState.PackSlots).mapError(asThrowable)
                   _     <- ZIO.foreachDiscard(goods)(inventoryRepo.addItem(hero.id, _).mapError(asThrowable))
                   _     <- heroDao.updateSilver(user.userId, hero.silver + FetShopState.PackSilver)
                   // Как любая репутация, идёт и в дневной наказ Горна «репутация за день».
                   _     <- heroDao.updateGuildReputation(user.userId, hero.guildReputation + FetShopState.PackReputation)
                   now   <- nowMs
                   azat  <- AzatData.load(heroDao, user.userId, now)
                   _     <- AzatData.save(heroDao, user.userId, azat.blessedFor(FetShopState.PackBlessingMs, now))
                   _     <- renderer.show(user, Screen(content.text("fet.pack.done"), Nil))
                   _     <- showShop(user, renderer)
                 } yield ()
        } yield ()
      case _ => showShop(user, renderer)
    }

  /** Вещи набора, каждая со своим номером из базы: стопки отваров (и склянки
    * внутри них — у каждой свой номер, иначе распакованные слиплись бы) и
    * оружие со щитом. */
  private def packGoods(hero: Hero, items: ItemRepository): Task[List[Item]] =
    for {
      stacks <- ZIO.foreach(FetShopState.PackBrews) { kind =>
                  ZIO.foreach(List.fill(Bundling.Size)(BrewKind.item(kind)))(items.persist(hero.id, _))
                    .flatMap(inside => ZIO.fromOption(Bundling.pack(inside))
                      .orElseFail(new Throwable(s"Стопка «${kind.label}» не собралась")))
                    .flatMap(items.persist(hero.id, _))
                }
      seed   <- Random.nextLong
      gear   <- ZIO.foreach(FetShopState.packGear(Rng(seed)))(items.persist(hero.id, _))
    } yield stacks ++ gear

  // ── Вспомогательное ────────────────────────────────────────────────────────

  private def backRow: List[Choice] =
    List(Choice("FetShop", content.text("fet.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def withKind(ua: UserAction)(f: ArtifactKind => Task[Unit]): Task[Unit] =
    ua.payload
      .flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("kind")))
      .flatMap(ArtifactKind.withNameOption) match {
        case Some(kind) => f(kind)
        case None       => ZIO.unit
      }

  /** Докуда Фет соберёт артефакт этому герою (см. [[HeroArtifacts.maxTier]]). */
  private def topTier(user: User, kind: ArtifactKind): Task[Int] =
    quest.load(user).map(HeroArtifacts.maxTier(kind, _))

  private def mine(user: User, kind: ArtifactKind): Task[Artifact] =
    getHero(user).flatMap(h => artifacts.get(h.id).mapError(asThrowable)).map(_.of(kind))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object FetShopState {

  /** Сколько золота Фет отсыпает за кусок живой кожи. */
  val QuestDoubloons: Long = 25L

  /** Почём у Фета каждое зелье. */
  val PotionPriceDoubloons: Long = 100L

  /** Сколько зелий в ряду клавиатуры прилавка. */
  val PotionsPerRow: Int = 2

  /** Зелье исчезновения: почём склянка и на сколько дней она прячет героя из
    * рейтингов. */
  val VanishPriceDoubloons: Long = 150L
  val VanishDays: Int            = 7
  val VanishMs: Long             = VanishDays.toLong * 24L * 60L * 60L * 1000L

  /** Метка в `scene_data`: открыта карточка Зелья исчезновения — число в чате
    * значит, сколько склянок взять. */
  val VanishKey: String = "fetVanish"

  /** До какого момента туман продержится после ещё `n` склянок: срок
    * прибавляется к остатку, а кончился — отсчёт от сейчас. */
  def vanishUntil(current: Option[Long], nowMs: Long, n: Int): Long =
    current.filter(_ > nowMs).getOrElse(nowMs) + n.toLong * VanishMs

  /** Почём купон на смену расы. */
  val CouponPriceDoubloons: Long = 100L

  /** Почём купон на смену имени. */
  val NameCouponPriceDoubloons: Long = 50L

  /** Беспроигрышная лотерея: столько больших рун за столько дублонов. */
  val LotteryRunes: Int           = 3
  val LotteryPriceDoubloons: Long = 35L

  /** Больше стольких игр за сутки по Москве не сыграть. */
  val LotteryDailyLimit: Long = 7L

  /** Вытянутые руны списком: одинаковые — одной строкой со счётом. */
  def runeLines(runes: List[Item]): String = {
    val titles = runes.map(_.displayTitle)
    titles.distinct.map { t =>
      val n = titles.count(_ == t)
      if (n > 1) s"• $t ×$n" else s"• $t"
    }.mkString("\n")
  }

  /** Руны лотереи: узор каждой — случайный из всех ([[RuneStone.all]]),
    * независимо от прочих, поэтому повторы бывают — как и в добыче. */
  def drawRunes: zio.UIO[List[pangea.model.rune.Rune]] = {
    val pool = RuneStone.all.toVector
    if (pool.isEmpty) ZIO.succeed(Nil)
    else ZIO.foreach(List.range(0, LotteryRunes))(_ => Random.nextIntBounded(pool.size).map(pool.lift)).map(_.flatten)
  }

  /** Кожа упыря — остаётся только от Гнилого Джо. */
  def ghoulSkin(item: Item): Boolean = item.material.contains(MaterialKind.GhoulSkin)

  /** Набор успешного искателя: почём, что в нём и сколько. Числа — те же, что в
    * текстах `fet.pack.pitch` и `fet.pack.done`. */
  val PackPriceDoubloons: Long = 600L
  val PackSilver: Long         = 25000L
  val PackReputation: Long     = 500L
  val PackSlots: Long          = 7L
  val PackBlessingDays: Long   = 5L
  val PackBlessingMs: Long     = PackBlessingDays * 24L * 60L * 60L * 1000L

  /** Стопки отваров набора: живая вода, шнапс и костоправный — от травм. */
  val PackBrews: List[BrewKind] = List(BrewKind.LivingWater, BrewKind.Schnapps, BrewKind.BoneSetter)

  /** Оружие и щит набора — фиолетовые этого уровня: двуручное и одноручное
    * ближнего боя, лук и щит. */
  val PackGearLvl: Long = 5L
  val PackGear: List[(ItemType, Option[Grip])] = List(
    ItemType.Weapon -> Some(Grip.TwoHanded),
    ItemType.Weapon -> Some(Grip.OneHanded),
    ItemType.Weapon -> Some(Grip.Bow),
    ItemType.Shield -> None)

  /** Оружие и щит набора; статы и гнёзда — как у любой фиолетовой вещи этого
    * уровня. Номеров у них пока нет: их даст база. */
  def packGear(rng: Rng): List[Item] =
    PackGear.foldLeft((List.empty[Item], rng)) { case ((acc, r), (itemType, grip)) =>
      val (item, next) = ItemGenerator.createItemOfType(itemType, PackGearLvl, Rarity.Purple, r, grip)
      (acc :+ item, next)
    }._1
}
