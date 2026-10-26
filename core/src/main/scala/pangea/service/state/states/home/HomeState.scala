package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import io.circe.{Json, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.home.{Furniture, HomeData, HomeRates, House}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.stats.{ParamsBuff, StatBoost}
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.service.purse.Purse
import pangea.service.schedule.Scheduler
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «🏠 Дом» из города. Пока дома нет — канцелярия совета: Юнона Кинтслев
  * продаёт дома. В первый раз — сцена: листок на доске у ратуши и знакомство.
  * Когда дом есть — сам дом: кровать, библиотека (со второго), дверь к Юноне,
  * если захочется дом побольше.
  *
  * Покупка — в два шага: в канцелярии дома кнопками без цен, нажал на дом —
  * рассказ Юноны и цены кнопками «Подписать за …»; нажал цену — подписал. Дом
  * выше покупается с зачётом того, что есть (см. [[House.doubloonPrice]]); за
  * серебро — только домик, и только без дома.
  *
  * «Обустройство» у Юноны — обстановка дома за дублоны ([[Furniture]]): по
  * месту на вещь ([[House.slots]]). В домике одно место завалено хламом:
  * «Прибраться» самому — 4 часа, как сон (прервался — наубиранное не
  * пропадает, доделывает таймер [[TaskKind.HomeClean]]), или вывезти через
  * Юнону за серебро.
  *
  * Кровать — как комната в таверне, только своя и бесплатно: сон по таймеру
  * (2 / 1,5 / 1 ч), травмы проходят, HP, броня, энергия и отряд — как новые; встал
  * раньше — ничего этого нет. Выспался — ещё и «выспался»: больше опыта на
  * время ([[House.restedMs]]). Пока герой спит, отметка о начале сна лежит в
  * `scene_data`, а будит его таймер ([[TaskKind.HomeSleep]]). */
case class HomeState(
  heroDao:   HeroDao,
  scheduler: Scheduler,
  content:   SceneContent,
  bank:      Option[BankRepository] = None
) extends State {
  import HomeState._

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "HomeMain"    -> Target.Run { (u, _, r) => showHouse(u, r) },
      "HomeOffice"  -> Target.Run { (u, _, r) => office(u, r) },
      "HomeOffer"   -> Target.Run { (u, ua, r) => offer(u, ua, r) },
      "HomeBuy"     -> Target.Run { (u, ua, r) => buy(u, ua, r) },
      "HomeFurnish" -> Target.Run { (u, _, r) => furnish(u, r) },
      "HomeFurnItem"   -> Target.Run { (u, ua, r) => furnItem(u, ua, r) },
      "HomeFurnBuy"    -> Target.Run { (u, ua, r) => furnBuy(u, ua, r) },
      "HomeClutterPay" -> Target.Run { (u, _, r) => clutterPay(u, r) },
      "HomeSlots"      -> Target.Run { (u, _, r) => slots(u, r) },
      "HomeClean"      -> Target.Run { (u, _, r) => clean(u, r) },
      CleanStopKey     -> Target.Run { (u, _, r) => stopCleaning(u, r) },
      "HomeBed"     -> Target.Run { (u, _, r) => bed(u, r) },
      "HomeSleep"   -> Target.Run { (u, _, r) => sleep(u, r) },
      WakeKey       -> Target.Run { (u, _, r) => wake(u, r) },
      "HomeWakeYes" -> Target.Run { (u, _, r) => wakeEarly(u, r) },
      "HomeWakeNo"  -> Target.Run { (u, _, r) => showSleeping(u, r) },
      "HomeLibrary" -> Target.Run { (u, _, r) => library(u, r) },
      "HomeAltar"   -> Target.Goto(StateType.RuneAltar),
      "HomeHerbs"   -> Target.Goto(StateType.HerbWorkbench),
      "HomeGems"    -> Target.Goto(StateType.GemVault),
      "HomeAzat"    -> Target.Run { (u, _, r) => azat(u, r) },
      "HomeAzatUp"  -> Target.Run { (u, _, r) => azatRefill(u, r) },
      "LeaveHome"   -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.Home) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  /** Спит — экран сна; убирается — экран уборки; дом есть — дом; нет —
    * канцелярия Юноны. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    busy(user).flatMap {
      case (Some(_), _) => showSleeping(user, renderer).unit
      case (_, Some(_)) => showCleaning(user, renderer).unit
      case _            => loadHome(user).flatMap(home => (if (home.owns) showHouse(user, renderer) else office(user, renderer)).unit)
    }

  /** Пока герой спит, работают только кнопки пробуждения, пока убирается —
    * только «Прерваться»: старая кнопка из прежнего экрана не уведёт его ни из
    * постели, ни от хлама. */
  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    busy(user).flatMap {
      case (Some(_), _) if !actionOf(ua).exists(WakeActions.contains) => showSleeping(user, renderer)
      case (_, Some(_)) if !actionOf(ua).contains(CleanStopKey)        => showCleaning(user, renderer)
      case _                                                           => branch.act(user, ua, renderer)
    }

  // ── Канцелярия совета ───────────────────────────────────────────────────────

  /** Юнона и что у совета есть на продажу: дома — кнопками без цен (цены — в
    * рассказе о доме), «Обустройство» и выход. В первый раз — листок на доске и
    * знакомство; дальше — короткое приветствие, своё для тех, у кого дом есть. */
  private def office(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      home   <- loadHome(user)
      first   = !home.metJunona
      _      <- ZIO.when(first)(saveHome(user, home.copy(metJunona = true)))
      current = home.house
      text    =
        if (first) content.text("home.office.intro")
        else if (current.contains(House.Island)) content.format("home.office.top", "race" -> hero.race.genitivePlural)
        else if (current.isDefined) content.text("home.office.owner")
        else content.text("home.office.greeting")
      houses  = House.forSale(current).zipWithIndex.map { case (h, i) =>
                  Choice("HomeOffer", content.format("home.office.offer", "name" -> name(h)),
                    color = ChoiceColor.Primary, data = Map("h" -> h.key), row = Some(i))
                }
      furnish = Choice("HomeFurnish", content.text("home.office.furnish"), color = ChoiceColor.Secondary, row = Some(houses.size))
      back    =
        if (current.isDefined) Choice("HomeMain", content.text("home.office.home"), color = ChoiceColor.Negative, row = Some(houses.size + 1))
        else Choice("LeaveHome", content.text("home.office.leave"), color = ChoiceColor.Negative, row = Some(houses.size + 1))
      _      <- renderer.show(user, Screen(text, houses ++ List(furnish, back)))
    } yield StateType.Home

  /** Дом из списка: рассказ Юноны, особая реплика про зачёт, если дом уже
    * есть, и цены — кнопками «Подписать за …» (у домика без дома их две, за
    * серебро и за дублоны); нажал цену — подписал. «Назад» — к списку домов. */
  private def offer(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      onSale(ua, home) match {
        case None => office(user, renderer)
        case Some(h) =>
          val options = prices(h, home.house)
          val trade   = home.house.fold("")(old =>
            "\n\n" + content.format(s"home.houses.${h.key}.tradeFrom.${old.key}",
              "price" -> money(House.doubloonPrice(h, home.house))))
          val asked   = options.map { case (pay, price) => priceText(price, pay) }
            .reduce((a, b) => content.format("home.office.priceOr", "first" -> a, "second" -> b))
          val confirm = content.format("home.office.confirm", "name" -> name(h), "price" -> asked) +
            home.house.fold("")(old => " " + content.format("home.office.confirmTrade", "old" -> name(old)))
          val sign    = options.zipWithIndex.map { case ((pay, price), i) =>
            Choice("HomeBuy", content.format("home.office.sign", "price" -> priceText(price, pay)),
              color = ChoiceColor.Positive, data = Map("h" -> h.key, "pay" -> pay), row = Some(i))
          }
          val back    = Choice("HomeOffice", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(sign.size))
          renderer.show(user, Screen(content.text(s"home.houses.${h.key}.pitch") + trade + "\n\n" + confirm,
            sign :+ back)).as(StateType.Home)
      }
    }

  /** «Обустройство» без дома: обустраивать нечего — реплика Юноны и «Назад» в канцелярию. */
  private def furnishStub(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("home.office.furnishText"), List(
      Choice("HomeOffice", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(0))
    ))).as(StateType.Home)

  /** Подписать: деньги — сначала, дом — потом. Не хватило — Юнона говорит
    * сколько, и ничего не списано. Старый дом уходит в управление сам: дом у
    * героя всегда один. */
  private def buy(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      res  <- deal(ua, home) match {
        case None => office(user, renderer)
        case Some((h, pay, price)) =>
          charge(hero, pay, price).flatMap {
            case Left(lack) =>
              renderer.show(user, Screen(content.format("home.office.lack", "lack" -> priceText(lack, pay)), Nil)) *>
                office(user, renderer)
            case Right(_) =>
              // Алтарь Азата больше прежнего — первый дом с алтарём или переезд
              // на остров — приходит полным. Дальше энергию дома пополняют сами.
              val charges = if (h.altarCharges > home.altarMax) h.altarCharges else home.charges
              saveHome(user, home.copy(tier = h.tier, metJunona = true, charges = charges)) *>
                renderer.show(user, Screen(content.text(s"home.houses.${h.key}.bought"), Nil)) *>
                showHouse(user, renderer)
          }
      }
    } yield res

  /** Дом из кнопки — если он сейчас продаётся (дом ниже своего не продают). */
  private def onSale(ua: UserAction, home: HomeData): Option[House] =
    payload(ua, "h").flatMap(House.byKey).filter(House.forSale(home.house).contains)

  /** Цены дома по способам платы: за серебро — только домик без дома, дублоны —
    * всегда, уже с зачётом сданного. */
  private def prices(h: House, current: Option[House]): List[(String, Long)] =
    House.silverPrice(h, current).map(PaySilver -> _).toList :+ (PayDoubloons -> House.doubloonPrice(h, current))

  /** Что за сделка в кнопке: дом, который сейчас продаётся, способ платы и цена. */
  private def deal(ua: UserAction, home: HomeData): Option[(House, String, Long)] =
    for {
      h     <- onSale(ua, home)
      pay   <- payload(ua, "pay")
      price <- prices(h, home.house).collectFirst { case (`pay`, p) => p }
    } yield (h, pay, price)

  /** Списать цену: серебро — общим кошелём (с ячейкой), дублоны — с героя.
    * Не хватило — `Left` с тем, сколько не хватает. */
  private def charge(hero: Hero, pay: String, price: Long): Task[Either[Long, Unit]] =
    if (pay == PaySilver)
      purse.wallet(hero).flatMap { w =>
        if (!w.canAfford(price)) ZIO.left(price - w.total)
        else purse.charge(hero.userId, hero, price).as(Right(()))
      }
    else if (hero.doubloons < price) ZIO.left(price - hero.doubloons)
    else heroDao.updateDoubloons(hero.userId, hero.doubloons - price).as(Right(()))

  // ── Дом ─────────────────────────────────────────────────────────────────────

  private def showHouse(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house match {
        case None => office(user, renderer)
        case Some(h) =>
          // Ряд 0 — отдых, чтение и уборка, ряд 1 — хозяйство дома, дальше —
          // обстановка, Юнона и дверь.
          val second = h.tier >= House.Townhouse.tier
          val buttons = List(
            Some(Choice("HomeBed", content.text("home.house.bed"), color = ChoiceColor.Positive, row = Some(0))),
            Option.when(second)(Choice("HomeLibrary", content.text("home.house.library"), row = Some(0))),
            Option.when(home.hasClutter)(Choice("HomeClean", content.text("home.house.clean"), row = Some(0))),
            Some(Choice("HomeAltar", content.text("home.house.altar"), row = Some(1))),
            Option.when(second)(Choice("HomeHerbs", content.text("home.house.workbench"), row = Some(1))),
            Option.when(second)(Choice("HomeAzat", content.text("home.house.azat"), row = Some(1))),
            Option.when(h == House.Island)(Choice("HomeGems", content.text("home.house.gems"), row = Some(1))),
            Some(Choice("HomeSlots", content.text("home.house.slots"), row = Some(2))),
            Some(Choice("HomeOffice", content.text("home.house.office"), row = Some(2))),
            Some(Choice("LeaveHome", content.text("home.house.leave"), color = ChoiceColor.Negative, row = Some(3)))
          ).flatten
          renderer.show(user, Screen(content.text(s"home.houses.${h.key}.inside"), buttons)).as(StateType.Home)
      }
    }

  /** Библиотека — у каждого дома своя: маленькая в добротном доме, большая
    * личная на острове. В домике её нет. */
  private def library(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house.filter(_.tier >= House.Townhouse.tier) match {
        case None    => showHouse(user, renderer)
        case Some(h) => renderer.show(user, Screen(content.text(s"home.houses.${h.key}.library"), List(backToHouse))).as(StateType.Home)
      }
    }

  // ── Алтарь Азата ────────────────────────────────────────────────────────────

  /** Личный алтарь Азата — со второго дома: его магия питает весь дом (котелок
    * и всё, что работает на силе Азата). Энергия дома пополняется здесь же — по
    * цене храма. */
  private def azat(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      if (!home.house.exists(_.tier >= House.Townhouse.tier)) showHouse(user, renderer)
      else renderer.show(user, Screen(content.format("home.azat.text",
          "n" -> home.charges.toString, "max" -> home.altarMax.toString), List(
          Choice("HomeAzatUp", content.format("home.azat.refill", "charges" -> HomeRates.AltarRefillCharges.toString,
            "price" -> money(HomeRates.AltarRefillSilver)), color = ChoiceColor.Positive, row = Some(0)),
          backToHouse.copy(row = Some(1))
        ))).as(StateType.Home)
    }

  /** Пополнить: +[[HomeRates.AltarRefillCharges]] за серебро (с ячейкой), не выше потолка. */
  private def azatRefill(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      res  <- if (!home.house.exists(_.tier >= House.Townhouse.tier)) showHouse(user, renderer)
              else if (home.charges >= home.altarMax)
                renderer.show(user, Screen(content.format("home.azat.full",
                  "n" -> home.charges.toString, "max" -> home.altarMax.toString), Nil)) *> azat(user, renderer)
              else charge(hero, PaySilver, HomeRates.AltarRefillSilver).flatMap {
                case Left(lack) =>
                  renderer.show(user, Screen(content.format("home.azat.lack", "lack" -> money(lack)), Nil)) *> azat(user, renderer)
                case Right(_) =>
                  val next = (home.charges + HomeRates.AltarRefillCharges).min(home.altarMax)
                  saveHome(user, home.copy(charges = next)) *>
                    renderer.show(user, Screen(content.format("home.azat.refilled",
                      "n" -> next.toString, "max" -> home.altarMax.toString), Nil)) *>
                    azat(user, renderer)
              }
    } yield res

  // ── Обстановка ──────────────────────────────────────────────────────────────

  /** Места под обстановку: что где стоит и сколько свободно. Хлам в домике
    * тоже занимает место — «Прибраться» самому или вывезти через Юнону. */
  private def slots(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      if (!home.owns) office(user, renderer)
      else {
        val clutter = Option.when(home.hasClutter)(content.format("home.slots.clutter",
          "left" -> duration(HomeRates.ClutterCleanMs - home.cleanedMs), "price" -> money(HomeRates.ClutterSilver)))
        val placed  = home.furniture.flatMap(Furniture.byKey).map(furnName)
        val free    = List.fill(home.slotsFree)(content.text("home.slots.free"))
        val lines   = (clutter.toList ++ placed ++ free).zipWithIndex.map { case (l, i) =>
          content.format("home.slots.line", "n" -> (i + 1).toString, "what" -> l)
        }
        val text = content.format("home.slots.text", "used" -> home.slotsUsed.toString,
          "max" -> home.slotsMax.toString, "lines" -> lines.mkString("\n"))
        renderer.show(user, Screen(text, List(
          Option.when(home.hasClutter)(Choice("HomeClean", content.text("home.house.clean"), color = ChoiceColor.Positive, row = Some(0))),
          Some(Choice("HomeFurnish", content.text("home.slots.furnish"), row = Some(0))),
          Some(backToHouse.copy(row = Some(1)))
        ).flatten)).as(StateType.Home)
      }
    }

  /** «Прибраться»: взяться за хлам самому. Как сон — пока убирается, дом его
    * не отпускает; прервался — наубиранное не пропадает. Доделывает таймер. */
  private def clean(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      if (!home.hasClutter) showHouse(user, renderer)
      else for {
        now <- nowMs
        _   <- heroDao.writeSceneData(user.userId, Json.obj(CleanStartKey -> now.asJson))
        _   <- scheduler.schedule(user.userId, now + (HomeRates.ClutterCleanMs - home.cleanedMs).max(0L),
                 TaskKind.HomeClean, StateType.Home, CleanStopAction)
        s   <- showCleaning(user, renderer)
      } yield s
    }

  private def showCleaning(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      started <- cleanStartedAt(user)
      home    <- loadHome(user)
      left     = (HomeRates.ClutterCleanMs - home.cleanedMs - started.fold(0L)(now - _)).max(0L)
      _       <- renderer.show(user, Screen(content.format("home.clean.working", "remaining" -> remaining(left)),
                   List(Choice(CleanStopKey, content.text("home.clean.stop"), color = ChoiceColor.Negative))))
    } yield StateType.Home

  /** «Прерваться» — и таймер приходит сюда же. Наубиранное копится; хватило
    * — хлам разобран, место свободно. */
  private def stopCleaning(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      started <- cleanStartedAt(user)
      home    <- loadHome(user)
      _       <- clearCleaning(user)
      done     = home.cleanedMs + started.fold(0L)(now - _)
      res     <- if (started.isEmpty || !home.hasClutter) showHouse(user, renderer)
                 else if (done >= HomeRates.ClutterCleanMs)
                   saveHome(user, home.copy(clutterCleared = true, cleanedMs = HomeRates.ClutterCleanMs)) *>
                     say(user, content.text("home.clean.done"), renderer) *> showHouse(user, renderer)
                 else
                   saveHome(user, home.copy(cleanedMs = done)) *>
                     say(user, content.format("home.clean.paused", "left" -> duration(HomeRates.ClutterCleanMs - done)), renderer) *>
                     showHouse(user, renderer)
    } yield res

  private def clearCleaning(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null) *> scheduler.cancel(user.userId, TaskKind.HomeClean)

  // ── «Обустройство» у Юноны ──────────────────────────────────────────────────

  /** Папка «Обустройство»: что совет поставит в дом — по дому и местам; хлам из
    * домика — вывезти за серебро. Без дома обустраивать нечего. */
  private def furnish(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house match {
        case None => furnishStub(user, renderer)
        case Some(h) =>
          val clutter = Option.when(home.hasClutter)(Choice("HomeClutterPay",
            content.format("home.furnish.clutter", "price" -> money(HomeRates.ClutterSilver)),
            color = ChoiceColor.Positive, row = Some(0)))
          val items = Furniture.values.filter(_.minTier <= h.tier).zipWithIndex.map { case (f, i) =>
            Choice("HomeFurnItem", content.format("home.furnish.item", "name" -> furnName(f), "price" -> money(f.doubloons)),
              data = Map("f" -> f.key), row = Some(1 + i))
          }
          val text = content.format("home.furnish.text", "used" -> home.slotsUsed.toString, "max" -> home.slotsMax.toString) +
            (if (items.isEmpty) "\n\n" + content.text("home.furnish.nothingYet") else "")
          renderer.show(user, Screen(text, clutter.toList ++ items :+
            Choice("HomeOffice", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(1 + items.size))))
            .as(StateType.Home)
      }
    }

  /** Вещь из папки: что даёт, сколько мест свободно, и «Купить за …». */
  private def furnItem(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      furnOf(ua, home) match {
        case None => furnish(user, renderer)
        case Some(f) =>
          val text = furnText(f, "desc") + "\n\n" + content.format("home.furnish.free", "n" -> home.slotsFree.toString)
          renderer.show(user, Screen(text, List(
            Choice("HomeFurnBuy", content.format("home.furnish.buy", "price" -> money(f.doubloons)),
              color = ChoiceColor.Positive, data = Map("f" -> f.key), row = Some(0)),
            Choice("HomeFurnish", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(0))
          ))).as(StateType.Home)
      }
    }

  /** Купить: нужно свободное место и дублоны; вещь встаёт в дом сразу. */
  private def furnBuy(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      res  <- furnOf(ua, home) match {
        case None => furnish(user, renderer)
        case Some(_) if home.slotsFree <= 0 =>
          say(user, content.text("home.furnish.noSlot"), renderer) *> furnish(user, renderer)
        case Some(f) =>
          charge(hero, PayDoubloons, f.doubloons).flatMap {
            case Left(lack) =>
              say(user, content.format("home.office.lack", "lack" -> priceText(lack, PayDoubloons)), renderer) *> furnish(user, renderer)
            case Right(_) =>
              saveHome(user, home.copy(furniture = home.furniture :+ f.key)) *>
                say(user, furnText(f, "bought"), renderer) *> furnish(user, renderer)
          }
      }
    } yield res

  /** Вывезти хлам через Юнону — за серебро (с ячейкой), без уборки руками. */
  private def clutterPay(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      res  <- if (!home.hasClutter) furnish(user, renderer)
              else charge(hero, PaySilver, HomeRates.ClutterSilver).flatMap {
                case Left(lack) =>
                  say(user, content.format("home.office.lack", "lack" -> priceText(lack, PaySilver)), renderer) *> furnish(user, renderer)
                case Right(_) =>
                  saveHome(user, home.copy(clutterCleared = true)) *>
                    say(user, content.text("home.furnish.clutterDone"), renderer) *> furnish(user, renderer)
              }
    } yield res

  /** Вещь из кнопки — если её продают для этого дома. */
  private def furnOf(ua: UserAction, home: HomeData): Option[Furniture] =
    payload(ua, "f").flatMap(Furniture.byKey).filter(f => home.house.exists(_.tier >= f.minTier))

  private def furnName(f: Furniture): String = content.text(s"home.furniture.${f.key}.name")

  /** Текст о вещи (`desc` — в папке, `bought` — после покупки) — с её числами. */
  private def furnText(f: Furniture, part: String): String =
    content.format(s"home.furniture.${f.key}.$part",
      "herbs" -> HomeRates.ApothecaryHerbs.toString, "brews" -> HomeRates.ApothecaryBrews.toString,
      "gems" -> HomeRates.JewelerGems.toString)

  // ── Кровать ─────────────────────────────────────────────────────────────────

  private def bed(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house match {
        case None => office(user, renderer)
        case Some(h) =>
          val text = content.text(s"home.houses.${h.key}.bed") + "\n\n" +
            content.format("home.bed.info", "sleep" -> duration(h.sleepMs),
              "rested" -> duration(h.restedMs), "pct" -> HomeRates.RestedExpPct.toString)
          renderer.show(user, Screen(text, List(
            Choice("HomeSleep", content.text("home.bed.sleep"), color = ChoiceColor.Positive, row = Some(0)),
            backToHouse.copy(row = Some(1))
          ))).as(StateType.Home)
      }
    }

  /** Лечь спать: отметка в `scene_data`, будильник — таймером. */
  private def sleep(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house match {
        case None => office(user, renderer)
        case Some(h) =>
          for {
            now <- nowMs
            _   <- heroDao.writeSceneData(user.userId, Json.obj(SleepStartKey -> now.asJson))
            _   <- scheduler.schedule(user.userId, now + h.sleepMs, TaskKind.HomeSleep, StateType.Home, WakeAction)
            s   <- showSleeping(user, renderer)
          } yield s
      }
    }

  private def showSleeping(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      started <- sleepStartedAt(user)
      home    <- loadHome(user)
      left     = (started, home.house) match {
                   case (Some(s), Some(h)) => (h.sleepMs - (now - s)).max(0L)
                   case _                  => 0L
                 }
      _       <- renderer.show(user, Screen(content.format("home.bed.sleeping", "remaining" -> remaining(left)),
                   List(Choice(WakeKey, content.text("home.bed.wake"), color = ChoiceColor.Negative))))
    } yield StateType.Home

  /** Встать: выспался — травмы проходят, силы возвращаются, и ещё «выспался»;
    * рано — сперва спросить. Будит и таймер — тем же действием. */
  private def wake(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      started <- sleepStartedAt(user)
      home    <- loadHome(user)
      res     <- (started, home.house) match {
        case (Some(s), Some(h)) if now - s >= h.sleepMs => wokeRested(user, h, now, renderer)
        case (Some(s), Some(h)) =>
          renderer.show(user, Screen(
            content.format("home.bed.confirmWake", "remaining" -> remaining(h.sleepMs - (now - s))),
            List(
              Choice("HomeWakeYes", content.text("home.bed.wakeYes"), color = ChoiceColor.Negative, row = Some(0)),
              Choice("HomeWakeNo", content.text("home.bed.wakeNo"), row = Some(0))))).as(StateType.Home)
        case _ => clearSleep(user) *> enter(user, renderer).as(StateType.Home)
      }
    } yield res

  /** Как в снятой комнате таверны: травмы прочь, HP, броня и энергия — до
    * верха, отряд отдохнул вместе с героем. Сверху — «выспался». */
  private def wokeRested(user: User, h: House, now: Long, renderer: Renderer): Task[StateType] =
    for {
      hero    <- getHero(user)
      healed   = hero.copy(traumaUntil = None, traumaNames = Nil)
      _       <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(
                   hp = healed.effectiveMaxHp(now), armor = healed.effectiveMaxArmor(now), energy = healed.maxEnergy(now)))
      _       <- heroDao.updateSquad(user.userId, hero.squad.restored(hero.lvl))
      _       <- heroDao.updateTrauma(user.userId, None, Nil)
      _       <- heroDao.updateStatBoosts(user.userId,
                   hero.statBoosts.add(StatBoost(HomeRates.RestedBoost, ParamsBuff.zero, now + h.restedMs), now))
      _       <- clearSleep(user)
      _       <- renderer.show(user, Screen(content.format("home.bed.woke",
                   "rested" -> duration(h.restedMs), "pct" -> HomeRates.RestedExpPct.toString), Nil))
      s       <- showHouse(user, renderer)
    } yield s

  /** Встал раньше: ни лечения, ни бафа — просто снова дома. */
  private def wakeEarly(user: User, renderer: Renderer): Task[StateType] =
    clearSleep(user) *> showHouse(user, renderer)

  private def clearSleep(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null) *> scheduler.cancel(user.userId, TaskKind.HomeSleep)

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  private def backToHouse: Choice =
    Choice("HomeMain", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(0))

  private def name(h: House): String = content.text(s"home.houses.${h.key}.name")

  private def priceText(price: Long, pay: String): String =
    content.format(if (pay == PaySilver) "home.office.silver" else "home.office.doubloons", "price" -> money(price))

  private def sleepStartedAt(user: User): Task[Option[Long]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.hcursor.get[Long](SleepStartKey).toOption))

  private def cleanStartedAt(user: User): Task[Option[Long]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.hcursor.get[Long](CleanStartKey).toOption))

  /** Чем герой занят дома: когда лёг спать и когда взялся за уборку. */
  private def busy(user: User): Task[(Option[Long], Option[Long])] =
    heroDao.readSceneData(user.userId).map { scene =>
      (scene.flatMap(_.hcursor.get[Long](SleepStartKey).toOption), scene.flatMap(_.hcursor.get[Long](CleanStartKey).toOption))
    }

  private def say(user: User, text: String, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(text, Nil))

  private def loadHome(user: User): Task[HomeData] =
    heroDao.readHomeData(user.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

  private def saveHome(user: User, home: HomeData): Task[Unit] =
    heroDao.writeHomeData(user.userId, home.asJson)

  private def actionOf(ua: UserAction): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object HomeState {

  /** Способ платы в кнопке покупки. */
  val PaySilver: String    = "s"
  val PayDoubloons: String = "d"

  /** Когда герой лёг спать (`scene_data`). */
  val SleepStartKey: String = "homeSleepStartedAt"

  /** Кнопка «Встать» — её же шлёт будильник. */
  val WakeKey: String    = "HomeWake"
  val WakeAction: String = s"""{"action":"$WakeKey"}"""

  /** Что можно нажать, пока герой спит. */
  val WakeActions: Set[String] = Set(WakeKey, "HomeWakeYes", "HomeWakeNo")

  /** Когда герой взялся за хлам (`scene_data`). */
  val CleanStartKey: String = "homeCleanStartedAt"

  /** «Прерваться» — его же шлёт таймер, когда хлам разобран. */
  val CleanStopKey: String    = "HomeCleanStop"
  val CleanStopAction: String = s"""{"action":"$CleanStopKey"}"""

  /** Деньги с разрядами: 20 000. */
  def money(n: Long): String = n.toString.reverse.grouped(3).mkString(" ").reverse

  /** Сколько длится сон или «выспался»: «2 ч», «1 ч 30 мин», «45 мин». */
  def duration(ms: Long): String = {
    val mins = (ms / 60000L).max(0L)
    val h    = mins / 60L
    val m    = mins % 60L
    if (h > 0 && m > 0) s"$h ч $m мин" else if (h > 0) s"$h ч" else s"$m мин"
  }

  /** Сколько осталось спать — с минутами и, под конец, секундами. */
  def remaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val h    = secs / 3600L
    val m    = (secs % 3600L) / 60L
    if (h > 0) s"$h ч $m мин" else if (m > 0) s"$m мин" else s"$secs с"
  }
}
