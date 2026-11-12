package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import io.circe.{Json, jawn}
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.ItemGenerator
import pangea.model.hero.Hero
import pangea.model.hero.{AzatState, CubeStatus, Equipment}
import pangea.model.home.{Furniture, HomeData, HomeRates, House}
import pangea.model.artifact.ArtifactKind
import pangea.model.item.{Item, ItemType, QuestItemKind}
import pangea.model.quest.NpcQuest
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.stats.{ParamsBuff, StatBoost}
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.purse.Purse
import pangea.service.schedule.Scheduler
import pangea.service.state.states.{EquipmentState, InventoryState, LootState}
import pangea.service.state.{AppButton, AzatData, BeltContents, FirstStepsQuest, ItemMenu, NpcQuestDialog, NpcQuestLog, QuestSupport,
  SlotChoice, State, StoryBrandQuest, StoryFaceQuest, StoryLine, StoryPowderQuest, UserAction}
import zio.{Random, Task, ZIO}

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
  * `scene_data`, а будит его таймер ([[TaskKind.HomeSleep]]).
  *
  * «Разложить всё» — три «Положить всё» одной кнопкой ([[HomeStow]]): рунные
  * камни — на алтарь, травы — на полки верстака (со второго дома), камни — в
  * хранилище (на острове); чего в доме нет, то пропускается. */
case class HomeState(
  heroDao:       HeroDao,
  scheduler:     Scheduler,
  content:       SceneContent,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  bank:          Option[BankRepository] = None,
  // Живая сумка и Ларец: «Разложить всё» берёт травы и камни и из них.
  artifacts:     Option[ArtifactRepository] = None,
  // «Встать на ноги» засчитано здесь — в каком доме, идёт в подсчёт исходов.
  outcomes:      Option[QuestOutcomeDao] = None
) extends State {
  import HomeState._

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  /** Магические стойки: обмен надетого на то, что висит на стойке. */
  private val racks = Racks(heroDao, inventoryRepo, content)

  private val stow = HomeStow(inventoryRepo, artifacts, content)

  private val branch = new Branch(
    routes = Map(
      "HomeMain"    -> Target.Run { (u, _, r) => showHouse(u, r) },
      "HomeOffice"  -> Target.Run { (u, _, r) => office(u, r) },
      "HomeOffer"   -> Target.Run { (u, ua, r) => offer(u, ua, r) },
      "HomeBuy"     -> Target.Run { (u, ua, r) => buy(u, ua, r) },
      "HomeFurnish" -> Target.Run { (u, _, r) => furnish(u, r) },
      "HomeJunonaStory" -> Target.Run { (u, _, r) => junonaMarked(u, r) },
      "HomeJunonaPay"   -> Target.Run { (u, _, r) => junonaPay(u, r) },
      "HomeJunonaFace"  -> Target.Run { (u, _, r) => junonaCopies(u, r) },
      "HomeFurnItem"   -> Target.Run { (u, ua, r) => furnItem(u, ua, r) },
      "HomeFurnBuy"    -> Target.Run { (u, ua, r) => furnBuy(u, ua, r) },
      "HomeClutterPay" -> Target.Run { (u, _, r) => clutterPay(u, r) },
      "HomeStash"      -> Target.Run { (u, ua, r) => stash(u, ua, r) },
      "HomeStore"      -> Target.Run { (u, _, r) => store(u, r) },
      "HomeUnstash"    -> Target.Run { (u, ua, r) => unstash(u, ua, r) },
      "HomeSlots"      -> Target.Run { (u, _, r) => slots(u, r) },
      "HomeArmory"     -> Target.Run { (u, _, r) => armory(u, r) },
      "HomeRack"       -> Target.Run { (u, ua, r) => rack(u, ua, r) },
      "HomeRackSwap"   -> Target.Run { (u, ua, r) => rackSwap(u, ua, r) },
      "HomeRackTake"   -> Target.Run { (u, ua, r) => rackTake(u, ua, r) },
      "HomeRackPut"    -> Target.Run { (u, ua, r) => rackPutList(u, ua, r) },
      "HomeRackPick"   -> Target.Run { (u, ua, r) => rackPick(u, ua, r) },
      "HomeRackOff"    -> Target.Run { (u, ua, r) => rackOffList(u, ua, r) },
      "HomeRackDrop"   -> Target.Run { (u, ua, r) => rackDrop(u, ua, r) },
      "HomeBench"      -> Target.Run { (u, _, r) => bench(u, r) },
      "HomeCubeInsert" -> Target.Run { (u, _, r) => insertCube(u, r) },
      "HomeCube"       -> Target.Run { (u, _, r) => openCube(u, r) },
      "HomeRecipes"    -> Target.Run { (u, ua, r) => recipes(u, ua, r) },
      "HomeClean"      -> Target.Run { (u, _, r) => clean(u, r) },
      CleanStopKey     -> Target.Run { (u, _, r) => stopCleaning(u, r) },
      "HomeBed"     -> Target.Run { (u, _, r) => bed(u, r) },
      "HomeSleep"   -> Target.Run { (u, _, r) => sleep(u, r) },
      WakeKey       -> Target.Run { (u, _, r) => wake(u, r) },
      "HomeWakeYes" -> Target.Run { (u, _, r) => wakeEarly(u, r) },
      "HomeWakeNo"  -> Target.Run { (u, _, r) => showSleeping(u, r) },
      "HomeLibrary" -> Target.Run { (u, _, r) => library(u, r) },
      "HomeStowAll" -> Target.Run { (u, _, r) => stowAll(u, r) },
      "HomeAltar"   -> Target.Goto(StateType.RuneAltar),
      "HomeHerbs"   -> Target.Goto(StateType.HerbWorkbench),
      "HomeGems"    -> Target.Goto(StateType.GemVault),
      "HomeWall"    -> Target.Goto(StateType.TrophyWall),
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
      // Вошёл в свой дом — и «Встать на ноги», если все пункты выполнены,
      // засчитывается здесь (см. FirstStepsQuest.completeAtHome).
      case _            => loadHome(user).flatMap(home =>
                             (if (home.owns) FirstStepsQuest.completeAtHome(heroDao, content, user, home, renderer, outcomes) *> showHouse(user, renderer)
                              else office(user, renderer)).unit)
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
      // Основная линия: набросок узора при герое — спросить, кого ещё записал
      // совет; копии списка отмеченных — показать, кто переплетает книги совета.
      story  <- junonaOpen(user)
      copies <- junonaFaceOpen(user)
      marked  = Option.when(story)(Choice("HomeJunonaStory", content.text("story.storyBrand.junona.label"), row = Some(houses.size))) orElse
                  Option.when(copies)(Choice("HomeJunonaFace", content.text("story.storyFace.junona.label"), row = Some(houses.size)))
      back    =
        if (current.isDefined) Choice("HomeMain", content.text("home.office.home"), color = ChoiceColor.Negative, row = Some(houses.size + 1))
        else Choice("LeaveHome", content.text("home.office.leave"), color = ChoiceColor.Negative, row = Some(houses.size + 1))
      _      <- renderer.show(user, Screen(text, houses ++ List(furnish) ++ marked.toList ++ List(back)))
    } yield StateType.Home

  /** «Что горит на груди» на шаге Юноны, и набросок узора при герое. */
  private def junonaOpen(user: User): Task[Boolean] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryBrandQuest.toJunona(quests)) ZIO.succeed(false)
      else getHero(user).flatMap(hero => inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)))
        .map(inv => QuestSupport.hasItem(inv.items.data, QuestItemKind.StorySketch))
    }

  /** Кого записывает совет, Юнона даром не скажет: сперва пошлина — шкуры
    * Белого волка (StoryBrandQuest.JunonaPrice). Плата при герое — кнопка
    * «Отдать». */
  private def junonaMarked(user: User, renderer: Renderer): Task[StateType] =
    junonaOpen(user).flatMap {
      case false => office(user, renderer)
      case true  =>
        for {
          quests <- NpcQuestLog.load(heroDao, user.userId)
          _      <- NpcQuestLog.save(heroDao, user.userId, StoryBrandQuest.namePrice(quests))
          hero   <- getHero(user)
          canPay <- StoryBrandQuest.canPay(inventoryRepo, hero, StoryBrandQuest.JunonaPrice)
          _      <- renderer.show(user, StoryBrandQuest.askScreen(content, content.text("story.storyBrand.junona.ask"),
                      "HomeJunonaPay", "story.storyBrand.junona.payLabel", canPay,
                      Choice("HomeOffice", content.text("home.house.back"), color = ChoiceColor.Negative)))
        } yield StateType.Home
    }

  /** Пошлина отдана — Юнона о том, кого записывает совет: про Смотрителя — по
    * тому, как с ним поступили, — и про Сойку; дальше — встреча с ней. Платы
    * при герое нет — снова цена. */
  private def junonaPay(user: User, renderer: Renderer): Task[StateType] =
    junonaOpen(user).flatMap {
      case false => office(user, renderer)
      case true  =>
        for {
          quests <- NpcQuestLog.load(heroDao, user.userId)
          hero   <- getHero(user)
          paid   <- StoryBrandQuest.pay(inventoryRepo, hero, StoryBrandQuest.JunonaPrice)
          fate    = StoryPowderQuest.choice(quests).fold("")(f => content.text(s"story.storyBrand.junona.warden.${f.key}") + "\n\n")
          res    <- if (!paid) junonaMarked(user, renderer)
                    else NpcQuestLog.save(heroDao, user.userId, StoryBrandQuest.step(quests, 4)) *>
                      renderer.show(user, Screen(content.text("story.storyBrand.junona.paid").trim + "\n\n" + fate +
                        content.text("story.storyBrand.junona.text"), Nil)) *>
                      office(user, renderer)
        } yield res
    }

  /** «Лицо из прошлого» на шаге Юноны, и копии списка при герое. */
  private def junonaFaceOpen(user: User): Task[Boolean] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryFaceQuest.toJunona(quests)) ZIO.succeed(false)
      else getHero(user).flatMap(hero => inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)))
        .map(inv => QuestSupport.hasItem(inv.items.data, QuestItemKind.StoryCopies))
    }

  /** Юнона узнаёт бумагу совета и обещает стражу; копии остаются у неё, ключ
    * от архива — тоже. На ступенях канцелярии герою суют записку «Э.»: его
    * узнали. Глава закрыта: записка — в сумку (зацепка Рубежа I), серебро
    * совета, опыт. */
  private def junonaCopies(user: User, renderer: Renderer): Task[StateType] =
    for {
      open <- junonaFaceOpen(user)
      _    <- ZIO.when(open)(for {
                hero      <- getHero(user)
                survivors <- StoryLine.survivorsOf(heroDao, user.userId)
                _         <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StoryCopies, QuestItemKind.StoryArchiveKey))
                _         <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StoryNote)
                silver     = StoryFaceQuest.silver
                _         <- heroDao.updateSilver(user.userId, hero.silver + silver)
                done      <- NpcQuestDialog(heroDao, content, NpcQuest.StoryFace, "StoryFace")
                               .complete(user, hero.copy(silver = hero.silver + silver), identity)
                (_, expLine) = done
                _         <- renderer.show(user, Screen(content.text("story.storyFace.junona.text"), Nil))
                _         <- renderer.show(user, Screen(
                               List("story.storyFace.note.arrives", s"story.storyFace.note.with${survivors.suffix}",
                                 "story.storyFace.note.end").map(k => content.text(k).trim).mkString("\n\n"), Nil))
                _         <- renderer.show(user, Screen(
                               content.format("story.storyFace.reward", "silver" -> silver.toString, "exp" -> expLine) +
                                 "\n\n" + content.text("story.storyFace.questDone"), Nil))
              } yield ())
      res  <- office(user, renderer)
    } yield res

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
              // В доме просторнее из кладовой встаёт, что спрятано первым, — сколько хватит мест.
              val moved   = home.copy(tier = h.tier, metJunona = true, charges = charges)
              val settled = moved.unpacked
              val placed  = moved.storedFurniture.take(moved.storedFurniture.size - settled.storedFurniture.size)
              saveHome(user, settled) *>
                renderer.show(user, Screen(content.text(s"home.houses.${h.key}.bought"), Nil)) *>
                ZIO.when(placed.nonEmpty)(say(user, content.format("home.store.unpacked",
                  "names" -> placed.map(furnName).mkString(", ")), renderer)) *>
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
          // Ряд 0 — отдых, оружейная, чтение и уборка, ряд 1 — хозяйство дома
          // (на острове он полон: пять кнопок), ряд 2 — «Разложить всё», дальше —
          // обстановка, Юнона и дверь.
          val second = h.tier >= House.Townhouse.tier
          val buttons = List(
            Some(Choice("HomeBed", content.text("home.house.bed"), color = ChoiceColor.Positive, row = Some(0))),
            Some(Choice("HomeArmory", content.text("home.house.armory"), row = Some(0))),
            Option.when(second)(Choice("HomeLibrary", content.text("home.house.library"), row = Some(0))),
            Option.when(home.hasClutter)(Choice("HomeClean", content.text("home.house.clean"), row = Some(0))),
            Option.when(home.hasWall)(Choice("HomeWall", content.text("home.house.wall"), row = Some(0))),
            Some(Choice("HomeAltar", content.text("home.house.altar"), row = Some(1))),
            Option.when(second)(Choice("HomeHerbs", content.text("home.house.workbench"), row = Some(1))),
            Option.when(second)(Choice("HomeAzat", content.text("home.house.azat"), row = Some(1))),
            Option.when(h == House.Island)(Choice("HomeGems", content.text("home.house.gems"), row = Some(1))),
            Option.when(home.count(Furniture.ArtisanBench) > 0)(Choice("HomeBench", content.text("home.house.bench"), row = Some(1))),
            Some(Choice("HomeStowAll", content.text("home.house.stowAll"), color = ChoiceColor.Positive, row = Some(2))),
            Some(Choice("HomeSlots", content.text("home.house.slots"), row = Some(3))),
            Some(Choice("HomeOffice", content.text("home.house.office"), row = Some(3))),
            Some(Choice("LeaveHome", content.text("home.house.leave"), color = ChoiceColor.Negative, row = Some(4)))
          ).flatten
          renderer.show(user, Screen(content.text(s"home.houses.${h.key}.inside"), buttons)).as(StateType.Home)
      }
    }

  /** «Разложить всё»: рунные камни — на алтарь, травы и отвары — на полки
    * верстака травника, камни — в хранилище, пока есть место; травы и отвары
    * берутся и из Живой сумки, камни — и из Ларца. Нет верстака или хранилища
    * в этом доме — их шаг пропускается. Дом пишется одной записью, итог —
    * одним сообщением. */
  private def stowAll(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house match {
        case None => office(user, renderer)
        case Some(h) =>
          for {
            hero  <- getHero(user)
            runes <- stow.runeStones(hero.id)
            herbs <- if (h.tier >= House.Townhouse.tier)
                       stow.gather(hero.id, HomeStow.isHerb, ArtifactKind.LivingBag, home.herbShelves - home.herbs.size)
                     else ZIO.succeed((List.empty[Item], 0))
            brews <- if (h.tier >= House.Townhouse.tier)
                       stow.gather(hero.id, HomeStow.isBrew, ArtifactKind.LivingBag, home.brewShelves - home.brews.size)
                     else ZIO.succeed((List.empty[Item], 0))
            gems  <- if (h == House.Island)
                       stow.gather(hero.id, HomeStow.isGem, ArtifactKind.Casket, home.gemShelves - home.gems.size)
                     else ZIO.succeed((List.empty[Item], 0))
            next   = HomeStow.addRunes(home, runes)
                       .copy(herbs = home.herbs ++ herbs._1, brews = home.brews ++ brews._1, gems = home.gems ++ gems._1)
            _     <- ZIO.when(next != home)(saveHome(user, next))
            lines  = List(
                       Option.when(runes.nonEmpty)(stow.runesText(runes)),
                       Option.when(herbs != ((Nil, 0)))(stow.herbsText(herbs._1.size, herbs._2)),
                       Option.when(brews != ((Nil, 0)))(stow.brewsText(brews._1.size, brews._2)),
                       Option.when(gems != ((Nil, 0)))(stow.gemsText(gems._1.size, gems._2))
                     ).flatten
            _     <- say(user, if (lines.isEmpty) content.text("home.house.stowNothing") else lines.mkString("\n\n"), renderer)
            s     <- showHouse(user, renderer)
          } yield s
      }
    }

  /** Библиотека — у каждого дома своя: маленькая в добротном доме, большая
    * личная на острове. В домике её нет. */
  private def library(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house.filter(_.tier >= House.Townhouse.tier) match {
        case None    => showHouse(user, renderer)
        case Some(h) =>
          val notes = home.recipeBook
          val book  = Option.when(notes.nonEmpty)(content.format("home.library.bookLine", "n" -> notes.size.toString))
          renderer.show(user, Screen(content.text(s"home.houses.${h.key}.library") + book.fold("")("\n\n" + _), List(
            Option.when(notes.nonEmpty)(Choice("HomeRecipes", content.text("home.library.book"), color = ChoiceColor.Positive,
              data = Map("p" -> "0"), row = Some(0))),
            Some(backToHouse.copy(row = Some(1)))
          ).flatten)).as(StateType.Home)
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
        val placed  = home.placed.map(furnName)
        val free    = List.fill(home.slotsFree)(content.text("home.slots.free"))
        val lines   = (clutter.toList ++ placed ++ free).zipWithIndex.map { case (l, i) =>
          content.format("home.slots.line", "n" -> (i + 1).toString, "what" -> l)
        }
        val stored = home.storedFurniture.size
        val storedLine = Option.when(stored > 0)(
          "\n\n" + content.format("home.slots.stored", "n" -> stored.toString)).getOrElse("")
        val text = content.format("home.slots.text", "used" -> home.slotsUsed.toString,
          "max" -> home.slotsMax.toString, "lines" -> lines.mkString("\n")) + storedLine
        renderer.show(user, Screen(text, List(
          Option.when(home.hasClutter)(Choice("HomeClean", content.text("home.house.clean"), color = ChoiceColor.Positive, row = Some(0))),
          Some(Choice("HomeFurnish", content.text("home.slots.furnish"), row = Some(0))),
          Option.when(home.count(Furniture.ArtisanBench) > 0)(Choice("HomeBench", content.text("home.house.bench"), row = Some(0))),
          Option.when(stored > 0)(Choice("HomeStore", content.format("home.store.button", "n" -> stored.toString), row = Some(1))),
          Some(backToHouse.copy(row = Some(1)))
        ).flatten)).as(StateType.Home)
      }
    }

  // ── Оружейная ───────────────────────────────────────────────────────────────

  /** Оружейная: у каждого дома своя — в домике чулан, в добротном доме светлая
    * комната, на острове целый зал. Ячеек — [[House.racks]]: в каждой стойка
    * со своим номером или пусто. Стойки ставит Юнона через «Обустройство»; есть
    * свободная ячейка — туда и кнопка. Кнопки стоек — по две в ряд. */
  private def armory(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house match {
        case None => office(user, renderer)
        case Some(h) =>
          val cells = (1 to home.racksMax).toList.map { n =>
            val what = home.racks.lift(n - 1).fold(content.text("home.slots.free")) { set =>
              content.format(if (Racks.real(set.allItems).isEmpty) "home.armory.rackEmpty" else "home.rack.title",
                "n" -> n.toString)
            }
            content.format("home.slots.line", "n" -> n.toString, "what" -> what)
          }
          val free = home.racks.size < home.racksMax
          val text = content.text(s"home.houses.${h.key}.armory") + "\n\n" +
            content.format("home.armory.count", "used" -> home.racks.size.toString, "max" -> home.racksMax.toString) +
            "\n" + cells.mkString("\n") +
            (if (free) "\n\n" + content.text("home.armory.buyHint") else "")
          val rackButtons = home.racks.indices.toList.map { i =>
            Choice("HomeRack", content.format("home.rack.button", "n" -> (i + 1).toString),
              data = Map("n" -> (i + 1).toString), row = Some(i / 2))
          }
          val lastRow = (home.racks.size + 1) / 2
          renderer.show(user, Screen(text, rackButtons ++ List(
            Option.when(free)(Choice("HomeFurnish", content.text("home.slots.furnish"), row = Some(lastRow))),
            Some(backToHouse.copy(row = Some(lastRow)))
          ).flatten)).as(StateType.Home)
      }
    }

  /** Стойка №n: что на ней висит; поменяться, надеть на стойку вещь из сумки,
    * снять одну вещь или всё в сумку, назад — в оружейную. */
  private def rack(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      rackNo(ua, home) match {
        case None => armory(user, renderer)
        case Some(n) =>
          val set   = home.racks(n - 1)
          val lines = EquipmentState.slots.map(s => s.name -> s.get(set)).filter(_._2.itemType != ItemType.NoItem)
            .map { case (slot, item) => content.format("home.rack.line", "slot" -> slot, "item" -> item.displayTitle) }
          val shown = if (lines.isEmpty) content.text("home.rack.empty") else lines.mkString("\n")
          val data  = Map("n" -> n.toString)
          renderer.show(user, Screen(content.format("home.rack.text", "n" -> n.toString, "set" -> shown), List(
            Choice("HomeRackSwap", content.text("home.rack.swap"), color = ChoiceColor.Positive, data = data, row = Some(0)),
            Choice("HomeRackPut", content.text("home.rack.put"), data = data, row = Some(0)),
            Choice("HomeRackOff", content.text("home.rack.off"), data = data, row = Some(1)),
            Choice("HomeRackTake", content.text("home.rack.take"), data = data, row = Some(1)),
            Choice("HomeArmory", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(2))
          ))).as(StateType.Home)
      }
    }

  /** «Поменяться» дома — то же, что ключ из сумки. */
  private def rackSwap(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      rackNo(ua, home) match {
        case None => armory(user, renderer)
        case Some(n) =>
          racks.swap(user, n, renderer).flatMap {
            case Left(why) => say(user, why, renderer) *> rack(user, ua, renderer)
            case Right(_)  => say(user, content.format("home.rack.swapped", "n" -> n.toString), renderer) *> rack(user, ua, renderer)
          }
      }
    }

  /** Снять со стойки всё в сумку — если в сумке хватит места. */
  private def rackTake(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res  <- rackNo(ua, home) match {
        case None => armory(user, renderer)
        case Some(n) =>
          val items = Racks.real(home.racks(n - 1).allItems)
          if (items.isEmpty) say(user, content.text("home.rack.emptyAlready"), renderer) *> rack(user, ua, renderer)
          else if (inv.freeSlots < items.count(!_.weightless).toLong)
            say(user, content.format("home.rack.noRoom", "n" -> items.size.toString), renderer) *> rack(user, ua, renderer)
          else for {
            _ <- saveHome(user, home.copy(racks = home.racks.updated(n - 1, Equipment.empty)))
            _ <- ZIO.foreachDiscard(items)(i => inventoryRepo.addItem(hero.id, i).mapError(e => new Throwable(e.toString)))
            _ <- say(user, content.format("home.rack.taken", "n" -> items.size.toString), renderer)
            s <- rack(user, ua, renderer)
          } yield s
      }
    } yield res

  /** Что из сумки можно надеть на стойку: то же, что на себя, — по страницам. */
  private def rackPutList(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res  <- rackNo(ua, home) match {
        case None => armory(user, renderer)
        case Some(n) =>
          val gear = inv.items.data.filter(HomeState.wearable)
          val back = Choice("HomeRack", content.text("home.house.back"), color = ChoiceColor.Negative, data = Map("n" -> n.toString))
          if (gear.isEmpty) renderer.show(user, Screen(content.text("home.rack.putNothing"), List(back.copy(row = Some(0))))).as(StateType.Home)
          else {
            val (pageItems, total, cur) = ItemMenu.page(gear, payload(ua, "p").flatMap(_.toIntOption).getOrElse(0))
            val buttons = pageItems.zipWithIndex.map { case (it, i) =>
              Choice("HomeRackPick", ItemMenu.truncate(it.displayTitle),
                data = Map("n" -> n.toString, "id" -> it.id.toString, "p" -> cur.toString), row = Some(i))
            }
            val nav = List(
              Option.when(cur > 0)(Choice("HomeRackPut", content.text("common.prev"),
                data = Map("n" -> n.toString, "p" -> (cur - 1).toString), row = Some(ItemMenu.NavRow))),
              Option.when(cur < total - 1)(Choice("HomeRackPut", content.text("common.next"),
                data = Map("n" -> n.toString, "p" -> (cur + 1).toString), row = Some(ItemMenu.NavRow)))
            ).flatten
            renderer.show(user, Screen(content.format("home.rack.putList", "n" -> n.toString,
              "page" -> (cur + 1).toString, "total" -> total.toString), buttons ++ nav :+ back.copy(row = Some(ItemMenu.NavRow + 1))))
              .as(StateType.Home)
          }
      }
    } yield res

  /** Надеть вещь из сумки на стойку — по тем же правилам, что на себя: по
    * уровню, кольцо и рука на выбор, если оба места заняты; снятое со стойки и
    * то, что не влезло в новый пояс, — в сумку. Сумка вмещает возвращённое. */
  private def rackPick(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      item  = payload(ua, "id").flatMap(_.toLongOption).flatMap(id => inv.items.data.find(i => i.id == id && HomeState.wearable(i)))
      res  <- (rackNo(ua, home), item) match {
        case (Some(n), Some(it)) =>
          val set  = home.racks(n - 1)
          val slot = payload(ua, "slot").flatMap(_.toIntOption)
          val page = payload(ua, "p").getOrElse("0")
          if (it.lvl > hero.lvl)
            say(user, content.format("inventory.tooHighLevel", "required" -> it.lvl.toString, "current" -> hero.lvl.toString), renderer) *>
              rackPutList(user, ua, renderer)
          else if (slot.isEmpty && SlotChoice.needed(set, it))
            renderer.show(user, SlotChoice.screen(content, it, set, "HomeRackPick",
              Map("n" -> n.toString, "id" -> it.id.toString, "p" -> page),
              Choice("HomeRackPut", content.text("home.house.back"), color = ChoiceColor.Negative,
                data = Map("n" -> n.toString, "p" -> page)))).as(StateType.Home)
          else {
            val done = InventoryState.lay(hero.copy(equipment = set), it, slot)
            InventoryState.equipBlock(inv, 0L, done.toBag, belt = it.itemType == ItemType.Belt) match {
              case Some(key) => say(user, content.format(key, "name" -> it.name), renderer) *> rackPutList(user, ua, renderer)
              case None =>
                for {
                  _       <- inventoryRepo.removeItem(it.id, hero.id).mapError(e => new Throwable(e.toString))
                  spilled <- BeltContents.persist(Some(itemRepo), hero.id, done.spilled)
                  _       <- ZIO.foreachDiscard(done.removed ++ spilled)(old => inventoryRepo.addItem(hero.id, old).ignore)
                  _       <- saveHome(user, home.copy(racks = home.racks.updated(n - 1, done.equipment)))
                  _       <- say(user, content.format("home.rack.putDone", "n" -> n.toString, "name" -> it.displayTitle,
                               "old" -> InventoryState.removedNames(done.removed)), renderer)
                  s       <- rack(user, ua, renderer)
                } yield s
            }
          }
        case (Some(_), None) => rackPutList(user, ua, renderer)
        case _               => armory(user, renderer)
      }
    } yield res

  /** Что снять со стойки по одной: по кнопке на занятое место. */
  private def rackOffList(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      rackNo(ua, home) match {
        case None => armory(user, renderer)
        case Some(n) =>
          val set  = home.racks(n - 1)
          val worn = EquipmentState.slots.zipWithIndex.filter { case (s, _) => s.get(set).itemType != ItemType.NoItem }
          if (worn.isEmpty) say(user, content.text("home.rack.emptyAlready"), renderer) *> rack(user, ua, renderer)
          else {
            val buttons = worn.zipWithIndex.map { case ((s, idx), i) =>
              Choice("HomeRackDrop", ItemMenu.truncate(content.format("home.rack.offButton", "slot" -> s.name,
                "item" -> s.get(set).displayTitle)), data = Map("n" -> n.toString, "s" -> idx.toString), row = Some(i / 2))
            }
            renderer.show(user, Screen(content.format("home.rack.offList", "n" -> n.toString), buttons :+
              Choice("HomeRack", content.text("home.house.back"), color = ChoiceColor.Negative,
                data = Map("n" -> n.toString), row = Some((worn.size + 1) / 2)))).as(StateType.Home)
          }
      }
    }

  /** Снять одну вещь со стойки в сумку — если в сумке есть место. */
  private def rackDrop(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res  <- (rackNo(ua, home), payload(ua, "s").flatMap(_.toIntOption).flatMap(EquipmentState.slots.lift)) match {
        case (Some(n), Some(slot)) =>
          val set  = home.racks(n - 1)
          val item = slot.get(set)
          if (item.itemType == ItemType.NoItem) rackOffList(user, ua, renderer)
          else if (!item.weightless && inv.freeSlots <= 0L) say(user, content.text("home.rack.offRoom"), renderer) *> rackOffList(user, ua, renderer)
          else for {
            _ <- inventoryRepo.addItem(hero.id, item).mapError(e => new Throwable(e.toString))
            _ <- saveHome(user, home.copy(racks = home.racks.updated(n - 1, slot.clear(set))))
            _ <- say(user, content.format("home.rack.offDone", "name" -> item.displayTitle), renderer)
            s <- rackOffList(user, ua, renderer)
          } yield s
        case (Some(_), None) => rackOffList(user, ua, renderer)
        case _               => armory(user, renderer)
      }
    } yield res

  // ── Верстак ремесленника ───────────────────────────────────────────────────

  /** Верстак: гнездо под куб Азата. Куба в гнезде нет — «Вставить куб»; есть —
    * «Открыть куб». */
  private def bench(user: User, renderer: Renderer): Task[StateType] =
    for {
      home <- loadHome(user)
      azat <- loadAzat(user)
      res  <- if (home.count(Furniture.ArtisanBench) == 0) slots(user, renderer)
              else {
                val action =
                  if (azat.cubeHome) Choice("HomeCube", content.text("home.bench.open"), color = ChoiceColor.Positive, row = Some(0))
                  else Choice("HomeCubeInsert", content.text("home.bench.insert"), color = ChoiceColor.Positive, row = Some(0))
                renderer.show(user, Screen(content.text(if (azat.cubeHome) "home.bench.textCube" else "home.bench.text"), List(
                  action, Choice("HomeSlots", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(1))
                ))).as(StateType.Home)
              }
    } yield res

  /** Вставить куб: он переезжает из Храма домой — насовсем. Куба нет или он
    * ещё спит — так и говорим. */
  private def insertCube(user: User, renderer: Renderer): Task[StateType] =
    for {
      home <- loadHome(user)
      azat <- loadAzat(user)
      res  <- if (home.count(Furniture.ArtisanBench) == 0) slots(user, renderer)
              else azat.cube match {
                case _ if azat.cubeHome => bench(user, renderer)
                case CubeStatus.Active =>
                  heroDao.writeAzatData(user.userId, azat.copy(cubeHome = true).asJson) *>
                    say(user, content.text("home.bench.inserted"), renderer) *> bench(user, renderer)
                case CubeStatus.FoundInactive => say(user, content.text("home.bench.cubeAsleep"), renderer) *> bench(user, renderer)
                case CubeStatus.None          => say(user, content.text("home.bench.noCube"), renderer) *> bench(user, renderer)
              }
    } yield res

  /** Открыть куб — тот же, что в Храме, только дома. */
  private def openCube(user: User, renderer: Renderer): Task[StateType] =
    for {
      home <- loadHome(user)
      azat <- loadAzat(user)
      res  <- if (azat.cubeHome && home.count(Furniture.ArtisanBench) > 0) ZIO.succeed(StateType.Cube)
              else bench(user, renderer)
    } yield res

  /** Книга рецептов — в библиотеке: что получалось в кубе на верстаке, по
    * страницам, по алфавиту, без отваров (см. [[HomeData.recipeBook]]). Без библиотеки
    * (домик) — обратно в дом: пишется книга и так. */
  private def recipes(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      if (!home.house.exists(_.tier >= House.Townhouse.tier) || home.recipeBook.isEmpty) library(user, renderer)
      else {
        val (lines, total, cur) = ItemMenu.page(home.recipeBook.sortBy(_.line)(ItemMenu.Alphabet),
                                    payload(ua, "p").flatMap(_.toIntOption).getOrElse(0), RecipesPerPage)
        val nav = List(
          Option.when(cur > 0)(Choice("HomeRecipes", content.text("common.prev"), data = Map("p" -> (cur - 1).toString), row = Some(0))),
          Option.when(cur < total - 1)(Choice("HomeRecipes", content.text("common.next"), data = Map("p" -> (cur + 1).toString), row = Some(0)))
        ).flatten
        renderer.show(user, Screen(content.format("home.library.recipes", "page" -> (cur + 1).toString,
          "total" -> total.toString, "lines" -> lines.map(n => "• " + n.line).mkString("\n")), nav :+
          Choice("HomeLibrary", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(1)))).as(StateType.Home)
      }
    }

  /** Номер стойки из кнопки — если такая стойка в доме есть. */
  private def rackNo(ua: UserAction, home: HomeData): Option[Int] =
    payload(ua, "n").flatMap(_.toIntOption).filter(n => n >= 1 && n <= home.racks.size)

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
                   AppButton.inRow(content, List(Choice(CleanStopKey, content.text("home.clean.stop"), color = ChoiceColor.Negative)))))
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
                     say(user, content.text("home.clean.done"), renderer) *> clutterFind(user)
                 else
                   saveHome(user, home.copy(cleanedMs = done)) *>
                     say(user, content.format("home.clean.paused", "left" -> duration(HomeRates.ClutterCleanMs - done)), renderer) *>
                     showHouse(user, renderer)
    } yield res

  /** Хлам разобран руками — на дне находка: случайная пурпурная вещь 4-го
    * уровня. Её экран — как у добычи: забрать, надеть или оставить; потом домой. */
  private def clutterFind(user: User): Task[StateType] =
    for {
      seed     <- Random.nextLong
      (item, _) = ItemGenerator.createItemAtLevel(HomeRates.ClutterFindLvl, HomeRates.ClutterFindRarity, Rng(seed))
      _        <- heroDao.writeSceneData(user.userId, LootState.LootData(items = List(item), silvers = Nil,
                    returnState = Some(StateType.Home), header = Some(content.text("home.clean.find"))).asJson)
    } yield StateType.Loot

  private def clearCleaning(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null) *> scheduler.cancel(user.userId, TaskKind.HomeClean)

  // ── «Обустройство» у Юноны ──────────────────────────────────────────────────

  /** Папка «Обустройство»: что совет поставит в дом — по дому и местам; хлам из
    * домика — вывезти за серебро. Без дома обустраивать нечего. Верстак и
    * посыльный — по одному: есть уже, в доме или в кладовой, — в папке их нет
    * (см. [[onlyOneOwned]]). */
  private def furnish(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      home.house match {
        case None => furnishStub(user, renderer)
        case Some(h) =>
          val clutter = Option.when(home.hasClutter)(Choice("HomeClutterPay",
            content.format("home.furnish.clutter", "price" -> money(HomeRates.ClutterSilver)),
            color = ChoiceColor.Positive, row = Some(0)))
          val items = Furniture.values.filter(f => f.minTier <= h.tier && !onlyOneOwned(f, home)).zipWithIndex.map { case (f, i) =>
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

  /** Вещь из папки: что даёт, сколько мест свободно (у стойки — ячеек в
    * оружейной), и «Купить за …». Верстак или посыльный уже есть (кнопка со
    * старого экрана) — «Купить» нет, вместо мест — почему. */
  private def furnItem(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      furnOf(ua, home) match {
        case None => furnish(user, renderer)
        case Some(f) =>
          val have = Option.when(onlyOneOwned(f, home))(buyRefusal(f, home)).flatten
          val free = if (f.takesSlot) content.format("home.furnish.free", "n" -> home.slotsFree.toString)
                     else content.format("home.furnish.freeArmory", "n" -> (home.racksMax - home.racks.size).max(0).toString)
          val text = furnText(f, "desc", home) + "\n\n" + have.getOrElse(free)
          val buy  = Option.when(have.isEmpty)(
            Choice("HomeFurnBuy", content.format("home.furnish.buy", "price" -> money(f.doubloons)),
              color = ChoiceColor.Positive, data = Map("f" -> f.key), row = Some(0)))
          renderer.show(user, Screen(text, buy.toList :+
            Choice("HomeFurnish", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(0))
          )).as(StateType.Home)
      }
    }

  /** Верстак и посыльный — по одному на героя: один уже есть, в доме или в
    * кладовой, — второго Юнона не продаёт. Покупку держит ещё и [[buyRefusal]]:
    * кнопка со старого экрана тоже ничего не купит. */
  private def onlyOneOwned(f: Furniture, home: HomeData): Boolean =
    (f == Furniture.ArtisanBench || f == Furniture.Courier) && home.owned(f) > 0

  /** Купить: нужны дублоны и место. Места нет — Юнона спрашивает, что спрятать
    * в кладовую ([[stashAsk]]); дублонов нет — до этого вопроса не доходит.
    * Вещь встаёт в дом сразу. Стойке место обстановки не нужно: ей нужна
    * ячейка оружейной, а её проверяет [[buyRefusal]]. */
  private def furnBuy(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      res  <- furnOf(ua, home) match {
        case None => furnish(user, renderer)
        case Some(f) =>
          buyRefusal(f, home).orElse(lackOf(hero, f)) match {
            case Some(why)                                 => say(user, why, renderer) *> furnish(user, renderer)
            case None if f.takesSlot && home.slotsFree <= 0 => stashAsk(user, home, Goal.Buy(f), renderer)
            case None                                      => place(user, hero, home, f, renderer)
          }
      }
    } yield res

  /** Почему эту вещь не купить, будь в доме хоть сколько места; `None` —
    * можно. Верстак и посыльный — по одному, стоек — сколько ячеек в
    * оружейной; то, что лежит в кладовой, второй раз не продают — его
    * возвращают. */
  private def buyRefusal(f: Furniture, home: HomeData): Option[String] = f match {
    case Furniture.ArtisanBench if home.count(f) > 0 => Some(content.text("home.furnish.benchHave"))
    case Furniture.Courier if home.count(f) > 0      => Some(content.text("home.furnish.courierHave"))
    case Furniture.ArtisanBench | Furniture.Courier if home.owned(f) > home.count(f) =>
      Some(content.text("home.furnish.inStore"))
    case Furniture.Rack if home.racks.size >= home.racksMax =>
      Some(content.format("home.furnish.rackMax", "max" -> home.racksMax.toString))
    case _ => None
  }

  /** Не хватает дублонов на вещь — сколько. */
  private def lackOf(hero: Hero, f: Furniture): Option[String] =
    Option.when(hero.doubloons < f.doubloons)(
      content.format("home.office.lack", "lack" -> priceText(f.doubloons - hero.doubloons, PayDoubloons)))

  /** Списать дублоны и поставить вещь в дом; стойку — в оружейную, на
    * следующий номер, и к ней — её номерной ключ. */
  private def place(user: User, hero: Hero, home: HomeData, f: Furniture, renderer: Renderer): Task[StateType] =
    charge(hero, PayDoubloons, f.doubloons).flatMap {
      case Left(lack) =>
        say(user, content.format("home.office.lack", "lack" -> priceText(lack, PayDoubloons)), renderer) *> furnish(user, renderer)
      case Right(_) if f == Furniture.Rack =>
        val bought = home.copy(racks = home.racks :+ Equipment.empty)
        val n      = bought.racks.size
        for {
          key <- itemRepo.persist(hero.id, QuestItemKind.item(QuestItemKind.rackKeys(n - 1)))
          _   <- inventoryRepo.addItem(hero.id, key).mapError(e => new Throwable(e.toString))
          _   <- saveHome(user, bought)
          _   <- say(user, furnText(f, "bought", bought) + "\n" + content.format("home.rack.keyGiven", "n" -> n.toString), renderer)
          s   <- furnish(user, renderer)
        } yield s
      case Right(_) =>
        val bought = home.copy(furniture = home.furniture :+ f.key)
        saveHome(user, bought) *> say(user, furnText(f, "bought", bought), renderer) *> furnish(user, renderer)
    }

  // ── Кладовая ────────────────────────────────────────────────────────────────

  /** «Что спрячем из текущей обстановки дома?» — мест нет, а вещь хотят купить
    * или вернуть из кладовой. На выбор — то, что стоит в доме и занимает
    * место; вещь того же рода прятать незачем. Прятать нечего — значит, мест
    * нет. */
  private def stashAsk(user: User, home: HomeData, goal: Goal, renderer: Renderer): Task[StateType] = {
    val kinds = Furniture.values.filter(k => k != goal.f && home.placed.contains(k))
    if (kinds.isEmpty) say(user, content.text("home.furnish.noSlot"), renderer) *> backFrom(user, goal, renderer)
    else {
      val buttons = kinds.zipWithIndex.map { case (k, i) =>
        Choice("HomeStash", ItemMenu.truncate(furnName(k)),
          data = Map("f" -> k.key, "for" -> goal.tag, "t" -> goal.f.key), row = Some(i))
      }
      val back = goal match {
        case _: Goal.Buy  => "HomeFurnish"
        case _: Goal.Back => "HomeStore"
      }
      renderer.show(user, Screen(content.text("home.stash.ask"), buttons :+
        Choice(back, content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(kinds.size))))
        .as(StateType.Home)
    }
  }

  /** Спрятать выбранное — и сразу сделать то, ради чего прятали: купить вещь
    * или вернуть другую из кладовой. Спрятать нельзя — Юнона говорит почему и
    * спрашивает снова. Покупку проверяем до того, как что-то спрятано: без
    * дублонов ничего не трогаем. */
  private def stash(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      azat <- loadAzat(user)
      res  <- goalOf(ua, home) match {
        case None => slots(user, renderer)
        case Some(goal) =>
          payload(ua, "f").flatMap(Furniture.byKey).filter(f => f != goal.f && home.placed.contains(f)) match {
            case None => stashAsk(user, home, goal, renderer)
            case Some(f) =>
              stashRefusal(f, home, azat) match {
                case Some(why) => say(user, why, renderer) *> stashAsk(user, home, goal, renderer)
                case None      => stashFor(user, hero, home, f, goal, renderer)
              }
          }
      }
    } yield res

  /** Место освобождено ради `goal`: купить или вернуть. Место есть и так (экран
    * устарел) — ничего не прячем. */
  private def stashFor(user: User, hero: Hero, home: HomeData, f: Furniture, goal: Goal,
                       renderer: Renderer): Task[StateType] = {
    val needed = home.slotsFree <= 0
    val freed  = if (needed) home.hidden(f) else home
    val hidden = ZIO.when(needed)(say(user, content.format("home.stash.hidden", "name" -> furnName(f)), renderer))
    goal match {
      case Goal.Buy(t) =>
        buyRefusal(t, home).orElse(lackOf(hero, t)) match {
          case Some(why) => say(user, why, renderer) *> furnish(user, renderer)
          case None      => saveHome(user, freed) *> hidden *> place(user, hero, freed, t, renderer)
        }
      case Goal.Back(t) =>
        if (!home.storedFurniture.contains(t)) store(user, renderer)
        else
          saveHome(user, freed.returned(t)) *> hidden *>
            say(user, content.format("home.store.returned", "name" -> furnName(t)), renderer) *>
            store(user, renderer)
    }
  }

  /** Почему это сейчас не спрятать; `None` — можно. Прячут только пустое: в
    * верстаке нет куба, полкам и хранилищу хватает места и без сундука. */
  private def stashRefusal(f: Furniture, home: HomeData, azat: AzatState): Option[String] = {
    val after = home.hidden(f)
    f match {
      case Furniture.ArtisanBench if azat.cubeHome => Some(content.text("home.stash.benchCube"))
      case Furniture.ApothecaryChest if home.herbs.size > after.herbShelves || home.brews.size > after.brewShelves =>
        Some(content.format("home.stash.shelves",
          "herbs" -> after.herbShelves.toString, "brews" -> after.brewShelves.toString))
      case Furniture.JewelerChest if home.gems.size > after.gemShelves =>
        Some(content.format("home.stash.gems", "gems" -> after.gemShelves.toString))
      case _ => None
    }
  }

  /** Кладовая: что спрятано, и «Вернуть» на каждый род вещей. Пусто — назад к
    * обстановке. */
  private def store(user: User, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      if (!home.owns) office(user, renderer)
      else if (home.storedFurniture.isEmpty) slots(user, renderer)
      else {
        val lines = home.storedFurniture.map(f => content.format("home.store.line", "name" -> furnName(f)))
        val kinds = Furniture.values.filter(home.storedFurniture.contains)
        val buttons = kinds.zipWithIndex.map { case (k, i) =>
          Choice("HomeUnstash", ItemMenu.truncate(content.format("home.store.return", "name" -> furnName(k))),
            data = Map("f" -> k.key), row = Some(i))
        }
        renderer.show(user, Screen(content.format("home.store.text", "lines" -> lines.mkString("\n")), buttons :+
          Choice("HomeSlots", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(kinds.size))))
          .as(StateType.Home)
      }
    }

  /** Вернуть из кладовой: есть место — вещь встаёт сразу; нет — Юнона
    * спрашивает, что спрятать взамен. */
  private def unstash(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    loadHome(user).flatMap { home =>
      payload(ua, "f").flatMap(Furniture.byKey).filter(home.storedFurniture.contains) match {
        case None                           => store(user, renderer)
        case Some(f) if home.slotsFree <= 0 => stashAsk(user, home, Goal.Back(f), renderer)
        case Some(f) =>
          saveHome(user, home.returned(f)) *>
            say(user, content.format("home.store.returned", "name" -> furnName(f)), renderer) *>
            store(user, renderer)
      }
    }

  /** Цель из кнопки «спрятать»: что купить (если это продают для этого дома) или
    * что вернуть из кладовой. Стойке место обстановки не нужно — ради неё ничего
    * не прячут (старая кнопка такого не сделает). */
  private def goalOf(ua: UserAction, home: HomeData): Option[Goal] =
    payload(ua, "t").flatMap(Furniture.byKey).filter(_.takesSlot).flatMap { t =>
      payload(ua, "for").flatMap {
        case Goal.BuyTag  => Option.when(home.house.exists(_.tier >= t.minTier))(Goal.Buy(t))
        case Goal.BackTag => Some(Goal.Back(t))
        case _            => None
      }
    }

  /** Назад туда, откуда спрашивали: к «Обустройству» или в кладовую. */
  private def backFrom(user: User, goal: Goal, renderer: Renderer): Task[StateType] = goal match {
    case _: Goal.Buy  => furnish(user, renderer)
    case _: Goal.Back => store(user, renderer)
  }

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
  private def furnText(f: Furniture, part: String, home: HomeData): String =
    content.format(s"home.furniture.${f.key}.$part",
      "herbs" -> HomeRates.ApothecaryHerbs.toString, "brews" -> HomeRates.ApothecaryBrews.toString,
      "gems" -> HomeRates.JewelerGems.toString,
      "racks" -> home.racks.size.toString, "racksMax" -> home.racksMax.toString, "n" -> home.racks.size.toString)

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
                   AppButton.inRow(content, List(Choice(WakeKey, content.text("home.bed.wake"), color = ChoiceColor.Negative)))))
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

  private def loadAzat(user: User): Task[AzatState] =
    nowMs.flatMap(now => AzatData.load(heroDao, user.userId, now))

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

  /** Ради чего прячут обстановку в кладовую: купить вещь или вернуть другую из
    * кладовой. `tag` — как цель записана в кнопке. */
  sealed trait Goal {
    def f: Furniture
    def tag: String
  }
  object Goal {
    val BuyTag: String  = "buy"
    val BackTag: String = "back"

    final case class Buy(f: Furniture) extends Goal { def tag: String = BuyTag }
    final case class Back(f: Furniture) extends Goal { def tag: String = BackTag }
  }

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

  /** Строк книги рецептов на странице. */
  val RecipesPerPage: Int = 10

  /** Что можно надеть на стойку: то же, что на себя, — снаряжение, кроме фляг
    * (их кладут в пояс) и сюжетного. */
  def wearable(item: pangea.model.item.Item): Boolean =
    ItemType.equippable.contains(item.itemType) && item.itemType != ItemType.Flask && !item.isQuestItem

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
