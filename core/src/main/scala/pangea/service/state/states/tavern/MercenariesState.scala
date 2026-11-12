package pangea.service.state.states.tavern

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{BrewKind, Item}
import pangea.model.squad.{AllyKind, AllyRates, Squad}
import pangea.model.quest.NpcQuests
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.service.purse.Purse
import pangea.model.hero.Achievement
import pangea.service.state.{CityExit, LostSquadQuest, NpcQuestLog, QuestSupport, SquadDuty, State, UserAction}
import pangea.util.TimeLeft
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Наёмники» в таверне: четверо за дальним столом — история, цена и «Нанять».
  * Кто уже в отряде или ушёл по свитку (на сутки) — за столом не сидит. Вернувшийся
  * встречает репликой о свитке и снова нанимается. Йорген берёт дублонами,
  * Плюх — Живой водой, Брамбл — шнапсом из красавки, Фаэлин — Ядовитой
  * смазкой и сверху дублонами (отваров — уровень на пять, но не меньше одного). */
case class MercenariesState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  bank:          Option[BankRepository] = None
) extends State {
  import MercenariesState._

  /** Кошель: своё серебро, а следом — ячейка в Торговом доме. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "MercList"      -> Target.Run { (u, _, r) => showList(u, r).as(StateType.Mercenaries) },
      "MercCard"      -> Target.Run { (u, ua, r) => withKind(ua, u, r)(k => showCard(u, k, r)).as(StateType.Mercenaries) },
      "MercHire"      -> Target.Run { (u, ua, r) => withKind(ua, u, r)(k => hire(u, k, r)).as(StateType.Mercenaries) },
      // Поднять из-за стола того, кто ещё отдыхает, — за серебро сверх обычной платы.
      "MercRush"      -> Target.Run { (u, ua, r) => withKind(ua, u, r)(k => showRush(u, k, r)).as(StateType.Mercenaries) },
      "MercRushHire"  -> Target.Run { (u, ua, r) => withKind(ua, u, r)(k => rush(u, k, r)).as(StateType.Mercenaries) },
      "BackFromMercs" -> Target.Goto(StateType.Tavern),
      CityExit.route
    ),
    fallback = Target.Run { (u, _, r) => showList(u, r).as(StateType.Mercenaries) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.Mercenaries

  override def enter(user: User, renderer: Renderer): Task[Unit] = showList(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Кто за столом: не в отряде и не в отлучке. Отработавшие свой день сперва
    * уходят, вернувшихся по свитку — встречаем. */
  private def showList(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      hero0 <- getHero(user)
      hero  <- SquadDuty.settle(heroDao, content, user, hero0, now, renderer)
      back   = hero.squad.returned(now)
      _    <- ZIO.foreachDiscard(back) { k =>
                renderer.show(user, Screen(content.format("mercenaries.returned",
                  "name" -> k.name, "line" -> content.text(s"mercenaries.${key(k)}.returned")), Nil))
              }
      squad = back.foldLeft(hero.squad)(_ welcomeBack _)
      _    <- ZIO.when(back.nonEmpty)(heroDao.updateSquad(user.userId, squad))
      quests <- NpcQuestLog.load(heroDao, user.userId)
      free  = available(squad, now, LostSquadQuest.bramblCalled(quests))
      // Кого за столом нет, потому что он в отлучке, — с тем, когда вернётся.
      away  = absent(squad, now, LostSquadQuest.bramblCalled(quests))
      gone  = away.map { case (k, at) =>
                content.format("mercenaries.away", "name" -> k.name, "left" -> TimeLeft.format(at - now))
              }
      head  = if (free.isEmpty) content.text("mercenaries.empty") else content.text("mercenaries.title")
      text  = if (gone.isEmpty) head else head + "\n\n" + gone.mkString("\n")
      buttons = free.map(k => Choice("MercCard", Choice.fit(k.name), data = Map("kind" -> k.entryName), row = Some(0)))
      // Кто отдыхает, того можно поднять раньше срока — за серебро.
      rushes  = away.map { case (k, _) => Choice("MercRush",
                  Choice.fit(content.format("mercenaries.rushButton", "name" -> k.name)),
                  data = Map("kind" -> k.entryName), row = Some(1)) }
      _    <- renderer.show(user, Screen(text, buttons ++ rushes ++ List(
                content.choice("BackFromMercs", "mercenaries.back").copy(row = Some(2)),
                CityExit.button(content, Some(2)))))
    } yield ()

  /** Карточка: история, реплика с ценой, «Нанять». Пока идёт «Пропавшая
    * дружина», у Брамбла своя: в первый раз он узнаёт фонарь старшины и
    * рассказывает, что знает, дальше просто зовёт в штольню — и идёт даром. */
  private def showCard(user: User, kind: AllyKind, renderer: Renderer): Task[Unit] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (kind == AllyKind.Gnome && LostSquadQuest.bramblCalled(quests)) showSquadCard(user, quests, renderer)
      else showPlainCard(user, kind, renderer)
    }

  private def showSquadCard(user: User, quests: NpcQuests, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      told  = LostSquadQuest.bramblTold(quests)
      _    <- ZIO.unless(told)(LostSquadQuest.told(heroDao, user.userId))
      story = content.text("lostSquad.brambl." + (if (told) "again" else LostSquadQuest.key(hero, "story")))
      _    <- renderer.show(user, Screen(story + "\n\n" + content.text("lostSquad.brambl.free"), List(
                content.choice("MercHire", "lostSquad.brambl.takeLabel").copy(data = Map("kind" -> AllyKind.Gnome.entryName), row = Some(0)),
                content.choice("MercList", "mercenaries.toList").copy(row = Some(1)))))
    } yield ()

  private def showPlainCard(user: User, kind: AllyKind, renderer: Renderer): Task[Unit] =
    getHero(user).zip(NpcQuestLog.load(heroDao, user.userId)).flatMap { case (hero, quests) =>
      val story = content.text(s"mercenaries.${key(kind)}.story")
      val price = kind match {
        case AllyKind.Human => content.format("mercenaries.human.price", "cost" -> AllyRates.HumanDoubloons.toString)
        case AllyKind.Elf   => content.format("mercenaries.elf.price",
          "n" -> AllyKind.brewsFor(hero.lvl).toString, "cost" -> AllyRates.ElfDoubloons.toString)
        // Брамбл помнит «Пропавшую дружину»: вывел — бутылка всегда, бросил — вдвое.
        case AllyKind.Gnome if LostSquadQuest.bramblFriend(quests) => content.text("lostSquad.brambl.priceFriend")
        case AllyKind.Gnome if LostSquadQuest.squadAbandoned(quests) =>
          content.format("lostSquad.brambl.priceGrudge", "n" -> LostSquadQuest.bramblBrews(quests, hero.lvl).toString)
        case k              => content.format(s"mercenaries.${key(k)}.price", "n" -> AllyKind.brewsFor(hero.lvl).toString)
      }
      renderer.show(user, Screen(story + "\n\n" + price, List(
        content.choice("MercHire", "mercenaries.hireLabel").copy(data = Map("kind" -> kind.entryName), row = Some(0)),
        content.choice("MercList", "mercenaries.toList").copy(row = Some(1)))))
    }

  /** Найм: место в отряде, плата, и наёмник встаёт на первую свободную позицию
    * строя, а мест там нет — в резерв. Брамбл, пока дружина ждёт, идёт даром —
    * и отдых его не держит. */
  private def hire(user: User, kind: AllyKind, renderer: Renderer): Task[Unit] =
    for {
      now    <- nowMs
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      called  = LostSquadQuest.bramblCalled(quests)
      free    = called && kind == AllyKind.Gnome
      _      <-
        if (!available(hero.squad, now, called).contains(kind)) showList(user, renderer)
        else if (!hero.squad.canTake)
          renderer.show(user, Screen(content.text("mercenaries.squadFull"), Nil)) *> showCard(user, kind, renderer)
        else (if (free) ZIO.none else pay(user, hero, kind, quests)).flatMap {
          case Some(refusal) => renderer.show(user, Screen(refusal, Nil)) *> showCard(user, kind, renderer)
          case None =>
            // Нанят, отдохнув как положено, — переработки забыты.
            val squad = hero.squad.welcomeBack(kind).rested(kind).hire(kind, hero.lvl, now)
            heroDao.updateSquad(user.userId, squad) *>
              renderer.show(user, Screen(content.format(if (free) "lostSquad.brambl.joined" else "mercenaries.hired",
                "name" -> kind.name), Nil)) *>
              reserveNote(user, squad, kind, renderer) *>
              showList(user, renderer)
        }
    } yield ()

  /** Встал в резерв, а не в строй, — скажем, где его искать. */
  private def reserveNote(user: User, squad: Squad, kind: AllyKind, renderer: Renderer): Task[Unit] =
    ZIO.when(squad.reserve.exists(_.kind == kind))(
      renderer.show(user, Screen(content.format("squad.inReserve", "name" -> kind.name), Nil))).unit

  /** Сценка «поднять раньше срока»: наёмник готов встать без отдыха — за
    * серебро сверх обычной платы. Каждый следующий раз дороже. */
  private def showRush(user: User, kind: AllyKind, renderer: Renderer): Task[Unit] =
    for {
      now    <- nowMs
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <-
        if (!absent(hero.squad, now, LostSquadQuest.bramblCalled(quests)).exists(_._1 == kind)) showList(user, renderer)
        else {
          val silver = rushSilver(hero.squad, kind)
          val line   = content.format(s"mercenaries.${key(kind)}.rush",
            "silver" -> silver.toString, "cost" -> doubloonsFor(kind).toString,
            "n" -> brewCount(kind, hero, quests).toString)
          renderer.show(user, Screen(content.text("mercenaries.rushIntro") + "\n\n" + line, List(
            Choice("MercRushHire", Choice.fit(content.format("mercenaries.rushLabel", "silver" -> silver.toString)),
              data = Map("kind" -> kind.entryName), row = Some(0)),
            content.choice("MercList", "mercenaries.toList").copy(row = Some(1)))))
        }
    } yield ()

  /** Поднять отдыхающего: серебро и обычная плата, и он в отряде. Больше
    * десяти таких раз — «Переработки наше всё». */
  private def rush(user: User, kind: AllyKind, renderer: Renderer): Task[Unit] =
    for {
      now    <- nowMs
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      silver  = rushSilver(hero.squad, kind)
      _      <-
        if (!absent(hero.squad, now, LostSquadQuest.bramblCalled(quests)).exists(_._1 == kind)) showList(user, renderer)
        else if (!hero.squad.canTake)
          renderer.show(user, Screen(content.text("mercenaries.squadFull"), Nil)) *> showRush(user, kind, renderer)
        else purse.wallet(hero).flatMap { wallet =>
          if (!wallet.canAfford(silver))
            renderer.show(user, Screen(content.format("mercenaries.notEnoughSilver",
              "cost" -> silver.toString, "have" -> wallet.total.toString), Nil)) *> showRush(user, kind, renderer)
          else pay(user, hero, kind, quests).flatMap {
          case Some(refusal) => renderer.show(user, Screen(refusal, Nil)) *> showRush(user, kind, renderer)
          case None =>
            val squad = hero.squad.rushed(kind, hero.lvl, now)
            for {
              // Йорген и Фаэлин берут дублонами: героя читаем заново, чтобы не затереть их списание.
              fresh <- getHero(user)
              _     <- purse.charge(user.userId, fresh, silver)
              _     <- heroDao.updateSquad(user.userId, squad)
              _     <- renderer.show(user, Screen(content.format("mercenaries.rushed", "name" -> kind.name), Nil))
              _     <- reserveNote(user, squad, kind, renderer)
              _     <- ZIO.when(squad.overtimeTotal > OvertimeGoal)(
                         QuestSupport.grant(heroDao, content, user, fresh, Achievement.Overtime, renderer))
              _     <- showList(user, renderer)
            } yield ()
          }
        }
    } yield ()

  /** Сколько бутылок берут Плюх и Брамбл — как при обычном найме. */
  private def brewCount(kind: AllyKind, hero: Hero, quests: NpcQuests): Long =
    if (kind == AllyKind.Gnome) LostSquadQuest.bramblBrews(quests, hero.lvl) else AllyKind.brewsFor(hero.lvl)

  /** Списать плату; текст отказа, если платить нечем. Брамбл берёт по
    * памяти о «Пропавшей дружине» (см. LostSquadQuest.bramblBrews). */
  private def pay(user: User, hero: Hero, kind: AllyKind, quests: NpcQuests): Task[Option[String]] =
    kind match {
      case AllyKind.Human =>
        // Йорген берёт дублонами и всегда одну и ту же цену: в ячейке их не
        // держат, так что кошель тут ни при чём.
        val cost = AllyRates.HumanDoubloons
        if (hero.doubloons < cost)
          ZIO.succeed(Some(content.format("mercenaries.notEnoughDoubloons",
            "cost" -> cost.toString, "have" -> hero.doubloons.toString)))
        else heroDao.updateDoubloons(user.userId, hero.doubloons - cost).as(None)
      case AllyKind.Elf =>
        // Фаэлин берёт и смазкой (по уровню), и дублонами (всегда столько же):
        // чего-то не хватает — не берём ничего.
        val cost = AllyRates.ElfDoubloons
        val n    = brewCount(AllyKind.Elf, hero, quests).toInt
        inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
          val salves = brews(inv.items.data, BrewKind.VenomSalve)
          if (salves.size < n)
            ZIO.succeed(Some(content.format("mercenaries.notEnoughBrews",
              "n" -> n.toString, "brew" -> BrewKind.VenomSalve.label, "have" -> salves.size.toString)))
          else if (hero.doubloons < cost)
            ZIO.succeed(Some(content.format("mercenaries.notEnoughDoubloons",
              "cost" -> cost.toString, "have" -> hero.doubloons.toString)))
          else
            inventoryRepo.removeItems(salves.take(n).map(_.id).toSet, hero.id).mapError(e => new Throwable(e.toString)) *>
              heroDao.updateDoubloons(user.userId, hero.doubloons - cost).as(None)
        }
      case k =>
        val brew = brewFor(k)
        val n    = brewCount(k, hero, quests).toInt
        inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
          val bottles = brews(inv.items.data, brew)
          if (bottles.size < n)
            ZIO.succeed(Some(content.format("mercenaries.notEnoughBrews",
              "n" -> n.toString, "brew" -> brew.label, "have" -> bottles.size.toString)))
          else
            inventoryRepo.removeItems(bottles.take(n).map(_.id).toSet, hero.id)
              .mapError(e => new Throwable(e.toString)).as(None)
        }
    }

  private def withKind(ua: UserAction, user: User, renderer: Renderer)(f: AllyKind => Task[Unit]): Task[Unit] =
    ua.payload
      .flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("kind")))
      .flatMap(AllyKind.withNameOption)
      .fold(showList(user, renderer))(f)

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object MercenariesState {

  /** Шаг цены «поднять раньше срока»: первый раз — столько серебра, каждый
    * следующий без отдыха — ещё столько же сверху. */
  val RushSilverStep: Long = 5000L

  /** Больше стольких подъёмов за серебро — «Переработки наше всё». */
  val OvertimeGoal: Int = 10

  /** Сколько серебра стоит поднять его сейчас. */
  def rushSilver(squad: Squad, kind: AllyKind): Long = RushSilverStep * (squad.overtimeOf(kind) + 1)

  /** Ключ в scenes.yaml: `mercenaries.<key>`. Нежити и крысы за столом не
   *  бывает — первую поднимают на алтаре, вторая сама выскакивает в бою; ключи
   *  им заведены, чтобы ветка была полной. */
  def key(kind: AllyKind): String = kind match {
    case AllyKind.Human  => "human"
    case AllyKind.Murloc => "murloc"
    case AllyKind.Gnome  => "gnome"
    case AllyKind.Elf    => "elf"
    case AllyKind.Undead => "undead"
    case AllyKind.Rat    => "rat"
    case AllyKind.SquadGnome => "squadGnome"
    case AllyKind.MurlocKin  => "murlocKin"
    case AllyKind.HedgeElf   => "hedgeElf"
    case AllyKind.CaravanKhajiit => "caravanKhajiit"
  }

  /** Кто сейчас за столом. Пока идёт «Пропавшая дружина» (`bramblCalled`),
    * Брамбл сидит там, даже если только что ушёл отдыхать. */
  def available(squad: Squad, nowMs: Long, bramblCalled: Boolean = false): List[AllyKind] =
    AllyKind.hireable.filterNot { k =>
      squad.has(k) || (squad.isAway(k, nowMs) && !(k == AllyKind.Gnome && bramblCalled))
    }

  /** Кто в отлучке и когда вернётся — в том же порядке, что и за столом. Кто в
    * отряде, тот не в отлучке; Брамбл, пока его зовёт дружина, сидит за столом. */
  def absent(squad: Squad, nowMs: Long, bramblCalled: Boolean = false): List[(AllyKind, Long)] =
    AllyKind.hireable.filterNot(k => squad.has(k) || (k == AllyKind.Gnome && bramblCalled))
      .flatMap(k => squad.backAt(k, nowMs).map(k -> _))


  /** Сколько дублонов берут за найм: Йорген — только их, Фаэлин — сверх смазки. */
  def doubloonsFor(kind: AllyKind): Long = kind match {
    case AllyKind.Human => AllyRates.HumanDoubloons
    case AllyKind.Elf   => AllyRates.ElfDoubloons
    case _              => 0L
  }

  /** Чем берут Плюх, Брамбл и Фаэлин. */
  def brewFor(kind: AllyKind): BrewKind = kind match {
    case AllyKind.Gnome => BrewKind.Schnapps
    case AllyKind.Elf   => BrewKind.VenomSalve
    case _              => BrewKind.LivingWater
  }

  def brews(items: List[Item], kind: BrewKind): List[Item] = items.filter(_.brew.contains(kind))
}
