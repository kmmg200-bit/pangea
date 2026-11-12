package pangea.service.state.states.hero

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.squad.{Ally, AllyKind, AllyRates}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.squad.SquadOrders
import pangea.service.state.{SquadDuty, State, UserAction}
import zio.{Task, ZIO}

/** «Отряд» в меню персонажа: строй с позициями, карточки союзников и героя —
  * состояние и место, перестановка (занятое место — меняются местами, в том
  * числе с героем) и увольнение. Встать можно на любое место; в бою строй
  * съезжает к месту 1, а промежутки между бойцами остаются (см.
  * Squad.frontShift).
  *
  * «Резерв» — кто при герое, но не в строю (см. Squad.reserve): туда уходят
  * из карточки союзника, а оттуда выставляют в строй (мест нет — меняются с
  * тем, кого назовут) или выгоняют. В резерве союзника адресуют номером в нём
  * и именем: список мог сдвинуться, пока игрок думал. */
case class SquadState(heroDao: HeroDao, content: SceneContent) extends State {

  /** Приказы отряду — общие с Mini App. */
  private val orders = new SquadOrders(heroDao, content)

  private val branch = new Branch(
    routes = Map(
      "SquadList"   -> Target.Run { (u, _, r) => showList(u, r).as(StateType.Squad) },
      "SquadAlly"   -> Target.Run { (u, ua, r) => withAlly(ua)(p => showAlly(u, p, r)).as(StateType.Squad) },
      "SquadMove"   -> Target.Run { (u, ua, r) => move(u, ua, r).as(StateType.Squad) },
      "SquadHero"   -> Target.Run { (u, _, r) => showHero(u, r).as(StateType.Squad) },
      "SquadHeroMove" -> Target.Run { (u, ua, r) => moveHero(u, ua, r).as(StateType.Squad) },
      "SquadDismiss" -> Target.Run { (u, ua, r) => withAlly(ua)(p => confirmDismiss(u, p, r)).as(StateType.Squad) },
      "SquadDismissDo" -> Target.Run { (u, ua, r) => withAlly(ua)(p => dismiss(u, p, r)).as(StateType.Squad) },
      "SquadRise"   -> Target.Run { (u, ua, r) => withAlly(ua)(p => takeRisenPlace(u, p, r)).as(StateType.Squad) },
      "SquadRiseNo" -> Target.Run { (u, _, r) => dismissRisen(u, r).as(StateType.Squad) },
      "SquadToReserve" -> Target.Run { (u, ua, r) => withAlly(ua)(p => toReserve(u, p, r)).as(StateType.Squad) },
      "SquadReserve"   -> Target.Run { (u, _, r) => showReserve(u, r).as(StateType.Squad) },
      "SquadReserveAlly" -> Target.Run { (u, ua, r) =>
        withReserved(ua)((i, who) => showReserveAlly(u, i, who, r)).as(StateType.Squad) },
      "SquadDeploy" -> Target.Run { (u, ua, r) =>
        withReserved(ua)((i, who) => deploy(u, i, who, None, r)).as(StateType.Squad) },
      "SquadSwapIn" -> Target.Run { (u, ua, r) =>
        withReserved(ua)((i, who) => deploy(u, i, who, parseAlly(ua), r)).as(StateType.Squad) },
      "SquadReserveDismiss" -> Target.Run { (u, ua, r) =>
        withReserved(ua)((i, who) => confirmReserveDismiss(u, i, who, r)).as(StateType.Squad) },
      "SquadReserveDismissDo" -> Target.Run { (u, ua, r) =>
        withReserved(ua)((i, who) => dismissReserve(u, i, who, r)).as(StateType.Squad) },
      "BackFromSquad" -> Target.Goto(StateType.HeroStats)
    ),
    fallback = Target.Run { (u, _, r) => showList(u, r).as(StateType.Squad) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.Squad

  override def enter(user: User, renderer: Renderer): Task[Unit] = showList(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Строй: все места, на каждом герой, союзник или пусто. */
  private def showList(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      hero0 <- getHero(user)
      hero  <- SquadDuty.settle(heroDao, content, user, hero0, now, renderer)
      // Восставший ждёт у шатра, а мест в строю нет: сперва решим, кем за него
      // пожертвовать, — как у алтаря в пещере.
      _     <- hero.squad.pendingRisen.fold(showLines(user, hero, renderer))(_ => askRisen(user, hero, renderer))
    } yield ()

  /** Экран «кем жертвуем» для восставшего: весь отряд кнопками и отказ. */
  private def askRisen(user: User, hero: Hero, renderer: Renderer): Task[Unit] =
    hero.squad.pendingRisen.fold(ZIO.unit: Task[Unit]) { form =>
      val buttons = hero.squad.inOrder.zipWithIndex.map { case (a, i) =>
        Choice("SquadRise", Choice.fit(a.name), data = Map("pos" -> a.position.toString),
          row = Some(i / SquadState.PerRow))
      }
      val rows = (hero.squad.allies.size + SquadState.PerRow - 1) / SquadState.PerRow
      val no   = content.choice("SquadRiseNo", "squad.risenNo").copy(color = ChoiceColor.Negative, row = Some(rows))
      renderer.show(user, Screen(content.format("squad.risenWaits", "name" -> form.name), buttons :+ no))
    }

  /** Ждущий занимает место названного союзника — тот покидает отряд. */
  private def takeRisenPlace(user: User, pos: Int, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      orders.takeRisenPlace(user, hero, pos, renderer)
        .flatMap(squad => showLines(user, hero.copy(squad = squad.getOrElse(hero.squad)), renderer))
    }

  /** Ждущего отпускают: тело оседает прахом. */
  private def dismissRisen(user: User, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      orders.dismissRisen(user, hero, renderer)
        .flatMap(squad => showLines(user, hero.copy(squad = squad.getOrElse(hero.squad)), renderer))
    }

  private def showLines(user: User, hero: Hero, renderer: Renderer): Task[Unit] = {
      val lines = (1 to AllyRates.Positions).map { pos =>
        if (pos == hero.squad.heroPos) content.format("squad.lineHero", "n" -> pos.toString)
        else hero.squad.allyAt(pos) match {
          case Some(a) =>
            val s = a.statsAt(hero.lvl)
            content.format("squad.lineAlly", "n" -> pos.toString, "name" -> a.name,
              "hp" -> a.hp.toString, "maxHp" -> s.hp.toString, "armor" -> a.armor.toString, "maxArmor" -> s.armor.toString)
          case None => content.format("squad.lineEmpty", "n" -> pos.toString)
        }
      }
      // Союзников бывает до десяти, и герой среди них — по двое в ряд, в порядке
      // строя, «Назад» под ними: в клавиатуру ВК это укладывается с запасом.
      val entries = (None :: hero.squad.allies.map(Some(_))).sortBy(_.fold(hero.squad.heroPos)(_.position))
      val buttons = entries.zipWithIndex.map {
        case (None, i)    => content.choice("SquadHero", "squad.heroLabel").copy(row = Some(i / SquadState.PerRow))
        case (Some(a), i) =>
          Choice("SquadAlly", Choice.fit(a.name), data = Map("pos" -> a.position.toString), row = Some(i / SquadState.PerRow))
      }
      val rows = (entries.size + SquadState.PerRow - 1) / SquadState.PerRow
      // Последний ряд: резерв (сколько в нём) и «Назад».
      val reserve = Choice("SquadReserve", content.format("squad.reserveLabel",
        "n" -> hero.squad.reserve.size.toString, "max" -> AllyRates.ReserveSize.toString), row = Some(rows))
      val back = content.choice("BackFromSquad", "squad.back").copy(row = Some(rows))
      renderer.show(user, Screen(content.text("squad.title") + "\n" + lines.mkString("\n"), buttons :+ reserve :+ back))
  }

  /** Подстановки карточки союзника: кто, состояние и статы. Крыса бьёт без
    * стихии — в её карточке на этом месте зубы. */
  private def cardArgs(a: Ally, hero: Hero): Seq[(String, String)] = {
    val s = a.statsAt(hero.lvl)
    Seq("name" -> a.name, "race" -> a.kind.race.toString,
      "element" -> SquadState.strikeLabel(content, a.kind),
      "hp" -> a.hp.toString, "maxHp" -> s.hp.toString, "armor" -> a.armor.toString, "maxArmor" -> s.armor.toString,
      "energy" -> a.energy.toString, "maxEnergy" -> s.energy.toString,
      "atk" -> s.atk.toString, "accuracy" -> s.accuracy.toString, "defence" -> s.defence.toString, "evasion" -> s.evasion.toString)
  }

  /** Карточка союзника: статы, текущее состояние, позиция, кнопки. Ключ здесь
    * — место в строю: поднятых с алтаря в отряде может быть несколько, и вид
    * их не различает. */
  private def showAlly(user: User, at: Int, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      hero.squad.allyAt(at) match {
        case None => showList(user, renderer)
        case Some(a) =>
          val text = content.format("squad.card", (cardArgs(a, hero) :+ ("pos" -> a.position.toString)): _*)
          // Мест в строю одиннадцать — кнопки перестановки идут по трое в ряд.
          val moves = (1 to AllyRates.Positions).filter(_ != a.position).zipWithIndex.map { case (pos, i) =>
            val who = if (pos == hero.squad.heroPos) content.text("squad.posHero")
                      else hero.squad.allyAt(pos).map(x => Choice.fit(x.name)).getOrElse(content.text("squad.posFree"))
            Choice("SquadMove", Choice.fit(content.format("squad.moveLabel", "n" -> pos.toString, "who" -> who)),
              data = Map("pos" -> a.position.toString, "to" -> pos.toString), row = Some(i / SquadState.MovesPerRow))
          }.toList
          val rows = (moves.size + SquadState.MovesPerRow - 1) / SquadState.MovesPerRow
          // «В резерв» — рядом с «Выгнать»; крысу туда не берут.
          val toReserve = Option.when(hero.squad.reservable(a))(content.choice("SquadToReserve", "squad.toReserveLabel")
            .copy(data = Map("pos" -> a.position.toString), row = Some(rows)))
          val dismiss = content.choice("SquadDismiss", "squad.dismissLabel")
            .copy(data = Map("pos" -> a.position.toString), row = Some(rows))
          val back = content.choice("SquadList", "squad.toList").copy(row = Some(rows + 1))
          renderer.show(user, Screen(text, moves ++ toReserve.toList ++ List(dismiss, back)))
      }
    }

  // ── Резерв ──────────────────────────────────────────────────────────────────

  /** Союзник из строя — в резерв; дальше снова строй. */
  private def toReserve(user: User, at: Int, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap(orders.toReserve(user, _, at, renderer)) *> showList(user, renderer)

  /** Резерв: кто в нём, списком и кнопками по двое в ряд, «К отряду» под ними.
    * Сроки у резерва идут как в строю — ушедших сперва отпускаем. */
  private def showReserve(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      hero0 <- getHero(user)
      hero  <- SquadDuty.settle(heroDao, content, user, hero0, now, renderer)
      _     <- renderer.show(user, reserveScreen(hero))
    } yield ()

  private def reserveScreen(hero: Hero): Screen = {
    val reserve = hero.squad.reserve
    val max     = AllyRates.ReserveSize.toString
    val toList  = content.choice("SquadList", "squad.toList")
    if (reserve.isEmpty) Screen(content.format("squad.reserveEmpty", "max" -> max), List(toList))
    else {
      val lines = reserve.zipWithIndex.map { case (a, i) =>
        val s = a.statsAt(hero.lvl)
        content.format("squad.lineAlly", "n" -> (i + 1).toString, "name" -> a.name,
          "hp" -> a.hp.toString, "maxHp" -> s.hp.toString, "armor" -> a.armor.toString, "maxArmor" -> s.armor.toString)
      }
      val buttons = reserve.zipWithIndex.map { case (a, i) =>
        Choice("SquadReserveAlly", Choice.fit(a.name), data = reserved(i, a), row = Some(i / SquadState.PerRow))
      }
      val rows = (reserve.size + SquadState.PerRow - 1) / SquadState.PerRow
      Screen(content.format("squad.reserveTitle", "n" -> reserve.size.toString, "max" -> max) + "\n" + lines.mkString("\n"),
        buttons :+ toList.copy(row = Some(rows)))
    }
  }

  /** Карточка союзника в резерве: состояние и статы, «Выставить в отряд»,
    * «Выгнать» и «К резерву». */
  private def showReserveAlly(user: User, i: Int, who: String, renderer: Renderer): Task[Unit] =
    withReservedAlly(user, i, who, renderer) { (hero, a) =>
      val data = reserved(i, a)
      renderer.show(user, Screen(content.format("squad.reserveCard", cardArgs(a, hero): _*), List(
        content.choice("SquadDeploy", "squad.deployLabel").copy(data = data, color = ChoiceColor.Positive, row = Some(0)),
        content.choice("SquadReserveDismiss", "squad.dismissLabel").copy(data = data, row = Some(0)),
        content.choice("SquadReserve", "squad.toReserveList").copy(row = Some(1)))))
    }

  /** Из резерва — в строй: на свободное место, а мест нет — вместо того, кого
    * назовут (`swapAt`). Не назвали, а мест нет, — спросим, кого отправить в
    * резерв взамен. */
  private def deploy(user: User, i: Int, who: String, swapAt: Option[Int], renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      orders.deploy(user, hero, i, who, swapAt, renderer).flatMap {
        case Some(_) => showReserve(user, renderer)
        case None =>
          hero.squad.reserve.lift(i).filter(_.name == who) match {
            case Some(a) if hero.squad.full => askSwapIn(user, hero, i, a, renderer)
            case _                          => showReserve(user, renderer)
          }
      }
    }

  /** Мест в строю нет: кого отправить в резерв взамен — весь строй кнопками,
    * кроме крыс (им в резерве не место), и «Назад» к карточке. */
  private def askSwapIn(user: User, hero: Hero, i: Int, a: Ally, renderer: Renderer): Task[Unit] = {
    val line    = hero.squad.inOrder.filter(hero.squad.reservable)
    val buttons = line.zipWithIndex.map { case (b, k) =>
      Choice("SquadSwapIn", Choice.fit(b.name), data = reserved(i, a) + ("pos" -> b.position.toString),
        row = Some(k / SquadState.PerRow))
    }
    val rows = (line.size + SquadState.PerRow - 1) / SquadState.PerRow
    val back = content.choice("SquadReserveAlly", "squad.back").copy(data = reserved(i, a), row = Some(rows))
    renderer.show(user, Screen(content.format("squad.deploySwap", "name" -> a.name), buttons :+ back))
  }

  private def confirmReserveDismiss(user: User, i: Int, who: String, renderer: Renderer): Task[Unit] =
    withReservedAlly(user, i, who, renderer) { (_, a) =>
      val data = reserved(i, a)
      renderer.show(user, Screen(dismissAsk(a), List(
        Choice("SquadReserveDismissDo", content.text("squad.dismissYes"), data = data, color = ChoiceColor.Negative, row = Some(0)),
        Choice("SquadReserveAlly", content.text("squad.dismissNo"), data = data, row = Some(0)))))
    }

  private def dismissReserve(user: User, i: Int, who: String, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap(orders.dismissReserve(user, _, i, who, renderer)) *> showReserve(user, renderer)

  /** Союзник из резерва по номеру и имени; ушёл или сдвинулся — снова резерв. */
  private def withReservedAlly(user: User, i: Int, who: String, renderer: Renderer)(f: (Hero, Ally) => Task[Unit]): Task[Unit] =
    getHero(user).flatMap { hero =>
      hero.squad.reserve.lift(i).filter(_.name == who).fold(showReserve(user, renderer))(a => f(hero, a))
    }

  /** Адрес союзника в резерве для кнопки: номер в нём и имя. */
  private def reserved(i: Int, a: Ally): Map[String, String] = Map("r" -> i.toString, "who" -> a.name)

  private def withReserved(ua: UserAction)(f: (Int, String) => Task[Unit]): Task[Unit] =
    (payloadField(ua, "r").flatMap(_.toIntOption), payloadField(ua, "who")) match {
      case (Some(i), Some(who)) => f(i, who)
      case _                    => ZIO.unit
    }

  /** Карточка героя: его место и кнопки, куда встать. Занятое место — союзник
    * оттуда встаёт на прежнее место героя. */
  private def showHero(user: User, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      val at    = hero.squad.heroPos
      val moves = (1 to AllyRates.Positions).filter(_ != at).zipWithIndex.map { case (pos, i) =>
        val who = hero.squad.allyAt(pos).map(x => Choice.fit(x.name)).getOrElse(content.text("squad.posFree"))
        Choice("SquadHeroMove", Choice.fit(content.format("squad.moveLabel", "n" -> pos.toString, "who" -> who)),
          data = Map("to" -> pos.toString), row = Some(i / SquadState.MovesPerRow))
      }.toList
      val rows = (moves.size + SquadState.MovesPerRow - 1) / SquadState.MovesPerRow
      val back = content.choice("SquadList", "squad.toList").copy(row = Some(rows))
      renderer.show(user, Screen(content.format("squad.heroCard", "pos" -> at.toString), moves :+ back))
    }

  private def moveHero(user: User, ua: UserAction, renderer: Renderer): Task[Unit] =
    payloadField(ua, "to").flatMap(_.toIntOption) match {
      case None => showList(user, renderer)
      case Some(to) =>
        getHero(user).flatMap(orders.moveHero(user, _, to, renderer)) *> showHero(user, renderer)
    }

  private def move(user: User, ua: UserAction, renderer: Renderer): Task[Unit] =
    (parseAlly(ua), payloadField(ua, "to").flatMap(_.toIntOption)) match {
      case (Some(from), Some(to)) =>
        getHero(user).flatMap(orders.move(user, _, from, to, renderer)).flatMap {
          case None    => showList(user, renderer)
          case Some(_) => showAlly(user, to, renderer)
        }
      case _ => showList(user, renderer)
    }

  /** «Выгнать?» — наёмника потом можно нанять снова, прочие (поднятые, крыса)
    * уходят насовсем: в таверне таких не нанимают. */
  private def dismissAsk(a: Ally): String =
    content.format(if (AllyKind.hireable.contains(a.kind)) "squad.dismissConfirm" else "squad.dismissConfirmGone",
      "name" -> a.name)

  private def confirmDismiss(user: User, at: Int, renderer: Renderer): Task[Unit] =
    withAllyAt(user, at, renderer) { a =>
      renderer.show(user, Screen(dismissAsk(a), List(
        Choice("SquadDismissDo", content.text("squad.dismissYes"), data = Map("pos" -> at.toString), color = ChoiceColor.Negative, row = Some(0)),
        Choice("SquadAlly", content.text("squad.dismissNo"), data = Map("pos" -> at.toString), row = Some(0)))))
    }

  private def dismiss(user: User, at: Int, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap(orders.dismiss(user, _, at, renderer)) *> showList(user, renderer)

  private def withAllyAt(user: User, at: Int, renderer: Renderer)(f: Ally => Task[Unit]): Task[Unit] =
    getHero(user).flatMap(_.squad.allyAt(at).fold(showList(user, renderer))(f))

  private def withAlly(ua: UserAction)(f: Int => Task[Unit]): Task[Unit] =
    parseAlly(ua).fold[Task[Unit]](ZIO.unit)(f)

  private def parseAlly(ua: UserAction): Option[Int] =
    payloadField(ua, "pos").flatMap(_.toIntOption)

  private def payloadField(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object SquadState {

  /** Чем бьёт союзник — в его карточке на месте стихии: значок стихии, у
    * Фаэлина — яд, у крысы — зубы. */
  def strikeLabel(content: SceneContent, kind: pangea.model.squad.AllyKind): String =
    kind.element.map(_.emoji).getOrElse(
      content.text(if (kind.poisonPct > 0L) "squad.poisonStrike" else "squad.noElement"))
  /** Сколько кнопок союзников и перестановок помещается в ряд клавиатуры. */
  val PerRow: Int      = 2
  val MovesPerRow: Int = 3
}
