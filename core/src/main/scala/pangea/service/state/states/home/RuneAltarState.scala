package pangea.service.state.states.home

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor, jawn}
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.home.HomeData
import pangea.model.item.Item
import pangea.model.rune.{Rune, RuneData, RuneStone, RuneStoneSize}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.{ItemMenu, State, UiScene, UserAction}
import zio.{Task, ZIO}

/** Рунический алтарь в доме (есть в любом доме: Юнона не стала снимать его с
  * домика, а в домах выше он переезжает вместе с героем). Рунные камни ложатся
  * на него одной кнопкой и становятся счётом узоров по каждой руне: большая —
  * пять, малая — одна ([[RuneStoneSize.points]]). Места на алтаре не считают.
  *
  * Вещи с рунами алтарь тоже берёт — «Сдать вещи»: каждая рассыпается узорами
  * своей руны по редкости ([[Rune.points]]), в тот же счёт, что и камни. Какие
  * редкости уходят — та же настройка, что у Казимира для «Сдать всё»
  * (`RuneData.burnRarities`).
  *
  * По руне — две вещи: углубить понимание её узорами, как у Казимира (до
  * потолка по уровню, остаток ждёт на алтаре), и забрать камнями в сумку:
  * сколько пятёрок — столько больших, остаток — малыми. Что не влезло в сумку,
  * остаётся на алтаре. Счёт живёт в `home_data` (`HomeData.runes`). */
case class RuneAltarState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "AltarMenu"   -> Target.Run { (u, ua, r) => menu(u, page(ua, "p"), r) },
      "AltarPutAll" -> Target.Run { (u, _, r) => putAll(u, r) },
      "AltarGive"       -> Target.Run { (u, _, r) => giveConfirm(u, r) },
      "AltarGiveYes"    -> Target.Run { (u, _, r) => giveAll(u, r) },
      "AltarSettings"   -> Target.Run { (u, _, r) => settings(u, r) },
      "AltarBurnRarity" -> Target.Run { (u, ua, r) => toggleRarity(u, ua, r) },
      "AltarBurnSets"   -> Target.Run { (u, _, r) => toggleSets(u, r) },
      "AltarRune"   -> Target.Run { (u, ua, r) => runeScreen(u, ua, r) },
      "AltarDeepen" -> Target.Run { (u, ua, r) => ask(u, ua, RuneAltarState.Deepen, r) },
      "AltarTake"   -> Target.Run { (u, ua, r) => ask(u, ua, RuneAltarState.Take, r) },
      "LeaveAltar"  -> Target.Run { (u, _, _) => clearPending(u).as(StateType.Home) }
    ),
    // Написанное у алтаря: число узоров, если алтарь его ждёт; иначе — сам алтарь.
    fallback = Target.Run { (u, ua, r) => onText(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = menu(user, 0, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Описание алтаря, сколько на нём узоров, руны кнопками по страницам;
    * «Положить все руны», «Сдать вещи», «Что сдавать» и «Назад». */
  private def menu(user: User, p: Int, renderer: Renderer): Task[StateType] =
    clearPending(user) *> loadHome(user).flatMap { home =>
      // Без дома и алтаря нет — обратно к Юноне.
      if (!home.owns) ZIO.succeed(StateType.Home)
      else {
        val runes = onAltar(home)
        val total = home.runes.values.sum
        val (pageRunes, totalPages, cur) = ItemMenu.page(runes, p)
        val buttons = pageRunes.zipWithIndex.map { case ((rune, n), i) =>
          Choice("AltarRune", ItemMenu.truncate(content.format("home.altar.runeLine", "name" -> rune.label, "n" -> n.toString)),
            data = Map("k" -> rune.key, "rp" -> cur.toString), row = Some(i))
        }
        val nav = List(
          Option.when(cur > 0)(Choice("AltarMenu", content.text("common.prev"), data = Map("p" -> (cur - 1).toString), row = Some(ItemMenu.NavRow))),
          Option.when(cur < totalPages - 1)(Choice("AltarMenu", content.text("common.next"), data = Map("p" -> (cur + 1).toString), row = Some(ItemMenu.NavRow)))
        ).flatten
        val tail = List(
          Choice("AltarPutAll", content.text("home.altar.putAll"), color = ChoiceColor.Positive, row = Some(ItemMenu.NavRow + 1)),
          Choice("AltarGive", content.text("home.altar.give"), color = ChoiceColor.Positive, row = Some(ItemMenu.NavRow + 1)),
          Choice("AltarSettings", content.text("home.altar.settings"), row = Some(ItemMenu.NavRow + 1)),
          Choice("LeaveAltar", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow + 1))
        )
        // Под кнопками только значки — их расшифровка идёт текстом над счётом узоров.
        val text = content.text("home.altar.text") + "\n\n" + content.text("home.altar.legend") + "\n\n" +
          (if (total == 0L) content.text("home.altar.empty") else content.format("home.altar.total", "n" -> total.toString))
        renderer.show(user, Screen(text, buttons ++ nav ++ tail)).as(StateType.RuneAltar)
      }
    }

  /** Все рунные камни из сумки — на алтарь, счётом по рунам. */
  private def putAll(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      inv    <- inventoryRepo.get(hero.id).mapError(asThrowable)
      stones  = inv.items.data.flatMap(i => i.runeStone.map(rs => (i, rs)))
      res    <- if (stones.isEmpty) say(user, content.text("home.altar.nothingToPut"), renderer) *> menu(user, 0, renderer)
                else {
                  val added = stones.groupMapReduce(_._2.runeKey)(_._2.size.points)(_ + _)
                  for {
                    home <- loadHome(user)
                    _    <- inventoryRepo.removeItems(stones.map(_._1.id).toSet, hero.id).mapError(asThrowable)
                    _    <- saveHome(user, added.foldLeft(home) { case (h, (k, n)) => h.withRune(k, h.runeCount(k) + n) })
                    lines = added.toList.flatMap { case (k, n) => Rune.byKey(k).map(r => (r.label, n)) }.sortBy(_._1)
                              .map { case (name, n) => content.format("home.altar.putLine", "name" -> name, "n" -> n.toString) }
                    _    <- say(user, content.format("home.altar.put", "lines" -> lines.mkString("\n")), renderer)
                    s    <- menu(user, 0, renderer)
                  } yield s
                }
    } yield res

  // ── Вещи с рунами ───────────────────────────────────────────────────────────

  /** Сколько вещей уйдёт по «Сдать вещи» — и спросить, точно ли: вещи
    * рассыпаются, обратно их не собрать. */
  private def giveConfirm(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      plan  = RuneAltarState.givePlan(hero, inv.items.data)
      res  <- if (plan.isEmpty) say(user, content.text("home.altar.nothingToGive"), renderer) *> menu(user, 0, renderer)
              else renderer.show(user, Screen(content.format("home.altar.giveConfirm", "n" -> plan.size.toString), List(
                Choice("AltarGiveYes", content.text("home.altar.giveYes"), color = ChoiceColor.Positive, row = Some(0)),
                Choice("AltarMenu", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(0))
              ))).as(StateType.RuneAltar)
    } yield res

  /** Вещи с рунами из сумки — узорами на алтарь: каждая по своей редкости. */
  private def giveAll(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      plan  = RuneAltarState.givePlan(hero, inv.items.data)
      res  <- if (plan.isEmpty) say(user, content.text("home.altar.nothingToGive"), renderer) *> menu(user, 0, renderer)
              else {
                val added = plan.groupMapReduce(_._2.key)(p => Rune.points(p._1.rarity))(_ + _)
                for {
                  home <- loadHome(user)
                  _    <- inventoryRepo.removeItems(plan.map(_._1.id).toSet, hero.id).mapError(asThrowable)
                  _    <- saveHome(user, added.foldLeft(home) { case (h, (k, n)) => h.withRune(k, h.runeCount(k) + n) })
                  lines = added.toList.flatMap { case (k, n) => Rune.byKey(k).map(r => (r.label, n)) }.sortBy(_._1)
                            .map { case (name, n) => content.format("home.altar.putLine", "name" -> name, "n" -> n.toString) }
                  _    <- say(user, content.format("home.altar.given", "n" -> plan.size.toString,
                            "lines" -> lines.mkString("\n")), renderer)
                  s    <- menu(user, 0, renderer)
                } yield s
              }
    } yield res

  /** Что сдавать: редкости переключателями — та же настройка, что у Казимира. */
  private def settings(user: User, renderer: Renderer): Task[StateType] =
    getHero(user).flatMap(hero => renderer.show(user, settingsScreen(hero.runes))).as(StateType.RuneAltar)

  private def settingsScreen(data: RuneData): Screen = {
    def state(on: Boolean) = content.text(if (on) "merchant.junk.on" else "merchant.junk.off")
    def color(on: Boolean) = if (on) ChoiceColor.Positive else ChoiceColor.Negative
    val groups  = MerchantState.JunkRarityGroups
    val buttons = groups.zipWithIndex.map { case (g, i) =>
      val on = g.rarities.forall(data.burns)
      Choice("AltarBurnRarity", content.format("merchant.junk.rarity", "emoji" -> g.emoji, "state" -> state(on)),
        color = if (on) ChoiceColor.Positive else ChoiceColor.Negative, data = Map("g" -> g.id),
        row = Some(i / MerchantState.JunkRaritiesPerRow))
    }
    val rows = (groups.size + MerchantState.JunkRaritiesPerRow - 1) / MerchantState.JunkRaritiesPerRow
    // Вещи наборов — своим переключателем: выключенный их бережёт.
    val sets = Choice("AltarBurnSets", content.format("merchant.junk.sets", "state" -> state(data.burnSets)),
      color = color(data.burnSets), row = Some(rows))
    Screen(content.text("home.altar.settingsText"),
      (buttons :+ sets) :+ Choice("AltarMenu", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(rows + 1)))
  }

  /** Переключить защиту вещей наборов — настройка общая с Казимиром. */
  private def toggleSets(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      updated = hero.runes.copy(burnSets = !hero.runes.burnSets)
      _      <- heroDao.updateRunes(user.userId, updated)
      _      <- renderer.show(user, settingsScreen(updated))
    } yield StateType.RuneAltar

  private def toggleRarity(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      res  <- payload(ua, "g").flatMap(id => MerchantState.JunkRarityGroups.find(_.id == id)) match {
        case None    => settings(user, renderer)
        case Some(g) =>
          val updated = hero.runes.toggleBurn(g.rarities)
          heroDao.updateRunes(user.userId, updated) *>
            renderer.show(user, settingsScreen(updated)).as(StateType.RuneAltar)
      }
    } yield res

  /** Экран руны: сколько узоров, сколько выйдет камнями, понимание; углубить,
    * забрать, назад — на ту страницу списка, с которой руну выбрали. */
  private def runeScreen(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    runeScreenOf(user, runeOf(ua), page(ua, "rp"), renderer)

  private def runeScreenOf(user: User, picked: Option[Rune], rp: Int, renderer: Renderer): Task[StateType] =
    for {
      _    <- clearPending(user)
      hero <- getHero(user)
      home <- loadHome(user)
      res  <- picked.filter(r => home.runeCount(r.key) > 0L) match {
        case None => menu(user, rp, renderer)
        case Some(rune) =>
          val n    = home.runeCount(rune.key)
          val data = Map("k" -> rune.key, "rp" -> rp.toString)
          val text = content.format("home.altar.rune", "name" -> rune.label, "n" -> n.toString,
            "big" -> (n / RuneStone.PiecesPerBig).toString, "small" -> (n % RuneStone.PiecesPerBig).toString,
            "u" -> hero.runes.understandingOf(rune).toString, "cap" -> Rune.cap(hero.lvl).toString)
          renderer.show(user, Screen(text, List(
            Choice("AltarDeepen", content.text("home.altar.deepen"), color = ChoiceColor.Positive, data = data, row = Some(0)),
            Choice("AltarTake", content.text("home.altar.take"), data = data, row = Some(0)),
            Choice("AltarMenu", content.text("home.house.back"), color = ChoiceColor.Negative,
              data = Map("p" -> rp.toString), row = Some(1))
          ))).as(StateType.RuneAltar)
      }
    } yield res

  /** «Углубить» и «Забрать» сперва спрашивают, сколько узоров: алтарь
    * запоминает, над какой руной и что делать, и ждёт числа (см. [[onText]]). */
  private def ask(user: User, ua: UserAction, act: String, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      rp    = page(ua, "rp")
      res  <- runeOf(ua).filter(r => home.runeCount(r.key) > 0L) match {
        case None => say(user, content.text("home.altar.gone"), renderer) *> menu(user, rp, renderer)
        case Some(rune) =>
          val n    = home.runeCount(rune.key)
          val text =
            if (act == RuneAltarState.Deepen)
              content.format("home.altar.askDeepen", "name" -> rune.label, "n" -> n.toString,
                "u" -> hero.runes.understandingOf(rune).toString, "cap" -> Rune.cap(hero.lvl).toString)
            else content.format("home.altar.askTake", "name" -> rune.label, "n" -> n.toString)
          writeScene(user, RuneAltarState.AltarScene(Some(act), Some(rune.key), rp)) *>
            renderer.show(user, Screen(text, List(
              Choice("AltarRune", content.text("home.house.back"), color = ChoiceColor.Negative,
                data = Map("k" -> rune.key, "rp" -> rp.toString), row = Some(0))
            ))).as(StateType.RuneAltar)
      }
    } yield res

  /** Написанное у алтаря: число узоров для того, что алтарь ждёт; ждать нечего —
    * просто алтарь. Больше, чем лежит, — берётся сколько есть. */
  private def onText(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    readScene(user).flatMap { scene =>
      (scene.act, scene.rune.flatMap(Rune.byKey)) match {
        case (Some(act), Some(rune)) =>
          ua.text.filterNot(_.isWhitespace).toLongOption.filter(_ > 0L) match {
            case None =>
              say(user, content.text("home.altar.notNumber"), renderer) *>
                ask(user, UserAction("", Some(Map("k" -> rune.key, "rp" -> scene.page.toString).asJson.noSpaces)),
                  act, renderer)
            case Some(n) =>
              clearPending(user) *>
                (if (act == RuneAltarState.Deepen) deepen(user, rune, n, scene.page, renderer)
                 else take(user, rune, n, scene.page, renderer))
          }
        case _ => menu(user, 0, renderer)
      }
    }

  /** Углубить понимание `asked` узорами с алтаря (не больше, чем лежит) — как у
    * Казимира, до потолка по уровню; лишнее остаётся лежать. */
  private def deepen(user: User, rune: Rune, asked: Long, rp: Int, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      home <- loadHome(user)
      n     = home.runeCount(rune.key)
      res  <-
        if (n <= 0L) say(user, content.text("home.altar.gone"), renderer) *> menu(user, rp, renderer)
        else {
          val cap = Rune.cap(hero.lvl)
          val (updated, gained) = hero.runes.deepen(rune, asked.min(n), cap)
          if (gained == 0L)
            say(user, content.format("home.altar.atCap", "name" -> rune.label, "max" -> cap.toString), renderer) *>
              runeScreenOf(user, Some(rune), rp, renderer)
          else for {
            _ <- heroDao.updateRunes(user.userId, updated)
            _ <- saveHome(user, home.withRune(rune.key, n - gained))
            _ <- say(user, content.format("home.altar.deepened", "name" -> rune.label,
                   "u" -> updated.understandingOf(rune).toString, "gain" -> gained.toString,
                   "left" -> (n - gained).toString), renderer)
            s <- if (n - gained > 0L) runeScreenOf(user, Some(rune), rp, renderer) else menu(user, rp, renderer)
          } yield s
        }
    } yield res

  /** Забрать в сумку `asked` узоров (не больше, чем лежит) камнями: пятёрки —
    * большими (по слоту на каждую), остаток — малыми (у них свой предел в
    * сумке). Что не влезло — остаётся на алтаре. */
  private def take(user: User, rune: Rune, asked: Long, rp: Int, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      home <- loadHome(user)
      n     = home.runeCount(rune.key)
      res  <-
        if (n <= 0L) say(user, content.text("home.altar.gone"), renderer) *> menu(user, rp, renderer)
        else {
          val want     = asked.min(n)
          val per      = RuneStone.PiecesPerBig.toLong
          val bigFit   = (want / per).min(inv.freeSlots)
          val smallFit = (want % per).min((Item.HoardLimit - inv.hoardCount(s"rune:${rune.key}")).max(0).toLong)
          val left     = n - bigFit * per - smallFit
          if (bigFit + smallFit == 0L)
            say(user, content.text("home.altar.noRoom"), renderer) *> runeScreenOf(user, Some(rune), rp, renderer)
          else for {
            stones <- ZIO.foreach(List.fill(bigFit.toInt)(RuneStoneSize.Big) ++ List.fill(smallFit.toInt)(RuneStoneSize.Small)) { size =>
                        itemRepo.persist(hero.id, RuneStone.item(rune, size))
                      }
            _      <- ZIO.foreachDiscard(stones)(s => inventoryRepo.addItem(hero.id, s).mapError(asThrowable))
            _      <- saveHome(user, home.withRune(rune.key, left))
            line    = content.format("home.altar.taken", "big" -> bigFit.toString, "small" -> smallFit.toString) +
                        (if (left > 0L) "\n" + content.format("home.altar.takenLeft", "left" -> left.toString) else "")
            _      <- say(user, line, renderer)
            s      <- if (left > 0L) runeScreenOf(user, Some(rune), rp, renderer) else menu(user, rp, renderer)
          } yield s
        }
    } yield res

  private def readScene(user: User): Task[RuneAltarState.AltarScene] =
    UiScene.read(heroDao, user.userId, UiScene.Altar, RuneAltarState.AltarScene())

  private def writeScene(user: User, scene: RuneAltarState.AltarScene): Task[Unit] =
    UiScene.write(heroDao, user.userId, UiScene.Altar, scene)

  /** Снять ожидание числа — если оно было: лишней записи не делаем. */
  private def clearPending(user: User): Task[Unit] =
    readScene(user).flatMap(s => ZIO.when(s.act.isDefined)(UiScene.clear(heroDao, user.userId, UiScene.Altar)).unit)

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  /** Руны на алтаре — по алфавиту, только те, где узоры есть. */
  private def onAltar(home: HomeData): List[(Rune, Long)] =
    home.runes.toList.collect { case (k, n) if n > 0L => Rune.byKey(k).map(_ -> n) }.flatten.sortBy(_._1.label)

  private def say(user: User, text: String, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(text, Nil))

  private def runeOf(ua: UserAction): Option[Rune] = payload(ua, "k").flatMap(Rune.byKey)

  private def page(ua: UserAction, key: String): Int = payload(ua, key).flatMap(_.toIntOption).getOrElse(0)

  private def payload(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def loadHome(user: User): Task[HomeData] =
    heroDao.readHomeData(user.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))

  private def saveHome(user: User, home: HomeData): Task[Unit] =
    heroDao.writeHomeData(user.userId, home.asJson)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object RuneAltarState {

  /** Что алтарь ждёт числом: вложить узоры в понимание или забрать в сумку. */
  val Deepen: String = "deepen"
  val Take: String   = "take"

  /** Ожидание числа у алтаря: что делать (`act`), с какой руной и на какую
    * страницу списка вернуться. Пусто — ничего не ждёт. */
  final case class AltarScene(act: Option[String] = None, rune: Option[String] = None, page: Int = 0)
  object AltarScene {
    implicit val encoder: Encoder[AltarScene] = deriveEncoder
    implicit val decoder: Decoder[AltarScene] = (c: HCursor) =>
      for {
        act  <- c.getOrElse[Option[String]]("act")(None)
        rune <- c.getOrElse[Option[String]]("rune")(None)
        page <- c.getOrElse[Int]("page")(0)
      } yield AltarScene(act, rune, page)
  }

  /** Что уйдёт по «Сдать вещи»: вещи с рунами тех редкостей, что отмечены в
    * настройке (она общая с Казимиром), — каждая вместе со своей руной. Рунные
    * камни сюда не идут: для них «Положить все руны». */
  def givePlan(hero: Hero, items: List[Item]): List[(Item, Rune)] =
    items.filter(i => i.runeStone.isEmpty && !i.isQuestItem && hero.runes.burnsGear(i))
      .flatMap(i => Rune.of(i).map(i -> _))
}
