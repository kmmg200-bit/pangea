package pangea.service.state.states.home

import io.circe.jawn
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.home.HomeData
import pangea.model.item.Item
import pangea.model.monster.{Monster, NamedTrophy, Race}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.{ItemMenu, State, UserAction}
import zio.{Task, ZIO}

/** 🏆 Стена трофеев — встроенная, в доме на острове. На неё вешают уникальные
  * трофеи именных мобов ([[NamedTrophy]]): каждый висящий — прибавка к урону
  * героя по расе хозяина (в бою — `SoloPveBattle.trophyMult`). Повешенное
  * висит насовсем.
  *
  * На экране — сколько собрано по расам; нажал на расу — чьи трофеи висят, а
  * на месте остальных пусто. «Повесить трофеи» — уникальные трофеи из сумки,
  * которых на стене ещё нет: по одному или все разом. Живёт в `home_data`
  * (`HomeData.wall`). */
case class TrophyWallState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {
  import TrophyWallState._

  private val branch = new Branch(
    routes = Map(
      "WallMenu"   -> Target.Run { (u, _, r) => menu(u, r) },
      "WallRace"   -> Target.Run { (u, ua, r) => raceScreen(u, ua, r) },
      "WallHang"   -> Target.Run { (u, ua, r) => hangList(u, page(ua), r) },
      "WallPut"    -> Target.Run { (u, ua, r) => put(u, ua, r) },
      "WallPutAll" -> Target.Run { (u, _, r) => putAll(u, r) },
      "LeaveWall"  -> Target.Goto(StateType.Home)
    ),
    fallback = Target.Run { (u, _, r) => menu(u, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = menu(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Сколько собрано — всего и по расам, с прибавкой к урону; расы кнопками. */
  private def menu(user: User, renderer: Renderer): Task[StateType] =
    withWall(user) { home =>
      for {
        bag <- hangable(user, home)
        lines = Races.map(r => raceLine(r, home))
        text  = content.format("home.wall.text", "n" -> home.wall.distinct.size.toString,
                  "total" -> NamedTrophy.titles.size.toString, "lines" -> lines.mkString("\n"))
        races = Races.zipWithIndex.map { case (r, i) =>
                  Choice("WallRace", content.format("home.wall.raceButton", "race" -> raceName(r),
                    "n" -> hung(home, r).size.toString, "total" -> namesOf(r).size.toString),
                    data = Map("r" -> r.entryName), row = Some(i / RacesPerRow))
                }
        tailRow = (Races.size + RacesPerRow - 1) / RacesPerRow
        tail = List(
                 Choice("WallHang", content.format("home.wall.hang", "n" -> bag.size.toString),
                   color = ChoiceColor.Positive, row = Some(tailRow)),
                 Choice("LeaveWall", content.text("home.house.back"), color = ChoiceColor.Negative, row = Some(tailRow + 1))
               )
        s <- renderer.show(user, Screen(text, races ++ tail)).as(StateType.TrophyWall)
      } yield s
    }

  /** Трофеи одной расы: висящие — по названию, на месте прочих пусто. */
  private def raceScreen(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWall(user) { home =>
      payload(ua, "r").flatMap(Race.withNameOption).filter(Races.contains) match {
        case None => menu(user, renderer)
        case Some(race) =>
          val lines = namesOf(race).map { n =>
            if (home.wall.contains(n)) s"${Item.NamedTrophyMark} ${NamedTrophy.titles.getOrElse(n, n)}"
            else content.text("home.wall.empty")
          }
          val text = content.format("home.wall.race", "line" -> raceLine(race, home), "lines" -> lines.mkString("\n"))
          renderer.show(user, Screen(text, List(
            Choice("WallMenu", content.text("home.wall.back"), color = ChoiceColor.Negative, row = Some(0))
          ))).as(StateType.TrophyWall)
      }
    }

  /** Что из сумки можно повесить — кнопками по страницам, и «Повесить все». */
  private def hangList(user: User, p: Int, renderer: Renderer): Task[StateType] =
    withWall(user) { home =>
      hangable(user, home).flatMap { bag =>
        if (bag.isEmpty) say(user, content.text("home.wall.nothing"), renderer) *> menu(user, renderer)
        else {
          val (pageItems, totalPages, cur) = ItemMenu.page(bag, p)
          val buttons = pageItems.zipWithIndex.map { case (it, i) =>
            Choice("WallPut", ItemMenu.truncate(content.format("home.wall.putOne", "name" -> it.name)),
              data = Map("id" -> it.id.toString, "p" -> cur.toString), row = Some(i))
          }
          val nav = List(
            Option.when(cur > 0)(Choice("WallHang", content.text("common.prev"), data = Map("p" -> (cur - 1).toString), row = Some(ItemMenu.NavRow))),
            Option.when(cur < totalPages - 1)(Choice("WallHang", content.text("common.next"), data = Map("p" -> (cur + 1).toString), row = Some(ItemMenu.NavRow)))
          ).flatten
          val tail = List(
            Choice("WallPutAll", content.text("home.wall.putAll"), color = ChoiceColor.Positive, row = Some(ItemMenu.NavRow + 1)),
            Choice("WallMenu", content.text("home.wall.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow + 1))
          )
          renderer.show(user, Screen(content.format("home.wall.pick", "n" -> bag.size.toString), buttons ++ nav ++ tail))
            .as(StateType.TrophyWall)
        }
      }
    }

  /** Повесить один трофей. Такой уже висит — так и говорим, трофей остаётся в сумке. */
  private def put(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    withWall(user) { home =>
      for {
        hero <- getHero(user)
        inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
        wanted = payload(ua, "id").flatMap(_.toLongOption)
                   .flatMap(id => inv.items.data.find(i => i.id == id && i.id != 0L))
                   .flatMap(i => i.namedTrophy.map(i -> _))
        s <- wanted match {
               case None => hangList(user, page(ua), renderer)
               case Some((item, named)) if home.wall.contains(named.monster) =>
                 say(user, content.format("home.wall.already", "name" -> item.name), renderer) *> hangList(user, page(ua), renderer)
               case Some((item, named)) =>
                 val hungHome = home.copy(wall = home.wall :+ named.monster)
                 inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable) *>
                   saveHome(user, hungHome) *>
                   say(user, hungLine(item.name, named.monster, hungHome), renderer) *>
                   hangList(user, page(ua), renderer)
             }
      } yield s
    }

  /** Повесить всё, чего на стене нет; повторы из сумки не трогаем. */
  private def putAll(user: User, renderer: Renderer): Task[StateType] =
    withWall(user) { home =>
      for {
        hero <- getHero(user)
        bag  <- hangable(user, home)
        s    <- if (bag.isEmpty) say(user, content.text("home.wall.nothing"), renderer) *> menu(user, renderer)
                else {
                  val monsters = bag.flatMap(_.namedTrophy).map(_.monster)
                  inventoryRepo.removeItems(bag.map(_.id).toSet, hero.id).mapError(asThrowable) *>
                    saveHome(user, home.copy(wall = home.wall ++ monsters)) *>
                    say(user, content.format("home.wall.hungAll", "names" -> bag.map(_.name).mkString(", ")), renderer) *>
                    menu(user, renderer)
                }
      } yield s
    }

  // ── Вспомогательное ─────────────────────────────────────────────────────────

  /** Уникальные трофеи из сумки, которых на стене ещё нет, — по одному на
    * именного (второй такой же в сумке ждать не будет: висит же первый). */
  private def hangable(user: User, home: HomeData): Task[List[Item]] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
    } yield TrophyWallState.hangable(inv.items.data, home.wall)

  /** «Орки — 2 из 12 · +2% урона против орков». */
  private def raceLine(race: Race, home: HomeData): String = {
    val n   = hung(home, race).size
    val pct = home.wallPct.getOrElse(race.entryName, 0)
    content.format("home.wall.raceLine", "race" -> raceName(race), "n" -> n.toString, "total" -> namesOf(race).size.toString) +
      (if (pct > 0) content.format("home.wall.raceBonus", "pct" -> pct.toString, "races" -> race.genitivePlural) else "")
  }

  /** «Вы вешаете на стену …. Против орков: +2% урона.» */
  private def hungLine(name: String, monster: String, home: HomeData): String =
    NamedTrophy.raceOf(monster).fold(content.format("home.wall.hungPlain", "name" -> name)) { race =>
      content.format("home.wall.hung", "name" -> name, "races" -> race.genitivePlural,
        "pct" -> home.wallPct.getOrElse(race.entryName, 0).toString)
    }

  private def raceName(race: Race): String = TrophyWallState.raceName(content, race)

  /** Стена — только на острове; нет — обратно в дом. */
  private def withWall(user: User)(f: HomeData => Task[StateType]): Task[StateType] =
    loadHome(user).flatMap(home => if (home.hasWall) f(home) else ZIO.succeed(StateType.Home))

  private def say(user: User, text: String, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(text, Nil))

  private def page(ua: UserAction): Int = payload(ua, "p").flatMap(_.toIntOption).getOrElse(0)

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

object TrophyWallState {

  /** Раса во множественном числе для стены: «Орки», «Каджиты». */
  def raceName(content: SceneContent, race: Race): String = content.text(s"home.wall.races.${race.entryName}")

  /** Расы именных — в порядке рас игры. */
  val Races: List[Race] = Race.values.toList.filter(Monster.legendaryNames.contains)

  /** Сколько рас в ряду кнопок. */
  val RacesPerRow: Int = 4

  /** Именные этой расы — в порядке списка. */
  def namesOf(race: Race): List[String] = Monster.legendaryNames.getOrElse(race, Nil)

  /** Чьи трофеи этой расы висят на стене. */
  def hung(home: HomeData, race: Race): List[String] = namesOf(race).filter(home.wall.contains)

  /** Уникальные трофеи из `items`, которых на стене `wall` нет, — по одному на
    * именного. */
  def hangable(items: List[Item], wall: List[String]): List[Item] =
    items.filter(i => i.id != 0L && i.isNamedTrophy)
      .flatMap(i => i.namedTrophy.map(n => n.monster -> i))
      .filterNot { case (monster, _) => wall.contains(monster) }
      .distinctBy(_._1)
      .map(_._2)
}
