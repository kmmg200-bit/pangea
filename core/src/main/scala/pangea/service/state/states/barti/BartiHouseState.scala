package pangea.service.state.states.barti

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.quest.{Difficulty, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.story.StoryFight
import pangea.service.state.{BartiDoorQuest, BigDealQuest, NpcQuestLog, QuestSupport, State, StoryMorningQuest, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Дом Барти — синяя дверь на Торговой площади (см. BigDealQuest). Сюда ведёт
  * карточка адреса, из города. Впервые — затея с контрабандой: каджитам
  * нужны шнапс и живая вода, платят они золотом; задание взято. Дальше Барти
  * принимает товар хоть по одной склянке («📦 Отдать товар» берёт из сумки
  * всё, что ему ещё нужно) и складывает у себя. Собрано всё — ждать каравана:
  * пока герой ниже уровня задания, Барти говорит «рано, караван не пришёл»
  * (для этого задания так решил пользователь); дорос — караван пришёл, и
  * задание ведёт к нему. «Уйти» — на Торговую площадь.
  *
  * Акт II, «Наутро», путь «Сам по себе» (см. StoryMorningQuest): сюда ведёт
  * карточка книги ячейки. Барти читает о себе «болтает, платить пивом» и
  * становится партнёром героя; следом в дверь стучат люди Братства — за
  * книгой. «Драться» — сюжетный бой, без «Сбежать», Барти со сковородой — в
  * строю; добыча вернёт сюда с отметкой победы — опись урожая, глава закрыта.
  * Без хранилища вещей (`itemRepo`) этой ветки нет: опись класть некуда. */
case class BartiHouseState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  itemRepo:      Option[ItemRepository] = None
) extends State {
  import BartiHouseState._

  private val branch = new Branch(
    routes = Map(
      "BartiGive"       -> Target.Run { (u, _, r) => give(u, r) },
      "BartiStoryFight" -> Target.Run { (u, _, r) => storyFight(u, r) },
      "BartiDoorFight"  -> Target.Run { (u, _, r) => doorFight(u, r) },
      "BartiLeave"      -> Target.Goto(StateType.MarketSquare)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.BartiHouse) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets ++ Set(StateType.Battle, StateType.BartiHouse)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      items  <- bag(hero)
      data   <- heroDao.readSceneData(user.userId)
      story   = itemRepo.filter(_ => StoryMorningQuest.atBarti(quests))
      _      <- if (story.isDefined && StoryFight.won(data, WonKey) && StoryMorningQuest.toBartiFight(quests))
                  storyWon(user, renderer)
                else if (story.isDefined && StoryMorningQuest.toBarti(quests)) storyTalk(user, quests, renderer)
                else if (story.isDefined) renderer.show(user, knockScreen)
                // «Синяя дверь» (только одиночка): поджигатели Братства.
                else if (StoryFight.won(data, DoorWonKey) && BartiDoorQuest.toNight(quests)) doorWon(user, renderer)
                else if (BartiDoorQuest.available(quests)) doorBegin(user, quests, renderer)
                else if (BartiDoorQuest.toNight(quests)) renderer.show(user, nightScreen(None))
                else if (BigDealQuest.canBegin(quests) && QuestSupport.hasItem(items, QuestItemKind.BartiAddress))
                  begin(user, quests, items, renderer)
                else if (BigDealQuest.collecting(quests)) renderer.show(user, stockScreen(None, quests, items))
                else if (BigDealQuest.waiting(quests)) gate(user, hero, quests, None, renderer)
                else if (BigDealQuest.toCaravan(quests)) renderer.show(user, Screen(content.text("barti.house.arrived").trim, List(leave)))
                else renderer.show(user, Screen(content.text("barti.house.closed"), List(leave)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave: Choice =
    content.choice("BartiLeave", "barti.house.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(1))

  /** Впервые: затея с контрабандой — задание взято. */
  private def begin(user: User, quests: NpcQuests, items: List[Item], renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      taken  = quests.take(NpcQuest.BigDeal, now)
      _     <- NpcQuestLog.save(heroDao, user.userId, taken)
      _     <- renderer.show(user, Screen(content.text("barti.house.pitch").trim, Nil))
      _     <- renderer.show(user, stockScreen(Some(content.format("barti.taken",
                 "title" -> content.text("journal.quests.bigDeal.title"),
                 "step"  -> content.list("journal.quests.bigDeal.steps").headOption.getOrElse(""))), taken, items))
    } yield ()

  /** Сколько у Барти уже сложено, и — если в сумке есть, что ему ещё нужно, —
    * «Отдать товар». */
  private def stockScreen(lead: Option[String], quests: NpcQuests, items: List[Item]): Screen = {
    val s    = BigDealQuest.stock(quests)
    val line = content.format("barti.house.stock", "schnapps" -> s.schnapps.toString, "water" -> s.water.toString,
      "goal" -> BigDealQuest.Goal.toString)
    val give = Option.when(BigDealQuest.takeable(items, s).nonEmpty)(
      content.choice("BartiGive", "barti.house.giveLabel").copy(color = ChoiceColor.Positive, row = Some(0)))
    Screen((lead.toList :+ line).mkString("\n\n"), give.toList :+ leave)
  }

  /** «Отдать товар»: Барти берёт из сумки всё, что ему ещё нужно, по одной
    * склянке, — и складывает у себя. Собрал всё — ждать каравана. */
  private def give(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      items  <- bag(hero)
      s       = BigDealQuest.stock(quests)
      take    = BigDealQuest.takeable(items, s)
      _      <- if (!BigDealQuest.collecting(quests)) enter(user, renderer)
                else if (take.isEmpty) renderer.show(user, stockScreen(Some(content.text("barti.house.nothing")), quests, items))
                else for {
                  _     <- ZIO.foreachDiscard(take)(i => inventoryRepo.removeItem(i.id, hero.id).mapError(e => new Throwable(e.toString)))
                  next   = take.flatMap(_.brew).foldLeft(s)((acc, k) => acc.plus(k, 1))
                  stored = BigDealQuest.withStock(quests, next)
                  saved  = if (next.full) BigDealQuest.step(stored, 2) else stored
                  _     <- NpcQuestLog.save(heroDao, user.userId, saved)
                  said   = content.format("barti.house.given", "schnapps" -> next.schnapps.toString, "water" -> next.water.toString)
                  left   = items.filterNot(i => take.exists(_.id == i.id))
                  _     <- if (next.full) gate(user, hero, saved, Some(said), renderer)
                           else renderer.show(user, stockScreen(Some(said), saved, left))
                } yield ()
    } yield StateType.BartiHouse

  /** Товар собран: ниже уровня задания — «рано, караван не пришёл» (так решил
    * пользователь для этого задания); дорос — караван пришёл, задание ведёт к
    * нему. `lead` — что сказать перед этим. */
  private def gate(user: User, hero: Hero, quests: NpcQuests, lead: Option[String], renderer: Renderer): Task[Unit] =
    if (!BigDealQuest.arrives(hero))
      renderer.show(user, Screen((lead.toList :+ content.text("barti.house.early").trim).mkString("\n\n"), List(leave)))
    else NpcQuestLog.save(heroDao, user.userId, BigDealQuest.step(quests, 3)) *>
      renderer.show(user, Screen((lead.toList :+ content.text("barti.house.arrived").trim).mkString("\n\n"), List(leave)))

  // ── Акт II, «Наутро», путь «Сам по себе» ──────────────────────────────────

  /** Барти читает о себе в книге ячейки и становится партнёром — тут же в
    * дверь стучат люди Братства. Встречались ли они в этой жизни — по «Ходовому
    * товару»: если нет, Барти видит героя впервые. Глава на втором шаге. */
  private def storyTalk(user: User, quests: NpcQuests, renderer: Renderer): Task[Unit] = {
    val met = quests.of(NpcQuest.HotGoods).step > 0
    NpcQuestLog.save(heroDao, user.userId, StoryMorningQuest.step(quests, 2)) *>
      renderer.show(user, Screen(
        List(if (met) "story.storyMorning.barti.met" else "story.storyMorning.barti.stranger", "story.storyMorning.barti.talk")
          .map(k => content.text(k).trim).mkString("\n\n"), Nil)) *>
      renderer.show(user, knockScreen)
  }

  /** Стук в дверь: счетовод ячейки и его люди — за книгой. */
  private def knockScreen: Screen =
    Screen(
      content.text("story.storyMorning.barti.knock").trim + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryMorningQuest.Level)),
      List(content.choice("BartiStoryFight", "story.storyMorning.barti.fightLabel").copy(row = Some(0)),
        content.choice("BartiLeave", "story.storyMorning.barti.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(0))))

  /** Бой со счетоводом и его людьми; Барти — в строю. Добыча вернёт сюда. */
  private def storyFight(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (itemRepo.isEmpty || !StoryMorningQuest.toBartiFight(quests)) enter(user, renderer).as(StateType.BartiHouse)
      else for {
        hero <- getHero(user)
        _    <- StoryFight.start(heroDao, user, hero,
                  StoryMorningQuest.gang(content.text("story.storyMorning.barti.leaderName")),
                  List(StoryMorningQuest.bartiGuest(content)), StoryMorningQuest.BartiStory, StateType.BartiHouse, WonKey)
        _    <- renderer.show(user, Screen(content.text("story.storyMorning.barti.fightStart"), Nil))
      } yield StateType.Battle
    }

  /** Отбились: у счетовода в кармане — опись урожая; глава закрыта. */
  private def storyWon(user: User, renderer: Renderer): Task[Unit] =
    ZIO.foreachDiscard(itemRepo) { repo =>
      for {
        _    <- heroDao.writeSceneData(user.userId, Json.Null)
        hero <- getHero(user)
        _    <- StoryMorningQuest.finish(heroDao, inventoryRepo, repo, content, user, hero, Path.Alone,
                  content.text("story.storyMorning.barti.won"), List(leave.copy(row = Some(0))), renderer)
      } yield ()
    }

  // ── Акт II, «Синяя дверь» (только одиночка) ────────────────────────────────

  /** Впервые: метка Братства на двери, Барти признаётся, что похвастался, —
    * задание взято, ночь у двери. */
  private def doorBegin(user: User, quests: NpcQuests, renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      taken  = quests.take(NpcQuest.BartiDoor, now)
      _     <- NpcQuestLog.save(heroDao, user.userId, taken)
      line   = content.format("barti.taken", "title" -> content.text("journal.quests.bartiDoor.title"),
                 "step" -> content.list("journal.quests.bartiDoor.steps").headOption.getOrElse(""))
      _     <- renderer.show(user, nightScreen(Some(content.text("barti.door.pitch").trim + "\n\n" + line)))
    } yield ()

  /** Ночь у синей двери: идут поджигатели — демоны. `lead` — что сказать перед этим. */
  private def nightScreen(lead: Option[String]): Screen =
    Screen(
      (lead.toList :+ content.text("barti.door.night").trim).mkString("\n\n") + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(BartiDoorQuest.Level)),
      List(content.choice("BartiDoorFight", "barti.door.fightLabel").copy(row = Some(0)),
        content.choice("BartiLeave", "barti.door.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(0))))

  /** Бой с поджигателями; Барти со сковородой — в строю. Добыча вернёт сюда. */
  private def doorFight(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!BartiDoorQuest.toNight(quests)) enter(user, renderer).as(StateType.BartiHouse)
      else for {
        hero <- getHero(user)
        _    <- StoryFight.start(heroDao, user, hero, BartiDoorQuest.foes(content.text("barti.door.leaderName")),
                  List(StoryMorningQuest.bartiGuest(content, BartiDoorQuest.Level)), BartiDoorQuest.FightStory,
                  StateType.BartiHouse, DoorWonKey)
        _    <- renderer.show(user, Screen(content.text("barti.door.fightStart"), Nil))
      } yield StateType.Battle
    }

  /** Отбились: дом в дырах, но цел, мебель спасена; теперь это и убежище героя. */
  private def doorWon(user: User, renderer: Renderer): Task[Unit] =
    for {
      _      <- heroDao.writeSceneData(user.userId, Json.Null)
      hero   <- getHero(user)
      reward <- BartiDoorQuest.finish(heroDao, content, user, hero)
      _      <- renderer.show(user, Screen(List(content.text("barti.door.won").trim, content.text("barti.door.shelter").trim, reward)
                  .mkString("\n\n"), List(leave.copy(row = Some(0)))))
    } yield ()

  private def bag(hero: Hero): Task[List[Item]] =
    inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).map(_.items.data)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object BartiHouseState {

  /** Отметка в `scene_data`: люди Братства у Барти побиты (Акт II, «Наутро»). */
  val WonKey: String = "storyBartiHouseWon"

  /** Отметка в `scene_data`: поджигатели у синей двери побиты («Синяя дверь»). */
  val DoorWonKey: String = "bartiDoorWon"
}
