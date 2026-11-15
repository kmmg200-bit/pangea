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
import pangea.service.state.BartiRoofQuest.Deal
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.states.story.StoryFight
import pangea.service.state.{BartiDeliveryQuest, BartiRoofQuest, NpcQuestLog, QuestSupport, State, StoryToolQuest, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Лавка Барти на Торговой площади — «Крыша» (см. BartiRoofQuest). Сюда ведёт
  * кнопка с площади, пока задание доступно или идёт, и карточка квитанции —
  * из города. Впервые — Барти показывает лавку и квитанцию ночной стражи:
  * задание взято на первом шаге своего пути, квитанция — в сумку. Щиту — к
  * Юноне (в лавке ждать нечего). Тени Братство предлагает решить вопрос само:
  * согласился — задание закрыто без боя, а Барти теперь должен Братству;
  * отказался — ночью в лавку. Ночью приходит капитан ночной стражи с четырьмя:
  * «Драться» — сюжетный бой, без «Сбежать», награда как за обычный бой, Барти
  * прячется под прилавком; добыча вернёт сюда с отметкой победы — задание
  * закрыто. Ушёл или погиб — стража придёт следующей ночью. «Уйти» — на
  * Торговую площадь.
  *
  * После «Крыши» здесь же — «Большая поставка» (см. BartiDeliveryQuest):
  * впервые Барти показывает доски, которыми ему заплатили вперёд, и рассказывает
  * рецепт самогона — задание взято. Самогон носят ему по бутыли («Отдать
  * самогон»); собран — «Везти в лагерь»: сюжетный бой с пьяной бригадой
  * Чернолома, без «Сбежать», Барти — под телегой; добыча вернёт сюда с
  * отметкой победы — задание на шаге Гуннара (доски — на городскую стройку). */
case class BartiShopState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {
  import BartiShopState._

  private val branch = new Branch(
    routes = Map(
      "RoofBrotherhood" -> Target.Run { (u, _, r) => letBrotherhood(u, r) },
      "RoofSelf"        -> Target.Run { (u, _, r) => bySelf(u, r) },
      "RoofFight"       -> Target.Run { (u, _, r) => fight(u, r) },
      "DeliveryGive"    -> Target.Run { (u, _, r) => give(u, r) },
      "DeliveryGo"      -> Target.Run { (u, _, r) => deliver(u, r) },
      "RoofLeave"       -> Target.Goto(StateType.MarketSquare)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.BartiShop) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.BartiShop)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      data   <- heroDao.readSceneData(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (StoryFight.won(data, WonKey) && BartiRoofQuest.toNight(quests)) won(user, quests, renderer)
                else if (BartiRoofQuest.available(quests)) begin(user, quests, renderer)
                // «Большая поставка» — после «Крыши».
                else if (StoryFight.won(data, DeliveryWonKey) && BartiDeliveryQuest.toCamp(quests)) deliveryWon(user, quests, renderer)
                else if (BartiDeliveryQuest.available(quests)) deliveryBegin(user, quests, renderer)
                else if (BartiDeliveryQuest.toCollect(quests)) bag(user).flatMap(items => renderer.show(user, collectScreen(None, quests, items)))
                else if (BartiDeliveryQuest.toCamp(quests)) renderer.show(user, cartScreen(None))
                else if (BartiDeliveryQuest.toGunnar(quests))
                  renderer.show(user, Screen(content.text("barti.delivery.toGunnar").trim, List(leave.copy(row = Some(0)))))
                else renderer.show(user, stepScreen(quests, None))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave: Choice =
    content.choice("RoofLeave", "barti.roof.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(1))

  private def difficulty: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(BartiRoofQuest.Level))

  /** Впервые: лавка, квитанция стражи — в сумку, задание взято на первом шаге
    * своего пути; дальше — экран этого шага. */
  private def begin(user: User, quests: NpcQuests, renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero  <- getHero(user)
      _     <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.BartiReceipt)
      taken  = BartiRoofQuest.take(quests, now)
      _     <- NpcQuestLog.save(heroDao, user.userId, taken)
      line   = content.format("barti.taken", "title" -> content.text("journal.quests.bartiRoof.title"),
                 "step" -> content.list("journal.quests.bartiRoof.steps").lift(taken.of(NpcQuest.BartiRoof).step - 1)
                   .getOrElse(""))
      // Одиночке ждать некого и спрашивать некого — Барти сразу говорит, когда
      // приходит стража.
      alone  = Option.when(StoryToolQuest.choice(quests).contains(Path.Alone))(content.text("barti.roof.alone").trim)
      _     <- renderer.show(user, stepScreen(taken, Some((List(content.text("barti.roof.pitch").trim) ++ alone.toList :+ line)
                 .mkString("\n\n"))))
    } yield ()

  /** Экран шага: Щиту — к Юноне, Тени — предложение Братства, ночью — стража у
    * двери; задания нет — Барти торгует. `lead` — что сказать перед этим. */
  private def stepScreen(quests: NpcQuests, lead: Option[String]): Screen = {
    def text(body: String) = (lead.toList :+ body).mkString("\n\n")
    if (BartiRoofQuest.toJunona(quests)) Screen(text(content.text("barti.roof.shield").trim), List(leave.copy(row = Some(0))))
    else if (BartiRoofQuest.toOffer(quests))
      Screen(text(content.text("barti.roof.offer").trim + "\n\n" + difficulty), List(
        content.choice("RoofBrotherhood", "barti.roof.brotherhoodLabel").copy(row = Some(0)),
        content.choice("RoofSelf", "barti.roof.selfLabel").copy(row = Some(0)),
        leave))
    else if (BartiRoofQuest.toNight(quests))
      Screen(text(content.text("barti.roof.night").trim + "\n\n" + difficulty),
        List(content.choice("RoofFight", "barti.roof.fightLabel").copy(row = Some(0)), leave.copy(row = Some(0))))
    else Screen(text(content.text("barti.roof.idle")), List(leave.copy(row = Some(0))))
  }

  /** Тень: пусть решает Братство — без боя; задание закрыто, Барти должен
    * Братству. Не тот шаг — экран шага. */
  private def letBrotherhood(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!BartiRoofQuest.toOffer(quests)) renderer.show(user, stepScreen(quests, None))
      else for {
        hero   <- getHero(user)
        reward <- BartiRoofQuest.finish(heroDao, inventoryRepo, content, user, hero, Some(Deal.Brotherhood))
        _      <- renderer.show(user, Screen(content.text("barti.roof.brotherhood").trim + "\n\n" + reward, List(leave.copy(row = Some(0)))))
      } yield ()
    }.as(StateType.BartiShop)

  /** Тень: разобраться самому — ночью в лавку. Не тот шаг — экран шага. */
  private def bySelf(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!BartiRoofQuest.toOffer(quests)) renderer.show(user, stepScreen(quests, None))
      else {
        val next = BartiRoofQuest.step(quests, 3)
        NpcQuestLog.save(heroDao, user.userId, next) *>
          renderer.show(user, stepScreen(next, Some(content.text("barti.roof.self").trim)))
      }
    }.as(StateType.BartiShop)

  /** Ночью: капитан ночной стражи и четверо; Барти — под прилавком. Добыча
    * вернёт сюда с отметкой победы. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!BartiRoofQuest.toNight(quests)) renderer.show(user, stepScreen(quests, None)).as(StateType.BartiShop)
      else for {
        hero <- getHero(user)
        _    <- StoryFight.start(heroDao, user, hero, BartiRoofQuest.foes(content.text("barti.roof.captainName")), Nil,
                  BartiRoofQuest.FightStory, StateType.BartiShop, WonKey)
        _    <- renderer.show(user, Screen(content.text("barti.roof.fightStart"), Nil))
      } yield StateType.Battle
    }

  /** Отбились: что стало с капитаном — по пути; задание закрыто. */
  private def won(user: User, quests: NpcQuests, renderer: Renderer): Task[Unit] =
    for {
      _      <- heroDao.writeSceneData(user.userId, Json.Null)
      hero   <- getHero(user)
      path    = StoryToolQuest.choice(quests)
      deal    = Option.when(path.contains(Path.Shadow))(Deal.Self)
      coda    = content.text(s"barti.roof.coda.${path.fold(Path.Alone.key)(_.key)}").trim
      reward <- BartiRoofQuest.finish(heroDao, inventoryRepo, content, user, hero, deal)
      _      <- renderer.show(user, Screen(List(content.text("barti.roof.won").trim, coda, reward).mkString("\n\n"),
                  List(leave.copy(row = Some(0)))))
    } yield ()

  // ── «Большая поставка» ──────────────────────────────────────────────────────

  private def deliveryDifficulty: String =
    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(BartiDeliveryQuest.Level))

  /** Впервые: доски вместо платы, заказ и рецепт самогона — задание взято,
    * рецепт — в книгу. Дальше — сколько у Барти сложено. */
  private def deliveryBegin(user: User, quests: NpcQuests, renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      taken  = quests.take(NpcQuest.BartiDelivery, now).withRecipe(BartiDeliveryQuest.Recipe)
      _     <- NpcQuestLog.save(heroDao, user.userId, taken)
      items <- bag(user)
      line   = content.format("barti.taken", "title" -> content.text("journal.quests.bartiDelivery.title"),
                 "step" -> content.list("journal.quests.bartiDelivery.steps").headOption.getOrElse(""))
      _     <- renderer.show(user, collectScreen(Some(content.text("barti.delivery.pitch").trim + "\n\n" + line), taken, items))
    } yield ()

  /** Сколько бутылей у Барти уже сложено, и — если в сумке есть самогон, —
    * «Отдать самогон». `lead` — что сказать перед этим. */
  private def collectScreen(lead: Option[String], quests: NpcQuests, items: List[Item]): Screen = {
    val have = BartiDeliveryQuest.stock(quests)
    val line = content.format("barti.delivery.stock", "have" -> have.toString, "goal" -> BartiDeliveryQuest.Goal.toString)
    val give = Option.when(BartiDeliveryQuest.takeable(items, have).nonEmpty)(
      content.choice("DeliveryGive", "barti.delivery.giveLabel").copy(color = ChoiceColor.Positive, row = Some(0)))
    Screen((lead.toList :+ line :+ deliveryDifficulty).mkString("\n\n"), give.toList :+ leave)
  }

  /** «Отдать самогон»: Барти берёт из сумки бутыли по одной — сколько ему ещё
    * нужно. Собрал все — телега гружёная, пора в лагерь. */
  private def give(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      items  <- bag(user)
      have    = BartiDeliveryQuest.stock(quests)
      take    = BartiDeliveryQuest.takeable(items, have)
      _      <- if (!BartiDeliveryQuest.toCollect(quests)) enter(user, renderer)
                else if (take.isEmpty) renderer.show(user, collectScreen(Some(content.text("barti.delivery.nothing").trim), quests, items))
                else for {
                  _     <- ZIO.foreachDiscard(take)(i => inventoryRepo.removeItem(i.id, hero.id).mapError(e => new Throwable(e.toString)))
                  next   = have + take.size
                  stored = BartiDeliveryQuest.withStock(quests, next)
                  full   = next >= BartiDeliveryQuest.Goal
                  saved  = if (full) BartiDeliveryQuest.step(stored, 2) else stored
                  _     <- NpcQuestLog.save(heroDao, user.userId, saved)
                  said   = content.format("barti.delivery.given", "have" -> next.toString, "goal" -> BartiDeliveryQuest.Goal.toString)
                  left   = items.filterNot(i => take.exists(_.id == i.id))
                  _     <- if (full) renderer.show(user, cartScreen(Some(said)))
                           else renderer.show(user, collectScreen(Some(said), saved, left))
                } yield ()
    } yield StateType.BartiShop

  /** Самогон собран: телега гружёная, Барти на козлах — «Везти в лагерь». */
  private def cartScreen(lead: Option[String]): Screen =
    Screen((lead.toList :+ content.text("barti.delivery.cart").trim :+ deliveryDifficulty).mkString("\n\n"),
      List(content.choice("DeliveryGo", "barti.delivery.goLabel").copy(row = Some(0)), leave.copy(row = Some(0))))

  /** В лагерь: бригада вскрывает ящики до расчёта и дерётся за бутыли — бой с
    * пьяным десятником и четырьмя, Барти под телегой. Добыча вернёт сюда с
    * отметкой победы. */
  private def deliver(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!BartiDeliveryQuest.toCamp(quests)) enter(user, renderer).as(StateType.BartiShop)
      else for {
        hero <- getHero(user)
        _    <- StoryFight.start(heroDao, user, hero, BartiDeliveryQuest.foes(content.text("barti.delivery.foremanName")), Nil,
                  BartiDeliveryQuest.FightStory, StateType.BartiShop, DeliveryWonKey)
        _    <- renderer.show(user, Screen(content.text("barti.delivery.camp").trim, Nil))
      } yield StateType.Battle
    }

  /** Отбились: остаток — тоже досками, и доски — Гуннару. Задание на третьем шаге. */
  private def deliveryWon(user: User, quests: NpcQuests, renderer: Renderer): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null) *>
      NpcQuestLog.save(heroDao, user.userId, BartiDeliveryQuest.step(quests, 3)) *>
      renderer.show(user, Screen(content.text("barti.delivery.won").trim, List(leave.copy(row = Some(0)))))

  private def bag(user: User): Task[List[Item]] =
    getHero(user).flatMap(hero => inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).map(_.items.data))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object BartiShopState {

  /** Отметка в `scene_data`: ночная стража у лавки Барти побита. */
  val WonKey: String = "bartiRoofWon"

  /** Отметка в `scene_data`: пьяная бригада в лагере у Старой вырубки побита. */
  val DeliveryWonKey: String = "bartiDeliveryWon"
}
