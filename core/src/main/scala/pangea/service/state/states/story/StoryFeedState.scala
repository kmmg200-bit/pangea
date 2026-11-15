package pangea.service.state.states.story

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.model.quest.{Difficulty, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.parcel.Parcels
import pangea.service.state.StoryCollectorQuest.Deal
import pangea.service.state.StoryFeedQuest.Brand
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.{NpcQuestDialog, NpcQuestLog, QuestOutcomeLog, QuestSupport, State, StoryFeedQuest, StoryGearPost,
  StoryLine, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Рубеж II «Чем кормить Клеймо» (см. StoryFeedQuest): дегтярный склад, третья
  * ночь. Сюда ведёт карточка купчей, из города.
  *
  * Шаг 1 — ночь: гости пути и Сойка на крыше (если её простили), приходит
  * Сборщик с четырьмя людьми. «Драться» — сюжетный бой, без «Сбежать», награда
  * как за обычный; Сборщик тянет узор из героя (см. PatternPull). Добыча вернёт
  * сюда с отметкой победы — рубеж на втором шаге.
  *
  * Шаг 2 — Сборщик связан: накормить, усмирить или приручить Клеймо. Выбор
  * закрывает рубеж: снаряжение выбора — в сумку (не влезло — посылкой),
  * серебро, опыт, своё достижение, исход — в «как и вы»; глава 11 взята и
  * висит. За Сборщиком приходит глава Братства и помнит договор главы 10 —
  * кроме Щита, отдавшего Сборщика совету. Купчая остаётся в сумке. */
case class StoryFeedState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  outcomes:      Option[QuestOutcomeDao] = None,
  parcels:       Option[Parcels] = None
) extends State {
  import StoryFeedState._

  private val milestone = NpcQuestDialog(heroDao, content, NpcQuest.StoryFeed, "StoryFeed")

  private val branch = new Branch(
    routes = Map(
      "FeedFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "FeedFeed"  -> Target.Run { (u, _, r) => decide(u, r, Brand.Feed) },
      "FeedCalm"  -> Target.Run { (u, _, r) => decide(u, r, Brand.Calm) },
      "FeedTame"  -> Target.Run { (u, _, r) => decide(u, r, Brand.Tame) },
      "FeedLeave" -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryFeed) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryFeed)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      data   <- heroDao.readSceneData(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (StoryFight.won(data, WonKey) && StoryFeedQuest.toFight(quests))
                  heroDao.writeSceneData(user.userId, Json.Null) *>
                    NpcQuestLog.save(heroDao, user.userId, StoryFeedQuest.step(quests, 2)) *>
                    renderer.show(user, choiceScreen(Some(content.text("story.storyFeed.won").trim)))
                else if (StoryFeedQuest.toFight(quests)) renderer.show(user, nightScreen(quests))
                else if (StoryFeedQuest.toChoice(quests)) renderer.show(user, choiceScreen(None))
                else renderer.show(user, Screen(content.text("story.storyFeed.gone"), List(leave.copy(row = Some(0)))))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave: Choice =
    content.choice("FeedLeave", "story.storyFeed.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(3))

  private def texts(keys: List[String]): String = keys.map(k => content.text(k).trim).mkString("\n\n")

  /** Третья ночь на складе: кто ждёт со мной, Сойка на крыше и Сборщик. */
  private def nightScreen(q: NpcQuests): Screen = {
    val keys = List("story.storyFeed.night.arrive", s"story.storyFeed.night.${StoryFeedQuest.path(q).key}") ++
      Option.when(StoryFeedQuest.sojkaOnRoof(q))("story.storyFeed.night.sojka").toList ++
      List("story.storyFeed.night.collector")
    Screen(texts(keys) + "\n\n" + content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryFeedQuest.Level)),
      List(content.choice("FeedFight", "story.storyFeed.fightLabel").copy(row = Some(0)), leave.copy(row = Some(0))))
  }

  /** Сборщик связан — чем кормить Клеймо. `head` — строки победы, если экран
    * сразу после боя. */
  private def choiceScreen(head: Option[String]): Screen =
    Screen((head.toList :+ content.text("story.storyFeed.choice.text").trim).mkString("\n\n"),
      List(
        content.choice("FeedFeed", "story.storyFeed.choice.feedLabel").copy(row = Some(0)),
        content.choice("FeedCalm", "story.storyFeed.choice.calmLabel").copy(row = Some(1)),
        content.choice("FeedTame", "story.storyFeed.choice.tameLabel").copy(row = Some(2)),
        leave))

  /** Бой на складе: Сборщик и его люди, гости пути и Сойка; узор тянет Сборщик.
    * Добыча вернёт сюда с отметкой победы. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryFeedQuest.toFight(quests)) enter(user, renderer).as(StateType.StoryFeed)
      else for {
        hero <- getHero(user)
        name  = content.text("story.storyFeed.collectorName")
        _    <- StoryFight.start(heroDao, user, hero, StoryFeedQuest.foes(name), StoryFeedQuest.guests(content, quests),
                  StoryFeedQuest.FightStory, StateType.StoryFeed, WonKey, puller = Some(name))
        _    <- renderer.show(user, Screen(content.text("story.storyFeed.fightStart").trim, Nil))
      } yield StateType.Battle
    }

  /** Выбор закрывает рубеж: снаряжение — в сумку (не влезло — посылкой),
    * серебро и опыт, достижение выбора, исход — в «как и вы»; глава 11 взята. */
  private def decide(user: User, renderer: Renderer, brand: Brand): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryFeedQuest.toChoice(quests)) enter(user, renderer)
      else for {
        now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        hero   <- getHero(user)
        seed   <- Random.nextLong
        given  <- StoryGearPost.give(inventoryRepo, itemRepo, parcels, hero, content.text("story.storyFeed.gearFrom"),
                    StoryFeedQuest.gear(brand, Rng(seed)), now)
        (bagged, posted) = given
        silver  = StoryFeedQuest.silver
        _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
        done   <- milestone.complete(user, hero.copy(silver = hero.silver + silver),
                    q => q.update(NpcQuest.StoryFeed)(_.copy(counter = brand.n)).take(NpcQuest.StoryScars, now))
        (leveled, expLine) = done
        stats  <- QuestOutcomeLog.recordAndCount(outcomes, content, hero, brand.outcome)
        after  <- NpcQuestLog.load(heroDao, user.userId)
        _      <- renderer.show(user, Screen(endText(quests, brand), Nil))
        _      <- QuestSupport.grant(heroDao, content, user, leveled, brand.achievement, renderer)
        _      <- renderer.show(user, Screen(List(
                    content.format(s"story.storyFeed.reward.${brand.key}", "silver" -> silver.toString, "exp" -> expLine) +
                      gearLines(bagged, posted),
                    QuestOutcomeLog.withStats(content.text("story.storyFeed.questDone"), stats),
                    StoryLine.takenLine(content, NpcQuest.StoryScars, after)).mkString("\n\n"), List(leave.copy(row = Some(0)))))
      } yield ()
    }.as(StateType.StoryFeed)

  /** Что стало со Сборщиком и с Клеймом, откуда снаряжение, и глава Братства —
    * он приходит за Сборщиком и помнит договор главы 10. У Щита, отдавшего
    * Сборщика совету, старика нет: о договоре — записка. */
  private def endText(q: NpcQuests, brand: Brand): String = {
    val council = brand == Brand.Calm && StoryFeedQuest.path(q) == Path.Shield
    val body    = if (council) "story.storyFeed.brand.calmShield" else s"story.storyFeed.brand.${brand.key}"
    val oldMan  = Option.unless(council)(s"story.storyFeed.oldMan.${brand.key}")
    val bound   = StoryFeedQuest.deal(q).filter(d => d == Deal.Place || d == Deal.Peace).map { d =>
                    if (council) "story.storyFeed.deal.council"
                    else if (brand == Brand.Calm) s"story.storyFeed.deal.${d.key}"
                    else "story.storyFeed.deal.broken"
                  }
    texts(List(body, s"story.storyFeed.gear.${brand.key}") ++ oldMan.toList ++ bound.toList)
  }

  private def gearLines(bagged: List[Item], posted: Option[String]): String =
    bagged.map(i => "\n" + content.format("story.storyTool.gearLine", "name" -> i.displayTitle)).mkString +
      posted.fold("")("\n\n" + _)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryFeedState {

  /** Отметка в `scene_data`: Сборщик и его люди побиты. */
  val WonKey: String = "storyFeedWon"
}
