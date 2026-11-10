package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.Difficulty
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{NpcQuestLog, QuestSupport, State, StoryFaceQuest, StoryLine, StoryListQuest, UserAction}
import zio.{Random, Task, ZIO}

/** «Лицо из прошлого», шаги 2 и 3: переплётная у рыбного рынка (см.
  * StoryFaceQuest). Сюда ведёт карточка ключа, из города. Днём — лицо из
  * прошлого за конторкой (или у пресса, или оба): он не узнаёт героя, а
  * ударить при свидетелях нельзя — глава на третьем шаге. Ночью — ячейка
  * Братства в задней комнате (у порога — знак Сойки: простил её — перо
  * «открыто», порвал — перо там, где ячейку предупредили). «Драться» —
  * сюжетный бой с отрядом героя, без «Сбежать», награда как за обычный бой;
  * добыча возвращает сюда с отметкой победы ([[StoryBinderyState.Won]]) —
  * копии списка в сумку, глава на четвёртом шаге: к Горну. Ушёл или погиб —
  * ячейка ждёт, ключ поведёт снова. */
case class StoryBinderyState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "BinderyFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "BinderyLeave" -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryBindery) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryBindery)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      won    <- heroDao.readSceneData(user.userId).map(_.exists(_.as[StoryBinderyState.Won].exists(_.won)))
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- if (won && StoryFaceQuest.toRaid(quests)) copies(user, renderer)
                else if (StoryFaceQuest.toFace(quests)) face(user, renderer)
                else if (StoryFaceQuest.toRaid(quests)) renderer.show(user, doorScreen(StoryListQuest.choice(quests).map(_.key)))
                else renderer.show(user, Screen(content.text("story.storyFace.bindery.gone"), List(leave)))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave = content.choice("BinderyLeave", "story.storyFace.bindery.leaveLabel")

  /** Днём: лицо из прошлого — и почему ударить сейчас нельзя. Глава на
    * третьем шаге: прийти ночью. */
  private def face(user: User, renderer: Renderer): Task[Unit] =
    for {
      survivors <- StoryLine.survivorsOf(heroDao, user.userId)
      quests    <- NpcQuestLog.load(heroDao, user.userId)
      _         <- NpcQuestLog.save(heroDao, user.userId, StoryFaceQuest.step(quests, 3))
      _         <- renderer.show(user, Screen(
                     List(s"story.storyFace.bindery.face${survivors.suffix}", "story.storyFace.bindery.faceEnd")
                       .map(k => content.text(k).trim).mkString("\n\n"), List(leave)))
    } yield ()

  /** Ночью: чёрный ход, знак Сойки — по тому, как с ней кончилось в «Списке», —
    * и ячейка в задней комнате. */
  private def doorScreen(sojka: Option[String]): Screen =
    Screen(
      (List(content.text("story.storyFace.bindery.door")) ++
        sojka.map(k => content.text(s"story.storyFace.bindery.sojka.$k")).toList ++
        List(content.text("story.storyFace.bindery.doorEnd"))).map(_.trim).mkString("\n\n") + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryFaceQuest.Level)),
      List(
        content.choice("BinderyFight", "story.storyFace.bindery.fightLabel").copy(row = Some(0)),
        content.choice("BinderyLeave", "story.storyFace.bindery.leaveLabel").copy(row = Some(0))))

  /** Бой с ячейкой: старший и трое уровня задания, отряд с героем. Добыча
    * вернёт сюда — с отметкой, что ячейка перебита. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!StoryFaceQuest.toRaid(quests)) enter(user, renderer).as(StateType.StoryBindery)
                else for {
                  guards   <- ZIO.succeed(StoryFaceQuest.guards(content.text("story.storyFace.leaderName")))
                  energies <- ZIO.foreach(guards)(m =>
                                Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                  .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                  battle    = SoloPveBattle.fromGroup(guards, hero, energies).copy(story = Some(StoryFaceQuest.FightStory))
                  routing   = LootData(Nil, Nil, returnState = Some(StateType.StoryBindery),
                                eventData = Some(StoryBinderyState.WonMark.asJson))
                  _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _        <- renderer.show(user, Screen(content.text("story.storyFace.bindery.fightStart"), Nil))
                } yield StateType.Battle
    } yield res

  /** Ячейка перебита: копии списка — в сумку, глава на четвёртом шаге. */
  private def copies(user: User, renderer: Renderer): Task[Unit] =
    for {
      _      <- heroDao.writeSceneData(user.userId, Json.Null)
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      _      <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StoryCopies)
      _      <- NpcQuestLog.save(heroDao, user.userId, StoryFaceQuest.step(quests, 4))
      _      <- renderer.show(user, Screen(content.text("story.storyFace.bindery.copies"), List(leave)))
    } yield ()

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryBinderyState {

  /** Отметка в scene_data: ячейка в переплётной перебита. */
  final case class Won(won: Boolean = true)

  object Won {
    implicit val encoder: Encoder[Won] = (w: Won) => Json.obj("storyBinderyWon" -> w.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры.
    implicit val decoder: Decoder[Won] = (c: HCursor) => c.get[Boolean]("storyBinderyWon").map(Won(_))
  }

  val WonMark: Won = Won(won = true)
}
