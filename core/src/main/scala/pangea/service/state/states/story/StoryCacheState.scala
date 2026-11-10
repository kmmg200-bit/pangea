package pangea.service.state.states.story

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.QuestItemKind
import pangea.model.quest.{Difficulty, NpcQuest}
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{NpcQuestDialog, NpcQuestLog, QuestSupport, State, StoryLine, StoryMarkQuest, UserAction}
import zio.{Random, Task, ZIO}

/** «Метка под меткой», шаг 3: тайник Ульфа Сороки у изгороди (см.
  * StoryMarkQuest). Сюда ведёт карточка чёрного плаща — «Искать тайник
  * Сороки», в лабиринте с пятого поворота (InventoryState). Тайник сторожат
  * трое зверей, отмеченных тьмой. «Отбить тайник» — сюжетный бой с отрядом
  * героя, без «Сбежать», награда как за обычный бой; добыча возвращает сюда с
  * отметкой победы ([[StoryCacheState.Won]]) — и тайник открыт: плащ уходит,
  * шифровка Братства (зацепка следующей главы) — в сумку, заначка Сороки — в
  * кошель, глава закрыта. Ушёл или погиб — тайник ждёт, плащ поведёт снова. */
case class StoryCacheState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  // Закрытие главы — как у заданий горожан: опыт и строка про него.
  private val chapter = NpcQuestDialog(heroDao, content, NpcQuest.StoryMark, "StoryMark")

  private val branch = new Branch(
    routes = Map(
      "CacheFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "CacheLeave" -> Target.Goto(StateType.Dungeon)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryCache) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryCache)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      won    <- heroDao.readSceneData(user.userId).map(_.exists(_.as[StoryCacheState.Won].exists(_.won)))
      quests <- NpcQuestLog.load(heroDao, user.userId)
      hero   <- getHero(user)
      _      <- if (!StoryMarkQuest.toCache(quests))
                  heroDao.writeSceneData(user.userId, Json.Null) *>
                    renderer.show(user, Screen(content.text("story.storyMark.cache.gone"), List(onward)))
                else if (won) open(user, hero, renderer)
                else renderer.show(user, lairScreen)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def lairScreen: Screen =
    Screen(
      content.text("story.storyMark.cache.lair") + "\n\n" +
        content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryMarkQuest.Level)),
      List(
        content.choice("CacheFight", "story.storyMark.cache.fightLabel").copy(row = Some(0)),
        content.choice("CacheLeave", "story.storyMark.cache.leaveLabel").copy(row = Some(0))))

  private def onward = content.choice("CacheLeave", "story.storyMark.cache.onwardLabel")

  /** Бой со стражей тайника: трое меченых зверей уровня задания, отряд с ним.
    * Добыча вернёт сюда — с отметкой, что тайник отбит. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (!StoryMarkQuest.toCache(quests)) enter(user, renderer).as(StateType.StoryCache)
                else for {
                  guards   <- ZIO.succeed(StoryMarkQuest.guards)
                  energies <- ZIO.foreach(guards)(m =>
                                Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                                  .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
                  battle    = SoloPveBattle.fromGroup(guards, hero, energies).copy(story = Some(StoryMarkQuest.FightStory))
                  routing   = LootData(Nil, Nil, returnState = Some(StateType.StoryCache),
                                eventData = Some(StoryCacheState.WonMark.asJson))
                  _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
                  _        <- heroDao.writeSceneData(user.userId, routing.asJson)
                  _        <- renderer.show(user, Screen(content.text("story.storyMark.cache.fightStart"), Nil))
                } yield StateType.Battle
    } yield res

  /** Тайник отбит: шифровка — в сумку, заначка Сороки — в кошель, глава
    * закрыта опытом. Над подписью «Э.» герой думает о том, кто из двоих жив. */
  private def open(user: User, hero: Hero, renderer: Renderer): Task[Unit] =
    for {
      _      <- heroDao.writeSceneData(user.userId, Json.Null)
      lore   <- heroDao.readLoreData(user.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))
      _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StoryCloak))
      _      <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StoryCipher)
      silver  = StoryMarkQuest.silver
      _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
      // Следующая глава берётся сразу: она в журнале, а что делать — на карточке шифровки.
      now    <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      done   <- chapter.complete(user, hero.copy(silver = hero.silver + silver), _.take(NpcQuest.StoryPowder, now))
      (_, expLine) = done
      next   <- NpcQuestLog.load(heroDao, user.userId)
      _      <- renderer.show(user, Screen(
                  List(content.text("story.storyMark.cache.opened"), content.text("story.storyMark.cipher"),
                    content.text(StoryMarkQuest.cipherKey(lore))).map(_.trim).mkString("\n\n"), Nil))
      _      <- renderer.show(user, Screen(
                  content.format("story.storyMark.cache.reward", "silver" -> silver.toString, "exp" -> expLine) +
                    "\n\n" + content.text("story.storyMark.questDone") + "\n\n" +
                    StoryLine.takenLine(content, NpcQuest.StoryPowder, next), List(onward)))
    } yield ()

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryCacheState {

  /** Отметка в scene_data: стража перебита, тайник можно открывать. */
  final case class Won(won: Boolean = true)

  object Won {
    implicit val encoder: Encoder[Won] = (w: Won) => Json.obj("storyCacheWon" -> w.won.asJson)
    // Ключ свой: в scene_data лежат и чужие структуры.
    implicit val decoder: Decoder[Won] = (c: HCursor) => c.get[Boolean]("storyCacheWon").map(Won(_))
  }

  val WonMark: Won = Won(won = true)
}
