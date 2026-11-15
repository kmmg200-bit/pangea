package pangea.service.state.states.story

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.quest.{Difficulty, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.purse.Purse
import pangea.service.state.StoryToolQuest.Path
import pangea.service.state.{HedgeSongQuest, NpcQuestDialog, NpcQuestLog, QuestSupport, State, StoryClearingQuest,
  StoryLine, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Просека»: просека Чернолома в изгороди (см. StoryClearingQuest). Сюда ведёт
  * карточка пропуска, из лабиринта. Бригада, бочки с порохом вдоль просеки,
  * Тихие поют изгороди закрыться — и у каждого пути своё дело к бригаде:
  * Тень пришла «объяснить договор» и может просто отдать бригаде долг
  * Братства (серебро с рук, нехватку кошель доберёт из ячейки Торгового
  * дома), Щит — с Горном и встречным приказом Юноны, одиночка — с ящиком
  * шнапса Барти, и рядом встаёт Элиар.
  *
  * «Драться» — сюжетный бой, без «Сбежать», награда как за обычный; на поле
  * стоят бочки с порохом. Добыча вернёт сюда с отметкой победы. Откупом или
  * боем — глава закрыта: пропуск уходит, накладная на лес — в сумку, серебро
  * и опыт, глава 9 взята и висит до обновления. Ушёл или погиб — бригада
  * никуда не делась, пропуск поведёт снова. */
case class StoryClearingState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  bank:          Option[BankRepository] = None
) extends State {
  import StoryClearingState._

  private val chapter = NpcQuestDialog(heroDao, content, NpcQuest.StoryClearing, "StoryClearing")

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "ClearingFight" -> Target.Run { (u, _, r) => fight(u, r) },
      "ClearingPay"   -> Target.Run { (u, _, r) => pay(u, r) },
      "ClearingLeave" -> Target.Goto(StateType.Dungeon)
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.StoryClearing) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.Battle, StateType.StoryClearing)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      data   <- heroDao.readSceneData(user.userId)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      hero   <- getHero(user)
      _      <- if (StoryFight.won(data, WonKey) && StoryClearingQuest.atClearing(quests))
                  done(user, quests, s"story.storyClearing.clearing.won${pathKey(quests).capitalize}", paid = false, renderer)
                else if (StoryClearingQuest.atClearing(quests)) renderer.show(user, clearingScreen(quests, hero))
                else renderer.show(user, Screen(content.text("story.storyClearing.clearing.gone"), List(leave.copy(row = Some(0)))))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def leave: Choice =
    content.choice("ClearingLeave", "story.storyClearing.clearing.leaveLabel").copy(color = ChoiceColor.Negative, row = Some(1))

  private def pathKey(q: NpcQuests): String = StoryClearingQuest.path(q).key

  private def foreman: String = content.text("story.storyClearing.foremanName")

  /** Просека: что тут творится, дело своего пути, слова Фаэлина, если он при
    * герое, и сложность. Тень может отдать долг Братства — кнопка с суммой. */
  private def clearingScreen(q: NpcQuests, hero: Hero): Screen = {
    val shadow = StoryClearingQuest.path(q) == Path.Shadow
    val faelin = Option.when(HedgeSongQuest.faelin(hero))(content.text("story.storyClearing.clearing.faelin").trim)
    val text   = (List(content.text("story.storyClearing.clearing.text").trim,
                    content.format(s"story.storyClearing.clearing.${pathKey(q)}",
                      "wages" -> StoryClearingQuest.wages.toString).trim) ++ faelin.toList :+
                    content.format("marisa.difficulty", "difficulty" -> Difficulty.render(StoryClearingQuest.Level)))
                   .mkString("\n\n")
    val payOff = Option.when(shadow)(content.choice("ClearingPay", "story.storyClearing.clearing.payLabel",
                   "wages" -> StoryClearingQuest.wages.toString).copy(color = ChoiceColor.Positive, row = Some(0)))
    Screen(text, payOff.toList ++
      List(content.choice("ClearingFight", "story.storyClearing.clearing.fightLabel").copy(row = Some(0)), leave))
  }

  /** Бой с бригадой: вся бригада, бочки с порохом на поле, гости — по пути.
    * Добыча вернёт сюда с отметкой победы. */
  private def fight(user: User, renderer: Renderer): Task[StateType] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { quests =>
      if (!StoryClearingQuest.atClearing(quests)) enter(user, renderer).as(StateType.StoryClearing)
      else for {
        hero <- getHero(user)
        _    <- StoryFight.start(heroDao, user, hero, StoryClearingQuest.foes(foreman), StoryClearingQuest.guests(content, quests),
                  StoryClearingQuest.FightStory, StateType.StoryClearing, WonKey, StoryClearingQuest.Kegs)
        _    <- renderer.show(user, Screen(content.text(s"story.storyClearing.clearing.fightStart${pathKey(quests).capitalize}"), Nil))
      } yield StateType.Battle
    }

  /** Тень отдаёт бригаде долг Братства: хватает — глава закрыта без драки, нет —
    * так и сказать. У прочих путей кнопки нет — старое сообщение ничего не даст. */
  private def pay(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- NpcQuestLog.load(heroDao, user.userId)
      hero   <- getHero(user)
      _      <- if (!StoryClearingQuest.atClearing(quests) || StoryClearingQuest.path(quests) != Path.Shadow) enter(user, renderer)
                else purse.charge(user.userId, hero, StoryClearingQuest.wages).flatMap {
                  case Some(_) => done(user, quests, "story.storyClearing.clearing.paid", paid = true, renderer)
                  case None    =>
                    val screen = clearingScreen(quests, hero)
                    renderer.show(user, screen.copy(text = content.format("story.storyClearing.clearing.notEnough",
                      "wages" -> StoryClearingQuest.wages.toString) + "\n\n" + screen.text))
                }
    } yield StateType.StoryClearing

  /** Бригада побита или получила своё (`paid` — откуп Тени): рассказ пути
    * (`leadKey`) и накладная десятника; пропуск уходит, накладная — в сумку,
    * серебро и опыт, глава закрыта, глава 9 взята. */
  private def done(user: User, quests: NpcQuests, leadKey: String, paid: Boolean, renderer: Renderer): Task[Unit] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      _      <- heroDao.writeSceneData(user.userId, Json.Null)
      hero   <- getHero(user)
      _      <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.StoryPass))
      _      <- QuestSupport.giveItem(inventoryRepo, itemRepo, hero, QuestItemKind.StoryWaybill)
      // Героя читаем уже после откупа: награда ложится поверх того, что осталось.
      silver  = StoryClearingQuest.silver
      _      <- heroDao.updateSilver(user.userId, hero.silver + silver)
      res    <- chapter.complete(user, hero.copy(silver = hero.silver + silver), _.take(NpcQuest.StoryFurnace, now))
      (_, expLine) = res
      after  <- NpcQuestLog.load(heroDao, user.userId)
      // Накладная: Тени её отдаёт сам десятник — вместо расписки или вместе с
      // договором после драки; у прочих она находится у него в кармане.
      shadow  = StoryClearingQuest.path(quests) == Path.Shadow
      waybill = if (paid) "story.storyClearing.waybill.given"
                else if (shadow) "story.storyClearing.waybill.handed"
                else "story.storyClearing.waybill.found"
      _      <- renderer.show(user, Screen(List(leadKey, waybill, "story.storyClearing.waybill.text")
                  .map(k => content.text(k).trim).mkString("\n\n"), Nil))
      _      <- renderer.show(user, Screen(List(
                  content.format(s"story.storyClearing.reward.${pathKey(quests)}", "silver" -> silver.toString, "exp" -> expLine),
                  content.text("story.storyClearing.questDone"),
                  StoryLine.takenLine(content, NpcQuest.StoryFurnace, after)).mkString("\n\n"), List(leave.copy(row = Some(0)))))
    } yield ()

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object StoryClearingState {

  /** Отметка в `scene_data`: бригада Чернолома побита. */
  val WonKey: String = "storyClearingWon"
}
