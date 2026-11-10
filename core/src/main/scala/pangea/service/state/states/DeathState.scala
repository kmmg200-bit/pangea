package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{AzatState, Hero}
import pangea.model.item.ItemStack
import pangea.model.quest.QuestOutcome
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.model.artifact.ArtifactKind
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.model.trauma.{Trauma, TraumaRoll}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.marisa.MarisaHuntState
import pangea.service.state.{AzatData, BarrelThievesQuest, BeltContents, HedgeSongQuest, LostSquadQuest, MarisaQuest, MurlocQuest,
  QuestOutcomeLog, State, StoryMarkQuest, StoryPowderQuest, StrayCasketQuest, UserAction}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

case class DeathState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  artifacts:     Option[ArtifactRepository] = None,
  // Пал от коллектора — исход «Письма Марисе» (см. QuestOutcome).
  outcomes:      Option[QuestOutcomeDao] = None
) extends State {

  override def targetStates: Set[StateType] = Set(StateType.Rest)

  // Death is an effect node, not a screen: entering it runs the full death event
  // (penalties, trauma, drops) immediately — no "continue" button — then routes
  // straight to Rest via autoAdvance.
  override def autoAdvance: Option[StateType] = Some(StateType.Rest)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now          <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero         <- heroDao.getHeroByUserId(user.userId)
                        .flatMap(ZIO.fromOption(_))
                        .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
      battleJson   <- heroDao.readActiveBattle(user.userId)
      battle        = battleJson.flatMap(_.as[SoloPveBattle].toOption)
      monsterName   = battle.map(_.monsterName).getOrElse("Монстр")
      // Смерть от коллектора («Письмо Марисе»): серебра уходит не меньше 15 000,
      // дублонов — 105, задание закрывается, проснуться — в городе.
      collector     = battle.exists(_.story.contains(MarisaQuest.CollectorStory))
      // Смерть в налёте на деревню мурлоков: штрафы обычные, задание остаётся
      // (карта на руках), проснуться — в городе.
      raid          = battle.exists(_.story.contains(MurlocQuest.RaidStory))
      // Смерть у завала в штольне: штрафы обычные, дружина снова ждёт за
      // завалом, проснуться — в городе.
      escort        = battle.exists(_.story.contains(LostSquadQuest.EscortStory))
      // Смерть от хозяина бродячего ларца: штрафы обычные, задание остаётся на
      // том же шаге — проснуться в лабиринте, ларец снова поведёт к нему.
      stray         = battle.exists(_.story.contains(StrayCasketQuest.FightStory))
      // Смерть в волнах у поющего эльфа: штрафы обычные, задание так и не
      // начато — эльфа надо встретить снова; проснуться в лабиринте.
      hedge         = battle.exists(_.story.contains(HedgeSongQuest.WaveStory))
      // Смерть на складе у причала: штрафы обычные, украденное всё ещё у банды —
      // по следу можно идти снова; проснуться в порту.
      thieves       = battle.exists(_.story.contains(BarrelThievesQuest.FightStory))
      // Смерть у тайника Сороки: штрафы обычные, глава на том же шаге —
      // проснуться в лабиринте, тайник ждёт следующего осмотра.
      cache         = battle.exists(_.story.contains(StoryMarkQuest.FightStory))
      // Смерть на дегтярном складе: глава на том же шаге — проснуться в
      // порту, шифровка поведёт на склад снова.
      warehouse     = battle.exists(_.story.contains(StoryPowderQuest.FightStory))
      // Герой пал раньше, а отряд доигрывал за него и проиграл: травму за этот
      // бой он уже получил, когда упал (см. BattleState) — второй смерть не даёт.
      fellEarlier   = battle.exists(_.group.heroDown)
      withMarisa   <- heroDao.readSceneData(user.userId).map(_.flatMap(_.as[LootData].toOption)
                        .flatMap(_.eventData).flatMap(_.as[MarisaHuntState.Progress].toOption).exists(_.withMarisa))
      // Благословение Азата смягчает штраф: теряется на BlessingBonusPct% меньше.
      azat         <- AzatData.load(heroDao, user.userId, now)
      blessed       = azat.blessingActive(now)
      expPenalty    = (hero.exp * DeathState.ExpLossPct / 100L).max(0L)
      expLost       = if (blessed) expPenalty * (100L - AzatState.BlessingBonusPct) / 100L else expPenalty
      newExp        = (hero.exp - expLost).max(0L)
      silverLost    = if (collector) (hero.silver / 2).max(MarisaQuest.DeathSilverMin).min(hero.silver) else hero.silver / 2
      newSilver     = hero.silver - silverLost
      doubloonsLost = if (collector) MarisaQuest.DeathDoubloons.min(hero.doubloons) else 0L

      // traumas that are still active are kept; expired ones reset to empty list
      existingNames = if (hero.traumaActive(now)) hero.traumaNames else Nil

      // Тир травмы (лёгкие → средние → тяжёлые) выбирает общий хелпер: тем же
      // правилом пользуются и другие источники травм, напр. смерч элементаля.
      pool          = TraumaRoll.pool(existingNames)

      // «Первопроходец» (порог 9): травмы вдвое короче.
      traumaUntil   = hero.traumaUntilWithNew(now)

      _            <- heroDao.updateExpAndLevel(user.userId, newExp, hero.lvl, hero.upgradePoints)
      _            <- heroDao.updateSilver(user.userId, newSilver)
      _            <- ZIO.when(doubloonsLost > 0L)(heroDao.updateDoubloons(user.userId, hero.doubloons - doubloonsLost))
      _            <- heroDao.clearActiveBattle(user.userId)
      // Первое сообщение после смерти — сразу убираем боевую клавиатуру, чтобы
      // не висела поверх «обморока».
      _            <- renderer.show(user, Screen(
                        content.format("death.penalty",
                          "expLost"    -> expLost.toString,
                          "silverLost" -> silverLost.toString), Nil, hideKeyboard = true))
      _            <- ZIO.unless(fellEarlier)(applyTrauma(user, existingNames, pool, traumaUntil, renderer))

      _            <- dropItems(user, hero.id, monsterName, renderer)
      // Пал в бою, куда брали отряд, — резерв уходит. В бой один на один отряд
      // не брали: он оставался дома, и резерв цел.
      _            <- ZIO.when(battle.exists(_.squadTaken))(reserveLeaves(user, hero, now, renderer))
      // Время в мёртвом режиме растёт с уровнем героя и асимптотически
      // стремится к 90 минутам, не достигая их:
      //   deathTimeMinutes = 90 - 89 * exp(-k * (level - 1))
      // На 1 уровне == 1 минута; k — коэффициент роста.
      // «Первопроходец» (порог 9) сокращает этот отдых вдвое.
      deathRestMinutes = 90.0 - 89.0 * math.exp(-DeathState.RestGrowthK * (hero.lvl - 1L))
      deathRestMs      = (deathRestMinutes * 60000.0 * hero.sets.deathRestMult).toLong
      // Коллектор: задание закрыто здесь же, а что сказать и куда идти —
      // RestState прочитает при пробуждении. Как «Письмо Марисе» закончили
      // другие — готовым текстом (`wakeNote`), после «Квест завершён».
      fellStats    <- if (collector) MarisaQuest.finish(heroDao, inventoryRepo, content, user.userId, hero) *>
                        QuestOutcomeLog.recordAndCount(outcomes, content, hero, QuestOutcome.MarisaFell)
                      else ZIO.none
      wake          = if (collector) Json.obj(
                        "wakeTo"    -> (StateType.GlobalMap: StateType).asJson,
                        "wakeLines" -> List(
                          if (withMarisa) "marisa.deathReturnWithMarisa" else "marisa.deathReturn",
                          "marisa.questDone").asJson,
                        "wakeNote"  -> fellStats.asJson)
                      else if (raid) Json.obj(
                        "wakeTo"    -> (StateType.GlobalMap: StateType).asJson,
                        "wakeLines" -> List("murlocVillage.raid.death").asJson)
                      else if (escort) Json.obj(
                        "wakeTo"    -> (StateType.GlobalMap: StateType).asJson,
                        "wakeLines" -> List("lostSquad.escort.death").asJson)
                      else if (stray) Json.obj("wakeLines" -> List("strayCasket.death").asJson)
                      else if (hedge) Json.obj("wakeLines" -> List("hedgeSong.death").asJson)
                      else if (thieves) Json.obj(
                        "wakeTo"    -> (StateType.HarborQuarter: StateType).asJson,
                        "wakeLines" -> List("barrelThieves.death").asJson)
                      else if (cache) Json.obj("wakeLines" -> List("story.storyMark.cache.death").asJson)
                      else if (warehouse) Json.obj(
                        "wakeTo"    -> (StateType.HarborQuarter: StateType).asJson,
                        "wakeLines" -> List("story.storyPowder.warehouse.death").asJson)
                      else Json.obj()
      _            <- heroDao.writeSceneData(user.userId, Json.obj(
                        "restDurationMs" -> deathRestMs.asJson,
                        "postDeath"      -> true.asJson).deepMerge(wake))
    } yield ()

  // Unused in the normal flow (Death routes onward via autoAdvance). Kept as a
  // safe fallback so a hero somehow sitting in Death still moves to Rest.
  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    ZIO.succeed(StateType.Rest)

  // Даёт новую травму из текущего тира; если пул пуст (все травмы уже собраны —
  // максимум), новую не выдаёт, лишь продлевает таймер снятия на 8 часов и
  // показывает соответствующее сообщение.
  /** Что лежит в артефактах и может пропасть вместе с сумкой — с шансом
    * потери, который зависит от ступени артефакта (см. [[DeathState.stashLossPct]]). */
  private def stashItems(heroId: pangea.model.hero.HeroId): Task[List[(ArtifactKind, Int, pangea.model.item.Item)]] =
    artifacts match {
      case None => ZIO.succeed(Nil)
      case Some(repo) =>
        repo.get(heroId).either.map {
          case Right(all) =>
            ArtifactKind.values.toList.filterNot(_.safeFromDeath).flatMap { kind =>
              val a = all.of(kind)
              a.items.data.filterNot(_.isQuestItem).map(item => (kind, DeathState.stashLossPct(a.tier), item))
            }
          case Left(_) => Nil
        }
    }

  /** Один бросок на вещь: `lossPct` процентов, что она пропадёт. */
  private def rollLoss(item: pangea.model.item.Item, lossPct: Int)(remove: => zio.ZIO[Any, Any, Any]): Task[Option[pangea.model.item.Item]] =
    Random.nextIntBetween(0, 100).flatMap { roll =>
      if (roll >= lossPct) ZIO.none else remove.ignore.as(Some(item))
    }

  /** Резерв уходит от павшего героя весь (см. [[pangea.model.squad.Squad.reserveLeaves]]):
    * наёмники — на сутки, поднятые — насовсем. Одна строка на всех: одинаковые
    * имена схлопнуты в «имя ×N». Пуст резерв — ни записи, ни строки. */
  private def reserveLeaves(user: User, hero: Hero, nowMs: Long, renderer: Renderer): Task[Unit] = {
    val (squad, gone) = hero.squad.reserveLeaves(nowMs)
    ZIO.when(gone.nonEmpty)(
      heroDao.updateSquad(user.userId, squad) *>
        renderer.show(user, Screen(content.format("death.reserveLeft",
          "names" -> BeltContents.counted(gone.map(_.name))), Nil))).unit
  }

  private def applyTrauma(
    user:          User,
    existingNames: List[String],
    pool:          Seq[Trauma],
    traumaUntil:   Long,
    renderer:      Renderer
  ): Task[Unit] =
    if (pool.isEmpty)
      heroDao.updateTrauma(user.userId, Some(traumaUntil), existingNames) *>
        renderer.show(user, Screen(content.text("death.traumaMax"), Nil))
    else
      for {
        idx   <- Random.nextIntBetween(0, pool.length)
        trauma = pool(idx)
        _     <- heroDao.updateTrauma(user.userId, Some(traumaUntil), existingNames :+ trauma.name)
        _     <- renderer.show(user, Screen(
                   content.format("death.trauma",
                     "traumaName"  -> trauma.name,
                     "description" -> trauma.description), Nil))
      } yield ()

  private def dropItems(user: User, heroId: pangea.model.hero.HeroId, monsterName: String, renderer: Renderer): Task[Unit] =
    for {
      inventory <- inventoryRepo.get(heroId).orElse(ZIO.succeed(
                     pangea.model.inventory.Inventory(0L, heroId, 0L,
                       pangea.model.inventory.Inventory.Items(Nil))))
      // Сюжетные предметы при смерти не теряются.
      realItems  = inventory.items.data.filter(i => i.id != 0L && !i.isQuestItem)
      // Ларец и Живая сумка от смерти не спасают, но берегут: чем выше ступень,
      // тем реже из них что-то пропадает. Прячет целиком только Миниатюрный
      // шкаф (см. `ArtifactKind`).
      stash     <- stashItems(heroId)
      // Бросок идёт на КАЖДЫЙ предмет отдельно, в том числе на каждый камень и
      // каждую горсть пыли: сложенные в одну строку на экране, в сумке они
      // остаются разными вещами, и уносят их поштучно, а не стопкой.
      lostBag   <- ZIO.foreach(realItems) { item =>
                     rollLoss(item, DeathState.LossPct)(inventoryRepo.removeItem(item.id, heroId).unit)
                   }.map(_.flatten)
      lostStash <- ZIO.foreach(stash) { case (kind, lossPct, item) =>
                     rollLoss(item, lossPct)(ZIO.foreachDiscard(artifacts)(_.take(heroId, kind, item.id)))
                   }.map(_.flatten)
      lost       = lostBag ++ lostStash
      // Одно сообщение на всё потерянное: одинаковые вещи схлопнуты в «имя ×N».
      _         <- ZIO.when(lost.nonEmpty) {
                     val names = lost
                       .groupBy(i => ItemStack.key(i).getOrElse(i.id.toString))
                       .values.toList
                       .sortBy(_.head.name)
                       .map(group =>
                         if (group.sizeIs > 1) s"${group.head.name} ×${group.size}" else group.head.name)
                     renderer.show(user, Screen(
                       content.format("death.itemDropped",
                         "monsterName" -> monsterName,
                         "itemName"    -> names.mkString(", ")), Nil))
                   }
    } yield ()
}

object DeathState {
  // Коэффициент роста времени мёртвого режима по уровню героя (см. формулу в enter).
  val RestGrowthK: Double = 0.01

  /** Сколько процентов опыта сгорает при смерти (до скидки благословения). */
  val ExpLossPct: Long = 10L

  /** Шанс потерять вещь при смерти, в процентах на каждую. */
  val LossPct: Int = 25

  /** Насколько каждая ступень ларца или Живой сумки бережёт добро внутри. */
  val StashSavePctPerTier: Int = 3

  /** Шанс потерять вещь из ларца или Живой сумки: [[LossPct]] минус
    * [[StashSavePctPerTier]] за ступень — на 5-й ступени остаётся 10%. */
  def stashLossPct(tier: Int): Int = (LossPct - StashSavePctPerTier * tier).max(0)
}
