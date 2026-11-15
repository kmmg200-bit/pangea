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
import pangea.repository.item.ItemRepository
import pangea.service.parcel.Parcels
import pangea.model.trauma.{Trauma, TraumaRoll}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.marisa.MarisaHuntState
import pangea.service.state.{AzatData, BarrelThievesQuest, BartiDeliveryQuest, BartiDoorQuest, BartiRoofQuest, BartiSaltQuest, BeltContents, BigDealQuest, BramblQuest, HedgeSongQuest, HotGoodsQuest, JorgenQuest, LostSquadQuest, MarisaQuest,
  FaelinQuest, MurlocQuest, PlukhQuest, QuestOutcomeLog, State, StoryFaceQuest, StoryClearingQuest, StoryFurnaceQuest, StoryListQuest, StoryMarkQuest, StoryMorningQuest, StoryNewcomerQuest, StoryPowderQuest,
  StoryToolQuest,
  StrayCasketQuest, UserAction, VillageLair, Villages}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

case class DeathState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  artifacts:     Option[ArtifactRepository] = None,
  // Пал от коллектора — исход «Письма Марисе» (см. QuestOutcome).
  outcomes:      Option[QuestOutcomeDao] = None,
  // Пал в засаде у каравана — амулет от каджитов: сохранить и, если не влез в
  // сумку, — посылкой (см. BigDealQuest.pay).
  itemRepo:      Option[ItemRepository] = None,
  parcels:       Option[Parcels] = None
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
      // Смерть в логове огненных (долг Йоргена): штрафы обычные, список отряда
      // при герое — к логову можно снова; проснуться в лабиринте.
      jorgenLair    = battle.exists(_.story.contains(JorgenQuest.LairStory))
      // Смерть в Зале предков (замок Брамбла): штрафы обычные, замок при герое
      // — к залу ведёт любая серебряная жила; проснуться в лабиринте.
      bramblHall    = battle.exists(_.story.contains(BramblQuest.HallStory))
      // Смерть в бою с ловчими (багор Плюха): штрафы обычные, печать при герое —
      // позвать их можно снова; проснуться в лабиринте.
      plukhHunt     = battle.exists(_.story.contains(PlukhQuest.HuntStory))
      // Смерть у старых корней (лук Фаэлина): штрафы обычные, срубленный лук при
      // герое — к корням можно вернуться; проснуться в лабиринте.
      faelinRoots   = battle.exists(_.story.contains(FaelinQuest.RootsStory))
      // Смерть в налёте на деревню: штрафы обычные, деревни больше нет —
      // проснуться в лабиринте. Смерть в логове беды деревни (банда, гнездо):
      // контракт при герое, логово ждёт — тоже в лабиринте, со строкой логова.
      villageRaid   = battle.exists(_.story.contains(Villages.RaidStory))
      villageLair   = battle.flatMap(b => VillageLair.all.find(l => b.story.contains(l.story)))
      // Смерть в засаде на караван деревни: караван разграблен, контракт
      // сорван (так решил пользователь) — проснуться в лабиринте со строкой засады.
      villageCaravan = battle.exists(_.story.contains(Villages.CaravanStory))
      // Смерть в стане воров артефакта: контракт при герое, но след остыл —
      // погоню начинать заново; проснуться в лабиринте со строкой стана.
      villageArtifact = battle.exists(_.story.contains(Villages.ArtifactStory))
      // Смерть в бою с охотниками за головами на выходе с заставы: голову главаря
      // они забрали, а логово всё равно разбито — к старосте за платой без
      // доплаты; проснуться в лабиринте со строкой охотников.
      villageHunters = battle.exists(_.story.contains(Villages.HuntersStory))
      // Смерть на складе у причала: штрафы обычные, украденное всё ещё у банды —
      // по следу можно идти снова; проснуться в порту.
      thieves       = battle.exists(_.story.contains(BarrelThievesQuest.FightStory))
      // Смерть у тайника Сороки: штрафы обычные, глава на том же шаге —
      // проснуться в лабиринте, тайник ждёт следующего осмотра.
      cache         = battle.exists(_.story.contains(StoryMarkQuest.FightStory))
      // Смерть на дегтярном складе: глава на том же шаге — проснуться в
      // порту, шифровка поведёт на склад снова.
      warehouse     = battle.exists(_.story.contains(StoryPowderQuest.FightStory))
      // Смерть у Мельничного моста: глава на том же шаге — проснуться в
      // городе, страница поведёт к мосту снова.
      bridge        = battle.exists(_.story.contains(StoryListQuest.FightStory))
      // Смерть в переплётной и на кладбище: глава на том же шаге — проснуться
      // в городе, ключ (или записка) поведёт снова.
      bindery       = battle.exists(_.story.contains(StoryFaceQuest.FightStory))
      graveyard     = battle.exists(_.story.contains(StoryToolQuest.FightStory))
      // Смерть в «Трёх костях», в подвале совета и у Барти («Наутро»): глава
      // на том же шаге — проснуться у порта, в городе или на Торговой площади.
      den           = battle.exists(_.story.contains(StoryMorningQuest.DenStory))
      cellar        = battle.exists(_.story.contains(StoryMorningQuest.CellarStory))
      bartiHouse    = battle.exists(_.story.contains(StoryMorningQuest.BartiStory))
      // Смерть у лавки Барти («Крыша»): шаг тот же — проснуться на Торговой
      // площади, стража придёт следующей ночью.
      bartiShop     = battle.exists(_.story.contains(BartiRoofQuest.FightStory))
      // Смерть у синей двери («Синяя дверь»): шаг тот же — проснуться на Торговой
      // площади, поджигатели вернутся.
      bartiDoor     = battle.exists(_.story.contains(BartiDoorQuest.FightStory))
      // Смерть в лагере у Старой вырубки («Большая поставка»): шаг тот же —
      // Барти увёз самогон обратно, проснуться на Торговой площади.
      delivery      = battle.exists(_.story.contains(BartiDeliveryQuest.FightStory))
      // Смерть на соляной колее («Соль»): шаг тот же — каджиты отвезли героя к
      // Сухому колодцу, проснуться в городе.
      salt          = battle.exists(_.story.contains(BartiSaltQuest.FightStory))
      // Смерть на главной площади («Новенький»): шаг тот же — проснуться там же.
      square        = battle.exists(_.story.exists(StoryNewcomerQuest.FightStories.contains))
      // Смерть на просеке («Просека»): шаг тот же — проснуться в лабиринте,
      // пропуск поведёт снова.
      clearing      = battle.exists(_.story.contains(StoryClearingQuest.FightStory))
      // Смерть у Загона («Печь»): шаг тот же — Горн или каджиты увезли героя,
      // проснуться в городе, накладная поведёт снова.
      furnaceRaid   = battle.exists(_.story.contains(StoryFurnaceQuest.RaidStory))
      furnaceNight  = battle.exists(_.story.contains(StoryFurnaceQuest.NightStory))
      // Смерть у изгороди, где звери зажали Барти: задание так и не начато —
      // Барти встретится снова; проснуться в лабиринте.
      barti         = battle.exists(_.story.contains(HotGoodsQuest.FightStory))
      // Смерть в засаде у каравана: «Большое дело» всё равно закрыто — каджиты
      // отбились сами и заплатили меньше; Барти расскажет, когда герой очнётся.
      ambush        = battle.exists(_.story.contains(BigDealQuest.AmbushStory))
      // Герой пал раньше, а отряд доигрывал за него и проиграл: травму за этот
      // бой он уже получил, когда упал (см. BattleState) — второй смерть не даёт.
      fellEarlier   = battle.exists(_.group.heroDown)
      withMarisa   <- heroDao.readSceneData(user.userId).map(_.flatMap(_.as[LootData].toOption)
                        .flatMap(_.eventData).flatMap(_.as[MarisaHuntState.Progress].toOption).exists(_.withMarisa))
      // Благословение Азата смягчает штраф: теряется на BlessingBonusPct% меньше,
      // а отдых после смерти вдвое короче (см. DeathState.restMs).
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
      // Время в мёртвом режиме растёт с уровнем героя, но не дольше 30 минут;
      // «Первопроходец» (порог 9) и благословение Азата сокращают его — каждое
      // вдвое (см. DeathState.restMs).
      deathRestMs   = DeathState.restMs(hero.lvl, hero.sets.deathRestMult, blessed)
      // Коллектор: задание закрыто здесь же, а что сказать и куда идти —
      // RestState прочитает при пробуждении. Как «Письмо Марисе» закончили
      // другие — готовым текстом (`wakeNote`), после «Квест завершён».
      fellStats    <- if (collector) MarisaQuest.finish(heroDao, inventoryRepo, content, user.userId, hero) *>
                        QuestOutcomeLog.recordAndCount(outcomes, content, hero, QuestOutcome.MarisaFell)
                      else ZIO.none
      // Засада у каравана: плата, опыт и «Квест завершён» — готовым текстом
      // (`wakeNote`), после рассказа Барти. Опыт ложится поверх штрафа за смерть.
      // Как «Большое дело» закончили другие — следом, как у «Письма Марисе».
      dealPaid     <- if (ambush) BigDealQuest.pay(heroDao, inventoryRepo, itemRepo, parcels, content, user,
                        hero.copy(exp = newExp, silver = newSilver), BigDealQuest.FallenDoubloons, "barti.caravan.fallenReward",
                        "barti.caravan.amuletFallen")
                      else ZIO.none
      dealStats    <- if (dealPaid.isDefined) QuestOutcomeLog.recordAndCount(outcomes, content, hero, QuestOutcome.BigDealFell)
                      else ZIO.none
      dealNote      = dealPaid.map(QuestOutcomeLog.withStats(_, dealStats))
      // Караван разграблен: береста уходит из сумки, договор — из журнала, и
      // деревни снова встречаются.
      _            <- ZIO.when(villageCaravan)(Villages.abandon(heroDao, inventoryRepo, user.userId, hero))
      _            <- ZIO.when(villageHunters)(Villages.dropHead(inventoryRepo, hero))
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
                      else if (jorgenLair) Json.obj("wakeLines" -> List("jorgen.lair.death").asJson)
                      else if (bramblHall) Json.obj("wakeLines" -> List("bramblLock.hall.death").asJson)
                      else if (plukhHunt) Json.obj("wakeLines" -> List("plukhGaff.hunt.death").asJson)
                      else if (faelinRoots) Json.obj("wakeLines" -> List("faelinBow.roots.death").asJson)
                      else if (villageRaid) Json.obj("wakeLines" -> List("village.raid.death").asJson)
                      else if (villageCaravan) Json.obj("wakeLines" -> List("village.caravan.death").asJson)
                      else if (villageArtifact) Json.obj("wakeLines" -> List("village.artifact.death").asJson)
                      else if (villageHunters) Json.obj("wakeLines" -> List("village.bandits.hunters.death").asJson)
                      else if (villageLair.isDefined)
                        Json.obj("wakeLines" -> villageLair.map(l => s"village.${l.texts}.death").toList.asJson)
                      else if (thieves) Json.obj(
                        "wakeTo"    -> (StateType.HarborQuarter: StateType).asJson,
                        "wakeLines" -> List("barrelThieves.death").asJson)
                      else if (cache) Json.obj("wakeLines" -> List("story.storyMark.cache.death").asJson)
                      else if (warehouse) Json.obj(
                        "wakeTo"    -> (StateType.HarborQuarter: StateType).asJson,
                        "wakeLines" -> List("story.storyPowder.warehouse.death").asJson)
                      else if (bridge) Json.obj(
                        "wakeTo"    -> (StateType.GlobalMap: StateType).asJson,
                        "wakeLines" -> List("story.storyList.bridge.death").asJson)
                      else if (bindery) Json.obj(
                        "wakeTo"    -> (StateType.GlobalMap: StateType).asJson,
                        "wakeLines" -> List("story.storyFace.bindery.death").asJson)
                      else if (graveyard) Json.obj(
                        "wakeTo"    -> (StateType.GlobalMap: StateType).asJson,
                        "wakeLines" -> List("story.storyTool.graveyard.death").asJson)
                      else if (den) Json.obj(
                        "wakeTo"    -> (StateType.HarborQuarter: StateType).asJson,
                        "wakeLines" -> List("story.storyMorning.den.death").asJson)
                      else if (cellar) Json.obj(
                        "wakeTo"    -> (StateType.GlobalMap: StateType).asJson,
                        "wakeLines" -> List("story.storyMorning.cellar.death").asJson)
                      else if (bartiHouse) Json.obj(
                        "wakeTo"    -> (StateType.MarketSquare: StateType).asJson,
                        "wakeLines" -> List("story.storyMorning.barti.death").asJson)
                      else if (square) Json.obj(
                        "wakeTo"    -> (StateType.CityCenter: StateType).asJson,
                        "wakeLines" -> List("story.storyNewcomer.square.death").asJson)
                      else if (clearing) Json.obj("wakeLines" -> List("story.storyClearing.clearing.death").asJson)
                      else if (furnaceRaid) Json.obj(
                        "wakeTo"    -> (StateType.GlobalMap: StateType).asJson,
                        "wakeLines" -> List("story.storyFurnace.pen.deathShield").asJson)
                      else if (furnaceNight) Json.obj(
                        "wakeTo"    -> (StateType.GlobalMap: StateType).asJson,
                        "wakeLines" -> List("story.storyFurnace.pen.deathAlone").asJson)
                      else if (bartiDoor) Json.obj(
                        "wakeTo"    -> (StateType.MarketSquare: StateType).asJson,
                        "wakeLines" -> List("barti.door.death").asJson)
                      else if (delivery) Json.obj(
                        "wakeTo"    -> (StateType.MarketSquare: StateType).asJson,
                        "wakeLines" -> List("barti.delivery.death").asJson)
                      else if (salt) Json.obj(
                        "wakeTo"    -> (StateType.GlobalMap: StateType).asJson,
                        "wakeLines" -> List("barti.salt.death").asJson)
                      else if (bartiShop) Json.obj(
                        "wakeTo"    -> (StateType.MarketSquare: StateType).asJson,
                        "wakeLines" -> List("barti.roof.death").asJson)
                      else if (barti) Json.obj("wakeLines" -> List("barti.meet.death").asJson)
                      else if (ambush) Json.obj(
                        "wakeTo"    -> (StateType.MarketSquare: StateType).asJson,
                        "wakeLines" -> List("barti.caravan.fallen").asJson,
                        "wakeNote"  -> dealNote.asJson)
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
    * потери, который зависит от ступени артефакта (см. [[DeathState.stashLossPct]]).
    * Сюжетное не пропадает и оттуда. */
  private def stashItems(heroId: pangea.model.hero.HeroId): Task[List[(ArtifactKind, Int, pangea.model.item.Item)]] =
    artifacts match {
      case None => ZIO.succeed(Nil)
      case Some(repo) =>
        repo.get(heroId).either.map {
          case Right(all) =>
            ArtifactKind.values.toList.filterNot(_.safeFromDeath).flatMap { kind =>
              val a = all.of(kind)
              a.items.data.filterNot(i => i.isQuestItem || i.isStoryGear).map(item => (kind, DeathState.stashLossPct(a.tier), item))
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
      // Сюжетные предметы и сюжетная экипировка при смерти не теряются.
      realItems  = inventory.items.data.filter(i => i.id != 0L && !i.isQuestItem && !i.isStoryGear)
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
  // Коэффициент роста времени мёртвого режима по уровню героя (см. [[restMs]]).
  val RestGrowthK: Double = 0.01

  /** Дольше скольких минут отдых после смерти не длится — до сокращений. */
  val MaxRestMinutes: Double = 30.0

  /** Сколько длится отдых после смерти, мс. Растёт с уровнем героя:
    *   90 − 89 · e^(−k · (уровень − 1)) минут
    * — на 1-м уровне ровно минута, — но не дольше [[MaxRestMinutes]] (потолок
    * наступает с 41-го уровня). `mult` — доля, что оставляет «Первопроходец»
    * (порог 9 — половину); пал благословлённым (`blessed`) — ещё на
    * [[AzatState.BlessingDeathRestCutPct]]% короче, то есть не дольше 15 минут. */
  def restMs(lvl: Long, mult: Double, blessed: Boolean): Long = {
    val minutes  = (90.0 - 89.0 * math.exp(-RestGrowthK * (lvl - 1L))).min(MaxRestMinutes)
    val blessing = if (blessed) (100L - AzatState.BlessingDeathRestCutPct) / 100.0 else 1.0
    (minutes * 60000.0 * mult * blessing).toLong
  }

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
