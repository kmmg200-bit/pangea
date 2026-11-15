package pangea.service.state

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Choice, ChoiceColor, Renderer, SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.marisa.MarisaHuntState
import pangea.service.state.states.murloc.MurlocVillageState
import pangea.service.state.states.race.RaceQuestState
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Кнопки заданий на карточках сюжетных предметов — одни на бота и приложение.
  * Какая кнопка стоит на карточке сейчас, решает [[QuestCard.choice]]; что
  * будет, если её нажать, — [[go]]: сцена задания, отказ словами бота, вопрос
  * или дело прямо здесь (письмо Марисы). Где герой, решает вызывающий: бот — по
  * `return_state` («Персонаж» открыт оттуда), приложение — по сцене героя
  * ([[QuestCard.place]]). В сцену задания ведёт слой переходов — ход бота или
  * дверь из приложения ([[SceneDoor]]): сцены друг друга не знают. */
final class QuestCard(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: ItemRepository,
                      content: SceneContent) {
  import QuestCard.{Go, Prep}

  /** Нажали кнопку `id` с карточки. `hero` и журнал `quests` — как сейчас,
    * `from` — где герой: городские задания начинают только из города, задания
    * лабиринта — только с его развилки. Шаг не тот — кнопка устарела. */
  def go(id: String, hero: Hero, quests: NpcQuests, from: Option[StateType]): Go = {
    val inCity      = from.exists(StateType.cityStates.contains)
    val inLabyrinth = from.contains(StateType.Dungeon)

    def refused(key: String): Go = Go.Refused(content.text(key), toList = false)

    // Карты и купоны: отказ — и к списку сумки, как было в боте.
    def mapOrCoupon(key: String)(go: => Go): Go =
      if (inCity) go else Go.Refused(content.text(key), toList = true)

    def fromCity(onStep: Boolean, key: String, scene: StateType, prep: Prep = QuestCard.NoPrep): Go =
      if (!onStep) Go.Stale else if (!inCity) refused(key) else Go.Enter(scene, prep)

    def inMaze(onStep: Boolean, key: String, scene: StateType, deep: Option[String] = None): Go =
      if (!onStep) Go.Stale
      else if (!inLabyrinth) refused(key)
      else deep.fold[Go](Go.Enter(scene, QuestCard.NoPrep))(Go.Refused(_, toList = false))

    // Контракт деревни: из лабиринта, не ближе уровня деревень, и сцена — с
    // чистого листа.
    def inMazeFresh(onStep: Boolean, target: StateType): Go =
      inMaze(onStep, "village.card.onlyInLabyrinth", target,
        Option.unless(Villages.deepEnough(hero.dungeonLevel))(
          content.format("village.card.tooShallow", "level" -> Villages.MinDungeonLevel.toString))) match {
        case Go.Enter(to, _) => Go.Enter(to, scene(Json.Null))
        case other           => other
      }

    def hunt(withMarisa: Boolean): Go =
      Go.Enter(StateType.MarisaHunt, scene(MarisaHuntState.Progress(MarisaHuntState.Step.Road, withMarisa).asJson))

    id match {
      case "OpenLetter" => Go.Here((user, r) => openLetter(user, hero, r))
      // Карта Кельвина: если Мариса ждёт — сначала спросить, брать ли её.
      case "UseKelvinMap" =>
        mapOrCoupon("marisa.mapOnlyInCity")(
          if (quests.onStep(NpcQuest.Marisa, 3)) Go.Ask(content.screen("marisa.mapTakeMarisa")) else hunt(withMarisa = false))
      case "MapWithMarisa" => mapOrCoupon("marisa.mapOnlyInCity")(hunt(withMarisa = true))
      case "MapAlone"      => mapOrCoupon("marisa.mapOnlyInCity")(hunt(withMarisa = false))
      // Карта деревни мурлоков: дальше развилку показывает сама деревня.
      case "UseMurlocMap" =>
        mapOrCoupon("marisa.mapOnlyInCity")(Go.Enter(StateType.MurlocVillage,
          scene(MurlocVillageState.Progress(MurlocVillageState.Step.Route).asJson)))
      // Купоны: квест расы начинается сразу, новое имя пишут текстом.
      case "RaceChange" =>
        mapOrCoupon("raceQuest.onlyInCity")(Go.Enter(StateType.RaceQuest, scene(RaceQuestState.Progress.start.asJson)))
      case "NameChange" => mapOrCoupon("nameChange.onlyInCity")(Go.Enter(StateType.NameChange, QuestCard.NoPrep))
      // Сытый бродячий ларец ведёт к хозяину — только в лабиринте, на любом уровне.
      case "StrayGo" => inMaze(StrayCasketQuest.leads(quests), "strayCasket.card.onlyInLabyrinth", StateType.StrayCasket)
      case "BarrelTrail" => fromCity(BarrelThievesQuest.onTrail(quests), "barrelThieves.onlyInCity", StateType.BarrelThieves)
      // Тайник Сороки — не ближе пятого поворота.
      case "StoryCacheGo" =>
        inMaze(StoryMarkQuest.toCache(quests), "story.storyMark.card.onlyInLabyrinth", StateType.StoryCache,
          Option.unless(StoryMarkQuest.deepEnough(hero.dungeonLevel))(
            content.format("story.storyMark.card.tooShallow", "level" -> StoryMarkQuest.CacheLevel.toString)))
      // Логово огненных — не ближе поворота, где водятся логова элементалей.
      case "JorgenLairGo" =>
        inMaze(JorgenQuest.toLair(quests), "jorgen.card.onlyInLabyrinth", StateType.JorgenLair,
          Option.unless(JorgenQuest.deepEnough(hero.dungeonLevel))(
            content.format("jorgen.card.tooShallow", "level" -> JorgenQuest.LairDungeonLevel.toString)))
      case "FaelinRootsGo" => inMaze(FaelinQuest.toRoots(quests), "faelinBow.card.onlyInLabyrinth", StateType.FaelinRoots)
      // Контракт деревни: к беде деревни, потом — назад за платой; всё — из
      // лабиринта и не ближе уровня, где встречаются деревни. Сцена — с
      // чистого листа: что осталось в scene_data от прошлой деревни, не её.
      case "VillageGo" =>
        Villages.goTo(quests).fold[Go](Go.Stale) { case (scene, _) => inMazeFresh(onStep = true, scene) }
      case "VillageReturn" =>
        inMazeFresh(Villages.toPay(quests), StateType.Village)
      case "PlukhHuntGo"   => inMaze(PlukhQuest.toHunt(quests), "plukhGaff.card.onlyInLabyrinth", StateType.PlukhHunt)
      // Главы сюжета: начинают из города, и глава берётся здесь, если ещё не взята.
      case "StoryPortGo" =>
        fromCity(StoryPowderQuest.portOpen(quests), "story.storyPowder.card.onlyInCity", StateType.StoryWarehouse,
          takeChapter(NpcQuest.StoryPowder, quests))
      case "StoryArchiveGo" =>
        fromCity(StoryFaceQuest.archiveOpen(quests), "story.storyFace.card.onlyInCity", StateType.StoryArchive,
          takeChapter(NpcQuest.StoryFace, quests))
      case "StoryGraveyardGo" =>
        fromCity(StoryToolQuest.open(quests), "story.storyTool.card.onlyInCity", StateType.StoryGraveyard,
          takeChapter(NpcQuest.StoryTool, quests))
      case "StoryBinderyGo" => fromCity(StoryFaceQuest.toBindery(quests), "story.storyFace.card.onlyInCity", StateType.StoryBindery)
      case "StoryMeetGo"    => fromCity(StoryBrandQuest.toMeeting(quests), "story.storyBrand.card.onlyInCity", StateType.StoryMeeting)
      case "StorySojkaGo" =>
        fromCity(StoryListQuest.toSojka(quests) || StoryListQuest.toConfront(quests), "story.storyList.card.onlyInCity",
          StateType.StorySojka)
      case "StoryBridgeGo"  => fromCity(StoryListQuest.toBridge(quests), "story.storyList.card.onlyInCity", StateType.StoryBridge)
      case "BartiHouseGo"   => fromCity(BigDealQuest.houseOpen(quests), "barti.card.onlyInCity", StateType.BartiHouse)
      case "BartiCaravanGo" => fromCity(BigDealQuest.toCaravan(quests), "barti.card.caravanOnlyInCity", StateType.BartiCaravan)
      // Акт II, «Наутро»: Тень — в «Три кости» за писарем, Щит — ночью в подвал
      // совета, одиночка — к Барти. Глава к этому времени уже взята дома.
      case "StoryDenGo"    => fromCity(StoryMorningQuest.toDen(quests), "story.storyMorning.card.onlyInCity", StateType.StoryDen)
      case "StoryCellarGo" => fromCity(StoryMorningQuest.toCellar(quests), "story.storyMorning.card.onlyInCity", StateType.StoryCellar)
      case "StoryBartiGo"  => fromCity(StoryMorningQuest.atBarti(quests), "story.storyMorning.card.onlyInCity", StateType.BartiHouse)
      // «Новенький»: с карточки описи — на главную площадь, из города.
      case "StorySquareGo" => fromCity(StoryNewcomerQuest.atSquare(quests), "story.storyNewcomer.card.onlyInCity", StateType.StorySquare)
      // «Просека»: с карточки пропуска — к просеке в изгороди, из лабиринта.
      case "StoryClearingGo" =>
        inMaze(StoryClearingQuest.atClearing(quests), "story.storyClearing.card.onlyInLabyrinth", StateType.StoryClearing)
      // «Крыша»: с карточки квитанции — в лавку Барти, из города.
      case "BartiShopGo"   => fromCity(BartiRoofQuest.toShop(quests), "barti.roof.card.onlyInCity", StateType.BartiShop)
      // «Соль» (одиночка): с карточки накладной — спросить Барти, потом к каравану.
      case "BartiSaltAsk"  => fromCity(BartiSaltQuest.available(quests), "barti.salt.card.onlyInCity", StateType.BartiHouse)
      case "BartiSaltGo"   => fromCity(BartiSaltQuest.toCaravan(quests), "barti.salt.card.onlyInCity", StateType.BartiCaravan)
      // «Печь»: с карточки накладной — в Загон, из города (у Щита — после Юноны,
      // у одиночки — после «Соли»).
      case "StoryFurnaceGo" =>
        fromCity(StoryFurnaceQuest.atFurnace(quests), "story.storyFurnace.card.onlyInCity", StateType.StoryFurnace)
      // «Сборщик»: с карточки книги печи — в ночь своего пути, с купчей — на склад.
      case "StoryCollectorGo" =>
        fromCity(StoryCollectorQuest.toFight(quests), "story.storyCollector.card.onlyInCity", StateType.StoryCollector)
      case "StoryWarehouseGo" =>
        fromCity(StoryCollectorQuest.toWarehouse(quests), "story.storyCollector.card.onlyInCity", StateType.StoryCollector)
      // Рубеж II: с карточки купчей — на склад, к Сборщику.
      case "StoryFeedGo" =>
        fromCity(StoryFeedQuest.open(quests), "story.storyCollector.card.onlyInCity", StateType.StoryFeed)
      case _                => Go.Stale
    }
  }

  /** Завести сцену задания: её `scene_data` — до входа в неё. */
  private def scene(data: Json): Prep = (user, _) => heroDao.writeSceneData(user.userId, data)

  /** Глава ещё не взята — взять её здесь: «Новое задание» говорят перед сценой. */
  private def takeChapter(chapter: NpcQuest, quests: NpcQuests): Prep = (user, renderer) =>
    ZIO.when(!quests.isTaken(chapter))(
      ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS)).flatMap { now =>
        val taken = quests.take(chapter, now)
        NpcQuestLog.save(heroDao, user.userId, taken) *>
          renderer.show(user, Screen(StoryLine.takenLine(content, chapter, taken), Nil))
      }).unit

  /** Вскрыть письмо Марисы: текст Кельвина и карта с обратной стороны. */
  private def openLetter(user: User, hero: Hero, renderer: Renderer): Task[Unit] =
    for {
      given <- MarisaQuest.revealMap(heroDao, inventoryRepo, itemRepo, user.userId, hero)
      _     <- renderer.show(user, Screen(content.text("marisa.letterText"), Nil))
      _     <- ZIO.when(given)(renderer.show(user, Screen(
                 content.format("marisa.questItemAdded", "item" -> QuestItemKind.KelvinMap.displayName), Nil)))
    } yield ()
}

object QuestCard {

  /** Что сделать перед сценой: `Renderer` — куда говорить. */
  type Prep = (User, Renderer) => Task[Unit]

  val NoPrep: Prep = (_, _) => ZIO.unit

  /** Что будет, если нажать кнопку. */
  sealed trait Go
  object Go {
    /** Шаг уже не тот: кнопка устарела. */
    case object Stale extends Go
    /** Отсюда нельзя — сказать `text`. `toList` — бот после отказа уходит к
      * списку сумки, а не на карточку: так у карт и купонов. */
    final case class Refused(text: String, toList: Boolean) extends Go
    /** Сначала спросить: ответы — кнопки этого экрана ([[answers]]). */
    final case class Ask(screen: Screen) extends Go
    /** Сделать прямо здесь, без сцены; `run` говорит, что вышло. */
    final case class Here(run: Prep) extends Go
    /** В сцену задания `scene`; `prep` — что сделать перед ней. */
    final case class Enter(scene: StateType, prep: Prep) extends Go
  }

  /** Все кнопки — как их зовёт бот. */
  val Ids: List[String] = List(
    "OpenLetter", "UseKelvinMap", "MapWithMarisa", "MapAlone", "UseMurlocMap", "RaceChange", "NameChange",
    "StrayGo", "BarrelTrail", "StoryCacheGo", "JorgenLairGo", "FaelinRootsGo", "PlukhHuntGo",
    "StoryPortGo", "StoryArchiveGo", "StoryGraveyardGo", "StoryBinderyGo", "StoryMeetGo", "StorySojkaGo",
    "StoryBridgeGo", "BartiHouseGo", "BartiCaravanGo", "VillageGo", "VillageReturn",
    "StoryDenGo", "StoryCellarGo", "StoryBartiGo", "BartiShopGo", "StorySquareGo", "StoryClearingGo",
    "BartiSaltAsk", "BartiSaltGo", "StoryFurnaceGo", "StoryCollectorGo", "StoryWarehouseGo", "StoryFeedGo")

  /** Ответы на вопрос кнопки `id`: карта Кельвина спрашивает, брать ли Марису. */
  def answers(id: String): Set[String] =
    if (id == "UseKelvinMap") Set("MapWithMarisa", "MapAlone") else Set.empty

  /** Кнопка `act` — та, что стоит на карточке `item` сейчас, или ответ на её вопрос. */
  def offers(content: SceneContent, item: Item, quests: NpcQuests, act: String): Boolean =
    choice(content, item, quests).exists(c => c.id == act || answers(c.id).contains(act))

  /** Где герой на самом деле: «Персонаж» и его экраны открыты оттуда, что
    * помнит `return_state`, — прочие сцены и есть место. */
  def place(state: StateType, returnState: Option[StateType]): Option[StateType] =
    if (StateType.characterMenu.contains(state)) returnState else Some(state)

  /** Кнопка задания на карточке сюжетного предмета `item` при журнале `quests`;
    * у предметов без задания и не на своём шаге — нет. */
  def choice(content: SceneContent, item: Item, quests: NpcQuests): Option[Choice] = {
    def positive(id: String, key: String) = content.choice(id, key).copy(color = ChoiceColor.Positive, row = Some(0))
    def plain(id: String, key: String)    = content.choice(id, key).copy(row = Some(0))
    item.questItem match {
      case Some(QuestItemKind.MarisaLetter) =>
        Some(positive("OpenLetter", if (quests.of(NpcQuest.Marisa).bonus) "marisa.readLetterLabel" else "marisa.openLetterLabel"))
      case Some(QuestItemKind.KelvinMap)        => Some(positive("UseKelvinMap", "marisa.mapUseLabel"))
      case Some(QuestItemKind.MurlocVillageMap) => Some(positive("UseMurlocMap", "murlocVillage.map.useLabel"))
      // Купон на смену расы: «Сменить расу» — квест начнётся сразу.
      case Some(QuestItemKind.RaceCoupon) => Some(plain("RaceChange", "raceQuest.useLabel"))
      case Some(QuestItemKind.NameCoupon) => Some(plain("NameChange", "nameChange.useLabel"))
      // Сытый бродячий ларец: «Идти за ларцом» — он поведёт к хозяину.
      case Some(QuestItemKind.StrayCasket) if StrayCasketQuest.leads(quests) => Some(plain("StrayGo", "strayCasket.goLabel"))
      // Улика из бочки, место известно: «Идти по следу».
      case Some(QuestItemKind.BarrelClue) if BarrelThievesQuest.onTrail(quests) =>
        Some(plain("BarrelTrail", "barrelThieves.trailLabel"))
      // Чёрный плащ, Сорока назван: «Искать тайник Сороки».
      case Some(QuestItemKind.StoryCloak) if StoryMarkQuest.toCache(quests) =>
        Some(plain("StoryCacheGo", "story.storyMark.card.searchLabel"))
      // Список отряда Йоргена: «Идти к логову» огненных.
      case Some(QuestItemKind.JorgenRoster) if JorgenQuest.toLair(quests) => Some(plain("JorgenLairGo", "jorgen.card.goLabel"))
      // Срубленный лук Фаэлина: «Вернуть лук изгороди».
      case Some(QuestItemKind.CutBow) if FaelinQuest.toRoots(quests) => Some(plain("FaelinRootsGo", "faelinBow.card.goLabel"))
      // Печать Сноходцев, зов перевёрнут: «Позвать ловчих».
      case Some(QuestItemKind.SnowwalkerSeal) if PlukhQuest.toHunt(quests) =>
        Some(plain("PlukhHuntGo", "plukhGaff.card.huntLabel"))
      // Шифровка, пора в порт: «Идти в порт».
      case Some(QuestItemKind.StoryCipher) if StoryPowderQuest.portOpen(quests) =>
        Some(plain("StoryPortGo", "story.storyPowder.card.goLabel"))
      // Набросок узора, Юнона назвала Сойку: «Идти на встречу».
      case Some(QuestItemKind.StorySketch) if StoryBrandQuest.toMeeting(quests) =>
        Some(plain("StoryMeetGo", "story.storyBrand.card.goLabel"))
      // Страница списка: рассказать Сойке о канцелярии, потом — к мосту, потом — снова к ней.
      case Some(QuestItemKind.StoryPage) if StoryListQuest.toSojka(quests) =>
        Some(plain("StorySojkaGo", "story.storyList.card.tellLabel"))
      case Some(QuestItemKind.StoryPage) if StoryListQuest.toBridge(quests) =>
        Some(plain("StoryBridgeGo", "story.storyList.card.bridgeLabel"))
      case Some(QuestItemKind.StoryPage) if StoryListQuest.toConfront(quests) =>
        Some(plain("StorySojkaGo", "story.storyList.card.confrontLabel"))
      // Ключ от архива: открыть архив (глава берётся здесь), потом — в переплётную днём и ночью.
      case Some(QuestItemKind.StoryArchiveKey) if StoryFaceQuest.archiveOpen(quests) =>
        Some(plain("StoryArchiveGo", "story.storyFace.card.archiveLabel"))
      case Some(QuestItemKind.StoryArchiveKey) if StoryFaceQuest.toFace(quests) =>
        Some(plain("StoryBinderyGo", "story.storyFace.card.faceLabel"))
      case Some(QuestItemKind.StoryArchiveKey) if StoryFaceQuest.toRaid(quests) =>
        Some(plain("StoryBinderyGo", "story.storyFace.card.raidLabel"))
      // Записка «Э.»: на кладбище — Рубеж I берётся здесь.
      case Some(QuestItemKind.StoryNote) if StoryToolQuest.open(quests) =>
        Some(plain("StoryGraveyardGo", "story.storyTool.card.goLabel"))
      // Акт II, «Наутро»: поручение Э. — в «Три кости»; грамота совета — в
      // подвал совета; книга ячейки — к Барти.
      case Some(QuestItemKind.StoryShadowOrder) if StoryMorningQuest.toDen(quests) =>
        Some(plain("StoryDenGo", "story.storyMorning.card.denLabel"))
      case Some(QuestItemKind.StoryCouncilWrit) if StoryMorningQuest.toCellar(quests) =>
        Some(plain("StoryCellarGo", "story.storyMorning.card.cellarLabel"))
      case Some(QuestItemKind.StoryCellBook) if StoryMorningQuest.atBarti(quests) =>
        Some(plain("StoryBartiGo", "story.storyMorning.card.bartiLabel"))
      // Опись урожая: на главную площадь — к новенькому.
      case Some(QuestItemKind.StoryHarvest) if StoryNewcomerQuest.atSquare(quests) =>
        Some(plain("StorySquareGo", "story.storyNewcomer.card.squareLabel"))
      // Пропуск на просеку: к бригаде Чернолома, в изгородь (у Щита — после Юноны).
      case Some(QuestItemKind.StoryPass) if StoryClearingQuest.atClearing(quests) =>
        Some(plain("StoryClearingGo", "story.storyClearing.card.goLabel"))
      // Накладная на лес, одиночка: где Загон — спросить Барти, потом к каравану.
      case Some(QuestItemKind.StoryWaybill) if BartiSaltQuest.available(quests) =>
        Some(plain("BartiSaltAsk", "barti.salt.card.askLabel"))
      case Some(QuestItemKind.StoryWaybill) if BartiSaltQuest.toCaravan(quests) =>
        Some(plain("BartiSaltGo", "barti.salt.card.goLabel"))
      // Накладная на лес, глава 9: в Загон — у каждого пути своей кнопкой; после
      // боя — к печи. У Тени боя нет: её кнопка — с урожаем.
      case Some(QuestItemKind.StoryWaybill) if StoryFurnaceQuest.atFurnace(quests) =>
        val path = StoryFurnaceQuest.path(quests)
        val key  = if (StoryFurnaceQuest.toFork(quests) && path != StoryToolQuest.Path.Shadow) "goFork" else s"go${path.key.capitalize}"
        Some(plain("StoryFurnaceGo", s"story.storyFurnace.card.$key"))
      // Книга печи, глава 10: в ночь своего пути.
      case Some(QuestItemKind.StoryLedger) if StoryCollectorQuest.toFight(quests) =>
        Some(plain("StoryCollectorGo", s"story.storyCollector.card.go${StoryCollectorQuest.path(quests).key.capitalize}"))
      // Купчая на дегтярный склад: на склад — к тем, кто там ждёт.
      case Some(QuestItemKind.StoryDeed) if StoryCollectorQuest.toWarehouse(quests) =>
        Some(plain("StoryWarehouseGo", "story.storyCollector.card.warehouseLabel"))
      // Купчая, Рубеж II: ждать Сборщика на складе, потом — к связанному.
      case Some(QuestItemKind.StoryDeed) if StoryFeedQuest.open(quests) =>
        Some(plain("StoryFeedGo", if (StoryFeedQuest.toFight(quests)) "story.storyFeed.card.goFight" else "story.storyFeed.card.goChoice"))
      // Квитанция ночной стражи: Тени — за ответом Братству, ночью — встречать стражу.
      case Some(QuestItemKind.BartiReceipt) if BartiRoofQuest.toShop(quests) =>
        Some(plain("BartiShopGo", if (BartiRoofQuest.toNight(quests)) "barti.roof.card.nightLabel" else "barti.roof.card.shopLabel"))
      // Адрес Барти: к нему домой, на Торговую площадь, — «Большое дело».
      case Some(QuestItemKind.BartiAddress) if BigDealQuest.houseOpen(quests) =>
        Some(plain("BartiHouseGo", "barti.card.houseLabel"))
      // Караван пришёл: ночью к нему, к Сухому колодцу.
      case Some(QuestItemKind.BartiAddress) if BigDealQuest.toCaravan(quests) =>
        Some(plain("BartiCaravanGo", "barti.card.caravanLabel"))
      // Контракт деревни: к логову беды — пока оно стоит; сделано — назад за платой.
      case Some(QuestItemKind.VillageContract) if Villages.goTo(quests).isDefined =>
        Villages.goTo(quests).map { case (_, texts) => plain("VillageGo", s"village.card.${texts}Label") }
      case Some(QuestItemKind.VillageContract) if Villages.toPay(quests) =>
        Some(positive("VillageReturn", "village.card.returnLabel"))
      // Голова главаря: логово разбито — с ней тоже назад к старосте.
      case Some(QuestItemKind.BanditHead) if Villages.toPay(quests) =>
        Some(positive("VillageReturn", "village.card.returnLabel"))
      case _ => None
    }
  }
}
