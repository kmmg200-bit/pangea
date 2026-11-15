package pangea.service.state.states.guild

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Hero, MasterHornBoosts}
import pangea.model.item.{QuestItemKind, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.bank.BankRepository
import pangea.service.purse.Purse
import pangea.model.quest.{DailyKind, DailyNpc, DailyRates}
import pangea.model.squad.{AllyKind, AllyRates}
import pangea.service.state.{CityExit, DailyDialog, DailyQuestLog, JorgenQuest, MercQuests, NpcQuestDialog, NpcQuestLog, QuestSupport,
  State, StoryFaceQuest, StoryMarkQuest, StoryMorningQuest, UserAction}
import zio.{Task, ZIO}

/**
 * Мастер Горн прокачивает характеристики (Броня, Уклонение, Атака, Защита,
 * Точность, Энергия, Вместимость инвентаря) за репутацию охотников И серебро
 * одновременно — обе валюты списываются в одинаковом числе. На каждой клавише —
 * confirm-экран с ценой; формула цены — `a(n) = 35·1.2^(n−1) − 30`, округление
 * вверх, где `n` — порядковый номер следующего улучшения этой характеристики
 * для героя (см. `hero.masterHornBoosts`).
 *
 * Меню — по две кнопки в ряд: характеристики, за ними задания и наказ;
 * «Назад» и «В город» — нижним рядом (см. `menuRows`).
 */
case class MasterHornState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  bank:          Option[BankRepository] = None
) extends State {
  import MasterHornState._

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  /** «Ржавая вилка»: набрать сто репутации трофеями — и одно улучшение даром.
    * Завязка смотрит на оружие героя. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.Horn, "Horn")

  /** Наказ Горна: на каждый день своя проверка стали. */
  private val daily = DailyDialog(heroDao, content, DailyNpc.Horn, "Horn")

  /** «Жетоны из пепла» — долг Йоргена: жетоны павших сдают Горну. */
  private val jorgen = NpcQuestDialog(heroDao, content, NpcQuest.JorgenDebt, "Jorgen")

  private val branch = new Branch(
    routes = Map(
      quest.questAction   -> Target.Run { (u, _, r) => questTalk(u, r) },
      quest.acceptAction  -> Target.Run { (u, _, r) => quest.accept(u, r) *> enter(u, r).as(StateType.MasterHorn) },
      quest.declineAction -> Target.Run { (u, _, r) => enter(u, r).as(StateType.MasterHorn) },
      "ImproveArmor"         -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Armor) },
      "ImproveEvasion"       -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Evasion) },
      "ImproveAttack"        -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Attack) },
      "ImproveDefence"       -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Defence) },
      "ImproveAccuracy"      -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Accuracy) },
      "ImproveEnergy"        -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Energy) },
      "ImproveInventory"     -> Target.Run { (u, _, r) => askImprove(u, r, Stat.Inventory) },
      "ConfirmImprove"       -> Target.Run { (u, _, r) => confirmImprove(u, r) },
      "CancelImprove"        -> Target.Run { (u, _, r) => enter(u, r).as(StateType.MasterHorn) },
      "LeaveMasterHorn"      -> Target.Goto(StateType.TrainingHall),
      daily.openAction       -> Target.Run { (u, _, r) => showDaily(u, r) },
      daily.takeAction       -> Target.Run { (u, _, r) => takeDaily(u, r) },
      daily.handAction       -> Target.Run { (u, _, r) => handDaily(u, r) },
      "HornDailyBack"        -> Target.Run { (u, _, r) => enter(u, r).as(StateType.MasterHorn) },
      "HornStory"            -> Target.Run { (u, _, r) => storyLedger(u, r) },
      "HornFace"             -> Target.Run { (u, _, r) => faceNumbers(u, r) },
      "HornTokens"           -> Target.Run { (u, _, r) => handTokens(u, r) },
      "HornClerk"            -> Target.Run { (u, _, r) => askClerk(u, r) },
      "HornCellar"           -> Target.Run { (u, _, r) => askMen(u, r) },
      CityExit.route
    ),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.MasterHorn) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    // Сбрасываем выбор стата при возврате на главный экран — confirm-сценарий
    // считается завершённым (или прерванным CancelImprove).
    heroDao.writeSceneData(user.userId, Json.Null) *>
      quest.load(user).flatMap { quests =>
        val base = content.screen("guild.masterHorn.menu")
        val (front, back) = base.choices.partition(_.id != "LeaveMasterHorn")
        for {
          now  <- nowMs
          hero <- getHero(user)
          work <- daily.today(hero, now)
          // Основная линия: споротая метка с плаща при герое — Горн сверит номер с учётом;
          // копии списка отмеченных — Горн узнает в них свои номера.
          cloak  <- if (StoryMarkQuest.toHorn(quests)) hasStory(hero, QuestItemKind.StoryCloak) else ZIO.succeed(false)
          copies <- if (StoryFaceQuest.toHorn(quests)) hasStory(hero, QuestItemKind.StoryCopies) else ZIO.succeed(false)
          // Долг Йоргена: жетоны павших при герое — Горн впишет их в учёт.
          tokens <- if (JorgenQuest.toHorn(quests)) hasStory(hero, QuestItemKind.JorgenTokens) else ZIO.succeed(false)
          // Акт II, «Наутро»: Тень с поручением Э. спрашивает про писаря, Щит с
          // грамотой совета — про людей для подвала.
          order  <- if (StoryMorningQuest.toHorn(quests)) hasStory(hero, QuestItemKind.StoryShadowOrder) else ZIO.succeed(false)
          writ   <- if (StoryMorningQuest.toHornMen(quests)) hasStory(hero, QuestItemKind.StoryCouncilWrit) else ZIO.succeed(false)
          story   = Option.when(cloak)(content.choice("HornStory", "story.storyMark.horn.label"))
          face    = Option.when(copies)(content.choice("HornFace", "story.storyFace.horn.label"))
          ashes   = Option.when(tokens)(content.choice("HornTokens", "jorgen.horn.label"))
          clerk   = Option.when(order)(content.choice("HornClerk", "story.storyMorning.horn.label"))
          men     = Option.when(writ)(content.choice("HornCellar", "story.storyMorning.hornCellar.label"))
          free    = isFree(quests)
          stats   = front.map(c => statOf(c.id).fold(c)(stat => c.copy(label = priceLabel(c.label, hero, stat, free))))
          deeds   = stats ++ quest.button(quests).toList ++ daily.button(work).toList ++ story.toList ++
                      face.toList ++ clerk.toList ++ men.toList ++ ashes.toList
          _    <- renderer.show(user, CityExit.on(base.copy(choices = menuRows(deeds, back)), content))
        } yield ()
      }

  /** «Метка под меткой»: Горн сверяет номер со споротой метки с учётом —
    * плащ носил Ульф Сорока, которого нашли на пятом повороте без вещей, а
    * тайник его так и не нашли. Не тот шаг или плаща при герое нет — меню. */
  private def storyLedger(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- quest.load(user)
      hero   <- getHero(user)
      cloak  <- hasStory(hero, QuestItemKind.StoryCloak)
      _      <- if (!StoryMarkQuest.toHorn(quests) || !cloak) enter(user, renderer)
                else NpcQuestLog.save(heroDao, user.userId, StoryMarkQuest.logged(quests)) *>
                  renderer.show(user, Screen(content.text("story.storyMark.horn.text"), Nil)) *> enter(user, renderer)
    } yield StateType.MasterHorn

  /** «Лицо из прошлого»: Горн узнаёт в копиях списка свои номера учёта — тех,
    * кто ложился в лабиринте и вставал, — и даёт слово Гильдии; дальше — к
    * Юноне. Не тот шаг или копий при герое нет — меню. */
  private def faceNumbers(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- quest.load(user)
      hero   <- getHero(user)
      copies <- hasStory(hero, QuestItemKind.StoryCopies)
      _      <- if (!StoryFaceQuest.toHorn(quests) || !copies) enter(user, renderer)
                else NpcQuestLog.save(heroDao, user.userId, StoryFaceQuest.step(quests, 5)) *>
                  renderer.show(user, Screen(content.text("story.storyFace.horn.text"), Nil)) *> enter(user, renderer)
    } yield StateType.MasterHorn

  /** «Жетоны из пепла»: Горн вписывает павших в учёт — семьям их доля, — и
    * задание закрыто: список и жетоны уходят из сумки, опыт — как за задание
    * горожанина, а потолок Йоргена у героя растёт (при герое он — сразу, см.
    * MercQuests). Не тот шаг или жетонов при герое нет — меню. */
  private def handTokens(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- quest.load(user)
      hero   <- getHero(user)
      tokens <- hasStory(hero, QuestItemKind.JorgenTokens)
      _      <- if (!JorgenQuest.toHorn(quests) || !tokens) enter(user, renderer)
                else for {
                  _    <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.JorgenRoster, QuestItemKind.JorgenTokens))
                  done <- jorgen.complete(user, hero, identity)
                  (_, expLine) = done
                  _    <- MercQuests.grow(heroDao, user.userId, hero, AllyKind.Human)
                  grew  = content.format("jorgen.horn.grown", "name" -> AllyKind.Human.name,
                            "lvl" -> (AllyKind.Human.maxLvl + AllyRates.QuestGrowth).toString)
                  _    <- renderer.show(user, Screen(content.text("jorgen.horn.text"), Nil))
                  _    <- renderer.show(user, Screen(List(grew, expLine, content.text("jorgen.questDone")).mkString("\n\n"), Nil))
                  _    <- enter(user, renderer)
                } yield ()
    } yield StateType.MasterHorn

  /** «Наутро», путь Тени: Горн ищет своего писаря — тот носил Братству его
    * книги — и сам говорит, где тот обычно сидит. Герой врёт, что не видел.
    * Глава на втором шаге: в «Три кости». Не тот шаг или поручения при герое
    * нет — меню. */
  private def askClerk(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- quest.load(user)
      hero   <- getHero(user)
      order  <- hasStory(hero, QuestItemKind.StoryShadowOrder)
      _      <- if (!StoryMorningQuest.toHorn(quests) || !order) enter(user, renderer)
                else NpcQuestLog.save(heroDao, user.userId, StoryMorningQuest.step(quests, 2)) *>
                  renderer.show(user, Screen(content.text("story.storyMorning.horn.text"), Nil)) *> enter(user, renderer)
    } yield StateType.MasterHorn

  /** «Наутро», путь Щита: Горн даёт двоих своих и идёт в подвал сам. Глава на
    * третьем шаге: ночь в подвале совета. Не тот шаг или грамоты при герое
    * нет — меню. */
  private def askMen(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- quest.load(user)
      hero   <- getHero(user)
      writ   <- hasStory(hero, QuestItemKind.StoryCouncilWrit)
      _      <- if (!StoryMorningQuest.toHornMen(quests) || !writ) enter(user, renderer)
                else NpcQuestLog.save(heroDao, user.userId, StoryMorningQuest.step(quests, 3)) *>
                  renderer.show(user, Screen(content.text("story.storyMorning.hornCellar.text"), Nil)) *> enter(user, renderer)
    } yield StateType.MasterHorn

  private def hasStory(hero: Hero, kind: QuestItemKind): Task[Boolean] =
    inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      .map(inv => QuestSupport.hasItem(inv.items.data, kind))

  // ── Наказ на день ──────────────────────────────────────────────────────────

  private def showDaily(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      task <- daily.today(hero, now)
      _    <- daily.show(user, task, now, renderer)
    } yield StateType.MasterHorn

  private def takeDaily(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      _    <- daily.take(user, hero, now, renderer)
    } yield StateType.MasterHorn

  /** Кузнец платит тем, что у него есть, — именем в гильдии, и ровно
    * столько же, сколько даёт опыта за тот же наказ. */
  private def handDaily(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      paid <- daily.hand(user, hero, now)
      _    <- ZIO.foreachDiscard(paid) { case (_, exp) =>
                val rep = DailyRates.reputation(hero.lvl)
                heroDao.updateGuildReputation(user.userId, hero.guildReputation + rep) *>
                  renderer.show(user, Screen(content.format("daily.horn.reward",
                    "rep" -> rep.toString, "exp" -> exp.toString), Nil))
              }
      _    <- enter(user, renderer)
    } yield StateType.MasterHorn

  // ── Задание Горна ──────────────────────────────────────────────────────────

  /** Кнопка задания: завязка по оружию героя, пока не взято; счёт репутации на
    * первом шаге; на втором — напоминание, что улучшение даром. */
  private def questTalk(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- quest.load(user)
      p       = quests.of(NpcQuest.Horn)
      _ <- if (p.done) enter(user, renderer)
           else if (!p.taken) quest.offer(user, renderer, quest.format("intro", "verdict" -> verdict(hero)))
           else if (p.step == 1)
             renderer.show(user, Screen(quest.format("step1Fail", "rep" -> p.counter.toString), Nil)) *> enter(user, renderer)
           else renderer.show(user, Screen(quest.text("step2Hint"), Nil)) *> enter(user, renderer)
    } yield StateType.MasterHorn

  /** Что Горн думает об оружии героя: серое и белое — хлам, лучше — «хоть
    * что-то», без оружия — отдельная реплика. */
  private def verdict(hero: Hero): String = {
    val weapon = hero.equipment.mainWeapon
    if (hero.equipment.unarmed) quest.text("verdictBareHands")
    else if (weapon.rarity == Rarity.Gray || weapon.rarity == Rarity.White) quest.format("verdictJunk", "weapon" -> weapon.name)
    else quest.format("verdictDecent", "weapon" -> weapon.name)
  }

  /** Улучшение даром — пока задание на втором шаге. */
  private def freeImprove(user: User): Task[Boolean] =
    quest.load(user).map(isFree)

  /** Подпись кнопки характеристики в меню: сколько репутации стоит её
    * следующее улучшение (серебра уходит столько же), а пока по заданию
    * улучшение даром — так и написано. */
  private def priceLabel(label: String, hero: Hero, stat: Stat, free: Boolean): String =
    if (free) content.format("guild.masterHorn.freeLabel", "stat" -> label)
    else content.format("guild.masterHorn.costLabel", "stat" -> label, "cost" -> cost(hero, stat).toString)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Confirm-экран: запоминаем выбранный стат в scene_data и показываем «Вы
  // можете улучшить X на +1 за N очков» с двумя кнопками.
  private def askImprove(user: User, renderer: Renderer, stat: Stat): Task[StateType] =
    for {
      hero <- getHero(user)
      free <- freeImprove(user)
      _    <- heroDao.writeSceneData(user.userId, Json.obj(StatKey -> stat.entryName.asJson))
      text  = if (free) quest.format("confirmFree", "stat" -> stat.label, "step" -> stat.step.toString)
              else content.format("guild.masterHorn.confirm.text",
                     "stat" -> stat.label,
                     "step" -> stat.step.toString,
                     "cost" -> cost(hero, stat).toString)
      _    <- renderer.show(user, Screen(text, content.screen("guild.masterHorn.confirm").choices))
    } yield StateType.MasterHorn

  // Применяет прокачку: проверяет репутацию, списывает её, повышает
  // характеристику на +1 и инкрементирует счётчик прокачек этого стата.
  // После прокачки/неудачи остаёмся на confirm-экране того же стата — игрок может
  // продолжить вкачивать одну и ту же характеристику без возврата в меню.
  private def confirmImprove(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero      <- getHero(user)
      sceneData <- heroDao.readSceneData(user.userId)
      statOpt    = sceneData
                     .flatMap(_.hcursor.get[String](StatKey).toOption)
                     .flatMap(Stat.withNameOption)
      next <- statOpt match {
        case None =>
          // Sentinel: scene_data пустой — confirm-сценарий не начат. Возвращаемся в меню.
          renderer.show(user, Screen(content.text("guild.masterHorn.notReady"), Nil)) *>
            enter(user, renderer).as(StateType.MasterHorn)
        case Some(stat) =>
          val price = cost(hero, stat)
          freeImprove(user).zip(purse.wallet(hero)).flatMap { case (free, wallet) =>
            if (free)
              // Задание Горна: этот раз — даром, и это его развязка.
              writeBoost(user, hero, stat, 0L) *> questFinish(user, hero, stat, renderer)
            else if (hero.guildReputation < price)
              renderer.show(user, Screen(
                content.format("guild.masterHorn.notEnough", "cost" -> price.toString), Nil)) *>
                askImprove(user, renderer, stat)
            else if (!wallet.canAfford(price))
              renderer.show(user, Screen(
                content.format("guild.masterHorn.notEnoughSilver", "cost" -> price.toString), Nil)) *>
                askImprove(user, renderer, stat)
            else
              applyBoost(user, hero, stat, price, renderer) *>
                // «Молот не должен стынуть» — заказ идёт в наказ на день.
                DailyQuestLog.add(heroDao, user.userId, DailyNpc.Horn, DailyKind.HornUpgrade, 1L) *>
                askImprove(user, renderer, stat)
          }
      }
    } yield next

  /** Развязка задания Горна после бесплатного улучшения: рассказ и опыт. */
  private def questFinish(user: User, hero: Hero, stat: Stat, renderer: Renderer): Task[StateType] =
    for {
      done <- quest.complete(user, hero, identity)
      (_, expLine) = done
      weapon = hero.equipment.mainWeapon
      closing = if (hero.equipment.unarmed) quest.text("closingBareHands")
                else quest.format("closingWeapon", "weapon" -> weapon.name)
      outro   = quest.format("outro", "closing" -> closing)
      _    <- renderer.show(user, Screen(outro, Nil))
      _    <- renderer.show(user, Screen(quest.format("reward", "stat" -> stat.label, "step" -> stat.step.toString, "exp" -> expLine), Nil))
      _    <- enter(user, renderer)
    } yield StateType.MasterHorn

  // Бусты Горна не пишутся в `fightStats` напрямую — они хранятся в
  // `masterHornBoosts` и прибавляются на чтение в `Hero.fightStatsWith` /
  // `maxArmor`. Единственное исключение — Inventory: вместимость живёт в
  // отдельном репозитории. Значение буста — накопленный прирост стата
  // (`stat.step` за прокачку), поэтому его можно прибавлять к статам напрямую.
  //
  // Цена списывается ОБЕИМИ валютами — репутацией и серебром — в одинаковом
  // числе (см. `cost`): прокачка стоит `price` очков репутации И `price` серебра.
  private def applyBoost(user: User, hero: Hero, stat: Stat, price: Long, renderer: Renderer): Task[Unit] = {
    val remainingRep = hero.guildReputation - price
    for {
      // Серебро — вместе с ячейкой: нехватку на руках кошель добрал из неё.
      wallet <- purse.wallet(hero)
      remainingSilver = (wallet.total - price).max(0L)
      _ <- writeBoost(user, hero, stat, price)
      _ <- renderer.show(user, Screen(
        content.format("guild.masterHorn.applied",
          "stat" -> stat.label, "step" -> stat.step.toString,
          "cost" -> price.toString, "remaining" -> remainingRep.toString,
          "remainingSilver" -> remainingSilver.toString), Nil))
    } yield ()
  }

  /** Записи прокачки без сообщения: списание обеих валют, вместимость, буст. */
  private def writeBoost(user: User, hero: Hero, stat: Stat, price: Long): Task[Unit] =
    for {
      _ <- heroDao.updateGuildReputation(user.userId, hero.guildReputation - price)
      _ <- DailyQuestLog.reputationSpent(heroDao, user.userId, price)
      _ <- purse.charge(user.userId, hero, price)
      _ <- ZIO.when(stat == Stat.Inventory)(inventoryRepo.increaseCapacity(hero.id, stat.step).orElse(ZIO.unit))
      _ <- heroDao.updateMasterHornBoosts(user.userId, bumped(hero.masterHornBoosts, stat))
    } yield ()

  private def nowMs: Task[Long] =
    ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object MasterHornState {
  import enumeratum._

  // `step` — на сколько единиц растёт характеристика за одну прокачку. Боевые
  // статы качаются по +3, а Энергия и Вместимость инвентаря — по +1.
  sealed abstract class Stat(val label: String, val step: Long) extends EnumEntry
  object Stat extends Enum[Stat] {
    val values = findValues
    case object Armor         extends Stat("Броня", 3)
    case object Evasion       extends Stat("Уклонение", 3)
    case object Attack        extends Stat("Атака", 3)
    case object Defence       extends Stat("Защита", 3)
    case object Accuracy      extends Stat("Точность", 3)
    case object Energy        extends Stat("Энергия", 1)
    case object Inventory     extends Stat("Вместимость инвентаря", 1)
  }

  /** Характеристика под кнопкой меню: «ImproveArmor» — Броня. Кнопки меню
    * названы по характеристикам, поэтому ищем по имени. */
  private def statOf(action: String): Option[Stat] =
    Stat.values.find(stat => s"Improve${stat.entryName}" == action)

  /** Кнопок в ряду меню — по две (решил пользователь): подпись с ценой длинная.
    * Рядов тогда с «Назад» — до семи, в предел ВК (десять, см.
    * `VkRenderer.MaxRows`) это влезает. */
  val ButtonsPerRow: Int = 2

  /** Раскладка меню: характеристики, а за ними задания и наказ — по
    * [[ButtonsPerRow]] в ряд (сейчас их от семи до двенадцати — четыре-шесть
    * рядов); «Назад» — нижним рядом, «В город» встанет к нему (CityExit.on). */
  private[guild] def menuRows(deeds: List[Choice], back: List[Choice]): List[Choice] = {
    val grid = deeds.zipWithIndex.map { case (c, i) => c.copy(row = Some(i / ButtonsPerRow)) }
    val last = (deeds.size + ButtonsPerRow - 1) / ButtonsPerRow
    grid ++ back.map(_.copy(row = Some(last)))
  }

  /** Улучшение даром — пока задание Горна на втором шаге. */
  private def isFree(quests: NpcQuests): Boolean = quests.onStep(NpcQuest.Horn, 2)

  /** Накопленный прирост стата от Мастера Горна (кратен `stat.step`). */
  def boostsFor(hero: Hero, stat: Stat): Long = boostsFor(hero.masterHornBoosts, stat)

  private def boostsFor(b: MasterHornBoosts, stat: Stat): Long = stat match {
    case Stat.Armor         => b.armor
    case Stat.Evasion       => b.evasion
    case Stat.Attack        => b.attack
    case Stat.Defence       => b.defence
    case Stat.Accuracy      => b.accuracy
    case Stat.Energy        => b.energy
    case Stat.Inventory     => b.inventory
  }

  private def bumped(b: MasterHornBoosts, stat: Stat): MasterHornBoosts = stat match {
    case Stat.Armor         => b.copy(armor         = b.armor     + stat.step)
    case Stat.Evasion       => b.copy(evasion       = b.evasion   + stat.step)
    case Stat.Attack        => b.copy(attack        = b.attack    + stat.step)
    case Stat.Defence       => b.copy(defence       = b.defence   + stat.step)
    case Stat.Accuracy      => b.copy(accuracy      = b.accuracy  + stat.step)
    case Stat.Energy        => b.copy(energy        = b.energy    + stat.step)
    case Stat.Inventory     => b.copy(inventory     = b.inventory + stat.step)
  }

  /**
   * Цена следующей прокачки в очках репутации:
   *   `a(n) = ceil(35 × 1.2^(n−1) − 30)`,
   * где `n` — порядковый номер следующей прокачки этого стата
   * (`boostsFor(hero, stat) / stat.step + 1`, так как буст хранит накопленный
   * прирост, а не число прокачек).
   * Первая прокачка стоит 5, далее каждая дороже. Минимум — 1.
   */
  def cost(hero: Hero, stat: Stat): Long = {
    val n     = boostsFor(hero, stat) / stat.step + 1
    val raw   = 35.0 * math.pow(1.2, (n - 1).toDouble) - 30.0
    math.ceil(raw).toLong.max(1L)
  }

  private val StatKey = "masterHornStat"
}
