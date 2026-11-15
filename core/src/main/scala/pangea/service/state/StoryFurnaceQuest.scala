package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.BattleAlly
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests, QuestOutcome}
import pangea.model.squad.AllyKind
import pangea.service.state.StoryNewcomerQuest.Fate
import pangea.service.state.StoryPowderQuest.WardenFate
import pangea.service.state.StoryToolQuest.Path

/** Глава 9 основной линии, «Печь» ([[NpcQuest.StoryFurnace]], см.
  * [[StoryLine]]). Ведёт её накладная на лес из главы 8: в конце просеки —
  * Загон, старая солеварня, о Кинэте там не слышали. Печь топят живым деревом
  * из изгороди — оно горит неделями, — и в ней с живого снимают Клеймо и
  * пережигают в другого: у одного узор глубже, от другого остаётся пустой
  * человек. Мастер печи жалуется на сырые дрова и сроки.
  *
  * Ветки — по пути Рубежа I:
  *  - Тень (1 шаг): Э. посылает героя курьером — отвезти по просеке «урожай».
  *    Легенда держится (так решил пользователь): боя нет, сразу развилка;
  *  - Щит (4 шага): Юнона находит Загон в бумагах совета — склад соли, сданный
  *    за монету с её печатью и чужой подписью; штурм с Горном и асессором
  *    Штилем (бой, оба в строю); развилка; Юнона — что нашлось;
  *  - Сам по себе (2 шага, сперва «Соль» у Барти): с соляным караваном до
  *    Загона, ночью через крышу — ночная стража (бой, Ра'Тхар и Ма'Кесс в
  *    строю); развилка.
  *
  * Развилка у печи — у всех ([[Fork]]): разбить или оставить; исход — в
  * подсчёт «как и вы». Помнит глава и прошлые выборы: отданный Братству или
  * совету новенький из главы 7 сидит здесь в клети, выведенного через изгородь
  * — нет; отпущенный в главе 2 Смотритель служит при печи приказчиком. В книге
  * печи вместо имён номера, а одна запись крупнее других: «Сборщику лично —
  * отмеченный из Кинэта», зацепка главы 10. Враги, сложность и награда — по
  * уровню задания, а не героя. */
object StoryFurnaceQuest {

  /** Уровень задания: по нему сложность, враги и награда. */
  val Level: Int = 41

  /** Сюжетные бои: штурм Загона (Щит) и ночная стража (одиночка). Без
    * «Сбежать», награда как за обычный бой. */
  val RaidStory: String  = "storyFurnaceRaid"
  val NightStory: String = "storyFurnaceNight"
  val FightStories: Set[String] = Set(RaidStory, NightStory)

  /** Охрана Загона: именной старший и четверо — люди уровня задания. */
  val GuardRace: Race       = Race.Human
  val LeaderRarity: Rarity  = Rarity.Mythical
  val GuardRarity: Rarity   = Rarity.Rare
  val Guards: Int           = 4

  /** Асессор совета в строю у Щита — по силе редкий человек. */
  val EnvoyRarity: Rarity = Rarity.Rare

  /** Серебро за главу — за уровень задания, как в прежних главах. */
  val SilverPerLvl: Long = 200L

  def silver: Long = SilverPerLvl * Level

  /** Что стало с печью: номер — в `counter` главы. */
  sealed abstract class Fork(val n: Long, val key: String, val outcome: QuestOutcome)

  object Fork {
    case object Broken extends Fork(1L, "broken", QuestOutcome.FurnaceBroken)
    case object Kept   extends Fork(2L, "kept", QuestOutcome.FurnaceKept)

    val all: List[Fork] = List(Broken, Kept)
  }

  /** Путь Рубежа I — по нему ветка; не записан — как у одиночки. */
  def path(q: NpcQuests): Path = StoryToolQuest.choice(q).getOrElse(Path.Alone)

  private def onStep(q: NpcQuests, n: Int): Boolean = q.onStep(NpcQuest.StoryFurnace, n)

  /** Одиночка едет в Загон с соляным караваном: без «Соли» дороги туда нет. */
  def caravanReady(q: NpcQuests): Boolean = q.isDone(NpcQuest.BartiSalt)

  /** Щит, шаг 1: Юнона ищет Загон в бумагах совета. */
  def toJunona(q: NpcQuests): Boolean = path(q) == Path.Shield && onStep(q, 1)

  /** Щит, шаг 4: рассказать Юноне, что нашлось в Загоне. */
  def toTell(q: NpcQuests): Boolean = path(q) == Path.Shield && onStep(q, 4)

  /** Бой у Загона: у Щита — штурм, у одиночки — ночь через крышу. */
  def toFight(q: NpcQuests): Boolean = path(q) match {
    case Path.Shield => onStep(q, 2)
    case Path.Alone  => onStep(q, 1) && caravanReady(q)
    case Path.Shadow => false
  }

  /** Развилка у печи: у Тени — сразу по приезде. */
  def toFork(q: NpcQuests): Boolean = path(q) match {
    case Path.Shadow => onStep(q, 1)
    case Path.Shield => onStep(q, 3)
    case Path.Alone  => onStep(q, 2)
  }

  /** С карточки накладной — в Загон: к бою или к печи. */
  def atFurnace(q: NpcQuests): Boolean = toFight(q) || toFork(q)

  /** Шаг развилки у своего пути — после боя. */
  def forkStep(q: NpcQuests): Int = path(q) match {
    case Path.Shield => 3
    case Path.Alone  => 2
    case Path.Shadow => 1
  }

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryFurnace)(_.copy(step = n))

  /** Что стало с печью — записано у главы (и у Щита до разговора с Юноной). */
  def fork(q: NpcQuests): Option[Fork] = Fork.all.find(_.n == q.of(NpcQuest.StoryFurnace).counter)

  /** Новенький из главы 7 здесь, в клети: его отдали Братству или совету (а
    * совет перепродал). Выведенного через изгородь здесь нет. */
  def newcomerHere(q: NpcQuests): Boolean =
    StoryNewcomerQuest.fate(q).exists(f => f == Fate.Brotherhood || f == Fate.Council)

  /** Смотритель, отпущенный в главе 2, служит при печи приказчиком. */
  def wardenHere(q: NpcQuests): Boolean = StoryPowderQuest.choice(q).contains(WardenFate.Released)

  /** Охрана Загона: старший — первым, именной, и четверо. */
  def foes(leader: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, LeaderRarity, Some(leader)) ::
      List.fill(Guards)(MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, GuardRarity))

  /** Кто встаёт в строй гостем: у Щита — Горн и асессор Штиль, у одиночки —
    * капитаны каравана; все — уровня задания. */
  def guests(content: SceneContent, q: NpcQuests): List[BattleAlly] = path(q) match {
    case Path.Shield =>
      List(
        StoryMorningQuest.guest(AllyKind.GuildMaster, content.text("story.storyMorning.cellar.hornName"),
          StoryMorningQuest.HornRarity, Level),
        StoryMorningQuest.guest(AllyKind.CouncilEnvoy, content.text("story.storyFurnace.envoyName"), EnvoyRarity, Level))
    case Path.Alone  => content.list("barti.caravan.kinCaptains").map(BigDealQuest.captainAt(_, Level))
    case Path.Shadow => Nil
  }

  /** Строка карточки накладной — что делать сейчас, по пути и шагу. Одиночке
    * без «Соли» — её строка (спросить Барти, ехать к каравану). Глава 8 не
    * закрыта или 9-я не идёт — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryClearing) || !q.isTaken(NpcQuest.StoryFurnace)) ""
    else if (path(q) == Path.Alone && !caravanReady(q)) BartiSaltQuest.cardLine(content, q)
    else content.list(s"story.storyFurnace.card.${path(q).key}").lift(q.of(NpcQuest.StoryFurnace).step - 1).getOrElse("")

  /** Строка карточки книги печи: у Щита — к Юноне, дальше — глава 10. */
  def ledgerLine(content: SceneContent, q: NpcQuests): String =
    if (toTell(q)) content.text("story.storyFurnace.card.tell")
    else StoryCollectorQuest.cardLine(content, q)
}
