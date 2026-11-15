package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.BattleAlly
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests, QuestOutcome}
import pangea.model.squad.AllyKind
import pangea.service.state.StoryListQuest.SojkaFate
import pangea.service.state.StoryToolQuest.Path

/** Глава 10 основной линии, «Сборщик» ([[NpcQuest.StoryCollector]], см.
  * [[StoryLine]]). Ведёт её книга печи из главы 9: «Сборщику лично —
  * отмеченный из Кинэта». Сборщик сам приходит в Кинэт за героем, и у каждого
  * пути — своя цена:
  *  - Тень: приказ сдать героя подписал сам Сборщик, а Э. приходит ночью и
  *    говорит, что второй раз его не продаст (так решил пользователь), —
  *    люди Сборщика, бой, Э. в строю; потом Э. советует купить склад;
  *  - Щит: советник Рубус — тот, у кого второй ключ от печати Юноны, —
  *    подписал выдачу героя и Сойки Братству за тишину в городе; Юнона
  *    выводит через архив с пропуском, Горн держит гильдейский зал — бой со
  *    стражей совета, Горн и двое искателей в строю;
  *  - Сам по себе: Сборщик покупает Барти, Барти приносит задаток — и
  *    Сборщик присылает людей проверить: бой у синей двери, Барти в строю.
  *
  * Дальше у всех — дегтярный склад: герой покупает его у Торгового дома за
  * [[WarehousePrice]] под ловушку на Сборщика. На складе — Сойка (по главе 4)
  * и глава Братства, вежливый старик с больными ногами: место в Братстве
  * (и убрать того, кто героя продавал) или покой — либо ни то, ни другое
  * ([[Deal]], выбор помним и считаем в «как и вы»). В сумке остаётся купчая —
  * зацепка Рубежа II. Враги, сложность и награда — по уровню задания. */
object StoryCollectorQuest {

  /** Уровень задания: по нему сложность, враги и награда. */
  val Level: Int = 46

  /** Сюжетные бои главы — по пути: без «Сбежать», награда как за обычный бой. */
  val NightStory: String = "storyCollectorNight"
  val HallStory: String  = "storyCollectorHall"
  val CheckStory: String = "storyCollectorCheck"
  val FightStories: Set[String] = Set(NightStory, HallStory, CheckStory)

  /** Враги: именной старший и четверо — люди уровня задания. */
  val FoeRace: Race         = Race.Human
  val LeaderRarity: Rarity  = Rarity.Mythical
  val HenchRarity: Rarity   = Rarity.Rare
  val Henchmen: Int         = 4

  /** Э. в строю у Тени — по силе легендарный. */
  val AgentRarity: Rarity = Rarity.Legendary

  /** Дегтярный склад — столько просит Торговый дом (так решил пользователь). */
  val WarehousePrice: Long = 250000L

  /** Серебро за главу — за уровень задания, как в прежних главах. */
  val SilverPerLvl: Long = 200L

  def silver: Long = SilverPerLvl * Level

  /** Договор с главой Братства: номер — в `counter` главы. */
  sealed abstract class Deal(val n: Long, val key: String, val outcome: QuestOutcome)

  object Deal {
    case object Place  extends Deal(1L, "place", QuestOutcome.CollectorPlace)
    case object Peace  extends Deal(2L, "peace", QuestOutcome.CollectorPeace)
    case object Refuse extends Deal(3L, "refuse", QuestOutcome.CollectorRefuse)

    val all: List[Deal] = List(Place, Peace, Refuse)
  }

  /** Путь Рубежа I — по нему ветка; не записан — как у одиночки. */
  def path(q: NpcQuests): Path = StoryToolQuest.choice(q).getOrElse(Path.Alone)

  private def onStep(q: NpcQuests, n: Int): Boolean = q.onStep(NpcQuest.StoryCollector, n)

  /** У Щита глава начинается у Юноны — на шаг дольше. */
  private def shift(q: NpcQuests): Int = if (path(q) == Path.Shield) 1 else 0

  /** Щит, шаг 1: Юнона в архиве — что решил совет. */
  def toJunona(q: NpcQuests): Boolean = path(q) == Path.Shield && onStep(q, 1)

  /** Бой: у Тени — ночью с Э., у Щита — в гильдейском зале, у одиночки — у Барти. */
  def toFight(q: NpcQuests): Boolean = onStep(q, 1 + shift(q))

  /** Купить дегтярный склад у Торгового дома. */
  def toBuy(q: NpcQuests): Boolean = onStep(q, buyStep(q))

  /** На складе: Сойка и глава Братства. */
  def toWarehouse(q: NpcQuests): Boolean = onStep(q, warehouseStep(q))

  def buyStep(q: NpcQuests): Int       = 2 + shift(q)
  def warehouseStep(q: NpcQuests): Int = 3 + shift(q)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryCollector)(_.copy(step = n))

  /** Договор с главой Братства — у закрытой главы. */
  def deal(q: NpcQuests): Option[Deal] =
    Option.when(q.isDone(NpcQuest.StoryCollector))(q.of(NpcQuest.StoryCollector).counter).flatMap(n => Deal.all.find(_.n == n))

  /** Простила ли Сойку глава 4: её слова на складе. Не записано — порвали. */
  def sojkaForgiven(q: NpcQuests): Boolean = StoryListQuest.choice(q).contains(SojkaFate.Forgave)

  /** Ключ боя своего пути. */
  def fightStory(q: NpcQuests): String = path(q) match {
    case Path.Shadow => NightStory
    case Path.Shield => HallStory
    case Path.Alone  => CheckStory
  }

  /** Враги: старший — первым, именной, и четверо. */
  def foes(leader: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, FoeRace, LeaderRarity, Some(leader)) ::
      List.fill(Henchmen)(MonsterGenerator.generateOfRaceAndRarity(Level, FoeRace, HenchRarity))

  /** Кто встаёт в строй гостем: у Тени — Э., у Щита — Горн и двое искателей,
    * у одиночки — Барти со сковородой; все — уровня задания (`lvl`: на Рубеже II
    * те же гости встают его уровнем). */
  def guests(content: SceneContent, q: NpcQuests, lvl: Int = Level): List[BattleAlly] = path(q) match {
    case Path.Shadow =>
      List(StoryMorningQuest.guest(AllyKind.BrotherhoodAgent, content.text("story.storyCollector.agentName"), AgentRarity, lvl))
    case Path.Shield =>
      StoryMorningQuest.guest(AllyKind.GuildMaster, content.text("story.storyMorning.cellar.hornName"),
        StoryMorningQuest.HornRarity, lvl) ::
        content.list("story.storyMorning.cellar.seekers").map(name =>
          StoryMorningQuest.guest(AllyKind.GuildSeeker, name, StoryMorningQuest.SeekerRarity, lvl))
    case Path.Alone => List(StoryMorningQuest.bartiGuest(content, lvl))
  }

  /** Строка карточки книги печи — что делать сейчас, по пути и шагу, пока склад
    * не куплен. Глава 9 не закрыта или 10-я не идёт — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryFurnace) || !q.isTaken(NpcQuest.StoryCollector)) ""
    else content.list(s"story.storyCollector.card.${path(q).key}").lift(q.of(NpcQuest.StoryCollector).step - 1).getOrElse("")

  /** Строка карточки купчей: на склад — или, после главы, Рубеж II. */
  def deedLine(content: SceneContent, q: NpcQuests): String =
    if (toWarehouse(q)) content.text(s"story.storyCollector.deed.${path(q).key}")
    else StoryFeedQuest.cardLine(content, q)
}
