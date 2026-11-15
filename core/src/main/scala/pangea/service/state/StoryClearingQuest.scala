package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.BattleAlly
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.AllyKind
import pangea.service.state.StoryToolQuest.Path

/** Глава 8 основной линии, «Просека» ([[NpcQuest.StoryClearing]], см.
  * [[StoryLine]]). Ведёт её пропуск на просеку из главы 7: лабиринт уводит куда
  * угодно, а Братству нужна дорога в одно место, и гильдия Чернолом рубит её
  * в изгороди прямой полосой. Где топор вязнет, корни рвут порохом — бочки
  * стоят вдоль просеки и рвутся и в бою ([[pangea.model.battle.PowderKeg]]);
  * Тихие поют изгороди закрыться, но пила быстрее песни.
  *
  * Ветки — по пути Рубежа I:
  *  - Тень (1 шаг): Братство задержало бригаде плату, и героя посылают
  *    «объяснить договор»: долг можно заплатить самому — или драться;
  *  - Щит (2 шага): Юнона узнаёт на бирке свою печать — пользуется ею кто-то
  *    выше неё — и пишет встречный приказ; Горн идёт к изгороди сам, а
  *    лесорубы бумаг не читают — бой, Горн в строю;
  *  - Сам по себе (1 шаг): в лагерь пускают с ящиком шнапса Барти; бригада
  *    катит бочку под певцов — бой, Элиар Тихая Ветвь в строю.
  *
  * Бригада — вся, как у старых корней в задании Фаэлина: десять простых,
  * четверо мифических и десятник, именной легендарный. У десятника — накладная
  * на лес в Загон, зацепка главы 9. Враги, сложность и награда — по уровню
  * задания, а не героя. */
object StoryClearingQuest {

  /** Уровень задания: по нему сложность, враги и награда. */
  val Level: Int = 36

  /** Ключ сюжетного боя на просеке: без «Сбежать», награда как за обычный бой. */
  val FightStory: String = "storyClearing"

  /** Сколько бочек с порохом стоит на поле в начале боя. */
  val Kegs: Int = 6

  /** Бригада Чернолома: десять простых, четверо мифических и десятник. */
  val Loggers: Int          = 10
  val LoggerRarity: Rarity  = Rarity.Common
  val Foremen: Int          = 4
  val ForemanRarity: Rarity = Rarity.Mythical
  val LeaderRarity: Rarity  = Rarity.Legendary

  /** Сколько Братство задолжало бригаде — Тень может отдать сам: тысяча за
    * уровень задания, как долг писаря в «Наутре». */
  val WagesPerLvl: Long = 1000L

  def wages: Long = WagesPerLvl * Level

  /** Серебро за главу — за уровень задания, как в прежних главах. */
  val SilverPerLvl: Long = 200L

  def silver: Long = SilverPerLvl * Level

  /** Путь Рубежа I — по нему ветка; не записан — как у одиночки. */
  def path(q: NpcQuests): Path = StoryToolQuest.choice(q).getOrElse(Path.Alone)

  /** Щит, шаг 1: показать бирку Юноне. */
  def toJunona(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryClearing, 1) && path(q) == Path.Shield

  /** На просеку: у Щита — после Юноны, у остальных — сразу. */
  def atClearing(q: NpcQuests): Boolean = path(q) match {
    case Path.Shield => q.onStep(NpcQuest.StoryClearing, 2)
    case _           => q.onStep(NpcQuest.StoryClearing, 1)
  }

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryClearing)(_.copy(step = n))

  /** Бригада уровня задания: впереди простые, за ними мифические, последним —
    * десятник, именной. */
  def foes(leader: String): List[Monster] = {
    def goblin(rarity: Rarity, name: Option[String] = None): Monster =
      MonsterGenerator.generateOfRaceAndRarity(Level, Race.Goblin, rarity, name)
    List.fill(Loggers)(goblin(LoggerRarity)) ++ List.fill(Foremen)(goblin(ForemanRarity)) :+
      goblin(LeaderRarity, Some(leader))
  }

  /** Кто встаёт в строй гостем: у Щита — Горн, у одиночки — Элиар; Тень на
    * стороне заказчика, и с ним никого. Оба — уровня задания. */
  def guests(content: SceneContent, q: NpcQuests): List[BattleAlly] = path(q) match {
    case Path.Shield =>
      List(StoryMorningQuest.guest(AllyKind.GuildMaster, content.text("story.storyMorning.cellar.hornName"),
        StoryMorningQuest.HornRarity, Level))
    case Path.Alone  => List(HedgeSongQuest.elfAt(Level))
    case Path.Shadow => Nil
  }

  /** Строка карточки пропуска — что делать сейчас, по пути и шагу. Глава 7 не
    * закрыта или 8-я уже закрыта — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryNewcomer) || !q.isTaken(NpcQuest.StoryClearing)) ""
    else content.list(s"story.storyClearing.card.${path(q).key}").lift(q.of(NpcQuest.StoryClearing).step - 1).getOrElse("")
}
