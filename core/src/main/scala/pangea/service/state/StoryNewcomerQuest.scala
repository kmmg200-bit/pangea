package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests, QuestOutcome}
import pangea.model.skill.MonsterSkill
import pangea.service.state.StoryToolQuest.Path

/** Глава 7 основной линии, «Новенький» ([[NpcQuest.StoryNewcomer]], см.
  * [[StoryLine]]). Ведёт её опись урожая из главы 6: на главную площадь из
  * портала падает новый отмеченный — Мило, торговец сукном, — и за ним
  * приходят раньше героя. У Тени — стража совета: у сержанта бумага
  * «в канцелярию, на запись», а Э. велел «в ячейку, тихо». У Щита и одиночки —
  * ловцы Братства с настоящими сетями (MonsterSkill.CatcherNet); Щит бьётся
  * один, Горн подходит уже после драки, а у одиночки Барти стоит за фонтаном.
  * После боя — куда девать новенького ([[Fate]]): исход — в подсчёт «как и
  * вы». У главаря — пропуск на просеку с печатью канцелярии, зацепка главы 8.
  *
  * Шаги: глава берётся в конце «Наутро» — 1: главная площадь, бой
  * (StorySquareState); 2: куда девать новенького — глава закрыта. Враги,
  * сложность и награда — по уровню задания, а не героя. */
object StoryNewcomerQuest {

  /** Уровень задания: по нему сложность, враги и награда. */
  val Level: Int = 31

  /** Сюжетные бои на площади: у Тени — со стражей совета, у остальных — с
    * ловцами (их сети бросаются только в этом бою). Награда как за обычный
    * бой, без «Сбежать». */
  val GuardStory: String    = "storySquare"
  val CatchersStory: String = MonsterSkill.CatcherNet.Story
  val FightStories: Set[String] = Set(GuardStory, CatchersStory)

  /** Те, кто пришёл за новеньким: именной главарь и четверо — люди. */
  val FoeRace: Race          = Race.Human
  val LeaderRarity: Rarity   = Rarity.Mythical
  val HenchmanRarity: Rarity = Rarity.Rare
  val Henchmen: Int          = 4

  /** Серебро за главу — за уровень задания, как в Акте I. */
  val SilverPerLvl: Long = 200L

  def silver: Long = SilverPerLvl * Level

  /** Куда девать новенького: номер — в `counter` закрытой главы. */
  sealed abstract class Fate(val n: Long, val key: String, val outcome: QuestOutcome)

  object Fate {
    case object Brotherhood extends Fate(1L, "brotherhood", QuestOutcome.NewcomerBrotherhood)
    case object Council     extends Fate(2L, "council", QuestOutcome.NewcomerCouncil)
    case object Hedge       extends Fate(3L, "hedge", QuestOutcome.NewcomerHedge)

    val all: List[Fate] = List(Brotherhood, Council, Hedge)
  }

  def toSquare(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryNewcomer, 1)
  def toFate(q: NpcQuests): Boolean   = q.onStep(NpcQuest.StoryNewcomer, 2)

  /** С карточки описи — на главную площадь: к бою или к выбору. */
  def atSquare(q: NpcQuests): Boolean = toSquare(q) || toFate(q)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryNewcomer)(_.copy(step = n))

  /** Кто пришёл за новеньким: у Тени — стража совета, у остальных — ловцы. */
  def guards(q: NpcQuests): Boolean = StoryToolQuest.choice(q).contains(Path.Shadow)

  /** Куда дели новенького — у закрытой главы. */
  def fate(q: NpcQuests): Option[Fate] =
    Option.when(q.isDone(NpcQuest.StoryNewcomer))(q.of(NpcQuest.StoryNewcomer).counter).flatMap(n => Fate.all.find(_.n == n))

  /** Главарь — первым, именной, и четверо. */
  def foes(leader: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, FoeRace, LeaderRarity, Some(leader)) ::
      List.fill(Henchmen)(MonsterGenerator.generateOfRaceAndRarity(Level, FoeRace, HenchmanRarity))

  /** Строка карточки описи — что делать сейчас. Глава 6 не закрыта или 7-я уже
    * закрыта — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryMorning) || q.isDone(NpcQuest.StoryNewcomer)) ""
    else content.list("story.storyNewcomer.card.steps").lift(q.of(NpcQuest.StoryNewcomer).step - 1).getOrElse("")
}
