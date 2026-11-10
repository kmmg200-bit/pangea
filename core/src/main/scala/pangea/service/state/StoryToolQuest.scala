package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.Achievement
import pangea.model.item.QuestItemKind
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests, QuestOutcome}
import pangea.service.state.StoryLine.Survivors

/** Рубеж I основной линии, «Чьё ты орудие» ([[NpcQuest.StoryTool]], см.
  * [[StoryLine]]) — конец Акта I. Ведёт его записка «Э.»: старое кладбище за
  * городской стеной, полночь. Там ждёт тот, кто пережил пролог (или оба), — и
  * его люди. Он узнал героя и предлагает сделку: войти в Братство самому.
  * Бой с ячейкой, а после — выбор пути ([[Path]]): Тень — войти в Братство под
  * новым лицом; Щит — подать знак страже совета и людям Горна; сам по себе —
  * свести счёты и забрать тайник ячейки. У каждого пути — своё достижение,
  * свой знак в сумке (зацепка Акта II) и строка «как и вы».
  *
  * Шаги: рубеж берётся с карточки записки («Идти на кладбище», из города) — 1:
  * встреча и бой (StoryGraveyardState); 2: выбор пути. Закрыт — `counter`
  * хранит путь: на следующем рубеже его можно сменить, а этот выбор остаётся
  * записанным. Враги, сложность и награда — по уровню задания, а не героя. */
object StoryToolQuest {

  /** Уровень задания: по нему сложность, ячейка и награда. */
  val Level: Int = 25

  /** Ключ сюжетного боя на кладбище: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "storyGraveyard"

  /** Люди ячейки — люди, уровня задания. Один выживший — мифический, при нём
    * трое; двое — оба редкие, при них двое: сила боя та же. */
  val GuardRace: Race         = Race.Human
  val GuardRarity: Rarity     = Rarity.Uncommon
  val SoloRarity: Rarity      = Rarity.Mythical
  val SoloGuards: Int         = 3
  val PairRarity: Rarity      = Rarity.Rare
  val PairGuards: Int         = 2

  /** Элорин — эльф, Элиас — человек (см. StoryLine.survivors). */
  val ElorinRace: Race = Race.Elf
  val EliasRace: Race  = Race.Human

  /** Рубеж щедрее главы: столько серебра за уровень задания. */
  val SilverPerLvl: Long = 300L

  def toGraveyard(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryTool, 1)
  def toChoice(q: NpcQuests): Boolean    = q.onStep(NpcQuest.StoryTool, 2)

  /** Кнопка «Идти на кладбище» на карточке записки: рубеж можно начать или он
    * в работе. */
  def open(q: NpcQuests): Boolean = toGraveyard(q) || toChoice(q) || StoryLine.canBegin(q, NpcQuest.StoryTool)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryTool)(_.copy(step = n))

  /** Путь, выбранный на рубеже. Номер — в `counter` закрытого рубежа; знак
    * пути остаётся в сумке. */
  sealed abstract class Path(val n: Long, val key: String, val outcome: QuestOutcome, val achievement: Achievement,
                             val token: QuestItemKind)

  object Path {
    case object Shadow extends Path(1L, "shadow", QuestOutcome.ToolShadow, Achievement.ShadowPath, QuestItemKind.StoryShadowRing)
    case object Shield extends Path(2L, "shield", QuestOutcome.ToolShield, Achievement.ShieldPath, QuestItemKind.StoryCouncilWrit)
    case object Alone  extends Path(3L, "alone", QuestOutcome.ToolAlone, Achievement.OwnPath, QuestItemKind.StoryCellBook)

    val all: List[Path] = List(Shadow, Shield, Alone)
  }

  /** Какой путь выбран на Рубеже I. */
  def choice(q: NpcQuests): Option[Path] =
    Option.when(q.isDone(NpcQuest.StoryTool))(q.of(NpcQuest.StoryTool).counter).flatMap(n => Path.all.find(_.n == n))

  def finish(q: NpcQuests, path: Path): NpcQuests = q.finish(NpcQuest.StoryTool).update(NpcQuest.StoryTool)(_.copy(counter = path.n))

  /** Строка карточки записки — что делать сейчас. Глава 5 не закрыта или
    * рубеж уже пройден — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryFace) || q.isDone(NpcQuest.StoryTool)) ""
    else if (!q.isTaken(NpcQuest.StoryTool)) content.text("story.storyTool.card.ready")
    else content.list("story.storyTool.card.steps").lift(q.of(NpcQuest.StoryTool).step - 1).getOrElse("")

  /** Кто ждёт на кладбище: выживший (или оба — первым Элорин) и люди ячейки. */
  def foes(survivors: Survivors, content: SceneContent): List[Monster] = {
    def named(race: Race, rarity: Rarity, key: String) =
      MonsterGenerator.generateOfRaceAndRarity(Level, race, rarity, Some(content.text(s"story.storyTool.names.$key")))
    def guards(n: Int) = List.fill(n)(MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, GuardRarity))
    survivors match {
      case Survivors.Elorin => named(ElorinRace, SoloRarity, "elorin") :: guards(SoloGuards)
      case Survivors.Elias  => named(EliasRace, SoloRarity, "elias") :: guards(SoloGuards)
      case Survivors.Both   =>
        named(ElorinRace, PairRarity, "elorin") :: named(EliasRace, PairRarity, "elias") :: guards(PairGuards)
    }
  }

  def silver: Long = SilverPerLvl * Level
}
