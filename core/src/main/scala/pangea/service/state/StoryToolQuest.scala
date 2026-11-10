package pangea.service.state

import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.Achievement
import pangea.model.item.{Item, ItemType, QuestItemKind, Rarity => ItemRarity}
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
  * своё снаряжение (сюжетная экипировка 25-го уровня с характеристиками
  * легендарной, см. [[gear]]) и строка «как и вы»; у Щита и одиночки — ещё и
  * знак пути в сумке. Акт II начнётся от записанного пути ([[choice]]), а не
  * от вещи: перстень Тени носят и могут продать.
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

  /** Выжившие — легендарные именные враги: и один, и двое. Люди ячейки —
    * люди, уровня задания: при одном выжившем — трое редких, при двоих — двое
    * необычных. */
  val GuardRace: Race            = Race.Human
  val SoloRarity: Rarity         = Rarity.Legendary
  val SoloGuards: Int            = 3
  val SoloGuardRarity: Rarity    = Rarity.Rare
  val PairRarity: Rarity         = Rarity.Legendary
  val PairGuards: Int            = 2
  val PairGuardRarity: Rarity    = Rarity.Uncommon

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

  /** Вещь из снаряжения пути: вид и имя. */
  final case class Piece(itemType: ItemType, name: String)

  /** Снаряжение пути — этого уровня, с характеристиками этой редкости. */
  val GearLvl: Long          = 25L
  val GearRarity: ItemRarity = ItemRarity.Orange

  /** Путь, выбранный на рубеже. Номер — в `counter` закрытого рубежа. `token` —
    * знак пути в сумке (сюжетный предмет), `gear` — снаряжение пути, `gearLine`
    * — своя строка о том, откуда оно (`story.storyTool.path.<key>.gear`). */
  sealed abstract class Path(val n: Long, val key: String, val outcome: QuestOutcome, val achievement: Achievement,
                             val token: Option[QuestItemKind], val gear: List[Piece], val gearLine: Boolean)

  object Path {
    /** Тень: знак пути — сам перстень Братства, его носят. Отдаёт его выживший —
      * об этом уже говорит текст пути. */
    case object Shadow extends Path(1L, "shadow", QuestOutcome.ToolShadow, Achievement.ShadowPath, None,
      List(Piece(ItemType.Ring, "Перстень Братства")), gearLine = false)
    /** Щит: знак — грамота совета; именные доспех и щит по приказу Юноны
      * подвозят к дому героя. */
    case object Shield extends Path(2L, "shield", QuestOutcome.ToolShield, Achievement.ShieldPath,
      Some(QuestItemKind.StoryCouncilWrit),
      List(Piece(ItemType.ChestPlate, "Именной доспех стража Кинэта"), Piece(ItemType.Shield, "Именной щит стража Кинэта")),
      gearLine = true)
    /** Сам по себе: знак — книга ячейки; маску герой находит в её тайнике. */
    case object Alone extends Path(3L, "alone", QuestOutcome.ToolAlone, Achievement.OwnPath,
      Some(QuestItemKind.StoryCellBook), List(Piece(ItemType.Helmet, "Маска без лица")), gearLine = true)

    val all: List[Path] = List(Shadow, Shield, Alone)
  }

  /** Снаряжение пути: каждая вещь — своего вида, уровня [[GearLvl]], с
    * характеристиками редкости [[GearRarity]] и своим именем; сюжетная (см.
    * `Item.isStoryGear`): в списках ★ вместо кружка редкости. */
  def gear(path: Path, rng: Rng): List[Item] =
    path.gear.foldLeft((List.empty[Item], rng)) { case ((acc, r), piece) =>
      val (item, next) = ItemGenerator.createItemOfType(piece.itemType, GearLvl, GearRarity, r)
      (acc :+ item.withName(piece.name).asStoryGear, next)
    }._1

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
    def guards(n: Int, rarity: Rarity) = List.fill(n)(MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, rarity))
    survivors match {
      case Survivors.Elorin => named(ElorinRace, SoloRarity, "elorin") :: guards(SoloGuards, SoloGuardRarity)
      case Survivors.Elias  => named(EliasRace, SoloRarity, "elias") :: guards(SoloGuards, SoloGuardRarity)
      case Survivors.Both   =>
        named(ElorinRace, PairRarity, "elorin") :: named(EliasRace, PairRarity, "elias") :: guards(PairGuards, PairGuardRarity)
    }
  }

  def silver: Long = SilverPerLvl * Level
}
