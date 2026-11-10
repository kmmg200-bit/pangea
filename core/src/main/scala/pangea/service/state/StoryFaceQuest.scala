package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}

/** Глава 5 основной линии, «Лицо из прошлого» ([[NpcQuest.StoryFace]], см.
  * [[StoryLine]]). Ведёт её ключ от архива канцелярии, что оставила Сойка.
  * Ночью в архиве — книга отмеченных, свежий переплёт с ярлычком «Переплётная
  * Брея, у рыбного рынка» и заметки на полях рукой того, кто пережил пролог
  * ([[StoryLine.survivors]]). Днём в переплётной — его лицо: он не узнаёт
  * героя, а ударить при свидетелях нельзя. Ночью там же — ячейка Братства
  * (бой) и копии списка отмеченных: против многих имён — гильдейские номера.
  * Горн узнаёт свои номера и даёт слово Гильдии, Юнона — бумагу совета и
  * обещает стражу; на ступенях канцелярии герою суют записку «Э.» — его узнали
  * по почерку драки. Записка — зацепка Рубежа I.
  *
  * Шаги: глава берётся с карточки ключа («Открыть архив», из города) — 1:
  * архив (StoryArchiveState); 2: переплётная днём; 3: она же ночью (бой,
  * StoryBinderyState); 4: Горн; 5: Юнона — глава закрыта. Враги, сложность и
  * награда — по уровню задания, а не героя. */
object StoryFaceQuest {

  /** Уровень задания: по нему сложность, люди ячейки и награда. */
  val Level: Int = 21

  /** Ключ сюжетного боя в переплётной: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "storyBindery"

  /** Ячейка в переплётной: старший и трое — люди, уровня задания. */
  val GuardCount: Int      = 3
  val GuardRace: Race      = Race.Human
  val GuardRarity: Rarity  = Rarity.Uncommon
  val LeaderRarity: Rarity = Rarity.Rare

  /** Сколько серебра за уровень задания платит совет в конце главы. */
  val SilverPerLvl: Long = 200L

  def toArchive(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryFace, 1)
  def toFace(q: NpcQuests): Boolean    = q.onStep(NpcQuest.StoryFace, 2)
  def toRaid(q: NpcQuests): Boolean    = q.onStep(NpcQuest.StoryFace, 3)
  def toHorn(q: NpcQuests): Boolean    = q.onStep(NpcQuest.StoryFace, 4)
  def toJunona(q: NpcQuests): Boolean  = q.onStep(NpcQuest.StoryFace, 5)

  /** Кнопка «Открыть архив» на карточке ключа: главу можно начать или она на
    * шаге архива. */
  def archiveOpen(q: NpcQuests): Boolean = toArchive(q) || StoryLine.canBegin(q, NpcQuest.StoryFace)

  /** В переплётную с карточки ключа: днём — посмотреть, ночью — за копиями. */
  def toBindery(q: NpcQuests): Boolean = toFace(q) || toRaid(q)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryFace)(_.copy(step = n))

  /** Строка карточки ключа (и копий) — что делать сейчас. Глава 4 не закрыта
    * или 5 уже закрыта — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryList) || q.isDone(NpcQuest.StoryFace)) ""
    else if (!q.isTaken(NpcQuest.StoryFace)) content.text("story.storyFace.card.ready")
    else content.list("story.storyFace.card.steps").lift(q.of(NpcQuest.StoryFace).step - 1).getOrElse("")

  /** Старший ячейки и его люди: он — первым, именной. */
  def guards(leaderName: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, LeaderRarity, Some(leaderName)) ::
      List.fill(GuardCount)(MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, GuardRarity))

  def silver: Long = SilverPerLvl * Level
}
