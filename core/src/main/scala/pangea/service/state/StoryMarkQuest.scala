package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.LoreData
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}

/** Глава 1 основной линии, «Метка под меткой» ([[NpcQuest.StoryMark]], см.
  * [[StoryLine]]). Ведёт её чёрный плащ — первая зацепка: герой находит его у
  * изгороди, когда «Встать на ноги» пройдено (StoryCloakState). Ришелье
  * спарывает с плаща гильдейскую метку — под ней знак Братства; по номеру на
  * метке Горн называет Ульфа Сороку, которого нашли без вещей на пятом
  * повороте; тайник Сороки ищут с карточки плаща, в лабиринте. Тайник сторожат
  * меченые звери, а в нём — следующая зацепка, шифровка за подписью «Э.».
  *
  * Шаги: 1 — показать плащ Ришелье; 2 — показать метку Горну; 3 — найти тайник
  * Сороки с [[CacheLevel]]-го поворота лабиринта (бой, StoryCacheState).
  * Закрывается, когда тайник отбит: плащ уходит, шифровка — в сумку. */
object StoryMarkQuest {

  /** Уровень задания: по нему сложность, звери у тайника и заначка Сороки —
    * не по уровню героя. */
  val Level: Int = 5

  /** С какого поворота лабиринта искать тайник Сороки. */
  val CacheLevel: Int = 5

  /** Ключ сюжетного боя у тайника: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "storyCache"

  /** Кто сторожит тайник: звери изгороди, отмеченные тьмой, — уровня задания. */
  val GuardCount: Int     = 3
  val GuardRace: Race     = Race.Animal
  val GuardRarity: Rarity = Rarity.Uncommon

  /** Заначка Сороки — столько серебра за уровень задания. */
  val SilverPerLvl: Long = 150L

  def toRichelieu(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryMark, 1)
  def toHorn(q: NpcQuests): Boolean      = q.onStep(NpcQuest.StoryMark, 2)
  def toCache(q: NpcQuests): Boolean     = q.onStep(NpcQuest.StoryMark, 3)

  /** Ришелье спорол метку — к Горну. */
  def shown(q: NpcQuests): NpcQuests = q.update(NpcQuest.StoryMark)(_.copy(step = 2))

  /** Горн назвал Сороку — искать тайник. */
  def logged(q: NpcQuests): NpcQuests = q.update(NpcQuest.StoryMark)(_.copy(step = 3))

  /** Искать тайник можно с этого поворота лабиринта. */
  def deepEnough(dungeonLevel: Int): Boolean = dungeonLevel >= CacheLevel

  /** Что плащ говорит на карточке сейчас — по шагу главы. Глава закрыта или не
    * взята — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    Option.when(q.isTaken(NpcQuest.StoryMark))(q.of(NpcQuest.StoryMark).step)
      .flatMap(n => content.list("story.storyMark.card.steps").lift(n - 1))
      .getOrElse("")

  /** Стража тайника. */
  def guards: List[Monster] =
    List.fill(GuardCount)(MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, GuardRarity).copy(marked = true))

  def silver: Long = SilverPerLvl * Level

  /** Ключ мысли героя над подписью «Э.» — о том, кто из двоих жив. */
  def cipherKey(lore: LoreData): String = "story.storyMark.cache.signed" + StoryLine.survivors(lore).suffix
}
