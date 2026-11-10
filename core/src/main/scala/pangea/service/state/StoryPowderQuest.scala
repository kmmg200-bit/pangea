package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests, QuestOutcome}

/** Глава 2 основной линии, «Порох у причала» ([[NpcQuest.StoryPowder]], см.
  * [[StoryLine]]). Ведёт её шифровка Братства из тайника Сороки: «склад у
  * причала, где пахнет дёгтем», «Смотритель проверит печать». Глава берётся
  * в конце главы 1, когда тайник отбит. С карточки шифровки — «Идти в порт»
  * (из города): дегтярный склад, бочки пороха,
  * Смотритель узнаёт печать, но не курьера — бой с его людьми. Потом —
  * развилка: добить Смотрителя, отпустить или сдать совету (исход — в
  * подсчёт, см. [[QuestOutcome]]). В бумагах Смотрителя — следующая зацепка,
  * набросок узора: Клеймо героя и приписка «брать живым».
  *
  * Шаги: 1 — пробраться на склад (бой, StoryWarehouseState); 2 — люди
  * Смотрителя перебиты, решить его судьбу. Закрыта — `counter` хранит выбор
  * ([[WardenFate]]): его помнят следующие главы. */
object StoryPowderQuest {

  /** Уровень задания: по нему сложность, люди Смотрителя и его касса — не по
    * уровню героя. */
  val Level: Int = 9

  /** Ключ сюжетного боя на складе: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "storyWarehouse"

  /** Люди Смотрителя: трое, уровня задания; во главе — сам Смотритель. */
  val GuardCount: Int      = 3
  val GuardRace: Race      = Race.Human
  val GuardRarity: Rarity  = Rarity.Uncommon
  val WardenRarity: Rarity = Rarity.Rare

  /** Касса Смотрителя — столько серебра за уровень задания. */
  val SilverPerLvl: Long = 200L

  def toWarehouse(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryPowder, 1)
  def beaten(q: NpcQuests): Boolean      = q.onStep(NpcQuest.StoryPowder, 2)

  /** С карточки шифровки можно в порт: глава в работе (склад или судьба
    * Смотрителя) — или её можно начать (если в конце главы 1 она почему-то не
    * взята): первая глава закрыта, другой главы в работе нет. Уровень героя не
    * важен. */
  def portOpen(q: NpcQuests): Boolean =
    toWarehouse(q) || beaten(q) || StoryLine.canBegin(q, NpcQuest.StoryPowder)

  /** Люди Смотрителя перебиты — решать его судьбу. */
  def won(q: NpcQuests): NpcQuests = q.update(NpcQuest.StoryPowder)(_.copy(step = 2))

  /** Что герой сделал со Смотрителем. Номер — в `counter` закрытой главы. */
  sealed abstract class WardenFate(val n: Long, val key: String, val outcome: QuestOutcome)

  object WardenFate {
    case object Killed   extends WardenFate(1L, "killed", QuestOutcome.PowderKilled)
    case object Released extends WardenFate(2L, "released", QuestOutcome.PowderReleased)
    case object Handed   extends WardenFate(3L, "handed", QuestOutcome.PowderHanded)

    val all: List[WardenFate] = List(Killed, Released, Handed)

    def byKey(key: String): Option[WardenFate] = all.find(_.key == key)
  }

  /** Как кончилась глава — для реплик следующих глав. */
  def choice(q: NpcQuests): Option[WardenFate] =
    Option.when(q.isDone(NpcQuest.StoryPowder))(q.of(NpcQuest.StoryPowder).counter)
      .flatMap(n => WardenFate.all.find(_.n == n))

  /** Закрыть главу выбором. */
  def finish(q: NpcQuests, c: WardenFate): NpcQuests =
    q.update(NpcQuest.StoryPowder)(_.copy(counter = c.n)).finish(NpcQuest.StoryPowder)

  /** Строка карточки шифровки: пора в порт. Глава 1 не закрыта или 2 уже
    * закрыта — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryMark) || q.isDone(NpcQuest.StoryPowder)) ""
    else content.text("story.storyPowder.card.ready")

  /** Смотритель и его люди: он — первым, именной. */
  def guards(wardenName: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, WardenRarity, Some(wardenName)) ::
      List.fill(GuardCount)(MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, GuardRarity))

  def silver: Long = SilverPerLvl * Level
}
