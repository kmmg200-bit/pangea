package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests, QuestOutcome}

/** Глава 4 основной линии, «Список» ([[NpcQuest.StoryList]], см. [[StoryLine]]).
  * Ведёт её страница из списка отмеченных, которую отдала Сойка: на ней новое
  * имя героя и «Кинэт. Ждать». Рахадим узнаёт бумагу — гербовые книги
  * Торгового дома идут только в канцелярию совета: кто-то там переписывает
  * имена для Братства. Сойка обещает показать того, кто носит бумаги, — ночью
  * у Мельничного моста; там засада чистильщиков Братства (бой), а на их
  * главаре — приказ: отмеченного приведёт «птица», за это её вычеркнут.
  * Сойка продала героя. Разговор с ней — развилка: простить или порвать (исход
  * — в подсчёт «как и вы»); так или иначе на столе остаётся ключ от архива
  * канцелярии — зацепка следующей главы.
  *
  * Шаги: глава берётся у Рахадима (страница при герое) и сразу идёт дальше —
  * 2: рассказать Сойке (с карточки, из города); 3: к Мельничному мосту (бой,
  * StoryBridgeState); 4: найти Сойку — развилка (StorySojkaState). Закрыта —
  * `counter` хранит выбор ([[SojkaFate]]). Враги, сложность и награда — по
  * уровню задания, а не героя. */
object StoryListQuest {

  /** Уровень задания: по нему сложность, чистильщики и награда. */
  val Level: Int = 17

  /** Ключ сюжетного боя у моста: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "storyBridge"

  /** Чистильщики Братства: главарь и трое — люди, уровня задания. */
  val GuardCount: Int      = 3
  val GuardRace: Race      = Race.Human
  val GuardRarity: Rarity  = Rarity.Uncommon
  val LeaderRarity: Rarity = Rarity.Rare

  /** Сколько серебра за уровень задания — в кошельке главаря. */
  val SilverPerLvl: Long = 200L

  def toRakhadim(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryList, 1)
  def toSojka(q: NpcQuests): Boolean    = q.onStep(NpcQuest.StoryList, 2)
  def toBridge(q: NpcQuests): Boolean   = q.onStep(NpcQuest.StoryList, 3)
  def toConfront(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryList, 4)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryList)(_.copy(step = n))

  /** Что герой сделал с Сойкой после моста. Номер — в `counter` закрытой главы. */
  sealed abstract class SojkaFate(val n: Long, val key: String, val outcome: QuestOutcome)

  object SojkaFate {
    case object Forgave extends SojkaFate(1L, "forgave", QuestOutcome.ListForgave)
    case object Broke   extends SojkaFate(2L, "broke", QuestOutcome.ListBroke)

    val all: List[SojkaFate] = List(Forgave, Broke)
  }

  /** Как кончилась глава — для реплик следующих глав. */
  def choice(q: NpcQuests): Option[SojkaFate] =
    Option.when(q.isDone(NpcQuest.StoryList))(q.of(NpcQuest.StoryList).counter)
      .flatMap(n => SojkaFate.all.find(_.n == n))

  /** Строка карточки страницы — что делать сейчас. Глава 3 не закрыта или 4
    * уже закрыта — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryBrand) || q.isDone(NpcQuest.StoryList)) ""
    else if (!q.isTaken(NpcQuest.StoryList)) content.text("story.storyList.card.ready")
    else content.list("story.storyList.card.steps").lift(q.of(NpcQuest.StoryList).step - 1).getOrElse("")

  /** Главарь чистильщиков и его люди: он — первым, именной. */
  def guards(leaderName: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, LeaderRarity, Some(leaderName)) ::
      List.fill(GuardCount)(MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, GuardRarity))

  def silver: Long = SilverPerLvl * Level
}
