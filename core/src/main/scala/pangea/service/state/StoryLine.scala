package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.hero.{Hero, LoreData}
import pangea.model.quest.{FirstSteps, NpcQuest, NpcQuests}
import pangea.model.state.StateType
import pangea.model.user.UserId
import zio.{Task, ZIO}

/** Основная сюжетная линия — главы после «Встать на ноги». Глава — задание
  * ([[NpcQuest]]) с шагами и своим уровнем задания (враги, сложность,
  * награда). По уровню героя главы не запираются и «рано» не говорят: следующая
  * открыта, как только закрыта прежняя; сложность — на карточке зацепки, а
  * сунулся слабым — его забота.
  *
  * Ведёт главу предмет в сумке: что делать сейчас — на его карточке, и дальше
  * идут через неё. Зацепки находятся по одной, по порядку: первую герой
  * находит сам — когда «Встать на ноги» засчитано дома (FirstStepsQuest), на
  * [[FirstCluePasses]]-м исследовании уровня лабиринта (`firstClue`); каждая
  * следующая — в развязке предыдущей главы. Пока зацепки нет, следующую
  * главу игроку не показывают. В журнале текущая глава стоит сюжетным
  * заданием (📌), а не среди заданий горожан.
  *
  * Тексты глав — `story.<ключ задания>` в `scenes.yaml`, шаги для журнала —
  * `journal.quests.<ключ>.steps`, как у заданий горожан. */
object StoryLine {

  /** Глава и — для первой — сцена, где герой находит первую зацепку. У
    * следующих глав зацепка уже в сумке: её дала развязка прежней. */
  final case class Chapter(quest: NpcQuest, firstClue: Option[StateType] = None)

  /** Главы по порядку. */
  val Chapters: List[Chapter] = List(
    Chapter(NpcQuest.StoryMark, firstClue = Some(StateType.StoryCloak)),
    Chapter(NpcQuest.StoryPowder),
    Chapter(NpcQuest.StoryBrand),
    Chapter(NpcQuest.StoryList),
    Chapter(NpcQuest.StoryFace),
    // Рубеж I: глава с выбором пути, последняя в Акте I.
    Chapter(NpcQuest.StoryTool)
  )

  /** На каком по счёту исследовании уровня после «Встать на ноги» герой
    * находит первую зацепку. */
  val FirstCluePasses: Int = 5

  def isChapter(q: NpcQuest): Boolean = Chapters.exists(_.quest == q)

  /** Глава в работе: взята и не закрыта. */
  def active(quests: NpcQuests): Option[NpcQuest] = Chapters.map(_.quest).find(quests.isTaken)

  /** Глава, которую можно начать прямо сейчас: «Встать на ноги» засчитано,
    * другой главы в работе нет — первая незакрытая. */
  def startable(quests: NpcQuests, lore: LoreData): Option[Chapter] =
    if (!lore.firstStepsDone || active(quests).isDefined) None
    else Chapters.find(c => !quests.isDone(c.quest))

  /** Главу можно начать с её зацепки: прежняя глава закрыта, эта ещё не
    * взята и не закрыта, другой в работе нет. Уровень героя не важен. Зацепка
    * у героя — её проверяет тот, кто начинает (карточка или NPC). */
  def canBegin(quests: NpcQuests, q: NpcQuest): Boolean = {
    val i = Chapters.indexWhere(_.quest == q)
    i >= 0 && Chapters.lift(i - 1).forall(p => quests.isDone(p.quest)) && !quests.isTaken(q) && !quests.isDone(q) &&
      active(quests).isEmpty
  }

  /** Исследование уровня лабиринта: пока первая зацепка не найдена, каждое
    * считается, и на [[FirstCluePasses]]-м герой выходит на неё — встреча
    * назначена (см. DungeonState). Данные героя читаются, только когда дело
    * может дойти до зацепки: задания горожан сданы. */
  def firstClueDue(heroDao: HeroDao, hero: Hero, quests: NpcQuests): Task[Option[StateType]] =
    Chapters.find(c => !quests.isDone(c.quest))
      .filter(c => c.firstClue.isDefined && active(quests).isEmpty &&
        FirstSteps.CityQuests.forall(quests.isDone)) match {
      case None => ZIO.none
      case Some(c) =>
        heroDao.readLoreData(hero.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty)).flatMap { lore =>
          if (!lore.firstStepsDone) ZIO.none
          else {
            val passes = (lore.cluePasses + 1).min(FirstCluePasses)
            ZIO.when(passes != lore.cluePasses)(heroDao.writeLoreData(hero.userId, lore.copy(cluePasses = passes).asJson))
              .as(Option.when(passes >= FirstCluePasses)(c.firstClue).flatten)
          }
        }
    }

  /** Зацепку уже пора найти: задание засчитано и исследований хватило. */
  def clueReady(lore: LoreData): Boolean = lore.firstStepsDone && lore.cluePasses >= FirstCluePasses

  /** Кто из двоих предателей пережил пролог. Человеком герой проснулся в теле
    * Элиаса — жив Элорин; эльфом — в теле Элорина, жив Элиас; гномом и орком
    * (и у героев, чей пролог не записан) — живы оба, и встретит он обоих. */
  sealed trait Survivors { def suffix: String }

  object Survivors {
    case object Elorin extends Survivors { val suffix: String = "Elorin" }
    case object Elias  extends Survivors { val suffix: String = "Elias" }
    case object Both   extends Survivors { val suffix: String = "Both" }
  }

  def survivors(lore: LoreData): Survivors = lore.prologueBranch match {
    case Some("Human") => Survivors.Elorin
    case Some("Elf")   => Survivors.Elias
    case _             => Survivors.Both
  }

  /** Кто пережил пролог — по записи героя (нет записи — живы оба). */
  def survivorsOf(heroDao: HeroDao, userId: UserId): Task[Survivors] =
    heroDao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty)).map(survivors)

  /** Название главы для игрока. */
  def title(content: SceneContent, q: NpcQuest): String = content.text(s"journal.quests.${q.key}.title")

  /** Что делать в главе сейчас — строка шага для журнала. В «Что горит на
    * груди» собеседник шага, назвавший цену разговора, — строкой с ценой
    * (`priced`, см. [[StoryBrandQuest.priced]]). */
  def step(content: SceneContent, q: NpcQuest, quests: NpcQuests): String = {
    val list = if (q == NpcQuest.StoryBrand && StoryBrandQuest.priced(quests)) "priced" else "steps"
    content.list(s"journal.quests.${q.key}.$list").lift(quests.of(q).step - 1).getOrElse("")
  }

  /** Строка «новое задание»: название и первый шаг. */
  def takenLine(content: SceneContent, q: NpcQuest, quests: NpcQuests): String =
    content.format("story.taken", "title" -> title(content, q), "step" -> step(content, q, quests))
}
