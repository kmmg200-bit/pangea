package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.hero.{Hero, LoreData}
import pangea.model.home.HomeData
import pangea.model.quest.{BoardDesk, DailyNpc, DailyQuests, DailyRates, NpcQuest, NpcQuests}
import pangea.service.state.states.guild.QuestBoardState
import zio.Task

/** «📜 Задания» из «Персонажа»: всё, что у героя сейчас в работе. Сюжетное
  * «Встать на ноги», задания горожан на своём шаге, сегодняшние поручения и
  * объявления с доски. Только читает, ничего не пишет.
  *
  * Журнал собирается данными ([[Journal]]): бот печатает его одним сообщением
  * ([[render]]), Mini App раскладывает по разделам — с одними и теми же словами. */
object QuestJournal {

  /** Сюжетное задание: название и список целей с отметками. */
  final case class Story(title: String, objectives: String)

  /** Задание горожанина: название и что делать на этом шаге. */
  final case class Npc(title: String, step: String)

  /** Сегодняшнее поручение: чьё и что осталось. */
  final case class Daily(npc: String, line: String)

  /** Объявление с доски: какая доска, что просят, сделано ли. */
  final case class Board(board: String, what: String, ready: Boolean)

  final case class Journal(story: Option[Story], npcs: List[Npc], dailies: List[Daily], boards: List[Board]) {
    def isEmpty: Boolean = story.isEmpty && npcs.isEmpty && dailies.isEmpty && boards.isEmpty
  }

  def text(heroDao: HeroDao, content: SceneContent, hero: Hero, nowMs: Long): Task[String] =
    load(heroDao, content, hero, nowMs).map(format(content, _))

  /** Журнал героя: читает всё, что в работе, и ничего не пишет. */
  def load(heroDao: HeroDao, content: SceneContent, hero: Hero, nowMs: Long): Task[Journal] =
    for {
      lore   <- heroDao.readLoreData(hero.userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))
      quests <- NpcQuestLog.load(heroDao, hero.userId)
      daily  <- DailyQuestLog.load(heroDao, hero.userId)
      board  <- heroDao.readQuestData(hero.userId).map(_.flatMap(_.as[BoardDesk].toOption))
      home   <- heroDao.readHomeData(hero.userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
    } yield journal(content, hero, lore, quests, daily, board, nowMs, home)

  /** Разделы — через пустую строку: сюжет, горожане, поручения, доска. Пусто
    * везде — так и говорим. */
  def render(content: SceneContent, hero: Hero, lore: LoreData, quests: NpcQuests, daily: DailyQuests,
             board: Option[BoardDesk], nowMs: Long, home: HomeData = HomeData.empty): String =
    format(content, journal(content, hero, lore, quests, daily, board, nowMs, home))

  def journal(content: SceneContent, hero: Hero, lore: LoreData, quests: NpcQuests, daily: DailyQuests,
              board: Option[BoardDesk], nowMs: Long, home: HomeData = HomeData.empty): Journal = {
    // Сюжет: «Встать на ноги», пока не засчитано дома, а дальше — глава
    // основной линии в работе (см. StoryLine). Главы — не задания горожан: в
    // их список они не идут.
    val story = Option.when(lore.firstSteps && !lore.firstStepsDone)(
      Story(content.text("firstSteps.title"), FirstStepsQuest.objectives(content, hero, quests, home)))
      .orElse(StoryLine.active(quests).map(q => Story(StoryLine.title(content, q), StoryLine.step(content, q, quests))))
    val npcs = NpcQuest.values.toList.filter(q => quests.isTaken(q) && !StoryLine.isChapter(q)).map(npc(content, _, quests))
    val today = DailyRates.dayOf(nowMs)
    val dailies = DailyNpc.values.toList.flatMap { npc =>
      daily.today(npc, today).filter(t => t.taken && !t.done).map { t =>
        Daily(content.text(s"journal.dailyNpc.${npc.key}"),
          content.format(s"daily.${npc.key}.tasks.${t.kind.key}.${if (t.ready) "ready" else "active"}",
            "goal" -> t.need.toString, "have" -> t.progress.toString, "what" -> t.what))
      }
    }
    // Доска переписывается по понедельникам вместе со взятым, а ушёл на два
    // раздела вперёд — доска уже не открыта: с таких в работе ничего не числится.
    val boards = board.toList.flatMap(_.fresh(nowMs, hero.lvl).boards).flatMap { data =>
      data.slots.filter(_.taken).map(slot => Board(data.board.title, QuestBoardState.ask(content, slot), slot.ready))
    }
    Journal(story, npcs, dailies, boards)
  }

  /** Журнал одним сообщением — так его показывает бот. */
  def format(content: SceneContent, j: Journal): String = {
    val story   = j.story.map(s => content.format("journal.story", "title" -> s.title, "objectives" -> s.objectives)).toList
    val npcs    = j.npcs.map(n => content.format("journal.npc", "title" -> n.title, "step" -> n.step))
    val dailies = j.dailies.map(d => content.format("journal.daily", "npc" -> d.npc, "line" -> d.line))
    val boards  = j.boards.map(b => content.format("journal.board", "board" -> b.board, "what" -> b.what,
                    "state" -> boardState(content, b)))
    val sections = List(story, npcs, dailies, boards).filter(_.nonEmpty).map(_.mkString("\n"))
    if (sections.isEmpty) content.text("journal.empty")
    else content.text("journal.header") + "\n\n" + sections.mkString("\n\n")
  }

  /** «в работе» или «сделано, можно сдавать». */
  def boardState(content: SceneContent, b: Board): String =
    content.text(if (b.ready) "journal.boardReady" else "journal.boardActive")

  /** Задание горожанина: название и что делать сейчас — по шагу, со счётчиком,
    * где он есть. Шаг, которому нет строки, задание не прячет: тогда остаётся
    * одно название. */
  private def npc(content: SceneContent, q: NpcQuest, quests: NpcQuests): Npc = {
    val p     = quests.of(q)
    val base  = s"journal.quests.${q.key}"
    val steps = content.list(s"$base.steps")
    val step  = steps.lift(p.step - 1).map(_.replace("{counter}", p.counter.toString))
    // Вскрытое письмо Марисе — это и карта к тайнику: можно идти, не дожидаясь Марисы.
    val extra = Option.when(q == NpcQuest.Marisa && p.bonus && p.step < 3)(content.text(s"$base.map"))
    Npc(content.text(s"$base.title"), (step.toList ++ extra).mkString(" "))
  }
}
