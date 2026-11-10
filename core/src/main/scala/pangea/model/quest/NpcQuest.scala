package pangea.model.quest

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.item.Gem

/** Сюжетные задания горожан — по одному на каждого, кто встречает новичка в
  * Кинэте. Каждое учит одной механике и рассказывает свой кусок истории города.
  * Ключ — имя секции в `scenes.yaml` (`npcQuests.<key>`). */
sealed abstract class NpcQuest(val key: String) extends EnumEntry

object NpcQuest extends Enum[NpcQuest] {
  val values: IndexedSeq[NpcQuest] = findValues

  /** Трактирщик, «Плата за первую кружку»: принести любой трофей. */
  case object Innkeeper extends NpcQuest("innkeeper")

  /** Ришелье, «Товар с того света»: продать три серых вещи через «Продать хлам». */
  case object Richelieu extends NpcQuest("richelieu")

  /** Густаво, «Подопытный»: выпить зелье, победить троих, пока оно действует. */
  case object Gustavo extends NpcQuest("gustavo")

  /** Жрец, «То, что не пропадает»: показать найденный камень. */
  case object Priest extends NpcQuest("priest")

  /** Мастер Горн, «Ржавая вилка»: набрать сто репутации, получить улучшение даром. */
  case object Horn extends NpcQuest("horn")

  // Трое из Торгового дома и гильдии просят большего, чем первые встречные:
  // за их заданиями надо спускаться глубоко и встречаться с теми, кто там сторожит.

  /** Казимир, «Первый узор»: принести большой рунный камень — на осколках
    * узор рваный, и читать по ним он не станет. */
  case object Kazimir extends NpcQuest("kazimir")

  /** Фет, «Живая кожа»: принести кожу упыря с Гнилого Джо — из такой он
    * доращивает Живую сумку. */
  case object Fet extends NpcQuest("fet")

  /** Рахадим, «Залог доверия»: принести кусок с элементаля — железо,
    * которое не остывает, или камень, тянущий к себе другие. Аукцион сюда не замешан:
    * банкир учит не торгам, а тому, зачем нужна ячейка. */
  case object Rakhadim extends NpcQuest("rakhadim")

  /** «Письмо Марисе»: письмо с семьдесят пятого убитого, поиски адресата, тайник
    * Кельвина и коллектор. Шаги: 1 — письмо в руках, 2 — спросил Трактирщика,
    * 3 — нашёл Марису (ждёт в Портовом квартале); `bonus` — письмо вскрыто,
    * карта на руках. */
  case object Marisa extends NpcQuest("marisa")

  /** С какого по счёту убитого моба выпадает письмо Марисе. */
  val MarisaLetterKill: Long = 75L

  /** «Деревня Мурлоков»: после двухсотого убитого к герою выходит старейшина
    * Мрачноглаз и просит принести в деревню снаряжение. Шаги: 1 — сотня
    * набрана, старейшина ещё не вышел; 2 — карта на руках, `counter` — сколько
    * снаряжения сдано; 3 — двадцать сдано, но клинок не влез в сумку и ждёт в
    * деревне. Закрывается помощью (клинок, «Любимец Мурлоков») или налётом
    * («Гроза Мурлоков»). */
  case object Murloc extends NpcQuest("murloc")

  /** С какого по счёту убитого к герою выходит старейшина мурлоков. */
  val MurlocElderKill: Long = 200L

  /** Сколько снаряжения просит старейшина. */
  val MurlocGearGoal: Long = 20L

  /** «Пропавшая дружина»: с пятисотого убитого — фонарь старшины гномьей
    * дружины «Двенадцать фонарей». Шаги: 1 — фонарь на руках, штольня не
    * найдена (`counter` = 1 — Брамбл рассказал, что знал); 2 — штольня под
    * серебряной жилой найдена, дружина ждёт решения; 3 — дружина выведена,
    * дары не приняты; 4 — груз забран, нести Гуннару на Стройку. `bonus` —
    * дружина брошена: Брамбл больше не нанимается. */
  case object LostSquad extends NpcQuest("lostSquad")

  /** С какого по счёту убитого выпадает фонарь старшины. */
  val LostSquadKill: Long = 500L

  /** «Бродячий ларец»: редкая встреча в лабиринте (см. RareQuests) — чужой
    * Ларец Азата, голодный, без хозяина. Шаги: 1 — ларец увязался за героем,
    * `counter` — сколько камней ему скормлено; 2 — сыт и помнит дорогу к
    * хозяину; 3 — хозяин упокоен, ларец нести Фету. Камни, что он съел, лежат
    * в [[NpcQuests.strayGems]] — Фет вернёт их в ларец героя. */
  case object StrayCasket extends NpcQuest("strayCasket")

  /** «Тот, кто уговаривает деревья»: редкая встреча в лабиринте (см.
    * RareQuests) — эльф Элиар поёт изгороди, прося у неё ветвь, а на песню идут
    * орки-лесорубы, три волны. Задание берётся только победой: шаг 1 — ветвь на
    * руках, нести Фету. «Не сейчас», уход между волнами и смерть его не
    * начинают — эльфа надо встретить снова. */
  case object HedgeSong extends NpcQuest("hedgeSong")

  /** «Вскрытая бочка»: гоблины-рабы вскрыли бочку в порту (см.
    * BarrelThievesQuest) — унесли всё, оставили улику. Шаги: 1 — бочка
    * вскрыта, узнать в городе, чья работа (Ришелье); 2 — место известно, идти
    * по следу. `counter` — какая банда (номер в списке банд). Задание
    * повторяется: после возврата бочку могут вскрыть снова — тогда оно берётся
    * заново, на первый шаг. */
  case object BarrelThieves extends NpcQuest("barrelThieves")

  // ── Основная линия (см. StoryLine): главы по уровням героя, после «Встать на
  // ноги». Ключи лежат в `npc_quests` — не переименовывать.

  /** Глава 1, «Метка под меткой» (задание 5-го уровня): ведёт её чёрный плащ,
    * найденный у изгороди, — под гильдейской меткой на нём знак Братства.
    * Шаги: 1 — показать плащ Ришелье (спорет метку); 2 — показать метку Горну;
    * 3 — найти тайник Ульфа Сороки с карточки плаща, с пятого поворота
    * лабиринта (бой). Закрывается, когда тайник отбит: плащ уходит, в сумке —
    * шифровка Братства за подписью «Э.», зацепка следующей главы. */
  case object StoryMark extends NpcQuest("storyMark")

  /** Глава 2, «Порох у причала»: ведёт шифровка из тайника Сороки. Берётся в
    * конце главы 1 и, пока глава не вышла, висит в журнале на первом шаге —
    * дождаться груза Братства в порту; пройти её пока нельзя (см. StoryLine). */
  case object StoryPowder extends NpcQuest("storyPowder")

  def byKey(key: String): Option[NpcQuest] = values.find(_.key == key)

  /** Сколько репутации Горн просит набрать трофеями. */
  val HornReputationGoal: Long = 100L

  /** Сколько побед под зельем нужно Густаво. */
  val GustavoVictoriesGoal: Long = 3L

  /** Сколько серых вещей Ришелье хочет купить разом. */
  val RichelieuGrayGoal: Int = 3

  /** Опыт за задание — как за столько боёв на текущем этаже. */
  val ExpPerDungeonLevel: Long = 5L

  /** Вступительным заданиям горожан ([[FirstSteps.CityQuests]]) — столько
    * опыта сверху: на первых этажах опыт по этажу совсем мал, а эти задания
    * и даются новичку. */
  val StarterExp: Long = 40L
}

/** Ход одного задания. `step` — 0, пока не взято, дальше номер текущего шага;
  * `counter` — что копится на шаге (победы, репутация); `since` — отметка
  * времени начала шага, если шагу важно время; `bonus` — разовая поблажка от
  * NPC (Густаво наливает ещё одно бесплатное зелье); `done` — выполнено. */
final case class NpcQuestProgress(
  step:    Int     = 0,
  counter: Long    = 0L,
  since:   Long    = 0L,
  bonus:   Boolean = false,
  done:    Boolean = false
) {
  def taken: Boolean  = step > 0 && !done
  def onStep(n: Int): Boolean = taken && step == n
}

object NpcQuestProgress {
  val empty: NpcQuestProgress = NpcQuestProgress()

  implicit val encoder: Encoder[NpcQuestProgress] = (p: NpcQuestProgress) => Json.obj(
    "step"    -> p.step.asJson,
    "counter" -> p.counter.asJson,
    "since"   -> p.since.asJson,
    "bonus"   -> p.bonus.asJson,
    "done"    -> p.done.asJson
  )

  implicit val decoder: Decoder[NpcQuestProgress] = (c: HCursor) =>
    for {
      step    <- c.getOrElse[Int]("step")(0)
      counter <- c.getOrElse[Long]("counter")(0L)
      since   <- c.getOrElse[Long]("since")(0L)
      bonus   <- c.getOrElse[Boolean]("bonus")(false)
      done    <- c.getOrElse[Boolean]("done")(false)
    } yield NpcQuestProgress(step, counter, since, bonus, done)
}

/** Все сюжетные задания героя и знания, которые они дают. Durable, лежит в
  * `heroes.npc_quests`. `recipes` — рецепты куба, которые герою уже объяснили
  * (пока только запись; показ в меню куба — следующим шагом). `strayGems` —
  * камни, которые съел бродячий ларец ([[NpcQuest.StrayCasket]]): вид и
  * качество каждого, по порядку; в конце задания Фет кладёт их в ларец героя. */
final case class NpcQuests(
  quests:    Map[String, NpcQuestProgress] = Map.empty,
  recipes:   List[String]                  = Nil,
  strayGems: List[Gem]                     = Nil
) {
  def of(q: NpcQuest): NpcQuestProgress = quests.getOrElse(q.key, NpcQuestProgress.empty)

  def updated(q: NpcQuest, p: NpcQuestProgress): NpcQuests = copy(quests = quests.updated(q.key, p))

  def update(q: NpcQuest)(f: NpcQuestProgress => NpcQuestProgress): NpcQuests = updated(q, f(of(q)))

  def isDone(q: NpcQuest): Boolean = of(q).done

  def isTaken(q: NpcQuest): Boolean = of(q).taken

  def onStep(q: NpcQuest, n: Int): Boolean = of(q).onStep(n)

  /** Взять задание: первый шаг, счётчики с нуля. */
  def take(q: NpcQuest, now: Long): NpcQuests = updated(q, NpcQuestProgress(step = 1, since = now))

  /** Закрыть задание. */
  def finish(q: NpcQuest): NpcQuests = update(q)(_.copy(done = true))

  def withRecipe(recipe: String): NpcQuests =
    if (recipes.contains(recipe)) this else copy(recipes = recipes :+ recipe)
}

object NpcQuests {
  val empty: NpcQuests = NpcQuests()

  /** Рецепт куба «три горсти пыли → надколотый камень», который открывает Жрец. */
  val DustAssemblyRecipe: String = "dustAssembly"

  implicit val encoder: Encoder[NpcQuests] = (q: NpcQuests) => Json.obj(
    "quests"    -> q.quests.asJson,
    "recipes"   -> q.recipes.asJson,
    "strayGems" -> q.strayGems.asJson
  )

  /** Декодер рукописный, с запасными значениями: новое поле не должно ронять
    * разбор уже сохранённых заданий (см. память о производных декодерах). */
  implicit val decoder: Decoder[NpcQuests] = (c: HCursor) =>
    for {
      quests    <- c.getOrElse[Map[String, NpcQuestProgress]]("quests")(Map.empty)
      recipes   <- c.getOrElse[List[String]]("recipes")(Nil)
      strayGems <- c.getOrElse[List[Gem]]("strayGems")(Nil)
    } yield NpcQuests(quests, recipes, strayGems)
}
