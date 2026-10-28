package pangea.model.hero

/** Имя героя: от [[MinLength]] до [[MaxLength]] символов, русские или
  * латинские буквы, слова — через одиночный пробел или дефис («Ли Вэй»,
  * «Анна-Мария»). Внутри слова алфавит один: «Аdmin» с латинской «A» не
  * пройдёт. Имена жителей Пангеи и служебные слова заняты навсегда
  * ([[Reserved]]). Уникальность среди героев проверяет база. */
object HeroName {

  val MinLength: Int = 3
  val MaxLength: Int = 16

  /** Почему имя не подходит. */
  sealed trait Problem
  object Problem {
    /** Короче [[MinLength]] или длиннее [[MaxLength]]. */
    case object Length extends Problem
    /** Не те символы: цифры, эмодзи, смесь алфавитов в слове, два пробела подряд. */
    case object Letters extends Problem
    /** Имя жителя Пангеи или служебное слово. */
    case object Reserved extends Problem
  }

  /** Слова, которых не должно быть в имени: жители Пангеи и служебные. Сравнение —
    * по слову целиком, без регистра, «ё» как «е». */
  val Reserved: Set[String] = Set(
    // жители Пангеи
    "фет", "ришелье", "густаво", "рахадим", "юнона", "кинтслев", "казимир", "гуннар",
    "брамбл", "медноус", "мариса", "кельвин", "горн", "азат", "джо", "долорес", "моньо",
    "торбальд", "йорген", "кинэт",
    // служебные
    "админ", "администратор", "модератор", "поддержка", "техподдержка", "бот", "система",
    "пангея", "admin", "administrator", "moderator", "support", "bot", "system", "pangea"
  ).map(fold)

  private val Word: String     = "(?:[А-Яа-яЁё]+|[A-Za-z]+)"
  private val Pattern: String  = s"$Word(?:[ -]$Word)*"

  /** Обрезать края и схлопнуть пробелы: «  Ли   Вэй » → «Ли Вэй». */
  def normalize(raw: String): String = raw.trim.split("\\s+").mkString(" ")

  /** Проверить ввод игрока. `Right` — имя в том виде, в каком его запишем. */
  def validate(raw: String): Either[Problem, String] = {
    val name = normalize(raw)
    if (name.length < MinLength || name.length > MaxLength) Left(Problem.Length)
    else if (!name.matches(Pattern)) Left(Problem.Letters)
    else if (name.split("[ -]").exists(w => Reserved.contains(fold(w)))) Left(Problem.Reserved)
    else Right(name)
  }

  private def fold(word: String): String = word.toLowerCase.replace('ё', 'е')
}
