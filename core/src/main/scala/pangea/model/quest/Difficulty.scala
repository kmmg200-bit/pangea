package pangea.model.quest

/** Сложность задания — знаки (как римское число) и число уровня в скобках. */
object Difficulty {
  val Bone: String     = "🦴" // 1
  val Skull: String    = "💀" // 5
  val Coffin: String   = "⚰️" // 10
  val Cemetery: String = "🪦" // 50
  val Darkness: String = "🌑" // 100

  private val Digits: List[(Int, String)] = List(
    100 -> Darkness,
    90  -> (Coffin + Darkness),
    50  -> Cemetery,
    40  -> (Coffin + Cemetery),
    10  -> Coffin,
    9   -> (Bone + Coffin),
    5   -> Skull,
    4   -> (Bone + Skull),
    1   -> Bone
  )

  private def glyphs(n: Int): String = {
    @annotation.tailrec
    def go(left: Int, digits: List[(Int, String)], acc: StringBuilder): String =
      digits match {
        case Nil                      => acc.toString
        case (v, s) :: _ if left >= v => go(left - v, digits, acc.append(s))
        case _ :: rest                => go(left, rest, acc)
      }
    go(n, Digits, new StringBuilder)
  }

  /** Одна строка для игрока; ниже единицы — первая ступень. */
  def render(n: Int): String = {
    val lvl = n.max(1)
    s"Сложность: ${glyphs(lvl)} ($lvl ур.)"
  }
}