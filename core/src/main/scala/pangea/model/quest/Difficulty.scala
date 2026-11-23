package pangea.model.quest

/** Текст сложности задания для игрока. */
object Difficulty {

  /** Ниже единицы показываем первый уровень. */
  def render(n: Int): String = s"сложность ${n.max(1)} ур."
}