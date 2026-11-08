package pangea.model.top

import enumeratum._
import pangea.model.hero.Hero

/** Какие бывают топы в Mini App. `entryName` — ключ в запросе приложения
  * (`/api/app/top?kind=…`); в базе не хранится. */
sealed trait TopKind extends EnumEntry {

  /** Чем этот топ меряет героя — то же, что колонка в [[pangea.dao.top.TopDao]]. */
  def valueOf(hero: Hero): Long
}

object TopKind extends Enum[TopKind] {
  val values: IndexedSeq[TopKind] = findValues

  /** По уровню, внутри уровня — по опыту. */
  case object Level extends TopKind {
    override val entryName: String = "level"
    def valueOf(hero: Hero): Long  = hero.lvl
  }

  /** По самому глубокому достигнутому уровню лабиринта. */
  case object Depth extends TopKind {
    override val entryName: String = "depth"
    def valueOf(hero: Hero): Long  = hero.maxDungeonLevel.toLong
  }

  /** По тому, сколько врагов герой убил за всю жизнь. */
  case object Kills extends TopKind {
    override val entryName: String = "kills"
    def valueOf(hero: Hero): Long  = hero.kills
  }
}
