package pangea.model.top

import enumeratum._

/** Какие бывают топы в Mini App. `entryName` — ключ в запросе приложения
  * (`/api/app/top?kind=…`); в базе не хранится. Чем каждый топ меряет героя,
  * считает [[pangea.dao.top.TopDao]]: топы по достижениям и рецептам видят
  * только число, а не то, что за ним. */
sealed trait TopKind extends EnumEntry

object TopKind extends Enum[TopKind] {
  val values: IndexedSeq[TopKind] = findValues

  /** По уровню; при равенстве — опыт, когда встал на уровень, номер героя. */
  case object Level extends TopKind {
    override val entryName: String = "level"
  }

  /** По самому глубокому достигнутому уровню лабиринта. */
  case object Depth extends TopKind {
    override val entryName: String = "depth"
  }

  /** По тому, сколько врагов герой убил за всю жизнь. */
  case object Kills extends TopKind {
    override val entryName: String = "kills"
  }

  /** По тому, сколько у героя достижений. */
  case object Achievements extends TopKind {
    override val entryName: String = "achievements"
  }

  /** Алхимик: сколько разных отваров герой сварил — в котелке, в кубе Азата
    * или в Живой сумке. */
  case object Alchemy extends TopKind {
    override val entryName: String = "alchemy"
  }

  /** По записям в книге рецептов библиотеки — что получалось в кубе Азата на
    * верстаке ремесленника; стандартные отвары не в счёт, а то, что варит только
    * куб, — в счёт (см. `HomeData.recipeBook`). */
  case object Recipes extends TopKind {
    override val entryName: String = "recipes"
  }
}
