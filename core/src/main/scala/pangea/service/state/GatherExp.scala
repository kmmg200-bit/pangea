package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.user.User
import zio.{Task, ZIO}

/** Опыт за собирательство в лабиринте: сорванный цветок, выбранная до дна
  * серебряная жила, раскопанный схрон. Платят долей от уровня лабиринта —
  * здесь она в процентах (0,3 — это 30): обычное округление, но не меньше
  * единицы, чтобы и на первых уровнях что-то капало. */
object GatherExp {

  /** Цветок с поляны — каждый, и узнанный, и «странный». */
  val FlowerPct: Long = 30L

  /** Серебряная жила, выбранная до конца. */
  val VeinPct: Long = 100L

  /** Схрон, раскопанный до конца, — что бы в нём ни оказалось. */
  val StashPct: Long = 80L

  /** Сколько опыта за находку на этом уровне лабиринта, до прибавки «выспался». */
  def amount(dungeonLevel: Int, pct: Long): Long =
    ((dungeonLevel.toLong * pct + 50L) / 100L).max(1L)

  /** Начислить опыт за находку — с прибавкой «выспался» и прокачкой уровня.
    * Возвращает строку для игрока: «+N опыта» и, если случился, новый уровень.
    * Опыт остановлен — пустая строка: говорить не о чем. */
  def grant(heroDao: HeroDao, content: SceneContent, user: User, hero: Hero, pct: Long,
            nowMs: Long): Task[String] = {
    val got = hero.expFrom(amount(hero.dungeonLevel, pct), nowMs)
    val up  = hero.gainExp(got)
    if (got <= 0L) ZIO.succeed("")
    else heroDao.updateExpAndLevel(user.userId, up.exp, up.lvl, up.upgradePoints).as(
      content.format("gather.exp", "exp" -> got.toString) +
        (if (up.lvl > hero.lvl) "\n" + content.format("gather.levelUp", "level" -> up.lvl.toString) else ""))
  }
}
