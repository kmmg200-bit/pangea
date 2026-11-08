package pangea.model.top

import pangea.model.hero.HeroId
import pangea.model.monster.Race
import pangea.model.user.{UserId, VkId}

/** Герой в топе: имя (если выбрано), раса, уровень и то, по чему топ
  * считается (`value`). `vkId` — для ссылки на страницу игрока в ВК. */
final case class TopRow(
  heroId: HeroId,
  userId: UserId,
  vkId:   VkId,
  name:   Option[String],
  race:   Race,
  lvl:    Long,
  value:  Long
)

/** Снимок топа: первые герои и место каждого героя — всё из одного порядка.
  *
  * @param rows   первые герои топа, по порядку
  * @param places место каждого героя; у того, кого в топе нет (админ), — то,
  *               которое он занял бы: свой герой ему виден всё равно
  * @param total  сколько героев в топе
  */
final case class TopBoard(rows: List[TopRow], places: Map[HeroId, Long], total: Long)

object TopBoard {

  /** Снимок из порядка всех героев (номер и в топе ли он) и строк первых
    * `size` из них. Строки идут порядком `order`; чьей строки нет (героя
    * успели удалить между запросами), того в первых и нет. */
  def build(order: Vector[(HeroId, Boolean)], details: List[TopRow], size: Int): TopBoard = {
    val (places, total) = order.foldLeft((Map.empty[HeroId, Long], 0L)) {
      case ((acc, ranked), (id, true))  => (acc + (id -> (ranked + 1L)), ranked + 1L)
      case ((acc, ranked), (id, false)) => (acc + (id -> (ranked + 1L)), ranked)
    }
    val byId = details.map(r => r.heroId -> r).toMap
    TopBoard(firsts(order, size).flatMap(byId.get), places, total)
  }

  /** Первые `size` героев топа — чьи строки нужны снимку. */
  def firsts(order: Vector[(HeroId, Boolean)], size: Int): List[HeroId] =
    order.iterator.collect { case (id, true) => id }.take(size).toList
}
