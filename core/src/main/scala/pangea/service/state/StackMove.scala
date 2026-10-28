package pangea.service.state

import pangea.engine.{Choice, ChoiceColor, SceneContent}
import pangea.model.item.{Item, ItemStack}
import zio.{Task, ZIO}

/** Стопка одинаковых вещей целиком (см. [[ItemStack]]).
  *
  * Списки хранилищ двигают за нажатие одну вещь. Взял или положил одну из
  * стопки — рядом с ней, в том же ряду, встаёт «⏬ все N»: остаток стопки
  * одним нажатием. Переносят по одной, пока принимает другая сторона, — и тем
  * же порядком выгребают хранилище целиком («⏬ Забрать всё»).
  */
object StackMove {

  /** Сколько вещей перенесли из скольких. `refusal` — почему встали, если
    * встали раньше конца: сумка полна, хранилище забито, предел пыли. */
  final case class Moved(items: List[Item], total: Int, refusal: Option[String]) {
    def count: Int = items.size
    def rest: Int  = total - count
  }

  /** Вещи той же стопки, что `item`, в порядке списка; вещь без стопки — одна. */
  def same(items: List[Item], item: Item): List[Item] =
    ItemStack.key(item).fold(List(item))(k => items.filter(i => ItemStack.key(i).contains(k)))

  /** Стопка `item` в том порядке, в каком её переносят, — с конца, как и по
    * одной (см. [[ItemStack.grouped]]): влезла не вся — остаток держит место
    * стопки в списке. */
  def stack(items: List[Item], item: Item): List[Item] = same(items, item).reverse

  /** Перенести `items` по одной. `move` отвечает, легла ли вещь: `None` —
    * легла, `Some(причина)` — нет. Одинаковые вещи упираются в одно и то же,
    * поэтому стопку первая неудача останавливает. Хранилище целиком (`stopAtFirst
    * = false`) идёт дальше: не влезла одна — другая, может, и влезет. */
  def run(items: List[Item], stopAtFirst: Boolean = true)(move: Item => Task[Option[String]]): Task[Moved] =
    ZIO.foldLeft(items)((Vector.empty[Item], Option.empty[String])) { case ((done, why), it) =>
      if (stopAtFirst && why.isDefined) ZIO.succeed((done, why))
      else move(it).map {
        case None         => (done :+ it, why)
        case Some(reason) => (done, why.orElse(Some(reason)))
      }
    }.map { case (done, why) => Moved(done.toList, items.size, why.filter(_ => done.size < items.size)) }

  /** Что перенесли — одной строкой: «Аметист ×3, Живая вода». */
  def names(items: List[Item]): String =
    ItemStack.grouped(items).map { case (it, n) => if (n > 1) s"${it.name} ×$n" else it.name }.mkString(", ")

  /** Итог переноса для заголовка списка: `doneKey` с перечнем, а не влезло что-то
    * — и сколько. Не перенесли ничего — только причина. */
  def summary(content: SceneContent, moved: Moved, doneKey: String, extra: (String, String)*): String =
    if (moved.count == 0) moved.refusal.getOrElse(content.format("stack.rest", "n" -> moved.total.toString))
    else {
      val done = content.format(doneKey, (("items" -> names(moved.items)) +: extra): _*)
      if (moved.rest > 0) s"$done ${content.format("stack.rest", "n" -> moved.rest.toString)}" else done
    }

  /** «⏬ все N» в ряду стопки `hot` — из неё только что взяли или положили одну.
    * Остаток в одну вещь своей кнопки не стоит: его и так берут одним нажатием.
    * Ряды — как у [[ItemMenu.stackButtons]]: стопка на странице `idx` стоит в ряду `idx`. */
  def allButton(page: List[(Item, Int)], hot: Option[String], prefix: String, content: SceneContent): List[Choice] =
    page.zipWithIndex.collect {
      case ((it, n), idx) if n >= 2 && hot.isDefined && ItemStack.key(it) == hot =>
        Choice(s"$prefix${it.id}", content.format("stack.all", "n" -> n.toString),
          color = ChoiceColor.Positive, row = Some(idx))
    }

  /** Стопка, из которой взяли одну, — для [[allButton]]. */
  def hotOf(item: Item): Option[String] = ItemStack.key(item)

  /** Строка над списком: что только что случилось — и сам заголовок. */
  def withNote(note: Option[String], header: String): String =
    note.fold(header)(n => s"$n\n\n$header")
}
