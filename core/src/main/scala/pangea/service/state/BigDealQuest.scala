package pangea.service.state

import pangea.engine.SceneContent
import pangea.model.item.{BrewKind, Item}
import pangea.model.quest.{NpcQuest, NpcQuests}

/** «Большое дело» ([[NpcQuest.BigDeal]]) — вторая часть линии Барти:
  * контрабанда с каджитским караваном. Ведёт адрес Барти из «Ходового товара»:
  * с карточки — к нему домой, на Торговую площадь. Барти затевает продать
  * каджитам живую воду и шнапс — «платят не серебром, золотом» — и просит
  * [[Goal]] бутылок шнапса и столько же склянок живой воды; носить можно хоть
  * по одной, он складывает у себя ([[Stock]]).
  *
  * Шаги: 1 — товар собирается у Барти; 2 — всё собрано, ждать каравана.
  * Каравана пока нет — Барти ждёт его у окна. Враги, сложность и награда — по
  * уровню задания. */
object BigDealQuest {

  /** Уровень задания: по нему сложность (и, с караваном, стража и плата). */
  val Level: Int = 11

  /** Сколько бутылок шнапса и склянок живой воды просит Барти — каждого. */
  val Goal: Int = 20

  /** Что Барти принимает: шнапс и живую воду. */
  val Goods: List[BrewKind] = List(BrewKind.Schnapps, BrewKind.LivingWater)

  /** Сколько у Барти уже сложено. Лежит в `counter` задания одним числом:
    * шнапс — сотни, живая вода — единицы (`1207` — 12 шнапса и 7 живой воды);
    * больше [[Goal]] каждого он не берёт. */
  final case class Stock(schnapps: Int, water: Int) {
    def full: Boolean   = schnapps >= Goal && water >= Goal
    def counter: Long   = schnapps.toLong * 100L + water.toLong
    def of(kind: BrewKind): Int = if (kind == BrewKind.Schnapps) schnapps else if (kind == BrewKind.LivingWater) water else 0

    /** Сложить `n` склянок этого вида — не больше, чем ещё нужно. */
    def plus(kind: BrewKind, n: Int): Stock =
      if (kind == BrewKind.Schnapps) copy(schnapps = (schnapps + n).min(Goal))
      else if (kind == BrewKind.LivingWater) copy(water = (water + n).min(Goal))
      else this
  }

  object Stock {
    val empty: Stock = Stock(0, 0)

    def fromCounter(counter: Long): Stock =
      Stock(((counter / 100L) % 100L).toInt.min(Goal), (counter % 100L).toInt.min(Goal))
  }

  def stock(q: NpcQuests): Stock = Stock.fromCounter(q.of(NpcQuest.BigDeal).counter)

  def withStock(q: NpcQuests, s: Stock): NpcQuests = q.update(NpcQuest.BigDeal)(_.copy(counter = s.counter))

  def collecting(q: NpcQuests): Boolean = q.onStep(NpcQuest.BigDeal, 1)
  def waiting(q: NpcQuests): Boolean    = q.onStep(NpcQuest.BigDeal, 2)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.BigDeal)(_.copy(step = n))

  /** Задание можно начать: «Ходовой товар» закрыт, «Большого дела» ещё не было. */
  def canBegin(q: NpcQuests): Boolean =
    q.isDone(NpcQuest.HotGoods) && !q.isTaken(NpcQuest.BigDeal) && !q.isDone(NpcQuest.BigDeal)

  /** С карточки адреса — к Барти домой: задание можно начать или оно в работе. */
  def houseOpen(q: NpcQuests): Boolean = canBegin(q) || q.isTaken(NpcQuest.BigDeal)

  /** Что из сумки Барти возьмёт сейчас: склянки по одной (не из стопок) — не
    * больше, чем ему ещё нужно каждого. */
  def takeable(items: List[Item], s: Stock): List[Item] =
    Goods.flatMap(k => items.filter(_.brew.contains(k)).take((Goal - s.of(k)).max(0)))

  /** Строка карточки адреса — что делать сейчас. «Ходовой товар» не закрыт
    * или «Большое дело» уже закрыто — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.HotGoods) || q.isDone(NpcQuest.BigDeal)) ""
    else if (!q.isTaken(NpcQuest.BigDeal)) content.text("barti.card.ready")
    else content.list("barti.card.steps").lift(q.of(NpcQuest.BigDeal).step - 1).getOrElse("")
}
