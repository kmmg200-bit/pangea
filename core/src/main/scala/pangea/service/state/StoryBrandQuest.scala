package pangea.service.state

import pangea.engine.{Choice, ChoiceColor, SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.item.{Item, MaterialKind}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.repository.inventory.InventoryRepository
import zio.{Task, ZIO}

/** Глава 3 основной линии, «Что горит на груди» ([[NpcQuest.StoryBrand]], см.
  * [[StoryLine]]). Ведёт её набросок узора из бумаг Смотрителя — Клеймо героя
  * и приписка «брать живым». Казимир читает узоры и шлёт к жрецу; жрец
  * называет таких, как герой, отмеченными и шлёт к Юноне — совет записывает
  * всех, кто попадает в Кинэт не по-людски; Юнона называет Сойку — второго
  * отмеченного в городе. Встреча с ней — с карточки наброска, из города
  * (StoryMeetingState): она отдаёт следующую зацепку, страницу из списка
  * отмеченных, где стоит и новое имя героя.
  *
  * Даром не говорит никто, кроме Сойки: Казимир, жрец и Юнона сперва называют
  * цену разговора ([[price]]) и рассказывают, только когда её отдали.
  *
  * Шаги: 1 — Казимир (глава берётся у него, когда он называет цену); 2 —
  * жрец; 3 — Юнона; 4 — встреча с Сойкой. Закрывается, когда страница у
  * героя: набросок уходит. */
object StoryBrandQuest {

  /** Уровень задания — его сложность на карточке наброска. */
  val Level: Int = 13

  /** Сколько чего просят за разговор. */
  final case class Price(kind: MaterialKind, count: Int)

  /** Казимир: огонь, который не гаснет, и камень, который держит форму. */
  val KazimirPrice: List[Price] = List(Price(MaterialKind.EverburningIron, 2), Price(MaterialKind.MagicStone, 2))

  /** Жрец: проклятые кости — упокоить в храме. */
  val PriestPrice: List[Price] = List(Price(MaterialKind.CursedBones, 2))

  /** Юнона: пошлина совета за выписку из книг. */
  val JunonaPrice: List[Price] = List(Price(MaterialKind.WhiteWolfHide, 2))

  /** Цена разговора на шаге: 1 — Казимир, 2 — жрец, 3 — Юнона. Сойка (4) не
    * просит ничего. */
  def price(step: Int): List[Price] = step match {
    case 1 => KazimirPrice
    case 2 => PriestPrice
    case 3 => JunonaPrice
    case _ => Nil
  }

  def toKazimir(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryBrand, 1)
  def toPriest(q: NpcQuests): Boolean  = q.onStep(NpcQuest.StoryBrand, 2)
  def toJunona(q: NpcQuests): Boolean  = q.onStep(NpcQuest.StoryBrand, 3)
  def toMeeting(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryBrand, 4)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryBrand)(_.copy(step = n))

  /** Тот, к кому ведёт шаг, уже назвал свою цену: `counter` хранит шаг, на
    * котором её назвали. Следующий шаг — следующий собеседник, и его цену ещё
    * не называли. */
  def priced(q: NpcQuests): Boolean = {
    val p = q.of(NpcQuest.StoryBrand)
    p.taken && price(p.step).nonEmpty && p.counter == p.step.toLong
  }

  /** Цена текущего шага названа. */
  def namePrice(q: NpcQuests): NpcQuests = q.update(NpcQuest.StoryBrand)(p => p.copy(counter = p.step.toLong))

  /** Чем из сумки заплатить: материалы россыпью — из стопки их не отдать (см.
    * `Bundling`), — ровно сколько просят. Не хватает хоть чего-то — None. */
  def payment(items: List[Item], price: List[Price]): Option[List[Item]] = {
    val picked = price.map(p => p -> items.filter(_.material.contains(p.kind)).take(p.count))
    Option.when(picked.forall { case (p, got) => got.sizeIs == p.count })(picked.flatMap(_._2))
  }

  /** Плата при герое — можно отдавать. */
  def canPay(inventoryRepo: InventoryRepository, hero: Hero, price: List[Price]): Task[Boolean] =
    bag(inventoryRepo, hero).map(payment(_, price).isDefined)

  /** Забрать плату из сумки. Не хватает — ничего не трогает и отвечает false. */
  def pay(inventoryRepo: InventoryRepository, hero: Hero, price: List[Price]): Task[Boolean] =
    bag(inventoryRepo, hero).flatMap { items =>
      payment(items, price) match {
        case Some(paid) =>
          inventoryRepo.removeItems(paid.map(_.id).toSet, hero.id).mapError(e => new Throwable(e.toString)).as(true)
        case None => ZIO.succeed(false)
      }
    }

  private def bag(inventoryRepo: InventoryRepository, hero: Hero): Task[List[Item]] =
    inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).map(_.items.data)

  /** Экран цены: собеседник называет её (`text`). Плата при герое — кнопка
    * «Отдать» (`payId`, подпись по ключу `payLabel`); и «Назад» — к нему в меню. */
  def askScreen(content: SceneContent, text: String, payId: String, payLabel: String, canPay: Boolean, back: Choice): Screen =
    Screen(text, Option.when(canPay)(content.choice(payId, payLabel).copy(color = ChoiceColor.Positive, row = Some(0))).toList :+
      back.copy(row = Some(1)))

  /** Строка карточки наброска — что делать сейчас; цена названа — с ценой.
    * Глава 2 не закрыта или 3 уже закрыта — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryPowder) || q.isDone(NpcQuest.StoryBrand)) ""
    else if (!q.isTaken(NpcQuest.StoryBrand)) content.text("story.storyBrand.card.ready")
    else content.list(if (priced(q)) "story.storyBrand.card.priced" else "story.storyBrand.card.steps")
      .lift(q.of(NpcQuest.StoryBrand).step - 1).getOrElse("")
}
