package pangea.model.home

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.hero.Equipment
import pangea.model.item.{Item, Rarity}
import pangea.model.monster.NamedTrophy

/** Дом героя — один из трёх, по ступеням. Продаёт их Юнона Кинтслев,
  * представитель городского совета. Дом выше покупается с зачётом того, что
  * есть: старый уходит обратно в управление совета, и его цена в дублонах
  * идёт в счёт нового. Продать или потерять дом иначе нельзя — только
  * рестартом. Что было в доме ниже, есть и в доме выше: хозяйство переезжает
  * вместе с героем.
  *
  * Числа — прямо в вариантах: вариант не трогает свой companion (иначе при
  * загрузке классов можно поймать взаимную блокировку). */
sealed abstract class House(
  val tier: Int,
  /** Ключ в `scenes.yaml`: `home.houses.<key>.*`. */
  val key: String,
  /** Цена в дублонах; её же совет засчитывает, когда дом сдают в зачёт следующего. */
  val doubloons: Long,
  /** Сколько спать на своей кровати, чтобы травмы прошли и силы вернулись. */
  val sleepMs: Long,
  /** Сколько после сна держится «выспался» — прибавка к опыту (см. [[HomeRates.RestedExpPct]]). */
  val restedMs: Long,
  /** Потолок энергии личного алтаря Азата; 0 — алтаря в доме нет. */
  val altarCharges: Int,
  /** Сколько мест под обстановку (см. [[Furniture]]): встроенное — кровать,
    * алтари, верстак, хранилище — их не занимает. */
  val slots: Int,
  /** Сколько магических стоек можно поставить ([[Furniture.Rack]]). */
  val racks: Int
)

object House {

  /** Домик в две комнаты: всё нужное и ничего сверх. Сон — 2 часа, «выспался» — 4.
    * Два места под обстановку, одно из них поначалу завалено хламом
    * ([[HomeData.hasClutter]]). */
  case object Cottage extends House(1, "cottage", 100L, 2L * 3600000L, 4L * 3600000L, 0, 2, 1)

  /** Добротный дом: свой алтарь Азата на 100 энергии, верстак травника,
    * библиотека. Сон — 1,5 часа, «выспался» — 8. */
  case object Townhouse extends House(2, "townhouse", 800L, 5400000L, 8L * 3600000L, 100, 4, 2)

  /** Дом на парящем острове, куда ведёт ключ-портал; алтарь Азата — на 300
    * энергии, полки верстака шире ([[HomeRates.IslandHerbShelves]]), десять мест
    * под обстановку. Сон — 1 час, «выспался» — 12. */
  case object Island extends House(3, "island", 2000L, 3600000L, 12L * 3600000L, 300, 10, 4)

  val values: List[House] = List(Cottage, Townhouse, Island)

  def byTier(tier: Int): Option[House] = values.find(_.tier == tier)

  def byKey(key: String): Option[House] = values.find(_.key == key)

  /** Что можно купить, когда есть `current`: только дома выше него. */
  def forSale(current: Option[House]): List[House] = values.filter(h => current.forall(_.tier < h.tier))

  /** Цена дома в дублонах с зачётом того, что сдают в управление. */
  def doubloonPrice(target: House, current: Option[House]): Long =
    target.doubloons - current.fold(0L)(_.doubloons)

  /** За серебро продаётся только домик — и только тому, у кого дома ещё нет. */
  def silverPrice(target: House, current: Option[House]): Option[Long] =
    Option.when(target == Cottage && current.isEmpty)(HomeRates.CottageSilver)
}

/** Обстановка дома: покупается у Юноны в «Обустройстве» за дублоны и
  * занимает в доме одно место ([[House.slots]]). При переезде едет вместе с
  * героем. `minTier` — с какого дома её продают.
  *
  * Числа — прямо в вариантах (см. заметку у [[House]]). */
sealed abstract class Furniture(
  /** Ключ: в `HomeData.furniture` и в `scenes.yaml` (`home.furniture.<key>.*`). */
  val key: String,
  val doubloons: Long,
  val minTier: Int
)

object Furniture {

  /** Магическая стойка: держит целый комплект снаряжения, и одной кнопкой (или
    * ключом из сумки — где угодно, кроме боя) надетое меняется местами с тем, что
    * висит на ней. Стоек в доме — не больше [[House.racks]]; к каждой — свой
    * номерной ключ. */
  case object Rack extends Furniture("rack", 100L, 1)

  /** Верстак ремесленника с гнездом под куб Азата: вставил куб — он переехал из
    * Храма домой; работает так же, а всё, что в нём получится, пишется в книгу
    * рецептов ([[HomeData.recipes]]). В доме — один: куб-то один. */
  case object ArtisanBench extends Furniture("artisan", 100L, 1)

  /** Личный посыльный: серебро из ячейки Торгового дома он передаст по
    * «Передать» откуда угодно — из лабиринта, с поисков клада, посреди задания.
    * Серебро на руках вне города он не трогает: носит только из ячейки.
    * Покупка разовая: второго не продают, а обстановка из дома не уходит и
    * переезжает вместе с героем. */
  case object Courier extends Furniture("courier", 100L, 1)

  /** Сундук аптекаря — у верстака травника: полки шире на
    * [[HomeRates.ApothecaryHerbs]] трав и [[HomeRates.ApothecaryBrews]] отваров. */
  case object ApothecaryChest extends Furniture("apothecary", 100L, 2)

  /** Роскошный сундук ювелира — в хранилище камней: шире на [[HomeRates.JewelerGems]]. */
  case object JewelerChest extends Furniture("jeweler", 100L, 3)

  val values: List[Furniture] = List(Rack, ArtisanBench, Courier, ApothecaryChest, JewelerChest)

  def byKey(key: String): Option[Furniture] = values.find(_.key == key)
}

/** Числа дома, которые не держит сам [[House]]. */
object HomeRates {

  /** Домик за серебро. */
  val CottageSilver: Long = 20000L

  /** «Выспался дома»: на столько процентов больше опыта из любого источника. */
  val RestedExpPct: Long = 10L

  /** Имя бафа «выспался» среди временных бафов героя (`Hero.statBoosts`). */
  val RestedBoost: String = "home:rested"

  /** Верстак травника: сколько трав держат полки — всего, не по видам. */
  val HerbShelves: Int = 50

  /** Полка готовых отваров. */
  val BrewShelves: Int = 25

  /** Котелок: мест — как в кубе Азата. */
  val CauldronSlots: Int = 9

  /** Хранилище камней на острове. */
  val GemVault: Int = 50

  /** На острове полки верстака шире по умолчанию: трав и отваров сверху. */
  val IslandHerbShelves: Int = 100
  val IslandBrewShelves: Int = 50

  /** Сундук аптекаря: трав и отваров сверху — за каждый. */
  val ApothecaryHerbs: Int = 100
  val ApothecaryBrews: Int = 50

  /** Роскошный сундук ювелира: камней сверху — за каждый. */
  val JewelerGems: Int = 100

  /** Хлам прежнего хозяина домика: разобрать самому — 4 часа (с перерывами,
    * наубиранное не пропадает), вывезти через Юнону — за серебро. */
  val ClutterCleanMs: Long = 4L * 3600000L
  val ClutterSilver: Long  = 5000L

  /** Разобрал хлам сам — в нём находка: случайная вещь этого уровня и
    * редкости (пурпурной). Вывез через Юнону — находки нет. */
  val ClutterFindLvl: Long         = 4L
  val ClutterFindRarity: Rarity    = Rarity.Violet

  /** Пополнение алтаря: столько энергии за столько серебра — как в храме. */
  val AltarRefillCharges: Int = 50
  val AltarRefillSilver: Long = 10000L
}

/** Всё о доме героя, что хранится в `heroes.home_data`: `tier` — ступень дома
  * (0 — дома нет), `metJunona` — была ли первая встреча: листок на доске у
  * ратуши и знакомство с Юноной; `runes` — рунический алтарь: сколько узоров
  * лежит по каждой руне (ключ руны → число в малых рунах, большая — пять).
  *
  * Со второго дома: `herbs` — травы на полках верстака, `brews` — полка готовых
  * отваров, `cauldron` — что лежит в котелке, `charges` — энергия дома от
  * личного алтаря Азата (её тратят котелок и хранилище камней). На острове —
  * `gems`: хранилище камней.
  *
  * Обстановка: `furniture` — купленное у Юноны, по месту на вещь (ключи
  * [[Furniture]]); `clutterCleared` — хлам в домике разобран или вывезен;
  * `cleanedMs` — сколько уже наубирано руками (уборку можно прервать);
  * `racks` — что висит на магических стойках: стойка №N — `racks(N - 1)`;
  * `recipes` — книга рецептов: что получилось в кубе на верстаке ремесленника
  * (видна в библиотеке, когда она есть, а пишется и без неё).
  *
  * На острове — `wall`: Стена трофеев, чьи уникальные трофеи на ней висят
  * (имена именных мобов, без повторов; см. [[NamedTrophy]]).
  *
  * `stored` — кладовая: спрятанная обстановка, ключи [[Furniture]] в том
  * порядке, в каком её прятали. Места она не занимает и не работает; вернуть
  * — когда в доме есть место, а при переезде в дом просторнее она встаёт сама
  * ([[unpacked]]). Стойки в кладовой — всегда старшие по номеру: ключи от
  * младших работают как прежде. */
final case class HomeData(
  tier:      Int               = 0,
  metJunona: Boolean           = false,
  runes:     Map[String, Long] = Map.empty,
  herbs:     List[Item]        = Nil,
  brews:     List[Item]        = Nil,
  cauldron:  List[Item]        = Nil,
  charges:   Int               = 0,
  gems:      List[Item]        = Nil,
  furniture: List[String]      = Nil,
  clutterCleared: Boolean      = false,
  cleanedMs: Long              = 0L,
  racks:     List[Equipment]   = Nil,
  recipes:   List[RecipeNote]  = Nil,
  wall:      List[String]      = Nil,
  stored:    List[String]      = Nil
) {
  def house: Option[House] = House.byTier(tier)

  /** Хлам прежнего хозяина: есть только в домике, пока его не разобрали. При
    * переезде остаётся в домике — новый дом чистый. */
  def hasClutter: Boolean = house.contains(House.Cottage) && !clutterCleared

  /** Мест под обстановку всего, занято (хлам тоже занимает) и свободно. */
  def slotsMax: Int  = house.fold(0)(_.slots)
  def slotsUsed: Int = furniture.size + (if (hasClutter) 1 else 0)
  def slotsFree: Int = (slotsMax - slotsUsed).max(0)

  /** Сколько в доме такой обстановки. */
  def count(f: Furniture): Int = furniture.count(_ == f.key)

  /** Сколько такой обстановки у героя всего — в доме и в кладовой. */
  def owned(f: Furniture): Int = count(f) + stored.count(_ == f.key)

  /** Стоек в кладовой — это старшие по номеру. */
  def storedRacks: Int = stored.count(_ == Furniture.Rack.key)

  /** Стойки, что стоят в доме: №1..activeRacks. */
  def activeRacks: Int = (racks.size - storedRacks).max(0)

  /** Убрать в кладовую одну такую вещь; стойку — старшую из стоящих. */
  def hidden(f: Furniture): HomeData = {
    val i = furniture.lastIndexOf(f.key)
    if (i < 0) this else copy(furniture = furniture.patch(i, Nil, 1), stored = stored :+ f.key)
  }

  /** Вернуть из кладовой в дом одну такую вещь. */
  def returned(f: Furniture): HomeData = {
    val i = stored.indexOf(f.key)
    if (i < 0) this else copy(stored = stored.patch(i, Nil, 1), furniture = furniture :+ f.key)
  }

  /** Переезд в дом просторнее: из кладовой встаёт то, что спрятано первым, —
    * сколько хватит мест. */
  @scala.annotation.tailrec
  final def unpacked: HomeData =
    stored.headOption.flatMap(Furniture.byKey) match {
      case Some(f) if slotsFree > 0 => returned(f).unpacked
      case _                        => this
    }

  /** Есть ли в доме личный посыльный ([[Furniture.Courier]]): спрятанный в
    * кладовую не работает. */
  def hasCourier: Boolean = count(Furniture.Courier) > 0

  /** Сколько стоек можно поставить в этот дом. */
  def racksMax: Int = house.fold(0)(_.racks)

  /** Книга рецептов с новыми записями — без повторов. */
  def noted(notes: List[RecipeNote]): HomeData = copy(recipes = (recipes ++ notes).distinct)

  private def island: Boolean = house.contains(House.Island)

  /** Полки верстака: трав и отваров — с прибавкой острова и сундуков аптекаря. */
  def herbShelves: Int = HomeRates.HerbShelves + (if (island) HomeRates.IslandHerbShelves else 0) +
    count(Furniture.ApothecaryChest) * HomeRates.ApothecaryHerbs
  def brewShelves: Int = HomeRates.BrewShelves + (if (island) HomeRates.IslandBrewShelves else 0) +
    count(Furniture.ApothecaryChest) * HomeRates.ApothecaryBrews

  /** Стена трофеев — встроенная, в доме на острове. */
  def hasWall: Boolean = island

  /** Прибавка Стены трофеев к урону по расам: entryName расы → проценты. Без
    * стены — ничего, что бы на ней ни числилось. */
  def wallPct: Map[String, Int] = if (hasWall) NamedTrophy.pctByRace(wall) else Map.empty

  /** Хранилище камней — с роскошными сундуками ювелира. */
  def gemShelves: Int = HomeRates.GemVault + count(Furniture.JewelerChest) * HomeRates.JewelerGems

  /** Потолок энергии дома — алтарь Азата этого дома (см. [[House.altarCharges]]). */
  def altarMax: Int = house.fold(0)(_.altarCharges)

  def owns: Boolean = house.isDefined

  /** Сколько узоров руны `key` на алтаре. */
  def runeCount(key: String): Long = runes.getOrElse(key, 0L)

  /** Записать, сколько узоров руны на алтаре; ноль — руна уходит из счёта. */
  def withRune(key: String, count: Long): HomeData =
    copy(runes = if (count > 0L) runes.updated(key, count) else runes - key)
}

/** Запись в книге рецептов: что положили в куб (названия, по вещи) и что
  * вышло (`count` — сколько штук). */
final case class RecipeNote(ingredients: List[String], result: String, count: Int) {

  /** «Ромашка + Календула + Крапива → Костоправный отвар ×2»: одинаковое —
    * числом, в порядке первого появления. */
  def line: String = {
    val parts = ingredients.distinct.map { name =>
      val n = ingredients.count(_ == name)
      if (n > 1) s"$name ×$n" else name
    }
    parts.mkString(" + ") + " → " + result + (if (count > 1) s" ×$count" else "")
  }
}

object RecipeNote {

  /** Как в книге записан камень для божественного оружия (см. CubeCraft). */
  val DivineGem: String = "Целый камень (3 тир и выше)"

  /** Божественное оружие раньше ковали из надколотых камней: старая запись в
    * книге читается уже по нынешнему рецепту, иначе книга врала бы. */
  private val OldDivineGem: String = "Надколотый камень"

  implicit val encoder: Encoder[RecipeNote] = deriveEncoder
  implicit val decoder: Decoder[RecipeNote] = (c: HCursor) =>
    for {
      ingredients <- c.getOrElse[List[String]]("ingredients")(Nil)
      result      <- c.getOrElse[String]("result")("")
      count       <- c.getOrElse[Int]("count")(1)
    } yield {
      val current =
        if (result == "Божественное оружие") ingredients.map(i => if (i == OldDivineGem) DivineGem else i)
        else ingredients
      RecipeNote(current, result, count)
    }
}

object HomeData {
  val empty: HomeData = HomeData()

  implicit val encoder: Encoder[HomeData] = deriveEncoder
  implicit val decoder: Decoder[HomeData] = (c: HCursor) =>
    for {
      tier  <- c.getOrElse[Int]("tier")(0)
      met   <- c.getOrElse[Boolean]("metJunona")(false)
      runes    <- c.getOrElse[Map[String, Long]]("runes")(Map.empty)
      herbs    <- c.getOrElse[List[Item]]("herbs")(Nil)
      brews    <- c.getOrElse[List[Item]]("brews")(Nil)
      cauldron <- c.getOrElse[List[Item]]("cauldron")(Nil)
      charges  <- c.getOrElse[Int]("charges")(0)
      gems     <- c.getOrElse[List[Item]]("gems")(Nil)
      furn     <- c.getOrElse[List[String]]("furniture")(Nil)
      cleared  <- c.getOrElse[Boolean]("clutterCleared")(false)
      cleaned  <- c.getOrElse[Long]("cleanedMs")(0L)
      racks    <- c.getOrElse[List[Equipment]]("racks")(Nil)
      recipes  <- c.getOrElse[List[RecipeNote]]("recipes")(Nil)
      wall     <- c.getOrElse[List[String]]("wall")(Nil)
      stored   <- c.getOrElse[List[String]]("stored")(Nil)
    } yield HomeData(tier, met, runes, herbs, brews, cauldron, charges, gems, furn, cleared, cleaned, racks, recipes,
      wall, stored)
}
