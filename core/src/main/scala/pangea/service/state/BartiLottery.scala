package pangea.service.state

import pangea.domain.Rng
import pangea.generator.item.{FlaskGenerator, GemGenerator, ItemGenerator, MaterialGenerator, TreasureMapGenerator}
import pangea.model.hero.LoreData
import pangea.model.item.{Gem, Item, ItemType, MaterialKind, Rarity}
import pangea.model.rune.{RuneStone, RuneStoneSize}

import scala.annotation.tailrec

/** Серебряная лотерея Барти — «мешок наудачу» в его доме (так решил
  * пользователь). Это его собственная лотерея: мешок конфиската за [[Price]]
  * серебра, и что внутри, Барти не знает сам — даже не подсматривает. В мешке
  * [[Size]] вещей, каждая — своим броском по [[Table]]: больше всего хлама,
  * цветов и ингредиентов, изредка — то, за что в лабиринте полжизни отдают.
  *
  * Вещи — уровня героя, с тем же разбросом, что с мобов; цветы — как на поляне:
  * незнакомую траву герой видит странным или необычным цветком. Больше
  * [[DailyLimit]] мешков за сутки по Москве Барти не продаёт: учёт — по журналу
  * событий (`GameEvent.BartiLottery`), каждый мешок — своя строка. */
object BartiLottery {

  /** Почём мешок — серебром (так решил пользователь). */
  val Price: Long = 5000L

  /** Сколько вещей в мешке. */
  val Size: Int = 10

  /** Сколько мешков в сутки (по Москве) Барти продаёт одному герою — так решил
    * пользователь. */
  val DailyLimit: Long = 10L

  /** Что может лежать в мешке. Один вид — одна строка таблицы. */
  sealed trait Prize

  object Prize {
    /** Экипировка этой редкости. */
    final case class Gear(rarity: Rarity) extends Prize
    /** Материал — один из `kinds`, поровну. */
    final case class Material(kinds: List[MaterialKind]) extends Prize
    /** Трава этого ранга (1 — простая, 2 — редкая) — какой её видит герой. */
    final case class Herb(rank: Int) extends Prize
    case object Flask     extends Prize
    case object Gem       extends Prize
    case object MapHalf   extends Prize
    case object SmallRune extends Prize
    case object BigRune   extends Prize
  }

  /** Ингредиенты минибоссов — по одному с каждого, явным списком: новый босс
    * сам в мешок не попадает. */
  val BossIngredients: List[MaterialKind] = List(
    MaterialKind.EverburningIron, MaterialKind.MagicStone, MaterialKind.StormHeart, MaterialKind.EternalIce,
    MaterialKind.GhoulSkin, MaterialKind.WhiteWolfHide, MaterialKind.RatKingBlood, MaterialKind.CursedBones)

  /** Что выпадает и с каким весом — на тысячу, в сумме тысяча. */
  val Table: List[(Prize, Int)] = List(
    // Хлам — 38%.
    Prize.Gear(Rarity.Gray)                          -> 130,
    Prize.Gear(Rarity.White)                         -> 100,
    Prize.Material(List(MaterialKind.RatPelt))       -> 50,
    Prize.Material(List(MaterialKind.RatTail))       -> 40,
    Prize.Material(List(MaterialKind.PlagueWorms))   -> 20,
    Prize.Material(List(MaterialKind.SpiderSilk))    -> 40,
    // Цветы — 28,5%: среди редких трав и нераскрытая роза.
    Prize.Herb(1)                                    -> 240,
    Prize.Herb(2)                                    -> 45,
    // Ингредиенты — 10,6%.
    Prize.Material(MaterialKind.dusts.toList)        -> 60,
    Prize.Material(List(MaterialKind.VenomGland))    -> 30,
    Prize.Material(BossIngredients)                  -> 12,
    Prize.Material(List(MaterialKind.RedSpiderGland)) -> 4,
    // Обычные вещи — 9,5%.
    Prize.Gear(Rarity.Green)                         -> 60,
    Prize.Gear(Rarity.Blue)                          -> 35,
    // То, ради чего покупают, — 13,4%.
    Prize.Gear(Rarity.Purple)                        -> 25,
    Prize.Gear(Rarity.Violet)                        -> 8,
    Prize.Gear(Rarity.Orange)                        -> 4,
    Prize.Flask                                      -> 20,
    Prize.Gem                                        -> 15,
    Prize.MapHalf                                    -> 8,
    Prize.SmallRune                                  -> 40,
    Prize.BigRune                                    -> 9,
    Prize.Material(List(MaterialKind.RuneChisel))    -> 5
  )

  val TotalWeight: Long = Table.map(_._2.toLong).sum

  /** Один мешок: [[Size]] вещей, каждая своим броском. `heroLvl` — уровень
    * героя: по нему вещи (с разбросом) и карта; `lore` — что герой знает о
    * травах. Номеров у вещей нет: их даст база. */
  def sack(heroLvl: Long, lore: LoreData, rng: Rng): (List[Item], Rng) =
    (1 to Size).foldLeft((List.empty[Item], rng)) { case ((acc, r), _) =>
      val (prize, r1) = roll(r)
      val (item, r2)  = make(prize, heroLvl, lore, r1)
      (acc :+ item, r2)
    }

  /** Что выпало одним броском — по весам [[Table]]. */
  def roll(rng: Rng): (Prize, Rng) = {
    val (hit, next) = rng.between(0L, TotalWeight.max(1L))
    @tailrec
    def walk(rest: List[(Prize, Int)], acc: Long): Prize = rest match {
      case (prize, w) :: tail => if (hit < acc + w || tail.isEmpty) prize else walk(tail, acc + w)
      case Nil                => Prize.Gear(Rarity.Gray)
    }
    (walk(Table, 0L), next)
  }

  /** Вещь по выпавшему виду. */
  def make(prize: Prize, heroLvl: Long, lore: LoreData, rng: Rng): (Item, Rng) =
    prize match {
      case Prize.Gear(rarity) => ItemGenerator.createItem(heroLvl, rarity, rng)
      case Prize.Material(kinds) =>
        val (kind, next) = rng.pick(if (kinds.isEmpty) List(MaterialKind.RatTail) else kinds)
        (MaterialGenerator.item(kind), next)
      case Prize.Herb(rank) =>
        val pool         = MaterialKind.herbsOfRank(rank).toList
        val (herb, next) = rng.pick(if (pool.isEmpty) List(MaterialKind.unknownFlower(rank)) else pool)
        (MaterialGenerator.item(HerbLore.recognised(lore, herb)), next)
      case Prize.Flask     => FlaskGenerator.roll(rng)
      case Prize.Gem       => GemGenerator.randomGem(Gem.MinGrade, rng)
      case Prize.MapHalf   => (TreasureMapGenerator.create(heroLvl, half = true), rng)
      case Prize.SmallRune => rune(RuneStoneSize.Small, rng)
      case Prize.BigRune   => rune(RuneStoneSize.Big, rng)
    }

  private def rune(size: RuneStoneSize, rng: Rng): (Item, Rng) = {
    val (r, next) = rng.pick(RuneStone.all)
    (RuneStone.item(r, size), next)
  }

  /** Легендарная экипировка в мешке — Барти скажет об этом своё. Фляга сюда не
    * считается: у неё своя редкость. */
  def jackpot(items: List[Item]): Boolean =
    items.exists(i => i.rarity == Rarity.Orange && ItemType.settable.contains(i.itemType))
}
