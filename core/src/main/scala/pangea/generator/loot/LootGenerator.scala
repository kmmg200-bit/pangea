package pangea.generator.loot

import pangea.domain.Rng
import pangea.generator.item.{FlaskGenerator, GemGenerator, ItemGenerator, ItemNameGenerator, MaterialGenerator, TreasureMapGenerator}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemSet, ItemType, MaterialKind, TrophyKind}
import pangea.model.monster.{MiniBoss, Race, Rarity => MobRarity}
import pangea.model.rune.{RuneStone, RuneStoneSize}
import pangea.model.item.{Gem => GemModel, Rarity => ItemRarity}

import scala.annotation.tailrec

/** Чистое ядро дропа лута при победе (§20 ARCHITECTURE). Решает «что выпало»
  * детерминированно по seed; персист/показ — забота shell
  * (BattleState/LootState).
  *
  * Алгоритм:
  *   1. По тиру моба катаем несколько слотов дропа (см. `dropChances`). Каждый
  *      слот срабатывает независимо со своим шансом. 2. Для сработавшего слота
  *      взвешенно выбираем категорию (см. `categoryWeights`). Сумма весов < 100
  *      — остаток это «пусто» (слот ничего не даёт). Уже выпавшая категория
  *      исключается из следующих слотов (без повторов, ответ 4). 3. Экипировка:
  *      редкость по тиру моба (`gearRarityWeights`), уровень — с разбросом из
  *      `ItemGenerator` (ответ 5). Трофей: тип по тиру (`trophyWeights`) +
  *      раса/уровень. Серебро: `lvl×4 ±20%` (и «груда», и обычное — одна
  *      формула).
  */
object LootGenerator {

  sealed trait LootDrop {

    /** Предмет этого дропа, если дроп предметный (у серебра предмета нет).
      * Единая точка для экранов добычи: раньше каждый из них перечислял виды
      * дропа своим `collect`, и новый вид молча терялся бы в обоих. */
    def itemOpt: Option[Item] = this match {
      case LootDrop.Gear(i)      => Some(i)
      case LootDrop.Trophy(i)    => Some(i)
      case LootDrop.MapHalf(i)   => Some(i)
      case LootDrop.Gem(i)       => Some(i)
      case LootDrop.Flask(i)     => Some(i)
      case LootDrop.Rune(i)      => Some(i)
      case LootDrop.Silver(_, _) => None
      case LootDrop.Doubloons(_) => None
    }
  }
  object LootDrop {
    final case class Gear(item: Item)                    extends LootDrop
    final case class Trophy(item: Item)                  extends LootDrop
    final case class MapHalf(item: Item)                 extends LootDrop
    final case class Gem(item: Item)                     extends LootDrop
    final case class Flask(item: Item)                   extends LootDrop
    final case class Rune(item: Item)                    extends LootDrop
    final case class Silver(amount: Long, pile: Boolean) extends LootDrop
    final case class Doubloons(amount: Long)             extends LootDrop
  }

  sealed trait Category
  object Category {
    case object Gear       extends Category
    case object Trophy     extends Category
    case object SilverPile extends Category
    case object MapHalf    extends Category
    case object Gem        extends Category
    case object Flask      extends Category
    case object Rune       extends Category
    case object RuneSmall  extends Category
  }

  // Сколько слотов дропа и шанс каждого (в %), по тиру моба.
  private def dropChances(tier: MobRarity): List[Int] =
    tier match {
      case MobRarity.Common    => List(30)
      case MobRarity.Uncommon  => List(60, 30)
      case MobRarity.Rare      => List(100, 60)
      case MobRarity.Mythical  => List(100, 60)
      case MobRarity.Legendary => List(100, 100, 60)
    }

  // Веса категорий (в %); сумма = 100 → «пусто» нет (для первого слота). На
  // последующих слотах уже выпавшая категория исключается, суммарный вес активных
  // падает, и появляется доля «пусто». У мифических и легендарных мобов 1% забран
  // у серебра под половинку карты сокровищ (MapHalf).
  //
  // Камень-усилитель (Gem) забран у уже существующих категорий, а не добавлен
  // сверху, чтобы сумма осталась 100:
  //   Редкие и мифические — 1% у трофея;
  //   Легендарные        — 5%: 2% у трофея и 3% у серебра.
  // Фляга (Flask) — 1% у всех тиров, забранный у экипировки (Gear: 35 → 34).
  // Рунные камни — 5% у всех тиров, забранные у экипировки (34 → 29), и эти
  // пять поделены между большой руной (1%) и малой (4%).
  private def categoryWeights(tier: MobRarity): List[(Category, Int)] =
    tier match {
      case MobRarity.Rare =>
        List(Category.Gear -> 29, Category.Trophy -> 38, Category.SilverPile -> 26, Category.Gem -> 1,
             Category.Flask -> 1, Category.Rune -> 1, Category.RuneSmall -> 4)
      case MobRarity.Mythical =>
        List(Category.Gear -> 29, Category.Trophy -> 38, Category.SilverPile -> 25, Category.MapHalf -> 1,
             Category.Gem -> 1, Category.Flask -> 1, Category.Rune -> 1, Category.RuneSmall -> 4)
      case MobRarity.Legendary =>
        List(Category.Gear -> 29, Category.Trophy -> 37, Category.SilverPile -> 22, Category.MapHalf -> 1,
             Category.Gem -> 5, Category.Flask -> 1, Category.Rune -> 1, Category.RuneSmall -> 4)
      case _ =>
        List(Category.Gear -> 29, Category.Trophy -> 39, Category.SilverPile -> 26, Category.Flask -> 1,
             Category.Rune -> 1, Category.RuneSmall -> 4)
    }

  // Редкость выпавшей экипировки, веса в долях 1/1_000_000 (сумма = 1_000_000).
  // Числа из §20 с исправленными суммами до 100% (Green добирает остаток).
  /** Веса редкостей вещи по тиру моба. Оранжевая вещь падает только с
    * ИМЕННЫХ — легендарных мобов, у которых есть собственное имя (см.
    * [[pangea.generator.monster.MonsterGenerator.legendaryName]]). У прочих её
    * билеты отданы синей: безымянный моб легендарной вещи не носит. */
  private def gearRarityWeights(tier: MobRarity): List[(ItemRarity, Long)] =
    tier match {
      case MobRarity.Common =>
        List(
          ItemRarity.Gray   -> 450000L,
          ItemRarity.White  -> 350000L,
          ItemRarity.Green  -> 164995L,
          ItemRarity.Blue   -> 20005L,
          ItemRarity.Purple -> 10000L,
          ItemRarity.Violet -> 5000L
        )
      case MobRarity.Uncommon =>
        List(
          ItemRarity.Gray   -> 300000L,
          ItemRarity.White  -> 450000L,
          ItemRarity.Green  -> 194995L,
          ItemRarity.Blue   -> 30005L,
          ItemRarity.Purple -> 20000L,
          ItemRarity.Violet -> 5000L
        )
      case MobRarity.Rare | MobRarity.Mythical =>
        List(
          ItemRarity.Gray   -> 190000L,
          ItemRarity.White  -> 400000L,
          ItemRarity.Green  -> 310000L,
          ItemRarity.Blue   -> 65000L,
          ItemRarity.Purple -> 30000L,
          ItemRarity.Violet -> 5000L
        )
      case MobRarity.Legendary =>
        List(
          ItemRarity.Gray   -> 0L,
          ItemRarity.White  -> 185000L,
          ItemRarity.Green  -> 310000L,
          ItemRarity.Blue   -> 250000L,
          ItemRarity.Purple -> 130000L,
          ItemRarity.Violet -> 95000L,
          ItemRarity.Orange -> 30000L
        )
    }

  // Вид трофея и веса (в %, сумма = 100) по тиру моба.
  private def trophyWeights(tier: MobRarity): List[(TrophyKind, Int)] =
    tier match {
      case MobRarity.Common =>
        List(
          TrophyKind.Relic    -> 1,
          TrophyKind.Talisman -> 9,
          TrophyKind.Head     -> 20,
          TrophyKind.Sack     -> 70
        )
      case MobRarity.Uncommon =>
        List(
          TrophyKind.Relic    -> 5,
          TrophyKind.Talisman -> 15,
          TrophyKind.Head     -> 20,
          TrophyKind.Sack     -> 60
        )
      case MobRarity.Rare =>
        List(
          TrophyKind.Relic    -> 15,
          TrophyKind.Talisman -> 20,
          TrophyKind.Head     -> 25,
          TrophyKind.Sack     -> 40
        )
      case MobRarity.Mythical =>
        List(
          TrophyKind.Relic    -> 15,
          TrophyKind.Talisman -> 25,
          TrophyKind.Head     -> 30,
          TrophyKind.Sack     -> 30
        )
      case MobRarity.Legendary =>
        List(
          TrophyKind.Relic    -> 25,
          TrophyKind.Talisman -> 45,
          TrophyKind.Head     -> 20,
          TrophyKind.Sack     -> 10
        )
    }

  /** Что срезают со зверя и с каким весом (в %). Сумма меньше ста — остаток
    * это «пусто»: с большинства крыс не берут ничего. `None` — малая руна: их
    * в канализацию нанесло водой, и попадаются они изредка. */
  val RatSpoils: List[(Option[MaterialKind], Int)] = List(
    Some(MaterialKind.RatPelt)      -> 30,
    Some(MaterialKind.RatTail)      -> 25,
    Some(MaterialKind.PlagueWorms)  -> 6,
    None                            -> 3,
    Some(MaterialKind.RatKingBlood) -> 1
  )

  /** Добыча с паука из гнезда сетеплётов — вместо крысиной: шёлк, ядовитые
    * железы, сокровища в коконе, изредка — малая руна, застрявшая в паутине. С
    * красного сверх того — его железа, всегда (см. BattleState.breedDrops). */
  val SpiderSpoils: List[(Option[MaterialKind], Int)] = List(
    Some(MaterialKind.SpiderSilk)     -> 35,
    Some(MaterialKind.VenomGland)     -> 15,
    Some(MaterialKind.CocoonTreasure) -> 5,
    None                              -> 3
  )

  /** Награбленное с обозов — сверх обычной добычи: с каждого павшего одна вещь
    * с шансом, вид — по весу (в сумме сто). Это добыча деревни и её контрактов,
    * в обычных боях его нет (решил пользователь): на разбойничьей заставе —
    * [[BanditSpoilsChancePct]]%, с разбойников в засаде на караван — вдвое реже,
    * [[AmbushSpoilsChancePct]]%. Ришелье берёт его по твёрдой цене
    * (MaterialKind.silverPrice). */
  val BanditSpoilsChancePct: Long = 30L
  val AmbushSpoilsChancePct: Long = 15L
  val BanditSpoils: List[(MaterialKind, Int)] = List(
    MaterialKind.Timber        -> 25,
    MaterialKind.HempRope      -> 18,
    MaterialKind.TannedLeather -> 15,
    MaterialKind.WoolCloth     -> 15,
    MaterialKind.IronIngot     -> 10,
    MaterialKind.PineTar       -> 10,
    MaterialKind.CopperIngot   -> 7
  )

  /** Бросок награбленного с одного павшего: с шансом `chancePct`% — одна вещь
    * (см. [[BanditSpoils]]). */
  def banditLoot(rng: Rng, chancePct: Long): (List[LootDrop], Rng) = {
    val (fired, r1) = rng.between(0L, 100L)
    if (fired >= chancePct) (Nil, r1)
    else {
      val (kind, r2) = pickWeighted(BanditSpoils, r1)
      (List(LootDrop.Gear(MaterialGenerator.item(kind))), r2)
    }
  }

  /** Тайник разбойников с заставы, который показал отпущенный: `n` вещей
    * награбленного с обозов — каждая своим броском по той же таблице
    * ([[BanditSpoils]]). Добыча деревни и её контрактов. */
  def banditCache(n: Int, rng: Rng): (List[Item], Rng) =
    (1 to n.max(0)).foldLeft((List.empty[Item], rng)) { case ((acc, r), _) =>
      val (kind, next) = pickWeighted(BanditSpoils, r)
      (acc :+ MaterialGenerator.item(kind), next)
    }

  /** Сколько бросков добычи со зверя: с обычного один, с чумного два — второй
    * пожиже. Легендарных и мифических зверей пока не водится, но если заведутся
    * — считаются как чумные. */
  private def ratChances(tier: MobRarity): List[Int] =
    tier match {
      case MobRarity.Common | MobRarity.Uncommon => List(50)
      case _                                     => List(70, 25)
    }

  /** Добыча со зверя. Ни трофея, ни серебра, ни дублонов, ни камней, ни вещей:
    * крыса не носит кошелька и не числится ни за одной гильдией — с неё берут
    * только то, что можно срезать. Вид, уже выпавший в этом бою, из следующего
    * броска выбывает: двух одинаковых шкурок с одной крысы не снять. */
  private def beastLoot(tier: MobRarity, spoils: List[(Option[MaterialKind], Int)], rng: Rng): (List[LootDrop], Rng) = {
    @tailrec
    def loop(slots: List[Int], used: Set[MaterialKind], acc: List[LootDrop], r: Rng): (List[LootDrop], Rng) =
      slots match {
        case Nil => (acc.reverse, r)
        case chance :: rest =>
          val (fired, r1) = r.between(0L, 100L)
          if (fired >= chance) loop(rest, used, acc, r1)
          else {
            val active     = spoils.filterNot { case (m, _) => m.exists(used.contains) }
            val (roll, r2) = r1.between(0L, 100L)
            @tailrec
            def walk(rem: List[(Option[MaterialKind], Int)], acc2: Long): Option[Option[MaterialKind]] =
              rem match {
                case Nil             => None
                case (m, w) :: tail  => if (roll < acc2 + w) Some(m) else walk(tail, acc2 + w)
              }
            walk(active, 0L) match {
              case None               => loop(rest, used, acc, r2)
              case Some(Some(kind))   =>
                loop(rest, used + kind, LootDrop.Gear(MaterialGenerator.item(kind)) :: acc, r2)
              case Some(None)         =>
                val (rune, r3) = r2.pick(RuneStone.all)
                loop(rest, used, LootDrop.Rune(RuneStone.item(rune, RuneStoneSize.Small)) :: acc, r3)
            }
          }
      }
    loop(ratChances(tier), Set.empty, Nil, rng)
  }

  def roll(
      tier: MobRarity,
      race: Race,
      killLevel: Long,
      rng: Rng,
      gearChanceBonusPct: Long = 0L,
      rarityBumpPct: Long = 0L,
      // Что срезают со зверя: по умолчанию — крысиное, в гнезде сетеплётов — паучье.
      beastSpoils: List[(Option[MaterialKind], Int)] = RatSpoils
  ): (List[LootDrop], Rng) = if (race == Race.Animal) beastLoot(tier, beastSpoils, rng) else {
    // Бонус к шансу экипировки (топаз в снаряжении) добавляется к весу категории Gear.
    val weights = categoryWeights(tier).map {
      case (Category.Gear, w) => Category.Gear -> (w + gearChanceBonusPct.toInt)
      case other              => other
    }
    @tailrec
    def loop(
        slots: List[Int],
        used: Set[Category],
        acc: List[LootDrop],
        r: Rng
    ): (List[LootDrop], Rng) =
      slots match {
        case Nil => (acc.reverse, r)
        case chance :: rest =>
          val (roll, r1) = r.between(0L, 100L)
          if (roll >= chance) loop(rest, used, acc, r1)
          else {
            val (catOpt, r2) = pickCategory(weights, used, r1)
            catOpt match {
              case None => loop(rest, used, acc, r2)
              case Some(cat) =>
                val (drop, r3) = makeDrop(cat, tier, race, killLevel, r2, rarityBumpPct)
                loop(rest, used + cat, drop :: acc, r3)
            }
          }
      }
    loop(dropChances(tier), Set.empty, Nil, rng)
  }

  /** Дроп с минибосса. В отличие от обычного лута он есть ВСЕГДА и не зависит
    * от таблицы категорий: выпадает `0..1 + BossLvL` предметов, каждый — по
    * своей раскладке босса (элементаль, Джо, волк, король, Некромант).
    *
    * Свой ингредиент — тот, что в кубе переводит вещь в его набор (шкура волка,
    * кровь короля, кости Некроманта…), — половина бросков, пока он с этого
    * босса не выпал, и больше одного с босса не падает (см. [[rollBossScale]]).
    * Выпал — следующие броски идут по шкале без него, по одной прочей добыче.
    * Фиолетовых и легендарных вещей от этого не прибавляется: их шанс на бросок
    * считается от полной шкалы, и лишние вещи набора — синие (см. [[setRarity]]).
    *
    * Уровень вещи берётся от уровня ГЕРОЯ с разбросом ±1: у босса свой BossLvL
    * (1..10), и вещь по нему была бы мусором. `floorLvl` — этаж встречи: по нему
    * считается клык волка. */
  def rollMiniBoss(
      boss: MiniBoss,
      bossLvl: Long,
      heroLvl: Long,
      rng: Rng,
      floorLvl: Long = 1L
  ): (List[LootDrop], Rng) = {
    val (extra, r0) = rng.between(0L, 2L) // 0 или 1 сверх BossLvL
    // С Крысиного короля вещей вдвое меньше: большую часть его добычи составляют
    // ингредиенты, вещи набора «Крыса» идут лишь на четверть билетов.
    // Столько же вещей и с Некроманта: его добыча идёт тем же счётом, что у
    // короля, — половина от уровня босса, но не меньше одной.
    val raw         = extra + bossLvl
    val halved      = boss == MiniBoss.RatKing || boss == MiniBoss.Necromancer
    val count       = (if (halved) raw / 2L else raw).toInt.max(1)
    (0 until count).foldLeft((List.empty[LootDrop], r0, true)) { case ((acc, r, left), _) =>
      val (drops, r2) = bossDrop(boss, bossLvl, heroLvl, floorLvl, left, r)
      (acc ++ drops, r2, left && !drops.exists(isIngredient(boss, _)))
    } match { case (drops, r, _) => (drops, r) }
  }

  /** Один бросок добычи минибосса — по его раскладке. `ingredientLeft` — свой
    * ингредиент с этого босса ещё не падал. У Некроманта череп идёт с парой
    * дублонов: бросок один, а записей две. */
  private def bossDrop(
      boss: MiniBoss,
      bossLvl: Long,
      heroLvl: Long,
      floorLvl: Long,
      ingredientLeft: Boolean,
      rng: Rng
  ): (List[LootDrop], Rng) = boss match {
    case MiniBoss.RatKing     => single(ratKingDrop(boss, heroLvl, ingredientLeft, rng))
    case MiniBoss.Necromancer => necromancerDrop(boss, heroLvl, ingredientLeft, rng)
    case MiniBoss.WhiteWolf   => single(wolfDrop(boss, bossLvl, heroLvl, floorLvl, ingredientLeft, rng))
    case MiniBoss.RottenJoe   => single(joeDrop(boss, bossLvl, heroLvl, ingredientLeft, rng))
    case _                    => single(elementalDrop(boss, heroLvl, ingredientLeft, rng))
  }

  private def single(drop: (LootDrop, Rng)): (List[LootDrop], Rng) = (List(drop._1), drop._2)

  /** Свой ли это ингредиент босса — тот, что падает с него не больше одного. */
  def isIngredient(boss: MiniBoss, drop: LootDrop): Boolean =
    drop.itemOpt.exists(_.material.contains(boss.ingredient))

  /** Свой ингредиент босса как добыча. */
  private def ingredientOf(boss: MiniBoss): LootDrop = LootDrop.Gear(MaterialGenerator.item(boss.ingredient))

  /** Доля своего ингредиента в бросках минибосса, пока он с этого босса не
    * выпал: половина — он весит столько же, сколько вся прочая добыча вместе. */
  val BossIngredientSharePct: Long = 50L

  /** Бросок по шкале добычи минибосса: впереди — его ингредиент (пока он с
    * этого босса не выпал) с долей [[BossIngredientSharePct]] от всей шкалы, за
    * ним — `others` долей прочей добычи, раскладка которой у каждого босса своя.
    * Выпал ингредиент — бросок идёт по одному прочему, и его доля делится между
    * прочим соразмерно весам.
    *
    * Возвращает бросок по шкале прочего, `[0, others)`, или None, если выпал
    * ингредиент; размер шкалы, по которой шёл бросок, и всю шкалу (от них
    * считается редкость вещи набора, см. [[setRarity]]); и Rng. */
  private def rollBossScale(others: Long, ingredientLeft: Boolean, rng: Rng): (Option[Long], Long, Long, Rng) = {
    val ingredient = others * BossIngredientSharePct / (100L - BossIngredientSharePct).max(1L)
    val full       = ingredient + others
    val rolled     = if (ingredientLeft) full else others
    val (roll, r1) = rng.between(0L, rolled)
    val rest       = if (ingredientLeft) roll - ingredient else roll
    (Option.when(rest >= 0L)(rest), rolled, full, r1)
  }

  /** С Белого волка: шкура — половина бросков, пока она с этого волка не
    * выпала; прочее — клык (20 долей), вещь «Охотника» (40), резец рунописца
    * (10) и большая руна (5). Редкость вещи — общая для минибоссов (см.
    * [[setRarity]]). */
  private def wolfDrop(
      boss: MiniBoss,
      bossLvl: Long,
      heroLvl: Long,
      floorLvl: Long,
      ingredientLeft: Boolean,
      rng: Rng
  ): (LootDrop, Rng) =
    rollBossScale(WolfOthers, ingredientLeft, rng) match {
      case (None, _, _, r1) => (ingredientOf(boss), r1)
      case (Some(roll), rolled, full, r1) =>
        if (roll < WolfFangUntil) (LootDrop.Trophy(wolfFang(bossLvl, floorLvl)), r1)
        else if (roll < WolfGearUntil) setDrop(boss, heroLvl, rolled, full, r1)
        else if (roll < WolfChiselUntil) (chisel, r1)
        else bigRune(r1)
    }

  /** Клык Белого волка: трофей расы «Животное», уровень — этаж встречи,
    * коэффициент — 6 за каждый BossLvL (лежит на самом предмете). */
  def wolfFang(bossLvl: Long, floorLvl: Long): Item = Item(
    id = -1L,
    name = TrophyKind.Fang.displayName,
    lvl = floorLvl.max(1L),
    rarity = ItemRarity.Gray,
    itemType = ItemType.Trophy,
    attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
    details = ItemDetails.Trophy(Race.Animal.entryName, TrophyKind.Fang,
      Some(MiniBoss.WhiteWolf.FangCoefPerLvl * bossLvl))
  )

  /** Границы прочей добычи волка (0..74): до 20 — клык, до 60 — вещь набора,
    * до 70 — резец рунописца, дальше — большая руна. */
  val WolfFangUntil: Long   = 20L
  val WolfGearUntil: Long   = 60L
  val WolfChiselUntil: Long = 70L
  val WolfOthers: Long      = 75L

  /** С элементаля: ингредиент — половина бросков, пока он не выпал; прочее —
    * большая руна (10 долей), резец рунописца (10) и вещь его набора (40),
    * редкость которой решается общим для минибоссов порядком (см.
    * [[setRarity]]). */
  private def elementalDrop(boss: MiniBoss, heroLvl: Long, ingredientLeft: Boolean, rng: Rng): (LootDrop, Rng) =
    rollBossScale(ElementalOthers, ingredientLeft, rng) match {
      case (None, _, _, r1) => (ingredientOf(boss), r1)
      case (Some(roll), rolled, full, r1) =>
        if (roll < ElementalRuneWeight) bigRune(r1)
        else if (roll < ElementalRuneWeight + ElementalChiselWeight) (chisel, r1)
        else setDrop(boss, heroLvl, rolled, full, r1)
    }

  /** С Гнилого Джо: кожа упыря — половина бросков, пока она не выпала; прочее —
    * расколотый усилитель (20 долей), горсть дублонов (20), большая руна (10),
    * резец рунописца (10) и вещь «Упыря» (15); редкость её — та же, что у всех
    * минибоссов (см. [[setRarity]]). */
  private def joeDrop(boss: MiniBoss, bossLvl: Long, heroLvl: Long, ingredientLeft: Boolean, rng: Rng): (LootDrop, Rng) =
    rollBossScale(JoeOthers, ingredientLeft, rng) match {
      case (None, _, _, r1) => (ingredientOf(boss), r1)
      case (Some(roll), rolled, full, r1) =>
        if (roll < JoeGemUntil) {
          // Усилитель — любой камень или череп, но всегда самого низкого качества
          // («расколотый»), тем же роллом, что и обычный дроп камня.
          val (gem, r2) = GemGenerator.randomGem(GemModel.MinGrade, r1)
          (LootDrop.Gem(gem), r2)
        } else if (roll < JoeDoubloonsUntil) {
          // Дублоны: 2 × BossLvL с разбросом ±20%, но не меньше одного.
          val base       = JoeDoubloonsPerLvl * bossLvl
          val (pct, r2)  = r1.between(100L - JoeDoubloonSpreadPct, 100L + JoeDoubloonSpreadPct + 1L)
          (LootDrop.Doubloons((base * pct / 100L).max(1L)), r2)
        } else if (roll < JoeRuneUntil) bigRune(r1)
        else if (roll < JoeChiselUntil) (chisel, r1)
        else setDrop(boss, heroLvl, rolled, full, r1)
    }

  /** С Крысиного короля: кровь — половина бросков, пока она не выпала; прочее
    * — по билетам: большая руна 50, чумные черви 25, резец рунописца 18 и вещи
    * набора «Крыса» 32; редкость вещи решается общим порядком (см.
    * [[setRarity]]). Черви — не его ингредиент: их сколько выпадет, столько и
    * есть. */
  private def ratKingDrop(boss: MiniBoss, heroLvl: Long, ingredientLeft: Boolean, rng: Rng): (LootDrop, Rng) =
    rollBossScale(RatKingTickets, ingredientLeft, rng) match {
      case (None, _, _, r1) => (ingredientOf(boss), r1)
      case (Some(roll), rolled, full, r1) =>
        if (roll < RatKingRuneTickets) bigRune(r1)
        else if (roll < RatKingRuneTickets + RatKingWormTickets)
          (LootDrop.Gear(MaterialGenerator.item(MaterialKind.PlagueWorms)), r1)
        else if (roll < RatKingRuneTickets + RatKingWormTickets + RatKingChiselTickets) (chisel, r1)
        else setDrop(boss, heroLvl, rolled, full, r1)
    }

  /** С Некроманта: проклятые кости — половина бросков, пока они не выпали;
    * прочее — вещь его набора (40 долей, редкость по общему порядку, см.
    * [[setRarity]]), резец рунописца (10) и расколотый череп с парой дублонов
    * (25). Череп здесь не случайный камень, а именно череп: ему и место в
    * оружии некроманта. */
  private def necromancerDrop(boss: MiniBoss, heroLvl: Long, ingredientLeft: Boolean, rng: Rng): (List[LootDrop], Rng) =
    rollBossScale(NecroOthers, ingredientLeft, rng) match {
      case (None, _, _, r1) => (List(ingredientOf(boss)), r1)
      case (Some(roll), rolled, full, r1) =>
        if (roll < NecroGearUntil)
          setDrop(boss, heroLvl, rolled, full, r1) match {
            case (drop, r) => (List(drop), r)
          }
        else if (roll < NecroChiselUntil) (List(chisel), r1)
        // Череп идёт с парой дублонов: это один ролл, но две записи в добыче.
        else (List(
          LootDrop.Gem(GemGenerator.item(pangea.model.item.GemKind.Skull, GemModel.MinGrade)),
          LootDrop.Doubloons(NecroSkullDoubloons)), r1)
    }

  /** Границы прочей добычи Некроманта (0..74): до 40 — вещь набора, до 50 —
    * резец рунописца, дальше — череп с дублонами. */
  val NecroGearUntil:   Long = 40L
  val NecroChiselUntil: Long = 50L
  val NecroOthers:      Long = 75L
  /** Сколько дублонов идёт вместе с черепом. */
  val NecroSkullDoubloons: Long = 2L

  /** Билеты прочей добычи Крысиного короля: большая руна 50, черви 25, резец
    * рунописца 18 и вещь набора 32. Кровь — ещё столько же сверх них. */
  val RatKingRuneTickets:   Long = 50L
  val RatKingWormTickets:   Long = 25L
  val RatKingChiselTickets: Long = 18L
  val RatKingGearTickets:   Long = 32L
  val RatKingTickets:       Long = RatKingRuneTickets + RatKingWormTickets + RatKingChiselTickets + RatKingGearTickets

  /** Резец рунописца: есть в прочей добыче каждого минибосса (см.
    * `pangea.model.item.Chisel`). */
  private def chisel: LootDrop = LootDrop.Gear(MaterialGenerator.item(MaterialKind.RuneChisel))

  /** Большая руна с минибосса: вид равновероятен среди всех, как и в добыче. */
  private def bigRune(rng: Rng): (LootDrop, Rng) = {
    val (rune, r1) = rng.pick(RuneStone.all)
    (LootDrop.Rune(RuneStone.item(rune, RuneStoneSize.Big)), r1)
  }

  /** Редкость вещи набора с минибосса, в промилле от его вещей набора:
    * фиолетовая — 5%, легендарная — полпроцента, остальное синее. В промилле —
    * иначе полпроцента не выразить. */
  val BossPurpleOfGearPerMille: Long = 50L
  val BossOrangeOfGearPerMille: Long = 5L

  /** Редкость вещи набора с минибосса (см. [[BossPurpleOfGearPerMille]]).
    * `full` — вся шкала его добычи, `rolled` — шкала, по которой шёл этот
    * бросок: без выпавшего ингредиента она короче.
    *
    * Доли считаются от ПОЛНОЙ шкалы: на полной шкале фиолетовых и легендарных
    * ровно 5% и 0,5% вещей набора, а выпавший ингредиент их на бросок не
    * поднимает — его доля уходит прочей добыче, и лишние вещи набора синие.
    * Внутри вещи набора это 5% и 0,5%, умноженные на rolled/full: без
    * ингредиента — меньше. Считаем в миллионных долях. */
  private def setRarity(rolled: Long, full: Long, rng: Rng): (ItemRarity, Rng) = {
    def share(perMille: Long): Long = perMille * 1000L * rolled / full.max(1L)
    val purple       = share(BossPurpleOfGearPerMille)
    val orange       = share(BossOrangeOfGearPerMille)
    val (roll, next) = rng.between(0L, 1000000L)
    if (roll < purple) (ItemRarity.Purple, next)
    else if (roll < purple + orange) (ItemRarity.Orange, next)
    else (ItemRarity.Blue, next)
  }

  /** Вещь набора босса со своей редкостью — один ролл на то и другое. Шкалы — как
    * у [[setRarity]]. */
  private def setDrop(boss: MiniBoss, heroLvl: Long, rolled: Long, full: Long, rng: Rng): (LootDrop, Rng) = {
    val (rarity, r1) = setRarity(rolled, full, rng)
    setGear(boss, heroLvl, rarity, r1)
  }

  /** Вещь набора этого босса заданной редкости: уровень героя ±1, но строго в
    * границах игры — на первом уровне разброс не уводит вещь в нулевой, на
    * последнем в 151-й. Имя перекатываем как сетовое: имя набора встаёт вместо
    * титула. */
  private def setGear(boss: MiniBoss, heroLvl: Long, rarity: ItemRarity, rng: Rng): (LootDrop, Rng) = {
    val (delta, r2) = rng.between(-1L, 2L)
    val lvl         = (heroLvl + delta).max(1L).min(Hero.MaxLevel)
    val (item, r3)  = ItemGenerator.createItemAtLevel(lvl, rarity, r2)
    boss.set match {
      // Набора у минибосса нет — выпадает вещь его уровня той же редкости,
      // только без набора.
      case None => (LootDrop.Gear(item), r3)
      case Some(set) =>
        val (name, r4) = ItemNameGenerator.setName(item.itemType, item.rarity, set, r3, item.grip)
        (LootDrop.Gear(item.copy(name = name, set = Some(set))), r4)
    }
  }

  /** Редкость вещи малого расового набора (в %, сумма = 100). */
  val RaceSetRarityWeights: List[(ItemRarity, Int)] = List(
    ItemRarity.Green  -> 30,
    ItemRarity.Blue   -> 25,
    ItemRarity.Purple -> 22, // фиолетовая
    ItemRarity.Violet -> 22, // пурпурная
    ItemRarity.Orange -> 1
  )

  /** Вещь малого набора расы `race` — её роняет каждый легендарный моб этой
    * расы, ровно одну: ни благословение, ни что-либо ещё ни число, ни редкость не
    * поднимают. Уровень — как у прочей добычи с моба. У рас без набора
    * (элементали, нежить, звери, сооружения) — ничего. Имя перекатываем как
    * сетовое: имя набора встаёт вместо титула. */
  def raceSetGear(race: Race, killLevel: Long, rng: Rng): (Option[Item], Rng) =
    ItemSet.ofRace(race) match {
      case None => (None, rng)
      case Some(set) =>
        val (rarity, r1) = pickWeighted(RaceSetRarityWeights, rng)
        val lvl          = killLevel.max(1L).min(Hero.MaxLevel)
        val (item, r2)   = ItemGenerator.createItemAtLevel(lvl, rarity, r1)
        val (name, r3)   = ItemNameGenerator.setName(item.itemType, item.rarity, set, r2, item.grip)
        (Some(item.copy(name = name, set = Some(set))), r3)
    }

  /** Прочая добыча элементаля, в долях: большая руна, резец рунописца и вещь
    * его набора — какой она редкости, решает общий для минибоссов порядок (см.
    * [[setRarity]]). Ингредиент — ещё столько же, сколько всё это вместе. */
  val ElementalRuneWeight: Long   = 10L
  val ElementalChiselWeight: Long = 10L
  val ElementalGearWeight: Long   = 40L
  val ElementalOthers: Long       = ElementalRuneWeight + ElementalChiselWeight + ElementalGearWeight

  /** Границы прочей добычи Джо (0..74): до 20 — усилитель, до 40 — дублоны, до
    * 50 — большая руна, до 60 — резец рунописца, дальше — вещь набора. */
  val JoeGemUntil: Long       = 20L
  val JoeDoubloonsUntil: Long = 40L
  val JoeRuneUntil: Long      = 50L
  val JoeChiselUntil: Long    = 60L
  val JoeOthers: Long         = 75L

  /** Сколько дублонов за уровень босса роняет Джо и с каким разбросом (в %). */
  val JoeDoubloonsPerLvl: Long   = 2L
  val JoeDoubloonSpreadPct: Long = 20L

  /** Доп. дропы от пассивок героя, независимые от основного ролла [[roll]] (каждый
    * со своим шансом): «Таксидермист» — шанс на лишний трофей, «Ювелир» — на
    * отдельную груду серебра по обычной формуле дропа. Шансы в процентах (0 —
    * пассивки нет; понимание руны их поднимает, см. HeroPassives). Чистое ядро:
    * числа, не Hero. */
  def rollPassiveDrops(
      trophyChancePct: Long,
      silverChancePct: Long,
      tier: MobRarity,
      race: Race,
      killLevel: Long,
      rng: Rng
  ): (List[LootDrop], Rng) = {
    // Трофей «Таксидермиста» — только с рас, с которых падают обычные трофеи
    // (см. [[Race.trophyLoot]]): не зверь, не элементаль, не нежить и пр.
    val (trophy, r1) =
      if (trophyChancePct > 0L && Race.trophyLoot(race))
        rollChance(trophyChancePct, rng)(makeDrop(Category.Trophy, tier, race, killLevel, _))
      else (None, rng)
    val (silver, r2) =
      if (silverChancePct > 0L) rollChance(silverChancePct, r1) { r =>
        val (amount, rr) = rollSilver(killLevel, r)
        (LootDrop.Silver(amount, pile = false), rr)
      }
      else (None, r1)
    (List(trophy, silver).flatten, r2)
  }

  /** Благословение Азата после минибосса: с шансом `chancePct`% — лишняя вещь его
    * набора: фиолетовая на 5%, легендарная на полпроцента, остальное синее (вся
    * её шкала — она сама, см. [[setRarity]]). Лишняя вещь — с того, кого убили,
    * а не случайная. Набора у него ещё нет (Громовой) — вещь его уровня той же
    * редкости, как и в его добыче (см. [[setGear]]). RNG тратится всегда. */
  def rollBlessingBossGear(chancePct: Long, boss: MiniBoss, heroLvl: Long, rng: Rng): (Option[Item], Rng) = {
    val (roll, r1) = rng.between(0L, 100L)
    if (roll < chancePct) {
      val (drop, r2) = setDrop(boss, heroLvl, 1L, 1L, r1)
      (drop.itemOpt, r2)
    } else (None, r1)
  }

  /** Благословение Азата: с шансом `chancePct`% — дополнительная экипировка (редкость
    * по тиру моба). RNG тратится всегда. */
  def rollBlessingExtraGear(chancePct: Long, tier: MobRarity, killLevel: Long, rng: Rng): (Option[Item], Rng) = {
    val (roll, r1) = rng.between(0L, 100L)
    if (roll < chancePct) {
      val (rarity, r2) = pickGearRarity(gearRarityWeights(tier), r1)
      val (item, r3)   = ItemGenerator.createItem(killLevel, rarity, r2)
      (Some(item), r3)
    } else (None, r1)
  }


  // С шансом `pct`% выполнить `make` (даёт дроп), иначе None. RNG тратится всегда.
  private def rollChance(pct: Long, rng: Rng)(make: Rng => (LootDrop, Rng)): (Option[LootDrop], Rng) = {
    val (roll, r1) = rng.between(0L, 100L)
    if (roll < pct) { val (drop, r2) = make(r1); (Some(drop), r2) }
    else (None, r1)
  }

  // Взвешенный выбор категории среди ещё не выпавших; остаток до 100 — «пусто» (None).
  private def pickCategory(
      weights: List[(Category, Int)],
      used: Set[Category],
      rng: Rng
  ): (Option[Category], Rng) = {
    val active       = weights.filterNot { case (c, _) => used.contains(c) }
    val (roll, next) = rng.between(0L, 100L)
    @tailrec
    def walk(rem: List[(Category, Int)], acc: Long): Option[Category] =
      rem match {
        case Nil => None
        case (c, w) :: tail =>
          val upper = acc + w
          if (roll < upper) Some(c) else walk(tail, upper)
      }
    (walk(active, 0L), next)
  }

  private def makeDrop(
      cat: Category,
      tier: MobRarity,
      race: Race,
      killLevel: Long,
      rng: Rng,
      rarityBumpPct: Long = 0L
  ): (LootDrop, Rng) =
    cat match {
      case Category.Gear =>
        val (rarity0, r1) = pickGearRarity(gearRarityWeights(tier), rng)
        // Благословение Азата: с шансом rarityBumpPct поднять редкость на тир,
        // но не выше потолка этого моба — у безымянного это фиолетовая.
        val (rarity, r1b) = bumpRarity(rarity0, rarityBumpPct, rarityCap(tier), r1)
        val (item, r2)    = ItemGenerator.createItem(killLevel, rarity, r1b)
        (LootDrop.Gear(item), r2)

      case Category.Trophy =>
        val (kind, r1) = pickWeighted(trophyWeights(tier), rng)
        val trophy = Item(
          id = -1L,
          name = s"${kind.displayName} (${race.toString})",
          lvl = killLevel.max(1L),
          rarity = ItemRarity.Gray,
          itemType = ItemType.Trophy,
          attack = 0,
          accuracy = 0,
          energy = 0,
          armor = 0,
          defence = 0,
          evasion = 0,
          details = ItemDetails.Trophy(race.entryName, kind)
        )
        (LootDrop.Trophy(trophy), r1)

      case Category.SilverPile =>
        val (amount, r1) = rollSilver(killLevel, rng)
        (LootDrop.Silver(amount, pile = true), r1)

      case Category.MapHalf =>
        // половинка карты сокровищ — по уровню убитого моба; RNG не тратит
        (LootDrop.MapHalf(TreasureMapGenerator.create(killLevel, half = true)), rng)

      case Category.Flask =>
        // Фляга: редкость и вид по своим таблицам, к этажу не привязана.
        val (flask, r1) = FlaskGenerator.roll(rng)
        (LootDrop.Flask(flask), r1)

      case Category.Gem =>
        // Камень-усилитель 1-го тира («надколотый»). Вид равновероятен среди всех
        // семи, черепа в том числе — в отличие от серебряной жилы, где череп исключён.
        val (gem, r1) = GemGenerator.randomGem(GemModel.MinGrade, rng)
        (LootDrop.Gem(gem), r1)

      case Category.Rune =>
        // Большая руна: вид равновероятен среди всех, боевых и пассивных.
        val (rune, r1) = rng.pick(RuneStone.all)
        (LootDrop.Rune(RuneStone.item(rune, RuneStoneSize.Big)), r1)

      case Category.RuneSmall =>
        val (rune, r1) = rng.pick(RuneStone.all)
        (LootDrop.Rune(RuneStone.item(rune, RuneStoneSize.Small)), r1)
    }

  // Серебро: базис lvl×4 с разбросом ±20%, минимум 1.
  private def rollSilver(killLevel: Long, rng: Rng): (Long, Rng) = {
    val base        = killLevel.max(1L) * 4L
    val (pct, next) = rng.between(80L, 121L) // 80..120 %
    ((base * pct / 100L).max(1L), next)
  }

  private def pickWeighted[A](weights: List[(A, Int)], rng: Rng): (A, Rng) = {
    val total        = weights.map(_._2).sum.max(1)
    val (roll, next) = rng.between(0L, total.toLong)
    @tailrec
    def walk(rem: List[(A, Int)], acc: Long): A =
      rem match {
        case (a, _) :: Nil  => a
        case (a, w) :: tail => if (roll < acc + w) a else walk(tail, acc + w)
        case Nil            => weights.head._1
      }
    (walk(weights, 0L), next)
  }

  // Лестница редкостей для повышения тира благословением.
  private val rarityLadder: List[ItemRarity] =
    List(ItemRarity.Gray, ItemRarity.White, ItemRarity.Green, ItemRarity.Blue,
      ItemRarity.Purple, ItemRarity.Violet, ItemRarity.Orange)

  /** Выше какой редкости вещь с этого моба не поднимается: оранжевую носят
    * только ИМЕННЫЕ — легендарные мобы со своим именем. */
  def rarityCap(tier: MobRarity): ItemRarity =
    if (tier == MobRarity.Legendary) ItemRarity.Orange else ItemRarity.Violet

  // С шансом `pct`% поднимает редкость на один тир, но не выше `cap`
  // (RNG тратится только если pct>0).
  private def bumpRarity(r: ItemRarity, pct: Long, cap: ItemRarity, rng: Rng): (ItemRarity, Rng) =
    if (pct <= 0L) (r, rng)
    else {
      val (roll, next) = rng.between(0L, 100L)
      if (roll < pct) {
        val i = rarityLadder.indexOf(r)
        val up = if (i >= 0 && i < rarityLadder.size - 1) rarityLadder(i + 1) else r
        (if (ItemRarity.atMost(up, cap)) up else r, next)
      } else (r, next)
    }

  private def pickGearRarity(
      weights: List[(ItemRarity, Long)],
      rng: Rng
  ): (ItemRarity, Rng) = {
    val total        = weights.map(_._2).sum.max(1L)
    val (roll, next) = rng.between(0L, total)
    @tailrec
    def walk(rem: List[(ItemRarity, Long)], acc: Long): ItemRarity =
      rem match {
        case (a, _) :: Nil  => a
        case (a, w) :: tail => if (roll < acc + w) a else walk(tail, acc + w)
        case Nil            => weights.head._1
      }
    (walk(weights, 0L), next)
  }
}
