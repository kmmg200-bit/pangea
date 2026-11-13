package pangea.service.state.states.events.cave

import pangea.model.battle.{Burn, Element, Poison, SoloPveBattle}
import pangea.model.cave.CaveRates
import pangea.model.item.{BrewKind, FlaskEffect, Item, ItemDetails, MaterialKind}
import pangea.model.monster.Monster
import pangea.service.state.BeltContents

/** Что вещь даёт пещере, если пустить её в дело на пороге. */
sealed trait CaveBoon
object CaveBoon {

  /** Мобы одурманены: атака и энергия у всех срезаны. Даёт сонный дурман и
    * дымная фляга — от них одинаково клонит в сон. */
  case object Dope extends CaveBoon

  /** Все мобы пещеры входят в бой отравленными (глефовый гриб). */
  case object Poison extends CaveBoon

  /** Вещь истрачена впустую: пещере от неё ни холодно, ни жарко. */
  case object None extends CaveBoon
}

/** Что вещь даёт гнезду сетеплётов, если пустить её в дело у входа (см.
  * `VillageLairState`): то же, что пещере, и ещё два — огонь и паучий цвет. */
sealed trait NestBoon
object NestBoon {

  /** Пауки одурманены, как в пещере: дымная фляга и сонный дурман. */
  case object Dope extends NestBoon

  /** Пауки отравлены ещё до боя, как в пещере: глефовый гриб. */
  case object Poison extends NestBoon

  /** Пауки начинают бой горящими: огненная фляга и рубиновая пыль. */
  case object Burn extends NestBoon

  /** Паучий цвет уводит выводок: остаются матка и красные. */
  case object Lure extends NestBoon

  /** Вещь истрачена впустую. */
  case object None extends NestBoon
}

/** Что можно пустить в дело на пороге пещеры и у каравана: вещь при герое — в
  * сумке или в руке (по id) — или вещь из ячейки пояса (по номеру ячейки). */
sealed trait Supply { def item: Item }
object Supply {
  final case class Carried(item: Item) extends Supply
  final case class OnBelt(slot: Int, item: Item) extends Supply
}

object CaveSupply {

  /** Что из пояса можно пустить в дело: по ячейке — вещь, собранная по её сути
    * (см. BeltContents.item). Высыпанной пыли, выпитого отвара и осыпавшейся
    * розы нет — брать из них нечего; пустая фляга есть: она так и остаётся
    * флягой. Номер — настоящий номер ячейки. */
  def beltSupplies(belt: Item): List[Supply.OnBelt] =
    BeltContents.slotsOf(belt).zipWithIndex.flatMap { case (slot, i) =>
      BeltContents.item(slot).map(Supply.OnBelt(i, _))
    }

  /** Пояс после того, как из ячейки `slot` взяли одно (см. `BeltSlot.taken`). */
  def beltTaken(belt: Item, slot: Int): Item = belt.details match {
    case b: ItemDetails.Belt => b.slots.lift(slot).fold(belt)(s => belt.copy(details = b.updated(slot, s.taken)))
    case _                   => belt
  }

  /** Чем эта вещь поможет в пещере. Смотрим по сути предмета, а не по имени:
    * дымная фляга опознаётся по своему эффекту, каким бы редким ни был экземпляр. */
  def boonOf(item: Item): CaveBoon = item.details match {
    case ItemDetails.Flask(FlaskEffect.Smoke(_), _, _)      => CaveBoon.Dope
    case ItemDetails.Brew(BrewKind.SleepingDope)            => CaveBoon.Dope
    case ItemDetails.Material(MaterialKind.GlaiveMushroom)  => CaveBoon.Poison
    case _                                                  => CaveBoon.None
  }

  /** Заряды вещи, если они у неё есть: такую не забирают целиком, у неё тратят
    * один глоток (фляга, пояс, божественное оружие, роза). */
  def charged(item: Item): Option[ItemDetails.Charged] = item.details match {
    case c: ItemDetails.Charged => Some(c)
    case _                      => None
  }

  /** Чем эта вещь поможет у гнезда сетеплётов: по сути предмета, как и
    * [[boonOf]]. Так решил пользователь: дым слабит пауков, огненная фляга и
    * рубиновая пыль поджигают, паучий цвет уводит выводок. */
  def nestBoonOf(item: Item): NestBoon = item.details match {
    case ItemDetails.Flask(FlaskEffect.Smoke(_), _, _)             => NestBoon.Dope
    case ItemDetails.Brew(BrewKind.SleepingDope)                   => NestBoon.Dope
    case ItemDetails.Material(MaterialKind.GlaiveMushroom)         => NestBoon.Poison
    case ItemDetails.Flask(FlaskEffect.Splash(Element.Fire), _, _) => NestBoon.Burn
    case ItemDetails.Material(MaterialKind.RubyDust)               => NestBoon.Burn
    case ItemDetails.Material(MaterialKind.SpiderBloom)            => NestBoon.Lure
    case _                                                         => NestBoon.None
  }

  // ── Что подготовка делает с боем: общее у пещеры и гнезда ───────────────────

  /** Одурманенный моб бьёт слабее и копит энергию медленнее. */
  def weaken(m: Monster): Monster =
    m.copy(fightStats = m.fightStats.copy(
      atk    = cutDope(m.fightStats.atk).max(1L),
      energy = cutDope(m.fightStats.energy)))

  /** Число, срезанное дурманом ([[CaveRates.DopeCutPct]]). */
  def cutDope(value: Long): Long = value * (100L - CaveRates.DopeCutPct) / 100L

  /** Глефовый гриб: отравлены все, и тот, кто в паре, и те, кто в строю. */
  def poisonAll(battle: SoloPveBattle): SoloPveBattle = {
    val poison = Some(Poison.onHit)
    val paired = battle.withEffects(battle.effects.copy(monsterPoison = poison))
    paired.copy(group = paired.group.copy(others =
      paired.group.others.map(s => s.copy(effects = s.effects.copy(monsterPoison = poison)))))
  }

  /** Горят все — и в паре, и в строю — пламенем силы `pct`%. */
  def burnAll(battle: SoloPveBattle, pct: Int): SoloPveBattle = {
    val burn   = Burn.of(pct)
    val paired = battle.withEffects(battle.effects.copy(monsterBurn = burn))
    paired.copy(group = paired.group.copy(others =
      paired.group.others.map(s => s.copy(effects = s.effects.copy(monsterBurn = burn)))))
  }
}
