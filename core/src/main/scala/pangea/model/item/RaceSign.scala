package pangea.model.item

import pangea.model.monster.Race

/** Знак рода — материал малого расового набора (см. `ItemSet.race`), по одному
  * на расу. Куб делает его из трёх вещей набора или из [[Talismans]] талисманов
  * этой расы, а знак с вещью даёт ту же вещь в наборе своей расы — как
  * ингредиент минибосса (см. `CubeCraft`). Так решил пользователь.
  *
  * Связь «раса — знак» держим здесь, а не на самих вариантах `MaterialKind`:
  * так при инициализации варианты не трогают ни свой companion, ни `Race`. */
object RaceSign {

  /** Сколько талисманов одной расы идёт на один знак. */
  val Talismans: Int = 5

  /** Раса → её знак. Ровно те расы, у которых есть малый набор. */
  val byRace: List[(Race, MaterialKind)] = List(
    Race.Human   -> MaterialKind.RaceSignHuman,
    Race.Elf     -> MaterialKind.RaceSignElf,
    Race.Murloc  -> MaterialKind.RaceSignMurloc,
    Race.Orc     -> MaterialKind.RaceSignOrc,
    Race.Goblin  -> MaterialKind.RaceSignGoblin,
    Race.Demon   -> MaterialKind.RaceSignDemon,
    Race.Gnome   -> MaterialKind.RaceSignGnome,
    Race.Khajiit -> MaterialKind.RaceSignKhajiit
  )

  /** Знак этой расы; у рас без малого набора его нет. */
  def of(race: Race): Option[MaterialKind] = byRace.collectFirst { case (r, k) if r == race => k }

  /** Чей это знак — если материал вообще знак рода. */
  def raceOf(kind: MaterialKind): Option[Race] = byRace.collectFirst { case (r, k) if k == kind => r }

  /** Знак набора: у малого расового — знак его расы, у прочих — нет. */
  def ofSet(set: ItemSet): Option[MaterialKind] = set.race.flatMap(of)

  /** Набор, в который знак переводит вещь. */
  def setOf(kind: MaterialKind): Option[ItemSet] = raceOf(kind).flatMap(ItemSet.ofRace)
}
