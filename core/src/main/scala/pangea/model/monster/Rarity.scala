package pangea.model.monster

import enumeratum.{DoobieEnum, _}

sealed trait Rarity extends EnumEntry {
  val factor: Double

  /** Ранг моба: 1 — самый слабый, 5 — именной. На кнопке выбора цели моба зовут
    * «Раса-ранг» (см. `MonsterSlot.targetName`). */
  val rank: Int
}

object Rarity extends Enum[Rarity] with DoobieEnum[Rarity] {

  val values = findValues

  case object Common extends Rarity {
    override def toString: String = "Обычный"
    val factor                    = 0.8
    val rank                      = 1
  }

  case object Uncommon extends Rarity {
    override def toString: String = "Необычный"
    val factor                    = 1.2
    val rank                      = 2
  }

  case object Rare extends Rarity {
    override def toString: String = "Редкий"
    val factor                    = 1.6
    val rank                      = 3
  }

  case object Mythical extends Rarity {
    override def toString: String = "Мифический"
    val factor                    = 2.0
    val rank                      = 4
  }

  case object Legendary extends Rarity {
    override def toString: String = "Легендарный"
    val factor                    = 3
    val rank                      = 5
  }
}
