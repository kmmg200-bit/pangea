package pangea.model.item

/** Ловчая сеть — доп. оружие, сплетённое в кубе из паучьего шёлка и пеньки
  * (см. `CubeCraft`). В бою её бросают до [[Reach]] мест от себя: опутанный моб
  * раунд не сходит с места — как от «Пригвоздить» (`monsterRootedTurns`). Раз
  * за раунд, как и божественное оружие из того же слота, и раунд бросок не
  * завершает. Бросков [[Charges]], на последнем сеть рвётся. Так решил
  * пользователь. */
object TrapNet {

  val Name: String = "Ловчая сеть"

  val Description: String =
    "Липкая серая сеть с тяжёлой кромкой. Брошенная, она раскрывается на лету и " +
    "облепляет того, на кого упала: раунд он не сойдёт с места. Добросить её можно " +
    "на два шага — не дальше."

  /** Сколько бросков держит сеть. */
  val Charges: Int = 3

  /** Дальше скольких мест от героя сеть не долетает. */
  val Reach: Int = 2

  /** Сколько раундов держит опутанного. */
  val HoldRounds: Int = 1

  /** Новая сеть: уровня нет — надеть её может любой; статов не даёт. */
  def item: Item =
    Item(
      id       = -1L,
      name     = Name,
      lvl      = 1L,
      rarity   = Rarity.White,
      itemType = ItemType.AdditionalWeapon,
      attack   = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details  = ItemDetails.Net(Charges, Charges))
}
