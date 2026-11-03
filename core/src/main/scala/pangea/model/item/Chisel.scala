package pangea.model.item

import pangea.model.skill.Skill

/** «Резец рунописца» ([[MaterialKind.RuneChisel]]): переписывает узор на вещи —
  * активное умение оружия и нагрудника или пассивку прочего снаряжения — на
  * другой из тех, что бывают у вещей этого вида. Нынешний узор в розыгрыш не
  * входит. Вещь без пассивки, у которой она бывает, получает новую.
  *
  * Щит и парирующий кинжал резцу не даются: их умение задано самой вещью (см.
  * `Skill.handSkills`). Не даются и божественное оружие, фляги, пояс — узора
  * на них нет. */
object Chisel {

  /** Во что может превратиться вещь: та же вещь, но с другим узором. Пусто —
    * резцу здесь делать нечего. */
  def variants(item: Item): List[Item] = item.details match {
    case ItemDetails.Weapon(skill) =>
      val pool = if (item.ranged) Skill.rangedSkills else Skill.weaponSkills
      pool.filterNot(_ == skill).map(s => item.copy(details = ItemDetails.Weapon(s)))
    case ItemDetails.Armor(skill) =>
      Skill.armorSkills.filterNot(_ == skill).map(s => item.copy(details = ItemDetails.Armor(s)))
    case ItemDetails.Passive(kind) =>
      PassiveKind.poolFor(item.itemType).filterNot(_ == kind).map(k => item.copy(details = ItemDetails.Passive(k)))
    case ItemDetails.Plain if ItemType.equippable.contains(item.itemType) =>
      PassiveKind.poolFor(item.itemType).map(k => item.copy(details = ItemDetails.Passive(k)))
    case _ => Nil
  }

  /** Можно ли переписать узор на этой вещи. */
  def fits(item: Item): Boolean = variants(item).nonEmpty

  /** Как зовётся узор вещи сейчас — умение или пассивка; у вещи без узора — None. */
  def pattern(item: Item): Option[String] =
    item.activeSkill.map(_.label).orElse(item.passive.map(_.label))
}
