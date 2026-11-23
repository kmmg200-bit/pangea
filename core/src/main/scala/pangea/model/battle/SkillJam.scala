package pangea.model.battle

/** Почему сорвалось следующее умение: яд красного сетеплёта или громовой
  * раскат. В базе — строка; старые бои с одним `heroSkillJammed: true`
  * читаются как паук. */
object SkillJam {
  val Spider:  String = "spider"
  val Thunder: String = "thunder"

  def isThunder(kind: Option[String]): Boolean = kind.contains(Thunder)
}