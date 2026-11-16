package pangea.model.battle

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax.EncoderOps
import pangea.model.hero.{Equipment, Hero, MasterHornBoosts, WeaponDust}
import pangea.model.monster.Race
import pangea.model.rune.RuneData
import pangea.model.squad.Squad
import pangea.model.stats.{BaseStats, FightStats, StatBoosts}

/** Тело, в котором герой дерётся этот бой, — не его собственное: в прологе это
  * Лео, Тень Братства, ассасин 25-го уровня, ещё до кинжала в спину. Тело едет
  * в записи боя: бой читает героя через него ([[on]]), ход пишет назад в него
  * же ([[after]]). Настоящий герой этого боя не касается — ни статы, ни вещи,
  * ни опыт, ни убитые, ни травмы; кончился бой (или его сбросил /home) — тело
  * уходит вместе с записью.
  *
  * `sparePct` — страховка: HP героя в этом теле не опускается ниже такой доли
  * максимума, и смерти в таком бою нет (см. BattleState.spared); ниже двух
  * таких долей удары за ход вдвое мягче (BattleState.cushioned). 0 — тело
  * падает, как обычное. */
final case class BattleBody(
  lvl:        Long,
  race:       Race,
  baseStats:  BaseStats,
  fightStats: FightStats,
  equipment:  Equipment,
  runes:      RuneData   = RuneData.empty,
  statBoosts: StatBoosts = StatBoosts.none,
  weaponDust: WeaponDust = WeaponDust.empty,
  sparePct:   Long       = 0L
) {

  /** Настоящий герой в этом теле: id, имя и прочее — его, сила — тела. Ни
    * отряда, ни травм, ни достижений, ни прокачки у Горна: у тела своё
    * прошлое, и чужое ему ни к чему. */
  def on(hero: Hero): Hero = hero.copy(
    lvl              = lvl,
    exp              = 0L,
    upgradePoints    = 0L,
    race             = race,
    baseStats        = baseStats,
    fightStats       = fightStats,
    equipment        = equipment,
    runes            = runes,
    statBoosts       = statBoosts,
    weaponDust       = weaponDust,
    squad            = Squad.empty,
    traumaUntil      = None,
    traumaNames      = Nil,
    achievements     = Nil,
    masterHornBoosts = MasterHornBoosts.empty,
    expFrozen        = false)

  /** Что ход сделал с телом: HP, броня, энергия, пояс, бафы, смазка. */
  def after(hero: Hero): BattleBody = copy(
    fightStats = hero.fightStats,
    equipment  = hero.equipment,
    statBoosts = hero.statBoosts,
    weaponDust = hero.weaponDust)
}

object BattleBody {

  implicit val encoder: Encoder[BattleBody] = (b: BattleBody) => Json.obj(
    "lvl"        -> b.lvl.asJson,
    "race"       -> b.race.entryName.asJson,
    "baseStats"  -> b.baseStats.asJson,
    "fightStats" -> b.fightStats.asJson,
    "equipment"  -> b.equipment.asJson,
    "runes"      -> b.runes.asJson,
    "statBoosts" -> b.statBoosts.asJson,
    "weaponDust" -> b.weaponDust.asJson,
    "sparePct"   -> b.sparePct.asJson)

  implicit val decoder: Decoder[BattleBody] = (c: HCursor) =>
    for {
      lvl        <- c.get[Long]("lvl")
      race       <- c.get[String]("race").map(r => Race.withNameOption(r).getOrElse(Race.Human))
      baseStats  <- c.get[BaseStats]("baseStats")
      fightStats <- c.get[FightStats]("fightStats")
      equipment  <- c.get[Equipment]("equipment")
      runes      <- c.getOrElse[RuneData]("runes")(RuneData.empty)
      statBoosts <- c.getOrElse[StatBoosts]("statBoosts")(StatBoosts.none)
      weaponDust <- c.getOrElse[WeaponDust]("weaponDust")(WeaponDust.empty)
      sparePct   <- c.getOrElse[Long]("sparePct")(0L)
    } yield BattleBody(lvl, race, baseStats, fightStats, equipment, runes, statBoosts, weaponDust, sparePct)
}
