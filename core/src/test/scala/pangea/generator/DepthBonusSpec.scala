package pangea.generator

import pangea.domain.Rng
import pangea.generator.monster.{DepthRates, MonsterGenerator}
import pangea.model.monster.{Race, Rarity}
import pangea.model.skill.MonsterEnergy
import zio.test._

/** Глубина (решение пользователя): с 26-го уровня моба к каждой его базовой
  * ставке +2 — до умножения на уровень, редкость и расу. По уровню самого моба:
  * в лабиринте, в заданиях, контрактах, событиях — одинаково. */
object DepthBonusSpec extends ZIOSpecDefault {

  /** Ставки растут с уровнем линейно: без прибавки 125-й уровень сильнее 25-го
    * ровно впятеро, а с ней — ещё во столько, во сколько ставка с +2 больше
    * прежней. */
  private def grown(c: Double): Double = 5.0 * (c + DepthRates.DeepBonus) / c

  private def near(x: Double, y: Double): Boolean = math.abs(x - y) < 0.04

  def spec = suite("Глубина: +2 к базовым ставкам мобов с 26-го уровня")(
    test("к каждой базовой ставке +2: атака 10→12, HP 40→42, броня 23→25, защита 7→9, уклонение 16,25→18,25, точность 8,25→10,25; энергия та же") {
      val at25  = MonsterGenerator.generateOfRaceAndRarity(25, Race.Human, Rarity.Rare).fightStats
      val at125 = MonsterGenerator.generateOfRaceAndRarity(125, Race.Human, Rarity.Rare).fightStats
      def ratio(a: Long, b: Long): Double = a.toDouble / b.toDouble
      assertTrue(
        near(ratio(at125.atk, at25.atk), grown(10.0)),
        near(ratio(at125.hp, at25.hp), grown(40.0)),
        near(ratio(at125.armor, at25.armor), grown(23.0)),
        near(ratio(at125.defence, at25.defence), grown(7.0)),
        near(ratio(at125.evasion, at25.evasion), grown(16.25)),
        near(ratio(at125.accuracy, at25.accuracy), grown(8.25)),
        MonsterGenerator.generateOfRaceAndRarity(30, Race.Human, Rarity.Rare).fightStats.energy == MonsterEnergy.maxEnergy(30L)
      )
    },
    test("на 25-м — без прибавки, с 26-го и глубже — с ней: шаг с 25-го на 26-й — не 4%, а вчетверо с лишним больше") {
      val at24 = MonsterGenerator.generateOfRaceAndRarity(24, Race.Orc, Rarity.Common).fightStats
      val at25 = MonsterGenerator.generateOfRaceAndRarity(25, Race.Orc, Rarity.Common).fightStats
      val at26 = MonsterGenerator.generateOfRaceAndRarity(26, Race.Orc, Rarity.Common).fightStats
      assertTrue(
        DepthRates.bonus(25) == 0.0 && DepthRates.bonus(26) == 2.0 && DepthRates.bonus(150) == 2.0,
        at25.atk.toDouble / at24.atk < 1.06,
        at26.atk.toDouble / at25.atk > 1.2
      )
    },
    test("моб задания того же уровня — как моб лабиринта; страж-«Отмеченный» — с прибавкой и ещё +20%") {
      val mobs   = (1L to 40L).toList.map(s => MonsterGenerator.generate(30, Rng(s))._1).filterNot(_.marked)
      val (g, _) = MonsterGenerator.generateMarked(26, Rng(5L))
      val base26 = MonsterGenerator.generateOfRaceAndRarity(26, g.race, g.rarity).fightStats
      assertTrue(
        mobs.nonEmpty,
        mobs.forall(m => m.fightStats == MonsterGenerator.generateOfRaceAndRarity(30, m.race, m.rarity).fightStats),
        g.fightStats.atk == (base26.atk * 1.2).toLong.max(1L) && g.fightStats.hp == (base26.hp * 1.2).toLong.max(1L)
      )
    }
  )
}
