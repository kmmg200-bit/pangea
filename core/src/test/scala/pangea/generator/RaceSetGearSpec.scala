package pangea.generator

import pangea.domain.Rng
import pangea.generator.loot.LootGenerator
import pangea.model.item.{ItemSet, Rarity}
import pangea.model.monster.Race
import zio.test._

/** Вещь малого расового набора: её роняет легендарный моб своей расы. */
object RaceSetGearSpec extends ZIOSpecDefault {

  override def spec = suite("Вещь малого расового набора")(

    test("набор — расы моба, уровень — добычи, имя набора вместо титула; у рас без набора — ничего") {
      val drops = Race.mortals.toList.map(r => r -> LootGenerator.raceSetGear(r, 37L, Rng(5L))._1)
      assertTrue(drops.forall { case (r, it) => it.exists(_.set == ItemSet.ofRace(r)) }) &&
      assertTrue(drops.forall { case (_, it) => it.exists(_.lvl == 37L) }) &&
      assertTrue(drops.forall { case (_, it) => it.exists(i => i.set.exists(s => i.name.endsWith(s.title))) }) &&
      assertTrue(LootGenerator.raceSetGear(Race.Elemental, 37L, Rng(5L))._1.isEmpty)
    },

    test("редкость: зелёная 30, синяя 25, фиолетовая 22, пурпурная 22, легендарная 1 — и никаких других") {
      val items = (1L to 20000L).toList.flatMap(s => LootGenerator.raceSetGear(Race.Orc, 20L, Rng(s))._1)
      def pct(r: Rarity): Double = items.count(_.rarity == r).toDouble * 100.0 / items.size
      val allowed: Set[Rarity] = Set(Rarity.Green, Rarity.Blue, Rarity.Purple, Rarity.Violet, Rarity.Orange)
      assertTrue(items.size == 20000 && items.forall(i => allowed.contains(i.rarity))) &&
      assertTrue(LootGenerator.RaceSetRarityWeights.map(_._2).sum == 100) &&
      assertTrue(pct(Rarity.Green) > 28.5 && pct(Rarity.Green) < 31.5) &&
      assertTrue(pct(Rarity.Blue) > 23.5 && pct(Rarity.Blue) < 26.5) &&
      assertTrue(pct(Rarity.Purple) > 20.5 && pct(Rarity.Purple) < 23.5) &&
      assertTrue(pct(Rarity.Violet) > 20.5 && pct(Rarity.Violet) < 23.5) &&
      assertTrue(pct(Rarity.Orange) > 0.6 && pct(Rarity.Orange) < 1.5)
    }
  )
}
