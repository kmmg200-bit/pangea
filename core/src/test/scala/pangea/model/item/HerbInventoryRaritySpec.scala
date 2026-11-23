package pangea.model.item

import zio.test._

import pangea.generator.item.MaterialGenerator

object HerbInventoryRaritySpec extends ZIOSpecDefault {

  override def spec = suite("HerbInventoryRarity")(
    test("травы 1-го ранга — белая редкость, 2-го — фиолетовая") {
      val r1 = MaterialGenerator.item(MaterialKind.Chamomile)
      val r2 = MaterialGenerator.item(MaterialKind.BubbleLily)
      val g1 = MaterialGenerator.item(MaterialKind.Mithril)
      assertTrue(
        r1.rarity == Rarity.White,
        r2.rarity == Rarity.Purple,
        MaterialGenerator.item(MaterialKind.StrangeFlower).rarity == Rarity.White,
        MaterialGenerator.item(MaterialKind.UnusualFlower).rarity == Rarity.Purple,
        g1.rarity == Rarity.Gray,
        Rarity.order(r1.inventoryRarity) < Rarity.order(r2.inventoryRarity)
      )
    },
    test("старая запись с Gray у травы — inventoryRarity по рангу") {
      val old = MaterialGenerator.item(MaterialKind.BubbleLily).copy(rarity = Rarity.Gray)
      assertTrue(old.inventoryRarity == Rarity.Purple)
    }
  )
}