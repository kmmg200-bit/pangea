package pangea.service.parcel

import pangea.engine.SceneContent
import pangea.generator.item.GemGenerator
import pangea.model.hero.HeroId
import pangea.model.item.{GemKind, Item, ItemType, Rarity}
import pangea.model.parcel.Parcel
import pangea.test.{TestBankRepository, TestParcelDao}
import zio.test._

object ParcelsReportSpec extends ZIOSpecDefault {

  private val content = SceneContent.load()
  private val parcels = Parcels(TestParcelDao.empty, TestBankRepository.withCells(2), content)

  private def skull(id: Long) = GemGenerator.item(GemKind.Skull, 1).copy(id = id)
  private val sword = Item(41L, "Меч", 5L, Rarity.Gray, ItemType.Weapon, attack = 3, accuracy = 0, energy = 0,
    armor = 0, defence = 0, evasion = 0)
  private def parcel(id: Long, from: String, item: Item) = Parcel(id, HeroId(1L), from, item, 0L)

  override def spec = suite("Письмо о посылках")(
    test("одинаковое от одного отправителя — одной строкой «×N», от разных — порознь, по порядку") {
      val landed = Parcels.Landing(burned = Nil, mailed = Nil, started = false, delivered = List(
        parcel(1L, "Алиса", skull(31L)), parcel(2L, "Алиса", sword), parcel(3L, "Борис", skull(33L)),
        parcel(4L, "Алиса", skull(32L))))
      val title = skull(0L).displayTitle
      assertTrue(parcels.report(landed).contains(content.format("parcel.delivered",
        "items" -> s"$title ×2 (Алиса), ${sword.displayTitle} (Алиса), $title (Борис)")))
    }
  )
}
