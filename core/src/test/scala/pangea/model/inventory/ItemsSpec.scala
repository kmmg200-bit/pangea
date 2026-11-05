package pangea.model.inventory

import pangea.generator.item.MaterialGenerator
import pangea.model.inventory.Inventory.Items
import pangea.model.item.MaterialKind
import zio.test._

/** Снять из хранилища одну вещь по номеру — именно одну, даже если номер
  * разделили несколько не сохранённых в базу (−1). */
object ItemsSpec extends ZIOSpecDefault {

  private val sapphire = MaterialGenerator.item(MaterialKind.SapphireDust)
  private val ruby     = MaterialGenerator.item(MaterialKind.RubyDust)

  override def spec = suite("Items.withoutOne")(
    test("общий номер −1: снимается первая, вторая остаётся") {
      assertTrue(Items(List(sapphire, ruby)).withoutOne(-1L) == List(ruby))
    },
    test("номера нет — всё на месте; свой номер — только эта вещь") {
      val a = sapphire.copy(id = 5L)
      val b = ruby.copy(id = 6L)
      assertTrue(Items(List(a, b)).withoutOne(7L) == List(a, b)) &&
      assertTrue(Items(List(a, b)).withoutOne(6L) == List(a))
    }
  )
}
