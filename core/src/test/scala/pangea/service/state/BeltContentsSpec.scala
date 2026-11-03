package pangea.service.state

import pangea.model.item.{BeltSlot, BrewKind, FlaskKind, Item, ItemDetails, ItemType, MaterialKind, Rarity}
import zio.test._

/** Содержимое пояса за героем: переезд в новый пояс и выкладывание при снятии. */
object BeltContentsSpec extends ZIOSpecDefault {

  private def belt(id: Long, rarity: Rarity, slots: List[BeltSlot] = Nil): Item =
    Item(id, s"Пояс $id", 1L, rarity, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0, details = ItemDetails.Belt(slots))

  private val flask = BeltSlot.Flask(FlaskKind.Healer, Rarity.Blue, 3)
  private val dust  = BeltSlot.Dust(MaterialKind.RubyDust)
  private val drunk = BeltSlot.Brew(BrewKind.WolfCall, count = 0)

  override def spec = suite("BeltContents")(

    test("в пояс побольше переезжает всё, старый уходит пустым") {
      val s = BeltContents.swap(belt(1L, Rarity.Green, List(flask, dust)), belt(2L, Rarity.Blue))
      assertTrue(BeltContents.slotsOf(s.next) == List(flask, dust)) &&
      assertTrue(s.next.id == 2L && BeltContents.slotsOf(s.worn).isEmpty && s.worn.id == 1L) &&
      assertTrue(s.spilled.isEmpty && s.moved)
    },

    test("в пояс поменьше — полные ячейки первыми; лишнее вещами в сумку, лишняя пустая пропадает") {
      val s = BeltContents.swap(belt(1L, Rarity.Blue, List(drunk, dust, flask)), belt(2L, Rarity.Gray))
      assertTrue(BeltContents.slotsOf(s.next) == List(dust)) &&
      // фляга вернулась флягой со своими зарядами, выпитый отвар — пустая ячейка, его нет
      assertTrue(s.spilled.map(_.name) == List(FlaskKind.Healer.itemName(Rarity.Blue))) &&
      assertTrue(s.spilled.flatMap(_.details match {
        case f: ItemDetails.Flask => List(f.charges)
        case _                    => Nil
      }) == List(3))
    },

    test("то, что уже лежало в новом поясе, встаёт после переложенного") {
      val s = BeltContents.swap(belt(1L, Rarity.Gray, List(flask)), belt(2L, Rarity.Green, List(dust)))
      assertTrue(BeltContents.slotsOf(s.next) == List(flask, dust)) &&
      assertTrue(s.spilled.isEmpty)
    },

    test("со старого пустого пояса перекладывать нечего") {
      val s = BeltContents.swap(belt(1L, Rarity.Gray), belt(2L, Rarity.Green))
      assertTrue(!s.moved && s.spilled.isEmpty && BeltContents.slotsOf(s.next).isEmpty)
    },

    test("снятие: пояс пустой, содержимое — вещами; пустая фляга остаётся флягой, высыпанная пыль — ничем") {
      val emptyFlask = flask.copy(charges = 0)
      val (bare, items) = BeltContents.strip(belt(1L, Rarity.Blue, List(emptyFlask, dust.withCount(0), dust)))
      assertTrue(BeltContents.slotsOf(bare).isEmpty && bare.id == 1L) &&
      assertTrue(items.map(_.name) == List(FlaskKind.Healer.itemName(Rarity.Blue), MaterialKind.RubyDust.displayName))
    },

    // ── Стопки: пыль, отвар, святая вода ─────────────────────────────────────
    test("стопка в ячейке — столько, сколько у пояса ячеек: у белого одна, у легендарного пять") {
      assertTrue(ItemDetails.Belt.stackFor(Rarity.White) == 1 && ItemDetails.Belt.stackFor(Rarity.Green) == 2) &&
      assertTrue(ItemDetails.Belt.stackFor(Rarity.Orange) == 5) &&
      assertTrue((Rarity.values: Seq[Rarity]).forall(r => ItemDetails.Belt.stackFor(r) == ItemDetails.Belt.capacityFor(r)))
    },

    test("стопку снимают целиком: в сумку — все штуки, что в ней остались") {
      val (_, items) = BeltContents.strip(belt(1L, Rarity.Orange,
        List(BeltSlot.Brew(BrewKind.WortDecoction, 3), BeltSlot.HolyWater(2), BeltSlot.Dust(MaterialKind.RubyDust, 0))))
      assertTrue(items.count(_.brew.contains(BrewKind.WortDecoction)) == 3) &&
      assertTrue(items.count(_.material.contains(MaterialKind.HolyWater)) == 2) &&
      assertTrue(items.size == 5) &&
      assertTrue(BeltContents.names(items) == s"${BrewKind.WortDecoction.label} ×3, ${MaterialKind.HolyWater.displayName} ×2")
    },

    test("в пояс поменьше стопка переезжает своей новой меркой — лишнее в сумку") {
      val s = BeltContents.swap(belt(1L, Rarity.Orange, List(BeltSlot.Brew(BrewKind.WortDecoction, 5))),
                                belt(2L, Rarity.Green))
      assertTrue(BeltContents.slotsOf(s.next) == List(BeltSlot.Brew(BrewKind.WortDecoction, 2))) &&
      assertTrue(s.spilled.size == 3 && s.spilled.forall(_.brew.contains(BrewKind.WortDecoction)))
    },

    test("доливка: неполная ячейка того же вида — сперва она, новой ячейки не нужно; иначе новая") {
      val b = ItemDetails.Belt(List(flask, BeltSlot.Brew(BrewKind.WortDecoction, 1)))
      val unit = BeltSlot.Brew(BrewKind.WortDecoction)
      assertTrue(b.putStack(unit, 10, 5, 5).contains((ItemDetails.Belt(List(flask, BeltSlot.Brew(BrewKind.WortDecoction, 5))), 4))) &&
      assertTrue(b.putStack(unit, 2, 5, 5).map(_._2).contains(2)) &&
      // другой отвар — новая ячейка; места нет — None
      assertTrue(b.putStack(BeltSlot.Brew(BrewKind.CatBrew), 9, 3, 3)
        .contains((ItemDetails.Belt(b.slots :+ BeltSlot.Brew(BrewKind.CatBrew, 3)), 3))) &&
      assertTrue(b.putStack(BeltSlot.Brew(BrewKind.CatBrew), 9, 2, 2).isEmpty)
    },

    test("старая запись ячейки (потрачено / нет) читается как стопка из нуля или одной") {
      import io.circe.syntax.EncoderOps
      val old  = io.circe.Json.obj("type" -> "Brew".asJson, "kind" -> "WortDecoction".asJson, "spent" -> true.asJson)
      val fresh = io.circe.Json.obj("type" -> "Dust".asJson, "kind" -> "RubyDust".asJson, "spent" -> false.asJson)
      val holy = io.circe.Json.obj("type" -> "Holy".asJson)
      assertTrue(old.as[BeltSlot] == Right(BeltSlot.Brew(BrewKind.WortDecoction, 0))) &&
      assertTrue(fresh.as[BeltSlot] == Right(BeltSlot.Dust(MaterialKind.RubyDust, 1))) &&
      assertTrue(holy.as[BeltSlot] == Right(BeltSlot.HolyWater(1))) &&
      assertTrue((BeltSlot.Brew(BrewKind.CatBrew, 4): BeltSlot).asJson.as[BeltSlot] == Right(BeltSlot.Brew(BrewKind.CatBrew, 4)))
    }
  )
}
