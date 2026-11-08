package pangea.model.item

import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
import pangea.model.skill.Skill
import zio.test._

/** Стопка: что в неё собирается, как она лежит в базе и как меняет места в
  * сумке, когда её собирают и распаковывают. */
object BundlingSpec extends ZIOSpecDefault {

  private def mithril(id: Long): Item = MaterialGenerator.item(MaterialKind.Mithril).copy(id = id)
  private def dust(id: Long): Item    = MaterialGenerator.item(MaterialKind.RubyDust).copy(id = id)
  private def ruby(id: Long): Item    = GemGenerator.item(GemKind.Ruby, 1).copy(id = id)

  private def sword(id: Long): Item =
    Item(id, s"Меч $id", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  private def trophy(id: Long): Item =
    Item(id, "Голова (Человек)", 5L, Rarity.Gray, ItemType.Trophy, attack = 0, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0, details = ItemDetails.Trophy("Human", TrophyKind.Head))

  private def rune(id: Long, size: RuneStoneSize): Item =
    RuneStone.item(Rune.Active(Skill.CunningStrike), size).copy(id = id)

  /** Пять одинаковых — с номерами 101…105. */
  private def five(make: Long => Item): List[Item] = (101L to 105L).toList.map(make)

  private def swords(n: Int): List[Item] = (1 to n).toList.map(i => sword(200L + i))

  private def bag(items: List[Item]): Inventory = Inventory(1L, HeroId(1L), 20L, Inventory.Items(items))

  override def spec = suite("Стопка")(

    test("собираются камни, материалы, пыль, отвары и большие руны; трофеи, малые руны, снаряжение и стопки — нет") {
      val bundle = Bundling.pack(five(mithril)).get
      assertTrue(
        Bundling.packable(ruby(1L)),
        Bundling.packable(mithril(1L)),
        Bundling.packable(dust(1L)),
        Bundling.packable(BrewKind.item(BrewKind.LivingWater)),
        Bundling.packable(rune(1L, RuneStoneSize.Big)),
        !Bundling.packable(trophy(1L)),
        !Bundling.packable(rune(1L, RuneStoneSize.Small)),
        !Bundling.packable(sword(1L)),
        !Bundling.packable(bundle)
      )
    },

    test("в стопке ровно пять одинаковых: четыре, вперемешку или трофеи не собираются") {
      assertTrue(
        Bundling.pack(five(mithril).take(4)).isEmpty,
        Bundling.pack(five(mithril).take(4) :+ ruby(9L)).isEmpty,
        Bundling.pack(five(trophy)).isEmpty,
        Bundling.pack(five(mithril)).exists(b => b.isBundle && Bundling.contents(b) == five(mithril))
      )
    },

    test("название, уровень, редкость и описание — от того, что внутри") {
      val b = Bundling.pack(five(mithril)).get
      assertTrue(
        b.name == "Стопка: Мифрил ×5",
        b.displayTitle == "📦 Мифрил",
        b.itemType == ItemType.Bundle,
        b.lvl == mithril(1L).lvl && b.rarity == mithril(1L).rarity,
        b.statsLines == mithril(1L).statsLines,
        !ItemType.equippable.contains(ItemType.Bundle)
      )
    },

    test("одинаковые стопки — одной строкой списка, как одинаковые камни") {
      val a = Bundling.pack(five(mithril)).get.copy(id = 1L)
      val b = Bundling.pack((301L to 305L).toList.map(mithril)).get.copy(id = 2L)
      val c = Bundling.pack(five(ruby)).get.copy(id = 3L)
      assertTrue(ItemStack.grouped(List(a, c, b)).map { case (i, n) => i.id -> n } == List(2L -> 2, 3L -> 1))
    },

    test("шкаф стопку берёт, ларец и Живая сумка — нет, даже если внутри камни или травы") {
      val gems  = Bundling.pack(five(ruby)).get
      val herbs = Bundling.pack(five(id => MaterialGenerator.item(MaterialKind.values.find(_.isHerb).get).copy(id = id))).get
      assertTrue(
        ArtifactKind.Wardrobe.accepts(gems) && ArtifactKind.Wardrobe.accepts(herbs),
        !ArtifactKind.Casket.accepts(gems),
        !ArtifactKind.LivingBag.accepts(herbs),
        ArtifactKind.forItem(gems).isEmpty && ArtifactKind.forItem(herbs).isEmpty
      )
    },

    test("стопка пишется в базу и читается обратно как была — со всем, что внутри") {
      val b = Bundling.pack(five(ruby)).get.copy(id = 7L)
      assertTrue(decode[Item](b.asJson.noSpaces) == Right(b))
    },

    test("образец записи стопки читается") {
      def inner(id: Long) =
        s"""{"id":$id,"name":"Мифрил","lvl":1,"rarity":"Gray","itemType":"Material","attack":0,"accuracy":0,
           |"energy":0,"armor":0,"defence":0,"evasion":0,"hp":0,"details":{"type":"Material","kind":"Mithril"},
           |"sockets":[]}""".stripMargin
      val json =
        s"""{"id":7,"name":"Стопка: Мифрил ×5","lvl":1,"rarity":"Gray","itemType":"Bundle","attack":0,
           |"accuracy":0,"energy":0,"armor":0,"defence":0,"evasion":0,"hp":0,
           |"details":{"type":"Bundle","items":[${(101L to 105L).map(inner).mkString(",")}]},"sockets":[]}""".stripMargin
      assertTrue(decode[Item](json) == Right(Bundling.pack(five(mithril)).get.copy(id = 7L)))
    },

    suite("Места в сумке")(

      test("собрать — пять мест становятся одним") {
        val inv  = bag(five(mithril) ++ swords(15))
        val pack = Bundling.pack(five(mithril)).get
        assertTrue(inv.swapped(five(mithril), List(pack)).map(_.occupied) == Right(16L))
      },

      test("распаковать — нужно ещё четыре места; меньше — отказ") {
        val pack = Bundling.pack(five(mithril)).get
        assertTrue(
          bag(pack :: swords(15)).swapped(List(pack), five(mithril)).map(_.occupied) == Right(20L),
          bag(pack :: swords(16)).swapped(List(pack), five(mithril)) == Left(Inventory.SwapRefusal.NoRoom)
        )
      },

      test("пыль: стопке нужно место, а распаковке — запас до предела горстей") {
        val pack = Bundling.pack(five(dust)).get
        val many = (1 to 96).toList.map(i => dust(400L + i))
        assertTrue(
          bag(five(dust) ++ swords(20)).swapped(five(dust), List(pack)) == Left(Inventory.SwapRefusal.NoRoom),
          bag(five(dust) ++ swords(19)).swapped(five(dust), List(pack)).isRight,
          // 96 горстей и ещё пять из стопки — больше сотни
          bag(pack :: many).swapped(List(pack), five(dust)) == Left(Inventory.SwapRefusal.HoardFull),
          // распаковка пыли место освобождает — пройдёт и в полной сумке
          bag(pack :: (many.drop(1) ++ swords(19))).swapped(List(pack), five(dust)).map(_.occupied) == Right(19L)
        )
      },

      test("того, что убираем, в сумке нет — обмена нет") {
        val pack = Bundling.pack(five(mithril)).get
        assertTrue(bag(five(mithril).take(4)).swapped(five(mithril), List(pack)) == Left(Inventory.SwapRefusal.Missing))
      }
    )
  )
}
