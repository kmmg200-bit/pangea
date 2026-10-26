package pangea.model.monster

import io.circe.syntax.EncoderOps
import pangea.model.home.HomeData
import pangea.model.item.{Item, ItemDetails, ItemType}
import zio.test._

/** Уникальные трофеи именных: у каждого из 96 — свой, с его именем; стена
  * считает прибавку по расам, каждого именного — один раз. */
object NamedTrophySpec extends ZIOSpecDefault {

  private val allNames = Monster.legendaryNames.values.flatten.toList

  override def spec = suite("Уникальные трофеи именных")(

    test("у каждого именного свой трофей с его именем — и только у них") {
      assertTrue(allNames.size == 96 && NamedTrophy.titles.size == 96) &&
        assertTrue(NamedTrophy.titles.keySet == allNames.toSet) &&
        assertTrue(NamedTrophy.titles.values.toSet.size == 96) &&
        assertTrue(NamedTrophy.titles("Каркан Кровавый Топор") == "Ухо Каркана Кровавого Топора") &&
        assertTrue(NamedTrophy.titles("Шныга Трёхпалый") == "Палец Шныги Трёхпалого") &&
        assertTrue(NamedTrophy.titles("Шмыг Кишкодёр") == "Ножик Шмыга Кишкодёра")
    },

    test("трофей — уникальный, без уровня в названии; чужая раса и незнакомое имя ничего не дают") {
      val ear = NamedTrophy.item("Каркан Кровавый Топор", Race.Orc.entryName, 7L)
      assertTrue(ear.exists(i => i.itemType == ItemType.NamedTrophy && i.name == "Ухо Каркана Кровавого Топора" &&
          i.namedTrophy.contains(ItemDetails.NamedTrophy("Каркан Кровавый Топор", Race.Orc.entryName)) && i.lvl == 7L)) &&
        assertTrue(ear.map(_.displayTitle).contains("🏆 Ухо Каркана Кровавого Топора")) &&
        assertTrue(NamedTrophy.item("Каркан Кровавый Топор", Race.Goblin.entryName, 7L).isEmpty) &&
        assertTrue(NamedTrophy.item("Коллектор", Race.Human.entryName, 7L).isEmpty) &&
        // запись читается обратно той же
        assertTrue(ear.map(_.asJson).flatMap(_.as[Item].toOption) == ear)
    },

    test("стена: по проценту за именного его расы, повторы не в счёт; без острова — ничего") {
      val wall   = List("Каркан Кровавый Топор", "Грумм Две Косы", "Шныга Трёхпалый", "Грумм Две Косы")
      val island = HomeData(tier = 3, wall = wall)
      assertTrue(NamedTrophy.pctByRace(wall) == Map(Race.Orc.entryName -> 2, Race.Goblin.entryName -> 1)) &&
        assertTrue(island.wallPct == Map(Race.Orc.entryName -> 2, Race.Goblin.entryName -> 1) && island.hasWall) &&
        assertTrue(HomeData(tier = 2, wall = wall).wallPct.isEmpty) &&
        // старая запись дома — без стены
        assertTrue(io.circe.jawn.decode[HomeData]("""{"tier":3}""").map(_.wall) == Right(Nil)) &&
        assertTrue(island.asJson.as[HomeData].map(_.wall) == Right(wall))
    }
  )
}
