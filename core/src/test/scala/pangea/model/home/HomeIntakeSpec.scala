package pangea.model.home

import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.item.{BrewKind, Bundling, GemKind, Item, ItemType, MaterialKind, Rarity}
import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
import pangea.model.skill.Skill
import zio.test._

/** Что из пришедшего посылкой дом берёт сразу: руны — на алтарь в любом доме,
  * травы и отвары — на верстак травника со второго дома, камни — в хранилище
  * на острове, пока есть место. Стопки, сюжетное и всё прочее — мимо дома. */
object HomeIntakeSpec extends ZIOSpecDefault {

  private val cunning = Rune.Active(Skill.CunningStrike)

  private def herb(id: Long): Item  = MaterialGenerator.item(MaterialKind.Chamomile).copy(id = id)
  private def brew(id: Long): Item  = BrewKind.item(BrewKind.LivingWater).copy(id = id)
  private def ruby(id: Long): Item  = GemGenerator.item(GemKind.Ruby, 1).copy(id = id)
  private def rune(id: Long, size: RuneStoneSize = RuneStoneSize.Big): Item = RuneStone.item(cunning, size).copy(id = id)
  private def sword(id: Long): Item =
    Item(id, "Меч", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  private val cottage   = HomeData(tier = 1, metJunona = true)
  private val townhouse = HomeData(tier = 2, metJunona = true)
  private val island    = HomeData(tier = 3, metJunona = true)

  private def taken(home: HomeData, items: Item*): List[(Long, HomePlace)] =
    HomeIntake.of(home, items.toList).taken.map { case (item, place) => item.id -> place }

  override def spec = suite("Посылки — сразу в дом")(

    test("дома нет — ничего не берётся") {
      val r = HomeIntake.of(HomeData.empty, List(rune(1L), herb(2L), brew(3L), ruby(4L)))
      assertTrue(r.taken.isEmpty && r.home == HomeData.empty)
    },

    test("домик: только руны — на алтарь (большая — 5, малая — 1); травы, отвары и камни мимо") {
      val r = HomeIntake.of(cottage, List(rune(1L), rune(2L, RuneStoneSize.Small), herb(3L), brew(4L), ruby(5L)))
      assertTrue(r.taken.map { case (i, p) => i.id -> p } == List(1L -> HomePlace.Runes, 2L -> HomePlace.Runes)) &&
      assertTrue(r.home.runeCount(cunning.key) == 6L && r.home.herbs.isEmpty && r.home.brews.isEmpty && r.home.gems.isEmpty)
    },

    test("добротный дом: травы и отвары — на верстак, пока есть место; камни мимо") {
      val crowded = townhouse.copy(herbs = List.tabulate(townhouse.herbShelves - 1)(i => herb(1000L + i)))
      val r       = HomeIntake.of(crowded, List(herb(1L), herb(2L), brew(3L), ruby(4L)))
      assertTrue(r.taken.map { case (i, p) => i.id -> p } == List(1L -> HomePlace.Herbs, 3L -> HomePlace.Brews)) &&
      assertTrue(r.home.herbs.size == townhouse.herbShelves && r.home.brews.map(_.id) == List(3L) && r.home.gems.isEmpty)
    },

    test("полка отваров полна — отвар мимо дома") {
      val full = townhouse.copy(brews = List.tabulate(townhouse.brewShelves)(i => brew(1000L + i)))
      assertTrue(taken(full, brew(1L)).isEmpty)
    },

    test("остров: камни — в хранилище, пока есть место") {
      val crowded = island.copy(gems = List.tabulate(island.gemShelves - 1)(i => ruby(1000L + i)))
      assertTrue(taken(crowded, ruby(1L), ruby(2L)) == List(1L -> HomePlace.Gems)) &&
      assertTrue(taken(island, herb(1L), brew(2L), ruby(3L), rune(4L)) ==
        List(1L -> HomePlace.Herbs, 2L -> HomePlace.Brews, 3L -> HomePlace.Gems, 4L -> HomePlace.Runes))
    },

    test("берёт ли дом вещь вообще — по одному виду, без дома и мест") {
      val stack = Bundling.pack((101L to 105L).toList.map(rune(_))).get
      assertTrue(List(rune(1L), herb(2L), brew(3L), ruby(4L)).forall(HomeIntake.wanted)) &&
      assertTrue(!HomeIntake.wanted(sword(5L)) && !HomeIntake.wanted(stack) &&
                 !HomeIntake.wanted(rune(6L).copy(story = Some(true))))
    },

    test("стопки, сюжетное и прочее — мимо дома, даже руны") {
      val stack = Bundling.pack((101L to 105L).toList.map(rune(_))).get
      assertTrue(taken(island, stack, rune(1L).copy(story = Some(true)), sword(2L)).isEmpty)
    }
  )
}
