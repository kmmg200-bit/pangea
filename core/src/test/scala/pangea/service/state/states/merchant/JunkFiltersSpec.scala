package pangea.service.state.states.merchant

import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import pangea.model.item.{Item, ItemDetails, ItemSet, ItemType, PassiveKind, Rarity}
import pangea.model.rune.RuneData
import pangea.model.user.UserId
import pangea.service.state.states.home.RuneAltarState
import pangea.test.TestFixtures
import zio.test._

/** Фильтры фляг и вещей наборов: в продаже хлама — оба, в сдаче на руны (алтарь
  * и Казимир) — только наборы: руны на фляге не бывает. */
object JunkFiltersSpec extends ZIOSpecDefault {

  private def thing(id: Long, t: ItemType, set: Option[ItemSet] = None): Item =
    Item(id, "Вещь", 10L, Rarity.Gray, t, attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Passive(PassiveKind.Hunter), set = set)

  private val flask   = thing(1L, ItemType.Flask)
  private val setHelm = thing(2L, ItemType.Helmet, Some(ItemSet.Hunter))
  private val helm    = thing(3L, ItemType.Helmet)

  override def spec = suite("Фильтры фляг и наборов")(

    test("хлам: фляги и вещи наборов по умолчанию идут как прежде, выключенный переключатель их бережёт") {
      val default = MerchantState.JunkSaleSettings()
      val noFlask = default.copy(flaskSale = Some(false))
      val noSets  = default.copy(setSale = Some(false))
      assertTrue(List(flask, setHelm, helm).forall(MerchantState.isJunk(_, default))) &&
      assertTrue(!MerchantState.isJunk(flask, noFlask) && MerchantState.isJunk(setHelm, noFlask)) &&
      assertTrue(!MerchantState.isJunk(setHelm, noSets) && MerchantState.isJunk(flask, noSets)) &&
      assertTrue(MerchantState.isJunk(helm, noFlask) && MerchantState.isJunk(helm, noSets))
    },

    test("старая настройка хлама без новых полей читается: фляги и наборы — как прежде") {
      val old = decode[MerchantState.JunkSaleSettings]("""{"rarities":["Gray"],"passives":true,"actives":true,"trophies":false}""")
      assertTrue(old.exists(s => s.flasks && s.sets))
    },

    test("руны: выключенные наборы алтарь не сдаёт, а прочее — как прежде; настройка переживает запись") {
      val hero   = TestFixtures.hero(UserId(1L))
      val keep   = hero.copy(runes = hero.runes.copy(burnSets = false))
      val plan   = (h: pangea.model.hero.Hero) => RuneAltarState.givePlan(h, List(setHelm, helm)).map(_._1.id)
      assertTrue(plan(hero) == List(2L, 3L) && plan(keep) == List(3L)) &&
      assertTrue(!keep.runes.burnsItem(setHelm) && keep.runes.burnsItem(helm)) &&
      assertTrue(decode[RuneData](keep.runes.asJson.noSpaces).exists(!_.burnSets)) &&
      // у старых записей поля нет — наборы сдаются, как раньше
      assertTrue(decode[RuneData]("""{"active":[],"passive":[]}""").exists(_.burnSets))
    }
  )
}
