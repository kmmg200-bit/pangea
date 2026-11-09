package pangea.service.miniapp

import pangea.engine.SceneContent
import pangea.generator.item.FlaskGenerator
import pangea.model.artifact.HeroArtifacts
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.hero.Hero
import pangea.model.home.HomeData
import pangea.model.inventory.Inventory
import pangea.model.item.{BeltSlot, BrewKind, FlaskKind, Grip, Item, ItemDetails, ItemType, Rarity}
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.trauma.Trauma
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.equipment.{BeltWork, BrewWork}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository}
import zio.test._
import zio.Ref

/** То, чего нет в экранах бота: какие кнопки приложение получает данными и что
  * итог идёт в ответ, а не в чат. Сами правила проверены через экраны — отвары
  * в BrewSpec, пояс в InventoryStateSpec и EquipmentStateSpec. */
object MiniAppBrewSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("1"), TelegramId("-1"))
  private val base   = TestFixtures.hero(userId).copy(equipment = TestFixtures.emptyEquipment)

  private lazy val content: SceneContent = SceneContent.load()

  private def brew(kind: BrewKind, id: Long = 1L): Item = BrewKind.item(kind).copy(id = id)

  private val sword: Item =
    Item(10L, "Меч", 1L, Rarity.Blue, ItemType.Weapon,
      attack = 12, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(Grip.OneHanded))

  /** Синий пояс держит три ячейки. */
  private def belted(hero: Hero, slots: BeltSlot*): Hero =
    hero.copy(equipment = hero.equipment.copy(belt = Item(30L, "Пояс", 1L, Rarity.Blue, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Belt(slots.toList))))

  private def healer(charges: Int): BeltSlot.Flask = BeltSlot.Flask(FlaskKind.Healer, Rarity.Blue, charges)

  private def slots(h: Hero): List[BeltSlot] = h.equipment.belt.details match {
    case b: ItemDetails.Belt => b.slots
    case _                   => Nil
  }

  private def view(h: Hero, items: List[Item]): MiniAppView.InventoryView =
    MiniAppView.inventory(h, Inventory(1L, h.id, 20L, Inventory.Items(items)),
      MiniAppView.Stores(HeroArtifacts.empty(h.id), Barrel.empty(h.id), BankVault.empty(h.id), HomeData.empty),
      content, _.name, 0L)

  private def asks(h: Hero, kind: BrewKind): List[MiniAppView.BrewAskView] =
    view(h, List(brew(kind))).bagActs.flatMap(_.brew)

  private def hurt(names: String*): Hero = base.copy(traumaUntil = Some(999999999L), traumaNames = names.toList)

  private def setup(h: Hero, items: List[Item]) =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      bag    = TestInventoryRepository.withItems(items)
      lines <- Ref.make(Vector.empty[String])
    } yield (dao, bag, new BrewWork(dao, bag, content), new BeltWork(dao, bag, TestItemRepository.make, content),
      new MiniApp.Collect(lines), lines)

  override def spec = suite("MiniApp: отвары и пояс")(
    suite("что приходит приложению")(
      test("отвар, который пьют сразу, переспрашивает, тратить ли склянку") {
        val only = asks(base, BrewKind.ClearEye)
        assertTrue(
          only.map(_.action) == List("drink"),
          only.forall(a => a.options.isEmpty && a.refusal.isEmpty),
          only.map(_.question) == List(content.format("miniapp.brew.confirm", "name" -> brew(BrewKind.ClearEye).name)),
          only.map(_.label) == List(content.text("brew.drink"))
        )
      },
      test("боевой отвар с карточки не пьют — только на пояс; шнапс — ни то ни другое") {
        val acts = view(base, List(brew(BrewKind.WortDecoction), brew(BrewKind.Schnapps, 2L))).bagActs
        assertTrue(acts.map(a => (a.item, a.brew, a.belt)) == List((1L, Nil, true)))
      },
      test("костоправный: лёгкие и средние на выбор, тяжёлые названы; без травм и с одними тяжёлыми — почему нельзя") {
        val mixed = asks(hurt(Trauma.BruisedLeg.name, Trauma.BruisedLeg.name, Trauma.SplitSkull.name), BrewKind.BoneSetter)
        val heavy = asks(hurt(Trauma.SplitSkull.name), BrewKind.BoneSetter)
        val whole = asks(base, BrewKind.BoneSetter)
        assertTrue(
          mixed.map(_.action) == List("cure"),
          mixed.flatMap(_.options).map(_.id) == List(Trauma.BruisedLeg.name),
          mixed.flatMap(_.hint) == List(content.format("brew.heavyStay", "names" -> Trauma.SplitSkull.name)),
          mixed.flatMap(_.refusal).isEmpty,
          heavy.flatMap(_.refusal) == List(content.text("brew.onlyHeavy")),
          whole.flatMap(_.refusal) == List(content.text("brew.noTraumas"))
        )
      },
      test("приговор спрашивает род: все смертные") {
        val only = asks(base, BrewKind.Sentence)
        assertTrue(
          only.map(_.action) == List("sentence"),
          only.flatMap(_.options).map(_.id) == Race.mortals.toList.map(_.entryName)
        )
      },
      test("мазь: с оружием — переспрашивает, без оружия — почему нельзя") {
        val armed = base.copy(equipment = base.equipment.copy(weapon = sword))
        def coat(h: Hero) = asks(h, BrewKind.VenomSalve).filter(_.action == "coat")
        assertTrue(
          coat(armed).flatMap(_.refusal).isEmpty && coat(armed).size == 1,
          coat(base).flatMap(_.refusal) == List(content.text("brew.noWeapon"))
        )
      },
      test("живая вода: заправка — только неполные фляги, номер ячейки; полные — почему нельзя; подходящей нет — кнопки нет") {
        def refill(h: Hero) = asks(h, BrewKind.LivingWater).filter(_.action == "refill")
        val halfFull = refill(belted(base, healer(healer(0).maxCharges), healer(0)))
        val allFull  = refill(belted(base, healer(healer(0).maxCharges)))
        assertTrue(
          halfFull.flatMap(_.options).map(_.id) == List("1"),
          halfFull.flatMap(_.refusal).isEmpty,
          allFull.flatMap(_.refusal) == List(content.text("brew.flaskFull")),
          refill(base).isEmpty
        )
      },
      test("пояс: есть ячейка — без вопроса; нет — спросить, что снять, и подписи ячеек как в боте") {
        val flask = FlaskGenerator.item(FlaskKind.Smith, Rarity.Blue).copy(id = 7L)
        val free  = view(belted(base, healer(1)), List(flask))
        val full  = view(belted(base, healer(1), healer(0), healer(2)), List(flask))
        assertTrue(
          free.bagActs.map(a => (a.belt, a.beltFull)) == List((true, None)),
          full.bagActs.flatMap(_.beltFull) == List(content.format("inventory.beltSwapChoice", "name" -> FlaskKind.Smith.label)),
          full.equipment.flatMap(_.belt).flatMap(_.cells).map(_.swap).take(2) == List(
            content.format("inventory.beltSwapSlot", "name" -> FlaskKind.Healer.label),
            content.format("inventory.beltSwapSlotEmpty", "name" -> FlaskKind.Healer.label))
        )
      },
      test("в бою из сумки ничего не достать") {
        assertTrue(view(base.copy(state = StateType.Battle), List(brew(BrewKind.ClearEye))).bagActs.isEmpty)
      }
    ),
    suite("что делает")(
      test("глоток: склянка ушла, баф лёг, итог — в ответ приложению") {
        val item = brew(BrewKind.ClearEye)
        for {
          t <- setup(base, List(item))
          (dao, bag, brews, _, say, lines) = t
          ok    <- brews.drink(user, base, item, say)
          saved <- dao.getHeroByUserId(userId).someOrFailException
          said  <- lines.get
        } yield assertTrue(ok, bag.snapshot.isEmpty, saved.statBoosts.hasActive("herb:agi", 0L), said.size == 1)
      },
      test("чужим отваром травму не лечат и род не называют; склянка цела, сказать нечего") {
        val item = brew(BrewKind.ClearEye)
        val sick = hurt(Trauma.BruisedLeg.name)
        for {
          t <- setup(sick, List(item))
          (dao, bag, brews, _, say, lines) = t
          cured     <- brews.cure(user, sick, item, Trauma.BruisedLeg.name, say)
          sentenced <- brews.sentence(user, sick, item, Race.mortals.toList.headOption.getOrElse(Race.values.head), say)
          drunk     <- brews.drink(user, sick, brew(BrewKind.BoneSetter), say)
          saved     <- dao.getHeroByUserId(userId).someOrFailException
          said      <- lines.get
        } yield assertTrue(!cured, !sentenced, !drunk, bag.snapshot == List(item),
          saved.traumaNames == List(Trauma.BruisedLeg.name), said.isEmpty)
      },
      test("заправка: полная фляга — склянка цела; неполная — налита, склянка ушла") {
        val item = brew(BrewKind.LivingWater)
        val h    = belted(base, healer(healer(0).maxCharges), healer(0))
        for {
          t <- setup(h, List(item))
          (dao, bag, brews, _, say, _) = t
          full  <- brews.refill(user, h, item, 0, say)
          kept   = bag.snapshot
          open  <- brews.refill(user, h, item, 1, say)
          saved <- dao.getHeroByUserId(userId).someOrFailException
        } yield assertTrue(!full, kept == List(item), open, bag.snapshot.isEmpty,
          slots(saved) == List(healer(healer(0).maxCharges), healer(healer(0).maxCharges)))
      },
      test("на пояс: легло; полон — Full, сумка цела; вместо ячейки — снятое в сумку; вынуть — ячейка свободна") {
        val flask = FlaskGenerator.item(FlaskKind.Smith, Rarity.Blue).copy(id = 7L)
        val free  = belted(base, healer(1), healer(2))
        val full  = belted(base, healer(1), healer(2), healer(3))
        for {
          t <- setup(free, List(flask))
          (dao, bag, _, belts, say, _) = t
          put    <- belts.put(user, free, flask, say)
          placed <- dao.getHeroByUserId(userId).someOrFailException
          empty   = bag.snapshot
          t2 <- setup(full, List(flask))
          (dao2, bag2, _, belts2, say2, _) = t2
          refused <- belts2.put(user, full, flask, say2)
          kept     = bag2.snapshot
          swapped <- belts2.swap(user, full, flask, Some(1), say2)
          after   <- dao2.getHeroByUserId(userId).someOrFailException
          out      = bag2.snapshot
          took    <- belts2.take(user, after, 0, say2)
          gone    <- belts2.take(user, after, 9, say2)
          last    <- dao2.getHeroByUserId(userId).someOrFailException
        } yield assertTrue(
          put == BeltWork.Put.Done,
          slots(placed).map(_.name) == List(FlaskKind.Healer.label, FlaskKind.Healer.label, FlaskKind.Smith.label),
          empty.isEmpty,
          refused match { case BeltWork.Put.Full(_, _) => true; case _ => false },
          kept == List(flask),
          swapped == BeltWork.Put.Done,
          slots(after).map(_.name) == List(FlaskKind.Healer.label, FlaskKind.Smith.label, FlaskKind.Healer.label),
          out.map(_.name) == List(FlaskGenerator.item(FlaskKind.Healer, Rarity.Blue).name),
          took,
          !gone,
          slots(last).map(_.name) == List(FlaskKind.Smith.label, FlaskKind.Healer.label)
        )
      },
      test("запрос приложения: известные действия читаются, без выбора или с чужим — нет") {
        import MiniApp.BrewAct
        assertTrue(
          BrewAct.parse("drink", None).contains(BrewAct.Drink),
          BrewAct.parse("coat", None).contains(BrewAct.Coat),
          BrewAct.parse("cure", Some("Ушиб ноги")).contains(BrewAct.Cure("Ушиб ноги")),
          BrewAct.parse("sentence", Some("Orc")).contains(BrewAct.Sentence("Orc")),
          BrewAct.parse("refill", Some("2")).contains(BrewAct.Refill(2)),
          BrewAct.parse("refill", Some("x")).isEmpty,
          BrewAct.parse("cure", None).isEmpty,
          BrewAct.parse("sell", Some("1")).isEmpty
        )
      }
    )
  )
}
