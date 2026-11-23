package pangea.service.miniapp

import pangea.engine.SceneContent
import pangea.model.artifact.HeroArtifacts
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.hero.{Achievement, AzatState, HeroId, Knowledge, LoreData}
import pangea.model.home.{HomeData, House}
import pangea.model.inventory.Inventory
import pangea.model.item.{BeltSlot, BrewKind, FlaskKind, Gem, GemKind, Grip, Item, ItemDetails, ItemType, Rarity}
import pangea.model.monster.Race
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.trauma.Trauma
import pangea.model.user.{UserId, VkId}
import pangea.model.top.{TopBoard, TopKind, TopRow, TopStanding}
import pangea.service.state.QuestJournal
import pangea.service.state.states.EquipmentState
import pangea.test.TestFixtures
import zio.test._

/** Инвентарь для Mini App: стопки — как в боте, все слоты снаряжения на месте,
  * левая рука под двуручным — занята, а не пуста. «Персонаж» — те же числа,
  * что в карточке бота, «Задания» — тот же журнал, «Отряд» — без ушедших. */
object MiniAppViewSpec extends ZIOSpecDefault {

  private lazy val content: SceneContent = SceneContent.load()

  /** Хранилищ у героя нет: пустая бочка, ни ячейки, ни артефактов, ни дома. */
  private def nothing(hero: pangea.model.hero.Hero, home: HomeData = HomeData.empty): MiniAppView.Stores =
    MiniAppView.Stores(HeroArtifacts.empty(hero.id), Barrel.empty(hero.id), BankVault.empty(hero.id), home)

  private def inv(hero: pangea.model.hero.Hero, items: List[Item]): Inventory =
    Inventory(1L, hero.id, 20L, Inventory.Items(items))

  private def ruby(id: Long): Item =
    Item(id, "Надколотый рубин", 1L, Rarity.Gray, ItemType.Gem,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Gem(Gem(GemKind.Ruby, Gem.MinGrade)))

  private val sword: Item =
    Item(10L, "Меч", 3L, Rarity.Blue, ItemType.Weapon,
      attack = 12, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(Grip.OneHanded))

  private val greatsword: Item =
    Item(20L, "Двуручник", 5L, Rarity.Green, ItemType.Weapon,
      attack = 30, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(Grip.TwoHanded))

  override def spec = suite("Инвентарь Mini App")(

    test("одинаковые камни — одной стопкой со счётчиком, оружие — само по себе") {
      val hero = TestFixtures.hero(UserId(1L))
      val view = MiniAppView.inventory(hero, inv(hero, List(ruby(1L), sword, ruby(2L))), nothing(hero), content, _.name, 0L)
      assertTrue(
        view.items.map(i => i.kind -> i.count) == List("Gem" -> 2, "Weapon" -> 1),
        view.items.lastOption.exists(i => i.id == 10L && i.rarity == "Blue" && i.text.contains("Меч")),
        view.occupied == 3L,
        view.capacity == 20L
      )
    },

    test("стопка — значком и фильтром того, что внутри, с «📦» в названии; в куб её не предлагают") {
      val hero   = TestFixtures.hero(UserId(1L))
      val bundle = pangea.model.item.Bundling.pack((101L to 105L).toList.map(ruby)).get.copy(id = 50L)
      val view   = MiniAppView.inventory(hero, inv(hero, List(bundle, ruby(1L))), nothing(hero), content, _.name, 0L)
      val craft  = MiniAppView.craft(hero, AzatState.empty, HomeData.empty, inv(hero, List(bundle, ruby(1L))), content, _ => "")
      assertTrue(
        view.items.map(i => i.id -> i.kind) == List(50L -> "Gem", 1L -> "Gem"),
        view.items.headOption.exists(_.title == "📦 Надколотый рубин"),
        view.occupied == 2L,
        craft.cube.bag.map(_.id) == List(1L)
      )
    },

    test("снаряжение — все слоты; левая рука под двуручным занята им") {
      val hero = TestFixtures.hero(UserId(1L)).copy(id = HeroId(7L),
        equipment = TestFixtures.emptyEquipment.copy(weapon = greatsword))
      val view  = MiniAppView.inventory(hero, inv(hero, Nil), nothing(hero), content, _.name, 0L)
      val right = view.equipment.find(_.name == EquipmentState.RightHandName)
      val left  = view.equipment.find(_.name == EquipmentState.LeftHandName)
      assertTrue(
        view.equipment.size == EquipmentState.slots.size,
        right.flatMap(_.item).exists(_.id == 20L),
        left.exists(s => s.item.isEmpty && s.busy.contains(EquipmentState.LeftBusyText)),
        view.equipment.filter(_.item.isEmpty).forall(_.name != EquipmentState.RightHandName)
      )
    },

    test("персонаж: родное и текущее, запасы, травмы и благословение — как в карточке бота") {
      val hour = 3600000L
      val base = TestFixtures.hero(UserId(1L))
      // Орку раса приписывает силы и тела, а ловкость и ум убавляет: родное у
      // всех четырёх по десять, текущее разошлось.
      val hero = base.copy(race = Race.Orc, name = Some("Грох"),
        baseStats   = base.baseStats.copy(str = 10L, vit = 10L, agi = 10L, int = 10L),
        traumaUntil = Some(5 * hour),
        traumaNames = List(Trauma.BruisedLeg.name))
      val azat = AzatState.empty.copy(blessingUntil = Some(3 * hour), instantRests = 7)
      val view = MiniAppView.character(hero, azat, LoreData.empty, content, hour)
      val card = hero.card(hour)
      assertTrue(
        view.name.contains("Грох"),
        view.race == card.race,
        view.str.current == 12L && view.str.native == 10L,
        view.int.current == 8L && view.int.native == 10L,
        view.hp.current == card.hp && view.hp.max == card.maxHp,
        view.armor.max == card.maxArmor,
        view.attack == card.fight.atk && view.evasion == card.fight.evasion,
        view.traumas.map(_.name) == List(Trauma.BruisedLeg.name),
        view.traumaLeft.contains("4ч 0мин"),
        view.blessingLeft.contains("2ч 0мин"),
        view.instantRests == 7
      )
    },

    test("персонаж: кончилось — не показываем; имени нет — пусто") {
      val hero = TestFixtures.hero(UserId(1L)).copy(name = None,
        traumaUntil = Some(1000L), traumaNames = List(Trauma.BruisedLeg.name))
      val view = MiniAppView.character(hero, AzatState.empty.copy(blessingUntil = Some(1000L)), LoreData.empty, content, 2000L)
      assertTrue(
        view.name.isEmpty,
        view.traumas.isEmpty && view.traumaLeft.isEmpty,
        view.blessingLeft.isEmpty,
        view.rested.isEmpty
      )
    },

    test("персонаж: достижения — только полученные и сколько всего; знания — с тем, как узнал") {
      val hero = TestFixtures.hero(UserId(1L)).withAchievement(Achievement.Brewer1)
      val lore = LoreData.empty.copy(knowledge = List(Knowledge.FlowersRank1.entryName),
                   selfTaught = List(Knowledge.FlowersRank1.entryName))
      val view = MiniAppView.character(hero, AzatState.empty, lore, content, 0L)
      assertTrue(
        view.achievements.total == Achievement.values.size,
        view.achievements.got == List(MiniAppView.AchievementView(Achievement.Brewer1.title, Achievement.Brewer1.bonusLine)),
        view.knowledge.map(k => k.title -> k.how) ==
          List(Knowledge.FlowersRank1.title -> content.text("knowledge.selfTaught"))
      )
    },

    test("персонаж: эффекты — те же, что на экране «Эффекты» бота, с описанием") {
      val hero = TestFixtures.hero(UserId(1L)).copy(equipment = TestFixtures.emptyEquipment.copy(weapon = sword))
      val view = MiniAppView.character(hero, AzatState.empty, LoreData.empty, content, 0L)
      val bot  = pangea.service.state.states.hero.SkillsState.entries(hero)
      assertTrue(
        view.effects.map(_.label) == bot.map(_.label),
        view.effects.map(_.text) == bot.map(_.describe(hero))
      )
    },

    test("задания: разделы журнала словами бота, сделанное объявление — коротко, на плашку") {
      val hero = TestFixtures.hero(UserId(1L))
      val j    = QuestJournal.Journal(
        story   = Some(QuestJournal.Story("Встать на ноги", "⬜ цель")),
        npcs    = List(QuestJournal.Npc("Подопытный", "выпить зелье")),
        dailies = List(QuestJournal.Daily("Густаво", "травы 2/6")),
        boards  = List(QuestJournal.Board("1–25", "принести шкуру", ready = true)))
      val view = MiniAppView.quests(hero, j, content)
      assertTrue(
        view.story.exists(_.title == "Встать на ноги"),
        view.npcs.map(_.step) == List("выпить зелье"),
        view.dailies.map(_.npc) == List("Густаво"),
        view.boards.map(_.state) == List(content.text("miniapp.boardReady"))
      )
    },

    test("задания: пройденные — как собрал их бот, с подсчётом других и своей строкой") {
      import pangea.service.state.{QuestOutcomeStats, QuestsDone}
      val hero = TestFixtures.hero(UserId(1L))
      val done = List(
        QuestsDone.Done("Встать на ноги", story = true, first = Some("📊 Вы — среди первых…"), stats = Nil),
        QuestsDone.Done("Письмо Марисе", story = false, first = None,
          stats = List(QuestOutcomeStats.Row("60%", "бросили Марису ни с чем", mine = false),
                       QuestOutcomeStats.Row("40%", "спасли Марису", mine = true))))
      val view = MiniAppView.quests(hero, QuestJournal.Journal(None, Nil, Nil, Nil), content, done)
      assertTrue(
        view.done.map(d => (d.title, d.story)) == List(("Встать на ноги", true), ("Письмо Марисе", false)),
        view.done.headOption.flatMap(_.first).contains("📊 Вы — среди первых…"),
        view.done.lastOption.map(_.stats.map(r => (r.pct, r.label, r.mine))).contains(
          List(("60%", "бросили Марису ни с чем", false), ("40%", "спасли Марису", true)))
      )
    },

    test("отряд: ушедшего по найму не показываем, у оставшегося — сколько ему ещё") {
      val hour  = 3600000L
      val stays = Ally(AllyKind.Human, position = 2, hp = 10L, armor = 0L, energy = 0L, hiredUntil = 5 * hour)
      val gone  = Ally(AllyKind.Murloc, position = 3, hp = 10L, armor = 0L, energy = 0L, hiredUntil = hour / 2)
      val hero  = TestFixtures.hero(UserId(1L)).copy(squad = Squad(heroPos = 1, allies = List(gone, stays)))
      val view  = MiniAppView.squad(hero, content, hour)
      assertTrue(
        view.allies.map(_.name) == List(AllyKind.Human.name),
        view.allies.flatMap(_.leaves) == List("4ч 0мин"),
        view.heroPosition == 1,
        view.allies.headOption.exists(a => a.element.nonEmpty && a.hp.current == 10L)
      )
    },

    test("отряд: резерв — по порядку, со сроками и без места в строю; ушедшего из резерва тоже не показываем") {
      val hour  = 3600000L
      val inLine = Ally(AllyKind.Human, position = 2, hp = 10L, armor = 0L, energy = 0L, hiredUntil = 5 * hour)
      val waits  = Ally(AllyKind.Gnome, position = 0, hp = 7L, armor = 0L, energy = 0L, hiredUntil = 3 * hour)
      val gone   = Ally(AllyKind.Murloc, position = 0, hp = 10L, armor = 0L, energy = 0L, hiredUntil = hour / 2)
      val hero   = TestFixtures.hero(UserId(1L)).copy(squad =
                     Squad(heroPos = 1, allies = List(inLine), reserve = List(gone, waits)))
      val view   = MiniAppView.squad(hero, content, hour)
      assertTrue(
        view.reserveSize == 10,
        view.allies.map(_.name) == List(AllyKind.Human.name),
        view.reserve.map(_.name) == List(AllyKind.Gnome.name),
        view.reserve.flatMap(_.leaves) == List("2ч 0мин"),
        view.reserve.forall(a => a.position == 0 && a.reservable && a.hp.current == 7L),
        view.allies.forall(_.reservable)
      )
    },

    test("хранилища: некупленные артефакты и дом без дома — с замком и тем, как открыть; бочка открыта всегда") {
      val hero = TestFixtures.hero(UserId(1L))
      val view = MiniAppView.inventory(hero, inv(hero, Nil), nothing(hero), content, _.name, 0L)
      def of(id: String) = view.storages.find(_.id == id)
      assertTrue(
        view.storages.map(_.id) == List("casket", "livingBag", "wardrobe", "barrel", "vault",
          "herbs", "brews", "cauldron", "gems", "runes", "wall", "racks"),
        of("casket").exists(_.locked.contains(content.text("miniapp.storages.casket.locked"))),
        of("vault").exists(_.locked.contains(content.text("miniapp.storages.vault.locked"))),
        of("barrel").exists(b => b.locked.isEmpty && b.capacity.contains(Barrel.MaxItems) &&
          b.silver.exists(_.max == Barrel.MaxSilver)),
        view.storages.filter(_.group == "home").forall(_.locked.contains(content.text("miniapp.noHouse")))
      )
    },

    test("хранилища: во втором доме верстак открыт, камни и стена — только на острове; алтарь считает узоры") {
      val hero = TestFixtures.hero(UserId(1L))
      val home = HomeData.empty.copy(tier = House.Townhouse.tier, herbs = List(ruby(5L)),
                   runes = Map(pangea.model.rune.Rune.Passive(pangea.model.item.PassiveKind.values.head).key -> 7L))
      val view = MiniAppView.inventory(hero, inv(hero, Nil), nothing(hero, home), content, _.name, 0L)
      def of(id: String) = view.storages.find(_.id == id)
      assertTrue(
        of("herbs").exists(h => h.locked.isEmpty && h.used.contains(1L) && h.sections.flatMap(_.items).size == 1),
        of("gems").exists(_.locked.contains(content.text("miniapp.storages.gems.locked"))),
        of("wall").exists(_.locked.contains(content.text("miniapp.storages.wall.locked"))),
        of("racks").exists(_.locked.contains(content.text("miniapp.storages.racks.locked"))),
        of("runes").exists(r => r.locked.isEmpty && r.counts.map(_.value) == List(7L))
      )
    },

    test("сумка: ключи стоек — одной строкой «★ Связка ключей (N)», без счётчика; один ключ — как есть") {
      import pangea.model.item.QuestItemKind
      val key2 = QuestItemKind.item(QuestItemKind.RackKey2).copy(id = 92L)
      val key1 = QuestItemKind.item(QuestItemKind.RackKey1).copy(id = 91L)
      val city = TestFixtures.hero(UserId(1L), state = StateType.MarketSquare)
      val two  = MiniAppView.inventory(city, inv(city, List(ruby(5L), key2, key1)), nothing(city), content, _.name, 0L)
      val one  = MiniAppView.inventory(city, inv(city, List(key2)), nothing(city), content, _.name, 0L)
      assertTrue(
        two.items.map(i => (i.id, i.title, i.count)) == List((5L, "Надколотый рубин", 1), (92L, "★ Связка ключей (2)", 1)),
        two.items.lastOption.exists(i => i.text.startsWith("★ Связка ключей") && i.text.contains("на одном кольце: №1, №2.")),
        one.items.map(i => (i.title, i.text)) == List(("★ Ключ стойки №2", "Ключ стойки №2"))
      )
    },

    test("стойки: раздел на каждую стойку оружейной со своим номером — её надевают; старая запись «в кладовой» их не прячет") {
      import pangea.model.hero.Equipment
      import pangea.model.home.Furniture
      // Так дом записывала прошлая версия: третья стойка — в кладовой. Теперь
      // все стойки в оружейной.
      val home = HomeData.empty.copy(tier = House.Townhouse.tier, racks = List.fill(3)(Equipment.empty),
                   stored = List(Furniture.Rack.key))
      val city = TestFixtures.hero(UserId(1L), state = StateType.MarketSquare)
      val view = MiniAppView.inventory(city, inv(city, Nil), nothing(city, home), content, _.name, 0L)
      assertTrue(
        view.storages.find(_.id == "racks").exists(_.sections.map(_.rack) == List(Some(1), Some(2), Some(3))),
        view.storages.filter(_.id != "racks").flatMap(_.sections).forall(_.rack.isEmpty)
      )
    },

    test("куда переложить: в открытые хранилища, которые берут такую вещь; сюжетное — никуда; вскрытая бочка — вне игры") {
      import pangea.model.artifact.{Artifact, ArtifactKind}
      import pangea.model.barrel.{BarrelThieves, StolenGoods}
      import pangea.model.inventory.Inventory.Items
      val hero  = TestFixtures.hero(UserId(1L), state = StateType.MarketSquare)
      val quest = sword.copy(id = 77L, itemType = ItemType.QuestItem)
      val arts  = HeroArtifacts.empty(hero.id).copy(casket = Artifact(ArtifactKind.Casket, 1, 0, Items(List(ruby(9L)))))
      val vault = BankVault(1L, hero.id, 1, Items(Nil), 0L)
      val stores = MiniAppView.Stores(arts, Barrel.empty(hero.id), vault, HomeData.empty)
      val view  = MiniAppView.inventory(hero, inv(hero, List(sword, ruby(5L), quest)), stores, content, _.name, 0L)
      def to(from: String, id: Long) = view.moves.find(m => m.from == from && m.item == id).map(_.to)
      val robbed = stores.copy(barrel = Barrel.empty(hero.id).copy(thieves = Some(BarrelThieves(0L, Some(StolenGoods(Nil, 0L))))))
      val noBarrel = MiniAppView.inventory(hero, inv(hero, List(sword)), robbed, content, _.name, 0L)
      assertTrue(
        to("bag", sword.id).contains(List("barrel", "vault")),
        to("bag", 5L).contains(List("casket", "barrel", "vault")),
        to("bag", 77L).isEmpty,
        to("casket", 9L).contains(List("bag", "barrel", "vault")),
        noBarrel.moves.flatMap(_.to).forall(_ != "barrel")
      )
    },

    test("камни и резец в слоте: что вставлено и что спросить, есть ли гнездо, чем вставить; без резца — без кнопки") {
      import pangea.generator.item.MaterialGenerator
      import pangea.model.artifact.{Artifact, ArtifactKind}
      import pangea.model.inventory.Inventory.Items
      import pangea.model.item.MaterialKind
      val gem    = Gem(GemKind.Ruby, Gem.MinGrade)
      val helm   = sword.copy(id = 30L, name = "Шлем стража", itemType = ItemType.Helmet, grip = None, sockets = List(Some(gem), None))
      val hero   = TestFixtures.hero(UserId(1L)).copy(equipment = TestFixtures.emptyEquipment.copy(helmet = helm))
      val chisel = MaterialGenerator.item(MaterialKind.RuneChisel).copy(id = 900L)
      val arts   = HeroArtifacts.empty(hero.id).copy(casket = Artifact(ArtifactKind.Casket, 1, 0, Items(List(ruby(6L)))))
      val stores = nothing(hero).copy(artifacts = arts)
      val full   = MiniAppView.inventory(hero, inv(hero, List(ruby(5L), chisel)), stores, content, _.name, 0L)
      val bare   = MiniAppView.inventory(hero, inv(hero, Nil), nothing(hero), content, _.name, 0L)
      def slot(v: MiniAppView.InventoryView) = v.equipment.find(_.name == "Шлем")
      assertTrue(
        slot(full).exists(_.gems == List(MiniAppView.SocketedView(0, gem.displayName, content.format("equipment.breakGemConfirm",
          "gem" -> gem.displayName, "count" -> gem.dustYield.toString, "dust" -> gem.dust.displayName)))),
        slot(full).exists(_.freeSocket),
        slot(full).exists(_.chisel.contains(content.format("inventory.chisel.confirm", "name" -> "Шлем стража",
          "pattern" -> content.text("inventory.chisel.noPattern")))),
        full.gemStock.map(g => g.from -> g.item.id) == List("bag" -> 5L, "casket" -> 6L),
        slot(bare).exists(_.chisel.isEmpty),
        bare.gemStock.isEmpty
      )
    },

    test("куда переложить вне города: только между тем, что при себе; в бою — никуда; почему — сказано") {
      import pangea.model.artifact.{Artifact, ArtifactKind}
      import pangea.model.inventory.Inventory.Items
      def at(state: StateType) = {
        val hero  = TestFixtures.hero(UserId(1L), state = state)
        val arts  = HeroArtifacts.empty(hero.id).copy(casket = Artifact(ArtifactKind.Casket, 1, 0, Items(List(ruby(9L)))))
        val vault = BankVault(1L, hero.id, 1, Items(List(sword.copy(id = 40L))), 0L)
        val store = MiniAppView.Stores(arts, Barrel.empty(hero.id), vault, HomeData.empty)
        MiniAppView.inventory(hero, inv(hero, List(sword, ruby(5L))), store, content, _.name, 0L)
      }
      def move(v: MiniAppView.InventoryView, from: String, id: Long) = v.moves.find(m => m.from == from && m.item == id)
      val maze  = at(StateType.Dungeon)
      val fight = at(StateType.Battle)
      val town  = at(StateType.MarketSquare)
      assertTrue(
        // Лабиринт: камень — в Ларец и обратно, остальное закрыто и об этом сказано.
        move(maze, "bag", 5L).exists(m => m.to == List("casket") && m.far),
        move(maze, "casket", 9L).exists(m => m.to == List("bag") && m.far),
        // Меч в лабиринте класть некуда: шкафа нет, а ячейка и бочка — в городе.
        move(maze, "bag", sword.id).exists(m => m.to.isEmpty && m.far),
        // Из ячейки — никуда, хотя сумка при себе: сама ячейка в городе.
        move(maze, "vault", 40L).exists(m => m.to.isEmpty && m.far),
        maze.storesAway.contains(content.text("miniapp.storages.away")),
        // Бой: ни туда, ни обратно — и причина своя.
        fight.moves.forall(m => m.to.isEmpty && m.far),
        fight.storesAway.contains(content.text("miniapp.storages.inBattle")),
        // Город: всё доступно, ничего не «далеко».
        town.moves.forall(!_.far),
        town.storesAway.isEmpty,
        move(town, "vault", 40L).exists(_.to.contains("bag"))
      )
    },

    test("перенос под замком: вне города — только при себе, в бою — никак, в городе — всё") {
      import pangea.model.artifact.ArtifactKind
      import pangea.service.storage.ItemStore.Place
      val casket = Place.Art(ArtifactKind.Casket)
      def barred(state: StateType, from: Place, to: Place) =
        MiniApp.moveBarred(content, TestFixtures.hero(UserId(1L), state = state), from, to)
      val away   = content.text("miniapp.storages.away")
      val battle = content.text("miniapp.storages.inBattle")
      assertTrue(
        barred(StateType.Dungeon, Place.Bag, casket).isEmpty,
        barred(StateType.Dungeon, casket, Place.Art(ArtifactKind.Wardrobe)).isEmpty,
        // Хватает одного конца вне досягаемости — в любую сторону.
        barred(StateType.Dungeon, Place.Bag, Place.Vault).contains(away),
        barred(StateType.Dungeon, Place.Barrel, Place.Bag).contains(away),
        barred(StateType.Dungeon, casket, Place.Barrel).contains(away),
        barred(StateType.Battle, Place.Bag, casket).contains(battle),
        barred(StateType.MarketSquare, Place.Vault, Place.Barrel).isEmpty
      )
    },

    test("что выбросить: в городе — из сумки и открытых хранилищ, кроме сюжетного; вне города — при себе; в бою — ничего") {
      import pangea.model.artifact.{Artifact, ArtifactKind}
      import pangea.model.barrel.{BarrelThieves, StolenGoods}
      import pangea.model.inventory.Inventory.Items
      def at(state: StateType, robbed: Boolean = false) = {
        val hero   = TestFixtures.hero(UserId(1L), state = state)
        val arts   = HeroArtifacts.empty(hero.id).copy(casket = Artifact(ArtifactKind.Casket, 1, 0, Items(List(ruby(9L)))))
        val vault  = BankVault(1L, hero.id, 1, Items(List(sword.copy(id = 40L), sword.copy(id = 41L).asStoryGear)), 0L)
        val barrel = Barrel.empty(hero.id).withItems(List(ruby(50L)))
        val thief  = barrel.copy(thieves = Some(BarrelThieves(0L, Some(StolenGoods(Nil, 0L)))))
        val stores = MiniAppView.Stores(arts, if (robbed) thief else barrel, vault, HomeData.empty)
        val quest  = sword.copy(id = 77L, itemType = ItemType.QuestItem)
        MiniAppView.inventory(hero, inv(hero, List(sword, ruby(5L), ruby(6L), quest)), stores, content, _.name, 0L)
      }
      def drops(v: MiniAppView.InventoryView) = v.drops.map(d => d.from -> d.item)
      assertTrue(
        // Стопка рубинов — одной строкой, как в списке; предмет задания и ★ — нет.
        drops(at(StateType.MarketSquare)) == List("bag" -> sword.id, "bag" -> 6L, "casket" -> 9L, "barrel" -> 50L, "vault" -> 40L),
        // Лабиринт: только сумка и то, что при себе.
        drops(at(StateType.Dungeon)) == List("bag" -> sword.id, "bag" -> 6L, "casket" -> 9L),
        drops(at(StateType.Battle)).isEmpty,
        // Вскрытая ворами бочка не отдаёт ничего.
        !drops(at(StateType.MarketSquare, robbed = true)).exists(_._1 == "barrel")
      )
    },

    test("выбросить под рукой: вне города — только при себе, в бою — ничего, в городе — всё; почему — своими словами") {
      import pangea.model.artifact.ArtifactKind
      import pangea.service.storage.ItemStore.Place
      def barred(state: StateType, from: Place) =
        MiniApp.dropBarred(content, TestFixtures.hero(UserId(1L), state = state), from)
      assertTrue(
        barred(StateType.Dungeon, Place.Bag).isEmpty,
        barred(StateType.Dungeon, Place.Art(ArtifactKind.Wardrobe)).isEmpty,
        barred(StateType.Dungeon, Place.Vault).contains(content.text("miniapp.storages.dropAway")),
        barred(StateType.Battle, Place.Bag).contains(content.text("miniapp.storages.dropInBattle")),
        barred(StateType.MarketSquare, Place.Barrel).isEmpty
      )
    },

    test("пояс в снаряжении — по ячейкам: сколько из скольких, пустая помнит, чем была") {
      val belt = Item(30L, "Пояс", 1L, Rarity.Blue, ItemType.Belt,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        details = ItemDetails.Belt(List(
          BeltSlot.Flask(FlaskKind.Healer, Rarity.Blue, 0),
          BeltSlot.Brew(BrewKind.LivingWater, 2))))
      val hero = TestFixtures.hero(UserId(1L)).copy(equipment = TestFixtures.emptyEquipment.copy(belt = belt))
      val view = MiniAppView.inventory(hero, inv(hero, Nil), nothing(hero), content, _.name, 0L)
      val cells = view.equipment.flatMap(_.belt).flatMap(_.cells)
      assertTrue(
        view.equipment.flatMap(_.belt).map(_.capacity) == List(ItemDetails.Belt.capacityFor(Rarity.Blue)),
        cells.map(c => c.name -> c.uses) == List(FlaskKind.Healer.label -> 0, BrewKind.LivingWater.label -> 2),
        cells.lastOption.exists(_.max == ItemDetails.Belt.capacityFor(Rarity.Blue))
      )
    },

    test("снаряжение: в слот — только подходящее по типу и уровню; разница — как после надевания") {
      val better = sword.copy(id = 11L, name = "Меч получше", attack = 30, lvl = 1L)
      val tooHigh = sword.copy(id = 12L, name = "Меч не по силам", attack = 99, lvl = 50L)
      val hero = TestFixtures.hero(UserId(1L)).copy(equipment = TestFixtures.emptyEquipment.copy(weapon = sword.copy(lvl = 1L)))
      val view = MiniAppView.inventory(hero, inv(hero, List(better, tooHigh, ruby(1L))), nothing(hero), content, _.name, 0L)
      val right = view.equipment.find(_.name == EquipmentState.RightHandName)
      val cands = right.toList.flatMap(_.candidates)
      // Как надевает бот: статы сдвигаются на разницу вещей (Hero.reequip).
      val after = hero.reequip(hero.equipment.copy(weapon = better), 0L).card(0L)
      val atk   = after.fight.atk - hero.card(0L).fight.atk
      assertTrue(
        cands.map(_.item.id) == List(11L),
        cands.flatMap(_.deltas).find(_.key == "attack").exists(d => d.value == atk && d.value == 18L),
        right.flatMap(_.item).exists(_.stats.exists(st => st.key == "attack" && st.value == 12L)),
        view.equipment.find(_.name == "Шлем").exists(_.candidates.isEmpty)
      )
    },

    test("топ: места по порядку, свой герой помечен и без ссылки; вне показанных — отдельной строкой со своим числом") {
      val hero = TestFixtures.hero(UserId(1L)).copy(id = HeroId(7L), name = None)
      def row(id: Long, lvl: Long, name: Option[String] = None) =
        TopRow(HeroId(id), UserId(id), VkId(s"10$id"), name, Race.Orc, lvl, lvl)
      val inTop = MiniAppView.top(hero, TopKind.Level,
                    TopBoard(List(row(3L, 20L, Some("Грох")), row(7L, 12L)), Map(HeroId(7L) -> TopStanding(2L, 12L)), 5L),
                    "Безымянный")
      // Число — из снимка топа: книги рецептов у героя под рукой нет.
      val outside = MiniAppView.top(hero, TopKind.Recipes,
                      TopBoard(List(row(3L, 20L, Some("Грох"))), Map(HeroId(7L) -> TopStanding(4L, 40L)), 5L), "Безымянный")
      assertTrue(
        inTop.rows.map(r => r.place -> r.me) == List(1L -> false, 2L -> true),
        inTop.rows.map(_.name) == List("Грох", "Безымянный"),
        inTop.rows.map(_.profile) == List(Some("https://vk.ru/id103"), None),
        inTop.me.isEmpty,
        outside.rows.forall(!_.me),
        outside.me.exists(m => m.place == 4L && m.me && m.value == 40L && m.profile.isEmpty),
        inTop.kind == "level" && inTop.total == 5L
      )
    }
    ,
    test("крафт: куб — по статусу, сила — заряды или энергия дома, в сумке нет сюжетного, вне города — почему нельзя") {
      import pangea.model.hero.CubeStatus
      val c      = SceneContent.load()
      val hero   = TestFixtures.hero(UserId(1L), state = StateType.MarketSquare)
      val quest  = sword.copy(id = 77L, itemType = ItemType.QuestItem)
      val bag    = inv(hero, List(ruby(5L), quest))
      val absent = MiniAppView.craft(hero, AzatState.empty, HomeData.empty, bag, c, _ => "")
      val active = AzatState.empty.copy(cube = CubeStatus.Active, cubeCharges = 37, cubeItems = List(ruby(9L)))
      val inCity = MiniAppView.craft(hero, active, HomeData.empty, bag, c, _ => "")
      val away   = MiniAppView.craft(hero.copy(state = StateType.Dungeon), active, HomeData.empty, bag, c, _ => "")
      assertTrue(
        absent.cube.status == "absent" && absent.cube.locked.exists(_.contains("200")),
        inCity.cube.status == "active" && inCity.cube.locked.isEmpty,
        inCity.cube.power == MiniAppView.CubePowerView("own", 37L, AzatState.MaxCharges.toLong),
        inCity.cube.items.map(i => i.id -> i.count) == List(9L -> 1),
        inCity.cube.bag.map(_.id) == List(5L),
        inCity.cube.away.isEmpty,
        away.cube.away.contains(c.text("miniapp.craft.away"))
      )
    },

    test("аукцион: чем платить и куда класть — как пришло; вне города — почему не купить") {
      val c        = SceneContent.load()
      val hero     = TestFixtures.hero(UserId(1L), state = StateType.TradeHouse)
      val lot      = AuctionLot(3L, HeroId(2L), sword, 300L, AuctionCurrency.Doubloons, 0L, AuctionLot.LifetimeMs)
      val funds    = MiniAppView.Funds(silver = 1200L, bagFree = 4L)
      val inCity   = MiniAppView.auction(hero, List(lot), Nil, 0L, Nil, _ => "", funds, c)
      val away     = MiniAppView.auction(hero.copy(state = StateType.Battle), List(lot), Nil, 0L, Nil, _ => "", funds, c)
      val vaultOnly = MiniAppView.Funds(silver = 400L, bagFree = 4L)
      val courier  = MiniAppView.auction(hero.copy(state = StateType.Battle), List(lot), Nil, 0L, Nil, _ => "",
                       vaultOnly, c, hasCourier = true)
      assertTrue(
        inCity.lots.map(l => (l.id, l.currency, l.mine)) == List((3L, "doubloons", false)),
        inCity.silver == 1200L && inCity.bagFree == 4L,
        inCity.away.isEmpty,
        away.away.contains(c.text("miniapp.auction.away")),
        courier.away.isEmpty && courier.silver == 400L
      )
    },

    test("аукцион: стопка на витрине и в своих лотах — со счётом, как в боте; прочее — как было") {
      val c      = SceneContent.load()
      val hero   = TestFixtures.hero(UserId(1L), state = StateType.TradeHouse)
      val gems   = (101L to 105L).toList.map(i => pangea.generator.item.GemGenerator.item(GemKind.Ruby, 1).copy(id = i))
      val bundle = pangea.model.item.Bundling.pack(gems).get.copy(id = 60L)
      val pack   = AuctionLot(3L, HeroId(2L), bundle, 300L, AuctionCurrency.Silver, 0L, AuctionLot.LifetimeMs)
      val one    = AuctionLot(4L, hero.id, sword, 300L, AuctionCurrency.Silver, 0L, AuctionLot.LifetimeMs)
      val view   = MiniAppView.auction(hero, List(pack, one), List(one), 0L, Nil, _ => "",
                     MiniAppView.Funds(silver = 0L, bagFree = 1L), c)
      assertTrue(
        view.lots.map(_.item.title) == List("📦 Надколотый рубин ×5", sword.displayTitle),
        view.mine.map(_.item.title) == List(sword.displayTitle)
      )
    },

    test("аукцион: выставить — что взял бы Рахадим, стопками, стопка со счётом; вне города — почему нельзя") {
      import pangea.generator.item.MaterialGenerator
      import pangea.model.item.{Bundling, MaterialKind, QuestItemKind}
      val c      = SceneContent.load()
      val hero   = TestFixtures.hero(UserId(1L), state = StateType.TradeHouse)
      val trophy = Item(9L, "Мешок с пожитками (Орк)", lvl = 10L, Rarity.Gray, ItemType.Trophy,
                     attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)
      val key    = QuestItemKind.item(QuestItemKind.RackKey1).copy(id = 91L)
      val dust   = (101L to 105L).toList.map(i => MaterialGenerator.item(MaterialKind.RubyDust).copy(id = i))
      val bundle = Bundling.pack(dust).get.copy(id = 7L)
      val story  = greatsword.asStoryGear.copy(id = 21L)
      val bag    = List(sword, ruby(1L), ruby(2L), trophy, key, dust.head, bundle, story)
      val funds  = MiniAppView.Funds(silver = 0L, bagFree = 1L)
      val view   = MiniAppView.auction(hero, Nil, Nil, 0L, bag, _ => "", funds, c)
      val away   = MiniAppView.auction(hero.copy(state = StateType.Battle), Nil, Nil, 0L, bag, _ => "", funds, c)
      assertTrue(
        view.sell.items.map(i => (i.title, i.count)) ==
          List((sword.displayTitle, 1), (ruby(1L).displayTitle, 2), ("📦 Рубиновая пыль ×5", 1)),
        view.sell.minPrice == AuctionLot.MinPrice && view.sell.maxPrice == AuctionLot.MaxPrice,
        view.sell.feePct == AuctionLot.FeePct && view.sell.maxLots == AuctionLot.MaxLots,
        view.sell.empty == c.text("bank.auction.nothingToSell"),
        view.sell.away.isEmpty,
        away.sell.away.contains(c.text("miniapp.auction.sellAway"))
      )
    }
  )
}