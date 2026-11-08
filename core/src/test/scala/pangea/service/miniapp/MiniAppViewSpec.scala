package pangea.service.miniapp

import pangea.engine.SceneContent
import pangea.model.artifact.HeroArtifacts
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.hero.{Achievement, AzatState, HeroId, Knowledge, LoreData}
import pangea.model.home.{HomeData, House}
import pangea.model.inventory.Inventory
import pangea.model.item.{BeltSlot, BrewKind, FlaskKind, Gem, GemKind, Grip, Item, ItemDetails, ItemType, Rarity}
import pangea.model.monster.Race
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.trauma.Trauma
import pangea.model.user.{UserId, VkId}
import pangea.model.top.{TopBoard, TopKind, TopRow}
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

    test("топ: места по порядку, свой герой помечен и без ссылки; вне показанных — отдельной строкой") {
      val hero = TestFixtures.hero(UserId(1L)).copy(id = HeroId(7L), name = None, kills = 40L)
      def row(id: Long, lvl: Long, name: Option[String] = None) =
        TopRow(HeroId(id), UserId(id), VkId(s"10$id"), name, Race.Orc, lvl, lvl)
      val inTop = MiniAppView.top(hero, TopKind.Level,
                    TopBoard(List(row(3L, 20L, Some("Грох")), row(7L, 12L)), Map(HeroId(7L) -> 2L), 5L), "Безымянный")
      val outside = MiniAppView.top(hero, TopKind.Kills,
                      TopBoard(List(row(3L, 20L, Some("Грох"))), Map(HeroId(7L) -> 4L), 5L), "Безымянный")
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
      import pangea.model.state.StateType
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
    }
  )
}