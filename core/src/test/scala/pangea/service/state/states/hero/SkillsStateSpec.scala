package pangea.service.state.states.hero

import pangea.engine.SceneContent
import pangea.model.hero.Equipment
import pangea.model.rune.{Rune, RuneData}
import pangea.model.item.{Item, ItemDetails, ItemSet, ItemType, PassiveKind, Rarity}
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

object SkillsStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def weaponWith(skill: Skill, id: Long): Item =
    Item(id, "Оружие", 1L, Rarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Weapon(skill))

  private def passiveItem(itemType: ItemType, kind: PassiveKind, id: Long): Item =
    Item(id, "Предмет", 1L, Rarity.Gray, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Passive(kind))

  // Нагрудник с активным навыком (не пассивкой) — для тестов активных умений.
  private def armorSkillItem(skill: Skill, id: Long): Item =
    Item(id, "Нагрудник", 1L, Rarity.Gray, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Armor(skill))

  private def makeState(equipment: Equipment) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(equipment = equipment))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (SkillsState(heroDao, content), heroDao, renderer)

  override def spec = suite("SkillsState")(

    test("enter без снаряжения с умениями → пустой список, только Назад") {
      for {
        t <- makeState(TestFixtures.emptyEquipment)
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.map(_.id) == List("BackFromSkills"))
    },

    test("«Эффекты»: клейма с тела — с пометкой «на теле», в описании — понимание; заголовок «Эффекты»") {
      val runes = RuneData.empty.brand(Rune.Active(Skill.Ram)).brand(Rune.Passive(PassiveKind.Healer))
        .copy(understanding = Map(Rune.Active(Skill.Ram).key -> 12L, Rune.Passive(PassiveKind.Healer).key -> 3L))
      val eq = TestFixtures.emptyEquipment.copy(weapon = weaponWith(Skill.CunningStrike, 1L))
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(equipment = eq, runes = runes))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = SkillsState(heroDao, content)
        _        <- state.enter(testUser, renderer)
        list     <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, tap(s"ActiveSkill_${Rune.bodySlotId(Skill.Ram)}"), renderer)
        ram      <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, tap("PassiveSkill_Healer_body"), renderer)
        healer   <- renderer.sentScreens.map(_.last)
      } yield assertTrue(list.text.startsWith("🎓 Эффекты")) &&
              assertTrue(list.choices.map(_.label).take(3) ==
                List("Хитрый удар (Правая рука)", "Таран (на теле)", "Целитель (на теле)")) &&
              assertTrue(ram.text.contains("Понимание: 12.") && healer.text.contains("Понимание: 3."))
    },

    test("умение и на вещи, и на теле — две кнопки: с вещи и «на теле», в карточках — что не складываются") {
      val runes = RuneData.empty.brand(Rune.Active(Skill.CunningStrike)).brand(Rune.Passive(PassiveKind.Jeweler))
      val eq    = TestFixtures.emptyEquipment.copy(
        weapon    = weaponWith(Skill.CunningStrike, 1L),
        firstRing = passiveItem(ItemType.Ring, PassiveKind.Jeweler, id = 30L))
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(equipment = eq, runes = runes))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = SkillsState(heroDao, content)
        _        <- state.enter(testUser, renderer)
        list     <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, tap("ActiveSkill_1"), renderer)
        onItem   <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, tap(s"ActiveSkill_${Rune.bodySlotId(Skill.CunningStrike)}"), renderer)
        onBody   <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, tap("PassiveSkill_Jeweler_body"), renderer)
        ringBody <- renderer.sentScreens.map(_.last)
        labels    = list.choices.map(_.label)
      } yield assertTrue(labels.take(4) == List(
                "Хитрый удар (Правая рука)", "Хитрый удар (на теле)", "Ювелир (Кольцо 1)", "Ювелир (на теле)")) &&
              assertTrue(onItem.text.contains("Это же умение выжжено у вас на теле. Одинаковые умения не складываются — действует одно.")) &&
              assertTrue(onBody.text.contains("Это же умение есть и на надетой вещи. Одинаковые умения не складываются — действует одно.")) &&
              assertTrue(ringBody.text.contains("Это же умение есть и на надетой вещи."))
    },

    test("enter с оружием и нагрудником → 2 кнопки активных умений с их названиями") {
      val eq = TestFixtures.emptyEquipment.copy(
        weapon     = weaponWith(Skill.SweepingStrike, id = 10L),
        chestPlate = armorSkillItem(Skill.MinorHeal, id = 20L)
      )
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        ids      = screens.last.choices.map(_.id)
        labels   = screens.last.choices.map(_.label)
      } yield assertTrue(ids.contains("ActiveSkill_10")) &&
              assertTrue(ids.contains("ActiveSkill_20")) &&
              assertTrue(labels.contains(s"${Skill.SweepingStrike.label} (Правая рука)")) &&
              assertTrue(labels.contains(s"${Skill.MinorHeal.label} (Нагрудник)"))
    },

    test("enter с пассивным кольцом → кнопка пассивки с её названием") {
      val eq = TestFixtures.emptyEquipment.copy(
        firstRing = passiveItem(ItemType.Ring, PassiveKind.Jeweler, id = 30L)
      )
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.exists(c =>
        c.id == "PassiveSkill_Jeweler" && c.label == s"${PassiveKind.Jeweler.label} (Кольцо 1)"))
    },

    test("два предмета с одной пассивкой в разных слотах → одна кнопка, слоты в подписи; в карточке — что не складываются") {
      val eq = TestFixtures.emptyEquipment.copy(
        boots = passiveItem(ItemType.Boots, PassiveKind.Stash, id = 40L),
        pants = passiveItem(ItemType.Pants, PassiveKind.Stash, id = 41L)
      )
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        _       <- state.action(testUser, tap("PassiveSkill_Stash"), renderer)
        card    <- renderer.sentScreens.map(_.last)
      } yield assertTrue(screens.last.choices.count(_.id == "PassiveSkill_Stash") == 1) &&
              assertTrue(screens.last.choices.find(_.id == "PassiveSkill_Stash").exists(
                _.label == s"${PassiveKind.Stash.label} (Штаны, Сапоги)")) &&
              assertTrue(card.text.contains("Штаны, Сапоги. Одинаковые умения не складываются — действует одно."))
    },

    test("одно умение с двух вещей — одна кнопка, оба слота в подписи; с одной вещи — один слот") {
      val both = TestFixtures.emptyEquipment.copy(
        weapon   = weaponWith(Skill.CunningStrike, id = 10L),
        leftHand = weaponWith(Skill.CunningStrike, id = 11L)
      )
      val single = TestFixtures.emptyEquipment.copy(
        weapon = weaponWith(Skill.CunningStrike, id = 10L),
        boots  = passiveItem(ItemType.Boots, PassiveKind.Stash, id = 40L)
      )
      for {
        t <- makeState(both)
        (state, _, renderer) = t
        _     <- state.enter(testUser, renderer)
        twice <- renderer.sentScreens.map(_.last.choices.filter(_.id.startsWith("ActiveSkill_")).map(_.label))
        s <- makeState(single)
        (one, _, oneRenderer) = s
        _     <- one.enter(testUser, oneRenderer)
        once  <- oneRenderer.sentScreens.map(_.last.choices.map(_.label))
        _     <- one.action(testUser, tap("ActiveSkill_10"), oneRenderer)
        card  <- oneRenderer.sentScreens.map(_.last)
      } yield assertTrue(twice == List(s"${Skill.CunningStrike.label} (Правая рука, Левая рука)")) &&
              assertTrue(once.take(2) == List(
                s"${Skill.CunningStrike.label} (Правая рука)", s"${PassiveKind.Stash.label} (Сапоги)")) &&
              assertTrue(!card.text.contains("Одинаковые умения"))
    },

    test("набор — с числом надетых частей: «Дикое пламя 6»") {
      def piece(itemType: ItemType, id: Long): Item =
        passiveItem(itemType, PassiveKind.Stash, id).copy(details = ItemDetails.Plain, set = Some(ItemSet.WildFlame))
      val eq = TestFixtures.emptyEquipment.copy(
        helmet       = piece(ItemType.Helmet, 1L),
        shoulderPads = piece(ItemType.ShoulderPads, 2L),
        bracelets    = piece(ItemType.Bracelets, 3L),
        gloves       = piece(ItemType.Gloves, 4L),
        pants        = piece(ItemType.Pants, 5L),
        boots        = piece(ItemType.Boots, 6L)
      )
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _    <- state.enter(testUser, renderer)
        list <- renderer.sentScreens.map(_.last)
      } yield assertTrue(list.choices.find(_.id == "Set_WildFlame").exists(_.label == "🛡 Дикое пламя 6"))
    },

    test("клик по активному умению → показывает полное описание с энергией и перезарядкой") {
      val eq = TestFixtures.emptyEquipment.copy(weapon = weaponWith(Skill.SweepingStrike, id = 10L))
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.action(testUser, tap("ActiveSkill_10"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains(Skill.SweepingStrike.label)) &&
              assertTrue(screens.last.text.contains("Перезарядка")) &&
              assertTrue(screens.last.choices.map(_.id) == List("SkillsList"))
    },

    test("клик по пассивке → показывает её описание") {
      val eq = TestFixtures.emptyEquipment.copy(
        firstRing = passiveItem(ItemType.Ring, PassiveKind.Jeweler, id = 30L))
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.action(testUser, tap("PassiveSkill_Jeweler"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains(PassiveKind.Jeweler.describe))
    },

    test("SkillsList (Назад с детального экрана) → снова список") {
      val eq = TestFixtures.emptyEquipment.copy(weapon = weaponWith(Skill.SweepingStrike, id = 10L))
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _       <- state.action(testUser, tap("ActiveSkill_10"), renderer)
        _       <- state.action(testUser, tap("SkillsList"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.exists(_.id == "ActiveSkill_10"))
    },

    test("BackFromSkills → переход в HeroStats") {
      for {
        t <- makeState(TestFixtures.emptyEquipment)
        (state, _, renderer) = t
        result <- state.action(testUser, tap("BackFromSkills"), renderer)
      } yield assertTrue(result == StateType.HeroStats)
    },

    test("11 умений/пассивок → 8 кнопок на первой странице + След., остальные на второй") {
      val eq = Equipment(
        helmet           = passiveItem(ItemType.Helmet, PassiveKind.Stealthy, id = 1L),
        shoulderPads     = passiveItem(ItemType.ShoulderPads, PassiveKind.Taxidermist, id = 2L),
        chestPlate       = armorSkillItem(Skill.MinorHeal, id = 3L),
        bracelets        = passiveItem(ItemType.Bracelets, PassiveKind.Blending, id = 4L),
        gloves           = passiveItem(ItemType.Gloves, PassiveKind.Terrifying, id = 5L),
        pants            = passiveItem(ItemType.Pants, PassiveKind.Glittering, id = 6L),
        boots            = passiveItem(ItemType.Boots, PassiveKind.Reinforced, id = 7L),
        amulet           = passiveItem(ItemType.Amulet, PassiveKind.Focused, id = 8L),
        firstRing        = passiveItem(ItemType.Ring, PassiveKind.Jeweler, id = 9L),
        secondRing       = passiveItem(ItemType.Ring, PassiveKind.Marauder, id = 10L),
        belt             = Item.NoItem,
        weapon           = weaponWith(Skill.SweepingStrike, id = 11L),
        additionalWeapon = Item.NoItem
      )
      for {
        t <- makeState(eq)
        (state, _, renderer) = t
        _        <- state.enter(testUser, renderer)
        screens1 <- renderer.sentScreens
        page1     = screens1.last.choices
        _        <- state.action(testUser, tap("SkillsNext"), renderer)
        screens2 <- renderer.sentScreens
        page2     = screens2.last.choices
      } yield assertTrue(page1.count(_.id.startsWith("ActiveSkill_")) + page1.count(_.id.startsWith("PassiveSkill_")) == 8) &&
              assertTrue(page1.exists(_.id == "SkillsNext")) &&
              assertTrue(!page1.exists(_.id == "SkillsPrev")) &&
              assertTrue(page2.count(_.id.startsWith("ActiveSkill_")) + page2.count(_.id.startsWith("PassiveSkill_")) == 3) &&
              assertTrue(page2.exists(_.id == "SkillsPrev")) &&
              assertTrue(!page2.exists(_.id == "SkillsNext"))
    }
  )
}
