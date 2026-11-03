package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.hero.Hero
import pangea.model.item.{Chisel, FlaskKind, Grip, Item, ItemDetails, ItemType, MaterialKind, PassiveKind, Rarity}
import pangea.model.skill.Skill
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** «Резец рунописца»: переписывает узор на вещи — умение оружия и нагрудника
  * или пассивку прочего снаряжения — на другой из тех, что бывают у такой
  * вещи, и не на нынешний. Применяется из сумки к надетому и к лежащему в ней. */
object RuneChiselSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))
  private def selectItem(itemId: Long): UserAction =
    UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}$itemId"}"""))

  private def gear(id: Long, name: String, t: ItemType, details: ItemDetails = ItemDetails.Plain,
                   grip: Option[Grip] = None): Item =
    Item(id, name, 5L, Rarity.Blue, t, attack = 1, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0,
      grip = grip, details = details)

  private val chisel  = MaterialGenerator.item(MaterialKind.RuneChisel).copy(id = 900L)
  private val helm    = gear(20L, "Шлем", ItemType.Helmet, ItemDetails.Passive(PassiveKind.Stealthy))
  private val ring    = gear(30L, "Перстень", ItemType.Ring, ItemDetails.Passive(PassiveKind.Jeweler))
  private val stashed = gear(40L, "Сапоги", ItemType.Boots, ItemDetails.Passive(PassiveKind.Stash))

  private val baseHero = TestFixtures.hero(userId)
  private def wearing(items: Item*): Hero =
    baseHero.copy(equipment = items.foldLeft(baseHero.equipment)((eq, i) => eq.replacing(i)))

  private def makeState(hero: Hero, items: List[Item]) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (InventoryState(heroDao, invRepo, TestItemRepository.make, content, None), heroDao, invRepo, renderer)

  override def spec = suite("«Резец рунописца»")(

    test("узор меняется только на тот, что бывает у такой вещи, и не на нынешний") {
      val sword  = gear(1L, "Меч", ItemType.Weapon, ItemDetails.Weapon(Skill.QuickStrike), Some(Grip.OneHanded))
      val bow    = gear(2L, "Лук", ItemType.Weapon, ItemDetails.Weapon(Skill.PinningShot), Some(Grip.Bow))
      val chest  = gear(3L, "Кираса", ItemType.ChestPlate, ItemDetails.Armor(Skill.Ram))
      val bare   = gear(4L, "Голый шлем", ItemType.Helmet)
      def skills(i: Item) = Chisel.variants(i).flatMap(_.activeSkill).toSet
      assertTrue(skills(sword) == (Skill.weaponSkills.toSet - Skill.QuickStrike)) &&
      assertTrue(skills(bow) == (Skill.rangedSkills.toSet - Skill.PinningShot)) &&
      assertTrue(skills(chest) == (Skill.armorSkills.toSet - Skill.Ram)) &&
      assertTrue(Chisel.variants(ring).flatMap(_.passive).toSet ==
                 (PassiveKind.poolFor(ItemType.Ring).toSet - PassiveKind.Jeweler)) &&
      // у шлема пассивок две: «Скрытность» сменится только на «Охотника»
      assertTrue(Chisel.variants(helm).flatMap(_.passive) == List(PassiveKind.Hunter)) &&
      // вещь без пассивки, у которой она бывает, получает новую
      assertTrue(Chisel.variants(bare).flatMap(_.passive).toSet == PassiveKind.poolFor(ItemType.Helmet).toSet) &&
      // остальное в вещи не трогается
      assertTrue(Chisel.variants(sword).forall(v => v.copy(details = sword.details) == sword))
    },

    test("щит, парирующий кинжал, фляга, пояс и материалы резцу не даются") {
      val shield = gear(5L, "Щит", ItemType.Shield, ItemDetails.HandSkill(Skill.Block))
      val dagger = gear(6L, "Кинжал", ItemType.Weapon, ItemDetails.HandSkill(Skill.Counterstrike), Some(Grip.Parrying))
      val flask  = gear(7L, "Фляга", ItemType.Flask, ItemDetails.Flask(FlaskKind.Healer.effect, 3, 3))
      val belt   = gear(8L, "Пояс", ItemType.Belt, ItemDetails.Belt())
      assertTrue(List(shield, dagger, flask, belt, chisel).forall(i => !Chisel.fits(i)))
    },

    test("надетый шлем: список, подтверждение — и «Скрытность» становится «Охотником», резец рассыпается") {
      for {
        t <- makeState(wearing(helm), List(chisel, ring))
        (state, heroDao, invRepo, r) = t
        _      <- state.action(testUser, selectItem(chisel.id), r)
        card   <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("ChiselUse"), r)
        list   <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("ChiselPick", "item" -> helm.id.toString), r)
        ask    <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("ChiselDo", "item" -> helm.id.toString), r)
        hero   <- heroDao.getHeroByUserId(userId).map(_.get)
        inv    <- invRepo.get(baseHero.id)
        log    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(card.choices.exists(_.id == "ChiselUse")) &&
              assertTrue(list.choices.filter(_.id == "ChiselPick").map(_.label) ==
                List("🧍 Шлем — Скрытность", "🎒 Перстень — Ювелир")) &&
              assertTrue(ask.text.contains("Переписать узор на «Шлем»? Сейчас: Скрытность.")) &&
              assertTrue(hero.equipment.helmet.passive.contains(PassiveKind.Hunter)) &&
              assertTrue(!inv.items.data.exists(_.material.contains(MaterialKind.RuneChisel))) &&
              assertTrue(log.contains("Узор на «Шлем» переписан: Скрытность → Охотник."))
    },

    test("вещь в сумке: пассивка меняется на другую из её пула") {
      for {
        t <- makeState(baseHero, List(chisel, ring))
        (state, _, invRepo, r) = t
        _   <- state.action(testUser, tap("ChiselDo", "item" -> ring.id.toString), r)
        inv <- invRepo.get(baseHero.id)
        now  = inv.items.data.find(_.id == ring.id).flatMap(_.passive)
      } yield assertTrue(now.exists(p => p != PassiveKind.Jeweler && PassiveKind.poolFor(ItemType.Ring).contains(p))) &&
              assertTrue(!inv.items.data.exists(_.id == chisel.id))
    },

    test("сапоги с «Тайником»: сколько мест освободить — видно в списке и при выборе, до подтверждения не дойти") {
      // 14 вещей и резец: без «Тайника» сумке в 20 мест останется 10, а лежать
      // в ней будет 14 — освободить 4
      val stuff = (100L until 114L).toList.map(i => gear(i, s"Вещь $i", ItemType.Gloves))
      for {
        t <- makeState(wearing(stashed), chisel :: stuff)
        (state, heroDao, invRepo, r) = t
        _    <- state.action(testUser, selectItem(chisel.id), r)
        _    <- state.action(testUser, tap("ChiselUse"), r)
        list <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("ChiselPick", "item" -> stashed.id.toString), r)
        pick <- r.sentScreens.map(_.takeRight(2).map(_.text).mkString("\n"))
        // старая кнопка подтверждения — та же проверка
        _    <- state.action(testUser, tap("ChiselDo", "item" -> stashed.id.toString), r)
        hero <- heroDao.getHeroByUserId(userId).map(_.get)
        inv  <- invRepo.get(baseHero.id)
        log  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(list.text.contains("«Сапоги»: резец снимет с неё «Тайник»") && list.text.contains("мест: 4")) &&
              assertTrue(pick.contains("освободите в сумке мест: 4") && !log.contains("Переписать узор на «Сапоги»")) &&
              assertTrue(hero.equipment.boots.passive.contains(PassiveKind.Stash)) &&
              assertTrue(inv.items.data.exists(_.id == chisel.id))
    },

    test("нет вещей под резец — так и сказано") {
      for {
        t <- makeState(baseHero, List(chisel))
        (state, _, _, r) = t
        _    <- state.action(testUser, tap("ChiselUse"), r)
        last <- r.sentScreens.map(_.last)
      } yield assertTrue(last.text.contains("Переписывать нечего"))
    }
  )
}
