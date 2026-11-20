package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Grip, Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.rune.{Rune, RuneStone}
import pangea.model.skill.Skill
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** «Мощный выстрел»: кнопкой тетиву натягивают — ход без урона; пока она
  * натянута, та же кнопка — «Тянуть дальше» (+50%, не больше трёх раз), а
  * «Атака» спускает стрелу, и дальше всё как у любого умения. Рука, держащая
  * тетиву, не берёт ни пояса, ни других умений; выжидать и шагать — можно. */
object PowerShotSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val lvl = 10L

  private val bow: Item =
    Item(101L, "Лук", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(Grip.Bow),
      details = ItemDetails.Weapon(Skill.PowerShot))

  /** Таран на кирасе — умение, которое рука с тетивой не возьмёт. */
  private val chest: Item =
    Item(202L, "Кираса", 1L, ItemRarity.Gray, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Armor(Skill.Ram))

  private val belt: Item =
    Item(303L, "Пояс", 1L, ItemRarity.Gray, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Belt())

  private val hero: Hero = {
    val base = TestFixtures.hero(userId)
    base.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 100L, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 10_000L),
      baseStats  = base.baseStats.copy(str = 7, agi = 11, int = 13, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = bow, chestPlate = chest, belt = belt))
  }

  private val full = 100000L

  /** Противник без стихии на оружии: у человека молния, и её прок тратит свой
    * бросок — броски здесь не заданы, и от любой поправки в ударе моба выстрел
    * мог бы уйти в те 5% промаха, что остаются всегда. */
  private def monster: Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = 1, hp = full, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  /** Один моб в паре с героем; умения готовы. */
  private def paired: SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(List(monster), hero, Nil)
    b.copy(skillSlots = b.skillSlots.map(_.copy(cooldown = 0)))
  }

  private def makeState(b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, hero)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def shotSlot(b: SoloPveBattle) = b.slotByItem(101L).get

  private val shotLine = """«Мощный выстрел» ×([0-9,]+) → [^:\n]+: (\d+) урона""".r

  /** Натянуть, `holds` раз потянуть дальше и спустить «Атакой»: множитель и урон выстрела. */
  private def shootAfter(holds: Int) =
    for {
      t <- makeState(paired)
      (state, _, r) = t
      _    <- state.action(testUser, tap("Skill_101"), r)
      _    <- ZIO.foreachDiscard(1 to holds)(_ => state.action(testUser, tap("Skill_101"), r))
      _    <- state.action(testUser, tap("Attack"), r)
      log  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield shotLine.findFirstMatchIn(log).map(m => (m.group(1), m.group(2).toLong))

  override def spec = suite("«Мощный выстрел»")(

    test("натянуть: урона нет, ход кончается; вместо умений — «Тянуть дальше (×1,5)»") {
      for {
        t <- makeState(paired)
        (state, dao, r) = t
        _      <- state.action(testUser, tap("Skill_101"), r)
        after  <- battleOf(dao)
        screen <- r.sentScreens.map(_.last)
        log    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("🏹 «Мощный выстрел»: тетива натянута.")) &&
              assertTrue(after.monsterCurrentHp == full && shotSlot(after).aim.contains(0)) &&
              assertTrue(shotSlot(after).cooldown == 0 && shotSlot(after).uses == 0) &&
              // ход ушёл: моб ответил, раунд кончился
              assertTrue(after.roundsFought == 1) &&
              assertTrue(screen.text.contains("Тетива натянута: мощный выстрел ×1.")) &&
              assertTrue(screen.choices.exists(_.label == "🏹 Тянуть дальше (×1,5)")) &&
              assertTrue(!screen.choices.exists(c => c.label == "Таран" || c.label == "Мощный выстрел")) &&
              assertTrue(screen.choices.exists(_.id == "Attack"))
    },

    test("тянуть дальше можно трижды — ×2,5; на четвёртый тетива на пределе, ход не тратится") {
      for {
        t <- makeState(paired)
        (state, dao, r) = t
        _      <- state.action(testUser, tap("Skill_101"), r)
        _      <- ZIO.foreachDiscard(1 to 3)(_ => state.action(testUser, tap("Skill_101"), r))
        third  <- battleOf(dao)
        screen <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("Skill_101"), r)
        fourth <- battleOf(dao)
        said   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(shotSlot(third).aim.contains(3) && third.roundsFought == 4) &&
              assertTrue(said.contains("🏹 «Мощный выстрел»: тетива туже — ×2,5.")) &&
              assertTrue(screen.text.contains("Тетива натянута до предела: мощный выстрел ×2,5.")) &&
              assertTrue(!screen.choices.exists(_.label.startsWith("🏹 Тянуть дальше"))) &&
              assertTrue(said.contains("дальше не потянуть") && fourth.roundsFought == 4 && shotSlot(fourth).aim.contains(3))
    },

    test("«Атака» спускает стрелу как умение: урон, следом обычная атака, перезарядка — с этой минуты") {
      for {
        t <- makeState(paired)
        (state, dao, r) = t
        _     <- state.action(testUser, tap("Skill_101"), r)
        _     <- state.action(testUser, tap("Skill_101"), r)
        _     <- state.action(testUser, tap("Attack"), r)
        after <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("🏹 «Мощный выстрел» ×1,5 →")) &&
              assertTrue(after.monsterCurrentHp < full) &&
              assertTrue(shotSlot(after).aim.isEmpty && shotSlot(after).cooldown == Skill.PowerShot.cooldown) &&
              assertTrue(shotSlot(after).uses == 1)
    },

    test("чем дольше тянул, тем сильнее: ×2,5 бьёт больше чем в полтора раза сильнее ×1") {
      for {
        once  <- shootAfter(0)
        three <- shootAfter(3)
      } yield {
        // разброс ±20% не перекроет: ×2,5 против ×1 — не меньше чем в 1,67 раза
        val stronger = once.zip(three).exists { case ((_, low), (_, high)) => high * 2 > low * 3 }
        assertTrue(once.map(_._1).contains("1") && three.map(_._1).contains("2,5")) && assertTrue(stronger)
      }
    },

    test("пока тетива натянута, рука занята: пояс и другие умения не выходят, ход не тратится") {
      for {
        t <- makeState(paired)
        (state, dao, r) = t
        _     <- state.action(testUser, tap("Skill_101"), r)
        _     <- state.action(testUser, tap("Belt"), r)
        _     <- state.action(testUser, tap("Skill_202"), r)
        after <- battleOf(dao)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(said.contains("Рука держит тетиву — до пояса сейчас не дотянуться.")) &&
              assertTrue(said.contains("Рука держит тетиву — другие умения сейчас не выйдут.")) &&
              assertTrue(after.roundsFought == 1 && shotSlot(after).aim.contains(0) && after.monsterCurrentHp == full)
    },

    test("бить некого: «Ждать» тетиву не опускает, «Выстрелить» — стрела мимо, умение на перезарядке") {
      // Моб далеко, луку не достать; тетива уже натянута раз.
      val far = {
        val b = paired
        b.copy(group = b.group.copy(activePos = 9))
          .updateSlot(101L)(_.copy(aim = Some(1)))
      }
      for {
        t <- makeState(far)
        (state, dao, r) = t
        _      <- state.action(testUser, tap("Wait"), r)
        waited <- battleOf(dao)
        screen <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("LooseAim"), r)
        after  <- battleOf(dao)
        said   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(shotSlot(waited).aim.contains(1)) &&
              assertTrue(screen.choices.exists(c => c.id == "LooseAim" && c.label == "🏹 Выстрелить")) &&
              assertTrue(said.contains("Стрела уходит мимо: стрелять было не в кого.")) &&
              assertTrue(shotSlot(after).aim.isEmpty && shotSlot(after).cooldown == Skill.PowerShot.cooldown) &&
              assertTrue(after.monsterCurrentHp == full)
    },

    test("руна и описание: свой камень, лук и арбалет, натяжка в описании") {
      val look = RuneStone.look(Rune.Active(Skill.PowerShot))
      assertTrue(Skill.rangedSkills.contains(Skill.PowerShot) && look.genitive == "Мощного выстрела") &&
      assertTrue(Skill.PowerShot.describe(hero).contains("Только с луком или арбалетом.")) &&
      assertTrue(Skill.PowerShot.effect == Skill.Effect.Charge(holdPct = 50, maxHolds = 3)) &&
      assertTrue(BattleState.drawMult(50, 0) == "1" && BattleState.drawMult(50, 1) == "1,5" &&
                 BattleState.drawMult(50, 3) == "2,5")
    }
  )
}
