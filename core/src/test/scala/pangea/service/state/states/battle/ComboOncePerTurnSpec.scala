package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.skill.{MonsterEnergy, Skill}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Комбо стихий — не больше одного за ход героя, Огонь с Холодом в одном
  * броске гасят друг друга и в комбо не идут, а соседи, которых задевает
  * умение, тоже получают проки стихий оружия. */
object ComboOncePerTurnSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))

  private val lvl = 10L

  /** Строка комбо Огонь+Воздух и строка комбо Молния+Холод. */
  private val FireAir   = "Огонь и воздух, объединив усилия"
  private val LightCold = "сковали и душу"
  private val Steam     = "Огонь и холод в одном ударе гасят друг друга"
  private val AirSingle = "Шквальный ветер"
  private val Lightning = "выжгла"

  /** Все броски по 10: проки стихий (шанс 30%) срабатывают, промахов (бросок не
    * выше 5) нет. */
  private val procsFire = TestRandom.feedInts(List.fill(200)(10): _*) *> TestRandom.feedLongs(List.fill(50)(100L): _*)

  /** Оружие с «Размашистым ударом» и камнями `gems`. */
  private def weapon(gems: GemKind*): Item =
    Item(101L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Weapon(Skill.SweepingStrike),
      sockets = gems.toList.map(k => Some(Gem(k, 1))))

  private def hero(gems: GemKind*): Hero = {
    val h = TestFixtures.hero(userId)
    h.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 20, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 10_000L),
      baseStats  = h.baseStats.copy(str = 50, int = 50, vit = 5000, agi = 50),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon(gems: _*)))
  }

  /** Крепкий моб: переживёт и комбо (30% здоровья), и удар. */
  private def monster: Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = 1, hp = 1000000L, armor = 0, defence = 0, evasion = 0, accuracy = 9999,
                 energy = MonsterEnergy.maxEnergy(lvl)))

  /** Строй из `n` мобов; умения готовы сразу. */
  private def group(h: Hero, n: Int): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(List.fill(n)(monster), h, Nil)
    b.copy(skillSlots = b.skillSlots.map(_.copy(cooldown = 0)))
  }

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def times(log: String, s: String): Int = log.split(java.util.regex.Pattern.quote(s), -1).length - 1

  override def spec = suite("Комбо стихий: раз за ход; проки по соседям")(

    test("«Размашистый удар» с Огнём и Воздухом: комбо по цели одно, сосед загорается от прока Огня") {
      val h = hero(GemKind.Ruby, GemKind.Diamond)
      for {
        t <- makeState(h, group(h, 3))
        (state, dao, r) = t
        _     <- procsFire
        _     <- state.action(testUser, aimed("Skill_101", 1), r)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        after <- battleOf(dao)
        // сосед цели — на месте 2
        side   = after.group.others.headOption
      } yield assertTrue(times(log, FireAir) == 1) &&
              assertTrue(log.contains("Дуга удара задевает")) &&
              // прок по соседу: комбо уже было — стихии легли поодиночке, Огонь поджёг
              assertTrue(side.exists(_.effects.monsterBurn.isDefined)) &&
              assertTrue(after.comboUsed)
    },

    test("в следующий ход комбо снова возможно: счёт — на каждый ход героя") {
      val h = hero(GemKind.Ruby, GemKind.Diamond)
      for {
        t <- makeState(h, group(h, 1).copy(comboUsed = true))
        (state, _, r) = t
        _   <- procsFire
        _   <- state.action(testUser, tap("Attack"), r)
        log <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(times(log, FireAir) == 1)
    },

    test("Огонь и Холод разом гасят друг друга и в Огонь+Воздух не идут — Воздух срабатывает сам") {
      val h = hero(GemKind.Ruby, GemKind.Sapphire, GemKind.Diamond)
      for {
        t <- makeState(h, group(h, 1))
        (state, dao, r) = t
        _     <- procsFire
        _     <- state.action(testUser, tap("Attack"), r)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        after <- battleOf(dao)
      } yield assertTrue(log.contains(Steam) && !log.contains(FireAir)) &&
              assertTrue(log.contains(AirSingle) && !after.comboUsed) &&
              // ни пламени, ни мороза на цели
              assertTrue(after.effects.monsterBurn.isEmpty && after.effects.monsterColdDefenceCut == 0)
    },

    test("Огонь и Холод разом не пускают и в Молния+Холод — Молния срабатывает сама") {
      val h = hero(GemKind.Ruby, GemKind.Sapphire, GemKind.Topaz)
      for {
        t <- makeState(h, group(h, 1))
        (state, dao, r) = t
        _     <- procsFire
        _     <- state.action(testUser, tap("Attack"), r)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        after <- battleOf(dao)
      } yield assertTrue(log.contains(Steam) && !log.contains(LightCold) && log.contains(Lightning)) &&
              assertTrue(after.effects.monsterSkillBlockedTurns == 0 && !after.comboUsed)
    },

    test("все четыре стихии разом — ни одного комбо: Огонь с Холодом гасят друг друга") {
      val h = hero(GemKind.Ruby, GemKind.Diamond, GemKind.Topaz, GemKind.Sapphire)
      for {
        t <- makeState(h, group(h, 1))
        (state, _, r) = t
        _   <- procsFire
        _   <- state.action(testUser, tap("Attack"), r)
        log <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains(Steam) && !log.contains(FireAir) && !log.contains(LightCold))
    },

    test("без стихий в оружии по соседям ничего не бросается — как прежде") {
      val h = hero()
      for {
        t <- makeState(h, group(h, 3))
        (state, dao, r) = t
        _     <- procsFire
        _     <- state.action(testUser, aimed("Skill_101", 1), r)
        after <- battleOf(dao)
      } yield assertTrue(after.group.others.forall(_.effects.monsterBurn.isEmpty) && !after.comboUsed)
    }
  )
}
