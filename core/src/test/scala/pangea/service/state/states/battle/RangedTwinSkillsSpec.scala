package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Grip, Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
import pangea.model.skill.Skill
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Выстрелы по образцу ударов: тот же приём, что у умения ближнего боя, только
  * из лука или арбалета — то же действие, перезарядка, цена и урон. Отличают
  * их лишь общие правила стрелка: дальность оружия и слабость вблизи. */
object RangedTwinSkillsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))

  /** Выстрел и удар, по образцу которого он сделан. */
  private val twins: List[(Skill, Skill)] = List(
    Skill.QuickShot    -> Skill.QuickStrike,
    Skill.CunningShot  -> Skill.CunningStrike,
    Skill.BleedingShot -> Skill.Bleeding,
    Skill.WeakSpotShot -> Skill.WeakSpotStrike,
    Skill.SplitShot    -> Skill.SweepingStrike
  )

  private val lvl = 10L

  private def bow(skill: Skill): Item =
    Item(101L, "Лук", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(Grip.Bow),
      details = ItemDetails.Weapon(skill))

  private def hero(right: Item): Hero = {
    val base = TestFixtures.hero(userId)
    base.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 100L, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 10_000L),
      baseStats  = base.baseStats.copy(str = 7, agi = 11, int = 13, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = right))
  }

  private val full = 100000L

  private def monster: Monster =
    Monster(0L, lvl, Race.Human, Rarity.Common,
      FightStats(atk = 1, hp = full, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  /** Строй: первый моб в паре с героем на первом месте, остальные — на `places`. */
  private def battle(h: Hero, places: List[Int]): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(List.fill(places.size + 1)(monster), h, Nil)
    b.copy(skillSlots = b.skillSlots.map(_.copy(cooldown = 0)), group = b.group.copy(places = places))
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

  /** Хвост описания про статы и энергию — с первой фразы о влиянии на урон. */
  private def statsPart(s: Skill): String = {
    val d = s.description
    d.substring(d.lastIndexOf(". ", d.indexOf(" влияние")) + 2)
  }

  override def spec = suite("Выстрелы по образцу ударов")(

    test("выстрел — близнец удара: то же действие, перезарядка, цена, урон и строка о статах") {
      val h    = hero(bow(Skill.QuickShot))
      val same = twins.forall { case (shot, strike) =>
        shot.effect == strike.effect && shot.cooldown == strike.cooldown &&
        shot.initialCooldown == strike.initialCooldown &&
        shot.baseValue(h, 0L) == strike.baseValue(h, 0L) && shot.energyCost(h) == strike.energyCost(h) &&
        statsPart(shot) == statsPart(strike)
      }
      val named = twins.map(_._1.label)
      assertTrue(same) &&
      assertTrue(named == List("Быстрый выстрел", "Хитрый выстрел", "Кровопускающий выстрел",
                               "Выстрел в слабое место", "Расщепляющий выстрел"))
    },

    test("выстрелы — умения стрелка: только с луком или арбалетом, и описание об этом говорит") {
      val h       = hero(bow(Skill.QuickShot))
      val shots   = twins.map(_._1)
      val onlyBow = shots.forall(s => Skill.usableWith(s, ranged = true) && !Skill.usableWith(s, ranged = false))
      val told    = shots.forall(_.describe(h).contains("Только с луком или арбалетом."))
      val listed  = shots.forall(s => Skill.rangedSkills.contains(s) && !Skill.weaponSkills.contains(s))
      assertTrue(onlyBow && told && listed)
    },

    test("у каждого выстрела — своя руна: камень с узором, а не общий вид") {
      val runes = twins.map { case (shot, _) => Rune.Active(shot) }
      val drawn = runes.forall(r => !RuneStone.look(r).engraving.startsWith("Узор врезан глубоко"))
      val drops = runes.forall(r => RuneStone.all.contains(r))
      assertTrue(drops && drawn) &&
      assertTrue(RuneStone.name(Rune.Active(Skill.CunningShot), RuneStoneSize.Big) == "Большая руна Хитрого выстрела")
    },

    test("«Расщепляющий выстрел» бьёт на всю дальность лука, а осколки достаются соседям цели") {
      val h = hero(bow(Skill.SplitShot))
      for {
        t <- makeState(h, battle(h, List(3, 4, 5)))
        (state, dao, r) = t
        _     <- state.action(testUser, aimed("Skill_101", 4), r)
        after <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("Стрела раскололась о врага") && log.contains("Осколки стрелы задевают")) &&
              assertTrue(!log.contains("Дуга удара")) &&
              // цель на четвёртом и её соседи на третьем и пятом (пятый луку не
              // достать — но осколкам всё равно), где бы они ни стояли после раунда
              assertTrue(after.group.others.count(_.currentHp < full) >= 3)
    },

    test("«Кровопускающий выстрел» пускает кровь и дальней цели") {
      val h = hero(bow(Skill.BleedingShot))
      for {
        t <- makeState(h, battle(h, List(4)))
        (state, dao, r) = t
        _     <- state.action(testUser, aimed("Skill_101", 4), r)
        after <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("Зазубренный наконечник вошёл глубоко")) &&
              assertTrue(after.group.others.exists(m => m.currentHp < full && m.effects.monsterBleed.isDefined))
    }
  )
}
