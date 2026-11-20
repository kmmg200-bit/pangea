package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Grip, Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.rune.{Rune, RuneStone}
import pangea.model.skill.Skill
import pangea.model.squad.AllyKind
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** «Перекат» на нагруднике: герой перекатывается по своему ряду на свободное
  * место, до трёх мест в сторону, и за каждое место получает +5% к уклонению
  * на этот раунд и следующий; потом — обычная атака с нового места. А ещё —
  * новые тексты «Отбросить»: щиты теперь свои, и нагрудник бьёт плечом. */
object RollSkillSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))

  private val lvl = 10L

  private val sword: Item =
    Item(101L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(Grip.OneHanded),
      details = ItemDetails.Weapon(Skill.QuickStrike))

  private val chest: Item =
    Item(202L, "Кираса", 1L, ItemRarity.Gray, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Armor(Skill.Roll))

  private val hero: Hero = {
    val base = TestFixtures.hero(userId)
    base.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 100L, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 10_000L),
      baseStats  = base.baseStats.copy(str = 7, agi = 11, int = 13, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = sword, chestPlate = chest))
  }

  private val full = 100000L

  // Люди шагают по месту за раунд (см. RaceTactics).
  private def monster: Monster =
    Monster(0L, lvl, Race.Human, Rarity.Common,
      FightStats(atk = 1, hp = full, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  /** Первый моб в паре с героем на первом месте, остальные — на `places`. */
  private def battle(places: List[Int]): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(List.fill(places.size + 1)(monster), hero, Nil)
    b.copy(skillSlots = b.skillSlots.map(_.copy(cooldown = 0)), group = b.group.copy(places = places))
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

  override def spec = suite("«Перекат» и «Отбросить»")(

    test("один на один: перекат на три места — +15% к уклонению на этот раунд и следующий, враг подходит сам") {
      for {
        t <- makeState(battle(Nil))
        (state, dao, r) = t
        _     <- state.action(testUser, aimed("Skill_202", 4), r)
        first <- battleOf(dao)
        log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        _     <- state.action(testUser, tap("Wait"), r)
        later <- battleOf(dao)
      } yield assertTrue(log.contains("💨 «Перекат»: на место 4") && log.contains("+15% к уклонению")) &&
              assertTrue(first.group.heroPos == 4 && first.heroBattleState.dodgeBonus == 15L) &&
              assertTrue(first.slotByItem(202L).exists(s => s.cooldown == Skill.Roll.cooldown && s.uses == 1)) &&
              // отошедший герой не в паре — враг шагнул к нему
              assertTrue(first.group.activePos > 1) &&
              // второй раунд прибавка ещё держала, после него — сошла
              assertTrue(later.heroBattleState.dodgeBonus == 0L)
    },

    test("на место союзника не перекатиться; пригвождённому — никуда, и ни хода, ни энергии это не стоит") {
      val rat     = BattleAlly(AllyKind.Rat, 2, 10L, 0L, 0L, FightStats(1, 10, 0, 0, 0, 1, 0), 1L)
      val withRat = battle(Nil).copy(group = battle(Nil).group.copy(allies = List(rat)))
      val pinned  = battle(Nil).copy(effects = battle(Nil).effects.copy(heroRootedTurns = 2))
      for {
        t <- makeState(pinned)
        (state, dao, r) = t
        _     <- state.action(testUser, aimed("Skill_202", 3), r)
        after <- battleOf(dao)
        h     <- dao.getHeroByUserId(userId).map(_.get)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(BattleState.rollSpots(withRat, 3) == List(3, 4)) &&
              assertTrue(BattleState.rollSpots(pinned, 3).isEmpty) &&
              assertTrue(said.contains("Вы пригвождены — не перекатиться.")) &&
              assertTrue(after.group.heroPos == 1 && after.roundsFought == 0) &&
              assertTrue(after.slotByItem(202L).exists(_.cooldown == 0) && h.fightStats.energy == hero.fightStats.energy)
    },

    test("в строю: экран мест с прибавкой; встал напротив врага — с ним в паре и бьёт его") {
      for {
        t <- makeState(battle(List(3)))
        (state, dao, r) = t
        _      <- state.action(testUser, tap("Skill_202"), r)
        screen <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, aimed("Skill_202", 3), r)
        after  <- battleOf(dao)
      } yield assertTrue(screen.text.contains("Перекат — куда?")) &&
              assertTrue(screen.choices.map(_.label).take(3) ==
                List("Место 2: +5% к уклонению", "Место 3: +10% к уклонению", "Место 4: +15% к уклонению")) &&
              assertTrue(after.group.heroPos == 3 && after.group.paired) &&
              assertTrue(after.monsterCurrentHp < full && after.heroBattleState.dodgeBonus == 10L)
    },

    test("«Отбросить» бьёт плечом, а не щитом; у «Переката» свой рунный камень") {
      assertTrue(Skill.Shove.hitTemplate == "💥 «Отбросить» → {target}: {} урона.") &&
      assertTrue(Skill.Shove.description.startsWith("Наплечники этого доспеха") && !Skill.Shove.description.contains("Умбон")) &&
      assertTrue(Skill.Shove.effect == Skill.Effect.Knockback) &&
      assertTrue(RuneStone.look(Rune.Active(Skill.Roll)).genitive == "Переката") &&
      assertTrue(Skill.armorSkills.contains(Skill.Roll) && !Skill.Roll.describe(hero).contains("луком"))
    }
  )
}
