package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{ArenaRef, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.monster.{Race, Rarity}
import pangea.model.skill.MonsterSkill
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test._
import zio.ZIO
import zio.test._

/** «Сеть» ловцов Братства (MonsterSkill.CatcherNet, Акт II, глава 7): урона нет,
  * а тот, на кого она упала, — герой или союзник перед ловцом — остаток раунда
  * и весь следующий не сходит с места. Только у людей и только в бою с
  * ловцами; сеть на герое снимается в конце раунда, как паутина, — на арене
  * пригвождение по-прежнему считает сама арена. */
object CatcherNetSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private val content                = ZIO.attempt(SceneContent.load())

  private def fighter(squad: Squad = Squad()): Hero =
    TestFixtures.hero(userId, dungeonLevel = 31).copy(lvl = 31L, squad = squad,
      fightStats = FightStats(atk = 10, hp = 1000000L, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 500))

  /** Ловец в паре, энергии — только на сеть: прочие умения на 31-м дороже. */
  private def catcher(h: Hero): SoloPveBattle = {
    val mob = MonsterGenerator.generateOfRaceAndRarity(31, Race.Human, Rarity.Rare)
    SoloPveBattle.from(mob, h).copy(monsterCurrentEnergy = 50L, story = Some(MonsterSkill.CatcherNet.Story), noKin = true)
  }

  /** Ход героя «Ждать» — ловец ходит своим ходом, раунд кончается. */
  private def waitTurn(h: Hero, battle: SoloPveBattle) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- d.writeActiveBattle(userId, battle.asJson)
      r <- TestRenderer.make
      c <- content
      _ <- TestRandom.setSeed(7L)
      _ <- BattleState(d, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Wait"), r)
      t <- texts(r)
      b <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
    } yield (t, b)

  private val front = Squad(heroPos = 2, allies = List(Ally(AllyKind.Human, 1, 100000L, 100000L, 500L)))

  override def spec = suite("Сеть ловцов")(

    test("20 энергии на любом уровне, только у людей и только в бою с ловцами; урона нет, герой два конца раунда не сходит с места; опутанного второй раз не опутывают") {
      val net    = MonsterSkill.CatcherNet
      val h      = fighter().copy(fightStats = fighter().fightStats.copy(armor = 1000L))
      val battle = catcher(h)
      val cast   = net.cast(battle, h, 0L)
      assertTrue(net.cost(1L) == 20L && net.cost(150L) == 20L) &&
      assertTrue(net.availableTo(Race.Human) && !net.availableTo(Race.Orc)) &&
      assertTrue(net.applicable(battle) && !net.applicable(battle.copy(story = None)) && !net.applicable(cast.battle)) &&
      assertTrue(cast.heroHp == h.fightStats.hp && cast.heroArmor == 1000L &&
        cast.battle.effects.heroRootedTurns == net.Turns && cast.battle.effects.heroRooted) &&
      assertTrue(cast.line.contains("«Сеть» — вы в сети 🪢, с места не сойти")) &&
      assertTrue(net.againstOther.exists(o => o.roots == net.Turns && o.fixed.isEmpty && o.factor == 0.0))
    },

    test("в бою: ловец в паре — сеть на героя, и к следующему раунду она ещё держит; перед ловцом союзник — сеть на союзника") {
      for {
        onHero <- waitTurn(fighter(), catcher(fighter()))
        onAlly <- waitTurn(fighter(front), catcher(fighter(front)))
      } yield assertTrue(onHero._1.contains("«Сеть» — вы в сети 🪢") && onHero._2.exists(_.effects.heroRooted)) &&
              assertTrue(onAlly._1.contains("«Сеть» → Йорген Кремень: в сети 🪢, с места не сойти") &&
                onAlly._2.exists(_.group.allies.exists(_.rooted > 0)) && onAlly._2.exists(!_.effects.heroRooted))
    },

    test("конец раунда: в бою с ловцами сеть держит на раунд меньше — на герое и на союзнике; на арене и в прочих боях пригвождение не трогаем") {
      val netted = catcher(fighter(front))
      val start  = netted.copy(effects = netted.effects.copy(heroRootedTurns = 2),
                     group = netted.group.copy(allies = netted.group.allies.map(_.copy(rooted = 2))))
      val once   = BattleState.webTicked(start)
      val twice  = BattleState.webTicked(once)
      val arena  = BattleState.webTicked(start.copy(arena = Some(ArenaRef(1L, 2L))))
      val other  = BattleState.webTicked(start.copy(story = None))
      assertTrue(once.effects.heroRootedTurns == 1 && once.effects.heroRooted && once.group.allies.forall(_.rooted == 1)) &&
      assertTrue(twice.effects.heroRootedTurns == 0 && !twice.effects.heroRooted && twice.group.allies.forall(_.rooted == 0)) &&
      assertTrue(arena.effects.heroRootedTurns == 2 && arena.group.allies.forall(_.rooted == 2)) &&
      assertTrue(other.effects.heroRootedTurns == 2 && other.group.allies.forall(_.rooted == 2))
    },

    test("строки: сеть держит героя — своя; по союзнику — своя") {
      for {
        c <- content
      } yield assertTrue(c.text("battle.netHolds").contains("Сеть держит") &&
                c.text("battle.ally.mobSkillNet").contains("{name}: в сети 🪢"))
    }
  )
}
