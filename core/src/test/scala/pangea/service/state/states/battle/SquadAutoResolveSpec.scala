package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.schedule.TaskKind
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Бой без героя доигрывается сам: три раунда идут по таймеру, а на следующем
  * такте отряд доводит бой до конца — победой, смертью героя или ничьей, — и
  * игрок видит сводку по каждому союзнику. */
object SquadAutoResolveSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val lvl = 10L

  private def ally(kind: AllyKind, pos: Int = 2, hp: Option[Long] = None, armor: Option[Long] = None): Ally = {
    val s = kind.stats(lvl)
    Ally(kind, pos, hp.getOrElse(s.hp), armor.getOrElse(s.armor), 0L)
  }

  /** Герой уже лежит: HP ноль, рядом — отряд. */
  private def downHero(allies: List[Ally]): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 20L, hp = 0L, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0),
      squad      = Squad(heroPos = 1, allies = allies))

  private def monster(hp: Long, atk: Long): Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = atk, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  /** Один моб напротив союзника на месте 2; без героя отыграно `rounds` раундов. */
  private def fight(h: Hero, hp: Long, atk: Long, rounds: Int): SoloPveBattle = {
    val b0 = SoloPveBattle.fromGroup(List(monster(hp, atk)), h, Nil)
    b0.copy(noKin = true, group = b0.group.copy(heroDown = true, activePos = 2, downRounds = rounds))
  }

  private def world(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      sch      <- TestScheduler.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content, sch), dao, renderer, sch)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def heroOf(dao: TestHeroDao) = dao.getHeroByUserId(userId).map(_.get)

  private def said(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Бой без героя доигрывается сам")(

    test("три раунда — по таймеру и с логом, четвёртый такт доигрывает бой; верх не взял никто — ничья") {
      // Брамбл бьёт холодом — ран, которые сами сводят моба в могилу, у него нет;
      // мобу с миллиардом HP он ничего не сделает, а моб без атаки не сделает ничего
      // ему. Такой бой не кончается сам.
      val h = downHero(List(ally(AllyKind.Gnome)))
      for {
        t <- world(h, fight(h, hp = 1000000000L, atk = 0L, rounds = 0))
        (state, dao, r, sch) = t
        t1     <- state.action(testUser, tap("SquadTick"), r)
        b1     <- battleOf(dao)
        t2     <- state.action(testUser, tap("SquadTick"), r)
        b2     <- battleOf(dao)
        t3     <- state.action(testUser, tap("SquadTick"), r)
        b3     <- battleOf(dao)
        last   <- r.sentScreens.map(_.last)
        shown  <- said(r)
        timers <- sch.scheduled
        t4     <- state.action(testUser, tap("SquadTick"), r)
        after  <- sch.scheduled
        left   <- dao.readActiveBattle(userId)
        hero   <- heroOf(dao)
        all    <- said(r)
        ending  = all.drop(shown.length)
      } yield assertTrue(List(t1, t2, t3) == List.fill(3)(StateType.Battle)) &&
              assertTrue(List(b1, b2, b3).map(_.group.downRounds) == List(1, 2, 3)) &&
              // каждый из трёх раундов ставит следующий такт через полминуты
              assertTrue(timers.count(_.kind == TaskKind.SquadFight) == 3) &&
              // после третьего — предупреждение, что дальше бой доиграют
              assertTrue(last.text.contains("через полминуты он доведёт бой до конца") && last.hideKeyboard) &&
              assertTrue(!shown.contains("Как дрался отряд")) &&
              // четвёртый такт: бой доигран за раз — ничья, таймеров больше нет
              assertTrue(t4 == StateType.Dungeon && left.isEmpty && after.size == timers.size) &&
              assertTrue(hero.fightStats.hp == BattleState.DownReviveHp) &&
              assertTrue(ending.contains("Как дрался отряд") && ending.contains("⚔ Брамбл Медноус — убито врагов: 0")) &&
              assertTrue(ending.contains("Бой затянулся, и верх не взял никто")) &&
              // выживший возвращается в отряд, как после любого боя
              assertTrue(hero.squad.has(AllyKind.Gnome))
    },

    test("доигранный бой выигран: награда как за победу, герой встаёт с 1 HP, в сводке — скольких добил союзник") {
      val h = downHero(List(ally(AllyKind.Gnome)))
      for {
        t <- world(h, fight(h, hp = 300L, atk = 0L, rounds = BattleState.ShownDownRounds))
        (state, dao, r, _) = t
        out  <- state.action(testUser, tap("SquadTick"), r)
        hero <- heroOf(dao)
        text <- said(r)
      } yield assertTrue(out == StateType.Loot && hero.fightStats.hp == BattleState.DownReviveHp) &&
              assertTrue(text.contains("Как дрался отряд")) &&
              // мобу нечем было умереть, кроме ударов Брамбла: холод ран не оставляет
              assertTrue(text.contains("⚔ Брамбл Медноус — убито врагов: 1")) &&
              assertTrue(text.contains("отряд отбился, и вы приходите в себя"))
    },

    test("моба добило пламя — в сводке он засчитан тому, кто бил его последним") {
      // Йорген бьёт в непробиваемую броню — HP моба не трогает, — а горение
      // снимает последние 50 HP. Раньше такого добитого не засчитывали никому:
      // у всех стояло «убито врагов: 0», хотя бой выиграл отряд.
      val h  = downHero(List(ally(AllyKind.Human)))
      val b0 = fight(h, hp = 1000L, atk = 0L, rounds = BattleState.ShownDownRounds)
      val b  = b0.copy(monsterCurrentHp = 50L, monsterCurrentArmor = 1000000L,
                 effects = b0.effects.copy(monsterBurn = Some(pangea.model.battle.Burn(10))))
      for {
        t <- world(h, b)
        (state, dao, r, _) = t
        // Йорген попал, без прока; моб попал (в броню)
        _    <- TestRandom.feedInts(99, 99, 99) *> TestRandom.feedLongs(100L, 100L)
        out  <- state.action(testUser, tap("SquadTick"), r)
        text <- said(r)
      } yield assertTrue(out == StateType.Loot) &&
              assertTrue(text.contains("⚔ Йорген Кремень — убито врагов: 1"))
    },

    test("доигранный бой проигран: пал весь отряд — смерть героя, в сводке — кто кого добил, павший не вернётся") {
      val h = downHero(List(ally(AllyKind.Gnome, hp = Some(50L), armor = Some(0L))))
      val b = fight(h, hp = 1000000000L, atk = 1000L, rounds = BattleState.ShownDownRounds)
      for {
        t <- world(h, b)
        (state, dao, r, _) = t
        out  <- state.action(testUser, tap("SquadTick"), r)
        hero <- heroOf(dao)
        text <- said(r)
        now  <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      } yield assertTrue(out == StateType.Death) &&
              assertTrue(text.contains(s"☠ Брамбл Медноус — убито врагов: 0. Кто добил: ${b.monsterName}")) &&
              assertTrue(text.contains("Последний из отряда")) &&
              // ушёл по свитку — как в обычном бою: из отряда вон, сутки в отлучке
              assertTrue(!hero.squad.has(AllyKind.Gnome) && hero.squad.isAway(AllyKind.Gnome, now))
    },

    test("союзника добило умение моба — он тоже уходит по свитку и сутки в отлучке") {
      // Раньше добитый умением оставался лежать в строю с нулём HP, а после боя
      // возвращался в отряд и вставал с первым же отдыхом.
      val h = downHero(List(ally(AllyKind.Human, hp = Some(1L), armor = Some(100000L))))
      val b = fight(h, hp = 1000000L, atk = 20L, rounds = 0).copy(monsterCurrentEnergy = 1000000L)
      for {
        t <- world(h, b)
        (state, dao, r, _) = t
        // Йорген попал, без прока; моб попал, но удар ушёл в броню; умение —
        // второе из равных по цене, «Дробящий удар»: мимо брони, прямо в HP.
        _    <- TestRandom.feedInts(50, 99, 100, 1) *> TestRandom.feedLongs(100L, 100L)
        out  <- state.action(testUser, tap("SquadTick"), r)
        hero <- heroOf(dao)
        text <- said(r)
        now  <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      } yield assertTrue(text.contains("«Дробящий удар» → Йорген Кремень")) &&
              assertTrue(text.contains("📜 Йорген Кремень: свиток — ушёл из боя.")) &&
              // он был последним — отложенная смерть героя
              assertTrue(out == StateType.Death) &&
              assertTrue(!hero.squad.has(AllyKind.Human) && hero.squad.isAway(AllyKind.Human, now))
    }
  )
}
