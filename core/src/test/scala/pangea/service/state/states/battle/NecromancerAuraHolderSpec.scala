package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{Aura, MonsterSlot, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
import pangea.model.squad.{Ally, AllyKind, Squad, UndeadForm}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Миазмы Некроманта держит он сам, где бы ни стоял: в полях боя или в строю —
  * первым или за спинами своих мертвецов. Раньше в строю смотрели только
  * первого живого: стоит перед Некромантом его же мертвец — а так бывает с
  * первого раунда, если герой стоит за своим отрядом, и после перемешивания, —
  * и миазмы молча гасли: энергия цела, живые не гниют, святая вода жжёт. */
object NecromancerAuraHolderSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val necroKind = MiniBoss.Necromancer.entryName

  /** Человек, которого не свалит ни удар, ни миазмы за раунд. */
  private def hero(squad: Squad = Squad.empty): Hero = TestFixtures.hero(userId).copy(
    lvl        = 10L,
    fightStats = FightStats(atk = 1, hp = 5000, armor = 0, defence = 0, evasion = 9999, accuracy = 9999, energy = 60),
    baseStats  = TestFixtures.hero(userId).baseStats.copy(int = 20, str = 1),
    squad      = squad)

  /** Поднятый без запаса энергии: умений не применяет, миазмы его только лечат. */
  private def undeadAlly(pos: Int): Ally =
    Ally(AllyKind.Undead, pos, 400L, 0L, 0L,
      undead = Some(UndeadForm("Поднятый орк", 10L,
        FightStats(atk = 50, hp = 1000, armor = 200, defence = 10, evasion = 10, accuracy = 100, energy = 0))))

  /** Герой за спинами своего отряда: двое поднятых на местах 1 и 2, он — на 3-м. */
  private val behindSquad: Hero = hero(Squad(heroPos = 3, allies = List(undeadAlly(1), undeadAlly(2))))

  /** Логово, как его собирает пещера: Некромант во главе, с полной энергией, за
    * ним — двое его мертвецов. Все на сто тысяч HP и никого не убьют. */
  private def lair(h: Hero): SoloPveBattle = {
    def stats(energy: Long) =
      FightStats(atk = 1, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = energy)
    val necro  = Monster(0L, 10L, Race.Undead, Rarity.Legendary, stats(1000L),
                   customName = Some(MiniBoss.Necromancer.monsterName))
    val guards = List.fill(2)(Monster(0L, 10L, Race.Orc, Rarity.Common, stats(0L)).copy(undead = true))
    SoloPveBattle.fromGroup(necro :: guards, h, List(1000L, 0L, 0L), headBoss = Some(MiniBoss.Necromancer))
      .copy(noKin = true)
  }

  /** Некромант в строю: кто стоит там и под каким индексом. */
  private def necroInLine(b: SoloPveBattle): Option[(MonsterSlot, Int)] =
    b.group.others.zipWithIndex.find(_._1.boss.contains(necroKind))

  /** Ход игрока: куда ушёл бой, каким он стал и что увидел игрок. */
  private def turn(h: Hero, b: SoloPveBattle): Task[(StateType, Option[SoloPveBattle], String)] =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      to    <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Wait"), r)
      after <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (to, after, log)

  override def spec = suite("Миазмы держит Некромант, где бы он ни стоял")(

    test("герой за отрядом: Некромант с первого раунда в строю за своим мертвецом — и держит ауру, и платит за неё сам") {
      val b = lair(behindSquad)
      val paid = b.spendAuraEnergy(100L)
      // в паре с героем — мертвец, Некромант в строю вторым, а первым там — живой мертвец
      assertTrue(b.bossKind.isEmpty && b.group.others.headOption.exists(s => s.alive && s.boss.isEmpty)) &&
      assertTrue(necroInLine(b).map(_._2).contains(1)) &&
      assertTrue(b.auraHolder.contains(Aura.Miasma -> 1000L)) &&
      // энергию тратит он, а не тот, кто стоит перед ним
      assertTrue(necroInLine(paid).exists(_._1.currentEnergy == 900L)) &&
      assertTrue(paid.group.others.headOption.map(_.currentEnergy) == b.group.others.headOption.map(_.currentEnergy))
    },

    test("спрятался за своих, а перемешивание поставило мертвеца напротив героя — ауру держит всё равно он") {
      // Некромант уходит за спины своих (место 4), перемешивание ставит мертвецов
      // на места 1 и 2, а его, держащего позицию, оставляет на месте
      val shuffled = lair(hero()).stepActiveTo(4).reshuffle(List(0, 1))
      assertTrue(shuffled.bossKind.isEmpty && necroInLine(shuffled).map(_._2).contains(1)) &&
      assertTrue(shuffled.auraHolder.contains(Aura.Miasma -> 1000L))
    },

    test("павший Некромант ауры не держит — ни в полях, ни в строю") {
      val b      = lair(hero())
      val fallen = b.copy(monsterCurrentHp = 0L)
      assertTrue(b.bossKind.contains(necroKind) && b.auraHolder.contains(Aura.Miasma -> 1000L)) &&
      assertTrue(fallen.auraHolder.isEmpty) &&
      // и энергию с павшего не списывают
      assertTrue(fallen.spendAuraEnergy(100L) == fallen)
    },

    test("в бою: Некромант за спиной своего мертвеца держит миазмы — живого героя они гноят") {
      for {
        out <- turn(behindSquad, lair(behindSquad))
        (to, after, log) = out
        necro = after.flatMap(a => (a.activeSlot :: a.group.others).find(_.boss.contains(necroKind)))
      } yield assertTrue(log.contains("🟣 Миазмы Некроманта держат поле боя")) &&
              assertTrue(log.contains("🟣 Тьма гложет и вас")) &&
              assertTrue(to == StateType.Battle && necro.exists(_.currentEnergy < 1000L))
    },

    test("в бою: с ним и святая вода гаснет, хоть он и за спиной своего мертвеца") {
      for {
        out <- turn(behindSquad, lair(behindSquad).copy(holyWater = true))
        (_, _, log) = out
      } yield assertTrue(log.contains("💧 Святая вода и миазмы тьмы гасят друг друга.")) &&
              assertTrue(!log.contains("💧 Святая вода: каждый мёртвый"))
    }
  )
}
