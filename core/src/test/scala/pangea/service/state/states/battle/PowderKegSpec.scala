package pangea.service.state.states.battle

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, BattleEffects, Burn, GroupState, MonsterSlot, PowderKeg, SoloPveBattle, Ward}
import pangea.model.hero.Hero
import pangea.model.monster.{MiniBoss, Race, Rarity}
import pangea.model.squad.AllyKind
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.battle.BattleState.{KegTarget, Outcome, TurnResult}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.test._
import zio.{Task, ZIO}

/** Бочки с порохом на поле (PowderKeg, просека Чернолома): в конце раунда одна
  * может рвануть — бьёт мимо брони и поджигает наугад, своих и чужих. Строка
  * одна на взрыв: кого задело, сколько и что загорелось, и сколько бочек
  * осталось. Павших разбирают тем же порядком, что павших от ауры. */
object PowderKegSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private val content = ZIO.attempt(SceneContent.load())

  /** Человек, по которому моб не попадает: всё, что он потеряет, снимут бочки. */
  private def hero(hp: Long = 1000000L): Hero =
    TestFixtures.hero(userId).copy(lvl = 36L,
      fightStats = FightStats(atk = 1, hp = hp, armor = 300, defence = 0, evasion = 9999, accuracy = 9999, energy = 60))

  private def ally(hp: Long = 1000L): BattleAlly =
    BattleAlly(AllyKind.Human, 1, hp, 500L, 0L,
      FightStats(atk = 1, hp = 1000, armor = 500, defence = 0, evasion = 0, accuracy = 1, energy = 0), 36L)

  private def goblin(hp: Long = 2000L): MonsterSlot =
    MonsterSlot(36L, Race.Goblin.entryName, Rarity.Common.entryName,
      FightStats(atk = 1, hp = 2000, armor = 300, defence = 0, evasion = 0, accuracy = 1, energy = 0),
      hp, 300L, marked = false, 0L, BattleEffects.empty)

  /** Орк в паре с героем, гоблин — в строю, Йорген — рядом с героем, на поле
    * шесть бочек. Без сородичей: подкрепление не придёт. */
  private def field(activeHp: Long = 5000L, others: List[MonsterSlot] = List(goblin()),
                    allies: List[BattleAlly] = List(ally())): SoloPveBattle =
    SoloPveBattle(
      monsterLvl          = 36L,
      monsterRace         = Race.Orc.entryName,
      monsterRarity       = Rarity.Common.entryName,
      monsterStats        = FightStats(atk = 1, hp = 5000, armor = 100, defence = 0, evasion = 0, accuracy = 1, energy = 0),
      monsterCurrentHp    = activeHp,
      monsterCurrentArmor = 100L,
      noKin               = true,
      kegs                = 6,
      group               = GroupState.empty.copy(heroPos = 2, activePos = 2, allies = allies, others = others,
                              places = others.indices.map(_ + 3).toList))

  private def blast(h: Hero, b: SoloPveBattle, hit: List[KegTarget], catches: Boolean = true): Task[TurnResult] =
    for {
      c   <- content
      dao <- TestHeroDao.withHero(userId, h)
    } yield BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
              .kegHits(TurnResult(h, b, Vector.empty, Outcome.Continue), hit, catches, 0L)

  private val all: List[KegTarget] = List(KegTarget.Hero, KegTarget.Ally(1), KegTarget.Active, KegTarget.Slot(0))

  /** Ходы подряд на одном бою — до первого взрыва, но не больше `n`: дальше
    * пламя от бочек копилось бы и решило бы бой. `reset` правит бой перед
    * каждым ходом. */
  private def rounds(h: Hero, b: SoloPveBattle, action: String, n: Int,
                     reset: SoloPveBattle => SoloPveBattle = identity): Task[(Option[SoloPveBattle], Hero, String)] =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      sch   <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- content
      _     <- TestRandom.setSeed(11L)
      state  = BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c, sch)
      read   = dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      _     <- ZIO.iterate(n)(_ > 0) { left =>
                 read.flatMap {
                   case Some(cur) if cur.kegs == b.kegs =>
                     dao.writeActiveBattle(userId, reset(cur).asJson) *> state.action(testUser, tap(action), r).as(left - 1)
                   case _ => ZIO.succeed(0)
                 }
               }
      after <- read
      hAft  <- dao.getHeroByUserId(userId).map(_.get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (after, hAft, log)

  override def spec = suite("Бочки с порохом")(

    test("взрыв: каждому 10% максимума HP мимо брони и пламя; строка одна, бочкой меньше") {
      val h = hero()
      val b = field()
      for {
        res <- blast(h, b, all)
        heroDmg = PowderKeg.damageOn(h.effectiveMaxHp(0L))
        a       = res.battle.group.allyAt(1)
        g       = res.battle.group.others.headOption
      } yield assertTrue(res.outcome == Outcome.Continue && res.battle.kegs == 5) &&
              assertTrue(res.hero.fightStats.hp == h.fightStats.hp - heroDmg && res.hero.fightStats.armor == 300L &&
                res.battle.effects.heroBurn.contains(Burn.onIgnite)) &&
              assertTrue(a.exists(x => x.hp == 900L && x.armor == 500L && x.burn.contains(Burn.onIgnite))) &&
              assertTrue(res.battle.monsterCurrentHp == 4500L && res.battle.monsterCurrentArmor == 100L &&
                res.battle.effects.monsterBurn.contains(Burn.onIgnite)) &&
              assertTrue(g.exists(x => x.currentHp == 1800L && x.currentArmor == 300L && x.effects.monsterBurn.contains(Burn.onIgnite))) &&
              assertTrue(res.log == Vector(s"💥 Взорвалась бочка с порохом: вы −$heroDmg 🔥, Йорген Кремень −100 🔥, " +
                s"${b.monsterName} −500 🔥, ${goblin().name} −200 🔥. Бочек осталось: 5."))
    },

    test("пламя: горящий разгорается, замёрзший оттаивает, несгораемого и элементаля не берёт; без броска — герой не горит") {
      val frozen  = ally().copy(coldCut = 10)
      val burning = ally().copy(position = 3, burn = Some(Burn(6)))
      val fireEl  = goblin().copy(race = Race.Elemental.entryName, boss = Some(MiniBoss.FireElemental.entryName))
      val b0      = field(others = List(fireEl), allies = List(frozen, burning)).copy(monsterRace = Race.Dragon.entryName)
      val cold    = b0.copy(effects = b0.effects.copy(heroColdDefenceCut = 15))
      val warded  = b0.copy(effects = b0.effects.copy(heroWards = Some(Set(Ward.Fire))))
      val hit     = List(KegTarget.Hero, KegTarget.Ally(1), KegTarget.Ally(3), KegTarget.Active, KegTarget.Slot(0))
      for {
        res     <- blast(hero(), cold, hit)
        unlucky <- blast(hero(), b0, List(KegTarget.Hero), catches = false)
        safe    <- blast(hero(), warded, List(KegTarget.Hero))
      } yield assertTrue(res.battle.effects.heroColdDefenceCut == 0 && res.battle.effects.heroBurn.isEmpty) &&
              assertTrue(res.battle.group.allyAt(1).exists(x => x.coldCut == 0 && x.burn.isEmpty)) &&
              assertTrue(res.battle.group.allyAt(3).exists(_.burn.contains(Burn(6).reignited))) &&
              assertTrue(res.battle.effects.monsterBurn.isEmpty) &&
              // Огненный элементаль — минибосс: с него взрыв снимает вдвое меньше, и пламя не берёт
              assertTrue(res.battle.group.others.headOption.exists(x => x.currentHp == 1900L && x.effects.monsterBurn.isEmpty)) &&
              assertTrue(unlucky.battle.effects.heroBurn.isEmpty && !unlucky.log.mkString.contains("🔥")) &&
              assertTrue(safe.battle.effects.heroBurn.isEmpty && !safe.log.mkString.contains("🔥"))
    },

    test("павшие: союзник уходит своей строкой, гоблин из строя — в павшие, того, кто в полях, — победа над ним") {
      val b = field(activeHp = 100L, others = List(goblin(hp = 150L)), allies = List(ally(hp = 50L)))
      for {
        res <- blast(hero(), b, List(KegTarget.Ally(1), KegTarget.Slot(0), KegTarget.Active))
      } yield assertTrue(res.log.headOption.contains(s"💥 Взорвалась бочка с порохом: Йорген Кремень −50 ☠, " +
                s"${goblin().name} −150 ☠, ${b.monsterName} −100 ☠. Бочек осталось: 5.")) &&
              assertTrue(res.log.contains("📜 Йорген Кремень: свиток — ушёл из боя.")) &&
              assertTrue(res.battle.group.allies.isEmpty && res.battle.group.alliesGone.map(_.name) == List("Йорген Кремень")) &&
              assertTrue(res.battle.group.others.isEmpty && res.battle.group.slain.size == 1) &&
              assertTrue(res.outcome == Outcome.Victory)
    },

    test("последняя бочка — своей строкой; герой на последних HP падает — это смерть") {
      for {
        last <- blast(hero(), field().copy(kegs = 1), List(KegTarget.Active))
        fell <- blast(hero(hp = 10L), field(), List(KegTarget.Hero))
      } yield assertTrue(last.battle.kegs == 0 && last.log == Vector(s"💥 Взорвалась последняя бочка с порохом: ${field().monsterName} −500 🔥.")) &&
              assertTrue(fell.hero.fightStats.hp == 0L && fell.outcome == Outcome.Death &&
                fell.log.headOption.contains("💥 Взорвалась бочка с порохом: вы −10. Бочек осталось: 5."))
    },

    test("бой, сохранённый до бочек, читается без них; бочки на бою не теряются") {
      val saved = field().copy(kegs = 4).asJson
      val old   = saved.mapObject(_.remove("kegs"))
      assertTrue(saved.hcursor.downField("kegs").focus.contains(Json.fromInt(4)) && old.hcursor.downField("kegs").focus.isEmpty) &&
      assertTrue(old.as[SoloPveBattle].toOption.exists(_.kegs == 0) && saved.as[SoloPveBattle].toOption.exists(_.kegs == 4))
    },

    test("в бою: бочки рвутся в конце раунда героя, а под строем видно, сколько их осталось") {
      for {
        out <- rounds(hero(), field(), "Wait", 12)
        (after, _, log) = out
      } yield assertTrue(log.contains("💥 Взорвалась бочка с порохом:") && log.contains("Бочек осталось: 5.")) &&
              assertTrue(log.contains("💣 Бочек с порохом на поле: 5") && after.exists(_.kegs == 5))
    },

    test("бой без героя: бочки рвутся и тогда, но лежачего не задевают") {
      val down = field().copy(group = field().group.copy(heroDown = true))
      for {
        out <- rounds(hero(hp = 0L), down, "SquadTick", 12, b => b.copy(group = b.group.copy(downRounds = 0)))
        (after, h, log) = out
      } yield assertTrue(log.contains("💥 Взорвалась бочка с порохом:") && !log.contains("вы −")) &&
              assertTrue(h.fightStats.hp == 0L && after.exists(b => b.group.heroDown && b.kegs == 5))
    }
  )
}
