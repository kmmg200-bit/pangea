package pangea.service.state.states.battle

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleEffects, GroupState, MonsterSlot, PatternPull, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.monster.{Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.UserId
import pangea.service.state.states.battle.BattleState.{Outcome, TurnResult}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository}
import zio.test._
import zio.{Task, ZIO}

/** «Тянет узор» (PatternPull, Рубеж II): раз в три раунда, в конце раунда,
  * Сборщик тянет Клеймо героя — только героя, стоящего на ногах. Атака героя
  * ниже на четверть два раунда, сам Сборщик восстанавливает десятую часть HP —
  * в полях он или в строю. Строка в логе — «ваше Клеймо будто используют
  * против вас». Старые бои читаются без этих полей. */
object PatternPullSpec extends ZIOSpecDefault {

  private val userId  = UserId(1L)
  private val content = ZIO.attempt(SceneContent.load())
  private val name    = "Сборщик"

  private def hero(hp: Long = 100000L): Hero =
    TestFixtures.hero(userId).copy(lvl = 50L,
      fightStats = FightStats(atk = 1000, hp = hp, armor = 300, defence = 0, evasion = 0, accuracy = 100, energy = 60))

  private def man(hp: Long = 4000L, named: Option[String] = None): MonsterSlot =
    MonsterSlot(50L, Race.Human.entryName, (if (named.isDefined) Rarity.Legendary else Rarity.Rare).entryName,
      FightStats(atk = 1, hp = 10000, armor = 300, defence = 0, evasion = 0, accuracy = 1, energy = 0),
      hp, 300L, marked = false, 0L, BattleEffects.empty, customName = named)

  /** Сборщик в паре с героем (или человек, а Сборщик — в строю), третий раунд. */
  private def field(round: Int = 3, collectorActive: Boolean = true, activeHp: Long = 5000L,
                    puller: Option[String] = Some(name), heroDown: Boolean = false): SoloPveBattle =
    SoloPveBattle(
      monsterLvl          = 50L,
      monsterRace         = Race.Human.entryName,
      monsterRarity       = (if (collectorActive) Rarity.Legendary else Rarity.Rare).entryName,
      monsterStats        = FightStats(atk = 1, hp = 10000, armor = 300, defence = 0, evasion = 0, accuracy = 1, energy = 0),
      monsterCurrentHp    = activeHp,
      monsterCurrentArmor = 300L,
      noKin               = true,
      customName          = Option.when(collectorActive)(name),
      roundsFought        = round,
      puller              = puller,
      group               = GroupState.empty.copy(heroPos = 2, activePos = 2, heroDown = heroDown,
                              others = List(if (collectorActive) man() else man(hp = 6000L, named = Some(name))),
                              places = List(3)))

  private def pull(h: Hero, b: SoloPveBattle, outcome: Outcome = Outcome.Continue): Task[TurnResult] =
    for {
      c   <- content
      dao <- TestHeroDao.withHero(userId, h)
    } yield BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
              .patternPull(TurnResult(h, b, Vector.empty, outcome))

  override def spec = suite("«Тянет узор» Сборщика")(

    test("третий раунд, Сборщик в полях: атака героя ниже два раунда, Сборщик +10% HP, строка о Клейме") {
      val h = hero()
      for {
        res <- pull(h, field())
      } yield assertTrue(res.battle.effects.heroDrainTurns.contains(PatternPull.Turns) && res.battle.effects.heroDrained) &&
              assertTrue(res.battle.monsterCurrentHp == 6000L) &&
              assertTrue(res.log == Vector(s"🩸 $name тянет узор: ваше Клеймо будто используют против вас — атака −25% на 2 раунда, ему +1000 HP.")) &&
              assertTrue(res.hero == h)
    },

    test("Сборщик в строю: лечится там же, узор тянет всё равно; у полного HP — не выше максимума") {
      for {
        side <- pull(hero(), field(collectorActive = false))
        full <- pull(hero(), field(activeHp = 9500L))
      } yield assertTrue(side.battle.group.others.headOption.map(_.currentHp).contains(7000L) &&
                side.battle.monsterCurrentHp == 5000L && side.battle.effects.heroDrained) &&
              assertTrue(full.battle.monsterCurrentHp == 10000L && full.log.exists(_.contains("ему +500 HP")))
    },

    test("не тот раунд, нет Сборщика, лежит герой, бой решён, Сборщик пал — узор никто не тянет") {
      for {
        early  <- pull(hero(), field(round = 2))
        nobody <- pull(hero(), field(puller = None))
        down   <- pull(hero(), field(heroDown = true))
        won    <- pull(hero(), field(), Outcome.Victory)
        dead   <- pull(hero(), field(activeHp = 0L))
      } yield assertTrue(List(early, nobody, down, won, dead).forall(r => !r.battle.effects.heroDrained && r.log.isEmpty))
    },

    test("узор возвращается за два раунда, держится при смене пары; атака режется на четверть") {
      val drained = field().copy(effects = BattleEffects.empty.copy(heroDrainTurns = Some(PatternPull.Turns)))
      val once    = drained.tickBuffs()
      val twice   = once.tickBuffs()
      assertTrue(once.effects.heroDrainTurns.contains(1) && once.effects.heroDrained) &&
      assertTrue(twice.effects.heroDrainTurns.isEmpty && !twice.effects.heroDrained) &&
      assertTrue(drained.effects.heroPart.heroDrainTurns.contains(PatternPull.Turns)) &&
      assertTrue(PatternPull.drainedAtk(1000L) == 750L && PatternPull.due(3) && PatternPull.due(6) &&
        !PatternPull.due(0) && !PatternPull.due(4))
    },

    test("бой, записанный до Рубежа II, читается: узор никто не тянет, на герое ничего") {
      val stored = field().copy(effects = BattleEffects.empty.copy(heroDrainTurns = Some(2))).asJson
      def drop(j: Json, key: String): Json = j.mapObject(_.remove(key))
      val old    = drop(stored, "puller").hcursor.downField("effects").withFocus(drop(_, "heroDrainTurns")).top
      val read   = old.flatMap(_.as[SoloPveBattle].toOption)
      assertTrue(read.exists(b => b.puller.isEmpty && b.effects.heroDrainTurns.isEmpty))
    }
  )
}
