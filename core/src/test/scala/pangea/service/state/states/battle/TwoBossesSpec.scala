package pangea.service.state.states.battle

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleEffects, GroupState, MonsterSlot, SlainMonster, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.monster.{BossRoundCap, MiniBoss, Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{JorgenQuest, UserAction}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.merc.JorgenLairState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.{Task, ZIO}
import zio.test._

/** Два минибосса в одном бою (логово Йоргена — двое огненных): круг и счёт
  * предела за раунд у каждого свои и ездят вместе с ним — в полях они на бою,
  * в строю на его слоте; бой, сохранённый по-старому, при чтении отдаёт круг
  * минибоссу в строю; пали оба — опыт и добыча за каждого. */
object TwoBossesSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val fire  = MiniBoss.FireElemental
  private val stats = fire.stats(1L)

  /** Огненный в паре: свой круг на бою. */
  private def fieldBoss: SoloPveBattle = SoloPveBattle(
    monsterLvl = 1L, monsterRace = Race.Elemental.entryName, monsterRarity = Rarity.Legendary.entryName,
    monsterStats = stats, monsterCurrentHp = stats.hp, monsterCurrentArmor = stats.armor,
    bossKind = Some(fire.entryName), bossTurn = 2, bossCharges = 1, bossTaken = 100L, bossArmorTaken = 50L)

  /** Второй огненный — в строю, на месте 2, со своим (пока нулевым) кругом. */
  private def sideBoss: MonsterSlot = MonsterSlot(1L, Race.Elemental.entryName, Rarity.Legendary.entryName, stats,
    stats.hp, stats.armor, marked = false, 0L, BattleEffects.empty, boss = Some(fire.entryName))

  private def twoBosses: SoloPveBattle =
    fieldBoss.copy(group = GroupState.empty.copy(others = List(sideBoss), places = List(2)))

  /** Герой высокого уровня: опыт за боссов не перевалит его на новый уровень. */
  private def hero: Hero = TestFixtures.hero(userId, dungeonLevel = 30).copy(
    lvl = 40L, exp = 0L,
    fightStats = FightStats(atk = 1000, hp = 100000, armor = 0, defence = 0,
                            evasion = 9999, accuracy = 9999, energy = 0))

  /** Добить и дождаться конца боя: уклонение не бывает ниже 5%. */
  private def finish(state: BattleState, r: TestRenderer): Task[StateType] = {
    def attack(left: Int): Task[StateType] =
      state.action(testUser, tap("Attack"), r).flatMap { st =>
        if (st == StateType.Battle && left > 1) attack(left - 1) else ZIO.succeed(st)
      }
    attack(10)
  }

  override def spec = suite("Два минибосса в одном бою")(

    test("у каждого свой круг: встал в пару — его круг на бою, ушёл в строй — увёз свой") {
      val swapped = twoBosses.engage(2)
      val back    = swapped.engage(1)
      val left    = swapped.group.others.head
      assertTrue(swapped.bossKind.contains(fire.entryName) && swapped.bossTurn == 0 && swapped.bossCharges == 0 &&
                 swapped.bossTaken == 0L && swapped.bossArmorTaken == 0L) &&
      assertTrue(left.bossTurn == 2 && left.bossCharges == 1 && left.bossTaken == 100L && left.bossArmorTaken == 50L) &&
      assertTrue(back.bossTurn == 2 && back.bossCharges == 1 && back.bossTaken == 100L &&
                 back.group.others.head.bossTurn == 0 && back.group.others.head.bossTaken == 0L)
    },

    test("удар по минибоссу в строю: предел — его собственный, счёт того, кто в полях, не тронут") {
      val (after, hurt) = twoBosses.hurtSlot(0, 0L, stats.hp)
      val side          = after.group.others.head
      val cap           = BossRoundCap.of(stats.hp, BossRoundCap.HpPct)
      assertTrue(hurt.hp == cap && side.currentHp == stats.hp - cap && side.bossTaken == cap) &&
      assertTrue(after.bossTaken == 100L && after.monsterCurrentHp == stats.hp)
    },

    test("новый раунд — предел считается заново и у тех, кто в строю") {
      val worn  = twoBosses.copy(group = twoBosses.group.copy(others = List(sideBoss.copy(bossTaken = 70L, bossArmorTaken = 30L))))
      val fresh = worn.freshRound
      assertTrue(fresh.bossTaken == 0L && fresh.bossArmorTaken == 0L) &&
      assertTrue(fresh.group.others.head.bossTaken == 0L && fresh.group.others.head.bossArmorTaken == 0L) &&
      // круг способностей раунд не трогает
      assertTrue(fresh.bossTurn == 2 && fresh.bossCharges == 1)
    },

    test("бой, сохранённый по-старому: в полях не минибосс, круг на бою — при чтении он у минибосса в строю") {
      val necro = sideBoss.copy(race = Race.Undead.entryName, boss = Some(MiniBoss.Necromancer.entryName))
      val minion = SoloPveBattle(
        monsterLvl = 3L, monsterRace = Race.Undead.entryName, monsterRarity = Rarity.Common.entryName,
        monsterStats = FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
        monsterCurrentHp = 10L, monsterCurrentArmor = 0L, monsterUndead = true,
        bossTurn = 3, bossCharges = 2, bossTaken = 70L)
        .copy(group = GroupState.empty.copy(others = List(necro), places = List(2)))
      // Старая запись: у слота полей круга ещё нет.
      val circleFields = List("bossTurn", "bossCharges", "bossRevives", "bossFirstSkillSpent", "bossTaken", "bossArmorTaken")
      val old = minion.asJson.hcursor.downField("group").downField("others").downN(0)
        .withFocus(_.mapObject(o => circleFields.foldLeft(o)(_.remove(_)))).top.getOrElse(Json.Null)
      val read     = old.as[SoloPveBattle].toOption
      val readSlot = read.flatMap(_.group.others.headOption)
      // Минибосс в полях — его круг и так на бою, слоты не трогаем.
      val fresh    = twoBosses.asJson.as[SoloPveBattle].toOption
      assertTrue(readSlot.exists(s => s.bossTurn == 3 && s.bossCharges == 2 && s.bossTaken == 70L)) &&
      assertTrue(read.exists(b => b.bossTurn == 0 && b.bossCharges == 0 && b.bossTaken == 0L)) &&
      assertTrue(fresh.exists(b => b.bossTurn == 2 && b.bossTaken == 100L && b.group.others.head.bossTurn == 0))
    },

    test("логово Йоргена: пали оба огненных — опыт и добыча за каждого, а добыча ведёт обратно в логово") {
      // Первый огненный уже пал, в паре — второй с одной единицей HP.
      val last = SoloPveBattle(
        monsterLvl = 1L, monsterRace = Race.Elemental.entryName, monsterRarity = Rarity.Legendary.entryName,
        monsterStats = stats.copy(evasion = 0L), monsterCurrentHp = 1L, monsterCurrentArmor = 0L,
        bossKind = Some(fire.entryName), story = Some(JorgenQuest.LairStory), lootLevel = Some(JorgenQuest.Level.toLong))
        .copy(group = GroupState.empty.copy(slain = List(SlainMonster(1L, Race.Elemental.entryName,
          Rarity.Legendary.entryName, marked = false, name = fire.monsterName, boss = Some(fire.entryName)))))
      for {
        dao  <- TestHeroDao.withHero(userId, hero)
        _    <- dao.writeActiveBattle(userId, last.asJson)
        _    <- dao.writeSceneData(userId, LootData(Nil, Nil, returnState = Some(StateType.JorgenLair),
                  eventData = Some(JorgenLairState.Progress(won = true).asJson)).asJson)
        r    <- TestRenderer.make
        c    <- ZIO.attempt(SceneContent.load())
        out  <- finish(BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c), r)
        loot <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        h    <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(out == StateType.Loot) &&
              // опыт — за обоих, по своему уровню босса
              assertTrue(h.exp == 2L * fire.expReward(1L)) &&
              // добыча — своя у каждого: первый экран и ещё один в очереди
              assertTrue(loot.exists(l => (l.items.nonEmpty || l.silvers.nonEmpty) && l.queue.size == 1 &&
                l.queue.forall(m => m.items.nonEmpty || m.silvers.nonEmpty))) &&
              // не в осмотр логова элементаля, а обратно к Йоргену
              assertTrue(loot.exists(_.returnState.contains(StateType.JorgenLair)))
    }
  )
}
