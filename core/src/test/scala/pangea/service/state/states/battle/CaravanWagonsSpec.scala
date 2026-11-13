package pangea.service.state.states.battle

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, Burn, Convoy, GroupState, SoloPveBattle, Wagon}
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.CaravanTrip
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Повозки обоза в засаде на караван: стоят «под» строем героя на местах 1–5.
  * Кто стоит на повозке, принимает удары за неё и сбивает с неё огонь;
  * непокрытую бьют и жгут — кроме ищеек Сноходцев. Пламя на повозке — с 10%
  * её HP за раунд. Побитые повозки после боя уходят туда, откуда начался бой,
  * огонь потушен. */
object CaravanWagonsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private val strikeFirst: UserAction = UserAction("", Some("""{"action":"Attack","target":"1"}"""))

  /** Герой на месте 1, крепкий и почти безобидный: мобы переживают раунд. */
  private val hero: Hero = TestFixtures.hero(userId).copy(
    lvl        = 10L,
    fightStats = FightStats(atk = 1, hp = 100000, armor = 100000, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    baseStats  = BaseStats(agi = 1, vit = 10, str = 1, int = 1))

  private def mob: Monster =
    Monster(0L, 10L, Race.Human, Rarity.Common,
      FightStats(atk = 100, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 10000, energy = 0))

  private def wagon(pos: Int, burn: Option[Burn] = None): Wagon = Wagon(pos, 1000L, 500L, 1000L, 500L, burn)
  private def wagons: List[Wagon] = (1 to 5).toList.map(wagon(_))

  /** Двое разбойников: один в паре с героем (место 1), второй — напротив повозки 2. */
  private def ambush(convoy: Convoy, allies: List[BattleAlly] = Nil): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(List(mob, mob), hero, Nil)
    b.copy(noKin = true, group = b.group.copy(convoy = Some(convoy), allies = allies))
  }

  private def ally(pos: Int, kind: AllyKind = AllyKind.Human): BattleAlly = {
    val stats = FightStats(atk = 1, hp = 100000, armor = 100000, defence = 0, evasion = 0, accuracy = 1, energy = 0)
    BattleAlly(kind, pos, hp = stats.hp, armor = stats.armor, energy = 0L, stats = stats, lvl = 10L)
  }

  /** Раунд: герой бьёт моба в паре; бой после раунда и всё, что герой увидел. */
  private def round(b: SoloPveBattle, sceneData: Option[Json] = None): Task[(Option[SoloPveBattle], String, Option[Json])] =
    for {
      dao   <- TestHeroDao.withHero(userId, hero)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      _     <- ZIO.foreachDiscard(sceneData)(dao.writeSceneData(userId, _))
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      _     <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, strikeFirst, r)
      saved <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      sd    <- dao.readSceneData(userId)
    } yield (saved, log, sd)

  private def wagonOf(b: Option[SoloPveBattle], pos: Int): Option[Wagon] =
    b.flatMap(_.group.convoy).flatMap(_.wagons.find(_.position == pos))

  override def spec = suite("Повозки обоза")(

    test("непокрытую повозку разбойник бьёт: сперва броня, потом HP; героя сбоку он уже не трогает") {
      for {
        res <- round(ambush(Convoy(wagons, hitsWagons = true, ignitePct = 0L)))
        (b, log, _) = res
        w2 = wagonOf(b, 2)
      } yield assertTrue(log.contains("бьёт повозку на месте 2")) &&
              assertTrue(w2.exists(w => w.armor < 500L || w.hp < 1000L)) &&
              // повозку под героем никто не тронул
              assertTrue(wagonOf(b, 1).contains(wagon(1)) && !log.contains("атаковал вас сбоку") && !log.contains("ударил сбоку"))
    },

    test("кто стоит на повозке, тот и принимает удар: союзник на месте 2 — повозка цела") {
      for {
        res <- round(ambush(Convoy(wagons, hitsWagons = true, ignitePct = 0L), List(ally(2))))
        (b, log, _) = res
      } yield assertTrue(!log.contains("бьёт повозку")) && assertTrue(wagonOf(b, 2).contains(wagon(2))) &&
              assertTrue(b.exists(_.group.allyAt(2).exists(a => a.hp < 100000L || a.armor < 100000L)))
    },

    test("ищейки Сноходцев повозок не трогают: моб напротив непокрытой повозки идёт на героя") {
      for {
        res <- round(ambush(Convoy(wagons, hitsWagons = false, ignitePct = 0L)))
        (b, log, _) = res
      } yield assertTrue(!log.contains("бьёт повозку")) && assertTrue(wagonOf(b, 2).contains(wagon(2)))
    },

    test("удар поджигает; непокрытая горит в конце раунда — 10% её HP, и разгорается; под героем огонь сбит") {
      val burning = wagons.map(w => if (w.position == 1) w.copy(burn = Some(Burn(Convoy.BurnStart))) else w)
      for {
        res <- round(ambush(Convoy(burning, hitsWagons = true, ignitePct = 100L)))
        (b, log, _) = res
        w2 = wagonOf(b, 2)
      } yield assertTrue(log.contains("повозка загорелась 🔥") && log.contains("Повозка на месте 2 горит: 100 урона")) &&
              assertTrue(w2.exists(_.burn.contains(Burn(Convoy.BurnStart).grown))) &&
              assertTrue(log.contains("Вы сбиваете огонь с повозки на месте 1") && wagonOf(b, 1).exists(_.burn.isEmpty))
    },

    test("сгоревшая и разбитая повозка выбывает: в бою её больше нет, строка об этом") {
      val weak = wagons.map(w => if (w.position == 2) w.copy(hp = 50L, armor = 0L, burn = Some(Burn(Convoy.BurnStart))) else w)
      for {
        res <- round(ambush(Convoy(weak, hitsWagons = false, ignitePct = 0L)))
        (b, log, _) = res
      } yield assertTrue(log.contains("Повозка на месте 2 сгорела")) &&
              assertTrue(wagonOf(b, 2).exists(w => !w.alive && w.burn.isEmpty)) &&
              assertTrue(b.flatMap(_.group.convoy).exists(c => c.intact == 4 && c.wagonAt(2).isEmpty))
    },

    test("моб у непокрытой повозки занят ею: к герою не идёт; нет повозки — идёт") {
      val far    = ambush(Convoy(wagons, hitsWagons = true, ignitePct = 0L))
      val placed = far.copy(group = far.group.copy(places = List(4)))
      val plain  = placed.copy(group = placed.group.copy(convoy = None))
      assertTrue(placed.closeIn._1.group.places == List(4)) &&
      assertTrue(plain.closeIn._1.group.places != List(4))
    },

    test("караванщик на повозке держит место; простой наёмник, которому некого бить, идёт к врагу") {
      val b      = SoloPveBattle.fromGroup(List(mob), hero, Nil)
      val group  = b.group.copy(convoy = Some(Convoy(wagons, hitsWagons = true, ignitePct = 0L)),
                     allies = List(ally(3, AllyKind.CaravanGuard), ally(6)))
      val moved  = b.copy(group = group).squadAdvance._1.group.allies
      assertTrue(moved.find(_.kind == AllyKind.CaravanGuard).exists(_.position == 3)) &&
      assertTrue(moved.find(_.kind == AllyKind.Human).exists(_.position != 6))
    },

    test("в строю видно повозки: на пустом месте — сама, под героем — рядом с ним") {
      for {
        dao <- TestHeroDao.withHero(userId, hero)
        _   <- dao.writeActiveBattle(userId, ambush(Convoy(wagons, hitsWagons = true, ignitePct = 0L)).asJson)
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        _   <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).enter(testUser, r)
        all <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(all.contains("1. 🟢 Вы · 🛒 100% VS") && all.contains("3. 🛒 Повозка ❤ 100% 🧥 100% VS ⚪"))
    },

    test("после победы повозки уходят туда, откуда начался бой: какими вышли из боя, огонь потушен") {
      val prey    = Monster(0L, 10L, Race.Human, Rarity.Common,
                      FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
      val strong  = hero.copy(fightStats = hero.fightStats.copy(atk = 1000000L, accuracy = 9999L))
      val beaten  = wagons.map(w => if (w.position == 3) w.copy(hp = 300L, armor = 0L, burn = Some(Burn(12))) else w)
      val battle  = SoloPveBattle.from(prey, strong).copy(noKin = true)
      val trip    = CaravanTrip(2, 1, 0L, Some(wagons))
      val routing = LootData(Nil, Nil, returnState = Some(StateType.VillageCaravan), eventData = Some(trip.asJson))
      for {
        dao  <- TestHeroDao.withHero(userId, strong)
        _    <- dao.writeActiveBattle(userId, battle.copy(group = battle.group.copy(convoy =
                  Some(Convoy(beaten, hitsWagons = true, ignitePct = 0L)))).asJson)
        _    <- dao.writeSceneData(userId, routing.asJson)
        r    <- TestRenderer.make
        c    <- ZIO.attempt(SceneContent.load())
        res  <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
        loot <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        back  = loot.flatMap(_.eventData).flatMap(_.as[CaravanTrip].toOption)
      } yield assertTrue(res == StateType.Loot && loot.exists(_.returnState.contains(StateType.VillageCaravan))) &&
              assertTrue(back.exists(t => t.leg == 2 && t.ambushes == 1 && t.wagons.contains(beaten.map(_.cooled))))
    },

    test("старые записи: строй без обоза и путь каравана без повозок читаются; повозки едут туда и обратно") {
      val group = GroupState(convoy = Some(Convoy(wagons, hitsWagons = false, ignitePct = 50L)))
      val old   = group.asJson.mapObject(_.remove("convoy"))
      val trip  = CaravanTrip(2, 1, 7L, Some(wagons))
      val bare  = Json.obj("caravanLeg" -> 2.asJson, "caravanAmbushes" -> 1.asJson, "caravanNextAt" -> 7L.asJson)
      assertTrue(group.asJson.as[GroupState].exists(_.convoy == group.convoy) && old.as[GroupState].exists(_.convoy.isEmpty)) &&
      assertTrue(trip.asJson.as[CaravanTrip].contains(trip) && bare.as[CaravanTrip].contains(CaravanTrip(2, 1, 7L, None))) &&
      assertTrue(Json.obj("position" -> 2.asJson, "hp" -> 40L.asJson).as[Wagon].contains(Wagon(2, 40L, 0L, 40L, 0L, None)))
    }
  )
}
