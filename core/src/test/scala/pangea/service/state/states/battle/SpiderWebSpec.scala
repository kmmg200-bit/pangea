package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, BattleEffects, SoloPveBattle, SpiderWeb, Ward}
import pangea.model.hero.Hero
import pangea.model.monster.{Breed, Monster, Race, Rarity}
import pangea.model.skill.MonsterSkill
import pangea.model.squad.AllyKind
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Паутина сетеплётов: взрослый и красный опутывают — героя и союзников.
  * Опутанный не сходит с места, удары по нему идут 30% в HP, 70% в броню.
  * Держит остаток раунда и весь следующий; шанс — по формуле пользователя. */
object SpiderWebSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** Герой в броне, без защиты и уклонения, с ловкостью в единицу: паутина
    * ложится почти наверняка. */
  private val hero: Hero = TestFixtures.hero(userId).copy(
    lvl        = 10L,
    fightStats = FightStats(atk = 1, hp = 10000, armor = 10000, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    baseStats  = BaseStats(agi = 1, vit = 10, str = 1, int = 1))

  private def spider(breed: Breed): Monster =
    Monster(0L, 50L, Race.Animal, Rarity.Rare,
      FightStats(atk = 1000, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 10000, energy = 0),
      customName = Some(breed.displayName), breed = Some(breed))

  private def foe(breed: Breed): SoloPveBattle = SoloPveBattle.from(spider(breed), hero).copy(noKin = true)

  /** Ход героя: бой после него, сколько снято с брони и с HP и что он увидел. */
  private def turn(b: SoloPveBattle, rolls: Task[Unit], h: Hero = hero): Task[(SoloPveBattle, Long, Long, String)] =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      _     <- rolls
      _     <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
      after <- dao.getHeroByUserId(userId).map(_.get)
      saved <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (saved, h.fightStats.armor - after.fightStats.armor, h.fightStats.hp - after.fightStats.hp, log)

  private def ally(pos: Int, webbed: Int = 0): BattleAlly = {
    val stats = FightStats(atk = 10, hp = 500, armor = 500, defence = 0, evasion = 0, accuracy = 10, energy = 0)
    BattleAlly(AllyKind.Human, pos, hp = stats.hp, armor = stats.armor, energy = 0L, stats = stats, lvl = 10L, webbed = webbed)
  }

  override def spec = suite("Паутина сетеплётов")(

    test("шанс: (уровень паука − ловкость + 100 / (уклонение − защита)) × 5, от 5% до 95%; делитель ноль — как единица") {
      assertTrue(SpiderWeb.chancePct(20L, 10L, 50L, 30L) == 75L) &&   // 20 − 10 + 100/20 = 15
      assertTrue(SpiderWeb.chancePct(20L, 10L, 30L, 50L) == 25L) &&   // 20 − 10 − 100/20 = 5
      assertTrue(SpiderWeb.chancePct(20L, 40L, 50L, 30L) == 5L) &&    // −15: не больше нуля — 5%
      assertTrue(SpiderWeb.chancePct(30L, 5L, 31L, 30L) == 95L) &&    // 125 × 5 — потолок
      assertTrue(SpiderWeb.chancePct(10L, 100L, 30L, 30L) == 50L)     // 10 − 100 + 100/1 = 10
    },

    test("плетут только взрослый и красный") {
      assertTrue(SpiderWeb.weaves(Some(Breed.AdultSpider)) && SpiderWeb.weaves(Some(Breed.RedSpider))) &&
      assertTrue(List(Breed.YoungSpider, Breed.SpiderMother, Breed.NewbornSpider).forall(b => !SpiderWeb.weaves(Some(b))) &&
        !SpiderWeb.weaves(None))
    },

    test("удар по опутанному: 70% в броню, 30% прямо в HP; брони не хватило — её доля тоже в HP") {
      val now    = 0L
      val webbed = foe(Breed.AdultSpider).copy(effects = BattleEffects(heroWebTurns = Some(2)))
      val (hp1, armor1, _) = MonsterSkill.physicalHit(webbed, hero, 1000L, now)
      val (hp2, armor2, _) = MonsterSkill.physicalHit(foe(Breed.AdultSpider), hero, 1000L, now)
      val thin             = hero.copy(fightStats = hero.fightStats.copy(armor = 200))
      val (hp3, armor3, _) = MonsterSkill.physicalHit(webbed, thin, 1000L, now)
      assertTrue(hero.fightStats.armor - armor1 == 700L && hero.fightStats.hp - hp1 == 300L) &&
      assertTrue(hero.fightStats.armor - armor2 == 1000L && hp2 == hero.fightStats.hp) &&
      assertTrue(armor3 == 0L && thin.fightStats.hp - hp3 == 800L)
    },

    test("в бою: удар взрослого опутывает героя — пометка в строке удара; паутина держит и следующий раунд") {
      for {
        // герой мажет, паук попадает, паутина ложится
        res <- turn(foe(Breed.AdultSpider), TestRandom.feedInts(1, 99, 1) *> TestRandom.feedLongs(100L, 100L))
        (saved, _, _, log) = res
      } yield assertTrue(log.contains("вы опутаны 🕸")) &&
              // легла на два конца раунда: один прошёл — держит ещё следующий
              assertTrue(saved.effects.heroWebTurns.contains(1) && saved.effects.heroWebbed && saved.effects.heroRooted)
    },

    test("в бою: по опутанному удар идёт и в HP, хотя броня цела; молодой паутину не плетёт") {
      for {
        held  <- turn(foe(Breed.AdultSpider).copy(effects = BattleEffects(heroWebTurns = Some(2))),
                   TestRandom.feedInts(1, 99, 99) *> TestRandom.feedLongs(100L, 100L))
        young <- turn(foe(Breed.YoungSpider), TestRandom.feedInts(1, 99, 1) *> TestRandom.feedLongs(100L, 100L))
      } yield assertTrue(held._3 > 0L && held._2 > 0L && math.abs(held._3 * 10L - (held._2 + held._3) * 3L) <= 10L) &&
              // свой раунд паутина отдержала — дальше её нет
              assertTrue(held._1.effects.heroWebTurns.contains(1)) &&
              assertTrue(!young._4.contains("опутаны") && young._1.effects.heroWebTurns.isEmpty && young._3 == 0L)
    },

    test("опутанный не сходит с места: ни шага, ни Тарана, ни Переката; на место опутанного союзника не встать") {
      val group  = foe(Breed.AdultSpider)
      val placed = group.copy(group = group.group.copy(heroPos = 2, allies = List(ally(1, webbed = 2), ally(3))))
      val held   = placed.copy(effects = placed.effects.copy(heroWebTurns = Some(1)))
      assertTrue(BattleState.stepTargets(placed) == List(3)) &&
      assertTrue(BattleState.stepTargets(held).isEmpty && BattleState.ramSpots(held).isEmpty &&
        BattleState.rollSpots(held, 3).isEmpty)
    },

    test("конец раунда снимает по раунду: и с героя, и с союзников; отряд опутанного не двигает") {
      val group   = foe(Breed.AdultSpider)
      val placed  = group.copy(effects = group.effects.copy(heroWebTurns = Some(2)),
                      group = group.group.copy(allies = List(ally(1, webbed = 2), ally(2))))
      val once    = BattleState.webTicked(placed)
      val twice   = BattleState.webTicked(once)
      assertTrue(once.effects.heroWebTurns.contains(1) && once.group.allies.map(_.webbed) == List(1, 0)) &&
      assertTrue(twice.effects.heroWebTurns.isEmpty && twice.group.allies.map(_.webbed) == List(0, 0))
    },

    test("паучья сыворотка: паутина взрослого не берёт — и в логе так и сказано") {
      val warded = foe(Breed.AdultSpider).copy(effects = BattleEffects().withWard(Ward.Spider))
      for {
        res <- turn(warded, TestRandom.feedInts(1, 99, 1) *> TestRandom.feedLongs(100L, 100L))
        (saved, _, _, log) = res
      } yield assertTrue(log.contains("Паучья сыворотка: паутина не взялась") && !log.contains("вы опутаны")) &&
              assertTrue(saved.effects.heroWebTurns.isEmpty && !saved.effects.heroRooted)
    },

    test("паучья сыворотка: яд сетеплёта не берёт, а чужой яд берёт, как обычно") {
      // Без брони: удар паука доходит до HP, и молодой травит.
      val bare     = hero.copy(fightStats = hero.fightStats.copy(armor = 0))
      val venomous = BattleEffects(monsterPoisonsOnHit = true)
      val young    = foe(Breed.YoungSpider).copy(effects = venomous.withWard(Ward.Spider))
      val rat      = SoloPveBattle.from(spider(Breed.YoungSpider).copy(customName = Some("Крыса"), breed = None), bare)
                       .copy(noKin = true, effects = venomous.withWard(Ward.Spider))
      for {
        safe  <- turn(young, TestRandom.feedInts(1, 99) *> TestRandom.feedLongs(100L, 100L), bare)
        bitten <- turn(rat, TestRandom.feedInts(1, 99) *> TestRandom.feedLongs(100L, 100L), bare)
        plain <- turn(foe(Breed.YoungSpider).copy(effects = venomous), TestRandom.feedInts(1, 99) *>
                   TestRandom.feedLongs(100L, 100L), bare)
      } yield assertTrue(safe._3 > 0L && safe._1.effects.heroPoison.isEmpty &&
                safe._4.contains("Паучья сыворотка: яд сетеплёта не взялся")) &&
              assertTrue(bitten._3 > 0L && bitten._1.effects.heroPoison.isDefined) &&
              assertTrue(plain._1.effects.heroPoison.isDefined)
    },

    test("сыворотка снимает, что оставили сетеплёты: паутину, яд и сорванное умение") {
      val fx = BattleEffects(heroWebTurns = Some(2), heroPoison = Some(pangea.model.battle.Poison(6)),
                 heroSkillJammed = Some(true)).withWard(Ward.Spider)
      assertTrue(fx.heroWebTurns.isEmpty && fx.heroPoison.isEmpty && !fx.skillJammed && fx.warded(Ward.Spider)) &&
      assertTrue(Breed.isSpider(Some(Breed.NewbornSpider)) && !Breed.isSpider(None))
    },

    test("старые записи без паутины читаются: у героя её нет, у союзника — ноль") {
      val fx   = BattleEffects(heroWebTurns = Some(2))
      val a    = ally(1, webbed = 2)
      val oldFx = fx.asJson.mapObject(_.remove("heroWebTurns"))
      val oldA  = a.asJson.mapObject(_.remove("webbed"))
      assertTrue(fx.asJson.as[BattleEffects].exists(_.heroWebTurns.contains(2)) &&
        oldFx.as[BattleEffects].exists(_.heroWebTurns.isEmpty)) &&
      assertTrue(a.asJson.as[BattleAlly].exists(_.webbed == 2) && oldA.as[BattleAlly].exists(_.webbed == 0))
    }
  )
}
