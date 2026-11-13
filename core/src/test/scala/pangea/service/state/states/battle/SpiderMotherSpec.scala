package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleAlly, BattleEffects, RedSpider, SkillSlotState, SlainMonster, SoloPveBattle, Ward}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Breed, Monster, Race, Rarity}
import pangea.model.skill.{MonsterSkill, Skill}
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.VillageRates
import pangea.service.state.{UserAction, Villages}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Матка и красный сетеплёт. Матка каждые три раунда рождает по два
  * новорождённых, пока в строю есть место; за них ни опыта, ни добычи.
  * Красный: десятая часть его удара всегда идёт мимо брони, удар травит с
  * шансом 20%, а отравив — с шансом 50% срывает следующее умение (энергия
  * уходит, умения нет). И по героям, и по союзникам. */
object SpiderMotherSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  /** Удар по месту 1: в гнезде у героя два соседа, и «Атака» без цели спрашивает, в кого. */
  private val strikeFirst: UserAction = UserAction("", Some("""{"action":"Attack","target":"1"}"""))

  /** Герой в броне, без защиты и уклонения: удары по нему ложатся предсказуемо. */
  private val hero: Hero = TestFixtures.hero(userId).copy(
    lvl        = 10L,
    fightStats = FightStats(atk = 1, hp = 10000, armor = 10000, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    baseStats  = BaseStats(agi = 1, vit = 10, str = 1, int = 1))

  private def spider(breed: Breed): Monster =
    Monster(0L, 50L, Race.Animal, Rarity.Rare,
      FightStats(atk = 1000, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 10000, energy = 0),
      customName = Some(breed.displayName), breed = Some(breed))

  private def foe(breed: Breed): SoloPveBattle = SoloPveBattle.from(spider(breed), hero).copy(noKin = true)

  private def battleOf(dao: TestHeroDao): Task[SoloPveBattle] =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  /** Ход героя в бою `b`: герой после хода, хранилище (там бой, пока он идёт) и
    * всё, что герой увидел. */
  private def act(h: Hero, b: SoloPveBattle, action: UserAction, rolls: Task[Unit]): Task[(Hero, TestHeroDao, String)] =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- dao.writeActiveBattle(userId, b.asJson)
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
      _     <- rolls
      _     <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, action, r)
      after <- dao.getHeroByUserId(userId).map(_.get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (after, dao, log)

  /** Ход, после которого бой идёт дальше: герой, бой и что герой увидел. */
  private def play(h: Hero, b: SoloPveBattle, action: UserAction, rolls: Task[Unit]): Task[(Hero, SoloPveBattle, String)] =
    for {
      res   <- act(h, b, action, rolls)
      saved <- battleOf(res._2)
    } yield (res._1, saved, res._3)

  private def play(h: Hero, b: SoloPveBattle, key: String, rolls: Task[Unit]): Task[(Hero, SoloPveBattle, String)] =
    play(h, b, tap(key), rolls)

  // герой мажет, паук попадает; дальше — броски яда, паутины и срыва
  private def redTurn(b: SoloPveBattle, rolls: Int*): Task[(Hero, SoloPveBattle, String)] =
    play(hero, b, "Attack", TestRandom.feedInts((1 +: 99 +: rolls): _*) *> TestRandom.feedLongs(100L, 100L))

  /** Герой с «Размашистым ударом» на мече (101) и энергией ровно на него. */
  private val skilled: Hero = {
    val base   = TestFixtures.hero(userId)
    val weapon = Item(101L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
                   attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
                   details = ItemDetails.Weapon(Skill.SweepingStrike))
    val armed  = base.copy(
      baseStats  = base.baseStats.copy(str = 50, int = 50, vit = 50, agi = 50),
      fightStats = base.fightStats.copy(atk = 50, accuracy = 50, defence = 20, hp = 5000, evasion = 0),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon))
    armed.copy(fightStats = armed.fightStats.copy(energy = Skill.SweepingStrike.energyCost(armed)))
  }

  /** Слабый моб: его ответ герою ничего не решает. */
  private def weak(jammed: Boolean): SoloPveBattle = SoloPveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 999, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 1),
    monsterCurrentHp    = 999L,
    monsterCurrentArmor = 0L,
    skillSlots          = List(SkillSlotState(101L, Skill.SweepingStrike)),
    effects             = BattleEffects(heroSkillJammed = Option.when(jammed)(true)),
    noKin               = true)

  /** Гнездо в строю: молодой в паре с героем, матка (или кто дан) — следом. */
  private def nest(lvl: Int, round: Int, others: List[Breed]): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(Villages.spider(Breed.YoungSpider, lvl) :: others.map(Villages.spider(_, lvl)), hero, Nil)
    b.copy(noKin = true, group = b.group.copy(round = round))
  }

  private def newborns(b: SoloPveBattle): List[pangea.model.battle.MonsterSlot] =
    b.group.others.filter(_.breed.contains(Breed.NewbornSpider))

  override def spec = suite("Матка и красный сетеплёт")(

    test("удар красного: десятая часть всегда мимо брони, даже целой; по опутанному — 30%, как от паутины") {
      val now              = 0L
      val red              = foe(Breed.RedSpider)
      val (hp1, armor1, _) = MonsterSkill.physicalHit(red, hero, 1000L, now)
      val (hp2, armor2, _) = MonsterSkill.physicalHit(red.copy(effects = BattleEffects(heroWebTurns = Some(2))), hero, 1000L, now)
      val (hp3, armor3, _) = MonsterSkill.physicalHit(foe(Breed.AdultSpider), hero, 1000L, now)
      assertTrue(hero.fightStats.armor - armor1 == 900L && hero.fightStats.hp - hp1 == 100L) &&
      assertTrue(hero.fightStats.armor - armor2 == 700L && hero.fightStats.hp - hp2 == 300L) &&
      assertTrue(hero.fightStats.armor - armor3 == 1000L && hp3 == hero.fightStats.hp) &&
      assertTrue(RedSpider.armorShare(webbed = false, Some(Breed.RedSpider)) == 0.9 &&
        RedSpider.armorShare(webbed = true, Some(Breed.AdultSpider)) == 0.7 &&
        RedSpider.armorShare(webbed = false, None) == 1.0)
    },

    test("в бою: удар красного травит с шансом, отравив — с шансом срывает следующее умение; не отравил — срыва нет") {
      for {
        jammed <- redTurn(foe(Breed.RedSpider), 1, 99, 1)    // яд — да, паутина — нет, срыв — да
        kept   <- redTurn(foe(Breed.RedSpider), 1, 99, 99)   // яд — да, срыв — нет
        clean  <- redTurn(foe(Breed.RedSpider), 99, 99)      // яда нет — и броска на срыв нет
      } yield assertTrue(jammed._2.effects.heroPoison.isDefined && jammed._2.effects.skillJammed) &&
              assertTrue(jammed._3.contains("вы отравлены")) &&
              // десятая часть удара — в HP при целой броне
              assertTrue(jammed._1.fightStats.hp < hero.fightStats.hp && clean._1.fightStats.hp < hero.fightStats.hp) &&
              assertTrue(kept._2.effects.heroPoison.isDefined && !kept._2.effects.skillJammed) &&
              assertTrue(clean._2.effects.heroPoison.isEmpty && !clean._2.effects.skillJammed)
    },

    test("защитный отвар от яда: яд не взял — и умение не сорвётся") {
      val warded = foe(Breed.RedSpider).copy(effects = BattleEffects(heroWards = Some(Set(Ward.Poison))))
      for {
        res <- redTurn(warded, 1, 99, 1)
      } yield assertTrue(res._2.effects.heroPoison.isEmpty && !res._2.effects.skillJammed)
    },

    test("сорванное умение: энергия ушла, урона нет, перезарядки нет, ход потрачен — и срыв один") {
      for {
        jammed <- play(skilled, weak(jammed = true), "Skill_101", ZIO.unit)
        normal <- play(skilled, weak(jammed = false), "Skill_101", ZIO.unit)
        attack <- play(skilled, weak(jammed = false), "Attack", ZIO.unit)
        (h, b, log) = jammed
      } yield assertTrue(log.contains("Яд паука, растекающийся по венам, не даёт вам использовать умение.")) &&
              assertTrue(b.monsterCurrentHp == 999L && normal._2.monsterCurrentHp < 999L) &&
              assertTrue(b.slotByItem(101L).exists(s => s.cooldown == 0 && s.uses == 0)) &&
              assertTrue(!b.effects.skillJammed && b.group.round == 1) &&
              // энергия списана, как за настоящее умение; за простой удар — нет
              assertTrue(h.fightStats.energy == normal._1.fightStats.energy && h.fightStats.energy < attack._1.fightStats.energy)
    },

    test("сорванный Блок: тратит энергию и бонусное действие, а ход остаётся — моб не бил") {
      val shield = Item(301L, "Щит", 1L, ItemRarity.Gray, ItemType.Shield,
                     attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
                     details = ItemDetails.HandSkill(Skill.Block))
      val h      = hero.copy(fightStats = hero.fightStats.copy(energy = 10_000L),
                     baseStats = hero.baseStats.copy(int = 50),
                     equipment = TestFixtures.emptyEquipment.copy(leftHand = shield))
      val b      = SoloPveBattle.from(spider(Breed.RedSpider), h).copy(noKin = true)
      for {
        res <- play(h, b.copy(effects = b.effects.copy(heroSkillJammed = Some(true))), "Skill_301", ZIO.unit)
        (after, mid, log) = res
      } yield assertTrue(log.contains("не даёт вам использовать умение")) &&
              assertTrue(mid.consumableUsedThisRound && mid.midRound && !mid.heroShieldUp && !mid.effects.skillJammed) &&
              assertTrue(after.fightStats.hp == h.fightStats.hp && mid.group.round == 0) &&
              assertTrue(after.fightStats.energy == h.fightStats.energy - Skill.Block.energyCost(h)) &&
              assertTrue(mid.skillSlots.find(_.itemId == 301L).exists(_.cooldown == 0))
    },

    test("союзника яд тоже срывает: энергия ушла, умения нет, и это видно в логе") {
      val ally = {
        val s = AllyKind.Human.stats(10L)
        Ally(AllyKind.Human, 2, s.hp, s.armor, 1000L)
      }
      val h    = TestFixtures.hero(userId).copy(
        lvl        = 10L,
        fightStats = FightStats(atk = 20, hp = 500000, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0),
        baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
        squad      = Squad(heroPos = 1, allies = List(ally)))
      val orc  = Monster(0L, 10L, Race.Orc, Rarity.Common,
                   FightStats(atk = 20, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))
      val b0   = SoloPveBattle.from(orc, h)
      val b    = b0.copy(group = b0.group.copy(allies = b0.group.allies.map(_.copy(skillJammed = true))))
      for {
        // герой; моб; удар союзника + прок; выбор умения (дробящий) — сорван; подкрепление
        res <- play(h, b, "Attack", TestRandom.feedInts(60, 99, 60, 99, 1, 99) *> TestRandom.feedLongs(100L, 100L, 100L))
        (_, after, log) = res
        a = after.group.allies.head
      } yield assertTrue(log.contains("Йорген Кремень: яд паука, растекающийся по венам, не даёт использовать умение.")) &&
              assertTrue(log.linesIterator.count(_.startsWith("⚔ Йорген")) == 1) &&
              assertTrue(!a.skillJammed && a.energy == 500L - 80L + 35L)
    },

    test("матка рожает в конце каждого третьего раунда: по двое, на 60% силы молодого, уровнем в неё") {
      val lvl = 10
      for {
        inLine <- play(hero, nest(lvl, round = 2, List(Breed.SpiderMother)), strikeFirst, ZIO.unit)
        paired <- play(hero, {
                    val b = SoloPveBattle.fromGroup(List(Villages.spider(Breed.SpiderMother, lvl)), hero, Nil)
                    b.copy(noKin = true, group = b.group.copy(round = 2))
                  }, "Attack", ZIO.unit)
        (_, b, log) = inLine
        young = Villages.spider(Breed.YoungSpider, lvl).fightStats
        baby  = Villages.newborn(lvl)
      } yield assertTrue(newborns(b).size == 2 && log.contains("новорождённые сетеплёты: 2")) &&
              assertTrue(newborns(b).forall(s => s.stats == baby.fightStats && s.customName.contains("Новорождённый сетеплёт"))) &&
              assertTrue(baby.fightStats.atk == young.atk * VillageRates.NewbornPct / 100L &&
                baby.fightStats.hp == young.hp * VillageRates.NewbornPct / 100L && baby.lvl == lvl.toLong) &&
              assertTrue(newborns(paired._2).size == 2)
    },

    test("матка не рожает: не третий раунд, матки нет; мест мало — сколько влезет") {
      val lvl = 10
      for {
        early  <- play(hero, nest(lvl, round = 0, List(Breed.SpiderMother)), strikeFirst, ZIO.unit)
        none   <- play(hero, nest(lvl, round = 2, List(Breed.YoungSpider, Breed.AdultSpider)), strikeFirst, ZIO.unit)
        packed <- play(hero, nest(lvl, round = 2, Breed.SpiderMother :: List.fill(12)(Breed.YoungSpider)), strikeFirst, ZIO.unit)
      } yield assertTrue(newborns(early._2).isEmpty && !early._3.contains("новорождённые")) &&
              assertTrue(newborns(none._2).isEmpty) &&
              // четырнадцать в строю из пятнадцати мест — родился один
              assertTrue(newborns(packed._2).size == 1 && packed._3.contains("новорождённые сетеплёты: 1"))
    },

    test("за новорождённых ни опыта, ни добычи: в счёт павших они не идут") {
      val strong = hero.copy(fightStats = hero.fightStats.copy(atk = 1_000_000L, accuracy = 9999L))
      val prey   = Monster(0L, 10L, Race.Animal, Rarity.Common,
                     FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
                     customName = Some(Breed.YoungSpider.displayName), breed = Some(Breed.YoungSpider))
      val baby   = SlainMonster(10L, Race.Animal.entryName, Rarity.Common.entryName, marked = false,
                     name = Breed.NewbornSpider.displayName, breed = Some(Breed.NewbornSpider))
      def fight(slain: List[SlainMonster]) = {
        val b = SoloPveBattle.from(prey, strong)
        act(strong, b.copy(group = b.group.copy(slain = slain)), tap("Attack"),
          TestRandom.feedInts(60, 90, 90) *> TestRandom.feedLongs(100L, 100L, 100L)).map(_._1)
      }
      for {
        plain <- fight(Nil)
        brood <- fight(List(baby, baby))
      } yield assertTrue(plain.exp > 0L && brood.exp == plain.exp) &&
              assertTrue(brood.kills == plain.kills && plain.kills == strong.kills + 1L)
    },

    test("старые записи без срыва читаются: у героя его нет, у союзника — нет") {
      val fx    = BattleEffects(heroSkillJammed = Some(true))
      val stats = FightStats(atk = 10, hp = 500, armor = 500, defence = 0, evasion = 0, accuracy = 10, energy = 0)
      val a     = BattleAlly(AllyKind.Human, 1, hp = 500L, armor = 500L, energy = 0L, stats = stats, lvl = 10L, skillJammed = true)
      val oldFx = fx.asJson.mapObject(_.remove("heroSkillJammed"))
      val oldA  = a.asJson.mapObject(_.remove("skillJammed"))
      assertTrue(fx.asJson.as[BattleEffects].exists(_.skillJammed) && oldFx.as[BattleEffects].exists(!_.skillJammed)) &&
      assertTrue(a.asJson.as[BattleAlly].exists(_.skillJammed) && oldA.as[BattleAlly].exists(!_.skillJammed))
    }
  )
}
