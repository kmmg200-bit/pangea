package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{SkillSlotState, SoloPveBattle}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.stats.Stat
import pangea.model.item.{BeltSlot, FlaskKind, Grip, Item, ItemDetails, ItemSet, ItemType, PassiveKind, ShieldRates,
  Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.rune.{Rune, RuneData}
import pangea.model.skill.{MonsterEnergy, Skill}
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.InventoryState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Прогон баланса фарма: как быстро и как успешно герой бьёт мобов четвёртого
  * тира (мифических) на [[LvlGap]] уровней выше себя.
  *
  * Три героя [[HeroLvl]]-го уровня, люди, по одному на основной набор:
  * «Каменный страж» (меч и щит), «Дикое пламя» (два меча), «Упырь» (два
  * кинжала). На каждом — полный набор из двенадцати фиолетовых вещей своего
  * уровня, четыре активных умения (три с вещей и одно клеймом на теле),
  * боевые пассивки на всех слотах, что их носят, и фиолетовый пояс с флягами
  * целителя, кузнеца и бодрости. Вещи — «средние»: те же формулы, что в
  * [[ItemGenerator]], без разброса, а прибавки выбраны под сборку, как
  * выбирал бы игрок. Камней в гнёздах, отряда, достижений и рога мастера нет:
  * это голая сила вещей, набора, умений и пассивок.
  *
  * Бой — как в лабиринте на уровне моба: моб случайной смертной расы;
  * мифический в конце первого раунда зовёт сородичей, каждый раунд может прийти
  * подкрепление. Герой жмёт кнопки как разумный игрок (см. [[decide]]): фляга,
  * когда худо, Блок бонусным действием, лечение и броня по нужде, остальное —
  * ударные умения по готовности и «Атака».
  *
  * Главный прогон — мифические на [[LvlGap]] уровней выше. Рядом, для
  * сравнения (см. [[Scenarios]]): мифические вровень и на пять выше, те же +10
  * с пониманием рун на потолке уровня и 50-й этаж, где тир моба выпадает как в
  * лабиринте. Каждый бой — с полного героя: отдых в лабиринте возвращает всё
  * за полминуты, а фляги — нет, поэтому глотки считаются.
  *
  * Итог — таблица в вывод теста: доля побед, смерти, затянувшиеся бои, раунды,
  * сколько HP и брони осталось, сколько глотков ушло. Проверки здесь только на
  * то, что прогон честный; пороги баланса — когда их назовёт пользователь. */
object FarmBalanceSpec extends ZIOSpecDefault {

  /** Уровень героев. */
  val HeroLvl: Long = 20L
  /** На сколько уровней моб выше героя. */
  val LvlGap: Int = 10
  /** Тир моба: четвёртый — мифический. */
  val MobRarity: Rarity = Rarity.Mythical
  /** Боёв на каждую расу моба: всего по `8 × FightsPerRace` на героя. */
  val FightsPerRace: Int = 5
  /** Раундов, после которых бой считается затянувшимся: мобы чинятся и лечат
    * друг друга быстрее, чем герой их бьёт, — ничья. */
  val RoundLimit: Int = 150

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def act(id: String, data: Map[String, String] = Map.empty): UserAction =
    UserAction("", Some((Map("action" -> id) ++ data).asJson.noSpaces))

  private lazy val content: SceneContent = SceneContent.load()

  // ── Средняя фиолетовая вещь ─────────────────────────────────────────────────

  private val Purple: ItemRarity = ItemRarity.Purple

  /** Одна вещь сборки: слот, профиль (тяжёлый — броня с защитой, лёгкий —
    * точность с уклонением), прибавки и спец-данные (умение, пассивка, пояс). */
  private final case class Piece(t: ItemType, heavy: Boolean, extras: List[Stat],
                                 details: ItemDetails = ItemDetails.Plain, grip: Option[Grip] = None)

  /** Вещь уровня героя — по формулам [[ItemGenerator]] (главный стат профиля,
    * прибавки, обязательная прибавка слота, срез хвата), без разброса ±10–20%. */
  private def make(id: Long, set: ItemSet, p: Piece): Item = {
    val l  = HeroLvl
    val r3 = Purple.factorR3
    val main =
      if (p.heavy) Item(id, s"${Purple.emoji} ${p.t}", l, Purple, p.t, attack = 0, accuracy = 0, energy = 0,
                        armor = (Purple.factorR * l).toLong, defence = (Purple.factorR1 * l).toLong, evasion = 0)
      else Item(id, s"${Purple.emoji} ${p.t}", l, Purple, p.t, attack = 0,
                accuracy = (Purple.factorR * 0.5 * l).toLong, energy = 0, armor = 0, defence = 0,
                evasion = (Purple.factorR1 * l).toLong)
    val extra = p.extras.foldLeft(main) { (it, s) =>
      s match {
        case Stat.Attack   => it.copy(attack = it.attack + (r3 * 0.4 * l).toLong)
        case Stat.Accuracy => it.copy(accuracy = it.accuracy + (r3 * 0.5 * l).toLong)
        case Stat.Energy   => it.copy(energy = it.energy + (r3 * 0.3 * l).toLong)
        case Stat.Armor    => it.copy(armor = it.armor + (r3 * 1.5 * l).toLong)
        case Stat.Defence  => it.copy(defence = it.defence + (r3 * 0.5 * l).toLong)
        case Stat.Evasion  => it.copy(evasion = it.evasion + (r3 * l).toLong)
      }
    }
    val mandatory = p.t match {
      case ItemType.Weapon     => extra.copy(attack = extra.attack + (l * (6.0 + r3)).toLong)
      case ItemType.ChestPlate => extra.copy(hp = extra.hp + (l * (ItemGenerator.ChestHpBase + r3)).toLong)
      case ItemType.Shield     => extra.copy(defence = extra.defence + ShieldRates.DefencePerLvl * l)
      case _                   => extra
    }
    val gripped = p.grip.fold(mandatory) { g =>
      def cut(v: Long): Long = v * g.statPct / 100L
      mandatory.copy(grip = Some(g), attack = cut(mandatory.attack), accuracy = cut(mandatory.accuracy),
        energy = cut(mandatory.energy), armor = cut(mandatory.armor), defence = cut(mandatory.defence),
        evasion = cut(mandatory.evasion), hp = cut(mandatory.hp))
    }
    gripped.copy(details = p.details, set = Some(set))
  }

  /** Фиолетовый пояс: фляги целителя, кузнеца и бодрости, полные. */
  private val Flasks: List[BeltSlot] =
    List(FlaskKind.Healer, FlaskKind.Smith, FlaskKind.Vigor).map(k => BeltSlot.Flask(k, Purple, FlaskKind.chargesFor(Purple)))

  private def passive(k: PassiveKind): ItemDetails = ItemDetails.Passive(k)

  // ── Сборки ──────────────────────────────────────────────────────────────────

  /** Герой сборки: имя, набор, характеристики (старт 4/4/4/4 и 76 очков за
    * 19 уровней), вещи по слотам, умение-клеймо и порядок ударных умений. */
  private final case class Build(
    name: String,
    set: ItemSet,
    stats: BaseStats,
    helmet: Piece, shoulders: Piece, chest: Piece, bracelets: Piece, gloves: Piece, pants: Piece, boots: Piece,
    amulet: Piece, ring1: Piece, ring2: Piece, belt: Piece, weapon: Piece, leftHand: Piece,
    brand: Skill,
    strikes: List[Skill]
  ) {
    def equipment: Equipment = Equipment(
      helmet = make(1L, set, helmet), shoulderPads = make(2L, set, shoulders), chestPlate = make(3L, set, chest),
      bracelets = make(4L, set, bracelets), gloves = make(5L, set, gloves), pants = make(6L, set, pants),
      boots = make(7L, set, boots), amulet = make(8L, set, amulet), firstRing = make(9L, set, ring1),
      secondRing = make(10L, set, ring2), belt = make(11L, set, belt), weapon = make(12L, set, weapon),
      additionalWeapon = Item.NoItem, leftHand = make(13L, set, leftHand))
  }

  import Stat._

  private val Tank  = List(Armor, Armor, Defence)
  private val Aim   = List(Accuracy, Accuracy, Evasion)
  private val Light = List(Evasion, Evasion, Accuracy)
  private val Blade = List(Attack, Attack, Accuracy)

  /** «Каменный страж»: меч и щит, тело и ум. Доспех и щит тяжёлые, прибавки —
    * в броню и защиту; руки и украшения — на точность, иначе он не попадает.
    * Хитрый удар с меча, Блок со щита, Укрепление с нагрудника и Малое
    * исцеление клеймом. */
  private val StoneGuard = Build(
    name = "Страж", set = ItemSet.StoneGuard,
    stats = BaseStats(agi = 6, vit = 30, str = 14, int = 30),
    helmet = Piece(ItemType.Helmet, heavy = true, Tank, passive(PassiveKind.Stealthy)),
    shoulders = Piece(ItemType.ShoulderPads, heavy = true, Tank, passive(PassiveKind.Spiky)),
    chest = Piece(ItemType.ChestPlate, heavy = true, Tank, ItemDetails.Armor(Skill.Reinforcement)),
    bracelets = Piece(ItemType.Bracelets, heavy = false, Aim, passive(PassiveKind.Toughness)),
    gloves = Piece(ItemType.Gloves, heavy = false, Aim, passive(PassiveKind.Blending)),
    pants = Piece(ItemType.Pants, heavy = true, Tank, passive(PassiveKind.Reinforced)),
    boots = Piece(ItemType.Boots, heavy = true, Tank, passive(PassiveKind.Impenetrable)),
    amulet = Piece(ItemType.Amulet, heavy = false, Aim, passive(PassiveKind.Healing)),
    ring1 = Piece(ItemType.Ring, heavy = false, Aim, passive(PassiveKind.Robber)),
    ring2 = Piece(ItemType.Ring, heavy = false, Aim, passive(PassiveKind.Healer)),
    belt = Piece(ItemType.Belt, heavy = true, Tank, ItemDetails.Belt(Flasks)),
    weapon = Piece(ItemType.Weapon, heavy = false, Blade, ItemDetails.Weapon(Skill.CunningStrike), Some(Grip.OneHanded)),
    leftHand = Piece(ItemType.Shield, heavy = true, List(Armor, Defence, Defence), ItemDetails.HandSkill(Skill.Block)),
    brand = Skill.MinorHeal,
    strikes = List(Skill.CunningStrike))

  /** «Дикое пламя»: два меча, сила и ум. Броня — тяжёлая, украшения и руки —
    * лёгкие. Быстрый удар и Вихрь клинка с мечей, Малое исцеление с
    * нагрудника, Хитрый удар клеймом. */
  private val WildFlame = Build(
    name = "Пламя", set = ItemSet.WildFlame,
    stats = BaseStats(agi = 6, vit = 16, str = 34, int = 24),
    helmet = Piece(ItemType.Helmet, heavy = true, List(Armor, Defence, Accuracy), passive(PassiveKind.Hunter)),
    shoulders = Piece(ItemType.ShoulderPads, heavy = true, List(Armor, Defence, Accuracy), passive(PassiveKind.QuickHands)),
    chest = Piece(ItemType.ChestPlate, heavy = true, List(Armor, Defence, Accuracy), ItemDetails.Armor(Skill.MinorHeal)),
    bracelets = Piece(ItemType.Bracelets, heavy = false, Light, passive(PassiveKind.Spiky)),
    gloves = Piece(ItemType.Gloves, heavy = false, Light, passive(PassiveKind.Blending)),
    pants = Piece(ItemType.Pants, heavy = true, List(Armor, Defence, Accuracy), passive(PassiveKind.Reinforced)),
    boots = Piece(ItemType.Boots, heavy = true, List(Armor, Defence, Accuracy), passive(PassiveKind.Impenetrable)),
    amulet = Piece(ItemType.Amulet, heavy = false, Light, passive(PassiveKind.Healing)),
    ring1 = Piece(ItemType.Ring, heavy = false, Light, passive(PassiveKind.Robber)),
    ring2 = Piece(ItemType.Ring, heavy = false, Light, passive(PassiveKind.Healer)),
    belt = Piece(ItemType.Belt, heavy = true, List(Armor, Defence, Accuracy), ItemDetails.Belt(Flasks)),
    weapon = Piece(ItemType.Weapon, heavy = false, Blade, ItemDetails.Weapon(Skill.QuickStrike), Some(Grip.OneHanded)),
    leftHand = Piece(ItemType.Weapon, heavy = false, Blade, ItemDetails.Weapon(Skill.BladeWhirl), Some(Grip.OneHanded)),
    brand = Skill.CunningStrike,
    strikes = List(Skill.BladeWhirl, Skill.CunningStrike, Skill.QuickStrike))

  /** «Упырь»: два кинжала, ловкость и ум. Всё лёгкое, прибавки — в уклонение и
    * точность. Кровотечение и Удар в слабое место с кинжалов, Малое исцеление
    * с нагрудника, Быстрый удар клеймом. */
  private val Ghoul = Build(
    name = "Упырь", set = ItemSet.Ghoul,
    stats = BaseStats(agi = 34, vit = 12, str = 4, int = 30),
    helmet = Piece(ItemType.Helmet, heavy = false, Light, passive(PassiveKind.Stealthy)),
    shoulders = Piece(ItemType.ShoulderPads, heavy = false, Light, passive(PassiveKind.Blending)),
    chest = Piece(ItemType.ChestPlate, heavy = false, Light, ItemDetails.Armor(Skill.MinorHeal)),
    bracelets = Piece(ItemType.Bracelets, heavy = false, Light, passive(PassiveKind.QuickHands)),
    gloves = Piece(ItemType.Gloves, heavy = false, Light, passive(PassiveKind.Spiky)),
    pants = Piece(ItemType.Pants, heavy = false, Light, passive(PassiveKind.QuickFeet)),
    boots = Piece(ItemType.Boots, heavy = false, Light, passive(PassiveKind.Impenetrable)),
    amulet = Piece(ItemType.Amulet, heavy = false, Light, passive(PassiveKind.Healing)),
    ring1 = Piece(ItemType.Ring, heavy = false, Light, passive(PassiveKind.Robber)),
    ring2 = Piece(ItemType.Ring, heavy = false, Light, passive(PassiveKind.Healer)),
    belt = Piece(ItemType.Belt, heavy = false, Light, ItemDetails.Belt(Flasks)),
    weapon = Piece(ItemType.Weapon, heavy = false, List(Attack, Accuracy, Accuracy),
               ItemDetails.Weapon(Skill.WeakSpotStrike), Some(Grip.OneHanded)),
    leftHand = Piece(ItemType.Weapon, heavy = false, List(Attack, Accuracy, Accuracy),
                 ItemDetails.Weapon(Skill.Bleeding), Some(Grip.OneHanded)),
    brand = Skill.QuickStrike,
    strikes = List(Skill.WeakSpotStrike, Skill.Bleeding, Skill.QuickStrike))

  private val Builds: List[Build] = List(StoneGuard, WildFlame, Ghoul)

  // ── Условия ─────────────────────────────────────────────────────────────────

  /** Условия прогона: на сколько уровней моб выше героя, его тир (нет — как
    * выпадет в лабиринте, с «отмеченными тьмой») и понимание рун героя — одно
    * число на все его руны, с вещей и с клейма (0 — руны как есть). */
  private final case class Scenario(label: String, gap: Int, rarity: Option[Rarity] = Some(MobRarity),
                                    understanding: Long = 0L) {
    def mobLvl: Int = HeroLvl.toInt + gap
  }

  /** Главный прогон: мифические на [[LvlGap]] уровней выше, понимания рун нет. */
  private val Main: Scenario = Scenario(s"мифические +$LvlGap", LvlGap)

  private val Scenarios: List[Scenario] = List(
    Scenario("мифические +0", 0),
    Scenario("мифические +5", 5),
    Main,
    Scenario(s"мифические +$LvlGap, понимание рун на потолке", LvlGap, understanding = Rune.cap(HeroLvl)),
    Scenario("50-й этаж, тир как выпадет", 30, rarity = None),
    Scenario("50-й этаж, тир как выпадет, понимание рун на потолке", 30, rarity = None,
      understanding = Rune.cap(HeroLvl)))

  /** Герой сборки — полный: HP, броня и энергия до потолков. Стоит на уровне
    * лабиринта, равном уровню моба: оттуда и сородичи, и подкрепления. */
  private def hero(b: Build, sc: Scenario): Hero = {
    val eq    = b.equipment
    val bare  = FightStats(atk = 22, hp = 0, armor = 0, defence = 8, evasion = 22, accuracy = 20, energy = 0)
    val stats = eq.allItems.foldLeft(bare)((f, it) => InventoryState.applyDelta(f, it, Item.NoItem))
    val brand = RuneData.empty.brand(Rune.Active(b.brand))
    val known = (eq.allItems.flatMap(Rune.of) :+ Rune.Active(b.brand)).map(_.key -> sc.understanding).toMap
    val raw   = TestFixtures.hero(userId, StateType.Battle, dungeonLevel = sc.mobLvl, maxDungeonLevel = sc.mobLvl).copy(
                  lvl = HeroLvl, baseStats = b.stats, fightStats = stats, equipment = eq,
                  runes = if (sc.understanding > 0L) brand.copy(understanding = known) else brand)
    raw.copy(fightStats = stats.copy(hp = raw.effectiveMaxHp(0L), armor = raw.effectiveMaxArmor(0L),
      energy = raw.maxEnergy(0L)))
  }

  // ── Игрок ───────────────────────────────────────────────────────────────────

  private def belt(h: Hero): List[BeltSlot] = h.equipment.belt.details match {
    case ItemDetails.Belt(slots) => slots
    case _                       => Nil
  }

  private def charges(h: Hero): Int = belt(h).collect { case f: BeltSlot.Flask => f.charges }.sum

  private def flaskAt(h: Hero, kind: FlaskKind): Option[Int] =
    Some(belt(h).indexWhere {
      case f: BeltSlot.Flask => f.kind == kind && f.charges > 0
      case _                 => false
    }).filter(_ >= 0)

  private def repairs(s: Skill): Boolean = s.effect match {
    case Skill.Effect.RepairArmor | Skill.Effect.GuardRepair(_, _) => true
    case _                                                         => false
  }

  /** Ход разумного игрока. Бонусное действие раунда (пояс или Блок): фляга
    * целителя ниже 35% HP, кузнеца ниже 20% брони, бодрости — когда не хватает
    * ни на одно умение; иначе — Блок. Ход: лечение ниже 55% HP, броня ниже 50%,
    * иначе ударные умения сборки по готовности, иначе «Атака». */
  private def decide(b: Build, h: Hero, battle: SoloPveBattle): UserAction = {
    val maxHp    = h.effectiveMaxHp(0L).max(1L)
    val maxArmor = h.effectiveMaxArmor(0L)
    val hpPct    = h.fightStats.hp * 100L / maxHp
    val armorPct = if (maxArmor <= 0L) 100L else h.fightStats.armor * 100L / maxArmor
    val bonus    = !battle.consumableUsedThisRound
    val ready    = battle.skillSlots.filter(s => s.cooldown <= 0 && !battle.bonusSlots.contains(s.itemId) &&
                     h.fightStats.energy >= s.skill.energyCost(h))
    val cheapest = battle.skillSlots.map(_.skill.energyCost(h)).minOption.getOrElse(0L)
    def use(s: SkillSlotState): UserAction = act(s"Skill_${s.itemId}")
    def drink(i: Int): UserAction          = act("BeltUse", Map("slot" -> i.toString))
    val healFlask  = flaskAt(h, FlaskKind.Healer).filter(_ => bonus && hpPct < 35L)
    val smithFlask = flaskAt(h, FlaskKind.Smith).filter(_ => bonus && armorPct < 20L)
    val vigorFlask = flaskAt(h, FlaskKind.Vigor).filter(_ => bonus && h.fightStats.energy < cheapest)
    val block      = ready.find(s => BattleState.bonusAction(s.skill.effect)).filter(_ => bonus)
    val heal       = ready.find(_.skill.effect == Skill.Effect.Heal).filter(_ => hpPct < 55L)
    val repair     = ready.find(s => repairs(s.skill)).filter(_ => armorPct < 50L)
    val strike     = b.strikes.flatMap(k => ready.find(_.skill == k)).headOption
    healFlask.map(drink)
      .orElse(smithFlask.map(drink))
      .orElse(vigorFlask.map(drink))
      .orElse(block.map(use))
      .orElse(heal.map(use))
      .orElse(repair.map(use))
      .orElse(strike.map(use))
      .getOrElse(act("Attack"))
  }

  private final case class World(battle: BattleState, dao: TestHeroDao, renderer: TestRenderer)

  /** Нажать; спросил бой цель — выбрать первую из предложенных. Ход не
    * случился (бить некого, умение не вышло) — «Ждать», как сделал бы игрок. */
  private def press(w: World, ua: UserAction): Task[StateType] =
    for {
      before <- w.dao.readActiveBattle(userId)
      _      <- w.renderer.reset
      state  <- w.battle.action(testUser, ua, w.renderer)
      shown  <- w.renderer.sentScreens
      pressed = BattleState.parseAction(ua)
      asked   = shown.lastOption.toList.flatMap(_.choices)
                  .find(c => c.data.contains("target") && pressed.contains(c.id))
      next   <- asked.fold[Task[StateType]](ZIO.succeed(state))(c => w.battle.action(testUser, act(c.id, c.data), w.renderer))
      after  <- w.dao.readActiveBattle(userId)
      moved  <- if (next == StateType.Battle && after == before) w.battle.action(testUser, act("Wait"), w.renderer)
                else ZIO.succeed(next)
    } yield moved

  private def battleOf(dao: TestHeroDao): Task[Option[SoloPveBattle]] =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  // ── Бой ─────────────────────────────────────────────────────────────────────

  /** Итог одного боя. `tier` — тир моба, с которым он начался; `rounds` —
    * сколько раундов шёл; HP и броня — в процентах от потолка после боя;
    * `sips` — сколько глотков ушло с пояса. */
  private final case class Fight(build: String, race: Race, tier: Rarity, state: StateType, rounds: Int,
                                 hpPct: Long, armorPct: Long, sips: Int) {
    def won: Boolean     = state == StateType.Loot
    def died: Boolean    = state == StateType.Death
    def stalled: Boolean = state == StateType.Battle
  }

  /** Бой с одним мобом расы `race`, как в лабиринте: мифический и легендарный
    * зовут сородичей, каждый раунд может прийти подкрепление. Бросками правит
    * `seed`. Бой дольше [[RoundLimit]] раундов бросаем — он затянулся. */
  private def fight(sc: Scenario, b: Build, race: Race, seed: Long): Task[Fight] = {
    val start = hero(b, sc)
    val mob   = sc.rarity match {
      case Some(tier) => MonsterGenerator.generateOfRaceAndRarity(sc.mobLvl, race, tier)
      case None       => MonsterGenerator.generateOfRace(sc.mobLvl, race, Rng(seed))._1
    }
    for {
      _        <- TestRandom.setSeed(seed)
      dao      <- TestHeroDao.withHero(userId, start)
      renderer <- TestRenderer.make
      w         = World(BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)
      pct      <- zio.Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      _        <- dao.writeActiveBattle(userId, SoloPveBattle.from(mob, start).withStartEnergy(pct).asJson)
      end      <- ZIO.iterate((StateType.Battle: StateType, 0, 0))(s =>
                    s._1 == StateType.Battle && s._2 < RoundLimit && s._3 < RoundLimit * 4) {
                    case (_, rounds, presses) =>
                      for {
                        battle <- battleOf(dao).someOrFail(new Throwable("боя нет"))
                        now    <- dao.getHeroByUserId(userId).someOrFail(new Throwable("героя нет"))
                        st     <- press(w, decide(b, now, battle))
                        next   <- battleOf(dao)
                        // Раунд прошёл, если бой его отсчитал или кончился.
                        ended   = st != StateType.Battle || next.exists(_.group.round > battle.group.round)
                      } yield (st, if (ended) rounds + 1 else rounds, presses + 1)
                  }
      after    <- dao.getHeroByUserId(userId).someOrFail(new Throwable("героя нет"))
    } yield {
      val (state, rounds, _) = end
      val maxArmor = after.effectiveMaxArmor(0L)
      Fight(b.name, race, mob.rarity, state, rounds,
        hpPct    = after.fightStats.hp.max(0L) * 100L / after.effectiveMaxHp(0L).max(1L),
        armorPct = if (maxArmor <= 0L) 0L else after.fightStats.armor.max(0L) * 100L / maxArmor,
        sips     = charges(start) - charges(after))
    }
  }

  /** Все бои условия: каждый герой — по [[FightsPerRace]] на каждую смертную
    * расу. Броски у одного и того же боя одни и те же при каждом прогоне. */
  private def run(sc: Scenario): Task[List[Fight]] = {
    val plan = for {
      b    <- Builds
      race <- Race.mortals.toList
      i    <- 1 to FightsPerRace
    } yield (b, race, 7919L * i + 104729L * Race.mortals.indexOf(race) + 1299709L * Builds.indexOf(b))
    ZIO.foreach(plan) { case (b, race, seed) => fight(sc, b, race, seed) }
  }

  // ── Сводка ──────────────────────────────────────────────────────────────────

  private def avg(xs: List[Long]): String =
    if (xs.isEmpty) "—" else f"${xs.sum.toDouble / xs.size}%.1f".replace('.', ',')

  private def median(xs: List[Long]): String =
    if (xs.isEmpty) "—" else xs.sorted.apply(xs.size / 2).toString

  /** Статы героя на старте боя — чтобы было видно, с чем он вышел. */
  private def card(sc: Scenario, b: Build): String = {
    val h = hero(b, sc)
    val f = h.effectiveFightStats(0L)
    val s = h.effectiveBaseStats(0L)
    s"${b.name} («${b.set.label}», ${h.sets.pieces(b.set)} вещей): " +
      s"СИЛ ${s.str} ТЕЛО ${s.vit} ЛОВ ${s.agi} ИНТ ${s.int} | ❤ ${h.fightStats.hp} 🧥 ${h.fightStats.armor} " +
      s"⚡ ${h.fightStats.energy} | ⚔ ${f.atk} 🛡 ${f.defence} 🎯 ${f.accuracy} 👁 ${f.evasion} | " +
      s"умения: ${h.activeSkillSlots.map(_.skill.label).mkString(", ")}"
  }

  private def mobCard(sc: Scenario, race: Race): String = {
    val f = MonsterGenerator.generateOfRaceAndRarity(sc.mobLvl, race, MobRarity).fightStats
    s"  $race: ❤ ${f.hp} 🧥 ${f.armor} ⚔ ${f.atk} 🛡 ${f.defence} 🎯 ${f.accuracy} 👁 ${f.evasion}"
  }

  private val Header: String =
    "Герой    | Побед   | Смертей | Затянуто | Раундов | Медиана | Макс | HP после | HP мин | Броня после | Глотков"

  private def row(name: String, fs: List[Fight]): String = {
    val won = fs.filter(_.won)
    f"$name%-8s | ${won.size}%3d/${fs.size}%-3d | ${fs.count(_.died)}%7d | ${fs.count(_.stalled)}%8d | " +
      f"${avg(won.map(_.rounds.toLong))}%7s | ${median(won.map(_.rounds.toLong))}%7s | " +
      f"${won.map(_.rounds).maxOption.fold("—")(_.toString)}%4s | ${avg(won.map(_.hpPct))}%7s%% | " +
      f"${won.map(_.hpPct).minOption.fold("—")(_.toString)}%5s%% | ${avg(won.map(_.armorPct))}%10s%% | " +
      f"${avg(fs.map(_.sips.toLong))}%7s"
  }

  /** Победы в разрезе: по расе моба — когда тир задан, по тиру — когда он
    * выпадает как в лабиринте. */
  private def split(sc: Scenario, fights: List[Fight]): List[String] =
    Builds.map { b =>
      val own = fights.filter(_.build == b.name)
      s"  ${b.name}: " + (sc.rarity match {
        case Some(_) =>
          Race.mortals.toList.map(r => own.filter(_.race == r)).map(fs => s"${fs.head.race} ${fs.count(_.won)}/${fs.size}")
        case None =>
          Rarity.values.toList.map(t => t -> own.filter(_.tier == t)).collect {
            case (t, fs) if fs.nonEmpty => s"$t ${fs.count(_.won)}/${fs.size}"
          }
      }).mkString(", ")
    }

  private def report(sc: Scenario, fights: List[Fight]): String = {
    val versus = sc.rarity.fold(s"мобов ${sc.mobLvl}-го")(t => s"$t ${sc.mobLvl}-го")
    (List("", s"=== $HeroLvl-й уровень против $versus: ${sc.label} ===") ++
      Builds.map(card(sc, _)) ++ List(s"$MobRarity моб ${sc.mobLvl}-го по расам:") ++
      Race.mortals.toList.map(mobCard(sc, _)) ++
      List(Header) ++ Builds.map(b => row(b.name, fights.filter(_.build == b.name))) ++
      List(if (sc.rarity.isDefined) "Побед по расам:" else "Побед по тиру моба:") ++ split(sc, fights)).mkString("\n")
  }

  override def spec = suite("Баланс фарма: герои 20-го уровня в фиолетовом")(
    test(s"три героя $HeroLvl-го уровня против мифических +$LvlGap и 50-го этажа: сводка") {
      for {
        runs <- ZIO.foreach(Scenarios)(sc => run(sc).map(sc -> _))
        _    <- ZIO.succeed(println(runs.map { case (sc, fs) => report(sc, fs) }.mkString("\n")))
        all   = runs.flatMap(_._2)
      } yield assertTrue(all.size == Scenarios.size * Builds.size * Race.mortals.size * FightsPerRace) &&
              // Прогон честный: бой кончается победой, смертью или ничьей по
              // затяжке — не бегством и не чем-то третьим.
              assertTrue(all.forall(f => f.won || f.died || f.stalled))
    }
  )
}
