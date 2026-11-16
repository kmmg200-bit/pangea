package pangea.service.state

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{BattleAlly, BattleBody, SoloPveBattle}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.{BeltSlot, FlaskKind, Gem, GemKind, Grip, Item, ItemDetails, ItemType}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.rune.{Rune, RuneData}
import pangea.model.skill.{MonsterEnergy, Skill}
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.User
import pangea.service.state.states.InventoryState
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.story.StoryFight
import zio.{Random, Task, ZIO}

/** Бой перед прологом: последнее дело Лео — ещё Тени Братства — вместе с
  * напарником Элиасом. Братство взяло заказ на головы знатного дома;
  * против — дворянская стража, десять человек, с Лео и Элиасом — три шестёрки
  * гильдии. Так решил пользователь: игрок с первой кнопки видит бой целиком —
  * умения, пояс, строй — как ассасин 25-го уровня.
  *
  * Герой дерётся в чужом теле (см. BattleBody): своё у него ещё первого
  * уровня, и этот бой его не касается — ни опыта, ни добычи, ни убитых в
  * счёт. Проиграть нельзя — HP Лео и Элиаса не опускается ниже
  * [[SparePct]], — но со стороны этого не видно: стража бьёт всерьёз, шестёрки
  * падают. После победы пролог идёт как был. */
object PrologueFight {

  /** Ключ боя: сюжетный — без «Сбежать» и без подкреплений. */
  val Story: String = "prologueHouse"

  /** Уровень боя: Лео, Элиас, шестёрки и стража — все двадцать пятого. */
  val Level: Int = 25

  /** Отметка победы в scene_data: с ней пролог идёт дальше (см. [[won]]). */
  val WonKey: String = "prologueFightWon"

  /** Страховка: HP Лео и Элиаса не опускается ниже этой доли максимума. */
  val SparePct: Long = 15L

  /** Элиас — равный герою: легендарный, «именной» человек уровня боя. */
  val EliasRarity: Rarity = Rarity.Legendary

  /** Шестёрки гильдии — по силе как человек-раб. */
  val LackeyRarity: Rarity = Rarity.Common
  val Lackeys: Int         = 3

  /** Стража дома: капитан — мифический, латники — редкие, стражники —
    * необычные. */
  val CaptainRarity: Rarity = Rarity.Mythical
  val KnightRarity: Rarity  = Rarity.Rare
  val GuardRarity: Rarity   = Rarity.Uncommon

  /** Стража бьёт сильнее своей редкости — дом платил за выучку: атака вдвое
    * против обычной. HP — своё: бой не тянется дольше. С такой стражей Лео,
    * пуская в ход умения, теряет треть-половину HP за бой, а одной «Атакой»
    * доходит до края страховки (подобрано прогоном боёв в PrologueFightSpec). */
  val GuardAtkPct: Long = 200L

  // ── Тело Лео ──────────────────────────────────────────────────────────────

  /** Характеристики: старт и 24 уровня очков — в ловкость и интеллект, как у
    * ассасина. */
  val BodyStats: BaseStats = BaseStats(agi = 40, vit = 24, str = 14, int = 34)

  /** Боевые статы без снаряжения — те же, с какими начинает каждый герой. */
  private val BareStats: FightStats =
    FightStats(atk = 22, hp = 0, armor = 0, defence = 8, evasion = 22, accuracy = 20, energy = 0)

  /** Идеальный рубин в гнезде: огонь на клинках. */
  private val Ruby: Option[Gem] = Some(Gem(GemKind.Ruby, Gem.MaxGrade))

  /** Вещь тела: легендарная, уровня боя. Номера — свои и положительные:
    * отрицательный номер бой считает клеймом на теле (Rune.isBodySlot). */
  private def piece(id: Long, name: String, t: ItemType, attack: Long = 0L, accuracy: Long = 0L,
                    energy: Long = 0L, armor: Long = 0L, defence: Long = 0L, evasion: Long = 0L,
                    hp: Long = 0L, details: ItemDetails = ItemDetails.Plain, sockets: List[Option[Gem]] = Nil,
                    grip: Option[Grip] = None): Item = {
    val orange = pangea.model.item.Rarity.Orange
    Item(id, s"${orange.emoji} $name", Level.toLong, orange, t, attack, accuracy, energy, armor, defence,
      evasion, hp, details, sockets, grip = grip)
  }

  /** Пояс: легендарные фляги — целителя, кузнеца, бодрости и огненная, полные. */
  private val Flasks: List[BeltSlot] =
    List(FlaskKind.Healer, FlaskKind.Smith, FlaskKind.Vigor, FlaskKind.Fire).map(kind =>
      BeltSlot.Flask(kind, pangea.model.item.Rarity.Orange, FlaskKind.chargesFor(pangea.model.item.Rarity.Orange)))

  /** Снаряжение: два огненных клинка («Быстрый удар» и «Хитрый удар»), куртка
    * с «Укреплением», легендарное всё остальное. Вместе со статами тела —
    * примерно легендарный человек 25-го уровня: ❤ 3300, 🛡 2846, ⚔ 825. */
  val Gear: Equipment = Equipment(
    helmet       = piece(9004, "Капюшон Тени", ItemType.Helmet, armor = 350, defence = 100, hp = 400),
    shoulderPads = piece(9005, "Наплечники Тени", ItemType.ShoulderPads, armor = 300, defence = 80, hp = 300),
    chestPlate   = piece(9003, "Куртка Тени", ItemType.ChestPlate, armor = 900, defence = 200, hp = 1200,
                     details = ItemDetails.Armor(Skill.Reinforcement)),
    bracelets    = piece(9006, "Наручи Тени", ItemType.Bracelets, armor = 250, evasion = 250),
    gloves       = piece(9007, "Перчатки Тени", ItemType.Gloves, armor = 250, accuracy = 80, evasion = 200),
    pants        = piece(9008, "Штаны Тени", ItemType.Pants, armor = 400, defence = 100, hp = 424),
    boots        = piece(9009, "Сапоги Тени", ItemType.Boots, armor = 396, evasion = 244, hp = 400),
    amulet       = piece(9010, "Амулет Тени", ItemType.Amulet, defence = 89, evasion = 100, energy = 60),
    firstRing    = piece(9011, "Кольцо Тени", ItemType.Ring, accuracy = 42, evasion = 60),
    secondRing   = piece(9012, "Перстень Тени", ItemType.Ring, accuracy = 42, evasion = 60),
    belt         = piece(9013, "Пояс Тени", ItemType.Belt, details = ItemDetails.Belt(Flasks)),
    weapon       = piece(9001, "Клинок Тени", ItemType.Weapon, attack = 400, accuracy = 200,
                     details = ItemDetails.Weapon(Skill.QuickStrike), sockets = List(Ruby, Ruby),
                     grip = Some(Grip.OneHanded)),
    additionalWeapon = Item.NoItem,
    leftHand     = piece(9002, "Второй клинок Тени", ItemType.Weapon, attack = 361, accuracy = 180,
                     details = ItemDetails.Weapon(Skill.CunningStrike), sockets = List(Ruby, Ruby),
                     grip = Some(Grip.OneHanded)))

  /** «Вихрь клинка» — клеймом на теле: умения носят только клинки и куртка. */
  val Brands: RuneData = RuneData.empty.brand(Rune.Active(Skill.BladeWhirl))

  /** Тело Лео — полное: HP, броня и энергия до своих потолков. Потолки
    * считаются по телу, надетому на героя (`hero`): у тела нет своего
    * отдельного счёта. */
  def body(hero: Hero, nowMs: Long): BattleBody = {
    val stats = Gear.allItems.foldLeft(BareStats)((f, it) => InventoryState.applyDelta(f, it, Item.NoItem))
    val raw   = BattleBody(lvl = Level.toLong, race = Race.Human, baseStats = BodyStats, fightStats = stats,
                  equipment = Gear, runes = Brands, sparePct = SparePct)
    val worn  = raw.on(hero)
    raw.copy(fightStats = stats.copy(hp = worn.effectiveMaxHp(nowMs), armor = worn.effectiveMaxArmor(nowMs),
      energy = worn.maxEnergy(nowMs)))
  }

  // ── Строй ─────────────────────────────────────────────────────────────────

  /** Дворянская стража по местам: капитан — напротив Лео, за ним латники,
    * дальше стражники. Имена — свои у каждого (одинаковых бой не терпит). */
  def foes(content: SceneContent): List[Monster] = {
    def guard(name: String, rarity: Rarity): Monster = {
      val m = MonsterGenerator.generateOfRaceAndRarity(Level, Race.Human, rarity, Some(name))
      m.copy(fightStats = m.fightStats.copy(atk = m.fightStats.atk * GuardAtkPct / 100L))
    }
    guard(content.text("registration.prologueFight.captain"), CaptainRarity) ::
      content.list("registration.prologueFight.knights").map(guard(_, KnightRarity)) ++
      content.list("registration.prologueFight.guards").map(guard(_, GuardRarity))
  }

  /** Рядом с Лео: Элиас — первым, у самого плеча, со страховкой, — и шестёрки. */
  def guests(content: SceneContent): List[BattleAlly] = {
    val elias = StoryMorningQuest.guest(AllyKind.BrotherhoodAgent, content.text("registration.prologueFight.elias"),
                  EliasRarity, Level)
    elias.copy(spare = elias.stats.hp * SparePct / 100L) ::
      List.fill(Lackeys)(StoryMorningQuest.guest(AllyKind.GuildLackey,
        content.text("registration.prologueFight.lackey"), LackeyRarity, Level))
  }

  /** Записать бой: Лео в своём прежнем теле, у плеча Элиас, дальше шестёрки;
    * против — стража со стартовой энергией, как у всех мобов. Победа вернёт в
    * регистрацию с отметкой [[WonKey]]. */
  def start(heroDao: HeroDao, user: User, hero: Hero, content: SceneContent, nowMs: Long): Task[Unit] = {
    val leo    = body(hero, nowMs)
    val guards = foes(content)
    for {
      energies <- ZIO.foreach(guards)(m =>
                    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct)))
      plain     = SoloPveBattle.fromGroup(guards, leo.on(hero), energies, squad = false)
      battle    = StoryMorningQuest.withGuests(plain.copy(story = Some(Story), body = Some(leo)), guests(content))
      routing   = LootData(Nil, Nil, returnState = Some(StateType.Registration), eventData = Some(StoryFight.mark(WonKey)))
      _        <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _        <- heroDao.writeSceneData(user.userId, routing.asJson)
    } yield ()
  }

  /** Бой пролога выигран: отметка в scene_data. */
  def won(scene: Option[Json]): Boolean = StoryFight.won(scene, WonKey)
}
