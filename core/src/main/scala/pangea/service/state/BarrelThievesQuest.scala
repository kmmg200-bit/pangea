package pangea.service.state

import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemNameGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.barrel.Barrel
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{Item, ItemSet}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}

/** «Вскрытая бочка» — всё, что нужно сюжету в разных сценах: когда бросать
  * жребий на кражу из бочки, какую банду пустить, кто в банде, как гоблины
  * перешивают украденное и сколько платить. Сам ход — [[NpcQuest.BarrelThieves]]
  * в `npc_quests`, украденное — в бочке (`barrels.thieves`); сцены — бочка в
  * порту (кража и сломанная бочка), Ришелье (кто это), карточка улики (по
  * следу), склад у причала (бой) и возврат в бочку.
  *
  * Задание повторяется: после возврата бочку могут вскрыть снова. Впервые —
  * банда первая из списка, 4000 серебра и «Своё не отдам»; дальше — любая
  * другая банда и 500 серебра. У героя-гоблина банда говорит иначе: `key`
  * подставляет вариант `<ключ>Kin`. */
object BarrelThievesQuest {

  /** С какого уровня героя бочку могут вскрыть. */
  val MinHeroLevel: Long = 3L

  /** Шанс кражи при открытии бочки, в процентах. */
  val ChancePct: Int = 5

  /** Жребий бросают не чаще раза в сутки. */
  val RollEveryMs: Long = 24L * 60L * 60L * 1000L

  /** Награда: впервые — столько серебра и «Своё не отдам», потом — меньше. */
  val FirstSilver: Long  = 4000L
  val RepeatSilver: Long = 500L

  /** Банда — четверо гоблинов-рабов уровня героя. */
  val GangSize: Int      = 4
  val GangRace: Race     = Race.Goblin
  val GangRarity: Rarity = Rarity.Common

  /** Набор, в который гоблины перешивают вещи из наборов. */
  val ReworkSet: ItemSet = ItemSet.Trickster

  /** Ключ сюжетного боя (см. `SoloPveBattle.story`). Награда как за обычный
    * бой — по уровню героя: банда его уровня. */
  val FightStory: String = "barrelThieves"

  /** Где в `scenes.yaml` список банд: первая — та, что вскрывает бочку впервые. */
  val GangsKey: String = "barrelThieves.gangs"

  /** Герой той же крови — банда говорит с ним иначе. */
  def isKin(hero: Hero): Boolean = hero.race == GangRace

  /** Ключ текста с поправкой на кровь героя: гоблину — `<key>Kin`. */
  def key(hero: Hero, base: String): String = if (isKin(hero)) base + "Kin" else base

  // ── Ход задания ─────────────────────────────────────────────────────────────

  /** Бочка вскрыта, украденное ещё у воров. */
  def active(q: NpcQuests): Boolean = q.isTaken(NpcQuest.BarrelThieves)

  /** Узнать в городе, чья работа. */
  def askAround(q: NpcQuests): Boolean = q.onStep(NpcQuest.BarrelThieves, 1)

  /** Место известно — по следу. */
  def onTrail(q: NpcQuests): Boolean = q.onStep(NpcQuest.BarrelThieves, 2)

  /** Какая банда — номер в списке банд. */
  def gangIndex(q: NpcQuests): Int = q.of(NpcQuest.BarrelThieves).counter.toInt

  /** Имя банды для текста. */
  def gangName(content: SceneContent, q: NpcQuests): String = {
    val gangs = content.list(GangsKey)
    gangs.lift(gangIndex(q)).orElse(gangs.headOption).getOrElse("")
  }

  /** Впервые ли бочку вскрывают: достижение даётся за первый возврат. */
  def firstTime(hero: Hero): Boolean = !hero.hasAchievement(Achievement.OwnBack)

  /** Можно ли бросать жребий на кражу сейчас: герой дорос, задание не идёт,
    * бочка цела и в ней есть хоть одна вещь, и сутки с прошлого жребия прошли. */
  def canRoll(hero: Hero, quests: NpcQuests, barrel: Barrel, nowMs: Long): Boolean =
    hero.lvl >= MinHeroLevel && !active(quests) && !barrel.robbed &&
      barrel.items.data.exists(!_.isQuestItem) && nowMs - barrel.thievesRolledAt >= RollEveryMs

  /** Какую банду пустить: впервые — первую, потом — любую, кроме первой
    * (`roll` — любое неотрицательное число). Банда одна — её же. */
  def pickGang(first: Boolean, gangs: Int, roll: Int): Int =
    if (first || gangs <= 1) 0 else 1 + math.abs(roll % (gangs - 1))

  /** Бочку вскрыли: задание на первый шаг, банда записана. */
  def started(q: NpcQuests, gang: Int, nowMs: Long): NpcQuests =
    q.take(NpcQuest.BarrelThieves, nowMs).update(NpcQuest.BarrelThieves)(_.copy(counter = gang.toLong))

  /** Ришелье назвал место: по следу. Не тот шаг — ничего. */
  def told(q: NpcQuests): NpcQuests =
    if (askAround(q)) q.update(NpcQuest.BarrelThieves)(_.copy(step = 2)) else q

  /** Награда серебром за возврат. */
  def silverFor(hero: Hero): Long = if (firstTime(hero)) FirstSilver else RepeatSilver

  // ── Банда и украденное ──────────────────────────────────────────────────────

  /** Четверо гоблинов-рабов уровня героя. */
  def gang(heroLvl: Long): List[Monster] =
    List.fill(GangSize)(MonsterGenerator.generateOfRaceAndRarity(heroLvl.toInt, GangRace, GangRarity))

  /** Гоблины перешили украденное: вещь из любого набора — теперь вещь
    * «Пройдохи», с тем же уровнем, редкостью и характеристиками, только имя
    * набора в названии другое. Вещи без набора и уже гоблинские — как были.
    * Возвращает всё и отдельно — перешитые. */
  def rework(items: List[Item], rng: Rng): (List[Item], List[Item]) = {
    val (all, changed, _) = items.foldLeft((List.empty[Item], List.empty[Item], rng)) { case ((acc, ch, r), item) =>
      if (item.set.isEmpty || item.set.contains(ReworkSet)) (acc :+ item, ch, r)
      else {
        val (name, next) = ItemNameGenerator.setName(item.itemType, item.rarity, ReworkSet, r, item.grip)
        val goblin       = item.copy(name = name, set = Some(ReworkSet))
        (acc :+ goblin, ch :+ goblin, next)
      }
    }
    (all, changed)
  }
}
