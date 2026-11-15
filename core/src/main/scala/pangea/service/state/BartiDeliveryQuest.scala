package pangea.service.state

import pangea.generator.monster.MonsterGenerator
import pangea.model.item.{BrewKind, Item}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}

/** «Большая поставка» — побочное задание Барти в Акте II
  * ([[NpcQuest.BartiDelivery]], задание 36-го уровня, все пути). Барти взял
  * заказ — [[Goal]] бутылей самогона в лагерь Чернолома у Старой вырубки (не
  * та бригада, что у просеки: у Чернолома их много), — и плату вперёд взял
  * досками: «серебром туго». Самогон — это шнапс, ещё раз через куб с полынью и
  * шалфеем: рецепт Барти даёт, а куба у него нет.
  *
  * Берётся в его лавке — кнопкой с экрана дома Барти, — когда «Крыша» закрыта.
  * Шаги: 1 — самогон собирается у Барти (носить можно хоть по одной бутыли,
  * сколько сложено — в `counter`); 2 — везти в лагерь: бригада вскрывает
  * ящики до расчёта и дерётся за бутыли (бой, Барти — под телегой); 3 — остаток
  * тоже досками, и сбыть их надо прорабу Гуннару на городской стройке: он
  * платит золотом. Враги, сложность и награда — по уровню задания, а не героя. */
object BartiDeliveryQuest {

  /** Уровень задания: по нему сложность и бригада. */
  val Level: Int = 36

  /** Ключ сюжетного боя в лагере: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "bartiDelivery"

  /** Сколько бутылей самогона ждут в лагере. */
  val Goal: Int = 10

  /** Что Барти принимает. */
  val Good: BrewKind = BrewKind.Moonshine

  /** Рецепт самогона, что Барти рассказывает, — ключ в [[NpcQuests.recipes]]. */
  val Recipe: String = s"brew:${Good.entryName}"

  /** Пьяная бригада: именной десятник и четверо — гоблины уровня задания. */
  val LoggerRace: Race         = Race.Goblin
  val ForemanRarity: Rarity    = Rarity.Mythical
  val LoggerRarity: Rarity     = Rarity.Rare
  val Loggers: Int             = 4

  /** Сколько дублонов Гуннар платит за доски — так решил пользователь. */
  val Doubloons: Long = 80L

  /** Задание можно взять: «Крыша» закрыта, «Поставки» ещё не было. */
  def available(q: NpcQuests): Boolean =
    q.isDone(NpcQuest.BartiRoof) && !q.isTaken(NpcQuest.BartiDelivery) && !q.isDone(NpcQuest.BartiDelivery)

  /** Кнопка лавки на экране дома Барти: задание доступно или идёт. */
  def shopOpen(q: NpcQuests): Boolean = available(q) || q.isTaken(NpcQuest.BartiDelivery)

  def toCollect(q: NpcQuests): Boolean = q.onStep(NpcQuest.BartiDelivery, 1)
  def toCamp(q: NpcQuests): Boolean    = q.onStep(NpcQuest.BartiDelivery, 2)
  def toGunnar(q: NpcQuests): Boolean  = q.onStep(NpcQuest.BartiDelivery, 3)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.BartiDelivery)(_.copy(step = n))

  /** Сколько бутылей у Барти уже сложено. */
  def stock(q: NpcQuests): Int = q.of(NpcQuest.BartiDelivery).counter.toInt.max(0).min(Goal)

  def withStock(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.BartiDelivery)(_.copy(counter = n.toLong))

  /** Что из сумки Барти возьмёт сейчас: бутыли по одной (не из стопок) — не
    * больше, чем ему ещё нужно. */
  def takeable(items: List[Item], have: Int): List[Item] =
    items.filter(_.brew.contains(Good)).take((Goal - have).max(0))

  /** Пьяная бригада: десятник — первым, именной, и четверо. */
  def foes(foreman: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, LoggerRace, ForemanRarity, Some(foreman)) ::
      List.fill(Loggers)(MonsterGenerator.generateOfRaceAndRarity(Level, LoggerRace, LoggerRarity))
}
