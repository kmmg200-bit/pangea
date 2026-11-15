package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.dao.quest.QuestOutcomeDao
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests, QuestOutcome}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.parcel.Parcels
import pangea.service.state.StoryToolQuest.Path
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Долговая яма» — побочное задание Барти в Акте II
  * ([[NpcQuest.BartiPit]], задание 44-го уровня, все пути). Барти занял у
  * Торгового дома под залог своего дома, вложился в «одно дело» и прогорел:
  * дом опечатан, сам он — в долговой яме совета. Берётся у синей двери — кнопкой
  * «Дом Барти» с Торговой площади, — когда «Большая поставка» закрыта, а герой
  * дорос до [[MinHeroLvl]]-го уровня: дом опечатан, экрана дома нет (см.
  * BartiHome), пока Барти не вытащат.
  *
  * Как вытащить — по пути Рубежа I, и выбор помним ([[Way]] — в `counter`):
  *  - Тень (шаг 1): у дома ждёт человек Братства — оно заплатит, и Барти станет
  *    его человеком; или заплатить самому;
  *  - Щит (шаг 2): Юнона — заплатить самому или пусть она спишет долг (в
  *    договоре проценты, которых совет не разрешал);
  *  - Сам по себе (шаг 3): ночью через яму — бой с надзирателем и стражей.
  * Напоследок Барти отдаёт сюжетный пояс надзирателя ★ — «случайно
  * приобрёл». Как вытащили — и в подсчёт «как и вы». Враги и сложность — по
  * уровню задания, а не героя. */
object BartiPitQuest {

  /** Уровень задания: по нему сложность, стража ямы и долг. */
  val Level: Int = 44

  /** Ключ сюжетного боя в яме: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "bartiPit"

  /** Долг Барти Торговому дому — тысяча за уровень задания. */
  val DebtPerLvl: Long = 1000L

  def debt: Long = DebtPerLvl * Level

  /** Стража ямы: именной надзиратель и четверо — люди уровня задания. */
  val GuardRace: Race       = Race.Human
  val WardenRarity: Rarity  = Rarity.Mythical
  val GuardRarity: Rarity   = Rarity.Rare
  val Guards: Int           = 4

  /** Пояс надзирателя ★ — сюжетная экипировка уровня задания (так решил
    * пользователь), оранжевая, как каджитский амулет. */
  val BeltLvl: Long          = Level.toLong
  val BeltRarity: ItemRarity = ItemRarity.Orange

  def belt(rng: Rng, name: String): Item =
    ItemGenerator.createItemOfType(ItemType.Belt, BeltLvl, BeltRarity, rng)._1.withName(name).asStoryGear

  /** Как вытащили Барти: номер — в `counter` закрытого задания, исход — в
    * подсчёт «как и вы». */
  sealed abstract class Way(val n: Long, val key: String, val outcome: QuestOutcome)

  object Way {
    /** Герой заплатил сам (Тень или Щит). */
    case object Self extends Way(1L, "self", QuestOutcome.PitSelf)
    /** Юнона списала долг (Щит). */
    case object Junona extends Way(2L, "junona", QuestOutcome.PitJunona)
    /** Заплатило Братство — Барти теперь его человек (Тень). */
    case object Brotherhood extends Way(3L, "brotherhood", QuestOutcome.PitBrotherhood)
    /** Вытащил ночью сам, через стражу (одиночка). */
    case object Breakout extends Way(4L, "breakout", QuestOutcome.PitBreakout)

    val all: List[Way] = List(Self, Junona, Brotherhood, Breakout)
  }

  /** С какого уровня героя «Яма» появляется: раньше её нет вовсе — ни
    * опечатанной двери, ни задания, «рано» не говорим. Так решил
    * пользователь — исключение из «задание по уровню героя не запираем», как
    * «Большое дело» до 11-го. Уже взятая «Яма» идёт дальше на любом уровне. */
  val MinHeroLvl: Long = 35L

  /** Задание можно взять: герой не ниже [[MinHeroLvl]], «Большая поставка»
    * закрыта, «Ямы» ещё не было. */
  def available(q: NpcQuests, hero: Hero): Boolean =
    hero.lvl >= MinHeroLvl && q.isDone(NpcQuest.BartiDelivery) && !q.isTaken(NpcQuest.BartiPit) &&
      !q.isDone(NpcQuest.BartiPit)

  /** Дом Барти опечатан: задание доступно или идёт. */
  def houseOpen(q: NpcQuests, hero: Hero): Boolean = available(q, hero) || q.isTaken(NpcQuest.BartiPit)

  /** Шаг — по пути Рубежа I: Тень — у дома (1), Щит — у Юноны (2), одиночка —
    * ночью в яму (3). */
  def firstStep(q: NpcQuests): Int = StoryToolQuest.choice(q) match {
    case Some(Path.Shadow) => 1
    case Some(Path.Shield) => 2
    case _                 => 3
  }

  def toOffer(q: NpcQuests): Boolean  = q.onStep(NpcQuest.BartiPit, 1)
  def toJunona(q: NpcQuests): Boolean = q.onStep(NpcQuest.BartiPit, 2)
  def toNight(q: NpcQuests): Boolean  = q.onStep(NpcQuest.BartiPit, 3)

  /** Взять задание — сразу на шаге своего пути. */
  def take(q: NpcQuests, now: Long): NpcQuests =
    q.take(NpcQuest.BartiPit, now).update(NpcQuest.BartiPit)(_.copy(step = firstStep(q)))

  /** Как вытащили Барти — у закрытого задания. */
  def way(q: NpcQuests): Option[Way] =
    Option.when(q.isDone(NpcQuest.BartiPit))(q.of(NpcQuest.BartiPit).counter).flatMap(n => Way.all.find(_.n == n))

  /** Стража ямы: надзиратель — первым, именной, и четверо. */
  def foes(warden: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, WardenRarity, Some(warden)) ::
      List.fill(Guards)(MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, GuardRarity))

  /** Барти на свободе: задание закрыто с тем, как его вытащили, опыт, а пояс
    * надзирателя — в сумку (не влез — посылкой, см. StoryGearPost; без хранилища
    * вещей — в старых тестах — пояса нет). Исход — в подсчёт «как и вы». Ответ —
    * рассказ, пояс, опыт и «Квест завершён» со статистикой. */
  def finish(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: Option[ItemRepository],
             parcels: Option[Parcels], outcomes: Option[QuestOutcomeDao], content: SceneContent, user: User, hero: Hero,
             way: Way): Task[String] =
    for {
      done <- NpcQuestDialog(heroDao, content, NpcQuest.BartiPit, "BartiPit")
                .complete(user, hero, _.update(NpcQuest.BartiPit)(_.copy(counter = way.n)))
      (_, expLine) = done
      gift <- itemRepo.fold[Task[String]](ZIO.succeed("")) { items =>
                for {
                  seed  <- Random.nextLong
                  now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
                  given <- StoryGearPost.give(inventoryRepo, items, parcels, hero, content.text("barti.caravan.amuletFrom"),
                             List(belt(Rng(seed), content.text("barti.pit.beltName"))), now)
                  (bagged, posted) = given
                } yield bagged.map(i => "\n\n" + content.format("barti.caravan.amuletLine", "name" -> i.displayTitle)).mkString +
                  posted.fold("")("\n\n" + _)
              }
      stats <- QuestOutcomeLog.recordAndCount(outcomes, content, hero, way.outcome)
    } yield List(
      content.format(s"barti.pit.freed.${way.key}", "debt" -> debt.toString).trim,
      content.text("barti.pit.belt").trim + gift,
      expLine,
      QuestOutcomeLog.withStats(content.text("barti.pit.questDone"), stats)).mkString("\n\n")
}
