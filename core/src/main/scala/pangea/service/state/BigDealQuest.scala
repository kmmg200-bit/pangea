package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{BattleAlly, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{BrewKind, Item, ItemType, QuestItemKind, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.{AllyKind, UndeadForm}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.parcel.Parcels
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Большое дело» ([[NpcQuest.BigDeal]]) — вторая часть линии Барти:
  * контрабанда с каджитским караваном. Ведёт адрес Барти из «Ходового товара»:
  * с карточки — к нему домой, на Торговую площадь. Барти затевает продать
  * каджитам живую воду и шнапс — «платят не серебром, золотом» — и просит
  * [[Goal]] бутылок шнапса и столько же склянок живой воды; носить можно хоть
  * по одной, он складывает у себя ([[Stock]]).
  *
  * Шаги: 1 — товар собирается у Барти; 2 — всё собрано, ждать каравана: пока
  * герой ниже уровня задания, Барти говорит «рано, караван не пришёл» (для
  * этого задания так решил пользователь); 3 — караван пришёл: ночью к нему, к
  * Сухому колодцу на Западной дороге (BartiCaravanState). По всему заданию
  * кажется, что каджиты кинут, — а засада оказывается от ночной стражи (бой).
  * Отбился — каджиты платят [[WonDoubloons]] дублонов; пал — задание всё равно
  * закрыто: каджиты отбились сами и заплатили [[FallenDoubloons]], об этом
  * расскажет Барти. Враги, сложность и награда — по уровню задания. */
object BigDealQuest {

  /** Уровень задания: по нему сложность (и, с караваном, стража и плата). */
  val Level: Int = 11

  /** Сколько бутылок шнапса и склянок живой воды просит Барти — каждого. */
  val Goal: Int = 20

  /** Ключ сюжетного боя у каравана: награда как за обычный бой, без «Сбежать». */
  val AmbushStory: String = "bartiAmbush"

  /** Ночная стража: капитан и трое — люди, уровня задания. */
  val GuardCount: Int       = 3
  val GuardRace: Race       = Race.Human
  val GuardRarity: Rarity   = Rarity.Uncommon
  val CaptainRarity: Rarity = Rarity.Rare

  /** Герой-каджит: караван встречает собрата, а в засаде двое капитанов
    * каравана сразу встают на его сторону — по силе редкие каджиты уровня
    * задания. */
  val KinCaptainRarity: Rarity = Rarity.Rare

  def isKin(hero: Hero): Boolean = hero.race == Race.Khajiit

  /** Капитан каравана в строю героя. */
  def captain(name: String): BattleAlly = {
    val stats = MonsterGenerator.generateOfRaceAndRarity(Level, Race.Khajiit, KinCaptainRarity).fightStats
    BattleAlly(AllyKind.CaravanKhajiit, position = 0, hp = stats.hp, armor = stats.armor, energy = stats.energy,
      stats = stats, lvl = Level.toLong, undead = Some(UndeadForm(name, Level.toLong, stats)), home = 0)
  }

  /** Поставить капитанов в строй — каждого на ближайшее к герою свободное
    * место. Отряд занял всё — кому места нет, тот бьётся из-за повозок и в
    * строй не встаёт. */
  def withCaptains(battle: SoloPveBattle, names: List[String]): SoloPveBattle =
    names.foldLeft(battle)((b, name) =>
      b.group.nearestFreeAllyPlace.fold(b)(pos =>
        b.copy(group = b.group.copy(allies = b.group.allies :+ captain(name).copy(position = pos)))))

  /** Сколько дублонов платят каджиты: герой отбился сам — и пал (каджиты
    * отбились без него и взяли долю за охрану товара). */
  val WonDoubloons: Long    = 125L
  val FallenDoubloons: Long = 85L

  /** Что Барти принимает: шнапс и живую воду. */
  val Goods: List[BrewKind] = List(BrewKind.Schnapps, BrewKind.LivingWater)

  /** Сколько у Барти уже сложено. Лежит в `counter` задания одним числом:
    * шнапс — сотни, живая вода — единицы (`1207` — 12 шнапса и 7 живой воды);
    * больше [[Goal]] каждого он не берёт. */
  final case class Stock(schnapps: Int, water: Int) {
    def full: Boolean   = schnapps >= Goal && water >= Goal
    def counter: Long   = schnapps.toLong * 100L + water.toLong
    def of(kind: BrewKind): Int = if (kind == BrewKind.Schnapps) schnapps else if (kind == BrewKind.LivingWater) water else 0

    /** Сложить `n` склянок этого вида — не больше, чем ещё нужно. */
    def plus(kind: BrewKind, n: Int): Stock =
      if (kind == BrewKind.Schnapps) copy(schnapps = (schnapps + n).min(Goal))
      else if (kind == BrewKind.LivingWater) copy(water = (water + n).min(Goal))
      else this
  }

  object Stock {
    val empty: Stock = Stock(0, 0)

    def fromCounter(counter: Long): Stock =
      Stock(((counter / 100L) % 100L).toInt.min(Goal), (counter % 100L).toInt.min(Goal))
  }

  def stock(q: NpcQuests): Stock = Stock.fromCounter(q.of(NpcQuest.BigDeal).counter)

  def withStock(q: NpcQuests, s: Stock): NpcQuests = q.update(NpcQuest.BigDeal)(_.copy(counter = s.counter))

  def collecting(q: NpcQuests): Boolean = q.onStep(NpcQuest.BigDeal, 1)
  def waiting(q: NpcQuests): Boolean    = q.onStep(NpcQuest.BigDeal, 2)
  def toCaravan(q: NpcQuests): Boolean  = q.onStep(NpcQuest.BigDeal, 3)

  /** Караван «пришёл», когда герой дорос до уровня задания. */
  def arrives(hero: Hero): Boolean = hero.lvl >= Level.toLong

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.BigDeal)(_.copy(step = n))

  /** Задание можно начать: «Ходовой товар» закрыт, «Большого дела» ещё не было. */
  def canBegin(q: NpcQuests): Boolean =
    q.isDone(NpcQuest.HotGoods) && !q.isTaken(NpcQuest.BigDeal) && !q.isDone(NpcQuest.BigDeal)

  /** С карточки адреса — к Барти домой: задание можно начать, товар
    * собирается или ждёт каравана. Пришёл караван — с карточки уже к нему. */
  def houseOpen(q: NpcQuests): Boolean = canBegin(q) || collecting(q) || waiting(q)

  /** Что из сумки Барти возьмёт сейчас: склянки по одной (не из стопок) — не
    * больше, чем ему ещё нужно каждого. */
  def takeable(items: List[Item], s: Stock): List[Item] =
    Goods.flatMap(k => items.filter(_.brew.contains(k)).take((Goal - s.of(k)).max(0)))

  /** Строка карточки адреса — что делать сейчас. «Ходовой товар» не закрыт
    * или «Большое дело» уже закрыто — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.HotGoods) || q.isDone(NpcQuest.BigDeal)) ""
    else if (!q.isTaken(NpcQuest.BigDeal)) content.text("barti.card.ready")
    else content.list("barti.card.steps").lift(q.of(NpcQuest.BigDeal).step - 1).getOrElse("")

  /** Капитан ночной стражи и его люди: он — первым, именной. */
  def guards(captainName: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, CaptainRarity, Some(captainName)) ::
      List.fill(GuardCount)(MonsterGenerator.generateOfRaceAndRarity(Level, GuardRace, GuardRarity))

  /** Амулет от каравана: Та'Нахир передаёт его герою через Барти — при любом
    * исходе засады. Сюжетная экипировка (см. `Item.isStoryGear`) этого уровня, с
    * характеристиками легендарной. */
  val AmuletName: String          = "Каджитский амулет"
  val AmuletLvl: Long             = 15L
  val AmuletRarity: ItemRarity    = ItemRarity.Orange

  def amulet(rng: Rng): Item =
    ItemGenerator.createItemOfType(ItemType.Amulet, AmuletLvl, AmuletRarity, rng)._1.withName(AmuletName).asStoryGear

  /** Каджиты заплатили: адрес уходит, дублоны — в кошель, опыт, задание закрыто,
    * а Барти передаёт амулет от каравана (`amuletKey` — его слова; не влез в
    * сумку — посылкой, см. StoryGearPost). Слова Барти, амулет, строка награды
    * (по ключу `rewardKey`) и «Квест завершён» — готовым текстом. Без
    * хранилища вещей (в старых тестах) амулета нет. Задание не у каравана —
    * ничего не делает. */
  def pay(heroDao: HeroDao, inventoryRepo: InventoryRepository, itemRepo: Option[ItemRepository], parcels: Option[Parcels],
          content: SceneContent, user: User, hero: Hero, doubloons: Long, rewardKey: String,
          amuletKey: String): Task[Option[String]] =
    NpcQuestLog.load(heroDao, user.userId).flatMap { q =>
      if (!toCaravan(q)) ZIO.none
      else for {
        _    <- QuestSupport.removeItems(inventoryRepo, hero, Set(QuestItemKind.BartiAddress))
        _    <- heroDao.updateDoubloons(user.userId, hero.doubloons + doubloons)
        done <- NpcQuestDialog(heroDao, content, NpcQuest.BigDeal, "BigDeal")
                  .complete(user, hero.copy(doubloons = hero.doubloons + doubloons), identity)
        (_, expLine) = done
        gift <- giveAmulet(inventoryRepo, itemRepo, parcels, content, hero)
      } yield Some(gift.fold("")(g => content.text(amuletKey).trim + g + "\n\n") +
        content.format(rewardKey, "doubloons" -> doubloons.toString, "exp" -> expLine) + "\n\n" +
        content.text("barti.caravan.questDone"))
    }

  /** Амулет — в сумку, не влез — посылкой. Ответ — строки о нём: что легло в
    * сумку и куда ушла посылка; без хранилища вещей — ничего. */
  private def giveAmulet(inventoryRepo: InventoryRepository, itemRepo: Option[ItemRepository], parcels: Option[Parcels],
                         content: SceneContent, hero: Hero): Task[Option[String]] =
    itemRepo match {
      case None => ZIO.none
      case Some(items) =>
        for {
          seed  <- Random.nextLong
          now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
          given <- StoryGearPost.give(inventoryRepo, items, parcels, hero, content.text("barti.caravan.amuletFrom"),
                     List(amulet(Rng(seed))), now)
          (bagged, posted) = given
        } yield Some(bagged.map(i => "\n\n" + content.format("barti.caravan.amuletLine", "name" -> i.displayTitle)).mkString +
          posted.fold("")("\n\n" + _))
    }
}
