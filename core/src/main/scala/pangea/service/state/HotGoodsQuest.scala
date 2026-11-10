package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.Hero
import pangea.model.item.{BrewKind, Item}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.user.UserId
import pangea.repository.inventory.InventoryRepository
import zio.{Random, Task, UIO, ZIO}

/** «Ходовой товар» ([[NpcQuest.HotGoods]]) — начало побочной линии Барти,
  * дурачка-осведомителя: герой знает его по прошлой жизни (ходили вместе на
  * дела), а Барти героя с новым лицом не узнаёт. Он не из Братства; в Братстве
  * про него говорили «уберём, когда захотим».
  *
  * С [[MinHeroLvl]]-го уровня героя осмотр уровня лабиринта — на любом повороте
  * — с шансом [[Chance]] выводит к Барти: его зажали у изгороди звери (бой уровня
  * задания). Отбил — Барти рассказывает, что в Кинэте любой грамотный и
  * способный человек раскрутится, даёт рецепты живой воды и шнапса из красавки
  * («ходовой товар», [[Recipes]]) и листок с «деловым планом», и требует завести
  * свой куб Азата. «Пройти мимо» или смерть задание не начинают — Барти
  * встретится снова.
  *
  * Шаги — по тому, что у героя есть ([[stepFor]]): 1 — своего куба нет; 2 —
  * куб есть, а в сумке нет склянки живой воды и бутылки шнапса; 3 — всё есть,
  * и на ближайшем осмотре лабиринта Барти сам находит героя: адрес его дома
  * на Торговой площади (зацепка «Большого дела») и должок за шалфей. */
object HotGoodsQuest {

  /** Уровень задания: по нему сложность, звери и награда. */
  val Level: Int = 3

  /** С какого уровня героя Барти может встретиться. */
  val MinHeroLvl: Long = 4L

  /** Шанс встречи на осмотр уровня — своим броском, после редких заданий. */
  val Chance: Double = 0.03

  /** Ключ сюжетного боя у изгороди: награда как за обычный бой, без «Сбежать». */
  val FightStory: String = "bartiHedge"

  /** Звери, что зажали Барти: двое, уровня задания. */
  val BeastCount: Int     = 2
  val BeastRace: Race     = Race.Animal
  val BeastRarity: Rarity = Rarity.Uncommon

  /** Должок за шалфей — столько серебра за уровень задания. */
  val SilverPerLvl: Long = 150L

  /** Ходовой товар: что Барти велит варить. */
  val Goods: List[BrewKind] = List(BrewKind.LivingWater, BrewKind.Schnapps)

  /** Рецепты, что Барти объясняет, — ключи в [[NpcQuests.recipes]]. */
  val Recipes: List[String] = Goods.map(k => s"brew:${k.entryName}")

  /** Задания ещё не было — Барти может встретиться. */
  def fresh(q: NpcQuests): Boolean = !q.isTaken(NpcQuest.HotGoods) && !q.isDone(NpcQuest.HotGoods)

  /** Бросок осмотра: дробный, как у редких заданий, — целые броски пула он не
    * сдвигает. Встретиться не может — не бросает вовсе. */
  def roll(q: NpcQuests, heroLvl: Long): UIO[Boolean] =
    if (heroLvl < MinHeroLvl || !fresh(q)) ZIO.succeed(false)
    else Random.nextDouble.map(_ < Chance)

  /** Есть ли в сумке по склянке каждого товара. Вещи в стопке для заданий не
    * считаются (см. Bundling). */
  def hasGoods(items: List[Item]): Boolean = Goods.forall(k => items.exists(_.brew.contains(k)))

  /** Шаг по тому, что у героя есть. */
  def stepFor(hasCube: Boolean, items: List[Item]): Int =
    if (!hasCube) 1 else if (!hasGoods(items)) 2 else 3

  /** Шаг взятого задания — заново по кубу и сумке, записанный, если сменился.
    * Задание не взято — как есть. */
  def refresh(heroDao: HeroDao, inventoryRepo: InventoryRepository, userId: UserId, hero: Hero): Task[NpcQuests] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (!q.isTaken(NpcQuest.HotGoods)) ZIO.succeed(q)
      else for {
        azat <- AzatData.read(heroDao, userId)
        inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
        step  = stepFor(azat.hasCube, inv.items.data)
        next  = q.update(NpcQuest.HotGoods)(_.copy(step = step))
        _    <- ZIO.when(step != q.of(NpcQuest.HotGoods).step)(NpcQuestLog.save(heroDao, userId, next))
      } yield next
    }

  /** Куб есть и товар в сумке: Барти пора найти героя. */
  def ready(q: NpcQuests): Boolean = q.onStep(NpcQuest.HotGoods, 3)

  /** Строка карточки плана — что делать сейчас. Задание не в работе — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isTaken(NpcQuest.HotGoods)) ""
    else content.list("barti.plan.steps").lift(q.of(NpcQuest.HotGoods).step - 1).getOrElse("")

  /** Звери у изгороди. */
  def beasts: List[Monster] =
    List.fill(BeastCount)(MonsterGenerator.generateOfRaceAndRarity(Level, BeastRace, BeastRarity))

  def silver: Long = SilverPerLvl * Level
}
