package pangea.service.state

import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.BattleAlly
import pangea.model.hero.Achievement
import pangea.model.item.{Item, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests, QuestOutcome}
import pangea.model.squad.AllyKind
import pangea.service.state.StoryCollectorQuest.Deal
import pangea.service.state.StoryToolQuest.{Path, Piece}

/** Рубеж II основной линии, «Чем кормить Клеймо» ([[NpcQuest.StoryFeed]], см.
  * [[StoryLine]]) — конец Акта II. Ведёт его купчая на дегтярный склад из
  * главы 10: на третью ночь Сборщик приходит туда за героем.
  *
  * Шаги: 1 — ночь на складе и бой (StoryFeedState): Сборщик, легендарный и
  * именной, и четверо его людей — по силе тоже легендарные, как именные (так
  * решил пользователь); в строю гости пути (как в главе 10) и Сойка
  * с крыши, если её простили в главе 4. Сборщик тянет узор из героя — только
  * из героя (так решил пользователь, см. `PatternPull`). 2 — Сборщик связан, и
  * герой решает, чем кормить Клеймо ([[Brand]]): накормить — забрать
  * украденный узор, Сборщик пуст; усмирить — отдать его живым (у Щита —
  * совету через Юнону, у прочих — главе Братства), своё Клеймо затихает;
  * приручить — запереть в подвале склада, пусть рассказывает. Договор из
  * главы 10 глава Братства помнит.
  *
  * У каждого выбора — своё достижение, своё снаряжение (сюжетная экипировка
  * 50-го уровня с характеристиками легендарной) и строка «как и вы», как у
  * Рубежа I. Купчая остаётся в сумке; глава 11 (Акт III) берётся и висит.
  * Враги, сложность и награда — по уровню задания, а не героя. */
object StoryFeedQuest {

  /** Уровень задания: по нему сложность, враги и награда. */
  val Level: Int = 50

  /** Сюжетный бой на складе: без «Сбежать», награда как за обычный бой. */
  val FightStory: String = "storyFeedWarehouse"

  /** Сборщик — легендарный именной человек, с ним четверо его людей — по
    * силе тоже легендарные, как именные (так решил пользователь). */
  val FoeRace: Race          = Race.Human
  val CollectorRarity: Rarity = Rarity.Legendary
  val HenchRarity: Rarity    = Rarity.Legendary
  val Henchmen: Int          = 4

  /** Сойка в строю — по силе мифическая. */
  val SojkaRarity: Rarity = Rarity.Mythical

  /** Рубеж щедрее главы: столько серебра за уровень задания, как у Рубежа I. */
  val SilverPerLvl: Long = 300L

  def silver: Long = SilverPerLvl * Level

  /** Снаряжение выбора — этого уровня, с характеристиками этой редкости. */
  val GearLvl: Long          = 50L
  val GearRarity: ItemRarity = ItemRarity.Orange

  /** Чем кормить Клеймо — выбор рубежа. Номер — в `counter` закрытого рубежа;
    * `gear` — снаряжение выбора, по вещи. */
  sealed abstract class Brand(val n: Long, val key: String, val outcome: QuestOutcome, val achievement: Achievement,
                              val gear: List[Piece])

  object Brand {
    /** Накормить: Клеймо забирает узор Сборщика — его перчатки героя. */
    case object Feed extends Brand(1L, "feed", QuestOutcome.FeedFed, Achievement.FedBrand,
      List(Piece(ItemType.Gloves, "Перчатки Сборщика")))
    /** Усмирить: Сборщика уводят живым — герою оставляют соляной оберег. */
    case object Calm extends Brand(2L, "calm", QuestOutcome.FeedCalmed, Achievement.CalmBrand,
      List(Piece(ItemType.Amulet, "Соляной оберег")))
    /** Приручить: Сборщик в подвале — наручи он снимает сам. */
    case object Tame extends Brand(3L, "tame", QuestOutcome.FeedTamed, Achievement.TameBrand,
      List(Piece(ItemType.Bracelets, "Наручи Сборщика")))

    val all: List[Brand] = List(Feed, Calm, Tame)
  }

  def toFight(q: NpcQuests): Boolean  = q.onStep(NpcQuest.StoryFeed, 1)
  def toChoice(q: NpcQuests): Boolean = q.onStep(NpcQuest.StoryFeed, 2)

  /** Рубеж идёт: на складе есть что делать. */
  def open(q: NpcQuests): Boolean = toFight(q) || toChoice(q)

  def step(q: NpcQuests, n: Int): NpcQuests = q.update(NpcQuest.StoryFeed)(_.copy(step = n))

  /** Что выбрано на Рубеже II. */
  def choice(q: NpcQuests): Option[Brand] =
    Option.when(q.isDone(NpcQuest.StoryFeed))(q.of(NpcQuest.StoryFeed).counter).flatMap(n => Brand.all.find(_.n == n))

  /** Путь Рубежа I — по нему гости и кому отдают Сборщика. */
  def path(q: NpcQuests): Path = StoryCollectorQuest.path(q)

  /** Договор с главой Братства из главы 10. */
  def deal(q: NpcQuests): Option[Deal] = StoryCollectorQuest.deal(q)

  /** Сойка на крыше — если её простили в главе 4. */
  def sojkaOnRoof(q: NpcQuests): Boolean = StoryCollectorQuest.sojkaForgiven(q)

  /** Сборщик — первым, именной, — и четверо его людей. */
  def foes(collector: String): List[Monster] =
    MonsterGenerator.generateOfRaceAndRarity(Level, FoeRace, CollectorRarity, Some(collector)) ::
      List.fill(Henchmen)(MonsterGenerator.generateOfRaceAndRarity(Level, FoeRace, HenchRarity))

  /** Гости пути — те же, что в главе 10, уровнем рубежа, — и Сойка, если она на
    * крыше. */
  def guests(content: SceneContent, q: NpcQuests): List[BattleAlly] =
    StoryCollectorQuest.guests(content, q, Level) ++
      Option.when(sojkaOnRoof(q))(
        StoryMorningQuest.guest(AllyKind.Sojka, content.text("story.storyFeed.sojkaName"), SojkaRarity, Level)).toList

  /** Снаряжение выбора: каждая вещь — своего вида, уровня [[GearLvl]], с
    * характеристиками редкости [[GearRarity]] и своим именем; сюжетная ★. */
  def gear(brand: Brand, rng: Rng): List[Item] =
    brand.gear.foldLeft((List.empty[Item], rng)) { case ((acc, r), piece) =>
      val (item, next) = ItemGenerator.createItemOfType(piece.itemType, GearLvl, GearRarity, r)
      (acc :+ item.withName(piece.name).asStoryGear, next)
    }._1

  /** Строка карточки купчей после главы 10: что делать на рубеже — или, когда
    * он пройден, глава 11. Глава 10 не закрыта — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (!q.isDone(NpcQuest.StoryCollector)) ""
    else if (q.isDone(NpcQuest.StoryFeed)) StoryScarsQuest.cardLine(content, q)
    else content.list("story.storyFeed.card.steps").lift(q.of(NpcQuest.StoryFeed).step - 1).getOrElse("")
}
