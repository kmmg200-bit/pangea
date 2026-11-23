package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.generator.item.GemGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.{Hero, HeroId}
import pangea.model.item.{Gem, GemKind, Item, QuestItemKind}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.user.UserId
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** «Бродячий ларец» — всё, что нужно сюжету в разных сценах: начать задание при
  * встрече в лабиринте, выбрать и скормить камень, узнать, на каком шаге
  * задание, собрать хозяина ларца для боя, вернуть съеденные камни. Сам ход —
  * [[NpcQuest.StrayCasket]] в `npc_quests`; сцены — встреча и бой в лабиринте
  * ([[pangea.service.state.states.straycasket.StrayCasketState]]), карточка
  * ларца в сумке (кормление) и Лавка Фета (развязка). */
object StrayCasketQuest {

  /** Сложность задания — 7 ур. По ней же и уровень хозяина ларца. */
  val Level: Int = 7

  /** Сколько камней ларец съест, пока не наестся. */
  val GemsGoal: Long = 10L

  /** Через сколько камней ларец показывает следующее воспоминание. */
  val MemoryEvery: Long = 2L

  /** Ключ сюжетного боя с хозяином ларца (см. `SoloPveBattle.story`). Как и
    * коллектор, бой награды по таблицам не даёт: развязка — у Фета. */
  val FightStory: String = "strayCasket"

  /** Хозяин ларца: искатель-человек, поднятый уже мёртвым, — именной. */
  val OwnerName: String   = "Ансельм Вейл"
  val OwnerRace: Race     = Race.Human
  val OwnerRarity: Rarity = Rarity.Legendary

  // ── Ход задания ─────────────────────────────────────────────────────────────

  /** Задания у героя ещё не было: ларец может встретиться. */
  def fresh(q: NpcQuests): Boolean = !q.isTaken(NpcQuest.StrayCasket) && !q.isDone(NpcQuest.StrayCasket)

  /** Ларец увязался за героем и голоден. */
  def hungry(q: NpcQuests): Boolean = q.onStep(NpcQuest.StrayCasket, 1)

  /** Сыт и помнит дорогу: отпущенный с карточки в лабиринте, ведёт к хозяину. */
  def leads(q: NpcQuests): Boolean = q.onStep(NpcQuest.StrayCasket, 2)

  /** Хозяин упокоен — ларец нести Фету. */
  def toFet(q: NpcQuests): Boolean = q.onStep(NpcQuest.StrayCasket, 3)

  /** Сколько камней ларец уже съел. */
  def fed(q: NpcQuests): Long = q.of(NpcQuest.StrayCasket).counter

  /** Сколько ещё до сытости. */
  def left(q: NpcQuests): Long = (GemsGoal - fed(q)).max(0L)

  /** Ещё один камень в ларце: записан в съеденные, счётчик на единицу больше;
    * десятый — ларец сыт, задание на втором шаге. Не голоден — ничего. */
  def fedOne(q: NpcQuests, gem: Gem): NpcQuests =
    if (!hungry(q)) q
    else {
      val n = fed(q) + 1L
      q.copy(strayGems = q.strayGems :+ gem)
        .update(NpcQuest.StrayCasket)(_.copy(counter = n, step = if (n >= GemsGoal) 2 else 1))
    }

  /** Какое воспоминание показать после `n`-го камня: каждое второе — следующее
    * по счёту, с первого. */
  def memoryAfter(n: Long): Option[Int] =
    Option.when(n > 0L && n % MemoryEvery == 0L)((n / MemoryEvery).toInt)

  /** Что сказать на карточке ларца — по шагу задания. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    if (hungry(q)) content.format("strayCasket.card.hungry", "fed" -> fed(q).toString, "goal" -> GemsGoal.toString)
    else if (leads(q)) content.text("strayCasket.card.leads")
    else if (toFet(q)) content.text("strayCasket.card.toFet")
    else ""

  // ── Камни ───────────────────────────────────────────────────────────────────

  /** Камни при герое — те, что ларец может съесть: из сумки и из своего ларца. */
  def gems(goods: List[QuestGoods.Good]): List[QuestGoods.Good] = goods.filter(_.item.gem.isDefined)

  /** Какие качества есть и сколько камней каждого — для кнопок кормления. */
  def grades(goods: List[QuestGoods.Good]): List[(Int, Int)] =
    gems(goods).flatMap(_.item.gem).groupBy(_.grade).toList.map { case (g, gs) => g -> gs.size }.sortBy(_._1)

  /** Какой камень этого качества ларец возьмёт: того вида, которого у героя
    * больше всего (при равенстве — по порядку видов), и сперва из сумки. Так
    * при герое остаётся камней разных видов побольше. */
  def pick(goods: List[QuestGoods.Good], grade: Int): Option[QuestGoods.Good] = {
    val ofGrade = gems(goods).filter(_.item.gem.exists(_.grade == grade))
    val byKind  = ofGrade.groupBy(_.item.gem.map(_.kind))
    byKind.toList
      .sortBy { case (kind, gs) => (-gs.size, kind.map(GemKind.values.indexOf(_)).getOrElse(Int.MaxValue)) }
      .headOption
      .flatMap { case (_, gs) => gs.find(_.in.isEmpty).orElse(gs.headOption) }
  }

  /** Камень, который ларец выхватывает при встрече: самого простого качества. */
  def cheapest(goods: List[QuestGoods.Good]): Option[QuestGoods.Good] =
    grades(goods).headOption.flatMap { case (g, _) => pick(goods, g) }

  /** Скормить камень: он уходит оттуда, где лежал, и записывается в
    * съеденные. Ларец не голоден (старая кнопка) — камень остаётся при герое.
    * Возвращает задания после — или None, если кормить было не время. */
  def feed(heroDao: HeroDao, inventory: InventoryRepository, artifacts: Option[ArtifactRepository],
           userId: UserId, heroId: HeroId, good: QuestGoods.Good): Task[Option[NpcQuests]] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      good.item.gem match {
        case Some(gem) if hungry(q) =>
          val next = fedOne(q, gem)
          QuestGoods.remove(inventory, artifacts, heroId, List(good)) *>
            NpcQuestLog.save(heroDao, userId, next).as(Some(next))
        case _ => ZIO.none
      }
    }

  /** Съеденные камни заново — как вещи, чтобы положить их герою. */
  def returned(q: NpcQuests): List[Item] = q.strayGems.map(g => GemGenerator.item(g.kind, g.grade))

  // ── Встреча и бой ───────────────────────────────────────────────────────────

  /** Встреча: ларец — в сумку, задание на первый шаг. Уже было — ничего. */
  def start(heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
            userId: UserId, hero: Hero, nowMs: Long): Task[Boolean] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (!fresh(q)) ZIO.succeed(false)
      else QuestSupport.giveItem(inventory, items, hero, QuestItemKind.StrayCasket) *>
        NpcQuestLog.save(heroDao, userId, q.take(NpcQuest.StrayCasket, nowMs).copy(strayGems = Nil)).as(true)
    }

  /** Хозяин ларца: именной искатель-человек уровня задания, поднятый мёртвым. */
  def owner: Monster =
    MonsterGenerator.generateOfRaceAndRarity(Level, OwnerRace, OwnerRarity, Some(OwnerName)).copy(undead = true)

  /** Хозяин упокоен: ларец свободен, нести его Фету. */
  def laidToRest(heroDao: HeroDao, userId: UserId): Task[Unit] =
    NpcQuestLog.modify(heroDao, userId)(q => if (leads(q)) q.update(NpcQuest.StrayCasket)(_.copy(step = 3)) else q).unit
}
