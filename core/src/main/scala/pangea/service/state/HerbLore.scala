package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.{Hero, Knowledge, LoreData}
import pangea.generator.item.MaterialGenerator
import pangea.model.item.{Item, MaterialKind, QuestItemKind}
import pangea.model.user.{User, UserId}
import pangea.repository.inventory.InventoryRepository
import zio.{Random, Task, ZIO}

/** Травы и знания о них — общее для поляны, Густаво и инвентаря: что герой
  * узнаёт в сорванном цветке, почём Густаво берёт травы, как читается трактат,
  * как опознаётся цветок из сумки. */
object HerbLore {

  /** Шанс редкой травы на поляне, в процентах; остальное — простые. */
  val RareHerbPct: Int = 2

  /** Цена травы у Густаво: множитель травы × это. Неопознанный цветок — по цене сена. */
  val HerbPriceUnit: Long    = 10L
  val StrangeFlowerPrice: Long = 5L

  /** Трактаты: цена у Густаво и знание, которое они дают. */
  val Treatise1Price: Long = 15000L
  val Treatise2Price: Long = 30000L

  /** Догадаться самому при странном цветке: о простых травах — интеллект ÷ 4
    * процентов, о редких — вдвое реже, интеллект ÷ 8. По книге — интеллект ÷ 2, и
    * каждая неудача прибавляет к следующему броску ещё 2 %. Не осилил — книгу
    * можно открыть снова через час. */
  val InsightIntDivisor: Int   = 4
  val InsightRareIntDivisor: Int = 8
  val ReadingIntDivisor: Int   = 2
  val ReadingFailureBonusPct: Int = 2
  val ReadingCooldownMs: Long  = 60L * 60L * 1000L

  def knowledgeOf(book: QuestItemKind): Option[Knowledge] = book match {
    case QuestItemKind.FlowerTreatise1 => Some(Knowledge.FlowersRank1)
    case QuestItemKind.FlowerTreatise2 => Some(Knowledge.FlowersRank2)
    case _                             => None
  }

  def isTreatise(kind: QuestItemKind): Boolean = knowledgeOf(kind).isDefined

  /** Что герой видит в сорванной траве: её саму, если знает ранг, иначе —
    * простую как странный цветок, редкую — как необычный. */
  def recognised(lore: LoreData, herb: MaterialKind): MaterialKind =
    if (Knowledge.forHerbRank(herb.herbRank).forall(lore.knows)) herb else MaterialKind.unknownFlower(herb.herbRank)

  def price(kind: MaterialKind): Long =
    if (MaterialKind.isUnknownFlower(kind)) StrangeFlowerPrice else kind.herbModifier * HerbPriceUnit

  /** Травы из вещей — каждая вместе со своим видом: цена и прочее считаются по
    * виду, и доставать его из вещи второй раз не нужно. */
  def herbs(items: List[Item]): List[(Item, MaterialKind)] =
    items.flatMap(i => i.material.filter(_.isHerb).map(i -> _))

  def readLore(heroDao: HeroDao, userId: UserId): Task[LoreData] =
    heroDao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))

  def writeLore(heroDao: HeroDao, userId: UserId, lore: LoreData): Task[Unit] =
    heroDao.writeLoreData(userId, lore.asJson)

  /** Трактаты о том, что герой уже знает, ему больше не нужны — каким бы путём
    * знание ни пришло. Такие книги уходят из сумки, а их счёт неудач и срок
    * «переварить» стираются; итоговые знания пишутся, если что-то изменилось.
    * Возвращает записанные знания и выброшенные книги. */
  def settleBooks(
      heroDao: HeroDao,
      inventoryRepo: InventoryRepository,
      userId: UserId,
      hero: Hero,
      lore: LoreData
  ): Task[(LoreData, List[Item])] =
    for {
      inv     <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      obsolete = inv.items.data.filter(i => i.questItem.flatMap(knowledgeOf).exists(lore.knows))
      _       <- ZIO.foreachDiscard(obsolete)(i => inventoryRepo.removeItem(i.id, hero.id).mapError(e => new Throwable(e.toString)))
      // Книга в сумке — значит, куплена: Густаво это помнит и после того, как она ушла.
      settled  = obsolete.flatMap(_.questItem).foldLeft(lore)((l, k) => l.bookMastered(k.entryName).bookBought(k.entryName))
      _       <- ZIO.when(settled != lore)(writeLore(heroDao, userId, settled))
    } yield (settled, obsolete)

  /** О чём можно догадаться по неопознанному цветку: по странному — о простых
    * травах, по необычному — о редких. Что уже знаешь, о том не догадываются. */
  def insightTarget(lore: LoreData, kind: MaterialKind): Option[Knowledge] =
    MaterialKind.unknownFlowerRank(kind).flatMap(Knowledge.forHerbRank).filterNot(lore.knows)

  /** Порог броска (1..100) на догадку по странному цветку: о простых травах —
    * интеллект ÷ 4, о редких — интеллект ÷ 8. */
  def insightChance(hero: Hero, nowMs: Long, knowledge: Knowledge = Knowledge.FlowersRank1): Long =
    hero.effectiveBaseStats(nowMs).int /
      (if (knowledge == Knowledge.FlowersRank2) InsightRareIntDivisor else InsightIntDivisor)

  /** Догадка по сорванному цветку — на поляне и в пещере одна и та же. Только
    * неопознанный цветок и только когда есть о чём догадываться (иначе броска нет
    * и случайное число не тратится). Удача — знание «сам дошёл», купленный трактат
    * о нём тихо уходит из сумки, игроку — строка. Возвращает знания после. */
  def insight(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent, renderer: Renderer,
              user: User, hero: Hero, lore: LoreData, kind: MaterialKind, nowMs: Long): Task[LoreData] =
    insightTarget(lore, kind) match {
      case None => ZIO.succeed(lore)
      case Some(knowledge) =>
        guess(heroDao, inventoryRepo, content, user, hero, lore, knowledge, nowMs).flatMap {
          case (after, None)       => ZIO.succeed(after)
          case (after, Some(line)) => renderer.show(user, Screen(line, Nil)).as(after)
        }
    }

  /** Бросок на догадку: удача — знание «сам дошёл», трактат о нём уходит из
    * сумки, и строка для игрока. Возвращает знания после и строку, если вышло. */
  private def guess(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent, user: User,
                    hero: Hero, lore: LoreData, knowledge: Knowledge, nowMs: Long): Task[(LoreData, Option[String])] =
    Random.nextIntBetween(1, 101).flatMap { roll =>
      if (roll > insightChance(hero, nowMs, knowledge)) ZIO.succeed((lore, None))
      else {
        val learned = lore.learn(knowledge, alone = true)
        for {
          _       <- writeLore(heroDao, user.userId, learned)
          // Купленный трактат об этом уходит из сумки тихо — герою о нём не напоминаем.
          settled <- settleBooks(heroDao, inventoryRepo, user.userId, hero, learned)
        } yield (settled._1, Some(
          content.text(if (knowledge == Knowledge.FlowersRank2) "knowledge.insightRare" else "knowledge.insight") +
            "\n" + content.format("knowledge.gained", "title" -> knowledge.title)))
      }
    }

  /** Что вышло из опознания цветка: строки для игрока и цветок, ставший травой
    * (None — рассыпался). */
  final case class Identified(lines: List[String], herb: Option[Item])

  /** Опознать неопознанный цветок из сумки — кнопка на его карточке. Ранг его
    * травы герою знаком — цветок становится случайной травой этого ранга. Не
    * знаком — цветок рассыпается в руках, а бросок на догадку идёт тот же, что
    * при сборе: по странному — о простых травах, по необычному — о редких. */
  def identify(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent, user: User,
               hero: Hero, item: Item, kind: MaterialKind, nowMs: Long): Task[Identified] =
    MaterialKind.unknownFlowerRank(kind) match {
      case None => ZIO.succeed(Identified(Nil, Some(item)))
      case Some(rank) =>
        readLore(heroDao, user.userId).flatMap { lore =>
          if (Knowledge.forHerbRank(rank).forall(lore.knows)) {
            val pool = MaterialKind.herbsOfRank(rank)
            for {
              idx  <- Random.nextIntBounded(pool.size)
              herb  = MaterialGenerator.item(pool(idx)).copy(id = item.id)
              _    <- inventoryRepo.updateItem(hero.id, herb).mapError(e => new Throwable(e.toString))
            } yield Identified(List(content.format("knowledge.identified", "name" -> herb.name)), Some(herb))
          } else
            for {
              _      <- inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString))
              result <- insightTarget(lore, kind).fold[Task[(LoreData, Option[String])]](ZIO.succeed((lore, None)))(k =>
                          guess(heroDao, inventoryRepo, content, user, hero, lore, k, nowMs))
            } yield Identified(content.text("knowledge.identifyCrumbled") ::
              result._2.fold(List(content.text("knowledge.identifyFailed")))(List(_)), None)
        }
    }

  /** Порог броска (1..100) на освоение трактата: интеллект ÷ 2 плюс 2 % за каждую
    * прошлую неудачу с этой книгой. */
  def readingChance(hero: Hero, nowMs: Long, failures: Int): Long =
    hero.effectiveBaseStats(nowMs).int / ReadingIntDivisor + failures.toLong * ReadingFailureBonusPct
}
