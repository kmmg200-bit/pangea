package pangea.service.state.states.home

import pangea.engine.SceneContent
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.HeroId
import pangea.model.home.HomeData
import pangea.model.item.Item
import pangea.model.rune.Rune
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import zio.{Task, ZIO}

/** Как дом забирает добро у героя: травы и отвары — на полки верстака (из
  * сумки, а следом из Живой сумки), камни — в хранилище (из сумки, а следом из
  * Ларца), рунные камни — на алтарь (из сумки). Общее для кнопок каждого
  * хранилища и для «Разложить всё» на главном экране дома.
  *
  * Дом здесь не пишется: взятое вызывающий кладёт в `HomeData` и сохраняет
  * сам — «Разложить всё» так сохраняет три хранилища одной записью. */
final case class HomeStow(
  inventoryRepo: InventoryRepository,
  artifacts:     Option[ArtifactRepository],
  content:       SceneContent
) {

  /** Всё, что `accepts`, — из сумки (кроме сюжетного), а следом из артефакта
    * `from`, пока хватает `room` мест. Возвращает взятое и сколько не влезло.
    * Артефакт отдаёт своё одной записью: остаток как есть, заряды не тратятся. */
  def gather(heroId: HeroId, accepts: Item => Boolean, from: ArtifactKind, room: Int): Task[(List[Item], Int)] =
    for {
      inv     <- inventoryRepo.get(heroId).mapError(asThrowable)
      fromBag  = inv.items.data.filter(i => accepts(i) && !i.isQuestItem)
      inside  <- artifacts.fold(ZIO.succeed(List.empty[Item]))(a =>
                   a.get(heroId).fold(_ => Nil, _.of(from).items.data))
      fromArt  = inside.filter(accepts)
      all      = fromBag ++ fromArt
      taken    = all.take(room.max(0))
      takenIds = taken.map(_.id).toSet
      _ <- ZIO.when(fromBag.exists(i => takenIds.contains(i.id)))(
             inventoryRepo.removeItems(fromBag.map(_.id).filter(takenIds.contains).toSet, heroId).mapError(asThrowable))
      _ <- ZIO.foreachDiscard(artifacts.filter(_ => fromArt.exists(i => takenIds.contains(i.id))))(a =>
             a.applyMagic(heroId, from, inside.filterNot(i => takenIds.contains(i.id)), 0).mapError(asThrowable))
    } yield (taken, all.size - taken.size)

  /** Все рунные камни из сумки: убирает их и возвращает, сколько очков
    * прибавилось каждой руне (ключ руны → очки). Места на алтаре не считают. */
  def runeStones(heroId: HeroId): Task[Map[String, Long]] =
    for {
      inv   <- inventoryRepo.get(heroId).mapError(asThrowable)
      stones = inv.items.data.flatMap(i => i.runeStone.map(rs => (i, rs)))
      _     <- ZIO.when(stones.nonEmpty)(
                 inventoryRepo.removeItems(stones.map(_._1.id).toSet, heroId).mapError(asThrowable))
    } yield stones.groupMapReduce(_._2.runeKey)(_._2.size.points)(_ + _)

  /** «На полки легло трав: N» и, если не всё влезло, — сколько осталось. */
  def herbsText(taken: Int, left: Int): String =
    content.format("home.workbench.herbsPut", "n" -> taken.toString) +
      (if (left > 0) "\n" + content.format("home.workbench.herbsLeft", "n" -> left.toString) else "")

  /** «В хранилище легло камней: N» и, если не всё влезло, — сколько осталось. */
  def gemsText(taken: Int, left: Int): String =
    content.format("home.gems.put", "n" -> taken.toString) +
      (if (left > 0) "\n" + content.format("home.gems.putLeft", "n" -> left.toString) else "")

  /** «Алтарь вбирает узоры» — по строке на руну, по алфавиту. */
  def runesText(added: Map[String, Long]): String =
    content.format("home.altar.put", "lines" -> HomeStow.runeLines(content, added).mkString("\n"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object HomeStow {

  def isHerb(it: Item): Boolean = it.material.exists(_.isHerb)

  def isGem(it: Item): Boolean = it.gem.isDefined

  /** Очки прибавились рунам на алтаре. */
  def addRunes(home: HomeData, added: Map[String, Long]): HomeData =
    added.foldLeft(home) { case (h, (k, n)) => h.withRune(k, h.runeCount(k) + n) }

  /** «• Руна: +N» по алфавиту — для любого прибавления на алтарь. */
  def runeLines(content: SceneContent, added: Map[String, Long]): List[String] =
    added.toList.flatMap { case (k, n) => Rune.byKey(k).map(r => (r.label, n)) }.sortBy(_._1)
      .map { case (name, n) => content.format("home.altar.putLine", "name" -> name, "n" -> n.toString) }
}
