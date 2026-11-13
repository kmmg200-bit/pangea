package pangea.service.equipment

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.hero.{Achievement, Hero, WeaponCoat}
import pangea.model.item.{BrewEffect, BrewRates, Item}
import pangea.model.monster.Race
import pangea.model.stats.{ParamsBuff, StatBoost}
import pangea.model.trauma.Trauma
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.AzatData
import pangea.service.state.states.InventoryState
import pangea.service.state.states.InventoryState.BrewFlask
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Отвар из сумки без экрана. Одно на бота — карточку отвара — и на Mini App:
  * правила у них одни. Всё меняет героя, поэтому зовётся под локом игрока: в
  * боте его держит ход, в приложении — сам запрос. Что сказать игроку, идёт в
  * `renderer`. `false` у действий — этим отваром так нельзя, склянка цела. */
final class BrewWork(heroDao: HeroDao, inventoryRepo: InventoryRepository, content: SceneContent) {

  private def now: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  /** Склянка уходит, игроку — итог. */
  private def spend(user: User, item: Item, hero: Hero, line: String, renderer: Renderer): Task[Boolean] =
    inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
      renderer.show(user, Screen(line, Nil)).as(true)

  /** Глоток отвара, который ни о чём не спрашивает: травму и род называют
    * сперва ([[cure]], [[sentence]]). */
  def drink(user: User, hero: Hero, item: Item, renderer: Renderer): Task[Boolean] =
    item.brew.map(_.effect) match {
      case Some(BrewEffect.InstantRest) =>
        for {
          at   <- now
          azat <- AzatData.load(heroDao, user.userId, at)
          _    <- heroDao.writeAzatData(user.userId, azat.copy(instantRests = azat.instantRests + BrewRates.InstantRests).asJson)
          res  <- spend(user, item, hero, content.format("brew.rests",
                    "count" -> BrewRates.InstantRests.toString,
                    "total" -> (azat.instantRests + BrewRates.InstantRests).toString), renderer)
        } yield res
      // Пузырьковый нектар: плёнка ждёт своего часа и срока не имеет.
      case Some(BrewEffect.Bubble) =>
        heroDao.updateStatBoosts(user.userId, hero.statBoosts.add(
          StatBoost(BrewRates.BubbleBoost, ParamsBuff.zero, Long.MaxValue), 0L)) *>
          spend(user, item, hero, content.text("brew.bubble"), renderer)
      // Зеркальный настой: копии ждут ближайшего боя там же, где пыль и
      // смазка, — и сходят вместе с ними, когда бой кончится.
      case Some(BrewEffect.Mirror(copies)) =>
        heroDao.updateWeaponDust(user.userId, hero.weaponDust.copy(mirrors = copies)) *>
          spend(user, item, hero, content.format("brew.mirrors", "count" -> copies.toString), renderer)
      // Волчий зов: запах держится час, но сгорает на первой же встрече.
      case Some(BrewEffect.WolfCall) =>
        for {
          at  <- now
          _   <- heroDao.updateStatBoosts(user.userId, hero.statBoosts.add(
                   StatBoost(BrewRates.WolfCallBoost, ParamsBuff.zero, at + Achievement.brewMs(hero, BrewRates.WolfCallMs)), at))
          res <- spend(user, item, hero, content.text("brew.wolfCall"), renderer)
        } yield res
      // Гнилой зов — узелок: так же час, только смрад ведёт к Гнилому Джо.
      case Some(BrewEffect.RotCall) =>
        for {
          at  <- now
          _   <- heroDao.updateStatBoosts(user.userId, hero.statBoosts.add(
                   StatBoost(BrewRates.RotCallBoost, ParamsBuff.zero, at + Achievement.brewMs(hero, BrewRates.RotCallMs)), at))
          res <- spend(user, item, hero, content.text("brew.rotCall"), renderer)
        } yield res
      case Some(BrewEffect.Boost(name, buff, label)) =>
        for {
          at  <- now
          _   <- heroDao.updateStatBoosts(user.userId,
                   hero.statBoosts.add(StatBoost(name, buff, at + Achievement.brewMs(hero, BrewRates.BoostDurationMs)), at))
          res <- spend(user, item, hero, content.format("brew.boosted",
                   "brew" -> item.name, "pct" -> BrewRates.BoostPct.toString, "stat" -> label), renderer)
        } yield res
      case _ => ZIO.succeed(false)
    }

  /** Род назван: имя ложится в баф, склянка пустеет. */
  def sentence(user: User, hero: Hero, item: Item, race: Race, renderer: Renderer): Task[Boolean] =
    if (!item.brew.map(_.effect).contains(BrewEffect.Sentence)) ZIO.succeed(false)
    else
      for {
        at  <- now
        _   <- heroDao.updateStatBoosts(user.userId, hero.statBoosts.add(
                 StatBoost(BrewRates.SentenceBoost + race.entryName, ParamsBuff.zero,
                   at + Achievement.brewMs(hero, BrewRates.SentenceMs)), at))
        res <- spend(user, item, hero, content.format("brew.sentenced", "race" -> race.toString), renderer)
      } yield res

  /** Лечить травму по имени: только лёгкую или среднюю и только ту, что есть. */
  def cure(user: User, hero: Hero, item: Item, trauma: String, renderer: Renderer): Task[Boolean] =
    if (!item.brew.map(_.effect).contains(BrewEffect.CureTrauma)) ZIO.succeed(false)
    else
      now.flatMap { at =>
        Trauma.byName(trauma).filter(BrewWork.curable(hero, at).contains) match {
          case None => ZIO.succeed(false)
          case Some(t) =>
            val names = BrewWork.removeFirst(hero.traumaNames, t.name)
            val until = if (names.isEmpty) None else hero.traumaUntil
            heroDao.updateTrauma(user.userId, until, names) *>
              spend(user, item, hero, content.format("brew.cured", "trauma" -> t.name), renderer)
        }
      }

  /** Заправить флягу из ячейки `slot`, если она всё ещё своего толка и неполная. */
  def refill(user: User, hero: Hero, item: Item, slot: Int, renderer: Renderer): Task[Boolean] =
    BrewWork.openFlasks(hero, item).find(_.slot == slot) match {
      case None => ZIO.succeed(false)
      case Some(f) =>
        heroDao.updateEquipment(user.userId, f.refilled) *>
          spend(user, item, hero, content.format("brew.refilled", "flask" -> f.name), renderer)
    }

  /** Смазать оружие: на ближайший бой удары по HP травят или пускают кровь.
    * Новая смазка сменяет прежнюю; без оружия мазать нечего — сказать, склянка цела. */
  def coat(user: User, hero: Hero, item: Item, renderer: Renderer): Task[Boolean] =
    item.brew.map(_.effect) match {
      case Some(BrewEffect.Coat(_, _)) if hero.equipment.unarmed =>
        renderer.show(user, Screen(content.text("brew.noWeapon"), Nil)).as(false)
      case Some(BrewEffect.Coat(coat, _)) =>
        val key = coat match {
          case WeaponCoat.Poison   => "brew.coatedPoison"
          case WeaponCoat.Bleed    => "brew.coatedBleed"
          case WeaponCoat.RedVenom => "brew.coatedRedVenom"
        }
        heroDao.updateWeaponDust(user.userId, hero.weaponDust.copy(coat = Some(coat))) *>
          spend(user, item, hero, content.format(key, "weapon" -> hero.equipment.mainWeapon.name), renderer)
      case _ => ZIO.succeed(false)
    }
}

object BrewWork {

  /** Какие травмы отвар берёт: лёгкие и средние, без повторов. */
  def curable(hero: Hero, now: Long): List[Trauma] =
    hero.activeTraumas(now).filter(_.severity != Trauma.Heavy).distinct

  /** Тяжёлые — их отвар только называет. */
  def heavy(hero: Hero, now: Long): List[Trauma] =
    hero.activeTraumas(now).filter(_.severity == Trauma.Heavy)

  /** Фляги на поясе, которые этот отвар вообще заправляет. */
  def fittingFlasks(hero: Hero, item: Item): List[BrewFlask] =
    item.brew.toList.flatMap(kind => InventoryState.brewFlasks(hero).filter(f => kind.refills.contains(f.source)))

  /** Те из них, в которые ещё есть куда лить. */
  def openFlasks(hero: Hero, item: Item): List[BrewFlask] =
    fittingFlasks(hero, item).filter(f => f.charges < f.maxCharges)

  private def removeFirst(names: List[String], name: String): List[String] = {
    val i = names.indexOf(name)
    if (i < 0) names else names.patch(i, Nil, 1)
  }
}
