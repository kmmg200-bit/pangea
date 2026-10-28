package pangea.service.promo

import pangea.dao.hero.HeroDao
import pangea.dao.promo.PromoDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.model.promo.{PromoCode, PromoReward}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.artifact.{ArtifactIntake, Intake}
import pangea.service.state.{AzatData, UserAction}
import zio.{Task, ZIO, ZLayer}

import java.util.concurrent.TimeUnit

/** Промокоды. Код пишут боту в личку голым сообщением с любого экрана — как
  * /home: ответ приходит письмом, а сцена и её кнопки остаются как были.
  * Каждый игрок вводит код один раз и только пока код не истёк.
  *
  * Награда — целиком или никак. Вещам, которым нужно место, сперва ищем его:
  * отвары ловит Живая сумка, пока в ней есть место, остальное ложится в сумку.
  * Не влезает — отказ, и код не сгорает: освободил место — ввёл снова. На почту
  * награда не уходит. */
final case class PromoCodes(
  dao:           PromoDao,
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  artifacts:     Option[ArtifactRepository],
  content:       SceneContent
) {
  import PromoCodes._

  /** Взять сообщение себе, если в нём промокод. `false` — это не код, пусть
    * сообщение разбирает игровой автомат. */
  def intercept(user: User, hero: Hero, action: UserAction, renderer: Renderer): Task[Boolean] =
    typed(action) match {
      case None => ZIO.succeed(false)
      case Some(code) =>
        for {
          now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
          said <- redeem(user, hero, code, now)
          _    <- renderer.show(user, Screen(said, Nil))
        } yield true
    }

  /** Ввести код. Ответ — что сказать игроку. */
  def redeem(user: User, hero: Hero, code: PromoCode, now: Long): Task[String] =
    if (now >= code.untilMs) say("promo.expired", code)
    // В прологе героя ещё нет: ни сумки по делу, ни расы, которую менять.
    else if (hero.state == StateType.Registration) say("promo.noHero", code)
    else
      dao.redeemed(user.userId, code.key).flatMap {
        case true => say("promo.already", code)
        case false =>
          shortage(hero, code.reward).flatMap {
            case Some((need, free)) =>
              ZIO.succeed(content.format("promo.noRoom", "need" -> need.toString, "free" -> free.toString))
            // Отметка — до выдачи: награду нельзя получить дважды, даже если
            // выдача споткнётся на середине.
            case None =>
              dao.redeem(user.userId, code.key, now).flatMap {
                case false => say("promo.already", code)
                case true  => grant(user, hero, code, now)
              }
          }
      }

  /** Сколько мест под награду не хватает в сумке: `Some((нужно, свободно))`.
    * Сюжетным вещам место не нужно; отвары и травы сперва забирает Живая
    * сумка — по одному на каждое её свободное место. */
  private def shortage(hero: Hero, reward: PromoReward): Task[Option[(Long, Long)]] =
    for {
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      arts <- ZIO.foreach(artifacts)(_.get(hero.id).option).map(_.flatten)
      bulky = reward.items.filterNot(i => i.isQuestItem || i.weightless)
      caught = bulky.foldLeft(Map.empty[ArtifactKind, Int]) { (taken, item) =>
                 arts.flatMap(all => ArtifactKind.forItem(item).filter(k => all.of(k).freeSlots > taken.getOrElse(k, 0))) match {
                   case Some(kind) => taken.updated(kind, taken.getOrElse(kind, 0) + 1)
                   case None       => taken
                 }
               }
      need  = (bulky.size - caught.values.sum).toLong
    } yield Option.when(need > inv.freeSlots)((need, inv.freeSlots))

  private def grant(user: User, hero: Hero, code: PromoCode, now: Long): Task[String] = {
    val reward = code.reward
    for {
      azat    <- AzatData.load(heroDao, user.userId, now)
      _       <- ZIO.when(reward.blessingMs > 0L)(AzatData.save(heroDao, user.userId, azat.blessedFor(reward.blessingMs, now)))
      landed  <- ZIO.foreach(reward.items)(give(hero, _))
      _       <- ZIO.when(reward.doubloons > 0L)(
                   heroDao.getHeroByUserId(user.userId).someOrFail(new Throwable(s"Нет героя у игрока ${user.userId.value}"))
                     .flatMap(h => heroDao.updateDoubloons(user.userId, h.doubloons + reward.doubloons)))
    } yield {
      val lines =
        landed.map { case (item, _) => content.format("promo.item", "name" -> item.displayTitle) } ++
          Option.when(reward.blessingMs > 0L)(content.format(
            if (azat.blessingActive(now)) "promo.blessingLonger" else "promo.blessing",
            "hours" -> (reward.blessingMs / HourMs).toString)) ++
          Option.when(reward.doubloons > 0L)(content.format("promo.doubloons", "amount" -> reward.doubloons.toString))
      val caught = landed.collect { case (item, Intake.ToArtifact(kind, free)) => ArtifactIntake.line(content, item, kind, free) }
      (content.format("promo.accepted", "word" -> code.word, "rewards" -> lines.mkString("\n")) :: caught).mkString("\n\n")
    }
  }

  /** Вещь — в Живую сумку или в сумку. Место проверено заранее, под тем же
    * локом, так что отказ тут — поломка: её в лог. */
  private def give(hero: Hero, template: Item): Task[(Item, Intake)] =
    for {
      item   <- itemRepo.persist(hero.id, template)
      intake <- ArtifactIntake.accept(artifacts, inventoryRepo, hero.id, item)
      _      <- ZIO.when(intake == Intake.Refused)(
                  ZIO.logError(s"Промокод: вещь «${item.name}» не легла к герою ${hero.id.value}, хотя место было"))
    } yield (item, intake)

  private def say(key: String, code: PromoCode): Task[String] =
    ZIO.succeed(content.format(key, "word" -> code.word))
}

object PromoCodes {

  private val HourMs: Long = 60L * 60L * 1000L

  /** Промокод в сообщении — только голым текстом: кнопка с похожей подписью
    * кодом не считается. */
  def typed(action: UserAction): Option[PromoCode] =
    if (action.payload.isDefined) None else PromoCode.find(action.text)

  val live: ZLayer[PromoDao with HeroDao with InventoryRepository with ItemRepository with ArtifactRepository with SceneContent,
                   Nothing, PromoCodes] =
    ZLayer.fromFunction((dao: PromoDao, heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
                         artifacts: ArtifactRepository, content: SceneContent) =>
      PromoCodes(dao, heroDao, inventory, items, Some(artifacts), content))
}
