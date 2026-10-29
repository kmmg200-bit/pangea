package pangea.service.promo

import io.circe.syntax.EncoderOps
import pangea.engine.{Renderer, SceneContent}
import pangea.model.artifact.ArtifactKind
import pangea.model.hero.{AzatState, HeroId}
import pangea.model.item.{BrewKind, Item, QuestItemKind}
import pangea.model.promo.PromoCode
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.{PlayerLock, State, StateHandler, UserAction}
import pangea.test._
import zio.{Ref, Task, ZIO}
import zio.test._

import java.time.Instant

/** Промокод «Живём»: слово, срок, награда, «один раз» и место под награду. */
object PromoCodeSpec extends ZIOSpecDefault {

  private val me     = User(UserId(1L), VkId("101"), TelegramId("tg1"))
  private val myHero = HeroId(1L)
  private val code   = PromoCode.Zhivyom

  private val hour = 60L * 60L * 1000L
  private val day  = 24L * hour

  // Среда, 28 октября 2026-го, полдень по Москве: код ещё действует.
  private val wednesday = Instant.parse("2026-10-28T09:00:00Z").toEpochMilli

  /** Герой с семью дублонами; в сумке — `bag`. */
  private def world(
    state:     StateType = StateType.Dungeon,
    bag:       List[Item] = Nil,
    artifacts: Option[TestArtifactRepository] = None
  ) =
    for {
      heroDao <- TestHeroDao.withHero(me.userId,
                   TestFixtures.hero(me.userId, state = state).copy(id = myHero, doubloons = 7L))
      content <- ZIO.attempt(SceneContent.load())
      inv      = TestInventoryRepository.withItems(bag)
      dao      = new TestPromoDao
    } yield (PromoCodes(dao, heroDao, inv, TestItemRepository.make, artifacts, content), heroDao, inv, dao)

  private def heroOf(dao: TestHeroDao) = dao.getHeroByUserId(me.userId).map(_.get)

  private def azatOf(dao: TestHeroDao) =
    dao.readAzatData(me.userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))

  /** Обычные вещи в сумку — каждая на своё место. */
  private def junk(n: Int): List[Item] =
    List.tabulate(n)(i => BrewKind.item(BrewKind.BoneSetter).copy(id = 1000L + i))

  /** Сцена-заглушка: запоминает, какие сообщения до неё дошли. */
  private final class Recording(seen: Ref[List[String]]) extends State {
    def enter(user: User, renderer: Renderer): Task[Unit] = ZIO.unit
    def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
      seen.update(_ :+ ua.text).as(StateType.Battle)
  }

  override def spec = suite("Промокод")(

    test("«Ренейм»: только купон на смену имени, до 00:00 5 ноября по Москве") {
      val r = PromoCode.Rename
      assertTrue(PromoCode.find("ренейм").contains(r) && PromoCode.find("Ренейм!").contains(r)) &&
      assertTrue(r.reward.items.flatMap(_.questItem) == List(pangea.model.item.QuestItemKind.NameCoupon)) &&
      assertTrue(r.reward.blessingMs == 0L && r.reward.doubloons == 0L) &&
      assertTrue(r.untilMs == PromoCode.moscowMidnight(java.time.LocalDate.of(2026, 11, 5)))
    },

    test("слово: регистр, «ё» и «е», кавычки и знаки не важны; похожее — не код, кнопка — не код") {
      assertTrue(PromoCode.find("Живём").contains(code)) &&
      assertTrue(PromoCode.find("живем").contains(code)) &&
      assertTrue(PromoCode.find("  «ЖИВЁМ!»  ").contains(code)) &&
      assertTrue(PromoCode.find("живём живём").isEmpty && PromoCode.find("живу").isEmpty) &&
      assertTrue(PromoCode.find("").isEmpty && PromoCode.find("!!!").isEmpty) &&
      assertTrue(PromoCodes.typed(UserAction("Живём", None)).contains(code)) &&
      assertTrue(PromoCodes.typed(UserAction("Живём", Some("""{"action":"Back"}"""))).isEmpty)
    },

    test("срок — до 00:00 понедельника 2 ноября по Москве, то есть до 21:00 воскресенья по UTC") {
      assertTrue(code.untilMs == Instant.parse("2026-11-01T21:00:00Z").toEpochMilli)
    },

    test("код принят: купон, живая вода, шнапс, благословение на сутки без 250 отдыхов и 50 дублонов") {
      for {
        t <- world()
        (codes, heroDao, inv, dao) = t
        hero  <- heroOf(heroDao)
        said  <- codes.redeem(me, hero, code, wednesday)
        after <- heroOf(heroDao)
        azat  <- azatOf(heroDao)
      } yield assertTrue(inv.snapshot.flatMap(_.questItem) == List(QuestItemKind.RaceCoupon)) &&
              assertTrue(inv.snapshot.flatMap(_.brew) == List(BrewKind.LivingWater, BrewKind.Schnapps)) &&
              assertTrue(after.doubloons == 7L + 50L) &&
              assertTrue(azat.blessingUntil.contains(wednesday + day) && azat.instantRests == 0) &&
              assertTrue(dao.snapshot.get((me.userId, code.key)).contains(wednesday)) &&
              assertTrue(said.contains("Код «Живём» принят")) &&
              assertTrue(said.contains("Купон на смену расы") && said.contains("Живая вода")) &&
              assertTrue(said.contains("Шнапс из красавки") && said.contains("🟡 50 дублонов")) &&
              assertTrue(said.contains("Благословение Азата на 24 ч"))
    },

    test("второй раз — отказ, и награда не повторяется") {
      for {
        t <- world()
        (codes, heroDao, inv, _) = t
        hero  <- heroOf(heroDao)
        _     <- codes.redeem(me, hero, code, wednesday)
        again <- heroOf(heroDao).flatMap(codes.redeem(me, _, code, wednesday + hour))
        after <- heroOf(heroDao)
      } yield assertTrue(again.contains("уже вводили")) &&
              assertTrue(inv.snapshot.size == 3 && after.doubloons == 7L + 50L)
    },

    test("благословение уже идёт — срок продлевается на сутки от остатка") {
      for {
        t <- world()
        (codes, heroDao, _, _) = t
        _    <- heroDao.writeAzatData(me.userId,
                  AzatState(blessingUntil = Some(wednesday + 2 * day), restsGrantedAt = Some(wednesday - hour)).asJson)
        hero <- heroOf(heroDao)
        said <- codes.redeem(me, hero, code, wednesday)
        azat <- azatOf(heroDao)
      } yield assertTrue(azat.blessingUntil.contains(wednesday + 3 * day)) &&
              assertTrue(said.contains("Благословение Азата продлено на 24 ч"))
    },

    test("срок вышел — отказ и ничего не выдано, а за миллисекунду до конца ещё можно") {
      for {
        t <- world()
        (codes, heroDao, inv, dao) = t
        hero  <- heroOf(heroDao)
        late  <- codes.redeem(me, hero, code, code.untilMs)
        empty  = inv.snapshot.isEmpty && dao.snapshot.isEmpty
        after <- heroOf(heroDao)
        azat  <- azatOf(heroDao)
        last  <- codes.redeem(me, hero, code, code.untilMs - 1L)
      } yield assertTrue(late.contains("Срок кода «Живём» вышел")) &&
              assertTrue(empty && after.doubloons == 7L && azat.blessingUntil.isEmpty) &&
              assertTrue(last.contains("принят"))
    },

    test("в сумке нет двух мест — отказ, код не сгорает; освободили место — сработал") {
      for {
        t <- world(bag = junk(19))
        (codes, heroDao, inv, dao) = t
        hero   <- heroOf(heroDao)
        full   <- codes.redeem(me, hero, code, wednesday)
        before  = (inv.snapshot.size, dao.snapshot.isEmpty)
        _      <- inv.removeItem(1000L, myHero).orElseFail(new Throwable("сумка не отдала вещь"))
        said   <- codes.redeem(me, hero, code, wednesday)
        after  <- heroOf(heroDao)
      } yield assertTrue(full.contains("нужно свободных мест в сумке — 2, а есть 1")) &&
              assertTrue(before == ((19, true))) &&
              assertTrue(said.contains("принят")) &&
              // купон места не занимает: 18 вещей, купон и две склянки
              assertTrue(inv.snapshot.size == 18 + 3 && after.doubloons == 7L + 50L)
    },

    test("отвары ловит Живая сумка: в полной сумке хватает места на купон") {
      val arts = TestArtifactRepository.of(bag = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1))
      for {
        t <- world(bag = junk(20), artifacts = Some(arts))
        (codes, heroDao, inv, _) = t
        hero <- heroOf(heroDao)
        said <- codes.redeem(me, hero, code, wednesday)
      } yield assertTrue(said.contains("принят")) &&
              assertTrue(arts.snapshot.of(ArtifactKind.LivingBag).items.data.flatMap(_.brew) ==
                List(BrewKind.LivingWater, BrewKind.Schnapps)) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.RaceCoupon))) &&
              assertTrue(said.contains("Живая вода → «Живая сумка»"))
    },

    test("в Живой сумке одно место, сумка полна — второй склянке некуда, отказ") {
      val arts = TestArtifactRepository.of(bag =
        TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1, items = junk(14).map(_.copy(id = 0L))))
      for {
        t <- world(bag = junk(20), artifacts = Some(arts))
        (codes, heroDao, inv, dao) = t
        hero <- heroOf(heroDao)
        said <- codes.redeem(me, hero, code, wednesday)
      } yield assertTrue(said.contains("нужно свободных мест в сумке — 1, а есть 0")) &&
              assertTrue(arts.snapshot.of(ArtifactKind.LivingBag).items.data.size == 14) &&
              assertTrue(inv.snapshot.size == 20 && dao.snapshot.isEmpty)
    },

    test("в прологе код не принимается — персонажа ещё нет") {
      for {
        t <- world(state = StateType.Registration)
        (codes, heroDao, inv, dao) = t
        hero <- heroOf(heroDao)
        said <- codes.redeem(me, hero, code, wednesday)
      } yield assertTrue(said.contains("когда вы создадите персонажа")) &&
              assertTrue(inv.snapshot.isEmpty && dao.snapshot.isEmpty)
    },

    test("код пишут с любого экрана, даже из боя: ответ письмом, бой и его кнопки не тронуты") {
      for {
        content  <- ZIO.attempt(SceneContent.load())
        heroDao  <- TestHeroDao.withHero(me.userId,
                      TestFixtures.hero(me.userId, state = StateType.Battle).copy(id = myHero, doubloons = 7L))
        userRepo <- TestUserRepository.withUsers(me)
        api      <- TestApi.make
        lock     <- PlayerLock.make
        seen     <- Ref.make(List.empty[String])
        inv       = TestInventoryRepository.accepting
        payouts   = Payouts(TestPayoutDao.empty, heroDao, content)
        parcels   = Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
        transfers = Transfers(inv, userRepo, parcels, new TestPlayers, content, heroDao, TestBankRepository.empty,
                      payouts, lock)
        codes     = PromoCodes(new TestPromoDao, heroDao, inv, TestItemRepository.make, None, content)
        handler   = new StateHandler(api, userRepo, new DaoBackedTestHeroRepository(heroDao), heroDao, payouts,
                      parcels, transfers, Map[StateType, State](StateType.Battle -> new Recording(seen)),
                      AdminPanel.disabled, content, lock, None, None, Some(codes))
        _        <- TestClock.setTime(Instant.ofEpochMilli(wednesday))
        _        <- handler.makeActionVK(me.vkId, 1L, UserAction("живем", None))
        _        <- handler.makeActionVK(me.vkId, 2L, UserAction("Живём", None))
        // обычный текст и кнопка с той же подписью идут бою, как и раньше
        _        <- handler.makeActionVK(me.vkId, 3L, UserAction("привет", None))
        _        <- handler.makeActionVK(me.vkId, 4L, UserAction("Живём", Some("""{"action":"Attack"}""")))
        sent     <- api.sentMessages
        toBattle <- seen.get
        hero     <- heroOf(heroDao)
      } yield assertTrue(sent.map(_._1).exists(_.contains("Код «Живём» принят"))) &&
              assertTrue(sent.map(_._1).exists(_.contains("уже вводили"))) &&
              // клавиатуру боя не трогаем: ответ уходит без кнопок
              assertTrue(sent.forall(_._2.isEmpty)) &&
              assertTrue(toBattle == List("привет", "Живём")) &&
              assertTrue(hero.state == StateType.Battle && hero.doubloons == 7L + 50L)
    }
  )
}
