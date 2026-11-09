package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{AzatState, Hero}
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.squad.{AllyKind, AllyRates, Squad, UndeadForm}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._
import zio.test.TestRandom

// Death — узел-эффект (§18): вся логика смерти отыгрывается в `enter`, а переход
// в Rest идёт через `autoAdvance` без действия игрока. Поэтому тесты дёргают `enter`.
object DeathStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private val testItem = Item(42L, "Меч судьбы", 1L, Rarity.Blue, ItemType.Weapon,
                              attack = 10, accuracy = 0, energy = 0,
                              armor = 0, defence = 0, evasion = 0)

  private def makeState(
    hero:      pangea.model.hero.Hero,
    items:     List[Item] = Nil,
    artifacts: Option[pangea.test.TestArtifactRepository] = None
  ) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (DeathState(heroDao, invRepo, content, artifacts), heroDao, invRepo, renderer)

  // exp=80 (внутри уровня 1), теряет 10% = 8, newExp=72
  private val richHero = TestFixtures.hero(userId).copy(exp = 80L, silver = 500L)

  private val mob = pangea.model.monster.Monster(0L, 1L, pangea.model.monster.Race.Orc,
                      pangea.model.monster.Rarity.Common, pangea.model.stats.FightStats(1, 10, 0, 0, 0, 1, 0))

  /** Смерть героя `h` в бою `battle`: отряд после неё и была ли строка о резерве. */
  private def afterDeath(h: Hero, battle: Json) =
    for {
      t <- makeState(h)
      (state, heroDao, _, renderer) = t
      _       <- heroDao.writeActiveBattle(userId, battle)
      _       <- state.enter(testUser, renderer)
      squad   <- heroDao.getHeroByUserId(userId).map(_.map(_.squad))
      screens <- renderer.sentScreens.map(_.map(_.text))
    } yield (squad, screens.filter(_.contains("Резерв")))

  override def spec = suite("DeathState")(

    test("камни из стопки теряются поштучно: бросок на каждый, а не на всю пачку") {
      // Пять одинаковых камней. Складываются они только на экране, поэтому
      // бросок идёт на каждый: два ролла из пяти «сгорают» (меньше 25 = потеря).
      val stones = (1 to 5).map(i =>
        pangea.generator.item.GemGenerator.item(pangea.model.item.GemKind.Amethyst, 1)
          .copy(id = 300L + i)).toList
      for {
        t <- makeState(richHero, stones)
        (state, _, invRepo, renderer) = t
        // Первый бросок съедает выбор травмы, дальше — по одному на камень.
        _    <- TestRandom.feedInts(1, 0, 99, 24, 25, 99)
        _    <- state.enter(testUser, renderer)
        left  = invRepo.snapshot
        log  <- renderer.sentScreens.map(_.map(_.text).mkString)
      } yield assertTrue(left.size == 3) &&
              // одно сообщение на всё потерянное, с количеством
              assertTrue(log.contains("×2"))
    },

    test("ларец и сумка смерти не помеха, а шкаф — помеха: бросок трясёт и артефакты") {
      import pangea.model.artifact.ArtifactKind
      import pangea.test.TestArtifactRepository
      def stone(id: Long) = pangea.generator.item.GemGenerator
        .item(pangea.model.item.GemKind.Amethyst, 1).copy(id = id)
      def leaf(id: Long) = pangea.generator.item.MaterialGenerator
        .item(pangea.model.item.MaterialKind.Chamomile).copy(id = id)
      val artifacts = TestArtifactRepository.of(
        casket   = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1, items = List(stone(101L), stone(102L))),
        bag      = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1, items = List(leaf(201L))),
        wardrobe = TestArtifactRepository.artifact(ArtifactKind.Wardrobe, tier = 1, items = List(testItem)))
      for {
        t <- makeState(richHero, items = Nil, artifacts = Some(artifacts))
        (state, _, _, renderer) = t
        // Первый бросок — выбор травмы; дальше по одному на вещь в артефактах:
        // ларец (два камня), сумка (лист). Шкаф в бросок не попадает вовсе.
        _   <- TestRandom.feedInts(1, 0, 99, 0)
        _   <- state.enter(testUser, renderer)
        all  = artifacts.snapshot
        log <- renderer.sentScreens.map(_.map(_.text).mkString)
      } yield assertTrue(all.of(ArtifactKind.Casket).items.data.map(_.id) == List(102L)) &&
              assertTrue(all.of(ArtifactKind.LivingBag).items.data.isEmpty) &&
              // шкаф цел: его содержимое смерть не трогает
              assertTrue(all.of(ArtifactKind.Wardrobe).items.data.map(_.id) == List(42L)) &&
              assertTrue(log.contains("аметист") && log.contains("Ромашка") && !log.contains("Меч судьбы"))
    },

    test("с благословением опыта теряется на 10% меньше") {
      val hero = TestFixtures.hero(userId).copy(exp = 1000L, silver = 500L)
      def lostWith(azat: Option[AzatState]) =
        for {
          t <- makeState(hero)
          (state, heroDao, _, renderer) = t
          _   <- ZIO.foreachDiscard(azat)(a => heroDao.writeAzatData(userId, a.asJson))
          _   <- state.enter(testUser, renderer)
          upd <- heroDao.getHeroByUserId(userId).map(_.get)
        } yield 1000L - upd.exp
      for {
        plain   <- lostWith(None)
        // Благословение на неделю вперёд: TestClock стоит на нуле.
        blessed <- lostWith(Some(AzatState(blessingUntil = Some(AzatState.BlessingDurationMs))))
      } yield assertTrue(plain == 100L) &&          // обычные 10% от 1000
              assertTrue(blessed == 90L)            // на 10% меньше самой потери
    },

    test("ступень ларца и сумки бережёт добро: 22% на 1-й, 13% на 4-й, 10% на 5-й") {
      import pangea.model.artifact.ArtifactKind
      import pangea.test.TestArtifactRepository
      def leaf(id: Long) = pangea.generator.item.MaterialGenerator
        .item(pangea.model.item.MaterialKind.Chamomile).copy(id = id)
      def stone(id: Long) = pangea.generator.item.GemGenerator
        .item(pangea.model.item.GemKind.Amethyst, 1).copy(id = id)
      val artifacts = TestArtifactRepository.of(
        casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 4, items = List(stone(101L), stone(102L))),
        bag    = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 5, items = List(leaf(201L), leaf(202L))))
      for {
        t <- makeState(richHero, items = List(testItem), artifacts = Some(artifacts))
        (state, _, invRepo, renderer) = t
        // Травма; меч в сумке (20 < 25 — пропал); ларец 4-й ступени: 12 < 13 —
        // пропал, 13 — уцелел; сумка 5-й: 9 < 10 — пропал, 10 — уцелел.
        _   <- TestRandom.feedInts(1, 20, 12, 13, 9, 10)
        _   <- state.enter(testUser, renderer)
        all  = artifacts.snapshot
      } yield assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(all.of(ArtifactKind.Casket).items.data.map(_.id) == List(102L)) &&
              assertTrue(all.of(ArtifactKind.LivingBag).items.data.map(_.id) == List(202L)) &&
              assertTrue(DeathState.stashLossPct(1) == 22 && DeathState.stashLossPct(4) == 13 &&
                         DeathState.stashLossPct(5) == 10)
    },

    test("enter → показывает экран потерь и авто-переходит в Rest") {
      for {
        triple              <- makeState(richHero)
        (state, _, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.exists(_.text.contains("Потери"))) &&
              assertTrue(state.autoAdvance.contains(StateType.Rest))
    },

    test("enter → теряет 10% опыта текущего уровня и 50% серебра, пишет restDurationMs") {
      for {
        triple                    <- makeState(richHero)
        (state, heroDao, _, renderer) = triple
        _                         <- state.enter(testUser, renderer)
        updated                   <- heroDao.getHeroByUserId(userId)
        screens                   <- renderer.sentScreens
        sceneData                 <- heroDao.readSceneData(userId)
      } yield assertTrue(updated.exists(_.exp == 72L)) &&
              assertTrue(updated.exists(_.silver == 250L)) &&
              assertTrue(screens.exists(_.text.contains("опыта"))) &&
              assertTrue(screens.exists(_.text.contains("серебра"))) &&
              // уровень 1 → ровно 1 минута (90 - 89*exp(0) = 1)
              assertTrue(sceneData.flatMap(_.hcursor.get[Long]("restDurationMs").toOption).contains(60000L))
    },

    test("restDurationMs растёт с уровнем героя и не достигает 90 минут") {
      val highHero = TestFixtures.hero(userId).copy(lvl = 50L)
      for {
        triple                    <- makeState(highHero)
        (state, heroDao, _, renderer) = triple
        _                         <- state.enter(testUser, renderer)
        sceneData                 <- heroDao.readSceneData(userId)
        ms                         = sceneData.flatMap(_.hcursor.get[Long]("restDurationMs").toOption).getOrElse(0L)
      } yield assertTrue(ms > 60000L) &&            // больше, чем на 1 уровне
              assertTrue(ms < 90L * 60000L)         // строго меньше асимптоты в 90 минут
    },

    test("enter → получает случайную именованную травму из списка") {
      for {
        triple                    <- makeState(richHero)
        (state, heroDao, _, renderer) = triple
        _                         <- state.enter(testUser, renderer)
        updated                   <- heroDao.getHeroByUserId(userId)
        screens                   <- renderer.sentScreens
      } yield assertTrue(updated.exists(_.traumaNames.nonEmpty)) &&
              assertTrue(updated.exists(h => h.traumaNames.forall(n => pangea.model.trauma.Trauma.byName(n).isDefined))) &&
              assertTrue(updated.exists(_.traumaUntil.isDefined)) &&
              assertTrue(screens.exists(_.text.contains("травм")))
    },

    test("пал раньше, а отряд доиграл и проиграл — смерть новую травму не даёт: её дало падение") {
      val fell      = richHero.copy(traumaUntil = Some(Long.MaxValue), traumaNames = List(pangea.model.trauma.Trauma.light.head.name))
      val monster   = pangea.model.monster.Monster(0L, 1L, pangea.model.monster.Race.Orc, pangea.model.monster.Rarity.Common,
                        pangea.model.stats.FightStats(1, 10, 0, 0, 0, 1, 0))
      val b         = pangea.model.battle.SoloPveBattle.from(monster, fell)
      for {
        triple                    <- makeState(fell)
        (state, heroDao, _, renderer) = triple
        _                         <- heroDao.writeActiveBattle(userId, b.copy(group = b.group.copy(heroDown = true)).asJson)
        _                         <- state.enter(testUser, renderer)
        updated                   <- heroDao.getHeroByUserId(userId)
        screens                   <- renderer.sentScreens
      } yield assertTrue(updated.exists(_.traumaNames == fell.traumaNames)) &&
              assertTrue(!screens.exists(_.text.contains("Новая травма"))) &&
              assertTrue(screens.exists(_.text.contains("Потери")))
    },

    test("enter при максимуме травм → новую не даёт, продлевает таймер, сообщает «не осталось живого места»") {
      val allNames = pangea.model.trauma.Trauma.all.map(_.name).toList
      val maxedHero = richHero.copy(
        traumaUntil = Some(3600000L),            // травмы ещё активны: час до конца
        traumaNames = allNames
      )
      for {
        triple                    <- makeState(maxedHero)
        (state, heroDao, _, renderer) = triple
        _                         <- state.enter(testUser, renderer)
        updated                   <- heroDao.getHeroByUserId(userId)
        screens                   <- renderer.sentScreens
      } yield assertTrue(updated.exists(_.traumaNames == allNames)) &&        // список не вырос
              // таймер продлён до полных восьми часов от смерти
              assertTrue(updated.exists(_.traumaUntil.contains(pangea.model.trauma.TraumaRoll.DurationMs))) &&
              assertTrue(screens.exists(_.text.contains("не осталось живого места")))
    },

    test("enter без предметов → очищает бой, не падает") {
      for {
        triple                        <- makeState(richHero, items = Nil)
        (state, heroDao, _, renderer)  = triple
        _                             <- state.enter(testUser, renderer)
        battle                        <- heroDao.readActiveBattle(userId)
      } yield assertTrue(state.autoAdvance.contains(StateType.Rest)) &&
              assertTrue(battle.isEmpty)
    },

    test("enter с предметами в инвентаре → каждый предмет имеет 25% шанс дропа (не крашится)") {
      val manyItems = (1 to 20).map(i =>
        Item(i.toLong, s"Предмет $i", 1L, Rarity.Gray, ItemType.Weapon,
             attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)
      ).toList
      for {
        triple                        <- makeState(richHero, items = manyItems)
        (state, _, invRepo, renderer)  = triple
        _                             <- state.enter(testUser, renderer)
      } yield assertTrue(state.autoAdvance.contains(StateType.Rest)) &&
              assertTrue(invRepo.snapshot.length <= 20)
    },

    test("enter с одним предметом → предмет может быть потерян или остаться") {
      for {
        triple                        <- makeState(richHero, items = List(testItem))
        (state, _, invRepo, renderer)  = triple
        _                             <- state.enter(testUser, renderer)
      } yield assertTrue(state.autoAdvance.contains(StateType.Rest)) &&
              assertTrue(invRepo.snapshot.length == 0 || invRepo.snapshot.length == 1)
    },

    test("пал в бою с отрядом — резерв уходит весь: наёмник на сутки, поднятые насовсем; строй цел") {
      val orc   = UndeadForm("Поднятый орк", 3L, AllyKind.Human.stats(3L))
      // Йорген на 2 остаётся в строю; Плюх и двое поднятых — в резерве
      val squad = Squad.empty.hire(AllyKind.Human, 1L, 0L).hire(AllyKind.Murloc, 1L, 0L)
                    .raise(orc, 1L, 0L).raise(orc, 1L, 0L).toReserve(3).toReserve(4).toReserve(5)
      val hero  = richHero.copy(squad = squad)
      for {
        res <- afterDeath(hero, SoloPveBattle.from(mob, hero).asJson)
        (after, lines) = res
      } yield assertTrue(squad.reserve.size == 3) &&
              assertTrue(after.exists(s => s.reserve.isEmpty && s.allies == squad.allies)) &&
              // TestClock стоит на нуле: Плюх сядет за стол ровно через сутки
              assertTrue(after.exists(s => s.isAway(AllyKind.Murloc, AllyRates.AwayMs - 1L) &&
                                           !s.isAway(AllyKind.Murloc, AllyRates.AwayMs))) &&
              assertTrue(lines == List("👣 Резерв не стал ждать, пока вы очнётесь, и разошёлся: Плюх, Поднятый орк ×2."))
    },

    test("пал один на один — отряд оставался дома, резерв цел; цел он и в бою, начатом до выкладки; пуст — строки нет") {
      val squad     = Squad.empty.hire(AllyKind.Human, 1L, 0L).hire(AllyKind.Murloc, 1L, 0L).toReserve(3)
      val hero      = richHero.copy(squad = squad)
      val noReserve = richHero.copy(squad = Squad.empty.hire(AllyKind.Human, 1L, 0L))
      // запись боя до пометки: поля squadTaken в ней нет
      val old       = SoloPveBattle.from(mob, hero).asJson.mapObject(_.remove("squadTaken"))
      for {
        alone  <- afterDeath(hero, SoloPveBattle.from(mob, hero, squad = false).asJson)
        before <- afterDeath(hero, old)
        empty  <- afterDeath(noReserve, SoloPveBattle.from(mob, noReserve).asJson)
      } yield assertTrue(squad.reserve.map(_.kind) == List(AllyKind.Murloc)) &&
              assertTrue(old.hcursor.downField("squadTaken").focus.isEmpty) &&
              assertTrue(alone == (Some(squad), Nil)) &&
              assertTrue(before == (Some(squad), Nil)) &&
              assertTrue(empty == (Some(noReserve.squad), Nil))
    }
  )
}
