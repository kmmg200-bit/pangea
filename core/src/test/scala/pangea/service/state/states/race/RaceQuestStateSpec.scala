package pangea.service.state.states.race

import io.circe.syntax.EncoderOps
import pangea.engine.{SceneContent, Screen}
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.InventoryState
import pangea.service.state.states.artifact.FetShopState
import pangea.service.state.states.race.RaceQuestState.{Progress, RaceQuest, Scene}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Квест смены расы: купон у Фета, «Сменить расу» в сумке (только из города),
  * выбор расы — и история этой расы до пробуждения в новом теле. */
object RaceQuestStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((Map("action" -> key) ++ data).asJson.noSpaces))

  private def coupon(id: Long): Item = QuestItemKind.item(QuestItemKind.RaceCoupon).copy(id = id)

  private def heroOf(race: Race): Hero = TestFixtures.hero(userId).copy(race = race)

  /** Раса героя, из которой в историю `q` можно уйти: любая, кроме её собственной. */
  private def from(q: RaceQuest): Race = if (q.race == Race.Human) Race.Orc else Race.Human

  private def quest(h: Hero, bag: List[Item] = List(coupon(900L))) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeSceneData(userId, Progress.start.asJson)
      inv  = TestInventoryRepository.withItems(bag)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (RaceQuestState(dao, inv, c), dao, inv, r, c)

  private def last(r: TestRenderer): Task[Screen] = r.sentScreens.map(_.last)

  private def raceOf(dao: TestHeroDao): Task[Option[Race]] = dao.raceSnapshot.map(_.get(userId))

  private def questOf(race: Race): RaceQuest =
    RaceQuestState.of(race).getOrElse(RaceQuest(race, "missing", Nil))

  /** Пройти историю целиком: выбор расы, «Решено» и в каждой сцене выбор
    * номер `n` (или последний, если выборов меньше). Возвращает, куда ведёт
    * последнее нажатие. */
  private def walk(state: RaceQuestState, r: TestRenderer, q: RaceQuest, n: Int): Task[StateType] = {
    def go(scene: Scene, fuel: Int): Task[StateType] =
      scene.picks.lift(n).orElse(scene.picks.lastOption) match {
        case None => ZIO.succeed(StateType.RaceQuest)
        case Some(pk) =>
          state.action(testUser, tap("RaceStep", "pick" -> pk.id), r).flatMap { out =>
            q.scene(pk.next) match {
              case Some(next) if !next.isFinal && fuel > 0 => go(next, fuel - 1)
              case _                                       => ZIO.succeed(out)
            }
          }
      }
    for {
      _   <- state.action(testUser, tap("RacePick", "race" -> q.race.entryName), r)
      _   <- state.action(testUser, tap("RaceGo", "race" -> q.race.entryName), r)
      out <- q.first.fold[Task[StateType]](ZIO.succeed(StateType.RaceQuest))(go(_, 20))
    } yield out
  }

  override def spec = suite("Квест смены расы")(

    test("у каждой смертной расы своя история: сцены связаны, финал один, все тексты на месте") {
      for {
        c     <- ZIO.attempt(SceneContent.load())
        texts <- ZIO.attempt(RaceQuestState.quests.flatMap { q =>
                   c.text(s"raceQuest.${q.key}.title") :: q.scenes.flatMap { s =>
                     c.text(s"raceQuest.${q.key}.${s.id}.text") :: s.picks.flatMap { pk =>
                       c.choice("RaceStep", s"raceQuest.${q.key}.${s.id}.${pk.id}").label ::
                         (if (pk.hasThen) List(c.text(s"raceQuest.${q.key}.${s.id}.${pk.id}Then")) else Nil)
                     }
                   }
                 })
      } yield assertTrue(RaceQuestState.quests.map(_.race).toSet == Race.mortals.toSet) &&
              assertTrue(RaceQuestState.quests.size == Race.mortals.size) &&
              assertTrue(RaceQuestState.quests.forall(q => q.scenes.count(_.isFinal) == 1 && q.first.exists(!_.isFinal))) &&
              assertTrue(RaceQuestState.quests.forall(q => q.scenes.forall(_.picks.forall(pk => q.scene(pk.next).isDefined)))) &&
              assertTrue(texts.forall(_.nonEmpty)) &&
              // письмо эльфам — привет от Братства, без подписи
              assertTrue(!c.text("raceQuest.elf.s5.text").contains("Элорин"))
    },

    test("выбор расы: все смертные, кроме своей, и «Не сейчас» — обратно в сумку") {
      for {
        t <- quest(heroOf(Race.Human))
        (state, dao, _, r, _) = t
        _     <- state.enter(testUser, r)
        pick  <- last(r)
        own   <- state.action(testUser, tap("RacePick", "race" -> Race.Human.entryName), r)
        still <- last(r)
        out   <- state.action(testUser, tap("RaceCancel"), r)
        scene <- dao.readSceneData(userId)
      } yield assertTrue(pick.choices.filter(_.id == "RacePick").flatMap(_.data.get("race")) ==
                Race.mortals.filterNot(_ == Race.Human).map(_.entryName).toList) &&
              assertTrue(pick.choices.lastOption.exists(_.id == "RaceCancel")) &&
              // свою расу не выбрать: тот же выбор ещё раз
              assertTrue(own == StateType.RaceQuest && still.choices == pick.choices) &&
              assertTrue(out == StateType.Inventory && scene.forall(_.isNull))
    },

    test("каждая история проходится до конца: раса новая, купон сгорел, итог с названием, «Вернуться в Кинэт» — в город") {
      ZIO.foreach(RaceQuestState.quests) { q =>
        for {
          t <- quest(heroOf(from(q)))
          (state, dao, inv, r, c) = t
          out  <- walk(state, r, q, 0)
          fin  <- last(r)
          race <- raceOf(dao)
          home <- state.action(testUser, tap("RaceHome"), r)
        } yield assertTrue(out == StateType.RaceQuest && race.contains(q.race)) &&
                assertTrue(!inv.snapshot.exists(_.questItem.contains(QuestItemKind.RaceCoupon))) &&
                assertTrue(fin.text.contains(s"Теперь вы — ${q.race}") && fin.text.contains(c.text(s"raceQuest.${q.key}.title"))) &&
                assertTrue(fin.choices.map(_.id) == List("RaceHome") && home == StateType.GlobalMap)
      }.map(_.reduce(_ && _))
    },

    test("второй выбор в сцене: его реплика открывает следующую сцену") {
      ZIO.foreach(RaceQuestState.quests) { q =>
        for {
          t <- quest(heroOf(from(q)))
          (state, _, _, r, c) = t
          _    <- walk(state, r, q, 1)
          said <- r.sentScreens.map(_.map(_.text))
          lines = q.scenes.flatMap(s => s.picks.lift(1).orElse(s.picks.lastOption).filter(_.hasThen)
                    .map(pk => c.text(s"raceQuest.${q.key}.${s.id}.${pk.id}Then")))
        } yield assertTrue(lines.nonEmpty && lines.forall(line => said.exists(_.startsWith(line))))
      }.map(_.reduce(_ && _))
    },

    test("перед «Решено» — заметки Ашалдарона о выбранной расе, как в прологе, и вопрос, кем проснуться") {
      for {
        t <- quest(heroOf(Race.Human))
        (state, _, _, r, _) = t
        _       <- state.action(testUser, tap("RacePick", "race" -> Race.Khajiit.entryName), r)
        confirm <- last(r)
        _       <- state.action(testUser, tap("RaceBack"), r)
        _       <- state.action(testUser, tap("RacePick", "race" -> Race.Demon.entryName), r)
        other   <- last(r)
      } yield assertTrue(confirm.text.startsWith(Race.Khajiit.description)) &&
              assertTrue(confirm.text.contains("Заметки путешественника Ашалдарона о расах Пангеи")) &&
              assertTrue(confirm.text.endsWith("Квест «Долг до утра». Кем проснуться: Каджит?")) &&
              assertTrue(confirm.choices.map(_.id) == List("RaceGo", "RaceBack", "RaceCancel")) &&
              assertTrue(other.text.startsWith(Race.Demon.description))
    },

    test("начатую историю не бросить: посторонний ввод и «Не сейчас» — та же сцена; после финала купон второй раз не сгорает") {
      val q = questOf(Race.Khajiit)
      for {
        t <- quest(heroOf(Race.Human), bag = List(coupon(900L), coupon(901L)))
        (state, dao, inv, r, _) = t
        _      <- state.action(testUser, tap("RacePick", "race" -> Race.Khajiit.entryName), r)
        _      <- state.action(testUser, tap("RaceGo", "race" -> Race.Khajiit.entryName), r)
        first  <- last(r)
        typed  <- state.action(testUser, UserAction("что угодно", None), r)
        again  <- last(r)
        cancel <- state.action(testUser, tap("RaceCancel"), r)
        again2 <- last(r)
        _      <- walk(state, r, q, 0)
        fin    <- last(r)
        stale  <- state.action(testUser, tap("RaceStep", "pick" -> "breath"), r)
        after  <- last(r)
        left   <- ZIO.succeed(inv.snapshot.count(_.questItem.contains(QuestItemKind.RaceCoupon)))
        race   <- raceOf(dao)
      } yield assertTrue(typed == StateType.RaceQuest && again == first) &&
              assertTrue(cancel == StateType.RaceQuest && again2 == first) &&
              assertTrue(stale == StateType.RaceQuest && after == fin) &&
              assertTrue(left == 1 && race.contains(Race.Khajiit))
    },

    test("купона в сумке нет — история обрывается: раса прежняя, в город") {
      for {
        t <- quest(heroOf(Race.Human), bag = Nil)
        (state, dao, _, r, _) = t
        out  <- walk(state, r, questOf(Race.Khajiit), 0)
        race <- raceOf(dao)
        said <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(out == StateType.GlobalMap && race.isEmpty) &&
              assertTrue(said.contains("Купона на смену расы в сумке не нашлось"))
    },

    test("здоровье выше потолка нового тела срезается, травмы остаются") {
      val orc = heroOf(Race.Orc).copy(traumaNames = List("Сломанное ребро"))
      val big = orc.copy(fightStats = orc.fightStats.copy(hp = 1_000_000L))
      for {
        t <- quest(big)
        (state, dao, _, r, _) = t
        _    <- walk(state, r, questOf(Race.Khajiit), 0)
        hero <- dao.getHeroByUserId(userId)
        cap   = big.copy(race = Race.Khajiit).effectiveMaxHp(0L)
      } yield assertTrue(hero.exists(h => h.fightStats.hp == cap && cap < 1_000_000L)) &&
              assertTrue(hero.exists(_.traumaNames == List("Сломанное ребро")))
    },

    test("купон в сумке: «Сменить расу»; не из города — отказ, из города — к выбору расы") {
      for {
        dao   <- TestHeroDao.withHero(userId, heroOf(Race.Human))
        inv    = TestInventoryRepository.withItems(List(coupon(900L)))
        r     <- TestRenderer.make
        c     <- ZIO.attempt(SceneContent.load())
        state  = InventoryState(dao, inv, TestItemRepository.make, c)
        _     <- state.action(testUser, tap(s"${InventoryState.ItemActionPrefix}900"), r)
        card  <- last(r)
        _     <- dao.writeReturnState(userId, Some(StateType.Dungeon))
        far   <- state.action(testUser, tap("RaceChange"), r)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        _     <- dao.writeReturnState(userId, Some(StateType.GlobalMap))
        near  <- state.action(testUser, tap("RaceChange"), r)
        scene <- dao.readSceneData(userId).map(_.flatMap(_.as[Progress].toOption))
      } yield assertTrue(card.choices.exists(ch => ch.id == "RaceChange" && ch.label == "🎭 Сменить расу")) &&
              assertTrue(card.text.contains("Купон на смену расы")) &&
              assertTrue(far == StateType.Inventory && said.contains("только в городе")) &&
              assertTrue(near == StateType.RaceQuest && scene.contains(Progress.start))
    },

    test("у Фета: купон за 100 дублонов ложится в сумку; не хватает — отказ; без хранилища вещей купона нет") {
      for {
        dao     <- TestHeroDao.withHero(userId, heroOf(Race.Human).copy(doubloons = 150L))
        inv      = TestInventoryRepository.withItems(Nil)
        r       <- TestRenderer.make
        c       <- ZIO.attempt(SceneContent.load())
        shop     = FetShopState(dao, TestArtifactRepository.empty, inv, c, Some(TestItemRepository.make))
        _       <- shop.action(testUser, tap("FetShop"), r)
        counter <- last(r)
        _       <- shop.action(testUser, tap("FetCoupon"), r)
        card    <- last(r)
        _       <- shop.action(testUser, tap("FetCouponBuy"), r)
        ask     <- last(r)
        _       <- shop.action(testUser, tap("FetCouponYes"), r)
        hero    <- dao.getHeroByUserId(userId)
        bought   = inv.snapshot.count(_.questItem.contains(QuestItemKind.RaceCoupon))
        _       <- shop.action(testUser, tap("FetCouponYes"), r)
        hero2   <- dao.getHeroByUserId(userId)
        said    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        plain    = FetShopState(dao, TestArtifactRepository.empty, inv, c)
        _       <- plain.action(testUser, tap("FetShop"), r)
        bare    <- last(r)
      } yield assertTrue(counter.choices.exists(ch => ch.id == "FetCoupon" && ch.label == "🎟 Купон на смену расы")) &&
              assertTrue(card.choices.exists(_.id == "FetCouponBuy") && ask.choices.exists(_.id == "FetCouponYes")) &&
              assertTrue(hero.exists(_.doubloons == 50L) && bought == 1) &&
              assertTrue(hero2.exists(_.doubloons == 50L) && inv.snapshot.size == 1 && said.contains("нужно 🟡 100")) &&
              assertTrue(!bare.choices.exists(_.id == "FetCoupon"))
    }
  )
}
