package pangea.service.state.states.arena

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.arena.ArenaFight
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Grip, Item, ItemDetails, ItemType, Rarity}
import pangea.model.skill.Skill
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.battle.BattleState
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Места на арене: каждый боец на своём месте, сам никто не ходит, а лук и его
  * умения работают так же, как в лабиринте. */
object ArenaPlacesSpec extends ZIOSpecDefault {

  private val oneId = UserId(1L)
  private val twoId = UserId(2L)
  private val one   = User(oneId, VkId("vk_one"), TelegramId("tg_one"))
  private val two   = User(twoId, VkId("vk_two"), TelegramId("tg_two"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))
  private def typed(text: String): UserAction = UserAction(text, None)

  private def content = ZIO.attempt(SceneContent.load())

  /** Боец: живучий, бьёт наверняка, чтобы ход не зависел от бросков. */
  private def fighter(id: UserId, heroId: Long, agi: Long, int: Long, weapon: Item = Item.NoItem): Hero = {
    val h = TestFixtures.hero(id)
    h.copy(
      id         = pangea.model.hero.HeroId(heroId),
      lvl        = 10L,
      baseStats  = h.baseStats.copy(agi = agi, int = int, vit = 500, str = 10),
      fightStats = FightStats(atk = 50L, hp = 100000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 1000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon))
  }

  private def bow(skill: Skill): Item =
    Item(101L, "Лук", 10L, Rarity.Blue, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      grip = Some(Grip.Bow), details = ItemDetails.Weapon(skill))

  /** Первый быстрее — он и ходит первым. */
  private def archer(skill: Skill): Hero = fighter(oneId, 1L, agi = 20, int = 20, weapon = bow(skill))
  private val swordsman: Hero = fighter(twoId, 2L, agi = 5, int = 5)

  /** Первый с Тараном на кирасе (умение под номером 202). */
  private val rammer: Hero = {
    val h     = fighter(oneId, 1L, agi = 20, int = 20)
    val chest = Item(202L, "Кираса", 10L, Rarity.Blue, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Armor(Skill.Ram))
    h.copy(equipment = h.equipment.copy(chestPlate = chest))
  }

  private def hpOf(dao: TestHeroDao, id: UserId): Task[Long] = dao.getHeroByUserId(id).map(_.get.fightStats.hp)

  private def arena(heroes: (UserId, Hero)*) =
    for {
      dao   <- TestHeroDao.make
      _     <- ZIO.foreachDiscard(heroes) { case (id, h) => dao.insertHero(h.copy(userId = id)) }
      fights = TestArenaDao.empty
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- content
    } yield (ArenaState(dao, fights, c, sched), BattleState(dao, TestInventoryRepository.accepting,
              TestItemRepository.make, c, sched, Some(fights)), dao, fights, sched, r)

  /** Двое на песке: первый записался по коду, второй его вписал. */
  private def started(first: Hero, second: Hero) =
    for {
      t <- arena(oneId -> first, twoId -> second)
      (state, _, _, fights, _, r) = t
      _ <- state.action(one, tap("ArenaByCode"), r)
      _ <- state.action(two, typed(fights.snapshot.head.code), r)
      _ <- state.action(one, tap("ArenaPoke"), r)
    } yield t

  private def battleOf(dao: TestHeroDao, id: UserId): Task[SoloPveBattle] =
    dao.readActiveBattle(id).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  /** Умения с раскачкой в тесте отдаём готовыми. */
  private def ready(dao: TestHeroDao, id: UserId): Task[Unit] =
    battleOf(dao, id).flatMap(b => dao.writeActiveBattle(id, b.copy(skillSlots = b.skillSlots.map(_.copy(cooldown = 0))).asJson))

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def fight(fights: TestArenaDao): ArenaFight = fights.snapshot.head

  override def spec = suite("Места на арене")(

    test("в начале боя бойцы стоят друг напротив друга, строй виден, сородичи не прибегают") {
      for {
        t <- started(archer(Skill.Disengage), swordsman)
        (_, battle, dao, fights, _, r) = t
        _    <- battle.enter(one, r)
        mine <- battleOf(dao, oneId)
        said <- texts(r)
      } yield assertTrue(fight(fights).a.pos == 1 && fight(fights).b.exists(_.pos == 1)) &&
              assertTrue(mine.group.heroPos == 1 && mine.group.activePos == 1 && mine.noKin) &&
              assertTrue(said.contains("1. 🟢 Вы VS 🔴"))
    },

    test("«Отскок»: стрелок отпрыгивает, и соперник видит его уже на новом месте — мечом не достать") {
      for {
        t <- started(archer(Skill.Disengage), swordsman)
        (_, battle, dao, fights, _, r) = t
        _      <- battle.action(one, aimed("Skill_101", 1), r)
        after   = fight(fights)
        _      <- r.reset
        _      <- battle.action(two, tap("ArenaPoke"), r)
        foeView <- battleOf(dao, twoId)
        told   <- texts(r)
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(after.a.pos == 3 && after.b.exists(_.pos == 1)) &&
              assertTrue(foeView.group.heroPos == 1 && foeView.group.activePos == 3) &&
              assertTrue(told.contains("соперник перешёл на позицию 3")) &&
              // мечнику до него не достать: вместо атаки — шаг или ожидание
              assertTrue(!screen.choices.exists(_.id == "Attack") && screen.choices.exists(_.id == "Move"))
    },

    test("сам на арене никто не ходит: после чужого хода соперник стоит, где стоял") {
      for {
        t <- started(archer(Skill.Disengage), swordsman)
        (_, battle, _, fights, _, r) = t
        _     <- battle.action(one, aimed("Skill_101", 1), r)
        _     <- battle.action(two, tap("ArenaPoke"), r)
        _     <- battle.action(two, tap("Wait"), r)
        after  = fight(fights)
      } yield assertTrue(after.a.pos == 3 && after.b.exists(_.pos == 1)) &&
              assertTrue(after.isTurn(oneId))
    },

    test("«Отбрасывающий выстрел» уносит соперника на другое место — и он узнаёт об этом из сводки") {
      for {
        t <- started(archer(Skill.PushShot), swordsman)
        (_, battle, dao, fights, _, r) = t
        _     <- ready(dao, oneId)
        _     <- battle.action(one, aimed("Skill_101", 1), r)
        after  = fight(fights)
        _     <- r.reset
        _     <- battle.action(two, tap("ArenaPoke"), r)
        told  <- texts(r)
      } yield assertTrue(after.a.pos == 1 && after.b.exists(_.pos == 3)) &&
              assertTrue(told.contains("вас отбросило на позицию 3"))
    },

    test("«Пригвоздить»: два своих хода соперник не двигается — ни шага, ни кнопки") {
      for {
        t <- started(archer(Skill.PinningShot), swordsman)
        (_, battle, dao, fights, _, r) = t
        _      <- ready(dao, oneId)
        _      <- battle.action(one, aimed("Skill_101", 1), r)
        pinned  = fight(fights).b.map(_.rooted)
        // отнесём соперника подальше, чтобы ему было куда шагать
        far     = fight(fights)
        _      <- fights.update(far.withSide(far.b.get.copy(pos = 4)), 0L)
        _      <- r.reset
        _      <- battle.action(two, tap("ArenaPoke"), r)
        view   <- battleOf(dao, twoId)
        told   <- texts(r)
        screen <- r.sentScreens.map(_.last)
        _      <- battle.action(two, tap("Wait"), r)
        left    = fight(fights).b.map(_.rooted)
      } yield assertTrue(pinned.contains(2) && told.contains("вас пригвоздили")) &&
              assertTrue(view.effects.heroRooted && BattleState.stepTargets(view).isEmpty) &&
              assertTrue(!screen.choices.exists(_.id == "Move")) &&
              // свой ход отстоял — путы на ход короче
              assertTrue(left.contains(1))
    },

    test("просроченный ход: достать соперника нечем — ожидание, и ход всё равно уходит") {
      for {
        t <- started(archer(Skill.Disengage), swordsman)
        (_, battle, _, fights, _, r) = t
        _     <- battle.action(one, aimed("Skill_101", 1), r)
        _     <- battle.action(two, tap("ArenaPoke"), r)
        _     <- r.reset
        _     <- battle.action(two, tap("ArenaTurn"), r)
        said  <- texts(r)
        after  = fight(fights)
      } yield assertTrue(said.contains("до соперника не достать")) &&
              assertTrue(after.isTurn(oneId) && after.lastLog.exists(_.contains("выжидает")))
    },

    test("Таран достаёт на два места: соперник на третьем — урон, и герой встаёт напротив него, о чём ему и сказано") {
      for {
        t <- started(rammer, swordsman)
        (_, battle, dao, fights, _, r) = t
        far    = fight(fights)
        _     <- fights.update(far.withSide(far.b.get.copy(pos = 3)), 0L)
        _     <- battle.action(one, tap("ArenaPoke"), r)
        _     <- ready(dao, oneId)
        hpWas <- hpOf(dao, twoId)
        _     <- r.reset
        _     <- battle.action(one, aimed("Skill_202", 3), r)
        after  = fight(fights)
        said  <- texts(r)
        hpNow <- hpOf(dao, twoId)
      } yield assertTrue(after.a.pos == 3 && after.b.exists(_.pos == 3)) &&
              assertTrue(hpNow < hpWas) &&
              // шаг — в конце хода, и строка о нём доходит до ходящего
              assertTrue(said.contains("Таран сработал: вы шагнули на место 3")) &&
              assertTrue(after.isTurn(twoId))
    },

    test("Таран на свободное место: рывок без удара — соперник цел и из сводки знает, что вы перешли") {
      for {
        t <- started(rammer, swordsman)
        (_, battle, dao, fights, _, r) = t
        _     <- ready(dao, oneId)
        _     <- battle.action(one, tap("Skill_202"), r)
        ask   <- r.sentScreens.map(_.last)
        hpWas <- hpOf(dao, twoId)
        _     <- battle.action(one, aimed("Skill_202", 2), r)
        said  <- texts(r)
        after  = fight(fights)
        hpNow <- hpOf(dao, twoId)
        _     <- r.reset
        _     <- battle.action(two, tap("ArenaPoke"), r)
        told  <- texts(r)
      } yield assertTrue(ask.text.contains("Таран — куда?")) &&
              // соперник напротив и два свободных места рядом
              assertTrue(ask.choices.filter(_.id == "Skill_202").flatMap(_.data.get("target")) == List("1", "2", "3")) &&
              assertTrue(ask.choices.exists(_.label.contains("2. — пусто —"))) &&
              assertTrue(said.contains("Таран: вы рванули на место 2.")) &&
              assertTrue(after.a.pos == 2 && after.b.exists(_.pos == 1) && hpNow == hpWas) &&
              // ход потрачен: умение на перезарядке, ход у соперника
              assertTrue(after.isTurn(twoId) && after.a.slots.exists(s => s.itemId == 202L && s.cooldown > 0)) &&
              assertTrue(told.contains("соперник перешёл на позицию 2") && !told.contains("вам нанесено"))
    },

    test("пригвождённый: Тараном бьёт, но с места не сходит — и знает почему; на пустое место не рвануть, ход цел") {
      for {
        t <- started(rammer, swordsman)
        (_, battle, dao, fights, _, r) = t
        f0    = fight(fights)
        _    <- fights.update(f0.withSide(f0.a.copy(rooted = 2)).withSide(f0.b.get.copy(pos = 2)), 0L)
        _    <- battle.action(one, tap("ArenaPoke"), r)
        _    <- ready(dao, oneId)
        _    <- r.reset
        _    <- battle.action(one, aimed("Skill_202", 3), r)
        spot <- texts(r)
        stay  = fight(fights)
        _    <- r.reset
        _    <- battle.action(one, aimed("Skill_202", 2), r)
        hit  <- texts(r)
        after = fight(fights)
      } yield assertTrue(spot.contains("с места Тараном не сойти") && stay.isTurn(oneId) && stay.a.pos == 1) &&
              assertTrue(hit.contains("врезаетесь во врага") && hit.contains("Таран не сдвинул вас с места")) &&
              assertTrue(after.a.pos == 1 && after.isTurn(twoId))
    },

    test("лук на арене слабее только вплотную: издалека — в полную силу") {
      val a = archer(Skill.Disengage)
      for {
        t <- started(a, swordsman)
        (_, _, dao, _, _, _) = t
        close <- battleOf(dao, oneId)
        far    = close.copy(group = close.group.copy(activePos = 3))
      } yield assertTrue(BattleState.rangedCloseMod(a, close) == 0.8 && BattleState.rangedCloseMod(a, far) == 1.0)
    }
  )
}
