package pangea.service.state.states.arena

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.arena.{ArenaFight, ArenaMode}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Grip, Item, ItemDetails, ItemType, Rarity}
import pangea.model.skill.Skill
import pangea.model.squad.{AllyKind, Squad}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.battle.BattleState
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Режим группового боя: режим выбирается первым, очередь у каждого своя, и в
  * групповом бою каждый выходит со своим отрядом — а отряд в «Отряде» после
  * боя прежний. */
object ArenaGroupSpec extends ZIOSpecDefault {

  private val oneId = UserId(1L)
  private val twoId = UserId(2L)
  private val one   = User(oneId, VkId("vk_one"), TelegramId("tg_one"))
  private val two   = User(twoId, VkId("vk_two"), TelegramId("tg_two"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def inMode(key: String, mode: ArenaMode): UserAction =
    UserAction("", Some(s"""{"action":"$key","mode":"${mode.entryName}"}"""))
  private def aimed(key: String, target: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","target":"$target"}"""))
  private def typed(text: String): UserAction = UserAction(text, None)

  private def content = ZIO.attempt(SceneContent.load())

  private def fighter(id: UserId, heroId: Long, agi: Long, int: Long, squad: Squad = Squad.empty): Hero = {
    val h = TestFixtures.hero(id)
    h.copy(
      id         = pangea.model.hero.HeroId(heroId),
      lvl        = 10L,
      baseStats  = h.baseStats.copy(agi = agi, int = int, vit = 500, str = 10),
      fightStats = FightStats(atk = 50L, hp = 100000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 1000),
      equipment  = TestFixtures.emptyEquipment,
      squad      = squad)
  }

  private val withOne = Squad.empty.hire(AllyKind.Human, 10L, 0L)
  private val withTwo = Squad.empty.hire(AllyKind.Human, 10L, 0L).hire(AllyKind.Gnome, 10L, 0L)

  /** Умение нагрудника под номером 202. */
  private def withChest(h: Hero, skill: Skill): Hero =
    h.copy(equipment = h.equipment.copy(chestPlate = Item(202L, "Кираса", 10L, Rarity.Blue, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, details = ItemDetails.Armor(skill))))

  /** Лук с умением под номером 101. */
  private def withBow(h: Hero, skill: Skill): Hero =
    h.copy(equipment = h.equipment.copy(weapon = Item(101L, "Лук", 10L, Rarity.Blue, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      grip = Some(Grip.Bow), details = ItemDetails.Weapon(skill))))

  /** Двое в групповом бою: первый записался, второй вписал код, первого позвали. */
  private def startedGroup(first: Hero, second: Hero) =
    for {
      t <- arena(oneId -> first, twoId -> second)
      (state, _, _, fights, _, r) = t
      _ <- state.action(one, inMode("ArenaByCode", ArenaMode.Group), r)
      _ <- state.action(two, typed(fight(fights).code), r)
      _ <- state.action(one, tap("ArenaPoke"), r)
    } yield t

  /** Умения с раскачкой в тесте отдаём готовыми. */
  private def ready(dao: TestHeroDao, id: UserId): Task[Unit] =
    battleOf(dao, id).flatMap(b => dao.writeActiveBattle(id, b.copy(skillSlots = b.skillSlots.map(_.copy(cooldown = 0))).asJson))

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

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

  private def battleOf(dao: TestHeroDao, id: UserId): Task[SoloPveBattle] =
    dao.readActiveBattle(id).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def fight(fights: TestArenaDao): ArenaFight = fights.snapshot.head

  override def spec = suite("Групповой бой на арене")(

    test("сначала выбирается режим, и дальше он едет в кнопках") {
      for {
        t <- arena(oneId -> fighter(oneId, 1L, 10, 10))
        (state, _, _, _, _, r) = t
        _    <- state.enter(one, r)
        menu <- r.sentScreens.map(_.last)
        _    <- state.action(one, tap("ArenaGroup"), r)
        mode <- r.sentScreens.map(_.last)
      } yield assertTrue(menu.choices.map(_.id).take(2) == List("ArenaSolo", "ArenaGroup")) &&
              assertTrue(mode.text.contains("Групповой бой")) &&
              assertTrue(mode.choices.filter(c => c.id == "ArenaNearest" || c.id == "ArenaByCode")
                           .forall(_.data.get("mode").contains("Group")))
    },

    test("очередь у каждого режима своя; в групповой видно, сколько бойцов выйдет с ждущим") {
      for {
        t <- arena(oneId -> fighter(oneId, 1L, 10, 10), twoId -> fighter(twoId, 2L, 5, 5, withTwo))
        (state, _, _, fights, _, r) = t
        _     <- state.action(two, inMode("ArenaNearest", ArenaMode.Group), r)
        _     <- state.action(one, inMode("ArenaNearest", ArenaMode.Solo), r)
        solo  <- r.sentScreens.map(_.last)
        _     <- state.action(one, inMode("ArenaNearest", ArenaMode.Group), r)
        group <- r.sentScreens.map(_.last)
        mine   = fights.snapshot.find(_.has(oneId))
      } yield assertTrue(!solo.choices.exists(_.id.startsWith(ArenaState.JoinPrefix))) &&
              assertTrue(group.choices.exists(c => c.id.startsWith(ArenaState.JoinPrefix) && c.label.contains("отряд: 2"))) &&
              // ждал один на один — нажал групповой: запись перешла туда же
              assertTrue(mine.exists(_.mode == ArenaMode.Group))
    },

    test("по записи — тот же код в другом режиме: запись переходит, код прежний") {
      for {
        t <- arena(oneId -> fighter(oneId, 1L, 10, 10))
        (state, _, _, fights, _, r) = t
        _     <- state.action(one, inMode("ArenaByCode", ArenaMode.Solo), r)
        first  = fight(fights)
        _     <- state.action(one, inMode("ArenaByCode", ArenaMode.Group), r)
        after  = fight(fights)
        said  <- r.sentScreens.map(_.last.text)
      } yield assertTrue(first.mode == ArenaMode.Solo && after.mode == ArenaMode.Group && after.code == first.code) &&
              assertTrue(said.contains("групповой бой"))
    },

    test("групповой бой: оба выходят со своими отрядами, по их местам; а отряд в «Отряде» после ран прежний") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20, squad = withOne)
      val slow  = fighter(twoId, 2L, agi = 5, int = 5, squad = withTwo)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, battle, dao, fights, _, r) = t
        _       <- state.action(one, inMode("ArenaByCode", ArenaMode.Group), r)
        _       <- state.action(two, typed(fight(fights).code), r)
        started  = fight(fights)
        _       <- state.action(one, tap("ArenaPoke"), r)
        mine    <- battleOf(dao, oneId)
        // первый бьёт союзника второго, стоящего на втором месте
        _       <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
        _       <- battle.action(one, aimed("Attack", 2), r)
        hurt     = fight(fights).b.flatMap(_.allies.find(_.ally.position == 2)).map(_.ally.hp)
        squadNow <- dao.getHeroByUserId(twoId).map(_.get.squad)
      } yield assertTrue(started.mode == ArenaMode.Group) &&
              assertTrue(started.a.allies.map(_.ally.kind) == List(AllyKind.Human)) &&
              assertTrue(started.b.exists(_.allies.map(_.ally.kind).toSet == Set[AllyKind](AllyKind.Human, AllyKind.Gnome))) &&
              // у первого в бою: свой Йорген — союзник, отряд второго — «мобы» под номерами
              assertTrue(mine.group.allies.map(_.kind) == List(AllyKind.Human)) &&
              assertTrue(mine.group.entries.map(_._2.arenaUnit).toSet == Set[Option[Int]](Some(1), Some(2))) &&
              // рана легла на сторону второго, а в «Отряде» у него всё как было
              assertTrue(hurt.exists(hp => started.b.flatMap(_.allies.find(_.ally.position == 2)).exists(_.ally.hp > hp))) &&
              assertTrue(squadNow == slow.squad)
    },

    test("Таран по союзнику соперника: герой встаёт напротив него, свой союзник — на место героя; соперник читает правду") {
      val ram  = withChest(fighter(oneId, 1L, agi = 20, int = 20, squad = withOne), Skill.Ram)
      val slow = fighter(twoId, 2L, agi = 5, int = 5, squad = withTwo)
      for {
        t <- startedGroup(ram, slow)
        (_, battle, dao, fights, _, r) = t
        _     <- ready(dao, oneId)
        _     <- r.reset
        // напротив союзника второго (место 2) стоит свой Йорген — он уступит место
        _     <- battle.action(one, aimed("Skill_202", 2), r)
        said  <- texts(r)
        after  = fight(fights)
        _     <- r.reset
        _     <- battle.action(two, tap("ArenaPoke"), r)
        told  <- texts(r)
      } yield assertTrue(after.a.pos == 2 && after.a.allies.map(_.ally.position) == List(1)) &&
              assertTrue(after.b.exists(_.pos == 1)) &&
              assertTrue(said.contains("Таран сработал: вы шагнули на место 2")) &&
              // в полях теперь его союзник, а ход передан ему самому
              assertTrue(said.contains("Ход передан сопернику (Безымянный боец)")) &&
              assertTrue(told.contains("соперник перешёл на позицию 2") && !told.contains("вас отбросило"))
    },

    test("«Отбросить» самого соперника: он отлетает в конец строя — и узнаёт об этом из сводки") {
      val shover = withChest(fighter(oneId, 1L, agi = 20, int = 20, squad = withOne), Skill.Shove)
      val slow   = fighter(twoId, 2L, agi = 5, int = 5, squad = withTwo)
      for {
        t <- startedGroup(shover, slow)
        (_, battle, dao, fights, _, r) = t
        _     <- ready(dao, oneId)
        _     <- battle.action(one, aimed("Skill_202", 1), r)
        after  = fight(fights)
        _     <- r.reset
        _     <- battle.action(two, tap("ArenaPoke"), r)
        told  <- texts(r)
      } yield assertTrue(after.b.exists(_.pos == 3)) &&
              // на его место вышел последний из отряда
              assertTrue(after.b.exists(_.allies.exists(a => a.ally.kind == AllyKind.Gnome && a.ally.position == 1))) &&
              assertTrue(told.contains("вас отбросило на позицию 3"))
    },

    test("«Заградительный залп» держит и отряд соперника: в его ход задетый союзник не шагает, а после хода путы спадают") {
      val archer = withBow(fighter(oneId, 1L, agi = 20, int = 20), Skill.Barrage)
      // у второго Йорген стоит через место: бить ему некого, и в свой ход он шагнул бы к врагу
      val slow   = fighter(twoId, 2L, agi = 5, int = 5, squad = withOne.move(AllyKind.Human, 3))
      def jorgen(f: ArenaFight) = f.b.flatMap(_.allies.find(_.ally.kind == AllyKind.Human)).map(_.ally)
      for {
        t <- startedGroup(archer, slow)
        (_, battle, dao, fights, _, r) = t
        _      <- ready(dao, oneId)
        _      <- battle.action(one, aimed("Skill_101", 1), r)
        pinned  = jorgen(fight(fights))
        _      <- r.reset
        _      <- battle.action(two, tap("ArenaPoke"), r)
        lines  <- r.sentScreens.map(_.flatMap(_.text.linesIterator))
        _      <- battle.action(two, tap("Wait"), r)
        stood   = jorgen(fight(fights))
        // путы спали: следующим своим ходом он идёт к врагу
        _      <- battle.action(one, tap("Wait"), r)
        _      <- battle.action(two, tap("ArenaPoke"), r)
        _      <- battle.action(two, tap("Wait"), r)
        walked  = jorgen(fight(fights))
      } yield assertTrue(pinned.exists(a => a.rooted == 1 && a.position == 3)) &&
              // в своём строю второй видит его с булавкой
              assertTrue(lines.exists(l => l.contains("Йорген Кремень") && l.contains("📌"))) &&
              assertTrue(stood.exists(a => a.rooted == 0 && a.position == 3)) &&
              assertTrue(walked.exists(_.position == 2))
    },

    test("один на один отряд остаётся за оградой") {
      val quick = fighter(oneId, 1L, agi = 20, int = 20, squad = withOne)
      val slow  = fighter(twoId, 2L, agi = 5, int = 5, squad = withTwo)
      for {
        t <- arena(oneId -> quick, twoId -> slow)
        (state, _, _, fights, _, r) = t
        _ <- state.action(one, inMode("ArenaByCode", ArenaMode.Solo), r)
        _ <- state.action(two, typed(fight(fights).code), r)
      } yield assertTrue(fight(fights).a.allies.isEmpty && fight(fights).b.exists(_.allies.isEmpty))
    }
  )
}
