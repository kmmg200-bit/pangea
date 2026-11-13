package pangea.service.state.states.events.cave

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.cave.{CaveRoom, CaveScene, RoomKind, SewerRates}
import pangea.model.hero.Hero
import pangea.model.monster.{MiniBoss, Race}
import pangea.model.quest.{BoardData, BoardDesk, BoardKind, BoardSlot}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Особая пещера лабиринта: хозяин — Крысиный король или Некромант — ждёт
  * ярусом ниже. Наверху в самой дальней комнате ход к нему, внизу — короткий ход
  * и логово. Платят за пещеру, только когда хозяин пал. */
object CaveDeepLairSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(lvl: Long = 40L): Hero =
    TestFixtures.hero(userId, dungeonLevel = 40).copy(lvl = lvl)

  private def cave(h: Hero = hero()) =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
    } yield (MonsterCaveState(dao, TestInventoryRepository.withItems(Nil), TestItemRepository.make, sched, c, None),
             dao, r)

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def sceneOf(dao: TestHeroDao): Task[Option[CaveScene]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[CaveScene].toOption))

  /** Пещера лабиринта на три комнаты: вход, за ним комната с `monsters` мобами,
    * справа — комната `kind` (в ней `lair` обитателей). Хозяин — `boss`. */
  private def smallCave(boss: String = "RatKing", kind: RoomKind = RoomKind.Stairs, monsters: Int = 0,
                        floor: Int = 1, lair: Int = 0): CaveScene =
    CaveScene(
      race  = Race.Orc.entryName,
      rooms = List(
        CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
        CaveRoom(0, 1, monsters, RoomKind.Empty, done = true),
        CaveRoom(1, 0, lair, kind)),
      at = 0, inside = true, floor = floor, boss = Some(boss))

  /** Доска, где взято «Выбить пещеру». */
  private val caveTaken: BoardData =
    BoardData(0L, "novice", List(BoardSlot(BoardKind.CaveClear, taken = true)))

  private def board(dao: TestHeroDao): Task[Option[BoardSlot]] =
    dao.readQuestData(userId).map(_.flatMap(_.as[BoardDesk].toOption).flatMap(_.boards.headOption)
      .flatMap(_.slots.find(_.kind == BoardKind.CaveClear)))

  override def spec = suite("Особая пещера: хозяин ярусом ниже")(

    test("ход вниз: короткий нижний ярус с логовом в конце; опыт, дурман и отрава идут вниз вместе с героем") {
      for {
        t <- cave()
        (state, dao, r) = t
        _     <- dao.writeSceneData(userId,
                   smallCave(monsters = 3).copy(at = 2, expEarned = 40L, weakened = true, poisoned = true).asJson)
        _     <- state.enter(testUser, r)
        room  <- r.sentScreens.map(_.last)
        down  <- state.action(testUser, tap("CaveDown"), r)
        deep  <- sceneOf(dao)
        said  <- texts(r)
      } yield assertTrue(room.text.contains("узкий лаз")) &&
              assertTrue(room.choices.exists(c => c.id == "CaveDown" && c.label == "Спуститься ниже")) &&
              assertTrue(down == StateType.MonsterCave && said.contains("Лаз выводит в тесный нижний ход")) &&
              assertTrue(deep.exists(d => d.floor == 2 && !d.away && d.at == 0 && d.race == Race.Orc.entryName)) &&
              assertTrue(deep.exists(d => d.boss.contains("RatKing") && d.rooms.lastOption.exists(_.kind == RoomKind.Lair))) &&
              assertTrue(deep.exists(d => d.expEarned == 40L && d.weakened && d.poisoned && !d.rewarded))
    },

    test("ход вниз сторожат — спуститься нельзя, пока они живы; в обычной пещере хода вниз нет вовсе") {
      for {
        t <- cave()
        (state, dao, r) = t
        guarded = smallCave().copy(at = 2, rooms = smallCave().rooms.updated(2, CaveRoom(1, 0, 3, RoomKind.Stairs)))
        _     <- dao.writeSceneData(userId, guarded.asJson)
        _     <- state.action(testUser, tap("CaveDown"), r)
        still <- sceneOf(dao)
        plain  = smallCave(kind = RoomKind.Empty).copy(boss = None, at = 2)
        _     <- dao.writeSceneData(userId, plain.asJson)
        _     <- state.action(testUser, tap("CaveDown"), r)
        flat  <- sceneOf(dao)
      } yield assertTrue(still.exists(s => s.floor == 1 && s.hasStairs)) &&
              assertTrue(flat.exists(s => s.floor == 1 && s.rooms.size == 3))
    },

    test("выбитый верх особой пещеры не платит: Некромант ждёт внизу, и задание доски не закрыто") {
      for {
        t <- cave()
        (state, dao, r) = t
        _     <- dao.writeQuestData(userId, caveTaken.asJson)
        _     <- dao.writeSceneData(userId, smallCave(boss = "Necromancer").copy(expEarned = 50L).asJson)
        _     <- state.enter(testUser, r)
        said  <- texts(r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        scene <- sceneOf(dao)
        slot  <- board(dao)
      } yield assertTrue(said.contains("тот, кто поднимал их, внизу")) &&
              assertTrue(after.exp == 0L && scene.exists(!_.rewarded)) &&
              assertTrue(slot.exists(s => s.taken && !s.done))
    },

    test("логово внизу выбито — пещера платит за всё и закрывает «Выбить пещеру»") {
      for {
        t <- cave()
        (state, dao, r) = t
        _     <- dao.writeQuestData(userId, caveTaken.asJson)
        _     <- dao.writeSceneData(userId,
                   smallCave(kind = RoomKind.Lair, floor = 2).copy(expEarned = 50L).asJson)
        _     <- state.enter(testUser, r)
        said  <- texts(r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        slot  <- board(dao)
      } yield assertTrue(said.contains("+80 опыта") && after.exp == 80L) &&
              assertTrue(said.contains("теперь всё чисто")) &&
              assertTrue(slot.exists(_.done))
    },

    test("особая пещера, начатая до нижних ярусов (логово на том же ярусе), доигрывается как раньше") {
      for {
        t <- cave()
        (state, dao, r) = t
        old    = smallCave(kind = RoomKind.Lair).copy(expEarned = 50L)
        _     <- dao.writeSceneData(userId, old.asJson)
        _     <- state.enter(testUser, r)
        said  <- texts(r)
        after <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(old.lastFloor && !old.hasStairs) &&
              assertTrue(said.contains("+80 опыта") && after.exp == 80L)
    },

    test("в логове нижнего яруса — бой с хозяином, уровень мобов — от лабиринта") {
      for {
        t <- cave(hero(lvl = 10L))
        (state, dao, r) = t
        _      <- dao.writeSceneData(userId,
                    smallCave(kind = RoomKind.Lair, floor = 2, lair = SewerRates.KingCount).asJson)
        out    <- state.action(testUser, tap("CaveRight"), r)
        battle <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        said   <- texts(r)
      } yield assertTrue(out == StateType.Battle && said.contains("Крысиный король")) &&
              assertTrue(battle.exists(_.boss.contains(MiniBoss.RatKing))) &&
              assertTrue(battle.exists(b => b.noKin && b.minionLvl == 40L))
    }
  )
}
