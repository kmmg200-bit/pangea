package pangea.service.state.states.events.cave

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.cave.{CaveRoom, CaveScene, RoomKind}
import pangea.model.hero.Hero
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Два яруса — и в заданиях на крыс и на Некроманта, и в особой пещере
  * лабиринта: спустившись, можно подняться обратно с места спуска. Верхний ярус
  * остаётся таким, каким его оставили, а нижний — каким его оставили, когда
  * поднимались: спуск ведёт туда же, а не на новый ярус. */
object CaveFloorsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, dungeonLevel = 40).copy(lvl = 40L)

  private def cave =
    for {
      dao   <- TestHeroDao.withHero(userId, hero)
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
    } yield (MonsterCaveState(dao, TestInventoryRepository.withItems(Nil), TestItemRepository.make, sched, c, None),
             dao, r)

  private def sceneOf(dao: TestHeroDao): Task[Option[CaveScene]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[CaveScene].toOption))

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  /** Верх особой пещеры: вход, комната с кучкой и травой, справа ход вниз — у
    * него и стоит герой. Опыт уже взят, пещера одурманена. */
  private def upper(monsters: Int = 3): CaveScene =
    CaveScene(
      race  = Race.Orc.entryName,
      rooms = List(
        CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
        CaveRoom(0, 1, monsters, RoomKind.Herb),
        CaveRoom(1, 0, 0, RoomKind.Stairs)),
      at = 2, inside = true, boss = Some("RatKing"), expEarned = 40L, weakened = true)

  /** Нижний ярус особой пещеры: место спуска, комната с травой, логово с королём. */
  private def lower(at: Int = 0, otherFloor: Option[CaveScene] = Some(upper())): CaveScene =
    CaveScene(
      race  = Race.Orc.entryName,
      rooms = List(
        CaveRoom(0, 0, 0, RoomKind.Empty),
        CaveRoom(0, 1, 0, RoomKind.Herb),
        CaveRoom(0, 2, 1, RoomKind.Lair)),
      at = at, inside = true, floor = 2, boss = Some("RatKing"), otherFloor = otherFloor)

  override def spec = suite("Ярусы: подняться наверх и снова спуститься")(

    test("спуск запоминает верх: на месте спуска — «Подняться наверх»; наверху всё как было, герой у хода вниз, опыт — с собой") {
      val fresh = upper()
      for {
        t <- cave
        (state, dao, r) = t
        _     <- dao.writeSceneData(userId, fresh.asJson)
        _     <- state.action(testUser, tap("CaveDown"), r)
        below <- sceneOf(dao)
        land  <- r.sentScreens.map(_.last)
        // внизу взяли ещё опыта — наверх он поднимется вместе с героем
        _     <- ZIO.foreachDiscard(below)(b => dao.writeSceneData(userId, b.copy(expEarned = 70L).asJson))
        out   <- state.action(testUser, tap("CaveUp"), r)
        up    <- sceneOf(dao)
        back  <- r.sentScreens.map(_.last)
      } yield {
        val stored  = below.exists(b => b.floor == 2 && b.at == 0 && b.canAscend && b.otherFloor.contains(fresh))
        val button  = land.choices.exists(c => c.id == "CaveUp" && c.label == "Подняться наверх")
        val asIs    = up.exists(u => u.floor == 1 && u.at == 2 && u.rooms == fresh.rooms && !u.canAscend)
        val carried = up.exists(u => u.expEarned == 70L && u.weakened && !u.rewarded)
        val keeps   = up.exists(u => u.otherFloor.exists(o => o.floor == 2 && o.otherFloor.isEmpty))
        assertTrue(stored && button && land.text.contains("ход, которым вы спустились")) &&
        assertTrue(out == StateType.MonsterCave && asIs && carried && keeps) &&
        assertTrue(back.text.contains("Здесь всё так, как вы оставили") && !back.choices.exists(_.id == "CaveUp")) &&
        // у хода вниз снова можно спуститься
        assertTrue(back.choices.exists(_.id == "CaveDown"))
      }
    },

    test("снова вниз — на тот же нижний ярус, каким его оставили, а не на новый") {
      for {
        t <- cave
        (state, dao, r) = t
        _     <- dao.writeSceneData(userId, upper().asJson)
        _     <- state.action(testUser, tap("CaveDown"), r)
        first <- sceneOf(dao)
        // внизу обыскали комнату — так её и оставили
        marked = first.map(f => f.withRoom(1, _.copy(done = true)))
        _     <- ZIO.foreachDiscard(marked)(m => dao.writeSceneData(userId, m.asJson))
        _     <- state.action(testUser, tap("CaveUp"), r)
        _     <- state.action(testUser, tap("CaveDown"), r)
        again <- sceneOf(dao)
        said  <- texts(r)
      } yield {
        val same  = marked.zip(again).exists { case (m, a) => a.rooms == m.rooms && a.floor == 2 && a.at == 0 }
        val keeps = again.exists(_.otherFloor.exists(u => u.floor == 1 && u.at == 2))
        assertTrue(same && keeps && said.contains("Лаз выводит в тесный нижний ход"))
      }
    },

    test("подняться можно только с места спуска; с нижнего яруса без запомненного верха — нельзя") {
      for {
        t <- cave
        (state, dao, r) = t
        _      <- dao.writeSceneData(userId, lower(at = 1).asJson)
        _      <- state.enter(testUser, r)
        inner  <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("CaveUp"), r)
        still  <- sceneOf(dao)
        _      <- dao.writeSceneData(userId, lower(otherFloor = None).asJson)
        _      <- state.enter(testUser, r)
        old    <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("CaveUp"), r)
        stuck  <- sceneOf(dao)
      } yield {
        val stay1 = still.exists(s => s.floor == 2 && s.at == 1)
        val stay2 = stuck.exists(s => s.floor == 2 && s.at == 0)
        assertTrue(!inner.choices.exists(_.id == "CaveUp") && stay1) &&
        assertTrue(!old.choices.exists(_.id == "CaveUp") && !old.text.contains("ход, которым вы спустились") && stay2)
      }
    },

    test("хозяин пал и награда выдана — наверху про «внизу ещё кто-то есть» уже не говорят") {
      val done = lower(otherFloor = Some(upper(monsters = 0)))
        .withRoom(2, _.copy(monsters = 0, done = true)).copy(expEarned = 50L, rewarded = true)
      for {
        t <- cave
        (state, dao, r) = t
        _     <- dao.writeSceneData(userId, done.asJson)
        _     <- state.action(testUser, tap("CaveUp"), r)
        _     <- state.enter(testUser, r)
        up    <- sceneOf(dao)
        said  <- texts(r)
      } yield {
        val paid = up.exists(u => u.floor == 1 && u.rewarded && u.cleared)
        assertTrue(paid && !said.contains("писк внизу не смолкает"))
      }
    },

    test("канализация — то же: поднялся, а ярус и задание прежние") {
      val sewer = CaveScene(
        race  = Race.Animal.entryName,
        rooms = List(
          CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
          CaveRoom(0, 1, 3, RoomKind.Empty, done = true),
          CaveRoom(1, 0, 0, RoomKind.Stairs)),
        at = 2, inside = true, sewer = true, questLvl = 7L)
      for {
        t <- cave
        (state, dao, r) = t
        _     <- dao.writeSceneData(userId, sewer.asJson)
        _     <- state.action(testUser, tap("CaveDown"), r)
        below <- sceneOf(dao)
        _     <- state.action(testUser, tap("CaveUp"), r)
        up    <- sceneOf(dao)
      } yield {
        val down  = below.exists(b => b.sewer && b.floor == 2 && b.canAscend)
        val again = up.exists(u => u.sewer && u.floor == 1 && u.questLvl == 7L && u.rooms == sewer.rooms)
        assertTrue(down && again)
      }
    },

    test("формат: сцена без запомненного яруса (как писали раньше) читается, а запомненный ярус едет туда и обратно") {
      val old     = Json.obj(
        "race"  -> Race.Orc.entryName.asJson,
        "rooms" -> List(CaveRoom(0, 0, 0, RoomKind.Empty)).asJson,
        "floor" -> 2.asJson)
      val decoded = old.as[CaveScene].toOption
      val nested  = lower()
      val round   = nested.asJson.as[CaveScene].toOption
      val noneOk  = decoded.exists(d => d.otherFloor.isEmpty && d.floor == 2 && !d.canAscend)
      val roundOk = round.contains(nested) && round.exists(_.otherFloor.contains(upper()))
      assertTrue(noneOk && roundOk)
    }
  )
}
