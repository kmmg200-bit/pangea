package pangea.service.miniapp

import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.squad.{AllyKind, Squad, UndeadForm}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.squad.SquadOrders
import pangea.test.{TestFixtures, TestHeroDao}
import zio.test._
import zio.{Ref, ZIO}

/** Отряд из приложения: общие с ботом [[SquadOrders]] и то, что приложение
  * добавляет сверху, — запрет в бою и строки итога в ответ, а не в чат. */
object MiniAppSquadSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)
  private val user   = User(userId, VkId("1"), TelegramId("-1"))
  private val lvl    = 10L

  private val hero: Hero = TestFixtures.hero(userId, state = StateType.Dungeon).copy(lvl = lvl,
    squad = Squad.empty.hire(AllyKind.Human, lvl, 0L).hire(AllyKind.Gnome, lvl, 0L))

  private def run(h: Hero)(order: (SquadOrders, Hero, MiniApp.Collect) => zio.Task[Option[Squad]]) =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      c     <- ZIO.attempt(SceneContent.load())
      lines <- Ref.make(Vector.empty[String])
      res   <- order(new SquadOrders(dao, c), h, new MiniApp.Collect(lines))
      saved <- dao.getHeroByUserId(userId).someOrFailException.map(_.squad)
      said  <- lines.get
    } yield (res, saved, said, c)

  def spec = suite("MiniApp: отряд")(
    test("переставить союзника на место героя: меняются местами, строка — в ответ приложению") {
      val human = hero.squad.allies.find(_.kind == AllyKind.Human).map(_.position).getOrElse(0)
      val heroAt = hero.squad.heroPos
      for {
        t <- run(hero)((o, h, say) => o.move(user, h, human, heroAt, say))
        (res, saved, said, c) = t
        name = hero.squad.allyAt(human).map(_.name).getOrElse("")
      } yield assertTrue(
        res.contains(saved),
        saved.heroPos == human,
        saved.allyAt(heroAt).exists(_.kind == AllyKind.Human),
        said == Vector(c.format("squad.moved", "name" -> name, "n" -> heroAt.toString))
      )
    },
    test("выгнать; на пустом месте — ни о чём: ничего не записано и не сказано") {
      val gnome = hero.squad.allies.find(_.kind == AllyKind.Gnome).map(_.position).getOrElse(0)
      for {
        t1 <- run(hero)((o, h, say) => o.dismiss(user, h, gnome, say))
        (res, saved, said, _) = t1
        t2 <- run(hero)((o, h, say) => o.dismiss(user, h, 11, say))
        (none, untouched, silent, _) = t2
      } yield assertTrue(
        res.isDefined && saved.allies.map(_.kind) == List(AllyKind.Human) && said.size == 1,
        none.isEmpty && untouched == hero.squad && silent.isEmpty
      )
    },
    test("восставший: занимает место названного или оседает прахом; не ждёт никто — ни о чём") {
      val form    = UndeadForm("Гоблин немощный раб", 16L, AllyKind.Human.stats(3L))
      val waiting = hero.copy(squad = hero.squad.copy(pendingRisen = Some(form)))
      val gnome   = hero.squad.allies.find(_.kind == AllyKind.Gnome).map(_.position).getOrElse(0)
      for {
        t1 <- run(waiting)((o, h, say) => o.takeRisenPlace(user, h, gnome, say))
        (took, after1, said1, _) = t1
        t2 <- run(waiting)((o, h, say) => o.dismissRisen(user, h, say))
        (dust, after2, said2, c) = t2
        t3 <- run(hero)((o, h, say) => o.dismissRisen(user, h, say))
        (none, _, silent, _) = t3
      } yield assertTrue(
        took.isDefined && after1.pendingRisen.isEmpty,
        after1.allyAt(gnome).exists(_.name == form.name) && !after1.allies.exists(_.kind == AllyKind.Gnome),
        said1.size == 1 && said1.head.contains(form.name),
        dust.isDefined && after2.pendingRisen.isEmpty && after2.allies.size == 2,
        said2 == Vector(c.text("squad.risenDust")),
        none.isEmpty && silent.isEmpty
      )
    },
    test("ждёт Тёмный волк, а свой в отряде уже есть: названный остаётся, волки дерутся — новый на месте прежнего") {
      def wolf(l: Long) = UndeadForm(UndeadForm.DarkWolfName, l, AllyKind.Human.stats(3L))
      val withOld = hero.squad.raise(wolf(4L), lvl, 0L)
      val waiting = hero.copy(squad = withOld.copy(pendingRisen = Some(wolf(9L))))
      val oldAt   = withOld.darkWolves.map(_.position).headOption.getOrElse(0)
      val gnome   = hero.squad.allies.find(_.kind == AllyKind.Gnome).map(_.position).getOrElse(0)
      for {
        t <- run(waiting)((o, h, say) => o.takeRisenPlace(user, h, gnome, say))
        (took, after, said, c) = t
      } yield assertTrue(
        took.isDefined && after.pendingRisen.isEmpty,
        after.darkWolves.map(w => w.position -> w.lvlAt(lvl)) == List(oldAt -> 9L),
        after.allies.exists(_.kind == AllyKind.Gnome) && after.allies.size == 3,
        said == Vector(c.format("squad.wolfFight", "name" -> UndeadForm.DarkWolfName))
      )
    },
    test("в бою строй не трогают: вид говорит почему") {
      val c = SceneContent.load()
      assertTrue(
        MiniAppView.squad(hero.copy(state = StateType.Battle), c, 0L).locked.contains(c.text("miniapp.squad.inBattle")),
        MiniAppView.squad(hero, c, 0L).locked.isEmpty
      )
    },
    test("резерв из приложения: в резерв и обратно на свободное место — те же строки, что в боте") {
      val human = hero.squad.allies.find(_.kind == AllyKind.Human).map(_.position).getOrElse(0)
      val name  = AllyKind.Human.name
      for {
        t1 <- run(hero)((o, h, say) => o.toReserve(user, h, human, say))
        (sent, after1, said1, c) = t1
        t2 <- run(hero.copy(squad = after1))((o, h, say) => o.deploy(user, h, 0, name, None, say))
        (back, after2, said2, _) = t2
      } yield assertTrue(
        sent.isDefined && after1.allyAt(human).isEmpty && after1.reserve.map(_.kind) == List(AllyKind.Human),
        said1 == Vector(c.format("squad.toReserveDone", "name" -> name)),
        back.isDefined && after2.reserve.isEmpty && after2.allyAt(human).exists(_.kind == AllyKind.Human),
        said2 == Vector(c.format("squad.deployed", "name" -> name, "n" -> human.toString))
      )
    },
    test("резерв из приложения: строй полон — меняются с названным; чужое имя на номере — ни о чём; выгнать") {
      val orc     = UndeadForm("Поднятый орк", 3L, AllyKind.Human.stats(3L))
      // Йорген и Брамбл в строю, ещё восемь поднятых — строй полон; в резерве орк
      val full    = (1 to 8).foldLeft(hero.squad)((s, i) => s.raise(orc.copy(name = s"Поднятый $i"), lvl, 0L))
      val crowded = hero.copy(squad = full.copy(reserve = List(
        pangea.model.squad.Ally(AllyKind.Undead, 0, 1L, 0L, 0L, hiredUntil = Long.MaxValue, undead = Some(orc)))))
      val human   = crowded.squad.allies.find(_.kind == AllyKind.Human).map(_.position).getOrElse(0)
      for {
        t1 <- run(crowded)((o, h, say) => o.deploy(user, h, 0, "Поднятый орк", None, say))
        (noRoom, _, quiet, _) = t1
        t2 <- run(crowded)((o, h, say) => o.deploy(user, h, 0, "Брамбл Медноус", Some(human), say))
        (wrong, _, quiet2, _) = t2
        t3 <- run(crowded)((o, h, say) => o.deploy(user, h, 0, "Поднятый орк", Some(human), say))
        (swapped, after, said, c) = t3
        t4 <- run(crowded)((o, h, say) => o.dismissReserve(user, h, 0, "Поднятый орк", say))
        (dropped, after4, said4, _) = t4
      } yield assertTrue(
        crowded.squad.full,
        noRoom.isEmpty && quiet.isEmpty && wrong.isEmpty && quiet2.isEmpty,
        swapped.isDefined && after.allyAt(human).exists(_.name == "Поднятый орк"),
        after.reserve.map(_.kind) == List(AllyKind.Human),
        said == Vector(c.format("squad.swappedIn", "name" -> "Поднятый орк", "gone" -> AllyKind.Human.name,
          "n" -> human.toString)),
        dropped.isDefined && after4.reserve.isEmpty && said4 == Vector(c.format("squad.dismissed", "name" -> "Поднятый орк"))
      )
    }
  )
}
