package pangea.model.top

import pangea.model.hero.HeroId
import pangea.model.monster.Race
import pangea.model.user.{UserId, VkId}
import zio.test._

object TopBoardSpec extends ZIOSpecDefault {

  private def row(id: Long) = TopRow(HeroId(id), UserId(id), VkId(id.toString), None, Race.Human, 1L, 1L)

  def spec = suite("TopBoard")(
    test("места — по порядку топа; кого в топе нет, тот стоит на месте, которое занял бы") {
      // 4 — админ: в топ не идёт, но место своё видит.
      val order = Vector((HeroId(2L), true, 9L), (HeroId(4L), false, 7L), (HeroId(5L), true, 7L), (HeroId(1L), true, 0L))
      val board = TopBoard.build(order, List(row(1L), row(5L), row(2L)), size = 2)
      assertTrue(
        board.rows.map(_.heroId) == List(HeroId(2L), HeroId(5L)),
        board.places == Map(HeroId(2L) -> TopStanding(1L, 9L), HeroId(4L) -> TopStanding(2L, 7L),
          HeroId(5L) -> TopStanding(2L, 7L), HeroId(1L) -> TopStanding(3L, 0L)),
        board.total == 3L
      )
    },
    test("нужны строки только первых из топа; пропавшего героя в первых нет") {
      val order = Vector((HeroId(9L), false, 1L), (HeroId(3L), true, 1L), (HeroId(8L), true, 1L), (HeroId(6L), true, 1L))
      val board = TopBoard.build(order, List(row(3L)), size = 2)
      assertTrue(
        TopBoard.firsts(order, 2) == List(HeroId(3L), HeroId(8L)),
        board.rows.map(_.heroId) == List(HeroId(3L)),
        TopBoard.build(Vector.empty, Nil, 100) == TopBoard(Nil, Map.empty, 0L)
      )
    }
  )
}
