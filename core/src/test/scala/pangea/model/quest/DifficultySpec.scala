package pangea.model.quest

import zio.test._

object DifficultySpec extends ZIOSpecDefault {
  override def spec = suite("Difficulty")(
    test("уровень сложности — текстом") {
      assertTrue(Difficulty.render(1) == "сложность 1 ур.") &&
      assertTrue(Difficulty.render(7) == "сложность 7 ур.") &&
      assertTrue(Difficulty.render(46) == "сложность 46 ур.")
    },
    test("ниже единицы — первый уровень") {
      assertTrue(Difficulty.render(0) == "сложность 1 ур.") &&
      assertTrue(Difficulty.render(-5) == "сложность 1 ур.")
    }
  )
}