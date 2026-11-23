package pangea.model.quest

import zio.test._

object DifficultySpec extends ZIOSpecDefault {
  override def spec = suite("Difficulty")(
    test("строка: Сложность, знаки и уровень") {
      assertTrue(Difficulty.render(1) == "Сложность: 🦴 (1 ур.)") &&
      assertTrue(Difficulty.render(7) == "Сложность: 💀🦴🦴 (7 ур.)") &&
      assertTrue(Difficulty.render(16) == "Сложность: ⚰️💀🦴 (16 ур.)") &&
      assertTrue(Difficulty.render(4) == "Сложность: 🦴💀 (4 ур.)")
    },
    test("ниже единицы — первая ступень") {
      assertTrue(Difficulty.render(0) == "Сложность: 🦴 (1 ур.)") &&
      assertTrue(Difficulty.render(-5) == "Сложность: 🦴 (1 ур.)")
    }
  )
}