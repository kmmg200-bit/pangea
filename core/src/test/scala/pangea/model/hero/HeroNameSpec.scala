package pangea.model.hero

import pangea.model.hero.HeroName.Problem
import zio.test._

/** Правила имени героя: длина, буквы одного алфавита в слове, запрет имён
  * жителей Пангеи и служебных слов. */
object HeroNameSpec extends ZIOSpecDefault {

  override def spec = suite("Имя героя")(

    test("русские и латинские буквы, слова через пробел или дефис; пробелы по краям и двойные схлопываются") {
      assertTrue(HeroName.validate("Тарен") == Right("Тарен")) &&
      assertTrue(HeroName.validate("Anna-Maria") == Right("Anna-Maria")) &&
      assertTrue(HeroName.validate("  Ли   Вэй ") == Right("Ли Вэй")) &&
      // в разных словах алфавиты могут быть разными
      assertTrue(HeroName.validate("Ёжик Smith") == Right("Ёжик Smith"))
    },

    test("апостроф — как дефис: между буквами; типографский «’» с телефона — обычный «'»") {
      assertTrue(HeroName.validate("Д'Артаньян") == Right("Д'Артаньян")) &&
      assertTrue(HeroName.validate("O'Brien") == Right("O'Brien")) &&
      assertTrue(HeroName.validate("Д’Артаньян") == Right("Д'Артаньян")) &&
      assertTrue(HeroName.validate("Ли Д'Ор-Вэй") == Right("Ли Д'Ор-Вэй")) &&
      assertTrue(HeroName.validate("'Тарен") == Left(Problem.Letters)) &&
      assertTrue(HeroName.validate("Тарен'") == Left(Problem.Letters)) &&
      assertTrue(HeroName.validate("Д''Арт") == Left(Problem.Letters)) &&
      assertTrue(HeroName.validate("Д'-Арт") == Left(Problem.Letters)) &&
      // житель Пангеи за апострофом не спрячется
      assertTrue(HeroName.validate("Д'Фет") == Left(Problem.Reserved))
    },

    test("от 3 до 16 символов") {
      assertTrue(HeroName.validate("Ли") == Left(Problem.Length)) &&
      assertTrue(HeroName.validate("Абв") == Right("Абв")) &&
      assertTrue(HeroName.validate("Абвгдеёжзийклмно") == Right("Абвгдеёжзийклмно")) &&
      assertTrue(HeroName.validate("Абвгдеёжзийклмноп") == Left(Problem.Length))
    },

    test("без цифр, значков, эмодзи и смеси алфавитов в одном слове") {
      assertTrue(HeroName.validate("Воин2000") == Left(Problem.Letters)) &&
      assertTrue(HeroName.validate("Тарен!") == Left(Problem.Letters)) &&
      assertTrue(HeroName.validate("Тарен 🔥") == Left(Problem.Letters)) &&
      // «А» латинская, «дмин» — русские
      assertTrue(HeroName.validate("Aдмин") == Left(Problem.Letters)) &&
      assertTrue(HeroName.validate("Ли--Вэй") == Left(Problem.Letters)) &&
      assertTrue(HeroName.validate("-Тарен") == Left(Problem.Letters))
    },

    test("имена жителей Пангеи и служебные слова — нельзя, в любом регистре и с «ё»") {
      assertTrue(HeroName.validate("Фет") == Left(Problem.Reserved)) &&
      assertTrue(HeroName.validate("фет великий") == Left(Problem.Reserved)) &&
      assertTrue(HeroName.validate("Сэр-Ришелье") == Left(Problem.Reserved)) &&
      assertTrue(HeroName.validate("ADMIN") == Left(Problem.Reserved)) &&
      assertTrue(HeroName.validate("Фетида") == Right("Фетида"))
    }
  )
}
