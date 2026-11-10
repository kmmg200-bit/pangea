package pangea.service.donation

import pangea.engine.SceneContent
import pangea.model.hero.Achievement
import pangea.model.payment.DonationTiers
import pangea.model.user.UserId
import pangea.test.{TestFixtures, TestHeroDao}
import zio.ZIO
import zio.test._

/** Истории Ришелье о донате: своя на каждую сумму, та же сумма ещё раз —
  * «опять тебе»; закрыл последнюю из семи сумм — «Любимец Проекта». */
object DonationStorySpec extends ZIOSpecDefault {

  private val r200  = 20000L
  private val r1000 = 100000L

  override def spec = suite("Истории Ришелье о донате")(

    test("у каждой из семи сумм — своя история, и все они есть в тексте") {
      for {
        c <- ZIO.attempt(SceneContent.load())
        texts = DonationStory.Stories.keys.toList.map(a => DonationStory.text(c, a, List(a, a + 1L)))
      } yield assertTrue(DonationStory.Stories.keySet ==
                Set(20000L, 50000L, 100000L, 200000L, 400000L, 600000L, 1000000L)) &&
              assertTrue(texts.forall(_.exists(_.startsWith("💬 Ришелье:")))) &&
              assertTrue(texts.flatten.distinct.size == 7)
    },

    test("первый донат — только история: «Любимец Проекта» не за первый донат") {
      for {
        c <- ZIO.attempt(SceneContent.load())
        t  = DonationStory.text(c, r200, List(r200)).getOrElse("")
      } yield assertTrue(t.contains("Илва") && !t.contains("Любимец"))
    },

    test("последняя из семи сумм — её история и последней строкой «Любимец Проекта»; повтор после — без него") {
      val six = DonationTiers.Amounts.filterNot(_ == r1000)
      for {
        c <- ZIO.attempt(SceneContent.load())
        done  = DonationStory.text(c, r1000, six :+ r1000).getOrElse("")
        again = DonationStory.text(c, r200, six :+ r1000 :+ r200).getOrElse("")
        // шесть сумм, одна из них дважды — набор ещё не собран
        short = DonationStory.text(c, r200, six :+ r200).getOrElse("")
      } yield assertTrue(done.contains("Банк разбирал старые ячейки") &&
                done.endsWith("\n\n🏆 Вы получили достижение «Любимец Проекта». +1 ко всем характеристикам.")) &&
              assertTrue(!again.contains("Любимец") && !short.contains("Любимец"))
    },

    test("та же сумма ещё раз — «опять»; другая сумма — её история; достижения больше нет") {
      for {
        c <- ZIO.attempt(SceneContent.load())
        again = DonationStory.text(c, r200, List(r200, r200)).getOrElse("")
        other = DonationStory.text(c, r1000, List(r200, r1000)).getOrElse("")
      } yield assertTrue(again.contains("Опять на твоё имя деньги пришли") && !again.contains("Илва")) &&
              assertTrue(other.contains("Банк разбирал старые ячейки")) &&
              assertTrue(!again.contains("Любимец") && !other.contains("Любимец"))
    },

    test("чужая сумма: сказать нечего") {
      for {
        c <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(DonationStory.text(c, 9900L, List(9900L)).isEmpty)
    },

    test("«Любимец Проекта» — +1 ко всем характеристикам; дописывается один раз") {
      val hero = TestFixtures.hero(UserId(1L))
      val got  = hero.withAchievement(Achievement.ProjectFavorite)
      for {
        dao    <- TestHeroDao.withHero(UserId(1L), hero)
        first  <- dao.addAchievement(UserId(1L), Achievement.ProjectFavorite.entryName)
        second <- dao.addAchievement(UserId(1L), Achievement.ProjectFavorite.entryName)
        after  <- dao.getHeroByUserId(UserId(1L)).map(_.get)
      } yield assertTrue(got.nativeBaseStats.str == hero.nativeBaseStats.str + 1L &&
                got.nativeBaseStats.int == hero.nativeBaseStats.int + 1L) &&
              assertTrue(first && !second && after.achievements == List("ProjectFavorite"))
    }
  )
}
