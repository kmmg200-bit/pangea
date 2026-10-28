package pangea.service.admin

import pangea.model.hero.HeroId
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test._
import zio.test._
import zio.{Duration, Ref, ZIO}

/** Имена из ВК тем, кто прошёл пролог до имён: первое подходящее и свободное
  * из «Имя Фамилия», «Имя», «Имя Ф». ВК в тестах всех зовёт «Иван Иванов». */
object NameBackfillSpec extends ZIOSpecDefault {

  private def user(n: Long): User = User(UserId(n), VkId(s"vk$n"), TelegramId(s"tg$n"))

  override def spec = suite("Имена из ВК")(

    test("кандидаты — по порядку и только те, что проходят правила имени") {
      assertTrue(NameBackfill.candidates("Иван", "Иванов") == List("Иван Иванов", "Иван", "Иван И")) &&
      // «Имя Фамилия» длиннее 16 — остаются короткие
      assertTrue(NameBackfill.candidates("Александра", "Константинопольская") == List("Александра", "Александра К")) &&
      // фамилии нет — «Имя» один раз
      assertTrue(NameBackfill.candidates("Иван", "") == List("Иван")) &&
      assertTrue(NameBackfill.candidates("✨Ксю✨", "Иванова").isEmpty) &&
      // «Ли» короче трёх — его пропускаем
      assertTrue(NameBackfill.candidates("Ли", "Ли") == List("Ли Ли", "Ли Л"))
    },

    test("безымянным — первое свободное; названных и тех, кто в прологе, не трогает; всё занято — без имени") {
      val ids = (1L to 6L).toList
      for {
        dao     <- TestHeroDao.make
        // второй уже назван, третий ещё в прологе — выберет имя сам
        _       <- ZIO.foreachDiscard(ids)(n =>
                     dao.insertHero(TestFixtures.hero(UserId(n), state = if (n == 3L) StateType.Registration else StateType.GlobalMap)
                       .copy(id = HeroId(n), name = Option.when(n == 2L)("Тарен"))))
        users   <- TestUserRepository.withUsers(ids.map(user): _*)
        api     <- TestApi.make
        running <- Ref.make(false)
        report  <- new NameBackfill(dao, users, api, running, Duration.Zero).run
        names   <- ZIO.foreach(ids)(n => dao.getHeroByUserId(UserId(n)).map(_.flatMap(_.name)))
        again   <- new NameBackfill(dao, users, api, running, Duration.Zero).run
      } yield assertTrue(names == List(Some("Иван Иванов"), Some("Тарен"), None, Some("Иван"), Some("Иван И"), None)) &&
              assertTrue(report == NameBackfill.Report(named = 3, skipped = 1, failed = 0)) &&
              // повторный запуск — только по тем, кто так и остался без имени
              assertTrue(again == NameBackfill.Report(named = 0, skipped = 1, failed = 0))
    }
  )
}
