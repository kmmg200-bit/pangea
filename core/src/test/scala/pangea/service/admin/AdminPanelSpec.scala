package pangea.service.admin

import pangea.dao.admin.{AdminDao, AdminStats, GameStats}
import pangea.model.hero.Hero
import pangea.model.item.{GemKind, Item, ItemDetails, ItemSet, ItemType, Rarity}
import pangea.model.rune.RuneData
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.stats.BaseStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.Background
import pangea.service.state.UserAction
import pangea.test._
import zio.test._
import zio.{Duration, Ref, Task, ZIO}

/** Админ-панель: вход по паролю, выдача вещей, деньги, сводка по серверу и
  * игровая статистика. Панель живёт поверх игры — ни состояние героя, ни сцена
  * от неё не меняются. */
object AdminPanelSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def text(t: String): UserAction        = UserAction(t, None)
  private def tap(id: String): UserAction        = UserAction("", Some(s"""{"action":"$id"}"""))

  private val Password = "проверочный-пароль"

  private val stubStats = AdminStats(
    heroes = 7L, activeToday = 3L, active7d = 5L,
    heroSilver = 1000L, vaultSilver = 500L, doubloons = 42L,
    levels = AdminStats.levelRows(List(3L, 17L, 40L, 99L, 150L), AdminDao.LevelStep, AdminDao.MaxLevel))

  private val stubGame = GameStats(
    heroes   = 5L,
    sets     = List(GameStats.SetRow(ItemSet.Hunter, 6, 54L), GameStats.SetRow(ItemSet.Ghoul, 8, 32L)),
    invested = BaseStats(agi = 10L, vit = 30L, str = 50L, int = 10L),
    free     = 12L,
    skills   = List(GameStats.SkillRow(Skill.CunningStrike, 3L, 1L), GameStats.SkillRow(Skill.QuickStrike, 2L, 0L)))

  /** Сводка — готовая, уровни прав — в памяти; адрес в ВК у каждого `vk<id>`. */
  private final class StubAdminDao(levels: Ref[Map[UserId, Int]]) extends AdminDao {
    def stats: Task[AdminStats] = ZIO.succeed(stubStats)
    def gameStats: Task[GameStats] = ZIO.succeed(stubGame)
    def level(userId: UserId): Task[Int] = levels.get.map(_.getOrElse(userId, 0))
    def setLevel(userId: UserId, level: Int): Task[Unit] =
      levels.update(m => if (level <= 0) m - userId else m.updated(userId, level))
    def withLevel(min: Int): Task[List[(UserId, VkId)]] =
      levels.get.map(_.toList.collect { case (id, l) if l >= min => (id, VkId(s"vk${id.value}")) }.sortBy(_._1.value))
  }

  /** Имена из ВК без пауз: в тестах время не идёт. */
  private def names(dao: TestHeroDao) =
    for {
      users   <- TestUserRepository.withUser(testUser)
      api     <- TestApi.make
      bg      <- Background.make(Background.Grace)
      running <- Ref.make(false)
    } yield new NameBackfill(dao, users, api, bg, running, Duration.Zero)

  private def panel(hero: Hero = TestFixtures.hero(userId), password: Option[String] = Some(Password)) =
    for {
      dao      <- TestHeroDao.withHero(userId, hero)
      inv       = TestInventoryRepository.accepting
      sessions <- Ref.make(Map.empty[UserId, AdminScreen])
      levels   <- Ref.make(Map.empty[UserId, Int])
      r        <- TestRenderer.make
      backfill <- names(dao)
    } yield (new AdminPanelLive(dao, inv, TestItemRepository.make, new StubAdminDao(levels), Map.empty,
               AdminConfig(password), sessions, backfill), dao, inv, r)

  /** Панель с заданными уровнями прав: их видно и после нажатий. */
  private def leveled(start: Map[UserId, Int], password: Option[String] = Some(Password)) =
    for {
      dao      <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
      sessions <- Ref.make(Map.empty[UserId, AdminScreen])
      levels   <- Ref.make(start)
      r        <- TestRenderer.make
      backfill <- names(dao)
    } yield (new AdminPanelLive(dao, TestInventoryRepository.accepting, TestItemRepository.make, new StubAdminDao(levels),
               Map.empty, AdminConfig(password), sessions, backfill), levels, r)

  private def said(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  /** Войти в панель и оказаться на главном экране. */
  private def enter(p: AdminPanelLive, r: TestRenderer): Task[Unit] =
    p.intercept(testUser, TestFixtures.hero(userId), text("/admin"), r) *>
      p.intercept(testUser, TestFixtures.hero(userId), text(Password), r).unit

  override def spec = suite("Админ-панель")(

    test("«/admin» просит пароль; верный пускает, неверный закрывает") {
      for {
        t <- panel()
        (p, _, _, r) = t
        opened <- p.intercept(testUser, TestFixtures.hero(userId), text("/admin"), r)
        ask    <- r.sentScreens.map(_.last)
        ok     <- p.intercept(testUser, TestFixtures.hero(userId), text(Password), r)
        main   <- r.sentScreens.map(_.last)
        // после закрытия сессии обычный текст панели уже не достаётся
        t2 <- panel()
        (p2, _, _, r2) = t2
        _      <- p2.intercept(testUser, TestFixtures.hero(userId), text("/admin"), r2)
        _      <- p2.intercept(testUser, TestFixtures.hero(userId), text("не тот"), r2)
        after  <- p2.intercept(testUser, TestFixtures.hero(userId), text("что-нибудь"), r2)
        wrong  <- said(r2)
      } yield assertTrue(opened && ok) &&
              assertTrue(ask.text.contains("Пришлите пароль")) &&
              assertTrue(main.choices.map(_.id) ==
                List("AdminItems", "AdminMoney", "AdminStats", "AdminGameStats", AdminPanel.Level1Id, AdminPanel.NamesId,
                     AdminPanel.CancelId)) &&
              assertTrue(wrong.contains("Неверно") && !after)
    },

    test("«Админ 1»: нажал — права и вход без пароля; нажал ещё раз — права сняты, пароль снова нужен") {
      val hero = TestFixtures.hero(userId)
      for {
        t <- leveled(Map.empty)
        (p, levels, r) = t
        _       <- enter(p, r)
        before  <- r.sentScreens.map(_.last)
        _       <- p.intercept(testUser, hero, tap(AdminPanel.Level1Id), r)
        granted <- said(r)
        marked  <- r.sentScreens.map(_.last)
        lvl1    <- levels.get
        // вышел и зашёл снова — пароля уже не спрашивают
        _       <- p.intercept(testUser, hero, tap(AdminPanel.CancelId), r)
        _       <- p.intercept(testUser, hero, text("/admin"), r)
        again   <- r.sentScreens.map(_.last)
        // нажал ещё раз — права сняты
        _       <- p.intercept(testUser, hero, tap(AdminPanel.Level1Id), r)
        revoked <- r.sentScreens.map(_.map(_.text))
        lvl0    <- levels.get
        _       <- p.intercept(testUser, hero, tap(AdminPanel.CancelId), r)
        _       <- p.intercept(testUser, hero, text("/admin"), r)
        ask     <- r.sentScreens.map(_.last)
      } yield assertTrue(before.choices.find(_.id == AdminPanel.Level1Id).exists(_.label == "Админ 1")) &&
              assertTrue(granted.contains(AdminPanel.Level1Granted) && lvl1 == Map(userId -> AdminPanel.Level1)) &&
              assertTrue(marked.choices.find(_.id == AdminPanel.Level1Id).exists(_.label == "✅ Админ 1")) &&
              assertTrue(again.text == AdminPanel.MainText) &&
              assertTrue(revoked.contains(AdminPanel.Level1Revoked) && lvl0.isEmpty) &&
              assertTrue(ask.text.contains("Пришлите пароль"))
    },

    test("выключенная панель (пароль не задан) не пускает и «Админ 1»") {
      for {
        t <- leveled(Map(userId -> AdminPanel.Level1), password = None)
        (p, _, r) = t
        _    <- p.intercept(testUser, TestFixtures.hero(userId), text("/admin"), r)
        ask  <- r.sentScreens.map(_.last)
        _    <- p.intercept(testUser, TestFixtures.hero(userId), text("что угодно"), r)
        says <- said(r)
      } yield assertTrue(ask.text.contains("Пришлите пароль") && says.contains(AdminConfig.EnvName))
    },

    test("кому пересылать /home и подтверждённый /restart: всем «Админ 1», кроме самого игрока") {
      val other = UserId(2L)
      for {
        t <- leveled(Map(userId -> AdminPanel.Level1, other -> AdminPanel.Level1, UserId(3L) -> 0))
        (p, _, _) = t
        fromPlayer <- p.homeWatchers(UserId(9L))
        fromAdmin  <- p.homeWatchers(userId)
      } yield assertTrue(fromPlayer == List(VkId("vk1"), VkId("vk2"))) &&
              assertTrue(fromAdmin == List(VkId("vk2")))
    },

    test("без ADMIN_PASSWORD панель не пускает никого") {
      for {
        t <- panel(password = None)
        (p, _, _, r) = t
        _    <- p.intercept(testUser, TestFixtures.hero(userId), text("/admin"), r)
        _    <- p.intercept(testUser, TestFixtures.hero(userId), text("любой пароль"), r)
        says <- said(r)
        // сессии больше нет: панель отдала ввод игре
        more <- p.intercept(testUser, TestFixtures.hero(userId), text("дальше"), r)
      } yield assertTrue(says.contains(AdminConfig.EnvName) && !more)
    },

    test("«/home» сильнее панели: сессия закрывается, ввод уходит игре") {
      for {
        t <- panel()
        (p, _, _, r) = t
        _      <- enter(p, r)
        escape <- p.intercept(testUser, TestFixtures.hero(userId), text("/home"), r)
        after  <- p.intercept(testUser, TestFixtures.hero(userId), text("что-нибудь"), r)
      } yield assertTrue(!escape && !after)
    },

    test("каталог: раздел, страница и выдача — вещь ложится в сумку") {
      for {
        t <- panel()
        (p, _, inv, r) = t
        hero = TestFixtures.hero(userId)
        _     <- enter(p, r)
        _     <- p.intercept(testUser, hero, tap("AdminItems"), r)
        _     <- p.intercept(testUser, hero, tap("AdminSections"), r)
        list  <- r.sentScreens.map(_.last)
        _     <- p.intercept(testUser, hero, tap(s"${AdminPanel.SectionPrefix}gem"), r)
        gems  <- r.sentScreens.map(_.last)
        _     <- p.intercept(testUser, hero, tap(AdminPanel.NextId), r)
        page2 <- r.sentScreens.map(_.last)
        _     <- p.intercept(testUser, hero, tap(s"${AdminPanel.GivePrefix}gem:${GemKind.Ruby.entryName}:3"), r)
        given <- said(r)
      } yield assertTrue(list.choices.exists(_.id == s"${AdminPanel.SectionPrefix}gem")) &&
              assertTrue(gems.text.contains("стр. 1/") && page2.text.contains("стр. 2/")) &&
              assertTrue(given.contains("Выдано")) &&
              assertTrue(inv.snapshot.exists(_.gem.exists(g => g.kind == GemKind.Ruby && g.grade == 3))) &&
              // все кнопки панели укладываются в клавиатуру ВК
              assertTrue(gems.choices.flatMap(_.row).groupBy(identity).forall(_._2.size <= 5))
    },

    test("поиск по куску названия находит вещь и выдаёт её") {
      for {
        t <- panel()
        (p, _, inv, r) = t
        hero = TestFixtures.hero(userId)
        _     <- enter(p, r)
        _     <- p.intercept(testUser, hero, tap("AdminItems"), r)
        _     <- p.intercept(testUser, hero, text("Живая вода"), r)
        found <- r.sentScreens.map(_.last)
        id     = found.choices.find(_.id.startsWith(AdminPanel.GivePrefix)).map(_.id).getOrElse("")
        _     <- p.intercept(testUser, hero, tap(id), r)
        // и по чепухе ничего не находится
        _     <- p.intercept(testUser, hero, text("абырвалг"), r)
        empty <- r.sentScreens.map(_.last)
      } yield assertTrue(found.text.contains("Найдено")) &&
              assertTrue(inv.snapshot.exists(_.brew.exists(_.label == "Живая вода"))) &&
              assertTrue(empty.text.contains("ничего не нашлось"))
    },

    test("экипировка собирается по слоту, редкости и уровню") {
      for {
        t <- panel()
        (p, _, inv, r) = t
        hero = TestFixtures.hero(userId)
        _    <- enter(p, r)
        _    <- p.intercept(testUser, hero, tap("AdminItems"), r)
        _    <- p.intercept(testUser, hero, tap("AdminEquip"), r)
        _    <- p.intercept(testUser, hero, tap(s"${AdminPanel.TypePrefix}${ItemType.Weapon.entryName}"), r)
        _    <- p.intercept(testUser, hero, tap(s"${AdminPanel.RarityPrefix}${Rarity.Orange.entryName}"), r)
        ask  <- r.sentScreens.map(_.last)
        // уровень пишем числом
        _    <- p.intercept(testUser, hero, text("75"), r)
        made  = inv.snapshot.headOption
        // чепуха и запредельный уровень не проходят, экран остаётся на месте
        _     <- p.intercept(testUser, hero, text("сто"), r)
        _     <- p.intercept(testUser, hero, text("500"), r)
        scold <- said(r)
      } yield assertTrue(ask.text.contains("пришлите уровень числом")) &&
              assertTrue(made.exists(i => i.itemType == ItemType.Weapon && i.rarity == Rarity.Orange && i.lvl == 75L)) &&
              assertTrue(scold.contains(AdminPanel.BadLevel)) &&
              // ни одна из двух неудачных попыток вещь не создала
              assertTrue(inv.snapshot.size == 1)
    },

    test("деньги начисляются кнопками номиналов") {
      val rich = TestFixtures.hero(userId).copy(silver = 100L, doubloons = 5L)
      for {
        t <- panel(rich)
        (p, dao, _, r) = t
        _     <- enter(p, r)
        _     <- p.intercept(testUser, rich, tap("AdminMoney"), r)
        money <- r.sentScreens.map(_.last)
        _     <- p.intercept(testUser, rich, tap(s"${AdminPanel.SilverPrefix}10000"), r)
        _     <- p.intercept(testUser, rich, tap(s"${AdminPanel.DoubloonPrefix}100"), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(money.choices.count(_.id.startsWith(AdminPanel.SilverPrefix)) == 3) &&
              assertTrue(after.silver == 10100L && after.doubloons == 105L)
    },

    test("сводка по серверу показывает игроков, деньги и уровни; наборы — в игровой статистике") {
      for {
        t <- panel()
        (p, _, _, r) = t
        _     <- enter(p, r)
        _     <- p.intercept(testUser, TestFixtures.hero(userId), tap("AdminStats"), r)
        stats <- r.sentScreens.map(_.last)
      } yield assertTrue(stats.text.contains("всего героев: 7")) &&
              assertTrue(stats.text.contains("заходили сегодня (с 00:00 МСК): 3") && stats.text.contains("заходили за неделю: 5")) &&
              assertTrue(stats.text.contains("серебро всего: 1 500") && stats.text.contains("дублоны: 42")) &&
              assertTrue(!stats.text.contains("Наборы") && !stats.text.contains("Охотник на 6")) &&
              // полосы уровней по 25, включая пустые
              assertTrue(stats.text.contains("1–25: 2") && stats.text.contains("26–50: 1")) &&
              assertTrue(stats.text.contains("51–75: 0") && stats.text.contains("126–150: 1"))
    },

    test("игровая статистика: наборы, вложенные очки с долями, свободные очки и умения с клеймами") {
      for {
        t <- panel()
        (p, _, _, r) = t
        _    <- enter(p, r)
        _    <- p.intercept(testUser, TestFixtures.hero(userId), tap("AdminGameStats"), r)
        game <- r.sentScreens.map(_.last)
      } yield assertTrue(game.text.contains("Считаем тех, кто заходил за неделю: 5")) &&
              assertTrue(game.text.contains("Охотник на 6: 54") && game.text.contains("Упырь на 8: 32")) &&
              assertTrue(game.text.contains("Сила: 50 (50%)") && game.text.contains("Телосложение: 30 (30%)") &&
                         game.text.contains("Ловкость: 10 (10%)") && game.text.contains("Интеллект: 10 (10%)")) &&
              assertTrue(game.text.contains("не распределено: 12")) &&
              assertTrue(game.text.contains(s"${Skill.CunningStrike.label}: 3 (на теле — 1)")) &&
              assertTrue(game.text.contains(s"${Skill.QuickStrike.label}: 2") &&
                         !game.text.contains(s"${Skill.QuickStrike.label}: 2 (")) &&
              assertTrue(game.choices.map(_.id) == List("AdminGameStats", "AdminMain"))
    },

    test("игровая статистика пустая — так и сказано, без деления на ноль") {
      val text = AdminPanel.gameStatsText(GameStats.empty)
      assertTrue(text.contains("никто не собрал и двух предметов набора")) &&
      assertTrue(text.contains("никто ещё не вкладывал очки") && text.contains("не распределено: 0")) &&
      assertTrue(text.contains("ни у кого нет активных умений"))
    },

    test("вложенные очки — только то, что выше старта; свободные — суммой") {
      val s    = Hero.StartBaseStats
      val none = RuneData()
      val g = GameStats.of(List(
        GameStats.HeroRow(TestFixtures.emptyEquipment, s, 4L, none),
        GameStats.HeroRow(TestFixtures.emptyEquipment, s.copy(str = s.str + 6L, vit = s.vit + 2L), 0L, none),
        GameStats.HeroRow(TestFixtures.emptyEquipment, s.copy(int = s.int + 3L), 8L, none)), AdminDao.MinWorn)
      assertTrue(g.heroes == 3L && g.free == 12L) &&
      assertTrue(g.invested == BaseStats(agi = 0L, vit = 2L, str = 6L, int = 3L)) &&
      // ниже старта не бывает, но и в минус счёт не уходит
      assertTrue(GameStats.invested(List(s.copy(agi = 1L))) == GameStats.Zero)
    },

    test("умения на героях — как в бою: два кинжала с одним умением — одно; клеймо — с пометкой «на теле»") {
      def dagger(id: Long, skill: Skill): Item =
        Item(id, "Кинжал", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0,
          armor = 0, defence = 0, evasion = 0, details = ItemDetails.Weapon(skill))
      val twoDaggers = TestFixtures.emptyEquipment.copy(
        weapon = dagger(1L, Skill.CunningStrike), leftHand = dagger(2L, Skill.CunningStrike))
      val bare = TestFixtures.emptyEquipment
      val rows = GameStats.skillRows(List(
        twoDaggers -> RuneData(),                                              // с двух кинжалов — одно
        twoDaggers -> RuneData(active = List(Skill.CunningStrike.entryName)),  // и на вещи, и клеймом
        bare       -> RuneData(active = List(Skill.QuickStrike.entryName)),    // только клеймом
        bare       -> RuneData()))                                             // умений нет — не в счёте
      assertTrue(rows == List(
        GameStats.SkillRow(Skill.CunningStrike, 2L, 1L),
        GameStats.SkillRow(Skill.QuickStrike, 1L, 1L)))
    },

    test("панель не трогает ни состояние героя, ни сцену") {
      val busy = TestFixtures.hero(userId, state = StateType.Battle)
      for {
        t <- panel(busy)
        (p, dao, _, r) = t
        _     <- dao.writeSceneData(userId, io.circe.Json.obj("важное" -> io.circe.Json.fromString("значение")))
        _     <- enter(p, r)
        _     <- p.intercept(testUser, busy, tap("AdminMoney"), r)
        _     <- p.intercept(testUser, busy, tap(s"${AdminPanel.SilverPrefix}1000"), r)
        _     <- p.intercept(testUser, busy, tap(AdminPanel.CancelId), r)
        hero  <- dao.getHeroByUserId(userId).map(_.get)
        scene <- dao.readSceneData(userId)
      } yield assertTrue(hero.state == StateType.Battle) &&
              assertTrue(scene.exists(_.hcursor.get[String]("важное").contains("значение")))
    },

    test("уровни живых героев раскладываются по полосам в 25 уровней") {
      val rows = AdminStats.levelRows(List(1L, 25L, 26L, 150L, 151L), AdminDao.LevelStep, AdminDao.MaxLevel)
      assertTrue(rows.map(r => (r.from, r.to)) ==
        List((1L, 25L), (26L, 50L), (51L, 75L), (76L, 100L), (101L, 125L), (126L, 150L))) &&
      assertTrue(rows.head.heroes == 2L) &&                       // 1 и 25
      assertTrue(rows(1).heroes == 1L) &&                          // 26
      assertTrue(rows.last.heroes == 1L) &&                        // 150; 151 за потолком и не в счёт
      assertTrue(rows.map(_.heroes).sum == 4L)
    },

    test("пароль не попадает в лог входящих сообщений") {
      val secret = "s3cret"
      assertTrue(AdminPanel.maskSecrets(secret, Some(secret)) == AdminPanel.Masked) &&
      // с пробелами по краям — тоже пароль
      assertTrue(AdminPanel.maskSecrets(s"  $secret ", Some(secret)) == AdminPanel.Masked) &&
      // прочие сообщения в логе остаются как есть
      assertTrue(AdminPanel.maskSecrets("/admin", Some(secret)) == "/admin") &&
      assertTrue(AdminPanel.maskSecrets("обычное сообщение", Some(secret)) == "обычное сообщение") &&
      assertTrue(AdminPanel.maskSecrets(secret, None) == secret)
    },

    test("наборы считаются по надетому, одиночный предмет не в счёт") {
      val six   = TestFixtures.wearingSet(ItemSet.Hunter, 6)
      val eight = TestFixtures.wearingSet(ItemSet.Ghoul, 8)
      val one   = TestFixtures.wearingSet(ItemSet.Hunter, 1)
      val rows  = GameStats.setRows(List(six, six, eight, one), AdminDao.MinWorn)
      assertTrue(rows.exists(r => r.set == ItemSet.Hunter && r.worn == 6 && r.heroes == 2)) &&
      assertTrue(rows.exists(r => r.set == ItemSet.Ghoul && r.worn == 8 && r.heroes == 1)) &&
      assertTrue(!rows.exists(_.worn < AdminDao.MinWorn))
    }
  )
}
