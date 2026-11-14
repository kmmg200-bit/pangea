package pangea.service.state.states.village

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.loot.LootGenerator
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.Race
import pangea.model.quest.NpcQuests
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.{BanditAftermath, VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{NpcQuestLog, UserAction, VillageLair}
import pangea.test._
import zio.test._
import zio.ZIO

/** Сдавшиеся на заставе: с шансом после боя из избы выходят трое разных рас и
  * бросают оружие. Перерезать — их кошели, немного серебра; отпустить — один
  * показывает тайник с награбленным с обозов. */
object VillageSurvivorsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val Level      = 20
  private val QuestLevel = 21
  private val Silver     = 1050L

  private val scene = VillageScene(Race.Human, 0, VillageQuest.BanditLair, Level, QuestLevel, Silver,
    leader = Some("Чёрный Виктор"), leaderRace = Some(Race.Human), city = 2)
  private val Name  = "Ольховый Брод"
  private def contract(step: Int): VillageContract = VillageContract.of(scene).copy(step = step)
  private def quests(step: Int = VillageContract.Taken): NpcQuests = NpcQuests.empty.copy(village = Some(contract(step)))

  private def paper: Item = QuestItemKind.item(QuestItemKind.VillageContract).copy(id = 901L)
  private def head: Item  = QuestItemKind.item(QuestItemKind.BanditHead).copy(id = 902L)

  private def hero: Hero = TestFixtures.hero(userId, dungeonLevel = Level).copy(lvl = 22L, silver = 100L)

  private val content = ZIO.attempt(SceneContent.load())

  private def dao(q: Option[NpcQuests], data: Option[Json]) =
    for {
      d <- TestHeroDao.withHero(userId, hero)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
      _ <- ZIO.foreachDiscard(data)(d.writeSceneData(userId, _))
    } yield d

  private def lair(d: TestHeroDao, c: SceneContent) =
    VillageLairState(d, TestInventoryRepository.withItems(List(paper, head)), TestItemRepository.make, c, VillageLair.Bandits)

  private val three: List[Race] = List(Race.Goblin, Race.Orc, Race.Human)

  override def spec = suite("Логово разбойников: сдавшиеся")(

    test("после боя сдаются трое разных рас — бросок; перерезать, отпустить или в «Персонаж»") {
      for {
        c     <- content
        d     <- dao(Some(quests()), Some(VillageLairState.wonMark(VillageLair.Bandits)))
        r     <- TestRenderer.make
        // сдаются (0), трое — первые по списку смертных рас, охотников нет (99)
        _     <- TestRandom.feedInts(0, 0, 0, 0, 99)
        _     <- lair(d, c).enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
        sd    <- d.readSceneData(userId)
        first  = Race.mortals.take(3).toList
        names  = first.map(_.toString.toLowerCase)
      } yield assertTrue(VillageRates.SurvivorsPct == 40 && VillageRates.SurvivorsCount == 3) &&
              assertTrue(shown.text.contains("Голову главаря вы заворачиваете в мешковину") &&
                shown.text.contains(s"выходят трое — ${names.head}, ${names(1)} и ${names(2)}")) &&
              assertTrue(shown.choices.map(_.id) == List("BanditsSurvivorsKill", "BanditsSurvivorsSpare", "OpenCharacter")) &&
              assertTrue(sd.flatMap(_.as[BanditAftermath].toOption).contains(BanditAftermath(survivors = first)))
    },

    test("перерезать: в кошелях — 5 серебра за уровень задания с каждого; ждут охотники — дальше они") {
      for {
        c     <- content
        d     <- dao(Some(quests(VillageContract.Done)), Some(BanditAftermath(hunters = true, survivors = three).asJson))
        r     <- TestRenderer.make
        res   <- lair(d, c).action(testUser, tap("BanditsSurvivorsKill"), r)
        shown <- r.sentScreens.map(_.last)
        h     <- d.getHeroByUserId(userId).map(_.get)
        sd    <- d.readSceneData(userId)
        purse  = VillageRates.SurvivorPursePerLvl * QuestLevel * 3L
      } yield assertTrue(res == StateType.VillageBandits && purse == 315L && h.silver == hero.silver + purse) &&
              assertTrue(shown.text.contains("В кошелях у троих") && shown.text.contains(s"💰 Получено $purse серебра.") &&
                shown.text.contains("вам преграждают дорогу пятеро") && shown.choices.map(_.id).contains("BanditsHuntersGive")) &&
              assertTrue(sd.flatMap(_.as[BanditAftermath].toOption).contains(BanditAftermath(hunters = true)))
    },

    test("перерезать, а больше ничего не ждёт — к старосте") {
      for {
        c     <- content
        d     <- dao(Some(quests(VillageContract.Done)), Some(BanditAftermath(survivors = three).asJson))
        r     <- TestRenderer.make
        _     <- lair(d, c).action(testUser, tap("BanditsSurvivorsKill"), r)
        shown <- r.sentScreens.map(_.last)
        sd    <- d.readSceneData(userId)
      } yield assertTrue(shown.text.contains("В кошелях у троих") && shown.text.contains(c.text("village.lair.returnHint")) &&
                shown.choices.map(_.id) == List("BanditsOut") && sd.contains(Json.Null))
    },

    test("отпустить: первый из троих показывает тайник — 2–3 вещи награбленного, через экран добычи; вернёт сюда") {
      for {
        c     <- content
        d     <- dao(Some(quests(VillageContract.Done)), Some(BanditAftermath(hunters = true, survivors = three).asJson))
        r     <- TestRenderer.make
        res   <- lair(d, c).action(testUser, tap("BanditsSurvivorsSpare"), r)
        shown <- r.sentScreens.map(_.last)
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        h     <- d.getHeroByUserId(userId).map(_.get)
        kinds  = LootGenerator.BanditSpoils.map(_._1)
      } yield assertTrue(res == StateType.Loot && shown.text.contains("Гоблин задерживается на шаг") &&
                shown.text.contains("Под старой сосной у ворот")) &&
              assertTrue(loot.exists(l => l.items.size >= VillageRates.CacheMin && l.items.size <= VillageRates.CacheMax &&
                l.items.forall(_.material.exists(kinds.contains)) && l.silvers.isEmpty &&
                l.returnState.contains(StateType.VillageBandits) &&
                l.eventData.flatMap(_.as[BanditAftermath].toOption).contains(BanditAftermath(hunters = true)))) &&
              assertTrue(h.silver == hero.silver)
    },

    test("с экрана добычи: ждать больше нечего — к старосте; ждут охотники — они") {
      for {
        c     <- content
        d1    <- dao(Some(quests(VillageContract.Done)), Some(BanditAftermath().asJson))
        r     <- TestRenderer.make
        _     <- lair(d1, c).enter(testUser, r)
        done  <- r.sentScreens.map(_.last)
        sd    <- d1.readSceneData(userId)
        d2    <- dao(Some(quests(VillageContract.Done)), Some(BanditAftermath(hunters = true).asJson))
        _     <- lair(d2, c).enter(testUser, r)
        wait  <- r.sentScreens.map(_.last)
      } yield assertTrue(done.text.contains("Больше на заставе делать нечего") &&
                done.text.contains(s"Дорога у деревни «$Name» свободна") && done.text.contains(c.text("village.lair.returnHint")) &&
                done.choices.map(_.id) == List("BanditsOut") && sd.contains(Json.Null)) &&
              assertTrue(wait.text.contains("вам преграждают дорогу пятеро"))
    },

    test("тайник: столько вещей, сколько просили, все — с обозов") {
      val (cache, _) = LootGenerator.banditCache(3, Rng(11L))
      val kinds      = LootGenerator.BanditSpoils.map(_._1)
      assertTrue(cache.size == 3 && cache.forall(_.material.exists(kinds.contains))) &&
      assertTrue(LootGenerator.banditCache(0, Rng(1L))._1.isEmpty)
    },

    test("запись «после боя»: со сдавшимися — их расы; без поля — сдавшихся нет") {
      val a = BanditAftermath(hunters = true, survivors = three)
      assertTrue(a.asJson.as[BanditAftermath] == Right(a) && !a.done) &&
      assertTrue(Json.obj(BanditAftermath.Key -> Json.True, "banditsHunters" -> Json.True).as[BanditAftermath] ==
        Right(BanditAftermath(hunters = true))) &&
      assertTrue(BanditAftermath(survivors = three).copy(survivors = Nil).done)
    }
  )
}
