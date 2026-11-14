package pangea.service.state.states.village

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.BattleAlly
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.Race
import pangea.model.quest.NpcQuests
import pangea.model.squad.{Ally, AllyKind, AllyRates, Placed, Squad, UndeadForm}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.{BanditAftermath, VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.service.state.states.battle.BattleState
import pangea.service.state.{NpcQuestLog, SquadDuty, UserAction, VillageLair, Villages}
import pangea.test._
import zio.test._
import zio.{ZIO, durationInt}

/** Пленник на заставе: с шансом после боя в погребе находится связанный пленник
  * любой из смертных рас, и он просится в отряд. Взяли — встаёт в отряд на 48
  * часов, по силе — редкий своей расы уровня задания; раса — в имени. */
object VillageCaptiveSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val Level      = 20
  private val QuestLevel = 21

  private val scene = VillageScene(Race.Human, 0, VillageQuest.BanditLair, Level, QuestLevel, 1050L,
    leader = Some("Чёрный Виктор"), leaderRace = Some(Race.Human), city = 2)
  private def quests(step: Int): NpcQuests =
    NpcQuests.empty.copy(village = Some(VillageContract.of(scene).copy(step = step)))
  private def contract: VillageContract = VillageContract.of(scene).copy(step = VillageContract.Done)

  private def paper: Item = QuestItemKind.item(QuestItemKind.VillageContract).copy(id = 901L)
  private def head: Item  = QuestItemKind.item(QuestItemKind.BanditHead).copy(id = 902L)

  private def hero(squad: Squad = Squad()): Hero =
    TestFixtures.hero(userId, dungeonLevel = Level).copy(lvl = 22L, squad = squad)

  private val content = ZIO.attempt(SceneContent.load())

  private def dao(h: Hero, q: Option[NpcQuests], data: Option[Json]) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
      _ <- ZIO.foreachDiscard(data)(d.writeSceneData(userId, _))
    } yield d

  private def lair(d: TestHeroDao, c: SceneContent) =
    VillageLairState(d, TestInventoryRepository.withItems(List(paper, head)), TestItemRepository.make, c, VillageLair.Bandits)

  private val gnome: Json = BanditAftermath(captive = Some(Race.Gnome)).asJson

  /** Крыса на сутки — чтобы занять место в строю или в резерве. */
  private def rat(pos: Int): Ally =
    Ally(AllyKind.Rat, pos, 1L, 0L, 0L, hiredUntil = Long.MaxValue, undead = Some(UndeadForm("Крыса", 1L, MonsterGenerator
      .generateOfRaceAndRarity(1, Race.Orc, pangea.model.monster.Rarity.Common).fightStats)))

  private val fullLine: List[Ally] = (2 to AllyRates.Positions).toList.map(rat)
  private val fullReserve: List[Ally] = List.fill(AllyRates.ReserveSize)(rat(0))

  override def spec = suite("Логово разбойников: пленник")(

    test("на заставе — связанный пленник (бросок): раса названа; взять, отпустить или в «Персонаж»") {
      for {
        c     <- content
        d     <- dao(hero(), Some(quests(VillageContract.Taken)), Some(VillageLairState.wonMark(VillageLair.Bandits)))
        r     <- TestRenderer.make
        // сдавшихся нет (99), пленник есть (0) — гном, седьмой из смертных рас (6), охотников нет (99)
        _     <- TestRandom.feedInts(99, 0, 6, 99)
        _     <- lair(d, c).enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
        sd    <- d.readSceneData(userId)
      } yield assertTrue(VillageRates.CaptivePct == 25 && Race.mortals.lift(6).contains(Race.Gnome)) &&
              assertTrue(shown.text.contains("Голову главаря вы заворачиваете в мешковину") &&
                shown.text.contains("сидит гном — возница с последнего обоза") && shown.text.contains("Возьмите меня с собой")) &&
              assertTrue(shown.choices.map(_.id) == List("BanditsCaptiveTake", "BanditsCaptiveSend", "OpenCharacter")) &&
              assertTrue(sd.flatMap(_.as[BanditAftermath].toOption).contains(BanditAftermath(captive = Some(Race.Gnome))))
    },

    test("взять с собой: встаёт в строй на 48 часов — по силе редкий своей расы уровня задания, раса в имени") {
      for {
        c     <- content
        _     <- TestClock.adjust(1.hour)
        d     <- dao(hero(), Some(quests(VillageContract.Done)), Some(gnome))
        r     <- TestRenderer.make
        res   <- lair(d, c).action(testUser, tap("BanditsCaptiveTake"), r)
        shown <- r.sentScreens.map(_.last)
        h     <- d.getHeroByUserId(userId).map(_.get)
        ally   = h.squad.allies.find(_.kind == AllyKind.FreedCaptive)
        stats  = MonsterGenerator.generateOfRaceAndRarity(QuestLevel, Race.Gnome, VillageRates.CaptiveRarity).fightStats
      } yield assertTrue(res == StateType.VillageBandits &&
                shown.text.contains("Освобождённый гном подбирает с земли разбойничий нож и встаёт в строй") &&
                shown.text.contains(c.text("village.lair.returnHint")) && shown.choices.map(_.id) == List("BanditsOut")) &&
              assertTrue(ally.exists(a => a.position > 0 && a.undead.exists(f => f.name == "Освобождённый гном" &&
                f.lvl == QuestLevel.toLong && f.stats == stats) && a.hp == stats.hp &&
                a.hiredUntil == 60L * 60L * 1000L + AllyRates.CaptiveMs)) &&
              assertTrue(AllyRates.CaptiveMs == 48L * 60L * 60L * 1000L)
    },

    test("строй полон — встаёт в резерв; полны и строй, и резерв — уходит домой") {
      for {
        c       <- content
        d1      <- dao(hero(Squad(allies = fullLine)), Some(quests(VillageContract.Done)), Some(gnome))
        r       <- TestRenderer.make
        _       <- lair(d1, c).action(testUser, tap("BanditsCaptiveTake"), r)
        reserve <- r.sentScreens.map(_.last)
        h1      <- d1.getHeroByUserId(userId).map(_.get)
        d2      <- dao(hero(Squad(allies = fullLine, reserve = fullReserve)), Some(quests(VillageContract.Done)), Some(gnome))
        _       <- lair(d2, c).action(testUser, tap("BanditsCaptiveTake"), r)
        noRoom  <- r.sentScreens.map(_.last)
        h2      <- d2.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(reserve.text.contains("В строю места нет — он пойдёт следом, в резерве") &&
                h1.squad.reserve.exists(_.kind == AllyKind.FreedCaptive) && !h1.squad.allies.exists(_.kind == AllyKind.FreedCaptive)) &&
              assertTrue(noRoom.text.contains("места нет ни в строю, ни в резерве") &&
                !(h2.squad.allies ++ h2.squad.reserve).exists(_.kind == AllyKind.FreedCaptive))
    },

    test("отпустить домой: уходит по тракту, в отряд не встаёт; дальше — охотники, если ждут") {
      for {
        c     <- content
        d     <- dao(hero(), Some(quests(VillageContract.Done)), Some(BanditAftermath(hunters = true, captive = Some(Race.Elf)).asJson))
        r     <- TestRenderer.make
        _     <- lair(d, c).action(testUser, tap("BanditsCaptiveSend"), r)
        shown <- r.sentScreens.map(_.last)
        h     <- d.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(shown.text.contains("Бывший пленник кивает") && shown.text.contains("вам преграждают дорогу пятеро")) &&
              assertTrue(h.squad.allies.isEmpty && h.squad.reserve.isEmpty)
    },

    test("сдавшиеся — раньше пленника: сначала их решают, потом пленник") {
      for {
        c     <- content
        d     <- dao(hero(), Some(quests(VillageContract.Done)),
                   Some(BanditAftermath(survivors = List(Race.Orc), captive = Some(Race.Elf)).asJson))
        r     <- TestRenderer.make
        st     = lair(d, c)
        _     <- st.enter(testUser, r)
        first <- r.sentScreens.map(_.last)
        _     <- st.action(testUser, tap("BanditsSurvivorsKill"), r)
        then_ <- r.sentScreens.map(_.last)
      } yield assertTrue(first.choices.map(_.id).contains("BanditsSurvivorsKill")) &&
              assertTrue(then_.text.contains("сидит эльф — возница") && then_.choices.map(_.id).contains("BanditsCaptiveTake"))
    },

    test("двое суток прошли: бывший пленник прощается и уходит из отряда") {
      val form   = Villages.freedCaptive(SceneContent.load(), contract, Race.Gnome)
      val placed = Squad().admitCaptive(form, 22L, 0L)
      for {
        c     <- content
        d     <- dao(hero(placed._1), None, None)
        r     <- TestRenderer.make
        after <- SquadDuty.settle(d, c, testUser, hero(placed._1), AllyRates.CaptiveMs + 1L, r)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        early <- SquadDuty.settle(d, c, testUser, hero(placed._1), AllyRates.CaptiveMs - 1L, r)
      } yield assertTrue(placed._2 == Placed.Line) &&
              assertTrue(said.contains("🏠 Освобождённый гном прощается: «Двое суток прошли — пора домой.") &&
                !after.squad.allies.exists(_.kind == AllyKind.FreedCaptive)) &&
              assertTrue(early.squad.allies.exists(_.kind == AllyKind.FreedCaptive))
    },

    test("в бою: зовут по имени целиком; пал — своя строка, а не «рассыпался»") {
      val form = Villages.freedCaptive(SceneContent.load(), contract, Race.Orc)
      val ally = BattleAlly(AllyKind.FreedCaptive, position = 2, hp = 0L, armor = 0L, energy = 0L, stats = form.stats,
        lvl = form.lvl, undead = Some(form), home = 2)
      assertTrue(form.name == "Освобождённый орк" && ally.callName == "Освобождённый орк") &&
      assertTrue(BattleState.goneKey(ally) == "battle.ally.captiveDown")
    },

    test("запись «после боя»: с пленником — его раса; без поля — пленника нет") {
      val a = BanditAftermath(captive = Some(Race.Khajiit))
      assertTrue(a.asJson.as[BanditAftermath] == Right(a) && !a.done) &&
      assertTrue(Json.obj(BanditAftermath.Key -> Json.True).as[BanditAftermath].exists(x => x.captive.isEmpty && x.done))
    }
  )
}
