package pangea.service.state.states.village

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.NpcQuests
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.{BanditAftermath, VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.{NpcQuestLog, UserAction, VillageLair, Villages}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Логово разбойников после боя: голову главаря забирают с собой, староста
  * доплачивает за неё 5 дублонов. С шансом на выходе с заставы ждут охотники за
  * головами — отдать им голову или драться. */
object VillageBanditsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val Level      = 20
  private val QuestLevel = 21
  private val Silver     = 1050L
  private val Leader     = "Чёрный Виктор"

  // Деревня людей (Ольховый Брод), город — Каэлис; главарь — человек.
  private val scene = VillageScene(Race.Human, 0, VillageQuest.BanditLair, Level, QuestLevel, Silver,
    leader = Some(Leader), leaderRace = Some(Race.Human), city = 2)
  private val Name  = "Ольховый Брод"
  private def contract(step: Int = VillageContract.Taken): VillageContract = VillageContract.of(scene).copy(step = step)
  private def quests(step: Int = VillageContract.Taken): NpcQuests = NpcQuests.empty.copy(village = Some(contract(step)))

  private def paper: Item = QuestItemKind.item(QuestItemKind.VillageContract).copy(id = 901L)
  private def head: Item  = QuestItemKind.item(QuestItemKind.BanditHead).copy(id = 902L)

  private def hero: Hero = TestFixtures.hero(userId, dungeonLevel = Level).copy(lvl = 22L, silver = 100L, doubloons = 2L)

  private val content = ZIO.attempt(SceneContent.load())

  private def dao(q: Option[NpcQuests], data: Option[Json] = None) =
    for {
      d <- TestHeroDao.withHero(userId, hero)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
      _ <- ZIO.foreachDiscard(data)(d.writeSceneData(userId, _))
    } yield d

  private def lair(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent) =
    VillageLairState(d, inv, TestItemRepository.make, c, VillageLair.Bandits)

  private def heads(inv: TestInventoryRepository): Int = inv.snapshot.count(_.questItem.contains(QuestItemKind.BanditHead))

  private def battleOf(d: TestHeroDao): Task[Option[SoloPveBattle]] =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private val waiting: Json = BanditAftermath(hunters = true).asJson

  override def spec = suite("Логово разбойников: голова главаря и охотники")(

    test("староста просит и голову: за неё — ещё 5 дублонов; на карточке контракта — тоже") {
      for {
        c <- content
        plea = c.format("village.plea.bandits.text", Villages.args(c, scene): _*)
        card = Villages.cardLine(c, quests())
      } yield assertTrue(VillageRates.HeadDoubloons == 5L) &&
              assertTrue(plea.contains("Принесёте мне голову главаря — доплачу 5 дублонов") && plea.contains("в городе Каэлис")) &&
              assertTrue(card.contains(s"Плата — $Silver серебра, за голову главаря — ещё 5 дублонов"))
    },

    test("банда перебита, охотников нет: голова в сумке, контракт сделан; карточка головы — к старосте") {
      for {
        c     <- content
        d     <- dao(Some(quests()), Some(VillageLairState.wonMark(VillageLair.Bandits)))
        inv    = TestInventoryRepository.withItems(List(paper))
        r     <- TestRenderer.make
        _     <- TestRandom.feedInts(99)
        _     <- lair(d, inv, c).enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
        q     <- NpcQuestLog.load(d, userId)
        line   = Villages.headLine(c, q)
        held   = inv.snapshot.find(_.questItem.contains(QuestItemKind.BanditHead))
        _     <- d.writeReturnState(userId, Some(StateType.Dungeon))
        st     = InventoryState(d, inv, TestItemRepository.make, c)
        _     <- ZIO.foreachDiscard(held)(h =>
                   st.action(testUser, UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}${h.id}"}""")), r))
        card  <- r.sentScreens.map(_.last)
      } yield assertTrue(shown.text.contains("Голову главаря вы заворачиваете в мешковину") &&
                shown.text.contains(c.text("village.lair.returnHint")) && shown.choices.map(_.id) == List("BanditsOut")) &&
              assertTrue(heads(inv) == 1 && q.village.exists(_.done)) &&
              assertTrue(line.contains(s"Голова главаря с заставы у деревни людей «$Name» — $Leader") &&
                line.contains("Староста доплатит за неё 5 дублонов")) &&
              assertTrue(card.text.contains("Голова главаря") && card.text.contains("Староста доплатит за неё 5 дублонов") &&
                card.choices.exists(_.id == "VillageReturn"))
    },

    test("на выходе — охотники за головами: требуют голову; отдать, драться или в «Персонаж»; контракт уже сделан") {
      for {
        c     <- content
        d     <- dao(Some(quests()), Some(VillageLairState.wonMark(VillageLair.Bandits)))
        inv    = TestInventoryRepository.withItems(List(paper))
        r     <- TestRenderer.make
        _     <- TestRandom.feedInts(0)
        _     <- lair(d, inv, c).enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
        sd    <- d.readSceneData(userId)
        q     <- NpcQuestLog.load(d, userId)
        _     <- lair(d, inv, c).enter(testUser, r)
        again <- r.sentScreens.map(_.last)
      } yield assertTrue(VillageRates.HuntersPct == 35) &&
              assertTrue(shown.text.contains("Голову главаря вы заворачиваете в мешковину") &&
                shown.text.contains("вам преграждают дорогу пятеро") && shown.text.contains(s"«$Leader? Мы шли по этому следу") &&
                shown.text.contains("Сбежать из этого боя не получится")) &&
              assertTrue(shown.choices.map(_.id) == List("BanditsHuntersGive", "BanditsHuntersFight", "OpenCharacter")) &&
              assertTrue(sd.flatMap(_.as[BanditAftermath].toOption).contains(BanditAftermath(hunters = true)) &&
                q.village.exists(_.done) && heads(inv) == 1) &&
              // вернулись из меню — охотники всё там же
              assertTrue(again.text.contains("вам преграждают дорогу пятеро") && !again.text.contains("мешковину"))
    },

    test("отдать голову: охотники уходят, головы в сумке нет — к старосте без доплаты") {
      for {
        c     <- content
        d     <- dao(Some(quests(VillageContract.Done)), Some(waiting))
        inv    = TestInventoryRepository.withItems(List(paper, head))
        r     <- TestRenderer.make
        res   <- lair(d, inv, c).action(testUser, tap("BanditsHuntersGive"), r)
        shown <- r.sentScreens.map(_.last)
        sd    <- d.readSceneData(userId)
      } yield assertTrue(res == StateType.VillageBandits && shown.text.contains("Вы бросаете мешок вожаку") &&
                shown.text.contains(c.text("village.lair.returnHint")) && shown.choices.map(_.id) == List("BanditsOut")) &&
              assertTrue(heads(inv) == 0 && inv.snapshot.exists(_.questItem.contains(QuestItemKind.VillageContract)) &&
                sd.contains(Json.Null))
    },

    test("драться: сюжетный бой — пятеро охотников уровня задания, вожак-именной последним; добыча вернёт сюда с отметкой") {
      for {
        c     <- content
        d     <- dao(Some(quests(VillageContract.Done)), Some(waiting))
        inv    = TestInventoryRepository.withItems(List(paper, head))
        r     <- TestRenderer.make
        res   <- lair(d, inv, c).action(testUser, tap("BanditsHuntersFight"), r)
        shown <- r.sentScreens.map(_.last)
        b     <- battleOf(d)
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        races  = b.toList.flatMap(x => x.monsterRace :: (x.group.others ++ x.group.queue).map(_.race))
      } yield assertTrue(res == StateType.Battle && shown.text.contains("вожак взводит арбалет")) &&
              assertTrue(b.exists(x => x.story.contains(Villages.HuntersStory) && x.lootLevel.contains(QuestLevel.toLong) && x.cantFlee)) &&
              assertTrue(races.size == VillageRates.HuntersCount + 1) &&
              assertTrue(loot.exists(l => l.returnState.contains(StateType.VillageBandits) &&
                l.eventData.exists(VillageLairState.huntersBeaten))) &&
              assertTrue(BattleState.RaidStories.contains(Villages.HuntersStory))
    },

    test("охотники: четверо покрепче — сброд смертных рас, вожак-именной последним; все уровня задания") {
      val crews = (1L to 30L).toList.map(s => Villages.bountyHunters(contract(), Rng(s)))
      assertTrue(crews.forall(g => g.size == VillageRates.HuntersCount + 1 && g.forall(_.lvl == QuestLevel.toLong))) &&
      assertTrue(crews.flatten.forall(m => Race.mortals.contains(m.race))) &&
      assertTrue(crews.forall(g => g.take(VillageRates.HuntersCount).forall(m => VillageRates.HuntersRarities.contains(m.rarity)))) &&
      assertTrue(crews.flatMap(_.lastOption).forall(m => m.rarity == Rarity.Legendary && m.customName.exists(_.nonEmpty)))
    },

    test("охотники перебиты: голова ваша — к старосте; в логове потом пусто") {
      for {
        c     <- content
        d     <- dao(Some(quests(VillageContract.Done)),
                   Some(BanditAftermath().asJson.deepMerge(VillageLairState.huntersBeatenMark)))
        inv    = TestInventoryRepository.withItems(List(paper, head))
        r     <- TestRenderer.make
        st     = lair(d, inv, c)
        _     <- st.enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
        sd    <- d.readSceneData(userId)
        _     <- st.enter(testUser, r)
        again <- r.sentScreens.map(_.last)
      } yield assertTrue(shown.text.contains("Последний охотник падает в дорожную пыль") &&
                shown.text.contains(c.text("village.lair.returnHint")) && shown.choices.map(_.id) == List("BanditsOut")) &&
              assertTrue(heads(inv) == 1 && sd.contains(Json.Null) && again.text == c.text("village.bandits.gone"))
    },

    test("пока охотники ждут, «Уйти» и «В лабиринт» — старые кнопки: охотники всё там же") {
      for {
        c     <- content
        d     <- dao(Some(quests(VillageContract.Done)), Some(waiting))
        r     <- TestRenderer.make
        st     = lair(d, TestInventoryRepository.withItems(List(paper, head)), c)
        left  <- st.action(testUser, tap("BanditsLeave"), r)
        out   <- st.action(testUser, tap("BanditsOut"), r)
        shown <- r.sentScreens.map(_.last)
        sd    <- d.readSceneData(userId)
      } yield assertTrue(left == StateType.VillageBandits && out == StateType.VillageBandits &&
                shown.text.contains("вам преграждают дорогу пятеро") && sd.contains(waiting))
    },

    test("пал в бою с охотниками: голову забрали, логово разбито — к старосте без доплаты") {
      for {
        c   <- content
        d   <- dao(Some(quests(VillageContract.Done)))
        inv  = TestInventoryRepository.withItems(List(paper, head))
        r   <- TestRenderer.make
        _   <- d.writeActiveBattle(userId, SoloPveBattle.fromGroup(Villages.bountyHunters(contract(), Rng(1L)), hero, Nil)
                 .copy(story = Some(Villages.HuntersStory)).asJson)
        _   <- DeathState(d, inv, c).enter(testUser, r)
        sd  <- d.readSceneData(userId)
        q   <- NpcQuestLog.load(d, userId)
      } yield assertTrue(sd.flatMap(_.hcursor.get[List[String]]("wakeLines").toOption).contains(List("village.bandits.hunters.death"))) &&
              assertTrue(heads(inv) == 0 && Villages.toPay(q)) &&
              assertTrue(c.text("village.bandits.hunters.death").contains("Мешка с головой нет"))
    },

    test("плата: с головой — ещё 5 дублонов и строка о ней, голова уходит из сумки; без головы — как было") {
      def payOff(items: List[Item]) =
        for {
          c    <- content
          d    <- dao(Some(quests(VillageContract.Done)))
          inv   = TestInventoryRepository.withItems(items)
          r    <- TestRenderer.make
          _    <- VillageState(d, inv, TestItemRepository.make, c).enter(testUser, r)
          paid <- r.sentScreens.map(_.last)
          h    <- d.getHeroByUserId(userId).map(_.get)
        } yield (paid, h, inv)
      for {
        withHead <- payOff(List(paper, head))
        bare     <- payOff(List(paper))
      } yield assertTrue(withHead._1.text.contains("Мешок с головой главаря староста принимает молча") &&
                withHead._1.text.contains(s"💰 Получено $Silver серебра и 5 дублонов.")) &&
              assertTrue(withHead._2.doubloons == hero.doubloons + 5L && withHead._2.silver == hero.silver + Silver &&
                withHead._3.snapshot.isEmpty) &&
              assertTrue(!bare._1.text.contains("Мешок с головой") && bare._1.text.contains(s"💰 Получено $Silver серебра.") &&
                bare._2.doubloons == hero.doubloons)
    },

    test("отказ от контракта уносит и голову; у гнезда сетеплётов головы не бывает") {
      for {
        c   <- content
        d   <- dao(Some(quests()))
        inv  = TestInventoryRepository.withItems(List(paper, head))
        _   <- Villages.abandon(d, inv, userId, hero)
        d2  <- dao(Some(NpcQuests.empty.copy(village = Some(contract().copy(quest = VillageQuest.SpiderNests)))),
                 Some(VillageLairState.wonMark(VillageLair.Spiders)))
        inv2 = TestInventoryRepository.withItems(List(paper))
        r   <- TestRenderer.make
        _   <- VillageLairState(d2, inv2, TestItemRepository.make, c, VillageLair.Spiders).enter(testUser, r)
      } yield assertTrue(inv.snapshot.isEmpty && heads(inv2) == 0)
    },

    test("запись «после боя» на заставе: своим ключом; без поля охотников — ждать нечего") {
      assertTrue(BanditAftermath(hunters = true).asJson.as[BanditAftermath] == Right(BanditAftermath(hunters = true))) &&
      assertTrue(Json.obj(BanditAftermath.Key -> Json.True).as[BanditAftermath].exists(_.done)) &&
      assertTrue(Json.obj("banditsHunters" -> Json.True).as[BanditAftermath].isLeft) &&
      assertTrue(VillageLairState.wonMark(VillageLair.Bandits).as[BanditAftermath].isLeft)
    }
  )
}
