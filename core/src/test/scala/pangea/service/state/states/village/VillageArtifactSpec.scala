package pangea.service.state.states.village

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Item, QuestItemKind}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{Difficulty, NpcQuests}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.{ArtifactTrail, CaravanTrip, VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.service.state.states.DeathState
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.{NpcQuestLog, UserAction, Villages}
import pangea.test._
import zio.test._
import zio.{Task, ZIO, durationInt}

/** Украденный артефакт — четвёртая беда деревни, с 35-го уровня: воры вынесли
  * святыню деревни (у каждой расы своя) и ушли в изгородь. Погоня по следу —
  * от двух до четырёх переходов по 3–6 минут, сколько — игроку не говорим; в
  * конце — стан воров: сюжетный бой с бандой и вором, у которого артефакт.
  * Победа — к старосте за платой. Сошёл со следа или пал в стане — погоню
  * начинать заново, контракт при герое. */
object VillageArtifactSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private val legDone: UserAction = UserAction("", Some(VillageTrailState.LegAction))

  private val Level      = 52
  private val QuestLevel = 54
  private val Silver     = 2700L

  private def hero: Hero = TestFixtures.hero(userId, dungeonLevel = Level).copy(lvl = 55L, silver = 100L)

  // Деревня мурлоков, первое название в списке (Илистая Заводь); город — третий (Каэлис).
  private val scene    = VillageScene(Race.Murloc, 0, VillageQuest.StolenArtifact, Level, QuestLevel, Silver, city = 2)
  private val Name     = "Илистая Заводь"
  private val Pearl    = "Жемчужина Прилива"
  private val PearlAcc = "Жемчужину Прилива"
  private def contract(step: Int = VillageContract.Taken): VillageContract = VillageContract.of(scene).copy(step = step)
  private def quests(step: Int = VillageContract.Taken): NpcQuests = NpcQuests.empty.copy(village = Some(contract(step)))
  private def paper: Item = QuestItemKind.item(QuestItemKind.VillageContract).copy(id = 901L)

  private val content = ZIO.attempt(SceneContent.load())

  private def dao(q: Option[NpcQuests] = Some(quests()), data: Option[Json] = None) =
    for {
      d <- TestHeroDao.withHero(userId, hero)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
      _ <- ZIO.foreachDiscard(data)(d.writeSceneData(userId, _))
    } yield d

  private def onTrail(t: ArtifactTrail): Option[Json] = Some(t.asJson)

  /** Сцена погони с записывающим планировщиком. */
  private def trail(d: TestHeroDao, inv: TestInventoryRepository, c: SceneContent): Task[(VillageTrailState, TestScheduler)] =
    TestScheduler.make.map(s => (VillageTrailState(d, inv, s, c), s))

  private def trailOf(d: TestHeroDao): Task[Option[ArtifactTrail]] =
    d.readSceneData(userId).map(_.flatMap(_.as[ArtifactTrail].toOption))

  private def battleOf(d: TestHeroDao): Task[Option[SoloPveBattle]] =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)

  /** Переход `leg` из `legs`, который уже кончился: часы потом уходят на десять
    * минут вперёд, а конец перехода — на первой из них. */
  private def ended(legs: Int, leg: Int): ArtifactTrail = ArtifactTrail(legs, leg, 60L * 1000L)

  override def spec = suite("Украденный артефакт")(

    test("пул: с 35-го уровня староста может попросить и об артефакте; контракт ведёт на след воров") {
      for {
        c <- content
      } yield assertTrue(VillageQuest.StolenArtifact.minLevel == 35 && VillageQuest.StolenArtifact.key == "artifact") &&
              assertTrue(!VillageQuest.openAt(34).contains(VillageQuest.StolenArtifact) &&
                VillageQuest.openAt(35) == List(VillageQuest.BanditLair, VillageQuest.SpiderNests,
                  VillageQuest.CaravanEscort, VillageQuest.StolenArtifact)) &&
              assertTrue(Villages.goTo(quests()) == Some(StateType.VillageTrail -> "artifact") &&
                Villages.goTo(quests(VillageContract.Done)).isEmpty && Villages.toTrail(quests()) &&
                !Villages.toTrail(quests(VillageContract.Done)) && Villages.toPay(quests(VillageContract.Done))) &&
              assertTrue(Villages.cardLine(c, quests()).contains(s"Деревня мурлоков «$Name» платит за свою украденную святыню — $Pearl") &&
                Villages.cardLine(c, quests()).contains(s"Плата — $Silver серебра") &&
                Villages.cardLine(c, quests()).contains("Идти по следу — с этой карточки, из лабиринта")) &&
              assertTrue(c.choice("x", "village.card.artifactLabel").label == "👣 Идти по следу") &&
              // дело сделано — артефакт у героя, его и несут старосте
              assertTrue(Villages.cardLine(c, quests(VillageContract.Done)).startsWith(s"$Pearl у вас") &&
                Villages.cardLine(c, quests(VillageContract.Done)).contains(s"«$Name» ждёт свою святыню")) &&
              assertTrue(Villages.journalStep(c, contract()).contains(s"вернуть деревне мурлоков «$Name» её святыню — $PearlAcc") &&
                Villages.journalStep(c, contract(VillageContract.Done)).startsWith(s"отнести $PearlAcc в деревню мурлоков «$Name»"))
    },

    test("святыни: у каждой смертной расы своя — имя, винительный падеж и слова старосты; о ней и просит староста") {
      for {
        c <- content
        arts = Race.mortals.toList.map(r => (Villages.artifact(c, r), Villages.artifactAcc(c, r), Villages.artifactAbout(c, r)))
        plea = c.format("village.plea.artifact.text", Villages.args(c, scene): _*)
      } yield assertTrue(arts.size == 8 && arts.forall { case (n, a, about) => n.nonEmpty && a.nonEmpty && about.nonEmpty }) &&
              assertTrue(arts.map(_._1).distinct.size == arts.size) &&
              assertTrue(Villages.artifact(c, Race.Murloc) == Pearl && Villages.artifactAcc(c, Race.Murloc) == PearlAcc &&
                Villages.artifactAcc(c, Race.Demon) == "Свечу Неугасимую") &&
              assertTrue(plea.contains(s"воры вынесли из ниши на площади $PearlAcc") &&
                plea.contains(Villages.artifactAbout(c, Race.Murloc)) && plea.contains(s"Плата — $Silver серебра") &&
                !plea.contains("{")) &&
              assertTrue(c.text("village.trouble.artifact").contains("ниша пуста"))
    },

    test("у ворот: след, сложность, подсказка без числа переходов; кнопки — по следу, персонаж, уйти, отказаться") {
      for {
        c     <- content
        d     <- dao()
        r     <- TestRenderer.make
        st    <- trail(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _     <- st.enter(testUser, r)
        gates <- r.sentScreens.map(_.last)
      } yield assertTrue(gates.text.contains(s"След начинается у ворот деревни «$Name»") &&
                gates.text.contains("Сложность: " + Difficulty.render(QuestLevel)) &&
                gates.text.contains("Сколько идти до воров, не скажет никто")) &&
              assertTrue(gates.choices.map(_.id) ==
                List("VillageTrailGo", "OpenCharacter", "VillageTrailLeave", "VillageTrailAbandon"))
    },

    test("«Идти по следу»: переходов — от двух до четырёх, первый — его текст, конец через 3–6 минут — в сцене и в планировщике") {
      for {
        c     <- content
        d     <- dao()
        r     <- TestRenderer.make
        pair  <- trail(d, TestInventoryRepository.withItems(List(paper)), c)
        (st, sched) = pair
        res   <- st.action(testUser, tap("VillageTrailGo"), r)
        road  <- r.sentScreens.map(_.last)
        t     <- trailOf(d)
        tasks <- sched.scheduled
        again <- st.action(testUser, tap("VillageTrailGo"), r) *> trailOf(d)
      } yield assertTrue(res == StateType.VillageTrail && road.text.contains("Вы ныряете в изгородь вслед за ворами")) &&
              assertTrue(t.exists(x => x.leg == 1 && x.legs >= VillageRates.TrailLegsMin && x.legs <= VillageRates.TrailLegsMax &&
                x.nextAt >= VillageRates.TrailLegMinMs && x.nextAt <= VillageRates.TrailLegMaxMs)) &&
              assertTrue(tasks.map(x => (x.kind, x.expectedState, x.action, x.fireAt)) ==
                t.toList.map(x => ((TaskKind.VillageTrail: TaskKind), (StateType.VillageTrail: StateType),
                  VillageTrailState.LegAction, x.nextAt))) &&
              // сколько переходов, не говорим
              assertTrue(!road.text.contains("переход") && road.choices.map(_.id) == List("OpenCharacter", "OpenApp", "VillageTrailQuit")) &&
              // второй раз не начинается: погоня уже идёт
              assertTrue(again == t)
    },

    test("переходы: средние — по очереди, последний — «стан совсем рядом», за ним — стан; раньше срока ничего не происходит") {
      def step(t: ArtifactTrail) =
        for {
          c    <- content
          d    <- dao(data = onTrail(t))
          r    <- TestRenderer.make
          st   <- trail(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
          _    <- TestClock.adjust(10.minutes)
          _    <- st.action(testUser, legDone, r)
          shown <- r.sentScreens.map(_.last)
          next <- trailOf(d)
        } yield (shown, next)
      for {
        c      <- content
        d0     <- dao(data = onTrail(ArtifactTrail(3, 1, 5L * 60L * 1000L)))
        r0     <- TestRenderer.make
        st0    <- trail(d0, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _      <- st0.action(testUser, legDone, r0)
        early  <- trailOf(d0)
        second <- step(ended(3, 1))
        third  <- step(ended(4, 2))
        last   <- step(ended(3, 2))
        short  <- step(ended(2, 1))
        camp   <- step(ended(3, 3))
      } yield assertTrue(early.contains(ArtifactTrail(3, 1, 5L * 60L * 1000L))) &&
              assertTrue(second._1.text.contains("След выводит к ручью") &&
                second._2.exists(t => t.leg == 2 && t.nextAt > 10L * 60L * 1000L)) &&
              // четыре перехода: второй и третий — средние, по очереди
              assertTrue(third._1.text.contains("На прогалине — кострище") && third._2.exists(_.leg == 3) &&
                !third._1.text.contains("стан воров совсем рядом")) &&
              assertTrue(last._1.text.contains("стан воров совсем рядом") && last._2.exists(t => t.leg == 3 && t.lastLeg)) &&
              // два перехода: за первым сразу последний
              assertTrue(short._1.text.contains("стан воров совсем рядом") && short._2.exists(_.leg == 2)) &&
              assertTrue(camp._1.text.contains("Стан воров — на прогалине") && camp._1.text.contains("Сбежать из этого боя не получится") &&
                camp._1.text.contains("Сложность: " + Difficulty.render(QuestLevel)) &&
                camp._1.choices.map(_.id) == List("VillageTrailFight", "OpenCharacter", "VillageTrailQuit", "VillageTrailAbandon") &&
                camp._2.contains(ArtifactTrail(3, 4, 0L)) && camp._2.exists(_.atCamp))
    },

    test("из меню: по следу — без номера перехода, таймер заново, кончившийся переход — сразу; у стана — снова стан") {
      for {
        c     <- content
        _     <- TestClock.adjust(10.minutes)
        d     <- dao(data = onTrail(ended(3, 2)))
        r     <- TestRenderer.make
        pair  <- trail(d, TestInventoryRepository.withItems(List(paper)), c)
        (st, sched) = pair
        _     <- st.enter(testUser, r)
        road  <- r.sentScreens.map(_.last)
        tasks <- sched.scheduled
        d2    <- dao(data = onTrail(ArtifactTrail(3, 4, 0L)))
        st2   <- trail(d2, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _     <- st2.enter(testUser, r)
        camp  <- r.sentScreens.map(_.last)
      } yield assertTrue(road.text == c.text("village.artifact.onTrail")) &&
              assertTrue(tasks.map(t => (t.kind, t.fireAt)) == List(TaskKind.VillageTrail -> 10L * 60L * 1000L)) &&
              assertTrue(camp.text.contains("Стан воров — на прогалине"))
    },

    test("стан: сюжетный бой без «Сбежать» — восемь воров уровня задания; добыча вернёт сюда с отметкой") {
      for {
        c     <- content
        d     <- dao(data = onTrail(ArtifactTrail(2, 3, 0L)))
        r     <- TestRenderer.make
        st    <- trail(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        res   <- st.action(testUser, tap("VillageTrailFight"), r)
        shown <- r.sentScreens.map(_.last)
        b     <- battleOf(d)
        loot  <- d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        foes   = b.toList.flatMap(x => x.monsterRace :: (x.group.others ++ x.group.queue).map(_.race))
      } yield assertTrue(res == StateType.Battle && shown.text.contains("первым вас замечает тот, что на тюках")) &&
              assertTrue(b.exists(x => x.story.contains(Villages.ArtifactStory) && x.lootLevel.contains(QuestLevel.toLong) && x.cantFlee)) &&
              assertTrue(foes.size == VillageRates.ThiefFront + VillageRates.ThiefBack + 1) &&
              assertTrue(loot.exists(l => l.returnState.contains(StateType.VillageTrail) &&
                l.eventData.exists(VillageTrailState.won))) &&
              assertTrue(BattleState.RaidStories.contains(Villages.ArtifactStory))
    },

    test("не у стана «Напасть» — старая кнопка: боя нет, показано, что сейчас") {
      for {
        c     <- content
        d     <- dao()
        r     <- TestRenderer.make
        st    <- trail(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        res   <- st.action(testUser, tap("VillageTrailFight"), r)
        shown <- r.sentScreens.map(_.last)
        b     <- battleOf(d)
      } yield assertTrue(res == StateType.VillageTrail && b.isEmpty && shown.text.contains("След начинается у ворот"))
    },

    test("воры: сборная из смертных рас уровня задания — вор-именной первым, за ним четверо и трое") {
      val gangs = (1L to 50L).toList.map(s => Villages.thiefGang(contract(), Rng(s)))
      val size  = VillageRates.ThiefFront + VillageRates.ThiefBack + 1
      assertTrue(gangs.forall(g => g.size == size && g.forall(_.lvl == QuestLevel.toLong))) &&
      assertTrue(gangs.flatten.forall(m => Race.mortals.contains(m.race)) && gangs.flatten.map(_.race).distinct.size > 3) &&
      assertTrue(gangs.forall(g => g.slice(1, 1 + VillageRates.ThiefFront).forall(m => VillageRates.ThiefFrontRarities.contains(m.rarity)) &&
        g.drop(1 + VillageRates.ThiefFront).forall(m => VillageRates.ThiefBackRarities.contains(m.rarity)))) &&
      assertTrue(gangs.flatMap(_.headOption).forall(m => m.rarity == Rarity.Legendary && m.customName.exists(_.nonEmpty))) &&
      // через lairMonsters — те же воры
      assertTrue(Villages.lairMonsters(contract(), Rng(7L)) == Villages.thiefGang(contract(), Rng(7L)))
    },

    test("победа: артефакт у героя — контракт «сделано», развязка и подсказка; второй вход — пусто") {
      for {
        c     <- content
        d     <- dao(data = Some(VillageTrailState.wonMark))
        r     <- TestRenderer.make
        st    <- trail(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _     <- st.enter(testUser, r)
        won   <- r.sentScreens.map(_.last)
        q     <- questOf(d)
        sd    <- d.readSceneData(userId)
        _     <- st.enter(testUser, r)
        again <- r.sentScreens.map(_.last)
        out   <- st.action(testUser, tap("VillageTrailOut"), r)
      } yield assertTrue(won.text.contains(s"Вы разворачиваете его: внутри — $Pearl. Вещь цела") &&
                won.text.contains(s"вернуть пропажу старосте деревни «$Name»") && won.text.contains(c.text("village.lair.returnHint")) &&
                won.choices.map(_.id) == List("VillageTrailOut")) &&
              assertTrue(q.village.exists(_.done) && sd.contains(Json.Null)) &&
              assertTrue(again.text == c.text("village.artifact.gone") && out == StateType.Dungeon)
    },

    test("плата: староста берёт святыню — серебро, опыт, контракт уходит из сумки") {
      for {
        c    <- content
        d    <- dao(q = Some(quests(VillageContract.Done)))
        inv   = TestInventoryRepository.withItems(List(paper))
        r    <- TestRenderer.make
        _    <- VillageState(d, inv, TestItemRepository.make, c).enter(testUser, r)
        paid <- r.sentScreens.map(_.last)
        h    <- d.getHeroByUserId(userId).map(_.get)
        q    <- questOf(d)
      } yield assertTrue(paid.text.contains(s"Староста берёт $PearlAcc обеими руками") &&
                paid.text.contains(s"Получено $Silver серебра") && paid.text.contains(c.text("village.questDone"))) &&
              assertTrue(h.silver == 100L + Silver && h.exp > hero.exp && q.village.isEmpty && inv.snapshot.isEmpty)
    },

    test("пришёл к старосте раньше времени — ниша ещё пуста, платы нет") {
      for {
        c     <- content
        d     <- dao()
        r     <- TestRenderer.make
        _     <- VillageState(d, TestInventoryRepository.withItems(List(paper)), TestItemRepository.make, c).enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
        h     <- d.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(shown.text.contains("Ниша на площади всё ещё пустует") && h.silver == 100L)
    },

    test("сойти со следа — с подтверждением: погоня сброшена, таймер снят, контракт при герое") {
      for {
        c       <- content
        d       <- dao(data = onTrail(ArtifactTrail(3, 2, 5L * 60L * 1000L)))
        inv      = TestInventoryRepository.withItems(List(paper))
        r       <- TestRenderer.make
        pair    <- trail(d, inv, c)
        (st, sched) = pair
        ask     <- st.action(testUser, tap("VillageTrailQuit"), r)
        confirm <- r.sentScreens.map(_.last)
        kept    <- trailOf(d)
        _       <- st.action(testUser, tap("VillageTrailBack"), r)
        back    <- r.sentScreens.map(_.last)
        done    <- st.action(testUser, tap("VillageTrailQuitYes"), r)
        bye     <- r.sentScreens.map(_.last)
        sd      <- d.readSceneData(userId)
        q       <- questOf(d)
        off     <- sched.cancelled
      } yield assertTrue(ask == StateType.VillageTrail && confirm.text.contains("Сойти со следа?") &&
                confirm.choices.map(_.id) == List("VillageTrailQuitYes", "VillageTrailBack") && kept.isDefined) &&
              assertTrue(back.text == c.text("village.artifact.onTrail")) &&
              assertTrue(done == StateType.Dungeon && bye.text.contains("погоню придётся начинать заново") && sd.contains(Json.Null)) &&
              assertTrue(q.village.exists(v => v.quest == VillageQuest.StolenArtifact && !v.done) && inv.snapshot.nonEmpty &&
                off.contains(userId -> TaskKind.VillageTrail))
    },

    test("у ворот «Уйти» — контракт при герое; «Отказаться» — как у логова: береста и договор уходят, таймер снят") {
      for {
        c     <- content
        d     <- dao()
        inv    = TestInventoryRepository.withItems(List(paper))
        r     <- TestRenderer.make
        pair  <- trail(d, inv, c)
        (st, sched) = pair
        left  <- st.action(testUser, tap("VillageTrailLeave"), r)
        kept   = inv.snapshot
        q1    <- questOf(d)
        _     <- st.action(testUser, tap("VillageTrailAbandon"), r)
        ask   <- r.sentScreens.map(_.last)
        done  <- st.action(testUser, tap("VillageTrailAbandonYes"), r)
        q2    <- questOf(d)
        off   <- sched.cancelled
      } yield assertTrue(left == StateType.Dungeon && q1.village.isDefined && kept.nonEmpty) &&
              assertTrue(ask.text.contains("Отказаться от контракта деревни") && done == StateType.Dungeon &&
                q2.village.isEmpty && inv.snapshot.isEmpty && off.contains(userId -> TaskKind.VillageTrail))
    },

    test("в пути «Уйти» — старая кнопка: погоня идёт дальше") {
      for {
        c    <- content
        d    <- dao(data = onTrail(ArtifactTrail(3, 2, 5L * 60L * 1000L)))
        r    <- TestRenderer.make
        st   <- trail(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        res  <- st.action(testUser, tap("VillageTrailLeave"), r)
        t    <- trailOf(d)
        out  <- st.action(testUser, tap("VillageTrailOut"), r)
        t2   <- trailOf(d)
      } yield assertTrue(res == StateType.VillageTrail && t.contains(ArtifactTrail(3, 2, 5L * 60L * 1000L))) &&
              assertTrue(out == StateType.VillageTrail && t2 == t)
    },

    test("пал в стане: след остыл, контракт при герое; проснуться в лабиринте со строкой стана") {
      for {
        c   <- content
        d   <- dao()
        inv  = TestInventoryRepository.withItems(List(paper))
        r   <- TestRenderer.make
        _   <- d.writeActiveBattle(userId, SoloPveBattle.fromGroup(Villages.thiefGang(contract(), Rng(1L)), hero, Nil)
                 .copy(story = Some(Villages.ArtifactStory)).asJson)
        _   <- DeathState(d, inv, c).enter(testUser, r)
        sd  <- d.readSceneData(userId)
        q   <- questOf(d)
      } yield assertTrue(sd.flatMap(_.hcursor.get[List[String]]("wakeLines").toOption).contains(List("village.artifact.death"))) &&
              assertTrue(sd.exists(_.hcursor.downField("wakeTo").failed) && sd.exists(_.as[ArtifactTrail].isLeft)) &&
              assertTrue(q.village.exists(v => v.quest == VillageQuest.StolenArtifact && !v.done) && inv.snapshot.nonEmpty) &&
              assertTrue(c.text("village.artifact.death").contains("погоню придётся начинать заново"))
    },

    test("контракта на артефакт нет — на следу пусто: только «В лабиринт»") {
      for {
        c     <- content
        d     <- dao(q = Some(quests().copy(village = Some(contract().copy(quest = VillageQuest.BanditLair)))))
        r     <- TestRenderer.make
        st    <- trail(d, TestInventoryRepository.withItems(List(paper)), c).map(_._1)
        _     <- st.enter(testUser, r)
        shown <- r.sentScreens.map(_.last)
        go    <- st.action(testUser, tap("VillageTrailGo"), r) *> trailOf(d)
      } yield assertTrue(shown.text == c.text("village.artifact.gone") && shown.choices.map(_.id) == List("VillageTrailOut")) &&
              assertTrue(go.isEmpty)
    },

    test("погоня в scene_data: своими ключами, без шага и таймера — с первого перехода; чужое — не погоня") {
      val t = ArtifactTrail(4, 2, 123L)
      assertTrue(t.asJson.as[ArtifactTrail] == Right(t)) &&
      assertTrue(Json.obj("trailLegs" -> 3.asJson).as[ArtifactTrail] == Right(ArtifactTrail(3, 1, 0L))) &&
      assertTrue(CaravanTrip(2, 1, 5L).asJson.as[ArtifactTrail].isLeft && VillageTrailState.wonMark.as[ArtifactTrail].isLeft &&
        ArtifactTrail(3, 1, 0L).asJson.as[CaravanTrip].isLeft) &&
      assertTrue(ArtifactTrail(3, 4, 0L).atCamp && !ArtifactTrail(3, 3, 1L).atCamp && ArtifactTrail(3, 3, 1L).lastLeg &&
        !ArtifactTrail(3, 2, 1L).lastLeg)
    }
  )
}
