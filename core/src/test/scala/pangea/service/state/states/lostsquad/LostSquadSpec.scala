package pangea.service.state.states.lostsquad

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{Item, ItemType, MaterialKind, QuestItemKind, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.quest.{NpcQuest, NpcQuests}
import pangea.model.squad.{AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.ConstructionState
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.events.SilverVeinState
import pangea.service.state.states.tavern.MercenariesState
import pangea.service.state.{LostSquadQuest, NpcQuestLog, UserAction}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** «Пропавшая дружина»: фонарь с пятисотого убитого, Брамбл за столом
  * наёмников, штольня под серебряной жилой, бой у завала и две развязки —
  * дары дружины или ящик, проданный Гуннару. */
object LostSquadSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def hero(kills: Long = 0L, squad: Squad = Squad.empty): Hero =
    TestFixtures.hero(userId).copy(kills = kills, squad = squad,
      fightStats = FightStats(atk = 50, hp = 200, armor = 0, defence = 0, evasion = 9999, accuracy = 9999, energy = 0))

  private def withBrambl(h: Hero): Hero = h.copy(squad = h.squad.hire(AllyKind.Gnome, h.lvl, 0L))

  /** Задание на шаге `step`; `told` — Брамбл уже рассказал, `gone` — дружина брошена. */
  private def quest(step: Int, told: Boolean = false, gone: Boolean = false): NpcQuests =
    NpcQuests.empty.take(NpcQuest.LostSquad, 0L)
      .update(NpcQuest.LostSquad)(_.copy(step = step, counter = if (told) 1L else 0L, bonus = gone))

  private def lantern: Item = QuestItemKind.item(QuestItemKind.ForemanLantern).copy(id = 501L)

  private def dao(h: Hero, q: Option[NpcQuests]) =
    for {
      d <- TestHeroDao.withHero(userId, h)
      _ <- ZIO.foreachDiscard(q)(NpcQuestLog.save(d, userId, _))
    } yield d

  private def questOf(d: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(d, userId)
  private def heroOf(d: TestHeroDao): Task[Hero]       = d.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer)                   = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private def lootOf(d: TestHeroDao): Task[Option[LootData]] =
    d.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))

  /** Моб в один удар — для победы на пятисотом. */
  private val weak = SoloPveBattle(
    monsterLvl = 1L, monsterRace = Race.Human.entryName, monsterRarity = Rarity.Common.entryName,
    monsterStats = FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp = 1L, monsterCurrentArmor = 0L)

  private def win(h: Hero, q: Option[NpcQuests] = None) =
    for {
      d   <- dao(h, q)
      _   <- d.writeActiveBattle(userId, weak.asJson)
      inv  = TestInventoryRepository.withItems(Nil)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
      _   <- BattleState(d, inv, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
    } yield (d, inv, r)

  override def spec = suite("Пропавшая дружина")(

    test("пятисотый убитый: фонарь в сумке, задание на первом шаге и подсказка про гнома в таверне") {
      for {
        t <- win(hero(kills = 499L))
        (d, inv, r) = t
        q    <- questOf(d)
        said <- texts(r)
      } yield assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.ForemanLantern))) &&
              assertTrue(LostSquadQuest.lanternOnHands(q) && !LostSquadQuest.bramblTold(q)) &&
              assertTrue(said.contains("Двенадцать фонарей · Двенадцатый") && said.contains("гнома-наёмника за дальним столом"))
    },

    test("Брамбл в отряде узнаёт фонарь сразу; кто перевалил за пятьсот раньше — получает на следующей победе, и только раз") {
      for {
        t1 <- win(withBrambl(hero(kills = 499L)))
        (d1, _, r1) = t1
        q1   <- questOf(d1)
        said <- texts(r1)
        t2 <- win(hero(kills = 700L))
        (d2, inv2, _) = t2
        q2   <- questOf(d2)
        t3 <- win(hero(kills = 700L), Some(quest(2)))
        (_, inv3, _) = t3
      } yield assertTrue(LostSquadQuest.bramblTold(q1) && said.contains("Брамбл выхватывает фонарь")) &&
              assertTrue(LostSquadQuest.lanternOnHands(q2) && inv2.snapshot.exists(_.questItem.contains(QuestItemKind.ForemanLantern))) &&
              assertTrue(inv3.snapshot.isEmpty)
    },

    test("таверна: Брамбл за столом даже после отдыха, рассказывает про фонарь и идёт даром") {
      val resting = hero().copy(squad = Squad.empty.copy(away = Map(AllyKind.Gnome.entryName -> Long.MaxValue)))
      for {
        d   <- dao(resting, Some(quest(1)))
        inv  = TestInventoryRepository.withItems(Nil)
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        state = MercenariesState(d, inv, c)
        _    <- state.enter(testUser, r)
        list <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("MercCard", "kind" -> AllyKind.Gnome.entryName), r)
        card <- r.sentScreens.map(_.last)
        told <- questOf(d)
        _    <- state.action(testUser, tap("MercHire", "kind" -> AllyKind.Gnome.entryName), r)
        h    <- heroOf(d)
        said <- texts(r)
      } yield assertTrue(list.choices.exists(c => c.id == "MercCard" && c.data.get("kind").contains(AllyKind.Gnome.entryName))) &&
              assertTrue(card.text.contains("„Двенадцать фонарей“ — моя дружина") && card.text.contains("Денег не надо")) &&
              assertTrue(LostSquadQuest.bramblTold(told)) &&
              assertTrue(h.squad.has(AllyKind.Gnome) && !h.squad.isAway(AllyKind.Gnome, 0L) && said.contains("закидывает кирку на плечо")) &&
              // без задания отдыхающий Брамбл за столом не сидит
              assertTrue(!MercenariesState.available(resting.squad, 0L).contains(AllyKind.Gnome))
    },

    test("жила с фонарём на руках: после серебра — в штольню; первый спуск — провал и зал у завала") {
      for {
        d   <- dao(hero(), Some(quest(1)))
        sch <- TestScheduler.make
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        _   <- TestRandom.feedInts(15, 90, 90)
        _   <- TestRandom.feedBooleans(true)
        out <- SilverVeinState(d, sch, c).action(testUser, tap("Harvest"), r)
        _   <- SquadMineState(d, TestInventoryRepository.withItems(List(lantern)), TestItemRepository.make, c).enter(testUser, r)
        hall <- r.sentScreens.map(_.last)
        said <- texts(r)
        q    <- questOf(d)
      } yield assertTrue(out == StateType.SquadMine) &&
              assertTrue(LostSquadQuest.mineFound(q)) &&
              assertTrue(said.contains("Кирка уходит в пустоту") && hall.text.contains("Двенадцать фонарей")) &&
              assertTrue(hall.choices.map(_.id) == List("MineDig", "MineCrate", "MineLeave"))
    },

    test("«Разобрать завал» — бой: гоблины уровня героя, последним их вожак Хруст Собиратель; после боя — назад в штольню") {
      for {
        d   <- dao(hero(), Some(quest(2)))
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        out <- SquadMineState(d, TestInventoryRepository.withItems(List(lantern)), TestItemRepository.make, c)
                 .action(testUser, tap("MineDig"), r)
        b   <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        loot <- lootOf(d)
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(b.exists(_.story.contains(LostSquadQuest.EscortStory))) &&
              assertTrue(b.exists(x => 1 + x.group.others.size + x.group.queue.size == 13)) &&
              assertTrue(b.exists(x => (x.group.others ++ x.group.queue).count(_.customName.contains(LostSquadQuest.LeaderName)) == 1)) &&
              assertTrue(b.exists(x => x.monsterRace == Race.Goblin.entryName && (x.group.others ++ x.group.queue).forall(_.race == Race.Goblin.entryName))) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.SquadMine)))
    },

    test("дружина выведена: шлем старшины (пурпурный, уровня героя, с точностью) и четыре куска; задание закрыто, +1 к телосложению") {
      val h0 = hero().copy(lvl = 12L)
      for {
        d   <- dao(h0, Some(quest(2)))
        _   <- d.writeSceneData(userId, SquadMineState.Progress(SquadMineState.Step.AfterEscort).asJson)
        inv  = TestInventoryRepository.withItems(List(lantern))
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        state = SquadMineState(d, inv, TestItemRepository.make, c)
        _    <- state.enter(testUser, r)
        thanks <- r.sentScreens.map(_.last)
        out  <- state.action(testUser, tap("MineGifts"), r)
        loot <- lootOf(d)
        q    <- questOf(d)
        h    <- heroOf(d)
      } yield assertTrue(thanks.text.contains("Двенадцатый фонарь") && thanks.choices.map(_.id) == List("MineGifts")) &&
              assertTrue(out == StateType.Loot && q.isDone(NpcQuest.LostSquad)) &&
              assertTrue(loot.exists(l => l.items.size == 1 + LostSquadQuest.GiftCount && l.returnState.contains(StateType.Dungeon))) &&
              assertTrue(loot.exists(_.items.headOption.exists(i => i.itemType == ItemType.Helmet && i.rarity == ItemRarity.Violet &&
                i.lvl == 12L && i.accuracy > 0L && i.name.contains("Двенадцать фонарей")))) &&
              assertTrue(loot.exists(_.items.tail.forall(i => i.material.exists(LostSquadQuest.GiftKinds.contains)))) &&
              assertTrue(h.hasAchievement(Achievement.SquadFriend) && h.nativeBaseStats.vit == h0.baseStats.vit + 1L) &&
              assertTrue(!inv.snapshot.exists(_.questItem.contains(QuestItemKind.ForemanLantern)))
    },

    test("дары не пропадают: ушёл с шага даров (хоть по /home) — жила снова ведёт в штольню, дружина ждёт у выхода") {
      for {
        d   <- dao(hero(), Some(quest(3)))
        sch <- TestScheduler.make
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        _   <- TestRandom.feedInts(15, 90, 90)
        _   <- TestRandom.feedBooleans(true)
        out <- SilverVeinState(d, sch, c).action(testUser, tap("Harvest"), r)
        state = SquadMineState(d, TestInventoryRepository.withItems(List(lantern)), TestItemRepository.make, c)
        _    <- state.enter(testUser, r)
        wait <- r.sentScreens.map(_.last)
        took <- state.action(testUser, tap("MineGifts"), r)
        loot <- lootOf(d)
        q    <- questOf(d)
      } yield assertTrue(out == StateType.SquadMine) &&
              // битва не повторяется: не «последний гоблин падает», а дружина у выхода
              assertTrue(wait.text.contains("ждут вас у выхода из штольни") && !wait.text.contains("Последний гоблин")) &&
              assertTrue(wait.choices.map(_.id) == List("MineGifts")) &&
              assertTrue(took == StateType.Loot && loot.exists(_.items.size == 1 + LostSquadQuest.GiftCount) && q.isDone(NpcQuest.LostSquad))
    },

    test("дружина дарит четыре куска, а не по куску на каждого из семерых") {
      assertTrue(LostSquadQuest.GiftCount == 4 && LostSquadQuest.gifts(Rng(7L))._1.size == 4)
    },

    test("шлем — всегда с точностью, дары — только железо и магический камень, обоих примерно поровну") {
      val h = hero().copy(lvl = 30L)
      val helmets = (1L to 60L).toList.map(s => LostSquadQuest.helmet(h, Rng(s)))
      val gifts   = (1L to 60L).toList.flatMap(s => LostSquadQuest.gifts(Rng(s))._1)
      val iron    = gifts.count(_.material.contains(MaterialKind.EverburningIron))
      assertTrue(helmets.forall(i => i.accuracy > 0L && i.rarity == ItemRarity.Violet && i.lvl == 30L)) &&
        assertTrue(gifts.size == 60 * LostSquadQuest.GiftCount && iron > gifts.size / 3 && iron < gifts.size * 2 / 3)
    },

    test("забрать ящик: Брамбл уходит по свитку и больше не нанимается; Гуннар даёт 10 000 серебра и кусок железа") {
      for {
        d   <- dao(withBrambl(hero()).copy(silver = 100L), Some(quest(2)))
        inv  = TestInventoryRepository.withItems(List(lantern))
        sch <- TestScheduler.make
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        out <- SquadMineState(d, inv, TestItemRepository.make, c).action(testUser, tap("MineCrate"), r)
        bag  = inv.snapshot
        q   <- questOf(d)
        h   <- heroOf(d)
        said <- texts(r)
        site = ConstructionState(d, sch, c, Some(inv))
        _    <- site.enter(testUser, r)
        menu <- r.sentScreens.map(_.last)
        _      <- site.action(testUser, tap("Foreman"), r)
        gunnar <- r.sentScreens.map(_.last)
        sold <- site.action(testUser, tap("SquadCrate"), r)
        h2   <- heroOf(d)
        q2   <- questOf(d)
        loot <- lootOf(d)
        end  <- texts(r)
      } yield assertTrue(out == StateType.Dungeon && LostSquadQuest.crateOnHands(q) && LostSquadQuest.squadAbandoned(q)) &&
              assertTrue(bag.exists(_.questItem.contains(QuestItemKind.SquadCrate))) &&
              assertTrue(!h.squad.has(AllyKind.Gnome) && said.contains("исчезает во вспышке свитка")) &&
              // ушёл по свитку — сейчас его за столом нет, а вернётся — вдвое дороже
              assertTrue(!MercenariesState.available(h.squad, 0L, LostSquadQuest.bramblCalled(q)).contains(AllyKind.Gnome)) &&
              assertTrue(MercenariesState.available(h.squad, Long.MaxValue, LostSquadQuest.bramblCalled(q)).contains(AllyKind.Gnome)) &&
              assertTrue(LostSquadQuest.bramblBrews(q, 30L) == 12L) &&
              // ящик несут прорабу: на стройке — его кнопка, у него — «Отдать ящик»
              assertTrue(menu.choices.exists(_.id == "Foreman") && !menu.choices.exists(_.id == "SquadCrate")) &&
              assertTrue(gunnar.choices.headOption.exists(_.id == "SquadCrate") && gunnar.text.contains("ящик у вас на плече")) &&
              assertTrue(sold == StateType.Loot && h2.silver == 100L + LostSquadQuest.CrateSilver) &&
              assertTrue(loot.exists(_.items.map(_.material) == List(Some(MaterialKind.EverburningIron)))) &&
              assertTrue(q2.isDone(NpcQuest.LostSquad) && h2.hasAchievement(Achievement.MineLooter)) &&
              assertTrue(end.contains("поминальный колокол") && !inv.snapshot.exists(_.questItem.isDefined))
    },

    test("прораб Гуннар по ходу задания: ждёт дружину, замечает ящик, благодарит за выведенных, молчит после проданного груза") {
      val rescued = quest(2).finish(NpcQuest.LostSquad)
      val sold    = quest(4, gone = true).finish(NpcQuest.LostSquad)
      assertTrue(LostSquadQuest.foremanLine(NpcQuests.empty).isEmpty) &&
        assertTrue(LostSquadQuest.foremanLine(quest(1)).contains("lostSquad.foreman.waiting")) &&
        assertTrue(LostSquadQuest.foremanLine(quest(2)).contains("lostSquad.foreman.waiting")) &&
        // о том, что дружину вывели, Гуннару сказали сразу — ещё до даров
        assertTrue(LostSquadQuest.foremanLine(quest(3)).contains("lostSquad.foreman.grateful")) &&
        assertTrue(LostSquadQuest.foremanLine(quest(4, gone = true)).contains("lostSquad.foreman.crateSeen")) &&
        assertTrue(LostSquadQuest.foremanLine(rescued).contains("lostSquad.foreman.grateful")) &&
        assertTrue(LostSquadQuest.foremanLine(sold).contains("lostSquad.foreman.cold"))
    },

    test("в бою у завала с героем трое дружинников: гномы-предводители его уровня на 75% HP и брони") {
      val h0 = hero().copy(lvl = 20L)
      val leader = pangea.generator.monster.MonsterGenerator.generateOfRaceAndRarity(20, Race.Gnome, Rarity.Mythical).fightStats
      for {
        d   <- dao(h0, Some(quest(2)))
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        _   <- SquadMineState(d, TestInventoryRepository.withItems(List(lantern)), TestItemRepository.make, c)
                 .action(testUser, tap("MineDig"), r)
        b   <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        gnomes = b.toList.flatMap(_.group.allies).filter(_.kind == AllyKind.SquadGnome)
      } yield assertTrue(gnomes.map(_.name) == LostSquadQuest.EscortGnomes) &&
              assertTrue(gnomes.forall(g => g.stats == leader && g.hp == leader.hp * 75L / 100L &&
                g.armor == leader.armor * 75L / 100L && g.lvl == 20L && g.home == 0 && !g.isUndead)) &&
              assertTrue(gnomes.map(_.position).distinct.size == 3 && b.exists(x => !gnomes.exists(_.position == x.group.heroPos)))
    },

    test("после боя дружинники уходят со своими: в отряд героя они не встают") {
      val h0 = hero()
      for {
        d   <- dao(h0, Some(quest(2)))
        battle = LostSquadQuest.withEscort(weak.copy(story = Some(LostSquadQuest.EscortStory)), h0.lvl)
        _   <- d.writeActiveBattle(userId, battle.asJson)
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        _   <- BattleState(d, TestInventoryRepository.withItems(Nil), TestItemRepository.make, c).action(testUser, tap("Attack"), r)
        h   <- heroOf(d)
      } yield assertTrue(battle.group.allies.size == 3) &&
              assertTrue(!h.squad.allies.exists(_.kind == AllyKind.SquadGnome) && h.squad.away.isEmpty)
    },

    test("Брамбл после задания: вывел дружину — всегда одна бутылка шнапса, бросил — вдвое, а без задания — как прежде") {
      val schnapps = (1L to 3L).toList.map(i => pangea.model.item.BrewKind.item(pangea.model.item.BrewKind.Schnapps).copy(id = 600L + i))
      val friend   = NpcQuests.empty.take(NpcQuest.LostSquad, 0L).finish(NpcQuest.LostSquad)
      val grudge   = quest(4, gone = true).finish(NpcQuest.LostSquad)
      for {
        d   <- dao(hero().copy(lvl = 30L), Some(friend))
        inv  = TestInventoryRepository.withItems(schnapps)
        r   <- TestRenderer.make
        c   <- ZIO.attempt(SceneContent.load())
        state = MercenariesState(d, inv, c)
        _    <- state.action(testUser, tap("MercCard", "kind" -> AllyKind.Gnome.entryName), r)
        card <- r.sentScreens.map(_.last)
        _    <- state.action(testUser, tap("MercHire", "kind" -> AllyKind.Gnome.entryName), r)
        h    <- heroOf(d)
        d2   <- dao(hero().copy(lvl = 30L), Some(grudge))
        r2   <- TestRenderer.make
        _    <- MercenariesState(d2, TestInventoryRepository.withItems(Nil), c)
                  .action(testUser, tap("MercCard", "kind" -> AllyKind.Gnome.entryName), r2)
        card2 <- r2.sentScreens.map(_.last)
      } yield assertTrue(card.text.contains("за одну бутылку шнапса")) &&
              assertTrue(h.squad.has(AllyKind.Gnome) && inv.snapshot.size == 2) &&
              assertTrue(card2.text.contains("бутылок: 12. Вдвое — за штольню")) &&
              assertTrue(LostSquadQuest.bramblBrews(friend, 30L) == 1L && LostSquadQuest.bramblBrews(grudge, 30L) == 12L &&
                LostSquadQuest.bramblBrews(NpcQuests.empty, 30L) == 6L)
    },

    test("«Расхититель штолен» добывает с жилы на десятую больше; в журнале — шаг задания") {
      val plain  = hero()
      val looter = plain.withAchievement(Achievement.MineLooter)
      val c      = SceneContent.load()
      val line   = pangea.service.state.QuestJournal.render(c, plain, pangea.model.hero.LoreData.empty, quest(1),
                     pangea.model.quest.DailyQuests.empty, None, 0L)
      assertTrue(Achievement.veinPct(plain) == 100L && Achievement.veinPct(looter) == 110L) &&
        assertTrue(line.contains("«Пропавшая дружина» — найти серебряную жилу"))
    }
  )
}
