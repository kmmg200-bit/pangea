package pangea.service.state.states.village

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.{FlaskGenerator, MaterialGenerator}
import pangea.model.battle.{Burn, Poison, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{BeltSlot, BrewKind, FlaskKind, Item, ItemDetails, ItemType, MaterialKind, QuestItemKind, Rarity}
import pangea.model.monster.{Breed, Race}
import pangea.model.quest.NpcQuests
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.village.{NestPrep, VillageContract, VillageQuest, VillageRates}
import pangea.service.state.states.events.cave.CaveSupply
import pangea.service.state.{NpcQuestLog, UserAction, VillageLair, Villages}
import pangea.test._
import zio.test._
import zio.{Task, ZIO}

/** Гнездо сетеплётов: у входа, как на пороге пещеры, можно пустить в дело вещь
  * из сумки или с пояса (решил пользователь). Дым и сонный дурман слабят
  * пауков, глефовый гриб травит, огненная фляга и рубиновая пыль поджигают —
  * пламя 4%, — паучий цвет уводит выводок: внизу матка и двое красных. */
object VillageNestSupplySpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val QuestLevel = 26

  private val contract = VillageContract(VillageQuest.SpiderNests, Race.Orc, 1, QuestLevel, 1700L, city = 2)

  private def hero(belt: Item = Item.NoItem): Hero =
    TestFixtures.hero(userId, dungeonLevel = 23).copy(lvl = 25L,
      equipment = TestFixtures.emptyEquipment.copy(belt = belt))

  private def paper: Item = QuestItemKind.item(QuestItemKind.VillageContract).copy(id = 901L)

  private def flask(kind: FlaskKind, id: Long): Item = FlaskGenerator.item(kind, Rarity.Blue).copy(id = id)

  private def material(kind: MaterialKind, id: Long): Item = MaterialGenerator.item(kind).copy(id = id)

  private def beltOf(slots: BeltSlot*): Item =
    Item(9L, "Пояс", 1L, Rarity.Blue, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Belt(slots.toList))

  private final case class Fix(state: VillageLairState, dao: TestHeroDao, inv: TestInventoryRepository, r: TestRenderer)

  private def nest(items: List[Item], h: Hero = hero()): Task[Fix] =
    for {
      c <- ZIO.attempt(SceneContent.load())
      d <- TestHeroDao.withHero(userId, h)
      _ <- NpcQuestLog.save(d, userId, NpcQuests.empty.copy(village = Some(contract)))
      inv = TestInventoryRepository.withItems(paper :: items)
      r <- TestRenderer.make
    } yield Fix(VillageLairState(d, inv, c, VillageLair.Spiders), d, inv, r)

  private def battleOf(d: TestHeroDao): Task[Option[SoloPveBattle]] =
    d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def lastText(r: TestRenderer): Task[String] = r.sentScreens.map(_.lastOption.fold("")(_.text))

  /** Все пауки боя: тот, кто в паре, и строй с очередью. */
  private def breeds(b: SoloPveBattle): List[Option[Breed]] =
    b.monsterBreed :: (b.group.others ++ b.group.queue).map(_.breed)

  override def spec = suite("Гнездо: вещи у входа")(

    test("у входа — «Использовать предмет»: скарб кнопками, сперва пояс, потом сумка; стопок и пустого нет") {
      val belt = beltOf(BeltSlot.Flask(FlaskKind.Smoke, Rarity.Blue, 3))
      for {
        f     <- nest(List(material(MaterialKind.SpiderBloom, 31L)), hero(belt))
        _     <- f.state.enter(testUser, f.r)
        intro <- f.r.sentScreens.map(_.last)
        _     <- f.state.action(testUser, tap("SpidersSupply"), f.r)
        list  <- f.r.sentScreens.map(_.last)
        ids    = list.choices.map(_.id)
      } yield assertTrue(intro.choices.map(_.id).contains("SpidersSupply")) &&
              assertTrue(ids.headOption.contains("SpidersBelt_0") && ids.contains("SpidersUse_31") &&
                ids.contains("SpidersUse_901") && ids.contains("SpidersBack"))
    },

    test("дымная фляга: пауки вялые — атака и энергия на 20% ниже; у фляги ушёл один заряд") {
      val smoke = flask(FlaskKind.Smoke, 31L)
      for {
        f     <- nest(List(smoke))
        _     <- f.state.action(testUser, tap("SpidersUse_31"), f.r)
        line  <- lastText(f.r)
        _     <- f.state.action(testUser, tap("SpidersFight"), f.r)
        b     <- battleOf(f.dao)
        left   = f.inv.snapshot.find(_.id == 31L).flatMap(CaveSupply.charged).map(_.charges)
        young  = Villages.spider(Breed.YoungSpider, QuestLevel)
        weak   = CaveSupply.weaken(young).fightStats.atk
        cut    = b.exists(x => x.monsterBreed.contains(Breed.YoungSpider) && x.monsterStats.atk == weak)
      } yield assertTrue(line.contains("атака и энергия у них просели на 20%")) &&
              assertTrue(left == CaveSupply.charged(smoke).map(_.charges - 1)) &&
              assertTrue(cut && weak < young.fightStats.atk)
    },

    test("огненная фляга и рубиновая пыль: все пауки начинают бой горящими — пламя 4%") {
      for {
        f    <- nest(List(flask(FlaskKind.Fire, 31L)))
        _    <- f.state.action(testUser, tap("SpidersUse_31"), f.r)
        line <- lastText(f.r)
        _    <- f.state.action(testUser, tap("SpidersFight"), f.r)
        b    <- battleOf(f.dao)
        g    <- nest(List(material(MaterialKind.RubyDust, 41L)))
        _    <- g.state.action(testUser, tap("SpidersUse_41"), g.r)
        _    <- g.state.action(testUser, tap("SpidersFight"), g.r)
        b2   <- battleOf(g.dao)
        fire   = Some(Burn(VillageRates.NestBurnPct))
        lit1   = b.exists(x => x.effects.monsterBurn == fire && x.group.others.forall(_.effects.monsterBurn == fire))
        lit2   = b2.exists(x => x.effects.monsterBurn == fire && x.group.others.forall(_.effects.monsterBurn == fire))
      } yield assertTrue(VillageRates.NestBurnPct == 4 && line.contains("пламя 4%")) &&
              assertTrue(lit1 && lit2) &&
              assertTrue(!g.inv.snapshot.exists(_.id == 41L))
    },

    test("глефовый гриб: пауки встречают героя отравленными") {
      for {
        f <- nest(List(material(MaterialKind.GlaiveMushroom, 31L)))
        _ <- f.state.action(testUser, tap("SpidersUse_31"), f.r)
        _ <- f.state.action(testUser, tap("SpidersFight"), f.r)
        b <- battleOf(f.dao)
        all = b.exists(x => x.effects.monsterPoison.contains(Poison.onHit) &&
                x.group.others.forall(_.effects.monsterPoison.contains(Poison.onHit)))
      } yield assertTrue(all)
    },

    test("паучий цвет: выводок уходит — в бою только матка и двое красных, и начало боя своё") {
      for {
        f     <- nest(List(material(MaterialKind.SpiderBloom, 31L)))
        _     <- f.state.action(testUser, tap("SpidersUse_31"), f.r)
        line  <- lastText(f.r)
        _     <- f.state.action(testUser, tap("SpidersFight"), f.r)
        start <- lastText(f.r)
        b     <- battleOf(f.dao)
        all    = b.toList.flatMap(breeds)
      } yield assertTrue(line.contains("Внизу остаются только матка и двое красных")) &&
              assertTrue(all.count(_.contains(Breed.SpiderMother)) == 1 &&
                all.count(_.contains(Breed.RedSpider)) == VillageRates.LuredRedSpiders && all.size == 3) &&
              assertTrue(VillageRates.LuredRedSpiders == 2 && start.contains("только двое красных"))
    },

    test("второй раз то же — «уже», вещь всё равно уходит; ненужная — впустую; сюжетное не трогают") {
      for {
        f      <- nest(List(material(MaterialKind.SpiderBloom, 31L), material(MaterialKind.SpiderBloom, 32L),
                    material(MaterialKind.Mithril, 33L)))
        _      <- f.state.action(testUser, tap("SpidersUse_31"), f.r)
        _      <- f.state.action(testUser, tap("SpidersUse_32"), f.r)
        again  <- lastText(f.r)
        _      <- f.state.action(testUser, tap("SpidersUse_33"), f.r)
        wasted <- lastText(f.r)
        _      <- f.state.action(testUser, tap("SpidersUse_901"), f.r)
        quest  <- lastText(f.r)
      } yield assertTrue(again.contains("тут уже не поможет") && wasted.contains("Гнезду до этого нет никакого дела")) &&
              assertTrue(quest.contains("эта вещь вам ещё пригодится")) &&
              assertTrue(f.inv.snapshot.map(_.id) == List(901L))
    },

    test("с пояса — как из сумки: у ячейки уходит один заряд") {
      val belt = beltOf(BeltSlot.Flask(FlaskKind.Smoke, Rarity.Blue, 3))
      for {
        f    <- nest(Nil, hero(belt))
        _    <- f.state.action(testUser, tap("SpidersBelt_0"), f.r)
        line <- lastText(f.r)
        h    <- f.dao.getHeroByUserId(userId).map(_.get)
        slots = h.equipment.belt.details match {
                  case b: ItemDetails.Belt => b.slots
                  case _                   => Nil
                }
      } yield assertTrue(line.contains("атака и энергия у них просели")) &&
              assertTrue(slots == List(BeltSlot.Flask(FlaskKind.Smoke, Rarity.Blue, 2)))
    },

    test("ушёл от оврага — подготовка пропала: бой снова с полным выводком") {
      for {
        f <- nest(List(material(MaterialKind.SpiderBloom, 31L)))
        _ <- f.state.action(testUser, tap("SpidersUse_31"), f.r)
        _ <- f.state.action(testUser, tap("SpidersLeave"), f.r)
        _ <- f.state.enter(testUser, f.r)
        _ <- f.state.action(testUser, tap("SpidersFight"), f.r)
        b <- battleOf(f.dao)
        young = b.toList.flatMap(breeds).count(_.contains(Breed.YoungSpider))
      } yield assertTrue(young == VillageRates.SpiderYoung)
    },

    test("подготовка — своими ключами: отметка победы за неё не читается, и она — не отметка победы") {
      val prep = NestPrep(weakened = true, lured = true, page = 2)
      assertTrue(prep.asJson.as[NestPrep].toOption.contains(prep)) &&
      assertTrue(VillageLairState.wonMark(VillageLair.Spiders).as[NestPrep].isLeft) &&
      assertTrue(!VillageLairState.won(VillageLair.Spiders, prep.asJson)) &&
      // у логова разбойников вещей у входа нет
      assertTrue(VillageLair.Bandits.quest != VillageQuest.SpiderNests)
    },

    test("сонный дурман слабит так же, как дым; чего гнезду не надо — пустое") {
      val dope = BrewKind.item(BrewKind.SleepingDope).copy(id = 31L)
      assertTrue(CaveSupply.nestBoonOf(dope) == pangea.service.state.states.events.cave.NestBoon.Dope) &&
      assertTrue(CaveSupply.nestBoonOf(material(MaterialKind.Mithril, 1L)) == pangea.service.state.states.events.cave.NestBoon.None)
    }
  )
}
