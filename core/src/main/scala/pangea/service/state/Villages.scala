package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{SceneContent, Screen}
import pangea.model.battle.{BattleAlly, Convoy, MonsterSlot, SoloPveBattle, Wagon}
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.{Breed, Monster, Race, Rarity}
import pangea.model.quest.NpcQuests
import pangea.model.squad.{AllyKind, UndeadForm}
import pangea.model.state.StateType
import pangea.model.user.UserId
import pangea.model.village.{CaravanAmbush, VillageContract, VillageQuest, VillageRates, VillageScene}
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import zio.{Task, ZIO}

/** Логово беды деревни — куда ведёт контракт, пока дело не сделано: своя
  * сцена, свой сюжетный бой и свои тексты (`village.<texts>.*`), кнопки —
  * `<ids>Fight` и соседи, отметка победы в scene_data — `wonKey`. Сцена у всех
  * логов одна — VillageLairState. Ключи логова разбойников — те же, что были у
  * его сцены до того, как она стала общей: в базе могут лежать его бои и
  * отметки. */
final case class VillageLair(quest: VillageQuest, scene: StateType, story: String, ids: String, texts: String,
                             wonKey: String)

object VillageLair {
  val Bandits: VillageLair =
    VillageLair(VillageQuest.BanditLair, StateType.VillageBandits, Villages.BanditsStory, "Bandits", "bandits", "villageBanditsWon")
  val Spiders: VillageLair =
    VillageLair(VillageQuest.SpiderNests, StateType.VillageSpiders, Villages.SpidersStory, "Spiders", "spiders", "villageSpidersWon")

  val all: List[VillageLair] = List(Bandits, Spiders)

  def of(quest: VillageQuest): Option[VillageLair] = all.find(_.quest == quest)
}

/** «Деревня» — событие лабиринта (1%, с [[MinDungeonLevel]]-го поворота): за
  * поворотом изгороди — деревня одной из смертных рас, и видно, что ей нужна
  * помощь. Раса, название и беда бросаются при встрече ([[VillageScene]]).
  *
  * Деревню можно разграбить — сюжетный бой с её жителями и сторожевой башней,
  * как налёт на мурлоков, награда как за обычный бой. Можно пойти к старосте:
  * он долго рассказывает, что стряслось, и просит помочь за плату. Взялся —
  * в сумке «Контракт деревни» ([[QuestItemKind.VillageContract]]), а в
  * `npc_quests` — договор ([[VillageContract]]); дальше ведёт карточка: к
  * делу, потом — назад за платой. Пока контракт при герое, деревни ему не
  * встречаются (см. DungeonState).
  *
  * Беды открываются по уровням лабиринта ([[VillageQuest.minLevel]]): логово
  * разбойников, гнездо сетеплётов, караван, украденный артефакт. Сцены:
  * деревня (VillageState), логово беды (VillageLairState — одна на все логова,
  * см. [[VillageLair]]), караван в пути (VillageCaravanState) и погоня за
  * ворами артефакта (VillageTrailState). */
object Villages {

  /** Ключ сюжетного боя — налёта на деревню (см. `SoloPveBattle.story`):
    * без «Сбежать», награда как за обычный бой уровня деревни. */
  val RaidStory: String = "villageRaid"

  /** Ключ сюжетного боя в логове разбойников: так же — без «Сбежать», награда
    * как за обычный бой уровня задания. */
  val BanditsStory: String = "villageBandits"

  /** Ключ сюжетного боя в гнезде сетеплётов: так же — без «Сбежать», награда
    * как за обычный бой уровня задания, а добыча со зверей — паучья. */
  val SpidersStory: String = "villageSpiders"

  /** Ключ сюжетного боя в засаде на караван: без «Сбежать», награда как за
    * обычный бой уровня задания. Пал — караван разграблен, контракт сорван. */
  val CaravanStory: String = "villageCaravan"

  /** Ключ сюжетного боя в стане воров артефакта: без «Сбежать», награда как за
    * обычный бой уровня задания. Пал — след остыл, контракт при герое. */
  val ArtifactStory: String = "villageArtifact"

  /** С какого поворота встречаются деревни: порог события (см.
    * `StateType.EventMinLevel`). К их бедам ходят не ближе. */
  val MinDungeonLevel: Int = StateType.EventMinLevel.getOrElse(StateType.Village, 1)

  def deepEnough(dungeonLevel: Int): Boolean = dungeonLevel >= MinDungeonLevel

  // ── Названия ────────────────────────────────────────────────────────────────

  /** Названия деревень этой расы (`village.names.<раса>`). */
  def names(content: SceneContent, race: Race): List[String] = content.list(s"village.names.${race.entryName}")

  /** Название по броску: строка из списка расы. */
  def name(content: SceneContent, race: Race, roll: Int): String = {
    val all = names(content, race)
    all.lift(Math.floorMod(roll, all.size.max(1))).getOrElse("")
  }

  /** Соседние города (`village.cities`): лабиринт выводит куда угодно в
    * Пангее, и о Кинэте в деревне могут не знать — живут своими городами. */
  def cities(content: SceneContent): List[String] = content.list("village.cities")

  def city(content: SceneContent, roll: Int): String = {
    val all = cities(content)
    all.lift(Math.floorMod(roll, all.size.max(1))).getOrElse("")
  }

  /** Местность вокруг деревни этой расы (`village.lands.<раса>`): куда вывела изгородь. */
  def lands(content: SceneContent, race: Race): List[String] = content.list(s"village.lands.${race.entryName}")

  def land(content: SceneContent, race: Race, roll: Int): String = {
    val all = lands(content, race)
    all.lift(Math.floorMod(roll, all.size.max(1))).getOrElse("")
  }

  /** Главарь банды по имени; имени нет — так, как зовут именного разбойника по таблице. */
  def leader(name: Option[String]): String =
    name.getOrElse(Monster.namesByRaceRarity.getOrElse((VillageRates.BanditLeaderDefaultRace, VillageRates.BanditLeaderRarity), ""))

  /** Святыня деревни этой расы (`village.artifacts.<раса>`) — её крадут воры в
    * беде «Украденный артефакт»: как она зовётся, она же в винительном падеже
    * («вынесли Жемчужину Прилива») и что это за вещь — словами старосты. */
  def artifact(content: SceneContent, race: Race): String = content.text(s"village.artifacts.${race.entryName}.name")

  def artifactAcc(content: SceneContent, race: Race): String = content.text(s"village.artifacts.${race.entryName}.acc")

  def artifactAbout(content: SceneContent, race: Race): String = content.text(s"village.artifacts.${race.entryName}.about")

  /** Подстановки текстов деревни: название, чья она, соседний город, главарь,
    * плата — серебро числом (`silver`) и вся плата словами (`pay`, см.
    * [[payLine]]), святыня деревни — как зовётся, в винительном падеже и что это
    * за вещь (см. [[artifact]]). */
  def args(content: SceneContent, race: Race, roll: Int, cityRoll: Int, leaderName: Option[String],
           silver: Long, doubloons: Long): Seq[(String, String)] =
    Seq("name" -> name(content, race, roll), "race" -> race.genitivePlural, "city" -> city(content, cityRoll),
      "leader" -> leader(leaderName), "silver" -> silver.toString, "pay" -> payLine(content, silver, doubloons),
      "artifact" -> artifact(content, race), "artifactAcc" -> artifactAcc(content, race),
      "artifactAbout" -> artifactAbout(content, race))

  def args(content: SceneContent, scene: VillageScene): Seq[(String, String)] =
    args(content, scene.race, scene.name, scene.city, scene.leader, scene.silver, scene.doubloons)

  def args(content: SceneContent, c: VillageContract): Seq[(String, String)] =
    args(content, c.race, c.name, c.city, c.leader, c.silver, c.doubloons)

  /** Плата словами: «350 серебра», а с дублонами — «500 серебра и 1 дублон»
    * (см. [[VillageRates.doubloons]]). */
  def payLine(content: SceneContent, silver: Long, doubloons: Long): String =
    if (doubloons <= 0L) content.format("village.payLine.silver", "silver" -> silver.toString)
    else content.format("village.payLine.both",
      "silver"    -> silver.toString,
      "doubloons" -> doubloons.toString,
      "word"      -> content.list("village.doubloonWords").lift(pluralForm(doubloons)).getOrElse(""))

  /** Какое слово из трёх: «1 дублон», «2 дублона», «5 дублонов» (и «11 дублонов», «21 дублон»). */
  private def pluralForm(n: Long): Int =
    if (n % 10 == 1 && n % 100 != 11) 0
    else if ((2L to 4L).contains(n % 10) && !(12L to 14L).contains(n % 100)) 1
    else 2

  /** Абзацы одним сообщением: тексты блоком (`|`) кончаются переводом строки —
    * без обрезки между абзацами выходило бы по две пустые строки. */
  def joined(parts: String*): String = Screen.joined(parts.map(_.trim): _*)

  // ── Ход контракта ───────────────────────────────────────────────────────────

  /** Взят ли контракт какой-нибудь деревни (выполнен — его уже нет). */
  def taken(q: NpcQuests): Boolean = q.village.isDefined

  /** Логово этой беды ещё стоит — идти к нему. */
  def toLair(q: NpcQuests, quest: VillageQuest): Boolean =
    q.village.exists(c => c.quest == quest && c.step == VillageContract.Taken)

  /** Логово разбойников ещё стоит — идти к нему. */
  def toBandits(q: NpcQuests): Boolean = toLair(q, VillageQuest.BanditLair)

  /** Куда ведёт контракт, пока дело не сделано: логово его беды. */
  def lairOf(q: NpcQuests): Option[VillageLair] =
    q.village.filter(_.step == VillageContract.Taken).flatMap(c => VillageLair.of(c.quest))

  /** Караван ещё не дошёл до города — идти к нему. */
  def toCaravan(q: NpcQuests): Boolean = toLair(q, VillageQuest.CaravanEscort)

  /** Артефакт ещё у воров — идти по их следу. */
  def toTrail(q: NpcQuests): Boolean = toLair(q, VillageQuest.StolenArtifact)

  /** Куда ведёт контракт, пока дело не сделано: сцена беды и ключ её текстов
    * (`village.card.<ключ>Label`) — логово, караван или след воров. */
  def goTo(q: NpcQuests): Option[(StateType, String)] =
    q.village.filter(_.step == VillageContract.Taken).flatMap { c =>
      c.quest match {
        case VillageQuest.CaravanEscort  => Some(StateType.VillageCaravan -> c.quest.key)
        case VillageQuest.StolenArtifact => Some(StateType.VillageTrail -> c.quest.key)
        case _                           => VillageLair.of(c.quest).map(l => l.scene -> l.texts)
      }
    }

  /** Дело сделано — вернуться к старосте за платой. У каравана такого шага нет:
    * за него платят в городе, куда он дошёл. */
  def toPay(q: NpcQuests): Boolean = q.village.exists(_.done)

  /** Взять контракт: предмет — в сумку, договор — в журнал. Уже взят
    * другой — ничего (false). */
  def take(heroDao: HeroDao, inventory: InventoryRepository, items: ItemRepository,
           userId: UserId, hero: Hero, contract: VillageContract): Task[Boolean] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (taken(q)) ZIO.succeed(false)
      else QuestSupport.giveItem(inventory, items, hero, QuestItemKind.VillageContract) *>
        NpcQuestLog.save(heroDao, userId, q.copy(village = Some(contract))).as(true)
    }

  /** Логово беды разбито: дальше — к старосте за платой. Не тот шаг — ничего (false). */
  def lairCleared(heroDao: HeroDao, userId: UserId, quest: VillageQuest): Task[Boolean] =
    NpcQuestLog.load(heroDao, userId).flatMap { q =>
      if (!toLair(q, quest)) ZIO.succeed(false)
      else NpcQuestLog.save(heroDao, userId, q.copy(village = q.village.map(_.copy(step = VillageContract.Done)))).as(true)
    }

  /** Староста платит: договор закрыт, контракт уходит из сумки, серебро,
    * дублоны (если обещаны) и опыт — герою. Договор стирается первым —
    * повторный вход второй платы не даст. Возвращает героя после и сколько
    * опыта начислено. */
  def pay(heroDao: HeroDao, inventory: InventoryRepository, userId: UserId, hero: Hero,
          contract: VillageContract, nowMs: Long): Task[(Hero, Long)] = {
    val exp     = hero.expFrom(VillageRates.exp(contract.level), nowMs)
    val leveled = hero.gainExp(exp).copy(silver = hero.silver + contract.silver,
                    doubloons = hero.doubloons + contract.doubloons.max(0L))
    for {
      _ <- NpcQuestLog.modify(heroDao, userId)(_.copy(village = None))
      _ <- QuestSupport.removeItems(inventory, hero, Set(QuestItemKind.VillageContract))
      _ <- heroDao.updateSilver(userId, leveled.silver)
      _ <- ZIO.when(contract.doubloons > 0L)(heroDao.updateDoubloons(userId, leveled.doubloons))
      _ <- heroDao.updateExpAndLevel(userId, leveled.exp, leveled.lvl, leveled.upgradePoints)
    } yield (leveled, exp)
  }

  /** Отказаться от контракта: он уходит из сумки, договор — из журнала, деревни снова встречаются. */
  def abandon(heroDao: HeroDao, inventory: InventoryRepository, userId: UserId, hero: Hero): Task[Unit] =
    NpcQuestLog.modify(heroDao, userId)(_.copy(village = None)) *>
      QuestSupport.removeItems(inventory, hero, Set(QuestItemKind.VillageContract))

  // ── Бои ─────────────────────────────────────────────────────────────────────

  private def roll(lvl: Int, race: Race, n: Int, pool: List[Rarity], rng: Rng): (List[Monster], Rng) =
    (1 to n).foldLeft((List.empty[Monster], rng)) { case ((acc, r), _) =>
      val (rarity, next) = r.pick(pool)
      (acc :+ MonsterGenerator.generateOfRaceAndRarity(lvl, race, rarity), next)
    }

  /** Защитники деревни на налёт: строй, очередь за ним, последним — именной, а
    * в хвосте — сторожевые башни (их `SoloPveBattle.fromGroup` ставит на
    * последние места строя, как у каравана). Всё — уровня деревни. */
  def raidMonsters(scene: VillageScene, rng: Rng): List[Monster] = {
    val lvl          = scene.level.max(1)
    val (front, r1)  = roll(lvl, scene.race, VillageRates.RaidFront, VillageRates.RaidFrontRarities, rng)
    val (queue, r2)  = roll(lvl, scene.race, VillageRates.RaidQueue, VillageRates.RaidQueueRarities, r1)
    val (named, _)   = MonsterGenerator.legendaryName(scene.race, r2)
    val last         = MonsterGenerator.generateOfRaceAndRarity(lvl, scene.race, VillageRates.RaidLastRarity, named)
    front ++ queue ++ (last :: List.fill(VillageRates.RaidTowers)(MonsterGenerator.tower(lvl)))
  }

  /** Банда в логове: сброд уровня задания — у каждого своя раса из смертных
    * рас лабиринта; главарь — последним, той расы и под тем именем, что выпали
    * при встрече с деревней. */
  def banditGang(contract: VillageContract, rng: Rng): List[Monster] = {
    val lvl         = contract.level.max(1)
    val (front, r1) = mixed(lvl, VillageRates.BanditFront, VillageRates.BanditFrontRarities, rng)
    val (back, _)   = mixed(lvl, VillageRates.BanditBack, VillageRates.BanditBackRarities, r1)
    val chief       = contract.leaderRace.getOrElse(VillageRates.BanditLeaderDefaultRace)
    front ++ back :+ MonsterGenerator.generateOfRaceAndRarity(lvl, chief, VillageRates.BanditLeaderRarity,
      Some(leader(contract.leader)))
  }

  /** Обитатели логова беды контракта: банда или выводок. */
  def lairMonsters(contract: VillageContract, rng: Rng): List[Monster] =
    contract.quest match {
      case VillageQuest.BanditLair     => banditGang(contract, rng)
      case VillageQuest.SpiderNests    => spiderBrood(contract, rng)
      // Логова у каравана нет — его бои в засадах (см. caravanAmbush).
      case VillageQuest.CaravanEscort  => caravanAmbush(contract, CaravanAmbush.Bandits, rng)
      // И у артефакта логова нет — бой в стане воров, в конце погони.
      case VillageQuest.StolenArtifact => thiefGang(contract, rng)
    }

  /** Стан воров уровня задания: первым — вор с артефактом, именной, любой из
    * смертных рас лабиринта, имя — из списка его расы; за ним — сброд всех
    * кровей, у каждого своя раса из тех же. Староста воров не видел, так что вор
    * бросается в начале боя, а не при встрече с деревней. Первым он стоит,
    * чтобы его можно было достать, пока он не ушёл (см.
    * [[VillageRates.ThiefEscapeRounds]]). */
  def thiefGang(contract: VillageContract, rng: Rng): List[Monster] = {
    val lvl         = contract.level.max(1)
    val (race, r1)  = rng.pick(VillageRates.BanditRaces)
    val (named, r2) = MonsterGenerator.legendaryName(race, r1)
    val (front, r3) = mixed(lvl, VillageRates.ThiefFront, VillageRates.ThiefFrontRarities, r2)
    val (back, _)   = mixed(lvl, VillageRates.ThiefBack, VillageRates.ThiefBackRarities, r3)
    MonsterGenerator.generateOfRaceAndRarity(lvl, race, VillageRates.ThiefRarity, named) :: (front ++ back)
  }

  /** Загнанный вор уровня задания: он сам — той же расы и под тем же именем,
    * что ушёл из стана (не запомнили — бросок заново), первым, и
    * [[VillageRates.CorneredHenchmen]] подручных покрепче, сброд тех же рас. */
  def cornered(contract: VillageContract, thief: Option[String], thiefRace: Option[Race], rng: Rng): List[Monster] = {
    val lvl          = contract.level.max(1)
    val (rolled, r1) = rng.pick(VillageRates.BanditRaces)
    val race         = thiefRace.getOrElse(rolled)
    val (named, r2)  = thief.fold(MonsterGenerator.legendaryName(race, r1))(n => (Some(n), r1))
    val (crew, _)    = mixed(lvl, VillageRates.CorneredHenchmen, VillageRates.ThiefBackRarities, r2)
    MonsterGenerator.generateOfRaceAndRarity(lvl, race, VillageRates.ThiefRarity, named) :: crew
  }

  /** Засада на караван уровня задания: четверо в строю и трое за ними. Банда —
    * сборная, из смертных рас лабиринта; Чернолом — гоблины-лесорубы; ищейки
    * Сноходцев — каджиты (кинжал молнии у них — умение расы). */
  def caravanAmbush(contract: VillageContract, ambush: CaravanAmbush, rng: Rng): List[Monster] = {
    val lvl = contract.level.max(1)
    def rows(race: Option[Race]): List[Monster] = {
      def row(n: Int, pool: List[Rarity], r: Rng): (List[Monster], Rng) =
        race.fold(mixed(lvl, n, pool, r))(roll(lvl, _, n, pool, r))
      val (front, r1) = row(VillageRates.CaravanAmbushFront, VillageRates.CaravanAmbushFrontRarities, rng)
      val (back, _)   = row(VillageRates.CaravanAmbushBack, VillageRates.CaravanAmbushBackRarities, r1)
      front ++ back
    }
    ambush match {
      case CaravanAmbush.Bandits   => rows(None)
      case CaravanAmbush.Chernolom => rows(Some(Race.Goblin))
      case CaravanAmbush.Hounds    => rows(Some(Race.Khajiit))
    }
  }

  /** Караванщик в строю героя: по силе — редкий той же расы, что деревня, уровня
    * задания. Имя, уровень и статы лежат на самом союзнике (UndeadForm), как у
    * капитанов каджитов в «Большом деле»; после боя в отряд он не встаёт. */
  def caravanGuard(contract: VillageContract, name: String): BattleAlly = {
    val lvl   = contract.level.max(1)
    val stats = MonsterGenerator.generateOfRaceAndRarity(lvl, contract.race, VillageRates.CaravanGuardRarity).fightStats
    BattleAlly(AllyKind.CaravanGuard, position = 0, hp = stats.hp, armor = stats.armor, energy = stats.energy,
      stats = stats, lvl = lvl.toLong, undead = Some(UndeadForm(name, lvl.toLong, stats)), home = 0)
  }

  /** Поставить караванщиков в строй: каждого — на свою повозку, которую никто
    * не занял (первую по порядку), а повозки все прикрыты — на ближайшее к герою
    * свободное место. Отряд занял всё — кому места нет, тот бьётся из-за повозок
    * и в строй не встаёт. */
  def withGuards(battle: SoloPveBattle, contract: VillageContract, names: List[String]): SoloPveBattle =
    names.foldLeft(battle) { (b, name) =>
      val g       = b.group
      val onWagon = g.convoy.toList.flatMap(_.wagons.filter(_.alive).map(_.position))
                      .find(p => p != g.heroPos && g.allyAt(p).isEmpty)
      onWagon.orElse(g.nearestFreeAllyPlace).fold(b)(pos =>
        b.copy(group = g.copy(allies = g.allies :+ caravanGuard(contract, name).copy(position = pos))))
    }

  /** Повозки обоза у ворот: [[VillageRates.CaravanWagons]], на первых местах ряда
    * героя; прочность каждой — HP и броня редкого моба той же расы, что деревня,
    * уровня задания (так решил пользователь). */
  def caravanWagons(contract: VillageContract): List[Wagon] = {
    val s = MonsterGenerator.generateOfRaceAndRarity(contract.level.max(1), contract.race, VillageRates.CaravanWagonRarity).fightStats
    (1 to VillageRates.CaravanWagons).toList.map(p => Wagon(p, s.hp, s.armor, s.hp, s.armor))
  }

  /** Обоз в засаде: повозки, какими дошли, и повадки тех, кто напал. */
  def convoy(wagons: List[Wagon], ambush: CaravanAmbush): Convoy =
    Convoy(wagons, ambush.hitsWagons, ambush.ignitePct, ambush.plunders)

  /** Выводок в гнезде: молодые, взрослые и матка — уровня задания; с шансом
    * [[VillageRates.RedSpiderChancePct]]% последним — ещё и красный. */
  def spiderBrood(contract: VillageContract, rng: Rng): List[Monster] = {
    val (roll, _) = rng.between(0L, 100L)
    spiderBrood(contract, red = roll < VillageRates.RedSpiderChancePct.toLong)
  }

  def spiderBrood(contract: VillageContract, red: Boolean): List[Monster] = {
    val lvl = contract.level.max(1)
    List.fill(VillageRates.SpiderYoung)(spider(Breed.YoungSpider, lvl)) ++
      List.fill(VillageRates.SpiderAdult)(spider(Breed.AdultSpider, lvl)) ++
      (spider(Breed.SpiderMother, lvl) :: Option.when(red)(spider(Breed.RedSpider, lvl)).toList)
  }

  /** Гнездо, из которого паучий цвет увёл выводок (см. `NestPrep.lured`):
    * матка и [[VillageRates.LuredRedSpiders]] красных — так решил пользователь. */
  def luredBrood(contract: VillageContract): List[Monster] = {
    val lvl = contract.level.max(1)
    spider(Breed.SpiderMother, lvl) :: List.fill(VillageRates.LuredRedSpiders)(spider(Breed.RedSpider, lvl))
  }

  /** Паук этой породы: зверь, но сила и ранг — как у моба-образца
    * ([[VillageRates.SpiderLike]]), имя — породы. */
  def spider(breed: Breed, lvl: Int): Monster = {
    val (race, rarity) = VillageRates.SpiderLike.getOrElse(breed, Race.Orc -> Rarity.Common)
    val like           = MonsterGenerator.generateOfRaceAndRarity(lvl.max(1), race, rarity)
    Monster(0L, like.lvl, VillageRates.SpiderRace, rarity, like.fightStats, customName = Some(breed.displayName),
      breed = Some(breed))
  }

  /** Новорождённый сетеплёт — его рождает матка в бою: тот же молодой, только
    * на [[VillageRates.NewbornPct]]% его силы. */
  def newborn(lvl: Int): Monster = {
    val young = spider(Breed.YoungSpider, lvl)
    val s     = young.fightStats
    def cut(v: Long): Long = v * VillageRates.NewbornPct / 100L
    young.copy(
      fightStats = s.copy(atk = cut(s.atk).max(1L), hp = cut(s.hp).max(1L), armor = cut(s.armor),
        defence = cut(s.defence), evasion = cut(s.evasion), accuracy = cut(s.accuracy)),
      customName = Some(Breed.NewbornSpider.displayName), breed = Some(Breed.NewbornSpider))
  }

  /** Повадки пород, что ложатся на бой при его начале: удар молодого
    * сетеплёта, дошедший до HP, травит — всегда, и героя, и союзников
    * (`monsterPoisonsOnHit`, как у Крысиного короля). */
  def withBreedTraits(battle: SoloPveBattle): SoloPveBattle = {
    def poisons(b: Option[Breed]) = b.contains(Breed.YoungSpider)
    def slot(s: MonsterSlot): MonsterSlot =
      if (poisons(s.breed)) s.copy(effects = s.effects.copy(monsterPoisonsOnHit = true)) else s
    battle.copy(
      effects = if (poisons(battle.monsterBreed)) battle.effects.copy(monsterPoisonsOnHit = true) else battle.effects,
      group   = battle.group.copy(others = battle.group.others.map(slot), queue = battle.group.queue.map(slot)))
  }

  /** `n` бандитов уровня `lvl`: раса у каждого своя — из [[VillageRates.BanditRaces]],
    * редкость — из `pool`. */
  private def mixed(lvl: Int, n: Int, pool: List[Rarity], rng: Rng): (List[Monster], Rng) =
    (1 to n).foldLeft((List.empty[Monster], rng)) { case ((acc, r), _) =>
      val (race, r1)   = r.pick(VillageRates.BanditRaces)
      val (rarity, r2) = r1.pick(pool)
      (acc :+ MonsterGenerator.generateOfRaceAndRarity(lvl, race, rarity), r2)
    }

  // ── Карточка и журнал ───────────────────────────────────────────────────────

  /** Что контракт говорит на карточке сейчас: чья деревня, о чём просили и куда
    * идти — или что пора за платой. Контракта нет — ничего. */
  def cardLine(content: SceneContent, q: NpcQuests): String =
    q.village.fold("") { c =>
      content.format(if (c.done) s"village.card.${doneKey(c)}" else s"village.card.${c.quest.key}", args(content, c): _*)
    }

  /** Строка журнала: что делать по контракту сейчас. */
  def journalStep(content: SceneContent, c: VillageContract): String =
    content.format(if (c.done) s"journal.village.${doneKey(c)}" else s"journal.village.${c.quest.key}", args(content, c): _*)

  /** Ключ текста «пора за платой»: у артефакта свой — вещь при герое, её и
    * несут старосте; у прочих бед — общий. */
  private def doneKey(c: VillageContract): String =
    if (c.quest == VillageQuest.StolenArtifact) "artifactReturn" else "return"
}
