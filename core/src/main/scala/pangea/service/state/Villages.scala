package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{SceneContent, Screen}
import pangea.model.battle.{MonsterSlot, SoloPveBattle}
import pangea.generator.monster.MonsterGenerator
import pangea.model.hero.Hero
import pangea.model.item.QuestItemKind
import pangea.model.monster.{Breed, Monster, Race, Rarity}
import pangea.model.quest.NpcQuests
import pangea.model.state.StateType
import pangea.model.user.UserId
import pangea.model.village.{VillageContract, VillageQuest, VillageRates, VillageScene}
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
  * разбойников, гнездо сетеплётов. Сцены: деревня (VillageState) и логово беды
  * (VillageLairState — одна на все логова, см. [[VillageLair]]). */
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

  /** Подстановки текстов деревни: название, чья она, соседний город, главарь, плата. */
  def args(content: SceneContent, race: Race, roll: Int, cityRoll: Int, leaderName: Option[String],
           silver: Long): Seq[(String, String)] =
    Seq("name" -> name(content, race, roll), "race" -> race.genitivePlural, "city" -> city(content, cityRoll),
      "leader" -> leader(leaderName), "silver" -> silver.toString)

  def args(content: SceneContent, scene: VillageScene): Seq[(String, String)] =
    args(content, scene.race, scene.name, scene.city, scene.leader, scene.silver)

  def args(content: SceneContent, c: VillageContract): Seq[(String, String)] =
    args(content, c.race, c.name, c.city, c.leader, c.silver)

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

  /** Дело сделано — вернуться к старосте за платой. */
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

  /** Староста платит: договор закрыт, контракт уходит из сумки, серебро и опыт — герою.
    * Договор стирается первым — повторный вход второй платы не даст. Возвращает
    * героя после и сколько опыта начислено. */
  def pay(heroDao: HeroDao, inventory: InventoryRepository, userId: UserId, hero: Hero,
          contract: VillageContract, nowMs: Long): Task[(Hero, Long)] = {
    val exp     = hero.expFrom(VillageRates.exp(contract.level), nowMs)
    val leveled = hero.gainExp(exp).copy(silver = hero.silver + contract.silver)
    for {
      _ <- NpcQuestLog.modify(heroDao, userId)(_.copy(village = None))
      _ <- QuestSupport.removeItems(inventory, hero, Set(QuestItemKind.VillageContract))
      _ <- heroDao.updateSilver(userId, leveled.silver)
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
      case VillageQuest.BanditLair  => banditGang(contract, rng)
      case VillageQuest.SpiderNests => spiderBrood(contract, rng)
    }

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
      content.format(if (c.done) "village.card.return" else s"village.card.${c.quest.key}", args(content, c): _*)
    }

  /** Строка журнала: что делать по контракту сейчас. */
  def journalStep(content: SceneContent, c: VillageContract): String =
    content.format(if (c.done) "journal.village.return" else s"journal.village.${c.quest.key}", args(content, c): _*)
}
