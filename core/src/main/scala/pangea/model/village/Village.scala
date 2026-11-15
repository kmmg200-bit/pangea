package pangea.model.village

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import pangea.model.battle.{Convoy, Wagon}
import pangea.model.monster.{Breed, Race, Rarity}
import pangea.model.quest.BoardRates

/** Беда деревни — то, о чём просит староста (см. [[VillageContract]]). Задания
  * открываются по мере того, как герой уходит дальше в лабиринт ([[minLevel]]):
  * логово разбойников — с того же уровня, что и сами деревни (15-го), гнездо
  * сетеплётов — с 20-го, караван — с 25-го, украденный артефакт — с 35-го;
  * дальше порядок решит пользователь. `entryName` лежит в базе — в
  * `scene_data` и в `npc_quests`: не переименовывать и не удалять.
  *
  * @param key      ключ текстов задания в `scenes.yaml` (`village.trouble.<key>` и соседи)
  * @param minLevel с какого поворота лабиринта староста может о нём попросить
  */
sealed abstract class VillageQuest(val key: String, val minLevel: Int) extends EnumEntry

object VillageQuest extends Enum[VillageQuest] {
  val values: IndexedSeq[VillageQuest] = findValues

  /** Логово разбойников: банда с главарём грабит дороги у деревни — разбить логово. */
  case object BanditLair extends VillageQuest("bandits", 15)

  /** Гнездо сетеплётов: пауки из изгороди засели в овраге у деревни и по ночам
    * таскают скотину и людей — выжечь гнездо с маткой. */
  case object SpiderNests extends VillageQuest("spiders", 20)

  /** Сопровождение каравана: деревня отправляет обоз в соседний город, а
    * дорога неспокойна — проводить его все переходы и отбиться от засад. */
  case object CaravanEscort extends VillageQuest("caravan", 25)

  /** Украденный артефакт: воры вынесли из деревни её святыню (у каждой расы
    * своя, `village.artifacts.<раса>`) и ушли в изгородь — пройти по следу до
    * их стана и вернуть вещь старосте. С 35-го (так решил пользователь). */
  case object StolenArtifact extends VillageQuest("artifact", 35)

  /** О чём староста может попросить на этом повороте. Мельче порога первого
    * задания деревни не встречаются; если всё же встретилась — просят о первом. */
  def openAt(dungeonLevel: Int): List[VillageQuest] =
    values.toList.filter(_.minLevel <= dungeonLevel) match {
      case Nil  => List(BanditLair)
      case open => open
    }

  implicit val encoder: Encoder[VillageQuest] = (q: VillageQuest) => q.entryName.asJson
  implicit val decoder: Decoder[VillageQuest] = (c: HCursor) =>
    c.as[String].flatMap(n => withNameOption(n).toRight(DecodingFailure(s"Unknown village quest '$n'", c.history)))
}

/** Раса в JSON деревни — именем варианта; чужое имя — ошибка разбора. */
private[village] object RaceJson {
  def decode(c: HCursor, field: String): Decoder.Result[Race] =
    c.get[String](field).flatMap(n => Race.withNameOption(n).toRight(DecodingFailure(s"Unknown race '$n'", c.history)))
}

/** Деревня, на которую вышел герой, — в `scene_data`, пока он в ней. Ключи
  * свои (`village…`): в `scene_data` лежат и чужие структуры, и чужое поле не
  * должно читаться как наше.
  *
  * @param race   чья деревня — её жители и выходят на налёт
  * @param name   бросок названия: строка из списка расы (`village.names.<раса>`).
  *               Список можно дополнять, не трогая уже встреченные деревни
  * @param quest  о чём попросит староста
  * @param level  уровень деревни — уровень лабиринта, где её нашли: по нему
  *               защитники при налёте
  * @param questLevel уровень задания старосты — от уровня деревни со сдвигом
  *               [[VillageRates.QuestLevelMin]]…[[VillageRates.QuestLevelMax]], не
  *               глубже дна лабиринта: по нему враги, сложность, плата и опыт
  * @param silver сколько староста заплатит за дело — бросок при встрече (см.
  *               [[VillageRates.silver]]): называет он ту же сумму, что и платит
  * @param leader главарь разбойников — у логова разбойников: выбран вместе с
  *               деревней, чтобы староста назвал того, кто потом и выйдет в бою
  * @param leaderRace раса главаря: банда сборная, и его имя — из списка его расы
  * @param city   бросок соседнего города (`village.cities`): лабиринт выводит
  *               куда угодно в Пангее, и деревня живёт своими городами
  * @param land   бросок местности вокруг (`village.lands.<раса>`)
  * @param step   что сейчас: приход, разговор со старостой, развязка налёта, плата
  * @param doubloons дублоны к серебру — по уровню задания (см.
  *               [[VillageRates.doubloons]]), выпадают при встрече вместе с
  *               серебром. В сценах, начатых до дублонов, поля нет — их нет и в плате
  * @param haggled со старостой уже торговались (см. [[VillageRates.HagglePct]]):
  *               второй раз он и слушать не станет. В сценах, начатых до торга, —
  *               не торговались
  */
final case class VillageScene(
  race:   Race,
  name:   Int,
  quest:  VillageQuest,
  level:  Int,
  questLevel: Int,
  silver: Long,
  leader: Option[String] = None,
  leaderRace: Option[Race] = None,
  city:   Int            = 0,
  land:   Int            = 0,
  step:   String         = VillageScene.Arrival,
  doubloons: Long        = 0L,
  haggled: Boolean       = false
)

object VillageScene {

  /** Только что вышел к деревне: налёт, староста или уйти. */
  val Arrival: String = "arrival"
  /** Староста рассказал о беде: взяться или отказаться. */
  val Headman: String = "headman"
  /** Налёт выигран — развязку покажет вход (сцену сюда кладёт добыча). */
  val AfterRaid: String = "afterRaid"
  /** Развязка налёта показана: пепелище. */
  val Razed: String = "razed"
  /** Староста заплатил: прощание. */
  val Paid: String = "paid"

  implicit val encoder: Encoder[VillageScene] = (s: VillageScene) => Json.obj(
    "villageRace"   -> s.race.entryName.asJson,
    "villageName"   -> s.name.asJson,
    "villageQuest"  -> s.quest.asJson,
    "villageLevel"  -> s.level.asJson,
    "villageQuestLevel" -> s.questLevel.asJson,
    "villageSilver" -> s.silver.asJson,
    "villageLeader" -> s.leader.asJson,
    "villageLeaderRace" -> s.leaderRace.map(_.entryName).asJson,
    "villageCity"   -> s.city.asJson,
    "villageLand"   -> s.land.asJson,
    "villageStep"   -> s.step.asJson,
    "villageDoubloons" -> s.doubloons.asJson,
    "villageHaggled" -> s.haggled.asJson)

  implicit val decoder: Decoder[VillageScene] = (c: HCursor) =>
    for {
      race   <- RaceJson.decode(c, "villageRace")
      name   <- c.getOrElse[Int]("villageName")(0)
      quest  <- c.get[VillageQuest]("villageQuest")
      level  <- c.getOrElse[Int]("villageLevel")(1)
      qLevel <- c.getOrElse[Int]("villageQuestLevel")(level)
      silver <- c.getOrElse[Long]("villageSilver")(VillageRates.silver(qLevel))
      leader <- c.getOrElse[Option[String]]("villageLeader")(None)
      chief  <- c.getOrElse[Option[String]]("villageLeaderRace")(None)
      city   <- c.getOrElse[Int]("villageCity")(0)
      land   <- c.getOrElse[Int]("villageLand")(0)
      step   <- c.getOrElse[String]("villageStep")(Arrival)
      // Сцены, начатые до дублонов: староста называл одно серебро.
      dbl    <- c.getOrElse[Long]("villageDoubloons")(0L)
      // Сцены, начатые до торга: со старостой не торговались.
      haggle <- c.getOrElse[Boolean]("villageHaggled")(false)
    } yield VillageScene(race, name, quest, level, qLevel, silver, leader, chief.flatMap(Race.withNameOption), city, land,
      step, dbl, haggle)
}

/** Контракт деревни, который взял герой (`npc_quests`, поле `village`):
  * чья деревня, о чём просили, за сколько и где задание сейчас. Одна за раз:
  * пока она при герое, деревни в лабиринте ему не встречаются.
  *
  * @param name   бросок названия деревни — как в [[VillageScene.name]]
  * @param level  уровень задания — см. [[VillageScene.questLevel]]
  * @param silver обещанная плата
  * @param leader главарь разбойников — у логова разбойников
  * @param step   [[VillageContract.Taken]] — идти делать; [[VillageContract.Done]] —
  *               сделано, вернуться за платой
  * @param city   бросок соседнего города — как в [[VillageScene.city]]
  * @param leaderRace раса главаря — как в [[VillageScene.leaderRace]]
  * @param doubloons обещанные дублоны — как в [[VillageScene.doubloons]]; в
  *               контрактах, взятых до дублонов, их нет
  */
final case class VillageContract(
  quest:  VillageQuest,
  race:   Race,
  name:   Int,
  level:  Int,
  silver: Long,
  leader: Option[String] = None,
  step:   Int            = VillageContract.Taken,
  city:   Int            = 0,
  leaderRace: Option[Race] = None,
  doubloons: Long        = 0L
) {
  def done: Boolean = step == VillageContract.Done
}

object VillageContract {

  /** Взялся — идти делать. */
  val Taken: Int = 1
  /** Сделано — вернуться к старосте за платой. */
  val Done: Int = 2

  /** Договор на деревню, о которой только что рассказал староста: уровень —
    * задания, а не деревни. */
  def of(scene: VillageScene): VillageContract =
    VillageContract(scene.quest, scene.race, scene.name, scene.questLevel, scene.silver, scene.leader, Taken, scene.city,
      scene.leaderRace, scene.doubloons)

  implicit val encoder: Encoder[VillageContract] = (v: VillageContract) => Json.obj(
    "quest"  -> v.quest.asJson,
    "race"   -> v.race.entryName.asJson,
    "name"   -> v.name.asJson,
    "level"  -> v.level.asJson,
    "silver" -> v.silver.asJson,
    "leader" -> v.leader.asJson,
    "step"   -> v.step.asJson,
    "city"   -> v.city.asJson,
    "leaderRace" -> v.leaderRace.map(_.entryName).asJson,
    "doubloons" -> v.doubloons.asJson)

  /** Декодер рукописный, с запасными значениями: новое поле не должно ронять
    * разбор уже взятых контрактов. */
  implicit val decoder: Decoder[VillageContract] = (c: HCursor) =>
    for {
      quest  <- c.get[VillageQuest]("quest")
      race   <- RaceJson.decode(c, "race")
      name   <- c.getOrElse[Int]("name")(0)
      level  <- c.getOrElse[Int]("level")(1)
      silver <- c.getOrElse[Long]("silver")(0L)
      leader <- c.getOrElse[Option[String]]("leader")(None)
      step   <- c.getOrElse[Int]("step")(Taken)
      city   <- c.getOrElse[Int]("city")(0)
      chief  <- c.getOrElse[Option[String]]("leaderRace")(None)
      // Контракты, взятые до дублонов: обещано было одно серебро.
      dbl    <- c.getOrElse[Long]("doubloons")(0L)
    } yield VillageContract(quest, race, name, level, silver, leader, step, city, chief.flatMap(Race.withNameOption), dbl)
}

/** Числа деревни. Вынесены отдельно: их читают и сцены, и тесты. */
object VillageRates {

  /** Плата старосты — столько серебра за уровень задания (он же сложность), с
    * разбросом ±[[SilverSpreadPct]]%: бросается при встрече с деревней. Так
    * решил пользователь; 2026-11-13 — 50 вместо 75, зато с дублонами (см.
    * [[doubloons]]). Уже взятые контракты платят, как обещано. */
  val SilverPerLvl: Long    = 50L
  val SilverSpreadPct: Long = 20L

  /** И по дублону за каждые [[DoubloonEveryLvls]] уровней задания, с
    * округлением вниз: на 1–6 — ни одного, на 7–13 — один, на 14–20 — два.
    * Разброса нет. Так решил пользователь (2026-11-13). */
  val DoubloonEveryLvls: Int = 7

  def doubloons(level: Int): Long = (level.max(0) / DoubloonEveryLvls).toLong

  /** Торг: у старосты можно поторговаться один раз — с таким шансом он прибавит
    * к серебру столько процентов, иначе стоит на своём (так предложил Claude,
    * пользователь одобрил; у всех заданий деревни). Дублоны торг не трогает. */
  val HagglePct: Int       = 50
  val HaggleBonusPct: Long = 15L

  def haggled(silver: Long): Long = silver * (100L + HaggleBonusPct) / 100L

  /** Плата без разброса. */
  def silver(level: Int): Long = SilverPerLvl * level.max(1).toLong

  /** Плата с разбросом: `pct` — от 100 − разброс до 100 + разброс процентов. */
  def silver(level: Int, pct: Long): Long = (silver(level) * pct / 100L).max(1L)

  /** Уровень задания старосты — уровень лабиринта, где нашли деревню, со
    * сдвигом от и до (бросок при встрече), но не ниже первого и не глубже дна
    * лабиринта. Так решил пользователь. */
  val QuestLevelMin: Int = -1
  val QuestLevelMax: Int = 4

  /** Дно лабиринта: глубже 150-го пути нет (см. DungeonState). */
  val QuestLevelCap: Int = 150

  def questLevel(level: Int, shift: Int): Int = (level + shift).max(1).min(QuestLevelCap)

  /** Опыт за выполненный контракт — от тяжёлого задания с доски гильдии того
    * же уровня (см. [[BoardRates.exp]]), только уровень — задания, а не героя,
    * и не целиком, а [[ExpPct]] процентов: 2026-11-15 срезали на пятую часть
    * (решил пользователь). Доска платит по-прежнему. */
  val ExpPct: Long = 80L

  def exp(level: Int): Long = (BoardRates.exp(level.max(1).toLong) * ExpPct / 100L).max(1L)

  /** Налёт — как на деревню мурлоков: десять в строю, за ними двенадцать
    * посильнее и последним — именной; у входа — сторожевая башня со стрелком. */
  val RaidFront: Int                  = 10
  val RaidFrontRarities: List[Rarity] = List(Rarity.Uncommon, Rarity.Rare)
  val RaidQueue: Int                  = 12
  val RaidQueueRarities: List[Rarity] = List(Rarity.Rare, Rarity.Mythical)
  val RaidLastRarity: Rarity          = Rarity.Legendary
  val RaidTowers: Int                 = 1

  /** Логово разбойников: банда сборная — у каждого своя раса из тех, что
    * встречаются в лабиринте (без рас минибоссов; так решил пользователь), —
    * восемь попроще, шестеро покрепче и главарь, именной, тоже любой из этих
    * рас. Все пятнадцать стоят в строю с начала боя. */
  val BanditRaces: List[Race]           = Race.mortals.toList
  /** Раса главаря, если она не записана: так звали главарей до сборных банд. */
  val BanditLeaderDefaultRace: Race     = Race.Human
  val BanditFront: Int                  = 8
  val BanditFrontRarities: List[Rarity] = List(Rarity.Uncommon, Rarity.Rare)
  val BanditBack: Int                   = 6
  val BanditBackRarities: List[Rarity]  = List(Rarity.Rare, Rarity.Mythical)
  val BanditLeaderRarity: Rarity        = Rarity.Legendary

  /** Голова главаря: после боя её забирают с заставы, и староста доплачивает за
    * неё столько дублонов — в городе за голову обещана награда (так решил
    * пользователь). */
  val HeadDoubloons: Long = 5L

  /** Охотники за головами: с таким шансом ждут на выходе с заставы и требуют
    * голову главаря (шанс решил Claude). Их четверо покрепче и вожак-именной —
    * сброд всех кровей, уровня задания. */
  val HuntersPct: Int                  = 35
  val HuntersCount: Int                = 4
  val HuntersRarities: List[Rarity]    = List(Rarity.Rare, Rarity.Mythical)
  val HuntersLeaderRarity: Rarity      = Rarity.Legendary

  /** Сдавшиеся: с таким шансом после боя из избы выходят столько разбойников
    * разных рас и бросают оружие (шанс и число решил Claude). Перерезать — в
    * кошелях у каждого столько серебра за уровень задания; отпустить — один
    * показывает тайник с награбленным с обозов, от и до вещей (решил
    * пользователь: «как предложил Claude»). */
  val SurvivorsPct: Int           = 40
  val SurvivorsCount: Int         = 3
  val SurvivorPursePerLvl: Long   = 5L
  val CacheMin: Int               = 2
  val CacheMax: Int               = 3

  /** Пленник: с таким шансом на заставе находится связанный пленник любой из
    * смертных рас (шанс решил Claude) — его держали ради выкупа. Возьмёте с
    * собой — встанет в отряд на 48 часов (AllyRates.CaptiveMs, решил
    * пользователь), по силе — как моб его расы такой редкости уровня задания. */
  val CaptivePct: Int        = 25
  val CaptiveRarity: Rarity  = Rarity.Rare

  /** Гнездо сетеплётов: пауки — звери своих пород ([[Breed]]): восемь
    * молодых, шестеро взрослых и матка; с шансом [[RedSpiderChancePct]]% к ним
    * выползает ещё и красный — шестнадцатым, ждёт места в строю. С пауков —
    * паучья добыча, а не крысиная (см. LootGenerator.SpiderSpoils). */
  val SpiderRace: Race       = Race.Animal
  val SpiderYoung: Int       = 8
  val SpiderAdult: Int       = 6
  val RedSpiderChancePct: Int = 1

  /** Подготовка у входа в гнездо (см. [[NestPrep]]). Подожжённые огненной
    * флягой или рубиновой пылью пауки начинают бой с пламенем такой силы, в %;
    * паучий цвет уводит выводок, и в гнезде остаются матка и столько красных.
    * Так решил пользователь. Дым и сонный дурман режут атаку и энергию, как в
    * пещере (`CaveRates.DopeCutPct`). */
  val NestBurnPct: Int     = 4
  val LuredRedSpiders: Int = 2

  /** Матка рождает новорождённых: каждые столько раундов — по столько, уровнем
    * в саму матку, пока в строю есть место (очередь входит первой); сила
    * новорождённого — столько процентов от молодого. Ни опыта, ни добычи за
    * них нет. */
  val MotherBroodEvery: Int = 3
  val MotherBroodCount: Int = 2
  val NewbornPct: Long      = 60L

  /** Сила и ранг каждой породы — как у моба этой расы и этого ранга (так решил
    * пользователь): молодой — как орк первого ранга, взрослый — как каджит
    * третьего, матка — как каджит четвёртого, красный — как именной эльф (но
    * бьёт вблизи: он зверь, и повадок эльфа-стрелка у него нет). */
  val SpiderLike: Map[Breed, (Race, Rarity)] = Map(
    Breed.YoungSpider  -> (Race.Orc -> Rarity.Common),
    Breed.AdultSpider  -> (Race.Khajiit -> Rarity.Rare),
    Breed.SpiderMother -> (Race.Khajiit -> Rarity.Mythical),
    Breed.RedSpider    -> (Race.Elf -> Rarity.Legendary))

  /** Караван: столько переходов до города, каждый — от и до (как цветы на
    * поляне); в конце перехода — засада с шансом [[CaravanAmbushPct]]%, а если
    * засад так и не было, на последнем она будет наверняка. Так решил
    * пользователь. */
  val CaravanLegs: Int      = 4
  val CaravanLegMinMs: Long = 3L * 60L * 1000L
  val CaravanLegMaxMs: Long = 6L * 60L * 1000L
  val CaravanAmbushPct: Int = 50

  /** Засада: четверо в строю и трое за ними — уровня задания, как передний и
    * задний ряд банды на заставе, только меньше. */
  val CaravanAmbushFront: Int                  = 4
  val CaravanAmbushFrontRarities: List[Rarity] = List(Rarity.Uncommon, Rarity.Rare)
  val CaravanAmbushBack: Int                   = 3
  val CaravanAmbushBackRarities: List[Rarity]  = List(Rarity.Rare, Rarity.Mythical)

  /** Караванщики в строю героя: по силе — редкие той же расы, что деревня,
    * уровня задания (так решил пользователь); имена — `village.caravan.guards`. */
  val CaravanGuardRarity: Rarity = Rarity.Rare

  /** Повозок в обозе — столько, на первых местах ряда героя; прочность каждой —
    * как HP и броня редкого моба той же расы, что деревня, уровня задания. Плата
    * — за то, что дошло: каждая потерянная повозка — её доля долой (так решил
    * пользователь). */
  val CaravanWagons: Int          = 5
  val CaravanWagonRarity: Rarity  = Rarity.Rare

  /** Погоня за ворами артефакта: переходов по следу — от и до, бросок в начале
    * погони. Сколько их, игроку не говорим: что воры близко, видно по тексту
    * (так решил пользователь). Каждый переход — от и до, как у каравана. */
  val TrailLegsMin: Int    = 2
  val TrailLegsMax: Int    = 4
  val TrailLegMinMs: Long  = 3L * 60L * 1000L
  val TrailLegMaxMs: Long  = 6L * 60L * 1000L

  /** Стан воров: сброд всех кровей, как засада на караван, — четверо попроще и
    * трое покрепче, а с ними вор с артефактом: именной, любой из смертных рас
    * лабиринта. Вор стоит в строю первым, за ним — остальные; все восемь — с
    * начала боя. */
  val ThiefFront: Int                  = 4
  val ThiefFrontRarities: List[Rarity] = List(Rarity.Uncommon, Rarity.Rare)
  val ThiefBack: Int                   = 3
  val ThiefBackRarities: List[Rarity]  = List(Rarity.Rare, Rarity.Mythical)
  val ThiefRarity: Rarity              = Rarity.Legendary

  /** Вор с артефактом в бою не задерживается: не свалили его за столько
    * раундов — уходит живым, и погоня продолжается (так решил пользователь).
    * Загнанному второй раз бежать некуда. Число — в подсказке у стана
    * (`village.artifact.thiefHint`, «{rounds} раунда» — слово для 2–4). */
  val ThiefEscapeRounds: Int = 4

  /** Загнанный вор: с ним столько подручных, что ушли вместе с ним, — покрепче,
    * как задний ряд стана. */
  val CorneredHenchmen: Int = 2

  /** Покупатель в стане: с таким шансом воров застают при сделке — у костра с
    * ними Некромант, минибосс (так решил пользователь; шанс — Claude). Бросок —
    * когда герой выходит к стану. */
  val NecromancerPct: Int = 25

  /** Маг Сноходцев — тот, кто заказал кражу: с таким шансом выходит после боя и
    * предлагает за артефакт дублоны (шанс решил Claude). Если покупателем был
    * Некромант, не выходит: сделка и так сорвана. */
  val MageOfferPct: Int = 30

  /** Маг платит золотом: примерно по дублону за каждые столько серебра платы
    * деревни, с округлением, и её дублоны — сверху (так решил пользователь). */
  val MageSilverPerDoubloon: Long = 400L

  def mageDoubloons(silver: Long, doubloons: Long): Long =
    (doubloons.max(0L) + (silver.max(0L) + MageSilverPerDoubloon / 2L) / MageSilverPerDoubloon).max(1L)
}

/** Что ещё ждёт героя на заставе после боя с бандой (логово разбойников), пока
  * он оттуда не ушёл, — в `scene_data`. Контракт к этому времени уже на шаге
  * «сделано», голова главаря — в сумке.
  *
  * Ждёт оно по порядку: сдавшиеся, пленник, потом — на выходе — охотники.
  *
  * @param hunters   на выходе ждут охотники за головами — отдать им голову или
  *                  драться (см. [[VillageRates.HuntersPct]])
  * @param survivors из избы вышли сдаваться разбойники этих рас — перерезать
  *                  или отпустить (см. [[VillageRates.SurvivorsPct]]); пусто — их
  *                  нет или уже решено
  * @param captive   на заставе нашёлся связанный пленник этой расы — взять в
  *                  отряд или отпустить домой (см. [[VillageRates.CaptivePct]])
  */
final case class BanditAftermath(hunters: Boolean = false, survivors: List[Race] = Nil, captive: Option[Race] = None) {
  /** Ждать больше нечего — к старосте. */
  def done: Boolean = !hunters && survivors.isEmpty && captive.isEmpty
}

object BanditAftermath {

  /** Ключ-метка: в `scene_data` лежат и чужие структуры, и наша узнаётся по нему. */
  val Key: String = "banditsAfter"

  implicit val encoder: Encoder[BanditAftermath] = (a: BanditAftermath) => Json.obj(
    Key                -> Json.True,
    "banditsHunters"   -> a.hunters.asJson,
    "banditsSurvivors" -> a.survivors.map(_.entryName).asJson,
    "banditsCaptive"   -> a.captive.map(_.entryName).asJson)

  /** Запись, сделанная до сдавшихся или до пленника, — без них: их и нет. */
  implicit val decoder: Decoder[BanditAftermath] = (c: HCursor) =>
    for {
      _         <- c.get[Boolean](Key)
      hunters   <- c.getOrElse[Boolean]("banditsHunters")(false)
      survivors <- c.getOrElse[List[String]]("banditsSurvivors")(Nil)
      captive   <- c.getOrElse[Option[String]]("banditsCaptive")(None)
    } yield BanditAftermath(hunters, survivors.flatMap(Race.withNameOption), captive.flatMap(Race.withNameOption))
}

/** Предложение мага Сноходцев после боя в стане — в `scene_data`, пока герой не
  * ответил: продать артефакт за столько дублонов или отказать. Ключ свой. */
final case class ArtifactOffer(doubloons: Long)

object ArtifactOffer {
  implicit val encoder: Encoder[ArtifactOffer] = (o: ArtifactOffer) => Json.obj("artifactOffer" -> o.doubloons.asJson)
  implicit val decoder: Decoder[ArtifactOffer] = (c: HCursor) => c.get[Long]("artifactOffer").map(ArtifactOffer(_))
}

/** Погоня за ворами артефакта — в `scene_data`, пока герой идёт по следу. Ключи
  * свои (`trail…`): в `scene_data` лежат и чужие структуры, и чужое поле не
  * должно читаться как наше.
  *
  * @param legs    сколько переходов до стана воров — бросок в начале погони
  *                ([[VillageRates.TrailLegsMin]]…[[VillageRates.TrailLegsMax]]);
  *                игроку не называется
  * @param leg     какой переход идёт (с 1-го); больше `legs` — герой у стана
  * @param nextAt  когда кончится переход (мс); у стана — 0
  * @param escaped вор с артефактом ушёл из боя в стане — погоня за ним одним:
  *                переход по его следу, потом он, загнанный, с подручными
  * @param thief   имя вора — бросок в начале боя в стане: за ним и гонятся
  * @param thiefRace его раса
  * @param necromancer воров застали при сделке: в стане с ними Некромант — бросок,
  *                когда герой вышел к стану (см. [[VillageRates.NecromancerPct]]).
  *                Едет и в погоню за беглецом: мага Сноходцев после неё не будет
  */
final case class ArtifactTrail(legs: Int, leg: Int, nextAt: Long, escaped: Boolean = false,
                               thief: Option[String] = None, thiefRace: Option[Race] = None,
                               necromancer: Boolean = false) {
  /** Все переходы пройдены: впереди стан воров (или загнанный вор). */
  def atCamp: Boolean = leg > legs

  /** Идёт последний переход: за ним — стан. */
  def lastLeg: Boolean = leg >= legs
}

object ArtifactTrail {

  /** Начало погони: первый из `legs` переходов кончится в `nextAt`. */
  def start(legs: Int, nextAt: Long): ArtifactTrail = ArtifactTrail(legs, 1, nextAt)

  /** Вор ушёл из боя: один переход по его следу — он кончится в `nextAt`. Кто
    * вор и был ли в стане Некромант, погоня помнит. */
  def after(thief: Option[String], thiefRace: Option[Race], nextAt: Long, necromancer: Boolean = false): ArtifactTrail =
    ArtifactTrail(1, 1, nextAt, escaped = true, thief = thief, thiefRace = thiefRace, necromancer = necromancer)

  implicit val encoder: Encoder[ArtifactTrail] = (t: ArtifactTrail) => Json.obj(
    "trailLegs"        -> t.legs.asJson,
    "trailLeg"         -> t.leg.asJson,
    "trailNextAt"      -> t.nextAt.asJson,
    "trailEscaped"     -> t.escaped.asJson,
    "trailThief"       -> t.thief.asJson,
    "trailThiefRace"   -> t.thiefRace.map(_.entryName).asJson,
    "trailNecromancer" -> t.necromancer.asJson)

  /** Погоня, начатая до того, как вор научился уходить, — без этих полей:
    * читается как погоня за всеми ворами, вор бросается в начале боя,
    * Некроманта в стане нет. */
  implicit val decoder: Decoder[ArtifactTrail] = (c: HCursor) =>
    for {
      legs    <- c.get[Int]("trailLegs")
      leg     <- c.getOrElse[Int]("trailLeg")(1)
      nextAt  <- c.getOrElse[Long]("trailNextAt")(0L)
      escaped <- c.getOrElse[Boolean]("trailEscaped")(false)
      thief   <- c.getOrElse[Option[String]]("trailThief")(None)
      race    <- c.getOrElse[Option[String]]("trailThiefRace")(None)
      necro   <- c.getOrElse[Boolean]("trailNecromancer")(false)
    } yield ArtifactTrail(legs, leg, nextAt, escaped, thief, race.flatMap(Race.withNameOption), necro)
}

/** Кто нападает на караван в засаде (так решил пользователь): сборная банда
  * разбойников, гоблины гильдии Чернолом, ищейки Сноходцев. Текст засады —
  * `village.caravan.ambush.<key>`. У каждой свои повадки с повозками: бьёт ли
  * она их и как часто удар поджигает. В базе не лежит: бросается в конце
  * перехода.
  *
  * @param hitsWagons бьёт ли засада повозки, которые никто не прикрыл
  * @param ignitePct  шанс, что удар по повозке её подожжёт, в процентах
  * @param plunders   несёт ли засада награбленное с обозов: с павших оно падает
  *                   вдвое реже, чем на разбойничьей заставе (см.
  *                   `LootGenerator.AmbushSpoilsChancePct`)
  */
sealed abstract class CaravanAmbush(val key: String, val hitsWagons: Boolean, val ignitePct: Long, val plunders: Boolean)

object CaravanAmbush {
  /** Разбойникам нужен товар: бьют повозки, удар поджигает с шансом 25%. При
    * них — награбленное с прежних обозов (решил пользователь). */
  case object Bandits   extends CaravanAmbush("bandits", hitsWagons = true, ignitePct = 25L, plunders = true)
  /** Гоблины Чернолома ходят с факелами: бьют повозки и поджигают вдвое чаще. */
  case object Chernolom extends CaravanAmbush("chernolom", hitsWagons = true, ignitePct = 50L, plunders = false)
  /** Ищейкам Сноходцев нужны люди, а не товар: повозок они не трогают. */
  case object Hounds    extends CaravanAmbush("hounds", hitsWagons = false, ignitePct = 0L, plunders = false)

  val all: List[CaravanAmbush] = List(Bandits, Chernolom, Hounds)
}

/** Караван в пути — в `scene_data`, пока герой при нём. Ключи свои
  * (`caravan…`): в `scene_data` лежат и чужие структуры, и чужое поле не
  * должно читаться как наше.
  *
  * @param leg      какой переход идёт (с 1-го); больше [[VillageRates.CaravanLegs]] —
  *                 все пройдены, караван в городе
  * @param ambushes сколько засад уже было: ни одной — на последнем переходе будет
  * @param nextAt   когда кончится переход (мс); 0 — переход ещё не начат (вернулись
  *                 с добычи после засады — пора начинать следующий)
  * @param wagons   повозки обоза, какими они вышли из последней засады: побитые
  *                 едут дальше побитыми, огонь потушен. Ключ — тот же, под которым
  *                 их возвращает бой ([[Convoy.ReturnKey]]). `None` — путь начат
  *                 до повозок: засады без них, плата целиком
  */
final case class CaravanTrip(leg: Int, ambushes: Int, nextAt: Long, wagons: Option[List[Wagon]] = None) {
  def arrived: Boolean = leg > VillageRates.CaravanLegs

  /** Сколько повозок цело; путь без повозок — None. */
  def intact: Option[Int] = wagons.map(_.count(_.alive))

  /** Повозок не осталось ни одной — везти нечего. */
  def plundered: Boolean = wagons.exists(ws => ws.nonEmpty && !ws.exists(_.alive))
}

object CaravanTrip {

  /** У ворот: первый переход, засад не было, таймера нет. */
  val start: CaravanTrip = CaravanTrip(1, 0, 0L)

  implicit val encoder: Encoder[CaravanTrip] = (t: CaravanTrip) => Json.obj(
    "caravanLeg"      -> t.leg.asJson,
    "caravanAmbushes" -> t.ambushes.asJson,
    "caravanNextAt"   -> t.nextAt.asJson,
    Convoy.ReturnKey  -> t.wagons.asJson)

  implicit val decoder: Decoder[CaravanTrip] = (c: HCursor) =>
    for {
      leg      <- c.get[Int]("caravanLeg")
      ambushes <- c.getOrElse[Int]("caravanAmbushes")(0)
      nextAt   <- c.getOrElse[Long]("caravanNextAt")(0L)
      wagons   <- c.getOrElse[Option[List[Wagon]]](Convoy.ReturnKey)(None)
    } yield CaravanTrip(leg, ambushes, nextAt, wagons)
}
