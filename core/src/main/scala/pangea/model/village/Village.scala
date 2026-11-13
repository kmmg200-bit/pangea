package pangea.model.village

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import pangea.model.monster.{Breed, Race, Rarity}
import pangea.model.quest.BoardRates

/** Беда деревни — то, о чём просит староста (см. [[VillageContract]]). Задания
  * открываются по мере того, как герой уходит дальше в лабиринт ([[minLevel]]):
  * логово разбойников — с того же уровня, что и сами деревни (15-го), гнездо
  * сетеплётов — с 20-го; следующим будет караван (25-й), дальше порядок решит
  * пользователь. `entryName` лежит в базе — в `scene_data` и в
  * `npc_quests`: не переименовывать и не удалять.
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
  step:   String         = VillageScene.Arrival
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
    "villageStep"   -> s.step.asJson)

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
    } yield VillageScene(race, name, quest, level, qLevel, silver, leader, chief.flatMap(Race.withNameOption), city, land, step)
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
  leaderRace: Option[Race] = None
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
      scene.leaderRace)

  implicit val encoder: Encoder[VillageContract] = (v: VillageContract) => Json.obj(
    "quest"  -> v.quest.asJson,
    "race"   -> v.race.entryName.asJson,
    "name"   -> v.name.asJson,
    "level"  -> v.level.asJson,
    "silver" -> v.silver.asJson,
    "leader" -> v.leader.asJson,
    "step"   -> v.step.asJson,
    "city"   -> v.city.asJson,
    "leaderRace" -> v.leaderRace.map(_.entryName).asJson)

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
    } yield VillageContract(quest, race, name, level, silver, leader, step, city, chief.flatMap(Race.withNameOption))
}

/** Числа деревни. Вынесены отдельно: их читают и сцены, и тесты. */
object VillageRates {

  /** Плата старосты — столько серебра за уровень задания (он же сложность), с
    * разбросом ±[[SilverSpreadPct]]%: бросается при встрече с деревней. Так
    * решил пользователь. */
  val SilverPerLvl: Long    = 75L
  val SilverSpreadPct: Long = 20L

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

  /** Опыт за выполненный контракт — как за тяжёлое задание с доски гильдии
    * того же уровня (см. [[BoardRates.exp]]), только уровень — задания, а не
    * героя. */
  def exp(level: Int): Long = BoardRates.exp(level.max(1).toLong)

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

  /** Гнездо сетеплётов: пауки — звери своих пород ([[Breed]]): восемь
    * молодых, шестеро взрослых и матка; с шансом [[RedSpiderChancePct]]% к ним
    * выползает ещё и красный — шестнадцатым, ждёт места в строю. С пауков —
    * паучья добыча, а не крысиная (см. LootGenerator.SpiderSpoils). */
  val SpiderRace: Race       = Race.Animal
  val SpiderYoung: Int       = 8
  val SpiderAdult: Int       = 6
  val RedSpiderChancePct: Int = 1

  /** Сила и ранг каждой породы — как у моба этой расы и этого ранга (так решил
    * пользователь): молодой — как орк первого ранга, взрослый — как каджит
    * третьего, матка — как каджит четвёртого, красный — как именной эльф (но
    * бьёт вблизи: он зверь, и повадок эльфа-стрелка у него нет). */
  val SpiderLike: Map[Breed, (Race, Rarity)] = Map(
    Breed.YoungSpider  -> (Race.Orc -> Rarity.Common),
    Breed.AdultSpider  -> (Race.Khajiit -> Rarity.Rare),
    Breed.SpiderMother -> (Race.Khajiit -> Rarity.Mythical),
    Breed.RedSpider    -> (Race.Elf -> Rarity.Legendary))
}
