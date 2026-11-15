package pangea.model.state

import enumeratum._
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax.EncoderOps

sealed trait StateType extends EnumEntry

object StateType extends Enum[StateType] with DoobieEnum[StateType] {

  val values = findValues

  implicit val encoder: Encoder[StateType] = (s: StateType) => s.entryName.asJson
  implicit val decoder: Decoder[StateType] = (c: HCursor) => c.as[String].map(StateType.withName)

  // Battle 36% · Village 1% · MonsterCave 1% · FlowerMeadow 2% · FoundItem 19% · Spring 18% ·
  // Girl 2% · SilverVein 9% · Caravan 1% · TreasureMobs 5% · TreasureDig 5% · ElementalLair 1%
  // (вес = число повторов в пуле, сумма = 100). Процент под логово элементаля забран у
  // боя, под девушку — у ручья, под поляну цветов — по одному у боя и у находки, под
  // пещеру с монстрами — снова у боя, под караван — у серебряной жилы, под деревню —
  // опять у боя. Деревня, пещера и поляна стоят сразу за боем, караван — сразу за
  // жилой, чтобы билеты событий дальше по списку не сдвигались. Проценты — для
  // полного пула: часть событий открывается только в глубине (см. EventMinLevel), и
  // выше пул меньше ста билетов.
  val BattleTickets: Int = 36

  val events: List[StateType] =
    List.fill(BattleTickets)(Battle) ++ List.fill(1)(Village) ++ List.fill(1)(MonsterCave) ++
      List.fill(2)(FlowerMeadow) ++ List.fill(19)(FoundItem) ++
      List.fill(18)(Spring) ++ List.fill(2)(Girl) ++
      List.fill(9)(SilverVein) ++ List.fill(1)(Caravan) ++
      List.fill(5)(TreasureMobs) ++ List.fill(5)(TreasureDig) ++
      List.fill(1)(ElementalLair)

  /** Пул событий с изменённым весом боя. `battleFactor` множит число «билетов»
   *  Battle в пуле: пассивки «Охотник» (×1.2) повышают долю боёв, «Скрытность»
   *  (×0.8) — понижает (см. [[pangea.model.hero.HeroPassives.battleEncounterFactor]]).
   *  Прочие события не трогаются — их абсолютный вес прежний, а относительная доля
   *  сдвигается за счёт изменения общего числа билетов. */
  def eventsWithBattleFactor(battleFactor: Double): List[StateType] = {
    val battleTickets = (BattleTickets * battleFactor).round.toInt.max(0)
    List.fill(battleTickets)(Battle) ++ events.filter(_ != Battle)
  }

  /** С какого уровня лабиринта событие попадает в пул: события открываются по
   *  мере спуска. Чего здесь нет, то встречается с первого уровня. У закрытого
   *  события в пуле нет ни одного билета, поэтому доли остальных выше. */
  val EventMinLevel: Map[StateType, Int] = Map(
    SilverVein    -> 2,  // первый уровень — только бои и находки
    TreasureDig   -> 2,  // в схроне может ждать Гнилой Джо
    Girl          -> 5,  // групповые бои
    TreasureMobs  -> 5,
    ElementalLair -> 7,  // минибосс
    MonsterCave   -> 10,
    Caravan       -> 10,
    Village       -> 15  // решил пользователь; задания старосты — по своим порогам (VillageQuest.minLevel)
  )

  /** Открыто ли событие на этом уровне лабиринта. */
  def eventOpen(event: StateType, dungeonLevel: Int): Boolean =
    dungeonLevel >= EventMinLevel.getOrElse(event, 1)

  /** Пул на этом уровне лабиринта: закрытые события выпадают целиком, а бой
   *  множится, как и прежде (см. [[eventsWithBattleFactor]]). */
  def eventsAt(dungeonLevel: Int, battleFactor: Double): List[StateType] =
    eventsWithBattleFactor(battleFactor).filter(eventOpen(_, dungeonLevel))

  case object GlobalMap     extends StateType
  case object HarborQuarter    extends StateType
  case object MarketSquare     extends StateType
  case object CityCenter       extends StateType // Центр города (Храм Азата)
  case object Home             extends StateType // «🏠 Дом»: Юнона Кинтслев продаёт дома, и сам дом героя
  case object RuneAltar        extends StateType // рунический алтарь в доме: узоры рунных камней счётом
  case object HerbWorkbench    extends StateType // верстак травника в доме: полки трав, котелок, полка отваров
  case object GemVault         extends StateType // хранилище камней на острове: по видам и качествам, улучшение 3 к 1
  case object TrophyWall       extends StateType // стена трофеев на острове: уникальные трофеи именных, прибавка к урону по расам
  case object TempleAzat       extends StateType // Храм Азата (Жрец / Зал / Уйти)
  case object Arena            extends StateType // арена: запись на бой с другим игроком
  case object HallAzat         extends StateType // Зал Азата (кубы, заряды)
  case object Cube             extends StateType // крафт в кубе Азата
  case object UnassumingBarrel extends StateType
  case object TradeHouse       extends StateType // Торговый дом возле Храма (банкир Рахадим)
  case object BankVault        extends StateType // хранилище героя в Торговом доме
  case object Auction          extends StateType // аукцион Торгового дома
  case object FetShop          extends StateType // Лавка Фета: сборные артефакты
  case object Backpack         extends StateType // рюкзак героя: сумка, ларец, живая сумка
  case object Casket           extends StateType // Ларец Азата
  case object LivingBag        extends StateType // Живая сумка
  case object Wardrobe         extends StateType // Миниатюрный шкаф
  case object Transfer         extends StateType // выбор вещи для передачи другому игроку
  case object Mail             extends StateType // почта Торгового дома: переданное, что не влезло в ячейку
  case object Trade            extends StateType // обмен между игроками в Торговом доме
  case object Registration extends StateType
  case object Dungeon      extends StateType
  case object HeroStats    extends StateType
  case object StreetMerchants extends StateType // уличные торговцы Торговой площади: Ришелье и Густаво
  case object FriendCode   extends StateType // «Код друга» из Персонажа: свой код и ввод чужого
  case object Battle       extends StateType
  case object Death        extends StateType

  case object FoundItem extends StateType
  case object Rest      extends StateType
  case object Spring    extends StateType
  case object Inventory  extends StateType
  case object Equipment  extends StateType
  case object Skills     extends StateType // список активных умений/пассивок героя
  case object Socketing  extends StateType // вставка камня-усилителя в гнездо снаряжения
  case object Loot       extends StateType
  case object RottenJoe  extends StateType // встреча с Гнилым Джо на раскопках
  case object ElementalLair extends StateType // логово элементаля: подход к минибоссу
  case object ElementalSearch extends StateType // осмотр логова после победы (афк-поиск камней)
  case object Merchant   extends StateType
  case object Gustavo         extends StateType
  case object GustavoHeal     extends StateType
  case object GustavoBoost    extends StateType
  case object GustavoSupplies extends StateType
  case object GustavoBelt     extends StateType
  case object Tavern     extends StateType
  case object QuestBoard extends StateType
  case object Innkeeper  extends StateType
  case object CardSeller extends StateType // «Подозрительный человек» — продавец карт
  case object GirlWaiting extends StateType // девушка с картой отца ждёт в таверне оплаты
  case object SilverVein     extends StateType
  case object Construction   extends StateType
  case object Guild          extends StateType
  case object TrophyExchange extends StateType
  case object TrainingHall   extends StateType
  case object MasterHorn     extends StateType
  case object MentorKazimir  extends StateType

  // События-«сокровища» подземелья.
  case object TreasureMobs     extends StateType // встреча мобов, выкопавших сокровище (интро с выбором)
  case object TreasureMobsFight extends StateType // эффект-нода: спавн очередного боя цепочки
  case object TreasureSchron   extends StateType // эффект-нода: выдача схрона после цепочки боёв
  case object TreasureDig      extends StateType // прикопанный схрон (раскопки по таймеру)
  case object Girl             extends StateType // девушка с криком «Помогите!» и трое вооружённых
  case object FlowerMeadow     extends StateType // поляна цветов: сбор трав по таймеру
  case object MonsterCave      extends StateType // пещера с монстрами: комнаты, кучки мобов, находки
  case object RaceRevenge      extends StateType // раса пришла за расплатой: именной и двое старших
  case object Caravan          extends StateType // караван: охрана, башни со стрелками и поклажа
  case object Knowledge        extends StateType // знания героя (меню персонажа)
  case object GustavoHerbs     extends StateType // Густаво: рассказ о травах и трактаты
  case object Squad            extends StateType // отряд героя (меню персонажа)
  case object Mercenaries      extends StateType // наёмники в таверне

  // Дорога к месту выездного задания с доски гильдии (~10 минут, без кнопок).
  case object QuestRoad extends StateType

  // Подворотня у таверны: засада на городских воров по объявлению с доски.
  // Эффект-нода: сразу в бой. Вторая — то, что с них сняли.
  case object Thieves       extends StateType
  case object ThievesSpoils extends StateType

  // Поход за сокровищем по карте клада.
  case object Outskirts    extends StateType // «За городом»: выбор карты и отправка в поход
  case object TreasureHunt extends StateType // сам поход по таймеру (~10 минут) → добыча

  // «Письмо Марисе».
  case object MarisaSearch extends StateType // поиски Долорес и Марисы в Портовом квартале
  case object MarisaHunt   extends StateType // поход к тайнику Кельвина по таймеру и встреча с коллектором

  // «Деревня Мурлоков».
  case object MurlocElder   extends StateType // старейшина выходит к герою после двухсотого убитого
  case object MurlocVillage extends StateType // по карте: налёт на деревню или помощь снаряжением

  // «Пропавшая дружина».
  case object SquadMine extends StateType // штольня под Серебряной жилой: вывести дружину или забрать груз

  // «Бродячий ларец».
  case object StrayCasket extends StateType // бродячий ларец в лабиринте: встреча, путь к хозяину, бой с ним

  // «Тот, кто уговаривает деревья».
  case object HedgeSong extends StateType // эльф поёт изгороди: встреча, три волны орков, ветвь

  // «Вскрытая бочка».
  case object BarrelThieves extends StateType // склад у причала: гоблины, что вскрыли бочку, и бой с ними
  case object BarrelReturn  extends StateType // украденное — обратно в бочку, награда; дальше — сама бочка

  // Основная линия, глава «Метка под меткой».
  case object StoryCloak extends StateType // первая зацепка: чёрный плащ у изгороди
  case object StoryCache extends StateType // тайник Ульфа Сороки у изгороди: меченые звери, шифровка Братства

  // Основная линия, главы «Порох у причала» и «Что горит на груди».
  case object StoryWarehouse extends StateType // дегтярный склад у причала: Смотритель, бой, его судьба
  case object StoryMeeting   extends StateType // встреча с Сойкой за дальним столом таверны

  // Основная линия, глава «Список».
  case object StoryBridge extends StateType // засада чистильщиков Братства у Мельничного моста
  case object StorySojka  extends StateType // Сойка за дальним столом: разговор о канцелярии, потом — о предательстве

  // Основная линия, глава «Лицо из прошлого» и Рубеж I «Чьё ты орудие».
  case object StoryArchive   extends StateType // архив канцелярии ночью: книга отмеченных и рука на её полях
  case object StoryBindery   extends StateType // переплётная у рыбного рынка: днём — лицо из прошлого, ночью — ячейка
  case object StoryGraveyard extends StateType // старое кладбище за городской стеной: встреча, бой, выбор пути

  // Акт II, глава «Наутро».
  case object StoryDen    extends StateType // «Три кости», игорный дом у порта: писарь Горна в залоге, откуп или бой
  case object StoryCellar extends StateType // подвал совета ночью: Брей за решёткой, Горн с искателями, чистильщики

  // Акт II, глава «Новенький».
  case object StorySquare extends StateType // главная площадь: новый отмеченный в фонтане, кто успел к нему первым, куда его

  // Побочная линия Барти.
  case object BartiMeet  extends StateType // Барти у изгороди: звери, «Ходовой товар», а потом — адрес
  case object BartiHouse extends StateType // дом Барти на Торговой площади: «Большое дело», товар для каравана
  case object BartiCaravan extends StateType // каджитский караван у Сухого колодца: сделка, засада стражи, дублоны
  case object BartiShop    extends StateType // лавка Барти на Торговой площади: «Крыша» — ночная стража берёт за охрану

  // Личные задания наёмников (см. MercQuests).
  case object JorgenLair extends StateType // логово огненных, где пал отряд Йоргена: двое элементалей, жетоны в пепле
  case object BramblHall extends StateType // Зал предков под серебряной жилой: двое каменных стражей, замок на гробницу
  case object PlukhHunt  extends StateType // ловчие Сноходцев на зов перевёрнутой печати: бой за багор Плюха
  case object FaelinRoots extends StateType // старые корни изгороди: вернуть срубленный лук Фаэлина, бой у корней

  // «Деревня» (событие лабиринта): деревня одной из смертных рас — разграбить
  // или помочь старосте (см. Villages).
  case object Village        extends StateType // деревня в лабиринте: приход, налёт или староста; плата за сделанное
  case object VillageBandits extends StateType // логово разбойников у деревни: бой с бандой и её главарём
  case object VillageSpiders extends StateType // гнездо сетеплётов у деревни: бой с выводком и маткой
  case object VillageCaravan extends StateType // караван деревни: переходы по тракту до города, засады в пути
  case object VillageTrail   extends StateType // погоня за ворами артефакта деревни: переходы по следу, бой в их стане

  // Смена расы: купон из лавки Фета — история расы до пробуждения в новом теле.
  case object RaceQuest extends StateType

  // Смена имени: купон из лавки Фета — новое имя текстом.
  case object NameChange extends StateType

  /** «Персонаж» и его экраны: открывают их откуда угодно, а откуда — помнит
    * `return_state` (см. `CharacterMenu`). Пока они открыты, герой на самом
    * деле там, откуда их открыл. */
  val characterMenu: Set[StateType] =
    Set(HeroStats, Backpack, Inventory, Socketing, Equipment, Skills, Knowledge, Squad, FriendCode)

  /** Городские сцены — где герой «в городе»: отсюда передают вещи и серебро,
    * сюда выручку аукциона приносят в руки, только отсюда открывают сюжетную
    * карту (её смотрят по `returnState` меню персонажа). Город — любой его
    * экран; не входят только меню «Персонаж» и сцены заданий (воры, дорога к
    * заданию, поиски Марисы и прочие). */
  val cityStates: Set[StateType] = Set(
    GlobalMap, HarborQuarter, MarketSquare, StreetMerchants, CityCenter, TempleAzat, HallAzat, Cube, UnassumingBarrel,
    TradeHouse, BankVault, Auction, FetShop, Mail, Transfer, Trade,
    Arena, Outskirts,
    Merchant, Gustavo, GustavoHeal, GustavoBoost, GustavoSupplies, GustavoBelt, GustavoHerbs,
    Tavern, Mercenaries, QuestBoard, Innkeeper, CardSeller, GirlWaiting, Construction, Guild, TrophyExchange, TrainingHall,
    MasterHorn, MentorKazimir, Home, RuneAltar, HerbWorkbench, GemVault, TrophyWall)
}
