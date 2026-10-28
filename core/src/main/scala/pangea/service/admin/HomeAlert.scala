package pangea.service.admin

import pangea.model.state.StateType
import pangea.model.user.VkId

/** Пересылка /home «Админ 1»: группа пересылает в личку админу само сообщение
  * игрока, а подпись к нему говорит, кто это и из какой сцены он выбирался. По
  * сцене проще найти, где игрок застрял. */
object HomeAlert {

  /** Подпись к пересланному /home. Ссылка на игрока — на случай, если ВК не
    * примет саму пересылку и уйдёт одна подпись. */
  def caption(player: VkId, from: StateType): String =
    s"🆘 /home от vk.com/id${player.value}\nСцена: ${sceneName(from)}"

  /** Сцена по-русски; новая, ещё не названная здесь, — именем из кода. */
  def sceneName(state: StateType): String = SceneNames.getOrElse(state, state.entryName)

  private val SceneNames: Map[StateType, String] = {
    import StateType._
    Map(
      // ── Город ──
      GlobalMap        -> "главный экран города",
      HarborQuarter    -> "Портовый квартал",
      MarketSquare     -> "Торговая площадь",
      CityCenter       -> "Центр города",
      UnassumingBarrel -> "неприметная бочка",
      Tavern           -> "таверна",
      Innkeeper        -> "трактирщик",
      CardSeller       -> "подозрительный человек",
      Mercenaries      -> "наёмники в таверне",
      Guild            -> "Гильдия искателей",
      QuestBoard       -> "доска заданий",
      TrophyExchange   -> "приём трофеев",
      TrainingHall     -> "тренировочный зал",
      MasterHorn       -> "Мастер Горн",
      MentorKazimir    -> "наставник Казимир",
      StreetMerchants  -> "уличные торговцы",
      Merchant         -> "Ришелье",
      Gustavo          -> "Густаво",
      GustavoHeal      -> "Густаво: лечение травм",
      GustavoBoost     -> "Густаво: зелья на характеристики",
      GustavoSupplies  -> "Густаво: расходники",
      GustavoBelt      -> "Густаво: пояс",
      GustavoHerbs     -> "Густаво: травы и трактаты",
      Construction     -> "стройка",
      TradeHouse       -> "Торговый дом",
      BankVault        -> "банковская ячейка",
      Auction          -> "аукцион",
      FetShop          -> "лавка Фета",
      Mail             -> "почта",
      Transfer         -> "передача вещей",
      TempleAzat       -> "Храм Азата",
      HallAzat         -> "Зал Азата",
      Cube             -> "куб Азата",
      Arena            -> "арена",
      Home             -> "дом и Юнона",
      RuneAltar        -> "рунический алтарь",
      HerbWorkbench    -> "верстак травника",
      GemVault         -> "хранилище камней",
      TrophyWall       -> "стена трофеев",
      Thieves          -> "разбойники в городе",
      ThievesSpoils    -> "кошельки разбойников",
      MarisaSearch     -> "поиски Марисы",
      Outskirts        -> "за городом",
      TreasureHunt     -> "поход за сокровищем",
      MarisaHunt       -> "поход к тайнику Кельвина",
      // ── Персонаж ──
      HeroStats        -> "меню «Персонаж»",
      Inventory        -> "сумка",
      Equipment        -> "снаряжение",
      Skills           -> "умения",
      Socketing        -> "вставка камня",
      Knowledge        -> "знания",
      Squad            -> "отряд",
      FriendCode       -> "код друга",
      Backpack         -> "рюкзак",
      Casket           -> "Ларец Азата",
      LivingBag        -> "Живая сумка",
      Wardrobe         -> "Миниатюрный шкаф",
      // ── Лабиринт ──
      Registration     -> "пролог",
      Dungeon          -> "лабиринт",
      Battle           -> "бой",
      Death            -> "смерть",
      Loot             -> "добыча",
      Rest             -> "отдых",
      Spring           -> "живительный ручеёк",
      FoundItem        -> "находка",
      SilverVein       -> "серебряная жила",
      FlowerMeadow     -> "цветочная поляна",
      Girl             -> "девушка в коридорах",
      RottenJoe        -> "Гнилой Джо",
      ElementalLair    -> "логово элементаля",
      ElementalSearch  -> "осмотр логова элементаля",
      TreasureMobs     -> "мобы у сокровища",
      TreasureMobsFight -> "бои за сокровище",
      TreasureSchron   -> "схрон после боёв",
      TreasureDig      -> "раскопки схрона",
      MonsterCave      -> "пещера с монстрами",
      Caravan          -> "караван",
      RaceRevenge      -> "месть расы",
      QuestRoad        -> "дорога по заданию",
      MurlocElder      -> "старейшина мурлоков",
      MurlocVillage    -> "деревня мурлоков",
      SquadMine        -> "штольня под Серебряной жилой",
      RaceQuest        -> "квест смены расы",
      NameChange       -> "смена имени"
    )
  }
}
