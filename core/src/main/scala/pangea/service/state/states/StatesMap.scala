package pangea.service.state.states

import pangea.dao.arena.ArenaDao
import pangea.dao.hero.HeroDao
import pangea.engine.{GraphValidator, Journal, Players, SceneContent}
import pangea.model.state.StateType
import pangea.model.state.StateType.{
  Battle,
  CardSeller,
  CityCenter,
  Construction,
  Cube,
  Death,
  Dungeon,
  ElementalLair,
  ElementalSearch,
  Equipment,
  FoundItem,
  GlobalMap,
  Home,
  RuneAltar,
  HerbWorkbench,
  GemVault,
  TrophyWall,
  SilverVein,
  Guild,
  Gustavo,
  GustavoBoost,
  GustavoBelt,
  GustavoHeal,
  GustavoSupplies,
  HallAzat,
  HarborQuarter,
  StreetMerchants,
  HeroStats,
  FriendCode,
  Innkeeper,
  Inventory,
  Loot,
  MarketSquare,
  MasterHorn,
  MentorKazimir,
  Merchant,
  Outskirts,
  QuestBoard,
  QuestRoad,
  Thieves,
  ThievesSpoils,
  Registration,
  Rest,
  RottenJoe,
  Skills,
  Socketing,
  Tavern,
  TempleAzat,
  TrainingHall,
  TreasureDig,
  Girl,
  GirlWaiting,
  FlowerMeadow,
  Arena,
  MonsterCave,
  RaceRevenge,
  Caravan,
  Knowledge,
  Squad,
  Mercenaries,
  GustavoHerbs,
  MarisaSearch,
  MarisaHunt,
  MurlocElder,
  MurlocVillage,
  SquadMine,
  StrayCasket,
  HedgeSong,
  BarrelThieves,
  BarrelReturn,
  StoryCloak,
  StoryCache,
  StoryWarehouse,
  StoryMeeting,
  StoryBridge,
  StorySojka,
  StoryArchive,
  StoryBindery,
  StoryGraveyard,
  StoryDen,
  StoryCellar,
  StorySquare,
  StoryClearing,
  StoryFurnace,
  StoryCollector,
  StoryFeed,
  BartiMeet,
  BartiHouse,
  BartiCaravan,
  BartiShop,
  JorgenLair,
  BramblHall,
  PlukhHunt,
  FaelinRoots,
  Village,
  VillageBandits,
  VillageSpiders,
  VillageCaravan,
  VillageTrail,
  RaceQuest,
  NameChange,
  TreasureHunt,
  TreasureMobs,
  TreasureMobsFight,
  TreasureSchron,
  TrophyExchange,
  UnassumingBarrel,
  TradeHouse,
  BankVault,
  Auction,
  FetShop,
  Backpack,
  Casket,
  LivingBag,
  Wardrobe,
  Transfer,
  Trade,
  Mail
}
import pangea.dao.quest.QuestOutcomeDao
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.auction.AuctionRepository
import pangea.repository.bank.BankRepository
import pangea.repository.barrel.BarrelRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.repository.user.UserRepository
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.Background
import pangea.service.payout.Payouts
import pangea.service.donation.Donations
import pangea.service.schedule.Scheduler
import pangea.service.state.State
import pangea.model.artifact.ArtifactKind
import pangea.service.state.states.arena.ArenaState
import pangea.service.state.states.artifact.{ArtifactState, BackpackState, FetShopState}
import pangea.service.state.states.bank.{AuctionState, BankVaultState, TradeHouseState}
import pangea.service.state.states.parcel.{MailState, TransferState}
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.events.{ElementalLairState, ElementalSearchState, FlowerMeadowState, GirlState, RaceRevengeState, RottenJoeState, SilverVeinState}
import pangea.service.state.states.events.caravan.CaravanState
import pangea.service.state.states.events.cave.MonsterCaveState
import pangea.service.state.states.temple.{CubeState, HallAzatState, TempleAzatState}
import pangea.service.state.states.events.treasure.{
  TreasureDigState,
  TreasureHuntState,
  TreasureMobsFightState,
  TreasureMobsState,
  TreasureSchronState
}
import pangea.service.state.states.guild.{
  GuildState,
  MasterHornState,
  MentorKazimirState,
  QuestBoardState,
  TrainingHallState,
  TrophyExchangeState
}
import pangea.service.state.states.events.item.FoundItemState
import pangea.service.state.states.marisa.{MarisaHuntState, MarisaSearchState}
import pangea.service.state.states.road.QuestRoadState
import pangea.service.state.states.events.thieves.{ThievesSpoilsState, ThievesState}
import pangea.service.state.states.murloc.{MurlocElderState, MurlocVillageState}
import pangea.service.state.states.lostsquad.SquadMineState
import pangea.service.state.states.straycasket.StrayCasketState
import pangea.service.state.states.hedgesong.HedgeSongState
import pangea.service.state.states.barrelthieves.{BarrelReturnState, BarrelThievesState}
import pangea.service.state.states.barti.{BartiCaravanState, BartiHouseState, BartiMeetState, BartiShopState}
import pangea.service.state.states.merc.{BramblHallState, FaelinRootsState, JorgenLairState, PlukhHuntState}
import pangea.service.state.states.village.{VillageCaravanState, VillageLairState, VillageState, VillageTrailState}
import pangea.service.state.VillageLair
import pangea.service.state.states.story.{StoryArchiveState, StoryBinderyState, StoryBridgeState, StoryCacheState, StoryCellarState,
  StoryClearingState, StoryCloakState, StoryCollectorState, StoryDenState, StoryFeedState, StoryFurnaceState, StoryGraveyardState, StoryMeetingState,
  StorySojkaState, StorySquareState, StoryWarehouseState}
import pangea.service.state.states.race.RaceQuestState
import pangea.service.state.states.name.NameChangeState
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.states.gustavo.{
  GustavoBoostState,
  GustavoBeltState,
  GustavoHealState,
  GustavoHerbsState,
  GustavoState,
  GustavoSuppliesState
}
import pangea.service.state.states.registration.RegistrationState
import pangea.service.state.states.home.{GemVaultState, HerbWorkbenchState, HomeState, RuneAltarState, TrophyWallState}
import pangea.service.state.states.hero.{FriendCodeState, HeroStatsState, KnowledgeState, SkillsState, SquadState}
import pangea.service.referral.Referrals
import pangea.service.state.states.tavern.{
  CardSellerState,
  GirlWaitingState,
  InnkeeperState,
  MercenariesState,
  TavernState
}
import zio.{ZIO, ZLayer}

case class StatesMap(states: Map[StateType, State])

object StatesMap {
  val live: ZLayer[
    Players
      with HeroDao
      with ArenaDao
      with InventoryRepository
      with BarrelRepository
      with BankRepository
      with AuctionRepository
      with ArtifactRepository
      with Payouts
      with Parcels
      with Transfers
      with pangea.service.trade.Trades
      with ItemRepository
      with UserRepository
      with Donations
      with Journal
      with SceneContent
      with Scheduler
      with Referrals
      with Background
      with QuestOutcomeDao,
    Throwable,
    StatesMap
  ] =
    ZLayer.fromZIO(
      for {
        players       <- ZIO.service[Players]
        heroDao       <- ZIO.service[HeroDao]
        inventoryRepo <- ZIO.service[InventoryRepository]
        barrelRepo    <- ZIO.service[BarrelRepository]
        bankRepo      <- ZIO.service[BankRepository]
        auctionRepo   <- ZIO.service[AuctionRepository]
        artifactRepo  <- ZIO.service[ArtifactRepository]
        payouts       <- ZIO.service[Payouts]
        parcels       <- ZIO.service[Parcels]
        transfers     <- ZIO.service[Transfers]
        trades        <- ZIO.service[pangea.service.trade.Trades]
        itemRepo      <- ZIO.service[ItemRepository]
        userRepo      <- ZIO.service[UserRepository]
        donations     <- ZIO.service[Donations]
        journal       <- ZIO.service[Journal]
        content       <- ZIO.service[SceneContent]
        scheduler     <- ZIO.service[Scheduler]
        arenaDao      <- ZIO.service[ArenaDao]
        referrals     <- ZIO.service[Referrals]
        background    <- ZIO.service[Background]
        outcomeDao    <- ZIO.service[QuestOutcomeDao]
        // Кошель героя: своё серебро, а следом — ячейка в Торговом доме.
        bank           = Some(bankRepo)
        // Ларец Азата и Живая сумка ловят добычу до того, как она попадёт в сумку.
        artifacts      = Some(artifactRepo)
        // Чем закончены задания с развилкой — для подсчёта «как и вы».
        outcomes       = Some(outcomeDao)
        states = Map[StateType, State](
          GlobalMap     -> GlobalMapState(heroDao, content),
          HarborQuarter -> HarborQuarterState(content),
          MarketSquare  -> MarketSquareState(content, Some(heroDao)),
          StreetMerchants -> StreetMerchantsState(content),
          UnassumingBarrel -> UnassumingBarrelState(
            heroDao,
            inventoryRepo,
            barrelRepo,
            content,
            // Улика воров («Вскрытая бочка») — новая вещь: без хранилища вещей её не создать.
            Some(itemRepo)
          ),
          Registration -> RegistrationState(
            players,
            heroDao,
            inventoryRepo,
            itemRepo,
            journal,
            content
          ),
          Dungeon   -> DungeonState(heroDao, inventoryRepo, scheduler, content),
          Arena     -> ArenaState(heroDao, arenaDao, content, scheduler, Some(players)),
          HeroStats -> HeroStatsState(heroDao, content, artifacts),
          FriendCode -> FriendCodeState(heroDao, referrals, content),
          FoundItem -> FoundItemState(
            heroDao,
            inventoryRepo,
            itemRepo,
            journal,
            content,
            artifacts
          ),
          Battle -> BattleState(heroDao, inventoryRepo, itemRepo, content, scheduler, Some(arenaDao), bank),
          Death  -> DeathState(heroDao, inventoryRepo, content, artifacts, outcomes, Some(itemRepo), Some(parcels)),
          Rest   -> RestState(heroDao, scheduler, content),
          Inventory -> InventoryState(
            heroDao,
            inventoryRepo,
            itemRepo,
            content,
            artifacts
          ),
          Equipment -> EquipmentState(heroDao, inventoryRepo, content, itemRepo),
          Skills    -> SkillsState(heroDao, content),
          Knowledge -> KnowledgeState(heroDao, content),
          Squad -> SquadState(heroDao, content),
          ElementalLair -> ElementalLairState(heroDao, content),
          RottenJoe     -> RottenJoeState(heroDao, content),
          ElementalSearch -> ElementalSearchState(heroDao, inventoryRepo, itemRepo, scheduler, content, artifacts),
          Socketing -> SocketingState(heroDao, inventoryRepo, content, itemRepo),
          CityCenter -> CityCenterState(content),
          TradeHouse -> TradeHouseState(heroDao, userRepo, bankRepo, parcels, inventoryRepo, donations, content, artifacts,
            Some(itemRepo)),
          Transfer   -> TransferState(heroDao, transfers, content, background),
          Trade      -> pangea.service.state.states.bank.TradeState(heroDao, trades, transfers, userRepo, players, content),
          Mail       -> MailState(heroDao, inventoryRepo, parcels, content),
          BankVault  -> BankVaultState(heroDao, inventoryRepo, bankRepo, content, artifacts),
          Auction    -> AuctionState(heroDao, inventoryRepo, auctionRepo, userRepo, bankRepo, payouts, players, content),
          FetShop    -> FetShopState(heroDao, artifactRepo, inventoryRepo, content, Some(itemRepo), Some(journal)),
          Backpack   -> BackpackState(heroDao, inventoryRepo, artifactRepo, content),
          Casket     -> ArtifactState(ArtifactKind.Casket, heroDao, inventoryRepo, itemRepo, artifactRepo, content),
          LivingBag  -> ArtifactState(ArtifactKind.LivingBag, heroDao, inventoryRepo, itemRepo, artifactRepo, content),
          Wardrobe   -> ArtifactState(ArtifactKind.Wardrobe, heroDao, inventoryRepo, itemRepo, artifactRepo, content),
          TempleAzat -> TempleAzatState(heroDao, inventoryRepo, itemRepo, content, artifacts, bank),
          HallAzat   -> HallAzatState(heroDao, content, bank, artifacts),
          Cube       -> CubeState(heroDao, inventoryRepo, itemRepo, content, bank),
          Loot -> LootState(heroDao, inventoryRepo, itemRepo, journal, content, artifacts, Some(parcels)),
          Merchant -> MerchantState(heroDao, inventoryRepo, itemRepo, content, bank),
          Gustavo  -> GustavoState(heroDao, inventoryRepo, content, Some(itemRepo), artifacts),
          GustavoHerbs    -> GustavoHerbsState(heroDao, inventoryRepo, itemRepo, content, bank),
          GustavoHeal     -> GustavoHealState(heroDao, content, bank),
          GustavoBoost    -> GustavoBoostState(heroDao, content, bank),
          GustavoSupplies -> GustavoSuppliesState(heroDao, content),
          GustavoBelt     -> GustavoBeltState(heroDao, content, bank),
          Tavern          -> TavernState(heroDao, scheduler, content, bank),
          Home            -> HomeState(heroDao, scheduler, content, inventoryRepo, itemRepo, bank, artifacts, outcomes, Some(parcels)),
          RuneAltar       -> RuneAltarState(heroDao, inventoryRepo, itemRepo, content),
          HerbWorkbench   -> HerbWorkbenchState(heroDao, inventoryRepo, itemRepo, content, artifacts),
          GemVault        -> GemVaultState(heroDao, inventoryRepo, itemRepo, content, artifacts),
          TrophyWall      -> TrophyWallState(heroDao, inventoryRepo, content),
          CardSeller -> CardSellerState(
            heroDao,
            inventoryRepo,
            itemRepo,
            content
          ),
          GirlWaiting -> GirlWaitingState(heroDao, inventoryRepo, itemRepo, content, bank),
          QuestBoard        -> QuestBoardState(heroDao, inventoryRepo, artifactRepo, scheduler, content),
          QuestRoad         -> QuestRoadState(heroDao, scheduler, content),
          Thieves           -> ThievesState(heroDao, content),
          ThievesSpoils     -> ThievesSpoilsState(heroDao, content),
          Innkeeper         -> InnkeeperState(heroDao, inventoryRepo, content, bank),
          Mercenaries       -> MercenariesState(heroDao, inventoryRepo, content, bank, Some(itemRepo)),
          SilverVein        -> SilverVeinState(heroDao, scheduler, content),
          TreasureMobs      -> TreasureMobsState(heroDao, content),
          TreasureMobsFight -> TreasureMobsFightState(heroDao, content),
          TreasureSchron    -> TreasureSchronState(heroDao, content),
          TreasureDig       -> TreasureDigState(heroDao, scheduler, content),
          Girl              -> GirlState(heroDao, inventoryRepo, itemRepo, barrelRepo, scheduler, content, bank),
          FlowerMeadow      -> FlowerMeadowState(heroDao, inventoryRepo, itemRepo, scheduler, content, artifacts),
          MonsterCave       -> MonsterCaveState(heroDao, inventoryRepo, itemRepo, scheduler, content, artifacts),
          RaceRevenge       -> RaceRevengeState(heroDao, content),
          Caravan           -> CaravanState(heroDao, inventoryRepo, itemRepo, content),
          MarisaSearch      -> MarisaSearchState(heroDao, inventoryRepo, itemRepo, content),
          MarisaHunt        -> MarisaHuntState(heroDao, inventoryRepo, scheduler, content, bank, outcomes),
          MurlocElder       -> MurlocElderState(heroDao, inventoryRepo, itemRepo, content),
          MurlocVillage     -> MurlocVillageState(heroDao, inventoryRepo, itemRepo, content, outcomes),
          SquadMine         -> SquadMineState(heroDao, inventoryRepo, itemRepo, content, outcomes),
          StrayCasket       -> StrayCasketState(heroDao, inventoryRepo, itemRepo, content, artifacts),
          HedgeSong         -> HedgeSongState(heroDao, inventoryRepo, itemRepo, content),
          BarrelThieves     -> BarrelThievesState(heroDao, content),
          BarrelReturn      -> BarrelReturnState(heroDao, inventoryRepo, barrelRepo, content),
          StoryCloak        -> StoryCloakState(heroDao, inventoryRepo, itemRepo, content),
          StoryCache        -> StoryCacheState(heroDao, inventoryRepo, itemRepo, content),
          StoryWarehouse    -> StoryWarehouseState(heroDao, inventoryRepo, itemRepo, content, outcomes),
          StoryMeeting      -> StoryMeetingState(heroDao, inventoryRepo, itemRepo, content),
          StoryBridge       -> StoryBridgeState(heroDao, content),
          StorySojka        -> StorySojkaState(heroDao, inventoryRepo, itemRepo, content, outcomes),
          StoryArchive      -> StoryArchiveState(heroDao, content),
          StoryBindery      -> StoryBinderyState(heroDao, inventoryRepo, itemRepo, content),
          StoryGraveyard    -> StoryGraveyardState(heroDao, inventoryRepo, itemRepo, content, outcomes, Some(parcels)),
          StoryDen          -> StoryDenState(heroDao, inventoryRepo, itemRepo, content, bank),
          StoryCellar       -> StoryCellarState(heroDao, content),
          StorySquare       -> StorySquareState(heroDao, inventoryRepo, itemRepo, content, outcomes),
          StoryClearing     -> StoryClearingState(heroDao, inventoryRepo, itemRepo, content, bank),
          StoryFurnace      -> StoryFurnaceState(heroDao, inventoryRepo, itemRepo, content, outcomes),
          StoryCollector    -> StoryCollectorState(heroDao, inventoryRepo, content, outcomes),
          StoryFeed         -> StoryFeedState(heroDao, inventoryRepo, itemRepo, content, outcomes, Some(parcels)),
          BartiMeet         -> BartiMeetState(heroDao, inventoryRepo, itemRepo, content),
          BartiHouse        -> BartiHouseState(heroDao, inventoryRepo, content, Some(itemRepo), Some(parcels), bank, outcomes,
                                 Some(journal), Some(artifactRepo)),
          BartiCaravan      -> BartiCaravanState(heroDao, inventoryRepo, content, outcomes, Some(itemRepo), Some(parcels)),
          BartiShop         -> BartiShopState(heroDao, inventoryRepo, itemRepo, content, outcomes),
          JorgenLair        -> JorgenLairState(heroDao, inventoryRepo, itemRepo, content),
          BramblHall        -> BramblHallState(heroDao, inventoryRepo, content),
          PlukhHunt         -> PlukhHuntState(heroDao, inventoryRepo, content),
          FaelinRoots       -> FaelinRootsState(heroDao, inventoryRepo, content),
          Village           -> VillageState(heroDao, inventoryRepo, itemRepo, content),
          VillageBandits    -> VillageLairState(heroDao, inventoryRepo, itemRepo, content, VillageLair.Bandits),
          VillageSpiders    -> VillageLairState(heroDao, inventoryRepo, itemRepo, content, VillageLair.Spiders),
          VillageCaravan    -> VillageCaravanState(heroDao, inventoryRepo, scheduler, content),
          VillageTrail      -> VillageTrailState(heroDao, inventoryRepo, scheduler, content),
          RaceQuest         -> RaceQuestState(heroDao, inventoryRepo, content),
          NameChange        -> NameChangeState(heroDao, inventoryRepo, content),
          Outskirts -> OutskirtsState(
            heroDao,
            inventoryRepo,
            scheduler,
            content
          ),
          TreasureHunt -> TreasureHuntState(heroDao, scheduler, content, inventoryRepo, itemRepo),
          Construction -> ConstructionState(heroDao, scheduler, content, Some(inventoryRepo), outcomes, Some(itemRepo)),
          Guild        -> GuildState(heroDao, content),
          TrophyExchange -> TrophyExchangeState(
            heroDao,
            inventoryRepo,
            content
          ),
          TrainingHall  -> TrainingHallState(content),
          MasterHorn    -> MasterHornState(heroDao, inventoryRepo, content, bank),
          MentorKazimir -> MentorKazimirState(heroDao, inventoryRepo, content)
        )
        _ <- GraphValidator.validate(states)
      } yield new StatesMap(states)
    )
}
