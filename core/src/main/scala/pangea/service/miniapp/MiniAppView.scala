package pangea.service.miniapp

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import pangea.engine.SceneContent
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.artifact.{Artifact, HeroArtifacts}
import pangea.model.bank.BankVault
import pangea.model.barrel.Barrel
import pangea.model.hero.{Achievement, AzatState, CubeStatus, Hero, HeroCard, Knowledge, LoreData}
import pangea.model.home.{HomeData, HomeRates, House}
import pangea.model.inventory.Inventory
import pangea.model.item.{BeltSlot, BrewEffect, Bundling, Chisel, GemBreaking, Item, ItemDetails, ItemStack, ItemType, MaterialKind, Rarity}
import pangea.model.monster.Race
import pangea.model.squad.AllyRates
import pangea.model.top.{TopBoard, TopKind}
import pangea.service.chat.Links
import pangea.service.cube.AzatCube
import pangea.service.equipment.{BeltWork, BrewWork}
import pangea.service.state.QuestJournal
import pangea.service.storage.ItemStore.Place
import pangea.service.state.states.{EquipmentState, InventoryState}
import pangea.service.state.states.hero.SkillsState
import pangea.service.state.states.home.{RuneAltarState, TrophyWallState}
import pangea.service.state.states.temple.HallAzatState
import pangea.service.state.states.hero.SkillsState.SkillEntry
import pangea.util.TimeLeft

/** Что видит Mini App: те же вещи, что и бот, но данными, а не экраном.
  * Описание вещи (`text`) — тот же текст, что в карточке бота
  * ([[pangea.service.state.states.InventoryState.cardText]], его передаёт `describe`): правила показа
  * остаются в одном месте, а приложение раскладывает вещи, фильтрует и
  * сортирует их. */
object MiniAppView {

  /** Вещь. `kind` — тип ([[ItemType]]), `rarity` — редкость ([[Rarity]]),
    * `rarityRank` — её место от серой к оранжевой (для сортировки),
    * `count` — сколько таких лежит одной стопкой. */
  final case class ItemView(
    id:         Long,
    title:      String,
    kind:       String,
    rarity:     String,
    rarityRank: Int,
    lvl:        Long,
    count:      Int,
    text:       String,
    stats:      List[ItemStatView],
    skill:      Option[String],
    passive:    Option[String],
    sockets:    Int,
    gems:       Int,
    set:        Option[String],
    variant:    Option[String],
    // Сюжетное — предмет или экипировка (см. Item.isStoryGear): у него голубая
    // рамка вместо цвета редкости, а в названии ★.
    story:      Boolean
  )

  /** Прибавка вещи или разница при замене: `key` — `attack`, `defence`,
    * `armor`, `hp`, `evasion`, `accuracy`, `energy`. Нулевые не присылаем. */
  final case class ItemStatView(key: String, value: Long)

  /** Вещь из сумки на этот слот: что изменится у героя, если её надеть (`deltas`,
    * со всем — наборами, камнями, двуручным поверх двух одноручных), и как
    * сдвинется её набор (`set`). */
  final case class CandidateView(item: ItemView, deltas: List[ItemStatView], set: Option[SetChangeView])

  /** Набор вещи-кандидата: сколько его надето сейчас, сколько станет и из скольких. */
  final case class SetChangeView(name: String, before: Int, after: Int, max: Int)

  /** Камень в гнезде надетой вещи: `socket` — номер гнезда, `confirm` — что
    * спросить перед ломкой. */
  final case class SocketedView(socket: Int, name: String, confirm: String)

  /** Камень, который можно вставить в гнездо: `from` — где лежит (сумка или
    * Ларец), `item` — стопкой, как в списке. */
  final case class GemStockView(from: String, item: ItemView)

  /** Слот снаряжения: что надето; `busy` — пустой, но занят (левая рука под
    * двуручным: само оружие — в правой); `belt` — что лежит в поясе. `index` —
    * номер слота в [[EquipmentState.slots]]: им приложение говорит, куда
    * надеть и что снять. `gems` — камни в гнёздах надетой, `freeSocket` — есть
    * ли свободное гнездо, `chisel` — что спросить перед резцом; нет — резец
    * здесь не сработает или его нет в сумке. */
  final case class SlotView(index: Int, name: String, item: Option[ItemView], busy: Option[String],
                            belt: Option[BeltView], candidates: List[CandidateView],
                            gems: List[SocketedView], freeSocket: Boolean, chisel: Option[String])

  /** Ячейка пояса: что в ней и сколько из скольких. Опустевшая помнит, чем
    * была полна, — `uses` у неё ноль. `swap` — подпись, когда вместо неё кладут
    * другое. */
  final case class BeltCellView(name: String, uses: Int, max: Int, swap: String)

  /** Пояс: сколько ячеек всего и что в каждой. */
  final case class BeltView(capacity: Int, cells: List[BeltCellView])

  /** Хранилище вещей во вкладке «Вещи». `group` — `self` (при себе), `city`
    * (в городе), `home` (в доме). Закрытое — `locked` с тем, как его открыть.
    * У открытого — вещи по разделам (у стоек — по стойке на раздел) или счёт
    * (руны на алтаре, трофеи на стене). `used` из `capacity` — занятые места,
    * где их считают. */
  final case class StorageView(
    id:       String,
    group:    String,
    title:    String,
    locked:   Option[String],
    used:     Option[Long],
    capacity: Option[Long],
    silver:   Option[PoolView],
    note:     Option[String],
    sections: List[SectionView],
    counts:   List[CountView]
  )
  /** Раздел хранилища. `rack` — номер стойки, если раздел — стойка: её
    * комплект можно надеть. */
  final case class SectionView(title: Option[String], items: List[ItemView], rack: Option[Int])
  final case class CountView(label: String, value: Long)

  /** Шапка героя: уровень и деньги — во всех ответах; имя (у прошедших пролог
    * до имён его нет) и раса — для подписи над куклой снаряжения. */
  final case class HeroView(lvl: Long, silver: Long, doubloons: Long, name: Option[String], race: String)

  /** Куда можно переложить вещь `item` (у стопки — та же, что в списке) из
    * хранилища `from` (`bag` — сумка) прямо сейчас: открытые хранилища,
    * которые берут такую вещь и до которых герой дотянется. Есть ли там
    * место, скажет бот. `far` — такую вещь взяли бы и другие, но до них сейчас
    * не дотянуться (почему — `storesAway`). */
  final case class MoveView(from: String, item: Long, to: List[String], far: Boolean)

  /** Один из вариантов, на что потратить отвар: `id` уходит боту. */
  final case class BrewOptionView(id: String, label: String)

  /** Что можно сделать с отваром. `action` — как действие зовётся в запросе;
    * `refusal` — почему сейчас нельзя: нажатие только скажет это. Есть `options`
    * — `question` спрашивает, на что тратить, и выбор сразу тратит склянку; нет
    * — `question` переспрашивает, тратить ли. `hint` — что ещё сказать к вопросу. */
  final case class BrewAskView(action: String, label: String, question: String, hint: Option[String],
                               options: List[BrewOptionView], refusal: Option[String])

  /** Чем можно воспользоваться из сумки: `item` — как в списке вещей, `brew` —
    * что можно сделать с отваром, `belt` — кладётся ли на пояс, `beltFull` — на
    * поясе нет места, и вот вопрос, что снять. */
  final case class BagActView(item: Long, brew: List[BrewAskView], belt: Boolean, beltFull: Option[String])

  /** Вещи героя. `gearLocked` — почему снаряжение и комплект со стойки сейчас
    * не сменить: герой в бою; `storesAway` — почему дотянуться удастся не до
    * всех хранилищ: герой не в городе или в бою; `gemStock` — камни, которые
    * можно вставить в гнездо; `bagActs` — отвары и что кладётся на пояс, пусто
    * в бою. */
  final case class InventoryView(
    hero:       HeroView,
    capacity:   Long,
    occupied:   Long,
    items:      List[ItemView],
    equipment:  List[SlotView],
    storages:   List[StorageView],
    gearLocked: Option[String],
    storesAway: Option[String],
    moves:      List[MoveView],
    gemStock:   List[GemStockView],
    bagActs:    List[BagActView]
  )

  /** Ответ на «надеть» или «снять»: вещи после этого и что сказать. */
  final case class InventoryActionView(inventory: InventoryView, note: Option[String])

  /** Лот. `currency` — `silver` или `doubloons`; `unsold` — неделя вышла, лот
    * ждёт хозяина. */
  final case class LotView(
    id:        Long,
    item:      ItemView,
    price:     Long,
    currency:  String,
    hoursLeft: Long,
    mine:      Boolean,
    unsold:    Boolean
  )

  /** Витрина и свои лоты (свои — с непроданными).
    *
    * @param silver  сколько серебра наберётся на покупку: на руках вместе с ячейкой
    * @param bagFree свободных мест в сумке: купленное ложится туда
    * @param away    почему сейчас нельзя покупать: герой не в городе
    */
  final case class AuctionView(hero: HeroView, lots: List[LotView], mine: List[LotView], silver: Long, bagFree: Long,
                               away: Option[String])

  /** Ответ на покупку: витрина после неё, что сказал Рахадим и куплено ли. */
  final case class AuctionActionView(auction: AuctionView, note: String, bought: Boolean)

  /** Чем платить и куда класть: серебро на руках вместе с ячейкой и свободные
    * места в сумке. */
  final case class Funds(silver: Long, bagFree: Long)

  /** Характеристика: сейчас — со всем, что действует, и родная — до расы, травм
    * и зелий. В боте родная стоит первой, а текущая в скобках. */
  final case class StatView(current: Long, native: Long)

  /** Запас — здоровье, броня, энергия: сколько есть из скольких. */
  final case class PoolView(current: Long, max: Long)

  /** Травма и что она режет — как на экране травм в боте. */
  final case class TraumaView(name: String, penalties: String)

  /** «Выспались дома»: прибавка к опыту и сколько ей ещё. */
  final case class RestedView(expPct: Long, left: String)

  /** Эффект — как кнопка и её карточка на экране «Эффекты» бота. `kind`:
    * `active` — умение, `passive` — пассивка, `set` — набор, `weapon` и
    * `armor` — сводка камней в оружии и в доспехах. */
  final case class EffectView(kind: String, label: String, text: String)

  /** Полученное достижение и что оно даёт. */
  final case class AchievementView(title: String, bonus: String)

  /** Достижения: полученные и сколько всего. Неполученные не раскрываем — как в боте. */
  final case class AchievementsView(total: Int, got: List[AchievementView])

  /** Знание: что умеет герой и как узнал — «дошёл сам» или «по книге». */
  final case class KnowledgeView(title: String, how: String, text: String)

  /** «Персонаж» — те же числа, что в карточке бота ([[Hero.card]]). Сроки —
    * готовым текстом, теми же словами, что в боте. `name` — имя героя; у
    * прошедших пролог до имён его нет. */
  final case class CharacterView(
    hero:          HeroView,
    name:          Option[String],
    race:          String,
    exp:           Long,
    neededExp:     Long,
    kills:         Long,
    expFrozen:     Boolean,
    rested:        Option[RestedView],
    blessingLeft:  Option[String],
    instantRests:  Int,
    str:           StatView,
    vit:           StatView,
    agi:           StatView,
    int:           StatView,
    hp:            PoolView,
    armor:         PoolView,
    energy:        PoolView,
    attack:        Long,
    defence:       Long,
    accuracy:      Long,
    evasion:       Long,
    upgradePoints: Long,
    traumaLeft:    Option[String],
    traumas:       List[TraumaView],
    effects:       List[EffectView],
    achievements:  AchievementsView,
    knowledge:     List[KnowledgeView]
  )

  /** «Задания» — журнал бота по разделам ([[QuestJournal.Journal]]). У
    * объявления с доски `state` — «в работе» или «готово». */
  final case class StoryView(title: String, objectives: String)
  final case class NpcQuestView(title: String, step: String)
  final case class DailyView(npc: String, line: String)
  final case class BoardView(board: String, what: String, ready: Boolean, state: String)
  final case class QuestsView(
    hero:    HeroView,
    story:   Option[StoryView],
    npcs:    List[NpcQuestView],
    dailies: List[DailyView],
    boards:  List[BoardView]
  )

  /** Союзник в строю или в резерве — как его карточка в боте. `position` — место
    * в строю, у резерва 0. `element` — значок стихии, у крысы — зубы. `leaves` —
    * сколько ему ещё в отряде: наёмнику до конца найма, поднятому — пока держит
    * тёмная сила; без срока — пусто. `reservable` — можно ли отправить его в
    * резерв: крысу туда не берут. */
  final case class AllyView(
    position: Int,
    name:     String,
    race:     String,
    element:  String,
    lvl:      Long,
    hp:       PoolView,
    armor:    PoolView,
    energy:   PoolView,
    attack:   Long,
    defence:  Long,
    accuracy: Long,
    evasion:  Long,
    leaves:   Option[String],
    reservable: Boolean
  )

  /** Строка топа: место, имя героя, раса, уровень и то, по чему топ
    * считается; `profile` — страница игрока в ВК (на себя ссылки нет);
    * `me` — это сам герой. */
  final case class TopEntryView(place: Long, name: String, profile: Option[String], race: String, lvl: Long,
                                value: Long, me: Boolean)

  /** Топ: первые герои и сколько их в топе всего. `me` — строка героя, если
    * он не попал в показанных; попал — его строка помечена в `rows`. У админа,
    * которого в топе нет, `me` есть всегда. */
  final case class TopView(hero: HeroView, kind: String, total: Long, rows: List[TopEntryView], me: Option[TopEntryView])

  /** Сила куба: `own` — свои заряды, `home` — энергия дома (алтарь Азата). */
  final case class CubePowerView(kind: String, current: Long, max: Long)

  /** Куб Азата.
    *
    * @param status   `absent` — куба нет, `found` — найден, но не ожил, `active` — работает
    * @param locked   пока куб не работает — как его получить или оживить (тексты Зала Азата)
    * @param home     куб переехал домой, на верстак ремесленника
    * @param items    что в кубе — каждая вещь своей ячейкой
    * @param bag      сумка стопками — что можно положить: сюжетное в куб не кладётся
    * @param away     почему сейчас нельзя класть и активировать: герой не в городе
    */
  final case class CubeView(status: String, locked: Option[String], home: Boolean, power: CubePowerView, capacity: Int,
                            items: List[ItemView], bag: List[ItemView], away: Option[String])

  final case class CraftView(hero: HeroView, cube: CubeView)

  /** Ответ на действие с кубом: куб после него, что сказать и номера вещей,
    * что только что получились. */
  final case class CraftActionView(craft: CraftView, note: Option[String], made: List[Long])

  /** «Отряд»: сколько мест в строю, где стоит герой и кто стоит рядом.
    * `pendingRisen` — восставший ждёт у шатра, а мест нет ни в строю, ни в
    * резерве: надо решить, кем за него пожертвовать. `locked` — почему строй
    * сейчас не перестроить: герой в бою. `reserve` — кто при герое, но не в
    * строю, по порядку: приказы адресуют его номером в этом списке и именем;
    * `reserveSize` — сколько в резерве мест. */
  final case class SquadView(
    hero:         HeroView,
    positions:    Int,
    heroPosition: Int,
    allies:       List[AllyView],
    pendingRisen: Option[String],
    locked:       Option[String],
    reserve:      List[AllyView],
    reserveSize:  Int
  )

  /** Ответ на приказ отряду: строй после него и что сказать. */
  final case class SquadActionView(squad: SquadView, note: Option[String])

  implicit val itemStatEncoder:  Encoder[ItemStatView]  = deriveEncoder
  implicit val itemEncoder:      Encoder[ItemView]      = deriveEncoder
  implicit val setChangeEncoder: Encoder[SetChangeView] = deriveEncoder
  implicit val candidateEncoder: Encoder[CandidateView] = deriveEncoder
  implicit val beltCellEncoder:  Encoder[BeltCellView]  = deriveEncoder
  implicit val beltEncoder:      Encoder[BeltView]      = deriveEncoder
  implicit val socketedEncoder:  Encoder[SocketedView]  = deriveEncoder
  implicit val slotEncoder:      Encoder[SlotView]      = deriveEncoder
  implicit val sectionEncoder:   Encoder[SectionView]   = deriveEncoder
  implicit val countEncoder:     Encoder[CountView]     = deriveEncoder
  implicit val storageEncoder:   Encoder[StorageView]   = deriveEncoder
  implicit val moveEncoder:      Encoder[MoveView]      = deriveEncoder
  implicit val gemStockEncoder:  Encoder[GemStockView]  = deriveEncoder
  implicit val brewOptionEncoder: Encoder[BrewOptionView] = deriveEncoder
  implicit val brewAskEncoder:   Encoder[BrewAskView]   = deriveEncoder
  implicit val bagActEncoder:    Encoder[BagActView]    = deriveEncoder
  implicit val heroEncoder:      Encoder[HeroView]      = deriveEncoder
  implicit val inventoryEncoder: Encoder[InventoryView] = deriveEncoder
  implicit val inventoryActionEncoder: Encoder[InventoryActionView] = deriveEncoder
  implicit val lotEncoder:       Encoder[LotView]       = deriveEncoder
  implicit val auctionEncoder:   Encoder[AuctionView]   = deriveEncoder
  implicit val auctionActionEncoder: Encoder[AuctionActionView] = deriveEncoder
  implicit val statEncoder:      Encoder[StatView]      = deriveEncoder
  implicit val poolEncoder:      Encoder[PoolView]      = deriveEncoder
  implicit val traumaEncoder:    Encoder[TraumaView]    = deriveEncoder
  implicit val restedEncoder:    Encoder[RestedView]    = deriveEncoder
  implicit val effectEncoder:      Encoder[EffectView]       = deriveEncoder
  implicit val achievementEncoder: Encoder[AchievementView]  = deriveEncoder
  implicit val achievementsEncoder: Encoder[AchievementsView] = deriveEncoder
  implicit val knowledgeEncoder:   Encoder[KnowledgeView]    = deriveEncoder
  implicit val characterEncoder: Encoder[CharacterView] = deriveEncoder
  implicit val storyEncoder:     Encoder[StoryView]     = deriveEncoder
  implicit val npcQuestEncoder:  Encoder[NpcQuestView]  = deriveEncoder
  implicit val dailyEncoder:     Encoder[DailyView]     = deriveEncoder
  implicit val boardEncoder:     Encoder[BoardView]     = deriveEncoder
  implicit val questsEncoder:    Encoder[QuestsView]    = deriveEncoder
  implicit val allyEncoder:      Encoder[AllyView]      = deriveEncoder
  implicit val squadEncoder:     Encoder[SquadView]     = deriveEncoder
  implicit val squadActionEncoder: Encoder[SquadActionView] = deriveEncoder
  implicit val topEntryEncoder:  Encoder[TopEntryView]  = deriveEncoder
  implicit val topEncoder:       Encoder[TopView]       = deriveEncoder
  implicit val cubePowerEncoder: Encoder[CubePowerView] = deriveEncoder
  implicit val cubeEncoder:      Encoder[CubeView]      = deriveEncoder
  implicit val craftEncoder:     Encoder[CraftView]     = deriveEncoder
  implicit val craftActEncoder:  Encoder[CraftActionView] = deriveEncoder

  def hero(h: Hero): HeroView = HeroView(h.lvl, h.silver, h.doubloons, h.name, h.raceName)

  /** Что лежит у героя по хранилищам — кроме сумки: она приходит своим полем. */
  final case class Stores(artifacts: HeroArtifacts, barrel: Barrel, vault: BankVault, home: HomeData)

  /** Сумка — стопками, как в боте, — всё надетое и остальные хранилища. */
  def inventory(h: Hero, inv: Inventory, stores: Stores, content: SceneContent, describe: Item => String,
                now: Long): InventoryView = {
    val before = h.card(now)
    val views  = storages(stores, content, describe)
    InventoryView(
      hero      = hero(h),
      capacity  = inv.maxItems,
      occupied  = inv.occupied,
      items     = bag(inv.items.data, content, describe),
      equipment = EquipmentState.slots.zipWithIndex.map { case (slot, idx) =>
                    val worn = slot.get(h.equipment)
                    SlotView(
                      index      = idx,
                      name       = slot.name,
                      // Описание надетой — что она даёт, без названия и без сравнения с собой.
                      item       = Option.when(worn.itemType != ItemType.NoItem)(view(worn, 1, InventoryState.gearBody(worn, Some(h)))),
                      busy       = Option.when(slot.busy(h.equipment))(EquipmentState.LeftBusyText),
                      belt       = belt(worn, content),
                      candidates = candidates(h, before, slot, inv.items.data, describe, now),
                      gems       = socketed(worn, content),
                      freeSocket = worn.hasFreeSocket,
                      chisel     = chiselAsk(worn, inv, content)
                    )
                  },
      storages  = views,
      gearLocked = Option.when(MiniApp.inBattle(h))(content.text("miniapp.equipment.inBattle")),
      storesAway = MiniApp.storesAway(content, h),
      moves      = moves(h, inv, stores, views),
      gemStock   = gemStock(inv, stores, describe),
      bagActs    = bagActs(h, inv, content, now)
    )
  }

  /** Что сделать с вещами сумки — те же кнопки, что на карточке в боте; в бою
    * из сумки ничего не достать. */
  private def bagActs(h: Hero, inv: Inventory, content: SceneContent, now: Long): List[BagActView] =
    if (MiniApp.inBattle(h)) Nil
    else InventoryState.listed(inv.items.data).flatMap { case (item, _) =>
      val asks = brewAsks(h, item, content, now)
      val slot = BeltSlot.of(item)
      Option.when(asks.nonEmpty || slot.isDefined)(
        BagActView(item.id, asks, slot.isDefined, slot.flatMap(beltFull(h, _, content))))
    }

  private def beltFull(h: Hero, slot: BeltSlot, content: SceneContent): Option[String] = {
    val worn = h.equipment.belt
    worn.details match {
      case b: ItemDetails.Belt if worn.itemType == ItemType.Belt && !BeltWork.fits(slot, b, worn.rarity) =>
        Some(content.format("inventory.beltSwapChoice", "name" -> slot.name))
      case _ => None
    }
  }

  private def brewAsks(h: Hero, item: Item, content: SceneContent, now: Long): List[BrewAskView] =
    item.brew.toList.flatMap { kind =>
      val confirm = content.format("miniapp.brew.confirm", "name" -> item.name)
      val drink = Option.when(kind.drinkable)(kind.effect match {
        case BrewEffect.CureTrauma => cureAsk(h, content, now)
        case BrewEffect.Sentence =>
          BrewAskView("sentence", content.text("brew.drink"), content.text("brew.whichRace"), None,
            Race.mortals.toList.map(r => BrewOptionView(r.entryName, r.toString)), None)
        case _ => BrewAskView("drink", content.text("brew.drink"), confirm, None, Nil, None)
      })
      val coat = kind.effect match {
        case BrewEffect.Coat(_, _) =>
          Some(BrewAskView("coat", content.text("brew.coat"), confirm, None, Nil,
            Option.when(h.equipment.unarmed)(content.text("brew.noWeapon"))))
        case _ => None
      }
      val refill = Option.when(BrewWork.fittingFlasks(h, item).nonEmpty) {
        val open = BrewWork.openFlasks(h, item).map(f => BrewOptionView(f.slot.toString, content.format("brew.flaskPick",
          "n" -> (f.slot + 1).toString, "name" -> f.name, "charges" -> f.charges.toString, "max" -> f.maxCharges.toString)))
        BrewAskView("refill", content.text("brew.refill"), content.text("brew.whichFlask"), None, open,
          Option.when(open.isEmpty)(content.text("brew.flaskFull")))
      }
      List(drink, coat, refill).flatten
    }

  /** Какую травму лечить: лёгкие и средние на выбор, тяжёлые — только названы. */
  private def cureAsk(h: Hero, content: SceneContent, now: Long): BrewAskView = {
    val curable = BrewWork.curable(h, now)
    val heavy   = BrewWork.heavy(h, now)
    BrewAskView("cure", content.text("brew.drink"), content.text("brew.whichTrauma"),
      Option.when(heavy.nonEmpty)(content.format("brew.heavyStay", "names" -> heavy.map(_.name).mkString(", "))),
      curable.map(t => BrewOptionView(t.name, t.name)),
      if (curable.nonEmpty) None
      else Some(content.text(if (heavy.isEmpty) "brew.noTraumas" else "brew.onlyHeavy")))
  }

  /** Камни в гнёздах вещи и что спросить перед ломкой — как в боте. */
  private def socketed(worn: Item, content: SceneContent): List[SocketedView] =
    GemBreaking.socketed(worn).map { case (i, g) =>
      SocketedView(i, g.displayName, content.format("equipment.breakGemConfirm",
        "gem" -> g.displayName, "count" -> g.dustYield.toString, "dust" -> g.dust.displayName))
    }

  /** Что спросить перед резцом; нет резца в сумке или узор этой вещи ему не по
    * зубам ([[Chisel.fits]]) — ничего. */
  private def chiselAsk(worn: Item, inv: Inventory, content: SceneContent): Option[String] =
    Option.when(Chisel.fits(worn) && inv.items.data.exists(_.material.contains(MaterialKind.RuneChisel)))(
      content.format("inventory.chisel.confirm", "name" -> worn.name,
        "pattern" -> Chisel.pattern(worn).getOrElse(content.text("inventory.chisel.noPattern"))))

  /** Камни для гнезда — сумка и Ларец ([[Place.gemSources]]): одинаковые одной
    * строкой, виды вместе, старшие первыми. */
  private def gemStock(inv: Inventory, st: Stores, describe: Item => String): List[GemStockView] = {
    def inside(p: Place): List[Item] = p match {
      case Place.Art(kind) => st.artifacts.of(kind).items.data
      case _               => inv.items.data
    }
    Place.gemSources.flatMap { p =>
      ItemStack.grouped(inside(p)).flatMap { case (item, count) =>
        item.gem.map(g => ((g.kind.entryName, -g.grade), GemStockView(p.id, view(item, count, describe(item)))))
      }
    }.sortBy(_._1).map(_._2)
  }

  /** Что где лежит и куда его можно переложить: в открытые хранилища, которые
    * берут такую вещь ([[ItemStore.Place.accepts]]) и до которых герой сейчас
    * дотянется ([[MiniApp.reach]]). Вскрытая ворами бочка не отдаёт и не берёт
    * ничего. Стопка — одной строкой, как в списке. */
  private def moves(h: Hero, inv: Inventory, st: Stores, storages: List[StorageView]): List[MoveView] = {
    val open = Place.all.filter(p => p == Place.Bag || storages.exists(s => s.id == p.id && s.locked.isEmpty))
      .filterNot(p => p == Place.Barrel && st.barrel.robbed)
    def inside(p: Place): List[Item] = p match {
      case Place.Bag         => inv.items.data
      case Place.Art(kind)   => st.artifacts.of(kind).items.data
      case Place.Barrel      => st.barrel.items.data
      case Place.Vault       => st.vault.items.data
      case Place.Shelf(kind) => kind.get(st.home)
    }
    val near = MiniApp.reach(h)
    for {
      from      <- open
      (item, _) <- ItemStack.grouped(inside(from))
      takers     = open.filter(p => p != from && p.accepts(item))
      if takers.nonEmpty
      to         = takers.filter(p => near(from) && near(p))
    } yield MoveView(from.id, item.id, to.map(_.id), far = to.size < takers.size)
  }

  /** Что из сумки встанет в слот ([[EquipmentState.fitting]]) и что изменится,
    * если надеть: вещь примеряется так же, как её надевает бот
    * ([[InventoryState.equipWith]]), и сравниваются карточки героя до и после.
    * Одинаковые вещи — одной строкой со счётчиком. */
  private def candidates(h: Hero, before: HeroCard, slot: EquipmentState.Slot, items: List[Item],
                         describe: Item => String, now: Long): List[CandidateView] =
    ItemStack.grouped(EquipmentState.fitting(h, slot, items)).map { case (it, count) =>
      val done  = InventoryState.equipWith(h, it, slot.pairSlot, now)
      val worn  = h.copy(equipment = done.equipment, fightStats = done.fightStats)
      val after = worn.card(now)
      CandidateView(
        item   = view(it, count, describe(it)),
        deltas = statList(
          "attack"   -> (after.fight.atk - before.fight.atk),
          "defence"  -> (after.fight.defence - before.fight.defence),
          "armor"    -> (after.maxArmor - before.maxArmor),
          "hp"       -> (after.maxHp - before.maxHp),
          "evasion"  -> (after.fight.evasion - before.fight.evasion),
          "accuracy" -> (after.fight.accuracy - before.fight.accuracy),
          "energy"   -> (after.maxEnergy - before.maxEnergy)),
        set    = it.set.map(st => SetChangeView(st.label, h.sets.shownPieces(st), worn.sets.shownPieces(st),
                   worn.sets.thresholds(st).maxOption.getOrElse(worn.sets.shownPieces(st))))
      )
    }

  private def statList(pairs: (String, Long)*): List[ItemStatView] =
    pairs.toList.collect { case (k, v) if v != 0L => ItemStatView(k, v) }

  private def stacked(items: List[Item], describe: Item => String): List[ItemView] =
    ItemStack.grouped(items).map { case (item, count) => view(item, count, describe(item)) }

  /** Сумка — как в боте ([[InventoryState.listed]]): стопками, а ключи стоек,
    * когда их два и больше, — одной строкой «★ Связка ключей (N)». Связка не
    * стопка: счётчика «×N» у неё нет — какие ключи на кольце, говорит её
    * карточка. */
  private def bag(items: List[Item], content: SceneContent, describe: Item => String): List[ItemView] = {
    val ring = InventoryState.keyRing(items)
    InventoryState.listed(items).map { case (item, count) =>
      ring.filter(_ => InventoryState.isRackKey(item)) match {
        case Some(keys) =>
          view(item, 1, InventoryState.keyRingText(content, keys.map(_._1)))
            .copy(title = InventoryState.keyRingLabel(content, keys.size))
        case None => view(item, count, describe(item))
      }
    }
  }

  /** Пояс по ячейкам — те же числа, что в его карточке ([[Item.statsLines]]). */
  private def belt(worn: Item, content: SceneContent): Option[BeltView] = worn.details match {
    case b: ItemDetails.Belt =>
      val cap = ItemDetails.Belt.capacityFor(worn.rarity)
      Some(BeltView(cap, b.slots.map { slot =>
        val swap = content.format(if (slot.uses > 0) "inventory.beltSwapSlot" else "inventory.beltSwapSlotEmpty", "name" -> slot.name)
        slot match {
          case f: BeltSlot.Flask  => BeltCellView(f.name, f.charges.max(0), f.maxCharges, swap)
          case st: BeltSlot.Stack => BeltCellView(st.name, st.count.max(0), cap, swap)
          case r: BeltSlot.Rose   => BeltCellView(r.name, r.charges.max(0), r.maxCharges, swap)
        }
      }))
    case _ => None
  }

  /** Хранилища в том порядке, в каком они стоят во вкладке: при себе, в
    * городе, в доме. Закрытые — тоже, с тем, как их открыть. */
  private def storages(st: Stores, content: SceneContent, describe: Item => String): List[StorageView] = {
    def t(id: String, key: String) = content.text(s"miniapp.storages.$id.$key")
    def items(list: List[Item]) = List(SectionView(None, stacked(list, describe), None))
    def open(id: String, group: String, used: Option[Long], cap: Option[Long], silver: Option[PoolView] = None,
             note: Option[String] = None, sections: List[SectionView] = Nil, counts: List[CountView] = Nil) =
      StorageView(id, group, t(id, "title"), None, used, cap, silver, note, sections, counts)
    def closed(id: String, group: String, why: String) =
      StorageView(id, group, t(id, "title"), Some(why), None, None, None, None, Nil, Nil)

    def artifact(id: String, a: Artifact) =
      if (!a.owned) closed(id, "self", t(id, "locked"))
      else open(id, "self", Some(a.occupied.toLong), Some(a.capacity.toLong), sections = items(a.items.data))

    val home  = st.home
    val tier  = home.house.fold(0)(_.tier)
    val bench = tier >= House.Townhouse.tier
    val isle  = home.house.contains(House.Island)
    // Без дома замок у всех хранилищ дома — и одна причина на всех.
    def atHome(id: String, available: Boolean)(view: => StorageView) =
      if (!home.owns) closed(id, "home", content.text("miniapp.noHouse"))
      else if (!available) closed(id, "home", t(id, "locked"))
      else view

    List(
      artifact("casket", st.artifacts.casket),
      artifact("livingBag", st.artifacts.bag),
      artifact("wardrobe", st.artifacts.wardrobe),
      open("barrel", "city", Some(st.barrel.occupied), Some(Barrel.MaxItems),
        silver = Some(PoolView(st.barrel.silver, Barrel.MaxSilver)),
        note = Option.when(st.barrel.robbed)(t("barrel", "robbed")),
        sections = items(st.barrel.items.data)),
      if (!st.vault.open) closed("vault", "city", t("vault", "locked"))
      else open("vault", "city", Some(st.vault.occupied), Some(st.vault.maxItems),
        silver = Some(PoolView(st.vault.silver, st.vault.maxSilver)), sections = items(st.vault.items.data)),
      atHome("herbs", bench)(open("herbs", "home", Some(home.herbs.size.toLong), Some(home.herbShelves.toLong),
        sections = items(home.herbs))),
      atHome("brews", bench)(open("brews", "home", Some(home.brews.size.toLong), Some(home.brewShelves.toLong),
        sections = items(home.brews))),
      atHome("cauldron", bench)(open("cauldron", "home", Some(home.cauldron.size.toLong),
        Some(HomeRates.CauldronSlots.toLong), sections = items(home.cauldron))),
      atHome("gems", isle)(open("gems", "home", Some(home.gems.size.toLong), Some(home.gemShelves.toLong),
        sections = items(home.gems))),
      atHome("runes", available = true)(open("runes", "home", None, None,
        counts = RuneAltarState.onAltar(home).map { case (rune, n) => CountView(rune.label, n) })),
      atHome("wall", isle)(open("wall", "home", None, None,
        counts = TrophyWallState.Races.map(r =>
          CountView(TrophyWallState.raceName(content, r), TrophyWallState.hung(home, r).size.toLong)))),
      atHome("racks", home.racks.nonEmpty)(open("racks", "home", None, None,
        sections = home.racks.zipWithIndex.map { case (rack, i) =>
          SectionView(Some(content.format("miniapp.storages.racks.rack", "n" -> (i + 1).toString)),
            rack.allItems.filter(_.itemType != ItemType.NoItem).map(it => view(it, 1, EquipmentState.itemStats(it))),
            Some(i + 1))
        }))
    )
  }

  /** Витрина и свои лоты. Описание вещи в лоте — со сравнением с надетым на
    * герое, как в карточке бота. */
  def auction(h: Hero, onSale: List[AuctionLot], mine: List[AuctionLot], now: Long,
              describe: Item => String, funds: Funds, content: SceneContent): AuctionView = {
    def lot(l: AuctionLot): LotView =
      LotView(
        id        = l.id,
        item      = view(l.item, 1, describe(l.item)),
        price     = l.price,
        currency  = currency(l.currency),
        hoursLeft = l.hoursLeft(now),
        mine      = l.sellerId == h.id,
        unsold    = l.unsold(now)
      )
    AuctionView(hero(h), onSale.map(lot), mine.map(lot), funds.silver, funds.bagFree,
      Option.unless(MiniApp.inCity(h))(content.text("miniapp.auction.away")))
  }

  /** «Персонаж». Благословение и быстрые отдыхи — из состояния храма (`azat`),
    * знания — из `lore`, остальное — с героя: карточка, эффекты, достижения. */
  def character(h: Hero, azat: AzatState, lore: LoreData, content: SceneContent, now: Long): CharacterView = {
    val c = h.card(now)
    CharacterView(
      hero          = hero(h),
      name          = h.name,
      race          = c.race,
      exp           = c.exp,
      neededExp     = c.neededExp,
      kills         = c.kills,
      expFrozen     = c.expFrozen,
      rested        = c.restedLeftMs.map(ms => RestedView(HomeRates.RestedExpPct, HeroCard.restedLeft(ms))),
      blessingLeft  = azat.blessingUntil.filter(_ > now).map(until => TimeLeft.format(until - now)),
      instantRests  = azat.instantRests,
      str           = StatView(c.stats.str, c.native.str),
      vit           = StatView(c.stats.vit, c.native.vit),
      agi           = StatView(c.stats.agi, c.native.agi),
      int           = StatView(c.stats.int, c.native.int),
      hp            = PoolView(c.hp, c.maxHp),
      armor         = PoolView(c.armor, c.maxArmor),
      energy        = PoolView(c.energy, c.maxEnergy),
      attack        = c.fight.atk,
      defence       = c.fight.defence,
      accuracy      = c.fight.accuracy,
      evasion       = c.fight.evasion,
      upgradePoints = c.upgradePoints,
      traumaLeft    = c.traumaLeftMs.map(HeroCard.traumaLeft),
      traumas       = c.traumas.map(t => TraumaView(t.name, t.penalties.shortText)),
      effects       = SkillsState.entries(h, SkillsState.buffs(content, h, now, azat.blessingUntil))
                        .map(e => EffectView(effectKind(e), e.label, e.describe(h))),
      achievements  = AchievementsView(Achievement.values.size,
                        Achievement.values.toList.filter(h.hasAchievement).map(a => AchievementView(a.title, a.bonusLine))),
      knowledge     = Knowledge.values.toList.filter(lore.knows).map { k =>
                        val how = if (lore.learnedAlone(k)) "knowledge.selfTaught" else "knowledge.fromBook"
                        KnowledgeView(k.title, content.text(how), k.description)
                      }
    )
  }

  /** Топ из снимка: первые герои и место самого героя. Не попал в первых —
    * его строка отдельно: место из снимка, остальное — с самого героя. Героя
    * без имени (прошёл пролог до имён, а из ВК имени не подобрали)
    * подписываем `noName`. */
  def top(h: Hero, kind: TopKind, board: TopBoard, noName: String): TopView = {
    val shown = board.rows.zipWithIndex.map { case (row, i) =>
      val me = row.heroId == h.id
      TopEntryView(i + 1L, row.name.getOrElse(noName), Option.unless(me)(Links.profile(row.vkId.value)),
        row.race.toString, row.lvl, row.value, me)
    }
    val mine = Option.unless(shown.exists(_.me))(board.places.get(h.id)).flatten.map { at =>
      TopEntryView(at.place, h.name.getOrElse(noName), None, h.race.toString, h.lvl, at.value, me = true)
    }
    TopView(hero = hero(h), kind = kind.entryName, total = board.total, rows = shown, me = mine)
  }

  /** «Крафт»: куб Азата, что в нём, и сумка, из которой в него кладут. */
  def craft(h: Hero, azat: AzatState, home: HomeData, inv: Inventory, content: SceneContent,
            describe: Item => String): CraftView = {
    val (status, locked) = azat.cube match {
      case CubeStatus.None =>
        "absent" -> Some(content.format("hall.cube.absent", "price" -> HallAzatState.CubePrice.toString))
      case CubeStatus.FoundInactive =>
        "found" -> Some(content.format("hall.cube.inactive", "doubloons" -> HallAzatState.ActivateDoubloons.toString,
          "silver" -> HallAzatState.ActivateSilver.toString))
      case CubeStatus.Active => "active" -> None
    }
    val power = AzatCube.power(azat, home) match {
      case AzatCube.Power.Home(n, max) => CubePowerView("home", n.toLong, max.toLong)
      case AzatCube.Power.Own(n)       => CubePowerView("own", n.toLong, AzatState.MaxCharges.toLong)
    }
    CraftView(hero(h), CubeView(
      status   = status,
      locked   = locked,
      home     = azat.cubeHome,
      power    = power,
      capacity = AzatState.CubeCapacity,
      items    = azat.cubeItems.map(i => view(i, 1, describe(i))),
      bag      = stacked(inv.items.data.filter(AzatCube.accepts), describe),
      away     = Option.unless(MiniApp.inCity(h))(content.text("miniapp.craft.away"))
    ))
  }

  private def effectKind(e: SkillEntry): String = e match {
    case _: SkillEntry.Active  => "active"
    case _: SkillEntry.Passive => "passive"
    case _: SkillEntry.Set     => "set"
    case SkillEntry.WeaponFx   => "weapon"
    case SkillEntry.ArmorFx    => "armor"
    case _: SkillEntry.Buffs   => "buffs"
  }

  /** «Задания» — тот же журнал, что в боте, по разделам. */
  def quests(h: Hero, j: QuestJournal.Journal, content: SceneContent): QuestsView =
    QuestsView(
      hero    = hero(h),
      story   = j.story.map(s => StoryView(s.title, s.objectives)),
      npcs    = j.npcs.map(n => NpcQuestView(n.title, n.step)),
      dailies = j.dailies.map(d => DailyView(d.npc, d.line)),
      // Сделанное — коротко, на плашку; «в работе» — как в журнале бота.
      boards  = j.boards.map(b => BoardView(b.board, b.what, b.ready,
                  if (b.ready) content.text("miniapp.boardReady") else QuestJournal.boardState(content, b)))
    )

  /** «Отряд». Кто уже ушёл (найм кончился, кости рассыпались), бот уберёт и
    * запишет при ближайшем визите героя туда, где отряд на виду; приложение
    * ничего не пишет и просто их не показывает. */
  def squad(h: Hero, content: SceneContent, now: Long): SquadView = {
    val (staying, _) = h.squad.settleUndead(now).expire(now)
    def view(a: pangea.model.squad.Ally): AllyView = {
      val s = a.statsAt(h.lvl)
      AllyView(
        position   = a.position,
        name       = a.name,
        race       = a.kind.race.toString,
        element    = a.kind.element.map(_.emoji).getOrElse(content.text("squad.noElement")),
        lvl        = a.lvlAt(h.lvl),
        hp         = PoolView(a.hp, s.hp),
        armor      = PoolView(a.armor, s.armor),
        energy     = PoolView(a.energy, s.energy),
        attack     = s.atk,
        defence    = s.defence,
        accuracy   = s.accuracy,
        evasion    = s.evasion,
        leaves     = Option.when(a.hiredUntil > now)(TimeLeft.format(a.hiredUntil - now)),
        reservable = staying.reservable(a)
      )
    }
    SquadView(
      hero         = hero(h),
      positions    = AllyRates.Positions,
      heroPosition = staying.heroPos,
      allies       = staying.inOrder.map(view),
      pendingRisen = staying.pendingRisen.map(_.name),
      locked       = Option.when(MiniApp.inBattle(h))(content.text("miniapp.squad.inBattle")),
      reserve      = staying.reserve.map(view),
      reserveSize  = AllyRates.ReserveSize
    )
  }

  /** Стопку приложение показывает видом того, что в ней лежит: значок и фильтр
    * — от содержимого, а что это стопка, видно по значку в названии («📦 Мифрил»). */
  private def view(item: Item, count: Int, text: String): ItemView = {
    val shape = Bundling.contents(item).headOption.getOrElse(item)
    ItemView(
      id         = item.id,
      title      = item.displayTitle,
      kind       = shape.itemType.entryName,
      rarity     = item.rarity.entryName,
      rarityRank = Rarity.order(item.rarity),
      lvl        = item.lvl,
      count      = count,
      text       = text,
      stats      = statList("attack" -> item.attack, "defence" -> item.defence, "armor" -> item.armor,
                     "hp" -> item.hp, "evasion" -> item.evasion, "accuracy" -> item.accuracy, "energy" -> item.energy),
      skill      = item.activeSkill.map(_.label),
      passive    = item.passive.map(_.label),
      sockets    = item.sockets.size,
      gems       = item.sockets.count(_.isDefined),
      set        = item.set.map(_.label),
      variant    = variant(shape),
      story      = item.isStoryGear || item.isQuestItem
    )
  }

  /** Чем вещь отличается внутри своего типа — для значка: лук и арбалет среди
    * оружия, трава и пыль среди материалов. */
  private def variant(item: Item): Option[String] = item.details match {
    case _ if item.itemType == ItemType.Weapon && item.ranged => Some("ranged")
    case ItemDetails.Material(k) if k.isHerb                  => Some("herb")
    case ItemDetails.Material(k) if k.gem.isDefined           => Some("dust")
    case _                                                    => None
  }

  private def currency(c: AuctionCurrency): String = c match {
    case AuctionCurrency.Silver    => "silver"
    case AuctionCurrency.Doubloons => "doubloons"
  }
}
