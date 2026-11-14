// Ответы бота — см. pangea.service.miniapp.MiniAppView.

export interface ItemView {
  id: number
  title: string
  /** Тип вещи — ItemType бота: Helmet, Weapon, Gem… */
  kind: string
  /** Редкость — Rarity бота: Gray, White, Green, Blue, Purple, Violet, Orange. */
  rarity: string
  /** Место редкости от серой к оранжевой. */
  rarityRank: number
  lvl: number
  /** Сколько таких лежит одной стопкой. */
  count: number
  /** Описание — тот же текст, что в карточке бота. */
  text: string
  /** Прибавки вещи, без нулевых. */
  stats: StatChange[]
  /** Активное умение и пассивка вещи. */
  skill: string | null
  passive: string | null
  /** Гнёзд всего и сколько из них с камнем. */
  sockets: number
  gems: number
  set: string | null
  /** Подвид для значка: `ranged` — лук или арбалет, `herb` — трава, `dust` — пыль. */
  variant: string | null
  /** Качество камня: 1 — надколотый … 5 — безупречный; у прочих вещей — нет. */
  grade?: number | null
  /** Сюжетное — предмет или экипировка: голубая рамка вместо цвета редкости. */
  story: boolean
}

/** Прибавка или разница: attack, defence, armor, hp, evasion, accuracy, energy. */
export interface StatChange {
  key: string
  value: number
}

/** Вещь из сумки на слот: что изменится у героя, если её надеть, и её набор. */
export interface CandidateView {
  item: ItemView
  deltas: StatChange[]
  set: { name: string; before: number; after: number; max: number } | null
}

export interface SlotView {
  /** Номер слота: им говорят боту, куда надеть и что снять. */
  index: number
  name: string
  item: ItemView | null
  /** Пустой слот, занятый чужой вещью: левая рука под двуручным. */
  busy: string | null
  /** Что лежит в поясе — только у слота пояса. */
  belt: BeltView | null
  /** Что из сумки встанет в этот слот. */
  candidates: CandidateView[]
  /** Камни в гнёздах надетой вещи. */
  gems: SocketedView[]
  /** Есть ли свободное гнездо. */
  freeSocket: boolean
  /** Что спросить перед резцом; нет — резец здесь не сработает или его нет в сумке. */
  chisel: string | null
}

/** Пояс: сколько ячеек всего и что в каждой. Опустевшая ячейка помнит, чем
  * была полна, — `uses` у неё ноль. */
export interface BeltView {
  capacity: number
  /** `swap` — подпись, когда вместо ячейки кладут другое. */
  cells: { name: string; uses: number; max: number; swap: string }[]
}

/** Вариант, на что потратить отвар: `id` уходит боту. */
export interface BrewOptionView {
  id: string
  label: string
}

/** Что можно сделать с отваром. `refusal` — почему сейчас нельзя; есть `options` —
  * `question` спрашивает, на что тратить; нет — переспрашивает, тратить ли. */
export interface BrewAskView {
  action: string
  label: string
  question: string
  hint: string | null
  options: BrewOptionView[]
  refusal: string | null
}

/** Чем можно воспользоваться из сумки. `beltFull` — на поясе нет места, и вот вопрос, что снять;
 *  `unpack` — это стопка, её можно распаковать; `pack` — таких в сумке на стопку хватает, её можно
 *  собрать (у бота старше приложения поля нет); `crush` — это камень, его можно расколоть, и вот
 *  вопрос перед этим; `dust` — это пыль, ею посыпают надетое оружие; `read` — это трактат, его
 *  можно читать; `quest` — кнопка задания на карточке сюжетного предмета. */
export interface BagActView {
  item: number
  brew: BrewAskView[]
  belt: boolean
  beltFull: string | null
  unpack: boolean
  pack?: boolean
  crush: string | null
  dust: boolean
  read: boolean
  quest: QuestActView | null
}

/** Кнопка задания — та же, что на карточке в боте: `act` — её имя у бота. */
export interface QuestActView {
  act: string
  label: string
}

/** Что спросить, прежде чем идти: вопрос и ответы — те же, что в боте. */
export interface QuestAskView {
  question: string
  options: QuestActView[]
}

export interface HeroView {
  lvl: number
  silver: number
  doubloons: number
  /** Имя героя; у прошедших пролог до имён его нет. */
  name: string | null
  race: string
}

export interface InventoryView {
  hero: HeroView
  capacity: number
  occupied: number
  items: ItemView[]
  equipment: SlotView[]
  /** Остальные хранилища: артефакты при себе, бочка и ячейка в городе, дом. */
  storages: StorageView[]
  /** Почему снаряжение и комплект со стойки сейчас не сменить: герой в бою. */
  gearLocked: string | null
  /** Почему дотянуться можно не до всех хранилищ: герой не в городе или в бою. */
  storesAway: string | null
  /** Куда можно переложить каждую вещь. */
  moves: MoveView[]
  /** Камни, которые можно вставить в гнездо: из сумки и Ларца. */
  gemStock: GemStockView[]
  /** Отвары и что кладётся на пояс; пусто в бою. */
  bagActs: BagActView[]
  /** Что можно выбросить прямо сейчас. */
  drops: DropView[]
}

/** Вещь `item` (у стопки — та же, что в списке) из хранилища `from` (`bag` —
  * сумка) можно выбросить прямо сейчас: хранилище под рукой, вещь не сюжетная. */
export interface DropView {
  from: string
  item: number
}

/** Камень в гнезде надетой вещи. */
export interface SocketedView {
  /** Номер гнезда: им приложение говорит, какой камень сломать. */
  socket: number
  name: string
  /** Что спросить перед ломкой. */
  confirm: string
}

/** Камень, который можно вставить в гнездо. */
export interface GemStockView {
  /** Где лежит: `bag` или `casket`. */
  from: string
  item: ItemView
}

/** Куда можно переложить вещь `item` (у стопки — та же, что в списке) из
  * хранилища `from` (`bag` — сумка) прямо сейчас. Есть ли там место, скажет
  * бот. `far` — взяли бы и другие, но до них сейчас не дотянуться. */
export interface MoveView {
  from: string
  item: number
  to: string[]
  far: boolean
}

/** Ответ на «надеть» или «снять»: вещи после этого и что сказать. */
export interface InventoryActionView {
  inventory: InventoryView
  note: string | null
}

/** Ответ на кнопку задания: вещи после неё, что сказать, ушло ли задание дальше
  * в чат с ботом (`chat`) и что спросить прежде (`ask`). */
export interface QuestActionView {
  inventory: InventoryView
  note: string | null
  chat: boolean
  ask: QuestAskView | null
}

/** Хранилище вещей. Закрытое — `locked` с тем, как его открыть; открытое —
  * разделы с вещами (у стоек — по разделу на стойку) или счёт (руны, трофеи). */
export interface StorageView {
  id: string
  group: 'self' | 'city' | 'home'
  title: string
  locked: string | null
  /** Сколько занято из скольких; у алтаря мест не считают — null. */
  used: number | null
  capacity: number | null
  silver: PoolView | null
  note: string | null
  /** Разделы с вещами; `rack` — номер стойки, если раздел — стойка: её комплект можно надеть. */
  sections: { title: string | null; items: ItemView[]; rack: number | null }[]
  counts: { label: string; value: number }[]
}

export type Currency = 'silver' | 'doubloons'

export interface LotView {
  id: number
  item: ItemView
  price: number
  currency: Currency
  hoursLeft: number
  mine: boolean
  /** Неделя вышла — лот ждёт хозяина в Торговом доме. */
  unsold: boolean
}

export interface AuctionView {
  hero: HeroView
  lots: LotView[]
  mine: LotView[]
  /** Сколько серебра наберётся на покупку: на руках вместе с ячейкой. */
  silver: number
  /** Свободных мест в сумке: купленное ложится туда. */
  bagFree: number
  /** Почему сейчас нельзя покупать: герой не в городе. */
  away: string | null
}

/** Ответ на покупку: витрина после неё, что сказал Рахадим и куплено ли. */
export interface AuctionActionView {
  auction: AuctionView
  note: string
  bought: boolean
}

/** Характеристика: сейчас — со всем, что действует, и родная — до расы, травм и зелий. */
export interface StatView {
  current: number
  native: number
}

/** Запас: здоровье, броня, энергия. */
export interface PoolView {
  current: number
  max: number
}

export interface TraumaView {
  name: string
  /** Что режет травма — тем же текстом, что в боте. */
  penalties: string
}

/** «Персонаж» — те же числа, что в карточке бота. Сроки — готовым текстом бота. */
export interface CharacterView {
  hero: HeroView
  name: string | null
  race: string
  exp: number
  neededExp: number
  kills: number
  expFrozen: boolean
  rested: { expPct: number; left: string } | null
  blessingLeft: string | null
  instantRests: number
  str: StatView
  vit: StatView
  agi: StatView
  int: StatView
  hp: PoolView
  armor: PoolView
  energy: PoolView
  attack: number
  defence: number
  accuracy: number
  evasion: number
  upgradePoints: number
  traumaLeft: string | null
  traumas: TraumaView[]
  effects: EffectView[]
  achievements: { total: number; got: { title: string; bonus: string }[] }
  knowledge: { title: string; how: string; text: string }[]
}

/** Эффект — как на экране «Эффекты» бота. */
export interface EffectView {
  kind: 'buffs' | 'active' | 'passive' | 'set' | 'weapon' | 'armor'
  label: string
  text: string
}

/** Пройденное задание. `stats` — как его закончили другие герои, от большей
  * доли к меньшей, свой исход — `mine`; пусто — развилки нет или закончивших
  * пока мало, тогда `first` — «вы среди первых». */
export interface DoneQuestView {
  title: string
  story: boolean
  first: string | null
  stats: { pct: string; label: string; mine: boolean }[]
}

/** Журнал заданий бота по разделам и пройденные задания. */
export interface QuestsView {
  hero: HeroView
  story: { title: string; objectives: string } | null
  npcs: { title: string; step: string }[]
  dailies: { npc: string; line: string }[]
  boards: { board: string; what: string; ready: boolean; state: string }[]
  done: DoneQuestView[]
}

export interface AllyView {
  position: number
  name: string
  race: string
  /** Значок стихии; у крысы — зубы. */
  element: string
  lvl: number
  hp: PoolView
  armor: PoolView
  energy: PoolView
  attack: number
  defence: number
  accuracy: number
  evasion: number
  /** Сколько ему ещё в отряде — текстом бота; без срока — null. */
  leaves: string | null
  /** Можно ли отправить его в резерв: крысу туда не берут. */
  reservable: boolean
}

export interface SquadView {
  hero: HeroView
  positions: number
  heroPosition: number
  allies: AllyView[]
  /** Восставший ждёт у шатра, а мест нет ни в строю, ни в резерве: кем за него
    * пожертвовать. */
  pendingRisen: string | null
  /** Почему строй сейчас не перестроить: герой в бою. */
  locked: string | null
  /** Резерв — кто при герое, но не в строю, по порядку (место в строю у них 0).
    * В приказах союзника из резерва зовут номером в этом списке и именем. */
  reserve: AllyView[]
  /** Сколько в резерве мест. */
  reserveSize: number
}

/** Ответ на приказ отряду: строй после него и что сказать. */
export interface SquadActionView {
  squad: SquadView
  note: string | null
}

/** Строка топа. `me` — сам герой. */
export interface TopEntryView {
  place: number
  /** Имя героя. */
  name: string
  /** Страница игрока в ВК; на себя ссылки нет. */
  profile: string | null
  race: string
  lvl: number
  value: number
  me: boolean
}

export type TopKind = 'level' | 'depth' | 'kills' | 'achievements' | 'alchemy' | 'recipes'

/** Топ: первые герои; `me` — строка героя, если он не попал в показанных. */
export interface TopView {
  hero: HeroView
  kind: TopKind
  total: number
  rows: TopEntryView[]
  me: TopEntryView | null
}

/** Сила куба: свои заряды или энергия дома (алтарь Азата). */
export interface CubePowerView {
  kind: 'own' | 'home'
  current: number
  max: number
}

/** Куб Азата. Пока куба нет или он не ожил — `locked`: как его получить. */
export interface CubeView {
  status: 'absent' | 'found' | 'active'
  locked: string | null
  /** Куб переехал домой, на верстак ремесленника. */
  home: boolean
  power: CubePowerView
  capacity: number
  /** Что в кубе — каждая вещь своей ячейкой. */
  items: ItemView[]
  /** Сумка стопками — то, что можно положить (сюжетное не кладётся). */
  bag: ItemView[]
  /** Почему сейчас нельзя класть и активировать (герой не в городе); пусто — можно. */
  away: string | null
}

export interface CraftView {
  hero: HeroView
  cube: CubeView
}

/** Ответ на действие с кубом: куб после него, что сказать и какие вещи только что получились. */
export interface CraftActionView {
  craft: CraftView
  note: string | null
  made: number[]
}

/** Вещь в обмене и номера всех таких же: стопку кладут по одной. */
export interface TradeItemView {
  item: ItemView
  ids: number[]
}

/** Сторона обмена: вещи, деньги, записка и всё одной строкой. */
export interface TradeSideView {
  items: TradeItemView[]
  silver: number
  doubloons: number
  note: string | null
  summary: string
}

/** Предложение глазами игрока: `mine` — что он отдаёт, `theirs` — что получает;
  * `proposed` — его предложение (отзывают), а не ему (отклоняют). */
export interface TradeOfferView {
  id: number
  partner: string
  mine: TradeSideView
  theirs: TradeSideView
  yourTurn: boolean
  proposed: boolean
  leftMs: number
}

/** Обмен: предложения, что при герое можно положить, правила. `away` — почему
  * сейчас можно только отозвать и отклонить. */
export interface TradeView {
  hero: HeroView
  offers: TradeOfferView[]
  stock: TradeItemView[]
  feePct: number
  minMoney: number
  maxNote: number
  away: string | null
}

/** Ответ на действие с обменом: `done` — сделано, иначе `note` — почему нет. */
export interface TradeActionView {
  trade: TradeView
  note: string | null
  done: boolean
}
