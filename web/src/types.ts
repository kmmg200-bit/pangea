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
  name: string
  item: ItemView | null
  /** Пустой слот, занятый чужой вещью: левая рука под двуручным. */
  busy: string | null
  /** Что лежит в поясе — только у слота пояса. */
  belt: BeltView | null
  /** Что из сумки встанет в этот слот. */
  candidates: CandidateView[]
}

/** Пояс: сколько ячеек всего и что в каждой. Опустевшая ячейка помнит, чем
  * была полна, — `uses` у неё ноль. */
export interface BeltView {
  capacity: number
  cells: { name: string; uses: number; max: number }[]
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
  sections: { title: string | null; items: ItemView[] }[]
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
  kind: 'active' | 'passive' | 'set' | 'weapon' | 'armor'
  label: string
  text: string
}

/** Журнал заданий бота по разделам. */
export interface QuestsView {
  hero: HeroView
  story: { title: string; objectives: string } | null
  npcs: { title: string; step: string }[]
  dailies: { npc: string; line: string }[]
  boards: { board: string; what: string; ready: boolean; state: string }[]
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
}

export interface SquadView {
  hero: HeroView
  positions: number
  heroPosition: number
  allies: AllyView[]
  pendingRisen: string | null
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

export type TopKind = 'level' | 'depth' | 'kills'

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
