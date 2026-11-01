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
}

export interface SlotView {
  name: string
  item: ItemView | null
  /** Пустой слот, занятый чужой вещью: левая рука под двуручным. */
  busy: string | null
}

export interface HeroView {
  lvl: number
  silver: number
  doubloons: number
}

export interface InventoryView {
  hero: HeroView
  capacity: number
  occupied: number
  items: ItemView[]
  equipment: SlotView[]
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
