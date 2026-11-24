import { launchParams } from './platform'
import type {
  AuctionActionView,
  AuctionSellView,
  AuctionView,
  CharacterView,
  CraftActionView,
  CraftView,
  Currency,
  InventoryActionView,
  InventoryView,
  QuestActionView,
  QuestsView,
  SquadActionView,
  SquadView,
  TopKind,
  TopView,
  HeroNamesView,
  TradeActionView,
  TradeView,
} from './types'

/** Где бот. Пусто — тот же адрес, что у приложения (на бою и в разработке через
  * прокси Vite); задаётся, если приложение лежит на хостинге ВК. */
const origin: string = import.meta.env.VITE_API_ORIGIN ?? ''

/** Почему не вышло: `expired` — приложение открыто больше суток назад,
  * `no-hero` — игрок ещё не начал игру, `bad-sign` / `off` — открыто не из ВК
  * или Mini App выключен, `offline` — нет связи, `server` — сбой бота. */
export class ApiError extends Error {
  constructor(readonly code: string) {
    super(code)
  }
}

async function get<T>(path: string): Promise<T> {
  return call<T>(path, 'GET')
}

/** Действие, которое меняет игру: бот делает его под тем же замком, что и ходы в чате. */
async function post<T>(path: string): Promise<T> {
  return call<T>(path, 'POST')
}

async function call<T>(path: string, method: 'GET' | 'POST'): Promise<T> {
  let res: Response
  try {
    res = await fetch(`${origin}/api/app/${path}`, { method, headers: { 'X-Launch-Params': launchParams } })
  } catch {
    throw new ApiError('offline')
  }
  if (res.ok) return (await res.json()) as T
  const body = (await res.json().catch(() => ({}))) as { error?: string }
  throw new ApiError(body.error ?? (res.status === 404 ? 'off' : 'server'))
}

/** Своя сторона обмена — в строку запроса. */
function tradeQuery(d: { items: number[]; silver: number; doubloons: number; note: string }): string {
  return (
    `items=${d.items.join(',')}&silver=${d.silver}&doubloons=${d.doubloons}` +
    (d.note ? `&note=${encodeURIComponent(d.note)}` : '')
  )
}

type TradeDraft = { items: number[]; silver: number; doubloons: number; note: string }

export const api = {
  trade: () => get<TradeView>('trade'),
  heroNames: () => get<HeroNamesView>('heroes'),
  tradePropose: (partner: string, d: TradeDraft) =>
    post<TradeActionView>(`trade/propose?partner=${encodeURIComponent(partner)}&${tradeQuery(d)}`),
  tradeAnswer: (id: number, d: TradeDraft) => post<TradeActionView>(`trade/answer?id=${id}&${tradeQuery(d)}`),
  tradeAgree: (id: number) => post<TradeActionView>(`trade/agree?id=${id}`),
  /** Подтвердить, когда партнёр уже согласился. */
  tradeAccept: (id: number) => post<TradeActionView>(`trade/accept?id=${id}`),
  tradeCancel: (id: number) => post<TradeActionView>(`trade/cancel?id=${id}`),
  character: () => get<CharacterView>('character'),
  quests: () => get<QuestsView>('quests'),
  squad: () => get<SquadView>('squad'),
  squadMove: (from: number, to: number) => post<SquadActionView>(`squad/move?from=${from}&to=${to}`),
  squadHero: (to: number) => post<SquadActionView>(`squad/hero?to=${to}`),
  squadDismiss: (at: number) => post<SquadActionView>(`squad/dismiss?at=${at}`),
  squadRisenTake: (at: number) => post<SquadActionView>(`squad/risen/take?at=${at}`),
  squadRisenDismiss: () => post<SquadActionView>('squad/risen/dismiss'),
  // Резерв: союзника в нём зовут номером в списке и именем — список мог сдвинуться.
  squadToReserve: (at: number) => post<SquadActionView>(`squad/reserve?at=${at}`),
  squadDeploy: (i: number, who: string, swap: number | null) =>
    post<SquadActionView>(
      `squad/reserve/deploy?i=${i}&who=${encodeURIComponent(who)}` + (swap === null ? '' : `&swap=${swap}`),
    ),
  squadReserveDismiss: (i: number, who: string) =>
    post<SquadActionView>(`squad/reserve/dismiss?i=${i}&who=${encodeURIComponent(who)}`),
  top: (kind: TopKind) => get<TopView>(`top?kind=${kind}`),
  inventory: () => get<InventoryView>('inventory'),
  equipWear: (slot: number, item: number) => post<InventoryActionView>(`equipment/wear?slot=${slot}&item=${item}`),
  equipTakeOff: (slot: number) => post<InventoryActionView>(`equipment/take-off?slot=${slot}`),
  equipBreakGem: (slot: number, socket: number) =>
    post<InventoryActionView>(`equipment/break-gem?slot=${slot}&socket=${socket}`),
  equipInsertGem: (slot: number, from: string, item: number) =>
    post<InventoryActionView>(`equipment/insert-gem?slot=${slot}&from=${from}&item=${item}`),
  equipChisel: (slot: number) => post<InventoryActionView>(`equipment/chisel?slot=${slot}`),
  beltPut: (item: number, cell: number | null) =>
    post<InventoryActionView>(`belt/put?item=${item}` + (cell === null ? '' : `&slot=${cell}`)),
  beltTake: (cell: number) => post<InventoryActionView>(`belt/take?slot=${cell}`),
  brewUse: (action: string, item: number, arg: string | null) =>
    post<InventoryActionView>(`brew/${action}?item=${item}` + (arg === null ? '' : `&arg=${encodeURIComponent(arg)}`)),
  bagUnpack: (item: number) => post<InventoryActionView>(`bag/unpack?item=${item}`),
  bagPack: (item: number) => post<InventoryActionView>(`bag/pack?item=${item}`),
  bagCrushGem: (item: number) => post<InventoryActionView>(`bag/crush-gem?item=${item}`),
  bagDust: (item: number) => post<InventoryActionView>(`bag/dust?item=${item}`),
  bagRead: (item: number) => post<InventoryActionView>(`bag/read?item=${item}`),
  bagQuest: (item: number, act: string) =>
    post<QuestActionView>(`bag/quest?item=${item}&act=${encodeURIComponent(act)}`),
  bagCombineMap: (item: number) => post<InventoryActionView>(`bag/combine-map?item=${item}`),
  bagStrayFeed: (item: number, grade: number) =>
    post<InventoryActionView>(`bag/stray-feed?item=${item}&grade=${grade}`),
  rackWear: (n: number) => post<InventoryActionView>(`racks/wear?n=${n}`),
  storeMove: (from: string, to: string, item: number, all: boolean) =>
    post<InventoryActionView>(`storages/move?from=${from}&to=${to}&item=${item}&all=${all}`),
  storeDrop: (from: string, item: number, all: boolean) =>
    post<InventoryActionView>(`storages/drop?from=${from}&item=${item}&all=${all}`),
  auction: () => get<AuctionView>('auction'),
  auctionBuy: (lot: number) => post<AuctionActionView>(`auction/buy?lot=${lot}`),
  auctionSell: (item: number, currency: Currency, price: number) =>
    post<AuctionSellView>(`auction/sell?item=${item}&currency=${currency}&price=${price}`),
  craft: () => get<CraftView>('craft'),
  cubeDeposit: (item: number) => post<CraftActionView>(`craft/cube/deposit?item=${item}`),
  cubeWithdraw: (item: number) => post<CraftActionView>(`craft/cube/withdraw?item=${item}`),
  cubeTakeAll: () => post<CraftActionView>('craft/cube/take-all'),
  cubeActivate: () => post<CraftActionView>('craft/cube/activate'),
}
