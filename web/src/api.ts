import { launchParams } from './platform'
import type {
  AuctionActionView,
  AuctionView,
  CharacterView,
  CraftActionView,
  CraftView,
  GearActionView,
  InventoryView,
  QuestsView,
  SquadActionView,
  SquadView,
  TopKind,
  TopView,
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

export const api = {
  character: () => get<CharacterView>('character'),
  quests: () => get<QuestsView>('quests'),
  squad: () => get<SquadView>('squad'),
  squadMove: (from: number, to: number) => post<SquadActionView>(`squad/move?from=${from}&to=${to}`),
  squadHero: (to: number) => post<SquadActionView>(`squad/hero?to=${to}`),
  squadDismiss: (at: number) => post<SquadActionView>(`squad/dismiss?at=${at}`),
  squadRisenTake: (at: number) => post<SquadActionView>(`squad/risen/take?at=${at}`),
  squadRisenDismiss: () => post<SquadActionView>('squad/risen/dismiss'),
  top: (kind: TopKind) => get<TopView>(`top?kind=${kind}`),
  inventory: () => get<InventoryView>('inventory'),
  equipWear: (slot: number, item: number) => post<GearActionView>(`equipment/wear?slot=${slot}&item=${item}`),
  equipTakeOff: (slot: number) => post<GearActionView>(`equipment/take-off?slot=${slot}`),
  auction: () => get<AuctionView>('auction'),
  auctionBuy: (lot: number) => post<AuctionActionView>(`auction/buy?lot=${lot}`),
  craft: () => get<CraftView>('craft'),
  cubeDeposit: (item: number) => post<CraftActionView>(`craft/cube/deposit?item=${item}`),
  cubeWithdraw: (item: number) => post<CraftActionView>(`craft/cube/withdraw?item=${item}`),
  cubeTakeAll: () => post<CraftActionView>('craft/cube/take-all'),
  cubeActivate: () => post<CraftActionView>('craft/cube/activate'),
}
