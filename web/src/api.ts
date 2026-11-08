import { launchParams } from './platform'
import type {
  AuctionView,
  CharacterView,
  CraftActionView,
  CraftView,
  InventoryView,
  QuestsView,
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
  top: (kind: TopKind) => get<TopView>(`top?kind=${kind}`),
  inventory: () => get<InventoryView>('inventory'),
  auction: () => get<AuctionView>('auction'),
  craft: () => get<CraftView>('craft'),
  cubeDeposit: (item: number) => post<CraftActionView>(`craft/cube/deposit?item=${item}`),
  cubeWithdraw: (item: number) => post<CraftActionView>(`craft/cube/withdraw?item=${item}`),
  cubeTakeAll: () => post<CraftActionView>('craft/cube/take-all'),
  cubeActivate: () => post<CraftActionView>('craft/cube/activate'),
}
