import { useState, type ReactElement } from 'react'
import { api } from './api'
import { Silver } from './icons'
import { AuctionTab } from './AuctionTab'
import { BagTab } from './BagTab'
import { EquipmentTab } from './EquipmentTab'
import type { HeroView } from './types'
import { ErrorView, Spinner } from './ui'
import { useLoad, type Loaded } from './useLoad'

type Tab = 'bag' | 'equipment' | 'auction'

const tabs: { id: Tab; label: string }[] = [
  { id: 'bag', label: 'Сумка' },
  { id: 'equipment', label: 'Снаряжение' },
  { id: 'auction', label: 'Аукцион' },
]

function HeroBar(props: { hero: HeroView | undefined; loading: boolean; onReload: () => void }) {
  return (
    <header className="hero">
      <span className="hero-stats">
        {props.hero ? (
          <>
            <span>Ур. {props.hero.lvl}</span>
            <span>
              <Silver /> {props.hero.silver.toLocaleString('ru-RU')}
            </span>
            <span>🟡 {props.hero.doubloons.toLocaleString('ru-RU')}</span>
          </>
        ) : (
          <span>Клеймо Пангеи</span>
        )}
      </span>
      <button
        className={props.loading ? 'reload reload-on' : 'reload'}
        onClick={props.onReload}
        aria-label="Обновить"
        disabled={props.loading}
      >
        ⟳
      </button>
    </header>
  )
}

/** Содержимое вкладки: первая загрузка — крутилка, ошибка — что делать. */
function Body<T>(props: { loaded: Loaded<T>; children: (data: T) => ReactElement }) {
  const { data, error, reload } = props.loaded
  if (error && !data) return <ErrorView code={error} onRetry={reload} />
  if (!data) return <Spinner />
  return props.children(data)
}

export function App() {
  const [tab, setTab] = useState<Tab>('bag')
  // Аукцион грузится, только когда его открыли: витрина — самый тяжёлый ответ.
  const [auctionSeen, setAuctionSeen] = useState(false)
  const inventory = useLoad(api.inventory)
  const auction = useLoad(api.auction, auctionSeen)

  const current = tab === 'auction' ? auction : inventory
  const hero = (tab === 'auction' ? auction.data?.hero : undefined) ?? inventory.data?.hero

  return (
    <div className="app">
      <HeroBar hero={hero} loading={current.loading} onReload={current.reload} />
      <nav className="tabs">
        {tabs.map((t) => (
          <button
            key={t.id}
            className={t.id === tab ? 'tab tab-on' : 'tab'}
            onClick={() => {
              setTab(t.id)
              if (t.id === 'auction') setAuctionSeen(true)
            }}
          >
            {t.label}
          </button>
        ))}
      </nav>
      <main className="content">
        {tab === 'bag' && <Body loaded={inventory}>{(d) => <BagTab data={d} />}</Body>}
        {tab === 'equipment' && <Body loaded={inventory}>{(d) => <EquipmentTab data={d} />}</Body>}
        {tab === 'auction' && <Body loaded={auction}>{(d) => <AuctionTab data={d} />}</Body>}
      </main>
    </div>
  )
}
