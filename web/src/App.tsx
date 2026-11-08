import { useState, type ReactElement } from 'react'
import { api } from './api'
import { Silver } from './icons'
import { AuctionTab } from './AuctionTab'
import { CharacterTab } from './CharacterTab'
import { EquipmentTab } from './EquipmentTab'
import { ItemsTab } from './ItemsTab'
import { QuestsTab } from './QuestsTab'
import { SquadTab } from './SquadTab'
import type { HeroView } from './types'
import type { GameIcon } from './gameIcons'
import { ErrorView, Icon, Segment, Spinner } from './ui'
import { useLoad, type Loaded } from './useLoad'

type Tab = 'character' | 'quests' | 'squad' | 'items' | 'auction'

const tabs: { id: Tab; icon: GameIcon; label: string }[] = [
  { id: 'character', icon: 'hooded-figure', label: 'Персонаж' },
  { id: 'quests', icon: 'scroll-quill', label: 'Задания' },
  { id: 'squad', icon: 'three-friends', label: 'Отряд' },
  { id: 'items', icon: 'knapsack', label: 'Вещи' },
  { id: 'auction', icon: 'gavel', label: 'Аукцион' },
]

/** Части «Персонажа»: сам герой и всё надетое. */
type HeroPart = 'hero' | 'equipment'

const heroParts: { id: HeroPart; label: string }[] = [
  { id: 'hero', label: 'Герой' },
  { id: 'equipment', label: 'Снаряжение' },
]

/** Вкладка из адреса (`#squad`, `#equipment`): так приложение можно открыть сразу на нужной. */
const hash = window.location.hash.replace(/^#/, '')

function initialTab(): Tab {
  return hash === 'equipment' ? 'character' : (tabs.find((t) => t.id === hash)?.id ?? 'character')
}

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
  // Приложение открывается на «Персонаже», как и меню героя в боте. Остальное
  // грузится, когда вкладку впервые открыли: витрина аукциона — самый тяжёлый ответ.
  const [tab, setTab] = useState<Tab>(initialTab)
  const [heroPart, setHeroPart] = useState<HeroPart>(hash === 'equipment' ? 'equipment' : 'hero')
  // Снаряжение, раз открытое, держит данные: назад к герою и обратно — без перезагрузки.
  const [equipSeen, setEquipSeen] = useState(hash === 'equipment')
  const [seen, setSeen] = useState<Set<Tab>>(() => new Set<Tab>([initialTab()]))
  const character = useLoad(api.character, seen.has('character'))
  const quests = useLoad(api.quests, seen.has('quests'))
  const squad = useLoad(api.squad, seen.has('squad'))
  // Снаряжение берёт данные у «Вещей»: надетое, сумка и что из неё встанет в слот.
  const inventory = useLoad(api.inventory, seen.has('items') || equipSeen)
  const auction = useLoad(api.auction, seen.has('auction'))

  const loads = { character: heroPart === 'equipment' ? inventory : character, quests, squad, items: inventory, auction }
  const current = loads[tab]
  // В шапке — самые свежие уровень и деньги: с открытой вкладки, а пока она
  // грузится — с любой уже загруженной.
  const hero =
    current.data?.hero ??
    character.data?.hero ??
    quests.data?.hero ??
    squad.data?.hero ??
    inventory.data?.hero ??
    auction.data?.hero

  const open = (id: Tab) => {
    setTab(id)
    setSeen((prev) => (prev.has(id) ? prev : new Set(prev).add(id)))
    window.scrollTo(0, 0)
  }

  return (
    <div className="app">
      <HeroBar hero={hero} loading={current.loading} onReload={current.reload} />
      <main className="content">
        {tab === 'character' && (
          <>
            <Segment
              options={heroParts}
              value={heroPart}
              onChange={(part) => {
                setHeroPart(part)
                if (part === 'equipment') setEquipSeen(true)
              }}
            />
            {heroPart === 'hero' ? (
              <Body loaded={character}>{(d) => <CharacterTab data={d} />}</Body>
            ) : (
              <Body loaded={inventory}>{(d) => <EquipmentTab data={d} />}</Body>
            )}
          </>
        )}
        {tab === 'quests' && <Body loaded={quests}>{(d) => <QuestsTab data={d} />}</Body>}
        {tab === 'squad' && <Body loaded={squad}>{(d) => <SquadTab data={d} />}</Body>}
        {tab === 'items' && <Body loaded={inventory}>{(d) => <ItemsTab data={d} />}</Body>}
        {tab === 'auction' && <Body loaded={auction}>{(d) => <AuctionTab data={d} />}</Body>}
        {/* Лицензия CC BY 3.0 требует назвать авторов иконок. */}
        <p className="credits">Иконки: game-icons.net (CC BY 3.0)</p>
      </main>
      <nav className="nav" aria-label="Разделы">
        <div className="nav-inner">
          {tabs.map((t) => (
            <button
              key={t.id}
              className={t.id === tab ? 'nav-item nav-on' : 'nav-item'}
              aria-current={t.id === tab ? 'page' : undefined}
              onClick={() => open(t.id)}
            >
              <Icon name={t.icon} size={24} className="nav-icon" />
              <span className="nav-label">{t.label}</span>
            </button>
          ))}
        </div>
      </nav>
    </div>
  )
}
