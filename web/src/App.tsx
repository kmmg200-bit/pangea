import { useEffect, useRef, useState, type ReactElement } from 'react'
import { api } from './api'
import { Silver } from './icons'
import { AuctionTab } from './AuctionTab'
import { CharacterTab } from './CharacterTab'
import { CraftTab } from './CraftTab'
import { EquipmentTab } from './EquipmentTab'
import { ItemsTab } from './ItemsTab'
import { QuestsTab } from './QuestsTab'
import { SquadTab } from './SquadTab'
import { TopTab } from './TopTab'
import type { HeroView, TopKind, TopView } from './types'
import type { GameIcon } from './gameIcons'
import { ErrorView, Icon, Segment, Spinner } from './ui'
import { useLoad, type Loaded } from './useLoad'

/** Разделы: нижняя панель и топ — его кубок в шапке. */
type Tab = 'character' | 'quests' | 'items' | 'craft' | 'auction' | 'top'

const tabs: { id: Exclude<Tab, 'top'>; icon: GameIcon; label: string }[] = [
  { id: 'character', icon: 'hooded-figure', label: 'Персонаж' },
  { id: 'quests', icon: 'scroll-quill', label: 'Задания' },
  { id: 'items', icon: 'knapsack', label: 'Вещи' },
  { id: 'craft', icon: 'anvil', label: 'Крафт' },
  { id: 'auction', icon: 'gavel', label: 'Аукцион' },
]

/** Части «Персонажа» — как в меню героя в боте: сам герой, всё надетое и отряд. */
type HeroPart = 'hero' | 'equipment' | 'squad'

const heroParts: { id: HeroPart; label: string }[] = [
  { id: 'hero', label: 'Герой' },
  { id: 'equipment', label: 'Снаряжение' },
  { id: 'squad', label: 'Отряд' },
]

/** Раздел из адреса (`#top`, `#equipment`, `#squad`): так приложение можно открыть сразу на нужном. */
const hash = window.location.hash.replace(/^#/, '')
const hashPart = heroParts.find((p) => p.id === hash)?.id

function initialTab(): Tab {
  if (hashPart) return 'character'
  if (hash === 'top') return 'top'
  return tabs.find((t) => t.id === hash)?.id ?? 'character'
}

/** Загрузка каждого топа — одна на всё время работы, чтобы `useLoad` не грузил заново. */
const topLoaders: Record<TopKind, () => Promise<TopView>> = {
  level: () => api.top('level'),
  depth: () => api.top('depth'),
  kills: () => api.top('kills'),
}

/** Что уже открывали: вкладку, часть «Персонажа» или топ (`top-level`). */
type Seen = Tab | HeroPart | `top-${TopKind}`

function HeroBar(props: {
  hero: HeroView | undefined
  loading: boolean
  onReload: () => void
  topOn: boolean
  onTop: () => void
}) {
  // Высота шапки — в переменную: под ней закрепляется куб, а шапка с большими
  // деньгами бывает в две строки.
  const ref = useRef<HTMLElement>(null)
  useEffect(() => {
    const el = ref.current
    if (!el) return
    const ro = new ResizeObserver(() =>
      document.documentElement.style.setProperty('--hero-h', `${el.offsetHeight}px`),
    )
    ro.observe(el)
    return () => ro.disconnect()
  }, [])
  return (
    <header className="hero" ref={ref}>
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
      <span className="hero-actions">
        <button
          className={props.topOn ? 'top-button top-button-on' : 'top-button'}
          onClick={props.onTop}
          aria-current={props.topOn ? 'page' : undefined}
        >
          <Icon name="trophy-cup" size={20} />
          <span>Топ</span>
        </button>
        <button
          className={props.loading ? 'reload reload-on' : 'reload'}
          onClick={props.onReload}
          aria-label="Обновить"
          disabled={props.loading}
        >
          ⟳
        </button>
      </span>
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
  const [heroPart, setHeroPart] = useState<HeroPart>(hashPart ?? 'hero')
  const [topKind, setTopKind] = useState<TopKind>('level')
  const [seen, setSeen] = useState<Set<Seen>>(() => {
    const first = initialTab()
    return new Set<Seen>([first, first === 'top' ? 'top-level' : (hashPart ?? 'hero')])
  })
  const character = useLoad(api.character, seen.has('hero'))
  const quests = useLoad(api.quests, seen.has('quests'))
  const squad = useLoad(api.squad, seen.has('squad'))
  // Снаряжение берёт данные у «Вещей»: надетое, сумка и что из неё встанет в слот.
  const inventory = useLoad(api.inventory, seen.has('items') || seen.has('equipment'))
  const auction = useLoad(api.auction, seen.has('auction'))
  const craft = useLoad(api.craft, seen.has('craft'))
  const tops: Record<TopKind, Loaded<TopView>> = {
    level: useLoad(topLoaders.level, seen.has('top-level')),
    depth: useLoad(topLoaders.depth, seen.has('top-depth')),
    kills: useLoad(topLoaders.kills, seen.has('top-kills')),
  }

  const heroLoads = { hero: character, equipment: inventory, squad }
  const current: Omit<Loaded<{ hero: HeroView }>, 'set'> =
    tab === 'character' ? heroLoads[heroPart] : tab === 'top' ? tops[topKind] : { quests, items: inventory, craft, auction }[tab]
  // В шапке — самые свежие уровень и деньги: с открытой вкладки, а пока она
  // грузится — с любой уже загруженной.
  const hero =
    current?.data?.hero ??
    character.data?.hero ??
    quests.data?.hero ??
    squad.data?.hero ??
    inventory.data?.hero ??
    craft.data?.hero ??
    auction.data?.hero

  const markSeen = (id: Seen) => setSeen((prev) => (prev.has(id) ? prev : new Set(prev).add(id)))

  const open = (id: Tab) => {
    setTab(id)
    markSeen(id)
    if (id === 'top') markSeen(`top-${topKind}`)
    window.scrollTo(0, 0)
  }

  return (
    <div className="app">
      <HeroBar
        hero={hero}
        loading={current.loading}
        onReload={current.reload}
        topOn={tab === 'top'}
        onTop={() => open('top')}
      />
      <main className="content">
        {tab === 'character' && (
          <>
            <Segment
              options={heroParts}
              value={heroPart}
              onChange={(part) => {
                setHeroPart(part)
                markSeen(part)
              }}
            />
            {heroPart === 'hero' && <Body loaded={character}>{(d) => <CharacterTab data={d} />}</Body>}
            {heroPart === 'equipment' && (
              <Body loaded={inventory}>
                {(d) => (
                  <EquipmentTab
                    data={d}
                    onChange={(v) => {
                      // Сумка — у «Вещей» те же данные; статы героя и сумка «Крафта»,
                      // если их уже открывали, перечитают.
                      inventory.set(v)
                      if (character.data) character.reload()
                      if (craft.data) craft.reload()
                    }}
                  />
                )}
              </Body>
            )}
            {heroPart === 'squad' && <Body loaded={squad}>{(d) => <SquadTab data={d} />}</Body>}
          </>
        )}
        {tab === 'quests' && <Body loaded={quests}>{(d) => <QuestsTab data={d} />}</Body>}
        {tab === 'items' && <Body loaded={inventory}>{(d) => <ItemsTab data={d} />}</Body>}
        {tab === 'craft' && <Body loaded={craft}>{(d) => <CraftTab data={d} />}</Body>}
        {tab === 'top' && (
          <TopTab
            kind={topKind}
            onKind={(k) => {
              setTopKind(k)
              markSeen(`top-${k}`)
            }}
            loaded={tops[topKind]}
          />
        )}
        {tab === 'auction' && (
          <Body loaded={auction}>
            {(d) => (
              <AuctionTab
                data={d}
                onChange={(v, bought) => {
                  // Деньги в шапке — из свежей витрины. Купленное легло в сумку —
                  // «Вещи» и «Крафт», если их уже открывали, перечитают её.
                  auction.set(v)
                  if (bought && inventory.data) inventory.reload()
                  if (bought && craft.data) craft.reload()
                }}
              />
            )}
          </Body>
        )}
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
